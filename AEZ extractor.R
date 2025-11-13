
# 40_aez_extractor_GEOCODE_FIRST.R — Geocode-first AEZ extraction (no local gazetteer)
# Fallback order:
#   1) Country singleton -> AEZ (skip raster)
#   2) Coords in text    -> FAO raster (enhanced patterns)
#   3) Geocoding terms   -> coords -> FAO raster   <-- geocoding FIRST, no CSV gazetteer
#   4) Country           -> country_aez_reference.csv
# Output: OUT_DIR/aez_only.csv  (File, AEZ, AEZ_Source, Country)

source("00_paths_and_utils.R")

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(readr); library(tibble); library(tidygeocoder);
  library(terra)
})

# ------------------- Paths (no gazetteer CSV) -------------------
COUNTRY_AEZ_REFERENCE_CSV <- file.path(PROJECT_DIR, "country_aez_reference.csv")

# ------------------- Load AEZ assets -----------------
aez_rast <- if (file.exists(AEZ_RAST_PATH)) tryCatch(terra::rast(AEZ_RAST_PATH), error=function(e) NULL) else NULL

prep_aez_legend <- function(df){
  if (is.null(df) || !nrow(df)) return(tibble(code=integer(), AEZ_NAME=character()))
  nm <- tolower(names(df)); names(df) <- nm
  code_col <- if ("code" %in% nm) "code" else names(df)[1]
  name_col <- if ("aez_name" %in% nm) "aez_name" else if ("name" %in% nm) "name" else names(df)[2]
  tibble(code = as.integer(df[[code_col]]),
         AEZ_NAME = as.character(df[[name_col]]))
}
aez_legend <- if (file.exists(AEZ_CODE_LEGEND)) {
  prep_aez_legend(readr::read_csv(AEZ_CODE_LEGEND, show_col_types = FALSE))
} else {
  tibble::tibble(code = integer(), AEZ_NAME = character())
}

# ------------------- Country→AEZ reference ----------------
load_country_aez_ref <- function(path){
  if (!file.exists(path)) return(tibble(country = character(), aez_list = I(list())))
  df <- readr::read_csv(path, show_col_types = FALSE)
  norm_name <- function(v){ v <- tolower(v); v <- gsub("\\p{Zs}+", " ", v, perl=TRUE); v <- trimws(v); gsub("[^a-z0-9]+", "_", v, perl=TRUE) }
  nm_raw <- names(df); nm <- norm_name(nm_raw); names(df) <- nm
  pick_col <- function(patterns){
    hits <- which(vapply(nm, function(n) any(stringr::str_detect(n, patterns)), logical(1)))
    if (length(hits)) nm[hits[1]] else NA_character_
  }
  country_col <- pick_col("(\\bcountry\\b|\\bnation\\b|\\biso2\\b|\\biso3\\b)")
  aez_col     <- pick_col("(\\baez\\b|agroecological|agro_ecological|zone)")
  rank_col    <- pick_col("(\\brank\\b|priority|order)")
  wt_col      <- pick_col("(weight|share|area|coverage)")
  if (is.na(country_col) || is.na(aez_col)) stop("country_aez_reference.csv must include columns for country and AEZ. Found: ", paste(nm_raw, collapse=", "))
  ref <- df %>%
    transmute(
      country = tools::toTitleCase(trimws(as.character(.data[[country_col]]))),
      aez     = trimws(as.character(.data[[aez_col]])),
      rank    = if (!is.na(rank_col)) suppressWarnings(as.numeric(.data[[rank_col]])) else NA_real_,
      weight  = if (!is.na(wt_col))   suppressWarnings(as.numeric(.data[[wt_col]]))   else NA_real_
    ) %>% filter(nzchar(country), nzchar(aez))
  ref %>% group_by(country) %>%
    arrange(desc(!is.na(weight)), desc(weight), desc(!is.na(rank)), rank, aez) %>%
    summarise(aez_list = list(unique(aez)), .groups="drop")
}
country_aez_ref <- load_country_aez_ref(COUNTRY_AEZ_REFERENCE_CSV)

pick_country_aez <- function(country_name){
  if (!nrow(country_aez_ref)) return("")
  row <- country_aez_ref[country_aez_ref$country == tools::toTitleCase(country_name), , drop=FALSE]
  if (!nrow(row)) return("")
  lst <- row$aez_list[[1]]; if (!length(lst)) return("")
  lst[1]
}

# ------------------- Country singletons ----------------
aez_singletons <- if (file.exists(COUNTRY_AEZ_SINGLETONS)) {
  readr::read_csv(COUNTRY_AEZ_SINGLETONS, show_col_types = FALSE) %>%
    mutate(country = tools::toTitleCase(trimws(country)),
           aez     = trimws(aez)) %>%
    distinct(country, aez, .keep_all = TRUE)
} else tibble(country=character(), aez=character())

# ------------------- Country detection helpers ----------------
make_country_regex <- function(countries)
  paste0("(?i)\\b(", paste(unique(escape_regex(countries[nzchar(countries)])), collapse="|"), ")\\b")

load_country_list <- function(){
  if (file.exists(COUNTRY_LIST_PATH)) unique(trimws(readr::read_lines(COUNTRY_LIST_PATH))) else character(0)
}

extract_study_countries <- function(text_raw, country_regex) {
  if (is.na(country_regex) || !nzchar(country_regex)) return(character(0))
  wins <- get_prioritized_windows(text_raw); if (!length(wins)) return(character(0))
  verb_re <- "(?i)\\b(conducted|carried out|implemented|performed|undertaken|executed|held|took place|study|trial|research|located|situated|field\\s+(experiment|trial)s?)\\b"
  prep_re <- "(?i)\\b(in|at|within|across)\\b"
  study_loc_head_re <- "(?i)\\b(study\\s+(area|site|location)|experimental\\s+(site|location))\\b"
  for (w in wins) {
    chunk <- clean_text(w$text); hits <- character(0)
    if (w$type == "abstract") {
      sents <- unlist(strsplit(chunk, "(?<=[.!?])\\s+", perl=TRUE))
      cand <- sents[stringr::str_detect(sents, verb_re) & stringr::str_detect(sents, prep_re)]
      hits <- unlist(stringr::str_extract_all(cand, country_regex))
      if (length(hits)) return(unique(tools::toTitleCase(tolower(trimws(hits)))))
    }
    if (w$type == "methods") {
      head <- substr(chunk, 1, min(nchar(chunk), 2000))
      sents <- unlist(strsplit(head, "(?<=[.!?])\\s+", perl=TRUE))
      cand <- sents[stringr::str_detect(sents, verb_re) & stringr::str_detect(sents, prep_re) |
                      stringr::str_detect(sents, study_loc_head_re)]
      hits <- unlist(stringr::str_extract_all(if(length(cand)) cand else head, country_regex))
      if (length(hits)) return(unique(tools::toTitleCase(tolower(trimws(hits)))))
    }
    hits <- unlist(stringr::str_extract_all(chunk, country_regex))
    if (length(hits)) return(unique(tools::toTitleCase(tolower(trimws(hits)))))
  }
  character(0)
}

# ------------------- Place-term extraction ----------------
extract_place_terms <- function(txt, country_regex){
  t <- clean_text(strip_affiliations(txt))
  wins <- get_prioritized_windows(t)
  keep_types <- c("title","abstract","methods")
  chunks <- if (length(wins)) vapply(wins[vapply(wins, function(x) x$type %in% keep_types, logical(1))], `[[`, character(1), "text") else character(0)
  sub <- if (length(chunks)) paste(chunks, collapse=" ") else t
  verb_re <- "(?i)\\b(conducted|carried out|implemented|performed|located|situated|took place|trial|study|research)\\b"
  prep_re <- "(?i)\\b(in|at|within|across|around|near)\\b"
  sents <- unlist(strsplit(sub, "(?<=[.!?])\\s+", perl=TRUE))
  sents <- sents[stringr::str_detect(sents, verb_re) & stringr::str_detect(sents, prep_re)]
  if (!length(sents)) sents <- sub
  spans <- unlist(stringr::str_extract_all(sents, "(?i)\\b(in|at|within|across|around|near)\\s+([A-Z][A-Za-z\\-\\.']+(?:\\s+[A-Z][A-Za-z\\-\\.']+)*)"))
  if (!length(spans)) return(character(0))
  cand <- unique(trimws(gsub("(?i)^(in|at|within|across|around|near)\\s+", "", spans)))
  if (nzchar(country_regex)) cand <- cand[!stringr::str_detect(cand, country_regex)]
  bad <- "(?i)^(materials? (and|&) methods?|methods?|introduction|background|keywords?|abstract|appendix|supplementary)$"
  cand <- cand[!stringr::str_detect(cand, bad)]
  cand <- cand[vapply(strsplit(cand, "\\s+"), length, integer(1)) <= 4]
  cand
}

# ------------------- Enhanced Coordinate parsing -----------------
strip_urls_emails <- function(x){
  x <- gsub("(https?://|www\\.)\\S+", " ", x, perl=TRUE)
  x <- gsub("\\bdoi:\\s*\\S+", " ", x, perl=TRUE)
  x <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", " ", x, perl=TRUE)
  x
}
normalize_decimal_commas <- function(x){ gsub("(?<=\\d),(?=\\d{2,8}(?:\\s*[°ºNSWE]|\\b))", ".", x, perl=TRUE) }
line_has_coord_cue <- function(line){ grepl("(?i)\\b(lat|lon|long|lng|coord|gps|utm|°|º|′|'|deg|north|south|east|west|N|S|E|W)\\b", line, perl=TRUE) }

DMS_FULL_PAT     <- "\\b(\\d{1,2})[°º]\\s*(\\d{1,2})['′]\\s*(\\d{1,2}(?:\\.\\d+)?)[\"″]?\\s*([NS])\\s*[,;\\s]+(\\d{1,3})[°º]\\s*(\\d{1,2})['′]\\s*(\\d{1,2}(?:\\.\\d+)?)[\"″]?\\s*([EW])\\b"
DMS_NO_SEC_PAT   <- "\\b(\\d{1,2})[°º]\\s*(\\d{1,2})['′]\\s*([NS])\\s*[,;\\s]+(\\d{1,3})[°º]\\s*(\\d{1,2})['′]\\s*([EW])\\b"
DMS_DEG_ONLY_PAT <- "\\b(\\d{1,2})[°º]\\s*([NS])\\s*[,;\\s]+(\\d{1,3})[°º]\\s*([EW])\\b"
DEC_CARD_PAT     <- "\\b([+-]?\\d{1,2}(?:\\.\\d{2,8})?)\\s*(?:[°º])?\\s*([NS])\\s*[,;\\s]+([+-]?\\d{1,3}(?:\\.\\d{2,8})?)\\s*(?:[°º])?\\s*([EW])\\b"
DEC_SIGNED_PAT   <- "(?<![\\dA-Za-z-])([+-]?\\d{1,2}(?:\\.\\d{2,8})?)\\s*[,;\\s]+([+-]?\\d{1,3}(?:\\.\\d{2,8})?)(?![\\dA-Za-z])"
DMS_SPACE_PAT    <- "\\b(\\d{1,2})\\s+deg\\s+(\\d{1,2})\\s+min\\s+([NS])\\s*[,;\\s]+(\\d{1,3})\\s+deg\\s+(\\d{1,2})\\s+min\\s+([EW])\\b"
DMS_WORD_PAT     <- "\\b(\\d{1,2})\\s*degrees?\\s+(\\d{1,2})\\s*minutes?\\s+([NS])\\s*[,;\\s]+(\\d{1,3})\\s*degrees?\\s+(\\d{1,2})\\s*minutes?\\s+([EW])\\b"

extract_coordinates <- function(txt){
  t <- strip_urls_emails(clean_text(normalize_decimal_commas(txt)))
  lines <- unlist(strsplit(t, "(?:\\n|(?<=\\.)\\s+)", perl=TRUE))
  lines <- lines[nchar(lines) >= 8 & nchar(lines) <= 500]
  cand <- c(
    unlist(lapply(lines, function(L) stringr::str_extract_all(L, DMS_FULL_PAT)[[1]])),
    unlist(lapply(lines, function(L) stringr::str_extract_all(L, DMS_NO_SEC_PAT)[[1]])),
    unlist(lapply(lines, function(L) stringr::str_extract_all(L, DMS_DEG_ONLY_PAT)[[1]])),
    unlist(lapply(lines, function(L) stringr::str_extract_all(L, DMS_SPACE_PAT)[[1]])),
    unlist(lapply(lines, function(L) stringr::str_extract_all(L, DMS_WORD_PAT)[[1]])),
    unlist(lapply(lines, function(L) stringr::str_extract_all(L, DEC_CARD_PAT)[[1]]))
  )
  cue_idx <- vapply(lines, line_has_coord_cue, logical(1))
  if (any(cue_idx)) {
    bare_hits <- unlist(lapply(lines[cue_idx], function(L) stringr::str_extract_all(L, DEC_SIGNED_PAT)[[1]]))
    if (length(bare_hits)) bare_hits <- bare_hits[grepl("\\.", bare_hits)]
    cand <- c(cand, bare_hits)
  }
  if (!length(cand)) return(character(0))
  cand <- unique(trimws(cand))
  junk_re <- "(?i)(\\$|€|£|%|±|\\b(?:mg|g|kg|mm|cm|km|m|km/h|m/s|ha|ppm|ppb|pH|°C|°F|eq)\\b)"
  cand <- cand[!stringr::str_detect(cand, junk_re)]
  cand <- cand[nchar(cand) >= 7 & nchar(cand) <= 70]
  cand
}

dms_to_decimal_pair <- function(s){
  s <- trimws(s)
  mm <- stringr::str_match(s, DMS_FULL_PAT)
  if (!any(is.na(mm))){
    lat <- as.numeric(mm[2]) + as.numeric(mm[3])/60 + as.numeric(mm[4])/3600
    lon <- as.numeric(mm[6]) + as.numeric(mm[7])/60 + as.numeric(mm[8])/3600
    if (toupper(mm[5])=="S") lat <- -lat
    if (toupper(mm[9])=="W") lon <- -lon
    return(c(lat, lon))
  }
  mm <- stringr::str_match(s, DMS_NO_SEC_PAT)
  if (!any(is.na(mm))){
    lat <- as.numeric(mm[2]) + as.numeric(mm[3])/60
    lon <- as.numeric(mm[5]) + as.numeric(mm[6])/60
    if (toupper(mm[4])=="S") lat <- -lat
    if (toupper(mm[7])=="W") lon <- -lon
    return(c(lat, lon))
  }
  mm <- stringr::str_match(s, DMS_DEG_ONLY_PAT)
  if (!any(is.na(mm))){
    lat <- as.numeric(mm[2]); lon <- as.numeric(mm[4])
    if (toupper(mm[3])=="S") lat <- -lat
    if (toupper(mm[5])=="W") lon <- -lon
    return(c(lat, lon))
  }
  mm <- stringr::str_match(s, DEC_CARD_PAT)
  if (!any(is.na(mm))){
    lat <- as.numeric(mm[2]); lon <- as.numeric(mm[4])
    if (toupper(mm[3])=="S") lat <- -lat
    if (toupper(mm[5])=="W") lon <- -lon
    return(c(lat, lon))
  }
  mm <- stringr::str_match(s, DMS_SPACE_PAT)
  if (!any(is.na(mm))){
    lat <- as.numeric(mm[2]) + as.numeric(mm[3])/60
    lon <- as.numeric(mm[5]) + as.numeric(mm[6])/60
    if (toupper(mm[4])=="S") lat <- -lat
    if (toupper(mm[7])=="W") lon <- -lon
    return(c(lat, lon))
  }
  mm <- stringr::str_match(s, DMS_WORD_PAT)
  if (!any(is.na(mm))){
    lat <- as.numeric(mm[2]) + as.numeric(mm[3])/60
    lon <- as.numeric(mm[5]) + as.numeric(mm[6])/60
    if (toupper(mm[4])=="S") lat <- -lat
    if (toupper(mm[7])=="W") lon <- -lon
    return(c(lat, lon))
  }
  mm <- stringr::str_match(s, DEC_SIGNED_PAT)
  if (!any(is.na(mm))) {
    if (!grepl("\\.", mm[2]) && !grepl("\\.", mm[3])) return(c(NA_real_, NA_real_))
    return(c(as.numeric(mm[2]), as.numeric(mm[3])))
  }
  c(NA_real_, NA_real_)
}

coords_to_dec <- function(strings){
  if (!length(strings)) return(tibble(lat=numeric(0), lon=numeric(0)))
  mat <- do.call(rbind, lapply(strings, dms_to_decimal_pair))
  if (is.null(dim(mat))) return(tibble(lat=numeric(0), lon=numeric(0)))
  valid <- !is.na(mat[,1]) & !is.na(mat[,2]) & mat[,1] >= -90 & mat[,1] <= 90 & mat[,2] >= -180 & mat[,2] <= 180
  if (!any(valid)) return(tibble(lat=numeric(0), lon=numeric(0)))
  unique(tibble(lat = as.numeric(mat[valid,1]), lon = as.numeric(mat[valid,2])))
}

# ------------------- AEZ from raster (CRS-safe) ---------------
aez_from_raster <- function(lat, lon){
  if (is.null(aez_rast) || !nrow(aez_legend) || !length(lat)) return("")
  pts <- terra::vect(cbind(lon, lat), crs = "EPSG:4326")
  rcrs <- tryCatch(terra::crs(aez_rast, proj=TRUE), error=function(e) NA_character_)
  if (!is.na(rcrs) && !grepl("4326|longlat", rcrs, ignore.case=TRUE)) {
    pts <- tryCatch(terra::project(pts, aez_rast), error=function(e) pts)
  }
  ext <- tryCatch(terra::extract(aez_rast, pts), error=function(e) NULL)
  if (is.null(ext) || !nrow(ext)) return("")
  val_col <- intersect(names(ext), names(aez_rast)); if (!length(val_col)) val_col <- names(ext)[ncol(ext)]
  codes <- as.integer(ext[[val_col[1]]])
  nm <- aez_legend$AEZ_NAME[match(codes, aez_legend$code)]
  nm <- nm[!is.na(nm) & nm != "Unknown"]
  if (!length(nm)) return("")
  names(sort(table(nm), decreasing = TRUE))[1]
}

# ------------------- Geocode-first lookup (NO local CSV) ------
geocode_terms_first <- function(place_terms, country_tc) {
  if (!length(place_terms)) return(tibble(lat=double(), lon=double()))
  if (!requireNamespace("tidygeocoder", quietly = TRUE)) return(tibble(lat=double(), lon=double()))
  ntry <- min(5, length(place_terms))
  addrs <- if (!is.na(country_tc)) paste(place_terms[1:ntry], country_tc) else place_terms[1:ntry]
  df <- tibble::tibble(address = addrs)
  res <- tryCatch(
    tidygeocoder::geocode(df, address = address, method = "osm", limit = 1, quiet = TRUE),
    error = function(e) tibble::tibble(address=character(), lat=numeric(), long=numeric())
  )
  if (!nrow(res) || !all(c("lat","long") %in% names(res))) return(tibble(lat=double(), lon=double()))
  good <- res %>% filter(!is.na(lat), !is.na(long))
  if (!nrow(good)) return(tibble(lat=double(), lon=double()))
  tibble(lat = as.numeric(good$lat), lon = as.numeric(good$long))
}

# ------------------- Run -------------------------------------
countries <- load_country_list()
country_regex <- make_country_regex(countries)
pdfs <- list.files(PDF_DIR, pattern="\\.pdf$", full.names=TRUE)
if (!length(pdfs)) stop("No PDFs in: ", PDF_DIR)

out <- lapply(pdfs, function(p){
  txt <- pdf_to_text(p)
  if (!nzchar(txt)) return(tibble(File=basename(p), AEZ="", AEZ_Source="", Country=""))
  
  wins  <- get_prioritized_windows(txt)
  types <- if (length(wins)) vapply(wins, function(x) x$type, character(1)) else character(0)
  methods_text <- if (any(types=="methods")) paste(vapply(wins[types=="methods"], function(x) x$text, character(1)), collapse=" ") else ""
  if (!nzchar(methods_text)) methods_text <- txt
  
  co <- extract_study_countries(txt, country_regex)
  co_tc <- if (length(co)) tools::toTitleCase(co[1]) else NA_character_
  
  # Step 1: country singleton
  if (length(co) == 1 && nrow(aez_singletons) && any(aez_singletons$country == co_tc)) {
    aezz <- aez_singletons$aez[aez_singletons$country == co_tc][1]
    return(tibble(File=basename(p), AEZ=aezz, AEZ_Source="country_singleton", Country=sc_join(co)))
  }
  
  aezz <- ""; src <- ""
  
  # Step 2: coords in text -> raster
  coords_raw <- unique(c(extract_coordinates(methods_text), extract_coordinates(txt)))
  dec <- coords_to_dec(coords_raw)
  if (nrow(dec)) {
    aezz <- aez_from_raster(dec$lat, dec$lon)
    if (nzchar(aezz)) src <- "coords->raster"
  }
  
  # Step 3: GEOCODING TERMS -> raster (NO local gazetteer)
  if (!nzchar(aezz)) {
    terms <- extract_place_terms(txt, country_regex)
    if (length(terms)) {
      gc <- geocode_terms_first(terms, co_tc)
      if (nrow(gc)) {
        aezz <- aez_from_raster(gc$lat, gc$lon)
        if (nzchar(aezz)) src <- "geocode->raster"
      }
    }
  }
  
  # Step 4: country -> reference map
  if (!nzchar(aezz) && length(co)) {
    aezz_try <- pick_country_aez(co_tc)
    if (nzchar(aezz_try)) { aezz <- aezz_try; src <- "country_reference" }
  }
  
  tibble(File=basename(p), AEZ=aezz, AEZ_Source=src, Country=sc_join(co))
}) %>% bind_rows()


out <- out %>% dplyr::rename(Country_from_AEZ = Country)
out <- out %>% mutate(File = basename(File)) %>% distinct(File, .keep_all = TRUE)
readr::write_csv(out, file.path(OUT_DIR, "aez_only.csv"))

