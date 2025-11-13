
# 30_activity_extractor.R  (fast, recall-safe, NA on miss)
base::source("00_paths_and_utils.R")
suppressWarnings(library(stringi))

# ---------------- knobs ----------------
REQUIRE_ACTION_FIRST <- FALSE       # accept abstract/methods/results lists even without verbs
WINDOW_STRICT        <- 140
WINDOW_FALLBACK      <- 180
INCLUDE_RESULTS      <- TRUE        # include small Results slices
RESULTS_SLICE_CHARS  <- 12000       # per "Results" hit
# --------------------------------------

# FAST cheatsheet reader: detect delimiter only
read_intervention_cheatsheet <- function(path){
  if (!file.exists(path)) return(tibble(activity_phrase=character()))
  first <- tryCatch(readr::read_lines(path, n_max = 1), error = function(e) "")
  delim <- if (grepl(";", first, fixed = TRUE)) ";" else ","
  df <- if (delim == ";") {
    readr::read_delim(path, delim = ";", show_col_types = FALSE,
                      locale = readr::locale(encoding = "Windows-1252"))
  } else {
    readr::read_csv(path, show_col_types = FALSE,
                    locale = readr::locale(encoding = "Windows-1252"))
  }
  names(df) <- tolower(trimws(names(df)))
  if (!"activity_phrase" %in% names(df)) stop("Cheatsheet must have column: activity_phrase")
  df |>
    dplyr::transmute(activity_phrase = trimws(.data$activity_phrase)) |>
    dplyr::filter(nzchar(.data$activity_phrase))
}

# word boundaries + tolerate hyphen/en-dash/em-dash
make_regex <- function(phrases){
  phrases <- unique(phrases[nzchar(phrases)]); if (!length(phrases)) return(NA_character_)
  esc <- escape_regex(phrases)
  esc <- gsub(" ", "[\\s\\-–—]+", esc)                   # spaces/hyphens/dashes
  paste0("(?i)\\b(?:", paste(esc, collapse="|"), ")\\b") # case-insensitive, word-boundary
}

drop_trailing_sections <- function(txt){
  sub("(?is)\\b(references|bibliography|acknowledg(e)?ments|appendix|supplementary materials?)\\b.*$", "", txt, perl=TRUE)
}

# title + abstract + methods (+ Results only). NO Discussion.
build_search_text <- function(raw_txt){
  if (!nzchar(raw_txt)) return("")
  txt <- clean_text(raw_txt)
  txt <- strip_affiliations(txt)
  txt <- drop_trailing_sections(txt)
  
  parts <- character(0)
  
  # prioritized windows
  wins <- get_prioritized_windows(txt)
  if (length(wins)) parts <- c(parts, vapply(wins, `[[`, "", "text"))
  
  # Results slices only (no Discussion)
  if (INCLUDE_RESULTS) {
    res_locs <- stringi::stri_locate_all_regex(txt, "(?i)\\bresults?\\b")[[1]]
    if (!is.null(res_locs) && nrow(res_locs)) {
      for (j in seq_len(nrow(res_locs))) {
        s <- res_locs[j,1]
        parts <- c(parts, substr(txt, s, min(nchar(txt), s + RESULTS_SLICE_CHARS)))
      }
    }
  }
  
  if (!length(parts)) txt else paste(parts, collapse = "\n")
}

.action_verbs <- "\\b(adopt|implement|use|apply|install|establish|train|intercrop|rotate|irrigat|harvest|store|dry|mulch|seed|sow|compost|manure|fertiliz|plant|construct|desilt|introduc|promot|deploy|practice|practised|practiced)\\w*\\b"

find_activities <- function(text_raw, regex,
                            require_action_first = TRUE,
                            w_strict = 140, w_fallback = 180){
  if (is.na(regex) || !nzchar(regex)) return(character(0))
  
  # primary: curated windows (+ optional results)
  txt  <- build_search_text(text_raw)
  locs <- stringi::stri_locate_all_regex(txt, regex)[[1]]
  
  # single lightweight fallback: main text without Discussion/Conclusion
  if (!nrow(locs)) {
    main <- sub("(?is)\\b(discussion|conclusion|concluding remarks)\\b.*$", "", clean_text(text_raw), perl=TRUE)
    main <- drop_trailing_sections(main)
    locs <- stringi::stri_locate_all_regex(main, regex)[[1]]
    if (!nrow(locs)) return(character(0))
    txt <- main
  }
  
  low   <- tolower(txt)
  terms <- trimws(substr(txt, locs[,1], locs[,2]))
  
  gate <- function(window, require_action){
    keep <- logical(length(terms))
    for (i in seq_along(terms)){
      s <- locs[i,1]; e <- locs[i,2]
      cs <- max(1, s - window); ce <- min(nchar(txt), e + window)
      ctx <- substr(low, cs, ce)
      keep[i] <- if (require_action) grepl(.action_verbs, ctx) else TRUE
    }
    unique(terms[keep])
  }
  
  if (require_action_first){
    strict <- gate(w_strict, TRUE)
    if (length(strict)) return(strict)
  }
  gate(w_fallback, FALSE)
}

# --- main ---
phr <- read_intervention_cheatsheet(CHEATSHEET_INTERVENTIONS)
activity_regex <- make_regex(phr$activity_phrase)

out <- lapply(pdfs, function(p){
  txt  <- pdf_to_text(p)
  hits <- tryCatch(
    find_activities(txt, activity_regex,
                    require_action_first = REQUIRE_ACTION_FIRST,
                    w_strict = WINDOW_STRICT,
                    w_fallback = WINDOW_FALLBACK),
    error=function(e) character(0)
  )
  
  if (length(hits)){
    key  <- tolower(trimws(hits))
    hits <- hits[!duplicated(key)]
  } else {
    hits <- character(0)
  }
  
  tibble::tibble(
    File = basename(p),
    `Intervention activity type (raw)` = if (length(hits)) sc_join(hits) else NA_character_,
    `Intervention activity type`       = if (length(hits)) sc_join(trimws(hits)) else NA_character_
  )
}) |> dplyr::bind_rows()

normalize_for_write <- function(d){
  if (is.null(d) || !nrow(d)) return(tibble::tibble(File=character()))
  if (!"File" %in% names(d)) d$File <- NA_character_
  d %>%
    dplyr::mutate(
      File = as.character(File),
      File = basename(File),
      File = gsub("\\\\", "/", File),
      File = gsub("\\s+", " ", trimws(File))
    ) %>%
    dplyr::filter(!is.na(File) & File != "") %>%
    dplyr::distinct(File, .keep_all = TRUE)
}

out <- normalize_for_write(out)
readr::write_csv(out, file.path(OUT_DIR, "activity_only.csv"))