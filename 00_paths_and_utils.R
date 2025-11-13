setwd("C:/Users/AMD/OneDrive - Shamba Centre/Shamba Centre Projects - Climate_Ag_evidencesynthesis_sciencepaper/data extraction protocol")

# ================= 00_paths_and_utils.R (for current extractors) =================

# -- 0) Packages --
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tibble)
  library(pdftools); library(tesseract)
  library(stringi)
})

# -- 1) Paths --
PROJECT_DIR <- "C:/Users/AMD/OneDrive - Shamba Centre/Shamba Centre Projects - Climate_Ag_evidencesynthesis_sciencepaper/data extraction protocol"
PDF_DIR     <- file.path(PROJECT_DIR, "papers")
OUT_DIR     <- file.path(PROJECT_DIR, "output_mod"); dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

COUNTRY_LIST_PATH          <- file.path(PROJECT_DIR, "countries_lmic.txt")
CHEATSHEET_INTERVENTIONS   <- file.path(PROJECT_DIR, "cheatsheet_interventions_merged_europe.csv")
AEZ_RAST_PATH              <- file.path(PROJECT_DIR, "mst_class_CRUTS32_Hist_8110_100_avg.tif")
AEZ_CODE_LEGEND            <- file.path(PROJECT_DIR, "aez_code_legend_mst_class_CRUTS32_Hist_8110_100_avg.csv")
AEZ_SHP_PATH               <- file.path(PROJECT_DIR, "aez_polygons.gpkg")
AEZ_MAPPING_CSV            <- file.path(PROJECT_DIR, "aez_mapping.csv")
CHEATSHEET_AEZ_PATH        <- file.path(PROJECT_DIR, "location_aez_cheatsheet.csv")
COUNTRY_AEZ_SINGLETONS     <- file.path(PROJECT_DIR, "country_single_aez.csv")

SECT_LOG       <- file.path(OUT_DIR, "missing_sections_log.csv")
SECT_FLAGS_ALL <- file.path(OUT_DIR, "section_flags_all.csv")

# -- 2) Helpers --
clean_text <- function(x){
  x <- stringi::stri_trans_general(x, "NFKC")
  x <- gsub("\u00A0", " ", x, fixed=TRUE)
  x <- gsub("[\u2018\u2019]", "'", x); x <- gsub("[\u201C\u201D]", "\"", x)
  x <- gsub("[\u2032']", "'", x); x <- gsub("[\u2033\"]", "\"", x)
  x <- gsub("(?<=\\w)-\\s*\\n\\s*(?=\\w)", "", x, perl=TRUE)   # de-hyphenate across line breaks
  x <- gsub("\\r?\\n+", " ", x)
  x <- gsub("\\s{2,}", " ", x)
  trimws(x)
}

pdf_to_text <- function(pdf_path){
  raw <- tryCatch(paste(pdftools::pdf_text(pdf_path), collapse="\n"), error=function(e) "")
  if (nchar(trimws(raw)) > 50) return(clean_text(raw))
  # OCR fallback
  tmpdir <- tempfile("pdfimg_"); dir.create(tmpdir)
  imgs <- tryCatch(pdftools::pdf_convert(pdf_path, dpi=300, filenames=file.path(tmpdir,"p%03d.png")),
                   error=function(e) character(0))
  if (!length(imgs)) { unlink(tmpdir, recursive=TRUE); return("") }
  eng_eur <- tesseract::tesseract(language = c("eng","spa","fra","por"))
  ocr_txt <- paste(vapply(imgs, function(im) tesseract::ocr(im, engine=eng_eur), character(1L)),
                   collapse="\n")
  unlink(tmpdir, recursive=TRUE)
  clean_text(ocr_txt)
}

strip_affiliations <- function(txt, first_chars = 20000){
  head <- substr(txt, 1, min(nchar(txt), first_chars))
  aff_terms <- paste(c("university","universidad","universidade","université","dept\\.?","department","facult(?:y|ad|é)",
                       "institute","instituto","institut","school of","escuela","address","dirección","direccion",
                       "street","st\\.","avenue","avenida","postal","c[oó]digo\\s*postal","zip",
                       "correo","email","e-mail","orcid","telephone","tel[eé]fono"), collapse="|")
  head <- stringr::str_replace_all(
    head, stringr::regex(sprintf("^.*(@|%s).*$", aff_terms), ignore_case=TRUE, multiline=TRUE), ""
  )
  head <- stringr::str_replace_all(
    head, stringr::regex("(?m)^.*\\b(PO Box|Apartado|Calle|Carrera|Campus|Suite|Room|Rm\\.|Edificio)\\b.*$", ignore_case=TRUE), ""
  )
  paste0(head, substr(txt, nchar(head)+1, nchar(txt)))
}

# Escape regex metacharacters (no external deps)
escape_regex <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(character(0))
  stringr::str_replace_all(x, "([\\.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1")
}

# Join vector; NA if empty
sc_join <- function(x, sep = "; ") {
  x <- unique(trimws(na.omit(x))); x <- x[nzchar(x)]
  if (!length(x)) return(NA_character_)
  paste(x, collapse = sep)
}

# -- 3) Section/Windowing (used by multiple extractors) --
.abstract_header_re <- "(?i)\\b(abstract|resumen|résumé|resumo|summary)\\b"
.methods_header_re <- paste0(
  "(?im)^\\s*",                                   # start of line, case-insens., multiline
  "(?:\\d+\\s*(?:\\.\\d+)*\\s*[\\.)]?\\s*)?",     # optional section numbering
  "(?:",
  "materials?\\s*(?:and|&)\\s*methods?|",       # Materials and Methods
  "methods?\\s*(?:and|&)\\s*materials?|",       # Methods and Materials
  "methodology|",                                # Methodology
  "research\\s+methodology|",                    # Research methodology
  "research\\s+methods?|",                       # Research methods
  "data\\s*(?:and|&)\\s*methods?|",              # Data and Methods
  "methods?\\s*(?:and|&)\\s*data|",              # Methods and Data
  "experimental\\s+design|",                     # Experimental design
  "study\\s+design"                              # Study design
  ,")\\b"
)

get_prioritized_windows <- function(text_raw, head_chars = 20000){
  out <- list()
  if (!nzchar(text_raw)) return(out)
  txt <- strip_affiliations(text_raw, first_chars = head_chars)
  
  # Abstract (~2k chars from header)
  abs_pos <- stringr::str_locate(txt, .abstract_header_re)
  if (!any(is.na(abs_pos))) {
    s <- abs_pos[1,1]; e <- min(nchar(txt), s + 2000)
    out[[length(out)+1]] <- list(type="abstract", text=substr(txt, s, e))
  }
  
  # Methods (~2k chars from header) ← rolled back
  met_pos <- stringr::str_locate(txt, .methods_header_re)
  if (!any(is.na(met_pos))) {
    s <- met_pos[1,1]; e <- min(nchar(txt), s + 2000)
    out[[length(out)+1]] <- list(type="methods", text=substr(txt, s, e))
  }
  
  # Head (first ~20k)
  out[[length(out)+1]] <- list(type="head", text=substr(txt, 1, min(nchar(txt), head_chars)))
  # Full text (fallback)
  out[[length(out)+1]] <- list(type="all", text=text_raw)
  out
}

# -- 4) Section flags/logs (also needed by merge script) --
#check_sections_one <- function(pdf_path){
 # txt <- pdf_to_text(pdf_path)
  #if (!nzchar(txt)) {
   # return(tibble(File = basename(pdf_path), Path = pdf_path,
    #              HasAbstract = FALSE, HasMethods = FALSE, Reason = "empty_or_unreadable"))
  #}
#  head <- substr(strip_affiliations(txt), 1, min(nchar(txt), 20000))
 # has_abs <- stringr::str_detect(head, .abstract_header_re)
  #has_met <- stringr::str_detect(head, .methods_header_re)
#  tibble(
 #   File        = basename(pdf_path),
  #  Path        = pdf_path,
   # HasAbstract = has_abs,
    #HasMethods  = has_met,
  #  Reason      = dplyr::case_when(
   #   !has_abs & !has_met ~ "no_abstract_and_methods",
    #  !has_abs            ~ "no_abstract",
     # !has_met            ~ "no_methods",
    #  TRUE                ~ ""
   # )
#  )
#}

#pdfs <- list.files(PDF_DIR, pattern="\\.pdf$", full.names=TRUE)
#section_check_df <- dplyr::bind_rows(lapply(pdfs, check_sections_one))

#readr::write_csv(section_check_df %>% dplyr::filter(!HasAbstract | !HasMethods), SECT_LOG)
#readr::write_csv(section_check_df, SECT_FLAGS_ALL)

#add_section_flags <- function(df){
 # dplyr::left_join(
  #  df,
   # section_check_df %>% dplyr::select(File, HasAbstract, HasMethods, SectionFlag = Reason),
  #  by = "File"
#  )
#}




























setwd("C:/Users/AMD/OneDrive - Shamba Centre/Shamba Centre Projects - Climate_Ag_evidencesynthesis_sciencepaper/data extraction protocol")

# ================= 00_paths_and_utils.R (ALT) =================

# -- 0) Packages --
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tibble)
  library(pdftools); library(tesseract)
  library(stringi)
})

# -- 1) Paths --
PROJECT_DIR <- "C:/Users/AMD/OneDrive - Shamba Centre/Shamba Centre Projects - Climate_Ag_evidencesynthesis_sciencepaper/data extraction protocol"
PDF_DIR     <- file.path(PROJECT_DIR, "papers")
OUT_DIR     <- file.path(PROJECT_DIR, "output_mod"); dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

COUNTRY_LIST_PATH          <- file.path(PROJECT_DIR, "countries_lmic.txt")
CHEATSHEET_INTERVENTIONS   <- file.path(PROJECT_DIR, "cheatsheet_interventions_merged_europe.csv")
AEZ_RAST_PATH              <- file.path(PROJECT_DIR, "mst_class_CRUTS32_Hist_8110_100_avg.tif")
AEZ_CODE_LEGEND            <- file.path(PROJECT_DIR, "aez_code_legend_mst_class_CRUTS32_Hist_8110_100_avg.csv")
AEZ_SHP_PATH               <- file.path(PROJECT_DIR, "aez_polygons.gpkg")
AEZ_MAPPING_CSV            <- file.path(PROJECT_DIR, "aez_mapping.csv")
CHEATSHEET_AEZ_PATH        <- file.path(PROJECT_DIR, "location_aez_cheatsheet.csv")
COUNTRY_AEZ_SINGLETONS     <- file.path(PROJECT_DIR, "country_single_aez.csv")

SECT_LOG       <- file.path(OUT_DIR, "missing_sections_log.csv")
SECT_FLAGS_ALL <- file.path(OUT_DIR, "section_flags_all.csv")

# -- 2) Helpers --
clean_text <- function(x){
  x <- stringi::stri_trans_general(x, "NFKC")
  x <- gsub("\u00A0", " ", x, fixed=TRUE)
  x <- gsub("[\u2018\u2019]", "'", x); x <- gsub("[\u201C\u201D]", "\"", x)
  x <- gsub("[\u2032']", "'", x); x <- gsub("[\u2033\"]", "\"", x)
  x <- gsub("(?<=\\w)-\\s*\\n\\s*(?=\\w)", "", x, perl=TRUE)   # de-hyphenate across line breaks
  x <- gsub("\\r?\\n+", " ", x)
  x <- gsub("\\s{2,}", " ", x)
  trimws(x)
}

# fully-cleaned text (for most extractors)
pdf_to_text <- function(pdf_path){
  raw <- tryCatch(paste(pdftools::pdf_text(pdf_path), collapse="\n"), error=function(e) "")
  if (nchar(trimws(raw)) > 50) return(clean_text(raw))
  # OCR fallback
  tmpdir <- tempfile("pdfimg_"); dir.create(tmpdir)
  imgs <- tryCatch(pdftools::pdf_convert(pdf_path, dpi=300, filenames=file.path(tmpdir,"p%03d.png")),
                   error=function(e) character(0))
  if (!length(imgs)) { unlink(tmpdir, recursive=TRUE); return("") }
  eng_eur <- tesseract::tesseract(language = c("eng","spa","fra","por"))
  ocr_txt <- paste(vapply(imgs, function(im) tesseract::ocr(im, engine=eng_eur), character(1L)),
                   collapse="\n")
  unlink(tmpdir, recursive=TRUE)
  clean_text(ocr_txt)
}

# line-preserving text (for heading detection)
pdf_to_text_lines <- function(pdf_path){
  tryCatch(paste(pdftools::pdf_text(pdf_path), collapse="\n"), error=function(e) "")
}

strip_affiliations <- function(txt, first_chars = 20000){
  head <- substr(txt, 1, min(nchar(txt), first_chars))
  aff_terms <- paste(c("university","universidad","universidade","université","dept\\.?","department","facult(?:y|ad|é)",
                       "institute","instituto","institut","school of","escuela","address","dirección","direccion",
                       "street","st\\.","avenue","avenida","postal","c[oó]digo\\s*postal","zip",
                       "correo","email","e-mail","orcid","telephone","tel[eé]fono"), collapse="|")
  head <- stringr::str_replace_all(
    head, stringr::regex(sprintf("^.*(@|%s).*$", aff_terms), ignore_case=TRUE, multiline=TRUE), ""
  )
  head <- stringr::str_replace_all(
    head, stringr::regex("(?m)^.*\\b(PO Box|Apartado|Calle|Carrera|Campus|Suite|Room|Rm\\.|Edificio)\\b.*$", ignore_case=TRUE), ""
  )
  paste0(head, substr(txt, nchar(head)+1, nchar(txt)))
}

# Escape regex metacharacters (no external deps)
escape_regex <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(character(0))
  stringr::str_replace_all(x, "([\\.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1")
}

# Join vector; NA if empty
sc_join <- function(x, sep = "; ") {
  x <- unique(trimws(na.omit(x))); x <- x[nzchar(x)]
  if (!length(x)) return(NA_character_)
  paste(x, collapse = sep)
}

# -- 3) Section/Windowing (used by multiple extractors) --
.abstract_header_re <- "(?i)\\b(abstract|resumen|résumé|resumo|summary)\\b"

# canonical list of Methods-like headings (extend as needed)
METHOD_HEADINGS <- c(
  "materials?\\s*(?:and|&)\\s*methods?",
  "methods?\\s*(?:and|&)\\s*materials?",
  "methodology",
  "research\\s+methodology",
  "research\\s+methods?",
  "data\\s*(?:and|&)\\s*methods?",
  "methods?\\s*(?:and|&)\\s*data",
  "experimental\\s+design",
  "study\\s+design",
  "analytical\\s+framework",
  "empirical\\s+(?:strategy|strategies|approach|model(?:s)?)",
  "statistical\\s+analysis",
  "econometric\\s+model(?:s)?"
)

# strict (start-of-line) + loose (anywhere) patterns
.methods_header_re <- paste0(
  "(?im)^\\s*(?:\\d+\\s*(?:\\.\\d+)*\\s*[\\.)]?\\s*)?(?:", paste(METHOD_HEADINGS, collapse="|"), ")\\b"
)
.methods_anywhere_re <- paste0("(?i)\\b(?:", paste(METHOD_HEADINGS, collapse="|"), ")\\b")

get_prioritized_windows <- function(text_raw, head_chars = 20000){
  out <- list()
  if (!nzchar(text_raw)) return(out)
  txt <- strip_affiliations(text_raw, first_chars = head_chars)
  
  # Abstract (~2k chars from header)
  abs_pos <- stringr::str_locate(txt, .abstract_header_re)
  if (!any(is.na(abs_pos))) {
    s <- abs_pos[1,1]; e <- min(nchar(txt), s + 2000)
    out[[length(out)+1]] <- list(type="abstract", text=substr(txt, s, e))
  }
  
  # Methods (~2k chars from header), strict then loose fallback
  met_pos <- stringr::str_locate(txt, .methods_header_re)
  if (any(is.na(met_pos))) met_pos <- stringr::str_locate(txt, .methods_anywhere_re)
  if (!any(is.na(met_pos))) {
    s <- met_pos[1,1]; e <- min(nchar(txt), s + 2000)
    out[[length(out)+1]] <- list(type="methods", text=substr(txt, s, e))
  }
  
  # Head (first ~20k)
  out[[length(out)+1]] <- list(type="head", text=substr(txt, 1, min(nchar(txt), head_chars)))
  # Full text (fallback)
  out[[length(out)+1]] <- list(type="all", text=text_raw)
  out
}

# -- 4) Section flags/logs (also needed by merge script) --
check_sections_one <- function(pdf_path){
  txt <- pdf_to_text_lines(pdf_path)  # keep line breaks for headings
  if (!nzchar(txt)) {
    return(tibble(File = basename(pdf_path), Path = pdf_path,
                  HasAbstract = FALSE, HasMethods = FALSE, Reason = "empty_or_unreadable"))
  }
  head <- substr(strip_affiliations(txt), 1, min(nchar(txt), 20000))
  has_abs <- stringr::str_detect(head, .abstract_header_re)
  has_met <- stringr::str_detect(head, .methods_header_re) | stringr::str_detect(head, .methods_anywhere_re)
  tibble(
    File        = basename(pdf_path),
    Path        = pdf_path,
    HasAbstract = has_abs,
    HasMethods  = has_met,
    Reason      = dplyr::case_when(
      !has_abs & !has_met ~ "no_abstract_and_methods",
      !has_abs            ~ "no_abstract",
      !has_met            ~ "no_methods",
      TRUE                ~ ""
    )
  )
}

pdfs <- list.files(PDF_DIR, pattern="\\.pdf$", full.names=TRUE)
section_check_df <- dplyr::bind_rows(lapply(pdfs, check_sections_one))

readr::write_csv(section_check_df %>% dplyr::filter(!HasAbstract | !HasMethods), SECT_LOG)
readr::write_csv(section_check_df, SECT_FLAGS_ALL)

add_section_flags <- function(df){
  dplyr::left_join(
    df,
    section_check_df %>% dplyr::select(File, HasAbstract, HasMethods, SectionFlag = Reason),
    by = "File"
  )
}

