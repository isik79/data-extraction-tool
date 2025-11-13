# 60_outcome_measures_extraction.R
source("00_paths_and_utils.R")

# ---- Load and prepare outcome cheatsheet ----
CHEATSHEET_OUTCOMES <- file.path(PROJECT_DIR, "cheatsheet_outcomes_FS_CR_ambiguity_only_europe.csv")

read_cheatsheet <- function(path) {
  readr::read_delim(path, delim=";", show_col_types = FALSE,
                    locale = locale(encoding="Windows-1252"))
}

outcome_cs <- read_cheatsheet(CHEATSHEET_OUTCOMES) %>%
  mutate(`Search phrase (exact from paper)` = trimws(`Search phrase (exact from paper)`),
         `Outcome type (FS or CR)` = trimws(`Outcome type (FS or CR)`),
         `Output column name in extracted_auto` = trimws(`Output column name in extracted_auto`))

# Create canonical maps for FS and CR
fs_map <- outcome_cs %>%
  dplyr::filter(`Outcome type (FS or CR)` == "FS") %>%
  dplyr::transmute(
    search_norm = tolower(trimws(`Search phrase (exact from paper)`)),
    canon       = trimws(`Output column name in extracted_auto`)
  )

cr_map <- outcome_cs %>%
  dplyr::filter(`Outcome type (FS or CR)` == "CR") %>%
  dplyr::transmute(
    search_norm = tolower(trimws(`Search phrase (exact from paper)`)),
    canon       = trimws(`Output column name in extracted_auto`)
  )

# ---- Helper functions ----
make_bounded_regex <- function(phrases) {
  phrases <- unique(phrases[nzchar(phrases)])
  if (length(phrases) == 0) return(NA_character_)
  esc <- escape_regex(phrases)
  esc <- gsub(" ", "\\\\s+", esc)
  paste0("(?i)\\b(", paste(esc, collapse="|"), ")\\b")
}

# Trim everything after trailing sections
drop_trailing_sections <- function(txt){
  sub("(?is)\\b(references|bibliography|acknowledg(e)?ments|appendix|supplementary materials?)\\b.*$", "", txt, perl=TRUE)
}

window_snippet <- function(text, start_idx, end_idx, words_each_side = 25) {
  span <- 800
  s <- max(1, start_idx - span)
  e <- min(nchar(text), end_idx + span)
  piece <- substr(text, s, e)
  toks <- unlist(strsplit(piece, "\\s+"))
  if (length(toks) > (2*words_each_side + 1)) {
    center <- floor(length(toks) / 2)
    from <- max(1, center - words_each_side)
    to   <- min(length(toks), center + words_each_side)
    snippet <- paste(toks[from:to], collapse = " ")
  } else {
    snippet <- paste(toks, collapse = " ")
  }
  snippet <- gsub(";", ",", snippet)
  snippet <- gsub("\\s{2,}", " ", snippet)
  trimws(snippet)
}

find_with_window <- function(text_raw, regex) {
  if (is.na(regex) || !nzchar(regex)) return(tibble(term = character(), context = character()))
  # strip affiliations & drop trailing sections BEFORE cleaning
  txt <- strip_affiliations(text_raw)
  txt <- drop_trailing_sections(txt)
  txt <- clean_text(txt)
  
  locs <- stringr::str_locate_all(txt, regex)[[1]]
  if (nrow(locs) == 0) return(tibble(term = character(), context = character()))
  out <- lapply(seq_len(nrow(locs)), function(i) {
    start <- locs[i,1]; end <- locs[i,2]
    term  <- stringr::str_sub(txt, start, end)
    ctx   <- window_snippet(txt, start, end, words_each_side = 25)
    tibble(term = trimws(term), context = ctx)
  })
  dplyr::bind_rows(out)
}

# ---- Create regex patterns ----
# FS: bounded regex for cleaner matching
fs_regex <- make_bounded_regex(
  outcome_cs %>%
    filter(`Outcome type (FS or CR)` == "FS") %>%
    pull(`Search phrase (exact from paper)`)
)

# CR: bounded regex (to avoid long-sentence partial hits)
cr_regex <- make_bounded_regex(
  outcome_cs %>%
    filter(`Outcome type (FS or CR)` == "CR") %>%
    pull(`Search phrase (exact from paper)`)
)

# ---- Main extraction function ----
extract_outcome_measures <- function(text_raw) {
  
  # Extract Food Security outcomes
  fs_ctx <- find_with_window(text_raw, fs_regex)
  found_fs <- unique(trimws(fs_ctx$term))
  found_fs_norm <- tolower(trimws(found_fs))
  mapped_fs <- unique(fs_map$canon[ match(found_fs_norm, fs_map$search_norm) ])
  mapped_fs <- mapped_fs[!is.na(mapped_fs)]
  
  # Extract Climate Resilience outcomes  
  cr_ctx <- find_with_window(text_raw, cr_regex)
  found_cr <- unique(trimws(cr_ctx$term))
  found_cr_norm <- tolower(trimws(found_cr))
  mapped_cr <- unique(cr_map$canon[ match(found_cr_norm, cr_map$search_norm) ])
  mapped_cr <- mapped_cr[!is.na(mapped_cr)]
  
  list(
    fs_measures = sc_join(mapped_fs),
    cr_measures = sc_join(mapped_cr),
    fs_raw_terms = sc_join(found_fs),
    cr_raw_terms = sc_join(found_cr)
  )
}

# ---- Process all PDFs ----
outcome_results <- tibble(
  File = character(),
  FoodSecurityMeasures = character(),
  ClimateResilienceMeasures = character(),
  FSRawTerms = character(),
  CRRawTerms = character()
)

for (pdf_path in pdfs) {
  file_name <- basename(pdf_path)
  cat("Processing:", file_name, "\n")
  
  txt_raw <- pdf_to_text(pdf_path)
  
  if (nchar(trimws(txt_raw)) < 50) {
    # No text extracted
    outcome_results <- outcome_results %>%
      add_row(
        File = file_name, 
        FoodSecurityMeasures = "",
        ClimateResilienceMeasures = "",
        FSRawTerms = "",
        CRRawTerms = ""
      )
    next
  }
  
  # Extract outcome measures
  outcomes <- extract_outcome_measures(txt_raw)
  
  outcome_results <- outcome_results %>%
    add_row(
      File = file_name,
      FoodSecurityMeasures = outcomes$fs_measures,
      ClimateResilienceMeasures = outcomes$cr_measures,
      FSRawTerms = outcomes$fs_raw_terms,
      CRRawTerms = outcomes$cr_raw_terms
    )
}

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

outcome_results <- normalize_for_write(outcome_results)
readr::write_csv(outcome_results, file.path(OUT_DIR, "outcome_measures_only.csv"))


