# 10_country_extractor.R
source("00_paths_and_utils.R")

make_country_regex <- function(countries) paste0(
  "(?i)\\b(", paste(unique(escape_regex(countries[nzchar(countries)])), collapse="|"), ")\\b"
)
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

countries <- load_country_list()
country_regex <- make_country_regex(countries)

out <- lapply(pdfs, function(p){
  txt <- pdf_to_text(p)
  tibble(File=basename(p), Country=sc_join(extract_study_countries(txt, country_regex)))
}) |> bind_rows()


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
readr::write_csv(out, file.path(OUT_DIR, "country_only.csv"))
