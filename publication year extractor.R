# 20_year_extractor.R
source("00_paths_and_utils.R")

extract_pubyear <- function(txt, pdf_path, head_chars=15000){
  keep_range <- function(y){ y <- as.integer(y); y[!is.na(y) & y >= 2005 & y <= 2025] }
  head0 <- clean_text(txt)
  cut <- stringr::str_locate(head0, "(?i)\\b(references|bibliography|literature cited|works cited)\\b")[1]
  if (!is.na(cut)) head0 <- substr(head0, 1, cut-1)
  head <- substr(head0, 1, min(nchar(head0), head_chars))
  lines <- unlist(strsplit(head, "\\r?\\n"))
  noise_re <- "(?i)downloaded|accessed|printed on|creativecommons|open access|crossmark|http|https|www\\.|@|e-mail|email|issn"
  lines <- lines[!stringr::str_detect(lines, noise_re)]
  head  <- paste(lines, collapse="\n")
  y_pub <- stringr::str_match(head, "(?i)(Available online|Published(?: online| ahead of print)?|Online first)[^\\n]{0,80}?(20[0-2][0-9])")[,3]
  y_pub <- keep_range(y_pub); if (length(y_pub)) return(as.character(y_pub[1]))
  mast <- lines[stringr::str_detect(lines, "(?i)journal|volume|issue|doi|publisher|elsevier|springer|wiley|taylor|sage|nature|frontiers|agron")]
  y_mast <- keep_range(unlist(stringr::str_extract_all(mast, "20[0-2][0-9]")))
  if (length(y_mast)) return(as.character(max(y_mast)))
  y_copy <- keep_range(stringr::str_match_all(head, "(?:©|\\(c\\)|\\(C\\)|Copyright)\\s*[:]?\\s*(20[0-2][0-9])")[[1]][,2])
  if (length(y_copy)) return(as.character(y_copy[1]))
  y_doi <- keep_range(stringr::str_match(head, "/(20[0-2][0-9])[\\./]")[,2]); if (length(y_doi)) return(as.character(y_doi[1]))
  ys <- keep_range(stringr::str_extract_all(head, "\\b20[0-2][0-9]\\b")[[1]]); if (length(ys)) return(as.character(as.integer(names(sort(table(ys),decreasing=TRUE))[1])))
  y_name <- keep_range(stringr::str_match(basename(pdf_path), "(20[0-2][0-9])\\d{0,3}")[,2]); if (length(y_name)) return(as.character(y_name[1]))
  info <- tryCatch(pdftools::pdf_info(pdf_path), error=function(e) NULL)
  if (!is.null(info)) {
    y_meta <- keep_range(stringr::str_extract(paste(info$created, info$modified, collapse=" "), "\\b20[0-2][0-9]\\b"))
    if (length(y_meta)) return(as.character(y_meta[1]))
  }
  ""
}

out <- lapply(pdfs, function(p){
  txt <- pdf_to_text(p)
  tibble(File=basename(p), PublicationYear=extract_pubyear(txt, p))
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
readr::write_csv(out, file.path(OUT_DIR, "year_only.csv"))

