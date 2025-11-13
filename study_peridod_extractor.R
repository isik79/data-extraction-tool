# 50_study_period_extraction.R
source("00_paths_and_utils.R")

extract_study_period <- function(text_raw) {
  if (nchar(trimws(text_raw)) < 50) return(character(0))
  
  wins <- get_prioritized_windows(text_raw)
  if (!length(wins)) return(character(0))
  
  # Only process abstract and methods sections
  for (w in wins) {
    if (!(w$type %in% c("abstract", "methods"))) next
    
    chunk <- clean_text(w$text)
    found_periods <- character(0)
    
    # For methods: focus on first part (2000 chars like country extractor)
    if (w$type == "methods") {
      chunk <- substr(chunk, 1, min(nchar(chunk), 2000))
    }
    
    # Pattern 1: Range with words "from YYYY to YYYY", "between YYYY and YYYY"
    pattern1 <- "(?i)\\b(?:from|between)\\s+(19[8-9][0-9]|20[0-2][0-9])\\s+(?:to|and|through|until|-|–)\\s+(19[8-9][0-9]|20[0-2][0-9])\\b"
    matches1 <- stringr::str_match_all(chunk, pattern1)[[1]]
    if (nrow(matches1) > 0) {
      for (i in seq_len(nrow(matches1))) {
        start_year <- as.numeric(matches1[i, 2])
        end_year <- as.numeric(matches1[i, 3])
        if (!is.na(start_year) && !is.na(end_year) && start_year <= end_year && (end_year - start_year) <= 15) {
          found_periods <- c(found_periods, paste(start_year, end_year, sep="-"))
        }
      }
    }
    
    # Pattern 2: Range with dash "YYYY-YYYY", "YYYY–YYYY"
    pattern2 <- "\\b(19[8-9][0-9]|20[0-2][0-9])\\s*[-–—]\\s*(19[8-9][0-9]|20[0-2][0-9])\\b"
    matches2 <- stringr::str_match_all(chunk, pattern2)[[1]]
    if (nrow(matches2) > 0) {
      for (i in seq_len(nrow(matches2))) {
        start_year <- as.numeric(matches2[i, 2])
        end_year <- as.numeric(matches2[i, 3])
        if (!is.na(start_year) && !is.na(end_year) && start_year <= end_year && (end_year - start_year) <= 15) {
          found_periods <- c(found_periods, paste(start_year, end_year, sep="-"))
        }
      }
    }
    
    # Pattern 3: Range with slash "YYYY/YY"
    pattern3 <- "\\b(19[8-9][0-9]|20[0-2][0-9])/(\\d{2})\\b"
    matches3 <- stringr::str_match_all(chunk, pattern3)[[1]]
    if (nrow(matches3) > 0) {
      for (i in seq_len(nrow(matches3))) {
        start_year <- as.numeric(matches3[i, 2])
        end_suffix <- as.numeric(matches3[i, 3])
        if (!is.na(start_year) && !is.na(end_suffix)) {
          start_century <- floor(start_year / 100) * 100
          end_year <- start_century + end_suffix
          if (end_year < start_year) end_year <- end_year + 100
          if (start_year <= end_year && (end_year - start_year) <= 15) {
            found_periods <- c(found_periods, paste(start_year, end_year, sep="-"))
          }
        }
      }
    }
    
    # Pattern 4: Years with temporal context (similar to country extractor's verb+prep approach)
    temporal_verbs <- "(?i)\\b(conducted|carried out|implemented|performed|undertaken|executed|lasted|spanned|covered|took place|occurred|ran|collected|sampled|monitored|measured|observed)\\b"
    temporal_preps <- "(?i)\\b(during|from|between|throughout|over|in|across)\\b"
    study_terms <- "(?i)\\b(study|research|experiment|trial|survey|fieldwork|data collection|field work|sampling|monitoring|observation|measurement)\\b"
    
    # Split into sentences and look for relevant ones
    sents <- unlist(strsplit(chunk, "(?<=[.!?])\\s+", perl=TRUE))
    relevant_sents <- sents[stringr::str_detect(sents, temporal_verbs) & 
                              stringr::str_detect(sents, temporal_preps) |
                              stringr::str_detect(sents, study_terms)]
    
    if (length(relevant_sents) > 0) {
      search_text <- paste(relevant_sents, collapse=" ")
      
      # Look for years in these relevant sentences
      year_pattern <- "\\b(19[8-9][0-9]|20[0-2][0-9])\\b"
      context_years <- stringr::str_extract_all(search_text, year_pattern)[[1]]
      context_years <- as.numeric(context_years)
      context_years <- unique(context_years[!is.na(context_years) & context_years >= 1980 & context_years <= 2025])
      
      if (length(context_years) > 0) {
        # If multiple years, create range; if single year, add as is
        if (length(context_years) > 1) {
          context_years <- sort(context_years)
          year_diff <- max(context_years) - min(context_years)
          if (year_diff <= 15 && year_diff >= 1) {
            found_periods <- c(found_periods, paste(min(context_years), max(context_years), sep="-"))
          } else if (year_diff == 0 || length(context_years) <= 3) {
            # Add individual years if they're the same or just a few different years
            for (year in context_years) {
              found_periods <- c(found_periods, as.character(year))
            }
          }
        } else {
          found_periods <- c(found_periods, as.character(context_years[1]))
        }
      }
    }
    
    # Pattern 5: Multiple consecutive years "2015, 2016, 2017"
    multi_year_pattern <- "\\b(19[8-9][0-9]|20[0-2][0-9])(?:\\s*,\\s*(19[8-9][0-9]|20[0-2][0-9]))+\\b"
    matches5 <- stringr::str_match_all(chunk, multi_year_pattern)[[1]]
    if (nrow(matches5) > 0) {
      for (i in seq_len(nrow(matches5))) {
        year_sequence <- matches5[i, 1]
        years <- as.numeric(stringr::str_extract_all(year_sequence, "\\b(19[8-9][0-9]|20[0-2][0-9])\\b")[[1]])
        years <- sort(unique(years[!is.na(years)]))
        if (length(years) >= 2 && (max(years) - min(years)) <= 15) {
          found_periods <- c(found_periods, paste(min(years), max(years), sep="-"))
        }
      }
    }
    
    # If we found periods in this section, return them
    if (length(found_periods) > 0) {
      return(unique(found_periods))
    }
  }
  
  character(0)
}

out <- lapply(pdfs, function(p){
  txt <- pdf_to_text(p)
  tibble(File=basename(p), StudyPeriod=sc_join(extract_study_period(txt)))
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
readr::write_csv(out, file.path(OUT_DIR, "study_period_only.csv"))
