# 70_study_scale_extraction.R  — returns ONE dominant scale per paper
source("00_paths_and_utils.R")

extract_study_scale <- function(text_raw) {
  if (nchar(trimws(text_raw)) < 50) return(NA_character_)
  wins <- get_prioritized_windows(text_raw); if (!length(wins)) return(NA_character_)
  
  # scale terms
  scale_patterns <- list(
    plot = c("plot","plots","plot level","plot-level","experimental plot","field plot",
             "research plot","test plot","plot scale","plot-scale","subplot","sub-plot"),
    field = c("field","fields","field level","field-level","field scale","field-scale",
              "farm field","agricultural field","field study","field trial","field experiment"),
    farm = c("farm","farms","farm level","farm-level","farm scale","farm-scale","household","households",
             "farm household","farming household","smallholder","smallholders","farmer","farmers",
             "farming system","farming systems","on-farm","on farm","farm-based","farm based"),
    community = c("community","communities","village","villages","community level","community-level",
                  "village level","village-level","local community","rural community","farming community",
                  "community scale","community-scale"),
    district = c("district","districts","regional","region","subnational","sub-national","administrative",
                 "county","counties","province","provinces","state","states","district level","district-level",
                 "regional level","regional-level"),
    multi_site = c("multi-site","multisite","multi site","multiple sites","multiple locations","multiple regions",
                   "multi-location","multilocation","multi location","cross-site","crosssite","cross site",
                   "multiple countries","multi-country","multicountry","multi country","mixed-site","mixedsite","mixed site")
  )
  
  # map + hierarchy (lowest -> highest)
  label_map <- c(plot="Plot", field="Field", farm="Farm",
                 community="Community", district="District", "multi_site"="Multi/Mixed-site")
  rank <- c(Plot=1, Field=2, Farm=3, Community=4, District=5, "Multi/Mixed-site"=6)
  
  make_scale_regex <- function(terms){
    terms_escaped <- escape_regex(terms)
    terms_spaced  <- gsub(" ", "\\\\s+", terms_escaped)
    paste0("(?i)\\b(", paste(terms_spaced, collapse="|"), ")\\b")
  }
  
  # contextual cues (reduces false positives)
  ctx <- c(
    "(?i)\\b(?:study|research|experiment|trial|survey|analysis|investigation|conducted|carried out|performed)\\s+(?:at|on|across|within)\\s+(?:the\\s+)?",
    "(?i)\\b(?:scale|level|basis)\\s+(?:of|was|were|is|are)\\s+",
    "(?i)\\b(?:data|information|measurements)\\s+(?:collected|gathered|obtained)\\s+(?:at|from|on)\\s+(?:the\\s+)?"
  )
  
  found <- character(0)
  
  for (w in wins) {
    if (!(w$type %in% c("abstract","methods"))) next
    chunk <- clean_text(w$text)
    if (w$type == "methods") chunk <- substr(chunk, 1, min(nchar(chunk), 2000))
    
    # detect scales
    for (nm in names(scale_patterns)) {
      rx <- make_scale_regex(scale_patterns[[nm]])
      if (stringr::str_detect(chunk, rx)) found <- c(found, nm)
      for (cpat in ctx) if (stringr::str_detect(chunk, paste0(cpat, rx))) found <- c(found, nm)
    }
    
    # mixed-site cues
    multi_ind <- c(
      "(?i)\\b(?:both|multiple|different|various)\\s+(?:scales|levels)\\b",
      "(?i)\\b(?:plot|farm|community|district)\\s+(?:and|to)\\s+(?:farm|community|district|regional)\\b",
      "(?i)\\b(?:across|spanning|covering)\\s+(?:multiple|different|various)\\s+(?:sites|locations|regions|areas)\\b"
    )
    for (mp in multi_ind) if (stringr::str_detect(chunk, mp)) found <- c(found, "multi_site")
    
    if (length(found)) break  # stop after first priority window with hits
  }
  
  if (!length(found)) return(NA_character_)
  
  # map to labels, keep only the highest by rank
  labs <- unique(label_map[found]); labs <- labs[!is.na(labs)]
  if (!length(labs)) return(NA_character_)
  labs[which.max(rank[labs])]
}

# ---- run over PDFs and save ----
scale_results <- tibble(File = character(), StudyScale = character())

for (pdf_path in pdfs) {
  file_name <- basename(pdf_path)
  txt_raw <- pdf_to_text(pdf_path)
  top_scale <- if (nchar(trimws(txt_raw)) >= 50) extract_study_scale(txt_raw) else NA_character_
  scale_results <- dplyr::add_row(scale_results, File = file_name, StudyScale = top_scale)
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

scale_results <- normalize_for_write(scale_results)
readr::write_csv(scale_results, file.path(OUT_DIR, "study_scale_only.csv"))

