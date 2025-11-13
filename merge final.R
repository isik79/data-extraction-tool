source("00_paths_and_utils.R")

# helper as before (optional)
std_file_key <- function(d){
  if (is.null(d) || !nrow(d)) return(tibble::tibble(File = character()))
  nm <- names(d); cand <- nm[tolower(nm) %in% c("file","filename","pdf","pdf_file","pdfname","pdf_name")]
  if (!"File" %in% nm) {
    if (length(cand)) d <- dplyr::rename(d, File = !!rlang::sym(cand[1])) else d$File <- NA_character_
  }
  d %>% dplyr::mutate(File = as.character(File)) %>% dplyr::filter(!is.na(File))
}

country      <- readr::read_csv(file.path(OUT_DIR, "country_only.csv"),                  show_col_types=FALSE) %>% std_file_key()
year         <- readr::read_csv(file.path(OUT_DIR, "year_only.csv"),                     show_col_types=FALSE) %>% std_file_key()
study_period <- readr::read_csv(file.path(OUT_DIR, "study_period_only.csv"),             show_col_types=FALSE) %>% std_file_key()
activity     <- readr::read_csv(file.path(OUT_DIR, "activity_only.csv"),                 show_col_types=FALSE) %>% std_file_key()
itype        <- readr::read_csv(file.path(OUT_DIR, "intervention_type_only.csv"),        show_col_types=FALSE) %>% std_file_key()
aez          <- readr::read_csv(file.path(OUT_DIR, "aez_only.csv"),                      show_col_types=FALSE) %>% std_file_key()
outcomes     <- readr::read_csv(file.path(OUT_DIR, "outcome_measures_only.csv"),         show_col_types=FALSE) %>% std_file_key()
study_scales <- readr::read_csv(file.path(OUT_DIR, "study_scale_only.csv"),              show_col_types=FALSE) %>%
  dplyr::rename(StudyScale = dplyr::any_of(c("StudyScale","Study scale","study_scale","studyScale"))) %>%
  std_file_key()

tables <- list(country, year, study_period, activity, itype, aez, outcomes, study_scales)

final <- Reduce(function(x, y) dplyr::full_join(x, y, by = "File"), tables) %>%
  dplyr::select(dplyr::any_of(c(
    "File","Country","PublicationYear","StudyPeriod",
    "Intervention activity type","Type of Intervention","AEZ",
    "FoodSecurityMeasures","ClimateResilienceMeasures","StudyScale"
  ))) %>%
  add_section_flags()   # <- adds HasAbstract, HasMethods, SectionFlag

readr::write_csv(final, file.path(OUT_DIR, "min_extract_country_year_studyperiod_activity_type_aez_outcomes.csv"))
