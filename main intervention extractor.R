# =========================
# Intervention extraction v5 (cheatsheet unchanged)
# =========================
source("00_paths_and_utils.R")

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(readr); library(tibble); library(purrr)
})

# ---- helpers ----
read_intervention_cheatsheet <- function(path){
  if (!file.exists(path)) return(tibble(activity_phrase=character(), maps_to_intervention=character()))
  readr::read_delim(path, delim=";", show_col_types=FALSE,
                    locale=readr::locale(encoding="Windows-1252")) |>
    dplyr::mutate(activity_phrase=trimws(.data$activity_phrase),
                  maps_to_intervention=trimws(.data$maps_to_intervention))
}

make_regex <- function(phrases){
  phrases <- unique(phrases[nzchar(phrases)])
  if (!length(phrases)) return(NA_character_)
  esc <- escape_regex(phrases)       # from 00_paths_and_utils.R
  esc <- gsub("\\s+", "\\\\s+", esc)
  paste0("(?i)(?<![A-Za-z])(", paste(esc, collapse="|"), ")(?![A-Za-z])")
}

# negation / hedging
is_negated_nearby <- function(text, start_idx, lookback = 60, lookahead = 10){
  lb <- max(1, start_idx - lookback)
  la <- min(nchar(text), start_idx + lookahead)
  ctx <- substr(text, lb, la)
  str_detect(ctx, "(?i)\\b(no\\s+(?:evidence|signs)\\s+of|not\\b|does\\s+not\\b|didn'?t\\b|lack\\s+of|absence\\s+of)\\b")
}

find_terms <- function(text_raw, regex, min_mentions = 1){
  if (is.na(regex) || !nzchar(regex) || !nzchar(text_raw)) return(character(0))
  txt <- clean_text(text_raw)
  locs <- stringr::str_locate_all(txt, regex)[[1]]
  if (nrow(locs) == 0) return(character(0))
  keep_idx <- which(!vapply(locs[,1], function(s) is_negated_nearby(txt, s), logical(1)))
  if (!length(keep_idx)) return(character(0))
  locs <- locs[keep_idx, , drop = FALSE]
  m <- tolower(trimws(stringr::str_sub(txt, locs[,1], locs[,2])))
  if (!length(m)) return(character(0))
  tab <- table(m)
  names(tab)[tab >= min_mentions]
}

map_activities_to_interventions <- function(activities, map_df){
  if (!length(activities) || !nrow(map_df)) return(character(0))
  acts_norm <- tolower(trimws(activities))
  map_df |>
    dplyr::mutate(act_norm = tolower(trimws(.data$activity_phrase))) |>
    dplyr::filter(.data$act_norm %in% acts_norm) |>
    dplyr::pull(.data$maps_to_intervention) |>
    stringr::str_split(";") |>
    unlist() |> trimws() |> unique()
}

score_interventions <- function(acts, map_df){
  if (!length(acts) || !nrow(map_df)) return(tibble(interv=character(), score=integer()))
  acts_norm <- tolower(trimws(acts))
  tmp <- map_df |>
    dplyr::mutate(act_norm = tolower(trimws(.data$activity_phrase))) |>
    dplyr::filter(.data$act_norm %in% acts_norm) |>
    dplyr::select(act_norm, maps_to_intervention)
  if (!nrow(tmp)) return(tibble(interv=character(), score=integer()))
  ivs <- unlist(strsplit(tmp$maps_to_intervention, ";", fixed = TRUE), use.names = FALSE)
  ivs <- trimws(ivs[nzchar(ivs)])
  dplyr::as_tibble(ivs) |>
    dplyr::rename(interv = value) |>
    dplyr::count(interv, name = "score")
}

normalize_interventions <- function(vals){
  canon <- c(
    "agroforestry"="Agroforestry",
    "crop diversification"="Crop Diversification",
    "soil conservation"="Soil Conservation",
    "improved water management"="Improved Water Management",
    "improved market access"="Improved Market Access",
    "post-harvest management and storage"="Post-Harvest Management and Storage",
    "access to climate information and extension services"="Access to Climate Information and Extension Services"
  )
  x <- unlist(strsplit(sc_join(vals), ";", fixed=TRUE))
  x <- trimws(x); x <- x[nzchar(x)]
  if (!length(x)) return(NA_character_)
  low <- tolower(x)
  out <- ifelse(low %in% names(canon), unname(canon[low]), stringr::str_to_title(x))
  paste(unique(out), collapse="; ")
}

# ---- proximity/context gating ----
near <- function(a, b, k = 3){
  paste0("(?i)(?:\\b", a, "\\b(?:\\W+\\w+){0,", k, "}\\W+\\b", b, "\\b|\\b", b, "\\b(?:\\W+\\w+){0,", k, "}\\W+\\b", a, "\\b)")
}

# Access-info: require climate within ±2 tokens of info/service terms
has_access_info_context <- function(txt){
  prox <- near("climate", "(information|service[s]?|advis(or|ory)|forecast|bulletin)", k = 2)
  stringr::str_detect(txt, prox)
}

# Diversification: only strong agronomic forms
has_diversification_context <- function(txt){
  strong <- "(?i)\\b(crop\\s+rotation|intercropp?ing|relay\\s+cropp?ing|mixed\\s+cropp?ing|strip\\s+cropp?ing|push[–-]?pull)\\b"
  stringr::str_detect(txt, strong)
}

# Market access: proximity of market/value chain with access/connect/sell
has_market_access_context <- function(txt){
  left <- "(market|value\\s*chain|buyer[s]?|sales|linkage[s]?|aggregation|trader[s]?|off\\-?taker[s]?)"
  right <- "(access|linkage[s]?|connect|sell|improv(e|ing|ement)|integration)"
  stringr::str_detect(txt, near(left, right, k = 4))
}

# Water management: require clear management context (not just 'irrigation' anywhere)
has_water_mgmt_context <- function(txt){
  left <- "(irrigation|drip|sprinkler|canal|check\\s+dam|water\\s+harvesting|rainwater\\s+harvesting|farm\\s+pond|cistern|bunds?)"
  right <- "(management|scheme|system|practice|technology|technique|project|program|improv(e|ement|ing))"
  exact <- "(?i)\\bwater\\s+management\\b|\\birrigation\\s+management\\b"
  stringr::str_detect(txt, exact) || stringr::str_detect(txt, near(left, right, k = 3))
}

requires_proximity <- function(interv){
  tolower(interv) %in% c(
    "access to climate information and extension services",
    "improved market access",
    "post-harvest management and storage",
    "crop diversification",
    "improved water management"           # NEW: gate water management too
  )
}

passes_proximity <- function(interv, txt){
  i <- tolower(interv)
  if (i == "access to climate information and extension services") return(has_access_info_context(txt))
  if (i == "improved market access") return(has_market_access_context(txt))
  if (i == "post-harvest management and storage") return(has_postharvest_context(txt))  # defined below
  if (i == "crop diversification") return(has_diversification_context(txt))
  if (i == "improved water management") return(has_water_mgmt_context(txt))
  TRUE
}

# Post-harvest: only qualified forms (kept from v4)
has_postharvest_context <- function(txt){
  pat <- paste(
    "\\bpost[- ]harvest\\b",
    near("(hermetic|metal|grain|on[- ]farm)", "storage"),
    near("(grain|solar|crop)", "drying"),
    near("(grain|metal|hermetic)", "silo[s]?"),
    sep="|"
  )
  stringr::str_detect(txt, pat)
}

# ---- run ----
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

cs <- read_intervention_cheatsheet(CHEATSHEET_INTERVENTIONS)
activity_regex <- make_regex(cs$activity_phrase)

out <- lapply(pdfs, function(p){
  txt_raw <- pdf_to_text(p)
  txt <- clean_text(txt_raw)
  
  # high-value sections
  wins <- get_prioritized_windows(txt_raw)
  high <- ""
  if (length(wins)) {
    types <- sapply(wins, `[[`, "type")
    texts <- sapply(wins, `[[`, "text")
    high <- paste(texts[types %in% c("abstract","methods")], collapse = " ")
  }
  high_clean <- clean_text(high)
  
  # adaptive threshold by doc length
  n_tok <- length(strsplit(txt, "\\s+")[[1]])
  mm_full <- if (n_tok > 40000) 3 else if (n_tok > 20000) 2 else 1
  
  # detect terms
  acts_high <- find_terms(high_clean, activity_regex, min_mentions = 1)
  acts_full <- find_terms(txt,        activity_regex, min_mentions = mm_full)
  
  # map activities -> interventions
  mapped_high <- map_activities_to_interventions(acts_high, cs)
  mapped_full <- map_activities_to_interventions(acts_full, cs)
  
  # proximity gating
  gate <- function(iv, tclean){
    if (!length(iv)) return(character(0))
    iv[ vapply(iv, passes_proximity, logical(1), txt = tclean) ]
  }
  mapped_high_g <- gate(mapped_high, high_clean)
  mapped_full_g <- gate(mapped_full, txt)
  
  # scoring
  sc_full <- score_interventions(acts_full, cs)
  sc_high <- score_interventions(acts_high, cs)
  score <- dplyr::full_join(sc_full, sc_high, by="interv", suffix=c("_f","_h")) |>
    dplyr::mutate(score_f = coalesce(score_f, 0L),
                  score_h = coalesce(score_h, 0L),
                  tot = score_f + 1.5*score_h)
  
  get_score_f <- function(iv, score_tbl){
    s <- score_tbl$score_f[match(iv, score_tbl$interv)]
    ifelse(is.na(s), 0L, s)
  }
  
  # stricter full-text inclusion unless in high
  # Access-info & Water Mgmt must have >=2 full-text mentions if not in high
  if ("Access to Climate Information and Extension Services" %in% mapped_full_g) {
    if (get_score_f("Access to Climate Information and Extension Services", score) < 2L &&
        !("Access to Climate Information and Extension Services" %in% mapped_high_g)) {
      mapped_full_g <- setdiff(mapped_full_g, "Access to Climate Information and Extension Services")
    }
  }
  if ("Improved Water Management" %in% mapped_full_g) {
    if (get_score_f("Improved Water Management", score) < 2L &&
        !("Improved Water Management" %in% mapped_high_g)) {
      mapped_full_g <- setdiff(mapped_full_g, "Improved Water Management")
    }
  }
  
  # --- gentle recall boosts (do NOT bypass cheatsheet: only if mapped) ---
  # Always allow Agroforestry & Soil Conservation through cap if they appear at least once in full or high
  recall_boost <- intersect(c("Agroforestry","Soil Conservation"), unique(c(mapped_high_g, mapped_full_g)))
  
  # cap logic
  cap_base <- 4
  keep_n <- max(cap_base, length(mapped_high_g))
  
  all_mapped <- unique(c(mapped_high_g, mapped_full_g))
  if (length(all_mapped) > keep_n) {
    order_full <- score$interv[order(-score$tot)]
    strict_min <- if (length(mapped_high_g) >= 2) 2L else 1L
    
    is_broad <- function(iv) tolower(iv) %in% c(
      "crop diversification",
      "access to climate information and extension services",
      "improved market access",
      "post-harvest management and storage",
      "improved water management"
    )
    
    strong_full <- score$interv[(score$score_f >= strict_min) | !is_broad(score$interv)]
    
    # RELAX: keep Market Access if exact phrase present
    if ("Improved Market Access" %in% mapped_full_g &&
        stringr::str_detect(txt, "(?i)\\bmarket\\s+access\\b")) {
      strong_full <- union(strong_full, "Improved Market Access")
    }
    
    # Always union recall boosts
    strong_full <- union(strong_full, recall_boost)
    
    ordered <- c(mapped_high_g,
                 setdiff(order_full, mapped_high_g),
                 setdiff(mapped_full_g, c(mapped_high_g, order_full)))
    ordered <- intersect(ordered, unique(c(mapped_high_g, strong_full)))
    chosen <- head(unique(ordered), keep_n)
  } else {
    chosen <- all_mapped
  }
  
  tibble(File = basename(p),
         `Type of Intervention` = normalize_interventions(chosen))
}) |> dplyr::bind_rows()

# outputs
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
readr::write_csv(out, file.path(OUT_DIR, "intervention_type_only.csv"))
