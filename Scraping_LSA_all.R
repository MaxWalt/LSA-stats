###############################################################################
#                                                                             #
#                         Extractor Alabus for LSA data                       #
#                         -----------------------------                       #      
#                                                                             #
#  1. Create URLs                                                             #
#  2. Alabus scraping: complete pipeline                                      #
#                                                                             #  
#                                                                             # 
#                                                                             #
#                                                 Maxime Walt, 2025           #
#                                                                             #  
###############################################################################

#=== 0. Load mandatory materials

# 0.1. Load libraries

library(tidyr)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(glue)
library(fs)
library(openxlsx)
library(readxl)

# 0.2. Load CSV with events and category codes

events_raw <- read_delim("events_df_from_excel.csv", delim = ";", show_col_types = FALSE)
categories_raw <- read_delim("categories_df_from_excel.csv", delim = ";", show_col_types = FALSE)

#=== 1. Create URLs

# 1.1. Clean the event df

events_df <- events_raw %>%
  select(Event, Type, disci_code) %>%
  distinct(Event, Type, disci_code) %>%
  arrange(Event, Type, disci_code)

# 1.2. Clean the category df

categories_df <- categories_raw %>%
  select(Category, Gender, blcat_code) %>%
  distinct(Category, Gender, blcat_code) %>%
  arrange(Gender, Category, blcat_code)

# 1.3. Compute a function to build URLs

build_alabus_urls <- function(events_df,
                              categories_df,
                              start_year = 2006,
                              end_year   = 2025) {
  
  years <- seq.int(start_year, end_year)
  
  # On crée un petit mapping indoor/outdoor
  indoor_map <- tibble(
    indoor = c(TRUE, FALSE)
  )
  
  # Produit cartésien : années × catégories × events × indoor
  df_full <- crossing(
    blyear       = years,
    categories_df,
    events_df,
    indoor_map
  )
  
  # Construction des URLs
  df_full <- df_full %>%
    mutate(
      # URL seulement si on a les deux codes
      url = if_else(
        is.na(blcat_code) | is.na(disci_code),
        NA_character_,
        glue(
          "https://alabus.swiss-athletics.ch/satweb/faces/bestlist.xhtml",
          "?lang=fr",
          "&blcat={blcat_code}",
          "&blyear={blyear}",
          "&disci={disci_code}",
          "&indoor={tolower(as.character(indoor))}",
          "&top=5000"
        )
      ),
      # Indicateur de complétude des tags
      missing_code = case_when(
        is.na(disci_code) & is.na(blcat_code) ~ "missing_disci_and_blcat",
        is.na(disci_code)                     ~ "missing_disci",
        is.na(blcat_code)                     ~ "missing_blcat",
        TRUE                                  ~ "ok"
      )
    ) %>%
    arrange(blyear, Gender, Category, Event, Type, indoor)
  
  df_full
}

# Apply function

all_urls <- build_alabus_urls(
  events_df     = events_df,
  categories_df = categories_df,
  start_year    = 2006,
  end_year      = 2025
)

# Quick view 
all_urls %>% count(missing_code)
all_urls %>% filter(missing_code != "ok")   # où il manque encore des tags
all_urls %>% filter(missing_code == "ok")   # toutes les URLs prêtes à scraper

# Save
write_csv(all_urls, "alabus_all_urls_2006_2025.csv")

#=== 2. Alabus scraping: complete pipeline

# 2.1. Check URLs and events

urls_all <- all_urls %>%
  dplyr::filter(
    missing_code == "ok",
    !is.na(url),
    url != ""
  ) %>%
  dplyr::select(blyear, Gender, Category, Event, Type, indoor, url)

# 2.2. Scraping helpers

# Conversion prudente des performances en secondes
safe_mark_to_seconds <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(NA_real_)
  
  # Si tu as déjà une fonction mark_to_seconds() dans ton environnement, on la privilégie
  if (exists("mark_to_seconds", mode = "function")) {
    val <- suppressWarnings(try(mark_to_seconds(x), silent = TRUE))
    if (!inherits(val, "try-error") && !is.na(val)) {
      return(as.numeric(val))
    }
  }
  
  # Format mm:ss(.xx)
  if (grepl("^\\d+:\\d{1,2}(\\.\\d+)?$", x)) {
    parts <- strsplit(x, ":")[[1]]
    mm <- as.numeric(parts[1])
    ss <- as.numeric(parts[2])
    return(mm * 60 + ss)
  }
  
  # Format hh:mm:ss(.xx)
  if (grepl("^\\d+:\\d{1,2}:\\d{1,2}(\\.\\d+)?$", x)) {
    parts <- strsplit(x, ":")[[1]]
    hh <- as.numeric(parts[1])
    mm <- as.numeric(parts[2])
    ss <- as.numeric(parts[3])
    return(hh * 3600 + mm * 60 + ss)
  }
  
  # Nombre brut (distances, sauts, lancers)
  num <- suppressWarnings(as.numeric(gsub(",", ".", x)))
  if (!is.na(num)) return(num)
  
  NA_real_
}

# Si aucune mark_to_seconds() n'est définie, on en crée une simple
if (!exists("mark_to_seconds", mode = "function")) {
  mark_to_seconds <- function(x) safe_mark_to_seconds(x)
}

# Nom de fichier / nom de dossier "safe"
safe_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^0-9a-z]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# Tag d'années pour les fichiers (ex: "06_25")
year_tag_from_df <- function(df) {
  if (!"season" %in% names(df)) return("unknown")
  yrs <- df$season[!is.na(df$season)]
  if (!length(yrs)) return("unknown")
  rng <- range(yrs)
  sprintf("%02d_%02d", rng[1] %% 100, rng[2] %% 100)
}

# Fallback de nettoyage (identité) si tu n'as pas de clean_alabus_table()
if (!exists("clean_alabus_table", mode = "function")) {
  clean_alabus_table <- function(df) df
}

# Catégorie club à partir de l'âge (logique strictement <)
age_to_cat <- function(age) {
  if (is.na(age)) return(NA_character_)
  if (age < 10)  return("U10")
  if (age < 12)  return("U12")
  if (age < 14)  return("U14")
  if (age < 16)  return("U16")
  if (age < 18)  return("U18")
  if (age < 20)  return("U20")
  if (age < 23)  return("U23")
  if (age >= 23) return("SEN")
  NA_character_
}

# 2.3. Flag which URLs are full/empty

flag_alabus_urls_with_data <- function(url_df,
                                       polite_delay = 0.0,
                                       gc_every     = 100L) {
  if (is.null(url_df) || nrow(url_df) == 0) {
    stop("url_df est vide ou NULL.")
  }
  if (!"url" %in% names(url_df)) {
    stop("url_df doit contenir une colonne 'url'.")
  }
  
  n <- nrow(url_df)
  has_data <- logical(n)
  
  # Message "vide" (avec fautes incluses : 'séelctionner', etc.)
  empty_pattern <- "(?i)veuillez s.?elctionner une cat|veuillez s.?electionner une cat|aucun r.?sultat|pas de r.?sultat"
  
  for (i in seq_len(n)) {
    page_url <- url_df$url[i]
    
    if (is.na(page_url) || page_url == "") {
      has_data[i] <- FALSE
      next
    }
    
    message(sprintf("[check %s/%s] %s", i, n, page_url))
    
    ok <- FALSE
    
    try({
      html <- rvest::read_html(page_url)
      
      # Conteneur global de la table (xpath validé sur tes tests)
      node <- rvest::html_node(html, xpath = '//*[@id="form_anonym:j_idt87"]')
      if (!inherits(node, "xml_missing")) {
        tab <- rvest::html_table(node, fill = TRUE)
        
        if (!is.null(tab) && nrow(tab) > 0) {
          first_cell <- as.character(tab[1, 1])
          if (!is.na(first_cell) &&
              grepl(empty_pattern, first_cell, perl = TRUE)) {
            ok <- FALSE
          } else {
            ok <- TRUE
          }
        }
      }
      
      rm(html)
    }, silent = TRUE)
    
    has_data[i] <- ok
    
    if (polite_delay > 0) Sys.sleep(polite_delay)
    if (i %% gc_every == 0) {
      gc()
      closeAllConnections()
    }
  }
  
  url_df$has_data <- has_data
  url_df
}

# 2.4. Scraper

scrape_alabus_toplists <- function(url_df,
                                   polite_delay = 0.0,
                                   gc_every     = 75L) {
  if (is.null(url_df) || nrow(url_df) == 0) return(tibble::tibble())
  if (!"url" %in% names(url_df)) stop("url_df must contain a column named 'url'.")
  
  n <- nrow(url_df)
  results <- vector("list", n)
  
  for (i in seq_len(n)) {
    page_url <- url_df$url[i]
    if (is.na(page_url) || page_url == "") next
    
    message(sprintf("[%s/%s] %s", i, n, page_url))
    
    tbl <- tryCatch({
      html <- rvest::read_html(page_url)
      
      # Conteneur global de la table
      node <- rvest::html_node(html, xpath = '//*[@id="form_anonym:j_idt87"]')
      if (inherits(node, "xml_missing")) return(NULL)
      
      out <- rvest::html_table(node, fill = TRUE)
      if (is.null(out) || !nrow(out)) return(NULL)
      
      # 1) normaliser les noms de colonnes
      nm <- names(out)
      nm[is.na(nm) | nm == ""] <- paste0("V", which(is.na(nm) | nm == ""))
      names(out) <- nm
      
      # 2) forcer toutes les colonnes brutes en character
      out[] <- lapply(out, as.character)
      
      # 3) standardiser la colonne club / société
      club_idx <- which(stringr::str_detect(
        tolower(names(out)),
        "soci.t.|societe|club"
      ))[1]
      if (!is.na(club_idx)) {
        names(out)[club_idx] <- "Societe"
      }
      
      # 4) gérer la colonne Vent
      # repérage colonne Résultat/Perf/Time
      res_idx <- which(stringr::str_detect(
        tolower(names(out)), "résultat|resultat|perf|mark|time"
      ))[1]
      if (is.na(res_idx)) res_idx <- 2L  # fallback grossier
      
      # repérage Vent éventuel
      vent_idx <- which(stringr::str_detect(
        tolower(names(out)), "^vent$|^vent "
      ))[1]
      
      if (!is.na(vent_idx)) {
        names(out)[vent_idx] <- "Vent"
        # si Vent n'est pas juste après Résultat, on réordonne
        if (vent_idx != (res_idx + 1L)) {
          cols_order <- c(
            seq_len(res_idx),
            vent_idx,
            setdiff(seq_len(ncol(out)), c(seq_len(res_idx), vent_idx))
          )
          out <- out[, cols_order]
        }
      } else {
        # on insère une colonne Vent vide juste après Résultat
        out <- tibble::add_column(out,
                                  Vent = NA_character_,
                                  .after = res_idx)
      }
      
      # 5) année de naissance (yob)
      if ("Date naiss." %in% names(out)) {
        out$yob <- suppressWarnings(
          as.integer(sub(".*(\\d{4})$", "\\1", out[["Date naiss."]]))
        )
      } else {
        yob_col <- names(out)[
          stringr::str_detect(tolower(names(out)),
                              "jahrgang|jg|year|naissance|geb")
        ][1]
        if (!is.na(yob_col) && yob_col %in% names(out)) {
          out$yob <- suppressWarnings(
            as.integer(stringr::str_extract(out[[yob_col]], "\\d{4}"))
          )
        } else {
          out$yob <- NA_integer_
        }
      }
      
      # 6) Méta-infos issues de url_df
      season     <- suppressWarnings(as.integer(url_df$blyear[i]))
      gender     <- url_df$Gender[i]
      category   <- url_df$Category[i]
      event_name <- url_df$Event[i]
      event_type <- url_df$Type[i]
      indoor     <- url_df$indoor[i]
      
      # 7) calcul de l’âge et catégorie club
      out$season       <- season
      out$gender       <- gender
      out$event_name   <- event_name
      out$event_type   <- event_type
      out$category_raw <- category
      out$indoor       <- indoor
      
      out$age <- dplyr::if_else(
        is.na(out$yob) | is.na(out$season),
        NA_integer_,
        as.integer(out$season) - as.integer(out$yob)
      )
      out$club_cat <- vapply(out$age, age_to_cat, character(1))
      
      out
    }, error = function(e) NULL)
    
    if (!is.null(tbl) && nrow(tbl)) {
      results[[i]] <- tbl
    }
    
    if (polite_delay > 0) Sys.sleep(polite_delay)
    if (i %% gc_every == 0) {
      gc()
      closeAllConnections()
    }
  }
  
  results <- purrr::compact(results)
  if (!length(results)) return(tibble::tibble())
  
  out <- dplyr::bind_rows(results)
  out <- suppressMessages(readr::type_convert(out, na = c("", "NA", "N/A")))
  
  if ("gender"       %in% names(out)) out$gender       <- as.character(out$gender)
  if ("event_name"   %in% names(out)) out$event_name   <- as.character(out$event_name)
  if ("category_raw" %in% names(out)) out$category_raw <- as.character(out$category_raw)
  if ("Societe"      %in% names(out)) out$Societe      <- as.character(out$Societe)
  
  out
}

# 2.5. Split by gender, catergory and indoor

split_event_data <- function(event_df,
                             event_name,
                             out_dir  = "data/alabus_split",
                             year_tag = "unknown") {
  fs::dir_create(out_dir)
  
  if (!"gender"       %in% names(event_df) ||
      !"category_raw" %in% names(event_df) ||
      !"indoor"       %in% names(event_df)) {
    stop("event_df doit contenir les colonnes 'gender', 'category_raw' et 'indoor'.")
  }
  
  ev_safe <- safe_name(event_name)
  out_dir_ev <- file.path(out_dir, ev_safe)
  fs::dir_create(out_dir_ev)
  
  groups <- event_df %>%
    dplyr::filter(!is.na(gender),
                  !is.na(category_raw),
                  !is.na(indoor)) %>%
    dplyr::distinct(gender, category_raw, indoor)
  
  if (nrow(groups) == 0) {
    message(">>> Aucun groupe (gender × category_raw × indoor) trouvé pour ", event_name)
    return(invisible(NULL))
  }
  
  for (i in seq_len(nrow(groups))) {
    g   <- groups$gender[i]
    cat <- groups$category_raw[i]
    ind <- groups$indoor[i]
    
    sub_df <- event_df %>%
      dplyr::filter(gender == g,
                    category_raw == cat,
                    indoor == ind)
    
    if (!nrow(sub_df)) next
    
    cat_safe   <- safe_name(cat)
    gen_safe   <- safe_name(g)
    ind_label  <- if (isTRUE(ind)) "indoor" else "outdoor"
    
    out_file <- file.path(
      out_dir_ev,
      glue::glue("CH_{ev_safe}_{cat_safe}_{gen_safe}_{ind_label}_{year_tag}.rds")
    )
    
    saveRDS(sub_df, out_file)
    message(glue::glue(">>> Sous-df sauvegardé : {out_file} (n = {nrow(sub_df)} lignes)"))
  }
  
  invisible(NULL)
}

# 2.6. Split by club LSA

split_event_data_ls <- function(event_df,
                                event_name,
                                club_name = "Lausanne-Sports Athlétisme",
                                out_dir   = "data/alabus_ls",
                                year_tag  = "unknown") {
  fs::dir_create(out_dir)
  
  ev_safe    <- safe_name(event_name)
  out_dir_ev <- file.path(out_dir, ev_safe)
  fs::dir_create(out_dir_ev)
  
  if (!"Societe" %in% names(event_df)) {
    message(">>> Pas de colonne 'Societe' dans event_df pour ", event_name)
    return(invisible(NULL))
  }
  
  df_ls <- event_df %>% dplyr::filter(Societe == club_name)
  
  if (!nrow(df_ls)) {
    message(">>> Aucun résultat pour le club ", club_name,
            " sur l'épreuve ", event_name)
    return(invisible(NULL))
  }
  
  # 1) Sauvegarde globale LS pour cette épreuve
  file_all <- file.path(
    out_dir_ev,
    glue::glue("CH_{ev_safe}_ls_{year_tag}.rds")
  )
  saveRDS(df_ls, file_all)
  message(">>> LS global sauvegardé : ", file_all,
          " (n = ", nrow(df_ls), " lignes)")
  
  # 2) Split LS par gender / club_cat / indoor
  if (!all(c("gender", "club_cat", "indoor") %in% names(df_ls))) {
    message(">>> Colonnes 'gender', 'club_cat' ou 'indoor' manquantes pour le split LS de ", event_name)
    return(invisible(NULL))
  }
  
  groups <- df_ls %>%
    dplyr::filter(!is.na(gender),
                  !is.na(club_cat),
                  !is.na(indoor)) %>%
    dplyr::distinct(gender, club_cat, indoor)
  
  if (nrow(groups) == 0) {
    message(">>> Aucun groupe LS (gender × club_cat × indoor) pour ", event_name)
    return(invisible(NULL))
  }
  
  for (i in seq_len(nrow(groups))) {
    g   <- groups$gender[i]
    cc  <- groups$club_cat[i]
    ind <- groups$indoor[i]
    
    sub_df <- df_ls %>%
      dplyr::filter(gender == g,
                    club_cat == cc,
                    indoor == ind)
    
    if (!nrow(sub_df)) next
    
    cc_safe   <- safe_name(cc)
    gen_safe  <- safe_name(g)
    ind_label <- if (isTRUE(ind)) "indoor" else "outdoor"
    
    out_file <- file.path(
      out_dir_ev,
      glue::glue("CH_{ev_safe}_{cc_safe}_{gen_safe}_{ind_label}_ls_{year_tag}.rds")
    )
    
    saveRDS(sub_df, out_file)
    message(glue::glue(
      ">>> Sous-df LS sauvegardé : {out_file} (n = {nrow(sub_df)} lignes)"
    ))
  }
  
  invisible(NULL)
}

# 2.7. Wrapper

run_one_event <- function(urls_all,
                          event_name,
                          indoor       = c(TRUE, FALSE),
                          genders      = NULL,
                          categories   = NULL,  # si NULL -> allW/allM par défaut
                          out_dir      = "data/alabus_raw",
                          split        = FALSE, # split tous clubs
                          split_dir    = "data/alabus_split",
                          polite_delay = 0.2,
                          overwrite    = FALSE,
                          split_ls     = FALSE, # split spécifique LS
                          ls_dir       = "data/alabus_ls",
                          club_name    = "Lausanne-Sports Athlétisme") {
  fs::dir_create(out_dir)
  
  df_ev <- urls_all %>%
    dplyr::filter(Event == event_name,
                  indoor %in% indoor)
  
  # Par défaut, on garde uniquement allW / allM
  if (is.null(categories)) {
    categories <- c("allW", "allM")
  }
  df_ev <- df_ev %>% dplyr::filter(Category %in% categories)
  
  if (!is.null(genders)) {
    df_ev <- df_ev %>% dplyr::filter(Gender %in% genders)
  }
  
  if (nrow(df_ev) == 0) {
    message("Aucune URL pour l'épreuve: ", event_name)
    return(invisible(NULL))
  }
  
  # 1) Flag
  message("=== Flag URLs with data for ", event_name, " ===")
  df_ev_flag <- flag_alabus_urls_with_data(df_ev, polite_delay = polite_delay)
  df_ev_ok   <- df_ev_flag %>% dplyr::filter(has_data)
  
  if (nrow(df_ev_ok) == 0) {
    message(">>> Aucune URL avec table valide pour ", event_name)
    return(invisible(NULL))
  }
  
  ev_safe       <- safe_name(event_name)
  out_dir_event <- file.path(out_dir, ev_safe)
  fs::dir_create(out_dir_event)
  
  # 2) Scraping
  message("=== Scraping ", event_name,
          " (", nrow(df_ev_ok), " URLs valides) ===")
  
  res <- scrape_alabus_toplists(df_ev_ok, polite_delay = polite_delay)
  if (is.null(res) || !nrow(res)) {
    message(">>> Aucun résultat pour ", event_name)
    return(invisible(NULL))
  }
  
  # 3) Mark_seconds avec safe_mark_to_seconds
  perf_col <- names(res)[
    stringr::str_detect(tolower(names(res)),
                        "perf|résultat|resultat|mark|time")
  ][1]
  
  if (!is.na(perf_col)) {
    res[[perf_col]] <- as.character(res[[perf_col]])
    res$Mark_seconds <- vapply(res[[perf_col]],
                               safe_mark_to_seconds,
                               numeric(1))
  } else {
    res$Mark_seconds <- NA_real_
  }
  
  res_clean <- clean_alabus_table(res)
  
  # 4) Sauvegarde globale (master, tous clubs)
  year_tag <- year_tag_from_df(res_clean)
  out_file <- file.path(
    out_dir_event,
    glue::glue("CH_{ev_safe}_{year_tag}.rds")
  )
  
  if (fs::file_exists(out_file) && !overwrite) {
    message(">>> Fichier existe déjà : ", out_file)
  } else {
    saveRDS(res_clean, out_file)
    message(">>> Sauvegardé : ", out_file, " (n = ", nrow(res_clean), " lignes)")
  }
  
  # 5) Split générique tous clubs (optionnel)
  if (split) {
    split_event_data(res_clean, event_name,
                     out_dir  = split_dir,
                     year_tag = year_tag)
  }
  
  # 6) Split spécifique Lausanne-Sports (optionnel)
  if (split_ls) {
    split_event_data_ls(res_clean, event_name,
                        club_name = club_name,
                        out_dir   = ls_dir,
                        year_tag  = year_tag)
  }
  
  invisible(res_clean)
}

# 2.8. Application

run_one_event(urls_all, "100", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "1000", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "10000", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "100H_76.2", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "100H_84", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "100H_91.4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "110H_106.7", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE) 
run_one_event(urls_all, "110H_91.4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "110H_99.1", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "150", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "1500", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "1500SC", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "1609", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "200", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "2000", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "20000", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "2000SC", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "200H", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "300", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "3000", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "3000SC", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "300H_76.2", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "300H_84", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "300H_91.4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "3x1000", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "3x600", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "3x800", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "400", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "400H_76.2", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "400H_91.4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "4x100", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "4x1500", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "4x200", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "4x400", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "4x800", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "50", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "500", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "5000", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "50H_106.7", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "50H_76.2_U12", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "50H_76.2_U14W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "50H_76.2_U16W_U14M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "50H_76.2_U18W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "50H_84", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "50H_91.4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "50H_99.1", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "5x80", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "5xlibre", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "60", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "600", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "60H_106.7", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "60H_76.2_U12", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "60H_76.2_U14W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "60H_76.2_U16W_U14M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "60H_76.2_U18W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "60H_84", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "60H_84.0", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "60H_91.4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "60H_99.1", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "6xlibre", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "80", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "800", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "80H_76.2", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "80H_84", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "americaine", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "balle_200", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "balle_80", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "decathlon_M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "decathlon_U18M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "decathlon_U20M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "decathlon_W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "disque_0.75", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "disque_1", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "disque_1.5", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "disque_1.75", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "disque_2", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "half", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "hauteur", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "heptathlon_M_i", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "heptathlon_U18W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "heptathlon_U20M_i", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "heptathlon_W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "heure", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "hexathlon", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "javelot_400", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "javelot_500", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "javelot_600", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "javelot_600_old", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "javelot_700", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "javelot_800", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "javelot_800_old", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "longueur", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "longueur_zone", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "marathon", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "marteau_3", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "marteau_4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "marteau_5", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "marteau_6", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "marteau_7.26", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "olympique", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "pentathlon_M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "pentathlon_U18M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "pentathlon_U18W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "pentathlon_U20M", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "pentathlon_W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "pentathlon_W_U20W", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "perche", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "poids_2.5", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "poids_3", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "poids_4", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "poids_5", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "poids_6", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "poids_7.26", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

gc()
closeAllConnections()

run_one_event(urls_all, "suedois", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "Triathlon sprint", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "triple", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "UBS kids Cup", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)
run_one_event(urls_all, "vaudoise", indoor = c(TRUE, FALSE), polite_delay = 0.2, split = FALSE, split_ls = TRUE)

#=== 3. Load LSA data

# 3.1. Load LSA records

# Functions to load

import_records_sheet <- function(path, sheet, gender, indoor, club_cat) {
  
  df <- read_excel(path, sheet = sheet, col_names = TRUE)
  
  # adapte si besoin: ici tu as constaté que lignes 1:3 sont du header "visuel"
  df <- df[-c(1:3), ]
  
  # garde seulement les colonnes utiles (selon ton fichier Femmes indoor)
  # NB: tu faisais -c(1,10:22); je reproduis le même esprit mais de façon plus sûre:
  df <- df %>% select(2:9)  # à ajuster si un fichier diffère
  
  names(df) <- c("event", "resultat", "type", "prenom", "nom", "yob", "date", "lieu")
  
  df %>%
    mutate(
      type     = if_else(is.na(type), "", as.character(type)),
      resultat = str_trim(paste(resultat, type)),
      athlete_first = as.character(prenom),
      athlete_last  = as.character(nom),
      athlete       = str_trim(paste(athlete_first, athlete_last)),
      yob      = parse_integer(as.character(yob)),
      date     = as.Date(date),
      gender   = gender,
      indoor   = indoor,
      club_cat = club_cat,
      source   = "legacy_record",
      mark_seconds = vapply(resultat, safe_mark_to_seconds, numeric(1))
    ) %>%
    select(event, resultat, mark_seconds, athlete, athlete_first, athlete_last,
           yob, date, lieu, gender, indoor, club_cat, source)
}
import_records_sheet2 <- function(path, sheet, gender, indoor, club_cat) {
  
  df <- read_excel(path, sheet = sheet, col_names = TRUE, range = cell_cols("A:I"),
                   col_types = c("numeric", "text", "text", 
                                 "text", "text", "text", "numeric", 
                                 "date", "text"))
  
  # adapte si besoin: ici tu as constaté que lignes 1:3 sont du header "visuel"
  df <- df[-c(1:3), ]
  
  # garde seulement les colonnes utiles (selon ton fichier Femmes indoor)
  # NB: tu faisais -c(1,10:22); je reproduis le même esprit mais de façon plus sûre:
  df <- df %>% select(2:9)  # à ajuster si un fichier diffère
  
  names(df) <- c("event", "resultat", "type", "prenom", "nom", "yob", "date", "lieu")
  
  df %>%
    mutate(
      type     = if_else(is.na(type), "", as.character(type)),
      resultat = str_trim(paste(resultat, type)),
      athlete_first = as.character(prenom),
      athlete_last  = as.character(nom),
      athlete       = str_trim(paste(athlete_first, athlete_last)),
      yob      = parse_integer(as.character(yob)),
      date     = as.Date(date),
      gender   = gender,
      indoor   = indoor,
      club_cat = club_cat,
      source   = "legacy_record",
      mark_seconds = vapply(resultat, safe_mark_to_seconds, numeric(1))
    ) %>%
    select(event, resultat, mark_seconds, athlete, athlete_first, athlete_last,
           yob, date, lieu, gender, indoor, club_cat, source)
}

# Male indoor

path_M_ind <- "~/Comité LSA/Statistiques/Records du club/LS RECORDS Hommes indoor.xlsx"

records_M_indoor <- bind_rows(
  import_records_sheet(path_M_ind, sheet = 1, gender = "male", indoor = TRUE, club_cat = "SEN"),
  import_records_sheet(path_M_ind, sheet = 2, gender = "male", indoor = TRUE, club_cat = "U23"),
  import_records_sheet(path_M_ind, sheet = 3, gender = "male", indoor = TRUE, club_cat = "U20"),
  import_records_sheet(path_M_ind, sheet = 4, gender = "male", indoor = TRUE, club_cat = "U18"),
  import_records_sheet(path_M_ind, sheet = 5, gender = "male", indoor = TRUE, club_cat = "U16")
)

glimpse(records_M_indoor)

# Female indoor

path_W_ind <- "~/Comité LSA/Statistiques/Records du club/LS RECORDS Femmes indoor.xlsx"

records_W_indoor <- bind_rows(
  import_records_sheet(path_W_ind, sheet = 1, gender = "female", indoor = TRUE, club_cat = "SEN"),
  import_records_sheet(path_W_ind, sheet = 2, gender = "female", indoor = TRUE, club_cat = "U23"),
  import_records_sheet(path_W_ind, sheet = 3, gender = "female", indoor = TRUE, club_cat = "U20"),
  import_records_sheet(path_W_ind, sheet = 4, gender = "female", indoor = TRUE, club_cat = "U18"),
  import_records_sheet(path_W_ind, sheet = 5, gender = "female", indoor = TRUE, club_cat = "U16")
)

glimpse(records_W_indoor)

# Male outdoor

path_M_out <- "~/Comité LSA/Statistiques/Records du club/LS RECORDS Hommes outdoor.xlsx"

records_M_outdoor <- bind_rows(
  import_records_sheet2(path_M_out, 1, "male", FALSE, "SEN"),
  import_records_sheet2(path_M_out, 2, "male", FALSE, "U23"),
  import_records_sheet2(path_M_out, 3, "male", FALSE, "U20"),
  import_records_sheet2(path_M_out, 4, "male", FALSE, "U18"),
  import_records_sheet2(path_M_out, 5, "male", FALSE, "U16")
)

glimpse(records_M_outdoor)

# Female outdoor

path_W_out <- "~/Comité LSA/Statistiques/Records du club/LS RECORDS Femmes outdoor.xlsx"

records_W_outdoor <- bind_rows(
  import_records_sheet2(path_W_out, 1, "female", FALSE, "SEN"),
  import_records_sheet2(path_W_out, 2, "female", FALSE, "U23"),
  import_records_sheet2(path_W_out, 3, "female", FALSE, "U20"),
  import_records_sheet2(path_W_out, 4, "female", FALSE, "U18"),
  import_records_sheet2(path_W_out, 5, "female", FALSE, "U16")
)

glimpse(records_W_outdoor)

# Merge records LSA

records_LSA_PA <- dplyr::bind_rows(records_M_outdoor, records_W_outdoor, records_M_indoor, records_W_indoor)

# 3.2. Load TOP 50

# Helpers & functions

load_top50_sheet_fixedlayout <- function(path,
                                         sheet,
                                         gender,
                                         indoor,
                                         club_cat_default = "ALL",
                                         source = "legacy_top50") {
  
  # ---- Helper: age -> club category (reuse yours if present)
  if (!exists("age_to_cat", mode = "function")) {
    age_to_cat <- function(age) {
      if (is.na(age)) return(NA_character_)
      if (age < 10)  return("U10")
      if (age < 12)  return("U12")
      if (age < 14)  return("U14")
      if (age < 16)  return("U16")
      if (age < 18)  return("U18")
      if (age < 20)  return("U20")
      if (age < 23)  return("U23")
      if (age >= 23) return("SEN")
      NA_character_
    }
  }
  
  # ---- Helper: parse Top50 date cells into Date
  safe_date_top50 <- function(x) {
    if (is.numeric(x)) {
      n <- suppressWarnings(as.numeric(x))
      if (is.na(n)) return(as.Date(NA))
      if (n >= 30000 && n < 80000) return(as.Date(n, origin = "1899-12-30")) # Excel
      if (n > -25000 && n < 30000) return(as.Date(n, origin = "1970-01-01")) # R Date numeric
      return(as.Date(NA))
    }
    
    s <- trimws(as.character(x))
    if (is.na(s) || s == "") return(as.Date(NA))
    
    if (grepl("^-?\\d+(\\.\\d+)?$", s)) {
      n <- suppressWarnings(as.numeric(s))
      if (is.na(n)) return(as.Date(NA))
      if (n >= 30000 && n < 80000) return(as.Date(n, origin = "1899-12-30"))
      if (n > -25000 && n < 30000) return(as.Date(n, origin = "1970-01-01"))
      return(as.Date(NA))
    }
    
    fmts <- c(
      "%Y-%m-%d",
      "%d.%m.%Y", "%d.%m.%y",
      "%d/%m/%Y", "%d/%m/%y",
      "%d-%m-%Y", "%d-%m-%y"
    )
    for (f in fmts) {
      d <- suppressWarnings(as.Date(s, format = f))
      if (!is.na(d)) return(d)
    }
    as.Date(NA)
  }
  
  # 1) Read sheet without headers
  raw <- readxl::read_excel(
    path      = path,
    sheet     = sheet,
    col_names = FALSE,
    col_types = "text"
  ) %>% tibble::as_tibble()
  
  if (nrow(raw) == 0) return(tibble::tibble())
  
  # 2) Find first data row (rank==1 and performance non-empty)
  col_rank <- raw[[2]]
  col_perf <- raw[[3]]
  
  start_row <- which(
    !is.na(col_rank) &
      stringr::str_trim(col_rank) %in% c("1", "01") &
      !is.na(col_perf) &
      stringr::str_trim(col_perf) != ""
  )[1]
  
  if (is.na(start_row)) {
    warning("No Top50 data detected in sheet: ", sheet)
    return(tibble::tibble())
  }
  
  # 3) Keep B:I if present; if some columns missing, pad them
  want_idx <- 2:9
  have_idx <- want_idx[want_idx <= ncol(raw)]
  
  dat <- raw %>%
    dplyr::slice(start_row:dplyr::n()) %>%
    dplyr::select(dplyr::all_of(have_idx)) %>%
    tibble::as_tibble()
  
  # pad missing columns up to 8 cols total (B:I = 8 cols)
  if (ncol(dat) < 8) {
    for (k in (ncol(dat) + 1):8) dat[[paste0("V", k)]] <- NA_character_
  }
  
  # enforce consistent names
  names(dat) <- c("rank", "resultat", "Vent", "prenom", "nom", "yob", "date", "lieu")
  
  # 4) Standardize + compute age-based club_cat
  out <- dat %>%
    dplyr::mutate(
      event = as.character(sheet),
      
      rank = suppressWarnings(readr::parse_integer(as.character(rank))),
      
      Vent = dplyr::if_else(is.na(Vent), "", as.character(Vent)),
      resultat = stringr::str_trim(stringr::str_c(as.character(resultat), " ", Vent)),
      
      athlete_first = as.character(prenom),
      athlete_last  = as.character(nom),
      athlete       = stringr::str_trim(stringr::str_c(athlete_first, " ", athlete_last)),
      
      yob  = suppressWarnings(readr::parse_integer(as.character(yob))),
      date = as.Date(sapply(date, safe_date_top50), origin = "1970-01-01"),
      lieu = as.character(lieu),
      
      gender = gender,
      indoor = indoor,
      source = source,
      
      season = dplyr::if_else(is.na(date), NA_integer_, as.integer(format(date, "%Y"))),
      age = dplyr::if_else(
        is.na(yob) | is.na(season),
        NA_integer_,
        as.integer(season) - as.integer(yob)
      ),
      
      club_cat = dplyr::if_else(
        is.na(age),
        as.character(club_cat_default),
        vapply(age, age_to_cat, character(1))
      ),
      
      mark_seconds = vapply(resultat, safe_mark_to_seconds, numeric(1))
    ) %>%
    dplyr::select(
      event, resultat, mark_seconds,
      athlete, athlete_first, athlete_last,
      yob, date, lieu,
      gender, indoor, club_cat, source,
      rank
    ) %>%
    dplyr::filter(!is.na(rank) & !is.na(resultat) & resultat != "")
  
  out
}

load_top50_workbook_fixedlayout <- function(path,
                                            gender,
                                            indoor,
                                            club_cat,
                                            sheets = NULL,
                                            source = "legacy_top50") {
  if (is.null(sheets)) sheets <- excel_sheets(path)
  
  purrr::map_dfr(
    sheets,
    ~load_top50_sheet_fixedlayout(
      path   = path,
      sheet  = .x,
      gender = gender,
      indoor = indoor,
      club_cat = club_cat,
      source = source
    )
  )
}
# Male outdoor

path_top50_M_out <- "~/Comité LSA/Statistiques/Records du club/LS TOP 50 Hommes outdoor.xlsx"

top50_M_outdoor <- load_top50_workbook_fixedlayout(
  path   = path_top50_M_out,
  gender = "male",
  indoor = FALSE,
  club_cat = "ALL"
)

glimpse(top50_M_outdoor)

# Female outdoor

path_top50_W_out <- "~/Comité LSA/Statistiques/Records du club/LS TOP 50 Femmes outdoor.xlsx"

top50_W_outdoor <- load_top50_workbook_fixedlayout(
  path   = path_top50_W_out,
  gender = "female",
  indoor = FALSE,
  club_cat = "ALL"
)

glimpse(top50_W_outdoor)

# Male indoor

path_top50_M_out <- "~/Comité LSA/Statistiques/Records du club/LS TOP 50 Hommes indoor.xlsx"

top50_M_indoor <- load_top50_workbook_fixedlayout(
  path   = path_top50_M_out,
  gender = "male",
  indoor = TRUE,
  club_cat = "ALL"
)

glimpse(top50_M_indoor)

# Female indoor

path_top50_W_out <- "~/Comité LSA/Statistiques/Records du club/LS TOP 50 Femmes indoor.xlsx"

top50_W_indoor <- load_top50_workbook_fixedlayout(
  path   = path_top50_W_out,
  gender = "female",
  indoor = TRUE,
  club_cat = "ALL"
)

glimpse(top50_W_indoor)

# Merge TOP 50 LSA

fix_legacy_year_shift <- function(df,
                                  wrong_min = 2026L,
                                  wrong_max = 2052L,
                                  shift     = 70L) {
  df %>%
    dplyr::mutate(
      # --- Fix date if the year is in the wrong future range ---
      date = {
        # Extract current year/month/day as character
        yr <- suppressWarnings(as.integer(format(date, "%Y")))
        mo <- format(date, "%m")
        da <- format(date, "%d")
        
        # Apply shift to years in the wrong range
        new_yr <- ifelse(
          !is.na(yr) & yr >= wrong_min & yr <= wrong_max,
          yr - shift,
          yr
        )
        
        # Rebuild Date; keep NA when year is NA
        as.Date(ifelse(
          is.na(new_yr),
          NA_character_,
          paste(new_yr, mo, da, sep = "-")
        ))
      },
      
  
    )
}

Top50_LSA_PA <- dplyr::bind_rows(
  top50_M_outdoor,
  top50_W_outdoor,
  top50_M_indoor,
  top50_W_indoor
) %>%
  fix_legacy_year_shift()

# 3.3. Load Top 10 

# Functions

load_top10_sheet_blocked <- function(path,
                                     sheet,
                                     gender = "female",
                                     source = "legacy_top10") {
  
  # ---- Helpers (reuse yours if already defined) ----
  if (!exists("safe_mark_to_seconds", mode = "function")) {
    stop("safe_mark_to_seconds() must exist in your environment.")
  }
  
  # Date parser: handles Excel serials + text dates
  safe_date_legacy <- function(x) {
    s <- trimws(as.character(x))
    if (is.na(s) || s == "") return(as.Date(NA))
    
    # numeric-like? (Excel serial)
    if (grepl("^-?\\d+(\\.\\d+)?$", s)) {
      n <- suppressWarnings(as.numeric(s))
      if (is.na(n)) return(as.Date(NA))
      # Excel serial modern range
      if (n >= 30000 && n < 80000) return(as.Date(n, origin = "1899-12-30"))
      # R Date numeric (rare here, but safe)
      if (n > -25000 && n < 30000) return(as.Date(n, origin = "1970-01-01"))
      return(as.Date(NA))
    }
    
    fmts <- c("%Y-%m-%d", "%d.%m.%Y", "%d.%m.%y", "%d/%m/%Y", "%d/%m/%y", "%d-%m-%Y", "%d-%m-%y")
    for (f in fmts) {
      d <- suppressWarnings(as.Date(s, format = f))
      if (!is.na(d)) return(d)
    }
    as.Date(NA)
  }
  
  # ---- Infer category + indoor from sheet name ----
  sh <- as.character(sheet)
  
  indoor <- grepl("indoor|halle|salle", tolower(sh))
  
  # club_cat mapping from sheet title
  # examples: "TOP10 U14W", "TOP10 W", "TOP10 W indoor"
  club_cat <- dplyr::case_when(
    grepl("U14", sh, ignore.case = TRUE) ~ "U14",
    grepl("U16", sh, ignore.case = TRUE) ~ "U16",
    grepl("U18", sh, ignore.case = TRUE) ~ "U18",
    grepl("U20", sh, ignore.case = TRUE) ~ "U20",
    grepl("U23", sh, ignore.case = TRUE) ~ "U23",
    TRUE ~ "SEN"
  )
  
  # ---- 1) Read sheet as raw grid (no headers) ----
  raw <- readxl::read_excel(
    path      = path,
    sheet     = sheet,
    col_names = FALSE
  ) %>% tibble::as_tibble()
  
  if (nrow(raw) == 0) return(tibble::tibble())
  
  # We expect (based on your file) something like:
  # A: event label (header rows)
  # B: rank
  # C: result
  # E: first name
  # F: last name
  # G: yob
  # H: date
  # I: place
  #
  # We'll reference by position defensively and pad missing columns.
  if (ncol(raw) < 9) {
    for (k in (ncol(raw) + 1):9) raw[[paste0("..pad", k)]] <- NA
  }
  
  # Name the first 9 columns for clarity
  names(raw)[1:9] <- c("A_event", "B_rank", "C_result", "D_extra",
                       "E_first", "F_last", "G_yob", "H_date", "I_lieu")
  
  # ---- 2) Detect event header rows and fill down ----
  # Event header rows: have event name in A_event AND no rank/result
  raw2 <- raw %>%
    dplyr::mutate(
      A_event = as.character(A_event),
      B_rank  = as.character(B_rank),
      C_result = as.character(C_result),
      
      is_event_header = !is.na(A_event) & trimws(A_event) != "" &
        (is.na(B_rank) | trimws(B_rank) == "") &
        (is.na(C_result) | trimws(C_result) == "")
    ) %>%
    dplyr::mutate(
      event = dplyr::if_else(is_event_header, trimws(A_event), NA_character_),
      event = tidyr::fill(tibble::tibble(event = event), event, .direction = "down")$event
    )
  
  # ---- 3) Keep rank rows 1..10 ----
  out <- raw2 %>%
    dplyr::mutate(
      rank = suppressWarnings(readr::parse_integer(B_rank)),
      resultat_base = as.character(C_result),
      extra = dplyr::if_else(is.na(D_extra), "", as.character(D_extra)),
      resultat = stringr::str_trim(stringr::str_c(resultat_base, " ", extra)),
      
      athlete_first = as.character(E_first),
      athlete_last  = as.character(F_last),
      athlete       = stringr::str_trim(stringr::str_c(athlete_first, " ", athlete_last)),
      
      yob  = suppressWarnings(readr::parse_integer(as.character(G_yob))),
      date = as.Date(sapply(H_date, safe_date_legacy), origin = "1970-01-01"),
      lieu = as.character(I_lieu),
      
      gender   = gender,
      indoor   = indoor,
      club_cat = club_cat,
      source   = source,
      
      mark_seconds = vapply(resultat, safe_mark_to_seconds, numeric(1))
    ) %>%
    dplyr::filter(!is.na(rank) & rank >= 1 & rank <= 10) %>%
    dplyr::select(
      event, resultat, mark_seconds,
      athlete, athlete_first, athlete_last,
      yob, date, lieu,
      gender, indoor, club_cat, source,
      rank
    )
  
  out
}

load_top10_workbook_blocked <- function(path,
                                        sheets = NULL,
                                        gender = "female",
                                        source = "legacy_top10") {
  
  if (is.null(sheets)) sheets <- readxl::excel_sheets(path)
  
  purrr::map_dfr(
    sheets,
    ~load_top10_sheet_blocked(
      path   = path,
      sheet  = .x,
      gender = gender,
      source = source
    )
  )
}

# Female

path_top10_W <- "~/Comité LSA/Statistiques/Records du club/LS TOP 10 Femmes.xlsx"

top10_W_all <- load_top10_workbook_blocked(path_top10_W, gender = "female")
dplyr::glimpse(top10_W_all)

# Male

path_top10_M <- "~/Comité LSA/Statistiques/Records du club/LS TOP 10 Hommes.xlsx"

top10_M_all <- load_top10_workbook_blocked(path_top10_M, gender = "male")
dplyr::glimpse(top10_M_all)

# Merge TOP 10 LSA

Top10_LSA_PA <- dplyr::bind_rows(top10_M_all, top10_W_all) %>%
  fix_legacy_year_shift()

# 3.4. Load record young categories

LSA_records_U10_U14 <- read_excel("data/ls_master/LSA_records_U10_U14.xlsx")

# 3.5. Load manual filling

LSA_records_manual_filling <- read_excel("data/ls_master/LSA_records_manual_filling.xlsx")

#=== 4. Merging everything pipeline

# 4.1. Core helpers and functions

to_logical_indoor <- function(x) {
  if (is.logical(x)) return(x)
  s <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    s %in% c("true","t","1","indoor","in","halle","salle","i") ~ TRUE,
    s %in% c("false","f","0","outdoor","out","plein air","o") ~ FALSE,
    TRUE ~ NA
  )
}

parse_date_multi <- function(x) {
  # Retourne as.Date(NA) si impossible
  if (length(x) == 0) return(as.Date(NA))
  
  # Vector-friendly : on parse élément par élément avec vapply
  parse_one <- function(v) {
    if (is.null(v) || is.na(v)) return(as.Date(NA))
    
    # 1) Si Excel a importé en numérique
    if (is.numeric(v)) {
      # Heuristique :
      # - 1800..2200 => une année
      if (v >= 1800 && v <= 2200) {
        return(as.Date(sprintf("%04d-01-01", as.integer(v))))
      }
      # - 1..60000 => Excel serial date
      if (v > 0 && v < 60000) {
        return(as.Date(v, origin = "1899-12-30"))
      }
      return(as.Date(NA))
    }
    
    s <- trimws(as.character(v))
    if (s == "" || is.na(s)) return(as.Date(NA))
    
    # 2) Année seule "2006"
    if (grepl("^\\d{4}$", s)) {
      yr <- as.integer(s)
      if (!is.na(yr) && yr >= 1800 && yr <= 2200) {
        return(as.Date(sprintf("%04d-01-01", yr)))
      }
    }
    
    # 3) Dates classiques (ISO / FR)
    # ISO: 2006-06-30
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(as.Date(s))
    
    # dd.mm.yyyy ou dd/mm/yyyy
    if (grepl("^\\d{1,2}[./-]\\d{1,2}[./-]\\d{4}$", s)) {
      s2 <- gsub("[./]", "-", s)
      parts <- strsplit(s2, "-", fixed = TRUE)[[1]]
      if (length(parts) == 3) {
        dd <- sprintf("%02d", as.integer(parts[1]))
        mm <- sprintf("%02d", as.integer(parts[2]))
        yy <- parts[3]
        return(as.Date(paste0(yy, "-", mm, "-", dd)))
      }
    }
    
    as.Date(NA)
  }
  
  vapply(x, parse_one, as.Date(NA))
}

# safe_mark_to_seconds <- function(x) { ... }

# Canonical schema
canonicalize_ls <- function(df) {
  # 0) Unique column names
  names(df) <- make.unique(names(df), sep = "__")
  
  # 1) Map common source columns if missing
  if (!"resultat" %in% names(df)) {
    cand <- intersect(c("Résultat", "Resultat", "resultat"), names(df))
    if (length(cand)) df$resultat <- df[[cand[1]]] else df$resultat <- NA
  }
  if (!"athlete" %in% names(df)) {
    cand <- intersect(c("Nom", "athlete"), names(df))
    if (length(cand)) df$athlete <- df[[cand[1]]] else df$athlete <- NA
  }
  if (!"date" %in% names(df)) {
    cand <- intersect(c("Date", "date"), names(df))
    if (length(cand)) df$date <- df[[cand[1]]] else df$date <- NA
  }
  if (!"lieu" %in% names(df)) {
    cand <- intersect(c("Lieu", "lieu"), names(df))
    if (length(cand)) df$lieu <- df[[cand[1]]] else df$lieu <- NA
  }
  
  # event & event_display
  if (!"event" %in% names(df)) {
    if ("event_name" %in% names(df)) {
      df$event <- df$event_name
    } else if ("event_display" %in% names(df)) {
      df$event <- df$event_display
    } else {
      df$event <- NA_character_
    }
  }
  if (!"event_display" %in% names(df)) {
    df$event_display <- df$event
  }
  
  # 2) Ensure required + optional columns exist
  required <- c(
    "event", "event_display",
    "resultat", "mark_seconds",
    "athlete", "yob", "date", "lieu",
    "gender", "indoor", "club_cat",
    "source", "season", "age"
  )
  
  optional <- c(
    "rank",
    "event_name","event_type","category_raw",
    "Societe","source_file",
    "metric","better_is_lower",
    "mark_distance","mark_points","mark",
    "metric_effective"
  )
  
  for (nm in c(required, optional)) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }
  
  # 3) Type normalization
  df <- df %>%
    dplyr::mutate(
      event           = as.character(event),
      event_display   = as.character(event_display),
      resultat        = as.character(resultat),
      athlete         = as.character(athlete),
      yob             = suppressWarnings(as.integer(yob)),
      date            = as.Date(vapply(date, parse_date_multi, as.Date(NA))),
      lieu            = as.character(lieu),
      gender          = as.character(gender),
      indoor          = to_logical_indoor(indoor),
      club_cat        = as.character(club_cat),
      source          = as.character(source),
      season          = suppressWarnings(as.integer(season)),
      age             = suppressWarnings(as.integer(age)),
      mark_seconds    = suppressWarnings(as.numeric(mark_seconds)),
      rank            = suppressWarnings(as.integer(rank)),
      event_name      = as.character(event_name),
      event_type      = as.character(event_type),
      category_raw    = as.character(category_raw),
      Societe         = as.character(Societe),
      source_file     = as.character(source_file),
      metric          = as.character(metric),
      better_is_lower = tolower(as.character(better_is_lower)) %in% c("true","t","1"),
      mark_distance   = suppressWarnings(as.numeric(mark_distance)),
      mark_points     = suppressWarnings(as.numeric(mark_points)),
      mark            = suppressWarnings(as.numeric(mark)),
      metric_effective = as.character(metric_effective)
    )
  
  # 4) Season/age backfill
  df <- df %>%
    dplyr::mutate(
      season = dplyr::if_else(
        is.na(season) & !is.na(date),
        as.integer(format(date, "%Y")),
        season
      ),
      age = dplyr::if_else(
        is.na(age) & !is.na(season) & !is.na(yob),
        as.integer(season - yob),
        age
      )
    )
  
  # 5) Keep required + optional first, then everything else
  df %>%
    dplyr::select(dplyr::all_of(required), dplyr::all_of(optional), dplyr::everything())
}

repair_perf_date_from_season <- function(df, min_age = 8L) {
  df %>%
    dplyr::mutate(
      # 1) parse date si c’est encore du texte sale
      date_parsed = as.Date(vapply(date, parse_date_multi, as.Date(NA))),
      
      # 2) si season manquant, le déduire de date_parsed
      season_fix = dplyr::coalesce(season, as.integer(format(date_parsed, "%Y"))),
      
      # 3) calcul age "théorique"
      age_fix = ifelse(!is.na(season_fix) & !is.na(yob), season_fix - yob, NA_integer_),
      
      # 4) si age aberrant (négatif ou trop jeune), forcer une date proxy au 01-01-season_fix
      date_fix = dplyr::case_when(
        !is.na(season_fix) & !is.na(yob) & (age_fix < 0 | age_fix < min_age) ~
          as.Date(paste0(season_fix, "-01-01")),
        !is.na(date_parsed) ~ date_parsed,
        TRUE ~ as.Date(NA)
      ),
      
      # 5) écrire back
      date   = date_fix,
      season = season_fix,
      age    = ifelse(!is.na(season_fix) & !is.na(yob), season_fix - yob, age)
    ) %>%
    dplyr::select(-date_parsed, -season_fix, -age_fix, -date_fix)
}

# Mark parsing helpers

# track times (string) -> cleaned string for seconds parser
clean_result_time <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[^0-9\\.:]", "")      # keep 0-9 : .
  x <- stringr::str_replace(x, "(\\.[0-9]{2}).*$", "\\1") # keep at most 2 decimals
  x
}

# distance / height
parse_distance_from_resultat <- function(x) {
  s <- as.character(x)
  s <- gsub(",", ".", s, fixed = TRUE)                            # decimal
  s <- gsub("\\s*m\\.?\\s*$", "", s, ignore.case = TRUE)          # trailing m / m.
  num_str <- stringr::str_extract(s, "[0-9]+\\.?[0-9]*")
  out <- suppressWarnings(as.numeric(num_str))
  out[is.nan(out)] <- NA_real_
  out
}

# multi-events (first 3–4 digits rule)
parse_points_from_resultat <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""
  s_num <- stringr::str_replace_all(s, "[^0-9]", "")
  s_num[is.na(s_num)] <- ""
  
  n <- nchar(s_num)
  out <- rep(NA_real_, length(s_num))
  
  idx_short <- n > 0 & n <= 4
  if (any(idx_short)) {
    out[idx_short] <- suppressWarnings(as.numeric(s_num[idx_short]))
  }
  
  idx_long <- n > 4
  if (any(idx_long)) {
    sub_str <- substr(s_num[idx_long], 1L, 4L)
    out[idx_long] <- suppressWarnings(as.numeric(sub_str))
  }
  
  out
}

# tolerant time parser for strings like "1:44.1m", "11:26.8", etc.
parse_time_loose <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""
  
  s <- gsub(",", ".", s, fixed = TRUE)
  s <- gsub("[[:space:]]*[A-Za-z\\.]+$", "", s) # remove trailing units
  s <- trimws(s)
  
  out <- rep(NA_real_, length(s))
  has_colon <- grepl(":", s, fixed = TRUE)
  
  # Cases with colon
  if (any(has_colon)) {
    parts_list <- strsplit(s[has_colon], ":", fixed = TRUE)
    out_colon <- vapply(parts_list, function(p) {
      p <- trimws(p)
      p <- p[nzchar(p)]
      if (length(p) == 2L) {
        mm <- suppressWarnings(as.numeric(p[1]))
        ss <- suppressWarnings(as.numeric(p[2]))
        if (is.na(mm) || is.na(ss)) return(NA_real_)
        mm * 60 + ss
      } else if (length(p) == 3L) {
        hh <- suppressWarnings(as.numeric(p[1]))
        mm <- suppressWarnings(as.numeric(p[2]))
        ss <- suppressWarnings(as.numeric(p[3]))
        if (any(is.na(c(hh, mm, ss)))) return(NA_real_)
        hh * 3600 + mm * 60 + ss
      } else {
        NA_real_
      }
    }, numeric(1))
    out[has_colon] <- out_colon
  }
  
  # Cases without colon: seconds
  if (any(!has_colon)) {
    secs <- suppressWarnings(as.numeric(s[!has_colon]))
    out[!has_colon] <- secs
  }
  
  out
}

# Compute marks COMPUTE MARKS

compute_marks_from_result <- function(df) {
  df %>%
    dplyr::mutate(
      metric     = as.character(metric),
      event_type = as.character(event_type),
      
      # Effective metric (robust to wrong mapping)
      metric_effective = dplyr::case_when(
        event_type == "run"              ~ "time",
        event_type %in% c("jump", "throw") ~ "distance",
        event_type == "multi"            ~ "points",
        
        metric %in% c("time","distance","height","points") ~ metric,
        
        stringr::str_detect(resultat, "pts\\b|points?\\b") ~ "points",
        stringr::str_detect(resultat, ":")                 ~ "time",
        stringr::str_detect(resultat, "m\\b")              ~ "distance",
        
        TRUE ~ NA_character_
      ),
      
      # Time: prefer existing mark_seconds, else parse from `resultat`
      mark_seconds = dplyr::if_else(
        metric_effective == "time",
        {
          existing   <- suppressWarnings(as.numeric(mark_seconds))
          need_parse <- is.na(existing) & !is.na(resultat)
          parsed     <- existing
          if (any(need_parse)) {
            parsed[need_parse] <- parse_time_loose(resultat[need_parse])
          }
          parsed
        },
        NA_real_
      ),
      
      # Distance / height
      mark_distance = dplyr::if_else(
        metric_effective %in% c("distance","height"),
        parse_distance_from_resultat(resultat),
        NA_real_
      ),
      
      # Points
      mark_points = dplyr::if_else(
        metric_effective == "points",
        parse_points_from_resultat(resultat),
        NA_real_
      ),
      
      # Unified mark
      mark = dplyr::case_when(
        metric_effective == "time"                     ~ mark_seconds,
        metric_effective %in% c("distance","height")   ~ mark_distance,
        metric_effective == "points"                   ~ mark_points,
        TRUE                                           ~ NA_real_
      )
    )
}

# Normalization

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, to = "ASCII//TRANSLIT")     # remove accents
  x <- str_replace_all(x, "[^A-Za-z0-9 ]", " ")
  x <- str_squish(x)
  tolower(x)
}

normalize_name <- function(x) {
  x <- normalize_text(x)
  # optional: remove single-letter tokens etc. if needed later
  x
}

# Vectorised direction (NO isTRUE)
attach_direction <- function(df) {
  df %>%
    mutate(
      direction = case_when(
        is.na(better_is_lower) ~ "min",  # safe default for track-like data
        better_is_lower        ~ "min",
        TRUE                   ~ "max"
      )
    )
}

# Fix accent-based duplicates at the database level by creating stable keys
# and keeping a preferred row (by source priority + presence of date/lieu).
clean_master_duplicates <- function(df) {
  
  # pick the best available category column WITHOUT referencing missing columns
  cat_col <- dplyr::case_when(
    "club_cat_raw" %in% names(df) ~ "club_cat_raw",
    "club_cat"     %in% names(df) ~ "club_cat",
    TRUE                         ~ NA_character_
  )
  
  df %>%
    dplyr::mutate(
      athlete_key = normalize_name(athlete),
      lieu_key    = normalize_name(lieu),
      
      mark_key = dplyr::case_when(
        !is.na(mark)         ~ sprintf("%.4f", mark),
        !is.na(mark_seconds) ~ sprintf("%.4f", mark_seconds),
        TRUE ~ NA_character_
      ),
      
      date_key = dplyr::if_else(is.na(date), "NA", as.character(date)),
      dob_key  = normalize_text(`Date naiss.`),
      
      # safe category key
      club_cat_key = if (!is.na(cat_col)) as.character(.data[[cat_col]]) else "NA",
      
      src_prio = source_priority(source)
    ) %>%
    dplyr::arrange(dplyr::desc(src_prio), dplyr::desc(!is.na(date)), dplyr::desc(!is.na(lieu))) %>%
    dplyr::distinct(
      event, gender, indoor, club_cat_key,
      athlete_key, yob, dob_key,
      mark_key, date_key, lieu_key,
      .keep_all = TRUE
    ) %>%
    dplyr::select(-athlete_key, -lieu_key, -mark_key, -date_key,
                  -dob_key, -club_cat_key, -src_prio)
}

# Age issues

recompute_club_cat_age <- function(age) {
  dplyr::case_when(
    is.na(age) ~ NA_character_,
    age < 10   ~ "U10",
    age < 12   ~ "U12",
    age < 14   ~ "U14",
    age < 16   ~ "U16",
    age < 18   ~ "U18",
    age < 20   ~ "U20",
    age < 23   ~ "U23",
    TRUE       ~ "SEN"
  )
}

flag_and_fix_age_cat <- function(df, min_age_ok = 6L, max_age_ok = 100L) {
  df %>%
    dplyr::mutate(
      club_cat_raw = club_cat,
      
      # age "théorique" basé sur season/yob
      age_calc = dplyr::if_else(!is.na(season) & !is.na(yob),
                                as.integer(season - yob),
                                NA_integer_),
      
      # flag qualité
      age_wrong = dplyr::case_when(
        is.na(age_calc)                 ~ TRUE,
        age_calc < min_age_ok           ~ TRUE,
        age_calc > max_age_ok           ~ TRUE,
        TRUE                            ~ FALSE
      ),
      
      # age réellement stocké (on privilégie age_calc si ok, sinon on garde age existant)
      age = dplyr::if_else(!age_wrong, age_calc, age),
      
      # catégorie recalculée si ok, sinon raw
      club_cat_eff = dplyr::if_else(!age_wrong,
                                    recompute_club_cat(season, yob),
                                    club_cat_raw),
      
      # on écrase club_cat par la catégorie "effective"
      club_cat = dplyr::coalesce(club_cat_eff, club_cat_raw, club_cat)
    ) %>%
    dplyr::select(-age_calc, -club_cat_eff)
}

# Dedup logic

source_priority <- function(src) {
  dplyr::case_when(
    src == "alabus_ls"     ~ 3L,
    src == "legacy_record" ~ 2L,
    src == "legacy_top10"  ~ 1L,
    src == "legacy_top50"  ~ 0L,
    TRUE                   ~ -1L
  )
}

athlete_id_key <- function(athlete, dob) {
  paste0(normalize_name(athlete), "||", normalize_text(dob))
}

dedup_ls <- function(df) {
  
  df <- canonicalize_ls(df) %>%
    dplyr::mutate(
      src_prio = source_priority(source),
      
      # keep the original category for traceability
      club_cat_raw = club_cat,
      
      # recompute canonical category
      club_cat_eff = recompute_club_cat(age),
      club_cat_eff = dplyr::coalesce(club_cat_eff, club_cat_raw),
      
      # ---- athlete identity (accent-proof) ----
      athlete_id = athlete_id_key(athlete, `Date naiss.`),
      
      # stable mark key
      mark_key = dplyr::case_when(
        !is.na(mark) ~ sprintf("%.4f", mark),
        TRUE         ~ NA_character_
      ),
      
      date_key = dplyr::if_else(is.na(date), "NA", as.character(date))
    )
  
  # PASS 1: strict-ish (event context + mark + date)
  df1 <- df %>%
    dplyr::mutate(
      key_strict = paste(
        event, gender, indoor, club_cat_eff,
        athlete_id, yob,
        mark_key, date_key,
        sep = "|"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(src_prio), dplyr::desc(!is.na(date)), dplyr::desc(!is.na(lieu))) %>%
    dplyr::group_by(key_strict) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
  
  # PASS 2: loose (if date is missing or inconsistent in one source)
  df2 <- df1 %>%
    dplyr::mutate(
      key_loose = paste(
        event, gender, indoor, club_cat_eff,
        athlete_id, yob,
        mark_key,
        sep = "|"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(src_prio), dplyr::desc(!is.na(date)), dplyr::desc(!is.na(lieu))) %>%
    dplyr::group_by(key_loose) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
  
  # write back canonical category
  df2 %>%
    dplyr::mutate(club_cat = club_cat_eff) %>%
    dplyr::select(-src_prio, -key_strict, -key_loose, -club_cat_eff, -athlete_id, -mark_key, -date_key)
}

dedup_key_yob <- function(df) {
  df %>%
    dplyr::mutate(
      athlete_norm = normalize_name(athlete),
      yob_norm     = as.character(yob),
      date_key     = dplyr::if_else(is.na(date), "NA", as.character(date)),
      mark_key     = dplyr::if_else(is.na(mark), NA_character_, sprintf("%.4f", mark)),
      dedup_key    = paste(event, gender, indoor, athlete_norm, yob_norm, date_key, mark_key, sep = "|")
    )
}

dedup_ls_yob <- function(df) {
  df2 <- df %>%
    canonicalize_ls() %>%
    dedup_key_yob() %>%
    dplyr::mutate(
      src_prio = source_priority(source),
      has_lieu = !is.na(lieu) & lieu != ""
    ) %>%
    dplyr::arrange(dplyr::desc(src_prio), dplyr::desc(has_lieu))
  
  df2 %>%
    dplyr::group_by(dedup_key) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dedup_key, -src_prio, -has_lieu, -athlete_norm, -yob_norm, -date_key, -mark_key)
}

# SB only dataset

make_ls_sb <- function(df) {
  
  df %>%
    canonicalize_ls() %>%
    compute_marks_from_result() %>%
    attach_direction() %>%
    dplyr::mutate(
      athlete_key = normalize_name(athlete),
      season      = dplyr::coalesce(season, as.integer(format(date, "%Y"))),
      
      # ensure we have a unified numeric "mark" for ranking
      mark = dplyr::coalesce(mark, mark_seconds, mark_distance, mark_points),
      
      # recompute canonical category (optional, but consistent with your plan)
      age     = dplyr::coalesce(age, season - yob),
      club_cat_raw = club_cat,
      club_cat = recompute_club_cat(season, yob),
      
      src_prio = source_priority(source),
      
      # lower score = better if direction == "min"
      # higher score = better if direction == "max"
      score = dplyr::case_when(
        direction == "min" ~  mark,
        direction == "max" ~ -mark,
        TRUE               ~  mark   # safe fallback
      )
    ) %>%
    dplyr::filter(!is.na(season), !is.na(mark), !is.na(athlete_key), athlete_key != "") %>%
    
    # ---- SB selection ----
  dplyr::group_by(event, gender, indoor, season, athlete_key) %>%
    dplyr::arrange(
      score,                              # best mark first
      dplyr::desc(src_prio),              # prefer alabus over legacy when equal
      dplyr::desc(!is.na(date)),          # keep dated record when equal
      date,                               # earlier date (optional; can be desc(date) if you prefer latest)
      dplyr::desc(!is.na(lieu))           # keep lieu when equal
    ) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    
    # keep key explicitly if useful
    dplyr::mutate(athlete = athlete_key) %>%  # optional: overwrite athlete display; otherwise remove this line
    dplyr::select(-athlete_key, -src_prio, -score)
}

# 4.2. Save and load helpers

save_master <- function(df, name, dir = "data/ls_master", xlsx = TRUE) {
  fs::dir_create(dir)
  
  rds_path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(df, rds_path)
  message(">>> Saved: ", rds_path, " (n=", nrow(df), ")")
  
  if (xlsx) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Le package 'openxlsx' est nécessaire.")
    }
    xlsx_path <- file.path(dir, paste0(name, ".xlsx"))
    openxlsx::write.xlsx(df, xlsx_path, overwrite = TRUE)
    message(">>> Saved: ", xlsx_path, " (n=", nrow(df), ")")
  }
  
  invisible(df)
}

load_master <- function(name, dir = "data/ls_master") {
  path <- file.path(dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Missing: ", path)
  readRDS(path)
}

# 4.3. Alabus loading and standarization

load_all_ls_opt <- function(ls_dir = "data/alabus_ls") {
  files <- fs::dir_ls(ls_dir, recurse = TRUE, glob = "*.rds")
  if (!length(files)) stop("Aucun fichier .rds trouvé dans ", ls_dir)
  message(">>> Fichiers LS trouvés : ", length(files))
  
  purrr::map_dfr(files, function(f) {
    x <- readRDS(f) %>% tibble::as_tibble()
    x$source_file <- f
    x[] <- lapply(x, as.character)
    x
  }) %>% dplyr::distinct()
}

standardize_alabus_ls <- function(df) {
  names(df) <- make.unique(names(df), sep = "___")
  
  if (!"Résultat" %in% names(df)) {
    cand <- intersect(c("resultat", "Resultat", "Résultat"), names(df))
    if (length(cand)) df <- df %>% dplyr::mutate(Résultat = .data[[cand[1]]])
  }
  
  df %>%
    dplyr::mutate(
      season      = suppressWarnings(as.integer(season)),
      yob         = suppressWarnings(as.integer(yob)),
      age         = suppressWarnings(as.integer(age)),
      gender      = as.character(gender),
      event_name  = as.character(event_name),
      event_type  = as.character(event_type),
      category_raw = as.character(category_raw),
      indoor      = to_logical_indoor(indoor),
      Societe     = as.character(Societe),
      Résultat    = as.character(Résultat)
    )
}

fix_duplicate_mark_seconds <- function(df) {
  names(df) <- make.unique(names(df), sep = "___")
  ms_all <- names(df)[grepl("^(mark_seconds|Mark_seconds)($|___\\d+$)", names(df))]
  if (!length(ms_all)) return(df)
  
  df %>%
    dplyr::mutate(
      mark_seconds = suppressWarnings(as.numeric(dplyr::coalesce(!!!rlang::syms(ms_all))))
    ) %>%
    dplyr::select(-dplyr::any_of(setdiff(ms_all, "mark_seconds")))
}

repair_mark_seconds <- function(df) {
  df <- fix_duplicate_mark_seconds(df)
  if (!"mark_seconds" %in% names(df)) df$mark_seconds <- NA_real_
  
  need <- is.na(df$mark_seconds) & !is.na(df$Résultat) & df$Résultat != ""
  if (!any(need)) return(df)
  
  cleaned <- clean_result_time(df$Résultat[need])
  ms2 <- vapply(cleaned, safe_mark_to_seconds, numeric(1))
  df$mark_seconds[need] <- ms2
  df
}

# Load & standardize alabus LS
ls_all_06_25 <- load_all_ls_opt("data/alabus_ls") %>%
  standardize_alabus_ls() %>%
  repair_mark_seconds() %>%
  dplyr::select(-dplyr::matches("^NH"), -dplyr::starts_with("res"), -dplyr::starts_with("Mark_seconds2"))

save_master(ls_all_06_25, "ls_all_06_25")

# 4.4. Legacy PA construction

ls_all_PA <- dplyr::bind_rows(
  standardize_ls_schema(records_LSA_PA),
  standardize_ls_schema(Top50_LSA_PA),
  standardize_ls_schema(Top10_LSA_PA),
  standardize_ls_schema(LSA_records_U10_U14),
  standardize_ls_schema(LSA_records_manual_filling)
) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    season = as.numeric(stringr::str_sub(string = date, start = 1, end = 4)),
    age    = as.numeric(season - yob)
  ) %>%
  tidyr::drop_na(event, resultat)

save_master(ls_all_PA, "ls_all_PA")

# 4.5. Event harmonisation

# Legacy PA event text fixes

ls_all_PA <- ls_all_PA %>%
  dplyr::mutate(
    event = dplyr::case_when(
      event == "50 m. haies" & club_cat %in% c("U16","U18")           ~ "50 m. haies (76.2)",
      event == "50 m. haies" & club_cat %in% c("U20","U23")           ~ "50 m. haies (84.0)",
      event == "60 m. haies" & club_cat %in% c("U16","U18")           ~ "60 m. haies (76.2)",
      event == "60 m. haies" & club_cat %in% c("U20","U23")           ~ "60 m. haies (84.0)",
      
      event %in% c("Décathlon","décathlon") & club_cat %in% c("U23","SEN") & gender == "male" ~ "Décathlon",
      event %in% c("Décathlon","décathlon") & club_cat == "U20" & gender == "male"           ~ "Décathlon (U20)",
      event %in% c("Décathlon","décathlon") & club_cat == "U18" & gender == "male"           ~ "Décathlon (U18)",
      event %in% c("Décathlon","décathlon") & gender == "female"                             ~ "Décathlon (femmes)",
      
      event %in% c("Heptathlon","heptathlon") & club_cat %in% c("U23","SEN") & gender == "male" & indoor ~ "Heptathlon (hommes)",
      event %in% c("Heptathlon","heptathlon") & club_cat == "U20" & gender == "male"                  ~ "Heptathlon (U20)",
      event %in% c("Heptathlon","heptathlon") & club_cat == "U18" & gender == "female"                ~ "Heptathlon (U18)",
      event %in% c("Heptathlon","heptathlon") & gender == "female"                                   ~ "Heptathlon (femmes)",
      
      event == "Poids" & club_cat %in% c("U16","U18") & gender == "female"           ~ "Poids (3 kg)",
      event == "Poids" & club_cat %in% c("U20","U23","SEN") & gender == "female"     ~ "Poids (4 kg)",
      
      event == "perche"                          ~ "Perche",
      
      event %in% c("Triathlon sprint (60-100-200 METRES)",
                   "Triathlon sprint (60-100-200 m.)",
                   "Triathlon sprint")          ~ "Triathlon sprint (60-100-200 m.)",
      
      event %in% c("Pentathlon","pentathlon") & club_cat %in% c("U23","SEN") & gender == "male" & indoor ~ "Pentathlon (hommes)",
      event %in% c("Pentathlon","pentathlon") & club_cat == "U20" & gender == "male"                   ~ "Pentathlon (U20)",
      event %in% c("Pentathlon","pentathlon") & club_cat == "U18"                                     ~ "Pentathlon (U18)",
      event %in% c("Pentathlon","pentathlon") & gender == "female"                                    ~ "Pentathlon (femmes)",
      
      TRUE ~ event
    )
  )

# Alabus event_name harmonisation

ls_all_06_25 <- ls_all_06_25 %>%
  dplyr::mutate(
    event_name = dplyr::case_when(
      event_name %in% c("50H_76.2_U16W_U14M","50H_76.2_U18W") ~ "50H_76.2",
      event_name %in% c("60H_76.2_U12","60H_76.2_U14W",
                        "60H_76.2_U16W_U14M","60H_76.2_U18W") ~ "60H_76.2",
      event_name %in% c("60H_84.0","60H_84")                  ~ "60H_84.0",
      TRUE                                                    ~ event_name
    )
  )

# Remove event_type from both before mapping (will be reintroduced via event_names)
ls_all_06_25$event_type <- NULL
ls_all_PA$event_type    <- NULL
ls_all_PA$event_name    <- NULL

# Remove accents once
ls_all_PA$event <- iconv(ls_all_PA$event, to = "ASCII//TRANSLIT")

# 4.6. Event metadata mapping

event_names <- readr::read_delim(
  "~/UNIL - Doctorat/1_Research_data/1_Data_collection/1_Scraping/Extractor_LSA/event_names.csv",
  delim = ";",
  show_col_types = FALSE
)

# Alabus: event_name code
lookup_alabus <- event_names %>%
  dplyr::filter(!is.na(event_name), event_name != "") %>%
  dplyr::distinct(event_name, .keep_all = TRUE) %>%
  dplyr::select(event_name, event_display, event_type, metric, unit, better_is_lower)

# Legacy: event code
lookup_legacy <- event_names %>%
  dplyr::filter(!is.na(event), event != "") %>%
  dplyr::distinct(event, .keep_all = TRUE) %>%
  dplyr::select(event, event_display, event_type, metric, unit, better_is_lower)

# Map for alabus
ls_all_06_25 <- ls_all_06_25 %>%
  dplyr::select(-dplyr::any_of(c("event_display","event_type","metric","unit","better_is_lower"))) %>%
  dplyr::left_join(lookup_alabus, by = "event_name") %>%
  dplyr::mutate(
    event = dplyr::coalesce(event_display, event_name),
    better_is_lower = dplyr::case_when(
      metric %in% c("time")                      ~ TRUE,
      metric %in% c("distance","height","points") ~ FALSE,
      TRUE                                       ~ better_is_lower
    )
  )

# Map for legacy PA
ls_all_PA$event <- iconv(ls_all_PA$event, to = "ASCII//TRANSLIT")

ls_all_PA <- ls_all_PA %>%
  dplyr::select(-dplyr::any_of(c("event_display","event_type","metric","unit","better_is_lower"))) %>%
  dplyr::left_join(lookup_legacy, by = "event") %>%
  dplyr::mutate(
    event = dplyr::coalesce(event_display, event),
    better_is_lower = dplyr::case_when(
      metric %in% c("time")                      ~ TRUE,
      metric %in% c("distance","height","points") ~ FALSE,
      TRUE                                       ~ better_is_lower
    )
  )

# 4.7. Final merge, marks and dedup

# Standardize each input source
ls_all_PA_std <- ls_all_PA %>%
  canonicalize_ls()

ls_all_06_25_std <- ls_all_06_25 %>%
  dplyr::mutate(source = "alabus_ls") %>%
  canonicalize_ls()

# Merge -> compute marks -> recompute category -> dedup (one pass)
ls_all_time_2025_pre <- dplyr::bind_rows(ls_all_06_25_std, ls_all_PA_std) %>%
  canonicalize_ls() %>%
  compute_marks_from_result() %>%
  dplyr::filter(resultat != "Vacant") %>%
  attach_direction() %>%
  clean_master_duplicates() %>%
  dplyr::mutate(
    season = dplyr::coalesce(season, lubridate::year(date))
  ) %>%
  flag_and_fix_age_cat(min_age_ok = 6L)

ls_all_time_2025 <- ls_all_time_2025_pre %>%
  dedup_ls_yob() %>%
  dplyr::select(
    -dplyr::any_of(c(
      "Societe", "source_file", "metric_effective",
      "No", "Résultat", "Vent", "Rang", "Nom", "Nat.", "Date naiss.",
      "Compétition", "Lieu", "Date",
      "athlete_first", "athlete_last", "club_cat_raw"
    ))
  )

# SB only
ls_all_time_2025_sb <- make_ls_sb(ls_all_time_2025)

# Sanity check (must be 0)
ls_all_time_2025_sb %>%
  dplyr::count(event, gender, indoor, season, athlete = normalize_name(athlete), sort = TRUE) %>%
  dplyr::filter(n > 1)

# Save final master
save_master(ls_all_time_2025_sb, "ls_all_time_2025_sb")

#=== 5. Outputs computations

#=== 5. Outputs computations (SIMPLE: use club_cat everywhere)

# 5.1. Directories, helpers and functions --------------------------------

ensure_output_dirs <- function(base = "data/ls_outputs") {
  fs::dir_create(base)
  fs::dir_create(file.path(base, "01_seasons"))
  fs::dir_create(file.path(base, "02_records"))
  fs::dir_create(file.path(base, "03_top50"))
  fs::dir_create(file.path(base, "04_top10"))
  fs::dir_create(file.path(base, "05_alabus_all"))
  invisible(TRUE)
}

# Excel sheet names
safe_sheet_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[:\\\\/\\?\\*\\[\\]]", "-")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x)
  ifelse(nchar(x) > 31, substr(x, 1, 31), x)
}

write_xlsx <- function(path, df) {
  openxlsx::write.xlsx(df, path, overwrite = TRUE)
  message(">>> wrote: ", path)
}

write_xlsx_sheets <- function(path, sheets_named_list) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets_named_list)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets_named_list[[nm]])
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  message(">>> wrote: ", path)
}

save_rds <- function(path, obj) {
  saveRDS(obj, path)
  message(">>> wrote: ", path)
}

# Standard export view (category = club_cat)
export_view <- function(df) {
  df %>%
    dplyr::mutate(
      event    = dplyr::coalesce(event_display, event),
      club_cat = as.character(club_cat),
      gender   = as.character(gender),
      indoor   = as.logical(indoor)
    ) %>%
    dplyr::select(
      event,
      resultat,
      mark,
      athlete, yob,
      gender,
      date, lieu,
      indoor,
      club_cat,
      dplyr::any_of(c("club_cat_raw", "age", "season", "source")) # optional but useful
    )
}

# Direction from better_is_lower (already curated by your mapping)
attach_direction <- function(df) {
  df %>%
    dplyr::mutate(
      direction = dplyr::case_when(
        is.na(better_is_lower) ~ "min",
        better_is_lower        ~ "min",
        TRUE                   ~ "max"
      )
    )
}

# Athlete id (best-effort: DOB if available else YOB)
athlete_id_key <- function(df) {
  df %>%
    dplyr::mutate(
      athlete_norm = normalize_name(athlete),
      dob_norm     = normalize_text(`Date naiss.`),
      yob_norm     = dplyr::if_else(is.na(yob), "NA", as.character(yob)),
      athlete_id   = dplyr::if_else(dob_norm != "",
                                    paste0(athlete_norm, "|", dob_norm),
                                    paste0(athlete_norm, "|", yob_norm))
    )
}

top_n_per_group <- function(df, group_cols, n = 50) {
  df %>%
    dplyr::filter(!is.na(mark)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::arrange(
      dplyr::if_else(direction == "min", mark, -mark),
      dplyr::desc(!is.na(date)),
      dplyr::desc(!is.na(lieu)),
      .by_group = TRUE
    ) %>%
    dplyr::mutate(rank_out = dplyr::row_number()) %>%
    dplyr::filter(rank_out <= n) %>%
    dplyr::ungroup()
}

# Unique athlete within group (one perf per athlete)
top_n_unique_athlete <- function(df, group_cols, n = 50) {
  df %>%
    dplyr::filter(!is.na(mark)) %>%
    athlete_id_key() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::arrange(
      dplyr::if_else(direction == "min", mark, -mark),
      dplyr::desc(!is.na(date)),
      dplyr::desc(!is.na(lieu)),
      .by_group = TRUE
    ) %>%
    dplyr::distinct(athlete_id, .keep_all = TRUE) %>%
    dplyr::mutate(rank_out = dplyr::row_number()) %>%
    dplyr::filter(rank_out <= n) %>%
    dplyr::ungroup() %>%
    dplyr::select(-athlete_norm, -dob_norm, -yob_norm, -athlete_id)
}

pick_best_per_group <- function(df, group_cols) {
  df %>%
    dplyr::filter(!is.na(mark)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::arrange(
      dplyr::if_else(direction == "min", mark, -mark),
      dplyr::desc(!is.na(date)),
      dplyr::desc(!is.na(lieu)),
      .by_group = TRUE
    ) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
}

# Split workbook: one sheet per club_cat × gender × indoor/outdoor
split_to_sheets <- function(df) {
  df2 <- df %>%
    dplyr::mutate(
      club_cat   = dplyr::coalesce(club_cat, "OPEN"),
      gender     = dplyr::coalesce(gender, "unknown"),
      indoor     = dplyr::if_else(is.na(indoor), FALSE, indoor),
      indoor_lbl = dplyr::if_else(indoor, "indoor", "outdoor"),
      sheet_key  = paste(club_cat, gender, indoor_lbl, sep = "_")
    )
  
  sheets <- split(df2 %>% dplyr::select(-indoor_lbl), df2$sheet_key)
  sheets <- lapply(sheets, function(x) x %>% dplyr::select(-sheet_key))
  
  nm <- names(sheets)
  names(sheets) <- vapply(nm, safe_sheet_name, character(1))
  sheets
}

# Generic exporter: global + split + rds
export_bundle <- function(df, out_dir, base_name) {
  fs::dir_create(out_dir)
  
  df_out <- export_view(df)
  
  save_rds(file.path(out_dir, paste0(base_name, ".rds")), df_out)
  write_xlsx(file.path(out_dir, paste0(base_name, ".xlsx")), df_out)
  
  sheets <- split_to_sheets(df_out)
  write_xlsx_sheets(file.path(out_dir, paste0(base_name, "_SPLIT.xlsx")), sheets)
  
  invisible(df_out)
}

# 5.2. OUTPUT #1: Seasons -----------------------------------------------

export_seasons <- function(df_all_time,
                           years = 2006:max(df_all_time$season, na.rm = TRUE),
                           out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "01_seasons")
  
  for (yr in years) {
    df_yr <- df_all_time %>% dplyr::filter(season == yr)
    if (nrow(df_yr) == 0) next
    export_bundle(df_yr, out_dir, base_name = paste0("LSA_season_", yr))
  }
  
  message(">>> Seasons exported: ", min(years), " to ", max(years))
  invisible(TRUE)
}

# 5.3. OUTPUT #2: Records (by club_cat + OPEN) --------------------------

export_records <- function(df_all_time,
                           up_to_year = max(df_all_time$season, na.rm = TRUE),
                           out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "02_records")
  
  df <- df_all_time %>%
    dplyr::filter(!is.na(season), season <= up_to_year) %>%
    attach_direction()
  
  # records by club_cat
  rec_cat <- pick_best_per_group(df, c("event", "club_cat", "gender", "indoor"))
  
  # OPEN records: ignore category
  rec_open <- pick_best_per_group(df, c("event", "gender", "indoor")) %>%
    dplyr::mutate(club_cat = "OPEN")
  
  records_all <- dplyr::bind_rows(rec_cat, rec_open) %>%
    dplyr::distinct(event, club_cat, gender, indoor, athlete, yob, date, lieu, mark, .keep_all = TRUE)
  
  export_bundle(records_all, out_dir, base_name = paste0("Records_up_to_", up_to_year))
}

# 5.4. OUTPUT #3: Top50 OPEN (event × gender × indoor) ------------------

export_top50 <- function(df_all_time,
                         up_to_year = max(df_all_time$season, na.rm = TRUE),
                         out_base = "data/ls_outputs",
                         unique_athlete = FALSE) {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "03_top50")
  
  df <- df_all_time %>%
    dplyr::filter(!is.na(season), season <= up_to_year) %>%
    attach_direction() %>%
    dplyr::mutate(club_cat = "OPEN")  # force OPEN
  
  top50 <- if (!unique_athlete) {
    top_n_per_group(df, group_cols = c("event", "gender", "indoor"), n = 50)
  } else {
    top_n_unique_athlete(df, group_cols = c("event", "gender", "indoor"), n = 50)
  }
  
  export_bundle(top50, out_dir, base_name = paste0("Top50_OPEN_up_to_", up_to_year))
}

# 5.5. OUTPUT #4: Top10 by category (event × club_cat × gender × indoor) -

export_top10 <- function(df_all_time,
                         up_to_year = max(df_all_time$season, na.rm = TRUE),
                         out_base = "data/ls_outputs",
                         unique_athlete = FALSE) {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "04_top10")
  
  df <- df_all_time %>%
    dplyr::filter(!is.na(season), season <= up_to_year) %>%
    attach_direction()
  
  top10 <- if (!unique_athlete) {
    top_n_per_group(df, group_cols = c("event", "club_cat", "gender", "indoor"), n = 10)
  } else {
    top_n_unique_athlete(df, group_cols = c("event", "club_cat", "gender", "indoor"), n = 10)
  }
  
  export_bundle(top10, out_dir, base_name = paste0("Top10_by_category_up_to_", up_to_year))
}

# 5.6. OUTPUT #5: All alabus scraped (raw-ish export) --------------------

export_alabus_all <- function(df_alabus_all, out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "05_alabus_all")
  
  df_alabus_all <- df_alabus_all %>%
    dplyr::mutate(
      club_cat = dplyr::coalesce(club_cat, "OPEN"),
      mark     = dplyr::coalesce(mark, mark_seconds)
    )
  
  export_bundle(df_alabus_all, out_dir, base_name = "Alabus_all_scraped")
}

# 5.7. Run EVERYTHING -----------------------------------------------------

run_all_outputs <- function(df_all_time, df_alabus_all,
                            seasons_from = 2006,
                            out_base = "data/ls_outputs",
                            top50_unique_athlete = FALSE,
                            top10_unique_athlete = FALSE) {
  
  ensure_output_dirs(out_base)
  
  years <- seasons_from:max(df_all_time$season, na.rm = TRUE)
  export_seasons(df_all_time, years = years, out_base = out_base)
  
  up_to <- max(df_all_time$season, na.rm = TRUE)
  export_records(df_all_time, up_to_year = up_to, out_base = out_base)
  export_top50(df_all_time, up_to_year = up_to, out_base = out_base, unique_athlete = top50_unique_athlete)
  export_top10(df_all_time, up_to_year = up_to, out_base = out_base, unique_athlete = top10_unique_athlete)
  
  export_alabus_all(df_alabus_all, out_base = out_base)
  
  message(">>> ALL OUTPUTS DONE (up to ", up_to, ")")
  invisible(TRUE)
}

# Example run:
run_all_outputs(
  df_all_time   = ls_all_time_2025_sb,  # <- votre master SB
  df_alabus_all = all_06_25,            # <- votre alabus complet
  seasons_from  = 2006,
  out_base      = "data/ls_outputs",
  top50_unique_athlete = FALSE,
  top10_unique_athlete = FALSE
)

































# 5.1. Directories, helpers and functions

# Directories

ensure_output_dirs <- function(base = "data/ls_outputs") {
  dir_create(base)
  dir_create(file.path(base, "01_seasons"))
  dir_create(file.path(base, "02_records"))
  dir_create(file.path(base, "03_top50"))
  dir_create(file.path(base, "04_top10"))
  dir_create(file.path(base, "05_alabus_all"))
  invisible(TRUE)
}

# Safe Excel helpers

safe_sheet_name <- function(x) {
  # Excel: max 31 chars, forbid : \ / ? * [ ]
  x <- as.character(x)
  x <- str_replace_all(x, "[:\\\\/\\?\\*\\[\\]]", "-")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  ifelse(nchar(x) > 31, substr(x, 1, 31), x)
}

write_xlsx <- function(path, df) {
  openxlsx::write.xlsx(df, path, overwrite = TRUE)
  message(">>> wrote: ", path)
}

write_xlsx_sheets <- function(path, sheets_named_list) {
  wb <- createWorkbook()
  for (nm in names(sheets_named_list)) {
    addWorksheet(wb, nm)
    writeData(wb, nm, sheets_named_list[[nm]])
  }
  saveWorkbook(wb, path, overwrite = TRUE)
  message(">>> wrote: ", path)
}

save_rds <- function(path, obj) {
  saveRDS(obj, path)
  message(">>> wrote: ", path)
}

# Standard export view

export_view <- function(df) {
  # minimal display variables (plus event + mark for usability)
  df %>%
    mutate(
      event = coalesce(event_display, event),
      club_cat_raw = as.character(club_cat_raw),
      gender = as.character(gender),
      indoor = as.logical(indoor)
    ) %>%
    select(
      event,
      resultat,
      mark,
      athlete, yob,
      gender,
      date, lieu,
      indoor,
      club_cat_raw
    )
}

# Ranking direction
# Use your curated better_is_lower (time TRUE, others FALSE)
attach_direction <- function(df) {
  df %>%
    mutate(
      # TRUE => min (time); FALSE => max (distance/points); NA => fallback to "min" for safety on track
      direction = case_when(
        is.na(better_is_lower) ~ "min",
        better_is_lower        ~ "min",
        TRUE                   ~ "max"
      )
    )
}

top_n_per_group <- function(df, group_cols, n = 50) {
  df %>%
    filter(!is.na(mark)) %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(
      if_else(direction == "min", mark, -mark),
      desc(!is.na(date)),
      desc(!is.na(lieu))
    ) %>%
    mutate(rank_out = row_number()) %>%
    filter(rank_out <= n) %>%
    ungroup()
}

top_n_unique_athlete <- function(df, group_cols, n = 50) {
  df <- df %>%
    filter(!is.na(mark)) %>%
    athlete_id_key() %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(
      if_else(direction == "min", mark, -mark),
      desc(!is.na(date)),
      desc(!is.na(lieu))
    ) %>%
    # keep only best performance per athlete within the group
    distinct(athlete_id, .keep_all = TRUE) %>%
    mutate(rank_out = row_number()) %>%
    filter(rank_out <= n) %>%
    ungroup() %>%
    select(-athlete_norm, -dob_norm, -yob_norm, -athlete_id)
  
  df
}


pick_best_per_group <- function(df, group_cols) {
  df %>%
    filter(!is.na(mark)) %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(
      if_else(direction == "min", mark, -mark),
      desc(!is.na(date)),
      desc(!is.na(lieu))
    ) %>%
    slice(1L) %>%
    ungroup()
}

# Split workbook: one sheet per category × gender × indoor

split_to_sheets <- function(df, split_cols = c("club_cat_raw", "gender", "indoor")) {
  
  df2 <- df %>%
    mutate(
      club_cat_raw = coalesce(club_cat_raw, "OPEN"),
      gender = coalesce(gender, "unknown"),
      indoor = if_else(is.na(indoor), FALSE, indoor),
      indoor_lbl = if_else(indoor, "indoor", "outdoor")
    )
  
  # build sheet key
  keys <- df2 %>%
    mutate(sheet_key = paste(club_cat_raw, gender, indoor_lbl, sep = "_")) %>%
    pull(sheet_key)
  
  df2$sheet_key <- keys
  
  sheets <- split(df2 %>% select(-indoor_lbl), df2$sheet_key)
  
  # clean names + drop helper
  sheets <- lapply(sheets, function(x) x %>% select(-sheet_key))
  nm <- names(sheets)
  names(sheets) <- vapply(nm, safe_sheet_name, character(1))
  
  sheets
}

# Generic exporter (global + split + rds)

export_bundle <- function(df, out_dir, base_name) {
  dir_create(out_dir)
  
  # GLOBAL (minimal view)
  df_out <- export_view(df)
  
  # Save global RDS + XLSX
  save_rds(file.path(out_dir, paste0(base_name, ".rds")), df_out)
  write_xlsx(file.path(out_dir, paste0(base_name, ".xlsx")), df_out)
  
  # Save split workbook
  sheets <- split_to_sheets(df_out, split_cols = c("club_cat_raw", "gender", "indoor"))
  write_xlsx_sheets(file.path(out_dir, paste0(base_name, "_SPLIT.xlsx")), sheets)
  
  invisible(df_out)
}

# Athlete id

athlete_id_key <- function(df) {
  df %>%
    mutate(
      athlete_norm = normalize_name(athlete),
      dob_norm     = normalize_text(`Date naiss.`),
      yob_norm     = if_else(is.na(yob), "NA", as.character(yob)),
      athlete_id   = if_else(dob_norm != "", paste0(athlete_norm, "|", dob_norm),
                             paste0(athlete_norm, "|", yob_norm))
    )
}

# 5.2. OUTPUT #1: Season performances from 2006 to 2025 (and after)

export_seasons <- function(df_all_time, years = 2006:max(df_all_time$season, na.rm = TRUE),
                           out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "01_seasons")
  
  for (yr in years) {
    df_yr <- df_all_time %>% filter(season == yr)
    if (nrow(df_yr) == 0) next
    export_bundle(df_yr, out_dir, base_name = paste0("LSA_season_", yr))
  }
  
  message(">>> Seasons exported: ", min(years), " to ", max(years))
  invisible(TRUE)
}

# 5.3. OUTPUT #2: Club records per category x gender x indoor/outdoor
# + OPEN records (male/female OPEN across all categories)

export_records <- function(df_all_time, up_to_year = max(df_all_time$season, na.rm = TRUE),
                           out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "02_records")
  
  df <- df_all_time %>%
    filter(!is.na(season), season <= up_to_year) %>%
    attach_direction()
  
  # records by category
  rec_cat <- pick_best_per_group(df, c("event", "club_cat_raw", "gender", "indoor"))
  
  # OPEN records: ignore category => club_cat_raw = "OPEN"
  rec_open <- pick_best_per_group(df, c("event", "gender", "indoor")) %>%
    mutate(club_cat_raw = "OPEN")
  
  records_all <- bind_rows(rec_cat, rec_open) %>%
    distinct(event, club_cat_raw, gender, indoor, athlete, yob, date, lieu, mark, .keep_all = TRUE)
  
  export_bundle(records_all, out_dir, base_name = paste0("Records_up_to_", up_to_year))
}

# 5.4. OUTPUT #3 Top 50 male/female × indoor/outdoor (OPEN categories)

export_top50 <- function(df_all_time, up_to_year = max(df_all_time$season, na.rm = TRUE),
                         out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "03_top50")
  
  df <- df_all_time %>%
    filter(!is.na(season), season <= up_to_year) %>%
    attach_direction() %>%
    mutate(club_cat_raw = "OPEN")  # force OPEN for splitting
  
  top50 <- top_n_per_group(df, group_cols = c("event", "gender", "indoor"), n = 50)
  
  export_bundle(top50, out_dir, base_name = paste0("Top50_OPEN_up_to_", up_to_year))
}

export_top50_overall <- function(df_all_time, up_to_year, out_base = "data/ls_outputs") {
  ensure_output_dirs(out_base)
  
  df <- prep_for_outputs(df_all_time, up_to_year)
  
  top50 <- top_n_unique_athlete(
    df,
    group_cols = c("event", "gender", "indoor"),
    n = 50
  )
  
  sheets <- list(
    male_outdoor   = top50 %>% filter(gender == "male",   indoor == FALSE),
    male_indoor    = top50 %>% filter(gender == "male",   indoor == TRUE),
    female_outdoor = top50 %>% filter(gender == "female", indoor == FALSE),
    female_indoor  = top50 %>% filter(gender == "female", indoor == TRUE)
  )
  
  out_path <- file.path(out_base, "Top50", glue::glue("Top50_overall_up_to_{up_to_year}.xlsx"))
  write_xlsx_sheets(out_path, sheets)
  
  invisible(top50)
}

# 5.5. OUTPUT #4: Top 10 per category × gender × indoor/outdoor

export_top10 <- function(df_all_time, up_to_year = max(df_all_time$season, na.rm = TRUE),
                         out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "04_top10")
  
  df <- df_all_time %>%
    filter(!is.na(season), season <= up_to_year) %>%
    attach_direction()
  
  top10 <- top_n_per_group(df, group_cols = c("event", "club_cat_raw", "gender", "indoor"), n = 10)
  
  export_bundle(top10, out_dir, base_name = paste0("Top10_by_category_up_to_", up_to_year))
}

export_top10_by_category <- function(df_all_time, up_to_year, out_base = "data/ls_outputs") {
  ensure_output_dirs(out_base)
  
  df <- prep_for_outputs(df_all_time, up_to_year)
  
  top10 <- top_n_unique_athlete(
    df,
    group_cols = c("event", "club_cat_raw", "gender", "indoor"),
    n = 10
  )
  
  sheets <- list(
    male_outdoor   = top10 %>% filter(gender == "male",   indoor == FALSE),
    male_indoor    = top10 %>% filter(gender == "male",   indoor == TRUE),
    female_outdoor = top10 %>% filter(gender == "female", indoor == FALSE),
    female_indoor  = top10 %>% filter(gender == "female", indoor == TRUE)
  )
  
  out_path <- file.path(out_base, "Top10", glue::glue("Top10_by_category_up_to_{up_to_year}.xlsx"))
  write_xlsx_sheets(out_path, sheets)
  
  invisible(top10)
}

# 5.6. OUTPUT #5 All alabus scraped data 2006-2025 (and after)
# (expects the cleaned "all alabus" df you already build)

export_alabus_all <- function(df_alabus_all, out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  out_dir <- file.path(out_base, "05_alabus_all")
  
  # You want the same "minimal display" export concept.
  # Ensure expected columns exist:
  df_alabus_all <- df_alabus_all %>%
    mutate(
      club_cat_raw = coalesce(club_cat_raw, club_cat),
      mark = coalesce(mark, mark_seconds)  # fallback if mark not present
    )
  
  export_bundle(df_alabus_all, out_dir, base_name = "Alabus_all_scraped")
}

# 5.7. Run EVERYTHING

run_all_outputs <- function(df_all_time, df_alabus_all,
                            seasons_from = 2006,
                            out_base = "data/ls_outputs") {
  
  ensure_output_dirs(out_base)
  
  # 1) seasons (2006..max)
  years <- seasons_from:max(df_all_time$season, na.rm = TRUE)
  export_seasons(df_all_time, years = years, out_base = out_base)
  
  # 2-4) cumulative up to latest
  up_to <- max(df_all_time$season, na.rm = TRUE)
  export_records(df_all_time, up_to_year = up_to, out_base = out_base)
  export_top50(df_all_time, up_to_year = up_to, out_base = out_base)
  export_top10(df_all_time, up_to_year = up_to, out_base = out_base)
  
  # 5) raw alabus all
  export_alabus_all(df_alabus_all, out_base = out_base)
  
  message(">>> ALL OUTPUTS DONE (up to ", up_to, ")")
  invisible(TRUE)
}

run_all_outputs(
  df_all_time   = ls_all_time_2025,
  df_alabus_all = all_06_25,
  seasons_from  = 2006,
  out_base      = "data/ls_outputs"
)





# WORKING ODL
#=== 5. Load and save all alabus scraped data

# Load all alabus
all_06_25 <- load_all_ls_opt("data/alabus_raw") %>%
  standardize_alabus_ls() %>%
  repair_mark_seconds() %>%
  select(-matches("^NH"), -starts_with("res"), -starts_with("Mark_seconds2"))

# Save
save_raw <- function(df, name, dir = "data/alabus_raw", xlsx = TRUE) {
  dir_create(dir)
  
  rds_path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(df, rds_path)
  message(">>> Saved: ", rds_path, " (n=", nrow(df), ")")
  
  if (xlsx) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Le package 'openxlsx' est nécessaire.")
    }
    xlsx_path <- file.path(dir, paste0(name, ".xlsx"))
    openxlsx::write.xlsx(df, xlsx_path, overwrite = TRUE)
    message(">>> Saved: ", xlsx_path, " (n=", nrow(df), ")")
  }
  
  invisible(df)
}

save_raw(all_06_25, "all_swa_06_25")

#=== 6. Outputs functions

library(dplyr)
library(stringr)
library(openxlsx)
library(fs)

# ---------- output dirs ----------
ensure_output_dirs <- function(base = "data/ls_outputs") {
  dir_create(file.path(base, "season"))
  dir_create(file.path(base, "Top50"))
  dir_create(file.path(base, "records"))
  dir_create(file.path(base, "Top10"))
  invisible(TRUE)
}

# ---------- event ranking direction ----------
# time -> lower is better
# distance/height/points -> higher is better
is_points_result <- function(x) {
  s <- tolower(as.character(x))
  str_detect(s, "pts|points|point|tétrathlon|tetrathlon|pentathlon|heptathlon|decathlon|triathlon|athlon|hauteur|longueur|perche|poids|disque|marteau|javelot")
}

is_time_result <- function(x) {
  s <- as.character(x)
  str_detect(s, ":|\\d+'\\d|\\d+h\\d+")  # 1:48.59 ; 1'48.59 ; 1h02...
}

# classify per EVENT (more stable than row-by-row):
event_direction_table <- function(df) {
  df %>%
    mutate(
      time_flag   = is_time_result(resultat),
      points_flag = is_points_result(resultat)
    ) %>%
    group_by(event) %>%
    summarise(
      prop_time   = mean(time_flag, na.rm = TRUE),
      prop_points = mean(points_flag, na.rm = TRUE),
      direction = case_when(
        prop_points >= 0.2 ~ "max",     # points events
        prop_time   >= 0.2 ~ "min",     # timed events
        TRUE              ~ "max"       # default for jumps/throws/etc.
      ),
      .groups = "drop"
    )
}

# attach direction to df (min/max)
attach_event_direction <- function(df) {
  dir_tbl <- event_direction_table(df)
  df %>% left_join(dir_tbl %>% select(event, direction), by = "event")
}

# ---------- ranking within groups ----------
pick_best_per_group <- function(df, group_cols) {
  # df must have: mark_seconds, direction
  df %>%
    filter(!is.na(mark_seconds)) %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(
      # if time: ascending, else descending
      if_else(direction == "min", mark_seconds, -mark_seconds),
      # tie-breakers: prefer having date + place
      desc(!is.na(date)),
      desc(!is.na(lieu))
    ) %>%
    slice(1) %>%
    ungroup()
}

top_n_per_group <- function(df, group_cols, n = 50) {
  df %>%
    filter(!is.na(mark_seconds)) %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(if_else(direction == "min", mark_seconds, -mark_seconds),
            desc(!is.na(date)),
            desc(!is.na(lieu))) %>%
    mutate(rank_out = row_number()) %>%
    filter(rank_out <= n) %>%
    ungroup()
}

# ---------- safe writer ----------
write_xlsx_sheets <- function(path, sheets_named_list) {
  wb <- createWorkbook()
  for (nm in names(sheets_named_list)) {
    addWorksheet(wb, nm)
    writeData(wb, nm, sheets_named_list[[nm]])
  }
  saveWorkbook(wb, path, overwrite = TRUE)
  message(">>> wrote: ", path)
}




#=== 6. Output #1: Season performances

export_season_performances <- function(df_all_time, year, out_base = "data/ls_outputs") {
  ensure_output_dirs(out_base)
  
  df_season <- df_all_time %>%
    filter(season == year)
  
  # Save as both RDS + XLSX for future reuse / share
  rds_path  <- file.path(out_base, "season", glue::glue("ls_all_{year}.rds"))
  xlsx_path <- file.path(out_base, "season", glue::glue("ls_all_{year}.xlsx"))
  
  saveRDS(df_season, rds_path)
  openxlsx::write.xlsx(df_season, xlsx_path, overwrite = TRUE)
  
  message(">>> season export done (", year, ") n=", nrow(df_season))
  invisible(df_season)
}


#=== 7. Output #2: Top 50 male/female × indoor/outdoor (pure best 50, across all categories)

export_top50_overall <- function(df_all_time, up_to_year, out_base = "data/ls_outputs") {
  ensure_output_dirs(out_base)
  
  df <- df_all_time %>%
    filter(!is.na(season), season <= up_to_year) %>%
    attach_event_direction()
  
  # groups = event + gender + indoor (NOT club_cat)
  top50 <- top_n_per_group(
    df,
    group_cols = c("event", "gender", "indoor"),
    n = 50
  )
  
  # split into 4 sheets for readability
  sheets <- list(
    male_outdoor   = top50 %>% filter(gender == "male",   indoor == FALSE),
    male_indoor    = top50 %>% filter(gender == "male",   indoor == TRUE),
    female_outdoor = top50 %>% filter(gender == "female", indoor == FALSE),
    female_indoor  = top50 %>% filter(gender == "female", indoor == TRUE)
  )
  
  out_path <- file.path(out_base, "Top50", glue::glue("Top50_overall_up_to_{up_to_year}.xlsx"))
  write_xlsx_sheets(out_path, sheets)
  
  invisible(top50)
}

#=== 8. Output #3: Club records per category × gender × indoor/outdoor
# Male and female category are "open": pure best best, across all categories

export_records_by_category <- function(df_all_time, up_to_year, out_base = "data/ls_outputs") {
  ensure_output_dirs(out_base)
  
  df <- df_all_time %>%
    filter(!is.na(season), season <= up_to_year) %>%
    attach_event_direction()
  
  records <- pick_best_per_group(
    df,
    group_cols = c("event", "club_cat", "gender", "indoor")
  )
  
  # workbook split by gender+indoor; inside sheets you still have club_cat column
  sheets <- list(
    male_outdoor   = records %>% filter(gender == "male",   indoor == FALSE),
    male_indoor    = records %>% filter(gender == "male",   indoor == TRUE),
    female_outdoor = records %>% filter(gender == "female", indoor == FALSE),
    female_indoor  = records %>% filter(gender == "female", indoor == TRUE)
  )
  
  out_path <- file.path(out_base, "records", glue::glue("Records_by_category_up_to_{up_to_year}.xlsx"))
  write_xlsx_sheets(out_path, sheets)
  
  invisible(records)
}

#=== 9. Output #4: Top 10 per category × gender × indoor/outdoor

export_top10_by_category <- function(df_all_time, up_to_year, out_base = "data/ls_outputs") {
  ensure_output_dirs(out_base)
  
  df <- df_all_time %>%
    filter(!is.na(season), season <= up_to_year) %>%
    attach_event_direction()
  
  top10 <- top_n_per_group(
    df,
    group_cols = c("event", "club_cat", "gender", "indoor"),
    n = 10
  )
  
  sheets <- list(
    male_outdoor   = top10 %>% filter(gender == "male",   indoor == FALSE),
    male_indoor    = top10 %>% filter(gender == "male",   indoor == TRUE),
    female_outdoor = top10 %>% filter(gender == "female", indoor == FALSE),
    female_indoor  = top10 %>% filter(gender == "female", indoor == TRUE)
  )
  
  out_path <- file.path(out_base, "Top10", glue::glue("Top10_by_category_up_to_{up_to_year}.xlsx"))
  write_xlsx_sheets(out_path, sheets)
  
  invisible(top10)
}


# Wrapper

run_ls_outputs <- function(df_all_time, year, out_base = "data/ls_outputs") {
  ensure_output_dirs(out_base)
  
  # 1) season file only for that year
  export_season_performances(df_all_time, year, out_base)
  
  # 2–4) cumulative outputs "up to year" (updated each year)
  export_top50_overall(df_all_time, up_to_year = year, out_base)
  export_records_by_category(df_all_time, up_to_year = year, out_base)
  export_top10_by_category(df_all_time, up_to_year = year, out_base)
  
  message(">>> all outputs updated up to ", year)
  invisible(TRUE)
}

# Usage: 
run_ls_outputs(ls_all_time_2025, year = 2025)




#=== 5. Output 1: LSA seasons performance (MUST BE SAVED in OUTPUTS!!!)

# 5.1. Function to get season performances

get_ls_season_performance <- function(ls_df,
                                      year      = 2025,
                                      club_name = "Lausanne-Sports Athlétisme") {
  if (!"season"  %in% names(ls_df)) stop("ls_df doit contenir une colonne 'season'.")
  if (!"Societe" %in% names(ls_df)) {
    warning("Colonne 'Societe' absente, je ne peux pas filtrer par club. On garde tout.")
    ls_year <- ls_df %>% dplyr::filter(season == year)
  } else {
    ls_year <- ls_df %>%
      dplyr::filter(
        season  == year,
        Societe == club_name
      )
  }
  
  # Si Mark_seconds manque, on le recalcule (au cas où)
  perf_col <- names(ls_year)[
    stringr::str_detect(tolower(names(ls_year)),
                        "perf|résultat|resultat|mark|time")
  ][1]
  
  if (!is.na(perf_col)) {
    ls_year[[perf_col]]   <- as.character(ls_year[[perf_col]])
    ls_year$Mark_seconds <- vapply(ls_year[[perf_col]],
                                   safe_mark_to_seconds,
                                   numeric(1))
  }
  
  ls_year
}

ls_all_2025 <- get_ls_season_performance(ls_all_06_25, year = 2025)

# 5.2. Select the variables of interest

ls_all_2025 <- ls_all_2025 %>%
  select(Résultat, Mark_seconds, Nom, Nat., "Date naiss.", Lieu, Date, gender, event_name, indoor, age, club_cat)

# 5.3. Save
save_ls_master(ls_all_2025, "ls_all_2025")
save_ls_master_xlsx(ls_all_2025, "ls_all_2025")

#=== 6. Top 50 male and female (indoor and outdoor)

#=== 7. Top 10 by category (indoor and outdoor)



#=== 8. Next year refresh pattern (2026)

refresh_all_time <- function(prev_name,
                             new_scraped_df,
                             year,
                             dir = "data/ls_master") {
  
  prev <- load_master(prev_name, dir)
  
  updated <- dplyr::bind_rows(
    prev,
    new_scraped_df %>% dplyr::mutate(source = "alabus_ls")
  ) %>%
    standardize_ls_schema() %>%
    dedup_ls()
  
  save_master(updated, paste0("ls_all_time_", year), dir = dir)
}

# Example later:
# ls_2026 <- load_all_ls("data/alabus_ls_2026") or whatever folder you decide
# refresh_all_time("ls_all_time_2025", ls_2026, 2026)




