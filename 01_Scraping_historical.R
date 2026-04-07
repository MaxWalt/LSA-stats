###############################################################################
#                                                                             #
#   01_Scraping_historical.R                                                  #
#   ──────────────────────────                                                #
#   Scrape Swiss Athletics (alabus) for ALL events, 2006–2025.               #
#                                                                             #
#   YOU ONLY RUN THIS ONCE.                                                   #
#   The results are saved to data/alabus_ls/ and do not need to be           #
#   regenerated unless you want to re-scrape from scratch.                   #
#                                                                             #
#   What this script does:                                                    #
#     Step 1 → Build all URLs (every event × category × year × indoor flag)  #
#     Step 2 → For each event, scrape pages and keep only LSA results        #
#     Step 3 → Save one .rds file per event in data/alabus_ls/               #
#                                                                             #
#   How to run:                                                               #
#     Open this file in RStudio and click "Source", or run:                  #
#     source("01_Scraping_historical.R")                                      #
#                                                                             #
#   Time: several hours (it scrapes ~80 events × 20 years)                   #
#   If it crashes mid-way, just run it again – events already saved          #
#   are skipped automatically (overwrite = FALSE).                            #
#                                                                             #
###############################################################################

source("helpers.R")   # loads all shared functions and folder paths


# =============================================================================
# STEP 1 — BUILD ALL URLS
# =============================================================================
# This creates a table where every row is one URL to scrape.
# Each URL = one event × one category × one year × indoor/outdoor.

# Load the event and category code tables
# (these CSVs link event names to the alabus URL codes)
events_raw     <- readr::read_delim(PATHS$events_csv,     delim = ";", show_col_types = FALSE)
categories_raw <- readr::read_delim(PATHS$categories_csv, delim = ";", show_col_types = FALSE)

events_df <- events_raw %>%
  dplyr::select(Event, Type, disci_code) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Event)

categories_df <- categories_raw %>%
  dplyr::select(Category, Gender, blcat_code) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Gender, Category)

# Build the full URL table
build_alabus_urls <- function(events_df, categories_df, start_year, end_year) {
  tidyr::crossing(
    blyear       = seq.int(start_year, end_year),
    categories_df,
    events_df,
    tibble::tibble(indoor = c(TRUE, FALSE))
  ) %>%
    dplyr::mutate(
      url = dplyr::if_else(
        is.na(blcat_code) | is.na(disci_code),
        NA_character_,
        glue::glue(
          "https://alabus.swiss-athletics.ch/satweb/faces/bestlist.xhtml",
          "?lang=fr&blcat={blcat_code}&blyear={blyear}",
          "&disci={disci_code}&indoor={tolower(as.character(indoor))}&top=5000"
        )
      ),
      missing_code = dplyr::case_when(
        is.na(disci_code) & is.na(blcat_code) ~ "missing_both",
        is.na(disci_code)                     ~ "missing_disci",
        is.na(blcat_code)                     ~ "missing_blcat",
        TRUE                                  ~ "ok"
      )
    ) %>%
    dplyr::arrange(blyear, Gender, Category, Event, indoor)
}

all_urls <- build_alabus_urls(events_df, categories_df,
                               start_year = 2006,
                               end_year   = 2025)

# Quick check: how many URLs are ready?
message("URL summary:")
print(dplyr::count(all_urls, missing_code))

# Keep only the valid, ready-to-scrape URLs
# Filter to allW / allM only (these are the "open" categories that return
# all results — we do not need to scrape every age category separately)
urls_to_scrape <- all_urls %>%
  dplyr::filter(
    missing_code == "ok",
    !is.na(url),
    Category %in% c("allW", "allM")
  ) %>%
  dplyr::select(blyear, Gender, Category, Event, Type, indoor, url)

message("Total URLs to scrape: ", nrow(urls_to_scrape))


# =============================================================================
# STEP 2 & 3 — SCRAPE + SAVE
# =============================================================================
# For each event, we:
#   a) Check which URLs actually have data (fast check before scraping)
#   b) Scrape the pages that have data
#   c) Filter to LSA athletes only
#   d) Save as .rds in data/alabus_ls/<event_name>/

# --- Internal scraping helpers ---

# Check whether a URL has actual results or just an empty page
flag_alabus_urls_with_data <- function(url_df, polite_delay = 0.2, gc_every = 100L) {
  n <- nrow(url_df)
  has_data <- logical(n)
  empty_pattern <- "(?i)veuillez s.?el[ce]tionner une cat|aucun r.?sultat|pas de r.?sultat"

  for (i in seq_len(n)) {
    page_url <- url_df$url[i]
    if (is.na(page_url) || page_url == "") { has_data[i] <- FALSE; next }

    message(sprintf("  [check %d/%d]", i, n))
    ok <- FALSE
    try({
      html <- rvest::read_html(page_url)
      node <- rvest::html_node(html, xpath = '//*[@id="form_anonym:j_idt87"]')
      if (!inherits(node, "xml_missing")) {
        tab <- rvest::html_table(node, fill = TRUE)
        if (!is.null(tab) && nrow(tab) > 0) {
          first_cell <- as.character(tab[1, 1])
          ok <- !is.na(first_cell) && !grepl(empty_pattern, first_cell, perl = TRUE)
        }
      }
      rm(html)
    }, silent = TRUE)

    has_data[i] <- ok
    if (polite_delay > 0) Sys.sleep(polite_delay)
    if (i %% gc_every == 0) { gc(); closeAllConnections() }
  }

  url_df$has_data <- has_data
  url_df
}

# Scrape a set of URLs and return one combined data frame
scrape_alabus_toplists <- function(url_df, polite_delay = 0.2, gc_every = 75L) {
  if (nrow(url_df) == 0) return(tibble::tibble())
  n       <- nrow(url_df)
  results <- vector("list", n)

  for (i in seq_len(n)) {
    page_url <- url_df$url[i]
    if (is.na(page_url) || page_url == "") next

    message(sprintf("  [scrape %d/%d]", i, n))

    tbl <- tryCatch({
      html <- rvest::read_html(page_url)
      node <- rvest::html_node(html, xpath = '//*[@id="form_anonym:j_idt87"]')
      if (inherits(node, "xml_missing")) return(NULL)

      out <- rvest::html_table(node, fill = TRUE)
      if (is.null(out) || nrow(out) == 0) return(NULL)

      # Clean column names
      nm <- names(out)
      nm[is.na(nm) | nm == ""] <- paste0("V", which(is.na(nm) | nm == ""))
      names(out) <- nm

      # Force all columns to character for safe binding later
      out[] <- lapply(out, as.character)

      # Rename club column to "Societe"
      club_idx <- which(stringr::str_detect(tolower(names(out)), "soci.t.|societe|club"))[1]
      if (!is.na(club_idx)) names(out)[club_idx] <- "Societe"

      # Handle the wind column (not always present)
      res_idx  <- which(stringr::str_detect(tolower(names(out)), "résultat|resultat|perf|mark"))[1]
      if (is.na(res_idx)) res_idx <- 2L
      vent_idx <- which(stringr::str_detect(tolower(names(out)), "^vent"))[1]
      if (!is.na(vent_idx)) {
        names(out)[vent_idx] <- "Vent"
      } else {
        out <- tibble::add_column(out, Vent = NA_character_, .after = res_idx)
      }

      # Extract year of birth from "Date naiss." column if present
      if ("Date naiss." %in% names(out)) {
        out$yob <- suppressWarnings(
          as.integer(sub(".*(\\d{4})$", "\\1", out[["Date naiss."]]))
        )
      } else {
        out$yob <- NA_integer_
      }

      # Add metadata columns from the URL table
      out$season       <- suppressWarnings(as.integer(url_df$blyear[i]))
      out$gender       <- url_df$Gender[i]
      out$event_name   <- url_df$Event[i]
      out$event_type   <- url_df$Type[i]
      out$category_raw <- url_df$Category[i]
      out$indoor       <- url_df$indoor[i]

      # Compute age and category at time of performance
      out$age      <- as.integer(out$season) - as.integer(out$yob)
      out$club_cat <- recompute_club_cat(out$season, out$yob)

      out
    }, error = function(e) NULL)

    if (!is.null(tbl) && nrow(tbl)) results[[i]] <- tbl
    if (polite_delay > 0) Sys.sleep(polite_delay)
    if (i %% gc_every == 0) { gc(); closeAllConnections() }
  }

  results <- purrr::compact(results)
  if (!length(results)) return(tibble::tibble())

  out <- dplyr::bind_rows(results)
  out <- suppressMessages(readr::type_convert(out, na = c("", "NA", "N/A")))

  # Keep key columns as character
  for (col in c("gender", "event_name", "event_type", "category_raw", "Societe")) {
    if (col %in% names(out)) out[[col]] <- as.character(out[[col]])
  }

  out
}

# Save only the LSA rows for one event
save_ls_event <- function(scraped_df, event_name, out_dir = PATHS$alabus_ls,
                           club_name = PATHS$club_name) {
  if (is.null(scraped_df) || nrow(scraped_df) == 0) return(invisible(NULL))

  # Filter to LSA only
  if ("Societe" %in% names(scraped_df)) {
    df_ls <- scraped_df %>% dplyr::filter(Societe == club_name)
  } else {
    message("  Warning: no 'Societe' column found for event ", event_name)
    return(invisible(NULL))
  }

  if (nrow(df_ls) == 0) {
    message("  No LSA results for: ", event_name)
    return(invisible(NULL))
  }

  # Save in a sub-folder named after the event
  ev_folder <- file.path(out_dir, safe_name(event_name))
  fs::dir_create(ev_folder)

  seasons  <- df_ls$season[!is.na(df_ls$season)]
  yr_range <- if (length(seasons)) {
    sprintf("%02d_%02d", min(seasons) %% 100, max(seasons) %% 100)
  } else "unknown"

  out_file <- file.path(ev_folder,
    glue::glue("LSA_{safe_name(event_name)}_{yr_range}.rds"))

  saveRDS(df_ls, out_file)
  message("  Saved: ", out_file, "  (", nrow(df_ls), " LSA rows)")
  invisible(df_ls)
}

# Main wrapper: run one event end-to-end
run_one_event <- function(urls_all, event_name,
                           out_dir      = PATHS$alabus_ls,
                           club_name    = PATHS$club_name,
                           polite_delay = 0.2,
                           overwrite    = FALSE) {

  # Check if already done
  ev_folder    <- file.path(out_dir, safe_name(event_name))
  already_done <- fs::dir_exists(ev_folder) && length(fs::dir_ls(ev_folder)) > 0
  if (already_done && !overwrite) {
    message("Already done (skipping): ", event_name)
    return(invisible(NULL))
  }

  # Get URLs for this event
  df_ev <- urls_all %>% dplyr::filter(Event == event_name)
  if (nrow(df_ev) == 0) {
    message("No URLs found for: ", event_name)
    return(invisible(NULL))
  }

  message("\n=== ", event_name, " (", nrow(df_ev), " URLs) ===")

  # Flag → scrape → save
  df_flagged <- flag_alabus_urls_with_data(df_ev, polite_delay = polite_delay)
  df_ok      <- df_flagged %>% dplyr::filter(has_data)

  if (nrow(df_ok) == 0) {
    message("  No pages with data found for: ", event_name)
    return(invisible(NULL))
  }

  scraped <- scrape_alabus_toplists(df_ok, polite_delay = polite_delay)
  save_ls_event(scraped, event_name, out_dir = out_dir, club_name = club_name)
}


# =============================================================================
# RUN — scrape all events
# =============================================================================
# This loops through every event in your events list.
# Events already saved are automatically skipped.
# If the script stops, just run it again from here — it will pick up where
# it left off.

all_events <- unique(urls_to_scrape$Event)
message("\nEvents to process: ", length(all_events))

for (ev in all_events) {
  run_one_event(urls_to_scrape, ev, polite_delay = 0.2)

  # Memory cleanup every ~5 events
  if (which(all_events == ev) %% 5 == 0) {
    gc()
    closeAllConnections()
  }
}

message("\n=== DONE: historical scraping 2006-2025 ===")
message("Results saved in: ", PATHS$alabus_ls)
