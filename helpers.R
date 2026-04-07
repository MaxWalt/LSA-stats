###############################################################################
#                                                                             #
#   helpers.R                                                                 #
#   ─────────                                                                 #
#   Shared functions used by ALL other scripts.                               #
#   Source this at the top of every script with: source("helpers.R")         #
#                                                                             #
#   Contains:                                                                 #
#     1. Libraries                                                            #
#     2. Folder paths (one place to change if you move things)               #
#     3. Performance parsing    (safe_mark_to_seconds)                       #
#     4. Age & category logic   (age_to_cat, recompute_club_cat)             #
#     5. Text & encoding        (normalize_name)                              #
#     6. Date parsing           (parse_date_cell)                            #
#     7. Small utilities        (safe_name, to_logical_indoor, ...)          #
#                                                                             #
###############################################################################


# =============================================================================
# 1. LIBRARIES
# =============================================================================
# Install any missing package with:  install.packages("package_name")

library(dplyr)      # data manipulation
library(tidyr)      # reshaping
library(purrr)      # map / map_dfr
library(readr)      # read_csv, read_delim
library(readxl)     # read_excel
library(stringr)    # str_detect, str_squish, ...
library(tibble)     # tibble, as_tibble
library(glue)       # glue() for building strings
library(fs)         # dir_create, dir_ls
library(openxlsx)   # write.xlsx
library(rvest)      # scraping: read_html, html_node, html_table


# =============================================================================
# 2. FOLDER PATHS
# =============================================================================
# All file paths are defined here.
# If you move your project folder, only change these lines.

PATHS <- list(
  # Input CSVs (event codes, category codes)
  events_csv     = "events_df_from_excel.csv",
  categories_csv = "categories_df_from_excel.csv",
  event_names    = "event_names.csv",

  # Where scraped data lives (one sub-folder per event)
  alabus_ls      = "data/alabus_ls",

  # Where the final merged databases are saved
  master         = "data/ls_master",

  # Where output files (records, top50, etc.) are saved
  outputs        = "data/ls_outputs",

  # Club name exactly as it appears in Swiss Athletics
  club_name      = "Lausanne-Sports Athlétisme"
)


# =============================================================================
# 3. PERFORMANCE PARSING
# =============================================================================

#' Convert a performance string to a single numeric value.
#'
#' - Times  (runs, hurdles)  → seconds        e.g. "1:51.12" → 111.12
#' - Distances / heights     → metres          e.g. "7.32" → 7.32
#' - Points (multi-events)   → integer points  e.g. "4'426 pts" → 4426
#' - Returns NA if the string cannot be parsed.

safe_mark_to_seconds <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(NA_real_)

  # hh:mm:ss(.xx)  e.g. "1:02:30.5"
  if (grepl("^\\d+:\\d{1,2}:\\d{1,2}(\\.\\d+)?$", x)) {
    parts <- strsplit(x, ":")[[1]]
    return(as.numeric(parts[1]) * 3600 +
           as.numeric(parts[2]) * 60  +
           as.numeric(parts[3]))
  }

  # mm:ss(.xx)  e.g. "1:51.12"
  if (grepl("^\\d+:\\d{1,2}(\\.\\d+)?$", x)) {
    parts <- strsplit(x, ":")[[1]]
    return(as.numeric(parts[1]) * 60 + as.numeric(parts[2]))
  }

  # Plain number  e.g. "10.46"  "7.32"  "4426"
  num <- suppressWarnings(as.numeric(gsub(",", ".", x)))
  if (!is.na(num)) return(num)

  NA_real_
}


# =============================================================================
# 4. AGE & CATEGORY LOGIC
# =============================================================================

#' Convert an age (in years) to a Swiss Athletics club category.
#'
#' Uses strict less-than thresholds, which is the Swiss Athletics convention.
#' e.g. an athlete who turns 16 during the year is still U16.

age_to_cat <- function(age) {
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

#' Compute club category from season year and year of birth.
#'
#' This is the version called throughout the pipeline.
#' age = season - yob  (Swiss Athletics: calendar-year age, not birthday age)

recompute_club_cat <- function(season, yob) {
  age <- suppressWarnings(as.integer(season) - as.integer(yob))
  age_to_cat(age)
}


# =============================================================================
# 5. TEXT & ENCODING
# =============================================================================

#' Normalise a name or text field to plain ASCII, lower-case.
#'
#' Applied BEFORE any deduplication key is built, so that
#' "Mühlbauer" and "Muhlbauer" are treated as the same athlete.

normalize_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, to = "ASCII//TRANSLIT")           # é→e, ü→u, etc.
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9 ]", " ")  # remove punctuation
  stringr::str_squish(tolower(x))                 # lower-case, trim spaces
}


# =============================================================================
# 6. DATE PARSING
# =============================================================================

#' Parse a date that could be:
#'   - already a Date / POSIXct object
#'   - an Excel serial number   (e.g. 44927)
#'   - a plain year             (e.g. 1986)
#'   - a string "dd.mm.yyyy" or "yyyy-mm-dd"
#'
#' Returns as.Date(NA) if nothing works.

parse_date_cell <- function(x) {
  parse_one <- function(v) {
    # Already a proper date type
    if (inherits(v, "Date"))   return(as.Date(v))
    if (inherits(v, "POSIXt")) return(as.Date(v))

    # Numeric
    if (is.numeric(v) && !is.na(v)) {
      if (v >= 1800 && v <= 2100) return(as.Date(sprintf("%04d-01-01", as.integer(v))))
      if (v > 20000 && v < 80000) return(as.Date(v, origin = "1899-12-30"))  # Excel serial
      return(as.Date(NA))
    }

    s <- trimws(as.character(v))
    if (is.na(s) || s == "") return(as.Date(NA))

    # Plain 4-digit year
    if (grepl("^\\d{4}$", s))
      return(as.Date(sprintf("%s-01-01", s)))

    # ISO: yyyy-mm-dd
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s))
      return(suppressWarnings(as.Date(s)))

    # French/Swiss: dd.mm.yyyy or dd/mm/yyyy
    if (grepl("^\\d{1,2}[./]\\d{1,2}[./]\\d{4}$", s)) {
      p <- strsplit(s, "[./]")[[1]]
      return(as.Date(sprintf("%s-%02d-%02d", p[3], as.integer(p[2]), as.integer(p[1]))))
    }

    as.Date(NA)
  }
  vapply(x, parse_one, as.Date(NA))
}


# =============================================================================
# 7. SMALL UTILITIES
# =============================================================================

#' Make a string safe for use in filenames and folder names.
#' e.g. "100 m. haies (91.4)" → "100_m_haies_91_4"

safe_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^0-9a-z]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

#' Convert various representations of indoor/outdoor to TRUE/FALSE.

to_logical_indoor <- function(x) {
  if (is.logical(x)) return(x)
  s <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    s %in% c("true", "1", "indoor", "in", "salle", "halle") ~ TRUE,
    s %in% c("false", "0", "outdoor", "out", "plein air")   ~ FALSE,
    TRUE ~ NA
  )
}

#' Source priority: alabus is most trustworthy, old PDFs least.
#' Used during deduplication to pick the best row when two rows conflict.

source_priority <- function(src) {
  dplyr::case_when(
    src == "alabus_ls"     ~ 3L,
    src == "legacy_record" ~ 2L,
    src == "legacy_top10"  ~ 1L,
    src == "legacy_top50"  ~ 0L,
    src == "legacy_pdf"    ~ 0L,
    TRUE                   ~ -1L
  )
}

#' Make a sheet name safe for Excel (max 31 chars, no special characters).

safe_sheet_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[:\\\\/\\?\\*\\[\\]]", "-")
  x <- stringr::str_squish(x)
  ifelse(nchar(x) > 31, substr(x, 1, 31), x)
}

#' Save a dataframe as both .rds and .xlsx.

save_master <- function(df, name, dir = PATHS$master) {
  fs::dir_create(dir)
  rds_path  <- file.path(dir, paste0(name, ".rds"))
  xlsx_path <- file.path(dir, paste0(name, ".xlsx"))
  saveRDS(df, rds_path)
  openxlsx::write.xlsx(df, xlsx_path, overwrite = TRUE)
  message("Saved: ", rds_path, "  (", nrow(df), " rows)")
  invisible(df)
}

#' Load a previously saved master .rds file.

load_master <- function(name, dir = PATHS$master) {
  path <- file.path(dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("File not found: ", path)
  readRDS(path)
}

# =============================================================================
# Confirm loading
# =============================================================================
message("helpers.R loaded OK")
