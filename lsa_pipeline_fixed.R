###############################################################################
#  01_helpers.R
#  ─────────────
#  All shared utility functions for the LSA pipeline.
#  Source this first. No side effects.
###############################################################################

library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(readr)

# ── 1. TEXT / ENCODING ───────────────────────────────────────────────────────

#' Normalise text to pure ASCII, lower-case.
#' Apply BEFORE any dedup key construction.
normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9 ]", " ")
  stringr::str_squish(tolower(x))
}

normalize_name <- function(x) normalize_text(x)


# ── 2. PERFORMANCE PARSING ───────────────────────────────────────────────────

#' Convert any performance string to seconds (or raw number for field events).
safe_mark_to_seconds <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(NA_real_)

  # mm:ss(.xx)
  if (grepl("^\\d+:\\d{1,2}(\\.\\d+)?$", x)) {
    p  <- strsplit(x, ":")[[1]]
    return(as.numeric(p[1]) * 60 + as.numeric(p[2]))
  }
  # hh:mm:ss(.xx)
  if (grepl("^\\d+:\\d{1,2}:\\d{1,2}(\\.\\d+)?$", x)) {
    p  <- strsplit(x, ":")[[1]]
    return(as.numeric(p[1]) * 3600 + as.numeric(p[2]) * 60 + as.numeric(p[3]))
  }
  # Legacy French notation  9"22  1'27"61
  if (grepl("^\\d+'\\d{2}\"", x)) {
    p <- regmatches(x, regexpr("(\\d+)'(\\d+)\"(\\d*)", x))
    m <- regmatches(x, gregexpr("\\d+", x))[[1]]
    if (length(m) >= 2)
      return(as.numeric(m[1]) * 60 + as.numeric(paste0(m[2], ".", if (length(m) > 2) m[3] else "0")))
  }
  if (grepl("^\\d+\"", x)) {
    m <- regmatches(x, gregexpr("[0-9]+", x))[[1]]
    if (length(m) >= 2) return(as.numeric(paste0(m[1], ".", m[2])))
    return(as.numeric(m[1]))
  }
  # Plain number (distances, heights, points)
  num <- suppressWarnings(as.numeric(gsub(",", ".", x)))
  if (!is.na(num)) return(num)

  NA_real_
}

#' Parse a mark field into a unified numeric 'mark':
#'   times      → seconds
#'   distances  → metres
#'   points     → integer points
parse_mark <- function(resultat, metric_effective) {
  dplyr::case_when(
    metric_effective == "time"                   ~ safe_mark_to_seconds(resultat),
    metric_effective %in% c("distance","height") ~ {
      s <- gsub(",", ".", as.character(resultat))
      s <- gsub("\\s*m\\.?\\s*$", "", s, ignore.case = TRUE)
      suppressWarnings(as.numeric(stringr::str_extract(s, "[0-9]+\\.?[0-9]*")))
    },
    metric_effective == "points"                 ~ {
      s <- stringr::str_replace_all(as.character(resultat), "[^0-9]", "")
      n <- nchar(s)
      suppressWarnings(as.numeric(substr(s, 1, min(n, 4))))
    },
    TRUE ~ NA_real_
  )
}


# ── 3. CATEGORY LOGIC ────────────────────────────────────────────────────────

#' Age → club category (strict < thresholds, Swiss Athletics convention)
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

#' BUG FIX: was missing from the pipeline.
#' season + yob → club category (the two-argument version called throughout)
recompute_club_cat <- function(season, yob) {
  age <- suppressWarnings(as.integer(season) - as.integer(yob))
  age_to_cat(age)
}

#' single-argument alias (kept for backward compat)
recompute_club_cat_age <- function(age) age_to_cat(age)


# ── 4. DATE PARSING ──────────────────────────────────────────────────────────

parse_date_multi <- function(x) {
  parse_one <- function(v) {
    if (inherits(v, "Date"))   return(as.Date(v))
    if (inherits(v, "POSIXt")) return(as.Date(v))
    if (is.numeric(v) && !is.na(v)) {
      if (v >= 1800 && v <= 2100) return(as.Date(sprintf("%04d-01-01", as.integer(v))))
      if (v >  20000 && v < 80000) return(as.Date(v, origin = "1899-12-30"))
      return(as.Date(NA))
    }
    s <- trimws(as.character(v))
    if (is.na(s) || s == "") return(as.Date(NA))
    if (grepl("^\\d{4}$", s)) return(as.Date(sprintf("%s-01-01", s)))
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(suppressWarnings(as.Date(s)))
    if (grepl("^\\d{1,2}[./]\\d{1,2}[./]\\d{4}$", s)) {
      p <- strsplit(s, "[./]")[[1]]
      return(as.Date(sprintf("%s-%02d-%02d", p[3], as.integer(p[2]), as.integer(p[1]))))
    }
    as.Date(NA)
  }
  vapply(x, parse_one, as.Date(NA))
}


# ── 5. INDOOR COERCION ───────────────────────────────────────────────────────

to_logical_indoor <- function(x) {
  if (is.logical(x)) return(x)
  s <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    s %in% c("true","t","1","indoor","in","halle","salle","i") ~ TRUE,
    s %in% c("false","f","0","outdoor","out","plein air","o") ~ FALSE,
    TRUE ~ NA
  )
}


# ── 6. SOURCE PRIORITY ───────────────────────────────────────────────────────

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


# ── 7. DIRECTION ─────────────────────────────────────────────────────────────

#' Attach competition direction from the curated better_is_lower column.
#' ONLY this version should exist. The heuristic string-based version is wrong.
attach_direction <- function(df) {
  df %>%
    dplyr::mutate(
      direction = dplyr::case_when(
        isTRUE(better_is_lower)  ~ "min",
        isFALSE(better_is_lower) ~ "max",
        is.na(better_is_lower)   ~ "min",   # safe default: track events
        TRUE                     ~ "min"
      )
    )
}


# ── 8. MISC ──────────────────────────────────────────────────────────────────

safe_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^0-9a-z]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

year_tag_from_df <- function(df) {
  if (!"season" %in% names(df)) return("unknown")
  yrs <- df$season[!is.na(df$season)]
  if (!length(yrs)) return("unknown")
  rng <- range(yrs)
  sprintf("%02d_%02d", rng[1] %% 100, rng[2] %% 100)
}

safe_sheet_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[:\\\\/\\?\\*\\[\\]]", "-")
  x <- stringr::str_squish(x)
  ifelse(nchar(x) > 31, substr(x, 1, 31), x)
}

###############################################################################
#  04_canonicalize.R  (append to or source after 01_helpers.R)
#  ─────────────────────────────────────────────────────────────
#  Canonical schema, compute_marks, dedup, make_sb
###############################################################################

# ── CANONICAL SCHEMA ─────────────────────────────────────────────────────────

#' BUG FIX: standardize_ls_schema was called but never defined.
#' This is the single authoritative schema normaliser.
#' Replaces BOTH the old standardize_ls_schema AND canonicalize_ls calls.
canonicalize_ls <- function(df) {
  names(df) <- make.unique(names(df), sep = "__")

  # ---- column aliases ----
  alias <- function(df, canonical, alts) {
    if (!canonical %in% names(df)) {
      found <- intersect(alts, names(df))
      if (length(found)) df[[canonical]] <- df[[found[1]]]
      else df[[canonical]] <- NA
    }
    df
  }

  df <- alias(df, "resultat",  c("Résultat","Resultat","resultat"))
  df <- alias(df, "athlete",   c("athlete","Nom"))
  df <- alias(df, "date",      c("date","Date"))
  df <- alias(df, "lieu",      c("lieu","Lieu"))
  df <- alias(df, "event",     c("event","event_name","event_display"))
  df <- alias(df, "event_display", c("event_display","event"))

  # BUG FIX: guard Date naiss. before touching it
  if (!"Date naiss." %in% names(df)) df[["Date naiss."]] <- NA_character_

  # ---- required + optional columns ----
  required <- c("event","event_display","resultat","mark_seconds",
                "athlete","yob","date","lieu","gender","indoor","club_cat",
                "source","season","age")
  optional <- c("rank","event_name","event_type","category_raw","Societe",
                "source_file","metric","better_is_lower","mark_distance",
                "mark_points","mark","metric_effective","Date naiss.",
                "athlete_first","athlete_last","club_cat_raw")

  for (nm in c(required, optional)) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }

  # ---- type coercion ----
  df <- df %>%
    dplyr::mutate(
      event           = as.character(event),
      event_display   = as.character(event_display),
      resultat        = as.character(resultat),
      # ENCODING: apply iconv to athlete BEFORE any key construction
      athlete         = iconv(as.character(athlete), to = "UTF-8", sub = "byte"),
      yob             = suppressWarnings(as.integer(yob)),
      date            = as.Date(vapply(date, function(v) parse_date_multi(v), as.Date(NA))),
      lieu            = as.character(lieu),
      gender          = as.character(gender),
      indoor          = to_logical_indoor(indoor),
      club_cat        = as.character(club_cat),
      source          = as.character(source),
      season          = suppressWarnings(as.integer(season)),
      age             = suppressWarnings(as.integer(age)),
      mark_seconds    = suppressWarnings(as.numeric(mark_seconds)),
      rank            = suppressWarnings(as.integer(rank)),
      better_is_lower = suppressWarnings(as.logical(better_is_lower)),
      mark_distance   = suppressWarnings(as.numeric(mark_distance)),
      mark_points     = suppressWarnings(as.numeric(mark_points)),
      mark            = suppressWarnings(as.numeric(mark))
    )

  # ---- backfill season / age ----
  df <- df %>%
    dplyr::mutate(
      season = dplyr::if_else(is.na(season) & !is.na(date),
                              as.integer(format(date, "%Y")), season),
      age    = dplyr::if_else(is.na(age) & !is.na(season) & !is.na(yob),
                              as.integer(season - yob), age)
    )

  df %>% dplyr::select(dplyr::all_of(required), dplyr::all_of(optional),
                       dplyr::everything())
}

# Alias: old name used elsewhere in the codebase
standardize_ls_schema <- canonicalize_ls


# ── COMPUTE MARKS ────────────────────────────────────────────────────────────

compute_marks_from_result <- function(df) {
  df %>%
    dplyr::mutate(
      metric_effective = dplyr::case_when(
        event_type == "run"                          ~ "time",
        event_type %in% c("jump","throw","hurdle")  ~ "distance",
        event_type == "multi"                        ~ "points",
        metric %in% c("time","distance","height","points") ~ metric,
        stringr::str_detect(resultat, ":")           ~ "time",
        stringr::str_detect(resultat, "m\\.?$")     ~ "distance",
        stringr::str_detect(tolower(resultat), "pts|points") ~ "points",
        TRUE                                         ~ NA_character_
      ),

      mark_seconds = dplyr::if_else(
        metric_effective == "time",
        vapply(resultat, safe_mark_to_seconds, numeric(1)),
        NA_real_
      ),

      mark_distance = dplyr::if_else(
        metric_effective %in% c("distance","height"), {
          s <- gsub(",", ".", as.character(resultat))
          s <- gsub("\\s*m\\.?\\s*$", "", s, ignore.case = TRUE)
          suppressWarnings(as.numeric(stringr::str_extract(s, "[0-9]+\\.?[0-9]*")))
        },
        NA_real_
      ),

      mark_points = dplyr::if_else(
        metric_effective == "points", {
          s <- stringr::str_replace_all(as.character(resultat), "[^0-9]", "")
          suppressWarnings(as.numeric(substr(s, 1, pmin(nchar(s), 4))))
        },
        NA_real_
      ),

      mark = dplyr::case_when(
        metric_effective == "time"                    ~ mark_seconds,
        metric_effective %in% c("distance","height")  ~ mark_distance,
        metric_effective == "points"                   ~ mark_points,
        TRUE                                          ~ NA_real_
      )
    )
}


# ── DEDUPLICATION ────────────────────────────────────────────────────────────

#' Compute athlete identity key.
#' ENCODING IS APPLIED HERE via normalize_name (which calls iconv).
athlete_id_key <- function(athlete, dob = NULL) {
  akey <- normalize_name(athlete)
  if (is.null(dob) || all(is.na(dob))) {
    akey
  } else {
    paste0(akey, "||", normalize_text(as.character(dob)))
  }
}

#' Full deduplication (two-pass: strict then loose)
dedup_ls <- function(df) {
  df <- canonicalize_ls(df) %>%
    dplyr::mutate(
      src_prio      = source_priority(source),
      club_cat_raw  = club_cat,
      # BUG FIX: recompute_club_cat now exists and takes season + yob
      club_cat_eff  = recompute_club_cat(season, yob),
      club_cat_eff  = dplyr::coalesce(club_cat_eff, club_cat_raw),
      # ENCODING: normalize BEFORE key construction
      athlete_id    = athlete_id_key(athlete, `Date naiss.`),
      mark_key      = dplyr::if_else(!is.na(mark), sprintf("%.4f", mark), NA_character_),
      date_key      = dplyr::if_else(is.na(date), "NA", as.character(date))
    )

  # Pass 1: event + mark + date (strict)
  df1 <- df %>%
    dplyr::mutate(key_strict = paste(event, gender, indoor, club_cat_eff,
                                     athlete_id, yob, mark_key, date_key, sep = "|")) %>%
    dplyr::arrange(dplyr::desc(src_prio), dplyr::desc(!is.na(date)), dplyr::desc(!is.na(lieu))) %>%
    dplyr::group_by(key_strict) %>% dplyr::slice(1L) %>% dplyr::ungroup()

  # Pass 2: event + mark only (loose – catches missing-date duplicates)
  df1 %>%
    dplyr::mutate(key_loose = paste(event, gender, indoor, club_cat_eff,
                                    athlete_id, yob, mark_key, sep = "|")) %>%
    dplyr::arrange(dplyr::desc(src_prio), dplyr::desc(!is.na(date)), dplyr::desc(!is.na(lieu))) %>%
    dplyr::group_by(key_loose) %>% dplyr::slice(1L) %>% dplyr::ungroup() %>%
    dplyr::mutate(club_cat = club_cat_eff) %>%
    dplyr::select(-src_prio, -key_strict, -key_loose, -club_cat_eff, -athlete_id, -mark_key, -date_key)
}

#' Simpler dedup (yob-based, no DOB needed)
dedup_ls_yob <- function(df) {
  df %>%
    canonicalize_ls() %>%
    dplyr::mutate(
      athlete_norm = normalize_name(athlete),  # encoding applied here
      mark_key  = dplyr::if_else(!is.na(mark), sprintf("%.4f", mark), NA_character_),
      date_key  = dplyr::if_else(is.na(date), "NA", as.character(date)),
      dedup_key = paste(event, gender, indoor, athlete_norm,
                        as.character(yob), date_key, mark_key, sep = "|"),
      src_prio  = source_priority(source),
      has_lieu  = !is.na(lieu) & lieu != ""
    ) %>%
    dplyr::arrange(dplyr::desc(src_prio), dplyr::desc(has_lieu)) %>%
    dplyr::group_by(dedup_key) %>% dplyr::slice(1L) %>% dplyr::ungroup() %>%
    dplyr::select(-dedup_key, -src_prio, -has_lieu, -athlete_norm, -mark_key, -date_key)
}


# ── FLAG & FIX AGE / CAT ─────────────────────────────────────────────────────

flag_and_fix_age_cat <- function(df, min_age_ok = 6L, max_age_ok = 100L) {
  df %>%
    dplyr::mutate(
      club_cat_raw = club_cat,
      age_calc = dplyr::if_else(!is.na(season) & !is.na(yob),
                                as.integer(season - yob), NA_integer_),
      age_wrong = is.na(age_calc) | age_calc < min_age_ok | age_calc > max_age_ok,
      age      = dplyr::if_else(!age_wrong, age_calc, age),
      # BUG FIX: recompute_club_cat now exists
      club_cat_eff = dplyr::if_else(!age_wrong,
                                    recompute_club_cat(season, yob),
                                    club_cat_raw),
      club_cat = dplyr::coalesce(club_cat_eff, club_cat_raw, club_cat)
    ) %>%
    dplyr::select(-age_calc, -club_cat_eff)
}


# ── SEASON-BEST DATASET ──────────────────────────────────────────────────────

make_ls_sb <- function(df) {
  df %>%
    canonicalize_ls() %>%
    compute_marks_from_result() %>%
    attach_direction() %>%
    dplyr::mutate(
      athlete_key  = normalize_name(athlete),   # encoding applied here
      season       = dplyr::coalesce(season, as.integer(format(date, "%Y"))),
      mark         = dplyr::coalesce(mark, mark_seconds, mark_distance, mark_points),
      age          = dplyr::coalesce(age, season - yob),
      club_cat_raw = club_cat,
      # BUG FIX: recompute_club_cat now exists
      club_cat     = recompute_club_cat(season, yob),
      src_prio     = source_priority(source),
      score        = dplyr::case_when(
        direction == "min" ~  mark,
        direction == "max" ~ -mark,
        TRUE               ~  mark
      )
    ) %>%
    dplyr::filter(!is.na(season), !is.na(mark), !is.na(athlete_key), athlete_key != "") %>%
    dplyr::group_by(event, gender, indoor, season, athlete_key) %>%
    dplyr::arrange(score, dplyr::desc(src_prio), dplyr::desc(!is.na(date)),
                   date, dplyr::desc(!is.na(lieu)), .by_group = TRUE) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(-athlete_key, -src_prio, -score)
}

###############################################################################
#  05_merge.R  (source after 01_helpers.R, 03_legacy_import.R, 04_canon...)
#  ────────────────────────────────────────────────────────────────────────
#  The merge + output pipeline.
###############################################################################

# ── SAVE / LOAD HELPERS ──────────────────────────────────────────────────────

save_master <- function(df, name, dir = "data/ls_master", xlsx = TRUE) {
  fs::dir_create(dir)
  rds_path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(df, rds_path)
  message(">>> Saved RDS: ", rds_path, " (n=", nrow(df), ")")
  if (xlsx) {
    xlsx_path <- file.path(dir, paste0(name, ".xlsx"))
    openxlsx::write.xlsx(df, xlsx_path, overwrite = TRUE)
    message(">>> Saved XLSX: ", xlsx_path)
  }
  invisible(df)
}

load_master <- function(name, dir = "data/ls_master") {
  path <- file.path(dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Missing master file: ", path)
  readRDS(path)
}

# ── ALABUS LS LOADER ─────────────────────────────────────────────────────────

load_all_ls_opt <- function(ls_dir = "data/alabus_ls") {
  files <- fs::dir_ls(ls_dir, recurse = TRUE, glob = "*.rds")
  if (!length(files)) stop("No .rds files found in ", ls_dir)
  message(">>> Found ", length(files), " LS files")
  purrr::map_dfr(files, function(f) {
    x <- readRDS(f) %>% tibble::as_tibble()
    x$source_file <- f
    x[] <- lapply(x, as.character)
    x
  }) %>% dplyr::distinct()
}

fix_duplicate_mark_seconds <- function(df) {
  names(df) <- make.unique(names(df), sep = "___")
  ms_all <- names(df)[grepl("^(mark_seconds|Mark_seconds)($|___\\d+$)", names(df))]
  if (!length(ms_all)) return(df)
  df %>%
    dplyr::mutate(
      mark_seconds = suppressWarnings(
        as.numeric(dplyr::coalesce(!!!rlang::syms(ms_all)))
      )
    ) %>%
    dplyr::select(-dplyr::any_of(setdiff(ms_all, "mark_seconds")))
}

clean_result_time <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[^0-9\\.:]", "")
  stringr::str_replace(x, "(\\.[0-9]{2}).*$", "\\1")
}

repair_mark_seconds <- function(df) {
  df <- fix_duplicate_mark_seconds(df)
  if (!"mark_seconds" %in% names(df)) df$mark_seconds <- NA_real_
  need <- is.na(df$mark_seconds) & !is.na(df$Résultat) & df$Résultat != ""
  if (!any(need)) return(df)
  cleaned <- clean_result_time(df$Résultat[need])
  df$mark_seconds[need] <- vapply(cleaned, safe_mark_to_seconds, numeric(1))
  df
}

standardize_alabus_ls <- function(df) {
  names(df) <- make.unique(names(df), sep = "___")
  if (!"Résultat" %in% names(df)) {
    cand <- intersect(c("resultat","Resultat","Résultat"), names(df))
    if (length(cand)) df$Résultat <- df[[cand[1]]]
  }
  df %>%
    dplyr::mutate(
      season       = suppressWarnings(as.integer(season)),
      yob          = suppressWarnings(as.integer(yob)),
      age          = suppressWarnings(as.integer(age)),
      gender       = as.character(gender),
      event_name   = as.character(event_name),
      event_type   = as.character(event_type),
      category_raw = as.character(category_raw),
      indoor       = to_logical_indoor(indoor),
      Societe      = as.character(Societe),
      Résultat     = as.character(Résultat)
    )
}

# ── MAIN MERGE PIPELINE ──────────────────────────────────────────────────────

#' Full merge: alabus LS + legacy PA → master dataset
#'
#' @param alabus_ls_dir  directory containing scraped LS .rds files
#' @param records_M_indoor_path  etc. – paths to legacy Excel files
#' @param event_names_path  path to event_names.csv
#' @return list(all_time = ..., all_time_sb = ...)
build_master <- function(
    alabus_ls_dir          = "data/alabus_ls",
    records_M_indoor_path  = NULL,
    records_M_outdoor_path = NULL,
    records_W_indoor_path  = NULL,
    records_W_outdoor_path = NULL,
    top10_M_path           = NULL,
    top10_W_path           = NULL,
    top50_M_outdoor_path   = NULL,
    top50_W_outdoor_path   = NULL,
    top50_M_indoor_path    = NULL,
    top50_W_indoor_path    = NULL,
    pdf_csv_paths          = NULL,
    event_names_path       = "event_names.csv"
) {

  # 1) Load event metadata
  event_names <- readr::read_delim(event_names_path, delim = ";",
                                    show_col_types = FALSE)

  lookup_alabus <- event_names %>%
    dplyr::filter(!is.na(event_name), event_name != "") %>%
    dplyr::distinct(event_name, .keep_all = TRUE) %>%
    dplyr::select(event_name, event_display, event_type, metric, unit, better_is_lower)

  lookup_legacy <- event_names %>%
    dplyr::filter(!is.na(event), event != "") %>%
    dplyr::distinct(event, .keep_all = TRUE) %>%
    dplyr::select(event, event_display, event_type, metric, unit, better_is_lower)

  # 2) Alabus LS
  message(">>> Loading alabus LS...")
  ls_all_06_25 <- load_all_ls_opt(alabus_ls_dir) %>%
    standardize_alabus_ls() %>%
    repair_mark_seconds() %>%
    dplyr::select(-dplyr::matches("^NH"),
                  -dplyr::starts_with("res"),
                  -dplyr::starts_with("Mark_seconds2"))

  # Harmonise event names before join
  ls_all_06_25 <- ls_all_06_25 %>%
    dplyr::mutate(
      event_name = dplyr::case_when(
        event_name %in% c("50H_76.2_U16W_U14M","50H_76.2_U18W") ~ "50H_76.2",
        event_name %in% c("60H_76.2_U12","60H_76.2_U14W",
                          "60H_76.2_U16W_U14M","60H_76.2_U18W") ~ "60H_76.2",
        event_name %in% c("60H_84.0","60H_84")                  ~ "60H_84.0",
        TRUE ~ event_name
      )
    ) %>%
    dplyr::select(-dplyr::any_of(c("event_display","event_type","metric","unit","better_is_lower"))) %>%
    dplyr::left_join(lookup_alabus, by = "event_name") %>%
    dplyr::mutate(
      event = dplyr::coalesce(event_display, event_name),
      source = "alabus_ls"
    )

  # 3) Legacy PA
  message(">>> Building legacy PA...")
  ls_all_PA <- build_legacy_all(
    records_M_indoor_path, records_M_outdoor_path,
    records_W_indoor_path, records_W_outdoor_path,
    top10_M_path, top10_W_path,
    top50_M_outdoor_path, top50_W_outdoor_path,
    top50_M_indoor_path, top50_W_indoor_path,
    pdf_csv_paths
  )

  # ENCODING: remove accents ONCE before event mapping join
  ls_all_PA <- ls_all_PA %>%
    dplyr::mutate(event = iconv(event, to = "ASCII//TRANSLIT"))

  ls_all_PA <- ls_all_PA %>%
    dplyr::select(-dplyr::any_of(c("event_display","event_type","metric","unit","better_is_lower"))) %>%
    dplyr::left_join(lookup_legacy, by = "event") %>%
    dplyr::mutate(event = dplyr::coalesce(event_display, event))

  # 4) Canonicalize each source
  message(">>> Canonicalizing...")
  ls_06_25_std <- ls_all_06_25 %>% canonicalize_ls()
  ls_PA_std    <- ls_all_PA    %>% canonicalize_ls()

  # 5) Merge → compute marks → fix categories → dedup
  message(">>> Merging, computing marks, deduplicating...")
  ls_all_time <- dplyr::bind_rows(ls_06_25_std, ls_PA_std) %>%
    canonicalize_ls() %>%
    compute_marks_from_result() %>%
    dplyr::filter(resultat != "Vacant") %>%
    attach_direction() %>%
    flag_and_fix_age_cat(min_age_ok = 6L) %>%
    dedup_ls_yob()

  # 6) Season-best
  message(">>> Building season-best dataset...")
  ls_all_time_sb <- make_ls_sb(ls_all_time)

  # Sanity: no athlete × event × season duplicates in SB
  dup_check <- ls_all_time_sb %>%
    dplyr::count(event, gender, indoor, season,
                 athlete_norm = normalize_name(athlete), sort = TRUE) %>%
    dplyr::filter(n > 1)
  if (nrow(dup_check) > 0) {
    warning("Season-best has ", nrow(dup_check), " duplicate athlete × event × season rows!")
    print(head(dup_check, 10))
  }

  list(all_time = ls_all_time, all_time_sb = ls_all_time_sb)
}


# ── MONTHLY REFRESH ──────────────────────────────────────────────────────────

#' Scrape only the current year and refresh the master.
#' Call this on a cron job (monthly or at season end).
refresh_current_year <- function(urls_all,
                                  prev_master_name = "ls_all_time_sb",
                                  master_dir       = "data/ls_master",
                                  alabus_ls_dir    = "data/alabus_ls",
                                  club_name        = "Lausanne-Sports Athlétisme",
                                  polite_delay     = 0.2) {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  message(">>> Refreshing year: ", yr)

  new_urls <- urls_all %>%
    dplyr::filter(blyear == yr, missing_code == "ok", !is.na(url))

  flagged  <- flag_alabus_urls_with_data(new_urls, polite_delay = polite_delay)
  scraped  <- scrape_alabus_toplists(flagged %>% dplyr::filter(has_data),
                                     polite_delay = polite_delay)
  if (is.null(scraped) || nrow(scraped) == 0) {
    message(">>> No new data scraped for ", yr)
    return(invisible(NULL))
  }

  # Filter to club
  new_ls <- scraped %>%
    dplyr::filter(Societe == club_name) %>%
    dplyr::mutate(source = "alabus_ls")

  # Load previous master and append
  prev <- load_master(prev_master_name, master_dir)

  updated <- dplyr::bind_rows(prev, canonicalize_ls(new_ls)) %>%
    canonicalize_ls() %>%
    compute_marks_from_result() %>%
    attach_direction() %>%
    flag_and_fix_age_cat() %>%
    dedup_ls_yob() %>%
    { make_ls_sb(.) }

  new_name <- paste0("ls_all_time_sb_", yr)
  save_master(updated, new_name, dir = master_dir)
  message(">>> Refresh complete: ", new_name)
  invisible(updated)
}
