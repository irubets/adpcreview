library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
library(readr)
library(openxlsx)
library(stringr)
library(purrr)

# Resolve companion functions beside this versioned application.
.app_sources <- Filter(function(x) is.character(x) && length(x) == 1L,
  lapply(sys.frames(), function(frame) frame$ofile))
.app_source <- if (length(.app_sources)) tail(.app_sources, 1L)[[1L]] else NULL
.app_dir <- if (exists(".app_dir", inherits = FALSE)) .app_dir else if (!is.null(.app_source)) dirname(normalizePath(.app_source)) else getwd()
if (!exists(".app_output_dir", inherits = FALSE)) .app_output_dir <- file.path(getwd(), "generated_visualizations")
source(file.path(.app_dir, "nca_feasibility.R"), local = TRUE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
source(file.path(.app_dir, "flag_explorer.R"), local = TRUE)
source(file.path(.app_dir, "ai_provider.R"), local = TRUE)
source(file.path(.app_dir, "ai_data_query.R"), local = TRUE)
source(file.path(.app_dir, "grouping_values_audit.R"), local = TRUE)
source(file.path(.app_dir, "case_explorer.R"), local = TRUE)
source(file.path(.app_dir, "profile_identification.R"), local = TRUE)
source(file.path(.app_dir, "adpc_quality_checks.R"), local = TRUE)
source(file.path(.app_dir, "review_html_report.R"), local = TRUE)
source(file.path(.app_dir, "review_excel_report.R"), local = TRUE)

safe_names <- function(x) {
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

pick_dataset_variable_v13 <- function(variable_names, candidates) {
  positions <- match(toupper(candidates), toupper(variable_names))
  positions <- positions[!is.na(positions)]
  if (length(positions)) variable_names[positions[1]] else ""
}

clean_local_path <- function(path) {
  if (is.null(path) || !length(path) || is.na(path[1])) return(path)
  path <- trimws(as.character(path[1]))
  if (nchar(path) >= 2L) {
    first <- substr(path, 1L, 1L)
    last <- substr(path, nchar(path), nchar(path))
    if (first == last && first %in% c('"', "'")) {
      path <- substr(path, 2L, nchar(path) - 1L)
    }
  }
  trimws(path)
}

read_input_dataset <- function(path) {
  path <- clean_local_path(path)
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    stop("No file path was provided.")
  }
  if (!file.exists(path)) {
    stop(sprintf("File does not exist: %s", path))
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else if (ext == "rds") {
    readRDS(path)
  } else if (ext %in% c("rda", "rdata")) {
    e <- new.env(parent = emptyenv())
    nm <- load(path, envir = e)
    e[[nm[1]]]
  } else {
    stop("Unsupported file type. Use sas7bdat, xpt, csv, rds, rda, or rdata.")
  }
}

compute_arrlt_review <- function(dat,
                                 id_var,
                                 analyte_var,
                                 visit_var,
                                 visitcd_var = NULL,
                                 nominal_var,
                                 actual_var,
                                 aval_var,
                                 mrrlt_var = NULL,
                                 adtm_var = NULL,
                                 arftdtm_var = NULL,
                                 pctdev_threshold = 20,
                                 absdev_threshold = 0.5,
                                 use_iqr = FALSE,
                                 iqr_multiplier = 1.5,
                                 early_window = 2,
                                 predose_neg_threshold = -0.5) {

  # Optional mappings may arrive as NULL, blank or NA from the UI.
  optional_name <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(trimws(x[1]))) NULL else trimws(x[1])
  }
  visitcd_var <- optional_name(visitcd_var)
  mrrlt_var <- optional_name(mrrlt_var)
  adtm_var <- optional_name(adtm_var)
  arftdtm_var <- optional_name(arftdtm_var)

  vars_needed <- c(id_var, analyte_var, visit_var, nominal_var, actual_var, aval_var)
  vars_optional <- c(visitcd_var, mrrlt_var, adtm_var, arftdtm_var)
  vars_all <- unique(c(vars_needed, vars_optional))
  vars_all <- vars_all[!is.na(vars_all) & vars_all != ""]

  missing_vars <- setdiff(vars_all, names(dat))
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required variable(s): %s", paste(missing_vars, collapse = ", ")))
  }

  out <- dat %>%
    mutate(
      .id      = as.character(.data[[id_var]]),
      .pctest  = as.character(.data[[analyte_var]]),
      .avisit  = as.character(.data[[visit_var]]),
      .visitcd = if (!is.null(visitcd_var) && visitcd_var != "") as.character(.data[[visitcd_var]]) else NA_character_,
      .nrrlt   = suppressWarnings(as.numeric(.data[[nominal_var]])),
      .arrlt   = suppressWarnings(as.numeric(.data[[actual_var]])),
      .mrrlt   = if (!is.null(mrrlt_var) && mrrlt_var != "") suppressWarnings(as.numeric(.data[[mrrlt_var]])) else NA_real_,
      .aval    = suppressWarnings(as.numeric(.data[[aval_var]])),
      .adtm    = if (!is.null(adtm_var) && adtm_var != "") .data[[adtm_var]] else NA,
      .arftdtm = if (!is.null(arftdtm_var) && arftdtm_var != "") .data[[arftdtm_var]] else NA
    ) %>%
    filter(!is.na(.pctest), !is.na(.avisit), is.finite(.nrrlt), is.finite(.arrlt)) %>%
    mutate(
      facet_label = ifelse(
        is.na(.visitcd) | .visitcd == "",
        paste(.pctest, .avisit, sep = " | "),
        paste(.pctest, .avisit, paste0("(", .visitcd, ")"), sep = " | ")
      ),
      visit_display = ifelse(
        is.na(.visitcd) | .visitcd == "",
        .avisit,
        paste0(.avisit, " [", .visitcd, "]")
      ),
      arrlt_dev    = .arrlt - .nrrlt,
      mrrlt_dev    = if_else(!is.na(.mrrlt), .mrrlt - .nrrlt, NA_real_),
      arrlt_pctdev = dplyr::if_else(.nrrlt != 0, 100 * abs(.arrlt - .nrrlt) / abs(.nrrlt), NA_real_),
      .log_aval    = if_else(!is.na(.aval) & .aval > 0, log(.aval), NA_real_)
    ) %>%
    group_by(.id, .pctest, .avisit, .visitcd, .nrrlt) %>%
    mutate(nrrlt_n = dplyr::n()) %>%
    ungroup() %>%
    group_by(.id, .pctest, .avisit, .visitcd, .mrrlt) %>%
    mutate(mrrlt_n = if_else(!is.na(.mrrlt), dplyr::n(), 0L)) %>%
    ungroup() %>%
    group_by(.id, .pctest, .avisit, .visitcd) %>%
    mutate(
      nrrlt_fl = if_else(nrrlt_n > 1, "Y", "N", missing = "N"),
      mrrlt_fl = if_else(!is.na(.mrrlt) & mrrlt_n > 1, "Y", "N", missing = "N"),
      nrrlt_dup = if_else(any(nrrlt_n > 1, na.rm = TRUE), "Y", "N", missing = "N"),
      mrrlt_dup = if_else(any(!is.na(.mrrlt) & mrrlt_n > 1, na.rm = TRUE), "Y", "N", missing = "N")
    ) %>%
    ungroup()

  # Population flags are evaluated at the complete subject-analyte-visit profile
  # level. APROFILE may contain a profile name instead of a conventional Y/N
  # flag: Y or any other populated value is included, while N and empty values
  # are excluded. Missing source variables remain unavailable.
  for (flag_name in c("APROFILE", "PKSUMXFL", "NCA01FL", "ANL01FL", "ANL02FL")) {
    source_name <- names(out)[match(flag_name, toupper(names(out)))]
    derived_name <- paste0(".profile_", tolower(flag_name))
    if (length(source_name) && !is.na(source_name)) {
      out <- out %>%
        group_by(.id, .pctest, .avisit, .visitcd) %>%
        mutate(
          !!derived_name := if_else(
            if (flag_name == "APROFILE") {
              profile_value <- toupper(trimws(as.character(.data[[source_name]])))
              any(!is.na(profile_value) & nzchar(profile_value) & profile_value != "N")
            }
            else any(toupper(trimws(as.character(.data[[source_name]]))) == "Y", na.rm = TRUE),
            "Y", "N_OR_EMPTY"
          )
        ) %>%
        ungroup()
    } else {
      out[[derived_name]] <- NA_character_
    }
  }

  if (isTRUE(use_iqr)) {
    stats <- out %>%
      group_by(.pctest, .avisit, .visitcd, .nrrlt) %>%
      summarise(
        n_group = sum(!is.na(.arrlt)),
        q1 = if (sum(!is.na(.arrlt)) >= 2) quantile(.arrlt, 0.25, na.rm = TRUE, names = FALSE, type = 2) else NA_real_,
        q3 = if (sum(!is.na(.arrlt)) >= 2) quantile(.arrlt, 0.75, na.rm = TRUE, names = FALSE, type = 2) else NA_real_,
        median_arrlt = median(.arrlt, na.rm = TRUE),
        iqr = if (sum(!is.na(.arrlt)) >= 2) IQR(.arrlt, na.rm = TRUE, type = 2) else NA_real_,
        .groups = "drop"
      ) %>%
      mutate(lower_iqr = q1 - iqr_multiplier * iqr, upper_iqr = q3 + iqr_multiplier * iqr)
  } else {
    stats <- out %>%
      distinct(.pctest, .avisit, .visitcd, .nrrlt) %>%
      mutate(n_group = NA_integer_, q1 = NA_real_, q3 = NA_real_,
             median_arrlt = NA_real_, iqr = NA_real_, lower_iqr = NA_real_, upper_iqr = NA_real_)
  }

  out <- out %>%
    left_join(stats, by = c(".pctest", ".avisit", ".visitcd", ".nrrlt")) %>%
    mutate(
      arrlt_outlierfl = if_else(
        !is.na(lower_iqr) & !is.na(upper_iqr) & (.arrlt < lower_iqr | .arrlt > upper_iqr),
        "Y", "N", missing = "N"
      ),
      predose_postdose_fl = if_else(.nrrlt == 0 & .arrlt > 0, "Y", "N", missing = "N"),
      postdose_predose_fl = if_else(.nrrlt > 0 & .arrlt <= 0, "Y", "N", missing = "N"),
      predose_early_fl = if_else(.nrrlt == 0 & .arrlt < predose_neg_threshold, "Y", "N", missing = "N"),
      mrrlt_ne_arrlt_fl = if_else(!is.na(.mrrlt) & !is.na(.arrlt) & .mrrlt != .arrlt, "Y", "N", missing = "N"),
      arrlt_negfl = postdose_predose_fl,
      arrlt_bigdevfl = if_else(!is.na(arrlt_pctdev) & arrlt_pctdev > pctdev_threshold, "Y", "N", missing = "N"),
      arrlt_absdevfl = if_else(abs(arrlt_dev) > absdev_threshold, "Y", "N", missing = "N"),
      early_positivefl = predose_postdose_fl,
      review_fl = if_else(
          arrlt_outlierfl     == "Y" |
          arrlt_bigdevfl      == "Y" |
          arrlt_absdevfl      == "Y" |
          predose_postdose_fl == "Y" |
          postdose_predose_fl == "Y" |
          predose_early_fl    == "Y" |
          mrrlt_ne_arrlt_fl   == "Y" |
          nrrlt_fl            == "Y" |
          mrrlt_fl            == "Y",
        "Y", "N", missing     = "N"
      ),
      review_reason = pmap_chr(
        list(arrlt_outlierfl, arrlt_bigdevfl, arrlt_absdevfl, predose_postdose_fl, postdose_predose_fl, predose_early_fl, mrrlt_ne_arrlt_fl, nrrlt_fl, mrrlt_fl),
        function(a, b, c, d, e, f, g, h, i) {
          reasons <- c(
            if (identical(a, "Y")) "IQR outlier" else NULL,
            if (identical(b, "Y")) "Large % deviation" else NULL,
            if (identical(c, "Y")) "Large absolute deviation" else NULL,
            if (identical(d, "Y")) "PREdose taken POST-dose" else NULL,
            if (identical(e, "Y")) "POST-dose taken PREdose" else NULL,
            if (identical(f, "Y")) paste0("PREdose suspiciously early (< ", predose_neg_threshold, ")") else NULL,
            if (identical(g, "Y")) "MRRLT differs from ARRLT" else NULL,
            if (identical(h, "Y")) "Profiles with duplicates at NRRLT" else NULL,
            if (identical(i, "Y")) "Profiles with duplicate MRRLT" else NULL
          )
          paste(reasons, collapse = "; ")
        }
      ),
      timing_case = dplyr::case_when(
        predose_postdose_fl == "Y" ~ "PREdose taken POST-dose",
        postdose_predose_fl == "Y" ~ "POST-dose taken PREdose",
        predose_early_fl    == "Y" ~ "PREdose suspiciously early",
        mrrlt_ne_arrlt_fl   == "Y" ~ "MRRLT differs from ARRLT",
        review_fl           == "Y" ~ "Other flagged",
        TRUE ~ "Normal"
      )
    ) %>%
    mutate(
      arrlt_dev  = round(arrlt_dev,  2),
      mrrlt_dev  = round(mrrlt_dev,  2),
      lower_iqr  = round(lower_iqr,  2),
      upper_iqr  = round(upper_iqr,  2)
    ) %>%
    arrange(.pctest, .avisit, .nrrlt, desc(review_fl), .id)

  subject_summary <- out %>%
    group_by(.id, .pctest, .avisit, .visitcd) %>%
    summarise(
      n_records = n(),
      n_review = sum(review_fl == "Y", na.rm = TRUE),
      max_abs_dev = suppressWarnings(max(abs(arrlt_dev), na.rm = TRUE)),
      max_pct_dev = suppressWarnings(max(arrlt_pctdev, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      max_abs_dev = ifelse(is.infinite(max_abs_dev), NA_real_, max_abs_dev),
      max_pct_dev = ifelse(is.infinite(max_pct_dev), NA_real_, max_pct_dev)
    ) %>%
    mutate(
      max_abs_dev = round(max_abs_dev,  2),
      max_pct_dev = round(max_pct_dev,  2)
    )

  list(data = out, subject_summary = subject_summary)
}

# =============================================================================
# PK PROFILE HELPERS
# =============================================================================

safe_col <- function(nm) {
  if (is.null(nm)) return(NULL)
  x <- trimws(nm)
  if (length(x) == 0 || all(is.na(x)) || all(x == "")) return(NULL)
  x[x == ""] <- NA_character_
  x[1]
}

GROUP_PALETTE <- c(
  "#1565C0", "#C62828", "#2E7D32", "#F57F17",
  "#6A1B9A", "#00838F", "#4E342E", "#E65100",
  "#880E4F", "#1B5E20", "#0D47A1", "#BF360C"
)

make_colour_map <- function(vals) {
  vals <- sort(unique(as.character(vals[!is.na(vals)])))
  pal  <- rep_len(GROUP_PALETTE, length(vals))
  setNames(pal, vals)
}

prepare_pk_data <- function(df,
                             id_var, analyte_var, visit_var,
                             visitcd_var, nominal_var, mrrlt_var,
                             aval_var, lloq_var,
                             route_var, atrt_var, arm_var,
                             cohort_var, agegrp_var, bsacat_var, sex_var,
                             filter_var = NULL, filter_val = "Y",
                             blq_half = FALSE) {
  if (!is.null(filter_var) && nzchar(filter_var) &&
      filter_var %in% names(df)) {
    if (identical(filter_val, "__NULL__")) {
      keep <- is.na(df[[filter_var]]) |
              trimws(as.character(df[[filter_var]])) == ""
    } else {
      keep <- !is.na(df[[filter_var]]) &
              as.character(df[[filter_var]]) == filter_val
    }
    df <- df[keep, , drop = FALSE]
  }
  gc <- function(var) {
    if (!is.null(var) && nzchar(var %||% "") && var %in% names(df)) {
      as.character(df[[var]])
    } else {
      rep(NA_character_, nrow(df))
    }
  }
  gn <- function(var) {
    if (!is.null(var) && nzchar(var %||% "") && var %in% names(df)) {
      suppressWarnings(as.numeric(df[[var]]))
    } else {
      rep(NA_real_, nrow(df))
    }
  }
  out           <- df
  out$.id       <- gc(id_var)
  out$.analyte  <- gc(analyte_var)
  out$.avisit   <- gc(visit_var)
  out$.visitcd  <- gc(visitcd_var)
  out$.nrrlt    <- gn(nominal_var)
  out$.mrrlt    <- gn(mrrlt_var)
  out$.aval_raw <- gn(aval_var)
  out$.lloq     <- gn(lloq_var)
  out$.route    <- gc(route_var)
  out$.atrt     <- gc(atrt_var)
  out$.arm      <- gc(arm_var)
  out$.cohort   <- gc(cohort_var)
  out$.agegrp   <- gc(agegrp_var)
  out$.bsacat   <- gc(bsacat_var)
  out$.sex      <- gc(sex_var)
  out <- out[!is.na(out$.analyte) & !is.na(out$.avisit), , drop = FALSE]
  out$.visit_display <- ifelse(
    is.na(out$.visitcd) | out$.visitcd == "",
    out$.avisit,
    paste0(out$.avisit, " [", out$.visitcd, "]")
  )
  out$.blq_flag  <- !is.na(out$.aval_raw) & out$.aval_raw == 0
  out$.aval_plot <- out$.aval_raw
  # AVAL is supplied already processed; preserve negative, zero and missing values.
  as.data.frame(out)
}

check_confounding <- function(pd) {
  if (all(is.na(pd$.atrt))) return(FALSE)
  d <- pd[!is.na(pd$.atrt) & !is.na(pd$.avisit), , drop = FALSE]
  if (nrow(d) == 0) return(FALSE)
  tbl <- tapply(d$.atrt, d$.avisit,
                function(x) length(unique(x[!is.na(x)])))
  all(tbl == 1)
}

profile_x_values <- function(pd, xvar) {
  values <- switch(
    xvar,
    nrrlt    = pd$.nrrlt,
    mrrlt    = pd$.mrrlt,
    pctptnum = {
      pctptnum_col <- names(pd)[toupper(names(pd)) == "PCTPTNUM"]
      if (length(pctptnum_col)) pd[[pctptnum_col[1]]] else NULL
    },
    combined = pd$.sas_x,
    stop(sprintf("Unsupported x-axis variable: %s", xvar))
  )
  if (is.null(values)) rep(NA_real_, nrow(pd)) else suppressWarnings(as.numeric(values))
}

profile_x_axis_label <- function(xvar, nominal_var = "NRRLT", mrrlt_var = "MRRLT") {
  switch(
    xvar,
    nrrlt = paste0("Mapped nominal/bucketed time (", nominal_var, ")"),
    mrrlt = paste0("Mapped analysis relative time (", mrrlt_var, ")"),
    pctptnum = "PC timepoint (PCTPTNUM)",
    xvar
  )
}

profile_x_group_quality <- function(pd, visit_col, xvar) {
  x_values <- profile_x_values(pd, xvar)
  usable <- is.finite(x_values) & !is.na(pd[[visit_col]]) & !is.na(pd$.analyte)
  if (!any(usable)) {
    return(list(counts = integer(), total = 0L, curves = 0L))
  }
  group_key <- paste(pd[[visit_col]][usable], pd$.analyte[usable], sep = " | ")
  counts <- tapply(x_values[usable], group_key, function(x) length(unique(x)))
  list(counts = counts, total = length(counts), curves = sum(counts >= 2L))
}

format_profile_time_labels <- function(x) {
  labels <- formatC(round(as.numeric(x), 3), format = "f", digits = 3)
  sub("\\.?0+$", "", labels)
}

resolve_profile_time_window <- function(requested, limits) {
  limits <- suppressWarnings(as.numeric(limits))
  if (length(limits) != 2L || any(!is.finite(limits)) || diff(limits) <= 0) {
    return(NULL)
  }
  requested <- suppressWarnings(as.numeric(requested))
  if (length(requested) != 2L || any(!is.finite(requested))) return(limits)
  requested <- sort(requested)
  if (requested[2] < limits[1] || requested[1] > limits[2]) return(limits)
  pmax(limits[1], pmin(limits[2], requested))
}

prepare_combined_visit_axis <- function(pd, visit_col, visits, xvar, xlab,
                                        spacing = c("numeric", "equal")) {
  spacing <- match.arg(spacing)
  pd$.sas_visit <- as.character(pd[[visit_col]])
  pd$.sas_time_raw <- profile_x_values(pd, xvar)
  pd$.sas_x <- NA_real_
  visits <- as.character(visits)
  visits <- visits[visits %in% unique(pd$.sas_visit)]
  pd <- pd[pd$.sas_visit %in% visits & is.finite(pd$.sas_time_raw), , drop = FALSE]
  if (nrow(pd) == 0 || length(visits) == 0) {
    return(list(data = pd, separators = numeric(), labels = data.frame(),
                breaks = NULL, break_labels = NULL, xlab = xlab, warning = NULL))
  }

  label_rows <- vector("list", length(visits))
  separators <- numeric()
  axis_breaks <- numeric()
  axis_labels <- character()
  time_counts <- setNames(vapply(visits, function(v) {
    length(unique(pd$.sas_time_raw[pd$.sas_visit == v]))
  }, integer(1)), visits)
  sparse_visits <- names(time_counts)[time_counts < 2L]
  axis_warning <- if (length(sparse_visits)) {
    shown <- utils::head(sparse_visits, 3L)
    suffix <- if (length(sparse_visits) > 3L) " ..." else ""
    paste0(
      "The selected X-axis (", xlab, ") has only one unique value in ",
      paste(shown, collapse = ", "), suffix,
      ". Choose a mapped nominal or relative-time variable with multiple within-visit timepoints."
    )
  } else {
    NULL
  }

  if (identical(spacing, "equal")) {
    panel_width <- max(3, max(time_counts) - 1)
    gap <- max(1, panel_width * 0.25)
    cursor <- 0
    previous_right <- NA_real_
    for (i in seq_along(visits)) {
      idx <- pd$.sas_visit == visits[i]
      raw <- pd$.sas_time_raw[idx]
      visit_times <- sort(unique(raw))
      visit_positions <- if (length(visit_times) == 1L) {
        cursor + panel_width / 2
      } else {
        seq(cursor, cursor + panel_width, length.out = length(visit_times))
      }
      pd$.sas_x[idx] <- visit_positions[match(raw, visit_times)]
      left_plot <- cursor
      right_plot <- cursor + panel_width
      label_rows[[i]] <- data.frame(x = mean(c(left_plot, right_plot)), label = visits[i])
      if (i > 1L) separators <- c(separators, mean(c(previous_right, left_plot)))
      axis_breaks <- c(axis_breaks, visit_positions)
      axis_labels <- c(axis_labels, format_profile_time_labels(visit_times))
      previous_right <- right_plot
      cursor <- right_plot + gap
    }
  } else if (identical(xvar, "mrrlt")) {
    pd$.sas_x <- pd$.sas_time_raw
    ranges <- lapply(visits, function(v) {
      x <- pd$.sas_x[pd$.sas_visit == v]
      c(min = min(x), max = max(x), center = stats::median(x))
    })
    for (i in seq_along(visits)) {
      label_rows[[i]] <- data.frame(x = ranges[[i]]["center"], label = visits[i])
      if (i > 1L) {
        previous_max <- ranges[[i - 1L]]["max"]
        current_min <- ranges[[i]]["min"]
        separators <- c(separators, if (current_min > previous_max) {
          mean(c(previous_max, current_min))
        } else {
          current_min
        })
      }
    }
  } else {
    all_range <- range(pd$.sas_time_raw, na.rm = TRUE)
    base_span <- diff(all_range)
    if (!is.finite(base_span) || base_span <= 0) base_span <- 1
    gap <- max(base_span * 0.12, 1)
    cursor <- 0
    previous_right <- NA_real_
    for (i in seq_along(visits)) {
      idx <- pd$.sas_visit == visits[i]
      raw <- pd$.sas_time_raw[idx]
      left_raw <- min(raw)
      right_raw <- max(raw)
      offset <- cursor - left_raw
      pd$.sas_x[idx] <- raw + offset
      left_plot <- left_raw + offset
      right_plot <- right_raw + offset
      label_rows[[i]] <- data.frame(x = mean(c(left_plot, right_plot)), label = visits[i])
      if (i > 1L) separators <- c(separators, mean(c(previous_right, left_plot)))

      visit_breaks <- sort(unique(raw))
      if (length(visit_breaks) > 7L) {
        visit_breaks <- pretty(range(raw), n = 5)
        visit_breaks <- visit_breaks[visit_breaks >= left_raw & visit_breaks <= right_raw]
      }
      axis_breaks <- c(axis_breaks, visit_breaks + offset)
      axis_labels <- c(axis_labels, format_profile_time_labels(visit_breaks))
      previous_right <- right_plot
      cursor <- right_plot + gap
    }
    xlab <- paste0(xlab, " (restarts within each visit)")
  }
  list(
    data = pd,
    separators = separators,
    labels = do.call(rbind, label_rows),
    breaks = if (identical(spacing, "numeric") && identical(xvar, "mrrlt")) NULL else axis_breaks,
    break_labels = if (identical(spacing, "numeric") && identical(xvar, "mrrlt")) NULL else axis_labels,
    xlab = xlab,
    warning = axis_warning
  )
}

add_combined_visit_guides <- function(g, axis_info) {
  if (length(axis_info$separators)) {
    g <- g + geom_vline(
      xintercept = axis_info$separators,
      colour = "grey35", linetype = "dashed", linewidth = 0.55
    )
  }
  if (nrow(axis_info$labels)) {
    g <- g + geom_text(
      data = axis_info$labels,
      aes(x = x, y = Inf, label = label),
      inherit.aes = FALSE, vjust = 1.4, size = 3.1,
      fontface = "bold", colour = "grey25"
    )
  }
  if (!is.null(axis_info$breaks)) {
    g$scales$scales <- Filter(
      function(scale) !any(scale$aesthetics %in% "x"),
      g$scales$scales
    )
    g <- g + scale_x_continuous(
      name = axis_info$xlab,
      breaks = axis_info$breaks,
      labels = axis_info$break_labels,
      limits = c(NA, NA),
      expand = expansion(mult = c(0.01, 0.04)),
      guide = guide_axis(angle = 45, n.dodge = 2, check.overlap = TRUE)
    )
  }
  g + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.title.x = element_text(margin = margin(t = 18)),
    plot.margin = margin(t = 7, r = 7, b = 20, l = 7)
  )
}

make_spaghetti <- function(pd,
                            xvar         = "nrrlt",
                            yscale       = "log",
                            colour_var   = ".route",
                            colour_map   = NULL,
                            title        = "",
                            subtitle     = "",
                            xlab         = "Nominal Time (h)",
                            ylab         = "Concentration",
                            legend_title = "Group",
                            alpha        = 0.75,
                            line_size    = 0.8,
                            line_group_var = NULL) {
  pd$X_VAR   <- profile_x_values(pd, xvar)
  pd$COL_VAR <- as.character(pd[[colour_var]])
  pd$LINE_GROUP <- if (!is.null(line_group_var) && line_group_var %in% names(pd)) {
    interaction(pd$.id, pd$COL_VAR, pd[[line_group_var]], drop = TRUE)
  } else {
    interaction(pd$.id, pd$COL_VAR, drop = TRUE)
  }
  tip_time <- if (".sas_time_raw" %in% names(pd)) pd$.sas_time_raw else pd$X_VAR
  if (is.null(colour_map)) colour_map <- make_colour_map(pd$COL_VAR)
  pd$TIP <- paste0(
    "Subject: ", pd$.id,
    "\nAnalyte: ", pd$.analyte,
    "\nVisit: ",   pd$.visit_display,
    "\nTime: ",    round(tip_time, 2), " h",
    "\nAVAL: ",    round(pd$.aval_plot, 3),
    ifelse(pd$.blq_flag, " [AVAL=0; supplied value]", "")
  )
  p <- ggplot(
    pd,
    aes(
      x      = X_VAR,
      y      = .aval_plot,
      colour = COL_VAR,
      group  = LINE_GROUP,
      text   = TIP
    )
  ) +
    geom_line(linewidth = line_size, alpha = alpha, na.rm = TRUE) +
    geom_point(
      data  = pd[!pd$.blq_flag, , drop = FALSE],
      size  = 2, alpha = alpha, na.rm = TRUE
    ) +
    geom_point(
      data        = pd[pd$.blq_flag, , drop = FALSE],
      shape       = 6, size = 2, colour = "grey55",
      show.legend = FALSE, na.rm = TRUE
    ) +
    scale_colour_manual(name = legend_title, values = colour_map) +
    scale_x_continuous(
      name   = xlab,
      limits = c(NA, NA),
      expand = expansion(mult = c(0, 0.04))
    ) +
    ggtitle(label = title, subtitle = subtitle) +
    theme_bw(base_size = 11) +
    theme(
      plot.title       = element_text(size = 11, face = "bold",  hjust = 0.5),
      plot.subtitle    = element_text(size =  9, colour = "grey35", hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position  = "right",
      legend.text      = element_text(size = 8),
      axis.title       = element_text(size = 9.5)
    )
  if (yscale == "log" && any(!is.na(pd$.aval_plot) & pd$.aval_plot <= 0)) {
    yscale <- "linear"
    p <- p + labs(caption = "Linear scale used to display supplied zero/negative AVAL values.")
  }
  if (yscale == "log" && any(is.finite(pd$.aval_plot) & pd$.aval_plot > 0)) {
    y_min <- min(pd$.aval_plot[pd$.aval_plot > 0], na.rm = TRUE)
    y_max <- max(pd$.aval_plot, na.rm = TRUE)
    lo    <- max(0.001, y_min * 0.5)
    hi    <- y_max * 2
    p <- p +
      scale_y_log10(
        name   = ylab,
        breaks = 10^seq(floor(log10(lo)), ceiling(log10(hi))),
        labels = scales::label_comma(),
        limits = c(lo, hi)
      ) +
      annotation_logticks(sides = "l", colour = "grey60", size = 0.25)
  } else {
    p <- p +
      scale_y_continuous(
        name   = ylab,
        labels = scales::label_comma(),
        limits = c(NA, NA),
        expand = expansion(mult = c(0, 0.05))
      )
  }
  p
}

summarise_subject_profiles <- function(pd, xvar = "nrrlt", group_var = NULL) {
  x_values <- profile_x_values(pd, xvar)
  group_values <- if (!is.null(group_var) && group_var %in% names(pd)) {
    as.character(pd[[group_var]])
  } else {
    rep("Overall", nrow(pd))
  }
  group_values[is.na(group_values) | !nzchar(group_values)] <- "(Missing)"
  visit_values <- if (".sas_visit" %in% names(pd)) {
    as.character(pd$.sas_visit)
  } else {
    rep("", nrow(pd))
  }
  raw_time_values <- if (".sas_time_raw" %in% names(pd)) {
    as.numeric(pd$.sas_time_raw)
  } else {
    x_values
  }
  subject_level <- data.frame(
    SUBJECT = as.character(pd$.id),
    GROUP = group_values,
    VISIT = visit_values,
    VISIT_TIME = raw_time_values,
    X_VAR = as.numeric(x_values),
    AVAL = as.numeric(pd$.aval_plot),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(SUBJECT), nzchar(SUBJECT), is.finite(X_VAR), is.finite(AVAL)) %>%
    group_by(SUBJECT, GROUP, VISIT, VISIT_TIME, X_VAR) %>%
    summarise(SUBJECT_VALUE = mean(AVAL), .groups = "drop")

  subject_level %>%
    group_by(GROUP, VISIT, VISIT_TIME, X_VAR) %>%
    summarise(
      MEAN = mean(SUBJECT_VALUE),
      SD = if (dplyr::n() > 1L) stats::sd(SUBJECT_VALUE) else NA_real_,
      N_SUBJECTS = dplyr::n_distinct(SUBJECT),
      .groups = "drop"
    ) %>%
    mutate(
      LOWER = ifelse(is.na(SD), MEAN, MEAN - SD),
      UPPER = ifelse(is.na(SD), MEAN, MEAN + SD)
    ) %>%
    arrange(GROUP, VISIT, X_VAR)
}

format_concentration_stat <- function(x) {
  if (!length(x) || !is.finite(x)) return("NA")
  format_profile_time_labels(signif(x, 6))
}

make_concentration_summary_tables <- function(pd,
                                              xvar = "nrrlt",
                                              group_var = NULL,
                                              group_label = "Group") {
  nominal_time <- profile_x_values(pd, "nrrlt")
  if (!any(is.finite(nominal_time))) nominal_time <- profile_x_values(pd, xvar)
  group_values <- if (!is.null(group_var) && group_var %in% names(pd)) {
    as.character(pd[[group_var]])
  } else {
    rep("Overall", nrow(pd))
  }
  group_values[is.na(group_values) | !nzchar(group_values)] <- "(Missing)"
  visit_values <- if (".sas_visit" %in% names(pd)) {
    as.character(pd$.sas_visit)
  } else if (".visit_display" %in% names(pd)) {
    as.character(pd$.visit_display)
  } else {
    rep("", nrow(pd))
  }
  visit_values[is.na(visit_values)] <- ""
  subject_level <- data.frame(
    SUBJECT = as.character(pd$.id),
    GROUP = group_values,
    VISIT = visit_values,
    TIME = suppressWarnings(as.numeric(nominal_time)),
    AVAL = suppressWarnings(as.numeric(pd$.aval_plot)),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(SUBJECT), nzchar(SUBJECT), is.finite(TIME), is.finite(AVAL)) %>%
    group_by(SUBJECT, GROUP, VISIT, TIME) %>%
    summarise(VALUE = mean(AVAL), .groups = "drop")
  if (nrow(subject_level) == 0) return(list())

  page_keys <- unique(subject_level[, c("GROUP", "VISIT"), drop = FALSE])
  page_keys <- page_keys[order(page_keys$GROUP, page_keys$VISIT), , drop = FALSE]
  pages <- vector("list", nrow(page_keys))
  page_names <- character(nrow(page_keys))
  statistic_names <- c(
    "N", "Mean", "SD", "Median", "Min - Max (Range)",
    "Geometric Mean", "Geometric CV (%)"
  )
  for (i in seq_len(nrow(page_keys))) {
    key <- page_keys[i, , drop = FALSE]
    page_data <- subject_level[
      subject_level$GROUP == key$GROUP & subject_level$VISIT == key$VISIT,
      , drop = FALSE
    ]
    times <- sort(unique(page_data$TIME))
    time_labels <- make.unique(format_profile_time_labels(times), sep = " #")
    page <- data.frame(Statistic = statistic_names, stringsAsFactors = FALSE)
    for (j in seq_along(times)) {
      time_value <- times[j]
      values <- page_data$VALUE[page_data$TIME == time_value]
      n_value <- length(values)
      sd_value <- if (n_value > 1L) stats::sd(values) else NA_real_
      min_value <- min(values)
      max_value <- max(values)
      all_positive <- all(values > 0)
      geo_mean <- if (all_positive) exp(mean(log(values))) else NA_real_
      geo_cv <- if (all_positive && n_value > 1L) {
        100 * sqrt(exp(stats::var(log(values))) - 1)
      } else {
        NA_real_
      }
      column_values <- c(
        as.character(n_value),
        format_concentration_stat(mean(values)),
        format_concentration_stat(sd_value),
        format_concentration_stat(stats::median(values)),
        paste0(
          format_concentration_stat(min_value), " - ",
          format_concentration_stat(max_value), " (",
          format_concentration_stat(max_value - min_value), ")"
        ),
        format_concentration_stat(geo_mean),
        format_concentration_stat(geo_cv)
      )
      page[[time_labels[j]]] <- column_values
    }
    group_part <- if (identical(key$GROUP, "Overall")) {
      "Overall"
    } else {
      paste0(group_label, ": ", key$GROUP)
    }
    page_names[i] <- if (nzchar(key$VISIT)) {
      paste0(group_part, " | Visit: ", key$VISIT)
    } else {
      group_part
    }
    pages[[i]] <- page
  }
  names(pages) <- make.unique(page_names)
  pages
}

make_mean_sd_profile <- function(pd,
                                 xvar = "nrrlt",
                                 yscale = "linear",
                                 title = "",
                                 subtitle = "",
                                 xlab = "Nominal Time (h)",
                                 ylab = "Concentration") {
  smry <- summarise_subject_profiles(pd, xvar)
  if (nrow(smry) == 0) return(list(plot = NULL, data = smry))

  raw_values <- pd$.aval_plot[is.finite(pd$.aval_plot)]
  use_log <- identical(yscale, "log") &&
    length(raw_values) > 0 && all(raw_values > 0) && all(smry$MEAN > 0)
  caption <- NULL
  if (identical(yscale, "log") && !use_log) {
    caption <- "Linear scale used to display supplied zero/negative AVAL values."
  }

  smry$LOWER_PLOT <- smry$LOWER
  if (use_log) {
    positive_floor <- max(0.001, min(raw_values) * 0.5)
    clipped <- smry$LOWER_PLOT <= 0
    smry$LOWER_PLOT[clipped] <- positive_floor
    if (any(clipped)) {
      caption <- "Lower mean - SD bound clipped at the positive plotting limit for log10 display."
    }
  }
  smry$TIP <- paste0(
    ifelse(nzchar(smry$VISIT), paste0("Visit: ", smry$VISIT, "\n"), ""),
    "Time: ", round(smry$VISIT_TIME, 3),
    "\nMean: ", round(smry$MEAN, 3),
    "\nSD: ", ifelse(is.na(smry$SD), "NA", round(smry$SD, 3)),
    "\nN subjects: ", smry$N_SUBJECTS
  )

  p <- ggplot(smry, aes(x = X_VAR, y = MEAN, text = TIP, group = VISIT)) +
    geom_ribbon(
      aes(x = X_VAR, ymin = LOWER_PLOT, ymax = UPPER, group = VISIT),
      fill = "#1565C0", alpha = 0.18, colour = NA, inherit.aes = FALSE
    ) +
    geom_line(colour = "#1565C0", linewidth = 1) +
    geom_point(colour = "#1565C0", size = 2.4) +
    scale_x_continuous(
      name = xlab,
      limits = c(NA, NA),
      expand = expansion(mult = c(0, 0.04))
    ) +
    ggtitle(label = title, subtitle = subtitle) +
    labs(caption = caption) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, colour = "grey35", hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 9.5)
    )
  if (use_log) {
    y_max <- max(smry$UPPER, na.rm = TRUE)
    p <- p +
      scale_y_log10(
        name = ylab,
        breaks = 10^seq(floor(log10(positive_floor)), ceiling(log10(y_max * 2))),
        labels = scales::label_comma(),
        limits = c(positive_floor, y_max * 2)
      ) +
      annotation_logticks(sides = "l", colour = "grey60", size = 0.25)
  } else {
    p <- p +
      scale_y_continuous(
        name = ylab,
        labels = scales::label_comma(),
        limits = c(NA, NA),
        expand = expansion(mult = c(0, 0.05))
      )
  }
  list(
    plot = p,
    data = smry %>%
      transmute(
        Visit = VISIT, `Visit Time` = VISIT_TIME, `Plot Time` = X_VAR,
        Mean = MEAN, SD = SD, `N Subjects` = N_SUBJECTS
      ) %>%
      { if (all(.$Visit == "")) {
          transmute(., Time = `Plot Time`, Mean, SD, `N Subjects`)
        } else . }
  )
}

make_mean_sd_errorbar_profile <- function(pd,
                                          xvar = "nrrlt",
                                          yscale = "linear",
                                          title = "",
                                          subtitle = "",
                                          xlab = "Nominal Time (h)",
                                          ylab = "Concentration",
                                          group_var = NULL,
                                          group_label = "Group",
                                          sd_sides = c("two", "upper"),
                                          jitter_pct = 0) {
  sd_sides <- match.arg(sd_sides)
  jitter_pct <- suppressWarnings(as.numeric(jitter_pct))
  if (!length(jitter_pct) || !is.finite(jitter_pct)) jitter_pct <- 0
  jitter_pct <- max(0, jitter_pct)
  smry <- summarise_subject_profiles(pd, xvar, group_var = group_var)
  if (nrow(smry) == 0) return(list(plot = NULL, data = smry))

  group_levels <- sort(unique(as.character(smry$GROUP)))
  smry$GROUP <- factor(smry$GROUP, levels = group_levels)
  group_offsets <- if (length(group_levels) > 1L && jitter_pct > 0) {
    setNames(seq(-1, 1, length.out = length(group_levels)), group_levels)
  } else {
    setNames(rep(0, length(group_levels)), group_levels)
  }
  positive_positions <- sort(unique(abs(smry$X_VAR[is.finite(smry$X_VAR) & smry$X_VAR != 0])))
  zero_reference <- if (length(positive_positions)) positive_positions[1] else 1
  jitter_reference <- ifelse(abs(smry$X_VAR) > .Machine$double.eps,
                             abs(smry$X_VAR), zero_reference)
  smry$X_PLOT <- smry$X_VAR +
    unname(group_offsets[as.character(smry$GROUP)]) * jitter_reference * jitter_pct / 100
  smry$SERIES <- interaction(smry$GROUP, smry$VISIT, drop = TRUE)

  raw_values <- pd$.aval_plot[is.finite(pd$.aval_plot)]
  use_log <- identical(yscale, "log") && length(raw_values) > 0 &&
    all(raw_values > 0) && all(smry$MEAN > 0)
  smry$LOWER_PLOT <- if (identical(sd_sides, "upper")) smry$MEAN else smry$LOWER
  smry$UPPER_PLOT <- smry$UPPER
  caption_parts <- character()
  if (identical(yscale, "log") && !use_log) {
    caption_parts <- c(caption_parts,
      "Linear scale used to display supplied zero/negative AVAL values.")
  }
  if (use_log) {
    positive_floor <- max(0.001, min(raw_values) * 0.5)
    clipped <- smry$LOWER_PLOT <= 0
    smry$LOWER_PLOT[clipped] <- positive_floor
    if (any(clipped)) {
      caption_parts <- c(caption_parts,
        "Lower mean - SD bound clipped at the positive plotting limit for log10 display.")
    }
  }
  if (jitter_pct > 0 && length(group_levels) > 1L) {
    caption_parts <- c(caption_parts, paste0(
      "Summary groups are horizontally offset by up to ",
      format_profile_time_labels(jitter_pct), "% of the nominal-time position."
    ))
  }
  caption <- if (length(caption_parts)) paste(caption_parts, collapse = " ") else NULL
  smry$TIP <- paste0(
    if (length(group_levels) > 1L || !identical(group_levels, "Overall")) {
      paste0(group_label, ": ", smry$GROUP, "\n")
    } else "",
    ifelse(nzchar(smry$VISIT), paste0("Visit: ", smry$VISIT, "\n"), ""),
    "Nominal time: ", round(smry$VISIT_TIME, 3),
    if (jitter_pct > 0 && length(group_levels) > 1L) {
      paste0("\nDisplayed time: ", round(smry$X_PLOT, 3))
    } else "",
    "\nMean: ", round(smry$MEAN, 3),
    "\nSD: ", ifelse(is.na(smry$SD), "NA", round(smry$SD, 3)),
    "\nN subjects: ", smry$N_SUBJECTS
  )
  finite_positions <- sort(unique(smry$X_PLOT[is.finite(smry$X_PLOT)]))
  position_gaps <- diff(finite_positions)
  position_gaps <- position_gaps[position_gaps > 0]
  cap_width <- if (length(position_gaps)) min(position_gaps) * 0.12 else 0.1
  colours <- make_colour_map(group_levels)
  shapes <- setNames(rep(c(16, 17, 15, 18, 8, 3, 7), length.out = length(group_levels)),
                     group_levels)
  line_types <- setNames(rep(c("solid", "dashed", "dotted", "dotdash", "longdash"),
                             length.out = length(group_levels)), group_levels)
  show_group_legend <- length(group_levels) > 1L || !identical(group_levels, "Overall")

  p <- ggplot(
    smry,
    aes(
      x = X_PLOT, y = MEAN, group = SERIES, colour = GROUP,
      shape = GROUP, linetype = GROUP, text = TIP
    )
  ) +
    geom_errorbar(
      aes(ymin = LOWER_PLOT, ymax = UPPER_PLOT),
      width = cap_width, linewidth = 0.55, na.rm = TRUE
    ) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 2.5, stroke = 0.8, na.rm = TRUE) +
    scale_colour_manual(name = group_label, values = colours) +
    scale_shape_manual(name = group_label, values = shapes) +
    scale_linetype_manual(name = group_label, values = line_types) +
    scale_x_continuous(
      name = xlab, limits = c(NA, NA), expand = expansion(mult = c(0.02, 0.04))
    ) +
    ggtitle(label = title, subtitle = subtitle) +
    labs(caption = caption) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, colour = "grey35", hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = if (show_group_legend) "right" else "none",
      legend.text = element_text(size = 8),
      axis.title = element_text(size = 9.5)
    )
  if (use_log) {
    y_max <- max(smry$UPPER_PLOT, na.rm = TRUE)
    p <- p +
      scale_y_log10(
        name = ylab,
        breaks = 10^seq(floor(log10(positive_floor)), ceiling(log10(y_max * 2))),
        labels = scales::label_comma(),
        limits = c(positive_floor, y_max * 2)
      ) +
      annotation_logticks(sides = "l", colour = "grey60", size = 0.25)
  } else {
    p <- p +
      scale_y_continuous(
        name = ylab, labels = scales::label_comma(), limits = c(NA, NA),
        expand = expansion(mult = c(0, 0.05))
      )
  }
  output_data <- smry %>%
    transmute(
      Group = as.character(GROUP), Visit = VISIT, `Visit Time` = VISIT_TIME,
      `Plot Time` = X_VAR, `Displayed Time` = X_PLOT,
      Mean = MEAN, SD = SD, `N Subjects` = N_SUBJECTS
    )
  if (all(output_data$Group == "Overall") && all(output_data$Visit == "")) {
    output_data <- output_data %>%
      transmute(Time = `Plot Time`, `Displayed Time`, Mean, SD, `N Subjects`)
  }
  list(plot = p, data = output_data)
}

as_plotly <- function(g, w = NULL, h = NULL, combined = FALSE) {
  legend_y <- if (isTRUE(combined)) -0.42 else -0.22
  bottom_margin <- if (isTRUE(combined)) 205 else 110
  x_axis <- if (isTRUE(combined)) {
    list(tickangle = -45, automargin = TRUE)
  } else {
    list(automargin = TRUE)
  }
  ggplotly(g, tooltip = "text", width = w, height = h) %>%
    layout(
      legend = list(orientation = "h", x = 0, y = legend_y, font = list(size = 10)),
      xaxis = x_axis,
      margin = list(l = 65, r = 25, t = 85, b = bottom_margin), autosize = TRUE
    ) %>%
    config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("autoScale2d", "lasso2d", "select2d"),
      displaylogo            = FALSE
    )
}


# V11 presentation helpers. Review and PK calculations remain unchanged.
setup_section <- function(title, ..., expanded = FALSE) {
  tags$details(class = "setup-section", open = if (expanded) "open" else NULL,
    tags$summary(title), div(class = "setup-body", ...))
}
case_selector <- function(inputId, label, ...) {
  args <- list(...)
  args$choices <- unique(c(
    args$choices,
    "Profiles with PKSUMXFL=Y", "Profiles with PKSUMXFL=N or empty",
    "Profiles with NCA01FL=Y", "Profiles with NCA01FL=N or empty",
    "Profiles with ANL01FL=Y", "Profiles with ANL01FL=N or empty",
    "Profiles with ANL02FL=Y", "Profiles with ANL02FL=N or empty"
  ))
  tags$details(class = "case-section", tags$summary("Specific cases to show",
    tags$span(class = "case-count", " (all cases)")),
    do.call(checkboxGroupInput, c(list(inputId = inputId, label = NULL), args)))
}
review_datatable <- function(data, ..., options = list(), extensions = character(),
                             class = "compact stripe nowrap") {
  defaults <- list(scrollX = TRUE, scrollY = "420px", scrollCollapse = TRUE,
    pageLength = 10, lengthMenu = c(10, 25, 50, 100), dom = "Blfrtip",
    buttons = list("colvis"), fixedColumns = list(leftColumns = 2))
  opts <- utils::modifyList(defaults, options)
  if (!"colvis" %in% unlist(opts$buttons)) opts$buttons <- c(opts$buttons, list("colvis"))
  DT::datatable(data, ..., options = opts,
    extensions = unique(c(extensions, "Buttons", "FixedColumns")), class = class)
}
page_slice <- function(values, page = 1L, size = 6L) {
  size <- max(1L, as.integer(size))
  pages <- max(1L, ceiling(length(values) / size))
  page <- if (length(page) != 1L || is.na(page)) 1L else max(1L, min(pages, as.integer(page)))
  start <- (page - 1L) * size + 1L
  list(values = if (length(values)) values[seq.int(start, min(length(values), start + size - 1L))] else values,
       page = page, pages = pages, total = length(values))
}

review_finding_specs <- list(
  flagged_records = list(title = "Flagged records", interest_col = "review_fl"),
  iqr_outlier = list(title = "IQR outliers", interest_col = "arrlt_outlierfl"),
  large_pct = list(title = "Large % deviation", interest_col = "arrlt_bigdevfl"),
  large_abs = list(title = "Large absolute deviation", interest_col = "arrlt_absdevfl"),
  predose_postdose = list(title = "PREdose taken POST-dose", interest_col = "predose_postdose_fl"),
  postdose_predose = list(title = "POST-dose taken PREdose", interest_col = "postdose_predose_fl"),
  predose_early = list(title = "PREdose suspiciously early", interest_col = "predose_early_fl"),
  relative_mismatch = list(title = "Analysis relative differs from calculated", interest_col = "mrrlt_ne_arrlt_fl"),
  duplicate_nominal = list(title = "Duplicate nominal time", interest_col = "nrrlt_fl"),
  duplicate_relative = list(title = "Duplicate analysis-relative time", interest_col = "mrrlt_fl")
)

build_review_finding_subset <- function(data, interest, key_cols) {
  data <- as.data.frame(data)
  interest <- as.logical(interest)
  interest[is.na(interest)] <- FALSE
  if (length(interest) != nrow(data)) stop("Interest indicator must have one value per record.")
  key_cols <- unique(intersect(key_cols, names(data)))
  if (!nrow(data) || !any(interest) || !length(key_cols)) {
    empty <- data[0, , drop = FALSE]
    empty$RECORD_OF_INTEREST <- character()
    return(list(data = empty, interest_records = 0L, profiles = 0L, key_cols = key_cols))
  }
  signatures <- profile_signature(data, key_cols)
  affected <- unique(signatures[interest])
  keep <- signatures %in% affected
  out <- data[keep, , drop = FALSE]
  out$RECORD_OF_INTEREST <- ifelse(interest[keep], "YES", "")
  out <- out[, c("RECORD_OF_INTEREST", key_cols,
                 setdiff(names(out), c("RECORD_OF_INTEREST", key_cols))), drop = FALSE]
  rownames(out) <- NULL
  list(
    data = out,
    interest_records = sum(interest),
    profiles = length(affected),
    key_cols = key_cols
  )
}

order_review_finding_columns <- function(data, key_cols = character(), mappings = list()) {
  data <- as.data.frame(data)
  actual_names <- names(data)
  resolve_one <- function(candidates) {
    candidates <- as.character(candidates %||% character())
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    if (!length(candidates)) return(character())
    idx <- match(toupper(candidates), toupper(actual_names))
    idx <- idx[!is.na(idx)]
    if (length(idx)) actual_names[idx[1]] else character()
  }
  resolve_each <- function(candidates) {
    unique(unlist(lapply(candidates, resolve_one), use.names = FALSE))
  }

  id_cols <- resolve_each(c("USUBJID", "SUBJID"))
  analyte_col <- resolve_one(c(mappings$analyte, "PCTESTCD", "PCTEST", "ATESTCD", "ATEST"))
  visit_col <- resolve_one(c(mappings$visit, "AVISIT", "VISIT"))
  excluded_group_names <- toupper(c(
    id_cols, "APROFILE", "PROFILE", "NRRLT", "MRRLT", "ARRLT", "ADTM",
    "PCRFTDTM", "PCRFTDTC", "ARFTDTM", "ARFTDTC"
  ))
  key_group_cols <- key_cols[
    toupper(key_cols) %in% toupper(actual_names) & !toupper(key_cols) %in% excluded_group_names
  ]
  key_group_cols <- actual_names[match(toupper(key_group_cols), toupper(actual_names))]
  context_cols <- resolve_each(c("COHORT", "COHORTCD", "ARMCD", "ARM"))
  grouping_cols <- unique(c(analyte_col, visit_col, key_group_cols, context_cols))

  preferred <- unique(c(
    resolve_one("RECORD_OF_INTEREST"),
    id_cols,
    grouping_cols,
    resolve_one("NRRLT"), resolve_one(mappings$nominal),
    resolve_one("MRRLT"), resolve_one(mappings$mrrlt),
    resolve_one("ARRLT"), resolve_one(mappings$actual),
    resolve_one("ADTM"), resolve_one(mappings$sample_time),
    resolve_one(mappings$dose_time),
    resolve_each(c("PCRFTDTM", "PCRFTDTC", "ARFTDTM", "ARFTDTC")),
    resolve_one(c(mappings$profile, "APROFILE", "PROFILE"))
  ))
  data[, c(preferred, setdiff(actual_names, preferred)), drop = FALSE]
}

pager_controls <- function(prefix, noun = "panels") {
  div(class = "page-toolbar",
    actionButton(paste0(prefix, "_prev"), "Previous", class = "btn-sm"),
    numericInput(paste0(prefix, "_page"), "Page", 1, min = 1, step = 1, width = "90px"),
    actionButton(paste0(prefix, "_next"), "Next", class = "btn-sm"),
    textOutput(paste0(prefix, "_page_status"), inline = TRUE))
}

pk_profiles_toolbar <- function(button_id, status_id) {
  div(class = "pk-prepare-toolbar",
    actionBttn(button_id, "Prepare / Update PK Profiles", style = "fill", color = "success"),
    textOutput(status_id, inline = TRUE))
}

pk_filter_controls <- function(prefix) {
  setup_section(
    "PK record filter (independent of Review)", expanded = TRUE,
    tags$p(class = "help-note",
      "Optional: filter the loaded dataset for all PK Profile tabs when Prepare / Update is clicked."),
    uiOutput(paste0(prefix, "_filter_variable_ui")),
    uiOutput(paste0(prefix, "_filter_rule_ui")),
    textOutput(paste0(prefix, "_filter_count"))
  )
}

v11_style <- tags$head(tags$style(HTML("
  body {background:#f4f6f9;color:#243447;font-family:Arial,sans-serif;font-size:14px;}
  .container-fluid {padding:0 20px 20px;}
  .app-header {display:flex;align-items:center;justify-content:space-between;gap:16px;padding:16px 0;}
  .app-header h2 {font-size:23px;margin:0;} .app-header .app-authors {margin-top:4px;color:#3f5268;} .app-header small {display:block;margin-top:3px;color:#64748b;}
  #setup-column {position:sticky;top:12px;max-height:calc(100vh - 24px);overflow-y:auto;}
  #setup-column .well {padding:12px;background:white;border:1px solid #dce3ed;box-shadow:none;}
  .setup-section {border-bottom:1px solid #e3e8ef;margin-bottom:8px;}
  summary {cursor:pointer;font-weight:600;padding:11px 4px;color:#234e75;}
  summary:focus-visible {outline:2px solid #2474ba;}
  .setup-body {padding:4px 4px 12px;} .setup-body hr {margin:12px 0;}
  .setup-body .form-group {margin-bottom:12px;}
  .setup-actions {position:sticky;top:0;background:white;z-index:5;padding:4px 0 12px;display:flex;gap:6px;flex-wrap:wrap;}
  #conc_group_plot, #facet_group_plot, #focus_plot, #profile_plot {height:clamp(380px,62vh,680px) !important;}
  #workspace-column {background:white;border:1px solid #dce3ed;border-radius:8px;padding:16px;min-width:0;}
  .setup-hidden #setup-column {display:none;} .setup-hidden #workspace-column {width:100%;}
  .nav-tabs > li > a {font-weight:600;} .nav-tabs {margin-bottom:12px;}
  .active-view {font-size:18px;font-weight:600;margin:12px 0;color:#234e75;}
  .case-section {background:#f7f9fc;border:1px solid #dce3ed;border-radius:5px;padding:0 10px;margin:8px 0 14px;}
  .case-section .shiny-options-group {display:grid;grid-template-columns:repeat(auto-fit,minmax(235px,1fr));gap:0 12px;}
  .case-section .checkbox {margin:5px 0;} .case-count {font-weight:normal;}
  .page-toolbar {display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin:12px 0;}
  .page-toolbar .form-group {margin:0;display:flex;align-items:center;gap:8px;}
  .page-toolbar .control-label {margin:0;} .page-toolbar input {width:65px;}
  .page-toolbar .shiny-input-container {width:110px !important;}
  .pk-prepare-toolbar {display:flex;align-items:center;gap:14px;flex-wrap:wrap;margin:0 0 16px;padding:12px 14px;background:#f0fdf8;border:1px solid #9ddbc8;border-radius:6px;}
  .pk-prepare-toolbar .shiny-text-output {font-weight:600;color:#245b4b;}
  #ind_subjects + .selectize-control .selectize-input {max-height:110px;overflow-y:auto;}
  table.dataTable tbody td {padding:5px 8px !important;vertical-align:top;}
  table.dataTable thead th {font-size:12px;} .dataTables_wrapper {margin:10px 0 18px;}
  #status_msg {font-size:14px;line-height:1.65;padding:18px;overflow-wrap:anywhere;font-family:monospace;white-space:normal;}
  #status_msg .review-summary-line {display:block;min-height:1.65em;}
  #status_msg .review-summary-link {color:#1565C0;text-decoration:underline;text-decoration-thickness:1px;text-underline-offset:2px;}
  .dataTables_wrapper .dt-buttons {margin:0 12px 8px 0;} pre {white-space:pre-wrap;font-size:12px;}
  .profile-grid {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px;}
  .profile-grid.single {grid-template-columns:minmax(0,1fr);}
  .profile-grid > div {min-width:0;} .plotly {max-width:100%;}
  .help-note {color:#5b6c80;margin:8px 0;}
  .ai-privacy-warning {margin:10px 0 14px;padding:12px 14px;border:2px solid #b42318;border-radius:6px;background:#fff1f0;color:#7a271a;}
  .ai-privacy-warning a {color:#7a271a;font-weight:700;text-decoration:underline;}
  .ai-toolbar {display:flex;align-items:center;gap:10px;flex-wrap:wrap;margin:8px 0 14px;}
  .ai-status {padding:6px 10px;border-radius:4px;background:#eef4f8;color:#234e75;}
  .ai-chat {border:1px solid #dce3ed;border-radius:6px;background:#f8fafc;padding:12px;min-height:180px;max-height:460px;overflow-y:auto;margin:10px 0 14px;}
  .ai-message {border-radius:6px;padding:10px 12px;margin:8px 0;white-space:pre-wrap;overflow-wrap:anywhere;}
  .ai-message.user {background:#e8f2fb;border-left:4px solid #2474ba;}
  .ai-message.assistant {background:white;border-left:4px solid #2f855a;}
  .ai-message.error {background:#fff5f5;border-left:4px solid #c53030;}
  .ai-message-label {font-size:12px;font-weight:700;text-transform:uppercase;color:#52677d;margin-bottom:4px;}
  .ai-context {font-size:12px;background:#f7f9fc;border:1px solid #dce3ed;border-radius:5px;padding:8px 10px;margin:8px 0;}
  .finding-note {margin:10px 0;padding:10px 12px;border-left:4px solid #2474ba;background:#f1f7fc;}
  .finding-empty {border-left-color:#64748b;background:#f7f9fc;color:#52677d;}
  @media(max-width:1100px) {.profile-grid {grid-template-columns:minmax(0,1fr);}}
  @media(min-width:768px) {#setup-column {width:280px;} #workspace-column {width:calc(100% - 280px);} .setup-hidden #workspace-column {width:100%;}}
  @media(max-width:767px) {#setup-column {position:static;max-height:none;} .app-header {align-items:flex-start;} .container-fluid {padding:0 10px 12px;}}
")), tags$script(HTML("
  $(function(){
    $('#toggle_setup').on('click',function(){
      var hidden=$('body').toggleClass('setup-hidden').hasClass('setup-hidden');
      $(this).attr('aria-expanded',String(!hidden)).text(hidden?'Show setup':'Hide setup');
      window.dispatchEvent(new Event('resize'));
    });
    $(document).on('change','.case-section input',function(){
      var box=$(this).closest('.case-section'), n=box.find('input:checked').length;
      box.find('.case-count').text(n?' ('+n+' selected)':' (all cases)');
    });
    $(document).on('shown.bs.tab','a[data-toggle=tab]',function(){
      if($.fn.dataTable) $.fn.dataTable.tables({visible:true,api:true}).columns.adjust();
      window.dispatchEvent(new Event('resize'));
    });
  });
")))

ui <- fluidPage(
  v11_style,
  div(class = "app-header",
    div(
      h2("ADNCA and ADPC Review"),
      div(class = "app-authors", "Igor Rubets and Marieve Dupuis, Certara Inc."),
      tags$small("Version with Multi-LLM | Choose OpenAI GPT, Anthropic Claude, or local Qwen/Ollama")
    ),
    tags$button(id = "toggle_setup", type = "button", class = "btn btn-default",
      `aria-controls` = "setup-column", `aria-expanded` = "true", "Hide setup")),
  sidebarLayout(
    htmltools::tagAppendAttributes(sidebarPanel(
      div(class = "setup-actions",
        actionBttn("run_review", "Run review", style = "fill", color = "primary")),
      setup_section("1. Input dataset", expanded = TRUE,
      textInput("file_path", "Local file path", placeholder = "C:/study/adpc.sas7bdat"),
      shinyFilesButton("browse_path", "Browse local file", "Choose a local dataset", multiple = FALSE),
      actionBttn("load_path", "Load from local path", style = "fill", color = "warning"),
      tags$hr(),

      ),
      setup_section("2. Core variable mapping (Review and PK)",
      uiOutput("var_select_ui"),
      tags$hr(),

      ),
      setup_section("3. Data filter",
      uiOutput("filter_select_ui"),
      tags$hr(),

      ),
      setup_section("4. Review thresholds",
      numericInput("pctdev_threshold", "Large percent deviation threshold (%)", value = 20, min = 0, step = 1),
      numericInput("absdev_threshold", "Large absolute deviation threshold (calculated - nominal time)", value = 0.5, min = 0, step = 0.1),
      checkboxInput("enable_iqr", "Run the optional IQR outlier test", value = FALSE),
      conditionalPanel("input.enable_iqr === true",
        numericInput("iqr_multiplier", "IQR multiplier", value = 1.5, min = 0.5, step = 0.1)),
      numericInput("predose_neg_threshold", "Flag suspicious PREdose calculated time when nominal time = 0 and calculated time <", value = -0.5, step = 0.1),
      numericInput("facet_ncol", "Facet overview: panels per row", value = 3, min = 1, max = 6, step = 1),
      tags$hr(),

      ),
      setup_section("5. Outputs and export",
      checkboxGroupInput(
        "outputs",
        "Enable outputs",
        choices = c(
          "Excel listing with filters" = "excel",
          "Conc vs Time overview" = "conc",
          "Conc vs Time by Group" = "conc_group",
          "Facet overview by Group" = "facet_group",
          "Facet overview plot" = "facet",
          "Focused nominal-time view" = "focus",
          "Subject concentration-time profile" = "profile"
        ),
        selected = c("excel", "conc", "conc_group", "facet_group", "facet", "focus", "profile")
      ),
      uiOutput("download_ui"),

      ),
      setup_section("6. Additional PK variable mapping",
      uiOutput("pk_var_map_ui"),
      tags$hr(),
      ),
      setup_section("7. PK grouping mapping",
      uiOutput("pk_group_map_ui"),
      tags$hr(),
      ),
      setup_section("8. PK plot options",
      radioButtons("pk_xvar_sel", "X-axis (time)",
                   choices  = c("Mapped nominal/bucketed time" = "nrrlt",
                                "Mapped analysis relative time" = "mrrlt"),
                   selected = "mrrlt"),
      radioButtons("pk_yscale_sel", "Y-axis scale",
                   choices  = c("log10 scale" = "log", "Linear" = "linear"),
                   selected = "linear"),
      tags$small("AVAL is displayed as supplied. Zero values are marked with triangles; no BLQ substitution is applied."),
      tags$hr(),
      ),
      width = 3
    ), id = "setup-column"),
    mainPanel(id = "workspace-column",
      div(class = "active-view", textOutput("active_view")),
      tabsetPanel(
        id = "main_tabs", selected = "Review summary",
        tabPanel("Review summary",
          tags$p(class = "help-note", "Dataset, active filter, record counts, and all review flag totals. Updated when you run the review."),
          tags$script(HTML("$(function(){Shiny.addCustomMessageHandler('adpc-review-kind',function(parameter){$('#main_tabs > li').each(function(i){$(this).toggle(!parameter || i===0);});});});")),
          uiOutput("review_mode_ui"),
          checkboxInput("review_save_local", "Save directly beside the loaded local file", TRUE),
          uiOutput("review_html_download_ui"), textOutput("review_saved_location"),
          uiOutput("status_msg"), uiOutput("review_details_ui")),
        tabPanel("AI Assistant",
          tags$p(class = "help-note",
            "Choose an AI provider to review aggregate results, plan deterministic data queries, or propose locally executable visualization code."),
          tags$div(class = "ai-privacy-warning",
            tags$strong("Strong privacy warning: "),
            "Blinding is strongly recommended before using OpenAI GPT or Anthropic Claude. These are external, hosted LLM services, so selected data and context leave the local computer when submitted. Local Qwen runs through Ollama on this computer and does not send the submitted data to an external LLM service. Suggested blinding tool: ",
            tags$a(
              href = "https://github.com/irubets/universal-blinder-sas",
              target = "_blank", rel = "noopener noreferrer",
              "Universal Blinder for SAS"
            ),
            "."
          ),
          div(class = "ai-toolbar",
            selectInput("ai_provider", "AI provider",
              choices = c("Local Qwen via Ollama" = "qwen", "OpenAI GPT" = "gpt",
                          "Anthropic Claude" = "anthropic"),
              selected = "qwen", width = "210px"),
            uiOutput("ai_model_ui", inline = TRUE),
            selectInput("ai_reasoning", "Reasoning",
              choices = c("Low" = "low", "Medium" = "medium", "High" = "high"),
              selected = "low", width = "140px"),
            actionButton("ai_check", "Check connection", class = "btn-sm btn-default"),
            actionButton("ai_clear", "Clear conversation", class = "btn-sm btn-default"),
            span(class = "ai-status", textOutput("ai_status", inline = TRUE))
          ),
          div(class = "ai-context", textOutput("ai_context_summary")),
          radioButtons(
            "ai_data_scope", "Data available to Review chat",
            choices = c(
              "Aggregate review summary" = "summary",
              "Whole loaded dataset (complete-query access)" = "full",
              "Named saved subset (complete-query access)" = "subset"
            ),
            selected = "summary", inline = TRUE
          ),
          uiOutput("ai_data_access_ui"),
          tags$fieldset(
            tags$legend("Named subsets"),
            tags$p(class = "help-note",
              "Set the Review listing controls, enter a name, and save a snapshot. The snapshot is kept for this app session and does not change when the listing controls change."),
            fluidRow(
              column(5, textInput("ai_subset_name", "Subset name", placeholder = "e.g., flagged predose records")),
              column(7, br(), actionButton("ai_save_subset", "Save current Review selection", class = "btn-primary"),
                     actionButton("ai_delete_subset", "Delete selected subset", class = "btn-default"))
            ),
            uiOutput("ai_saved_subset_ui"),
            textOutput("ai_subset_status")
          ),
          checkboxInput(
            "ai_include_case_context",
            "Include current Case explorer matches (requires explicit blinded-data confirmation; maximum 10 cases / 100 rows)",
            value = FALSE
          ),
          tabsetPanel(
            tabPanel("Review chat",
              div(class = "ai-chat", uiOutput("ai_chat")),
              textAreaInput(
                "ai_question", "Question for the selected AI", rows = 4, width = "100%",
                placeholder = "Example: Review the analysis-population flag combinations and identify patterns that need investigation."
              ),
              actionBttn("ai_ask", "Ask selected AI", style = "fill", color = "primary"),
              tags$small(class = "help-note",
                "Chat receives aggregate context and, when enabled above, bounded subject-level Case explorer records. It cannot change data or establish clinical or regulatory conclusions.")),
            tabPanel("Data query",
              tags$p(class = "help-note",
                "Describe a case-selection or tabulation request. AI creates a structured plan; validated R evaluates the complete selected dataset without a source-row limit."),
              fluidRow(
                column(3, uiOutput("query_data_source_ui")),
                column(9, textAreaInput("query_request", "Analysis request", rows = 4, width = "100%",
                  placeholder = "Example: Select SUBJID x APROFILE cases where MRRLT is duplicated; list each duplicated MRRLT and its record count."))
              ),
              uiOutput("query_privacy_ui"),
              actionBttn("query_run", "Plan and run complete query", style = "fill", color = "primary"),
              tags$hr(),
              verbatimTextOutput("query_status"),
              h4("AI interpretation"),
              verbatimTextOutput("query_answer"),
              tags$details(tags$summary("Generated validated R script"), verbatimTextOutput("query_script")),
              div(class = "ai-toolbar", downloadButton("query_download_csv", "Download complete CSV"),
                  downloadButton("query_download_xlsx", "Download complete Excel")),
              DTOutput("query_table")
            ),
            tabPanel("Visualization studio",
              fluidRow(
                column(4, selectInput("viz_data_source", "Local data for execution",
                  choices = c("Committed analysis data (after Data filter)" = "raw", "Derived timing review" = "review"))),
                column(8, textAreaInput("viz_request", "Visualization request", rows = 3, width = "100%",
                  placeholder = "Example: Show the percentage of Y values for every CRITxxFL and NCAxxFL, ordered by percentage."))
              ),
              actionBttn("viz_generate", "Generate proposal", style = "fill", color = "primary"),
              tags$hr(),
              h4(textOutput("viz_title")),
              textOutput("viz_rationale"),
              textAreaInput("viz_code", "Review and edit proposed R code", value = "", rows = 16, width = "100%"),
              verbatimTextOutput("viz_validation"),
              checkboxInput("viz_approve", "I reviewed this code and approve local execution and saving", FALSE),
              actionButton("viz_validate", "Validate code"),
              actionButton("viz_run", "Approve and run locally", class = "btn-success"),
              actionButton("viz_save", "Save approved R script"),
              textOutput("viz_saved_path"),
              plotlyOutput("viz_generated_plot", height = "600px"),
              tags$small(class = "help-note",
                "Generated code never runs automatically. Static checks block file, network, shell, package-installation, and environment-variable operations. Execution uses a local isolated environment with plot_data."))
          )),
        navbarMenu("Data",
        tabPanel("Case explorer", case_explorer_ui_v13("gpt")),
        tabPanel("Grouping values audit",
          tabsetPanel(
            id = "grouping_audit_tabs", selected = "Grouping values",
            tabPanel("Grouping values",
              tags$p(class = "help-note",
                "Actual unique values and record counts from the loaded dataset. Review CHECK rows before relying on grouping, filtering, or profile definitions."),
              verbatimTextOutput("grouping_audit_summary"),
              downloadButton("grouping_audit_download", "Download audit CSV"),
              DTOutput("grouping_audit_table")),
            tabPanel("Profile time points",
              tags$p(class = "help-note",
                "One row per automatically identified concentration-time profile, with the number and planned nominal-time values (normally NRRLT) observed in that profile. Run the review after changing mappings or filters."),
              verbatimTextOutput("profile_timepoints_summary"),
              div(class = "ai-toolbar",
                downloadButton("profile_timepoints_download", "Download profile time points CSV")),
              DTOutput("profile_timepoints_table"),
              tags$details(
                tags$summary("Show candidate profile-key assessment"),
                DTOutput("profile_candidates_table"))
            ),
            tabPanel("Subject ID mapping",
              tags$p(class = "help-note",
                "Distinct USUBJID-to-SUBJID mappings in the committed analysis scope. Red cells identify an ID mapped to multiple values in the other column; yellow cells identify missing IDs."),
              verbatimTextOutput("subject_id_mapping_summary"),
              downloadButton("subject_id_mapping_download", "Download subject ID mapping CSV"),
              DTOutput("subject_id_mapping_table")
            )
          )),
        tabPanel("Review finding subsets",
          tags$p(class = "help-note",
            "Each page includes complete concentration-time profiles containing the finding. Triggering records are highlighted as RECORD_OF_INTEREST = YES."),
          do.call(tabsetPanel, c(
            list(id = "review_finding_tabs", selected = names(review_finding_specs)[1]),
            lapply(names(review_finding_specs), function(id) {
              spec <- review_finding_specs[[id]]
              tabPanel(
                title = spec$title, value = id,
                uiOutput(paste0("finding_summary_", id)),
                downloadButton(paste0("finding_download_", id), "Download subset CSV"),
                DTOutput(paste0("finding_table_", id))
              )
            })
          ))
        ),
        tabPanel(
          "Data preview",
          br(),
          div(class = "help-note", textOutput("dataset_summary")),
          tabsetPanel(id = "preview_tabs",
            tabPanel("Raw input", DTOutput("preview")),
            tabPanel("Derived review", DTOutput("preview_derived")))
        ),
        tabPanel(
          "Analysis population flags",
          br(),
          tags$p(class = "help-note",
            "Explore CRITxx/CRITxxFL criteria, NCAxxFL/NCAxxXFL, PKSUMXFL, and NCA01FL/ANL01FL or NCA02FL/ANL02FL without assuming that different populations are errors."),
          fluidRow(
            column(4, uiOutput("flag_select_ui")),
            column(4, uiOutput("flag_pair_x_ui")),
            column(4, uiOutput("flag_pair_y_ui"))
          ),
          tabsetPanel(
            tabPanel("Definitions and counts", DTOutput("flag_dictionary"), plotlyOutput("flag_rate_plot", height = "520px")),
            tabPanel("Combinations",
              tags$p(class = "help-note", "Select flags above to define a combined population signature."),
              plotlyOutput("flag_combination_plot", height = "520px"),
              DTOutput("flag_combinations")),
            tabPanel("Pairwise comparison", DTOutput("flag_pair_table"), plotlyOutput("flag_pair_plot", height = "500px")),
            tabPanel("Potential issues", DTOutput("flag_issues")),
            tabPanel("Record drill-down",
              uiOutput("flag_value_filters_ui"),
              DTOutput("flag_records"))
          )
        ),
        ),
        navbarMenu("Timing Review",
        tabPanel(
          "Review listing",
          br(),
          fluidRow(
            column(4, checkboxInput("show_review_only", "Show only flagged review records", value = TRUE)),
            column(4, checkboxInput("show_selected_focus_only", "Restrict table to selected focus combination", value = FALSE))
          ),
          fluidRow(
            column(4, numericInput("pctdev_min_filter", "ARRLT_PCTDEV minimum (%)", value = 20, min = 0, step = 1)),
            column(4, numericInput("pctdev_max_filter", "ARRLT_PCTDEV maximum (%)", value = NA, min = 0, step = 1)),
            column(4, checkboxInput("pctdev_filter_review_only", "Apply ARRLT_PCTDEV filter only to flagged rows", value = FALSE))
          ),
          case_selector(
            "review_case_filter",
            "Specific cases to show",
            choices = c(
              "IQR outlier",
              "Large % deviation",
              "Large absolute deviation",
              "PREdose taken POST-dose",
              "POST-dose taken PREdose",
              "PREdose suspiciously early",
              "MRRLT differs from ARRLT",
              "Profiles with duplicates at NRRLT",
              "Profiles with duplicate MRRLT"
            )
          ),
          tags$div(
            style = "margin-bottom:8px;color:#555;",
            "Leave ARRLT_PCTDEV maximum empty to keep no upper limit. The dedicated range filter is useful when table scroll filters are not practical because of extreme values."
          ),
          DTOutput("review_table")
        ),
        tabPanel(
          "Conc vs Time",
          br(),
          fluidRow(
            column(4, checkboxInput("conc_review_only", "Show only flagged points", FALSE)),
            column(4, radioButtons("conc_yaxis", "Y-axis", choices = c("AVAL" = "aval", "ln(AVAL)" = "log_aval"), inline = TRUE))
          ),
          case_selector(
            "conc_case_filter",
            "Specific cases to show",
            choices = c(
              "IQR outlier",
              "Large % deviation",
              "Large absolute deviation",
              "PREdose taken POST-dose",
              "POST-dose taken PREdose",
              "PREdose suspiciously early",
              "MRRLT differs from ARRLT",
              "Profiles with duplicates at NRRLT",
              "Profiles with duplicate MRRLT"
            )
          ),
          tags$p(class = "help-note", "Select a point to open the corresponding group view. All combinations remain available across pages."),
          pager_controls("conc"),
          uiOutput("conc_plot_container"),
          tags$div(style = "margin-top:8px;color:#555;", "Tip: click a point in a facet to open Conc vs Time by Group for that PCTEST and AVISIT / VISITCD.")
        ),
        tabPanel(
          "Conc vs Time by Group",
          br(),
          fluidRow(
            column(4, selectInput("conc_group_pctest", "PCTEST", choices = NULL)),
            column(4, selectInput("conc_group_visit", "AVISIT / VISITCD", choices = NULL)),
            column(4, checkboxInput("conc_group_review_only", "Show only flagged points", FALSE))
          ),
          fluidRow(
            column(4, radioButtons("conc_group_yaxis", "Y-axis", choices = c("AVAL" = "aval", "ln(AVAL)" = "log_aval"), inline = TRUE))
          ),
          case_selector(
            "conc_group_case_filter",
            "Specific cases to show",
            choices = c(
              "IQR outlier",
              "Large % deviation",
              "Large absolute deviation",
              "PREdose taken POST-dose",
              "POST-dose taken PREdose",
              "PREdose suspiciously early",
              "MRRLT differs from ARRLT",
              "Profiles with duplicates at NRRLT",
              "Profiles with duplicate MRRLT"
            )
          ),
          plotlyOutput("conc_group_plot", height = "560px"),
          tags$div(style = "margin-top:8px;color:#555;", "Tip: click a point to jump to the Subject profile tab for that subject and combination."),
          htmlOutput("conc_group_summary"),
          DTOutput("conc_group_review_table")
        ),
        tabPanel(
          "Facet overview",
          br(),
          fluidRow(
            column(4, radioButtons("facet_metric", "Y-axis", choices = c("ARRLT" = "arrlt", "MRRLT" = "mrrlt", "ARRLT - NRRLT" = "arrlt_dev", "MRRLT - NRRLT" = "mrrlt_dev"), inline = TRUE)),
            column(4, checkboxInput("facet_points", "Overlay points", TRUE)),
            column(4, checkboxInput("facet_review_only", "Show only flagged points in overlay", FALSE))
          ),
          case_selector(
            "facet_case_filter",
            "Specific cases to show",
            choices = c(
              "IQR outlier",
              "Large % deviation",
              "Large absolute deviation",
              "PREdose taken POST-dose",
              "POST-dose taken PREdose",
              "PREdose suspiciously early",
              "MRRLT differs from ARRLT",
              "Profiles with duplicates at NRRLT",
              "Profiles with duplicate MRRLT"
            )
          ),
          tags$p(class = "help-note", "Select a point to open the corresponding group view. All combinations remain available across pages."),
          pager_controls("facet"),
          uiOutput("facet_plot_container"),
          tags$div(style = "margin-top:8px;color:#555;", "Tip: click a point in a facet to open Facet overview by Group for that PCTEST and AVISIT / VISITCD.")
        ),
        tabPanel(
          "Facet overview by Group",
          br(),
          fluidRow(
            column(4, selectInput("facet_group_pctest", "PCTEST", choices = NULL)),
            column(4, selectInput("facet_group_visit", "AVISIT / VISITCD", choices = NULL)),
            column(4, checkboxInput("facet_group_review_only", "Show only flagged points", FALSE))
          ),
          fluidRow(
            column(4, radioButtons("facet_group_metric", "Y-axis", choices = c("ARRLT" = "arrlt", "MRRLT" = "mrrlt", "ARRLT - NRRLT" = "arrlt_dev", "MRRLT - NRRLT" = "mrrlt_dev"), inline = TRUE)),
            column(4, checkboxInput("facet_group_points", "Overlay points", TRUE))
          ),
          case_selector(
            "facet_group_case_filter",
            "Specific cases to show",
            choices = c(
              "IQR outlier",
              "Large % deviation",
              "Large absolute deviation",
              "PREdose taken POST-dose",
              "POST-dose taken PREdose",
              "PREdose suspiciously early",
              "MRRLT differs from ARRLT",
              "Profiles with duplicates at NRRLT",
              "Profiles with duplicate MRRLT"
            )
          ),
          plotlyOutput("facet_group_plot", height = "560px"),
          tags$div(style = "margin-top:8px;color:#555;", "Tip: click a point to jump to the Subject profile tab for that subject and combination."),
          htmlOutput("facet_group_summary"),
          DTOutput("facet_group_review_table")
        ),
        tabPanel(
          "Focused view",
          br(),
          fluidRow(
            column(4, selectInput("focus_pctest", "PCTEST", choices = NULL)),
            column(4, selectInput("focus_visit", "AVISIT / VISITCD", choices = NULL)),
            column(4, selectInput("focus_nrrlt", "NRRLT", choices = NULL))
          ),
          fluidRow(
            column(4, checkboxInput("focus_review_only", "Only flagged records", FALSE)),
            column(4, radioButtons("focus_metric", "Y-axis", choices = c("ARRLT" = "arrlt", "MRRLT" = "mrrlt", "ARRLT - NRRLT" = "arrlt_dev", "MRRLT - NRRLT" = "mrrlt_dev"), inline = TRUE))
          ),
          plotlyOutput("focus_plot", height = "560px"),
          tags$div(style = "margin-top:8px;color:#555;", "Tip: click a point to jump to the Subject profile tab for that subject and combination."),
          DTOutput("focus_subjects")
        ),
        tabPanel(
          "Subject profile",
          br(),
          fluidRow(
            column(4, selectInput("profile_subject", "Subject", choices = c("All" = "__ALL__"))),
            column(4, selectInput("profile_pctest", "PCTEST", choices = c("All" = "__ALL__"))),
            column(4, selectInput("profile_visit", "AVISIT / VISITCD", choices = c("All" = "__ALL__")))
          ),
          fluidRow(
            column(4, radioButtons("profile_xaxis", "X-axis", choices = c("NRRLT" = "nrrlt", "ARRLT" = "arrlt", "MRRLT" = "mrrlt"), inline = TRUE)),
            column(4, radioButtons("profile_yaxis", "Y-axis", choices = c("AVAL" = "aval", "ln(AVAL)" = "log_aval"), inline = TRUE)),
            column(4, checkboxInput("profile_show_nominal_ref", "Show NRRLT reference line markers", TRUE))
          ),
          fluidRow(
            column(6, checkboxInput("profile_show_arrlt_labels", "Show ARRLT labels for flagged points", TRUE)),
            column(6, actionButton("profile_back_btn", "Back to previous tab"))
          ),
          plotlyOutput("profile_plot", height = "560px"),
          DTOutput("profile_table")
        ),

        ),
        navbarMenu("NCA",
        tabPanel(
          "NCA Feasibility by APROFILE",
          br(),
          tags$p("Calculate from the supplied ADPC values. Each APROFILE is separated by subject, analyte and optional additional keys. No ADNCA comparison or BLQ substitution is performed."),
          setup_section("Profile mapping", expanded = TRUE, uiOutput("nca_mapping_ui")),
          h4("Select records for NCA — one filter at a time"),
          tags$p(class = "help-note", "This filter starts from the loaded dataset and is independent of the Review and PK Profile filters."),
          uiOutput("nca_filter_variable_ui"),
          uiOutput("nca_filter_rule_ui"),
          textOutput("nca_filter_count"),
          fluidRow(
            column(4, numericInput("nca_start", "Interval start", 0)),
            column(4, textInput("nca_end", "Interval end (number or Inf)", "Inf")),
            column(4, selectInput("nca_auc", "AUC method", c("lin up/log down", "linear", "lin-log")))
          ),
          tags$p("Missing AVAL remains missing in the input and is omitted by PKNCA during calculation. All zero AVAL values are kept. Times outside the interval are listed. These settings assess feasibility; they do not establish study-specific NCA validity."),
          actionButton("nca_run", "Run PKNCA assessment"),
          downloadButton("nca_export", "Export NCA assessment"),
          verbatimTextOutput("nca_status"),
          tabsetPanel(id = "nca_results_tabs",
            tabPanel("Summary and profile",
              tags$p(class = "help-note", "Run an assessment, then select a summary row to inspect its original records."),
              DTOutput("nca_summary"),
              conditionalPanel("input.nca_summary_rows_selected && input.nca_summary_rows_selected.length > 0",
                h4("Selected APROFILE"), plotlyOutput("nca_profile_plot"), DTOutput("nca_profile_records"))),
            tabPanel("Cases and messages", DTOutput("nca_cases")),
            tabPanel("Parameters", DTOutput("nca_parameters")),
            tabPanel("Terminal diagnostics", DTOutput("nca_diagnostics")))
        ),
        ),
        navbarMenu("PK Profiles",
        tabPanel(
          "Individual Subject Profiles",
          br(),
          pk_profiles_toolbar("pk_prepare_ind", "pk_status_ind"),
          pk_filter_controls("pk_ind"),
          fluidRow(
            column(4,
              selectInput("ind_analyte", "Analyte", choices = NULL, width = "100%")
            ),
            column(4,
              selectInput("ind_colour_by", "Colour lines by",
                          choices = c(
                            "Route of Administration" = "route",
                            "Treatment (ATRT)"        = "atrt",
                            "ARM (ARMCD)"             = "arm",
                            "Cohort"                  = "cohort",
                            "Age Group"               = "agegrp",
                            "BSA Category"            = "bsacat"
                          ),
                          selected = "route")
            ),
            column(4,
              selectInput("ind_subjects", "Subjects",
                          choices = NULL, multiple = TRUE,
                          selectize = TRUE, width = "100%"),
              fluidRow(
                column(6, actionButton("ind_all",   "All",   class = "btn-sm btn-default")),
                column(6, actionButton("ind_clear", "Clear", class = "btn-sm btn-default"))
              )
            )
          ),
          uiOutput("confound_banner"),
          div(style = "margin-bottom:8px;", textOutput("ind_count", inline = TRUE)),
          fluidRow(
            column(4,
              checkboxInput("ind_use_time_window", "Focus on a time range", value = FALSE)
            ),
            column(8,
              conditionalPanel(
                condition = "input.ind_use_time_window",
                uiOutput("ind_time_window_ui")
              )
            )
          ),
          selectInput("ind_page_size", "Profiles per page", c("1 - enlarged" = 1, "4 - comparison" = 4, "6 - comparison" = 6), selected = 4),
          pager_controls("ind", "subjects"),
          uiOutput("ind_plots_ui")
        ),
        tabPanel(
          "Profiles by Groups",
          br(),
          pk_profiles_toolbar("pk_prepare_grp", "pk_status_grp"),
          pk_filter_controls("pk_grp"),
          fluidRow(
            column(3,
              selectInput("grp_analyte", "Analyte", choices = NULL, width = "100%")
            ),
            column(3,
              selectInput("grp_mode", "Grouping mode",
                          choices = c(
                            "(1) All subjects - given visit"     = "mode1",
                            "(2) By Treatment - given visit"     = "mode2",
                            "(3) By Treatment - by SEX"          = "mode3",
                            "(4) By Treatment - by BSA Category" = "mode4",
                            "(5) By Treatment - by Age Group"    = "mode5"
                          ),
                          selected = "mode1")
            ),
            column(3,
              selectInput("grp_visit", "Visit (AVISIT / VISITCD)",
                          choices = NULL, width = "100%")
            ),
            column(3,
              conditionalPanel(
                condition = "input.grp_mode !== 'mode1'",
                selectInput("grp_treatment", "Treatment (ATRT)", choices = NULL, width = "100%")
              )
            )
          ),
          uiOutput("grp_confound_banner"),
          uiOutput("grp_plot_ui")
        ),
        tabPanel(
          "SAS-style Spaghetti Plots",
          br(),
          pk_profiles_toolbar("pk_prepare_sas", "pk_status_sas"),
          pk_filter_controls("pk_sas"),
          fluidRow(
            column(4,
              radioButtons("sas_domain", "Dataset domain",
                           choices = c("ADPC (SUBJID / VISITCD / PCTEST)" = "adpc",
                                       "PC   (USUBJID / VISIT / PCTEST)"  = "pc"),
                           selected = "adpc")
            ),
            column(4,
              radioButtons("sas_mode", "Mode",
                           choices = c("1 - Subject + Visit + Analyte"      = "1",
                                       "2 - Subject + Visit, all Analytes"  = "2",
                                       "3 - Subject, all Visits + Analytes" = "3",
                                       "4 - Full batch"                     = "4",
                                       "5 - All Subjects, page by Visit + Analyte" = "5",
                                       "6 - Analyte + all Subjects, page by Visit" = "6"),
                           selected = "1")
            ),
            column(4,
              radioButtons("sas_yscale", "Y-axis scale",
                           choices  = c("Linear" = "linear", "log10" = "log"),
                           selected = "linear"),
              selectInput("sas_xvar", "X-axis variable",
                          choices = c("Mapped nominal/bucketed time" = "nrrlt",
                                      "Mapped analysis relative time" = "mrrlt",
                                      "PC timepoint (PCTPTNUM)" = "pctptnum"),
                          selected = "nrrlt")
            )
          ),
          fluidRow(
            column(4,
              conditionalPanel(condition = "['1', '2', '3'].indexOf(input.sas_mode) >= 0",
                               uiOutput("sas_subject_ui"))
            ),
            column(4,
              conditionalPanel(condition = "input.sas_mode === '1' || input.sas_mode === '2'",
                               uiOutput("sas_visit_ui"))
            ),
            column(4,
              conditionalPanel(condition = "input.sas_mode === '1' || input.sas_mode === '6'",
                               uiOutput("sas_analyte_ui"))
            )
          ),
          conditionalPanel(
            condition = "input.sas_mode === '5' || input.sas_mode === '6'",
            radioButtons(
              "sas_subject_summary", "Subject display",
              choices = c(
                "Spaghetti (individual subjects)" = "spaghetti",
                "Mean with SD dispersion band" = "mean_sd_band",
                "Mean with SD error bars" = "mean_sd_errorbar"
              ),
              selected = "spaghetti", inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "(input.sas_mode === '5' || input.sas_mode === '6') && (input.sas_subject_summary === 'mean_sd_band' || input.sas_subject_summary === 'mean_sd_errorbar')",
            fluidRow(
              column(4,
                selectInput(
                  "sas_summary_group", "Summary table grouping",
                  choices = c(
                    "Overall (all subjects)" = "none",
                    "ARM (ARMCD)" = "arm",
                    "Treatment (ATRT)" = "atrt",
                    "Cohort" = "cohort",
                    "Route of administration" = "route"
                  ),
                  selected = "none"
                )
              ),
              column(8,
                helpText("Tables are paged by the selected group and visit. For error-bar figures, this grouping also defines the plotted series.")
              )
            )
          ),
          conditionalPanel(
            condition = "(input.sas_mode === '5' || input.sas_mode === '6') && input.sas_subject_summary === 'mean_sd_errorbar'",
            fluidRow(
              column(6,
                radioButtons(
                  "sas_sd_sides", "SD error bars",
                  choices = c(
                    "Two-sided (mean +/- SD)" = "two",
                    "Upper only (mean + SD)" = "upper"
                  ),
                  selected = "two"
                )
              ),
              column(6,
                selectInput(
                  "sas_jitter_pct", "Horizontal jitter (% of nominal time)",
                  choices = c("None (0%)" = 0, "1%" = 1, "2.5%" = 2.5,
                              "5%" = 5, "10%" = 10),
                  selected = 2.5
                ),
                helpText("Jitter separates multiple summary groups symmetrically around the nominal time; it has no effect for Overall.")
              )
            )
          ),
          fluidRow(
            column(4,
              conditionalPanel(
                condition = "input.sas_mode === '5' || input.sas_mode === '6'",
                checkboxInput("sas_combine_visits", "Combine visits", value = FALSE)
              )
            ),
            column(8,
              conditionalPanel(
                condition = "(input.sas_mode === '5' || input.sas_mode === '6') && input.sas_combine_visits",
                uiOutput("sas_combined_visits_ui"),
                uiOutput("sas_combined_axis_diagnostic")
              )
            )
          ),
          fluidRow(
            column(4,
              checkboxInput("sas_use_time_window", "Focus on a time range", value = FALSE)
            ),
            column(8,
              conditionalPanel(
                condition = "input.sas_use_time_window",
                uiOutput("sas_time_window_ui")
              )
            )
          ),
          actionBttn("sas_run", "Generate Plots", style = "fill", color = "primary"),
          br(),
          br(),
          uiOutput("sas_plots_ui")
        )
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  output$active_view <- renderText(input$main_tabs %||% "Review summary")
  output$dataset_summary <- renderText({
    if (is.null(rv$raw)) return("Load a dataset using Setup to begin.")
    if (is.null(rv$review)) return(sprintf("%s | %s records loaded | Review not yet run", basename(rv$source_path %||% "Dataset"), format(nrow(rv$raw), big.mark = ",")))
    sprintf("%s | %s records loaded | %s reviewed | %s flagged", basename(rv$source_path %||% "Dataset"),
      format(nrow(rv$raw), big.mark = ","), format(nrow(rv$review$data), big.mark = ","),
      format(sum(rv$review$data$review_fl == "Y", na.rm = TRUE), big.mark = ","))
  })
  register_pager <- function(prefix, values, size) {
    state <- reactive(page_slice(values(), input[[paste0(prefix, "_page")]], size()))
    observeEvent(list(values(), size()), {
      updateNumericInput(session, paste0(prefix, "_page"), value = 1,
        max = max(1, ceiling(length(values()) / size())))
    }, ignoreInit = FALSE)
    observeEvent(input[[paste0(prefix, "_prev")]], {
      updateNumericInput(session, paste0(prefix, "_page"), value = max(1, state()$page - 1))
    })
    observeEvent(input[[paste0(prefix, "_next")]], {
      updateNumericInput(session, paste0(prefix, "_page"), value = min(state()$pages, state()$page + 1))
    })
    observeEvent(input[[paste0(prefix, "_page")]], {
      if (!identical(as.numeric(input[[paste0(prefix, "_page")]]), as.numeric(state()$page)))
        updateNumericInput(session, paste0(prefix, "_page"), value = state()$page)
    })
    output[[paste0(prefix, "_page_status")]] <- renderText({
      p <- state()
      sprintf("Page %d of %d | %d shown of %d", p$page, p$pages, length(p$values), p$total)
    })
    state
  }
  conc_page <- register_pager("conc", reactive(sort(unique(conc_data()$facet_label))), reactive((input$facet_ncol %||% 3) * 2))
  facet_page <- register_pager("facet", reactive(sort(unique(facet_data()$facet_label))), reactive((input$facet_ncol %||% 3) * 2))
  ind_page <- register_pager("ind", reactive(input$ind_subjects %||% character()), reactive(as.integer(input$ind_page_size %||% 4)))
  output$conc_plot_container <- renderUI({
    rows <- max(1, ceiling(length(conc_page()$values) / (input$facet_ncol %||% 3)))
    plotlyOutput("conc_plot", height = paste0(180 + rows * 300, "px"))
  })
  output$facet_plot_container <- renderUI({
    rows <- max(1, ceiling(length(facet_page()$values) / (input$facet_ncol %||% 3)))
    plotlyOutput("facet_plot", height = paste0(180 + rows * 300, "px"))
  })

  rv <- reactiveValues(
    raw = NULL, review = NULL, source_path = NULL,
    review_snapshot = NULL, review_saved_path = NULL, source_sha256 = NULL,
    ai_subsets = list(),
    last_tab_before_profile = "Conc vs Time by Group",
    last_conc_group_click = NULL,
    last_conc_click = NULL,
    last_facet_group_click = NULL,
    last_facet_click = NULL,
    last_focus_click = NULL,
    active_filter_var = NULL,
    active_filter_val = NULL,
    active_filter_n   = NULL,
    analysis_data     = NULL,
    review_mappings   = NULL,
    pk_data        = NULL,
    pk_data_stale  = FALSE,
    pk_filter_description = "No filter",
    pk_filter_variable = "",
    pk_filter_mode = "equals",
    pk_filter_value = "Y",
    pk_filter_selected_n = NULL,
    pk_filter_loaded_n = NULL,
    sas_plot_items = list(),
    nca = NULL
  )

  analysis_data <- reactive({
    req(rv$raw)
    rv$analysis_data %||% rv$raw
  })

  grouping_audit_data <- reactive({
    d <- analysis_data()
    grouping_values_audit(d, mapped = list(
      analyte = input$analyte_var, avisit = input$visit_var,
      visitcd = input$visitcd_var, nrrlt = input$nominal_var
    ))
  })
  output$grouping_audit_summary <- renderText({
    d <- grouping_audit_data()
    absent <- unique(d$Factor[d$Status == "VARIABLE NOT FOUND"])
    check_rows <- sum(grepl("^CHECK", d$Status))
    paste0(
      "Audit scope: ", if (is.null(rv$active_filter_var)) "NO FILTER" else paste0(rv$active_filter_var, " == ", rv$active_filter_val),
      " | ", format(nrow(analysis_data()), big.mark = ","), " of ", format(nrow(rv$raw), big.mark = ","), " loaded records.\n",
      "Factors requested: 11 | Detail rows requiring review: ", check_rows, "\n",
      "Variables not found: ", if (length(absent)) paste(absent, collapse = ", ") else "None", "\n",
      "Analyte, visit and nominal time follow the current core variable mappings; the audit updates when those mappings change."
    )
  })
  output$grouping_audit_table <- renderDT({
    d <- grouping_audit_data()
    review_datatable(d, rownames = FALSE, filter = "top",
      options = list(scrollX = TRUE, pageLength = 25, deferRender = TRUE)) %>%
      DT::formatStyle("Status", target = "row", backgroundColor = DT::styleEqual(
        c("CHECK MISSING/BLANK", "CHECK MULTIPLE CANDIDATE VARIABLES", "VARIABLE NOT FOUND"),
        c("#FFF3CD", "#FFF3CD", "#F8D7DA")))
  }, server = TRUE)
  output$grouping_audit_download <- downloadHandler(
    filename = function() paste0("grouping_values_audit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) utils::write.csv(grouping_audit_data(), file, row.names = FALSE, na = "")
  )

  profile_assessment <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    mapped <- rv$review_mappings %||% list(
      subject = input$id_var, analyte = input$analyte_var, visit = input$visit_var,
      nominal = input$nominal_var, actual = input$actual_var,
      mrrlt = input$mrrlt_var, profile = "APROFILE", sample_time = input$adtm_var
    )
    assessment <- identify_pk_profiles(rv$review$data, mapped)
    assessment$id_check <- subject_id_relationship(analysis_data())
    assessment
  })
  output$profile_timepoints_summary <- renderText({
    assessment <- profile_assessment()
    if (!assessment$available) return(assessment$message)
    paste0(
      assessment$message, "\n",
      "Mapped analysis nominal time: ", assessment$nominal_col,
      " | Listed planned time-point values: ", assessment$cycle_nominal_col,
      " | Profiles: ", format(assessment$n_profiles, big.mark = ","),
      " | Nominal-time dose cycles checked: ", format(assessment$cycle_count, big.mark = ","),
      if (assessment$unassigned_records > 0L)
        paste0(" | Records before a nominal-time zero: ", format(assessment$unassigned_records, big.mark = ",")) else ""
    )
  })
  output$profile_timepoints_table <- renderDT({
    assessment <- profile_assessment()
    validate(need(nrow(assessment$timepoints), "No concentration-time profiles are available."))
    review_datatable(assessment$timepoints, rownames = FALSE, filter = "top",
      options = list(scrollX = TRUE, pageLength = 25, deferRender = TRUE))
  }, server = TRUE)
  output$profile_candidates_table <- renderDT({
    assessment <- profile_assessment()
    validate(need(nrow(assessment$candidates), "No candidate profile keys could be assessed."))
    candidates <- assessment$candidates
    candidates$Valid <- ifelse(candidates$Valid, "YES", "NO")
    review_datatable(candidates, rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  }, server = FALSE)
  output$profile_timepoints_download <- downloadHandler(
    filename = function() paste0("profile_timepoints_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) utils::write.csv(profile_assessment()$timepoints, file, row.names = FALSE, na = "")
  )

  subject_id_mapping_data <- reactive({
    subject_id_mapping_audit(analysis_data())
  })
  output$subject_id_mapping_summary <- renderText({
    audit <- subject_id_mapping_data()
    if (!audit$available) return("USUBJID and SUBJID are both required for this audit.")
    if (isTRUE(audit$one_to_one)) {
      return("USUBJID:SUBJID is 1:1. No conflicting mappings were found.")
    }
    paste0(
      "WARNING: USUBJID:SUBJID is NOT 1:1\n",
      "USUBJID values mapped to multiple SUBJID values: ", length(audit$conflicting_usubjid), "\n",
      "SUBJID values mapped to multiple USUBJID values: ", length(audit$conflicting_subjid),
      if (audit$missing_usubjid || audit$missing_subjid)
        paste0("\nMissing IDs present: USUBJID = ", audit$missing_usubjid,
               "; SUBJID = ", audit$missing_subjid) else ""
    )
  })
  output$subject_id_mapping_table <- renderDT({
    audit <- subject_id_mapping_data()
    validate(need(audit$available, "USUBJID and SUBJID are both required for this audit."))
    validate(need(!audit$one_to_one, "USUBJID:SUBJID is 1:1; no exception table is required."))
    widget <- review_datatable(audit$table, rownames = FALSE, filter = "top",
      options = list(scrollX = TRUE, pageLength = 25, deferRender = TRUE))
    if (length(audit$affected_usubjid)) {
      widget <- DT::formatStyle(widget, "USUBJID",
        backgroundColor = DT::styleEqual(
          audit$affected_usubjid, rep("#F8D7DA", length(audit$affected_usubjid))
        ),
        fontWeight = DT::styleEqual(
          audit$affected_usubjid, rep("bold", length(audit$affected_usubjid))
        ))
    }
    if (length(audit$affected_subjid)) {
      widget <- DT::formatStyle(widget, "SUBJID",
        backgroundColor = DT::styleEqual(
          audit$affected_subjid, rep("#F8D7DA", length(audit$affected_subjid))
        ),
        fontWeight = DT::styleEqual(
          audit$affected_subjid, rep("bold", length(audit$affected_subjid))
        ))
    }
    if (audit$missing_usubjid) {
      widget <- DT::formatStyle(widget, "USUBJID",
        backgroundColor = DT::styleEqual("(missing)", "#FFF3CD"))
    }
    if (audit$missing_subjid) {
      widget <- DT::formatStyle(widget, "SUBJID",
        backgroundColor = DT::styleEqual("(missing)", "#FFF3CD"))
    }
    widget
  }, server = TRUE)
  output$subject_id_mapping_download <- downloadHandler(
    filename = function() paste0("subject_id_mapping_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) utils::write.csv(subject_id_mapping_data()$table, file,
                                               row.names = FALSE, na = "")
  )

  review_finding_result <- function(finding_id) {
    req(!identical(rv$review$kind, "parameter"), rv$review)
    spec <- review_finding_specs[[finding_id]]
    if (is.null(spec)) stop("Unknown review finding.")
    d <- as.data.frame(rv$review$data)
    assessment <- profile_assessment()
    key_cols <- assessment$selected$key_cols %||% character()
    if (!length(key_cols) || any(!key_cols %in% names(d))) {
      key_cols <- intersect(c(".id", ".pctest", ".avisit", ".visitcd"), names(d))
    }
    interest <- if (spec$interest_col %in% names(d)) d[[spec$interest_col]] == "Y" else rep(FALSE, nrow(d))
    result <- build_review_finding_subset(d, interest, key_cols)
    result$data <- order_review_finding_columns(
      result$data, key_cols = key_cols,
      mappings = rv$review_mappings %||% list()
    )
    result$title <- spec$title
    result$criterion_available <- spec$interest_col %in% names(d)
    result$interest_col <- spec$interest_col
    result
  }

  for (finding_id in names(review_finding_specs)) {
    local({
      id <- finding_id
      summary_id <- paste0("finding_summary_", id)
      table_id <- paste0("finding_table_", id)
      download_id <- paste0("finding_download_", id)

      output[[summary_id]] <- renderUI({
        result <- review_finding_result(id)
        if (!result$criterion_available || result$interest_records == 0L) {
          return(div(class = "finding-note finding-empty", "No relevant observations."))
        }
        div(
          class = "finding-note",
          tags$strong(result$title),
          sprintf(
            ": %s record(s) of interest in %s complete concentration-time profile(s); %s total profile record(s) displayed. Profile key: %s.",
            format(result$interest_records, big.mark = ","),
            format(result$profiles, big.mark = ","),
            format(nrow(result$data), big.mark = ","),
            paste(result$key_cols, collapse = " + ")
          )
        )
      })

      output[[table_id]] <- renderDT({
        result <- review_finding_result(id)
        validate(need(nrow(result$data) > 0L, "No relevant observations."))
        widget <- review_datatable(
          result$data, rownames = FALSE, filter = "top",
          options = list(scrollX = TRUE, pageLength = 25, deferRender = TRUE,
                         fixedColumns = list(leftColumns = min(2L, ncol(result$data))))
        )
        DT::formatStyle(
          widget, "RECORD_OF_INTEREST", target = "row",
          backgroundColor = DT::styleEqual("YES", "#FFF3CD"),
          fontWeight = DT::styleEqual("YES", "700")
        )
      }, server = TRUE)

      output[[download_id]] <- downloadHandler(
        filename = function() paste0("review_finding_", id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
        content = function(file) utils::write.csv(review_finding_result(id)$data, file, row.names = FALSE, na = "")
      )
    })
  }

  case_module <- case_explorer_server_v13(
    input, output, session, raw_reactive = analysis_data,
    mapping_reactive = reactive(list(
      subject = input$id_var, analyte = input$analyte_var,
      visit = input$visit_var, profile = "APROFILE",
      aval = input$aval_var, nrrlt = input$nominal_var,
      arrlt = input$actual_var, mrrlt = input$mrrlt_var
    )),
    ai_mode = "gpt"
  )

  ai_connection <- reactiveVal(list(ok = FALSE, message = "Connection not checked."))
  ai_display_messages <- reactiveVal(list())
  ai_api_history <- reactiveVal(list())
  viz_proposal <- reactiveVal(NULL)
  viz_result <- reactiveVal(NULL)
  viz_validation_state <- reactiveVal("No code validated.")
  viz_saved_file <- reactiveVal("")
  query_result <- reactiveVal(NULL)
  query_script_value <- reactiveVal("")
  query_status_value <- reactiveVal("No query has been run.")
  query_answer_value <- reactiveVal("")

  selected_provider <- reactive(ai_provider_normalize(input$ai_provider %||% "qwen"))
  selected_provider_label <- reactive(ai_provider_label(selected_provider()))
  selected_model <- reactive(input$ai_model %||% ai_provider_config(selected_provider())$model)

  ai_subset_names <- reactive(names(rv$ai_subsets %||% list()))

  output$ai_data_access_ui <- renderUI({
    hosted_note <- if (selected_provider() %in% c("gpt", "anthropic")) {
      " The selected provider is hosted externally, so submitted query results leave this computer."
    } else {
      " The selected provider runs locally through Ollama."
    }
    checkboxInput(
      "ai_data_access_ack",
      paste0(
        "I accept responsibility for allowing the AI Assistant to access data returned from the whole dataset or a saved subset.",
        hosted_note
      ),
      value = FALSE
    )
  })

  output$ai_saved_subset_ui <- renderUI({
    choices <- ai_subset_names()
    selected <- isolate(input$ai_saved_subset %||% "")
    if (!selected %in% choices) selected <- if (length(choices)) choices[[1]] else ""
    selectInput(
      "ai_saved_subset", "Saved subset",
      choices = if (length(choices)) choices else c("No saved subsets" = ""),
      selected = selected, width = "100%"
    )
  })

  output$ai_subset_status <- renderText({
    subsets <- rv$ai_subsets %||% list()
    if (!length(subsets)) return("No named subsets saved in this session.")
    paste(vapply(names(subsets), function(nm) {
      x <- subsets[[nm]]
      sprintf("%s: %s rows x %s columns", nm, format(nrow(x$data), big.mark = ","), ncol(x$data))
    }, character(1)), collapse = " | ")
  })

  observeEvent(input$ai_save_subset, {
    req(!identical(rv$review$kind, "parameter"), rv$review)
    nm <- trimws(input$ai_subset_name %||% "")
    if (!nzchar(nm)) {
      showNotification("Enter a name for the subset.", type = "warning", duration = 5)
      return()
    }
    if (nchar(nm) > 80L) {
      showNotification("Subset names must be 80 characters or fewer.", type = "warning", duration = 5)
      return()
    }
    subsets <- rv$ai_subsets %||% list()
    if (nm %in% names(subsets)) {
      showNotification("That subset name already exists. Delete it first or use a different name.", type = "warning", duration = 6)
      return()
    }
    snapshot <- as.data.frame(review_data_filtered())
    subsets[[nm]] <- list(
      data = snapshot,
      created_at = Sys.time(),
      source_path = rv$source_path,
      source_rows = nrow(rv$review$data)
    )
    rv$ai_subsets <- subsets
    updateTextInput(session, "ai_subset_name", value = "")
    showNotification(sprintf("Saved subset '%s' with %s rows.", nm, format(nrow(snapshot), big.mark = ",")),
      type = "message", duration = 5)
  })

  observeEvent(input$ai_delete_subset, {
    nm <- input$ai_saved_subset %||% ""
    subsets <- rv$ai_subsets %||% list()
    if (!nzchar(nm) || !nm %in% names(subsets)) {
      showNotification("Select a saved subset to delete.", type = "warning", duration = 5)
      return()
    }
    subsets[[nm]] <- NULL
    rv$ai_subsets <- subsets
    showNotification(sprintf("Deleted saved subset '%s'.", nm), type = "message", duration = 4)
  })

  ai_scope_data <- reactive({
    scope <- input$ai_data_scope %||% "summary"
    if (identical(scope, "full")) return(as.data.frame(rv$raw))
    if (identical(scope, "subset")) {
      nm <- input$ai_saved_subset %||% ""
      validate(need(nzchar(nm) && nm %in% ai_subset_names(), "Select a named saved subset."))
      return(as.data.frame(rv$ai_subsets[[nm]]$data))
    }
    NULL
  })

  ai_scope_label <- reactive({
    scope <- input$ai_data_scope %||% "summary"
    if (identical(scope, "full")) return("whole loaded dataset")
    if (identical(scope, "subset")) return(sprintf("saved subset '%s'", input$ai_saved_subset %||% ""))
    "aggregate review summary"
  })

  output$ai_model_ui <- renderUI({
    if (selected_provider() == "gpt") {
      selectInput("ai_model", "Model",
        choices = c("GPT-6 Luna — economical" = "gpt-6-luna",
                    "GPT-6 Sol — advanced" = "gpt-6-sol",
                    "GPT-6 Astra — expert" = "gpt-6-astra"),
        selected = Sys.getenv("OPENAI_MODEL", "gpt-6-luna"), width = "230px")
    } else if (selected_provider() == "anthropic") {
      textInput("ai_model", "Anthropic model",
        value = Sys.getenv("ANTHROPIC_MODEL", "claude-sonnet-5"), width = "230px")
    } else {
      textInput("ai_model", "Local Ollama model",
        value = Sys.getenv("LLM_MODEL", "qwen3.5:4b"), width = "230px")
    }
  })

  observeEvent(input$ai_provider, {
    ai_connection(list(ok = FALSE, message = paste(selected_provider_label(), "selected; connection not checked.")))
    ai_display_messages(list())
    ai_api_history(list())
  }, ignoreInit = TRUE)

  observeEvent(input$ai_check, {
    ai_connection(ai_provider_status(selected_provider(), selected_model()))
  }, ignoreInit = FALSE)

  output$ai_status <- renderText(ai_connection()$message)

  selected_case_context <- reactive({
    if (isTRUE(input$ai_include_case_context)) {
      return(case_module$bounded_context())
    }
    context <- case_module$ai_context()
    if (is.null(context) || isTRUE(context$blocked)) NULL else context
  })

  output$ai_context_summary <- renderText({
    if (is.null(rv$raw)) {
      return(sprintf("Context available to %s: no dataset; general guidance only.", selected_provider_label()))
    }
    scope <- input$ai_data_scope %||% "summary"
    if (!identical(scope, "summary")) {
      scoped <- tryCatch(ai_scope_data(), error = function(e) NULL)
      if (is.null(scoped)) {
        return(sprintf("Context requested for %s: select an available dataset or saved subset.", selected_provider_label()))
      }
      return(sprintf(
        paste0(
          "Data scope for %s: %s (%s rows x %s columns). The AI plans a read-only query, R evaluates every row in this scope, ",
          "and the complete result is sent only after responsibility is acknowledged and only when it fits the processing threshold."
        ),
        selected_provider_label(), ai_scope_label(), format(nrow(scoped), big.mark = ","), ncol(scoped)
      ))
    }
    if (is.null(rv$review)) {
      return(sprintf(
        "Context available to %s: dataset schema and dimensions (%s rows, %s columns); review not yet run.",
        selected_provider_label(), format(nrow(rv$raw), big.mark = ","), format(ncol(rv$raw), big.mark = ",")
      ))
    }
    filtered_n <- tryCatch(nrow(review_data_filtered()), error = function(e) 0L)
    case_context <- selected_case_context()
    if (!is.null(case_context)) {
      return(sprintf(
        paste0(
          "Context available to %s: aggregate review summary (%s reviewed records; %s currently visible through Review listing controls), ",
          "plus %s subject-level cases / %s records with subject identifiers.%s"
        ),
        selected_provider_label(), format(nrow(rv$review$data), big.mark = ","), format(filtered_n, big.mark = ","),
        format(case_context$included_case_count %||% 0L, big.mark = ","),
        format(case_context$included_row_count %||% 0L, big.mark = ","),
        if (isTRUE(case_context$truncated)) " Context was truncated to the local safety limit." else ""
      ))
    }
    sprintf(
      paste0(
        "Context available to %s: aggregate review summary (%s reviewed records; %s currently visible through Review listing controls). ",
        "Subject identifiers are not included unless explicitly enabled and confirmed as suitably blinded."
      ),
      selected_provider_label(), format(nrow(rv$review$data), big.mark = ","), format(filtered_n, big.mark = ",")
    )
  })

  output$ai_chat <- renderUI({
    messages <- ai_display_messages()
    if (!length(messages)) {
      return(tags$p(class = "help-note", "No conversation yet. Check the connection, then ask a question."))
    }
    tagList(lapply(messages, function(item) {
      div(
        class = paste("ai-message", item$type),
        div(class = "ai-message-label", item$label),
        div(item$text)
      )
    }))
  })

  observeEvent(input$ai_clear, {
    ai_display_messages(list())
    ai_api_history(list())
    updateTextAreaInput(session, "ai_question", value = "")
  })

  output$query_privacy_ui <- renderUI({
    tags$p(class = "help-note", "The selected AI creates a structured plan; validated R performs complete-data filtering locally. The complete result is sent for interpretation only after the responsibility acknowledgment above and only when it fits the processing threshold.")
  })
  output$query_data_source_ui <- renderUI({
    subset_choices <- stats::setNames(
      paste0("subset::", ai_subset_names()),
      paste0("Saved subset: ", ai_subset_names())
    )
    choices <- c(
      "Committed analysis data (after Data filter)" = "raw",
      "Derived timing review" = "review",
      subset_choices
    )
    selected <- isolate(input$query_data_source %||% "raw")
    if (!selected %in% unname(choices)) selected <- "raw"
    selectInput("query_data_source", "Dataset", choices = choices, selected = selected)
  })
  query_source_data <- reactive({
    req(rv$raw)
    source <- input$query_data_source %||% "raw"
    if (startsWith(source, "subset::")) {
      nm <- sub("^subset::", "", source)
      validate(need(nm %in% ai_subset_names(), "The selected saved subset is no longer available."))
      return(as.data.frame(rv$ai_subsets[[nm]]$data))
    }
    if (identical(source, "review")) {
      req(!identical(rv$review$kind, "parameter"), rv$review)
      rv$review$data
    } else analysis_data()
  })
  output$query_status <- renderText(query_status_value())
  output$query_answer <- renderText(query_answer_value())
  output$query_script <- renderText(query_script_value())
  output$query_table <- renderDT({
    x <- query_result(); req(x)
    review_datatable(x, rownames = FALSE, filter = "top",
      options = list(scrollX = TRUE, pageLength = 20, deferRender = TRUE))
  }, server = TRUE)

  observeEvent(input$query_run, {
    req(rv$raw)
    question <- trimws(input$query_request %||% "")
    if (!nzchar(question)) {
      showNotification("Enter an analysis request.", type = "warning"); return()
    }
    if (!isTRUE(input$ai_data_access_ack)) {
      showNotification("Acknowledge responsibility for AI data access before running a complete-data query.", type = "warning", duration = 8)
      return()
    }
    data <- query_source_data()
    query_result(NULL); query_script_value(""); query_answer_value("")
    query_status_value(paste(selected_provider_label(), "is creating a structured query plan..."))
    outcome <- tryCatch(withProgress(message = paste(selected_provider_label(), "is planning and R is evaluating the complete dataset..."), value = 0.3, {
      raw_plan <- ai_provider_plan_data_query(selected_provider(), question, data,
        model = selected_model(),
        reasoning_effort = input$ai_reasoning %||% "low")
      executed <- ai_execute_query_plan(data, raw_plan)
      script <- ai_query_script(executed$plan)
      list(executed = executed, script = script)
    }), error = function(e) e)
    if (inherits(outcome, "error")) {
      query_status_value(paste("Query failed:", conditionMessage(outcome)))
      showNotification(conditionMessage(outcome), type = "error", duration = 10); return()
    }
    result <- outcome$executed$data
    plan <- outcome$executed$plan
    query_result(result); query_script_value(outcome$script)
    model_payload <- ai_query_result_for_model(result, max_json_chars = 180000L)
    base_status <- sprintf("Complete query finished: %s result rows x %s columns. No source rows were omitted.",
      format(nrow(result), big.mark = ","), format(ncol(result), big.mark = ","))
    plan_notes <- paste(Filter(nzchar, c(plan$compiler_note %||% "", plan$data_note %||% "", plan$result_note %||% "")), collapse = " ")
    if (nzchar(plan_notes)) base_status <- paste(base_status, plan_notes)
    if (!model_payload$reasonable) {
      query_status_value(paste0(base_status, " The complete result is too large for reliable AI processing (",
        format(model_payload$json_chars, big.mark = ","), " JSON bytes); use the table or downloads."))
      query_answer_value("The deterministic R query completed successfully. The result was not sent back to the selected AI because its serialized size exceeds the processing threshold; the displayed and downloadable result is complete.")
      return()
    }
    query_status_value(paste0(base_status, " Complete result sent to ", selected_provider_label(), " for interpretation."))
    interpretation <- tryCatch(ai_provider_call(selected_provider(),
      paste0("Interpret this complete deterministic query result for the user's request. State the result-row count, answer directly, and do not imply that the result was sampled. Original request: ", question),
      list(validated_plan = plan, complete_query_result = result), history = list(),
      model = selected_model(),
      reasoning_effort = input$ai_reasoning %||% "low"), error = function(e) e)
    query_answer_value(if (inherits(interpretation, "error"))
      paste("The R query succeeded, but AI interpretation failed:", conditionMessage(interpretation)) else interpretation)
  })

  output$query_download_csv <- downloadHandler(
    filename = function() paste0("ai_query_result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) { req(query_result()); utils::write.csv(query_result(), file, row.names = FALSE, na = "") }
  )
  output$query_download_xlsx <- downloadHandler(
    filename = function() paste0("ai_query_result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) { req(query_result()); openxlsx::write.xlsx(query_result(), file, asTable = TRUE, overwrite = TRUE) }
  )

  observeEvent(input$ai_ask, {
    question <- trimws(input$ai_question %||% "")
    if (!nzchar(question)) {
      showNotification("Enter a question for the selected AI.", type = "warning", duration = 5)
      return()
    }

    scope <- input$ai_data_scope %||% "summary"
    if (!identical(scope, "summary") && !isTRUE(input$ai_data_access_ack)) {
      showNotification(
        "Acknowledge responsibility for AI data access before using the whole dataset or a saved subset.",
        type = "warning", duration = 8
      )
      return()
    }
    if (identical(scope, "subset")) {
      nm <- input$ai_saved_subset %||% ""
      if (!nzchar(nm) || !nm %in% ai_subset_names()) {
        showNotification("Select an available named subset.", type = "warning", duration = 6)
        return()
      }
    }

    status <- ai_provider_status(selected_provider(), selected_model())
    ai_connection(status)
    if (!isTRUE(status$ok)) {
      showNotification(status$message, type = "error", duration = 8)
      return()
    }

    display <- ai_display_messages()
    display[[length(display) + 1L]] <- list(type = "user", label = "You", text = question)
    ai_display_messages(display)

    if (identical(scope, "summary")) {
      active_filter <- if (!is.null(rv$active_filter_var)) {
        list(variable = rv$active_filter_var, value = rv$active_filter_val, retained_rows = rv$active_filter_n)
      } else {
        list(description = "None")
      }
      filtered <- if (!is.null(rv$review)) {
        tryCatch(review_data_filtered(), error = function(e) rv$review$data[0, , drop = FALSE])
      } else {
        NULL
      }
      context <- ai_review_context(
        raw = analysis_data(),
        review = rv$review,
        filtered = filtered,
        source_name = rv$source_path,
        active_filter = active_filter
      )
      case_context <- selected_case_context()
      if (!is.null(case_context)) {
        context$selected_case_context <- case_context
        context$privacy <- "Bounded subject-level case context explicitly enabled after blinded-data confirmation"
      }
    } else {
      scoped_data <- ai_scope_data()
      outcome <- tryCatch(
        withProgress(message = paste(selected_provider_label(), "is planning and R is querying", ai_scope_label()), value = 0.35, {
          raw_plan <- ai_provider_plan_data_query(
            selected_provider(), question, scoped_data,
            model = selected_model(), reasoning_effort = input$ai_reasoning %||% "low"
          )
          executed <- ai_execute_query_plan(scoped_data, raw_plan)
          list(executed = executed, script = ai_query_script(executed$plan, source_name = "selected_ai_scope"))
        }),
        error = function(e) e
      )
      if (inherits(outcome, "error")) {
        display <- ai_display_messages()
        display[[length(display) + 1L]] <- list(
          type = "error", label = "Data query error", text = conditionMessage(outcome)
        )
        ai_display_messages(display)
        return()
      }
      result <- outcome$executed$data
      query_result(result)
      query_script_value(outcome$script)
      model_payload <- ai_query_result_for_model(result, max_json_chars = 180000L)
      query_status_value(sprintf(
        "Review chat queried every row in %s and returned %s rows x %s columns.",
        ai_scope_label(), format(nrow(result), big.mark = ","), ncol(result)
      ))
      if (!model_payload$reasonable) {
        display <- ai_display_messages()
        display[[length(display) + 1L]] <- list(
          type = "error", label = "Complete result retained locally",
          text = paste0(
            "R evaluated every source row, but the complete result is too large to send reliably to the selected AI (",
            format(model_payload$json_chars, big.mark = ","),
            " JSON bytes). The complete result is available in the Data query table and downloads. Refine the question to reduce the result size."
          )
        )
        ai_display_messages(display)
        return()
      }
      context <- list(
        data_state = "Complete read-only query evaluated",
        data_access_scope = ai_scope_label(),
        responsibility_acknowledged = TRUE,
        source_dimensions = list(rows = nrow(scoped_data), columns = ncol(scoped_data)),
        source_column_names = names(scoped_data),
        validated_plan = outcome$executed$plan,
        complete_query_result = result,
        result_dimensions = list(rows = nrow(result), columns = ncol(result)),
        privacy = paste0(
          "User explicitly acknowledged responsibility. R evaluated every row in the selected scope; ",
          "the complete query result was supplied without sampling or truncation."
        )
      )
    }

    prior_history <- ai_api_history()
    answer <- tryCatch(
      withProgress(message = paste(selected_provider_label(), "is reviewing the selected context..."), value = 0.5, {
        ai_provider_call(selected_provider(), question, context, prior_history,
          model = selected_model(),
          reasoning_effort = input$ai_reasoning %||% "low")
      }),
      error = function(e) e
    )

    display <- ai_display_messages()
    if (inherits(answer, "error")) {
      display[[length(display) + 1L]] <- list(
        type = "error", label = "Connection error", text = conditionMessage(answer)
      )
      ai_display_messages(display)
      return()
    }

    display[[length(display) + 1L]] <- list(type = "assistant",
      label = paste(selected_provider_label(), "—", selected_model()), text = answer)
    ai_display_messages(display)
    ai_api_history(tail(c(
      prior_history,
      list(list(role = "user", content = question), list(role = "assistant", content = answer))
    ), 8))
    updateTextAreaInput(session, "ai_question", value = "")
  })

  visualization_data <- reactive({
    req(rv$raw)
    if (identical(input$viz_data_source, "review")) {
      validate(need(!is.null(rv$review), "Run the timing review before selecting derived review data."))
      as.data.frame(rv$review$data)
    } else {
      as.data.frame(analysis_data())
    }
  })

  visualization_context <- reactive({
    req(rv$raw)
    active_filter <- if (!is.null(rv$active_filter_var)) {
      list(variable = rv$active_filter_var, value = rv$active_filter_val,
           retained_rows = rv$active_filter_n)
    } else list(description = "None")
    context <- ai_review_context(
      raw = analysis_data(),
      review = rv$review,
      filtered = if (!is.null(rv$review)) review_data_filtered() else NULL,
      source_name = rv$source_path,
      active_filter = active_filter
    )
    case_context <- selected_case_context()
    if (!is.null(case_context)) {
      context$selected_case_context <- case_context
      context$privacy <- "Bounded subject-level case context explicitly enabled after blinded-data confirmation"
    }
    context
  })

  observeEvent(input$viz_generate, {
    req(rv$raw)
    request <- trimws(input$viz_request %||% "")
    if (!nzchar(request)) {
      showNotification("Describe the visualization you want the selected AI to propose.", type = "warning")
      return()
    }
    status <- ai_provider_status(selected_provider(), selected_model())
    ai_connection(status)
    if (!isTRUE(status$ok)) {
      showNotification(status$message, type = "error", duration = 8)
      return()
    }
    proposal <- tryCatch(
      withProgress(message = paste(selected_provider_label(), "is preparing a visualization proposal..."), value = 0.5, {
        ai_provider_generate_visualization(selected_provider(),
          request = request,
          context = visualization_context(),
          available_columns = names(visualization_data()),
          model = selected_model(),
          reasoning_effort = input$ai_reasoning %||% "medium"
        )
      }),
      error = function(e) e
    )
    if (inherits(proposal, "error")) {
      showNotification(conditionMessage(proposal), type = "error", duration = 10)
      return()
    }
    viz_proposal(proposal)
    viz_result(NULL)
    viz_saved_file("")
    updateTextAreaInput(session, "viz_code", value = proposal$code)
    updateCheckboxInput(session, "viz_approve", value = FALSE)
    check <- validate_visualization_code_v12(proposal$code)
    viz_validation_state(check$message)
  })

  output$viz_title <- renderText({
    proposal <- viz_proposal()
    if (is.null(proposal)) "No visualization proposal yet" else proposal$title
  })
  output$viz_rationale <- renderText({
    proposal <- viz_proposal()
    if (is.null(proposal)) "Enter a request and click Generate proposal." else proposal$rationale
  })
  output$viz_validation <- renderText(viz_validation_state())
  output$viz_saved_path <- renderText({
    path <- viz_saved_file()
    if (nzchar(path)) paste("Saved locally:", path) else ""
  })

  observeEvent(input$viz_validate, {
    check <- validate_visualization_code_v12(input$viz_code %||% "")
    viz_validation_state(check$message)
    if (!isTRUE(check$ok)) updateCheckboxInput(session, "viz_approve", value = FALSE)
  })

  run_visualization_code <- function() {
    req(isTRUE(input$viz_approve))
    code <- input$viz_code %||% ""
    check <- validate_visualization_code_v12(code)
    viz_validation_state(check$message)
    if (!isTRUE(check$ok)) stop(check$message)
    exec_env <- new.env(parent = baseenv())
    exec_env$plot_data <- visualization_data()
    eval(check$expression, envir = exec_env)
    if (!exists("result_plot", envir = exec_env, inherits = FALSE)) {
      stop("Approved code did not create result_plot.")
    }
    plot <- exec_env$result_plot
    if (!inherits(plot, c("ggplot", "plotly", "htmlwidget"))) {
      stop("result_plot must be a ggplot or plotly object.")
    }
    plot
  }

  observeEvent(input$viz_run, {
    if (!isTRUE(input$viz_approve)) {
      showNotification("Review the code and select the approval checkbox first.", type = "warning")
      return()
    }
    result <- tryCatch(run_visualization_code(), error = function(e) e)
    if (inherits(result, "error")) {
      showNotification(conditionMessage(result), type = "error", duration = 10)
      return()
    }
    viz_result(result)
    showNotification("Approved visualization executed locally.", type = "message")
  })

  output$viz_generated_plot <- renderPlotly({
    plot <- viz_result()
    req(plot)
    if (inherits(plot, "ggplot")) plotly::ggplotly(plot) else plot
  })

  observeEvent(input$viz_save, {
    if (!isTRUE(input$viz_approve)) {
      showNotification("Review the code and select the approval checkbox before saving.", type = "warning")
      return()
    }
    code <- input$viz_code %||% ""
    check <- validate_visualization_code_v12(code)
    viz_validation_state(check$message)
    if (!isTRUE(check$ok)) {
      showNotification(check$message, type = "error", duration = 8)
      return()
    }
    out_dir <- .app_output_dir
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    path <- file.path(out_dir, paste0("visualization_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"))
    header <- c(
      "# Generated by ADPC Review V13 Multi-LLM Visualization Studio",
      paste0("# Provider: ", selected_provider_label()),
      paste0("# Model: ", selected_model()),
      paste0("# Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      "# Expects a local data.frame named plot_data; creates result_plot.", ""
    )
    writeLines(c(header, code), path, useBytes = TRUE)
    viz_saved_file(normalizePath(path, winslash = "/", mustWork = FALSE))
    showNotification("Approved R script saved locally.", type = "message")
  })

  flag_detected <- reactive({
    req(rv$raw)
    detect_analysis_flags_v12(analysis_data())
  })

  output$flag_select_ui <- renderUI({
    flags <- flag_detected()$all_flags
    if (!length(flags)) return(tags$em("No supported analysis-population flags detected."))
    preferred <- unique(c(flag_detected()$crit_flags, flag_detected()$nca_flags,
                          flag_detected()$pksum_flags, flag_detected()$analysis_flags))
    selectInput("flag_selected", "Flags in combinations", choices = flags,
      selected = head(preferred, 8), multiple = TRUE, selectize = TRUE)
  })
  output$flag_pair_x_ui <- renderUI({
    flags <- flag_detected()$all_flags
    if (!length(flags)) return(NULL)
    selectInput("flag_pair_x", "Pairwise rows", choices = flags, selected = flags[1])
  })
  output$flag_pair_y_ui <- renderUI({
    flags <- flag_detected()$all_flags
    if (!length(flags)) return(NULL)
    default <- if (length(flags) > 1) flags[2] else flags[1]
    selectInput("flag_pair_y", "Pairwise columns", choices = flags, selected = default)
  })

  output$flag_dictionary <- renderDT({
    req(rv$raw)
    d <- flag_dictionary_v12(analysis_data())
    validate(need(nrow(d) > 0, "No supported flag variables were detected."))
    review_datatable(d, rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 15, columnDefs = list(list(width = "380px", targets = 3))))
  })

  output$flag_rate_plot <- renderPlotly({
    req(rv$raw)
    d <- flag_dictionary_v12(analysis_data())
    validate(need(nrow(d) > 0, "No supported flag variables were detected."))
    d$VARIABLE <- factor(d$VARIABLE, levels = rev(d$VARIABLE))
    p <- ggplot(d, aes(x = PCT_Y, y = VARIABLE, fill = FAMILY,
      text = paste0("Variable: ", VARIABLE, "<br>Family: ", FAMILY,
                    "<br>Y: ", N_Y, " (", PCT_Y, "%)",
                    "<br>N: ", N_N, "<br>Missing: ", N_MISSING))) +
      geom_col() +
      scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(x = "Records with Y", y = NULL, fill = "Flag family") +
      theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text")
  })

  flag_combinations_current <- reactive({
    req(rv$raw)
    d <- analysis_data()
    flags <- intersect(input$flag_selected %||% character(), names(d))
    validate(need(length(flags) > 0, "Select at least one flag."))
    flag_combinations_v12(d, flags, id_var = input$id_var %||% NULL, max_rows = 100L)
  })
  output$flag_combinations <- renderDT({
    review_datatable(flag_combinations_current(), rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 20))
  })
  output$flag_combination_plot <- renderPlotly({
    d <- head(flag_combinations_current(), 25)
    d$SHORT <- ifelse(nchar(d$COMBINATION) > 90,
      paste0(substr(d$COMBINATION, 1, 87), "..."), d$COMBINATION)
    d$SHORT <- factor(d$SHORT, levels = rev(d$SHORT))
    p <- ggplot(d, aes(x = N_RECORDS, y = SHORT,
      text = paste0(COMBINATION, "<br>Records: ", N_RECORDS,
                    ifelse(is.na(N_SUBJECTS), "", paste0("<br>Subjects: ", N_SUBJECTS))))) +
      geom_col(fill = "#1565C0") + labs(x = "Records", y = NULL) +
      theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text")
  })

  flag_pair_current <- reactive({
    req(rv$raw, input$flag_pair_x, input$flag_pair_y)
    flag_pair_crosstab_v12(analysis_data(), input$flag_pair_x, input$flag_pair_y)
  })
  output$flag_pair_table <- renderDT({
    review_datatable(flag_pair_current(), rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 20, dom = "tip"))
  })
  output$flag_pair_plot <- renderPlotly({
    d <- flag_pair_current()
    validate(need(nrow(d) > 0, "Select two available flags."))
    p <- ggplot(d, aes(x = X_VALUE, y = Y_VALUE, fill = N_RECORDS,
      text = paste0(X_VARIABLE, "=", X_VALUE, "<br>", Y_VARIABLE, "=", Y_VALUE,
                    "<br>Records: ", N_RECORDS))) +
      geom_tile(colour = "white") + geom_text(aes(label = N_RECORDS), colour = "black") +
      scale_fill_gradient(low = "#E3F2FD", high = "#1565C0") +
      labs(x = unique(d$X_VARIABLE), y = unique(d$Y_VARIABLE), fill = "Records") +
      theme_bw(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  output$flag_issues <- renderDT({
    req(rv$raw)
    review_datatable(flag_alias_issues_v12(analysis_data()), rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 20, dom = "tip"))
  })

  flag_filter_id <- function(flag) paste0("flag_value_", gsub("[^A-Za-z0-9]", "_", flag))
  output$flag_value_filters_ui <- renderUI({
    req(rv$raw)
    d <- analysis_data()
    flags <- intersect(input$flag_selected %||% character(), names(d))
    if (!length(flags)) return(tags$em("Select flags above to enable record drill-down."))
    tagList(lapply(flags, function(flag) {
      values <- sort(unique(flag_clean_value(d[[flag]])))
      selectInput(flag_filter_id(flag), flag,
        choices = c("All" = "__ALL__", values), selected = "__ALL__", width = "220px")
    }))
  })
  flag_records_current <- reactive({
    req(rv$raw)
    d <- as.data.frame(analysis_data())
    flags <- intersect(input$flag_selected %||% character(), names(d))
    for (flag in flags) {
      selected <- input[[flag_filter_id(flag)]] %||% "__ALL__"
      if (!identical(selected, "__ALL__")) {
        d <- d[flag_clean_value(d[[flag]]) == selected, , drop = FALSE]
      }
    }
    core <- intersect(c(input$id_var, input$analyte_var, input$visit_var,
                        input$visitcd_var, input$nominal_var, input$actual_var,
                        input$mrrlt_var, input$aval_var), names(d))
    criteria <- unique(sub("FL$", "", flags[grepl("^CRIT[0-9]+FL$", toupper(flags))]))
    criteria <- names(d)[toupper(names(d)) %in% toupper(criteria)]
    keep <- unique(c(core, flags, criteria))
    d[, keep, drop = FALSE]
  })
  output$flag_records <- renderDT({
    d <- flag_records_current()
    review_datatable(d, rownames = FALSE, filter = "top",
      options = list(scrollX = TRUE, pageLength = 20))
  })

  shinyFiles::shinyFileChoose(
    input, "browse_path",
    roots = shinyFiles::getVolumes()(),
    filetypes = c("", "sas7bdat", "xpt", "csv", "rds", "rda", "RData")
  )

  observeEvent(input$browse_path, {
    sel <- shinyFiles::parseFilePaths(shinyFiles::getVolumes()(), input$browse_path)
    if (nrow(sel) > 0) {
      updateTextInput(session, "file_path", value = as.character(sel$datapath[1]))
    }
  })

  load_dataset <- function(path) {
    path <- clean_local_path(path)
    rv$review_snapshot <- NULL
    rv$review_saved_path <- NULL
    rv$source_sha256 <- NULL
    rv$pk_data <- NULL
    rv$pk_data_stale <- FALSE
    rv$pk_filter_description <- "No filter"
    rv$pk_filter_variable <- ""
    rv$pk_filter_mode <- "equals"
    rv$pk_filter_value <- "Y"
    rv$pk_filter_selected_n <- NULL
    rv$pk_filter_loaded_n <- NULL
    rv$sas_plot_items <- list()
    rv$nca <- NULL
    rv$last_conc_group_click <- rv$last_conc_click <- NULL
    rv$last_facet_group_click <- rv$last_facet_click <- rv$last_focus_click <- NULL
    updateSelectInput(session, "ind_subjects", choices = character(), selected = character())
    dat <- tryCatch({
      before <- digest::digest(file=path, algo="sha256", serialize=FALSE)
      loaded <- read_input_dataset(path)
      after <- digest::digest(file=path, algo="sha256", serialize=FALSE)
      if (!identical(before,after)) stop("Source file changed while loading; reload it.")
      rv$source_sha256 <- before
      loaded
    }, error = function(e) e)
    if (inherits(dat, "error")) {
      rv$source_sha256 <- NULL
      rv$source_path <- NULL
      rv$raw <- NULL
      rv$analysis_data <- NULL
      rv$review <- NULL
      showNotification(dat$message, type = "error", duration = 8)
      return(invisible(FALSE))
    }
    rv$ai_subsets <- list()
    rv$raw <- as.data.frame(dat)
    rv$analysis_data <- rv$raw
    rv$review <- NULL
    rv$review_mappings <- NULL
    rv$source_path <- path
    updateTextInput(session, "file_path", value = path)
    rv$active_filter_var <- NULL
    rv$active_filter_val <- NULL
    rv$active_filter_n   <- NULL
    showNotification(sprintf("Loaded %s", basename(path)), type = "message", duration = 4)
    showNotification("Grouping values audit is ready under Data > Grouping values audit.", type = "message", duration = 7)
    invisible(TRUE)
  }

  observeEvent(input$load_path, {
    req(input$file_path)
    load_dataset(input$file_path)
  })

  output$var_select_ui <- renderUI({
    req(rv$raw)
    nm <- names(rv$raw)

    default_pick <- function(candidates) {
      for (candidate in candidates) {
        hit <- nm[toupper(nm) == toupper(candidate)]
        if (length(hit)) return(hit[1])
      }
      ""
    }

    tagList(
      selectInput("id_var", "Subject ID", choices = nm, selected = default_pick(c("USUBJID", "SUBJID"))),
      selectInput("analyte_var", "Analyte (ATEST / PCTEST)", choices = nm,
                  selected = default_pick(c("ATEST", "PCTEST"))),
      selectInput("visit_var", "Analysis Visit", choices = nm, selected = default_pick(c("AVISIT"))),
      selectInput("visitcd_var", "Analysis Visit Code (VISITCD optional)", choices = c("", nm), selected = default_pick(c("VISITCD"))),
      selectInput("nominal_var", "Nominal Time (for example NRRLT / NFRLT)", choices = nm,
                  selected = default_pick(c("NRRLT", "NFRLT", "PCTPTNUM"))),
      selectInput("actual_var", "Calculated Relative Time from Dose (for example ARRLT / AFRLT)", choices = nm,
                  selected = default_pick(c("ARRLT", "AFRLT"))),
      selectInput("mrrlt_var", "Analysis Relative Time from Dose (for example MRRLT / MFRLT)", choices = c("", nm),
                  selected = default_pick(c("MRRLT", "MFRLT", "ARRLT", "AFRLT"))),
      selectInput("aval_var", "Concentration", choices = nm, selected = default_pick(c("AVAL"))),
      selectInput("adtm_var", "ADTM (optional)", choices = c("", nm), selected = default_pick(c("ADTM"))),
      selectInput("arftdtm_var", "ARFTDTM / PCRFTDTM (optional)",
        choices = c("Not available / not used" = "", nm), selected = default_pick(c("ARFTDTM", "PCRFTDTM"))),
      tags$small("Reference and sample timestamps are included in review listings; ARRLT is read from the dataset, not recalculated from these timestamps.")
    )
  })

  output$filter_select_ui <- renderUI({
    req(rv$raw)
    nm <- names(rv$raw)

    # Detect the complete analysis-population flag system used in ADPC/ADNCA.
    detected_flags <- detect_analysis_flags_v12(rv$raw)
    filter_vars <- detected_flags$all_flags

    if (length(filter_vars) == 0) {
      return(tags$em(style = "color:#888;", "No CRIT/NCA/ANL/PKSUM filter variables detected in this dataset."))
    }

    build_label <- function(var) {
      if (grepl("^CRIT\\d+FL$", var, ignore.case = TRUE)) {
        # Use paired CRITxx variable values as meaning
        crit_base <- sub("FL$", "", toupper(var))
        crit_col  <- nm[toupper(nm) == crit_base]
        if (length(crit_col) == 1) {
          vals <- unique(as.character(rv$raw[[crit_col]]))
          vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
          if (length(vals) > 0) {
            meaning <- paste(vals[seq_len(min(3, length(vals)))], collapse = " / ")
            if (length(vals) > 3) meaning <- paste0(meaning, " ...")
            return(paste0(var, "  [", meaning, "]"))
          }
        }
        return(var)
      } else {
        # Use haven variable label for NCA/ANL/PKSUM flags.
        lbl <- attr(rv$raw[[var]], "label")
        if (!is.null(lbl) && nzchar(trimws(lbl))) {
          return(paste0(var, "  [", trimws(lbl), "]"))
        }
        return(var)
      }
    }

    choices <- setNames(filter_vars, vapply(filter_vars, build_label, character(1)))

    tagList(
      radioButtons("filter_enabled", "Apply Data filter to all downstream analyses",
        choices = c("NO FILTER" = "no", "USE FILTER" = "yes"), selected = "no", inline = TRUE),
      conditionalPanel(
        condition = "input.filter_enabled === 'yes'",
        selectInput("filter_var", "Filter variable", choices = choices, selected = filter_vars[1]),
        radioButtons(
          "filter_val", "Filter condition",
          choices  = c("Y" = "Y", "N" = "N", "NULL (empty)" = "__NULL__"),
          selected = "Y",
          inline   = TRUE
        )
      ),
      div(class = "help-note", textOutput("filter_commit_status"))
    )
  })

  output$filter_commit_status <- renderText({
    req(rv$raw)
    selected <- if (identical(input$filter_enabled, "yes"))
      paste0(input$filter_var %||% "(choose variable)", " == ", input$filter_val %||% "Y") else "NO FILTER"
    committed <- if (is.null(rv$active_filter_var)) "NO FILTER" else
      paste0(rv$active_filter_var, " == ", rv$active_filter_val, " (", format(rv$active_filter_n, big.mark = ","), " rows)")
    pending <- if (is.null(rv$review)) TRUE else {
      selected_key <- if (identical(input$filter_enabled, "yes")) paste(input$filter_var %||% "", input$filter_val %||% "Y") else "NO FILTER"
      committed_key <- if (is.null(rv$active_filter_var)) "NO FILTER" else paste(rv$active_filter_var, if (identical(rv$active_filter_val, "NULL (empty)")) "__NULL__" else rv$active_filter_val)
      !identical(selected_key, committed_key)
    }
    paste0("Current downstream scope: ", committed, if (pending) ". Click Run review to apply the selected scope: " else ". Selected scope is applied: ", selected, ".")
  })


  source(file.path(.app_dir, "adpc_review_reports.R"), local = TRUE)

  observeEvent(input$run_review, {
    req(rv$raw)

    if (identical(input$review_dataset_kind, "parameter")) {
      run_parameter_review()
      return()
    }
    vars <- safe_names(c(
      input$id_var, input$analyte_var, input$visit_var, input$nominal_var,
      input$actual_var, input$aval_var
    ))

    if (!all(!is.na(vars))) {
      showNotification("Please map the required variables before running the review.", type = "error", duration = 8)
      return()
    }

    # Apply data filter if one is selected
    dat_to_use <- rv$raw
    fvar <- input$filter_var
    fval <- input$filter_val %||% "Y"
    if (identical(input$filter_enabled, "yes") && !is.null(fvar) && nzchar(fvar) && fvar %in% names(rv$raw)) {
      if (identical(fval, "__NULL__")) {
        dat_to_use <- rv$raw %>%
          dplyr::filter(is.na(.data[[fvar]]) | trimws(as.character(.data[[fvar]])) == "")
      } else {
        dat_to_use <- rv$raw %>%
          dplyr::filter(!is.na(.data[[fvar]]) & as.character(.data[[fvar]]) == fval)
      }
      filter_label <- fval
      if (identical(fval, "__NULL__")) filter_label <- "NULL (empty)"
      showNotification(
        sprintf("Filter applied: %s == %s  (%d of %d rows retained)", fvar, filter_label, nrow(dat_to_use), nrow(rv$raw)),
        type = "message", duration = 5
      )
      if (nrow(dat_to_use) == 0) {
        showNotification("Filter resulted in 0 rows. Check your filter selection.", type = "error", duration = 8)
        return()
      }
      rv$active_filter_var <- fvar
      rv$active_filter_val <- filter_label
      rv$active_filter_n   <- nrow(dat_to_use)
    } else {
      rv$active_filter_var <- NULL
      rv$active_filter_val <- NULL
      rv$active_filter_n   <- NULL
    }

    res <- tryCatch(
      compute_arrlt_review(
        dat = dat_to_use,
        id_var = input$id_var,
        analyte_var = input$analyte_var,
        visit_var = input$visit_var,
        visitcd_var = safe_col(input$visitcd_var),
        nominal_var = input$nominal_var,
        actual_var = input$actual_var,
        aval_var = input$aval_var,
        mrrlt_var = safe_col(input$mrrlt_var),
        adtm_var = safe_col(input$adtm_var),
        arftdtm_var = safe_col(input$arftdtm_var),
        pctdev_threshold = input$pctdev_threshold,
        absdev_threshold = input$absdev_threshold,
        use_iqr = isTRUE(input$enable_iqr),
        iqr_multiplier = input$iqr_multiplier %||% 1.5,
        early_window = input$early_window,
        predose_neg_threshold = input$predose_neg_threshold
      ),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      rv$review <- NULL
      showNotification(res$message, type = "error", duration = 10)
      return()
    }

    rv$review <- res
    session$sendCustomMessage("adpc-review-kind", FALSE)
    rv$review_mappings <- list(
      subject = input$id_var, analyte = input$analyte_var, visit = input$visit_var,
      nominal = input$nominal_var, actual = input$actual_var, aval = input$aval_var,
      mrrlt = input$mrrlt_var, profile = "APROFILE", sample_time = input$adtm_var,
      dose_time = input$arftdtm_var
    )
    rv$analysis_data <- as.data.frame(dat_to_use)
    commit_concentration_report(dat_to_use)
    d <- rv$review$data

    pctests <- sort(unique(d$.pctest))
    visits_display <- d %>% distinct(visit_display) %>% arrange(visit_display)

    updateSelectInput(session, "focus_pctest", choices = pctests, selected = pctests[1] %||% NULL)
    updateSelectInput(session, "profile_pctest", choices = c("All" = "__ALL__", pctests), selected = "__ALL__")
    updateSelectInput(session, "conc_group_pctest", choices = pctests, selected = pctests[1] %||% NULL)
    updateSelectInput(session, "facet_group_pctest", choices = pctests, selected = pctests[1] %||% NULL)

    visit_choices <- visits_display$visit_display
    updateSelectInput(session, "focus_visit", choices = visit_choices, selected = visit_choices[1] %||% NULL)
    updateSelectInput(session, "profile_visit", choices = c("All" = "__ALL__", visit_choices), selected = "__ALL__")
    updateSelectInput(session, "conc_group_visit", choices = visit_choices, selected = visit_choices[1] %||% NULL)
    updateSelectInput(session, "facet_group_visit", choices = visit_choices, selected = visit_choices[1] %||% NULL)

    subj <- sort(unique(d$.id))
    updateSelectInput(session, "profile_subject", choices = c("All" = "__ALL__", subj), selected = "__ALL__")
  })

  review_summary_items <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    if (identical(rv$review$kind,"parameter")) return(rv$review_snapshot$summary_items)
    item <- function(text,kind="plain",target=NULL) {
      section_id <- if(kind=="finding") paste0("finding-",target) else if(kind=="audit") switch(target,
        "Grouping values"="audit-grouping-values", "Profile time points"="audit-profile-timepoints",
        "Subject ID mapping"="audit-subject-id-mapping", "") else ""
      list(text=text,kind=kind,target=target,section_id=section_id)
    }
    plain_line <- function(text) item(text)
    audit_line <- function(text,target) item(text,"audit",target)
    finding_line <- function(text,target) item(text,"finding",target)
    d <- rv$review$data
    s <- rv$review$subject_summary
    profile_info <- profile_assessment()
    subject_counts <- subject_identifier_counts(d)
    analysis_subject_counts <- subject_identifier_counts(analysis_data())
    n_subj_flagged <- sum(s$n_review > 0, na.rm = TRUE)
    n_profiles_nrrlt_dup <- d %>%
      group_by(.id, .pctest, .avisit, .visitcd) %>%
      summarise(nrrlt_dup = first(nrrlt_dup), .groups = "drop") %>%
      filter(nrrlt_dup == "Y") %>%
      nrow()
    n_profiles_mrrlt_dup <- d %>%
      group_by(.id, .pctest, .avisit, .visitcd) %>%
      summarise(mrrlt_dup = first(mrrlt_dup), .groups = "drop") %>%
      filter(mrrlt_dup == "Y") %>%
      nrow()
    profile_flag_counts <- function(column) {
      if (!column %in% names(d) || all(is.na(d[[column]]))) return("not present")
      profile_values <- d %>%
        group_by(.id, .pctest, .avisit, .visitcd) %>%
        summarise(value = first(.data[[column]]), .groups = "drop")
      paste0(sum(profile_values$value == "Y", na.rm = TRUE), " / ",
             sum(profile_values$value == "N_OR_EMPTY", na.rm = TRUE))
    }
    filter_line <- if (!is.null(rv$active_filter_var)) {
      sprintf("Active filter: %s == %s  (%d rows used)", rv$active_filter_var, rv$active_filter_val, rv$active_filter_n)
    } else {
      "Active filter: None"
    }
    id_relationship <- if (isTRUE(profile_info$id_check$one_to_one)) "USUBJID:SUBJID is 1:1"
      else if (isFALSE(profile_info$id_check$one_to_one)) "WARNING: USUBJID:SUBJID is NOT 1:1"
      else "USUBJID:SUBJID check unavailable"
    list(
      plain_line(paste0("Dataset: ", rv$source_path %||% "local dataset")),
      audit_line(paste0(
        "Review mappings: nominal = ", rv$review_mappings$nominal,
        " | calculated = ", rv$review_mappings$actual,
        " | analysis relative = ", if (nzchar(rv$review_mappings$mrrlt %||% "")) rv$review_mappings$mrrlt else "not mapped"
      ), "Grouping values"),
      audit_line(filter_line, "Grouping values"),
      finding_line(paste0(
        "Review ready. Records: ", nrow(d), " | Flagged records: ", sum(d$review_fl == "Y", na.rm = TRUE)
      ), "flagged_records"),
      audit_line(paste0(
        "Review unique USUBJID: ", if (is.na(subject_counts$usubjid)) "not present" else subject_counts$usubjid,
        " | Review unique SUBJID: ", if (is.na(subject_counts$subjid)) "not present" else subject_counts$subjid,
        " | Facet combinations: ", dplyr::n_distinct(d$facet_label)
      ), "Grouping values"),
      audit_line(paste0(
        "Analysis-scope unique USUBJID: ", if (is.na(analysis_subject_counts$usubjid)) "not present" else analysis_subject_counts$usubjid,
        " | Analysis-scope unique SUBJID: ", if (is.na(analysis_subject_counts$subjid)) "not present" else analysis_subject_counts$subjid,
        " | ", id_relationship
      ), "Subject ID mapping"),
      audit_line(profile_info$message, "Profile time points"),
      audit_line(paste0("Number of concentration-time profiles: ", profile_info$n_profiles), "Profile time points"),
      finding_line(paste0("Flagged subject-visit combinations: ", n_subj_flagged), "flagged_records"),
      finding_line(paste0("Records flagged as IQR outlier: ", sum(d$arrlt_outlierfl == "Y", na.rm = TRUE)), "iqr_outlier"),
      finding_line(paste0("Records with large % deviation: ", sum(d$arrlt_bigdevfl == "Y", na.rm = TRUE)), "large_pct"),
      finding_line(paste0("Records with large absolute deviation: ", sum(d$arrlt_absdevfl == "Y", na.rm = TRUE)), "large_abs"),
      finding_line(paste0("Records with PREdose taken POST-dose: ", sum(d$predose_postdose_fl == "Y", na.rm = TRUE)), "predose_postdose"),
      finding_line(paste0("Records with POST-dose taken PREdose: ", sum(d$postdose_predose_fl == "Y", na.rm = TRUE)), "postdose_predose"),
      finding_line(paste0("Records with PREdose suspiciously early: ", sum(d$predose_early_fl == "Y", na.rm = TRUE)), "predose_early"),
      finding_line(paste0("Records where ", rv$review_mappings$mrrlt, " differs from ", rv$review_mappings$actual, ": ", sum(d$mrrlt_ne_arrlt_fl == "Y", na.rm = TRUE)), "relative_mismatch"),
      finding_line(paste0("Subject-analyte-visit profiles with duplicate ", rv$review_mappings$nominal, ": ", n_profiles_nrrlt_dup), "duplicate_nominal"),
      finding_line(paste0("Subject-analyte-visit profiles with duplicate ", rv$review_mappings$mrrlt, ": ", n_profiles_mrrlt_dup), "duplicate_relative"),
      audit_line(paste0("Subject-analyte-visit profiles with APROFILE = Y (or populated) / N (or empty): ", profile_flag_counts(".profile_aprofile")), "Profile time points"),
      audit_line(paste0("Subject-visit profiles with PKSUMXFL=Y / N (or empty): ", profile_flag_counts(".profile_pksumxfl")), "Grouping values"),
      audit_line(paste0("Subject-visit profiles with NCA01FL=Y / N (or empty): ", profile_flag_counts(".profile_nca01fl")), "Grouping values"),
      audit_line(paste0("Subject-visit profiles with ANL01FL=Y / N (or empty): ", profile_flag_counts(".profile_anl01fl")), "Grouping values"),
      audit_line(paste0("Subject-visit profiles with ANL02FL=Y / N (or empty): ", profile_flag_counts(".profile_anl02fl")), "Grouping values")
    )
  })

  observeEvent(input$review_summary_audit_link, {
    target <- input$review_summary_audit_link
    req(target %in% c("Grouping values", "Profile time points", "Subject ID mapping"))
    updateTabsetPanel(session, "main_tabs", selected = "Grouping values audit")
    session$onFlushed(function() {
      updateTabsetPanel(session, "grouping_audit_tabs", selected = target)
    }, once = TRUE)
  })

  observeEvent(input$review_summary_finding_link, {
    target <- input$review_summary_finding_link
    req(target %in% names(review_finding_specs))
    updateTabsetPanel(session, "main_tabs", selected = "Review finding subsets")
    session$onFlushed(function() {
      updateTabsetPanel(session, "review_finding_tabs", selected = target)
    }, once = TRUE)
  })

  output$preview <- renderDT({
    req(rv$raw)
    review_datatable(head(rv$raw, 100), filter = "top", options = list(scrollX = TRUE, pageLength = 10))
  })

  output$preview_derived <- renderDT({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    review_datatable(head(format_review_listing(rv$review$data), 100), filter = "top", options = list(scrollX = TRUE, pageLength = 10))
  })

  mapped_label <- function(value, fallback) {
    if (is.null(value) || !length(value) || is.na(value[1]) || !nzchar(trimws(value[1]))) fallback else value[1]
  }
  axis_var_label <- function(code) {
    nominal_label <- mapped_label(input$nominal_var, "Nominal time")
    actual_label <- mapped_label(input$actual_var, "Calculated relative time")
    mrrlt_label <- mapped_label(input$mrrlt_var, "Analysis relative time")
    switch(
      code,
      nrrlt = nominal_label,
      arrlt = actual_label,
      mrrlt = mrrlt_label,
      arrlt_dev = paste(actual_label, "-", nominal_label),
      mrrlt_dev = paste(mrrlt_label, "-", nominal_label),
      aval = "AVAL",
      log_aval = "ln(AVAL)",
      code
    )
  }

  timing_colors <- c(
    "Normal" = "#4E79A7",
    "Other flagged" = "#d62728",
    "PREdose taken POST-dose" = "#ff7f0e",
    "POST-dose taken PREdose" = "#9467bd",
    "PREdose suspiciously early" = "#8c564b",
    "MRRLT differs from ARRLT" = "#2ca02c"
  )

  matches_selected_cases <- function(dat, selected_cases) {
    if (is.null(selected_cases) || length(selected_cases) == 0) {
      return(rep(TRUE, nrow(dat)))
    }
    keep <- rep(FALSE, nrow(dat))
    if ("IQR outlier" %in% selected_cases) keep <- keep | dat$arrlt_outlierfl == "Y"
    if ("Large % deviation" %in% selected_cases) keep <- keep | dat$arrlt_bigdevfl == "Y"
    if ("Large absolute deviation" %in% selected_cases) keep <- keep | dat$arrlt_absdevfl == "Y"
    if ("PREdose taken POST-dose" %in% selected_cases) keep <- keep | dat$predose_postdose_fl == "Y"
    if ("POST-dose taken PREdose" %in% selected_cases) keep <- keep | dat$postdose_predose_fl == "Y"
    if ("PREdose suspiciously early" %in% selected_cases) keep <- keep | dat$predose_early_fl == "Y"
    if ("MRRLT differs from ARRLT" %in% selected_cases) keep <- keep | dat$mrrlt_ne_arrlt_fl == "Y"
    if ("Profiles with duplicates at NRRLT" %in% selected_cases) keep <- keep | dat$nrrlt_dup == "Y"
    if ("Profiles with duplicate MRRLT" %in% selected_cases) keep <- keep | dat$mrrlt_dup == "Y"
    profile_case <- function(label_y, label_n, column) {
      values <- dat[[column]]
      result <- rep(FALSE, nrow(dat))
      if (label_y %in% selected_cases) result <- result | (!is.na(values) & values == "Y")
      if (label_n %in% selected_cases) result <- result | (!is.na(values) & values == "N_OR_EMPTY")
      result
    }
    keep <- keep | profile_case("Profiles with PKSUMXFL=Y", "Profiles with PKSUMXFL=N or empty", ".profile_pksumxfl")
    keep <- keep | profile_case("Profiles with NCA01FL=Y", "Profiles with NCA01FL=N or empty", ".profile_nca01fl")
    keep <- keep | profile_case("Profiles with ANL01FL=Y", "Profiles with ANL01FL=N or empty", ".profile_anl01fl")
    keep <- keep | profile_case("Profiles with ANL02FL=Y", "Profiles with ANL02FL=N or empty", ".profile_anl02fl")
    keep
  }

  review_data_filtered <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- rv$review$data

    if (isTRUE(input$show_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }

    if (isTRUE(input$show_selected_focus_only) &&
        !is.null(input$focus_pctest) &&
        !is.null(input$focus_visit) &&
        !is.null(input$focus_nrrlt) &&
        nzchar(as.character(input$focus_pctest)) &&
        nzchar(as.character(input$focus_visit)) &&
        nzchar(as.character(input$focus_nrrlt))) {
      d <- d %>%
        filter(
          .pctest == input$focus_pctest,
          visit_display == input$focus_visit,
          as.character(.nrrlt) == as.character(input$focus_nrrlt)
        )
    }

    case_keep <- rep(FALSE, nrow(d))
    if (!is.null(input$review_case_filter) && length(input$review_case_filter) > 0) {
      case_keep <- matches_selected_cases(d, input$review_case_filter)
    }

    pctdev_min <- suppressWarnings(as.numeric(input$pctdev_min_filter))
    pctdev_max <- suppressWarnings(as.numeric(input$pctdev_max_filter))

    if (!is.na(pctdev_min) || !is.na(pctdev_max)) {
      pctdev_in_range <- !is.na(d$arrlt_pctdev) &
        (is.na(pctdev_min) | d$arrlt_pctdev >= pctdev_min) &
        (is.na(pctdev_max) | d$arrlt_pctdev <= pctdev_max)

      if (!is.null(input$review_case_filter) && length(input$review_case_filter) > 0) {
        pctdev_in_range <- pctdev_in_range | case_keep
      }

      if (isTRUE(input$pctdev_filter_review_only)) {
        d <- d[pctdev_in_range | d$review_fl != "Y", , drop = FALSE]
      } else {
        d <- d[pctdev_in_range, , drop = FALSE]
      }
    }

    if (!is.null(input$review_case_filter) && length(input$review_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$review_case_filter), , drop = FALSE]
    }

    d
  })

  output$download_ui <- renderUI({
    if ("excel" %in% input$outputs) {
      downloadButton("download_excel", "Save Excel listing")
    }
  })

  format_review_listing <- function(dat) {
    dat %>%
      transmute(
        Subject = .id,
        PCTEST = .pctest,
        AVISIT = .avisit,
        VISITCD = .visitcd,
        NRRLT = .nrrlt,
        ARRLT = .arrlt,
        MRRLT = .mrrlt,
        NRRLT_FL = nrrlt_fl,
        MRRLT_FL = mrrlt_fl,
        NRRLT_DUP = nrrlt_dup,
        MRRLT_DUP = mrrlt_dup,
        MRRLTneARRLT = mrrlt_ne_arrlt_fl,
        AVAL = .aval,
        ARRLT_DEV = round(arrlt_dev, 2),
        MRRLT_DEV = round(mrrlt_dev, 2),
        ARRLT_PCTDEV = round(arrlt_pctdev, 1),
        LOWER_IQR = round(lower_iqr, 2),
        UPPER_IQR = round(upper_iqr, 2),
        IQR_OUTLIER = arrlt_outlierfl,
        NEGATIVE_ARRLT = arrlt_negfl,
        PREDOSE_POSTDOSE = predose_postdose_fl,
        POSTDOSE_PREDOSE = postdose_predose_fl,
        PREDOSE_EARLY = predose_early_fl,
        TIMING_CASE = timing_case,
        BIG_PCTDEV = arrlt_bigdevfl,
        BIG_ABSDEV = arrlt_absdevfl,
        EARLY_POSITIVE = early_positivefl,
        REVIEW = review_fl,
        REVIEW_REASON = review_reason,
        ADTM = .adtm,
        ARFTDTM = .arftdtm
      )
  }

  output$review_table <- renderDT({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- format_review_listing(review_data_filtered())

    review_datatable(
      d,
      filter = "top",
      extensions = c("Buttons", "FixedHeader", "FixedColumns"),
      options = list(
        scrollX = TRUE,
        scrollY = "600px",
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 4)
      )
    )
  })

  conc_data <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- rv$review$data
    if (isTRUE(input$conc_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$conc_case_filter) && length(input$conc_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$conc_case_filter), , drop = FALSE]
    }
    d %>% filter(!is.na(.nrrlt), !is.na(.aval))
  })

  output$conc_plot <- renderPlotly({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    validate(need("conc" %in% input$outputs, "Enable 'Conc vs Time overview' in the Outputs section."))
    d <- conc_data()
    d <- d[d$facet_label %in% conc_page()$values, , drop = FALSE]
    validate(need(nrow(d) > 0, "No data available for the selected concentration-time overview."))

    pts <- d %>%
      mutate(
        facet_click_key = paste(.pctest, .avisit, ifelse(is.na(.visitcd), "", .visitcd), sep = "||"),
        click_key = paste(.id, .pctest, .avisit, ifelse(is.na(.visitcd), "", .visitcd), .nrrlt, sep = "||"),
        tooltip_txt = paste(
          "Subject:", .id,
          "<br>PCTEST:", .pctest,
          "<br>AVISIT:", .avisit,
          ifelse(is.na(.visitcd), "", paste0("<br>VISITCD: ", .visitcd)),
          "<br>NRRLT:", .nrrlt,
          "<br>ARRLT:", .arrlt,
          "<br>MRRLT:", .mrrlt,
          "<br>NRRLT_FL:", nrrlt_fl,
          "<br>MRRLT_FL:", mrrlt_fl,
          "<br>NRRLT_DUP:", nrrlt_dup,
          "<br>MRRLT_DUP:", mrrlt_dup,
          "<br>AVAL:", .aval,
          "<br>Timing case:", timing_case,
          "<br>Reason:", ifelse(review_reason == "", "None", review_reason)
        )
      )

    use_log <- identical(input$conc_yaxis, "log_aval") && !any(pts$.aval <= 0, na.rm = TRUE)
    y_title <- if (use_log) "AVAL (log scale)" else if (identical(input$conc_yaxis, "log_aval")) "AVAL (linear: zero/negative values retained)" else "AVAL"

    g <- suppressWarnings(ggplot(
      pts,
      aes(x = .nrrlt, y = .aval, group = .id)
    )) +
      geom_line(alpha = 0.25, linewidth = 0.3, color = "grey60") +
      geom_point(aes(color = timing_case, key = facet_click_key, text = tooltip_txt), size = 2.2, alpha = 0.85) +
      labs(
        title = paste0("Conc vs Time overview: ", y_title, " by NRRLT"),
        x = "NRRLT",
        y = y_title
      ) +
      facet_wrap(~ facet_label, scales = "free_y", ncol = input$facet_ncol, labeller = label_wrap_gen(28)) +
      scale_color_manual(values = timing_colors, drop = FALSE) +
      theme_bw() +
      theme(
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    p_plotly <- ggplotly(g, tooltip = "text", source = "conc_src")
    p_plotly <- layout(p_plotly, title = list(y = 0.99), legend = list(orientation = "h", x = 0, y = -0.16), margin = list(t = 100, b = 120))
    if (use_log) {
      p_plotly <- layout(p_plotly, yaxis = list(type = "log", title = "AVAL (log scale)"))
    }
    p_plotly <- plotly::event_register(p_plotly, "plotly_click")
    p_plotly
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$conc_group_pctest)
    d <- rv$review$data %>% filter(.pctest == input$conc_group_pctest)
    visit_choices <- sort(unique(d$visit_display))
    selected_now <- isolate(input$conc_group_visit)
    selected_new <- if (!is.null(selected_now) && selected_now %in% visit_choices) selected_now else (visit_choices[1] %||% NULL)
    updateSelectInput(session, "conc_group_visit", choices = visit_choices, selected = selected_new)
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$facet_group_pctest)
    d <- rv$review$data %>% filter(.pctest == input$facet_group_pctest)
    visit_choices <- sort(unique(d$visit_display))
    selected_now <- isolate(input$facet_group_visit)
    selected_new <- if (!is.null(selected_now) && selected_now %in% visit_choices) selected_now else (visit_choices[1] %||% NULL)
    updateSelectInput(session, "facet_group_visit", choices = visit_choices, selected = selected_new)
  })

  conc_group_data <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$conc_group_pctest, input$conc_group_visit)
    d <- rv$review$data %>%
      filter(.pctest == input$conc_group_pctest, visit_display == input$conc_group_visit)
    if (isTRUE(input$conc_group_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$conc_group_case_filter) && length(input$conc_group_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$conc_group_case_filter), , drop = FALSE]
    }
    d %>% filter(!is.na(.nrrlt), !is.na(.aval))
  })

  output$conc_group_plot <- renderPlotly({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    validate(need("conc_group" %in% input$outputs, "Enable 'Conc vs Time by Group' in the Outputs section."))
    d <- conc_group_data()
    validate(need(nrow(d) > 0, "No data available for the selected concentration-time group."))

    pts <- d %>%
      mutate(
        click_key = paste(.id, .pctest, .avisit, ifelse(is.na(.visitcd), "", .visitcd), .nrrlt, sep = "||"),
        tooltip_txt = paste(
          "Subject:", .id,
          "<br>PCTEST:", .pctest,
          "<br>AVISIT:", .avisit,
          ifelse(is.na(.visitcd), "", paste0("<br>VISITCD: ", .visitcd)),
          "<br>NRRLT:", .nrrlt,
          "<br>ARRLT:", .arrlt,
          "<br>MRRLT:", .mrrlt,
          "<br>AVAL:", .aval,
          "<br>Timing case:", timing_case,
          "<br>Reason:", ifelse(review_reason == "", "None", review_reason)
        )
      )

    use_log <- identical(input$conc_group_yaxis, "log_aval") && !any(pts$.aval <= 0, na.rm = TRUE)
    y_title <- if (use_log) "AVAL (log scale)" else if (identical(input$conc_group_yaxis, "log_aval")) "AVAL (linear: zero/negative values retained)" else "AVAL"

    g <- suppressWarnings(ggplot(
      pts,
      aes(x = .nrrlt, y = .aval, group = .id)
    )) +
      geom_line(alpha = 0.3, linewidth = 0.35, color = "grey60") +
      geom_point(aes(color = timing_case, key = click_key, text = tooltip_txt), size = 2.5, alpha = 0.9) +
      labs(
        title = paste0("Conc vs Time by Group: ", y_title, " by NRRLT | ", input$conc_group_pctest, " | ", input$conc_group_visit),
        x = "NRRLT",
        y = y_title
      ) +
      scale_color_manual(values = timing_colors, drop = FALSE) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    p_plotly <- ggplotly(g, tooltip = "text", source = "conc_group_src")
    if (use_log) {
      p_plotly <- layout(p_plotly, yaxis = list(type = "log", title = "AVAL (log scale)"))
    }
    p_plotly <- plotly::event_register(p_plotly, "plotly_click")
    p_plotly
  })

  facet_group_data <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$facet_group_pctest, input$facet_group_visit)
    d <- rv$review$data %>%
      filter(.pctest == input$facet_group_pctest, visit_display == input$facet_group_visit)
    if (isTRUE(input$facet_group_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$facet_group_case_filter) && length(input$facet_group_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$facet_group_case_filter), , drop = FALSE]
    }
    d
  })

  output$facet_group_plot <- renderPlotly({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    validate(need("facet_group" %in% input$outputs, "Enable 'Facet overview by Group' in the Outputs section."))
    d <- facet_group_data()
    validate(need(nrow(d) > 0, "No data available for the selected facet-overview group."))

    yvar <- if (input$facet_group_metric %in% c("arrlt", "mrrlt")) paste0(".", input$facet_group_metric) else input$facet_group_metric
    ylab <- axis_var_label(input$facet_group_metric)

    pts <- d %>%
      mutate(
        click_key = paste(.id, .pctest, .avisit, ifelse(is.na(.visitcd), "", .visitcd), .nrrlt, sep = "||"),
        tooltip_txt = paste(
          "Subject:", .id,
          "<br>PCTEST:", .pctest,
          "<br>AVISIT:", .avisit,
          ifelse(is.na(.visitcd), "", paste0("<br>VISITCD: ", .visitcd)),
          "<br>NRRLT:", .nrrlt,
          "<br>ARRLT:", .arrlt,
          "<br>MRRLT:", .mrrlt,
          "<br>NRRLT_FL:", nrrlt_fl,
          "<br>MRRLT_FL:", mrrlt_fl,
          "<br>NRRLT_DUP:", nrrlt_dup,
          "<br>MRRLT_DUP:", mrrlt_dup,
          "<br>MRRLTneARRLT:", mrrlt_ne_arrlt_fl,
          "<br>AVAL:", .aval,
          "<br>ARRLT_DEV:", round(arrlt_dev, 3),
          "<br>ARRLT_PCTDEV:", round(arrlt_pctdev, 1),
          "<br>Timing case:", timing_case,
          "<br>Reason:", ifelse(review_reason == "", "None", review_reason)
        )
      )

    g <- suppressWarnings(ggplot(d, aes(x = factor(.nrrlt), y = .data[[yvar]]))) +
      geom_boxplot(outlier.shape = NA, fill = "grey85") +
      labs(
        title = paste("Facet overview by Group:", ylab, "by NRRLT |", input$facet_group_pctest, "|", input$facet_group_visit),
        x = "NRRLT",
        y = ylab
      ) +
      scale_color_manual(values = timing_colors, drop = FALSE) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (isTRUE(input$facet_group_points)) {
      g <- g +
        geom_point(
          data = pts,
          aes(
            x = factor(.nrrlt),
            y = .data[[yvar]],
            color = timing_case,
            key = click_key,
            text = tooltip_txt
          ),
          position = position_jitter(width = 0.15, height = 0),
          size = 2.5,
          alpha = 0.85
        )
    }

    p_plotly <- ggplotly(g, tooltip = "text", source = "facet_group_src")
    p_plotly <- plotly::event_register(p_plotly, "plotly_click")
    p_plotly
  })

  facet_data <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- rv$review$data
    if (isTRUE(input$facet_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$facet_case_filter) && length(input$facet_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$facet_case_filter), , drop = FALSE]
    }
    d
  })

  output$facet_plot <- renderPlotly({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    validate(need("facet" %in% input$outputs, "Enable 'Facet overview plot' in the Outputs section."))
    d <- facet_data()
    d <- d[d$facet_label %in% facet_page()$values, , drop = FALSE]
    validate(need(nrow(d) > 0, "No data available for the selected facet view."))

    yvar <- if (input$facet_metric %in% c("arrlt", "mrrlt")) paste0(".", input$facet_metric) else input$facet_metric
    ylab <- axis_var_label(input$facet_metric)

    pts <- d %>%
      mutate(
        facet_click_key = paste(.pctest, .avisit, ifelse(is.na(.visitcd), "", .visitcd), sep = "||"),
        click_key = paste(.id, .pctest, .avisit, ifelse(is.na(.visitcd), "", .visitcd), .nrrlt, sep = "||"),
        tooltip_txt = paste(
          "Subject:", .id,
          "<br>PCTEST:", .pctest,
          "<br>AVISIT:", .avisit,
          ifelse(is.na(.visitcd), "", paste0("<br>VISITCD: ", .visitcd)),
          "<br>NRRLT:", .nrrlt,
          "<br>ARRLT:", .arrlt,
          "<br>MRRLT:", .mrrlt,
          "<br>NRRLT_FL:", nrrlt_fl,
          "<br>MRRLT_FL:", mrrlt_fl,
          "<br>NRRLT_DUP:", nrrlt_dup,
          "<br>MRRLT_DUP:", mrrlt_dup,
          "<br>MRRLTneARRLT:", mrrlt_ne_arrlt_fl,
          "<br>AVAL:", .aval,
          "<br>ARRLT_DEV:", round(arrlt_dev, 3),
          "<br>ARRLT_PCTDEV:", round(arrlt_pctdev, 1),
          "<br>Timing case:", timing_case,
          "<br>Reason:", ifelse(review_reason == "", "None", review_reason)
        )
      )

    g <- suppressWarnings(ggplot(d, aes(x = factor(.nrrlt), y = .data[[yvar]]))) +
      geom_boxplot(outlier.shape = NA, fill = "grey85") +
      labs(
        title = paste("Facet overview:", ylab, "by NRRLT within PCTEST + AVISIT"),
        x = "NRRLT",
        y = ylab
      ) +
      facet_wrap(~ facet_label, scales = "free_y", ncol = input$facet_ncol, labeller = label_wrap_gen(28)) +
      scale_color_manual(values = timing_colors, drop = FALSE) +
      theme_bw() +
      theme(
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    if (isTRUE(input$facet_points)) {
      g <- g +
        geom_point(
          data = pts,
          aes(
            x = factor(.nrrlt),
            y = .data[[yvar]],
            color = timing_case,
            key = facet_click_key,
            text = tooltip_txt
          ),
          position = position_jitter(width = 0.15, height = 0),
          size = 2.2,
          alpha = 0.8
        )
    }

    p_plotly <- ggplotly(g, tooltip = "text", source = "facet_src")
    p_plotly <- layout(p_plotly, title = list(y = 0.99), legend = list(orientation = "h", x = 0, y = -0.16), margin = list(t = 100, b = 120))
    p_plotly <- plotly::event_register(p_plotly, "plotly_click")
    p_plotly
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$focus_pctest, input$focus_visit)
    d <- rv$review$data %>%
      filter(.pctest == input$focus_pctest, visit_display == input$focus_visit)
    nrrlts <- sort(unique(d$.nrrlt))
    updateSelectInput(session, "focus_nrrlt", choices = nrrlts, selected = nrrlts[1] %||% NULL)
  })

  focus_data <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$focus_pctest, input$focus_visit, input$focus_nrrlt)
    d <- rv$review$data %>%
      filter(
        .pctest == input$focus_pctest,
        visit_display == input$focus_visit,
        .nrrlt == as.numeric(input$focus_nrrlt)
      )
    if (isTRUE(input$focus_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    d %>%
      arrange(desc(abs(arrlt_dev)), .id) %>%
      mutate(
        subject_ord = factor(.id, levels = unique(.id)),
        click_key = paste(.id, .pctest, .avisit, ifelse(is.na(.visitcd), "", .visitcd), .nrrlt, sep = "||")
      )
  })

  output$focus_plot <- renderPlotly({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    validate(need("focus" %in% input$outputs, "Enable 'Focused nominal-time view' in the Outputs section."))
    d <- focus_data()
    validate(need(nrow(d) > 0, "No data for the selected focus combination."))

    yvar <- if (input$focus_metric %in% c("arrlt", "mrrlt")) paste0(".", input$focus_metric) else input$focus_metric
    ylab <- axis_var_label(input$focus_metric)

    p_plotly <- plot_ly(
      d,
      x = ~subject_ord,
      y = as.formula(paste0("~", yvar)),
      type = "scatter",
      mode = "markers",
      color = ~timing_case,
      colors = timing_colors,
      key = ~click_key,
      source = "focus_src",
      marker = list(size = 9),
      hovertext = ~paste(
        "Subject:", .id,
        "<br>PCTEST:", .pctest,
        "<br>AVISIT:", .avisit,
        ifelse(is.na(.visitcd), "", paste0("<br>VISITCD: ", .visitcd)),
        "<br>NRRLT:", .nrrlt,
        "<br>ARRLT:", .arrlt,
        "<br>MRRLT:", .mrrlt,
        "<br>NRRLT_FL:", nrrlt_fl,
        "<br>MRRLT_FL:", mrrlt_fl,
        "<br>NRRLT_DUP:", nrrlt_dup,
        "<br>MRRLT_DUP:", mrrlt_dup,
        "<br>MRRLTneARRLT:", mrrlt_ne_arrlt_fl,
        "<br>ARRLT_DEV:", round(arrlt_dev, 3),
        "<br>ARRLT_PCTDEV:", round(arrlt_pctdev, 1),
        "<br>AVAL:", .aval,
        "<br>Timing case:", timing_case,
        "<br>Reason:", ifelse(review_reason == "", "None", review_reason)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Focused review:", input$focus_pctest, "|", input$focus_visit, "| NRRLT =", input$focus_nrrlt),
        xaxis = list(title = "Subject", tickangle = -45),
        yaxis = list(title = ylab)
      )
    p_plotly <- plotly::event_register(p_plotly, "plotly_click")
    p_plotly
  })

  output$focus_subjects <- renderDT({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- focus_data() %>%
      transmute(
        Subject = .id,
        ARRLT = .arrlt,
        MRRLT = .mrrlt,
        NRRLT_FL = nrrlt_fl,
        MRRLT_FL = mrrlt_fl,
        NRRLT_DUP = nrrlt_dup,
        MRRLT_DUP = mrrlt_dup,
        MRRLTneARRLT = mrrlt_ne_arrlt_fl,
        ARRLT_DEV = arrlt_dev,
        MRRLT_DEV = mrrlt_dev,
        ARRLT_PCTDEV = round(arrlt_pctdev, 1),
        AVAL = .aval,
        IQR_OUTLIER = arrlt_outlierfl,
        BIG_PCTDEV = arrlt_bigdevfl,
        BIG_ABSDEV = arrlt_absdevfl,
        NEGATIVE_ARRLT = arrlt_negfl,
        PREDOSE_POSTDOSE = predose_postdose_fl,
        POSTDOSE_PREDOSE = postdose_predose_fl,
        PREDOSE_EARLY = predose_early_fl,
        TIMING_CASE = timing_case,
        REVIEW = review_fl,
        REVIEW_REASON = review_reason,
        ADTM = .adtm,
        ARFTDTM = .arftdtm
      )

    review_datatable(d, filter = "top", options = list(scrollX = TRUE, pageLength = 12))
  })

  output$conc_group_summary <- renderUI({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$conc_group_pctest, input$conc_group_visit)
    d <- rv$review$data %>%
      filter(.pctest == input$conc_group_pctest, visit_display == input$conc_group_visit)
    if (isTRUE(input$conc_group_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$conc_group_case_filter) && length(input$conc_group_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$conc_group_case_filter), , drop = FALSE]
    }
    n_subj <- dplyr::n_distinct(d$.id)
    HTML(sprintf('<div style="margin-top:10px;font-weight:bold;">Records in selected group review table | Subjects: %d</div>', n_subj))
  })

  output$conc_group_review_table <- renderDT({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$conc_group_pctest, input$conc_group_visit)
    d <- rv$review$data %>%
      filter(.pctest == input$conc_group_pctest, visit_display == input$conc_group_visit)
    if (isTRUE(input$conc_group_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$conc_group_case_filter) && length(input$conc_group_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$conc_group_case_filter), , drop = FALSE]
    }

    review_datatable(
      format_review_listing(d),
      filter = "top",
      extensions = c("Buttons", "FixedHeader", "FixedColumns"),
      options = list(
        scrollX = TRUE,
        scrollY = "600px",
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 4)
      )
    )
  })

  output$facet_group_summary <- renderUI({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$facet_group_pctest, input$facet_group_visit)
    d <- rv$review$data %>%
      filter(.pctest == input$facet_group_pctest, visit_display == input$facet_group_visit)
    if (isTRUE(input$facet_group_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$facet_group_case_filter) && length(input$facet_group_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$facet_group_case_filter), , drop = FALSE]
    }
    n_subj <- dplyr::n_distinct(d$.id)
    HTML(sprintf('<div style="margin-top:10px;font-weight:bold;">Records in selected group review table | Subjects: %d</div>', n_subj))
  })

  output$facet_group_review_table <- renderDT({
    req(!identical(rv$review$kind, "parameter"), rv$review, input$facet_group_pctest, input$facet_group_visit)
    d <- rv$review$data %>%
      filter(.pctest == input$facet_group_pctest, visit_display == input$facet_group_visit)
    if (isTRUE(input$facet_group_review_only)) {
      d <- d %>% filter(review_fl == "Y")
    }
    if (!is.null(input$facet_group_case_filter) && length(input$facet_group_case_filter) > 0) {
      d <- d[matches_selected_cases(d, input$facet_group_case_filter), , drop = FALSE]
    }

    review_datatable(
      format_review_listing(d),
      filter = "top",
      extensions = c("Buttons", "FixedHeader", "FixedColumns"),
      options = list(
        scrollX = TRUE,
        scrollY = "600px",
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 4)
      )
    )
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, "conc_group" %in% input$outputs, identical(input$main_tabs, "Conc vs Time by Group"))
    ed <- plotly::event_data("plotly_click", source = "conc_group_src")
    req(!is.null(ed), nrow(ed) > 0)
    click_sig <- paste(capture.output(str(ed)), collapse = " ")
    if (identical(rv$last_conc_group_click, click_sig)) return(invisible(NULL))
    rv$last_conc_group_click <- click_sig

    clicked <- NULL
    if (!is.null(ed$key)) {
      clicked <- ed$key[[1]] %||% ed$key[1]
    }
    if (is.null(clicked) || length(clicked) == 0 || all(is.na(clicked)) || !nzchar(as.character(clicked)[1])) {
      return(invisible(NULL))
    }

    parts <- strsplit(as.character(clicked)[1], "\\|\\|")[[1]]
    req(length(parts) >= 5)

    subj_clicked <- parts[1]
    pctest_clicked <- parts[2]
    avisit_clicked <- parts[3]
    visitcd_clicked <- parts[4]

    visit_display_clicked <- if (is.na(visitcd_clicked) || visitcd_clicked == "") {
      avisit_clicked
    } else {
      paste0(avisit_clicked, " [", visitcd_clicked, "]")
    }

    rv$last_tab_before_profile <- "Conc vs Time by Group"
    updateSelectInput(session, "profile_pctest", selected = pctest_clicked)
    session$onFlushed(function() {
      updateSelectInput(session, "profile_visit", selected = visit_display_clicked)
      updateSelectInput(session, "profile_subject", selected = subj_clicked)
      updateTabsetPanel(session, "main_tabs", selected = "Subject profile")
    }, once = TRUE)
  })


  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, "conc" %in% input$outputs, identical(input$main_tabs, "Conc vs Time"))
    ed <- plotly::event_data("plotly_click", source = "conc_src")
    req(!is.null(ed), nrow(ed) > 0)
    click_sig <- paste(capture.output(str(ed)), collapse = " ")
    if (identical(rv$last_conc_click, click_sig)) return(invisible(NULL))
    rv$last_conc_click <- click_sig
    clicked <- NULL
    if (!is.null(ed$key)) {
      clicked <- ed$key[[1]] %||% ed$key[1]
    }
    req(!is.null(clicked), nzchar(as.character(clicked)[1]))
    parts <- strsplit(as.character(clicked)[1], "\\|\\|")[[1]]
    req(length(parts) >= 3)

    pctest_clicked <- parts[1]
    avisit_clicked <- parts[2]
    visitcd_clicked <- parts[3]

    visit_display_clicked <- if (is.na(visitcd_clicked) || visitcd_clicked == "") {
      avisit_clicked
    } else {
      paste0(avisit_clicked, " [", visitcd_clicked, "]")
    }

    updateSelectInput(session, "conc_group_pctest", selected = pctest_clicked)
    session$onFlushed(function() {
      updateSelectInput(session, "conc_group_visit", selected = visit_display_clicked)
      updateTabsetPanel(session, "main_tabs", selected = "Conc vs Time by Group")
    }, once = TRUE)
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, "facet" %in% input$outputs, identical(input$main_tabs, "Facet overview"))
    ed <- plotly::event_data("plotly_click", source = "facet_src")
    req(!is.null(ed), nrow(ed) > 0)
    click_sig <- paste(capture.output(str(ed)), collapse = " ")
    if (identical(rv$last_facet_click, click_sig)) return(invisible(NULL))
    rv$last_facet_click <- click_sig
    clicked <- NULL
    if (!is.null(ed$key)) {
      clicked <- ed$key[[1]] %||% ed$key[1]
    }
    req(!is.null(clicked), nzchar(as.character(clicked)[1]))
    parts <- strsplit(as.character(clicked)[1], "\\|\\|")[[1]]
    req(length(parts) >= 3)

    pctest_clicked <- parts[1]
    avisit_clicked <- parts[2]
    visitcd_clicked <- parts[3]

    visit_display_clicked <- if (is.na(visitcd_clicked) || visitcd_clicked == "") {
      avisit_clicked
    } else {
      paste0(avisit_clicked, " [", visitcd_clicked, "]")
    }

    updateSelectInput(session, "facet_group_pctest", selected = pctest_clicked)
    session$onFlushed(function() {
      updateSelectInput(session, "facet_group_visit", selected = visit_display_clicked)
      updateTabsetPanel(session, "main_tabs", selected = "Facet overview by Group")
    }, once = TRUE)
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    pctest_choices <- sort(unique(as.character(rv$review$data$.pctest)))
    current_pctest <- isolate(input$profile_pctest)
    selected_pctest <- if (!is.null(current_pctest) && (current_pctest == "__ALL__" || current_pctest %in% pctest_choices)) current_pctest else "__ALL__"
    updateSelectInput(session, "profile_pctest", choices = c("All" = "__ALL__", pctest_choices), selected = selected_pctest)
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- rv$review$data
    if (!is.null(input$profile_pctest) && input$profile_pctest != "__ALL__" && nzchar(input$profile_pctest)) {
      d <- d %>% filter(.pctest == input$profile_pctest)
    }
    visit_choices <- sort(unique(as.character(d$visit_display)))
    current_visit <- isolate(input$profile_visit)
    selected_visit <- if (!is.null(current_visit) && (current_visit == "__ALL__" || current_visit %in% visit_choices)) current_visit else "__ALL__"
    updateSelectInput(session, "profile_visit", choices = c("All" = "__ALL__", visit_choices), selected = selected_visit)
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- rv$review$data
    if (!is.null(input$profile_pctest) && input$profile_pctest != "__ALL__" && nzchar(input$profile_pctest)) {
      d <- d %>% filter(.pctest == input$profile_pctest)
    }
    if (!is.null(input$profile_visit) && input$profile_visit != "__ALL__" && nzchar(input$profile_visit)) {
      d <- d %>% filter(visit_display == input$profile_visit)
    }
    subj_choices <- sort(unique(as.character(d$.id)))
    current_subject <- isolate(input$profile_subject)
    selected_subject <- if (!is.null(current_subject) && (current_subject == "__ALL__" || current_subject %in% subj_choices)) current_subject else "__ALL__"
    updateSelectInput(session, "profile_subject", choices = c("All" = "__ALL__", subj_choices), selected = selected_subject)
  })

  profile_data <- reactive({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- rv$review$data
    if (!is.null(input$profile_subject) && input$profile_subject != "__ALL__" && nzchar(input$profile_subject)) {
      d <- d %>% filter(as.character(.id) == as.character(input$profile_subject))
    }
    if (!is.null(input$profile_pctest) && input$profile_pctest != "__ALL__" && nzchar(input$profile_pctest)) {
      d <- d %>% filter(.pctest == input$profile_pctest)
    }
    if (!is.null(input$profile_visit) && input$profile_visit != "__ALL__" && nzchar(input$profile_visit)) {
      d <- d %>% filter(visit_display == input$profile_visit)
    }
    d
  })

  output$profile_plot <- renderPlotly({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    validate(need("profile" %in% input$outputs, "Enable 'Subject concentration-time profile' in the Outputs section."))
    d <- profile_data()
    validate(need(nrow(d) > 0, "No profile data for the selected subject / PCTEST / visit."))

    label_txt <- if (isTRUE(input$profile_show_arrlt_labels)) {
      ifelse(d$review_fl == "Y", paste0("ARRLT=", round(d$.arrlt, 3)), "")
    } else {
      NULL
    }

    xvar <- paste0(".", input$profile_xaxis)
    profile_yaxis <- input$profile_yaxis
    if (identical(profile_yaxis, "log_aval") && any(d$.aval <= 0, na.rm = TRUE)) profile_yaxis <- "aval"
    yvar <- paste0(".", profile_yaxis)

    d <- d %>%
      mutate(
        plot_x = .data[[xvar]],
        plot_y = .data[[yvar]]
      ) %>%
      arrange(plot_x, .arrlt)

    hover_txt <- paste(
      "Subject:", d$.id,
      "<br>PCTEST:", d$.pctest,
      "<br>AVISIT:", d$.avisit,
      ifelse(is.na(d$.visitcd), "", paste0("<br>VISITCD: ", d$.visitcd)),
      "<br>NRRLT:", d$.nrrlt,
      "<br>ARRLT:", d$.arrlt,
      "<br>MRRLT:", d$.mrrlt,
      "<br>NRRLT_FL:", d$nrrlt_fl,
      "<br>MRRLT_FL:", d$mrrlt_fl,
      "<br>NRRLT_DUP:", d$nrrlt_dup,
      "<br>MRRLT_DUP:", d$mrrlt_dup,
      "<br>MRRLTneARRLT:", d$mrrlt_ne_arrlt_fl,
      "<br>AVAL:", d$.aval,
      "<br>ln(AVAL):", round(d$.log_aval, 4),
      "<br>ARRLT_DEV:", round(d$arrlt_dev, 3),
      "<br>MRRLT_DEV:", round(d$mrrlt_dev, 3),
      "<br>ARRLT_PCTDEV:", round(d$arrlt_pctdev, 1),
      "<br>Timing case:", d$timing_case,
      "<br>Review:", d$review_fl,
      ifelse(d$review_reason == "", "", paste0("<br>Reason: ", d$review_reason))
    )

    p <- plot_ly()

    if (isTRUE(input$profile_show_nominal_ref) && identical(input$profile_xaxis, "nrrlt")) {
      p <- p %>% add_segments(
        data = d,
        x = ~.nrrlt,
        xend = ~.nrrlt,
        y = 0,
        yend = ~plot_y,
        inherit = FALSE,
        line = list(dash = "dot", width = 1),
        name = "Nominal time guide",
        showlegend = TRUE,
        hoverinfo = "none"
      )
    }

    d$label_txt <- if (is.null(label_txt)) rep(NA_character_, nrow(d)) else label_txt
    d$hover_txt <- hover_txt

    p <- p %>%
      add_lines(
        data = d,
        x = ~plot_x,
        y = ~plot_y,
        name = paste(axis_var_label(profile_yaxis), "by", axis_var_label(input$profile_xaxis)),
        hoverinfo = "none"
      )

    add_case_trace <- function(p, dat, case_name, color, symbol, size) {
      dd <- dat %>% filter(timing_case == case_name)
      if (nrow(dd) == 0) return(p)
      p %>% add_markers(
        data = dd,
        x = ~plot_x,
        y = ~plot_y,
        marker = list(size = size, color = color, symbol = symbol, line = list(width = 1, color = color)),
        text = ~label_txt,
        textposition = "top center",
        hovertext = ~hover_txt,
        hoverinfo = "text",
        name = case_name,
        inherit = FALSE
      )
    }

    p <- add_case_trace(p, d, "Normal", "#4E79A7", "circle", 9)
    p <- add_case_trace(p, d, "Other flagged", "#d62728", "circle-open", 11)
    p <- add_case_trace(p, d, "PREdose taken POST-dose", "#ff7f0e", "triangle-up", 14)
    p <- add_case_trace(p, d, "POST-dose taken PREdose", "#9467bd", "x", 14)
    p <- add_case_trace(p, d, "PREdose suspiciously early", "#8c564b", "diamond", 13)
    p <- add_case_trace(p, d, "MRRLT differs from ARRLT", "#2ca02c", "square", 12)

    p %>% layout(
      title = paste("Subject profile:", ifelse(is.null(input$profile_subject) || input$profile_subject == "__ALL__", "All subjects", input$profile_subject), "|", ifelse(is.null(input$profile_pctest) || input$profile_pctest == "__ALL__", "All PCTEST", input$profile_pctest), "|", ifelse(is.null(input$profile_visit) || input$profile_visit == "__ALL__", "All visits", input$profile_visit)),
      xaxis = list(title = axis_var_label(input$profile_xaxis)),
      yaxis = list(title = axis_var_label(profile_yaxis))
    )
  })

  output$profile_table <- renderDT({
    req(!identical(rv$review$kind, "parameter"), rv$review)
    d <- profile_data() %>%
      transmute(
        Subject = .id,
        PCTEST = .pctest,
        AVISIT = .avisit,
        VISITCD = .visitcd,
        NRRLT = .nrrlt,
        ARRLT = .arrlt,
        MRRLT = .mrrlt,
        NRRLT_FL = nrrlt_fl,
        MRRLT_FL = mrrlt_fl,
        NRRLT_DUP = nrrlt_dup,
        MRRLT_DUP = mrrlt_dup,
        MRRLTneARRLT = mrrlt_ne_arrlt_fl,
        AVAL = .aval,
        ARRLT_DEV = arrlt_dev,
        MRRLT_DEV = mrrlt_dev,
        ARRLT_PCTDEV = round(arrlt_pctdev, 1),
        PREDOSE_POSTDOSE = predose_postdose_fl,
        POSTDOSE_PREDOSE = postdose_predose_fl,
        PREDOSE_EARLY = predose_early_fl,
        TIMING_CASE = timing_case,
        REVIEW = review_fl,
        REVIEW_REASON = review_reason,
        ADTM = .adtm,
        ARFTDTM = .arftdtm
      )

    review_datatable(d, filter = "top", options = list(scrollX = TRUE, pageLength = 15))
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, "facet_group" %in% input$outputs, identical(input$main_tabs, "Facet overview by Group"))
    ed <- plotly::event_data("plotly_click", source = "facet_group_src")
    req(!is.null(ed), nrow(ed) > 0)
    click_sig <- paste(capture.output(str(ed)), collapse = " ")
    if (identical(rv$last_facet_group_click, click_sig)) return(invisible(NULL))
    rv$last_facet_group_click <- click_sig

    clicked <- NULL
    if (!is.null(ed$key)) {
      clicked <- ed$key[[1]] %||% ed$key[1]
    }
    if (is.null(clicked) || length(clicked) == 0 || all(is.na(clicked)) || !nzchar(as.character(clicked)[1])) {
      return(invisible(NULL))
    }

    parts <- strsplit(as.character(clicked)[1], "\\|\\|")[[1]]
    req(length(parts) >= 5)

    subj_clicked <- parts[1]
    pctest_clicked <- parts[2]
    avisit_clicked <- parts[3]
    visitcd_clicked <- parts[4]

    visit_display_clicked <- if (is.na(visitcd_clicked) || visitcd_clicked == "") {
      avisit_clicked
    } else {
      paste0(avisit_clicked, " [", visitcd_clicked, "]")
    }

    rv$last_tab_before_profile <- "Facet overview by Group"
    updateSelectInput(session, "profile_pctest", selected = pctest_clicked)
    session$onFlushed(function() {
      updateSelectInput(session, "profile_visit", selected = visit_display_clicked)
      updateSelectInput(session, "profile_subject", selected = subj_clicked)
      updateTabsetPanel(session, "main_tabs", selected = "Subject profile")
    }, once = TRUE)
  })

  observe({
    req(!identical(rv$review$kind, "parameter"), rv$review, "focus" %in% input$outputs, identical(input$main_tabs, "Focused view"))
    ed <- plotly::event_data("plotly_click", source = "focus_src")
    req(!is.null(ed), nrow(ed) > 0)
    click_sig <- paste(capture.output(str(ed)), collapse = " ")
    if (identical(rv$last_focus_click, click_sig)) return(invisible(NULL))
    rv$last_focus_click <- click_sig
    clicked <- ed$key[[1]] %||% ed$key[1]
    parts <- strsplit(as.character(clicked), "\\|\\|")[[1]]
    req(length(parts) >= 5)

    subj_clicked <- parts[1]
    pctest_clicked <- parts[2]
    avisit_clicked <- parts[3]
    visitcd_clicked <- parts[4]

    visit_display_clicked <- if (is.na(visitcd_clicked) || visitcd_clicked == "") {
      avisit_clicked
    } else {
      paste0(avisit_clicked, " [", visitcd_clicked, "]")
    }

    rv$last_tab_before_profile <- "Focused view"
    updateSelectInput(session, "profile_pctest", selected = pctest_clicked)
    session$onFlushed(function() {
      updateSelectInput(session, "profile_visit", selected = visit_display_clicked)
      updateSelectInput(session, "profile_subject", selected = subj_clicked)
      updateTabsetPanel(session, "main_tabs", selected = "Subject profile")
    }, once = TRUE)
  })

  observeEvent(input$profile_back_btn, {
    target_tab <- rv$last_tab_before_profile %||% "Conc vs Time by Group"
    updateTabsetPanel(session, "main_tabs", selected = target_tab)
  })
  # Clicking in Facet overview is intentionally disabled; hover still shows details.

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("arrlt_review_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      req(!identical(rv$review$kind, "parameter"), rv$review)
      d <- rv$review$data

      listing_all <- d %>%
        transmute(
          Subject = .id,
          PCTEST = .pctest,
          AVISIT = .avisit,
          VISITCD = .visitcd,
          NRRLT = .nrrlt,
          ARRLT = .arrlt,
          MRRLT = .mrrlt,
          NRRLT_FL = nrrlt_fl,
          MRRLT_FL = mrrlt_fl,
          NRRLT_DUP = nrrlt_dup,
          MRRLT_DUP = mrrlt_dup,
          MRRLTneARRLT = mrrlt_ne_arrlt_fl,
          AVAL = .aval,
          ARRLT_DEV = arrlt_dev,
          ARRLT_PCTDEV = round(arrlt_pctdev, 1),
          LOWER_IQR = lower_iqr,
          UPPER_IQR = upper_iqr,
          N_GROUP = n_group,
          IQR_OUTLIER = arrlt_outlierfl,
          NEGATIVE_ARRLT = arrlt_negfl,
          PREDOSE_POSTDOSE = predose_postdose_fl,
          POSTDOSE_PREDOSE = postdose_predose_fl,
          PREDOSE_EARLY = predose_early_fl,
          TIMING_CASE = timing_case,
          BIG_PCTDEV = arrlt_bigdevfl,
          BIG_ABSDEV = arrlt_absdevfl,
          EARLY_POSITIVE = early_positivefl,
          REVIEW = review_fl,
          REVIEW_REASON = review_reason,
          ADTM = as.character(.adtm),
          ARFTDTM = as.character(.arftdtm)
        )

      listing_flagged <- listing_all %>% filter(REVIEW == "Y")
      subject_summary <- rv$review$subject_summary %>%
        transmute(
          Subject = .id,
          PCTEST = .pctest,
          AVISIT = .avisit,
          VISITCD = .visitcd,
          N_RECORDS = n_records,
          N_REVIEW = n_review,
          MAX_ABS_DEV = max_abs_dev,
          MAX_PCT_DEV = max_pct_dev
        )

      wb <- openxlsx::createWorkbook()

      openxlsx::addWorksheet(wb, "Flagged_Records")
      openxlsx::writeDataTable(wb, "Flagged_Records", listing_flagged, withFilter = TRUE)
      openxlsx::freezePane(wb, "Flagged_Records", firstRow = TRUE)

      openxlsx::addWorksheet(wb, "All_Records")
      openxlsx::writeDataTable(wb, "All_Records", listing_all, withFilter = TRUE)
      openxlsx::freezePane(wb, "All_Records", firstRow = TRUE)

      openxlsx::addWorksheet(wb, "Subject_Summary")
      openxlsx::writeDataTable(wb, "Subject_Summary", subject_summary, withFilter = TRUE)
      openxlsx::freezePane(wb, "Subject_Summary", firstRow = TRUE)

      if (nrow(listing_flagged) > 0) {
        style_flag <- openxlsx::createStyle(fgFill = "#FFF2CC")
        openxlsx::addStyle(
          wb,
          sheet = "Flagged_Records",
          style = style_flag,
          rows = 2:(nrow(listing_flagged) + 1),
          cols = 1:ncol(listing_flagged),
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      openxlsx::setColWidths(wb, "Flagged_Records", cols = 1:ncol(listing_flagged), widths = "auto")
      openxlsx::setColWidths(wb, "All_Records", cols = 1:ncol(listing_all), widths = "auto")
      openxlsx::setColWidths(wb, "Subject_Summary", cols = 1:ncol(subject_summary), widths = "auto")

      ts <- format(Sys.time(), "%Y%m%d_%H%M")
      out_file <- file
      if (!is.null(rv$source_path) && nzchar(rv$source_path)) {
        out_file <- file.path(dirname(rv$source_path), paste0("arrlt_review_", ts, ".xlsx"))
      }

      openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
      if (!identical(normalizePath(out_file, winslash = "/", mustWork = FALSE), normalizePath(file, winslash = "/", mustWork = FALSE))) {
        file.copy(out_file, file, overwrite = TRUE)
      }
      showNotification(paste("Excel saved to:", out_file), type = "message", duration = 6)
    }
  )

  output$nca_mapping_ui <- renderUI({
    req(rv$raw)
    nm <- names(rv$raw)
    pick <- function(candidates) {
      for (x in candidates) { hit <- nm[toupper(nm) == x]; if (length(hit)) return(hit[1]) }
      ""
    }
    tagList(
      fluidRow(
        column(4, selectInput("nca_profile", "Profile (APROFILE)", c("Select profile variable" = "", nm), selected = pick("APROFILE"))),
        column(4, selectInput("nca_subject", "Subject", c("Select subject variable" = "", nm), selected = pick(c("USUBJID", "SUBJID")))),
        column(4, selectInput("nca_analyte", "Analyte", c("Select analyte variable" = "", nm), selected = pick(c("PCTESTCD", "PARAMCD", "PCTEST"))))
      ),
      fluidRow(
        column(4, selectInput("nca_time", "NCA time — modified actual or user-selected nominal", nm,
                              selected = pick(c("MRRLT", "MFRLT", "ARRLT", "AFRLT", "NRRLT", "NFRLT")))),
        column(4, tags$p("Concentration: AVAL (supplied values, unchanged)")),
        column(4, selectInput("nca_keys", "Additional profile keys (period, visit, etc.)", nm, multiple = TRUE))
      )
    )
  })
  output$nca_filter_variable_ui <- renderUI({
    req(rv$raw)
    d <- rv$raw
    nm <- names(d)
    preferred <- nm[grepl("^(NCA[0-9]+FL|ANL[0-9]+FL|PKSUMXFL|APROFILE)$", nm, ignore.case = TRUE)]
    selectInput("nca_filter_variable", "Filter variable",
      choices = c("No filter — all records" = "", preferred, setdiff(nm, preferred)), selected = "")
  })
  output$nca_filter_rule_ui <- renderUI({
    req(rv$raw)
    d <- rv$raw
    variable <- input$nca_filter_variable %||% ""
    if (!nzchar(variable) || !variable %in% names(d)) return(NULL)
    vals <- trimws(as.character(d[[variable]]))
    vals <- sort(unique(vals[!is.na(vals) & nzchar(vals)]))
    tagList(
      radioButtons("nca_filter_mode", "Keep records where this variable is",
        choices = c("Equal to a value" = "equals", "Null / empty (including missing)" = "empty",
                    "Filled in (not missing or empty)" = "filled"),
        selected = if (toupper(variable) == "APROFILE") "filled" else "equals", inline = TRUE),
      conditionalPanel("input.nca_filter_mode === 'equals'",
        selectInput("nca_filter_value", "Value", choices = unique(c("Y", "N", vals)), selected = "Y"))
    )
  })
  nca_filtered_preview <- reactive({
    req(rv$raw)
    d <- rv$raw
    filter_nca_records_v11(d, input$nca_filter_variable %||% "",
      input$nca_filter_mode %||% "equals", input$nca_filter_value %||% "Y")
  })
  output$nca_filter_count <- renderText({
    req(rv$raw)
    paste(nrow(nca_filtered_preview()), "of", nrow(rv$raw),
          "loaded records selected for submission to PKNCA (independent of Review scope).")
  })
  # Results belong to a particular selection; clear them when the rule changes.
  observeEvent(list(input$nca_filter_variable, input$nca_filter_mode, input$nca_filter_value), {
    rv$nca <- NULL
  }, ignoreNULL = FALSE)
  observeEvent(input$nca_run, {
    req(rv$raw)
    if (!nzchar(input$nca_profile %||% "") || !nzchar(input$nca_subject %||% "") || !nzchar(input$nca_analyte %||% "")) {
      showNotification("Map APROFILE, subject and analyte before assessment.", type = "error"); return()
    }
    end <- suppressWarnings(as.numeric(input$nca_end))
    if (length(end) != 1 || is.na(end) || !is.finite(input$nca_start) || end <= input$nca_start) {
      showNotification("Enter an interval end greater than start (Inf is allowed).", type = "error"); return()
    }
    config <- list(profile = input$nca_profile, subject = input$nca_subject,
      analyte = input$nca_analyte, extra_keys = input$nca_keys,
      time = input$nca_time, concentration = names(rv$raw)[toupper(names(rv$raw)) == "AVAL"][1],
      start = input$nca_start, end = end, auc_method = input$nca_auc,
      source = rv$source_path, conc_na = "drop", zero_handling = "keep all", application = "adpc_review",
      filter_variable = input$nca_filter_variable %||% "",
      filter_mode = input$nca_filter_mode %||% "equals", filter_value = input$nca_filter_value %||% "Y")
    if (is.na(config$concentration)) {
      showNotification("AVAL is required for NCA assessment.", type = "error"); return()
    }
    # Apply only the NCA-tab selection to the loaded dataset; do not inherit Review or PK filters.
    adpc <- rv$raw; adpc$.NCA_SOURCE_ROW <- seq_len(nrow(adpc))
    rv$nca <- NULL
    result <- tryCatch(withProgress(message = "Running PKNCA by APROFILE", value = 0, {
      run_nca_feasibility_v11(adpc, config, function(value, detail) setProgress(value = value, detail = detail))
    }), error = function(e) e)
    if (inherits(result, "error")) showNotification(conditionMessage(result), type = "error", duration = NULL) else rv$nca <- result
  })
  output$nca_status <- renderText({
    if (is.null(rv$nca)) return("Assessment not run for the current dataset.")
    counts <- table(rv$nca$summary$STATUS)
    paste("Time:", rv$nca$config$time, "| Concentration:", rv$nca$config$concentration,
      "| Records selected:", rv$nca$config$n_selected_records, "of", rv$nca$config$n_loaded_records,
      "| Profiles:", nrow(rv$nca$summary), "\n", paste(names(counts), counts, collapse = " | "))
  })
  nca_table <- function(x, selection = "none") review_datatable(x, rownames = FALSE, filter = "top", selection = selection,
    options = list(scrollX = TRUE, pageLength = 10))
  output$nca_summary <- renderDT({ req(rv$nca); nca_table(rv$nca$summary, "single") })
  output$nca_cases <- renderDT({ req(rv$nca); nca_table(rv$nca$cases) })
  output$nca_parameters <- renderDT({ req(rv$nca); nca_table(rv$nca$results[!is.na(rv$nca$results$ASSESSMENT), , drop = FALSE]) })
  output$nca_diagnostics <- renderDT({ req(rv$nca); nca_table(rv$nca$results[is.na(rv$nca$results$ASSESSMENT), , drop = FALSE]) })
  nca_selected <- reactive({
    req(rv$nca)
    row <- input$nca_summary_rows_selected
    if (!length(row) || row > nrow(rv$nca$summary)) return(NULL)
    rv$nca$input[rv$nca$input$RUN_PROFILE == rv$nca$summary$RUN_PROFILE[row], , drop = FALSE]
  })
  output$nca_profile_records <- renderDT({ d <- nca_selected(); req(d); nca_table(d) })
  output$nca_profile_plot <- renderPlotly({
    d <- nca_selected(); req(d)
    d <- d[order(d[[rv$nca$config$time]]), , drop = FALSE]
    plot_ly(d, x = d[[rv$nca$config$time]], y = d[[rv$nca$config$concentration]],
      type = "scatter", mode = "lines+markers", text = paste("Source row", d$.NCA_SOURCE_ROW), connectgaps = FALSE) %>%
      layout(xaxis = list(title = rv$nca$config$time), yaxis = list(title = rv$nca$config$concentration, type = "linear"))
  })
  output$nca_export <- downloadHandler(
    filename = function() paste0("PKNCA_Feasibility_v12_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      req(rv$nca)
      export_nca_feasibility_v11(rv$nca, file)
    }
  )

  # ===========================================================================
  # PK VARIABLE MAPPING UIs
  # ===========================================================================
  output$pk_var_map_ui <- renderUI({
    req(rv$raw)
    nm <- names(rv$raw)
    dp <- function(candidates) pick_dataset_variable_v13(nm, candidates)
    tagList(
      tags$p(class = "help-note",
             "Subject, analyte, visit, nominal time, relative time and concentration use the shared mappings in Section 2."),
      selectInput("v_lloq",    "LLOQ (optional)",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("PCLLOQ", "LLOQ"))),
      selectInput("v_sex",     "SEX (optional)",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("SEX", "SEXCD")))
    )
  })

  output$pk_group_map_ui <- renderUI({
    req(rv$raw)
    nm <- names(rv$raw)
    dp <- function(candidates) pick_dataset_variable_v13(nm, candidates)
    tagList(
      selectInput("g_route",  "Route of Administration",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("ECROUTE", "ROUTE", "EXTRT"))),
      selectInput("g_atrt",   "Treatment (ATRT)",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("ATRT", "TRT01A", "TRTGRP"))),
      selectInput("g_arm",    "ARM (ARMCD)",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("ARMCD", "ARM", "ACTARM"))),
      selectInput("g_cohort", "Cohort",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("COHORT", "COHORTCD", "COHORTC"))),
      selectInput("g_agegrp", "Age Group (AGEGRP)",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("AGEGRP", "AGEGR1", "AGECAT"))),
      selectInput("g_bsacat", "BSA Category (BSACAT)",
                  choices = c("(none)" = "", nm),
                  selected = dp(c("BSACAT", "BSAGR1", "BSAGRP")))
    )
  })

  # ===========================================================================
  # PREPARE / UPDATE PK PROFILES
  # ===========================================================================
  record_filter_description <- function(variable, mode, value) {
    if (is.null(variable) || !nzchar(variable)) return("No filter")
    switch(
      mode,
      equals = paste0(variable, " == ", value),
      empty = paste0(variable, " is NULL/empty"),
      filled = paste0(variable, " is filled"),
      paste0(variable, " (unknown rule)")
    )
  }

  register_pk_filter <- function(prefix) {
    variable_id <- paste0(prefix, "_filter_variable")
    mode_id <- paste0(prefix, "_filter_mode")
    value_id <- paste0(prefix, "_filter_value")
    output[[paste0(prefix, "_filter_variable_ui")]] <- renderUI({
      req(rv$raw)
      nm <- names(rv$raw)
      preferred <- nm[grepl(
        "^(NCA[0-9]+FL|ANL[0-9]+FL|PKSUMXFL|APROFILE)$",
        nm, ignore.case = TRUE
      )]
      selected_variable <- if (nzchar(rv$pk_filter_variable %||% "") &&
                               rv$pk_filter_variable %in% nm) {
        rv$pk_filter_variable
      } else {
        isolate(input[[variable_id]] %||% "")
      }
      selectInput(
        variable_id, "Filter variable",
        choices = c("No filter — all loaded records" = "", preferred, setdiff(nm, preferred)),
        selected = selected_variable
      )
    })
    output[[paste0(prefix, "_filter_rule_ui")]] <- renderUI({
      req(rv$raw)
      variable <- input[[variable_id]] %||% ""
      if (!nzchar(variable) || !variable %in% names(rv$raw)) return(NULL)
      values <- trimws(as.character(rv$raw[[variable]]))
      values <- sort(unique(values[!is.na(values) & nzchar(values)]))
      default_mode <- if (toupper(variable) == "APROFILE") "filled" else "equals"
      selected_mode <- if (identical(variable, rv$pk_filter_variable)) {
        rv$pk_filter_mode %||% default_mode
      } else {
        default_mode
      }
      value_choices <- unique(c("Y", "N", values))
      selected_value <- if (identical(variable, rv$pk_filter_variable) &&
                            rv$pk_filter_value %in% value_choices) {
        rv$pk_filter_value
      } else {
        "Y"
      }
      tagList(
        radioButtons(
          mode_id, "Keep records where this variable is",
          choices = c(
            "Equal to a value" = "equals",
            "Null / empty (including missing)" = "empty",
            "Filled in (not missing or empty)" = "filled"
          ),
          selected = selected_mode,
          inline = TRUE
        ),
        conditionalPanel(
          sprintf("input.%s === 'equals'", mode_id),
          selectInput(value_id, "Value", choices = value_choices, selected = selected_value)
        )
      )
    })
    filtered <- reactive({
      req(rv$raw)
      filter_nca_records_v11(
        rv$raw,
        input[[variable_id]] %||% "",
        input[[mode_id]] %||% "equals",
        input[[value_id]] %||% "Y"
      )
    })
    output[[paste0(prefix, "_filter_count")]] <- renderText({
      req(rv$raw)
      paste(
        nrow(filtered()), "of", nrow(rv$raw),
        "loaded records will be used when PK Profiles are prepared."
      )
    })
    list(
      data = filtered,
      variable = reactive(input[[variable_id]] %||% ""),
      mode = reactive(input[[mode_id]] %||% "equals"),
      value = reactive(input[[value_id]] %||% "Y")
    )
  }

  pk_filters <- list(
    ind = register_pk_filter("pk_ind"),
    grp = register_pk_filter("pk_grp"),
    sas = register_pk_filter("pk_sas")
  )

  prepare_pk_profiles <- function(filter_key) {
    req(rv$raw, input$id_var, input$analyte_var, input$visit_var,
        input$nominal_var, input$mrrlt_var, input$aval_var)
    filter_variable <- pk_filters[[filter_key]]$variable()
    filter_mode <- pk_filters[[filter_key]]$mode()
    filter_value <- pk_filters[[filter_key]]$value()
    filter_description <- record_filter_description(
      filter_variable, filter_mode, filter_value
    )
    selected_data <- pk_filters[[filter_key]]$data()
    if (nrow(selected_data) == 0) {
      showNotification("The selected PK filter returned 0 records.", type = "error", duration = 7)
      return()
    }
    pd <- tryCatch(
      prepare_pk_data(
        df          = selected_data,
        id_var      = input$id_var,
        analyte_var = input$analyte_var,
        visit_var   = input$visit_var,
        visitcd_var = safe_col(input$visitcd_var),
        nominal_var = input$nominal_var,
        mrrlt_var   = input$mrrlt_var,
        aval_var    = input$aval_var,
        lloq_var    = safe_col(input$v_lloq),
        route_var   = safe_col(input$g_route),
        atrt_var    = safe_col(input$g_atrt),
        arm_var     = safe_col(input$g_arm),
        cohort_var  = safe_col(input$g_cohort),
        agegrp_var  = safe_col(input$g_agegrp),
        bsacat_var  = safe_col(input$g_bsacat),
        sex_var     = safe_col(input$v_sex),
        filter_var  = NULL,
        filter_val  = "Y",
        blq_half    = FALSE
      ),
      error = function(e) e
    )
    if (inherits(pd, "error")) {
      showNotification(pd$message, type = "error", duration = 8)
      return()
    }
    if (nrow(pd) == 0) {
      showNotification("No PK data after filtering. Check variable mapping and filter.",
                       type = "warning", duration = 7)
      return()
    }
    rv$pk_data <- pd
    rv$pk_data_stale <- FALSE
    rv$pk_filter_description <- filter_description
    rv$pk_filter_variable <- filter_variable
    rv$pk_filter_mode <- filter_mode
    rv$pk_filter_value <- filter_value
    rv$pk_filter_selected_n <- nrow(selected_data)
    rv$pk_filter_loaded_n <- nrow(rv$raw)
    analytes <- sort(unique(pd$.analyte))
    updateSelectInput(session, "ind_analyte",  choices = analytes, selected = analytes[1])
    updateSelectInput(session, "grp_analyte",  choices = analytes, selected = analytes[1])
    visits <- sort(unique(pd$.visit_display))
    updateSelectInput(session, "grp_visit", choices = visits, selected = visits[1])
    trts <- sort(unique(pd$.atrt[!is.na(pd$.atrt)]))
    if (length(trts) == 0) trts <- "(none mapped)"
    updateSelectInput(session, "grp_treatment", choices = trts, selected = trts[1])
    showNotification(
      sprintf("PK data ready: %d rows | %d subjects | %d analytes | Filter: %s",
              nrow(pd), length(unique(pd$.id)), length(unique(pd$.analyte)),
              rv$pk_filter_description),
      type = "message", duration = 4)
  }

  observeEvent(input$pk_prepare_ind, prepare_pk_profiles("ind"), ignoreInit = TRUE)
  observeEvent(input$pk_prepare_grp, prepare_pk_profiles("grp"), ignoreInit = TRUE)
  observeEvent(input$pk_prepare_sas, prepare_pk_profiles("sas"), ignoreInit = TRUE)

  observeEvent(list(
    input$id_var, input$analyte_var, input$visit_var, input$visitcd_var,
    input$nominal_var, input$mrrlt_var, input$aval_var,
    input$v_lloq, input$v_sex, input$g_route, input$g_atrt, input$g_arm,
    input$g_cohort, input$g_agegrp, input$g_bsacat
  ), {
    if (!is.null(rv$pk_data)) rv$pk_data_stale <- TRUE
  }, ignoreInit = TRUE)

  pk_profile_status <- reactive({
    if (is.null(rv$raw)) return("Load a dataset to prepare PK profiles.")
    if (is.null(rv$pk_data)) return("PK profiles not prepared.")
    if (isTRUE(rv$pk_data_stale)) return("Mappings changed — update PK profiles before using these plots.")
    sprintf("PK profiles ready: %s records | %s subjects | %s analytes | Filter: %s (%s of %s loaded records)",
      format(nrow(rv$pk_data), big.mark = ","),
      format(length(unique(rv$pk_data$.id)), big.mark = ","),
      format(length(unique(rv$pk_data$.analyte)), big.mark = ","),
      rv$pk_filter_description,
      format(rv$pk_filter_selected_n, big.mark = ","),
      format(rv$pk_filter_loaded_n, big.mark = ","))
  })
  output$pk_status_ind <- renderText(pk_profile_status())
  output$pk_status_grp <- renderText(pk_profile_status())
  output$pk_status_sas <- renderText(pk_profile_status())

  # ===========================================================================
  # TAB: Individual Subject Profiles
  # ===========================================================================
  observeEvent(list(rv$pk_data, input$ind_analyte), {
    req(rv$pk_data, input$ind_analyte)
    subjs <- sort(unique(rv$pk_data$.id[rv$pk_data$.analyte == input$ind_analyte]))
    updateSelectInput(session, "ind_subjects", choices = subjs, selected = subjs)
  })
  observeEvent(input$ind_all, {
    req(rv$pk_data, input$ind_analyte)
    subjs <- sort(unique(rv$pk_data$.id[rv$pk_data$.analyte == input$ind_analyte]))
    updateSelectInput(session, "ind_subjects", selected = subjs)
  })
  observeEvent(input$ind_clear, {
    updateSelectInput(session, "ind_subjects", selected = character(0))
  })
  output$confound_banner <- renderUI({
    req(rv$pk_data)
    if (identical(input$ind_colour_by, "atrt") && check_confounding(rv$pk_data)) {
      div(style = "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;padding:8px 14px;margin-bottom:10px;color:#856404;font-weight:600;"
,
          "WARNING: ATRT and AVISIT are confounded -- each AVISIT has exactly one Treatment.")
    } else {
      NULL
    }
  })
  output$ind_count <- renderText({
    sprintf("%d subject(s) selected  |  Analyte: %s",
            length(input$ind_subjects), input$ind_analyte %||% "")
  })

  ind_time_scope_data <- reactive({
    if (is.null(rv$pk_data) || is.null(input$ind_analyte)) return(NULL)
    pd <- rv$pk_data[rv$pk_data$.analyte == input$ind_analyte, , drop = FALSE]
    subjects <- input$ind_subjects %||% character()
    if (length(subjects)) pd <- pd[pd$.id %in% subjects, , drop = FALSE]
    pd
  })

  ind_time_limits <- reactive({
    pd <- ind_time_scope_data()
    if (is.null(pd) || nrow(pd) == 0 || is.null(input$pk_xvar_sel)) return(numeric())
    x_values <- profile_x_values(pd, input$pk_xvar_sel)
    x_values <- x_values[is.finite(x_values)]
    if (!length(x_values)) numeric() else range(x_values)
  })

  output$ind_time_window_ui <- renderUI({
    limits <- ind_time_limits()
    if (!length(limits)) {
      return(tags$p(
        style = "color:#888; margin-top:7px;",
        "No finite time values are available for the selected analyte and subjects."
      ))
    }
    if (limits[1] == limits[2]) {
      return(tags$p(
        style = "color:#666; margin-top:7px;",
        paste0("Only one time value is available: ", signif(limits[1], 7), ".")
      ))
    }
    current <- resolve_profile_time_window(isolate(input$ind_time_window), limits)
    if (is.null(current)) current <- limits
    step <- diff(limits) / 200
    if (!is.finite(step) || step <= 0) step <- 1
    tagList(
      sliderInput(
        "ind_time_window", "Start - end time",
        min = limits[1], max = limits[2], value = current,
        step = signif(step, 4), sep = ""
      ),
      helpText(
        "The selected window is applied to every subject profile and remains active while paging. Use the Plotly toolbar for temporary visual zoom."
      )
    )
  })

  ind_plot_data <- reactive({
    pd <- ind_time_scope_data()
    if (is.null(pd) || nrow(pd) == 0) return(pd)
    if (!isTRUE(input$ind_use_time_window)) return(pd)
    time_window <- resolve_profile_time_window(input$ind_time_window, ind_time_limits())
    if (is.null(time_window)) return(pd)
    x_values <- profile_x_values(pd, input$pk_xvar_sel)
    keep <- is.finite(x_values) &
      x_values >= time_window[1] & x_values <= time_window[2]
    pd[keep, , drop = FALSE]
  })

  output$ind_plots_ui <- renderUI({
    req(rv$pk_data, input$ind_subjects, input$ind_analyte, input$ind_colour_by)
    pd <- ind_plot_data()
    if (is.null(pd) || nrow(pd) == 0) {
      return(tags$p("No data for the selected analyte, subjects and time range."))
    }
    col_field <- switch(input$ind_colour_by,
      route  = ".route",  atrt   = ".atrt",  arm    = ".arm",
      cohort = ".cohort", agegrp = ".agegrp", bsacat = ".bsacat",
      ".route"
    )
    col_label <- switch(input$ind_colour_by,
      route  = "Route",  atrt   = "Treatment", arm    = "ARM",
      cohort = "Cohort", agegrp = "Age Group",  bsacat = "BSA Cat",
      "Group"
    )
    colour_map <- make_colour_map(as.character(pd[[col_field]]))
    xvar       <- input$pk_xvar_sel
    yscale     <- input$pk_yscale_sel
    xlab       <- paste0(if (xvar == "nrrlt") input$nominal_var else input$mrrlt_var, " (h)")
    ylab       <- paste0(input$ind_analyte, " Concentration")
    subjs      <- ind_page()$values
    withProgress(message = "Building plots...", value = 0, {
      n         <- length(subjs)
      tags_list <- vector("list", n)
      for (i in seq_len(n)) {
        incProgress(1 / n, detail = subjs[i])
        s_pd <- pd[pd$.id == subjs[i], , drop = FALSE]
        if (nrow(s_pd) == 0) next
        r         <- s_pd[1, ]
        sub_parts <- character(0)
        if (!is.na(r$.cohort) && nzchar(r$.cohort))
          sub_parts <- c(sub_parts, paste0("Cohort: ", r$.cohort))
        if (!is.na(r$.atrt) && nzchar(r$.atrt))
          sub_parts <- c(sub_parts, paste0("Trt: ", r$.atrt))
        if (!is.na(r$.bsacat) && nzchar(r$.bsacat))
          sub_parts <- c(sub_parts, paste0("BSA: ", r$.bsacat))
        subtitle <- paste(sub_parts, collapse = "   |   ")
        g <- make_spaghetti(
          pd           = s_pd,
          xvar         = xvar,      yscale       = yscale,
          colour_var   = col_field, colour_map   = colour_map,
          title        = subjs[i],  subtitle     = subtitle,
          xlab         = xlab,      ylab         = ylab,
          legend_title = col_label
        )
        oid <- paste0("ind_plt_", gsub("[^A-Za-z0-9]", "_", subjs[i]))
        local({
          oid_ <- oid;  g_ <- g
          output[[oid_]] <- renderPlotly(as_plotly(g_))
        })
        tags_list[[i]] <- div(style = "background:#fff;border:1px solid #dde3ec;border-radius:8px;margin-bottom:14px;padding:8px;box-shadow:0 1px 4px rgba(0,0,0,.05);"
, plotlyOutput(oid, height = "460px"))
      }
      div(class = if (identical(input$ind_page_size, "1")) "profile-grid single" else "profile-grid",
          Filter(Negate(is.null), tags_list))
    })
  })

  # ===========================================================================
  # TAB: Profiles by Groups
  # ===========================================================================
  output$grp_confound_banner <- renderUI({
    req(rv$pk_data)
    if (input$grp_mode %in% c("mode2","mode3","mode4","mode5") &&
        check_confounding(rv$pk_data)) {
      div(style = "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;padding:8px 14px;margin-bottom:10px;color:#856404;font-weight:600;"
,
          "WARNING: ATRT and AVISIT are confounded. Treatment-based grouping may not be meaningful.")
    } else {
      NULL
    }
  })
  output$grp_plot_ui <- renderUI({
    req(rv$pk_data, input$grp_analyte, input$grp_visit, input$grp_mode)
    pd <- rv$pk_data
    pd <- pd[pd$.analyte == input$grp_analyte &
             pd$.visit_display == input$grp_visit, , drop = FALSE]
    if (nrow(pd) == 0) return(tags$p("No data for selected analyte / visit."))
    xvar   <- input$pk_xvar_sel
    yscale <- input$pk_yscale_sel
    xlab   <- paste0(if (xvar == "nrrlt") input$nominal_var else input$mrrlt_var, " (h)")
    ylab   <- paste0(input$grp_analyte, " Concentration")
    mode   <- input$grp_mode
    if (mode == "mode1") {
      g <- make_spaghetti(pd, xvar = xvar, yscale = yscale,
        colour_var = ".id", colour_map = make_colour_map(pd$.id),
        title    = paste0(input$grp_analyte, "  --  ", input$grp_visit),
        subtitle = paste0("All subjects (N=", length(unique(pd$.id)), ")"),
        xlab = xlab, ylab = ylab, legend_title = "Subject")
      output$grp_main_plot <- renderPlotly(as_plotly(g, h = 560))
      return(div(style = "background:#fff;border:1px solid #dde3ec;border-radius:8px;margin-bottom:14px;padding:8px;"
, plotlyOutput("grp_main_plot", height = "560px")))
    }
    trt    <- input$grp_treatment %||% ""
    pd_trt <- pd[!is.na(pd$.atrt) & pd$.atrt == trt, , drop = FALSE]
    if (nrow(pd_trt) == 0)
      return(tags$p(sprintf("No data for Treatment '%s' at visit '%s'.", trt, input$grp_visit)))
    if (mode == "mode2") {
      g <- make_spaghetti(pd_trt, xvar = xvar, yscale = yscale,
        colour_var = ".id", colour_map = make_colour_map(pd_trt$.id),
        title    = paste0(input$grp_analyte, " | Trt: ", trt, " | ", input$grp_visit),
        subtitle = paste0("N subjects = ", length(unique(pd_trt$.id))),
        xlab = xlab, ylab = ylab, legend_title = "Subject")
      output$grp_main_plot <- renderPlotly(as_plotly(g, h = 560))
      return(div(style = "background:#fff;border:1px solid #dde3ec;border-radius:8px;margin-bottom:14px;padding:8px;"
, plotlyOutput("grp_main_plot", height = "560px")))
    }
    panels_by <- function(cat_col, cat_label) {
      cats <- sort(unique(pd_trt[[cat_col]]))
      cats <- cats[!is.na(cats) & nzchar(cats)]
      if (length(cats) == 0)
        return(tags$p(paste(cat_label, "not mapped or no data.")))
      plist <- lapply(cats, function(cv) {
        d <- pd_trt[pd_trt[[cat_col]] == cv, , drop = FALSE]
        if (nrow(d) == 0) return(NULL)
        g <- make_spaghetti(d, xvar = xvar, yscale = yscale,
          colour_var = ".id", colour_map = make_colour_map(d$.id),
          title    = paste0(trt, " | ", cat_label, ": ", cv, " | ", input$grp_visit),
          subtitle = paste0("N = ", length(unique(d$.id))),
          xlab = xlab, ylab = ylab, legend_title = "Subject")
        oid <- paste0("grp_panel_", gsub("[^A-Za-z0-9]", "_", paste0(mode, "_", cv)))
        local({
          oid_ <- oid;  g_ <- g
          output[[oid_]] <- renderPlotly(as_plotly(g_, h = 500))
        })
        div(style = "background:#fff;border:1px solid #dde3ec;border-radius:8px;margin-bottom:14px;padding:8px;"
, plotlyOutput(oid, height = "500px"))
      })
      tagList(Filter(Negate(is.null), plist))
    }
    if (mode == "mode3") return(panels_by(".sex",    "SEX"))
    if (mode == "mode4") return(panels_by(".bsacat", "BSA Category"))
    if (mode == "mode5") return(panels_by(".agegrp", "Age Group"))
  })

  # ===========================================================================
  # TAB: SAS-style Spaghetti Plots
  # ===========================================================================
  sas_control_data <- reactive({
    if (!is.null(rv$pk_data) && !isTRUE(rv$pk_data_stale)) return(rv$pk_data)
    req(rv$raw, input$analyte_var, input$visit_var)
    raw <- rv$raw
    get_character <- function(variable) {
      if (!is.null(variable) && nzchar(variable %||% "") && variable %in% names(raw)) {
        as.character(raw[[variable]])
      } else {
        rep(NA_character_, nrow(raw))
      }
    }
    preview <- data.frame(
      .id = get_character(input$id_var),
      .analyte = get_character(input$analyte_var),
      .avisit = get_character(input$visit_var),
      .visitcd = get_character(input$visitcd_var),
      stringsAsFactors = FALSE
    )
    preview$.visit_display <- ifelse(
      is.na(preview$.visitcd) | preview$.visitcd == "",
      preview$.avisit,
      paste0(preview$.avisit, " [", preview$.visitcd, "]")
    )
    preview[
      !is.na(preview$.analyte) & nzchar(preview$.analyte) &
      !is.na(preview$.avisit) & nzchar(preview$.avisit),
      , drop = FALSE
    ]
  })
  sas_visit_col <- reactive({
    pd <- sas_control_data()
    if (identical(input$sas_domain, "adpc")) {
      if (all(is.na(pd$.visitcd) | pd$.visitcd == "")) ".visit_display" else ".visitcd"
    } else {
      ".avisit"
    }
  })
  sas_choices <- reactive({
    pd <- sas_control_data()
    visit_col <- sas_visit_col()
    list(
      subjects = sort(unique(pd$.id[!is.na(pd$.id)])),
      visits   = stringr::str_sort(
        unique(as.character(pd[[visit_col]][!is.na(pd[[visit_col]])])),
        numeric = TRUE
      ),
      analytes = sort(unique(pd$.analyte[!is.na(pd$.analyte)]))
    )
  })
  output$sas_subject_ui <- renderUI({
    ch <- sas_choices()
    selectInput("sas_subject", "Subject", choices = ch$subjects, selected = ch$subjects[1])
  })
  output$sas_visit_ui <- renderUI({
    ch <- sas_choices()
    selectInput("sas_visit", "Visit", choices = ch$visits, selected = ch$visits[1])
  })
  output$sas_analyte_ui <- renderUI({
    ch <- sas_choices()
    selectInput("sas_analyte", "Analyte", choices = ch$analytes, selected = ch$analytes[1])
  })
  output$sas_combined_visits_ui <- renderUI({
    visits <- sas_choices()$visits
    selected <- intersect(isolate(input$sas_combined_visits %||% character()), visits)
    if (length(selected) == 0) selected <- utils::head(visits, 2L)
    tagList(
      shinyWidgets::pickerInput(
        "sas_combined_visits", "Visits to combine",
        choices = visits, selected = selected, multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          selectedTextFormat = "count > 3", size = 10
        )
      ),
      radioButtons(
        "sas_combined_spacing", "Combined X-axis spacing",
        choices = c(
          "Readable profiles (equal timepoint spacing)" = "equal",
          "Proportional numeric time" = "numeric"
        ),
        selected = isolate(input$sas_combined_spacing %||% "equal"),
        inline = TRUE
      ),
      helpText(
        "Equal spacing makes packed timepoints visible while retaining their true labels. Proportional spacing preserves numeric distances; mapped relative time then remains continuous, while nominal time and PCTPTNUM restart within each visit."
      )
    )
  })
  output$sas_combined_axis_diagnostic <- renderUI({
    req(rv$raw, input$sas_xvar, input$sas_mode)
    if (is.null(rv$pk_data) || isTRUE(rv$pk_data_stale)) {
      return(div(
        style = "background:#e7f3ff;border:1px solid #9cc8ee;border-radius:5px;padding:8px 10px;margin-bottom:10px;color:#174a73;",
        "Visit choices are shown from the loaded dataset. Click Prepare / Update PK Profiles to apply the PK filter and enable plotting."
      ))
    }
    visits <- input$sas_combined_visits %||% character()
    if (!length(visits)) return(NULL)
    pd <- rv$pk_data
    visit_col <- sas_visit_col()
    pd <- pd[pd[[visit_col]] %in% visits, , drop = FALSE]
    if (identical(input$sas_mode, "6") && !is.null(input$sas_analyte)) {
      pd <- pd[pd$.analyte == input$sas_analyte, , drop = FALSE]
    }
    quality <- profile_x_group_quality(pd, visit_col, input$sas_xvar)
    counts <- quality$counts
    alternative_vars <- setdiff(c("mrrlt", "nrrlt", "pctptnum"), input$sas_xvar)
    alternative_quality <- lapply(alternative_vars, function(x) {
      profile_x_group_quality(pd, visit_col, x)
    })
    names(alternative_quality) <- alternative_vars
    alternative_scores <- vapply(alternative_quality, function(x) x$curves, integer(1))
    best_var <- if (length(alternative_scores)) {
      alternative_vars[which.max(alternative_scores)]
    } else {
      character()
    }
    best_quality <- if (length(best_var)) alternative_quality[[best_var]] else NULL
    recommendation <- if (!is.null(best_quality) && best_quality$curves > quality$curves) {
      paste0(
        " For these selections, ",
        profile_x_axis_label(best_var, input$nominal_var, input$mrrlt_var),
        " has multiple values in ", best_quality$curves, " of ", best_quality$total,
        " visit/analyte combination(s) and is the better X-axis choice."
      )
    } else {
      " No alternate mapped X-axis has more usable within-visit timepoints for these selections."
    }
    if (!length(counts)) {
      return(div(
        style = "background:#f8d7da;border:1px solid #e5aeb4;border-radius:5px;padding:8px 10px;margin-bottom:10px;color:#842029;",
        paste0("The selected X-axis variable has no finite values for these visits.", recommendation)
      ))
    }
    limited <- names(counts)[counts < 2L]
    if (!length(limited)) return(NULL)
    examples <- paste(utils::head(limited, 3L), collapse = "; ")
    axis_name <- profile_x_axis_label(input$sas_xvar, input$nominal_var, input$mrrlt_var)
    div(
      style = "background:#fff3cd;border:1px solid #e5c35b;border-radius:5px;padding:8px 10px;margin-bottom:10px;color:#664d03;",
      tags$strong("X-axis check: "),
      paste0(
        axis_name, " has only one unique value in ", length(limited),
        " selected visit/analyte combination(s), for example ", examples,
        ". A concentration-time curve cannot be formed there.", recommendation
      )
    )
  })

  sas_time_scope_data <- reactive({
    req(rv$raw, input$sas_xvar, input$sas_mode)
    if (is.null(rv$pk_data) || isTRUE(rv$pk_data_stale)) return(NULL)
    pd <- rv$pk_data
    mode <- input$sas_mode
    visit_col <- sas_visit_col()
    if (mode %in% c("1", "2", "3") && !is.null(input$sas_subject)) {
      pd <- pd[pd$.id == input$sas_subject, , drop = FALSE]
    }
    if (mode %in% c("1", "6") && !is.null(input$sas_analyte)) {
      pd <- pd[pd$.analyte == input$sas_analyte, , drop = FALSE]
    }
    if (mode %in% c("1", "2") && !is.null(input$sas_visit)) {
      pd <- pd[pd[[visit_col]] == input$sas_visit, , drop = FALSE]
    }
    if (mode %in% c("5", "6") && isTRUE(input$sas_combine_visits) &&
        length(input$sas_combined_visits)) {
      pd <- pd[pd[[visit_col]] %in% input$sas_combined_visits, , drop = FALSE]
    }
    pd
  })

  sas_time_limits <- reactive({
    pd <- sas_time_scope_data()
    if (is.null(pd) || nrow(pd) == 0) return(numeric())
    x_values <- profile_x_values(pd, input$sas_xvar)
    x_values <- x_values[is.finite(x_values)]
    if (!length(x_values)) numeric() else range(x_values)
  })

  output$sas_time_window_ui <- renderUI({
    req(rv$raw, input$sas_xvar, input$sas_mode)
    if (is.null(rv$pk_data) || isTRUE(rv$pk_data_stale)) {
      return(tags$p(
        style = "color:#666; margin-top:7px;",
        "Prepare / Update PK Profiles to calculate the available time range."
      ))
    }
    pd <- sas_time_scope_data()
    mode <- input$sas_mode
    limits <- sas_time_limits()
    if (length(limits) == 0) {
      return(tags$p(style = "color:#888; margin-top:7px;",
                    "No finite values are available for the selected X-axis variable."))
    }
    if (limits[1] == limits[2]) {
      return(tags$p(style = "color:#666; margin-top:7px;",
                    paste0("Only one time value is available: ", signif(limits[1], 7), ".")))
    }
    current <- isolate(input$sas_time_window)
    if (length(current) != 2L || any(!is.finite(current))) current <- limits
    current <- sort(as.numeric(current))
    if (current[2] < limits[1] || current[1] > limits[2]) {
      current <- limits
    } else {
      current <- pmax(limits[1], pmin(limits[2], current))
    }
    step <- diff(limits) / 200
    if (!is.finite(step) || step <= 0) step <- 1
    tagList(
      sliderInput(
        "sas_time_window", "Start - end time",
        min = limits[1], max = limits[2], value = current,
        step = signif(step, 4), sep = ""
      ),
      helpText(if (mode %in% c("5", "6") && isTRUE(input$sas_combine_visits) &&
                    identical(input$sas_combined_spacing %||% "equal", "equal")) {
        "The range is applied to the true time values before equal plot spacing. Use the Plotly toolbar for temporary visual zoom."
      } else if (mode %in% c("5", "6") && isTRUE(input$sas_combine_visits) &&
                    input$sas_xvar != "mrrlt") {
        "For a nominal axis, this range is applied within every selected visit. Use the Plotly toolbar for temporary visual zoom."
      } else {
        "The range is applied before plotting. Use the Plotly toolbar for temporary visual zoom."
      })
    )
  })
  observeEvent(input$sas_run, {
    if (is.null(rv$pk_data) || isTRUE(rv$pk_data_stale)) {
      showNotification(
        "Click Prepare / Update PK Profiles before generating plots.",
        type = "warning", duration = 7
      )
      return()
    }
    pd        <- rv$pk_data
    mode      <- input$sas_mode
    xvar      <- input$sas_xvar
    yscale    <- input$sas_yscale
    visit_col <- sas_visit_col()
    combine_visits <- mode %in% c("5", "6") && isTRUE(input$sas_combine_visits)
    combined_spacing <- input$sas_combined_spacing %||% "equal"
    selected_visits <- if (combine_visits) {
      available_visits <- sas_choices()$visits
      available_visits[available_visits %in% input$sas_combined_visits]
    } else {
      character()
    }
    if (combine_visits && length(selected_visits) == 0) {
      showNotification("Select at least one visit to combine.", type = "warning", duration = 5)
      rv$sas_plot_items <- list()
      return()
    }
    time_window <- NULL
    if (isTRUE(input$sas_use_time_window)) {
      limits <- sas_time_limits()
      requested <- input$sas_time_window
      if (length(limits) == 2L && diff(limits) > 0 &&
          length(requested) == 2L && all(is.finite(requested))) {
        requested <- sort(as.numeric(requested))
        time_window <- if (requested[2] < limits[1] || requested[1] > limits[2]) {
          limits
        } else {
          pmax(limits[1], pmin(limits[2], requested))
        }
      }
    }
    combos <- list()
    if (mode == "1") {
      req(input$sas_subject, input$sas_visit, input$sas_analyte)
      combos <- list(list(id = input$sas_subject,
                          visit = input$sas_visit,
                          analyte = input$sas_analyte))
    } else if (mode == "2") {
      req(input$sas_subject, input$sas_visit)
      s_pd     <- pd[pd$.id == input$sas_subject &
                     pd[[visit_col]] == input$sas_visit, , drop = FALSE]
      analytes <- sort(unique(s_pd$.analyte[!is.na(s_pd$.analyte)]))
      combos   <- lapply(analytes, function(a)
        list(id = input$sas_subject, visit = input$sas_visit, analyte = a))
    } else if (mode == "3") {
      req(input$sas_subject)
      s_pd  <- pd[pd$.id == input$sas_subject, , drop = FALSE]
      pairs <- unique(s_pd[, c(visit_col, ".analyte"), drop = FALSE])
      pairs <- pairs[!is.na(pairs[[visit_col]]) & !is.na(pairs$.analyte), , drop = FALSE]
      if (nrow(pairs) > 0) {
        pairs  <- pairs[order(pairs[[visit_col]], pairs$.analyte), ]
        combos <- lapply(seq_len(nrow(pairs)), function(i)
          list(id      = input$sas_subject,
               visit   = pairs[[visit_col]][i],
               analyte = pairs$.analyte[i]))
      }
    } else if (mode == "4") {
      full <- unique(pd[, c(".id", visit_col, ".analyte"), drop = FALSE])
      full <- full[!is.na(full$.id) & !is.na(full[[visit_col]]) &
                   !is.na(full$.analyte), , drop = FALSE]
      if (nrow(full) > 0) {
        full   <- full[order(full$.id, full[[visit_col]], full$.analyte), ]
        combos <- lapply(seq_len(nrow(full)), function(i)
          list(id      = full$.id[i],
               visit   = full[[visit_col]][i],
               analyte = full$.analyte[i]))
      }
    } else if (mode == "5") {
      if (combine_visits) {
        analytes <- sort(unique(pd$.analyte[
          !is.na(pd$.analyte) & pd[[visit_col]] %in% selected_visits
        ]))
        combos <- lapply(analytes, function(a)
          list(id = NULL, visits = selected_visits, analyte = a, combined = TRUE))
      } else {
        pairs <- unique(pd[, c(visit_col, ".analyte"), drop = FALSE])
        pairs <- pairs[!is.na(pairs[[visit_col]]) & !is.na(pairs$.analyte), , drop = FALSE]
        if (nrow(pairs) > 0) {
          pairs <- pairs[order(pairs[[visit_col]], pairs$.analyte), ]
          combos <- lapply(seq_len(nrow(pairs)), function(i)
            list(id = NULL, visit = pairs[[visit_col]][i], analyte = pairs$.analyte[i]))
        }
      }
    } else if (mode == "6") {
      req(input$sas_analyte)
      if (combine_visits) {
        combos <- list(list(
          id = NULL, visits = selected_visits,
          analyte = input$sas_analyte, combined = TRUE
        ))
      } else {
        visits <- stringr::str_sort(unique(as.character(pd[[visit_col]][
          !is.na(pd[[visit_col]]) & pd$.analyte == input$sas_analyte
        ])), numeric = TRUE)
        combos <- lapply(visits, function(v)
          list(id = NULL, visit = v, analyte = input$sas_analyte))
      }
    }
    if (length(combos) == 0) {
      showNotification("No data combinations found.", type = "warning", duration = 5)
      rv$sas_plot_items <- list()
      return()
    }
    if (length(combos) > 200) {
      showNotification(
        sprintf("Mode %s: %d combinations; showing first 200.", mode, length(combos)),
        type = "warning", duration = 7)
      combos <- combos[seq_len(200)]
    }
    items <- list()
    axis_warnings <- character()
    withProgress(message = "Building SAS-style plots...", value = 0, {
      nc <- length(combos)
      for (i in seq_len(nc)) {
        incProgress(1 / nc)
        co   <- combos[[i]]
        combined <- isTRUE(co$combined)
        combo_visits <- if (combined) co$visits else co$visit
        mask <- pd[[visit_col]] %in% combo_visits & pd$.analyte == co$analyte
        if (!is.null(co$id)) mask <- mask & pd$.id == co$id
        mask[is.na(mask)] <- FALSE
        s_pd <- pd[mask, , drop = FALSE]
        if (!is.null(time_window)) {
          raw_time <- profile_x_values(s_pd, xvar)
          in_window <- is.finite(raw_time) &
            raw_time >= time_window[1] & raw_time <= time_window[2]
          s_pd <- s_pd[in_window, , drop = FALSE]
        }
        if (nrow(s_pd) == 0) next
        all_subjects <- is.null(co$id)
        title_txt <- if (combined) {
          paste(co$analyte, "Combined visits", sep = "  |  ")
        } else if (all_subjects) {
          paste(co$visit, co$analyte, sep = "  |  ")
        } else {
          paste(co$id, co$visit, co$analyte, sep = "  |  ")
        }
        xlab_txt <- profile_x_axis_label(xvar, input$nominal_var, input$mrrlt_var)
        axis_info <- NULL
        plot_xvar <- xvar
        if (combined) {
          axis_info <- prepare_combined_visit_axis(
            s_pd, visit_col = visit_col, visits = combo_visits,
            xvar = xvar, xlab = xlab_txt, spacing = combined_spacing
          )
          if (nzchar(axis_info$warning %||% "")) {
            axis_warnings <- c(axis_warnings, axis_info$warning)
          }
          s_pd <- axis_info$data
          if (nrow(s_pd) == 0) next
          plot_xvar <- "combined"
          xlab_txt <- axis_info$xlab
        }
        n_subjects <- length(unique(s_pd$.id[!is.na(s_pd$.id)]))
        subtitle_txt <- if (combined) {
          paste0(
            "Visits: ", paste(combo_visits, collapse = ", "),
            " | N subjects = ", n_subjects, " | N records = ", nrow(s_pd)
          )
        } else if (all_subjects) {
          paste0("N subjects = ", n_subjects, " | N records = ", nrow(s_pd))
        } else {
          paste0("N plottable = ", nrow(s_pd))
        }
        plot_kind <- if (mode %in% c("5", "6")) {
          input$sas_subject_summary %||% "spaghetti"
        } else {
          "spaghetti"
        }
        summary_group_code <- input$sas_summary_group %||% "none"
        summary_group_var <- switch(
          summary_group_code,
          arm = ".arm", atrt = ".atrt", cohort = ".cohort", route = ".route",
          NULL
        )
        summary_group_label <- switch(
          summary_group_code,
          arm = "ARM", atrt = "Treatment", cohort = "Cohort", route = "Route",
          "Overall"
        )
        summary_tables <- NULL
        if (plot_kind %in% c("mean_sd", "mean_sd_band")) {
          mean_sd <- make_mean_sd_profile(
            pd = s_pd, xvar = plot_xvar, yscale = yscale,
            title = title_txt, subtitle = subtitle_txt,
            xlab = xlab_txt, ylab = "Concentration")
          g <- mean_sd$plot
          tbl <- mean_sd$data
          if (is.null(g)) next
        } else if (identical(plot_kind, "mean_sd_errorbar")) {
          mean_sd <- make_mean_sd_errorbar_profile(
            pd = s_pd, xvar = plot_xvar, yscale = yscale,
            title = title_txt, subtitle = subtitle_txt,
            xlab = xlab_txt, ylab = "Concentration",
            group_var = summary_group_var, group_label = summary_group_label,
            sd_sides = input$sas_sd_sides %||% "two",
            jitter_pct = input$sas_jitter_pct %||% 0
          )
          g <- mean_sd$plot
          tbl <- mean_sd$data
          if (is.null(g)) next
        } else {
          g <- make_spaghetti(
            pd = s_pd, xvar = plot_xvar, yscale = yscale,
            colour_var = ".id", colour_map = make_colour_map(s_pd$.id),
            title = title_txt, subtitle = subtitle_txt,
            xlab = xlab_txt, ylab = "Concentration", legend_title = "Subject",
            line_group_var = if (combined) ".sas_visit" else NULL)
          pctptnum_col <- names(s_pd)[toupper(names(s_pd)) == "PCTPTNUM"]
          keep <- intersect(
            c(".id", ".analyte", ".visit_display", ".nrrlt", ".mrrlt",
              ".aval_raw", ".aval_plot", ".blq_flag",
              pctptnum_col, ".sas_visit", ".sas_time_raw", ".sas_x"),
            names(s_pd))
          tbl           <- s_pd[, keep, drop = FALSE]
          colnames(tbl) <- sub("^\\.", "", colnames(tbl))
          tbl           <- tbl[order(if (combined) tbl$sas_x else tbl$nrrlt), , drop = FALSE]
        }
        if (plot_kind %in% c("mean_sd", "mean_sd_band", "mean_sd_errorbar")) {
          summary_tables <- make_concentration_summary_tables(
            s_pd, xvar = plot_xvar,
            group_var = summary_group_var, group_label = summary_group_label
          )
        }
        if (combined) g <- add_combined_visit_guides(g, axis_info)
        items[[length(items) + 1]] <- list(
          plot = g, data = tbl, combined = combined,
          summary_tables = summary_tables
        )
      }
    })
    rv$sas_plot_items <- items
    if (length(items) == 0) {
      showNotification(
        "No plottable data remain for the selected visits and time range.",
        type = "warning", duration = 6
      )
    } else {
      showNotification(sprintf("Generated %d SAS-style plot(s).", length(items)),
                       type = "message", duration = 4)
    }
    if (length(axis_warnings)) {
      showNotification(
        paste(unique(axis_warnings), collapse = " "),
        type = "warning", duration = 10
      )
    }
  })
  output$sas_plots_ui <- renderUI({
    items <- rv$sas_plot_items
    if (length(items) == 0)
      return(tags$p(style = "color:#888; margin-top:12px;",
                    "Click 'Generate Plots' after loading data."))
    tags_list <- vector("list", length(items))
    for (i in seq_along(items)) {
      it  <- items[[i]]
      oid <- paste0("sas_plt_", i)
      tid <- paste0("sas_tbl_", i)
      sid <- paste0("sas_tbl_page_", i)
      summary_tables <- it$summary_tables
      local({
        oid_ <- oid; tid_ <- tid; sid_ <- sid
        g_ <- it$plot; tbl_ <- it$data; combined_ <- isTRUE(it$combined)
        summary_tables_ <- summary_tables
        plot_height_ <- if (combined_) 560 else 440
        output[[oid_]] <- renderPlotly(as_plotly(
          g_, h = plot_height_, combined = combined_
        ))
        output[[tid_]] <- renderDT({
          display_data <- tbl_
          if (length(summary_tables_)) {
            selected_page <- input[[sid_]]
            if (is.null(selected_page) || !selected_page %in% names(summary_tables_)) {
              selected_page <- names(summary_tables_)[1]
            }
            display_data <- summary_tables_[[selected_page]]
          }
          review_datatable(
            display_data, rownames = FALSE, class = "compact stripe",
            options = list(scrollX = TRUE, pageLength = 10, dom = "tip")
          )
        })
      })
      summary_table_controls <- if (length(summary_tables)) {
        tagList(
          if (length(summary_tables) > 1L) {
            selectInput(
              sid, "Summary table page",
              choices = names(summary_tables), selected = names(summary_tables)[1]
            )
          } else {
            tags$h5(names(summary_tables)[1])
          },
          tags$p(
            class = "help-note",
            "Statistics are calculated from one value per subject and nominal time. Geometric statistics are NA when any contributing concentration is zero or negative; geometric CV% also requires at least two subjects."
          )
        )
      } else {
        NULL
      }
      tags_list[[i]] <- div(style = "background:#fff;border:1px solid #dde3ec;border-radius:8px;margin-bottom:14px;padding:8px;"
,
        plotlyOutput(oid, height = if (isTRUE(it$combined)) "560px" else "440px"),
        tags$details(
          tags$summary(style = "cursor:pointer; color:#1565C0; font-size:.85rem; margin-top:6px;"
,
            if (length(summary_tables)) "Show concentration summary table" else "Show underlying data table"),
          summary_table_controls,
          DTOutput(tid)
        )
      )
    }
    tagList(tags_list)
  })

  # Mappings must initialize even while their setup sections are collapsed.
  for (id in c(
    "var_select_ui", "filter_select_ui", "pk_var_map_ui", "pk_group_map_ui",
    "ind_time_window_ui", "sas_subject_ui", "sas_visit_ui", "sas_analyte_ui",
    "sas_combined_visits_ui", "sas_combined_axis_diagnostic", "sas_time_window_ui"
  )) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
