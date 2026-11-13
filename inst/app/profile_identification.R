# Identify and summarize the record grain for post-dose concentration-time profiles.

profile_find_col <- function(data, candidates, preferred = NULL) {
  nm <- names(data)
  if (!is.null(preferred) && length(preferred) && !is.na(preferred[1]) &&
      nzchar(trimws(preferred[1]))) {
    hit <- nm[toupper(nm) == toupper(trimws(preferred[1]))]
    if (length(hit)) return(hit[1])
  }
  positions <- match(toupper(candidates), toupper(nm))
  positions <- positions[!is.na(positions)]
  if (length(positions)) nm[positions[1]] else NULL
}

profile_find_populated_col <- function(data, candidates) {
  for (candidate in candidates) {
    column <- profile_find_col(data, candidate)
    if (!is.null(column) && any(profile_nonempty(data[[column]]))) return(column)
  }
  NULL
}

profile_nonempty <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

profile_values <- function(x) {
  value <- trimws(as.character(x))
  value[is.na(x) | !nzchar(value)] <- "(missing)"
  value
}

profile_signature <- function(data, columns) {
  if (!length(columns)) return(rep("", nrow(data)))
  do.call(paste, c(lapply(data[columns], profile_values), sep = "\r"))
}

subject_id_relationship <- function(data) {
  usubjid <- profile_find_col(data, "USUBJID")
  subjid <- profile_find_col(data, "SUBJID")
  if (is.null(usubjid) || is.null(subjid)) {
    return(list(available = FALSE, one_to_one = NA, usubjid = usubjid,
                subjid = subjid, conflicting_usubjid = 0L, conflicting_subjid = 0L))
  }
  keep <- profile_nonempty(data[[usubjid]]) & profile_nonempty(data[[subjid]])
  pairs <- unique(data.frame(
    usubjid = trimws(as.character(data[[usubjid]][keep])),
    subjid = trimws(as.character(data[[subjid]][keep])),
    stringsAsFactors = FALSE
  ))
  if (!nrow(pairs) || sum(keep) != nrow(data)) {
    return(list(available = TRUE, one_to_one = FALSE, usubjid = usubjid,
                subjid = subjid, conflicting_usubjid = NA_integer_,
                conflicting_subjid = NA_integer_))
  }
  usub_counts <- table(pairs$usubjid)
  sub_counts <- table(pairs$subjid)
  list(
    available = TRUE,
    one_to_one = all(usub_counts == 1L) && all(sub_counts == 1L),
    usubjid = usubjid,
    subjid = subjid,
    conflicting_usubjid = sum(usub_counts > 1L),
    conflicting_subjid = sum(sub_counts > 1L)
  )
}

subject_identifier_counts <- function(data) {
  count_values <- function(column) {
    if (is.null(column)) return(NA_integer_)
    values <- trimws(as.character(data[[column]]))
    values <- values[!is.na(values) & nzchar(values)]
    length(unique(values))
  }
  usubjid <- profile_find_col(data, "USUBJID")
  subjid <- profile_find_col(data, "SUBJID")
  list(
    usubjid = count_values(usubjid), subjid = count_values(subjid),
    usubjid_col = usubjid, subjid_col = subjid
  )
}

subject_id_mapping_audit <- function(data) {
  usubjid_col <- profile_find_col(data, "USUBJID")
  subjid_col <- profile_find_col(data, "SUBJID")
  if (is.null(usubjid_col) || is.null(subjid_col)) {
    return(list(
      available = FALSE, one_to_one = NA, table = data.frame(),
      conflicting_usubjid = character(), conflicting_subjid = character(),
      affected_usubjid = character(), affected_subjid = character(),
      missing_usubjid = FALSE, missing_subjid = FALSE
    ))
  }
  usubjid <- profile_values(data[[usubjid_col]])
  subjid <- profile_values(data[[subjid_col]])
  pairs <- unique(data.frame(
    USUBJID = usubjid, SUBJID = subjid,
    stringsAsFactors = FALSE, check.names = FALSE
  ))
  pairs <- pairs[order(pairs$USUBJID, pairs$SUBJID), , drop = FALSE]
  usub_map <- split(pairs$SUBJID[pairs$USUBJID != "(missing)"],
                    pairs$USUBJID[pairs$USUBJID != "(missing)"], drop = TRUE)
  sub_map <- split(pairs$USUBJID[pairs$SUBJID != "(missing)"],
                   pairs$SUBJID[pairs$SUBJID != "(missing)"], drop = TRUE)
  conflicting_usubjid <- names(Filter(
    function(x) length(unique(x[x != "(missing)"])) > 1L, usub_map
  ))
  conflicting_subjid <- names(Filter(
    function(x) length(unique(x[x != "(missing)"])) > 1L, sub_map
  ))
  missing_usubjid <- any(pairs$USUBJID == "(missing)")
  missing_subjid <- any(pairs$SUBJID == "(missing)")
  affected <- pairs$USUBJID %in% conflicting_usubjid |
    pairs$SUBJID %in% conflicting_subjid
  attention <- affected | pairs$USUBJID == "(missing)" | pairs$SUBJID == "(missing)"
  display_order <- order(!attention, pairs$USUBJID, pairs$SUBJID)
  pairs <- pairs[display_order, , drop = FALSE]
  affected <- affected[display_order]
  list(
    available = TRUE,
    one_to_one = !length(conflicting_usubjid) && !length(conflicting_subjid) &&
      !missing_usubjid && !missing_subjid,
    table = pairs,
    conflicting_usubjid = conflicting_usubjid,
    conflicting_subjid = conflicting_subjid,
    affected_usubjid = unique(pairs$USUBJID[affected]),
    affected_subjid = unique(pairs$SUBJID[affected]),
    missing_usubjid = missing_usubjid,
    missing_subjid = missing_subjid
  )
}

profile_cycle_assignments <- function(data, subject_col, analyte_col, nominal_col,
                                      order_col = NULL, boundary_col = NULL,
                                      boundary_context_col = NULL,
                                      zero_tolerance = sqrt(.Machine$double.eps)) {
  n <- nrow(data)
  if (!n) return(integer())
  subject <- profile_values(data[[subject_col]])
  analyte <- profile_values(data[[analyte_col]])
  nominal <- suppressWarnings(as.numeric(data[[nominal_col]]))
  boundary_value <- if (!is.null(boundary_col) && boundary_col %in% names(data))
    profile_values(data[[boundary_col]]) else rep("(missing)", n)
  boundary_context <- if (!is.null(boundary_context_col) && boundary_context_col %in% names(data))
    profile_values(data[[boundary_context_col]]) else rep("(missing)", n)
  source_row <- seq_len(n)
  if (!is.null(order_col) && order_col %in% names(data)) {
    order_value <- data[[order_col]]
    if (inherits(order_value, c("POSIXct", "POSIXlt", "Date")) || is.numeric(order_value)) {
      order_value <- suppressWarnings(as.numeric(order_value))
    } else {
      order_value <- trimws(as.character(order_value))
    }
    ord <- order(subject, analyte, is.na(order_value), order_value, source_row, na.last = TRUE)
  } else {
    ord <- order(subject, analyte, source_row, na.last = TRUE)
  }
  group <- paste(subject, analyte, sep = "\r")
  cycle <- integer(n)
  cycle_offset <- 0L
  for (idx in split(ord, group[ord], drop = TRUE)) {
    is_zero <- !is.na(nominal[idx]) & abs(nominal[idx]) <= zero_tolerance
    boundary <- boundary_value[idx]
    prior_boundary <- c("(missing)", head(boundary, -1L))
    context <- boundary_context[idx]
    prior_context <- c("(missing)", head(context, -1L))
    context_changed <- if (is.null(boundary_context_col)) rep(TRUE, length(idx)) else
      context != prior_context
    boundary_start <- boundary != "(missing)" & (
      seq_along(boundary) == 1L |
      ((prior_boundary == "(missing)" | boundary != prior_boundary) & context_changed)
    )
    local_cycle <- cumsum(is_zero | boundary_start)
    cycle[idx] <- ifelse(local_cycle > 0L, local_cycle + cycle_offset, 0L)
    if (length(local_cycle)) cycle_offset <- cycle_offset + max(local_cycle)
  }
  cycle
}

profile_validate_candidate <- function(data, cycle, key_cols, aligned_cols, label, type,
                                       allow_reused_keys = FALSE) {
  eligible <- cycle > 0L
  cycle_ids <- sort(unique(cycle[eligible]))
  if (!length(cycle_ids) || !length(key_cols) || any(!key_cols %in% names(data))) {
    return(data.frame(
      Candidate = label, Type = type, Valid = FALSE, Cycles = length(cycle_ids),
      `Cycles with missing values` = length(cycle_ids),
      `Cycles with multiple values` = 0L, `Reused profile keys` = 0L,
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  missing_cycles <- 0L
  multiple_cycles <- 0L
  for (cycle_id in cycle_ids) {
    idx <- which(cycle == cycle_id)
    for (column in aligned_cols) {
      values <- profile_values(data[[column]][idx])
      if (any(values == "(missing)")) missing_cycles <- missing_cycles + 1L
      if (length(unique(values[values != "(missing)"])) > 1L) multiple_cycles <- multiple_cycles + 1L
    }
  }
  eligible_data <- data[eligible, , drop = FALSE]
  signatures <- profile_signature(eligible_data, key_cols)
  cycles_by_key <- split(cycle[eligible], signatures, drop = TRUE)
  reused_keys <- sum(vapply(cycles_by_key, function(x) length(unique(x)) > 1L, logical(1)))
  valid <- missing_cycles == 0L && multiple_cycles == 0L &&
    (allow_reused_keys || reused_keys == 0L)
  data.frame(
    Candidate = label, Type = type, Valid = valid, Cycles = length(cycle_ids),
    `Cycles with missing values` = missing_cycles,
    `Cycles with multiple values` = multiple_cycles,
    `Reused profile keys` = reused_keys,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

format_nominal_values <- function(x) {
  value <- sort(unique(suppressWarnings(as.numeric(x))))
  value <- value[is.finite(value)]
  if (!length(value)) return("")
  formatted <- vapply(value, function(z) {
    trimws(format(signif(z, 10), trim = TRUE, scientific = FALSE))
  }, character(1))
  paste(formatted, collapse = ", ")
}

profile_timepoint_table <- function(data, key_cols, nominal_col) {
  if (!nrow(data) || !length(key_cols) || any(!c(key_cols, nominal_col) %in% names(data))) {
    return(data.frame())
  }
  signatures <- profile_signature(data, key_cols)
  rows <- lapply(split(seq_len(nrow(data)), signatures, drop = TRUE), function(idx) {
    key <- as.data.frame(data[idx[1], key_cols, drop = FALSE], stringsAsFactors = FALSE,
                         check.names = FALSE)
    nominal <- suppressWarnings(as.numeric(data[[nominal_col]][idx]))
    key[["Number of time points"]] <- length(unique(nominal[is.finite(nominal)]))
    key[["Unique time-point values"]] <- format_nominal_values(nominal)
    key
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[do.call(order, lapply(out[key_cols], function(x) trimws(as.character(x)))), , drop = FALSE]
}

identify_pk_profiles <- function(data, mapped = list()) {
  # USUBJID is the cross-study unique subject key. Use SUBJID only when USUBJID
  # is genuinely unavailable; a user mapping to SUBJID must not weaken profile grain.
  subject_col <- profile_find_col(data, "USUBJID")
  if (is.null(subject_col)) subject_col <- profile_find_col(data, c("USUBJID", "SUBJID"), mapped$subject)
  if (is.null(subject_col)) subject_col <- profile_find_col(data, "SUBJID")
  analyte_col <- profile_find_col(data, c("PCTESTCD", "PCTEST", "ATESTCD", "ATEST"), mapped$analyte)
  visit_col <- profile_find_col(data, c("AVISIT", "VISIT"), mapped$visit)
  nominal_col <- profile_find_col(data, c("NRRLT", "NFRLT", "PCTPTNUM"), mapped$nominal)
  profile_col <- profile_find_col(data, c("APROFILE", "PROFILE"), mapped$profile)
  dose_cols <- unique(unlist(Filter(Negate(is.null), lapply(
    c("PCRFTDTM", "PCRFTDTC", "ARFTDTM", "ARFTDTC"),
    function(x) profile_find_col(data, x)
  )), use.names = FALSE))
  cohort_col <- profile_find_populated_col(data, c("COHORT", "COHORTCD"))
  arm_col <- profile_find_populated_col(data, c("ARMCD", "ARM"))
  context_cols <- Filter(function(x) !is.null(x) && any(profile_nonempty(data[[x]])),
                         c(cohort_col, arm_col))
  context_cols <- unique(context_cols)
  order_col <- profile_find_col(data, c("ADTM", "PCDTC", "ADT"), mapped$sample_time)
  id_check <- subject_id_relationship(data)

  required <- list(subject_col, analyte_col, nominal_col)
  if (any(vapply(required, is.null, logical(1)))) {
    return(list(
      available = FALSE,
      message = "Map subject, analyte and nominal time to assess concentration-time profiles.",
      id_check = id_check, selected = NULL, candidates = data.frame(),
      timepoints = data.frame(), n_profiles = 0L, cycle_count = 0L,
      unassigned_records = nrow(data)
    ))
  }

  nominal_candidates <- unique(unlist(Filter(Negate(is.null), lapply(
    unique(c(mapped$nominal, "NRRLT", "NFRLT", "PCTPTNUM")),
    function(x) profile_find_col(data, x)
  )), use.names = FALSE))
  cycle_options <- lapply(nominal_candidates, function(candidate_nominal) {
    profile_cycle_assignments(
      data, subject_col, analyte_col, candidate_nominal, order_col
    )
  })
  cycle_counts <- vapply(cycle_options, function(x) length(unique(x[x > 0L])), integer(1))
  cycle_errors <- vapply(seq_along(nominal_candidates), function(i) {
    candidate_cycle <- cycle_options[[i]]
    if (!is.null(visit_col)) {
      audit <- profile_validate_candidate(
        data, candidate_cycle,
        unique(c(subject_col, analyte_col, visit_col, context_cols)),
        unique(c(visit_col, context_cols)), "visit", "AVISIT"
      )
    } else if (length(dose_cols)) {
      audit <- profile_validate_candidate(
        data, candidate_cycle,
        unique(c(subject_col, analyte_col, dose_cols[1], context_cols)),
        unique(c(dose_cols[1], context_cols)), "dose", "DOSE_TIME",
        allow_reused_keys = TRUE
      )
    } else {
      return(sum(candidate_cycle == 0L) + abs(length(unique(candidate_cycle[candidate_cycle > 0L])) - 1L))
    }
    audit[["Cycles with missing values"]] +
      audit[["Cycles with multiple values"]] + audit[["Reused profile keys"]]
  }, numeric(1))
  cycle_scores <- cycle_errors + 2 * (max(cycle_counts) - cycle_counts)
  # The mapped nominal variable wins ties; a different available time variable is
  # used only when its zero pattern is materially more coherent with profile grain.
  cycle_nominal_col <- nominal_candidates[which.min(cycle_scores)]
  cycle <- profile_cycle_assignments(
    data, subject_col, analyte_col, cycle_nominal_col, order_col,
    boundary_col = if (length(dose_cols)) dose_cols[1] else NULL,
    boundary_context_col = visit_col
  )
  cycle_count <- length(unique(cycle[cycle > 0L]))
  candidates <- list()
  definitions <- list()
  add_candidate <- function(type, discriminator, key_cols, label, allow_reused_keys = FALSE) {
    if (is.null(discriminator) || !length(discriminator)) return(invisible(NULL))
    aligned <- unique(c(discriminator, context_cols))
    key_cols <- unique(c(key_cols, context_cols))
    candidates[[length(candidates) + 1L]] <<- profile_validate_candidate(
      data, cycle, key_cols, aligned, label, type, allow_reused_keys
    )
    definitions[[length(definitions) + 1L]] <<- list(
      type = type, key_cols = key_cols, label = label, discriminator = discriminator
    )
  }

  if (!is.null(profile_col)) {
    add_candidate("APROFILE", profile_col, c(subject_col, profile_col),
                  paste(c(subject_col, profile_col, context_cols), collapse = " + "))
  }
  if (!is.null(visit_col)) {
    add_candidate("AVISIT", visit_col, c(subject_col, analyte_col, visit_col),
                  paste(c(subject_col, analyte_col, visit_col, context_cols), collapse = " + "))
  }
  for (dose_col in dose_cols) {
    add_candidate("DOSE_TIME", dose_col, c(subject_col, analyte_col, dose_col),
                  paste(c(subject_col, analyte_col, dose_col, context_cols), collapse = " + "),
                  allow_reused_keys = TRUE)
  }

  candidate_table <- if (length(candidates)) do.call(rbind, candidates) else data.frame()
  # Prefer an explicit valid APROFILE; otherwise prefer the user-friendly AVISIT
  # key over an equally valid dose-reference timestamp.
  priority <- c("APROFILE", "AVISIT", "DOSE_TIME")
  chosen <- NULL
  for (candidate_type in priority) {
    idx <- which(vapply(definitions, `[[`, character(1), "type") == candidate_type & candidate_table$Valid)
    if (length(idx)) {
      chosen <- definitions[[idx[1]]]
      break
    }
  }
  if (is.null(chosen) && length(definitions)) {
    fallback_order <- c("AVISIT", "DOSE_TIME", "APROFILE")
    for (candidate_type in fallback_order) {
      idx <- which(vapply(definitions, `[[`, character(1), "type") == candidate_type)
      if (length(idx)) {
        chosen <- definitions[[idx[1]]]
        chosen$fallback <- TRUE
        break
      }
    }
  }

  # The profile table describes the planned sampling schedule. Use the same
  # zero-based nominal variable that defined dose cycles (normally NRRLT), not
  # a mapped analysis-relative variable such as NFRLT.
  timepoints <- if (is.null(chosen)) data.frame() else
    profile_timepoint_table(data, chosen$key_cols, cycle_nominal_col)
  selected_valid <- !is.null(chosen) && !isTRUE(chosen$fallback)
  message <- if (is.null(chosen)) {
    "No usable concentration-time profile identifier was found."
  } else if (selected_valid) {
    paste0("Concentration-time profile after administered dose is identified by ", chosen$label)
  } else {
    paste0("WARNING: no tested key uniquely aligned with nominal-time dose cycles; table fallback is ", chosen$label)
  }

  list(
    available = !is.null(chosen), message = message, id_check = id_check,
    selected = chosen, candidates = candidate_table, timepoints = timepoints,
    n_profiles = nrow(timepoints), cycle_count = cycle_count,
    unassigned_records = sum(cycle == 0L), subject_col = subject_col,
    analyte_col = analyte_col, visit_col = visit_col, nominal_col = nominal_col,
    cycle_nominal_col = cycle_nominal_col,
    context_cols = context_cols, order_col = order_col
  )
}
