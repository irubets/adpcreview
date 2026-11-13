# Analysis-population flag explorer for ADPC review V12.

flag_natural_order <- function(x) {
  prefix <- sub("[0-9].*$", "", toupper(x))
  number <- suppressWarnings(as.integer(sub("^[^0-9]*([0-9]+).*$", "\\1", x)))
  number[is.na(number)] <- 9999L
  x[order(prefix, number, toupper(x))]
}

detect_analysis_flags_v12 <- function(df) {
  nm <- names(df)
  upper <- toupper(nm)
  crit_flags <- nm[grepl("^CRIT[0-9]+FL$", upper)]
  crit_text <- nm[grepl("^CRIT[0-9]+$", upper)]
  nca_flags <- nm[grepl("^NCA[0-9]+X?FL$", upper)]
  analysis_flags <- nm[grepl("^ANL0[12]FL$", upper)]
  pksum <- nm[upper == "PKSUMXFL"]
  list(
    crit_flags = flag_natural_order(crit_flags),
    crit_text = flag_natural_order(crit_text),
    nca_flags = flag_natural_order(nca_flags),
    analysis_flags = flag_natural_order(analysis_flags),
    pksum_flags = pksum,
    all_flags = unique(c(flag_natural_order(crit_flags), flag_natural_order(nca_flags),
                         flag_natural_order(analysis_flags), pksum))
  )
}

flag_clean_value <- function(x) {
  z <- toupper(trimws(as.character(x)))
  z[is.na(x) | !nzchar(z)] <- "(missing)"
  z
}

flag_family_v12 <- function(x) {
  u <- toupper(x)
  if (grepl("^CRIT", u)) return("Criterion")
  if (u == "PKSUMXFL") return("Summary population")
  if (grepl("^(NCA01|ANL01)FL$", u)) return("NCA inclusion")
  if (grepl("^(NCA02|ANL02)FL$", u)) return("Summary inclusion")
  if (grepl("^NCA[0-9]+XFL$", u)) return("NCA exclusion")
  if (grepl("^NCA[0-9]+FL$", u)) return("NCA flag")
  if (grepl("^ANL", u)) return("Analysis flag")
  "Other flag"
}

flag_dictionary_v12 <- function(df) {
  detected <- detect_analysis_flags_v12(df)
  if (!length(detected$all_flags)) return(data.frame())
  nm <- names(df)
  rows <- lapply(detected$all_flags, function(flag) {
    values <- flag_clean_value(df[[flag]])
    base <- sub("FL$", "", toupper(flag))
    desc_col <- nm[toupper(nm) == base]
    descriptions <- character()
    if (length(desc_col) == 1L && grepl("^CRIT[0-9]+$", base)) {
      descriptions <- trimws(as.character(df[[desc_col]]))
      descriptions <- sort(unique(descriptions[!is.na(descriptions) & nzchar(descriptions)]))
    }
    label <- attr(df[[flag]], "label") %||% ""
    data.frame(
      VARIABLE = flag,
      FAMILY = flag_family_v12(flag),
      DESCRIPTION_VARIABLE = if (length(desc_col)) desc_col[1] else "",
      DESCRIPTION = if (length(descriptions)) paste(descriptions, collapse = " | ") else "",
      LABEL = as.character(label),
      N_Y = sum(values == "Y"),
      N_N = sum(values == "N"),
      N_MISSING = sum(values == "(missing)"),
      N_OTHER = sum(!values %in% c("Y", "N", "(missing)")),
      PCT_Y = round(100 * mean(values == "Y"), 1),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

flag_long_summary_v12 <- function(df, flags = NULL) {
  available <- detect_analysis_flags_v12(df)$all_flags
  flags <- intersect(flags %||% available, available)
  if (!length(flags)) return(data.frame())
  do.call(rbind, lapply(flags, function(flag) {
    values <- flag_clean_value(df[[flag]])
    counts <- sort(table(values), decreasing = TRUE)
    data.frame(
      VARIABLE = flag,
      FAMILY = flag_family_v12(flag),
      VALUE = names(counts),
      N = as.integer(counts),
      PERCENT = round(100 * as.integer(counts) / length(values), 1),
      stringsAsFactors = FALSE
    )
  }))
}

flag_combinations_v12 <- function(df, flags, id_var = NULL, max_rows = 50L) {
  flags <- intersect(flags, names(df))
  if (!length(flags)) return(data.frame())
  normalized <- as.data.frame(lapply(df[flags], flag_clean_value), stringsAsFactors = FALSE)
  signature <- apply(normalized, 1, function(x) paste(paste(flags, x, sep = "="), collapse = " | "))
  groups <- split(seq_len(nrow(df)), signature)
  result <- do.call(rbind, lapply(names(groups), function(sig) {
    idx <- groups[[sig]]
    subjects <- if (!is.null(id_var) && id_var %in% names(df)) {
      length(unique(as.character(df[[id_var]][idx])))
    } else NA_integer_
    data.frame(COMBINATION = sig, N_RECORDS = length(idx), N_SUBJECTS = subjects,
               stringsAsFactors = FALSE)
  }))
  result <- result[order(-result$N_RECORDS, result$COMBINATION), , drop = FALSE]
  head(result, max_rows)
}

flag_alias_issues_v12 <- function(df) {
  pairs <- list(c("NCA01FL", "ANL01FL"), c("NCA02FL", "ANL02FL"))
  rows <- list()
  for (pair in pairs) {
    actual <- vapply(pair, function(x) {
      hit <- names(df)[toupper(names(df)) == x]
      if (length(hit)) hit[1] else ""
    }, character(1))
    if (all(nzchar(actual))) {
      a <- flag_clean_value(df[[actual[1]]])
      b <- flag_clean_value(df[[actual[2]]])
      bad <- a != b
      rows[[length(rows) + 1L]] <- data.frame(
        CHECK = paste(actual, collapse = " vs "),
        N_RECORDS = sum(bad),
        DETAIL = "Alias values disagree on the same record",
        stringsAsFactors = FALSE
      )
    }
  }
  detected <- detect_analysis_flags_v12(df)$all_flags
  for (flag in detected) {
    v <- flag_clean_value(df[[flag]])
    invalid <- !v %in% c("Y", "N", "(missing)")
    if (any(invalid)) {
      rows[[length(rows) + 1L]] <- data.frame(
        CHECK = paste(flag, "invalid values"),
        N_RECORDS = sum(invalid),
        DETAIL = paste(sort(unique(v[invalid])), collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(CHECK = "No structural issues detected", N_RECORDS = 0L,
                      DETAIL = "No alias disagreements or non-Y/N values were found."))
  }
  do.call(rbind, rows)
}

flag_pair_crosstab_v12 <- function(df, x, y) {
  if (!all(c(x, y) %in% names(df))) return(data.frame())
  tab <- as.data.frame(table(
    X_VALUE = flag_clean_value(df[[x]]),
    Y_VALUE = flag_clean_value(df[[y]]),
    useNA = "ifany"
  ), stringsAsFactors = FALSE)
  names(tab)[3] <- "N_RECORDS"
  tab$X_VARIABLE <- x
  tab$Y_VARIABLE <- y
  tab[, c("X_VARIABLE", "X_VALUE", "Y_VARIABLE", "Y_VALUE", "N_RECORDS")]
}

flag_context_v12 <- function(df, max_combinations = 20L) {
  detected <- detect_analysis_flags_v12(df)
  dictionary <- flag_dictionary_v12(df)
  default_combo <- unique(c(
    detected$crit_flags,
    detected$nca_flags,
    detected$pksum_flags,
    detected$analysis_flags
  ))
  default_combo <- head(default_combo, 12)
  list(
    detected_variables = detected,
    dictionary = dictionary,
    top_combinations = flag_combinations_v12(df, default_combo, max_rows = max_combinations),
    structural_checks = flag_alias_issues_v12(df),
    privacy = "Aggregate flag counts only; no subject identifiers or row-level records"
  )
}
