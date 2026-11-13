grouping_audit_columns <- function(data, mapped = list()) {
  nm <- names(data); upper <- toupper(nm)
  find_all <- function(candidates) nm[upper %in% toupper(candidates)]
  mapped_or_candidates <- function(mapped_name, candidates) {
    value <- mapped[[mapped_name]]
    if (!is.null(value) && length(value) && !is.na(value[1]) && nzchar(value[1]) && value[1] %in% nm) value[1]
    else head(find_all(candidates), 1L)
  }
  list(
    APROFILE = find_all("APROFILE"),
    Analyte = mapped_or_candidates("analyte", c("ATESTCD", "ATEST", "PCTESTCD", "PCTEST", "PARAMCD", "PARAM")),
    AVISIT = mapped_or_candidates("avisit", "AVISIT"),
    VISITCD = mapped_or_candidates("visitcd", "VISITCD"),
    `Nominal time` = mapped_or_candidates("nrrlt", c("NRRLT", "NFRLT", "PCTPTNUM")),
    SEX = find_all("SEX"),
    `Route of Administration` = find_all(c("ROUTE", "EXROUTE", "PCROUTE", "ADROUTE")),
    Treatment = find_all(c("TRTA", "TRTP", "TRT01A", "TRT01P", "ACTARM", "ARM", "TREATMENT", "TRT")),
    ARMCD = find_all("ARMCD"),
    COHORT = find_all(c("COHORT", "COHORTCD")),
    BSACAT = find_all("BSACAT")
  )
}

grouping_values_audit <- function(data, mapped = list()) {
  detected <- grouping_audit_columns(data, mapped)
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  for (factor_name in names(detected)) {
    columns <- detected[[factor_name]]
    if (!length(columns)) {
      add(Factor = factor_name, `Dataset variable` = "(not found)", `Variable label` = "",
          Status = "VARIABLE NOT FOUND", `Unique values` = 0L, Value = "", `N records` = NA_integer_, Percent = NA_real_)
      next
    }
    multiple <- length(columns) > 1L
    for (column in columns) {
      x <- data[[column]]
      display <- as.character(x)
      display[is.na(x)] <- "(MISSING)"
      display[!is.na(x) & trimws(display) == ""] <- "(EMPTY)"
      counts <- sort(table(display, useNA = "no"), decreasing = TRUE)
      values <- names(counts)
      status <- ifelse(values %in% c("(MISSING)", "(EMPTY)"), "CHECK MISSING/BLANK",
                       if (multiple) "CHECK MULTIPLE CANDIDATE VARIABLES" else "OK")
      label <- as.character(attr(x, "label") %||% "")
      for (i in seq_along(values)) {
        add(Factor = factor_name, `Dataset variable` = column, `Variable label` = label,
            Status = status[i], `Unique values` = length(values), Value = values[i],
            `N records` = as.integer(counts[i]), Percent = round(100 * as.integer(counts[i]) / length(x), 2))
      }
    }
  }
  do.call(rbind, rows)
}
