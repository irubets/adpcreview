# Independent APROFILE feasibility assessment; no ADNCA inputs.
NCA_PARAMETERS_V11 <- c(CMAX = "cmax", TMAX = "tmax", AUCLST = "auclast",
                       LAMZ = "lambda.z", LAMZHL = "half.life", AUCINF = "aucinf.obs")

# One record-selection rule at a time. NA, empty strings and whitespace are empty.
filter_nca_records_v11 <- function(adpc, variable = "", mode = "equals", value = "Y") {
  if (is.null(variable) || !nzchar(variable)) return(adpc)
  if (!variable %in% names(adpc)) stop("Filter variable is not present: ", variable)
  x <- trimws(as.character(adpc[[variable]]))
  empty <- is.na(x) | x == ""
  keep <- switch(mode,
    equals = !empty & x == trimws(value),
    empty = empty,
    filled = !empty,
    stop("Unknown NCA filter mode: ", mode))
  adpc[which(keep), , drop = FALSE]
}

run_nca_feasibility_v11 <- function(adpc, config, progress = function(...) NULL) {
  if (!requireNamespace("PKNCA", quietly = TRUE)) stop("Install PKNCA to run NCA assessment.")
  config$n_loaded_records <- nrow(adpc)
  adpc$.NCA_SOURCE_ROW <- seq_len(nrow(adpc))
  adpc <- filter_nca_records_v11(adpc, config$filter_variable,
    if (is.null(config$filter_mode)) "equals" else config$filter_mode,
    if (is.null(config$filter_value)) "Y" else config$filter_value)
  config$n_selected_records <- nrow(adpc)
  config$n_excluded_records <- config$n_loaded_records - config$n_selected_records
  if (!nrow(adpc)) stop("No records match the selected NCA filter. Change the filter before running.")
  keys <- unique(c(config$profile, config$subject, config$analyte, config$extra_keys))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  required <- unique(c(keys, config$time, config$concentration))
  if (!all(required %in% names(adpc))) stop("Missing mapped variables: ", paste(setdiff(required, names(adpc)), collapse = ", "))
  if (!length(keys) || !nzchar(config$profile)) stop("Map APROFILE before running.")
  if (!is.numeric(adpc[[config$time]]) || !is.numeric(adpc[[config$concentration]]))
    stop("Time and AVAL must be numeric. Original values are not converted or replaced.")
  groups <- dplyr::group_split(dplyr::group_by(adpc, dplyr::across(dplyr::all_of(keys))), .keep = TRUE)
  summaries <- results <- cases <- inputs <- list()
  for (i in seq_along(groups)) {
    d <- as.data.frame(groups[[i]])
    key <- d[1, keys, drop = FALSE]
    tag <- function(x) cbind(data.frame(RUN_PROFILE = i), key[rep(1, nrow(x)), , drop = FALSE], x)
    t <- d[[config$time]]; y <- d[[config$concentration]]
    in_interval <- is.finite(t) & t >= config$start & t <= config$end
    issue <- function(code, severity, detail, rows = integer(), parameter = "ALL") {
      cases[[length(cases) + 1L]] <<- tag(data.frame(PARAMETER = parameter, SEVERITY = severity,
        CASE = code, DETAIL = detail, SOURCE_ROWS = paste(d$.NCA_SOURCE_ROW[rows], collapse = ", ")))
    }
    invalid_key <- Reduce(`|`, lapply(d[keys], function(x) is.na(x) | trimws(as.character(x)) == ""))
    if (any(invalid_key)) issue("MISSING_PROFILE_KEY", "BLOCKER", "Profile, subject, analyte or additional grouping key is missing; records cannot be assigned safely.", which(invalid_key))
    if (any(!is.finite(t))) issue("MISSING_OR_NONFINITE_TIME", "INPUT", "Submitted unchanged; PKNCA validation may reject this profile.", which(!is.finite(t)))
    if (any(t < 0, na.rm = TRUE)) issue("NEGATIVE_TIME", "INPUT", "Negative relative times are retained; interval membership and PKNCA endpoint requirements determine whether they affect calculation.", which(t < 0))
    if (any(y < 0, na.rm = TRUE)) issue("NEGATIVE_AVAL", "INPUT", "Negative AVAL is retained and submitted unchanged to PKNCA.", which(y < 0))
    if (any(is.na(y))) issue("MISSING_AVAL", "INFORMATION", "Missing AVAL is retained in input; PKNCA conc.na='drop' omits missing concentrations during calculation.", which(is.na(y)))
    if (any(!is.na(y) & !is.finite(y))) issue("NONFINITE_AVAL", "INPUT", "Infinite AVAL is submitted unchanged.", which(!is.na(y) & !is.finite(y)))
    dup <- duplicated(t) | duplicated(t, fromLast = TRUE)
    if (any(dup & is.finite(t))) issue("DUPLICATE_TIME", "INPUT", "Duplicate times within this profile are not averaged or removed.", which(dup & is.finite(t)))
    if (any(is.finite(t) & !in_interval)) issue("OUTSIDE_INTERVAL", "INFORMATION", "Records are retained in submitted data but are outside the selected NCA interval.", which(is.finite(t) & !in_interval))
    if (!any(in_interval & is.finite(y))) issue("NO_USABLE_INTERVAL_CONCENTRATIONS", "INPUT", "No finite concentration at a finite time within the interval.")
    if (any(in_interval & is.finite(y)) && !any(in_interval & is.finite(y) & y > 0)) issue("NO_POSITIVE_CONCENTRATIONS", "INPUT", "No positive concentrations in the interval; a numerical zero result does not establish PK evaluability.")
    if (sum(in_interval & is.finite(y) & y > 0) < 3) issue("FEW_POSITIVE_CONCENTRATIONS", "INPUT", "Fewer than three positive concentrations in the interval; terminal-phase estimation may be unavailable.", parameter = "LAMZ / LAMZHL / AUCINF")
    input <- d; input$RUN_PROFILE <- i; inputs[[i]] <- input
    pd <- data.frame(concentration = y, time = t, profile = rep(as.character(i), nrow(d)))
    pd <- pd[order(pd$time, na.last = TRUE), , drop = FALSE]
    opts <- list(auc.method = config$auc_method, conc.na = "drop",
                 conc.blq = list(first = "keep", middle = "keep", last = "keep"))
    ok <- logical(length(NCA_PARAMETERS_V11)); has_warning <- FALSE
    for (j in seq_along(NCA_PARAMETERS_V11)) {
      parameter <- names(NCA_PARAMETERS_V11)[j]; pknca_name <- unname(NCA_PARAMETERS_V11[j])
      warnings <- character(); error <- NULL
      raw <- tryCatch(withCallingHandlers({
        if (any(invalid_key)) stop("Missing profile grouping key; PKNCA was not called.")
        interval <- data.frame(start = config$start, end = config$end)
        interval[[pknca_name]] <- TRUE
        conc <- PKNCA::PKNCAconc(pd, concentration ~ time | profile)
        data <- suppressMessages(PKNCA::PKNCAdata(conc, intervals = interval, options = opts))
        as.data.frame(suppressMessages(PKNCA::pk.nca(data)))
      }, warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) { error <<- conditionMessage(e); NULL })
      value <- NA_real_; exclusion <- ""
      if (!is.null(raw) && all(c("PPTESTCD", "PPORRES") %in% names(raw))) {
        selected <- raw[raw$PPTESTCD == pknca_name, , drop = FALSE]
        if (nrow(selected)) {
          value <- selected$PPORRES[1]
          if ("exclude" %in% names(selected) && !is.na(selected$exclude[1])) exclusion <- as.character(selected$exclude[1])
        }
        # Keep dependency results (terminal fit diagnostics included by PKNCA).
        results[[length(results) + 1L]] <- tag(cbind(data.frame(REQUESTED_PARAMETER = parameter), raw))
      }
      ok[j] <- is.numeric(value) && is.finite(value) && !nzchar(exclusion)
      detail <- paste(unique(c(error, warnings, if (nzchar(exclusion)) exclusion)), collapse = " | ")
      if (!ok[j] && !nzchar(detail)) detail <- "PKNCA returned no finite result; inspect positive post-peak points and terminal-phase eligibility."
      if (!ok[j]) issue(if (is.null(error)) "PARAMETER_NOT_CALCULABLE" else "PKNCA_ERROR", "BLOCKER", detail, parameter = parameter)
      if (length(warnings)) { has_warning <- TRUE; issue("PKNCA_WARNING", "WARNING", paste(unique(warnings), collapse = " | "), parameter = parameter) }
      results[[length(results) + 1L]] <- tag(data.frame(REQUESTED_PARAMETER = parameter,
        PPTESTCD = pknca_name, PPORRES = value, exclude = exclusion,
        ASSESSMENT = if (ok[j]) "CALCULATED" else "NOT CALCULABLE", MESSAGE = detail))
    }
    summaries[[i]] <- tag(data.frame(N_RECORDS = nrow(d), N_IN_INTERVAL = sum(in_interval),
      N_POSITIVE_IN_INTERVAL = sum(in_interval & is.finite(y) & y > 0),
      N_INVALID_TIME = sum(!is.finite(t)), N_DUPLICATE_TIME_RECORDS = sum(dup & is.finite(t)),
      N_MISSING_AVAL = sum(is.na(y)), N_NEGATIVE_AVAL = sum(y < 0, na.rm = TRUE),
      N_ZERO_AVAL = sum(y == 0, na.rm = TRUE), N_PARAMETERS_CALCULATED = sum(ok),
      STATUS = if (all(ok)) { if (has_warning) "CALCULATED WITH WARNINGS" else "CALCULATED" } else if (any(ok)) "PARTIALLY CALCULABLE" else "NOT CALCULABLE",
      UNAVAILABLE_PARAMETERS = paste(names(NCA_PARAMETERS_V11)[!ok], collapse = ", ")))
    progress(i / length(groups), paste("Profile", i, "of", length(groups)))
  }
  list(summary = dplyr::bind_rows(summaries), results = dplyr::bind_rows(results),
       cases = dplyr::bind_rows(cases), input = dplyr::bind_rows(inputs), config = config,
       metadata = data.frame(APPLICATION = "adpc_review_full_v11", R = R.version.string,
         PKNCA = as.character(utils::packageVersion("PKNCA")), RUN_TIME = as.character(Sys.time())))
}

export_nca_feasibility_v11 <- function(result, file) {
  cfg <- data.frame(SETTING = names(result$config), VALUE = vapply(result$config,
    function(x) paste(x, collapse = ", "), character(1)))
  openxlsx::write.xlsx(list(Summary = result$summary, Cases = result$cases,
    Results = result$results, Input = result$input, Configuration = cfg,
    Runtime = result$metadata), file)
}
