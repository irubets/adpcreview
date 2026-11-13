# Deterministic checks for ADPC/ADNCA concentration and parameter records.
# Source columns are never rewritten. Each outcome includes applicability.
adpc_quality_checks <- function(data, mappings, kind = "concentration", keys = character()) {
  d <- as.data.frame(data)
  n <- nrow(d)
  resolve <- function(x) {
    x <- as.character(x %||% ""); x <- x[!is.na(x) & nzchar(x)]
    hit <- match(toupper(x), toupper(names(d))); hit <- hit[!is.na(hit)]
    if (length(hit)) names(d)[hit[1]] else NULL
  }
  blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  checks <- list()
  add <- function(id, title, interest, rule, available = TRUE, request = "") {
    interest[is.na(interest)] <- FALSE
    checks[[id]] <<- list(title = title, interest = interest, rule = rule,
      available = available, request = request)
  }
  subject <- resolve(mappings$subject %||% c("USUBJID", "SUBJID"))
  analyte <- resolve(if (kind == "parameter") mappings$parameter %||% "PARAMCD" else mappings$analyte)
  value <- resolve(mappings$value %||% mappings$aval %||% "AVAL")
  visit <- resolve(mappings$visit)
  essentials <- unique(c(subject, analyte, if (kind == "concentration") visit))
  missing_id <- rep(FALSE, n)
  for (col in essentials) missing_id <- missing_id | blank(d[[col]])
  add("missing_keys", "Missing subject / analysis identifiers", missing_id,
      "Missing or blank mapped subject, analyte/parameter and (for concentration mode) visit identifiers. Missing identifiers prevent trustworthy grouping.",
      !is.null(subject) && !is.null(analyte), "Map subject and analyte/parameter identifiers.")
  y <- if (!is.null(value)) num(d[[value]]) else rep(NA_real_, n)
  add("invalid_value", "Missing or nonfinite analysis values", !is.finite(y),
      "Mapped concentration/parameter value is missing, nonnumeric, NaN or infinite. Values are not imputed. Missing results may be intentional; confirm exclusion and derivation conventions.",
      !is.null(value), "Map an analysis value column.")
  if (kind == "concentration") {
    add("negative_value", "Negative concentration", is.finite(y) & y < 0,
        "Finite mapped concentration below zero; no automatic correction or BLQ substitution.")
    for (role in c("nominal", "actual", "mrrlt")) {
      col <- resolve(mappings[[role]])
      add(paste0("missing_", role), paste("Missing / nonfinite", role, "time"),
          if (is.null(col)) rep(FALSE,n) else !is.finite(num(d[[col]])),
          paste("Mapped", role, "time must be finite numeric. Analysis-relative time is optional; absent mapping is not assessable. Source units must be consistent."),
          !is.null(col), paste("Map",role,"time to assess this check."))
    }
  }
  if (length(keys)) {
    keys <- unique(intersect(keys,names(d)))
    dup <- duplicated(d[keys]) | duplicated(d[keys],fromLast=TRUE)
    add("duplicate_key", "Repeated analysis key", dup,
        paste("Repeated values across the committed key:",paste(keys,collapse=" + "),". All repeated records are retained. Confirm legitimate repeat occasions before classifying as errors."))
  }
  exact <- duplicated(d) | duplicated(d,fromLast=TRUE)
  add("exact_duplicate", "Exact duplicate source records", exact,
      "All source columns compare equal using R data-frame duplication semantics. All copies are retained; row position is not part of the comparison.")
  unit <- resolve(if(kind=="parameter") c("AVALU","PARAMU") else c("AVALU","PCSTRESU"))
  units_bad <- rep(FALSE,n)
  if (!is.null(unit) && !is.null(analyte)) {
    units_bad <- blank(d[[unit]])
    groups <- split(seq_len(n),as.character(d[[analyte]]),drop=TRUE)
    for (idx in groups) if(length(unique(as.character(d[[unit]][idx][!blank(d[[unit]][idx])]))) > 1)
      units_bad[idx] <- TRUE
  }
  add("units", "Missing / inconsistent analysis units", units_bad,
      "Missing units or more than one populated unit within an analyte/PARAMCD; all rows of a conflicting unit group are candidates for review. No conversion is guessed. Legitimately stratified units require study context.",
      !is.null(unit) && !is.null(analyte), "Supply AVALU (or PARAMU/PCSTRESU) and analyte/PARAMCD.")
  if(kind=="parameter") {
    label <- resolve("PARAM"); mismatch <- rep(FALSE,n)
    if(!is.null(label) && !is.null(analyte)) {
      mismatch <- blank(d[[label]])
      for(idx in split(seq_len(n),as.character(d[[analyte]]),drop=TRUE))
        if(length(unique(as.character(d[[label]][idx][!blank(d[[label]][idx])]))) > 1) mismatch[idx] <- TRUE
    }
    add("parameter_label", "Missing / inconsistent parameter labels", mismatch,
        "PARAMCD has missing PARAM or multiple populated PARAM descriptions. All conflicting-label records are shown; no dictionary authority is inferred.",
        !is.null(label) && !is.null(analyte), "Supply PARAMCD and PARAM.")
  }
  flags <- names(d)[grepl("^(ANL[0-9]+FL|NCA[0-9]+FL|PKSUMXFL)$",toupper(names(d)))]
  invalid_flags <- rep(FALSE,n)
  for(col in flags) invalid_flags <- invalid_flags | (!blank(d[[col]]) & !toupper(trimws(as.character(d[[col]]))) %in% c("Y","N"))
  add("analysis_flags", "Nonstandard analysis flag values", invalid_flags,
      "Populated ANLnnFL, NCAnnFL and PKSUMXFL values other than Y/N (case-insensitive) are screened. Blank flags may be intentional. APROFILE is not treated as a binary flag.",
      length(flags)>0, "No ANLnnFL, NCAnnFL or PKSUMXFL found.")
  list(data=d, checks=checks, keys=keys, mappings=mappings, kind=kind)
}

adpc_quality_sections <- function(quality) {
  lapply(names(quality$checks),function(id) {
    z <- quality$checks[[id]]; d <- quality$data
    # Complete context using the committed key without the parameter/analyte code
    # is not inferred here: preserve the whole selected analysis profile.
    keys <- quality$keys
    if(!length(keys)) keys <- intersect(unlist(quality$mappings[c("subject","analyte","visit")]),names(d))
    if(length(keys)) result <- build_review_finding_subset(d,z$interest,keys) else {
      out <- d[z$interest,,drop=FALSE]; out$RECORD_OF_INTEREST <- rep("YES",nrow(out))
      result <- list(data=out,profiles=NA_integer_)
    }
    description <- if(!z$available) paste("Not assessable.",z$request) else
      paste(sum(z$interest),"record(s) require review;",nrow(result$data),"context records retained.")
    list(id=paste0("quality-",id),group="ADPC / ADNCA checks",title=z$title,
         description=description,rule=z$rule,
         tables=list(list(title="Affected records and analysis context",data=result$data,interest_column="RECORD_OF_INTEREST")))
  })
}
