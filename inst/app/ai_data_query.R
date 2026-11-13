# Validated, read-only data-query engine shared by the AI assistants.

ai_query_plan_schema <- function() {
  list(
    type = "object", additionalProperties = FALSE,
    properties = list(
      explanation = list(type = "string"),
      filters = list(type = "array", items = list(
        type = "object", additionalProperties = FALSE,
        properties = list(
          column = list(type = "string"),
          operator = list(type = "string", enum = c(
            "equals", "not_equals", "greater_than", "greater_or_equal",
            "less_than", "less_or_equal", "contains", "is_missing",
            "not_missing", "in"
          )),
          value = list(type = "string")
        ),
        required = c("column", "operator", "value")
      )),
      group_by = list(type = "array", items = list(type = "string")),
      duplicate_by = list(type = "array", items = list(type = "string")),
      select = list(type = "array", items = list(type = "string")),
      sort_by = list(type = "array", items = list(type = "string")),
      sort_desc = list(type = "boolean"),
      distinct = list(type = "boolean"),
      limit = list(type = "integer", minimum = 0)
    ),
    required = c("explanation", "filters", "group_by", "duplicate_by",
                 "select", "sort_by", "sort_desc", "distinct", "limit")
  )
}

ai_query_schema_description <- function(data) {
  type_one <- function(x) {
    if (inherits(x, c("POSIXct", "POSIXt"))) "datetime" else
      if (inherits(x, "Date")) "date" else
      if (is.numeric(x)) "numeric" else
      if (is.logical(x)) "logical" else "character"
  }
  data.frame(
    column = names(data),
    type = vapply(data, type_one, character(1)),
    label = vapply(data, function(x) as.character(attr(x, "label") %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
}

ai_query_planner_prompt <- function(question, data) {
  schema <- ai_query_schema_description(data)
  paste0(
    "Translate the user's request into a read-only data query plan. Return only the required JSON object.\n",
    "Use exact column names from DATASET SCHEMA. Do not invent columns.\n",
    "For duplicate analysis, put identifying case columns in group_by and the value whose repetition defines a duplicate in duplicate_by. ",
    "The engine will count the union of those columns and retain N_RECORDS > 1.\n",
    "Every requested output identifier in select must also appear in group_by or duplicate_by.\n",
    "N_RECORDS is added automatically after grouping; do not put N_RECORDS in select. It may be used in sort_by.\n",
    "Use filters only for conditions explicitly requested. For operator 'in', separate values with |. ",
    "Use limit=0 unless the user explicitly asks for a top/sample/limited result. Empty arrays are allowed.\n\n",
    "DATASET SCHEMA:\n",
    paste(apply(schema, 1, function(z) paste(z, collapse = " | ")), collapse = "\n"),
    "\n\nUSER REQUEST:\n", question
  )
}

ai_query_match_columns <- function(values, data_names, field) {
  values <- as.character(values %||% character())
  if (!length(values)) return(character())
  idx <- match(toupper(values), toupper(data_names))
  if (anyNA(idx)) stop(sprintf("Unknown %s column(s): %s", field, paste(values[is.na(idx)], collapse = ", ")))
  unique(data_names[idx])
}

ai_validate_query_plan <- function(plan, data) {
  if (!is.list(plan)) stop("The AI query plan is not a JSON object.")
  required <- c("explanation", "filters", "group_by", "duplicate_by", "select",
                "sort_by", "sort_desc", "distinct", "limit")
  missing <- setdiff(required, names(plan))
  if (length(missing)) stop(sprintf("Query plan is missing: %s", paste(missing, collapse = ", ")))
  plan$group_by <- ai_query_match_columns(plan$group_by, names(data), "group_by")
  plan$duplicate_by <- ai_query_match_columns(plan$duplicate_by, names(data), "duplicate_by")
  requested_select <- as.character(plan$select %||% character())
  plan$select <- ai_query_match_columns(requested_select[toupper(requested_select) != "N_RECORDS"], names(data), "select")
  if (length(plan$duplicate_by)) {
    promoted <- setdiff(plan$select, c(plan$group_by, plan$duplicate_by))
    if (length(promoted)) {
      plan$group_by <- unique(c(plan$group_by, promoted))
      plan$compiler_note <- paste0(
        "Added selected output column(s) to duplicate grouping keys: ",
        paste(promoted, collapse = ", ")
      )
    }
  }
  plan$sort_by <- as.character(plan$sort_by %||% character())
  filters <- plan$filters %||% list()
  if (is.data.frame(filters)) filters <- split(filters, seq_len(nrow(filters)))
  allowed_ops <- c("equals", "not_equals", "greater_than", "greater_or_equal",
                   "less_than", "less_or_equal", "contains", "is_missing", "not_missing", "in")
  filters <- lapply(filters, function(f) {
    if (!is.list(f) || is.null(f$column) || is.null(f$operator)) stop("Every filter requires column and operator.")
    f$column <- ai_query_match_columns(f$column, names(data), "filter")[[1]]
    f$operator <- as.character(f$operator)[1]
    f$value <- as.character(f$value %||% "")[1]
    if (!f$operator %in% allowed_ops) stop(sprintf("Unsupported filter operator: %s", f$operator))
    raw_values <- if (f$operator == "in") strsplit(f$value, "|", fixed = TRUE)[[1]] else f$value
    f$typed_value <- ai_query_coerce_value(raw_values, data[[f$column]])
    f
  })
  plan$filters <- filters
  plan$sort_desc <- isTRUE(plan$sort_desc)
  plan$distinct <- isTRUE(plan$distinct)
  plan$limit <- suppressWarnings(as.integer(plan$limit %||% 0L))
  if (is.na(plan$limit) || plan$limit < 0L) stop("Query limit must be zero or a positive integer.")
  query_keys <- unique(c(plan$group_by, plan$duplicate_by))
  empty_keys <- query_keys[vapply(query_keys, function(nm) {
    x <- data[[nm]]
    all(is.na(x) | trimws(as.character(x)) == "")
  }, logical(1))]
  if (length(empty_keys)) {
    plan$data_note <- paste0("Grouping column(s) contain only missing/blank values: ", paste(empty_keys, collapse = ", "))
  }
  plan
}

ai_query_coerce_value <- function(value, column) {
  if (is.numeric(column)) suppressWarnings(as.numeric(value)) else
    if (is.logical(column)) toupper(value) %in% c("TRUE", "T", "1", "Y", "YES") else
    if (inherits(column, "Date")) as.Date(value) else as.character(value)
}

ai_apply_query_filter <- function(data, filter) {
  x <- data[[filter$column]]
  op <- filter$operator
  if (op == "is_missing") return(is.na(x) | trimws(as.character(x)) == "")
  if (op == "not_missing") return(!is.na(x) & trimws(as.character(x)) != "")
  if (op == "contains") return(!is.na(x) & grepl(filter$value, as.character(x), ignore.case = TRUE, fixed = TRUE))
  values <- if (op == "in") strsplit(filter$value, "|", fixed = TRUE)[[1]] else filter$value
  values <- ai_query_coerce_value(values, x)
  if (op == "equals") !is.na(x) & x == values[[1]] else
    if (op == "not_equals") is.na(x) | x != values[[1]] else
    if (op == "greater_than") !is.na(x) & x > values[[1]] else
    if (op == "greater_or_equal") !is.na(x) & x >= values[[1]] else
    if (op == "less_than") !is.na(x) & x < values[[1]] else
    if (op == "less_or_equal") !is.na(x) & x <= values[[1]] else
    if (op == "in") !is.na(x) & x %in% values else rep(FALSE, nrow(data))
}

ai_execute_query_plan <- function(data, plan) {
  plan <- ai_validate_query_plan(plan, data)
  result <- as.data.frame(data, stringsAsFactors = FALSE)
  if (length(plan$filters)) {
    keep <- rep(TRUE, nrow(result))
    for (f in plan$filters) keep <- keep & ai_apply_query_filter(result, f)
    result <- result[keep, , drop = FALSE]
  }
  keys <- unique(c(plan$group_by, plan$duplicate_by))
  if (length(keys)) {
    if (length(plan$duplicate_by)) {
      result <- result[stats::complete.cases(result[plan$duplicate_by]), , drop = FALSE]
    }
    result <- dplyr::count(result, dplyr::across(dplyr::all_of(keys)), name = "N_RECORDS", .drop = FALSE)
    if (length(plan$duplicate_by)) result <- dplyr::filter(result, .data$N_RECORDS > 1L)
  } else if (plan$distinct) {
    result <- dplyr::distinct(result)
  }
  if (length(plan$select)) {
    wanted <- unique(c(plan$select, intersect("N_RECORDS", names(result))))
    missing <- setdiff(plan$select, names(result))
    if (length(missing)) stop(sprintf("Selected columns are unavailable after grouping: %s", paste(missing, collapse = ", ")))
    result <- result[, wanted, drop = FALSE]
  }
  sort_by <- ai_query_match_columns(plan$sort_by, names(result), "sort")
  if (length(sort_by)) {
    ord_args <- lapply(result[sort_by], function(x) if (plan$sort_desc) xtfrm(x) * -1 else xtfrm(x))
    result <- result[do.call(order, c(ord_args, list(na.last = TRUE))), , drop = FALSE]
  }
  if (plan$limit > 0L) result <- utils::head(result, plan$limit)
  rownames(result) <- NULL
  result_keys <- intersect(unique(c(plan$group_by, plan$duplicate_by)), names(result))
  blank_result_keys <- if (nrow(result)) result_keys[vapply(result_keys, function(nm) {
    x <- result[[nm]]
    all(is.na(x) | trimws(as.character(x)) == "")
  }, logical(1))] else character()
  if (length(blank_result_keys)) {
    plan$result_note <- paste0("All returned rows have missing/blank values for grouping column(s): ",
                               paste(blank_result_keys, collapse = ", "))
  }
  list(data = as.data.frame(result), plan = plan)
}

ai_query_r_string <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")

ai_query_script <- function(plan, source_name = "analysis_data") {
  qcols <- function(x) paste(vapply(x, ai_query_r_string, character(1)), collapse = ", ")
  lines <- c(
    "# Generated, validated, read-only query",
    sprintf("result <- %s", source_name)
  )
  for (f in plan$filters) {
    col <- sprintf(".data[[%s]]", ai_query_r_string(f$column))
    val <- ai_query_r_string(f$typed_value %||% f$value)
    expression <- switch(f$operator,
      equals = sprintf("!is.na(%s) & %s == %s", col, col, val),
      not_equals = sprintf("is.na(%s) | %s != %s", col, col, val),
      greater_than = sprintf("!is.na(%s) & %s > %s", col, col, val),
      greater_or_equal = sprintf("!is.na(%s) & %s >= %s", col, col, val),
      less_than = sprintf("!is.na(%s) & %s < %s", col, col, val),
      less_or_equal = sprintf("!is.na(%s) & %s <= %s", col, col, val),
      contains = sprintf("!is.na(%s) & grepl(%s, as.character(%s), ignore.case=TRUE, fixed=TRUE)", col, val, col),
      is_missing = sprintf("is.na(%s) | trimws(as.character(%s)) == \"\"", col, col),
      not_missing = sprintf("!is.na(%s) & trimws(as.character(%s)) != \"\"", col, col),
      `in` = sprintf("!is.na(%s) & %s %%in%% %s", col, col, val)
    )
    lines <- c(lines, sprintf("result <- dplyr::filter(result, %s)", expression))
  }
  keys <- unique(c(plan$group_by, plan$duplicate_by))
  if (length(plan$duplicate_by)) {
    lines <- c(lines,
      sprintf("result <- dplyr::filter(result, stats::complete.cases(dplyr::pick(dplyr::all_of(c(%s)))))", qcols(plan$duplicate_by)),
      sprintf("result <- dplyr::count(result, dplyr::across(dplyr::all_of(c(%s))), name=\"N_RECORDS\", .drop=FALSE)", qcols(keys)),
      "result <- dplyr::filter(result, .data$N_RECORDS > 1L)")
  } else if (length(keys)) {
    lines <- c(lines, sprintf("result <- dplyr::count(result, dplyr::across(dplyr::all_of(c(%s))), name=\"N_RECORDS\", .drop=FALSE)", qcols(keys)))
  } else if (plan$distinct) lines <- c(lines, "result <- dplyr::distinct(result)")
  if (length(plan$select)) lines <- c(lines, sprintf("result <- dplyr::select(result, dplyr::all_of(c(%s)), dplyr::any_of(\"N_RECORDS\"))", qcols(plan$select)))
  if (length(plan$sort_by)) {
    direction <- if (plan$sort_desc) "dplyr::desc" else "identity"
    lines <- c(lines, sprintf("result <- dplyr::arrange(result, dplyr::across(dplyr::all_of(c(%s)), %s))", qcols(plan$sort_by), direction))
  }
  if (plan$limit > 0L) lines <- c(lines, sprintf("result <- utils::head(result, %dL)", plan$limit))
  paste(lines, collapse = "\n")
}

ai_query_result_for_model <- function(result, max_json_chars) {
  json <- jsonlite::toJSON(result, dataframe = "rows", na = "null", digits = 10)
  list(reasonable = nchar(json, type = "bytes") <= max_json_chars,
       json_chars = nchar(json, type = "bytes"), json = json)
}
