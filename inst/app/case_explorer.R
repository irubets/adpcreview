# Deterministic subject/case explorer shared by local-Qwen and blinded-GPT builds.

case_find_col <- function(df, candidates) {
  hit <- names(df)[match(toupper(candidates), toupper(names(df)), nomatch = 0L)]
  hit <- hit[nzchar(hit)]
  if (length(hit)) hit[1] else NULL
}

case_mapped_col_v13 <- function(df, mapped, role, candidates) {
  selected <- mapped[[role]]
  if (!is.null(selected) && length(selected) && !is.na(selected[1]) &&
      nzchar(trimws(selected[1])) && selected[1] %in% names(df)) return(selected[1])
  case_find_col(df, candidates)
}

case_schema_v13 <- function(df, mapped = list()) {
  list(
    subject = case_mapped_col_v13(df, mapped, "subject", c("USUBJID", "SUBJID")),
    analyte = case_mapped_col_v13(df, mapped, "analyte", c("ATEST", "PCTEST", "PCTESTCD", "PARAM", "PARAMCD")),
    visit = case_mapped_col_v13(df, mapped, "visit", c("AVISIT", "VISIT", "VISITCD")),
    profile = case_mapped_col_v13(df, mapped, "profile", c("APROFILE", "PROFILE")),
    aval = case_mapped_col_v13(df, mapped, "aval", c("AVAL", "PCSTRESN")),
    mrrlt = case_mapped_col_v13(df, mapped, "mrrlt", c("MRRLT", "MFRLT")),
    nrrlt = case_mapped_col_v13(df, mapped, "nrrlt", c("NRRLT", "NFRLT", "PCTPTNUM")),
    arrlt = case_mapped_col_v13(df, mapped, "arrlt", c("ARRLT", "AFRLT"))
  )
}

case_flag_columns_v13 <- function(df) {
  names(df)[grepl("^(CRIT[0-9]+FL|NCA[0-9]+X?FL|ANL[0-9]+FL|PKSUMXFL)$",
                  names(df), ignore.case = TRUE)]
}

case_clean_flag <- function(x) {
  y <- toupper(trimws(as.character(x)))
  y[is.na(y) | !nzchar(y)] <- "(EMPTY)"
  y
}

case_profile_plot_v13 <- function(df, time_col, aval_col, title = "") {
  stopifnot(is.data.frame(df), time_col %in% names(df), aval_col %in% names(df))
  x <- suppressWarnings(as.numeric(df[[time_col]]))
  y <- suppressWarnings(as.numeric(df[[aval_col]]))
  ord <- order(x, seq_along(x), na.last = TRUE)

  plotly::plot_ly(
    x = x[ord], y = y[ord], type = "scatter", mode = "lines+markers",
    line = list(color = "#1565C0"),
    marker = list(color = "#D84315", size = 8),
    text = paste0(time_col, ": ", x[ord], "<br>", aval_col, ": ", y[ord]),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = list(text = title),
      xaxis = list(title = time_col),
      yaxis = list(title = aval_col)
    )
}

case_criterion_choices_v13 <- function(df, mapped = list()) {
  sc <- case_schema_v13(df, mapped)
  choices <- c()
  time_actual <- sc$mrrlt %||% sc$arrlt
  if (!is.null(sc$subject)) choices <- c(choices, "Duplicate subject/case/time records" = "duplicate_time")
  if (!is.null(sc$nrrlt) && !is.null(time_actual)) {
    choices <- c(choices,
      "Pre-dose nominal sample taken post-dose" = "predose_postdose",
      "Post-dose nominal sample taken pre-dose" = "postdose_predose")
  }
  for (flag in case_flag_columns_v13(df)) {
    label <- attr(df[[flag]], "label") %||% ""
    display <- if (nzchar(label)) paste0(flag, " = Y — ", label) else paste0(flag, " = Y")
    choices[display] <- paste0("flag:", flag)
  }
  choices
}

case_key_columns_v13 <- function(df, mode = "auto", mapped = list()) {
  sc <- case_schema_v13(df, mapped)
  if (is.null(sc$subject)) return(character())
  if (identical(mode, "profile") && !is.null(sc$profile)) return(unique(c(sc$subject, sc$profile)))
  if (identical(mode, "analyte_visit") && !is.null(sc$analyte) && !is.null(sc$visit)) {
    return(unique(c(sc$subject, sc$analyte, sc$visit)))
  }
  if (!is.null(sc$profile)) unique(c(sc$subject, sc$profile)) else
    unique(c(sc$subject, sc$analyte, sc$visit))
}

case_match_mask_v13 <- function(df, criteria, logic = "any", key = NULL, mapped = list()) {
  if (!length(criteria)) return(rep(TRUE, nrow(df)))
  if (is.null(key)) key <- case_key_columns_v13(df, mapped = mapped)
  sc <- case_schema_v13(df, mapped)
  actual <- sc$mrrlt %||% sc$arrlt
  masks <- lapply(criteria, function(criterion) {
    if (identical(criterion, "duplicate_time")) {
      time_col <- sc$mrrlt %||% sc$nrrlt %||% sc$arrlt
      cols <- unique(c(key, time_col))
      if (!length(cols)) return(rep(FALSE, nrow(df)))
      return(duplicated(df[cols]) | duplicated(df[cols], fromLast = TRUE))
    }
    if (identical(criterion, "predose_postdose")) {
      if (is.null(sc$nrrlt) || is.null(actual)) return(rep(FALSE, nrow(df)))
      return(!is.na(df[[sc$nrrlt]]) & !is.na(df[[actual]]) &
               as.numeric(df[[sc$nrrlt]]) == 0 & as.numeric(df[[actual]]) > 0)
    }
    if (identical(criterion, "postdose_predose")) {
      if (is.null(sc$nrrlt) || is.null(actual)) return(rep(FALSE, nrow(df)))
      return(!is.na(df[[sc$nrrlt]]) & !is.na(df[[actual]]) &
               as.numeric(df[[sc$nrrlt]]) > 0 & as.numeric(df[[actual]]) <= 0)
    }
    if (startsWith(criterion, "flag:")) {
      flag <- sub("^flag:", "", criterion)
      if (!(flag %in% names(df))) return(rep(FALSE, nrow(df)))
      return(case_clean_flag(df[[flag]]) == "Y")
    }
    rep(FALSE, nrow(df))
  })
  if (identical(logic, "all")) Reduce(`&`, masks) else Reduce(`|`, masks)
}

case_filtered_records_v13 <- function(df, criteria, logic = "any", subject_search = "", grain = "auto", mapped = list()) {
  key <- case_key_columns_v13(df, grain, mapped)
  if (!length(key)) return(df[0, , drop = FALSE])
  signatures <- do.call(paste, c(lapply(df[key], as.character), sep = "\r"))
  if (length(criteria)) {
    if (identical(logic, "case_all")) {
      case_sets <- lapply(criteria, function(z) unique(signatures[case_match_mask_v13(df, z, "any", key, mapped)]))
      matching_cases <- Reduce(intersect, case_sets)
    } else {
      row_match <- case_match_mask_v13(df, criteria, logic, key, mapped)
      matching_cases <- unique(signatures[row_match])
    }
    keep <- signatures %in% matching_cases
  } else keep <- rep(TRUE, nrow(df))
  sc <- case_schema_v13(df, mapped)
  if (nzchar(trimws(subject_search)) && !is.null(sc$subject)) {
    keep <- keep & grepl(trimws(subject_search), as.character(df[[sc$subject]]), ignore.case = TRUE)
  }
  df[keep, , drop = FALSE]
}

case_summary_v13 <- function(df, grain = "auto", mapped = list()) {
  key <- case_key_columns_v13(df, grain, mapped)
  if (!length(key) || !nrow(df)) return(data.frame())
  sc <- case_schema_v13(df, mapped)
  split_key <- interaction(df[key], drop = TRUE, lex.order = TRUE)
  rows <- lapply(split(seq_len(nrow(df)), split_key), function(idx) {
    x <- df[idx, , drop = FALSE]
    out <- as.list(x[1, key, drop = FALSE])
    out$N_RECORDS <- nrow(x)
    out$N_DISTINCT_TIMES <- if (!is.null(sc$mrrlt)) length(unique(x[[sc$mrrlt]][!is.na(x[[sc$mrrlt]])])) else
      if (!is.null(sc$nrrlt)) length(unique(x[[sc$nrrlt]][!is.na(x[[sc$nrrlt]])])) else NA_integer_
    out$N_AVAL <- if (!is.null(sc$aval)) sum(!is.na(x[[sc$aval]])) else NA_integer_
    as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  })
  do.call(rbind, rows)
}

case_ai_payload_v13 <- function(df, max_cases = 20L, max_rows = 200L, grain = "auto", mapped = list()) {
  if (!nrow(df)) return(list(message = "No matching case records"))
  sc <- case_schema_v13(df, mapped)
  key <- case_key_columns_v13(df, grain, mapped)
  flags <- case_flag_columns_v13(df)
  cols <- unique(c(key, sc$visit, sc$profile, sc$nrrlt, sc$mrrlt, sc$arrlt, sc$aval, flags))
  cols <- cols[!is.na(cols) & nzchar(cols) & cols %in% names(df)]
  signatures <- do.call(paste, c(lapply(df[key], as.character), sep = "\r"))
  chosen <- head(unique(signatures), max_cases)
  payload <- head(df[signatures %in% chosen, cols, drop = FALSE], max_rows)
  list(
    grain = key,
    selected_case_count = length(unique(signatures)),
    included_case_count = length(chosen),
    included_row_count = nrow(payload),
    truncated = length(unique(signatures)) > max_cases || sum(signatures %in% chosen) > max_rows,
    records = payload
  )
}

case_explorer_ui_v13 <- function(ai_mode = c("qwen", "gpt")) {
  ai_mode <- match.arg(ai_mode)
  tagList(
    tags$p(class = "help-note",
      "Find subject-level ADPC or ADNCA cases using timing rules and CRIT/NCA/ANL/PKSUM flags. Tables and plots are calculated locally."),
    fluidRow(
      column(3, uiOutput("case_criteria_ui")),
      column(2, selectInput("case_grain", "Case definition", c("Auto" = "auto", "Subject + analyte + visit" = "analyte_visit", "Subject + profile" = "profile"))),
      column(2, selectInput("case_logic", "Combine criteria", c("Any criterion (OR)" = "any", "All on same record" = "all", "All somewhere in case" = "case_all"), selected = "any")),
      column(3, textInput("case_subject_search", "Subject contains", placeholder = "Optional blinded ID")),
      column(2, uiOutput("case_time_ui"))
    ),
    numericInput("case_plot_limit", "Maximum profile figures", value = 12, min = 1, max = 40, width = "220px"),
    div(class = "ai-toolbar", textOutput("case_status", inline = TRUE),
      downloadButton("case_summary_download", "Download cases CSV"),
      downloadButton("case_records_download", "Download records CSV")),
    tabsetPanel(
      tabPanel("Cases", DTOutput("case_summary_table")),
      tabPanel("Records", DTOutput("case_records_table")),
      tabPanel("Concentration–time profiles", uiOutput("case_plots_ui")),
      tabPanel("AI case context",
        tags$p(class = "help-note", if (ai_mode == "qwen")
          "Optional: include up to 20 matching cases / 200 rows in Qwen chat. The endpoint is restricted to local Ollama."
          else "Optional: include up to 10 matching cases / 100 rows in GPT chat. Use only a suitably blinded dataset."),
        if (ai_mode == "gpt") checkboxInput("case_blinded_confirm",
          "I confirm that subject identifiers and case data are appropriately blinded for external API processing", FALSE) else
          checkboxInput("case_local_confirm",
            "I understand that the bounded case records will be sent to the local Ollama process", FALSE),
        checkboxInput("case_include_ai", "Include current matching cases in the next AI questions", FALSE),
        verbatimTextOutput("case_ai_preview")
      )
    )
  )
}

case_explorer_server_v13 <- function(input, output, session, raw_reactive,
                                     mapping_reactive = function() list(),
                                     ai_mode = c("qwen", "gpt")) {
  ai_mode <- match.arg(ai_mode)
  output$case_criteria_ui <- renderUI({
    d <- raw_reactive()
    req(d)
    choices <- case_criterion_choices_v13(d, mapping_reactive())
    selectizeInput("case_criteria", "Criteria", choices = choices, multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Choose one or more criteria"))
  })
  output$case_time_ui <- renderUI({
    d <- raw_reactive(); req(d)
    sc <- case_schema_v13(d, mapping_reactive())
    choices <- c()
    if (!is.null(sc$mrrlt)) choices[paste0("Relative time (", sc$mrrlt, ")")] <- "mrrlt"
    if (!is.null(sc$nrrlt)) choices[paste0("Nominal time (", sc$nrrlt, ")")] <- "nrrlt"
    selectInput("case_time", "Plot time", choices)
  })
  records <- reactive({
    d <- raw_reactive()
    req(d)
    case_filtered_records_v13(d, input$case_criteria %||% character(),
      input$case_logic %||% "any", input$case_subject_search %||% "", input$case_grain %||% "auto",
      mapping_reactive())
  })
  summaries <- reactive(case_summary_v13(records(), input$case_grain %||% "auto", mapping_reactive()))
  output$case_status <- renderText({
    sprintf("%s matching cases | %s records",
      format(nrow(summaries()), big.mark = ","), format(nrow(records()), big.mark = ","))
  })
  output$case_summary_table <- renderDT({
    d <- summaries()
    validate(need(nrow(d), "No matching cases."))
    DT::datatable(d, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 15))
  })
  output$case_records_table <- renderDT({
    d <- records()
    validate(need(nrow(d), "No matching records."))
    DT::datatable(d, rownames = FALSE, filter = "top", options = list(scrollX = TRUE, pageLength = 15))
  })
  output$case_summary_download <- downloadHandler(
    filename = function() paste0("case_summary_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(summaries(), file, row.names = FALSE, na = "")
  )
  output$case_records_download <- downloadHandler(
    filename = function() paste0("case_records_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(records(), file, row.names = FALSE, na = "")
  )
  output$case_plots_ui <- renderUI({
    d <- records(); req(nrow(d))
    sc <- case_schema_v13(d, mapping_reactive())
    key <- case_key_columns_v13(d, input$case_grain %||% "auto", mapping_reactive())
    validate(need(!is.null(sc$aval), "AVAL was not detected."))
    time_col <- if (identical(input$case_time, "nrrlt")) sc$nrrlt else sc$mrrlt
    validate(need(!is.null(time_col), paste(toupper(input$case_time %||% "mrrlt"), "was not detected.")))
    sig <- do.call(paste, c(lapply(d[key], as.character), sep = " | "))
    selected <- head(unique(sig), input$case_plot_limit %||% 12L)
    panels <- lapply(seq_along(selected), function(i) {
      s <- selected[i]; pd <- d[sig == s, , drop = FALSE]
      oid <- paste0("case_plot_", i)
      local({
        oid_ <- oid; pd_ <- pd; s_ <- s; time_ <- time_col; aval_ <- sc$aval
        output[[oid_]] <- renderPlotly({
          case_profile_plot_v13(pd_, time_, aval_, s_)
        })
      })
      div(style = "background:#fff;border:1px solid #dde3ec;border-radius:8px;margin-bottom:12px;padding:8px;",
          plotlyOutput(oid, height = "380px"))
    })
    tagList(panels)
  })
  payload <- reactive({
    max_cases <- if (ai_mode == "gpt") 10L else 20L
    max_rows <- if (ai_mode == "gpt") 100L else 200L
    case_ai_payload_v13(records(), max_cases, max_rows, input$case_grain %||% "auto", mapping_reactive())
  })
  output$case_ai_preview <- renderText({
    p <- payload()
    paste(capture.output(str(p, max.level = 3, give.attr = FALSE)), collapse = "\n")
  })
  ai_context <- reactive({
    if (!isTRUE(input$case_include_ai)) return(NULL)
    confirmed <- if (ai_mode == "gpt") isTRUE(input$case_blinded_confirm) else isTRUE(input$case_local_confirm)
    if (!confirmed) return(list(blocked = TRUE, reason = "Required case-data confirmation was not selected."))
    payload()
  })
  list(records = records, ai_context = ai_context, bounded_context = payload)
}
