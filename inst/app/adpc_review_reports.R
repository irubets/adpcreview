# Sourced in server scope. A report is an immutable snapshot of a completed run.
settings_table <- function(x) data.frame(Setting=names(x), Value=vapply(x,
  function(z) if(length(z)) paste(z,collapse=", ") else "Not mapped",character(1)),stringsAsFactors=FALSE)

output$review_mode_ui <- renderUI({
  req(rv$raw)
  nm <- names(rv$raw)
  parameter <- "PARAMCD" %in% toupper(nm) && !any(toupper(nm) %in% c("NRRLT","NFRLT","ARRLT","AFRLT"))
  tagList(
    selectInput("review_dataset_kind","Dataset review type",
      c("Concentration-time (ADPC / ADNCA)"="concentration","NCA parameters (PARAMCD / AVAL)"="parameter"),
      selected=if(parameter) "parameter" else "concentration"),
    conditionalPanel("input.review_dataset_kind === 'parameter'",
      selectInput("parameter_subject","Parameter subject",nm,selected=pick_dataset_variable_v13(nm,c("USUBJID","SUBJID"))),
      selectInput("parameter_code","Parameter code",nm,selected=pick_dataset_variable_v13(nm,"PARAMCD")),
      selectInput("parameter_value","Parameter value",nm,selected=pick_dataset_variable_v13(nm,"AVAL")),
      selectizeInput("parameter_keys","Unique parameter record key (include repeat occasions)",nm,multiple=TRUE,
        selected=nm[toupper(nm) %in% c("STUDYID","USUBJID","PARAMCD","APERIOD","AVISIT","ATPT","ADT","ADTM","TRTA")]),
      tags$p(class="help-note","Subject and parameter are always included in the key. Add period, visit, treatment, occasion or other study-specific identifiers. Parameter mode provides summary checks and listings; concentration plots and NCA recalculation are unavailable.")))
})

committed_settings <- function(mappings,kind,keys=character()) {
  settings_table(c(list(dataset_kind=kind,unique_parameter_key=keys),mappings,
    if(kind=="concentration") list(pctdev_threshold=input$pctdev_threshold %||% 20,
      absdev_threshold=input$absdev_threshold %||% .5, enable_iqr=isTRUE(input$enable_iqr),
      iqr_multiplier=input$iqr_multiplier %||% 1.5,early_window=input$early_window %||% 2,
      predose_neg_threshold=input$predose_neg_threshold %||% -.5) else list()))
}

snapshot_report <- function(summary,sections,quality,mappings,kind,scope,plot_data=data.frame()) {
  quality_sections <- adpc_quality_sections(quality)
  counts <- list(
    list(text=paste("Loaded records:",nrow(rv$raw),"| Selected analysis scope:",nrow(scope),
      "| User-filtered out:",nrow(rv$raw)-nrow(scope)),kind="plain",section_id=""),
    list(text=paste("Source-file SHA256:",rv$source_sha256 %||% "Unavailable (reload source file)"),kind="plain",section_id=""))
  for(i in seq_along(quality_sections)) {
    s <- quality_sections[[i]]
    counts[[length(counts)+1L]] <- list(text=paste(s$title,":",s$description),kind="quality",section_id=s$id)
  }
  overview <- list(id="scope-settings",group="Audits",title="Committed scope and mappings",
    description=paste("Run review commits settings and scope. Source values are preserved. Filter:",
      if(is.null(rv$active_filter_var)) "None" else paste(rv$active_filter_var,"==",rv$active_filter_val),
      ". Review, PK-profile and NCA scopes are independent. No NCA calculation is performed by Run review."),
    rule="The selected scope is a copy of loaded records after the committed user filter. Missing nominal/actual/analyte/visit values are excluded by the legacy concentration timing engine; their source rows remain in the quality checks and excluded-record listing. Parameter mode retains all selected records. Separate PKNCA results require their own assessment and are not represented as completed by this report.",
    tables=list(list(title="Committed mappings and settings",data=committed_settings(mappings,kind,quality$keys))))
  rv$review_snapshot <- list(title=if(kind=="parameter") "ADNCA parameter review" else "ADPC / ADNCA concentration review",
    generated=format(Sys.time(),"%Y-%m-%dT%H:%M%z"),source=rv$source_path %||% "local dataset",
    source_sha256=rv$source_sha256 %||% "Unavailable (reload source file)",
    summary_items=c(summary,counts),rules_settings=overview$tables[[1]]$data,
    sections=c(list(overview),sections,quality_sections),plot_data=plot_data,kind=kind)
  rv$review_saved_path <- NULL
}

commit_concentration_report <- function(scope) {
  mappings <- rv$review_mappings
  assessment <- profile_assessment()
  grouping <- grouping_values_audit(scope,mapped=list(analyte=mappings$analyte,avisit=mappings$visit,
    visitcd=input$visitcd_var,nrrlt=mappings$nominal))
  ids <- subject_id_relationship(scope)
  audits <- list(
    list(id="audit-grouping-values",group="Audits",title="Grouping values",
      description="Grouping audit within the committed analysis scope. Missing variables are disclosed.",
      rule="Lists available mapped/grouping values, missingness and consistency warnings; observed categories are not protocol-approved categories.",
      tables=list(list(title="Grouping values",data=grouping))),
    list(id="audit-profile-timepoints",group="Audits",title="Profile time points",
      description=assessment$message,rule="Candidate subject/analyte/profile/visit/reference keys are assessed against nominal-time dose cycles. Candidate and selected-key tables disclose missing/ambiguous profiles. This is a structural screen, not dose-history validation.",
      tables=list(list(title="Profile time points",data=assessment$timepoints %||% data.frame()),
        list(title="Candidate keys",data=assessment$candidates %||% data.frame()))),
    list(id="audit-subject-id-mapping",group="Audits",title="Subject ID mapping",
      description=if(!ids$available) "Not assessable: USUBJID and SUBJID are required." else if(isTRUE(ids$one_to_one)) "USUBJID:SUBJID is 1:1." else "Conflicting or missing identifiers require review.",
      rule="Checks distinct USUBJID/SUBJID pairs in both directions. Missing IDs and multiple partners are reported, without correction.",
      tables=list(list(title="Subject identifier exceptions",data=ids$table %||% data.frame()))))
  rules <- c(
    flagged_records="OR of enabled timing and duplicate record flags. Only records with the triggering condition are highlighted; complete affected profiles are retained.",
    iqr_outlier="Within analyte/visit/visit-code/nominal time, at least two actual times are required. Quantile type 2; flag strictly outside Q1 - multiplier*IQR or Q3 + multiplier*IQR. Optional timing screen, not concentration outliers.",
    large_pct="100 * abs(actual - nominal) / abs(nominal) strictly exceeds committed percentage threshold; nominal zero is not assessable by this rule.",
    large_abs="abs(actual - nominal) strictly exceeds committed absolute threshold. Mapped time units must agree.",
    predose_postdose="Nominal time equals zero and calculated/actual time is greater than zero.",
    postdose_predose="Nominal time is greater than zero and calculated/actual time is less than or equal to zero.",
    predose_early="Nominal time equals zero and actual time is strictly below the committed predose early threshold.",
    relative_mismatch="Both analysis-relative and actual times are present and differ by exact numeric inequality. Rounding and planned adjustments can be legitimate; no correction is inferred.",
    duplicate_nominal="Within subject/analyte/visit/visit-code, highlight records carrying repeated nominal values. Retain the rest of the affected profile as context.",
    duplicate_relative="Within subject/analyte/visit/visit-code, highlight records carrying repeated nonmissing analysis-relative times. Missing times are not duplicate findings.")
  findings <- lapply(names(review_finding_specs),function(id) {
    z <- review_finding_result(id)
    available <- z$criterion_available && !(id %in% c("relative_mismatch","duplicate_relative") && !nzchar(mappings$mrrlt %||% "")) &&
      !(id=="iqr_outlier" && !isTRUE(input$enable_iqr))
    list(id=paste0("finding-",id),group="Review findings",title=z$title,
      description=if(!available) "Not assessable: required mapping or enabled screen is unavailable." else
        paste(z$interest_records,"triggering record(s) in",z$profiles,"complete affected profiles;",nrow(z$data),"context records."),
      rule=unname(rules[id]),tables=list(list(title="Complete affected profiles",data=z$data,interest_column="RECORD_OF_INTEREST")))
  })
  # Reconcile on source positions before the legacy timing filter, without adding
  # identifiers to source data or relying on potentially duplicated clinical keys.
  numeric_ok <- function(col) is.finite(suppressWarnings(as.numeric(as.character(scope[[col]]))))
  retained <- !is.na(scope[[mappings$analyte]]) & !is.na(scope[[mappings$visit]]) &
    numeric_ok(mappings$nominal) & numeric_ok(mappings$actual)
  excluded <- scope[!retained,,drop=FALSE]
  audits[[length(audits)+1L]] <- list(id="timing-excluded",group="Audits",title="Records excluded from timing review",
    description=paste(nrow(excluded),"selected records absent from timing review. All selected rows are evaluated by source-quality checks."),
    rule="The timing engine requires nonmissing mapped analyte/visit and finite nominal/actual times. Excluded source records are listed without imputation or fabricated samples.",
    tables=list(list(title="Timing-excluded source records",data=excluded)))
  keys <- assessment$selected$key_cols %||% intersect(c(mappings$subject,mappings$analyte,mappings$visit),names(scope))
  quality <- adpc_quality_checks(scope,mappings,"concentration",keys=character())
  quality$keys <- intersect(keys,names(scope))
  summary <- review_summary_items()
  for (finding in findings) if (startsWith(finding$description,"Not assessable")) {
    for (i in seq_along(summary)) if (identical(summary[[i]]$section_id,finding$id))
      summary[[i]]$text <- paste(finding$title,":",finding$description)
  }
  snapshot_report(summary,c(audits,findings),quality,mappings,"concentration",scope,format_review_listing(rv$review$data))
}

run_parameter_review <- function() {
  mappings <- list(subject=input$parameter_subject,parameter=input$parameter_code,value=input$parameter_value)
  if(any(!vapply(mappings,function(x) length(x)==1 && !is.na(x) && x %in% names(rv$raw),logical(1)))) {
    showNotification("Map parameter subject, code and value before running review.",type="error"); return()
  }
  scope <- rv$raw
  rv$active_filter_var <- rv$active_filter_val <- rv$active_filter_n <- NULL
  if(identical(input$filter_enabled,"yes") && (input$filter_var %||% "") %in% names(scope)) {
    x <- scope[[input$filter_var]]; value <- input$filter_val %||% "Y"
    keep <- if(value=="__NULL__") is.na(x) | trimws(as.character(x))=="" else !is.na(x) & as.character(x)==value
    scope <- scope[keep,,drop=FALSE]
    rv$active_filter_var <- input$filter_var; rv$active_filter_val <- value; rv$active_filter_n <- nrow(scope)
  }
  keys <- unique(c(mappings$subject,mappings$parameter,intersect(input$parameter_keys,names(scope))))
  quality <- adpc_quality_checks(scope,mappings,"parameter",keys)
  rv$analysis_data <- scope
  rv$review <- list(data=scope,kind="parameter")
  rv$review_mappings <- mappings
  counts <- data.frame(PARAMETER=as.character(scope[[mappings$parameter]]),stringsAsFactors=FALSE)
  counts <- as.data.frame(table(counts$PARAMETER,useNA="ifany"),stringsAsFactors=FALSE)
  names(counts) <- c("PARAMCD","RECORDS")
  summary <- list(list(text=paste("Parameter review ready. Records:",nrow(scope),"| Subjects:",
    length(unique(scope[[mappings$subject]])),"| Parameters:",length(unique(scope[[mappings$parameter]]))),kind="plain",section_id=""),
    list(text=paste("Committed parameter record key:",paste(keys,collapse=" + ")),kind="plain",section_id=""),
    list(text="Concentration timing, plots and NCA recalculation do not apply to parameter-level records. No physiological bounds, expected parameter inventory or protocol eligibility is inferred.",kind="plain",section_id=""))
  sections <- list(list(id="parameter-inventory",group="Audits",title="Parameter inventory",
    description="Observed parameter counts; absent expected parameters cannot be inferred without a study specification.",
    rule="Count selected rows by mapped parameter code, including missing codes. Negative/zero values are not generically invalid for every NCA parameter; no universal range is applied.",
    tables=list(list(title="Observed parameters",data=counts))),
    list(id="parameter-data",group="Listings",title="Complete parameter data",
      description="All selected parameter records, preserving source values.",rule="Applies only the committed user filter; missing and nonfinite analysis results are retained.",
      tables=list(list(title="Parameter data",data=scope))))
  snapshot_report(summary,sections,quality,mappings,"parameter",scope)
  session$sendCustomMessage("adpc-review-kind",TRUE)
  updateTabsetPanel(session,"main_tabs",selected="Review summary")
}

output$status_msg <- renderUI({
  if(is.null(rv$raw)) return(tags$p("Load a dataset, choose its review type, map variables and click Run review."))
  if(is.null(rv$review_snapshot)) return(tags$p("Dataset loaded. Run review to create the summary and complete reports."))
  tagList(lapply(rv$review_snapshot$summary_items,function(item) {
    if(!nzchar(item$section_id %||% "")) return(div(class="review-summary-line",item$text))
    id <- item$section_id
    div(class="review-summary-line",tags$button(type="button",class="review-summary-link",
      style="border:0;background:none;padding:0;text-align:left;font:inherit;cursor:pointer;",
      onclick=sprintf("var node=document.getElementById('report-%s'); if(node){node.open=true;node.scrollIntoView({block:'start'});} return false;",id),item$text))
  }),tags$p(class="help-note","This summary and its reports use the last completed run. After changing mappings, thresholds or filters, click Run review again."))
})

output$review_details_ui <- renderUI({
  req(rv$review_snapshot)
  tagList(lapply(rv$review_snapshot$sections,function(s) {
    tags$details(id=paste0("report-",s$id),tags$summary(s$title),tags$p(s$description),tags$p(s$rule %||% ""),
      lapply(seq_along(s$tables),function(i) tagList(tags$h5(s$tables[[i]]$title),DTOutput(paste0("report_table_",s$id,"_",i)))))
  }))
})
observeEvent(rv$review_snapshot,{
  req(rv$review_snapshot)
  for(s in rv$review_snapshot$sections) for(i in seq_along(s$tables)) local({
    section_id <- s$id; table_index <- i
    output[[paste0("report_table_",section_id,"_",table_index)]] <- renderDT({
      sections <- rv$review_snapshot$sections
      section <- Filter(function(z) identical(z$id,section_id),sections)
      req(length(section)>0)
      table <- section[[1]]$tables[[table_index]]
      widget <- review_datatable(table$data,rownames=FALSE,filter="top",options=list(pageLength=25,scrollX=TRUE))
      if("RECORD_OF_INTEREST" %in% names(table$data)) widget <- DT::formatStyle(widget,"RECORD_OF_INTEREST",target="row",backgroundColor=DT::styleEqual("YES","#FFF3CD"))
      widget
    },server=TRUE)
  })
},ignoreNULL=TRUE)

review_report_payload <- reactive({ req(rv$review_snapshot); rv$review_snapshot })

# Export controls and writers are shared with the PC app's tested architecture.
  output$review_html_download_ui <- renderUI({
    wait_note <- tags$div(
      role = "note", class = "review-export-wait",
      style = paste("margin-top:12px; max-width:900px; padding:12px 16px;",
        "background:#fff0f0; border:1px solid #d9534f; border-left:6px solid #b42318;",
        "border-radius:5px; color:#9f1c16; font-size:16px;"),
      icon("exclamation-triangle"), " ", tags$strong("Please wait after clicking: "),
      "building a complete HTML ZIP or Excel workbook can take a few minutes for a large dataset. Keep this app open until the saved file path appears or the browser download begins."
    )
    if (is.null(rv$review)) {
      return(tagList(
        tags$div(
          tags$button(
            type = "button", class = "btn btn-default", disabled = "disabled",
            "Save HTML review (.zip)"
          ),
          tags$button(
            type = "button", class = "btn btn-default", disabled = "disabled",
            style = "margin-left:8px;", "Save Excel review (.xlsx)"
          )
        ),
        wait_note
      ))
    }
    tagList(
      tags$div(
        if (!identical(input$review_save_local, FALSE)) {
          tagList(
            actionButton("review_html_save_local", "Save HTML review (.zip)",
              icon = icon("save"), class = "btn-primary"),
            actionButton("review_excel_save_local", "Save Excel review (.xlsx)",
              icon = icon("save"), class = "btn-success", style = "margin-left:8px;")
          )
        } else {
          tagList(
            downloadButton("review_html_download", "Save HTML review (.zip)", class = "btn-primary"),
            downloadButton("review_excel_download", "Save Excel review (.xlsx)",
              class = "btn-success", style = "margin-left:8px;")
          )
        }
      ),
      wait_note,
      tags$div(class = "help-note",
        "HTML: the ZIP contains a linked multi-page report. Excel: one workbook contains a Summary/TOC, one filterable worksheet per audit or finding, highlighted records of interest, and plot-ready concentration data (concentration mode).")
    )
  })

  review_download_state <- new.env(parent = emptyenv())
  save_review_locally <- function(kind) {
    req(rv$review)
    tryCatch({
      source <- clean_local_path(rv$source_path)
      if (is.null(source) || !nzchar(source) || !file.exists(source)) {
        stop("The loaded local dataset file is unavailable. Use browser downloads or reload the local dataset file.")
      }
      directory <- dirname(normalizePath(source, winslash = "/", mustWork = TRUE))
      bundle_name <- review_report_bundle_name(rv$raw, source)
      filename <- if (kind == "html") paste0(bundle_name, ".zip") else review_excel_filename(rv$raw, source)
      destination <- file.path(directory, filename)
      suffix <- 1L
      while (file.exists(destination)) {
        suffix <- suffix + 1L
        destination <- file.path(directory, paste0(tools::file_path_sans_ext(filename),
          "_", suffix, ".", tools::file_ext(filename)))
      }
      temporary <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
      on.exit(unlink(temporary), add = TRUE)
      withProgress(message = "Saving complete review", value = 0, {
        payload <- review_report_payload()
        incProgress(0.2, detail = "Building report; please keep the app open")
        if (kind == "html") write_review_html_bundle(temporary, payload, bundle_name)
        else write_review_excel_report(temporary, payload)
        incProgress(0.7, detail = "Saving beside the source file")
        if (!file.copy(temporary, destination, overwrite = FALSE)) {
          stop("Could not save in the source file folder. Check folder permissions or use browser downloads.")
        }
      })
      rv$review_saved_path <- destination
      showNotification(paste("Review saved:", destination), type = "message", duration = 15)
    }, error = function(e) {
      showNotification(paste("Review save failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  }
  observeEvent(input$review_html_save_local, save_review_locally("html"))
  observeEvent(input$review_excel_save_local, save_review_locally("excel"))
  output$review_saved_location <- renderText({
    if (is.null(rv$review_saved_path)) "" else paste("Last saved review:", rv$review_saved_path)
  })
  output$review_html_download <- downloadHandler(
    filename = function() {
      req(rv$review)
      review_download_state$bundle_name <- review_report_bundle_name(rv$raw, rv$source_path)
      paste0(review_download_state$bundle_name, ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      req(rv$review)
      bundle_name <- review_download_state$bundle_name %||%
        review_report_bundle_name(rv$raw, rv$source_path)
      write_review_html_bundle(file, review_report_payload(), bundle_name)
      review_download_state$bundle_name <- NULL
    }
  )

  output$review_excel_download <- downloadHandler(
    filename = function() {
      req(rv$review)
      review_excel_filename(rv$raw, rv$source_path)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      req(rv$review)
      write_review_excel_report(file, review_report_payload())
    }
  )

  # These download links are inserted by renderUI; keep their URLs registered
  # even before the browser has bound or displayed the dynamic buttons.
  for (id in c("review_html_download", "review_excel_download")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
