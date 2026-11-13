app_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "adpc_review.R"))) {
  app_dir <- normalizePath(file.path(app_dir, ".."), mustWork = TRUE)
}
app_env <- new.env(parent = globalenv())
sys.source(file.path(app_dir, "adpc_review.R"), envir = app_env)

ui_html <- htmltools::renderTags(app_env$ui)$html
stopifnot(
  !grepl("file_upload", ui_html, fixed = TRUE),
  !grepl("Upload dataset", ui_html, fixed = TRUE),
  grepl("ai_data_access_ui", ui_html, fixed = TRUE),
  grepl("Save current Review selection", ui_html, fixed = TRUE),
  grepl("grouping_audit_tabs", ui_html, fixed = TRUE)
)

finding_source <- data.frame(
  PROFILE = c("P1", "P1", "P1", "P2", "P2"),
  TIME = c(0, 1, 2, 0, 1),
  VALUE = 1:5,
  stringsAsFactors = FALSE
)
finding <- app_env$build_review_finding_subset(
  finding_source, c(FALSE, TRUE, FALSE, FALSE, FALSE), "PROFILE"
)
stopifnot(
  nrow(finding$data) == 3L,
  finding$profiles == 1L,
  finding$interest_records == 1L,
  sum(finding$data$RECORD_OF_INTEREST == "YES") == 1L,
  all(finding$data$PROFILE == "P1")
)

unordered <- data.frame(
  EXTRA = 1, AFRLT = 1, SUBJID = "S1", MFRLT = 1, AVISIT = "V1",
  RECORD_OF_INTEREST = "YES", PCRFTDTM = "2026-01-01T08:00",
  NFRLT = 1, USUBJID = "STUDY-S1", ARRLT = 1, APROFILE = "P1",
  ADTM = "2026-01-01T09:00", ARMCD = "A", MRRLT = 1, ATEST = "Drug",
  NRRLT = 1, stringsAsFactors = FALSE
)
ordered <- app_env$order_review_finding_columns(
  unordered,
  key_cols = c("USUBJID", "ATEST", "AVISIT", "ARMCD"),
  mappings = list(
    analyte = "ATEST", visit = "AVISIT", nominal = "NFRLT",
    mrrlt = "MFRLT", actual = "AFRLT", sample_time = "ADTM",
    dose_time = "PCRFTDTM", profile = "APROFILE"
  )
)
stopifnot(identical(
  names(ordered)[seq_len(15)],
  c(
    "RECORD_OF_INTEREST", "USUBJID", "SUBJID", "ATEST", "AVISIT", "ARMCD",
    "NRRLT", "NFRLT", "MRRLT", "MFRLT", "ARRLT", "AFRLT", "ADTM",
    "PCRFTDTM", "APROFILE"
  )
))

shiny::testServer(app_env$server, {
  dat <- data.frame(
    USUBJID = c("U1", "U2"), SUBJID = c("S1", "S2"),
    PCTESTCD = "A", AVISIT = "V",
    NFRLT = c(0, 1), AFRLT = c(0, 1), MFRLT = c(0, 1), AVAL = c(2, 3),
    stringsAsFactors = FALSE
  )
  review_dat <- transform(
    dat,
    .id = USUBJID, .pctest = PCTESTCD, .avisit = AVISIT,
    .visitcd = NA_character_, .nrrlt = NFRLT, .arrlt = AFRLT,
    .mrrlt = MFRLT, .aval = AVAL, visit_display = AVISIT,
    review_fl = c("Y", "N"), arrlt_pctdev = c(25, 5)
  )
  rv$raw <- dat
  rv$analysis_data <- dat
  rv$review <- list(data = review_dat)
  rv$source_path <- "example.xpt"

  session$setInputs(
    show_review_only = FALSE,
    pctdev_min_filter = NA,
    pctdev_max_filter = NA,
    pctdev_filter_review_only = FALSE,
    review_case_filter = character(),
    ai_subset_name = "two rows"
  )
  session$setInputs(ai_save_subset = 1)
  session$flushReact()
  stopifnot(
    "two rows" %in% names(rv$ai_subsets),
    nrow(rv$ai_subsets[["two rows"]]$data) == 2L
  )

  session$setInputs(ai_data_scope = "full")
  stopifnot(nrow(ai_scope_data()) == 2L)
  session$setInputs(ai_data_scope = "subset", ai_saved_subset = "two rows")
  stopifnot(nrow(ai_scope_data()) == 2L)
})

cat("AI data access regression checks passed\n")
