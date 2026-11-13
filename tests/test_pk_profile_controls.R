app_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "adpc_review.R"))) {
  app_dir <- normalizePath(file.path(app_dir, ".."), mustWork = TRUE)
}
app_env <- new.env(parent = globalenv())
sys.source(file.path(app_dir, "adpc_review.R"), envir = app_env)

ui_text <- as.character(app_env$ui)
stopifnot(
  !grepl("Apply PK Plots", ui_text, fixed = TRUE),
  lengths(regmatches(ui_text, gregexpr("Prepare / Update PK Profiles", ui_text, fixed = TRUE))) == 3L,
  lengths(regmatches(ui_text, gregexpr("PK record filter (independent of Review)", ui_text, fixed = TRUE))) == 3L,
  grepl("independent of the Review and PK Profile filters", ui_text, fixed = TRUE),
  grepl("ind_use_time_window", ui_text, fixed = TRUE),
  grepl("Focus on a time range", ui_text, fixed = TRUE)
)

dat <- data.frame(
  USUBJID = rep(c("S1", "S2"), each = 2),
  ATEST = "DRUG",
  AVISIT = "DAY 1",
  NRRLT = rep(c(0, 1), 2),
  MRRLT = rep(c(0.1, 1.1), 2),
  AVAL = c(10, 8, 11, 9),
  ARMCD = rep(c("A", "B"), each = 2),
  NCA01FL = c("Y", "Y", "N", "N"),
  stringsAsFactors = FALSE
)

shiny::testServer(app_env$server, {
  rv$raw <- dat
  rv$analysis_data <- dat
  session$setInputs(
    id_var = "USUBJID", analyte_var = "ATEST", visit_var = "AVISIT",
    visitcd_var = "", nominal_var = "NRRLT", mrrlt_var = "MRRLT",
    aval_var = "AVAL", v_lloq = "", v_sex = "", g_route = "",
    g_atrt = "", g_arm = "ARMCD", g_cohort = "", g_agegrp = "",
    g_bsacat = ""
  )
  session$flushReact()

  session$setInputs(pk_prepare_ind = 1)
  session$flushReact()
  stopifnot(
    nrow(rv$pk_data) == nrow(dat),
    !isTRUE(rv$pk_data_stale),
    grepl("PK profiles ready: 4 records", output$pk_status_ind, fixed = TRUE)
  )

  # PK and NCA filters start from the loaded dataset, not the Review scope.
  rv$analysis_data <- dat[4, , drop = FALSE]
  session$setInputs(
    pk_ind_filter_variable = "NCA01FL",
    pk_ind_filter_mode = "equals",
    pk_ind_filter_value = "Y",
    pk_prepare_ind = 2
  )
  session$flushReact()
  stopifnot(
    nrow(rv$pk_data) == 2L,
    all(rv$pk_data$NCA01FL == "Y"),
    grepl("Filter: NCA01FL == Y (2 of 4 loaded records)", output$pk_status_ind, fixed = TRUE)
  )

  session$setInputs(
    ind_analyte = "DRUG", ind_subjects = "S1", ind_colour_by = "arm",
    pk_xvar_sel = "nrrlt", pk_yscale_sel = "linear", ind_page_size = "1",
    ind_use_time_window = TRUE, ind_time_window = c(0, 0.25)
  )
  session$flushReact()
  stopifnot(
    !is.null(output$ind_time_window_ui),
    nrow(ind_plot_data()) == 1L,
    identical(ind_plot_data()$.nrrlt, 0)
  )

  # A stale range from another mapped time axis resets to the current full range.
  session$setInputs(pk_xvar_sel = "mrrlt", ind_time_window = c(100, 200))
  session$flushReact()
  stopifnot(nrow(ind_plot_data()) == 2L)

  session$setInputs(
    nca_filter_variable = "NCA01FL",
    nca_filter_mode = "equals",
    nca_filter_value = "Y"
  )
  session$flushReact()
  stopifnot(
    nrow(nca_filtered_preview()) == 2L,
    grepl("2 of 4 loaded records", output$nca_filter_count, fixed = TRUE)
  )

  session$setInputs(nominal_var = "MRRLT")
  session$flushReact()
  stopifnot(
    isTRUE(rv$pk_data_stale),
    grepl("Mappings changed", output$pk_status_grp, fixed = TRUE)
  )
})

cat("PK profile control regression checks passed\n")
