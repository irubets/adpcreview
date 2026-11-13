app_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "adpc_review.R"))) {
  app_dir <- normalizePath(file.path(app_dir, ".."), mustWork = TRUE)
}
app_env <- new.env(parent = globalenv())
sys.source(file.path(app_dir, "adpc_review.R"), envir = app_env)

ui_text <- as.character(app_env$ui)
stopifnot(
  grepl("5 - All Subjects, page by Visit + Analyte", ui_text, fixed = TRUE),
  grepl("6 - Analyte + all Subjects, page by Visit", ui_text, fixed = TRUE),
  grepl("Mean with SD dispersion band", ui_text, fixed = TRUE),
  grepl("Mean with SD error bars", ui_text, fixed = TRUE),
  grepl("Horizontal jitter (% of nominal time)", ui_text, fixed = TRUE),
  grepl("Upper only (mean + SD)", ui_text, fixed = TRUE),
  grepl("Summary table grouping", ui_text, fixed = TRUE),
  grepl("Combine visits", ui_text, fixed = TRUE),
  grepl("Focus on a time range", ui_text, fixed = TRUE)
)

duplicate_time_data <- data.frame(
  .id = c("S1", "S1", "S2"),
  .nrrlt = c(0, 0, 0),
  .mrrlt = c(0, 0, 0),
  .aval_plot = c(10, 14, 20),
  stringsAsFactors = FALSE
)
smry <- app_env$summarise_subject_profiles(duplicate_time_data, "nrrlt")
stopifnot(
  nrow(smry) == 1L,
  smry$N_SUBJECTS == 2L,
  isTRUE(all.equal(smry$MEAN, 16)),
  isTRUE(all.equal(smry$SD, stats::sd(c(12, 20))))
)
mean_sd_plot <- app_env$make_mean_sd_profile(duplicate_time_data, "nrrlt", "linear")
stopifnot(inherits(app_env$as_plotly(mean_sd_plot$plot), "plotly"))

errorbar_data <- data.frame(
  .id = rep(c("S1", "S2", "S3", "S4"), each = 2),
  .arm = rep(c("A", "A", "B", "B"), each = 2),
  .nrrlt = rep(c(1, 2), 4),
  .mrrlt = rep(c(10, 20), 4),
  .aval_plot = c(10, 20, 14, 24, 30, 40, 34, 44),
  stringsAsFactors = FALSE
)
errorbar_profile <- app_env$make_mean_sd_errorbar_profile(
  errorbar_data, xvar = "nrrlt", yscale = "linear",
  group_var = ".arm", group_label = "ARM", sd_sides = "upper", jitter_pct = 5
)
errorbar_layer <- ggplot2::ggplot_build(errorbar_profile$plot)$data[[1]]
two_sided_profile <- app_env$make_mean_sd_errorbar_profile(
  errorbar_data, xvar = "nrrlt", yscale = "linear",
  group_var = ".arm", group_label = "ARM", sd_sides = "two", jitter_pct = 0
)
two_sided_layer <- ggplot2::ggplot_build(two_sided_profile$plot)$data[[1]]
summary_pages <- app_env$make_concentration_summary_tables(
  errorbar_data, xvar = "mrrlt", group_var = ".arm", group_label = "ARM"
)
zero_data <- errorbar_data
zero_data$.aval_plot[1] <- 0
zero_pages <- app_env$make_concentration_summary_tables(
  zero_data, xvar = "nrrlt", group_var = ".arm", group_label = "ARM"
)
stopifnot(
  inherits(app_env$as_plotly(errorbar_profile$plot), "plotly"),
  setequal(errorbar_profile$data$Group, c("A", "B")),
  any(errorbar_profile$data$`Displayed Time` != errorbar_profile$data$`Plot Time`),
  all(errorbar_layer$ymin == errorbar_layer$y),
  all(errorbar_layer$ymax >= errorbar_layer$y),
  any(two_sided_layer$ymin < two_sided_layer$y),
  all(two_sided_profile$data$`Displayed Time` == two_sided_profile$data$`Plot Time`),
  length(summary_pages) == 2L,
  all(vapply(summary_pages, function(x) identical(
    x$Statistic,
    c("N", "Mean", "SD", "Median", "Min - Max (Range)",
      "Geometric Mean", "Geometric CV (%)")
  ), logical(1))),
  identical(names(summary_pages[[1]]), c("Statistic", "1", "2")),
  identical(summary_pages[[1]][summary_pages[[1]]$Statistic == "Mean", "1"], "12"),
  summary_pages[[1]][summary_pages[[1]]$Statistic == "Geometric Mean", "1"] != "NA",
  identical(zero_pages[[1]][zero_pages[[1]]$Statistic == "Geometric Mean", "1"], "NA"),
  identical(zero_pages[[1]][zero_pages[[1]]$Statistic == "Geometric CV (%)", "1"], "NA")
)

pd <- data.frame(
  .id = rep(c("S1", "S2"), each = 3),
  .analyte = rep(c("DRUG", "DRUG", "METAB"), 2),
  .avisit = rep(c("DAY 1", "DAY 2", "DAY 1"), 2),
  .visitcd = rep(c("D1", "D2", "D1"), 2),
  .visit_display = rep(c("DAY 1 [D1]", "DAY 2 [D2]", "DAY 1 [D1]"), 2),
  .nrrlt = rep(c(0, 1, 0), 2),
  .mrrlt = c(0, 1, 0, 0.25, 2, 0),
  PCTPTNUM = 0,
  .arm = rep(c("A", "B"), each = 3),
  .aval_raw = c(10, 8, 4, 12, 10, 6),
  .aval_plot = c(10, 8, 4, 12, 10, 6),
  .blq_flag = FALSE,
  stringsAsFactors = FALSE
)

nominal_axis <- app_env$prepare_combined_visit_axis(
  pd[pd$.analyte == "DRUG", ], ".visitcd", c("D1", "D2"),
  "nrrlt", "Nominal Time"
)
continuous_axis <- app_env$prepare_combined_visit_axis(
  pd[pd$.analyte == "DRUG", ], ".visitcd", c("D1", "D2"),
  "mrrlt", "Relative Time"
)
packed_times <- c(0, 0.25, 2, 4)
packed_pd <- data.frame(
  .id = rep(c("S1", "S2"), each = length(packed_times)),
  .visitcd = rep(c("D1", "D2"), each = length(packed_times)),
  .nrrlt = rep(packed_times, 2),
  .mrrlt = rep(packed_times, 2),
  .aval_plot = seq_len(2 * length(packed_times)),
  stringsAsFactors = FALSE
)
equal_axis <- app_env$prepare_combined_visit_axis(
  packed_pd, ".visitcd", c("D1", "D2"),
  "nrrlt", "Nominal Time", spacing = "equal"
)
equal_d1 <- sort(unique(equal_axis$data$.sas_x[equal_axis$data$.sas_visit == "D1"]))
equal_d2 <- sort(unique(equal_axis$data$.sas_x[equal_axis$data$.sas_visit == "D2"]))
stopifnot(
  length(nominal_axis$separators) == 1L,
  nrow(nominal_axis$labels) == 2L,
  grepl("restarts within each visit", nominal_axis$xlab, fixed = TRUE),
  identical(continuous_axis$data$.sas_x, continuous_axis$data$.sas_time_raw),
  isTRUE(all.equal(diff(equal_d1), rep(diff(equal_d1)[1], 3))),
  isTRUE(all.equal(diff(equal_d2), rep(diff(equal_d2)[1], 3))),
  identical(equal_axis$break_labels, rep(c("0", "0.25", "2", "4"), 2)),
  identical(equal_axis$xlab, "Nominal Time"),
  nzchar(nominal_axis$warning)
)
combined_spaghetti <- app_env$make_spaghetti(
  nominal_axis$data, xvar = "combined", yscale = "linear",
  colour_var = ".id", line_group_var = ".sas_visit"
)
combined_spaghetti <- app_env$add_combined_visit_guides(combined_spaghetti, nominal_axis)
combined_mean_sd <- app_env$make_mean_sd_profile(
  nominal_axis$data, xvar = "combined", yscale = "linear"
)$plot
combined_mean_sd <- app_env$add_combined_visit_guides(combined_mean_sd, nominal_axis)
stopifnot(
  inherits(app_env$as_plotly(combined_spaghetti, combined = TRUE), "plotly"),
  inherits(app_env$as_plotly(combined_mean_sd, combined = TRUE), "plotly"),
  identical(combined_spaghetti$theme$axis.text.x$angle, 45)
)

recommend_pd <- data.frame(
  .analyte = "DRUG",
  .visitcd = rep(c("V1", "V2"), each = 4),
  .nrrlt = rep(c(10, 10, 10, 10, 20, 20, 20, 20)),
  .mrrlt = rep(c(0, 0.25, 2, 4), 2),
  pctptnum = rep(c(0, 1, 2, 3), 2),
  stringsAsFactors = FALSE
)
nominal_quality <- app_env$profile_x_group_quality(recommend_pd, ".visitcd", "nrrlt")
relative_quality <- app_env$profile_x_group_quality(recommend_pd, ".visitcd", "mrrlt")
stopifnot(
  nominal_quality$curves == 0L,
  relative_quality$curves == 2L,
  identical(app_env$profile_x_values(recommend_pd, "pctptnum"),
            as.numeric(recommend_pd$pctptnum))
)

shiny::testServer(app_env$server, {
  rv$raw <- data.frame(
    SUBJID = pd$.id, PCTEST = pd$.analyte,
    VISIT = pd$.avisit, VISITCD = pd$.visitcd,
    NRRLT = pd$.nrrlt, MRRLT = pd$.mrrlt,
    PCTPTNUM = pd$PCTPTNUM, AVAL = pd$.aval_plot,
    stringsAsFactors = FALSE
  )
  session$setInputs(
    id_var = "SUBJID", analyte_var = "PCTEST", visit_var = "VISIT",
    visitcd_var = "VISITCD", nominal_var = "NRRLT", mrrlt_var = "MRRLT",
    aval_var = "AVAL", sas_domain = "adpc", sas_mode = "6",
    sas_xvar = "nrrlt", sas_combine_visits = TRUE
  )
  session$flushReact()
  stopifnot(
    !is.null(output$sas_analyte_ui),
    !is.null(output$sas_combined_visits_ui),
    !is.null(output$sas_combined_axis_diagnostic)
  )

  rv$pk_data <- pd
  rv$pk_data_stale <- FALSE
  session$setInputs(
    sas_domain = "adpc", sas_mode = "5", sas_xvar = "nrrlt",
    sas_yscale = "linear", sas_subject_summary = "mean_sd_band",
    nominal_var = "NRRLT", mrrlt_var = "MRRLT", sas_combine_visits = FALSE
  )
  session$flushReact()
  rv$pk_data_stale <- FALSE
  session$setInputs(sas_run = 1)
  session$flushReact()
  stopifnot(
    length(rv$sas_plot_items) == 3L,
    all(vapply(rv$sas_plot_items, function(x) {
      identical(names(x$data), c("Time", "Mean", "SD", "N Subjects"))
    }, logical(1)))
  )

  session$setInputs(
    sas_mode = "6", sas_analyte = "DRUG",
    sas_subject_summary = "spaghetti", sas_run = 2
  )
  session$flushReact()
  stopifnot(
    length(rv$sas_plot_items) == 2L,
    all(vapply(rv$sas_plot_items, function(x) "id" %in% names(x$data), logical(1)))
  )

  session$setInputs(
    sas_combine_visits = TRUE, sas_combined_visits = c("D1", "D2"),
    sas_combined_spacing = "equal", sas_use_time_window = FALSE, sas_run = 3
  )
  session$flushReact()
  stopifnot(
    !is.null(output$sas_combined_visits_ui),
    !is.null(output$sas_combined_axis_diagnostic),
    any(grepl(
      "Mapped analysis relative time (MRRLT)",
      as.character(output$sas_combined_axis_diagnostic), fixed = TRUE
    )),
    length(rv$sas_plot_items) == 1L,
    all(c("sas_visit", "sas_time_raw", "sas_x") %in% names(rv$sas_plot_items[[1]]$data)),
    setequal(rv$sas_plot_items[[1]]$data$sas_visit, c("D1", "D2"))
  )

  session$setInputs(
    sas_subject_summary = "mean_sd", sas_use_time_window = TRUE,
    sas_time_window = c(0, 0.25), sas_run = 4
  )
  session$flushReact()
  stopifnot(
    !is.null(output$sas_time_window_ui),
    length(rv$sas_plot_items) == 1L,
    identical(unique(rv$sas_plot_items[[1]]$data$Visit), "D1"),
    all(rv$sas_plot_items[[1]]$data$`Visit Time` <= 0.25)
  )

  session$setInputs(
    sas_subject_summary = "spaghetti", sas_xvar = "pctptnum",
    sas_time_window = c(136, 148), sas_run = 5
  )
  session$flushReact()
  stopifnot(
    length(rv$sas_plot_items) == 1L,
    isTRUE(rv$sas_plot_items[[1]]$combined),
    nrow(rv$sas_plot_items[[1]]$data) > 0L
  )

  session$setInputs(
    sas_xvar = "nrrlt", sas_subject_summary = "mean_sd_errorbar",
    sas_summary_group = "arm", sas_sd_sides = "upper", sas_jitter_pct = "5",
    sas_use_time_window = FALSE, sas_run = 6
  )
  session$flushReact()
  stopifnot(
    length(rv$sas_plot_items) == 1L,
    setequal(rv$sas_plot_items[[1]]$data$Group, c("A", "B")),
    any(rv$sas_plot_items[[1]]$data$`Displayed Time` !=
        rv$sas_plot_items[[1]]$data$`Plot Time`),
    length(rv$sas_plot_items[[1]]$summary_tables) == 4L,
    all(vapply(rv$sas_plot_items[[1]]$summary_tables, function(x) {
      "Geometric CV (%)" %in% x$Statistic
    }, logical(1)))
  )
})

cat("SAS all-subject summary mode regression checks passed\n")
