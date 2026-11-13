app_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "adpc_review.R"))) {
  app_dir <- normalizePath(file.path(app_dir, ".."), mustWork = TRUE)
}
app_env <- new.env(parent = globalenv())
sys.source(file.path(app_dir, "adpc_review.R"), envir = app_env)

dat <- data.frame(
  SUBJID = c("S1", "S1", "S2", "S3"),
  PCTESTCD = "DRUG",
  AVISIT = "DAY 1",
  NRRLT = 999,
  MRRLT = 888,
  ARRLT = 777,
  NFRLT = c(0, 1, 0, 0),
  MFRLT = c(0.1, 1.1, 0.2, 0.3),
  AFRLT = c(0.1, 1.0, 0.2, 0.3),
  AVAL = c(10, 8, 7, 6),
  APROFILE = c("Named profile", "Named profile", "N", ""),
  stringsAsFactors = FALSE
)

review <- app_env$compute_arrlt_review(
  dat = dat,
  id_var = "SUBJID",
  analyte_var = "PCTESTCD",
  visit_var = "AVISIT",
  nominal_var = "NFRLT",
  actual_var = "AFRLT",
  aval_var = "AVAL",
  mrrlt_var = "MFRLT"
)$data

stopifnot(
  identical(review$.nrrlt, dat$NFRLT[c(1, 3, 4, 2)]),
  identical(review$.arrlt, dat$AFRLT[c(1, 3, 4, 2)]),
  identical(review$.mrrlt, dat$MFRLT[c(1, 3, 4, 2)]),
  all(review$.profile_aprofile[review$.id == "S1"] == "Y"),
  all(review$.profile_aprofile[review$.id %in% c("S2", "S3")] == "N_OR_EMPTY")
)

schema <- app_env$case_schema_v13(dat, list(
  subject = "SUBJID", analyte = "PCTESTCD", visit = "AVISIT",
  nrrlt = "NFRLT", mrrlt = "MFRLT", arrlt = "AFRLT", aval = "AVAL"
))
stopifnot(
  identical(schema$nrrlt, "NFRLT"),
  identical(schema$mrrlt, "MFRLT"),
  identical(schema$arrlt, "AFRLT")
)

profile_plot <- app_env$case_profile_plot_v13(dat, schema$mrrlt, schema$aval, "S1 | DRUG | DAY 1")
stopifnot(
  inherits(profile_plot, "plotly"),
  inherits(plotly::plotly_build(profile_plot), "plotly")
)

cat("variable mapping regression checks passed\n")
