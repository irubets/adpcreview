app_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(app_dir, "profile_identification.R"))) {
  app_dir <- normalizePath(file.path(app_dir, ".."), mustWork = TRUE)
}
source(file.path(app_dir, "profile_identification.R"))

dat <- data.frame(
  USUBJID = rep("STUDY-S1", 4),
  SUBJID = rep("S1", 4),
  PCTESTCD = rep("DRUG", 4),
  AVISIT = rep(c("DAY 1", "DAY 2"), each = 2),
  ARMCD = rep("A", 4),
  APROFILE = rep("DRUG PROFILE", 4),
  NFRLT = c(0, 0.1, 1, 1.1),
  NRRLT = c(0, 1, 0, 1),
  PCRFTDTM = rep(c("2026-01-01 08:00", "2026-01-02 08:00"), each = 2),
  ADTM = as.POSIXct(c(
    "2026-01-01 07:45", "2026-01-01 09:00",
    "2026-01-02 07:45", "2026-01-02 09:00"
  ), tz = "UTC"),
  stringsAsFactors = FALSE
)

assessment <- identify_pk_profiles(dat, list(
  subject = "USUBJID", analyte = "PCTESTCD", visit = "AVISIT",
  nominal = "NFRLT", sample_time = "ADTM", profile = "APROFILE"
))
stopifnot(
  identical(assessment$cycle_nominal_col, "NRRLT"),
  identical(assessment$nominal_col, "NFRLT"),
  identical(assessment$subject_col, "USUBJID"),
  identical(assessment$selected$type, "AVISIT"),
  assessment$n_profiles == 2L,
  identical(assessment$timepoints[["Number of time points"]], c(2L, 2L)),
  identical(assessment$timepoints[["Unique time-point values"]], c("0, 1", "0, 1")),
  !assessment$candidates$Valid[assessment$candidates$Type == "APROFILE"],
  assessment$candidates$Valid[assessment$candidates$Type == "AVISIT"]
)

profile_dat <- dat
profile_dat$APROFILE <- rep(c("P1", "P2"), each = 2)
profile_assessment <- identify_pk_profiles(profile_dat, list(
  subject = "USUBJID", analyte = "PCTESTCD", visit = "AVISIT",
  nominal = "NFRLT", sample_time = "ADTM", profile = "APROFILE"
))
stopifnot(identical(profile_assessment$selected$type, "APROFILE"))

dose_dat <- dat
dose_dat$AVISIT <- "REPEATED VISIT"
dose_assessment <- identify_pk_profiles(dose_dat, list(
  subject = "USUBJID", analyte = "PCTESTCD", visit = "AVISIT",
  nominal = "NFRLT", sample_time = "ADTM", profile = "APROFILE"
))
stopifnot(identical(dose_assessment$selected$type, "DOSE_TIME"))

bad_ids <- rbind(dat, transform(dat[1, , drop = FALSE], SUBJID = "S1-ALIAS"))
stopifnot(!subject_id_relationship(bad_ids)$one_to_one)
counts <- subject_identifier_counts(bad_ids)
stopifnot(counts$usubjid == 1L, counts$subjid == 2L)
mapping_audit <- subject_id_mapping_audit(bad_ids)
stopifnot(
  !mapping_audit$one_to_one,
  identical(mapping_audit$conflicting_usubjid, "STUDY-S1"),
  !length(mapping_audit$conflicting_subjid),
  identical(mapping_audit$affected_subjid, c("S1", "S1-ALIAS")),
  identical(names(mapping_audit$table), c("USUBJID", "SUBJID"))
)
reverse_ids <- rbind(dat, transform(dat[1, , drop = FALSE], USUBJID = "STUDY-S2"))
reverse_audit <- subject_id_mapping_audit(reverse_ids)
stopifnot(
  !reverse_audit$one_to_one,
  identical(reverse_audit$conflicting_subjid, "S1")
)
subjid_only <- dat[names(dat) != "USUBJID"]
stopifnot(identify_pk_profiles(subjid_only, list(
  subject = "SUBJID", analyte = "PCTESTCD", visit = "AVISIT",
  nominal = "NFRLT", sample_time = "ADTM", profile = "APROFILE"
))$subject_col == "SUBJID")
stopifnot(!identify_pk_profiles(data.frame(X = 1), list())$available)

cat("profile identification regression checks passed\n")
