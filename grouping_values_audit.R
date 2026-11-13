# Compatibility entry point. Canonical implementation: inst/app/grouping_values_audit.R
.compat_sources <- Filter(function(x) is.character(x) && length(x) == 1L,
  lapply(sys.frames(), function(frame) frame$ofile))
.compat_hint <- if (length(.compat_sources)) dirname(normalizePath(tail(.compat_sources, 1L)[[1L]], mustWork = FALSE)) else getwd()
.compat_candidates <- unique(c(.compat_hint, getwd(), dirname(.compat_hint)))
.compat_root <- .compat_candidates[file.exists(file.path(.compat_candidates, "inst", "app", "grouping_values_audit.R"))][1L]
if (is.na(.compat_root)) stop("Cannot locate inst/app/grouping_values_audit.R; source this entry point from the repository.")
source(file.path(.compat_root, "inst", "app", "grouping_values_audit.R"), local = TRUE)$value
