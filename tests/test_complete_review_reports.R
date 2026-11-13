# End-to-end synthetic concentration and parameter reports; no services or study data.
app_env <- new.env(parent=globalenv())
sys.source("adpc_review.R",envir=app_env)
dat <- data.frame(USUBJID=rep(c("S1","S2"),each=4),SUBJID=rep(c("1","2"),each=4),
  ATEST="DRUG",AVISIT="DAY 1",NRRLT=rep(c(0,1,1,2),2),ARRLT=rep(c(-.1,1,1.1,2),2),
  MRRLT=rep(c(0,1,1.1,2),2),AVAL=c(0,10,9,8,0,10,NA,-1),AVALU="ng/mL",NCA01FL="Y")
dat$ARRLT[8] <- NA
shiny::testServer(app_env$server, {
  rv$raw <- dat; rv$analysis_data <- dat
  source <- tempfile(fileext=".csv"); write.csv(dat,source,row.names=FALSE)
  rv$source_path <- source
  rv$source_sha256 <- digest::digest(file=source,algo="sha256",serialize=FALSE)
  session$setInputs(id_var="USUBJID",analyte_var="ATEST",visit_var="AVISIT",visitcd_var="",
    nominal_var="NRRLT",actual_var="ARRLT",mrrlt_var="MRRLT",aval_var="AVAL",adtm_var="",arftdtm_var="",
    filter_enabled="no",pctdev_threshold=20,absdev_threshold=.5,enable_iqr=FALSE,iqr_multiplier=1.5,
    early_window=2,predose_neg_threshold=-.5,review_dataset_kind="concentration")
  session$setInputs(run_review=1); session$flushReact()
  payload <- review_report_payload()
  stopifnot(nrow(rv$review$data)==7,nrow(rv$analysis_data)==8,
    any(vapply(payload$sections,function(s) identical(s$id,"timing-excluded"),logical(1))))
  omitted <- Filter(function(s) s$id=="timing-excluded",payload$sections)[[1]]
  stopifnot(nrow(omitted$tables[[1]]$data)==1,identical(rv$raw,dat))
  # Duplicate nominal flags identify repeated rows, not the full profile.
  finding <- review_finding_result("duplicate_nominal")
  stopifnot(finding$interest_records==4, nrow(finding$data)==7)
  old_settings <- payload$rules_settings
  old_summary <- payload$summary_items
  session$setInputs(nominal_var="ARRLT",pctdev_threshold=999)
  session$flushReact()
  stopifnot(identical(review_report_payload()$rules_settings,old_settings),
    identical(review_report_payload()$summary_items,old_summary))
  out <- tempfile(fileext=".zip"); xlsx <- tempfile(fileext=".xlsx")
  app_env$write_review_html_bundle(out,payload,"adpc_review_test")
  app_env$write_review_excel_report(xlsx,payload)
  zip_names <- unzip(out,list=TRUE)$Name
  stopifnot(any(grepl("evaluation-rules.html$",zip_names)),any(grepl("quality-invalid_value.html$",zip_names)))
  wb <- openxlsx::loadWorkbook(xlsx)
  stopifnot("Summary" %in% names(wb),"Plot data" %in% names(wb),any(grepl("quality|Missing",names(wb),ignore.case=TRUE)))
  plot <- openxlsx::read.xlsx(xlsx,sheet="Plot data",startRow=10)
  stopifnot(nrow(plot)==7,all(c("AVAL","ARRLT","PLOT_SERIES") %in% names(plot)))
  # Repeated saves preserve existing outputs and expose the saved path.
  save_review_locally("html"); first <- rv$review_saved_path
  save_review_locally("html"); second <- rv$review_saved_path
  save_review_locally("excel"); third <- rv$review_saved_path
  stopifnot(file.exists(first),file.exists(second),file.exists(third),first!=second)
  unlink(c(source,first,second,third,out,xlsx))
})

param <- data.frame(USUBJID=c("S1","S1","S1","S2"),PARAMCD=c("CMAX","CMAX","AUC","CMAX"),
  PARAM=c("Maximum","Maximum","Area","Changed label"),AVAL=c(10,11,NA,20),
  AVALU=c("ng/mL","ng/mL","ng*h/mL","mg/L"),ANL01FL=c("Y","Y","?","Y"),APERIOD=1)
shiny::testServer(app_env$server,{
  rv$raw <- param; rv$analysis_data <- param
  session$setInputs(review_dataset_kind="parameter",parameter_subject="USUBJID",parameter_code="PARAMCD",
    parameter_value="AVAL",parameter_keys=c("USUBJID","PARAMCD","APERIOD"),filter_enabled="no")
  session$setInputs(run_review=1); session$flushReact()
  payload <- review_report_payload()
  stopifnot(identical(rv$raw,param),nrow(rv$review$data)==4,identical(payload$kind,"parameter"))
  get <- function(id) Filter(function(s) identical(s$id,id),payload$sections)[[1]]
  dup <- get("quality-duplicate_key")$tables[[1]]$data
  stopifnot(sum(dup$RECORD_OF_INTEREST=="YES")==2,nrow(dup)==2,
    grepl("1 record",get("quality-invalid_value")$description),
    grepl("1 record",get("quality-analysis_flags")$description),
    grepl("3 record",get("quality-units")$description))
  out <- tempfile(fileext=".zip"); xlsx <- tempfile(fileext=".xlsx")
  app_env$write_review_html_bundle(out,payload,"adnca_parameters_test")
  app_env$write_review_excel_report(xlsx,payload)
  stopifnot(!"Plot data" %in% openxlsx::getSheetNames(xlsx))
  expanded <- tempfile(); dir.create(expanded); unzip(out,exdir=expanded)
  pages <- list.files(expanded,pattern="html$",recursive=TRUE,full.names=TRUE)
  for(page in pages) {
    content <- paste(readLines(page,warn=FALSE),collapse="\n")
    links <- regmatches(content,gregexpr('href="[^"#]+[.]html"',content))[[1]]
    for(link in links) stopifnot(file.exists(file.path(dirname(page),sub('"$','',sub('^href="','',link)))))
  }
  unlink(c(out,xlsx,expanded),recursive=TRUE)
})
cat("Complete concentration/parameter reports passed.\n")
