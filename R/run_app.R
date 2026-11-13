#' Create or launch the installed review application
#' @export
create_app <- function(output_dir = NULL) {
  app_dir <- system.file("app", package = "adpcreview", mustWork = TRUE)
  if (is.null(output_dir)) {
    output_dir <- file.path(tools::R_user_dir("adpcreview", "data"), "generated_visualizations")
  }
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    stop("output_dir must be one nonempty directory path.")
  }
  app_env <- new.env(parent = globalenv())
  app_env$.app_dir <- app_dir
  app_env$.app_output_dir <- normalizePath(path.expand(output_dir), winslash = "/", mustWork = FALSE)
  app <- source(file.path(app_dir, "adpc_review.R"), local = app_env, chdir = FALSE)$value
  if (!inherits(app, "shiny.appobj")) stop("The packaged application did not initialize.")
  app
}

#' @export
run_app <- function(host = "127.0.0.1", port = NULL,
                    launch.browser = getOption("shiny.launch.browser", interactive()),
                    output_dir = NULL, ...) {
  shiny::runApp(create_app(output_dir = output_dir), host = host, port = port,
                launch.browser = launch.browser, ...)
}
