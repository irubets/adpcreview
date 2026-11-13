\name{run_app}
\alias{run_app}
\alias{create_app}
\title{Create or Launch the Installed Review Application}
\usage{
create_app(output_dir = NULL)
run_app(host = "127.0.0.1", port = NULL,
  launch.browser = getOption("shiny.launch.browser", interactive()),
  output_dir = NULL, ...)
}
\arguments{
\item{output_dir}{Writable folder for approved generated visualizations. The default is a per-user application data folder. Review report destinations remain controlled in the app.}
\item{host}{Interface on which to listen; defaults to localhost.}
\item{port}{Port, or NULL to let Shiny select one.}
\item{launch.browser}{Whether to open a browser, or a browser function.}
\item{...}{Additional arguments passed to \code{shiny::runApp}.}
}
\description{Initializes an independent application environment using installed companion scripts. Input datasets and credentials are supplied at runtime, never bundled.}
\value{\code{create_app} returns a Shiny application object. \code{run_app} runs it until stopped and returns the result from Shiny.}
\details{The package launcher is byte-compiled during installation. Application scripts are bundled as Shiny resources; no native compiler is required. Optional hosted AI services transmit selected context outside the computer. The default output folder avoids writing into the installed package library.}
\examples{\dontrun{
adpcreview::run_app()
}}
