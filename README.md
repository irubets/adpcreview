# adpcreview

Review ADPC/ADNCA concentration-time data or parameter-level NCA records such as PARAMCD/AVAL. The package runs a local Shiny app with linked findings, interactive plots, source-file provenance and complete HTML/Excel review exports. Version **0.1.0**.

Read the [User Guide (HTML)](USERGUIDE.html) or [User Guide (Markdown)](USERGUIDE.md) for every Setup section, tab, workflow and output.

## First-time installation

### Install from GitHub

The public source repository is [irubets/adpcreview](https://github.com/irubets/adpcreview). Install directly from GitHub:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("irubets/adpcreview", dependencies = TRUE, upgrade = "never")
adpcreview::run_app()
```

Alternatively, download the supplied [adpcreview_0.1.0.tar.gz](https://github.com/irubets/adpcreview/raw/refs/heads/main/dist/adpcreview_0.1.0.tar.gz) source archive and follow the archive-installation steps below. Neither method requires a developer-machine folder.



Install R 4.3 or later and obtain **`adpcreview_0.1.0.tar.gz`** (or the current supplied version) from the maintainer. RStudio is optional. These instructions use the supplied archive and do not depend on the developer’s computer, repository folder or a CRAN listing for this app.

Install dependencies:

```r
install.packages(c(
  "shiny", "shinyWidgets", "shinyFiles", "DT", "plotly", "ggplot2",
  "dplyr", "tidyr", "haven", "readr", "openxlsx", "stringr", "purrr",
  "scales", "httr2", "jsonlite", "htmltools", "PKNCA", "zip", "digest"
), repos = "https://cloud.r-project.org")
```

Install the app archive without extracting it:

```r
install.packages(file.choose(), repos = NULL, type = "source")
# Select adpcreview_0.1.0.tar.gz.
```

Alternatively, if the archive is in your current working directory:

```r
install.packages("adpcreview_0.1.0.tar.gz", repos = NULL, type = "source")
```

Local source installs require dependencies to be installed first. The package has no native C/C++ compilation step; dependencies built from source may need system build tools. Restart R before updating a loaded package.

## Launch and use

```r
adpcreview::run_app()
```

Keep R running. Use the local address printed by Shiny if the browser does not open. Stop with Esc/Stop in RStudio or Ctrl+C in a terminal. Run simultaneous apps in separate R sessions.

1. Load the local dataset from Setup.
2. Choose concentration-time or NCA-parameter review and confirm the applicable mappings/key.
3. Choose the population and thresholds, then click **Run review**.
4. Inspect counts, source identity and linked complete-context findings.
5. Save the complete HTML ZIP and/or Excel review. Dedicated PK-profile and NCA tools use their own scopes and action buttons.

Supported inputs are XPT, SAS7BDAT, CSV, RDS, RDA and RData containing a reviewable data frame. Study data are selected at runtime; none are distributed with the package. Parameter mode provides inventory/listings and consistency screens; it does not recalculate NCA from parameter rows.

## Documentation after installation

```r
browseURL(system.file("docs", "USERGUIDE.html", package = "adpcreview", mustWork = TRUE))
help("run_app", package = "adpcreview")
```

The HTML guide is self-contained, including screenshots, and works offline. Markdown uses the accompanying guide-assets folder. Reports and separate NCA exports have different purposes; see the guide before sharing outputs.

## Optional AI

Core review does not need an AI account. OpenAI and Anthropic are optional hosted providers; local Qwen uses Ollama. Configure only intended providers in your user .Renviron, restart R, confirm data scope and blind data appropriately. The [guide](USERGUIDE.md#ai-assistant-optional-help-with-explicit-data-scope) covers configuration, query access and approval of generated visualization code. No provider availability or model subscription is implied.

## Data handling and output

Source values are preserved; findings are screening candidates, not automatic corrections or clinical adjudications. Input file SHA256 identifies file bytes. Reports can contain selected source records and need the same protection as input data. Generated visualizations default to a writable per-user data directory; override with `run_app(output_dir = "my-visualizations")`. Keep credentials and clinical data out of package distributions.

## For developers

Canonical app scripts are in `inst/app/`; root R scripts are compatibility entry points. `shiny::runApp(".")` still works from a source checkout. Public package entry points are `create_app()` and `run_app()`. Edit root USERGUIDE.md / README.md and run `python tools/render-docs.py` to refresh self-contained HTML and synchronize installed documentation (developer tool: Python with the markdown package). Run installed-package smoke tests with `Rscript tests/package-smoke.R`; public synthetic regression scripts are `tests/test_*.R`; additional development tests that require local study fixtures are not included in the public snapshot. Source package builds deliberately omit fixtures, clinical data, reports and development artifacts. Build with `R CMD build .`; check the resulting archive with `R CMD check --no-manual <archive.tar.gz>`. The guide describes 0.1.0; hosted CI and clinical validation are separate from documentation verification.

## License

Released under the [MIT License](LICENSE.md). Copyright 2026 Igor Rubets and Marieve Dupuis.
