# Standalone HTML export for ADPC / ADNCA review.
review_report_with_rules <- function(report) {
  if(any(vapply(report$sections,function(x) identical(x$id,"evaluation-rules"),logical(1)))) return(report)
  blocks <- lapply(report$sections,function(s) list(title=s$title,target=s$id,
    text=s$rule %||% s$description))
  report$sections <- c(list(list(id="evaluation-rules",group="Report guide",title="Evaluation rules and algorithms",
    description=paste("Screening findings require study review; no source values are corrected.",
      "Missing prerequisites mean not assessable, not a clean result. Amber rows trigger the stated finding; other rows supply context.",
      "Mapping, scope and settings below were committed by Run review. Parameter checks do not establish NCA validity or protocol ranges."),
    blocks=blocks,tables=list(list(title="Committed settings and mappings",data=report$rules_settings %||% data.frame())))),report$sections)
  report
}

review_report_escape <- function(x) {
  x <- enc2utf8(as.character(x))
  as.character(htmltools::htmlEscape(x, attribute = FALSE))
}

review_report_safe_id <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "-", as.character(x))
  x <- gsub("(^-+|-+$)", "", x)
  ifelse(nzchar(x), x, "section")
}

review_report_dataset_name <- function(data = NULL, source_path = NULL) {
  domain <- character()
  if (!is.null(data)) {
    data <- as.data.frame(data)
    domain_col <- names(data)[toupper(names(data)) == "DOMAIN"]
    if (length(domain_col)) {
      domain <- unique(tolower(trimws(as.character(data[[domain_col[1]]]))))
      domain <- domain[!is.na(domain) & nzchar(domain)]
    }
  }
  candidate <- if (length(domain) == 1L) domain else {
    path <- if (is.null(source_path) || !length(source_path) || is.na(source_path[1])) "adpc" else source_path[1]
    tools::file_path_sans_ext(basename(path))
  }
  candidate <- tolower(gsub("[^A-Za-z0-9._-]+", "_", candidate))
  candidate <- gsub("^_+|_+$", "", candidate)
  if (nzchar(candidate)) candidate else "adpc"
}

review_report_filename <- function(data = NULL, source_path = NULL, timestamp = Sys.time()) {
  dataset <- review_report_dataset_name(data, source_path)
  # Colons are not valid in Windows filenames, so HH-MM is the Windows-safe
  # representation of the ISO hour and minute.
  paste0(dataset, "_review_", format(timestamp, "%Y-%m-%dT%H-%M"), ".html")
}

review_report_bundle_name <- function(data = NULL, source_path = NULL, timestamp = Sys.time()) {
  sub("[.]html$", "", review_report_filename(data, source_path, timestamp))
}

review_report_zip_filename <- function(data = NULL, source_path = NULL, timestamp = Sys.time()) {
  paste0(review_report_bundle_name(data, source_path, timestamp), ".zip")
}

review_report_format_column <- function(x) {
  if (inherits(x, "POSIXt")) {
    out <- format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else if (inherits(x, "Date")) {
    out <- format(x, "%Y-%m-%d")
  } else if (is.list(x)) {
    out <- vapply(x, function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) "" else paste(as.character(value), collapse = "; ")
    }, character(1))
  } else {
    out <- as.character(x)
  }
  out[is.na(out)] <- ""
  out
}

review_report_json_for_script <- function(value) {
  json <- as.character(jsonlite::toJSON(value, auto_unbox = TRUE, na = "string",
                                        dataframe = "values", matrix = "rowmajor"))
  slash <- intToUtf8(92L)
  json <- gsub("<", paste0(slash, "u003c"), json, fixed = TRUE)
  json <- gsub(">", paste0(slash, "u003e"), json, fixed = TRUE)
  json <- gsub("&", paste0(slash, "u0026"), json, fixed = TRUE)
  json
}

review_report_write_table <- function(connection, data, title = NULL, interest_column = NULL,
                                      table_id = "review-table", page_size = 100L,
                                      pagination_threshold = 250L) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  table_id <- review_report_safe_id(table_id)
  if (!is.null(title) && nzchar(title)) {
    writeLines(paste0("<h3>", review_report_escape(title), "</h3>"), connection, useBytes = TRUE)
  }
  writeLines(
    paste0("<p class=\"table-meta\">", format(nrow(data), big.mark = ","), " records; ",
           format(ncol(data), big.mark = ","), " columns</p>"),
    connection, useBytes = TRUE
  )
  if (!nrow(data) || !ncol(data)) {
    writeLines("<p class=\"empty\">No relevant observations.</p>", connection, useBytes = TRUE)
    return(invisible(NULL))
  }

  formatted <- lapply(data, review_report_format_column)
  interest <- rep(FALSE, nrow(data))
  if (!is.null(interest_column) && interest_column %in% names(data)) {
    marker <- toupper(trimws(as.character(data[[interest_column]])))
    interest <- !is.na(marker) & marker %in% c("Y", "YES", "TRUE", "1")
  }

  use_pagination <- nrow(data) > as.integer(pagination_threshold)
  if (use_pagination) {
    payload <- list(
      rows = unname(do.call(cbind, formatted)),
      interest = unname(interest),
      pageSize = as.integer(page_size)
    )
    writeLines(c(
      paste0("<div class=\"table-controls\" id=\"", table_id, "-controls\">"),
      paste0("<button type=\"button\" id=\"", table_id,
             "-previous\" onclick=\"reviewTableMove('", table_id, "',-1)\">Previous</button>"),
      paste0("<span id=\"", table_id, "-status\">Preparing table…</span>"),
      paste0("<button type=\"button\" id=\"", table_id,
             "-next\" onclick=\"reviewTableMove('", table_id, "',1)\">Next</button>"),
      "</div>",
      "<p class=\"table-meta\">All records are embedded in this file; 100 rows are rendered at a time for browser responsiveness.</p>"
    ), connection, useBytes = TRUE)
  }
  writeLines("<div class=\"table-wrap\"><table><thead><tr>", connection, useBytes = TRUE)
  writeLines(paste0("<th>", review_report_escape(names(data)), "</th>"), connection, useBytes = TRUE)
  writeLines(paste0("</tr></thead><tbody", if (use_pagination) paste0(" id=\"", table_id, "-body\"") else "", ">"),
             connection, useBytes = TRUE)
  if (use_pagination) {
    writeLines("</tbody></table></div>", connection, useBytes = TRUE)
    writeLines(paste0(
      "<script type=\"application/json\" class=\"review-table-data\" data-table-id=\"",
      table_id, "\">", review_report_json_for_script(payload), "</script>"
    ), connection, useBytes = TRUE)
    return(invisible(NULL))
  }

  for (start in seq.int(1L, nrow(data), by = 250L)) {
    rows <- start:min(nrow(data), start + 249L)
    html_rows <- vapply(rows, function(i) {
      cells <- vapply(formatted, function(column) {
        paste0("<td>", review_report_escape(column[i]), "</td>")
      }, character(1))
      paste0("<tr", if (interest[i]) " class=\"record-interest\"" else "", ">",
             paste0(cells, collapse = ""), "</tr>")
    }, character(1))
    writeLines(html_rows, connection, useBytes = TRUE)
  }
  writeLines("</tbody></table></div>", connection, useBytes = TRUE)
  invisible(NULL)
}

write_review_html_report <- function(file, report, page = c("combined", "index", "section"), section_id = NULL) {
  page <- match.arg(page)
  report <- review_report_with_rules(report)
  stopifnot(is.list(report), length(report$summary_items) > 0L, length(report$sections) > 0L)
  connection <- base::file(file, open = "wb")
  on.exit(close(connection), add = TRUE)
  write <- function(...) writeLines(..., connection, useBytes = TRUE)

  title <- report$title %||% "ADPC / ADNCA Review"
  generated <- report$generated %||% format(Sys.time(), "%Y-%m-%dT%H:%M%z")
  source <- report$source %||% "local dataset"
  sections <- report$sections
  if (identical(page, "section")) {
    if (is.null(section_id) || !section_id %in% vapply(sections, `[[`, character(1), "id")) {
      stop("A valid section_id is required for a section report page.")
    }
  }
  section_href <- function(id) {
    id <- review_report_safe_id(id)
    if (identical(page, "combined")) paste0("#", id) else paste0(id, ".html")
  }
  summary_href <- if (identical(page, "combined")) "#review-summary" else "index.html"
  document_title <- title
  if (identical(page, "section")) {
    selected <- sections[[match(section_id, vapply(sections, `[[`, character(1), "id"))]]
    document_title <- paste(selected$title, title, sep = " - ")
  }

  write(c(
    "<!doctype html>",
    "<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    paste0("<title>", review_report_escape(document_title), "</title>"),
    "<style>",
    ":root{--nav:#17324d;--nav2:#244d70;--ink:#182532;--muted:#5d6b78;--line:#d8e0e8;--accent:#1769aa;--paper:#fff;--bg:#f4f7fa;}",
    "*{box-sizing:border-box}html{scroll-behavior:smooth}body{margin:0;background:var(--bg);color:var(--ink);font:14px/1.45 -apple-system,BlinkMacSystemFont,\"Segoe UI\",Arial,sans-serif}",
    ".toc{position:fixed;inset:0 auto 0 0;width:290px;overflow:auto;background:var(--nav);color:#fff;padding:24px 18px 32px}",
    ".toc h2{margin:0 0 18px;font-size:20px;color:#fff}.toc a{display:block;color:#eaf4ff;text-decoration:none;padding:7px 9px;border-radius:5px}.toc a:hover,.toc a:focus{background:var(--nav2)}",
    ".toc-group{margin:18px 9px 6px;color:#9fcaec;font-size:11px;font-weight:700;letter-spacing:.08em;text-transform:uppercase}",
    "main{margin-left:290px;padding:30px;max-width:calc(100vw - 290px)}section{scroll-margin-top:18px;background:var(--paper);border:1px solid var(--line);border-radius:9px;padding:24px;margin:0 0 24px;box-shadow:0 2px 8px rgba(20,45,70,.05)}",
    "h1{margin:0 0 8px;font-size:30px}h2{margin:0 0 14px;color:#153c5c}h3{margin:22px 0 8px}.metadata,.table-meta{color:var(--muted)}",
    ".summary-list{border-top:1px solid var(--line);margin-top:20px}.summary-line{padding:9px 4px;border-bottom:1px solid #e8edf2}.summary-line a{color:var(--accent);font-weight:600;text-decoration:none}.summary-line a:hover{text-decoration:underline}",
    ".section-note{white-space:pre-wrap;background:#f3f7fb;border-left:4px solid var(--accent);padding:10px 12px}.empty{color:var(--muted);font-style:italic}",
    ".table-wrap{overflow:auto;max-height:70vh;border:1px solid var(--line);border-radius:6px}table{border-collapse:separate;border-spacing:0;min-width:100%;font-size:12px}th,td{padding:6px 8px;border-right:1px solid #e4e9ee;border-bottom:1px solid #e4e9ee;text-align:left;vertical-align:top;white-space:nowrap}th{position:sticky;top:0;z-index:2;background:#eaf1f7;color:#17324d}tbody tr:nth-child(even){background:#f8fafc}.record-interest{background:#fff3cd!important;font-weight:600}",
    ".table-controls{display:flex;align-items:center;gap:12px;margin:10px 0}.table-controls button{border:1px solid #b9c6d2;border-radius:5px;background:#fff;color:#17324d;padding:5px 12px;cursor:pointer}.table-controls button:disabled{opacity:.45;cursor:not-allowed}.table-controls span{color:var(--muted);min-width:210px;text-align:center}",
    ".back{float:right;font-size:12px}footer{color:var(--muted);padding:4px 2px 30px}",
    "@media(max-width:900px){.toc{position:static;width:auto}.toc a{display:inline-block}.toc-group{display:inline-block;margin:8px}.toc-group:after{content:\":\"}main{margin-left:0;max-width:none;padding:16px}}",
    "@media print{.toc{display:none}main{margin:0;max-width:none;padding:0}section{box-shadow:none;break-inside:avoid}.table-wrap{max-height:none;overflow:visible}th{position:static}}",
    "</style></head><body><nav class=\"toc\"><h2>Review contents</h2>",
    paste0("<a href=\"", summary_href, "\">Review summary</a>")
  ))

  current_group <- NULL
  for (section in sections) {
    group <- section$group %||% "Details"
    if (!identical(group, current_group)) {
      write(paste0("<div class=\"toc-group\">", review_report_escape(group), "</div>"))
      current_group <- group
    }
    write(paste0("<a href=\"", section_href(section$id), "\">",
                 review_report_escape(section$title), "</a>"))
  }
  write("</nav><main>")
  write(paste0('<p class="metadata" style="overflow-wrap:anywhere">Dataset: ',review_report_escape(source),
    ', SHA256: ',review_report_escape(report$source_sha256 %||% 'Unavailable (reload source file)'),'</p>'))
  if (!identical(page, "section")) {
    write(c("<section id=\"review-summary\">",
            paste0("<h1>", review_report_escape(title), "</h1>"),
            paste0("<p class=\"metadata\">Generated ", review_report_escape(generated),
                   " · Source: ", review_report_escape(source), "</p>"),
            "<div class=\"summary-list\">"))
    for (item in report$summary_items) {
      text <- review_report_escape(item$text %||% "")
      item_section_id <- item$section_id %||% ""
      content <- if (nzchar(item_section_id)) {
        paste0("<a href=\"", section_href(item_section_id), "\">", text, "</a>")
      } else text
      write(paste0("<div class=\"summary-line\">", content, "</div>"))
    }
    write("</div></section>")
  }

  sections_to_write <- if (identical(page, "index")) {
    list()
  } else if (identical(page, "section")) {
    sections[vapply(sections, function(section) identical(section$id, section_id), logical(1))]
  } else sections
  for (section in sections_to_write) {
    write(c(
      paste0("<section id=\"", review_report_safe_id(section$id), "\">"),
      paste0("<a class=\"back\" href=\"", summary_href, "\">Back to summary</a>"),
      paste0("<h2>", review_report_escape(section$title), "</h2>")
    ))
    if (nzchar(section$description %||% "")) {
      write(paste0("<div class=\"section-note\">", review_report_escape(section$description), "</div>"))
    }
    tables <- section$tables %||% list()
    for (block in section$blocks %||% list()) {
      write(paste0('<h3>', review_report_escape(block$title), '</h3><p style="line-height:1.65;max-width:100ch">', review_report_escape(block$text), '</p>'))
      if (!is.null(block$target)) write(paste0('<p><a href="', section_href(block$target), '">Open this review item</a></p>'))
    }
    for (table_index in seq_along(tables)) {
      table <- tables[[table_index]]
      review_report_write_table(
        connection, table$data %||% data.frame(), table$title %||% NULL,
        table$interest_column %||% NULL,
        table_id = paste0(review_report_safe_id(section$id), "-table-", table_index)
      )
    }
    write("</section>")
  }
  write(c(
    "<footer>Standalone report generated by ADPC / ADNCA Review.</footer>",
    "<script>",
    "const reviewTableState={};",
    "function reviewTableRender(id){const s=reviewTableState[id],body=document.getElementById(id+'-body');if(!s||!body)return;const total=s.data.rows.length,pages=Math.max(1,Math.ceil(total/s.data.pageSize));s.page=Math.max(0,Math.min(s.page,pages-1));const first=s.page*s.data.pageSize,last=Math.min(total,first+s.data.pageSize);body.textContent='';const fragment=document.createDocumentFragment();for(let i=first;i<last;i++){const tr=document.createElement('tr');if(s.data.interest[i])tr.className='record-interest';for(const value of s.data.rows[i]){const td=document.createElement('td');td.textContent=value??'';tr.appendChild(td)}fragment.appendChild(tr)}body.appendChild(fragment);document.getElementById(id+'-status').textContent='Rows '+(total?first+1:0)+'–'+last+' of '+total+' · Page '+(s.page+1)+' of '+pages;document.getElementById(id+'-previous').disabled=s.page===0;document.getElementById(id+'-next').disabled=s.page>=pages-1}",
    "function reviewTableMove(id,delta){const s=reviewTableState[id];if(!s)return;s.page+=delta;reviewTableRender(id)}",
    "document.addEventListener('DOMContentLoaded',()=>{document.querySelectorAll('script.review-table-data').forEach(node=>{const id=node.dataset.tableId;reviewTableState[id]={data:JSON.parse(node.textContent),page:0};reviewTableRender(id)})});",
    "</script>",
    "</main></body></html>"
  ))
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}

write_review_html_bundle <- function(file, report, bundle_name) {
  report <- review_report_with_rules(report)
  bundle_name <- as.character(bundle_name)[1]
  if (!nzchar(bundle_name) || !identical(bundle_name, basename(bundle_name)) ||
      grepl("[^A-Za-z0-9._-]", bundle_name)) {
    stop("bundle_name must be a non-empty, filesystem-safe folder name.")
  }
  build_root <- tempfile("adpc-review-html-")
  bundle_dir <- file.path(build_root, bundle_name)
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(build_root, recursive = TRUE, force = TRUE), add = TRUE)

  write_review_html_report(file.path(bundle_dir, "index.html"), report, page = "index")
  for (section in report$sections) {
    section_file <- file.path(bundle_dir, paste0(review_report_safe_id(section$id), ".html"))
    write_review_html_report(
      section_file, report, page = "section", section_id = section$id
    )
  }

  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("The 'zip' package is required to create the multi-page HTML review download.")
  }
  zip::zipr(zipfile = file, files = bundle_name, root = build_root, include_directories = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}
