# Excel export for the ADPC / ADNCA review summary and linked audit/finding data.

review_excel_filename <- function(data = NULL, source_path = NULL, time = Sys.time()) {
  dataset <- if (exists("review_report_dataset_name", mode = "function")) {
    review_report_dataset_name(data, source_path)
  } else {
    "adpc"
  }
  paste0(dataset, "_review_", format(time, "%Y-%m-%dT%H-%M"), ".xlsx")
}

review_excel_sheet_label <- function(section) {
  prefix <- if (identical(section$group %||% "", "Audits")) "Audit - " else ""
  label <- paste0(prefix, section$title %||% section$id %||% "Review item")
  # Fixed substitutions avoid a nested character-class expression silently
  # leaving forbidden slash/bracket characters in OOXML worksheet names.
  for (character in c("[", "]", ":", "*", "?", "/", "\\")) {
    label <- gsub(character, " ", label, fixed = TRUE)
  }
  label <- gsub("[[:cntrl:]]", " ", label)
  label <- gsub("[[:space:]]+", " ", trimws(label))
  label <- gsub("^'+|'+$", "", label)
  if (!nzchar(label)) label <- "Review item"
  substr(label, 1L, 31L)
}

review_excel_sheet_names <- function(sections, reserved = c("Summary", "Plot data")) {
  used <- toupper(reserved)
  output <- character(length(sections))
  for (i in seq_along(sections)) {
    base <- review_excel_sheet_label(sections[[i]])
    candidate <- base
    suffix <- 1L
    while (toupper(candidate) %in% used) {
      suffix <- suffix + 1L
      marker <- paste0(" (", suffix, ")")
      candidate <- paste0(substr(base, 1L, 31L - nchar(marker)), marker)
    }
    output[i] <- candidate
    used <- c(used, toupper(candidate))
  }
  output
}

review_excel_clean_data <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  # Excel table headers are case-insensitive. Prepared PC data can contain both
  # an uppercase preparation flag and its lowercase working review flag.
  # Preserve every column/value, giving only conflicting headers a suffix.
  headers <- names(data)
  used <- character()
  for (i in seq_along(headers)) {
    base <- headers[i]
    if (is.na(base) || !nzchar(trimws(base))) base <- paste0("Column ", i)
    candidate <- base
    suffix <- 1L
    while (tolower(candidate) %in% used) {
      suffix <- suffix + 1L
      candidate <- paste0(base, " (", suffix, ")")
    }
    headers[i] <- candidate
    used <- c(used, tolower(candidate))
  }
  names(data) <- headers
  for (name in names(data)) {
    if (is.factor(data[[name]])) data[[name]] <- as.character(data[[name]])
    if (inherits(data[[name]], "haven_labelled")) data[[name]] <- unclass(data[[name]])
    if (is.character(data[[name]])) {
      blank <- !is.na(data[[name]]) & !nzchar(data[[name]])
      data[[name]][blank] <- NA_character_
    }
  }
  data
}

review_excel_column_widths <- function(data, maximum = 42) {
  data <- review_excel_clean_data(data)
  if (!ncol(data)) return(numeric())
  vapply(seq_along(data), function(i) {
    values <- data[[i]]
    if (length(values) > 500L) {
      values <- values[unique(c(seq_len(250L), seq.int(length(values) - 249L, length(values))))]
    }
    display <- as.character(values)
    display[is.na(display)] <- ""
    longest <- max(c(nchar(names(data)[i], type = "width"), nchar(display, type = "width")), na.rm = TRUE)
    min(maximum, max(10, longest + 2))
  }, numeric(1))
}

review_excel_write_link <- function(wb, sheet, row, col, target_sheet, text, link_style) {
  target <- paste0("#'", gsub("'", "''", target_sheet, fixed = TRUE), "'!A1")
  names(target) <- text
  class(target) <- "hyperlink"
  openxlsx::writeData(wb, sheet, x = target, startRow = row, startCol = col,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet, link_style, rows = row, cols = col, stack = TRUE)
}

review_excel_repair_package <- function(file) {
  work_dir <- tempfile("pc-review-xlsx-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(file, exdir = work_dir)

  relationship_files <- list.files(
    file.path(work_dir, "xl", "worksheets", "_rels"),
    pattern = "[.]rels$", full.names = TRUE
  )
  for (relationship_file in relationship_files) {
    xml <- paste(readLines(relationship_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
    relationships <- regmatches(
      xml,
      gregexpr("<Relationship[^>]*/>", xml, perl = TRUE)
    )[[1]]
    if (!length(relationships) || identical(relationships, character(0)) ||
        identical(relationships, "")) next

    removed_ids <- character()
    relationship_base <- dirname(dirname(relationship_file))
    for (relationship in relationships) {
      if (grepl('TargetMode="External"', relationship, fixed = TRUE)) next
      target <- sub('.* Target="([^"]+)".*', "\\1", relationship)
      id <- sub('.* Id="([^"]+)".*', "\\1", relationship)
      target_file <- normalizePath(file.path(relationship_base, target), winslash = "/",
                                   mustWork = FALSE)
      if (!file.exists(target_file)) {
        xml <- gsub(relationship, "", xml, fixed = TRUE)
        removed_ids <- c(removed_ids, id)
      }
    }
    if (!length(removed_ids)) next
    writeLines(xml, relationship_file, useBytes = TRUE)

    sheet_file <- file.path(
      relationship_base,
      sub("[.]rels$", "", basename(relationship_file))
    )
    if (file.exists(sheet_file)) {
      sheet_xml <- paste(readLines(sheet_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
      for (id in removed_ids) {
        escaped_id <- gsub("([][{}()+*^$|\\?.])", "\\\\\\1", id)
        sheet_xml <- gsub(
          paste0("<(drawing|legacyDrawing)[^>]*r:id=\"", escaped_id, "\"[^>]*/>"),
          "", sheet_xml, perl = TRUE
        )
        sheet_xml <- gsub(paste0(' r:id="', id, '"'), "", sheet_xml, fixed = TRUE)
      }
      writeLines(sheet_xml, sheet_file, useBytes = TRUE)
    }
  }

  content_types_file <- file.path(work_dir, "[Content_Types].xml")
  if (file.exists(content_types_file)) {
    content_types <- paste(readLines(content_types_file, warn = FALSE, encoding = "UTF-8"),
                           collapse = "")
    overrides <- regmatches(
      content_types,
      gregexpr("<Override[^>]*/>", content_types, perl = TRUE)
    )[[1]]
    if (length(overrides) && !identical(overrides, "")) {
      for (override in overrides) {
        part_name <- sub('.* PartName="/([^"]+)".*', "\\1", override)
        if (!file.exists(file.path(work_dir, part_name))) {
          content_types <- gsub(override, "", content_types, fixed = TRUE)
        }
      }
      writeLines(content_types, content_types_file, useBytes = TRUE)
    }
  }

  repaired <- tempfile(fileext = ".xlsx")
  archive_files <- list.files(work_dir, recursive = TRUE, all.files = TRUE,
                              include.dirs = FALSE, no.. = TRUE)
  zip::zipr(repaired, archive_files, root = work_dir, include_directories = FALSE,
            mode = "mirror")
  if (!file.copy(repaired, file, overwrite = TRUE)) {
    stop("Could not finalize the Excel review workbook.")
  }
  unlink(repaired, force = TRUE)
  invisible(file)
}

write_review_excel_report <- function(file, report) {
  stopifnot(is.list(report), length(report$summary_items) > 0L, length(report$sections) > 0L)

  sections <- report$sections
  section_sheets <- review_excel_sheet_names(sections)
  names(section_sheets) <- vapply(sections, function(x) x$id %||% "", character(1))

  wb <- openxlsx::createWorkbook(creator = "ADPC / ADNCA Review")
  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10, fontColour = "#243447")

  title_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold",
                                        fontColour = "#20364F")
  section_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10,
                                          textDecoration = "bold", fontColour = "#FFFFFF",
                                          fgFill = "#244A73", halign = "left", valign = "center")
  header_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10,
                                         textDecoration = "bold", fontColour = "#FFFFFF",
                                         fgFill = "#376F9F", halign = "center", valign = "center",
                                         border = "Bottom", borderColour = "#FFFFFF")
  note_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, fontColour = "#3D4B59",
                                       wrapText = TRUE, valign = "top")
  description_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10,
                                              fontColour = "#3D4B59", wrapText = FALSE,
                                              valign = "top")
  meta_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 9, fontColour = "#687785",
                                       textDecoration = "italic", wrapText = TRUE)
  link_style <- openxlsx::createStyle(fontName = "Arial", fontSize = 10, fontColour = "#0563C1",
                                       textDecoration = "underline", halign = "center", valign = "center")
  summary_border_style <- openxlsx::createStyle(
    border = c("top", "bottom", "left", "right"), borderColour = "#D6DFE8",
    valign = "center")
  interest_style <- openxlsx::createStyle(fgFill = "#FFF2CC", fontColour = "#5F4500",
                                           textDecoration = "bold")
  warning_style <- openxlsx::createStyle(fgFill = "#FFF2CC", fontColour = "#5F4500",
                                          wrapText = FALSE, valign = "center")
  warning_label_style <- openxlsx::createStyle(fgFill = "#FFF2CC", fontColour = "#5F4500",
                                                textDecoration = "bold", valign = "center")

  openxlsx::addWorksheet(wb, "Summary", gridLines = FALSE, tabColour = "#20364F", zoom = 90)
  openxlsx::writeData(wb, "Summary", report$title %||% "ADPC / ADNCA Review", startRow = 2, startCol = 1)
  openxlsx::addStyle(wb, "Summary", title_style, rows = 2, cols = 1)
  openxlsx::writeData(wb, "Summary", "Generated", startRow = 4, startCol = 1)
  openxlsx::writeData(wb, "Summary", report$generated %||% "", startRow = 4, startCol = 2)
  openxlsx::writeData(wb, "Summary", "Dataset", startRow = 5, startCol = 1)
  openxlsx::writeData(wb, "Summary", report$source %||% "", startRow = 5, startCol = 2)
  openxlsx::addStyle(wb, "Summary", meta_style, rows = 4:5, cols = 1:2, gridExpand = TRUE)
  openxlsx::writeData(wb,"Summary","SHA256",startRow=6,startCol=1)
  openxlsx::writeData(wb,"Summary",report$source_sha256 %||% "Unavailable (reload source file)",startRow=6,startCol=2)
  openxlsx::addStyle(wb,"Summary",meta_style,rows=6,cols=1:2,gridExpand=TRUE)

  write_identity <- function(sheet) {
    identity_style <- openxlsx::createStyle(fontName="Arial",fontSize=10,wrapText=TRUE,valign="top")
    openxlsx::writeData(wb,sheet,paste0("Dataset: ",report$source %||% "local dataset"),startRow=2,startCol=1)
    openxlsx::writeData(wb,sheet,paste0("SHA256: ",report$source_sha256 %||% "Unavailable (reload source file)"),startRow=4,startCol=1)
    for(row in c(2L,4L)) {
      openxlsx::mergeCells(wb,sheet,cols=1:8,rows=row)
      openxlsx::addStyle(wb,sheet,identity_style,rows=row,cols=1:8,gridExpand=TRUE)
      openxlsx::setRowHeights(wb,sheet,rows=row,heights=30)
    }
  }

  openxlsx::writeData(wb, "Summary", "Review summary", startRow = 7, startCol = 1)
  openxlsx::addStyle(wb, "Summary", section_style, rows = 7, cols = 1:2, gridExpand = TRUE)
  openxlsx::writeData(wb, "Summary", matrix(c("Open", "Summary item"), nrow = 1),
                      startRow = 8, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "Summary", header_style, rows = 8, cols = 1:2, gridExpand = TRUE)

  summary_row <- 9L
  for (item in report$summary_items) {
    openxlsx::writeData(wb, "Summary", item$text %||% "", startRow = summary_row, startCol = 2)
    summary_lines <- sum(pmax(1L, ceiling(nchar(strsplit(item$text %||% "", "\n", fixed=TRUE)[[1]]) / 88)))
    openxlsx::setRowHeights(wb, "Summary", rows=summary_row, heights=min(409,max(30,summary_lines*15)))
    section_id <- item$section_id %||% ""
    if (nzchar(section_id) && section_id %in% names(section_sheets)) {
      review_excel_write_link(wb, "Summary", summary_row, 1, section_sheets[[section_id]], "Open", link_style)
    }
    summary_row <- summary_row + 1L
  }
  if (summary_row > 9L) {
    openxlsx::addStyle(wb, "Summary", note_style, rows = 9:(summary_row - 1L), cols = 2, stack = TRUE)
    openxlsx::addStyle(wb, "Summary", summary_border_style,
      rows = 9:(summary_row - 1L), cols = 1:2, gridExpand = TRUE, stack = TRUE)
  }

  openxlsx::writeData(wb, "Summary", "Contents", startRow = 7, startCol = 4)
  openxlsx::addStyle(wb, "Summary", section_style, rows = 7, cols = 4:6, gridExpand = TRUE)
  openxlsx::writeData(wb, "Summary", matrix(c("Group", "Topic", "Open"), nrow = 1),
                      startRow = 8, startCol = 4, colNames = FALSE)
  openxlsx::addStyle(wb, "Summary", header_style, rows = 8, cols = 4:6, gridExpand = TRUE)
  for (i in seq_along(sections)) {
    row <- 8L + i
    openxlsx::writeData(
      wb, "Summary",
      data.frame(Group = sections[[i]]$group %||% "", Topic = sections[[i]]$title %||% "",
                 stringsAsFactors = FALSE),
      startRow = row, startCol = 4, colNames = FALSE
    )
    review_excel_write_link(wb, "Summary", row, 6, section_sheets[i], "Open", link_style)
  }
  if (!is.null(report$plot_data) && nrow(as.data.frame(report$plot_data)) > 0L) {
    row <- 9L + length(sections)
    openxlsx::writeData(wb, "Summary", data.frame(Group = "Tools", Topic = "Plot-ready concentration data"),
                        startRow = row, startCol = 4, colNames = FALSE)
    review_excel_write_link(wb, "Summary", row, 6, "Plot data", "Open", link_style)
  }
  openxlsx::setColWidths(wb, "Summary", cols = c(1, 2, 3, 4, 5, 6),
                         widths = c(10, 92, 3, 18, 38, 10))
  contents_end <- 8L + length(sections) + as.integer(
    !is.null(report$plot_data) && nrow(as.data.frame(report$plot_data)) > 0L)
  openxlsx::addStyle(wb, "Summary", summary_border_style,
    rows = 9:contents_end, cols = 4:6, gridExpand = TRUE, stack = TRUE)
  openxlsx::freezePane(wb, "Summary", firstActiveRow = 9)

  table_number <- 0L
  for (i in seq_along(sections)) {
    section <- sections[[i]]
    sheet <- section_sheets[i]
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE, zoom = 85)
    write_identity(sheet)
    review_excel_write_link(wb, sheet, 1, 1, "Summary", "Back to Summary", link_style)
    openxlsx::writeData(wb, sheet, section$title %||% "Review item", startRow = 3, startCol = 1)
    openxlsx::addStyle(wb, sheet, title_style, rows = 3, cols = 1)
    openxlsx::writeData(wb, sheet, section$description %||% "", startRow = 5, startCol = 1)
    openxlsx::addStyle(wb, sheet, description_style, rows = 5, cols = 1)
    openxlsx::setRowHeights(wb, sheet, rows = 5, heights = 48)

    next_row <- 8L
    first_header_row <- NULL
    maximum_columns <- 1L
    maximum_widths <- numeric()
    for (table in section$tables %||% list()) {
      data <- review_excel_clean_data(table$data %||% data.frame())
      openxlsx::writeData(wb, sheet, table$title %||% "Data", startRow = next_row, startCol = 1)
      openxlsx::addStyle(wb, sheet, section_style, rows = next_row,
                         cols = seq_len(max(1L, ncol(data))), gridExpand = TRUE)
      if (!nrow(data) || !ncol(data)) {
        openxlsx::writeData(wb, sheet, "No relevant observations.", startRow = next_row + 2L, startCol = 1)
        openxlsx::addStyle(wb, sheet, meta_style, rows = next_row + 2L, cols = 1)
        next_row <- next_row + 5L
        next
      }

      openxlsx::writeData(
        wb, sheet,
        sprintf("%s records; %s columns", format(nrow(data), big.mark = ","), ncol(data)),
        startRow = next_row + 1L, startCol = 1
      )
      openxlsx::addStyle(wb, sheet, meta_style, rows = next_row + 1L, cols = 1)
      header_row <- next_row + 3L
      if (is.null(first_header_row)) first_header_row <- header_row
      table_number <- table_number + 1L
      openxlsx::writeDataTable(
        wb, sheet, data,
        startRow = header_row, startCol = 1, rowNames = FALSE, withFilter = TRUE,
        tableStyle = "TableStyleMedium2", tableName = sprintf("ReviewTable%03d", table_number)
      )

      interest_column <- table$interest_column %||% ""
      if (nzchar(interest_column) && interest_column %in% names(data)) {
        interest_rows <- which(toupper(trimws(as.character(data[[interest_column]]))) == "YES")
        if (length(interest_rows)) {
          openxlsx::addStyle(
            wb, sheet, interest_style,
            rows = header_row + interest_rows, cols = seq_len(ncol(data)),
            gridExpand = TRUE, stack = TRUE
          )
        }
      }

      widths <- review_excel_column_widths(data)
      if (length(widths) > length(maximum_widths)) length(maximum_widths) <- length(widths)
      maximum_widths[seq_along(widths)] <- pmax(maximum_widths[seq_along(widths)], widths, na.rm = TRUE)
      maximum_columns <- max(maximum_columns, ncol(data))
      next_row <- header_row + nrow(data) + 3L
    }

    if (length(maximum_widths)) {
      maximum_widths[is.na(maximum_widths)] <- 12
      openxlsx::setColWidths(wb, sheet, cols = seq_along(maximum_widths), widths = maximum_widths)
    } else {
      openxlsx::setColWidths(wb, sheet, cols = 1, widths = 42)
    }
    # Input requests are often longer than the first identifier column. Merge
    # the description across a bounded area and size it without moving tables.
    description_columns <- min(8L,max(4L,maximum_columns))
    openxlsx::mergeCells(wb,sheet,cols=seq_len(description_columns),rows=5)
    description_widths <- rep(12,description_columns)
    if(length(maximum_widths)) description_widths[seq_len(min(length(maximum_widths),description_columns))] <- head(maximum_widths,description_columns)
    if(!length(maximum_widths)) description_widths[1] <- 42
    description_lines <- sum(pmax(1L,ceiling(nchar(strsplit(section$description %||% "", "\n",fixed=TRUE)[[1]]) / max(40,sum(description_widths)*.85))))
    openxlsx::setRowHeights(wb,sheet,rows=5,heights=min(409,max(48,description_lines*15)))
    if (!is.null(first_header_row)) {
      openxlsx::freezePane(
        wb, sheet, firstActiveRow = first_header_row + 1L,
        firstActiveCol = min(3L, maximum_columns + 1L)
      )
    }
  }

  plot_data <- review_excel_clean_data(report$plot_data %||% data.frame())
  if (nrow(plot_data) > 0L && ncol(plot_data) > 0L) {
    series_columns <- intersect(c("Subject", "PCTEST", "AVISIT", "VISITCD"), names(plot_data))
    if (length(series_columns)) {
      series <- do.call(paste, c(lapply(plot_data[series_columns], as.character), sep = " | "))
      plot_data <- data.frame(PLOT_SERIES = series, plot_data, check.names = FALSE,
                              stringsAsFactors = FALSE)
    }
    openxlsx::addWorksheet(wb, "Plot data", gridLines = FALSE, tabColour = "#4F81BD", zoom = 85)
    write_identity("Plot data")
    review_excel_write_link(wb, "Plot data", 1, 1, "Summary", "Back to Summary", link_style)
    openxlsx::writeData(wb, "Plot data", "Plot-ready concentration data", startRow = 3, startCol = 1)
    openxlsx::addStyle(wb, "Plot data", title_style, rows = 3, cols = 1)
    instructions <- matrix(c(
      "Filter", "Filter Subject, PCTEST, profile/visit, and any review flags to the records you want to plot.",
      "X and Y", "Use NRRLT, ARRLT, or MRRLT as X and AVAL as Y (mapped source units), then choose Insert > Scatter in Excel.",
      "Series", "PLOT_SERIES is a ready grouping label. No VBA or macros are included."
    ), ncol = 2, byrow = TRUE)
    openxlsx::writeData(wb, "Plot data", instructions, startRow = 5, startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, "Plot data", warning_style, rows = 5:7,
                       cols = seq_len(min(8L, ncol(plot_data))), gridExpand = TRUE)
    openxlsx::addStyle(wb, "Plot data", warning_label_style, rows = 5:7, cols = 1,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, "Plot data", rows = 5:7, heights = 22)
    openxlsx::writeDataTable(
      wb, "Plot data", plot_data, startRow = 10, startCol = 1,
      rowNames = FALSE, withFilter = TRUE, tableStyle = "TableStyleMedium2", tableName = "PlotDataTable"
    )
    if ("REVIEW" %in% names(plot_data)) {
      flagged <- which(toupper(trimws(as.character(plot_data$REVIEW))) == "Y")
      if (length(flagged)) {
        openxlsx::addStyle(wb, "Plot data", interest_style,
                           rows = 10L + flagged, cols = seq_len(ncol(plot_data)),
                           gridExpand = TRUE, stack = TRUE)
      }
    }
    openxlsx::setColWidths(wb, "Plot data", cols = seq_len(ncol(plot_data)),
                           widths = review_excel_column_widths(plot_data))
    openxlsx::freezePane(wb, "Plot data", firstActiveRow = 11, firstActiveCol = 3)
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  review_excel_repair_package(file)
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}
