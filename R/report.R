#' Generate Event Study Report
#'
#' Renders an automated HTML or PDF report from a completed event study task.
#' Uses a bundled RMarkdown template with configurable sections.
#'
#' @param task A fitted \code{EventStudyTask} or \code{PanelEventStudyTask}.
#' @param output_file Output file path. Default \code{"event_study_report.html"}.
#' @param format Output format: \code{"html"} (default) or \code{"pdf"}.
#' @param title Report title.
#' @param author Author name (optional).
#' @param sections Character vector of sections to include. Any subset of:
#'   \code{"summary"}, \code{"data"}, \code{"diagnostics"}, \code{"single_event"},
#'   \code{"multi_event"}, \code{"cross_sectional"}, \code{"appendix"}.
#' @param cross_sectional Optional cross-sectional regression results to include.
#' @param confidence_level Confidence level for plots. Default 0.95.
#' @param interactive Logical. Use interactive plotly plots in HTML output.
#'   Default TRUE.
#' @param ... Additional arguments passed to \code{rmarkdown::render}.
#'
#' @return The path to the generated report (invisibly).
#'
#' @export
generate_report <- function(task,
                              output_file = "event_study_report.html",
                              format = c("html", "pdf"),
                              title = "Event Study Report",
                              author = NULL,
                              sections = c("summary", "data", "diagnostics",
                                           "single_event", "multi_event",
                                           "cross_sectional", "appendix"),
                              cross_sectional = NULL,
                              confidence_level = 0.95,
                              interactive = TRUE,
                              ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for report generation. ",
         "Install it with: install.packages('rmarkdown')")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for report generation. ",
         "Install it with: install.packages('knitr')")
  }

  if (!inherits(task, "EventStudyTask") &&
      !inherits(task, "PanelEventStudyTask")) {
    stop("task must be an EventStudyTask or PanelEventStudyTask.")
  }

  format <- match.arg(format)

  # Locate the template
  template_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )

  if (template_path == "") {
    stop("Report template not found. Ensure the package is properly installed.")
  }

  # Build output format
  if (format == "html") {
    output_format <- rmarkdown::html_document(
      toc = TRUE, toc_float = TRUE,
      theme = "flatly", code_folding = "hide"
    )
  } else {
    output_format <- rmarkdown::pdf_document(toc = TRUE)
  }

  # Ensure output file has correct extension
  ext <- if (format == "html") ".html" else ".pdf"
  if (!grepl(paste0("\\", ext, "$"), output_file)) {
    output_file <- paste0(tools::file_path_sans_ext(output_file), ext)
  }

  # Resolve output directory
  output_dir <- dirname(output_file)
  output_name <- basename(output_file)

  if (output_dir == ".") {
    output_dir <- getwd()
  }

  # Render
  rmarkdown::render(
    input = template_path,
    output_format = output_format,
    output_file = output_name,
    output_dir = output_dir,
    params = list(
      task = task,
      title = title,
      author = author %||% "",
      sections = sections,
      cross_sectional = cross_sectional,
      confidence_level = confidence_level,
      interactive = interactive
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE,
    ...
  )

  output_path <- file.path(output_dir, output_name)
  message("Report generated: ", output_path)
  invisible(output_path)
}
