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
#' @param advice An optional grounded \code{Advice} object returned by
#'   \code{\link{es_advise}(task_type = "report_writing")}. When supplied and
#'   a valid \code{Advice}, renders a new \strong{AI Advisor Interpretation}
#'   section in the report. When \code{NULL} (the default), the existing render
#'   path is completely unchanged (byte-identical output). A supplied but
#'   invalid \code{advice} (not an \code{Advice} object) is silently coerced to
#'   \code{NULL} with exactly one \code{warning()} — the report is never broken.
#' @param narrative An optional named list keyed by section (e.g.
#'   \code{list(exec_summary = "...", data_methods = "...", results = "...",
#'   robustness = "...")}), typically the prose sections from an
#'   \code{\link{es_advise}(task_type = "report_writing")} offline or LLM call.
#'   When \code{NULL} (the default), the render output is byte-identical to the
#'   v0.63.x baseline (REPORT-03 backward-compat). A supplied but invalid
#'   \code{narrative} (not a named list) is silently coerced to \code{NULL}
#'   with exactly one \code{warning()} — the report is never broken.
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
                              advice = NULL,
                              narrative = NULL,
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

  # Validate advice param: a supplied non-Advice degrades gracefully (one warning,
  # skip section) — never breaks the report (ADV-07, T-07-05).
  if (!is.null(advice) && !inherits(advice, "Advice")) {
    warning(
      "generate_report(): 'advice' is not an Advice object \u2014 advice section will be skipped.",
      call. = FALSE
    )
    advice <- NULL
  }

  # Validate narrative param: a supplied non-list degrades gracefully (REPORT-03).
  # NULL path is byte-identical to baseline (no template change when NULL).
  if (!is.null(narrative)) {
    if (!is.list(narrative)) {
      warning(
        "generate_report(): 'narrative' must be a named list or NULL -- narrative will be skipped.",
        call. = FALSE
      )
      narrative <- NULL
    } else {
      # Each element must be a character scalar (or NULL) -- non-character values
      # would either silently coerce or crash inside the knitr template (CR-03).
      bad <- !vapply(narrative, function(v) {
        is.null(v) || (is.character(v) && length(v) == 1L)
      }, logical(1L))
      if (any(bad)) {
        warning(
          sprintf(
            "generate_report(): narrative section(s) [%s] are not character scalars -- narrative will be skipped.",
            paste(names(narrative)[bad], collapse = ", ")
          ),
          call. = FALSE
        )
        narrative <- NULL
      }
    }
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
      interactive = interactive,
      advice = advice,
      narrative = narrative
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE,
    ...
  )

  output_path <- file.path(output_dir, output_name)
  message("Report generated: ", output_path)
  invisible(output_path)
}
