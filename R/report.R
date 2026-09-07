#' Generate Event Study Report (Multi-Format)
#'
#' Renders an automated report from a completed event study task in one or more
#' output formats. Uses a bundled RMarkdown template with configurable sections.
#' The narrative is assembled ONCE before the format loop -- \code{es_advise} is
#' never called per format (NARR-01).
#'
#' @param task A fitted \code{EventStudyTask} or \code{PanelEventStudyTask}.
#' @param output_file Output file path (extension overridden per format).
#'   Default \code{"event_study_report.html"}.
#' @param format Character vector of output formats, any subset of
#'   \code{c("html", "pdf", "word", "md")}. Default \code{"html"}.
#'   Multiple formats render one file per format sharing a common basename.
#'   Missing optional toolchains emit one \code{message()} each and are skipped;
#'   only inability to render HTML raises \code{stop()}.
#' @param title Report title string.
#' @param author Author name (optional, default \code{""}).
#' @param sections Character vector of sections to include. Default is all six
#'   fixed sections:
#'   \code{c("exec_summary","data_methods","results","diagnostics","robustness","references")}.
#' @param cross_sectional Optional cross-sectional regression results to include.
#' @param confidence_level Confidence level for plots. Default 0.95.
#' @param interactive Logical. Use interactive plotly plots in HTML output.
#'   Default TRUE.
#' @param advice An optional grounded \code{Advice} object returned by
#'   \code{\link{es_advise}(task_type = "report_writing")}. When supplied and
#'   valid, renders an \strong{AI Advisor Interpretation} section. A supplied but
#'   invalid \code{advice} is silently coerced to \code{NULL} with one
#'   \code{warning()} -- the report is never broken.
#' @param narrative An optional named list keyed by section (e.g.
#'   \code{list(exec_summary = "...", data_methods = "...", results = "...",
#'   robustness = "...")}), typically from
#'   \code{\link{assemble_report_narrative}()}. When \code{NULL} (default), the
#'   render output is byte-identical to the v0.63.x baseline. A supplied but
#'   invalid \code{narrative} is silently coerced to \code{NULL} with one
#'   \code{warning()}.
#' @param provider Optional LLM provider passed to
#'   \code{\link{assemble_report_narrative}()} when \code{narrative} is
#'   \code{NULL}. If both \code{provider} and \code{narrative} are \code{NULL},
#'   a fully offline narrative is assembled. Ignored when a pre-built
#'   \code{narrative} list is supplied by the caller.
#' @param ... Additional arguments passed to \code{rmarkdown::render}.
#'
#' @return A named character vector of output file paths, invisibly, keyed by
#'   format name (e.g. \code{c(html = "/tmp/report.html", pdf = "/tmp/report.pdf")}).
#'   A single-format call returns a length-1 named vector: \code{result[["html"]]}
#'   and \code{result[[1L]]} both resolve (backward-compatible with prior
#'   single-path callers).
#'
#' @examples
#' \dontrun{
#' task <- run_event_study(my_task, ParameterSet$new())
#' # Single format (backward-compatible)
#' path <- generate_report(task, format = "html")
#' # Multi-format
#' paths <- generate_report(task, format = c("html", "pdf"))
#' paths[["html"]]
#' }
#'
#' @export
generate_report <- function(task,
                            output_file = "event_study_report.html",
                            format = "html",
                            title = "Event Study Report",
                            author = NULL,
                            sections = c("exec_summary", "data_methods", "results",
                                         "diagnostics", "robustness", "references"),
                            cross_sectional = NULL,
                            confidence_level = 0.95,
                            interactive = TRUE,
                            advice = NULL,
                            narrative = NULL,
                            provider = NULL,
                            ...) {

  # ---- 1. Hard deps: rmarkdown + knitr required for HTML baseline ----
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for report generation. ",
         "Install it with: install.packages('rmarkdown')")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for report generation. ",
         "Install it with: install.packages('knitr')")
  }

  # ---- 2. Task type guard ----
  if (!inherits(task, "EventStudyTask") &&
      !inherits(task, "PanelEventStudyTask")) {
    stop("task must be an EventStudyTask or PanelEventStudyTask.")
  }

  # ---- 3. Format vector validation (FORMAT-01) ----
  # Accept a character vector; intersect with allowed values (not match.arg).
  valid_formats <- c("html", "pdf", "word", "md")
  formats <- intersect(format, valid_formats)
  if (length(formats) == 0L) {
    stop("No valid format specified. 'format' must be one or more of: ",
         paste(valid_formats, collapse = ", "))
  }

  # ---- 4. Locate bundled template ----
  template_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  if (template_path == "") {
    stop("Report template not found. Ensure the package is properly installed.")
  }

  # ---- 5. Output base name and directory ----
  basename_no_ext <- tools::file_path_sans_ext(basename(output_file))
  output_dir      <- dirname(output_file)
  if (output_dir == ".") {
    output_dir <- getwd()
  }

  ext_map <- c(html = ".html", pdf = ".pdf", word = ".docx", md = ".md")

  # ---- 6. Validate advice param ----
  if (!is.null(advice) && !inherits(advice, "Advice")) {
    warning(
      "generate_report(): 'advice' is not an Advice object -- advice section will be skipped.",
      call. = FALSE
    )
    advice <- NULL
  }

  # ---- 7. Validate / assemble narrative (NARR-01) ----
  # Narrative is assembled ONCE before the format loop; the loop never calls
  # es_advise() or assemble_report_narrative() (LLM call budget enforcer).
  #
  # The narrative list may contain:
  #   - character scalar prose sections (exec_summary, data_methods, results, robustness)
  #   - metadata fields (section_sources = named list, report_mode = character scalar)
  # The validator only rejects elements that are non-NULL, non-character, and
  # NOT one of the known non-prose metadata fields (section_sources, report_mode).
  if (!is.null(narrative)) {
    if (!is.list(narrative)) {
      warning(
        "generate_report(): 'narrative' must be a named list or NULL -- narrative will be skipped.",
        call. = FALSE
      )
      narrative <- NULL
    } else {
      # Metadata fields are allowed to be non-character (e.g. section_sources is a list)
      metadata_keys <- c("section_sources", "report_mode")
      prose_keys    <- setdiff(names(narrative), metadata_keys)
      bad <- !vapply(narrative[prose_keys], function(v) {
        is.null(v) || (is.character(v) && length(v) == 1L)
      }, logical(1L))
      if (any(bad)) {
        warning(
          sprintf(
            "generate_report(): narrative section(s) [%s] are not character scalars -- narrative will be skipped.",
            paste(prose_keys[bad], collapse = ", ")
          ),
          call. = FALSE
        )
        narrative <- NULL
      }
    }
  }

  # When narrative was not supplied (or was coerced to NULL), assemble it once.
  # If provider is NULL, this produces a fully offline narrative (OFFLINE-01).
  if (is.null(narrative)) {
    diag <- tryCatch(
      es_diagnostics(task),
      error = function(e) NULL
    )
    if (!is.null(diag)) {
      narrative <- tryCatch(
        assemble_report_narrative(diagnostics = diag, provider = provider),
        error = function(e) NULL
      )
    }
  }

  # Compute KB references once (before loop) for use in the template
  references <- tryCatch({
    diag_for_refs <- if (!is.null(narrative)) {
      tryCatch(es_diagnostics(task), error = function(e) NULL)
    } else NULL
    if (!is.null(diag_for_refs)) .extract_kb_references(diag_for_refs) else list()
  }, error = function(e) list())

  # ---- 8. Console mode message ONCE after assembly (OFFLINE-02) ----
  if (!is.null(narrative) && is.list(narrative)) {
    mode_used <- narrative$report_mode %||% "offline"
    message(
      "Report mode: ",
      if (identical(mode_used, "ai")) "AI-grounded narrative" else "Offline rule-based narrative"
    )
  }

  # ---- 9. Build the base render params (same for every format) ----
  diag_for_render <- tryCatch(es_diagnostics(task), error = function(e) NULL)

  base_params <- list(
    task             = task,
    title            = title,
    author           = author %||% "",
    sections         = sections,
    cross_sectional  = cross_sectional,
    confidence_level = confidence_level,
    interactive      = interactive,
    advice           = advice,
    narrative        = narrative,
    diag             = diag_for_render,
    references       = references
  )

  # ---- 10. Format loop (FORMAT-01, FORMAT-02) ----
  output_paths <- character(0L)

  for (fmt in formats) {
    ext      <- ext_map[[fmt]]
    out_name <- paste0(basename_no_ext, ext)

    # Sanitise a per-format copy of narrative prose (FORMAT-04 / T-18-04)
    render_narrative <- narrative
    if (!is.null(render_narrative) && is.list(render_narrative)) {
      prose_keys <- c("exec_summary", "data_methods", "results", "robustness")
      for (pk in prose_keys) {
        if (!is.null(render_narrative[[pk]]) && is.character(render_narrative[[pk]])) {
          render_narrative[[pk]] <- .sanitise_prose(render_narrative[[pk]], fmt)
        }
      }
    }

    # Build per-format render params
    render_params <- base_params
    render_params$narrative <- render_narrative

    # Build the rmarkdown output format object (or NULL when toolchain unavailable)
    out_format <- .build_output_format(fmt)

    if (is.null(out_format)) {
      # Toolchain unavailable -- skip with one message (FORMAT-02)
      if (identical(fmt, "html")) {
        # HTML is the only format where absence is a fatal error
        stop("generate_report(): rmarkdown html_document() unavailable. ",
             "Install 'rmarkdown': install.packages('rmarkdown')")
      }
      message("generate_report(): skipping '", fmt, "' -- toolchain not available.")
      next
    }

    # Per-format fig.path to avoid figure-directory collisions across sequential
    # render calls on the same template (T-18-05 / Spike 1)
    fig_dir <- file.path(
      output_dir,
      paste0(basename_no_ext, "_", fmt, "_files", "/figure-")
    )

    rmarkdown::render(
      input          = template_path,
      output_format  = out_format,
      output_file    = out_name,
      output_dir     = output_dir,
      params         = render_params,
      output_options = list("fig.path" = fig_dir),
      envir          = new.env(parent = globalenv()),
      quiet          = TRUE,
      ...
    )

    out_path <- file.path(output_dir, out_name)
    output_paths[[fmt]] <- out_path
    message("Report generated: ", out_path)
  }

  if (length(output_paths) == 0L) {
    warning("generate_report(): no formats were successfully rendered.",
            call. = FALSE)
  }

  invisible(output_paths)
}


# ---------------------------------------------------------------------------
# .pdf_toolchain_available() -- check if a PDF/LaTeX toolchain is available
#
# Prefers tinytex::is_tinytex() (the CRAN-recommended provider); falls back
# to checking for system pdflatex on PATH.
#
# @return Logical scalar.
# @noRd
# ---------------------------------------------------------------------------

.pdf_toolchain_available <- function() {
  if (requireNamespace("tinytex", quietly = TRUE)) {
    return(isTRUE(tinytex::is_tinytex()))
  }
  nzchar(Sys.which("pdflatex"))
}


# ---------------------------------------------------------------------------
# .word_toolchain_available() -- check if pandoc is available for Word/MD
#
# rmarkdown::pandoc_available() is the preferred check; falls back to
# Sys.which("pandoc").
#
# @return Logical scalar.
# @noRd
# ---------------------------------------------------------------------------

.word_toolchain_available <- function() {
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    return(isTRUE(rmarkdown::pandoc_available()))
  }
  nzchar(Sys.which("pandoc"))
}


# ---------------------------------------------------------------------------
# .build_output_format() -- build the rmarkdown output format object
#
# Returns the appropriate rmarkdown::*_document() object for the requested
# format, or NULL when the required toolchain is unavailable.
#
# | fmt   | toolchain guard                  | constructor                         |
# |-------|----------------------------------|-------------------------------------|
# | html  | requireNamespace("rmarkdown")    | html_document(toc, toc_float, ...)  |
# | pdf   | .pdf_toolchain_available()       | pdf_document(toc)                   |
# | word  | .word_toolchain_available()      | word_document(toc)                  |
# | md    | .word_toolchain_available()      | md_document(variant="gfm")          |
#
# @param fmt Character scalar: one of "html", "pdf", "word", "md".
# @return An rmarkdown output format object, or NULL.
# @noRd
# ---------------------------------------------------------------------------

.build_output_format <- function(fmt) {
  switch(
    fmt,
    html = {
      if (!requireNamespace("rmarkdown", quietly = TRUE)) return(NULL)
      rmarkdown::html_document(
        toc       = TRUE,
        toc_float = TRUE,
        theme     = "flatly",
        code_folding = "hide"
      )
    },
    pdf = {
      if (!.pdf_toolchain_available()) return(NULL)
      rmarkdown::pdf_document(toc = TRUE)
    },
    word = {
      if (!.word_toolchain_available()) return(NULL)
      rmarkdown::word_document(toc = TRUE)
    },
    md = {
      if (!.word_toolchain_available()) return(NULL)
      rmarkdown::md_document(variant = "gfm")
    },
    NULL  # unknown format -> NULL
  )
}
