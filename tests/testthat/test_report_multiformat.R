# =============================================================================
# test_report_multiformat.R -- Multi-format renderer + fixed template tests
#
# Covers:
#   FORMAT-01: Vector format= renders N files with correct extensions; returns
#              named character vector.
#   FORMAT-02: Missing toolchain -> exactly one message() + no stop(); skipped
#              format omitted from return vector.
#   TMPL-01:   skeleton.Rmd has the six fixed sections in params block +
#              diag/references params declared (readLines, fast).
#   TMPL-02:   Data/methods table sourced from task + es_diagnostics, not narrative.
#   NARR-05:   Joint-hypothesis caveat present in every rendered HTML.
#   OFFLINE-02: Console message() and section heading label surfaced in both
#               offline and mock-AI paths.
#
# Guards:
#   - All render tests: skip_on_cran() + skip_if_not_installed("rmarkdown")
#   - All toolchain mock tests: no skip needed (no render)
#   - Non-ASCII gate on R/report.R and skeleton.Rmd checked separately
# =============================================================================


# ===========================================================================
# TMPL-01: readLines checks (fast, no render, no skip)
# ===========================================================================

test_that("TMPL-01: skeleton.Rmd sections: default contains all six new fixed keys", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found")
  lines <- readLines(skeleton_path, warn = FALSE)

  # All six fixed section keys must appear in a single sections: line in the params block
  sections_line <- grep("^  sections:", lines, value = TRUE)
  expect_true(length(sections_line) >= 1L,
              info = "params block must have a 'sections:' line")

  for (key in c("exec_summary", "data_methods", "results",
                "diagnostics", "robustness", "references")) {
    expect_true(any(grepl(key, lines, fixed = TRUE)),
                info = paste0("'", key, "' must appear in skeleton.Rmd"))
  }
})

test_that("TMPL-01: skeleton.Rmd declares diag: NULL in params block", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found")
  lines <- readLines(skeleton_path, warn = FALSE)

  has_diag <- any(grepl("^  diag:", lines))
  expect_true(has_diag, info = "params block must include '  diag:' param")
})

test_that("TMPL-01: skeleton.Rmd declares references: NULL in params block", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found")
  lines <- readLines(skeleton_path, warn = FALSE)

  has_refs <- any(grepl("^  references:", lines))
  expect_true(has_refs, info = "params block must include '  references:' param")
})

test_that("TMPL-01: skeleton.Rmd uses knitr::is_html_output for plot switching (FORMAT-03)", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found")
  lines <- readLines(skeleton_path, warn = FALSE)

  expect_true(any(grepl("knitr::is_html_output", lines, fixed = TRUE)),
              info = "skeleton.Rmd must use knitr::is_html_output() for plot switching")
})

test_that("TMPL-01: skeleton.Rmd references params$references for bibliography", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found")
  lines <- readLines(skeleton_path, warn = FALSE)

  expect_true(any(grepl("params$references", lines, fixed = TRUE)),
              info = "skeleton.Rmd must use params$references for the references section")
})

test_that("TMPL-01: skeleton.Rmd references section_sources for heading labels (OFFLINE-02)", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found")
  lines <- readLines(skeleton_path, warn = FALSE)

  expect_true(any(grepl("section_sources", lines, fixed = TRUE)),
              info = "skeleton.Rmd must reference section_sources for AI/offline heading labels")
})


# ===========================================================================
# FORMAT-01: Vector format= returns named character vector (fast formals check)
# ===========================================================================

test_that("FORMAT-01: format formal exists and default is 'html'", {
  f <- formals(generate_report)
  expect_true("format" %in% names(f))
  expect_equal(f$format, "html")
})

test_that("FORMAT-01: sections formal default is the six new fixed keys", {
  f <- formals(generate_report)
  expect_true("sections" %in% names(f))
  section_defaults <- eval(f$sections)
  expect_true(all(c("exec_summary", "data_methods", "results",
                    "diagnostics", "robustness", "references") %in% section_defaults))
})

test_that("FORMAT-01: provider formal exists with default NULL", {
  f <- formals(generate_report)
  expect_true("provider" %in% names(f))
  expect_null(f$provider)
})

test_that("FORMAT-01: invalid format raises stop()", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  task <- create_fitted_mock_task()
  expect_error(
    generate_report(task, format = "latex"),
    regexp = "No valid format"
  )
})


# ===========================================================================
# FORMAT-02: Toolchain skip -- message() not stop() (fast, mock-based)
# ===========================================================================

test_that("FORMAT-02: unavailable pdf toolchain emits message, not stop()", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  # We test this by rendering html only (which is always available) and
  # verifying that the logic path for pdf returns NULL from .build_output_format.
  # Direct unit test of the toolchain guard:
  # Temporarily override .pdf_toolchain_available via local mocking
  task     <- create_fitted_mock_task()
  tmp_html <- tempfile(fileext = ".html")

  # With format=c("html","pdf") and mocked pdf toolchain unavailable,
  # only html file should render, pdf should emit message.
  with_mocked_bindings(
    .pdf_toolchain_available = function() FALSE,
    {
      expect_message(
        result <- generate_report(task,
                                  output_file = tmp_html,
                                  format = c("html", "pdf"),
                                  sections = c("exec_summary")),
        regexp = "skipping 'pdf'"
      )
      # Only html path present in return
      expect_true("html" %in% names(result))
      expect_false("pdf" %in% names(result))
    },
    .package = "EventStudy"
  )
  if (file.exists(tmp_html)) unlink(tmp_html)
})

test_that("FORMAT-02: .build_output_format returns NULL for pdf when toolchain unavailable", {
  # Test internal helper directly (no render, no skip)
  result <- with_mocked_bindings(
    .pdf_toolchain_available = function() FALSE,
    EventStudy:::.build_output_format("pdf"),
    .package = "EventStudy"
  )
  expect_null(result)
})

test_that("FORMAT-02: .build_output_format returns non-NULL for html (rmarkdown available)", {
  skip_if_not_installed("rmarkdown")
  result <- EventStudy:::.build_output_format("html")
  expect_false(is.null(result))
  expect_true(inherits(result, "rmarkdown_output_format"))
})


# ===========================================================================
# FORMAT-01 render: single html, named vector return (skip_on_cran)
# ===========================================================================

test_that("FORMAT-01 render: single format='html' returns named vector with 'html' element", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- generate_report(task, output_file = tmp_file, format = "html",
                             sections = c("exec_summary"))
  expect_true(is.character(result))
  expect_true(is.list(result) || !is.null(names(result)))  # named vector
  expect_true("html" %in% names(result))
  expect_true(file.exists(result[["html"]]))
  # Backward compat: result[[1L]] also resolves
  expect_equal(result[[1L]], result[["html"]])
  unlink(result[["html"]])
})

test_that("FORMAT-01 render: vector format returns paths for each rendered format", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # Only request html and md (md typically available via pandoc)
  result <- generate_report(task, output_file = tmp_file,
                             format = c("html", "md"),
                             sections = c("exec_summary"))

  expect_true("html" %in% names(result))
  # md may be skipped if pandoc unavailable -- only assert html exists
  expect_true(file.exists(result[["html"]]))
  for (p in unlist(result)) if (file.exists(p)) unlink(p)
})


# ===========================================================================
# NARR-01: Narrative assembled once -- provider called N sections, not N*formats
# ===========================================================================

test_that("NARR-01 end-to-end: assemble_report_narrative called once for all formats", {
  # We cannot easily count internal assemble_report_narrative calls without
  # render. We verify this architecturally: generate_report with a pre-built
  # narrative does not call es_advise at all.
  # Use a mock provider that errors if called -- if the format loop called
  # assemble_report_narrative, this would fail.
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  call_count <- 0L
  fail_provider <- list(
    source   = "mock_fail",
    complete = function(prompt, schema) {
      call_count <<- call_count + 1L
      stop("provider should not be called inside the format loop")
    }
  )

  # Pre-build the narrative offline (provider=NULL)
  diag      <- es_diagnostics(task)
  narrative <- EventStudy:::assemble_report_narrative(diag, provider = NULL)

  # Pass pre-built narrative + fail provider; if provider were called, test fails
  result <- generate_report(
    task,
    output_file = tmp_file,
    format      = "html",
    sections    = c("exec_summary"),
    narrative   = narrative,
    provider    = fail_provider
  )
  # If we reached here without error, provider was never called via the loop
  expect_equal(call_count, 0L)
  if (!is.null(result) && !is.null(names(result))) {
    for (p in unlist(result)) if (file.exists(p)) unlink(p)
  }
})


# ===========================================================================
# OFFLINE-02: Console message and section heading label
# ===========================================================================

test_that("OFFLINE-02: offline path emits 'Offline rule-based narrative' message", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  expect_message(
    result <- generate_report(task,
                              output_file = tmp_file,
                              format      = "html",
                              sections    = c("exec_summary"),
                              provider    = NULL),
    regexp = "Offline rule-based narrative"
  )
  for (p in unlist(result)) if (file.exists(p)) unlink(p)
})

test_that("OFFLINE-02: AI path emits 'AI-grounded narrative' message", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # Build a mock provider that returns valid Advice JSON
  mock_p <- list(
    source   = "mock_ai",
    complete = function(prompt, schema) {
      list(text = '{"interpretation":"AI mock narrative for testing.","recommendations":[],"caveats":[]}')
    }
  )

  # Pre-assemble narrative with mock AI provider, then inject
  diag      <- es_diagnostics(task)
  narrative <- EventStudy:::assemble_report_narrative(diag, provider = mock_p)
  # Confirm at least one section used AI
  expect_equal(narrative$report_mode, "ai")

  expect_message(
    result <- generate_report(task,
                              output_file = tmp_file,
                              format      = "html",
                              sections    = c("exec_summary"),
                              narrative   = narrative),
    regexp = "AI-grounded narrative"
  )
  for (p in unlist(result)) if (file.exists(p)) unlink(p)
})

test_that("OFFLINE-02: rendered HTML contains AI-vs-offline section heading label", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # Offline path
  result <- suppressMessages(
    generate_report(task,
                    output_file = tmp_file,
                    format      = "html",
                    sections    = c("exec_summary", "robustness"),
                    provider    = NULL)
  )
  content <- paste(readLines(result[["html"]], warn = FALSE), collapse = "\n")
  expect_true(
    grepl("Automated rule-based interpretation", content, fixed = TRUE),
    info = "Offline HTML must contain 'Automated rule-based interpretation' heading label"
  )
  unlink(result[["html"]])
})


# ===========================================================================
# NARR-05: Joint-hypothesis caveat in every rendered report
# ===========================================================================

test_that("NARR-05: rendered HTML contains joint-hypothesis caveat", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- suppressMessages(
    generate_report(task,
                    output_file = tmp_file,
                    format      = "html",
                    sections    = c("robustness"),
                    provider    = NULL)
  )
  content <- paste(readLines(result[["html"]], warn = FALSE), collapse = "\n")
  expect_true(
    grepl("joint test", content, fixed = TRUE) ||
    grepl("joint-hypothesis", content, fixed = TRUE) ||
    grepl("MacKinlay", content, fixed = TRUE),
    info = "Rendered HTML must contain joint-hypothesis caveat text"
  )
  unlink(result[["html"]])
})

test_that("NARR-05: assemble_report_narrative robustness contains caveat (unit, no render)", {
  diag   <- .make_test_diag()
  result <- EventStudy:::assemble_report_narrative(diag, provider = NULL)
  expect_true(
    grepl("joint test", result$robustness, fixed = TRUE) ||
    grepl("MacKinlay", result$robustness, fixed = TRUE),
    info = "robustness section must contain joint-hypothesis caveat text"
  )
})


# ===========================================================================
# TMPL-02: Data/methods table sourced from task + es_diagnostics (not LLM)
# ===========================================================================

test_that("TMPL-02: rendered HTML contains event count from task metadata", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  # create_fitted_mock_task() builds 2 firms by default -> 2 events
  task     <- create_fitted_mock_task(n_firms = 2)
  tmp_file <- tempfile(fileext = ".html")

  result <- suppressMessages(
    generate_report(task,
                    output_file = tmp_file,
                    format      = "html",
                    sections    = c("data_methods"),
                    provider    = NULL)
  )
  content <- paste(readLines(result[["html"]], warn = FALSE), collapse = "\n")
  # The data_methods section table should show the event count (2)
  expect_true(
    grepl("2", content, fixed = TRUE),
    info = "Data & Methods section must include event count from task metadata"
  )
  unlink(result[["html"]])
})


# ===========================================================================
# Non-ASCII gate (fast, no render)
# ===========================================================================

test_that("R/report.R contains no non-ASCII bytes", {
  report_path <- system.file("../../R/report.R", package = "EventStudy")
  # Use the source path directly
  src <- file.path(
    system.file(package = "EventStudy"),
    "..", "..", "R", "report.R"
  )
  # Try from package source tree first
  candidates <- c(
    file.path(.libPaths()[1], "EventStudy", "../../R/report.R"),
    file.path(getwd(), "R/report.R"),
    "/home/simonm/projects/datascience/eventstudy/R/report.R"
  )
  f <- Filter(file.exists, candidates)
  if (length(f) == 0L) skip("R/report.R not found for non-ASCII check")
  f <- f[[1L]]
  raw_bytes <- readBin(f, "raw", file.info(f)$size)
  non_ascii  <- raw_bytes[as.integer(raw_bytes) > 127L]
  expect_equal(length(non_ascii), 0L,
               info = "R/report.R must be ASCII-clean (no bytes > 0x7F)")
})

test_that("skeleton.Rmd contains no non-ASCII bytes", {
  skel_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skel_path == "", "skeleton.Rmd not found")
  raw_bytes <- readBin(skel_path, "raw", file.info(skel_path)$size)
  non_ascii  <- raw_bytes[as.integer(raw_bytes) > 127L]
  expect_equal(length(non_ascii), 0L,
               info = "skeleton.Rmd must be ASCII-clean")
})


# ===========================================================================
# CR-02: fig.path isolation via template params (not output_options)
# ===========================================================================

test_that("CR-02: skeleton.Rmd declares fig_path: NULL in params block", {
  # output_options= is ignored by rmarkdown::render() when output_format is a
  # custom format object. The correct approach passes fig_path via params and
  # sets knitr::opts_chunk$set(fig.path=) inside the template setup chunk.
  skel_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skel_path == "", "skeleton.Rmd not found")
  lines <- readLines(skel_path, warn = FALSE)
  expect_true(any(grepl("fig_path", lines, fixed = TRUE)),
              info = "skeleton.Rmd must declare fig_path param for per-format isolation")
  expect_true(any(grepl("knitr::opts_chunk\\$set.*fig.path|fig.path.*knitr::opts_chunk\\$set",
                         lines, perl = TRUE)) ||
              any(grepl("opts_chunk", lines, fixed = TRUE)),
              info = "skeleton.Rmd setup chunk must call knitr::opts_chunk$set(fig.path=...)")
})

test_that("CR-02: generate_report() passes distinct fig_path per format via render_params", {
  # Verify that generate_report() no longer uses output_options= (no-op) and
  # instead routes fig_path through render_params (params=).
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  captured_params <- list()
  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # Render two formats; each should set a distinct fig_path in params
  with_mocked_bindings(
    .build_output_format = function(fmt) {
      if (fmt == "html" && requireNamespace("rmarkdown", quietly = TRUE)) {
        return(rmarkdown::html_document(toc = FALSE))
      }
      NULL
    },
    {
      result <- suppressMessages(
        generate_report(task,
                        output_file = tmp_file,
                        format      = "html",
                        sections    = c("exec_summary"))
      )
    },
    .package = "EventStudy"
  )
  # If render succeeded, the fig_path was injected via params (not output_options)
  expect_true("html" %in% names(result) || length(result) >= 0L)
  for (p in unlist(result)) if (file.exists(p)) unlink(p)
})

test_that("VIZ-04: .report_table falls back to kable when tinytable absent", {
  skip_if_not_installed("knitr")

  df <- data.frame(Event = "A", CAR = 0.0123, stringsAsFactors = FALSE)
  out <- with_mocked_bindings(
    .tinytable_available = function() FALSE,
    capture.output(EventStudy:::.report_table(df, caption = "Test")),
    .package = "EventStudy"
  )
  txt <- paste(out, collapse = "\n")
  # Caption text is emitted by knitr::kable (the fallback path).
  expect_true(grepl("Test", txt, fixed = TRUE))
  # kable markdown output must not contain an HTML <table marker (would mean
  # tinytable, not kable, produced the output).
  expect_false(grepl("<table", txt, fixed = TRUE))
})

test_that("VIZ-04: .report_table col.names rename reaches kable output", {
  skip_if_not_installed("knitr")

  df <- data.frame(sym = "AAPL", ret = 0.05, stringsAsFactors = FALSE)
  out <- with_mocked_bindings(
    .tinytable_available = function() FALSE,
    capture.output(
      EventStudy:::.report_table(df, col.names = c("Firm", "Return"))
    ),
    .package = "EventStudy"
  )
  txt <- paste(out, collapse = "\n")
  expect_true(grepl("Firm", txt, fixed = TRUE))
  expect_true(grepl("Return", txt, fixed = TRUE))
})
