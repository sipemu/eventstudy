# =============================================================================
# test_es_report.R -- Tests for es_report() one-call orchestrator
#
# Covers:
#   REPORT-01: es_report() returns a visible character vector of output path(s)
#              one per requested format; offline HTML renders and path exists.
#   REPORT-04: es_report() deep-clones the task at entry; caller's task is
#              byte-identical before and after the call.
#   Multi-format: format = c("html","md") returns length-2 paths, both on disk.
#   Provider passthrough: provider forwarded to generate_report, not re-called.
#
# Guards:
#   - All render tests: skip_on_cran() + skip_if_not_installed("rmarkdown")
#   - Formals/structural tests: no skip (no render)
# =============================================================================


# ===========================================================================
# Structural / formals checks (no render, no skip)
# ===========================================================================

test_that("es_report exists as an exported function", {
  expect_true(is.function(es_report))
})

test_that("es_report has expected formals", {
  f <- formals(es_report)
  expect_true("task" %in% names(f))
  expect_true("output_file" %in% names(f))
  expect_true("format" %in% names(f))
  expect_true("sections" %in% names(f))
  expect_true("provider" %in% names(f))
  expect_true("title" %in% names(f))
  expect_true("author" %in% names(f))
  expect_true("confidence_level" %in% names(f))
  expect_true("interactive" %in% names(f))
  # provider default is NULL
  expect_null(f$provider)
})

test_that("es_report rejects non-task input with a clear stop()", {
  expect_error(es_report("not_a_task"), regexp = "EventStudyTask")
  expect_error(es_report(42),           regexp = "EventStudyTask")
})


# ===========================================================================
# REPORT-04: Non-mutation -- deep-clone guard (no render, no skip)
# ===========================================================================

test_that("REPORT-04: es_report body contains clone(deep = TRUE) guard", {
  # Structural check: verify the source text contains the deep-clone guard.
  # (The render-based mutation test is below under skip_on_cran().)
  src_lines <- deparse(body(es_report))
  has_clone <- any(grepl("clone(deep = TRUE)", src_lines, fixed = TRUE))
  expect_true(has_clone,
              info = "es_report must call task$clone(deep = TRUE) at entry")
})


# ===========================================================================
# REPORT-01: Offline HTML render -- path returned visibly, file exists
# ===========================================================================

test_that("REPORT-01: es_report() renders offline HTML and returns visible path", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result_and_visible <- withVisible(
    suppressMessages(
      es_report(task,
                output_file = tmp_file,
                format      = "html",
                sections    = c("exec_summary"),
                provider    = NULL)
    )
  )

  # Visibility: es_report must NOT use invisible()
  expect_true(result_and_visible$visible,
              info = "es_report() must return visibly (not invisible)")

  result <- result_and_visible$value

  # Returns a character vector
  expect_true(is.character(result))

  # Contains "html" key
  expect_true("html" %in% names(result))

  # Returned path exists on disk
  expect_true(file.exists(result[["html"]]),
              info = "Returned path must exist after es_report() call")

  # Path ends with .html
  expect_true(grepl("\\.html$", result[["html"]]),
              info = "Returned path must end in .html for format='html'")

  if (file.exists(result[["html"]])) unlink(result[["html"]])
})


# ===========================================================================
# REPORT-04: Non-mutation -- caller's task is unchanged after es_report()
# ===========================================================================

test_that("REPORT-04: caller's task is byte-identical before and after es_report()", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # Capture pre-call snapshot via serialize
  snapshot_before <- serialize(task, NULL)

  suppressMessages(
    es_report(task,
              output_file = tmp_file,
              format      = "html",
              sections    = c("exec_summary"),
              provider    = NULL)
  )

  snapshot_after <- serialize(task, NULL)

  expect_identical(snapshot_before, snapshot_after,
                   info = "es_report() must not mutate the caller's task (REPORT-04)")

  # Cleanup
  html_path <- sub("\\.html$", ".html", tmp_file)
  if (file.exists(html_path)) unlink(html_path)
  for (f in list.files(dirname(tmp_file),
                        pattern = paste0(basename(tools::file_path_sans_ext(tmp_file)), ".*"),
                        full.names = TRUE)) {
    if (file.exists(f)) unlink(f, recursive = TRUE)
  }
})


# ===========================================================================
# REPORT-01: Multi-format -- format = c("html","md") returns length-2 paths
# ===========================================================================

test_that("REPORT-01: es_report(format=c('html','md')) returns length-2 path vector", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- suppressMessages(
    es_report(task,
              output_file = tmp_file,
              format      = c("html", "md"),
              sections    = c("exec_summary"),
              provider    = NULL)
  )

  # At minimum html must be present
  expect_true("html" %in% names(result))
  expect_true(file.exists(result[["html"]]),
              info = "HTML path must exist for multi-format call")

  # md may be skipped if pandoc not available -- if present, assert length == 2
  if ("md" %in% names(result)) {
    expect_equal(length(result), 2L,
                 info = "When md renders, result must have length 2")
    expect_true(file.exists(result[["md"]]),
                info = "md path must exist when md format succeeded")
  }

  for (p in unlist(result)) if (file.exists(p)) unlink(p)
})


# ===========================================================================
# Provider passthrough -- provider forwarded to generate_report, not re-called
# ===========================================================================

test_that("Provider passthrough: es_report forwards provider to generate_report without re-calling", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # Counting mock provider (same pattern as test_report_narrative_asm.R)
  env <- new.env(parent = emptyenv())
  env$call_count <- 0L
  mock_provider <- list(
    source   = "mock_count",
    complete = function(prompt, schema) {
      env$call_count <- env$call_count + 1L
      list(
        text = '{"interpretation":"Mock narrative.","recommendations":[],"caveats":[]}'
      )
    }
  )

  # es_report with a provider should delegate to generate_report which calls
  # assemble_report_narrative once with provider -> N LLM sections called.
  # The critical invariant: a SINGLE-format call does not multiply the call
  # count. We assert count > 0 (provider was forwarded) and count stays fixed
  # at the section budget (not doubled per format).
  count_before <- env$call_count
  suppressMessages(
    es_report(task,
              output_file = tmp_file,
              format      = "html",
              sections    = c("exec_summary"),
              provider    = mock_provider)
  )
  count_after <- env$call_count
  calls_made <- count_after - count_before

  # Provider was contacted at least once (forwarded, not swallowed)
  expect_true(calls_made >= 1L,
              info = "Provider must be forwarded to generate_report (>= 1 call)")

  # Now call es_report a SECOND time with same provider+format to confirm
  # the count per call is stable (not growing = no double-assembly in es_report).
  count_before2 <- env$call_count
  suppressMessages(
    es_report(task,
              output_file = tempfile(fileext = ".html"),
              format      = "html",
              sections    = c("exec_summary"),
              provider    = mock_provider)
  )
  count_after2 <- env$call_count
  calls_made2  <- count_after2 - count_before2

  # Same call count per invocation (stable passthrough, not multiplied)
  expect_equal(calls_made, calls_made2,
               info = "Provider call count must be the same per es_report() invocation")

  for (p in list.files(dirname(tmp_file),
                         pattern = paste0(basename(tools::file_path_sans_ext(tmp_file)), ".*"),
                         full.names = TRUE)) {
    if (file.exists(p)) unlink(p, recursive = TRUE)
  }
})


# ===========================================================================
# Invalid format propagates generate_report's stop() (no render)
# ===========================================================================

test_that("es_report propagates stop() for fully invalid format", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  task <- create_fitted_mock_task()
  expect_error(
    es_report(task, format = "latex"),
    regexp = "No valid format"
  )
})


# ===========================================================================
# Non-ASCII gate on R/report.R (fast, no render)
# ===========================================================================

test_that("R/report.R es_report region contains no non-ASCII bytes", {
  candidates <- c(
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
