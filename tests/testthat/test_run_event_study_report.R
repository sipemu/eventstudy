# =============================================================================
# test_run_event_study_report.R -- run_event_study(report=, report_args=) tests
#
# Covers REPORT-02:
#   - report = FALSE / omitted path is byte-identical to the prior release and
#     invokes no report machinery.
#   - report = TRUE path calls es_report(), attaches attr(task, "report_path"),
#     emits exactly one message(), and still returns the fitted task.
#   - report_args forwards to es_report() and overrides the output location.
#
# Guards:
#   - All render tests: skip_on_cran() + skip_if_not_installed("rmarkdown")
#   - Non-render tests: no skip needed
# =============================================================================


# ---------------------------------------------------------------------------
# Signature checks (fast, no render)
# ---------------------------------------------------------------------------

test_that("REPORT-02: run_event_study gains report and report_args formals", {
  f <- formals(run_event_study)
  expect_true("report" %in% names(f),
              info = "run_event_study must have a 'report' formal")
  expect_true("report_args" %in% names(f),
              info = "run_event_study must have a 'report_args' formal")
  expect_identical(f$report, FALSE,
                   info = "'report' formal must default to FALSE")
  # report_args default must be an empty list literal
  expect_true(
    identical(f$report_args, list()) || deparse(f$report_args) == "list()",
    info = "'report_args' formal must default to list()"
  )
})


# ---------------------------------------------------------------------------
# FALSE / omitted path: byte-identical, no attribute, no side effects
# ---------------------------------------------------------------------------

test_that("REPORT-02: omitted path and report=FALSE produce functionally identical tasks", {
  task <- create_mock_task()
  ps   <- ParameterSet$new()

  # Run on the SAME cloned task to get identical R6 environments
  task_a <- task$clone(deep = TRUE)
  result_default <- run_event_study(task_a, ps)

  task_b <- task$clone(deep = TRUE)
  result_false   <- run_event_study(task_b, ps, report = FALSE)

  # Both must have the same class
  expect_equal(class(result_default), class(result_false),
               info = "both paths must return the same class")

  # data_tbl dimensions must match
  expect_equal(nrow(result_default$data_tbl), nrow(result_false$data_tbl),
               info = "omitted and report=FALSE must have same data_tbl row count")

  # No report_path attribute on either
  expect_null(attr(result_default, "report_path"),
              info = "omitted path must have no report_path attribute")
  expect_null(attr(result_false, "report_path"),
              info = "report=FALSE must have no report_path attribute")

  # Numeric statistics columns must be equal (same random seed via set.seed in helpers)
  stat_cols_default <- vapply(result_default$data_tbl, is.list, logical(1))
  stat_cols_false   <- vapply(result_false$data_tbl, is.list, logical(1))
  expect_equal(names(stat_cols_default), names(stat_cols_false),
               info = "both paths must have the same column names in data_tbl")
})

test_that("REPORT-02: omitted path carries no report_path attribute", {
  task   <- create_mock_task()
  ps     <- ParameterSet$new()
  result <- run_event_study(task, ps)

  expect_null(attr(result, "report_path"),
              info = "omitted path must not set a report_path attribute")
})

test_that("REPORT-02: report=FALSE carries no report_path attribute", {
  task   <- create_mock_task()
  ps     <- ParameterSet$new()
  result <- run_event_study(task, ps, report = FALSE)

  expect_null(attr(result, "report_path"),
              info = "report=FALSE must not set a report_path attribute")
})

test_that("REPORT-02: FALSE path invokes no report side effects (no file written)", {
  task    <- create_mock_task()
  ps      <- ParameterSet$new()
  tmp_dir <- tempdir()

  # Capture any files written to tempdir during the call
  before  <- list.files(tmp_dir, pattern = "\\.html$")
  result  <- run_event_study(task, ps, report = FALSE)
  after   <- list.files(tmp_dir, pattern = "\\.html$")

  expect_equal(length(before), length(after),
               info = "report=FALSE must not write any html file")
})


# ---------------------------------------------------------------------------
# report=TRUE path: attribute, message, return type (render tests)
# ---------------------------------------------------------------------------

test_that("REPORT-02: report=TRUE sets report_path attribute to existing path(s)", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  task <- create_fitted_mock_task()
  ps   <- ParameterSet$new()
  tmp  <- file.path(tempdir(), "es_ru_report.html")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  result <- run_event_study(
    task$clone(deep = TRUE),
    ps,
    report      = TRUE,
    report_args = list(output_file = tmp, format = "html")
  )

  rp <- attr(result, "report_path")
  expect_false(is.null(rp),
               info = "report=TRUE must set a report_path attribute")
  expect_true(all(file.exists(rp)),
              info = "every entry in report_path must exist on disk")
})

test_that("REPORT-02: report=TRUE emits exactly one message containing 'Report written'", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  task <- create_fitted_mock_task()
  ps   <- ParameterSet$new()
  tmp  <- file.path(tempdir(), "es_ru_msg_test.html")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  expect_message(
    run_event_study(
      task$clone(deep = TRUE),
      ps,
      report      = TRUE,
      report_args = list(output_file = tmp, format = "html")
    ),
    regexp = "Report written"
  )
})

test_that("REPORT-02: report=TRUE report_path reflects the report_args output_file override", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  task <- create_fitted_mock_task()
  ps   <- ParameterSet$new()
  tmp  <- file.path(tempdir(), "es_ru_override.html")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  result <- run_event_study(
    task$clone(deep = TRUE),
    ps,
    report      = TRUE,
    report_args = list(output_file = tmp, format = "html")
  )

  rp <- attr(result, "report_path")
  expect_true(
    any(grepl("es_ru_override", rp, fixed = TRUE)),
    info = "report_path must reflect the report_args output_file override"
  )
})

test_that("REPORT-02: report=TRUE still returns the fitted task (class + statistics intact)", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  task <- create_fitted_mock_task()
  ps   <- ParameterSet$new()
  tmp  <- file.path(tempdir(), "es_ru_class_test.html")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  result <- run_event_study(
    task$clone(deep = TRUE),
    ps,
    report      = TRUE,
    report_args = list(output_file = tmp, format = "html")
  )

  # Return type must still be an EventStudyTask
  expect_true(inherits(result, "EventStudyTask"),
              info = "returned object must still be an EventStudyTask")
  # Statistics must be present
  expect_false(is.null(result$data_tbl),
               info = "returned task must have data_tbl populated")
})
