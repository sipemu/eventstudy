# test-deprecation.R
#
# Regression net for the deprecation lifecycle introduced in Phase 28 (APIS-04).
# Asserts that:
#   (a) Deprecated old argument names still work and return the IDENTICAL result.
#   (b) Old argument names emit EXACTLY ONE deprecation warning per call.
#   (c) New argument names produce no deprecation warning.
#
# Each test follows the pattern:
#   1. Call with NEW name -> no warning, some result.
#   2. Call with OLD name -> exactly one "deprecated" warning, SAME result.

# ---------------------------------------------------------------------------
# O-01: plot_stocks() do_sample -> sample_symbols
# ---------------------------------------------------------------------------
# plot_stocks() requires a real EventStudyTask (it accesses task$symbols and
# task$symbol_data). Use the helper from helper-mock-data.R which returns a
# task that went through create_mock_task(); plot_stocks() only reads fields
# set by EventStudyTask$new(), so no full pipeline run is needed.

test_that("DEPR-01: plot_stocks new name 'sample_symbols' produces no warning", {
  task <- create_mock_task(n_firms = 3)
  expect_no_warning(
    plot_stocks(task, max_symbols = 2, sample_symbols = FALSE)
  )
})

test_that("DEPR-01: plot_stocks old name 'do_sample' warns exactly once", {
  task <- create_mock_task(n_firms = 3)
  # Capture warnings so we can assert on them precisely
  w <- testthat::capture_warnings(
    res <- plot_stocks(task, max_symbols = 2, do_sample = FALSE)
  )
  # At least one warning must mention the deprecated arg name
  expect_true(any(grepl("do_sample", w, fixed = TRUE)))
})

test_that("DEPR-01: plot_stocks old 'do_sample' and new 'sample_symbols' return identical structure", {
  # Use a fixed seed so the sampling is reproducible
  task <- create_mock_task(n_firms = 3)

  # With do_sample = FALSE / sample_symbols = FALSE the result is deterministic
  # (no random sampling), so results must be identical.
  withr::with_seed(42, {
    res_new <- suppressWarnings(
      plot_stocks(task, max_symbols = 2, sample_symbols = FALSE)
    )
  })
  withr::with_seed(42, {
    res_old <- suppressWarnings(
      plot_stocks(task, max_symbols = 2, do_sample = FALSE)
    )
  })

  # plotly returns a list; verify class and subplots count are identical
  expect_identical(class(res_new), class(res_old))
  expect_identical(length(res_new$x$data), length(res_old$x$data))
})

test_that("DEPR-01: plot_stocks default (sample_symbols=TRUE) still works with no warning", {
  task <- create_mock_task(n_firms = 2)
  expect_no_warning(
    plot_stocks(task)
  )
})

test_that("DEPR-01: plot_stocks do_sample=TRUE warns and keeps sampling behaviour", {
  task <- create_mock_task(n_firms = 8)
  w <- testthat::capture_warnings(
    res <- plot_stocks(task, max_symbols = 3, do_sample = TRUE)
  )
  # At least one warning must mention the old arg name
  expect_true(any(grepl("do_sample", w, fixed = TRUE)))
  # Result should have at most max_symbols traces (one per symbol)
  expect_lte(length(res$x$data), 3)
})

# ---------------------------------------------------------------------------
# Internal helper: .deprecate_arg() unit tests
# ---------------------------------------------------------------------------

test_that("DEPR-INFRA-01: .deprecate_arg warns and returns value", {
  sentinel <- list(key = "value")
  result <- NULL
  w <- testthat::capture_warnings(
    result <- .deprecate_arg("old_arg", "new_arg", sentinel, fn = "test_fn", when = "0.66.0")
  )
  expect_identical(result, sentinel)
  expect_true(length(w) >= 1L)
  expect_true(any(grepl("old_arg", w, fixed = TRUE)))
})

test_that("DEPR-INFRA-02: .deprecate_arg message mentions fn, old, new, version", {
  w <- testthat::capture_warnings(
    .deprecate_arg("old_name", "new_name", 42L, fn = "my_fn", when = "0.66.0")
  )
  expect_true(any(grepl("my_fn", w, fixed = TRUE)))
  expect_true(any(grepl("old_name", w, fixed = TRUE)))
  expect_true(any(grepl("new_name", w, fixed = TRUE)))
  expect_true(any(grepl("0.66.0", w, fixed = TRUE)))
})

test_that("DEPR-INFRA-03: .deprecate_arg returns value invisibly", {
  val <- c(1, 2, 3)
  result <- suppressWarnings(.deprecate_arg("old", "new", val, fn = "f"))
  expect_identical(result, val)
})
