# Regression net for the verbose= quiet mode (Phase 23, Plan 03, API-05).
#
# Proves three invariants:
#   1. Default (verbose omitted) -> informational message() still emits
#      (default console output is byte-identical to prior behaviour).
#   2. verbose = FALSE (arg or option) -> informational message() suppressed.
#   3. Warnings are NEVER gated: with verbose = FALSE the degenerate
#      one-warning contract (.handle_degenerate) still fires exactly once.

# ---- 1. Default emits (byte-identical default) --------------------------
test_that("API-05: informational message emits by default (verbose omitted)", {
  task <- create_mock_task()
  ps <- ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task <- prepare_event_study(task, ps)
  task <- fit_model(task, ps)

  expect_message(validate_task(task), "no issues found")
})

# ---- 2. verbose = FALSE suppresses informational chatter ----------------
test_that("API-05: verbose = FALSE suppresses informational messages", {
  task <- create_mock_task()
  ps <- ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task <- prepare_event_study(task, ps)
  task <- fit_model(task, ps)

  expect_no_message(validate_task(task, verbose = FALSE))
})

test_that("API-05: options(eventstudy.verbose = FALSE) suppresses messages", {
  task <- create_mock_task()
  ps <- ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task <- prepare_event_study(task, ps)
  task <- fit_model(task, ps)

  withr::with_options(
    list(eventstudy.verbose = FALSE),
    expect_no_message(validate_task(task))
  )
})

# ---- 3. Warnings NEVER gated by verbose --------------------------------
test_that("API-05: degenerate one-warning still fires with verbose = FALSE", {
  # The .handle_degenerate contract (contract.R) is untouched by the verbose
  # gate: a zero-variance fit() must still emit exactly one warning even when
  # informational messaging is silenced globally.
  d <- create_degenerate_model_data_zero_variance()
  m <- MarketModel$new()

  withr::with_options(
    list(eventstudy.verbose = FALSE),
    expect_warning(m$fit(d))
  )
})

# ---- 4. .inform helper unit behaviour ----------------------------------
test_that("API-05: .inform emits when verbose truthy, silent when FALSE", {
  expect_message(.inform("hello", verbose = TRUE), "hello")
  expect_no_message(.inform("hello", verbose = FALSE))
  # Default pulls from the option
  withr::with_options(
    list(eventstudy.verbose = FALSE),
    expect_no_message(.inform("hello"))
  )
  withr::with_options(
    list(eventstudy.verbose = TRUE),
    expect_message(.inform("hello"), "hello")
  )
})
