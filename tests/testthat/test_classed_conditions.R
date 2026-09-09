# Regression net for API-03 (classed conditions) / API-04 (argument-naming
# messages). Each migrated stop() -> rlang::abort() site is asserted BOTH by
# its condition class (catchable programmatically) AND by a message substring
# (parity with the pre-migration wording), so the migration is provably
# additive: the class was added without silently dropping the user-facing text.

test_that("bad_argument: export_results rejects non-task with class + substring", {
  expect_error(
    export_results("not a task", tempfile(fileext = ".csv")),
    class = "eventstudy_error_bad_argument"
  )
  expect_error(
    export_results("not a task", tempfile(fileext = ".csv")),
    "must be an EventStudyTask"
  )
  # Parent class is catchable too.
  expect_error(
    export_results("not a task", tempfile(fileext = ".csv")),
    class = "eventstudy_error"
  )
})

test_that("bad_argument: unknown file extension raises class + 'Cannot infer format'", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)
  tmp <- tempfile(fileext = ".unknown")
  expect_error(export_results(task, tmp), class = "eventstudy_error_bad_argument")
  expect_error(export_results(task, tmp), "Cannot infer format")
})

test_that("bad_argument: set_formula rejects non-formula with class + substring", {
  mm <- MarketModel$new()
  expect_error(mm$set_formula("not a formula"), class = "eventstudy_error_bad_argument")
  expect_error(mm$set_formula("not a formula"), "Input must be a formula")
})

test_that("bad_argument: cross_sectional_regression rejects non-task", {
  expect_error(
    cross_sectional_regression("not a task", car ~ x, data.frame()),
    class = "eventstudy_error_bad_argument"
  )
  expect_error(
    cross_sectional_regression("not a task", car ~ x, data.frame()),
    "must be an EventStudyTask"
  )
})

test_that("missing_column: LinearFactorModel$fit raises class + 'requires columns'", {
  data <- create_mock_model_data()
  lfm <- LinearFactorModel$new()
  lfm$formula <- stats::as.formula("excess_return ~ market_excess + smb")
  lfm$required_columns <- c("excess_return", "market_excess", "smb")
  expect_error(lfm$fit(data), class = "eventstudy_error_missing_column")
  expect_error(lfm$fit(data), "requires columns")
})

test_that("missing_column: VolumeModel$fit raises class + 'firm_volume'", {
  data <- create_mock_model_data()
  vm <- VolumeModel$new()
  expect_error(vm$fit(data), class = "eventstudy_error_missing_column")
  expect_error(vm$fit(data), "firm_volume")
})

test_that("missing_column: cross_sectional_regression rejects data without event_id", {
  task <- create_fitted_mock_task()
  bad_data <- data.frame(x = 1:3)
  expect_error(
    cross_sectional_regression(task, car ~ x, bad_data),
    class = "eventstudy_error_missing_column"
  )
  expect_error(
    cross_sectional_regression(task, car ~ x, bad_data),
    "event_id"
  )
})

test_that("not_fitted: export before pipeline raises class + substring", {
  task <- create_mock_task()
  tmp <- tempfile(fileext = ".csv")
  expect_error(export_results(task, tmp), class = "eventstudy_error_not_fitted")
  expect_error(export_results(task, tmp), "No results available")
})

test_that("not_fitted: get_ar before fit raises class + substring", {
  task <- create_mock_task()
  expect_error(task$get_ar(), class = "eventstudy_error_not_fitted")
  expect_error(task$get_ar(), "not been calculated")
})

test_that("not_fitted: get_model_stats before fit raises class + substring", {
  task <- create_mock_task()
  expect_error(task$get_model_stats(), class = "eventstudy_error_not_fitted")
  expect_error(task$get_model_stats(), "not been fitted")
})

test_that("unknown_statistic: get_aar with unknown stat raises class + substring", {
  task <- create_fitted_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)
  expect_error(
    task$get_aar(stat_name = "NoSuchStat"),
    class = "eventstudy_error_unknown_statistic"
  )
  expect_error(
    task$get_aar(stat_name = "NoSuchStat"),
    "not found"
  )
})

test_that("unknown_event_id: get_ar with bad event_id raises class + substring", {
  task <- create_fitted_mock_task()
  expect_error(task$get_ar(event_id = 9999L), class = "eventstudy_error_unknown_event_id")
  expect_error(task$get_ar(event_id = 9999L), "not found")
})

test_that("unknown_column: car_by_group with bad group_var raises class + substring", {
  task <- create_fitted_mock_task()
  expect_error(
    car_by_group(task, group_var = "no_such_col"),
    class = "eventstudy_error_unknown_column"
  )
  expect_error(
    car_by_group(task, group_var = "no_such_col"),
    "not found"
  )
})
