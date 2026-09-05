test_that("validate_task succeeds on valid data", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  expect_message(validate_task(task), "no issues found")
})


test_that("validate_task rejects non-task objects", {
  expect_error(validate_task("not a task"), "must be an EventStudyTask")
})


test_that("validate_task works on unprepared task", {
  task = create_mock_task()
  # Should still run without erroring even before prepare_event_study
  expect_no_error(validate_task(task))
})


# --- Individual validation check tests (issue #3, gap #10) ---

test_that("validate_task warns on insufficient estimation window observations", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)

  # Estimation window has ~120 obs; setting threshold high should trigger warning
  # Multiple events may each trigger a warning, so use expect_warning with regexp
  expect_warning(
    expect_warning(
      validate_task(task, min_estimation_obs = 200),
      "Estimation window has only"
    ),
    "Estimation window has only"
  )
})


test_that("validate_task warns on window overlap", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)

  # Manually create overlap
  task$data_tbl$data[[1]]$estimation_window[
    task$data_tbl$data[[1]]$event_window == 1
  ][1] = 1

  expect_warning(validate_task(task), "overlap")
})


test_that("validate_task warns on thin trading", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  # Inject many zero returns into estimation window
  est_mask = task$data_tbl$data[[1]]$estimation_window == 1
  n_est = sum(est_mask)
  task$data_tbl$data[[1]]$firm_returns[est_mask][1:ceiling(n_est * 0.2)] = 0

  expect_warning(validate_task(task), "zero returns")
})


test_that("validate_task warns on time series gaps", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)

  # Inject a large date gap
  dates = as.Date(task$data_tbl$data[[1]]$date, format = "%d.%m.%Y")
  # Shift some dates to create a >5 day gap
  dates[50:100] = dates[50:100] + 30
  task$data_tbl$data[[1]]$date = format(dates, "%d.%m.%Y")

  expect_warning(validate_task(task), "gap")
})
