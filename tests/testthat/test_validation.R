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
