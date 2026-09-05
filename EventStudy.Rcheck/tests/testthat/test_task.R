test_that("EventStudyTask initializes correctly", {
  task = create_mock_task()
  expect_true(inherits(task, "EventStudyTask"))
  expect_equal(nrow(task$data_tbl), 2)
  expect_equal(length(task$symbols), 2)
})


test_that("EventStudyTask print method works", {
  task = create_mock_task()
  expect_output(print(task), "EventStudyTask")
  expect_output(print(task), "Events:")
  expect_output(print(task), "Groups:")
  expect_output(print(task), "Symbols:")
})


test_that("EventStudyTask symbols are read-only", {
  task = create_mock_task()
  expect_error(task$symbols <- c("X"), "read only")
})


test_that("EventStudyTask rejects missing columns in firm data", {
  firm_data = tibble(wrong_col = "A", date = "01.01.2020", adjusted = 100)
  index_data = create_mock_index_data()
  request = create_mock_request()
  expect_error(
    EventStudyTask$new(firm_data, index_data, request),
    "symbol"
  )
})


test_that("EventStudyTask rejects missing columns in request", {
  firm_data = create_mock_firm_data()
  index_data = create_mock_index_data()
  request = tibble(event_id = 1, firm_symbol = "A")  # Missing many columns
  expect_error(
    EventStudyTask$new(firm_data, index_data, request),
    "missing columns"
  )
})


test_that("EventStudyTask get_ar works after fitting", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  ar = task$get_ar()
  expect_true("abnormal_returns" %in% names(ar))
  expect_true("relative_index" %in% names(ar))
})


test_that("EventStudyTask get_car works after fitting", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  car = task$get_car()
  expect_true("car" %in% names(car))
  # CAR should be cumsum of AR
  expect_equal(car$car, cumsum(car$abnormal_returns))
})


test_that("EventStudyTask get_ar errors before fitting", {
  task = create_mock_task()
  expect_error(task$get_ar(), "not been calculated")
})


test_that("EventStudyTask get_model_stats works after fitting", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  stats = task$get_model_stats()
  expect_true(is.list(stats))
  expect_false(is.null(stats$sigma))
})


test_that("EventStudyTask summary works", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  s = task$summary()
  expect_equal(class(s), "EventStudySummary")
  expect_equal(s$n_events, 2)
  expect_output(print(s), "Event Study Summary")
})
