test_that("model_diagnostics returns tibble with correct columns", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  diag = model_diagnostics(task)
  expect_true(inherits(diag, "tbl_df"))
  expect_true("shapiro_p" %in% names(diag))
  expect_true("dw_stat" %in% names(diag))
  expect_true("ljung_box_p" %in% names(diag))
  expect_true("acf1" %in% names(diag))
  expect_true("sigma" %in% names(diag))
  expect_true("is_fitted" %in% names(diag))

  expect_equal(nrow(diag), 2)  # 2 events
  expect_true(all(diag$is_fitted))
})


test_that("model_diagnostics errors before fitting", {
  task = create_mock_task()
  expect_error(model_diagnostics(task), "not been fitted")
})


test_that("model_diagnostics filters by event_id", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  diag = model_diagnostics(task, event_id = 1)
  expect_equal(nrow(diag), 1)
})


test_that("pretrend_test works", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  result = pretrend_test(task)
  expect_true(inherits(result, "tbl_df"))
  expect_true("p_value" %in% names(result))
  expect_true("mean_pre_ar" %in% names(result))
})
