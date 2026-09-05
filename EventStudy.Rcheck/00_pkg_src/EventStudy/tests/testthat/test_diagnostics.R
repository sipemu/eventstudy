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


# --- Diagnostics edge cases (issue #3, gap #9) ---

test_that("pretrend_test errors before fitting", {
  task = create_mock_task()
  expect_error(pretrend_test(task), "not been fitted")
})


test_that("pretrend_test filters by group", {
  task = create_fitted_mock_task()
  result = pretrend_test(task, group = "TestGroup")
  expect_equal(nrow(result), 1)
  expect_equal(result$group, "TestGroup")
})


test_that("model_diagnostics values are reasonable", {
  task = create_fitted_mock_task()
  diag = model_diagnostics(task)

  # Shapiro-Wilk p-values are in [0, 1]
  expect_true(all(diag$shapiro_p >= 0 & diag$shapiro_p <= 1))
  # Durbin-Watson stat should be around 2 for uncorrelated residuals
  expect_true(all(diag$dw_stat > 0 & diag$dw_stat < 4))
  # sigma should be positive
  expect_true(all(diag$sigma > 0))
})
