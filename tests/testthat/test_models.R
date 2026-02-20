test_that("MarketModel fits correctly", {
  data = create_mock_model_data()
  mm = MarketModel$new()

  mm$fit(data)
  expect_true(mm$is_fitted)
  expect_equal(mm$model_name, "MarketModel")

  stats = mm$statistics
  expect_false(is.null(stats$alpha))
  expect_false(is.null(stats$beta))
  expect_false(is.null(stats$sigma))
  expect_false(is.null(stats$r2))
  expect_false(is.null(stats$degree_of_freedom))
  expect_false(is.null(stats$first_order_auto_correlation))
  expect_false(is.null(stats$residuals))
  expect_false(is.null(stats$forecast_error_corrected_sigma))
  expect_false(is.null(stats$forecast_error_corrected_sigma_car))

  # Beta should be close to 1.2 (the true value)
  expect_lt(abs(stats$beta - 1.2), 0.3)
  # R2 should be positive
  expect_gt(stats$r2, 0)
})


test_that("MarketModel calculates abnormal returns when fitted", {
  data = create_mock_model_data()
  mm = MarketModel$new()
  mm$fit(data)

  result = mm$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
  expect_equal(nrow(result), nrow(data))
  # Abnormal returns should be close to 0 on average in estimation window
  est_ar = result$abnormal_returns[result$estimation_window == 1]
  expect_lt(abs(mean(est_ar, na.rm = TRUE)), 0.01)
})


test_that("MarketModel returns NA when not fitted", {
  mm = MarketModel$new()
  data = create_mock_model_data()

  expect_warning(
    result <- mm$abnormal_returns(data),
    "not fitted"
  )
  expect_true(all(is.na(result$abnormal_returns)))
})


test_that("MarketModel set_formula works", {
  mm = MarketModel$new()
  new_formula = as.formula("firm_returns ~ index_returns + 0")
  mm$set_formula(new_formula)
  expect_equal(mm$formula, new_formula)
})


test_that("MarketModel set_formula rejects non-formula", {
  mm = MarketModel$new()
  expect_error(mm$set_formula("not a formula"), "Input must be a formula")
})


test_that("MarketAdjustedModel fits and calculates AR", {
  data = create_mock_model_data()
  mam = MarketAdjustedModel$new()

  mam$fit(data)
  expect_true(mam$is_fitted)
  expect_equal(mam$model_name, "MarketAdjustedModel")

  result = mam$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
  # AR = firm_returns - index_returns
  expected_ar = data$firm_returns - data$index_returns
  expect_equal(result$abnormal_returns, expected_ar)
})


test_that("MarketAdjustedModel statistics use estimation_tbl (bug fix)", {
  data = create_mock_model_data()
  mam = MarketAdjustedModel$new()

  # This should NOT error (was referencing undefined estimation_window_tbl)
  expect_no_error(mam$fit(data))
  expect_false(is.null(mam$statistics$residuals))
})


test_that("ComparisonPeriodMeanAdjustedModel fits and calculates AR", {
  data = create_mock_model_data()
  cpmam = ComparisonPeriodMeanAdjustedModel$new()

  cpmam$fit(data)
  expect_true(cpmam$is_fitted)

  # The fitted model stores the estimation window mean
  est_mean = mean(data$firm_returns[data$estimation_window == 1])
  expect_equal(cpmam$model, est_mean)

  result = cpmam$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
  expected_ar = data$firm_returns - est_mean
  expect_equal(result$abnormal_returns, expected_ar)
})


test_that("ComparisonPeriodMeanAdjustedModel statistics use estimation_tbl (bug fix)", {
  data = create_mock_model_data()
  cpmam = ComparisonPeriodMeanAdjustedModel$new()

  # This should NOT error (was referencing undefined estimation_window_tbl)
  expect_no_error(cpmam$fit(data))
  expect_false(is.null(cpmam$statistics$residuals))
})


test_that("ModelBase statistics are read-only", {
  mm = MarketModel$new()
  expect_error(mm$statistics <- list(), "read only")
  expect_error(mm$model <- NULL, "read only")
  expect_error(mm$is_fitted <- TRUE, "read only")
})


test_that("CustomModel inherits from MarketModel", {
  cm = CustomModel$new()
  expect_true(inherits(cm, "MarketModel"))
  expect_true(inherits(cm, "ModelBase"))
  expect_equal(cm$model_name, "CustomModel")
})
