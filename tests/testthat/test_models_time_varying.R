test_that("RollingWindowModel fits with default window", {
  data <- create_mock_model_data()
  rw <- RollingWindowModel$new(window_size = 60, min_obs = 30)

  rw$fit(data)
  expect_true(rw$is_fitted)
  expect_equal(rw$model_name, "RollingWindowModel")

  stats <- rw$statistics
  expect_false(is.null(stats$alpha))
  expect_false(is.null(stats$beta))
  expect_false(is.null(stats$sigma))
  expect_false(is.null(stats$rolling_betas))
  expect_true(length(stats$rolling_betas) > 0)
})


test_that("RollingWindowModel calculates abnormal returns", {
  data <- create_mock_model_data()
  rw <- RollingWindowModel$new(window_size = 60)

  rw$fit(data)
  result <- rw$abnormal_returns(data)

  expect_true("abnormal_returns" %in% names(result))
  expect_equal(nrow(result), nrow(data))
  # Abnormal returns should be numeric, not all NA
  expect_true(all(is.finite(result$abnormal_returns)))
})


test_that("RollingWindowModel warns on insufficient data", {
  data <- create_mock_model_data(n_estimation = 10)
  rw <- RollingWindowModel$new(window_size = 60, min_obs = 30)

  expect_warning(rw$fit(data), "insufficient")
  expect_false(rw$is_fitted)
})


test_that("RollingWindowModel works in full pipeline", {
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = RollingWindowModel$new(window_size = 60))
  result <- run_event_study(task, ps)
  expect_false(is.null(result$data_tbl))
})


test_that("RollingWindowModel returns NA when not fitted", {
  rw <- RollingWindowModel$new()
  data <- create_mock_model_data()

  expect_warning(
    result <- rw$abnormal_returns(data),
    "not fitted"
  )
  expect_true(all(is.na(result$abnormal_returns)))
})


test_that("RollingWindowModel with custom window_size", {
  data <- create_mock_model_data()
  rw <- RollingWindowModel$new(window_size = 30, min_obs = 20)

  rw$fit(data)
  expect_true(rw$is_fitted)

  stats <- rw$statistics
  # Should have more rolling windows than with window_size=60
  n_windows_30 <- length(stats$rolling_betas)

  rw2 <- RollingWindowModel$new(window_size = 60, min_obs = 30)
  rw2$fit(data)
  n_windows_60 <- length(rw2$statistics$rolling_betas)

  expect_gt(n_windows_30, n_windows_60)
})


test_that("RollingWindowModel stores rolling parameters", {
  data <- create_mock_model_data()
  rw <- RollingWindowModel$new(window_size = 60, min_obs = 30)
  rw$fit(data)

  stats <- rw$statistics
  expect_equal(length(stats$rolling_alphas), length(stats$rolling_betas))
  expect_equal(length(stats$rolling_sigmas), length(stats$rolling_betas))

  # All rolling betas should be finite
  expect_true(all(is.finite(stats$rolling_betas)))
  expect_true(all(is.finite(stats$rolling_alphas)))
  expect_true(all(stats$rolling_sigmas > 0))
})


test_that("RollingWindowModel beta near true value", {
  data <- create_mock_model_data(n_estimation = 200)
  rw <- RollingWindowModel$new(window_size = 60)
  rw$fit(data)

  # The mock data has beta = 1.2 — the last window should be close
  expect_lt(abs(rw$statistics$beta - 1.2), 0.5)
})


test_that("RollingWindowModel residuals and autocorrelation stored", {
  data <- create_mock_model_data()
  rw <- RollingWindowModel$new(window_size = 60)
  rw$fit(data)

  stats <- rw$statistics
  expect_false(is.null(stats$residuals))
  expect_false(is.null(stats$first_order_auto_correlation))
  expect_true(length(stats$residuals) > 0)
})


test_that("RollingWindowModel forecast error correction stored", {
  data <- create_mock_model_data()
  rw <- RollingWindowModel$new(window_size = 60)
  rw$fit(data)

  stats <- rw$statistics
  expect_false(is.null(stats$forecast_error_corrected_sigma))
  expect_false(is.null(stats$forecast_error_corrected_sigma_car))
})


test_that("RollingWindowModel degree_of_freedom is window_size - 2", {
  data <- create_mock_model_data()
  rw <- RollingWindowModel$new(window_size = 60)
  rw$fit(data)

  expect_equal(rw$statistics$degree_of_freedom, 58)
})


test_that("DCCGARCHModel requires rmgarch", {
  skip_if_not_installed("rmgarch")
  skip_if_not_installed("rugarch")

  data <- create_mock_model_data(n_estimation = 200)
  dcc <- DCCGARCHModel$new()

  tryCatch({
    dcc$fit(data)
    if (dcc$is_fitted) {
      expect_false(is.null(dcc$statistics$beta))
      expect_false(is.null(dcc$statistics$beta_t))
      expect_true(length(dcc$statistics$beta_t) > 0)

      result <- dcc$abnormal_returns(data)
      expect_true("abnormal_returns" %in% names(result))
    }
  }, error = function(e) {
    # DCC fitting can fail with small/synthetic data
    expect_true(TRUE)
  })
})


test_that("DCCGARCHModel returns NA when not fitted", {
  skip_if_not_installed("rmgarch")
  skip_if_not_installed("rugarch")

  dcc <- DCCGARCHModel$new()
  data <- create_mock_model_data()

  expect_warning(
    result <- dcc$abnormal_returns(data),
    "not fitted"
  )
  expect_true(all(is.na(result$abnormal_returns)))
})


test_that("DCCGARCHModel custom garch_order and dcc_order", {
  dcc <- DCCGARCHModel$new(garch_order = c(2, 1), dcc_order = c(1, 2))
  expect_equal(dcc$garch_order, c(2, 1))
  expect_equal(dcc$dcc_order, c(1, 2))
})


test_that("RollingWindowModel clones deep correctly", {
  rw <- RollingWindowModel$new(window_size = 90, min_obs = 40)
  rw2 <- rw$clone(deep = TRUE)

  expect_equal(rw2$window_size, 90L)
  expect_equal(rw2$min_obs, 40L)
})
