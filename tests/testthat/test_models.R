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


# --- Linear Factor Model tests ---

test_that("LinearFactorModel fits and predicts", {
  data <- create_mock_model_data()
  lfm <- LinearFactorModel$new()
  lfm$formula <- stats::as.formula("firm_returns ~ index_returns")
  lfm$required_columns <- c("firm_returns", "index_returns")

  lfm$fit(data)
  expect_true(lfm$is_fitted)
  expect_false(is.null(lfm$statistics$alpha))
  expect_false(is.null(lfm$statistics$beta))
  expect_false(is.null(lfm$statistics$sigma))

  result <- lfm$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
})

test_that("LinearFactorModel errors on missing columns", {
  data <- create_mock_model_data()
  lfm <- LinearFactorModel$new()
  lfm$formula <- stats::as.formula("excess_return ~ market_excess + smb")
  lfm$required_columns <- c("excess_return", "market_excess", "smb")

  expect_error(lfm$fit(data), "requires columns")
})


# --- Factor Model mock data helper ---

create_mock_factor_model_data <- function(n_estimation = 120, n_event = 11) {
  set.seed(42)
  n_total <- n_estimation + n_event
  smb <- rnorm(n_total, mean = 0, sd = 0.005)
  hml <- rnorm(n_total, mean = 0, sd = 0.005)
  mom <- rnorm(n_total, mean = 0, sd = 0.005)
  rmw <- rnorm(n_total, mean = 0, sd = 0.004)
  cma <- rnorm(n_total, mean = 0, sd = 0.004)
  rf <- rep(0.0001, n_total)
  market_excess <- rnorm(n_total, mean = 0.0003, sd = 0.015)

  # Firm excess return = alpha + betas * factors + noise
  excess_return <- 0.0005 + 1.1 * market_excess + 0.5 * smb - 0.3 * hml +
    0.2 * mom + 0.1 * rmw - 0.1 * cma + rnorm(n_total, sd = 0.008)

  tibble::tibble(
    firm_returns = excess_return + rf,
    index_returns = market_excess + rf,
    excess_return = excess_return,
    market_excess = market_excess,
    smb = smb,
    hml = hml,
    mom = mom,
    rmw = rmw,
    cma = cma,
    risk_free_rate = rf,
    estimation_window = c(rep(1, n_estimation), rep(0, n_event)),
    event_window = c(rep(0, n_estimation), rep(1, n_event)),
    relative_index = c(seq(-n_estimation, -1), seq(0, n_event - 1)),
    event_date = c(rep(0, n_estimation), 1, rep(0, n_event - 1))
  )
}


test_that("FamaFrench3FactorModel fits and calculates AR", {
  data <- create_mock_factor_model_data()
  ff3 <- FamaFrench3FactorModel$new()

  ff3$fit(data)
  expect_true(ff3$is_fitted)
  expect_equal(ff3$model_name, "FamaFrench3FactorModel")

  stats <- ff3$statistics
  expect_false(is.null(stats$sigma))
  expect_false(is.null(stats$r2))
  expect_gt(stats$r2, 0.3)

  result <- ff3$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
})

test_that("FamaFrench5FactorModel fits and calculates AR", {
  data <- create_mock_factor_model_data()
  ff5 <- FamaFrench5FactorModel$new()

  ff5$fit(data)
  expect_true(ff5$is_fitted)
  expect_equal(ff5$model_name, "FamaFrench5FactorModel")

  result <- ff5$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
  expect_equal(nrow(result), nrow(data))
})

test_that("Carhart4FactorModel fits and calculates AR", {
  data <- create_mock_factor_model_data()
  c4 <- Carhart4FactorModel$new()

  c4$fit(data)
  expect_true(c4$is_fitted)
  expect_equal(c4$model_name, "Carhart4FactorModel")

  result <- c4$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
})

test_that("Factor models error on missing columns", {
  data <- create_mock_model_data()  # no factor columns
  ff3 <- FamaFrench3FactorModel$new()
  expect_error(ff3$fit(data), "requires columns")
})


# --- GARCH Model tests ---

test_that("GARCHModel requires rugarch package", {
  # This test verifies the error message when rugarch is not available
  # In environments without rugarch, this will error with a clear message
  data <- create_mock_model_data()
  gm <- GARCHModel$new()

  tryCatch({
    gm$fit(data)
    # If rugarch is available, model should fit
    expect_true(gm$is_fitted)
    result <- gm$abnormal_returns(data)
    expect_true("abnormal_returns" %in% names(result))
  }, error = function(e) {
    expect_true(grepl("rugarch", conditionMessage(e)))
  })
})


# --- BHAR Model tests ---

test_that("BHARModel fits and calculates compound AR", {
  data <- create_mock_model_data()
  bhar <- BHARModel$new()

  bhar$fit(data)
  expect_true(bhar$is_fitted)
  expect_equal(bhar$model_name, "BHARModel")

  stats <- bhar$statistics
  expect_false(is.null(stats$sigma))

  result <- bhar$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
  # BHAR uses compounding, not simple subtraction
  expect_equal(nrow(result), nrow(data))
})


# --- Volume Model tests ---

test_that("VolumeModel fits and calculates abnormal volume", {
  data <- create_mock_model_data()
  data$firm_volume <- abs(rnorm(nrow(data), mean = 1e6, sd = 2e5))

  vm <- VolumeModel$new(log_transform = TRUE)

  vm$fit(data)
  expect_true(vm$is_fitted)
  expect_equal(vm$model_name, "VolumeModel")

  result <- vm$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
})

test_that("VolumeModel errors without firm_volume column", {
  data <- create_mock_model_data()
  vm <- VolumeModel$new()
  expect_error(vm$fit(data), "firm_volume")
})


# --- Volatility Model tests ---

test_that("VolatilityModel fits and calculates abnormal volatility", {
  data <- create_mock_model_data()
  volm <- VolatilityModel$new()

  volm$fit(data)
  expect_true(volm$is_fitted)
  expect_equal(volm$model_name, "VolatilityModel")

  result <- volm$abnormal_returns(data)
  expect_true("abnormal_returns" %in% names(result))
  expect_equal(nrow(result), nrow(data))
})


# --- HAC Standard Errors tests ---

test_that("MarketModel with HAC stores vcov_hac and se_hac", {
  skip_if_not_installed("sandwich")
  data <- create_mock_model_data()
  mm <- MarketModel$new(use_hac = TRUE)

  mm$fit(data)
  expect_true(mm$is_fitted)

  stats <- mm$statistics
  expect_false(is.null(stats$vcov_hac))
  expect_false(is.null(stats$se_hac))
  expect_equal(length(stats$se_hac), 2)  # intercept + slope
  # sigma should be unchanged (OLS sigma, not HAC)
  expect_false(is.null(stats$sigma))
})


test_that("MarketModel HAC sigma unchanged from OLS", {
  skip_if_not_installed("sandwich")
  data <- create_mock_model_data()
  mm_ols <- MarketModel$new(use_hac = FALSE)
  mm_hac <- MarketModel$new(use_hac = TRUE)

  mm_ols$fit(data)
  mm_hac$fit(data)

  expect_equal(mm_hac$statistics$sigma, mm_ols$statistics$sigma)
})


test_that("MarketModel HAC works in full pipeline", {
  skip_if_not_installed("sandwich")
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = MarketModel$new(use_hac = TRUE))
  result <- run_event_study(task, ps)
  expect_false(is.null(result$data_tbl))
})


test_that("LinearFactorModel with HAC stores vcov_hac", {
  skip_if_not_installed("sandwich")
  data <- create_mock_factor_model_data()
  ff3 <- FamaFrench3FactorModel$new(use_hac = TRUE)

  ff3$fit(data)
  expect_true(ff3$is_fitted)
  expect_false(is.null(ff3$statistics$vcov_hac))
  expect_false(is.null(ff3$statistics$se_hac))
})


test_that("MarketModel with custom hac_lag", {
  skip_if_not_installed("sandwich")
  data <- create_mock_model_data()
  mm <- MarketModel$new(use_hac = TRUE, hac_lag = 5)

  mm$fit(data)
  expect_true(mm$is_fitted)
  expect_false(is.null(mm$statistics$vcov_hac))
  expect_false(is.null(mm$statistics$se_hac))
})


test_that("FamaFrench5FactorModel with HAC works", {
  skip_if_not_installed("sandwich")
  data <- create_mock_factor_model_data()
  ff5 <- FamaFrench5FactorModel$new(use_hac = TRUE)

  ff5$fit(data)
  expect_true(ff5$is_fitted)
  expect_false(is.null(ff5$statistics$vcov_hac))
})


test_that("Carhart4FactorModel with HAC works", {
  skip_if_not_installed("sandwich")
  data <- create_mock_factor_model_data()
  c4 <- Carhart4FactorModel$new(use_hac = TRUE)

  c4$fit(data)
  expect_true(c4$is_fitted)
  expect_false(is.null(c4$statistics$vcov_hac))
})


test_that("HAC SEs differ from OLS SEs", {
  skip_if_not_installed("sandwich")
  data <- create_mock_model_data()
  mm_ols <- MarketModel$new(use_hac = FALSE)
  mm_hac <- MarketModel$new(use_hac = TRUE)

  mm_ols$fit(data)
  mm_hac$fit(data)

  # HAC SEs should differ from OLS SEs
  ols_se <- sqrt(diag(vcov(mm_ols$model)))
  hac_se <- mm_hac$statistics$se_hac
  # They should be different (not guaranteed to be larger/smaller)
  expect_false(identical(ols_se, hac_se))
})


test_that("MarketModel without HAC has no vcov_hac", {
  data <- create_mock_model_data()
  mm <- MarketModel$new(use_hac = FALSE)
  mm$fit(data)

  expect_true(is.null(mm$statistics$vcov_hac))
  expect_true(is.null(mm$statistics$se_hac))
})
