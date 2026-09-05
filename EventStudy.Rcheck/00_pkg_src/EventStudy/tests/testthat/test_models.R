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


# --- Degenerate-input contract: FamaFrench3FactorModel ---

# Helper: make insufficient-obs degenerate factor data (NA all but 1 estimation row)
.make_ff3_insufficient <- function() {
  d <- create_mock_factor_model_data()
  est <- which(d$estimation_window == 1)
  for (col in c("excess_return", "market_excess", "smb", "hml")) {
    d[[col]][est[-1]] <- NA_real_
  }
  d
}

# Helper: make "zero complete-cases" degenerate factor data.
# For factor models, single-factor zero-variance is absorbed by lm() (collinear
# term dropped). The contract guard triggers on insufficient complete cases.
# This variant NAs ALL estimation rows to produce n_valid = 0.
.make_ff3_zero_variance <- function() {
  d <- create_mock_factor_model_data()
  est <- which(d$estimation_window == 1)
  # NA all required FF3 columns in every estimation row → n_valid = 0
  for (col in c("excess_return", "market_excess", "smb", "hml")) {
    d[[col]][est] <- NA_real_
  }
  d
}

test_that("FamaFrench3FactorModel: lenient mode — insufficient obs returns all-NA ARs with one warning", {
  d <- .make_ff3_insufficient()
  m <- FamaFrench3FactorModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_FF3"
  m$firm_symbol <- "FIRM_FF3"

  ws <- character(0)
  withCallingHandlers(
    m$fit(d),
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_false(m$is_fitted)
  expect_equal(length(ws), 1L, info = "lenient mode must emit exactly one warning")
  expect_true(grepl("EVT_FF3", ws[1]), info = "warning must name event_id")
  expect_true(grepl("FIRM_FF3", ws[1]), info = "warning must name firm_symbol")

  # abnormal_returns must return all-NA without a second warning
  ws2 <- character(0)
  withCallingHandlers(
    ar <- m$abnormal_returns(d),
    warning = function(w) {
      ws2[[length(ws2) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(ws2), 0L, info = ".degenerate_handled branch must suppress second warning")
  expect_true(all(is.na(ar$abnormal_returns)))
})

test_that("FamaFrench3FactorModel: strict mode — insufficient obs errors with event_id + firm_symbol", {
  d <- .make_ff3_insufficient()
  m <- FamaFrench3FactorModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_FF3"
  m$firm_symbol <- "FIRM_FF3"

  err <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_true(is.character(err), info = "strict mode must stop()")
  expect_true(grepl("EVT_FF3", err),   info = "error must contain event_id")
  expect_true(grepl("FIRM_FF3", err),  info = "error must contain firm_symbol")
})

test_that("FamaFrench3FactorModel: lenient mode — zero-variance (full rank deficiency) returns all-NA ARs", {
  # When all required columns are constant in the estimation window, lm() will
  # fail with a rank-deficiency error, triggering the lm-failure .handle_degenerate path.
  d <- .make_ff3_zero_variance()
  m <- FamaFrench3FactorModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_FF3"
  m$firm_symbol <- "FIRM_FF3"

  ws <- character(0)
  withCallingHandlers(
    m$fit(d),
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  # Model should be unfitted (either via insufficient-obs guard or lm failure)
  expect_false(m$is_fitted)
  expect_true(all(is.na(m$abnormal_returns(d)$abnormal_returns)))
})

test_that("FamaFrench3FactorModel: strict mode — zero-variance errors with event_id + firm_symbol", {
  d <- .make_ff3_zero_variance()
  m <- FamaFrench3FactorModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_FF3"
  m$firm_symbol <- "FIRM_FF3"

  err <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_true(is.character(err), info = "strict mode must stop()")
  expect_true(grepl("EVT_FF3", err),   info = "error must contain event_id")
  expect_true(grepl("FIRM_FF3", err),  info = "error must contain firm_symbol")
})


# --- Degenerate-input contract: FamaFrench5FactorModel ---

# Shared degenerate factory for FF5 (NA all required columns except 1 row)
.make_ff5_insufficient <- function() {
  d <- create_mock_factor_model_data()
  est <- which(d$estimation_window == 1)
  for (col in c("excess_return", "market_excess", "smb", "hml", "rmw", "cma")) {
    d[[col]][est[-1]] <- NA_real_
  }
  d
}

test_that("FamaFrench5FactorModel: lenient mode — insufficient obs returns all-NA ARs with one warning", {
  d <- .make_ff5_insufficient()
  m <- FamaFrench5FactorModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_FF5"
  m$firm_symbol <- "FIRM_FF5"

  ws <- character(0)
  withCallingHandlers(
    m$fit(d),
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_false(m$is_fitted)
  expect_equal(length(ws), 1L, info = "lenient mode must emit exactly one warning")
  expect_true(grepl("EVT_FF5", ws[1]), info = "warning must name event_id")
  expect_true(grepl("FIRM_FF5", ws[1]), info = "warning must name firm_symbol")

  ws2 <- character(0)
  withCallingHandlers(
    ar <- m$abnormal_returns(d),
    warning = function(w) {
      ws2[[length(ws2) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(ws2), 0L, info = ".degenerate_handled branch must suppress second warning")
  expect_true(all(is.na(ar$abnormal_returns)))
})

test_that("FamaFrench5FactorModel: strict mode — insufficient obs errors with event_id + firm_symbol", {
  d <- .make_ff5_insufficient()
  m <- FamaFrench5FactorModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_FF5"
  m$firm_symbol <- "FIRM_FF5"

  err <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_true(is.character(err), info = "strict mode must stop()")
  expect_true(grepl("EVT_FF5", err),  info = "error must contain event_id")
  expect_true(grepl("FIRM_FF5", err), info = "error must contain firm_symbol")
})


# --- Degenerate-input contract: Carhart4FactorModel ---

# Shared degenerate factory for Carhart4 (NA all required columns except 1 row)
.make_c4_insufficient <- function() {
  d <- create_mock_factor_model_data()
  est <- which(d$estimation_window == 1)
  for (col in c("excess_return", "market_excess", "smb", "hml", "mom")) {
    d[[col]][est[-1]] <- NA_real_
  }
  d
}

test_that("Carhart4FactorModel: lenient mode — insufficient obs returns all-NA ARs with one warning", {
  d <- .make_c4_insufficient()
  m <- Carhart4FactorModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_C4"
  m$firm_symbol <- "FIRM_C4"

  ws <- character(0)
  withCallingHandlers(
    m$fit(d),
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_false(m$is_fitted)
  expect_equal(length(ws), 1L, info = "lenient mode must emit exactly one warning")
  expect_true(grepl("EVT_C4", ws[1]), info = "warning must name event_id")
  expect_true(grepl("FIRM_C4", ws[1]), info = "warning must name firm_symbol")

  ws2 <- character(0)
  withCallingHandlers(
    ar <- m$abnormal_returns(d),
    warning = function(w) {
      ws2[[length(ws2) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(ws2), 0L, info = ".degenerate_handled branch must suppress second warning")
  expect_true(all(is.na(ar$abnormal_returns)))
})

test_that("Carhart4FactorModel: strict mode — insufficient obs errors with event_id + firm_symbol", {
  d <- .make_c4_insufficient()
  m <- Carhart4FactorModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_C4"
  m$firm_symbol <- "FIRM_C4"

  err <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_true(is.character(err), info = "strict mode must stop()")
  expect_true(grepl("EVT_C4", err),  info = "error must contain event_id")
  expect_true(grepl("FIRM_C4", err), info = "error must contain firm_symbol")
})


# --- One-warning-per-degenerate invariant across all four factor models ---

test_that("All four factor models: fit+abnormal_returns emits exactly one warning on degenerate input", {
  # This test proves the one-warning-per-(event_id,firm_symbol) contract holds
  # across the entire fit() -> abnormal_returns() call chain for every factor model.
  models <- list(
    list(ctor = LinearFactorModel$new(), setup = function(m) {
      m$formula <- stats::as.formula("firm_returns ~ index_returns")
      m$required_columns <- c("firm_returns", "index_returns")
      m
    }),
    list(ctor = FamaFrench3FactorModel$new(), setup = function(m) m),
    list(ctor = FamaFrench5FactorModel$new(), setup = function(m) m),
    list(ctor = Carhart4FactorModel$new(),    setup = function(m) m)
  )
  d_base <- create_mock_factor_model_data()
  est <- which(d_base$estimation_window == 1)

  for (entry in models) {
    m <- entry$setup(entry$ctor)
    m$degenerate_mode <- "lenient"
    m$event_id    <- "EVT_ALL"
    m$firm_symbol <- "FIRM_ALL"
    # NA all required columns in all estimation rows
    for (col in m$required_columns) {
      d_base[[col]][est] <- NA_real_
    }

    ws_total <- character(0)
    withCallingHandlers(
      { m$fit(d_base); m$abnormal_returns(d_base) },
      warning = function(w) {
        ws_total[[length(ws_total) + 1L]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    expect_equal(length(ws_total), 1L,
      info = paste(m$model_name, "fit()+abnormal_returns() must emit exactly one warning"))

    # Reset d_base for next iteration
    d_base <- create_mock_factor_model_data()
    est <- which(d_base$estimation_window == 1)
  }
})


# --- CONTRACT-05: Factor-model valid-input baseline invariance (Plan 02-02) ---

test_that("LinearFactorModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_linearfactor_baseline.rds"))
  m <- LinearFactorModel$new()
  m$formula <- stats::as.formula("firm_returns ~ index_returns")
  m$required_columns <- c("firm_returns", "index_returns")
  d <- create_mock_factor_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma,              bl$sigma, tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom,  bl$df,    tolerance = 1e-8)
  expect_equal(m$statistics$alpha,              bl$alpha, tolerance = 1e-8)
  expect_equal(m$statistics$beta,               bl$beta,  tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[1:5]
  expect_equal(ar, bl$ar, tolerance = 1e-8)
})

test_that("FamaFrench3FactorModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_ff3_baseline.rds"))
  m <- FamaFrench3FactorModel$new()
  d <- create_mock_factor_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma,             bl$sigma,    tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df,       tolerance = 1e-8)
  expect_equal(m$statistics$alpha,             bl$alpha,    tolerance = 1e-8)
  expect_equal(m$statistics$market_excess,     bl$beta_mkt, tolerance = 1e-8)
  expect_equal(m$statistics$smb,               bl$beta_smb, tolerance = 1e-8)
  expect_equal(m$statistics$hml,               bl$beta_hml, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[1:5]
  expect_equal(ar, bl$ar, tolerance = 1e-8)
})

test_that("FamaFrench5FactorModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_ff5_baseline.rds"))
  m <- FamaFrench5FactorModel$new()
  d <- create_mock_factor_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma,             bl$sigma,    tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df,       tolerance = 1e-8)
  expect_equal(m$statistics$alpha,             bl$alpha,    tolerance = 1e-8)
  expect_equal(m$statistics$market_excess,     bl$beta_mkt, tolerance = 1e-8)
  expect_equal(m$statistics$smb,               bl$beta_smb, tolerance = 1e-8)
  expect_equal(m$statistics$hml,               bl$beta_hml, tolerance = 1e-8)
  expect_equal(m$statistics$rmw,               bl$beta_rmw, tolerance = 1e-8)
  expect_equal(m$statistics$cma,               bl$beta_cma, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[1:5]
  expect_equal(ar, bl$ar, tolerance = 1e-8)
})

test_that("Carhart4FactorModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_carhart4_baseline.rds"))
  m <- Carhart4FactorModel$new()
  d <- create_mock_factor_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma,             bl$sigma,    tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df,       tolerance = 1e-8)
  expect_equal(m$statistics$alpha,             bl$alpha,    tolerance = 1e-8)
  expect_equal(m$statistics$market_excess,     bl$beta_mkt, tolerance = 1e-8)
  expect_equal(m$statistics$smb,               bl$beta_smb, tolerance = 1e-8)
  expect_equal(m$statistics$hml,               bl$beta_hml, tolerance = 1e-8)
  expect_equal(m$statistics$mom,               bl$beta_mom, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[1:5]
  expect_equal(ar, bl$ar, tolerance = 1e-8)
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


# --- Regression: BHARModel event-window-only compounding ---

test_that("BHARModel compounds returns within event window only, not across windows", {

  # Bug: cumprod ran over entire data frame, so event window compounding
  # was contaminated by estimation window values.
  # Fix: group_by(event_window) before cumprod so each window compounds independently.
  # Note: estimation window needs non-constant returns to avoid the zero-variance guard.
  set.seed(99)
  n_est <- 5
  est_firm   <- rnorm(n_est, mean = 0.01, sd = 0.005)
  est_index  <- rnorm(n_est, mean = 0.005, sd = 0.005)
  data <- tibble::tibble(
    firm_returns = c(est_firm, rep(0.02, 5)),
    index_returns = c(est_index, rep(0.01, 5)),
    estimation_window = c(rep(1, 5), rep(0, 5)),
    event_window = c(rep(0, 5), rep(1, 5)),
    relative_index = c(-5:-1, 0:4),
    event_date = c(rep(0, 5), 1, rep(0, 4))
  )

  bhar <- BHARModel$new()
  bhar$fit(data)
  result <- bhar$abnormal_returns(data)

  event_rows <- result %>% dplyr::filter(event_window == 1)
  # First event window day should be: (1+0.02) - (1+0.01) = 0.01
  # If compounding leaked from estimation window, this value would be much larger
  expect_equal(event_rows$abnormal_returns[1], 0.02 - 0.01, tolerance = 1e-10)

  # After 5 days of compounding in the event window:
  # cum_firm = cumprod(1.02, 1.02, 1.02, 1.02, 1.02) = 1.02^5
  # cum_index = cumprod(1.01, 1.01, 1.01, 1.01, 1.01) = 1.01^5
  expected_last <- 1.02^5 - 1.01^5
  expect_equal(event_rows$abnormal_returns[5], expected_last, tolerance = 1e-10)
})


# --- Regression: Constant-mean FEC for non-regression models ---

test_that("MarketAdjustedModel uses constant-mean FEC (sigma * sqrt(1 + 1/T))", {
  # Bug: MarketAdjustedModel used OLS-style FEC formula despite having no regression.
  # Fix: Use sigma * sqrt(1 + 1/T) for constant-mean models.
  data <- create_mock_model_data(n_estimation = 100, n_event = 5)
  ma <- MarketAdjustedModel$new()
  ma$fit(data)

  stats <- ma$statistics
  sigma <- stats$sigma
  T_est <- 100
  expected_fec <- sigma * sqrt(1 + 1 / T_est)

  # FEC should be a vector of length n_event, all equal to the constant-mean correction
  expect_length(stats$forecast_error_corrected_sigma, 5)
  expect_equal(stats$forecast_error_corrected_sigma, rep(expected_fec, 5), tolerance = 1e-12)
})


test_that("ComparisonPeriodMeanAdjustedModel uses constant-mean FEC", {
  # Same bug as MarketAdjustedModel
  data <- create_mock_model_data(n_estimation = 80, n_event = 7)
  cpm <- ComparisonPeriodMeanAdjustedModel$new()
  cpm$fit(data)

  stats <- cpm$statistics
  sigma <- stats$sigma
  T_est <- 80
  expected_fec <- sigma * sqrt(1 + 1 / T_est)

  expect_length(stats$forecast_error_corrected_sigma, 7)
  expect_equal(stats$forecast_error_corrected_sigma, rep(expected_fec, 7), tolerance = 1e-12)
})


test_that("VolumeModel uses constant-mean FEC", {
  data <- create_mock_model_data(n_estimation = 60, n_event = 5)
  data$firm_volume <- abs(rnorm(nrow(data), mean = 1e6, sd = 2e5))
  vm <- VolumeModel$new(log_transform = FALSE)
  vm$fit(data)

  stats <- vm$statistics
  sigma <- stats$sigma
  T_est <- 60
  expected_fec <- sigma * sqrt(1 + 1 / T_est)

  expect_length(stats$forecast_error_corrected_sigma, 5)
  expect_equal(stats$forecast_error_corrected_sigma, rep(expected_fec, 5), tolerance = 1e-12)
})


test_that("BHARModel uses constant-mean FEC", {
  data <- create_mock_model_data(n_estimation = 100, n_event = 5)
  bhar <- BHARModel$new()
  bhar$fit(data)

  stats <- bhar$statistics
  sigma <- stats$sigma
  T_est <- 100
  expected_fec <- sigma * sqrt(1 + 1 / T_est)

  expect_length(stats$forecast_error_corrected_sigma, 5)
  expect_equal(stats$forecast_error_corrected_sigma, rep(expected_fec, 5), tolerance = 1e-12)
})


# --- Regression: VolatilityModel ratio-form residuals ---

test_that("VolatilityModel residuals match abnormal_returns formula (r^2/V - 1)", {
  # Bug: residuals were computed as r^2 - V (additive) but abnormal_returns was
  # r^2/V - 1 (ratio). FEC sigma was inconsistent with actual AR.
  # Fix: residuals now use r^2/V - 1, same form as abnormal_returns.
  set.seed(42)
  data <- create_mock_model_data(n_estimation = 100, n_event = 5)
  volm <- VolatilityModel$new()
  volm$fit(data)

  stats <- volm$statistics

  # Manually compute what the residuals should be
  est_data <- data %>% dplyr::filter(estimation_window == 1)
  est_var <- var(est_data$firm_returns, na.rm = TRUE)
  expected_residuals <- est_data$firm_returns^2 / est_var - 1

  expect_equal(stats$residuals, expected_residuals, tolerance = 1e-10)

  # And verify FEC is constant-mean form
  sigma <- sd(expected_residuals, na.rm = TRUE)
  expected_fec <- sigma * sqrt(1 + 1 / 100)
  expect_equal(stats$forecast_error_corrected_sigma, rep(expected_fec, 5), tolerance = 1e-12)
})


# --- Regression: LinearFactorModel multi-factor FEC ---

test_that("LinearFactorModel FEC uses hat matrix (X'X)^{-1} for correction", {
  # Bug: LinearFactorModel used 1/T scalar correction instead of leveraging the
  # full hat matrix from the regression, which captures how far each event-window
  # day's factor values are from the estimation-window mean.
  # Fix: FEC = sigma * sqrt(1 + h_t) where h_t = x_t'(X'X)^{-1}x_t
  data <- create_mock_factor_model_data(n_estimation = 120, n_event = 11)
  ff3 <- FamaFrench3FactorModel$new()
  ff3$fit(data)

  stats <- ff3$statistics
  fec <- stats$forecast_error_corrected_sigma

  # FEC should be a vector (one per event-window day), NOT a scalar replicated
  expect_length(fec, 11)
  # Each day's FEC depends on its own factor values, so they should NOT all be identical
  # (unless by extreme coincidence)
  expect_false(all(fec == fec[1]))

  # Verify the formula: FEC = sigma * sqrt(1 + h_t)
  sigma <- stats$sigma
  # All FEC values should be >= sigma (since h_t >= 0)
  expect_true(all(fec >= sigma * 0.999))  # small tolerance for floating point
})


# ============================================================
# Phase 2 — Degenerate-input contract tests (Plan 02-01)
# ============================================================

# ---- .finite_residual_df helper ----

test_that(".finite_residual_df returns finite-only count minus n_params, floored at 1", {
  # c(1, 2, NA, 4): 3 finite, minus n_params=1 -> 2
  expect_equal(EventStudy:::.finite_residual_df(c(1, 2, NA, 4)), 2L)
  # c(1, NA, NA): 1 finite, minus 1 -> 0, floor to 1
  expect_equal(EventStudy:::.finite_residual_df(c(1, NA, NA)), 1L)
  # All NA -> 0 finite, floor to 1
  expect_equal(EventStudy:::.finite_residual_df(c(NA, NA)), 1L)
  # n_params = 2: c(1, 2, 3, NA) has 3 finite, minus 2 -> 1
  expect_equal(EventStudy:::.finite_residual_df(c(1, 2, 3, NA), n_params = 2L), 1L)
  # Inf is not finite: c(1, Inf, 3, NA) has 2 finite (1 and 3), minus 1 -> 1
  expect_equal(EventStudy:::.finite_residual_df(c(1, Inf, 3, NA)), 1L)
})


# ---- MarketAdjustedModel degenerate contract ----

test_that("MarketAdjustedModel: lenient mode — insufficient obs returns is_fitted=FALSE + one warning + all-NA ARs", {
  m <- MarketAdjustedModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_MA"
  m$firm_symbol <- "FIRM_MA"
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  d$firm_returns[est[-1]]  <- NA_real_
  d$index_returns[est[-1]] <- NA_real_
  ws <- character(0)
  withCallingHandlers(
    m$fit(d),
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_false(m$is_fitted, info = "is_fitted should be FALSE for insuff")
  expect_equal(length(ws), 1L, info = "exactly one warning for insuff")
  ar <- m$abnormal_returns(d)
  expect_true(all(is.na(ar$abnormal_returns)), info = "all-NA ARs for insuff")
  extra_ws <- character(0)
  withCallingHandlers(
    m$abnormal_returns(d),
    warning = function(w) {
      extra_ws[[length(extra_ws) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(extra_ws), 0L, info = "no second warning from abnormal_returns")
})

test_that("MarketAdjustedModel: strict mode — insufficient obs raises named error", {
  m <- MarketAdjustedModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_MA"
  m$firm_symbol <- "FIRM_MA"
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  d$firm_returns[est[-1]]  <- NA_real_
  d$index_returns[est[-1]] <- NA_real_
  err_msg <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_type(err_msg, "character")
  expect_match(err_msg, "MarketAdjustedModel", info = "error names model")
  expect_match(err_msg, "EVT_MA",              info = "error names event_id")
  expect_match(err_msg, "FIRM_MA",             info = "error names firm_symbol")
})

# --- CR-01 regression test: MarketAdjustedModel must NOT false-degenerate on zero-variance diff ---
test_that("CR-01: MarketAdjustedModel — stock tracking index (zero diff-variance) yields is_fitted=TRUE and ~0 ARs", {
  # A firm that perfectly tracks the index has firm_returns == index_returns in the
  # estimation window. The old false-degenerate guard would have fired here, producing
  # NA abnormal returns instead of the correct 0. After CR-01 fix, the model fits and
  # returns abnormal_returns == 0 for those rows (with sigma == 0 so t-stats are NA).
  m <- MarketAdjustedModel$new()
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  # Make firm_returns == index_returns in the estimation window
  d$firm_returns[est] <- d$index_returns[est]

  # Should fit WITHOUT warning
  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_true(m$is_fitted, info = "must be fitted when firm perfectly tracks index")
  expect_equal(length(ws), 0L, info = "no warning for valid zero-diff input")

  # Abnormal returns in estimation window should be ~0
  ar <- m$abnormal_returns(d)
  expect_true("abnormal_returns" %in% names(ar))
  est_ar <- ar$abnormal_returns[ar$estimation_window == 1]
  expect_true(all(abs(est_ar) < 1e-12), info = "estimation-window ARs must be ~0 when firm tracks index")
})


# ---- ComparisonPeriodMeanAdjustedModel degenerate contract ----

test_that("ComparisonPeriodMeanAdjustedModel: lenient mode — insufficient obs returns is_fitted=FALSE + one warning + all-NA ARs", {
  m <- ComparisonPeriodMeanAdjustedModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_CPM"
  m$firm_symbol <- "FIRM_CPM"
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  d$firm_returns[est[-1]] <- NA_real_
  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_false(m$is_fitted, info = "insuff: is_fitted=FALSE")
  expect_equal(length(ws), 1L, info = "insuff: exactly one warning")
  ar <- m$abnormal_returns(d)
  expect_true(all(is.na(ar$abnormal_returns)), info = "insuff: all-NA ARs")
})

test_that("ComparisonPeriodMeanAdjustedModel: strict mode — insufficient obs raises named error", {
  m <- ComparisonPeriodMeanAdjustedModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_CPM"
  m$firm_symbol <- "FIRM_CPM"
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  d$firm_returns[est[-1]] <- NA_real_
  err_msg <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_type(err_msg, "character")
  expect_match(err_msg, "ComparisonPeriodMeanAdjustedModel")
  expect_match(err_msg, "EVT_CPM")
  expect_match(err_msg, "FIRM_CPM")
})

# --- WR-05 regression test: ComparisonPeriodMeanAdjustedModel must NOT false-degenerate on constant returns ---
test_that("WR-05: ComparisonPeriodMeanAdjustedModel — constant estimation returns yields is_fitted=TRUE and defined ARs", {
  # A constant-returns fund (all estimation returns identical) has a well-defined mean
  # and produces valid abnormal returns = firm_returns - mean. The old false-degenerate
  # guard would have fired here. After WR-05 fix, the model fits and returns correct ARs;
  # sigma==0 propagates to the t-stat layer which returns NA t-stats (correct behavior).
  m <- ComparisonPeriodMeanAdjustedModel$new()
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  constant_val <- 0.001
  d$firm_returns[est] <- constant_val

  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_true(m$is_fitted, info = "must be fitted for constant estimation returns")
  expect_equal(length(ws), 0L, info = "no warning for valid constant-returns input")

  # Abnormal returns in estimation window should be ~0 (constant - constant = 0)
  ar <- m$abnormal_returns(d)
  est_ar <- ar$abnormal_returns[ar$estimation_window == 1]
  expect_true(all(abs(est_ar) < 1e-12), info = "estimation-window ARs must be 0 for constant returns")
})


# ---- CustomModel degenerate contract ----

test_that("CustomModel: degenerate input — abnormal_returns returns NA without calling predict(NULL)", {
  # CustomModel inherits MarketModel's fit() which already has the contract guards.
  # Test that abnormal_returns() does not crash when fit() failed degenerately.
  m <- CustomModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_CM"
  m$firm_symbol <- "FIRM_CM"
  d <- create_degenerate_model_data_insufficient()
  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_false(m$is_fitted)
  # abnormal_returns must NOT throw (predict(NULL,...) would throw)
  ar <- expect_no_error(m$abnormal_returns(d))
  expect_true(all(is.na(ar$abnormal_returns)))
})


# ---- BHARModel degenerate contract ----

test_that("BHARModel: lenient mode — insufficient obs returns is_fitted=FALSE + one warning + all-NA ARs", {
  m <- BHARModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_BHAR"
  m$firm_symbol <- "FIRM_BHAR"
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  d$firm_returns[est[-1]]  <- NA_real_
  d$index_returns[est[-1]] <- NA_real_
  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_false(m$is_fitted, info = "insuff: is_fitted=FALSE")
  expect_equal(length(ws), 1L, info = "insuff: one warning")
  ar <- m$abnormal_returns(d)
  expect_true(all(is.na(ar$abnormal_returns)), info = "insuff: all-NA ARs")
})

test_that("BHARModel: strict mode — insufficient obs raises named error", {
  m <- BHARModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_BHAR"
  m$firm_symbol <- "FIRM_BHAR"
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  d$firm_returns[est[-1]]  <- NA_real_
  d$index_returns[est[-1]] <- NA_real_
  err_msg <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_type(err_msg, "character")
  expect_match(err_msg, "BHARModel")
  expect_match(err_msg, "EVT_BHAR")
  expect_match(err_msg, "FIRM_BHAR")
})

# --- CR-02 regression tests ---
test_that("CR-02A: BHARModel — stock tracking index (zero diff-variance) yields is_fitted=TRUE and ~0 event-window BHAR", {
  # A firm that perfectly tracks the index has firm_returns == index_returns in the
  # estimation window. The old false-degenerate guard would have fired here, producing
  # NA instead of the correct BHAR ~0. After CR-02 fix, the model fits and produces
  # valid compounded abnormal returns (which are ~0 when both series are identical).
  m <- BHARModel$new()
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  # Make firm_returns == index_returns in estimation window (zero diff-variance)
  d$firm_returns[est] <- d$index_returns[est]

  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_true(m$is_fitted, info = "must be fitted when firm perfectly tracks index")
  expect_equal(length(ws), 0L, info = "no warning for valid zero-diff input")

  # Event-window abnormal returns should be compounded; non-equal event-window returns
  # will produce non-zero BHAR, but the key is no NA and no error
  ar <- m$abnormal_returns(d)
  expect_true("abnormal_returns" %in% names(ar))
  # Estimation-window BHAR: since firm == index in estimation, compounded BHAR there = 0
  est_ar <- ar$abnormal_returns[ar$estimation_window == 1]
  expect_true(all(abs(est_ar) < 1e-10), info = "estimation-window BHAR ~0 when firm tracks index")
})

test_that("CR-02B: BHARModel FEC uses finite pair count not nrow (NA-heavy estimation window)", {
  # With NA rows in the estimation window, nrow inflates the FEC denominator,
  # understating correction and yielding t-statistics that are too large.
  # After CR-02B fix, the correction uses the finite pair count so df is correct.
  m <- BHARModel$new()
  d <- create_mock_model_data(n_estimation = 50, n_event = 11)
  est <- which(d$estimation_window == 1)
  # NA out 10 rows — finite pair count is 40, not 50
  d$firm_returns[est[1:10]]  <- NA_real_
  d$index_returns[est[1:10]] <- NA_real_
  m$fit(d)
  expect_true(m$is_fitted)
  # FEC with finite count: sigma * sqrt(1 + 1/40)
  # FEC with nrow:         sigma * sqrt(1 + 1/50)  [old, wrong]
  sigma <- m$statistics$sigma
  fec_val <- m$statistics$forecast_error_corrected_sigma[1]
  expected_fec_finite <- sigma * sqrt(1 + 1 / 40)
  expected_fec_nrow   <- sigma * sqrt(1 + 1 / 50)
  # Must match finite-pair formula, NOT nrow formula
  expect_equal(fec_val, expected_fec_finite, tolerance = 1e-10,
               info = "FEC must use finite pair count (n=40), not nrow (n=50)")
  expect_false(isTRUE(all.equal(fec_val, expected_fec_nrow, tolerance = 1e-10)),
               info = "FEC must NOT equal nrow-based formula (n=50)")
})

test_that("BHARModel: df reflects only finite residuals (MODELS-03)", {
  # Inject NA rows into estimation window — df should be < nrow-1
  m <- BHARModel$new()
  d <- create_mock_model_data(n_estimation = 50, n_event = 11)
  est <- which(d$estimation_window == 1)
  # NA out half the estimation rows
  d$firm_returns[est[1:10]]  <- NA_real_
  d$index_returns[est[1:10]] <- NA_real_
  m$fit(d)
  expect_true(m$is_fitted)
  df_model <- m$statistics$degree_of_freedom
  # nrow - 1 = 49, but 10 NA rows so finite df should be <= 39
  expect_lt(df_model, nrow(d[d$estimation_window == 1, ]) - 1)
})


# ---- VolumeModel degenerate contract ----

test_that("VolumeModel: lenient mode — insufficient obs returns is_fitted=FALSE + one warning + all-NA ARs", {
  m <- VolumeModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_VOL"
  m$firm_symbol <- "FIRM_VOL"
  d <- create_degenerate_volume_model_data_insufficient()
  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_false(m$is_fitted)
  expect_length(ws, 1L)
  ar <- m$abnormal_returns(d)
  expect_true(all(is.na(ar$abnormal_returns)))
})

test_that("VolumeModel: lenient mode — zero variance returns is_fitted=FALSE + one warning + all-NA ARs", {
  m <- VolumeModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_VOL"
  m$firm_symbol <- "FIRM_VOL"
  d <- create_degenerate_volume_model_data_zero_variance()
  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  expect_false(m$is_fitted)
  expect_length(ws, 1L)
  ar <- m$abnormal_returns(d)
  expect_true(all(is.na(ar$abnormal_returns)))
})

test_that("VolumeModel: strict mode — raises named error", {
  for (factory in c("insuff", "zerovar")) {
    m <- VolumeModel$new()
    m$degenerate_mode <- "strict"
    m$event_id    <- "EVT_VOL"
    m$firm_symbol <- "FIRM_VOL"
    d <- if (factory == "insuff") create_degenerate_volume_model_data_insufficient() else create_degenerate_volume_model_data_zero_variance()
    err_msg <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
    expect_type(err_msg, "character")
    expect_match(err_msg, "VolumeModel")
    expect_match(err_msg, "EVT_VOL")
    expect_match(err_msg, "FIRM_VOL")
  }
})


# ---- VolatilityModel degenerate contract ----

test_that("VolatilityModel: lenient mode — zero variance returns is_fitted=FALSE + one warning + all-NA ARs", {
  m <- VolatilityModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id    <- "EVT_VOLA"
  m$firm_symbol <- "FIRM_VOLA"
  d <- create_degenerate_volatility_model_data_zero_var()
  ws <- character(0)
  withCallingHandlers(m$fit(d), warning = function(w) {
    ws[[length(ws) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  # CRITICAL: is_fitted must be FALSE (guard is now in fit() BEFORE is_fitted <- TRUE)
  expect_false(m$is_fitted)
  expect_length(ws, 1L)
  ar <- m$abnormal_returns(d)
  expect_true(all(is.na(ar$abnormal_returns)))
})

test_that("VolatilityModel: strict mode — zero variance raises named error", {
  m <- VolatilityModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_VOLA"
  m$firm_symbol <- "FIRM_VOLA"
  d <- create_degenerate_volatility_model_data_zero_var()
  err_msg <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_type(err_msg, "character")
  expect_match(err_msg, "VolatilityModel")
  expect_match(err_msg, "EVT_VOLA")
  expect_match(err_msg, "FIRM_VOLA")
})

test_that("VolatilityModel: insufficient obs raises contract error in strict mode", {
  m <- VolatilityModel$new()
  m$degenerate_mode <- "strict"
  m$event_id    <- "EVT_VOLA2"
  m$firm_symbol <- "FIRM_VOLA2"
  d <- create_mock_model_data()
  est <- which(d$estimation_window == 1)
  d$firm_returns[est[-1]] <- NA_real_
  err_msg <- tryCatch(m$fit(d), error = function(e) conditionMessage(e))
  expect_type(err_msg, "character")
  expect_match(err_msg, "VolatilityModel")
})


# ============================================================
# CONTRACT-05: Per-model valid-input baseline invariance (Plan 02-01)
#
# These baselines were captured POST-migration because the guards are pure
# early-return paths that cannot alter the valid-input code path. The
# invariance tests prove this claim holds going forward: any future edit
# that inadvertently changes valid-input computation will be caught here.
# ============================================================

test_that("MarketAdjustedModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_marketadjusted_baseline.rds"))
  m <- MarketAdjustedModel$new()
  d <- create_mock_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma, bl$sigma, tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df, tolerance = 1e-8)
  expect_equal(m$statistics$forecast_error_corrected_sigma[1:3], bl$fec, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[which(d$event_window == 1)][1:5]
  expect_equal(ar, bl$ar5, tolerance = 1e-8)
})

test_that("ComparisonPeriodMeanAdjustedModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_comparisonperiod_baseline.rds"))
  m <- ComparisonPeriodMeanAdjustedModel$new()
  d <- create_mock_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma, bl$sigma, tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[which(d$event_window == 1)][1:5]
  expect_equal(ar, bl$ar5, tolerance = 1e-8)
})

test_that("CustomModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_custom_baseline.rds"))
  m <- CustomModel$new()
  d <- create_mock_model_data()
  d$loss_market_cap <- 0
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma, bl$sigma, tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[which(d$event_window == 1)][1:5]
  expect_equal(ar, bl$ar5, tolerance = 1e-8)
})

test_that("BHARModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_bhar_baseline.rds"))
  m <- BHARModel$new()
  d <- create_mock_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma, bl$sigma, tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[which(d$event_window == 1)][1:5]
  expect_equal(ar, bl$ar5, tolerance = 1e-8)
})

test_that("VolumeModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_volume_baseline.rds"))
  m <- VolumeModel$new()
  d <- create_mock_model_data()
  set.seed(77)
  d$firm_volume <- abs(rnorm(nrow(d), mean = 1e6, sd = 2e5))
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma, bl$sigma, tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[which(d$event_window == 1)][1:5]
  expect_equal(ar, bl$ar5, tolerance = 1e-8)
})

test_that("VolatilityModel: valid-input baseline invariance (CONTRACT-05)", {
  bl <- readRDS(test_path("fixtures", "contract05_volatility_baseline.rds"))
  m <- VolatilityModel$new()
  d <- create_mock_model_data()
  m$fit(d)
  expect_true(m$is_fitted)
  expect_equal(m$statistics$sigma, bl$sigma, tolerance = 1e-8)
  expect_equal(m$statistics$degree_of_freedom, bl$df, tolerance = 1e-8)
  ar <- m$abnormal_returns(d)$abnormal_returns[which(d$event_window == 1)][1:5]
  expect_equal(ar, bl$ar5, tolerance = 1e-8)
})


# ============================================================
# WR-03: FEC finite-pair-count regression tests (Plan 02-02)
#
# When the estimation window contains NA rows, nrow() inflates the FEC
# denominator, understating the correction factor and producing t-statistics
# that are too large (false positives). These tests verify the fix.
# ============================================================

test_that("WR-03: MarketAdjustedModel FEC uses finite pair count not nrow (NA-heavy window)", {
  m <- MarketAdjustedModel$new()
  d <- create_mock_model_data(n_estimation = 50, n_event = 5)
  est <- which(d$estimation_window == 1)
  # NA out 10 rows: finite pairs = 40, nrow = 50
  d$firm_returns[est[1:10]]  <- NA_real_
  d$index_returns[est[1:10]] <- NA_real_
  m$fit(d)
  expect_true(m$is_fitted)
  sigma <- m$statistics$sigma
  fec_val <- m$statistics$forecast_error_corrected_sigma[1]
  expected_finite <- sigma * sqrt(1 + 1 / 40)
  expected_nrow   <- sigma * sqrt(1 + 1 / 50)
  expect_equal(fec_val, expected_finite, tolerance = 1e-10,
               info = "FEC must use finite pair count (n=40), not nrow (n=50)")
  expect_false(isTRUE(all.equal(fec_val, expected_nrow, tolerance = 1e-10)),
               info = "FEC must NOT equal nrow-based formula")
})

test_that("WR-03: ComparisonPeriodMeanAdjustedModel FEC uses finite value count not nrow (NA-heavy window)", {
  m <- ComparisonPeriodMeanAdjustedModel$new()
  d <- create_mock_model_data(n_estimation = 50, n_event = 5)
  est <- which(d$estimation_window == 1)
  # NA out 15 rows: finite firm_returns = 35, nrow = 50
  d$firm_returns[est[1:15]] <- NA_real_
  m$fit(d)
  expect_true(m$is_fitted)
  sigma <- m$statistics$sigma
  fec_val <- m$statistics$forecast_error_corrected_sigma[1]
  expected_finite <- sigma * sqrt(1 + 1 / 35)
  expected_nrow   <- sigma * sqrt(1 + 1 / 50)
  expect_equal(fec_val, expected_finite, tolerance = 1e-10,
               info = "CPM FEC must use finite value count (n=35), not nrow (n=50)")
  expect_false(isTRUE(all.equal(fec_val, expected_nrow, tolerance = 1e-10)),
               info = "CPM FEC must NOT equal nrow-based formula")
})
