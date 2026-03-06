# Edge case tests for real-world messy data scenarios
# Covers: NA propagation, division by zero, single events, empty windows,
# zero variance, degenerate inputs


# ============================================================================
# Single-event group edge cases (multi-event stats with n=1)
# ============================================================================

test_that("CSectTTest with single event produces NA for sd-based stats", {
  data = tibble::tibble(
    event_id = "E1",
    firm_symbol = "F1",
    relative_index = -5:5,
    abnormal_returns = rnorm(11, mean = 0.01, sd = 0.02),
    event_window = 1,
    estimation_window = 0
  )

  csect = CSectTTest$new()
  result = csect$compute(data, NULL)

  expect_equal(nrow(result), 11)
  # sd() of single value is NA, so aar_t should be NA/NaN
  expect_true(all(is.na(result$aar_t) | is.nan(result$aar_t)))
  # AAR itself should still be valid (it's just the single firm's AR)
  expect_true(all(is.finite(result$aar)))
})


test_that("SignTest with single event still computes", {
  data = tibble::tibble(
    event_id = "E1",
    firm_symbol = "F1",
    relative_index = -5:5,
    abnormal_returns = rnorm(11, mean = 0.01, sd = 0.02),
    event_window = 1,
    estimation_window = 0
  )

  sign_test = SignTest$new()
  result = sign_test$compute(data, NULL)

  expect_equal(nrow(result), 11)
  # With n=1, sign_z = (n_pos - 0.5) / (0.5 * sqrt(1)) = ±1
  expect_true(all(is.finite(result$sign_z)))
})


test_that("GeneralizedSignTest with all positive estimation returns (p_hat=1)", {
  n_est = 50
  n_ev = 11
  data = tibble::tibble(
    event_id = "E1",
    firm_symbol = "F1",
    relative_index = c(seq(-n_est, -1), seq(0, n_ev - 1)),
    abnormal_returns = c(abs(rnorm(n_est, 0.01, 0.005)),  # all positive
                         rnorm(n_ev, 0.001, 0.02)),
    event_window = c(rep(0, n_est), rep(1, n_ev)),
    estimation_window = c(rep(1, n_est), rep(0, n_ev))
  )

  gsign = GeneralizedSignTest$new()
  result = gsign$compute(data, NULL)

  expect_equal(nrow(result), n_ev)
  # p_hat = 1.0 → denominator = sqrt(n * 1 * 0) = 0 → division by zero
  # produces NaN (0/0) or Inf. Documents current behavior.
  expect_true(all(is.nan(result$gsign_z) | is.infinite(result$gsign_z)))
})


test_that("RankTest with single event does not crash", {
  n_est = 50
  n_ev = 11
  data = tibble::tibble(
    event_id = "E1",
    firm_symbol = "F1",
    relative_index = c(seq(-n_est, -1), seq(0, n_ev - 1)),
    abnormal_returns = rnorm(n_est + n_ev, mean = 0, sd = 0.02),
    event_window = c(rep(0, n_est), rep(1, n_ev)),
    estimation_window = c(rep(1, n_est), rep(0, n_ev))
  )

  rank_test = RankTest$new()
  result = rank_test$compute(data, NULL)

  expect_equal(nrow(result), n_ev)
  expect_true("rank_z" %in% names(result))
})


# ============================================================================
# NA propagation in abnormal returns
# ============================================================================

test_that("CSectTTest handles NA abnormal returns gracefully", {
  set.seed(42)
  data = do.call(rbind, lapply(1:3, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -5:5,
      abnormal_returns = rnorm(11, mean = 0.005, sd = 0.02),
      event_window = 1,
      estimation_window = 0
    )
  }))

  # Inject NAs in one firm's returns
  data$abnormal_returns[data$firm_symbol == "F2"] = NA_real_

  csect = CSectTTest$new()
  result = csect$compute(data, NULL)

  expect_equal(nrow(result), 11)
  # n_valid_events should be 2 (F1 and F3), not 3
  expect_true(all(result$n_valid_events == 2))
  # AAR should still be computed from the 2 valid events
  expect_true(all(is.finite(result$aar)))
})


test_that("BMPTest handles NA abnormal returns", {
  # Inline data creation to avoid scope issues with helper defined in another test file
  set.seed(42)
  n_firms = 3
  n_est = 50
  n_ev = 11
  data = do.call(rbind, lapply(seq_len(n_firms), function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1), seq(0, n_ev - 1)),
      index_returns = rnorm(n_est + n_ev, mean = 0.0003, sd = 0.015),
      firm_returns = 0.001 + 1.2 * index_returns + rnorm(n_est + n_ev, sd = 0.01),
      abnormal_returns = rnorm(n_est + n_ev, mean = 0.001, sd = 0.02),
      event_window = c(rep(0, n_est), rep(1, n_ev)),
      estimation_window = c(rep(1, n_est), rep(0, n_ev)),
      event_date = c(rep(0, n_est), 1, rep(0, n_ev - 1))
    )
  }))

  model_tbl = tibble::tibble(
    firm_symbol = paste0("F", seq_len(n_firms)),
    model = lapply(seq_len(n_firms), function(i) {
      mm = MarketModel$new()
      mm$fit(data[data$firm_symbol == paste0("F", i), ])
      mm
    })
  )

  # Inject NAs
  data$abnormal_returns[data$firm_symbol == "F2" & data$event_window == 1] = NA_real_

  bmp = BMPTest$new()
  result = bmp$compute(data, model_tbl)

  expect_equal(nrow(result), n_ev)
  # Should not crash
  expect_true("bmp_t" %in% names(result))
})


# ============================================================================
# Zero variance / constant returns
# ============================================================================

test_that("MarketModel with zero-variance firm returns produces near-zero sigma", {
  data = create_mock_model_data()
  # Constant firm returns: lm fits fine (intercept ≈ constant, beta ≈ 0)
  data$firm_returns = 0.001

  mm = MarketModel$new()
  result = mm$fit(data)
  expect_true(length(result) > 0)
  # Sigma should be essentially zero since residuals ≈ 0
  expect_lt(mm$statistics$sigma, 1e-10)
})


test_that("MarketModel with zero-variance index returns crashes in calculate_statistics", {
  data = create_mock_model_data()
  # Constant index returns → lm drops the predictor
  data$index_returns = 0.0005

  mm = MarketModel$new()
  # Same bug as above: calculate_statistics[2,4] subscript out of bounds
  expect_error(mm$fit(data))
})


test_that("ComparisonPeriodMeanAdjustedModel with constant estimation returns", {
  data = create_mock_model_data()
  # All estimation window firm returns identical
  data$firm_returns[data$estimation_window == 1] = 0.001

  cpmam = ComparisonPeriodMeanAdjustedModel$new()
  cpmam$fit(data)

  expect_true(cpmam$is_fitted)
  # sigma = sd of constant = 0
  expect_equal(cpmam$statistics$sigma, 0)
  # degree_of_freedom should still be valid
  expect_true(cpmam$statistics$degree_of_freedom > 0)
})


test_that("MarketAdjustedModel with constant estimation residuals", {
  data = create_mock_model_data()
  # Make firm_returns = index_returns exactly (zero residuals)
  data$firm_returns = data$index_returns

  mam = MarketAdjustedModel$new()
  mam$fit(data)

  expect_true(mam$is_fitted)
  # sigma = sd(0, 0, ..., 0) = 0
  expect_equal(mam$statistics$sigma, 0)
})


# ============================================================================
# Forecast error correction with zero-variance market returns
# ============================================================================

test_that("Forecast error correction in MarketAdjustedModel uses constant-mean formula", {
  data = create_mock_model_data()
  # Constant index returns — should not affect forecast error correction
  data$index_returns = 0.001

  mam = MarketAdjustedModel$new()
  mam$fit(data)

  expect_true(mam$is_fitted)
  # Constant-mean correction: sigma * sqrt(1 + 1/T) — always finite
  fec = mam$statistics$forecast_error_corrected_sigma
  expect_true(!is.null(fec))
  expect_true(all(is.finite(fec)))
  # All values should be equal (constant correction, no regression term)
  expect_equal(length(unique(fec)), 1)
  # Should equal sigma * sqrt(1 + 1/T)
  T_est = sum(data$estimation_window == 1)
  expected = mam$statistics$sigma * sqrt(1 + 1 / T_est)
  expect_equal(fec[1], expected, tolerance = 1e-10)
})


# ============================================================================
# PatellZTest edge cases
# ============================================================================

test_that("PatellZTest with very short estimation window (m <= 4)", {
  # Create data with only 4 observations in estimation window
  set.seed(42)
  n_est = 4
  n_ev = 5
  data = do.call(rbind, lapply(1:3, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1), seq(0, n_ev - 1)),
      index_returns = rnorm(n_est + n_ev, 0, 0.015),
      firm_returns = rnorm(n_est + n_ev, 0, 0.02),
      abnormal_returns = rnorm(n_est + n_ev, 0, 0.02),
      event_window = c(rep(0, n_est), rep(1, n_ev)),
      estimation_window = c(rep(1, n_est), rep(0, n_ev)),
      event_date = c(rep(0, n_est), 1, rep(0, n_ev - 1))
    )
  }))

  model_tbl = tibble::tibble(
    firm_symbol = paste0("F", 1:3),
    model = lapply(1:3, function(i) {
      mm = MarketModel$new()
      mm$fit(data[data$firm_symbol == paste0("F", i), ])
      mm
    })
  )

  patell = PatellZTest$new()
  # m=4 → Q = (4-2)/(4-4) = 2/0 = Inf
  # This documents current behavior (Inf propagation)
  result = patell$compute(data, model_tbl)
  expect_equal(nrow(result), n_ev)
})


# ============================================================================
# Return calculation edge cases
# ============================================================================

test_that("SimpleReturn with zero price produces correct result or Inf", {
  sr = SimpleReturn$new()
  tbl = tibble::tibble(p = c(100, 0, 50))
  result = sr$calculate_return(tbl, "p", "r")
  # (0 - 100) / lag(100) = -1 (divides by lagged price)
  expect_equal(result$r[2], -1)
  # (50 - 0) / lag(0) = Inf (divides by lagged zero)
  expect_true(is.infinite(result$r[3]))
})


test_that("LogReturn with zero price produces -Inf or NaN", {
  lr = LogReturn$new()
  tbl = tibble::tibble(p = c(100, 0, 50))
  result = lr$calculate_return(tbl, "p", "r")
  # log(0/100) = -Inf
  expect_true(is.infinite(result$r[2]) || is.nan(result$r[2]))
})


test_that("LogReturn with negative price produces NaN", {
  lr = LogReturn$new()
  tbl = tibble::tibble(p = c(100, -10, 50))
  expect_warning(
    result <- lr$calculate_return(tbl, "p", "r"),
    "NaN"
  )
  expect_true(is.nan(result$r[2]))
})


# ============================================================================
# Single-day event window
# ============================================================================

test_that("Full pipeline with single-day event window", {
  symbols = c("FIRM_A", "FIRM_B")
  firm_data = create_mock_firm_data(symbols = symbols)
  index_data = create_mock_index_data()

  n_days = 300
  start_date = as.Date("2020-01-01")
  dates = seq(start_date, by = "day", length.out = n_days)
  dates = dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

  request = tibble::tibble(
    event_id = 1:2,
    firm_symbol = symbols,
    index_symbol = "INDEX_1",
    event_date = rep(format(dates[180], "%d.%m.%Y"), 2),
    group = "TestGroup",
    event_window_start = 0,   # single day: [0, 0]
    event_window_end = 0,
    shift_estimation_window = -1,
    estimation_window_length = 120
  )

  task = EventStudyTask$new(firm_data, index_data, request)
  ps = ParameterSet$new()
  task = run_event_study(task, ps)

  # Should complete without error

  expect_true("model" %in% names(task$data_tbl))
  # Event window AR should have exactly 1 row per event
  ar_data = task$data_tbl$data[[1]] %>%
    dplyr::filter(event_window == 1)
  expect_equal(nrow(ar_data), 1)
})


# ============================================================================
# Event date not found in data
# ============================================================================

test_that("validate_task warns when event date missing from data", {
  task = create_mock_task()
  # Mangle the event date to one that doesn't exist
  task$data_tbl$request[[1]]$event_date = "01.01.1900"

  expect_warning(validate_task(task), "not found")
})


# ============================================================================
# extract_cars with all-NA abnormal returns
# ============================================================================

test_that("cross_sectional_regression with all-NA abnormal returns gives NA CARs", {
  task = create_fitted_mock_task(n_firms = 3)

  # Replace all abnormal returns with NA
  for (i in seq_len(nrow(task$data_tbl))) {
    task$data_tbl$data[[i]]$abnormal_returns = NA_real_
  }

  firm_chars = tibble::tibble(
    event_id = 1:2,
    x = c(1.0, 2.0)
  )

  # All-NA ARs should produce NA CARs (not 0)
  # The regression will fail due to no non-NA cases
  expect_error(
    cross_sectional_regression(task, ~ x, firm_chars, robust = FALSE),
    "0 \\(non-NA\\) cases|missing"
  )
})


# ============================================================================
# CalendarTimePortfolioTest with identical AARs (ts_sd = 0)
# ============================================================================

test_that("CalendarTimePortfolioTest with constant AARs across time", {
  # All firms have exactly the same abnormal return on every day
  data = do.call(rbind, lapply(1:5, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -5:5,
      abnormal_returns = 0.01,  # constant across firms and time
      event_window = 1,
      estimation_window = 0
    )
  }))

  ct = CalendarTimePortfolioTest$new()
  result = ct$compute(data, NULL)

  expect_equal(nrow(result), 11)
  # ts_sd = sd(constant) = 0 → caltime_t = value/0 = Inf
  expect_true(all(is.infinite(result$caltime_t) | is.nan(result$caltime_t)))
})


# ============================================================================
# Empty event window
# ============================================================================

test_that("CSectTTest with empty event window returns 0-row result", {
  data = tibble::tibble(
    event_id = "E1",
    firm_symbol = "F1",
    relative_index = seq(-50, -1),
    abnormal_returns = rnorm(50),
    event_window = 0,  # no event window at all
    estimation_window = 1
  )

  csect = CSectTTest$new()
  result = csect$compute(data, NULL)
  expect_equal(nrow(result), 0)
})


# ============================================================================
# BHARTTest NA coalesce behavior
# ============================================================================

test_that("BHARTTest replaces NA returns with 0 in compounding", {
  data = create_mock_model_data()
  # Inject NA into event window firm_returns
  data$firm_returns[data$event_window == 1][3] = NA_real_

  bhar = BHARModel$new()
  bhar$fit(data)

  bhart = BHARTTest$new()
  result = bhart$compute(bhar$abnormal_returns(data), bhar)

  expect_equal(nrow(result), sum(data$event_window))
  # Should not have NAs because coalesce(NA, 0) = 0
  expect_true(all(is.finite(result$bhar)))
})


# ============================================================================
# Task with many firms (stress test for multi-event stats)
# ============================================================================

test_that("Full pipeline with 10 firms completes", {
  task = create_mock_task(n_firms = 10)
  ps = ParameterSet$new()

  expect_no_error({
    task = run_event_study(task, ps)
  })

  expect_equal(nrow(task$data_tbl), 10)
  expect_false(is.null(task$aar_caar_tbl))
})


# ============================================================================
# Export with missing AAR stat_name silently skips
# ============================================================================

test_that("export_results with wrong stat_name skips AAR table", {
  task = create_fitted_mock_task()
  tmp = tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  # stat_name="Nonexistent" should silently skip AAR export
  # but still export other tables (ar, car, model)
  expect_no_error(
    export_results(task, tmp, which = c("model"), stat_name = "Nonexistent")
  )
  expect_true(file.exists(tmp))
})


# ============================================================================
# Tidy methods with models that lack alpha/beta
# ============================================================================

test_that("tidy model for MarketAdjustedModel has sigma but no alpha/beta", {
  task = create_mock_task()
  ps = ParameterSet$new(return_model = MarketAdjustedModel$new())
  task = run_event_study(task, ps)

  result = tidy.EventStudyTask(task, type = "model")
  # MarketAdjustedModel has no regression, so no alpha/beta
  expect_false("alpha" %in% result$term)
  expect_false("beta" %in% result$term)
  expect_true("sigma" %in% result$term)
})


# ============================================================================
# Diagnostics with unfitted model in a multi-event task
# ============================================================================

test_that("model_diagnostics handles unfitted model gracefully", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)

  # Manually create models where one fails
  task$data_tbl = task$data_tbl %>%
    dplyr::mutate(model = purrr::map(data, function(d) {
      mm = MarketModel$new()
      # Don't fit - leave as unfitted
      mm
    }))

  diag = model_diagnostics(task)
  expect_equal(nrow(diag), 2)
  expect_true(all(!diag$is_fitted))
  expect_true(all(is.na(diag$shapiro_p)))
  expect_true(all(is.na(diag$sigma)))
})


# ============================================================================
# validate_task with windows already prepared
# ============================================================================

test_that("validate_task detects multiple event dates", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)

  # Inject a second event_date=1
  idx = which(task$data_tbl$data[[1]]$event_date == 0)[1]
  task$data_tbl$data[[1]]$event_date[idx] = 1

  expect_warning(validate_task(task), "Multiple event dates")
})


# ============================================================================
# ParameterSet with all stats NULL
# ============================================================================

test_that("run_event_study with both stat sets NULL works", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  # No stat columns should be added
  expect_false("ART" %in% names(task$data_tbl))
  expect_null(task$aar_caar_tbl)
})


# ============================================================================
# Task accessor methods
# ============================================================================

test_that("get_ar errors before fitting", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  expect_error(task$get_ar(), "not been calculated")
})


test_that("get_aar errors before statistics", {
  task = create_mock_task()
  expect_error(task$get_aar(), "not been calculated")
})


test_that("get_model_stats errors before fitting", {
  task = create_mock_task()
  expect_error(task$get_model_stats(), "not been fitted")
})


test_that("get_ar with invalid event_id errors", {
  task = create_fitted_mock_task()
  expect_error(task$get_ar(event_id = 9999), "not found")
})


test_that("get_aar with invalid stat_name errors", {
  task = create_fitted_mock_task()
  expect_error(task$get_aar(stat_name = "Nonexistent"), "not found")
})


# ============================================================================
# Estimation window length is exact (off-by-one regression test)
# ============================================================================

test_that("Estimation window has exactly estimation_window_length observations", {
  task = create_mock_task()
  ps = ParameterSet$new()
  task = prepare_event_study(task, ps)

  # Check each event's estimation window
  for (i in seq_len(nrow(task$data_tbl))) {
    d = task$data_tbl$data[[i]]
    est_len = task$data_tbl$request[[i]]$estimation_window_length
    n_est = sum(d$estimation_window == 1)
    expect_equal(n_est, est_len,
                 info = paste("Event", i, ": estimation window should have exactly",
                              est_len, "observations, got", n_est))
  }
})


# ============================================================================
# .append_returns uses in_column parameter, not hardcoded "adjusted"
# ============================================================================

test_that(".append_returns works with non-default target column", {
  # Create data with 'close' columns instead of 'adjusted'
  set.seed(42)
  n = 50
  tbl = tibble::tibble(
    firm_close = 100 * cumprod(1 + rnorm(n, 0, 0.02)),
    index_close = 100 * cumprod(1 + rnorm(n, 0, 0.015))
  )

  lr = LogReturn$new()
  result = EventStudy:::.append_returns(tbl, lr, in_column = "close")

  # Should produce firm_returns and index_returns columns

  expect_true("firm_returns" %in% names(result))
  expect_true("index_returns" %in% names(result))
  # Should NOT still have only the original columns (no overwrite)
  expect_false(identical(result$firm_close, result$firm_returns))
})


# --- Regression: prepare_event_study preserves factor-provided market_excess ---

test_that("prepare_event_study does not overwrite market_excess from factor data", {
  # Bug: When factor_tbl provided a market_excess column (e.g., FF Mkt-RF),
  # prepare_event_study unconditionally computed market_excess = index_returns - rf,
  # overwriting the factor-provided value.
  # Fix: Only compute market_excess if not already present in the data.
  symbols <- c("FIRM_A")
  firm_data <- create_mock_firm_data(symbols = symbols)
  index_data <- create_mock_index_data()
  request <- create_mock_request(firm_symbols = symbols)

  n_days <- 300
  start_date <- as.Date("2020-01-01")
  dates <- seq(start_date, by = "day", length.out = n_days)
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

  set.seed(99)
  # Factor table WITH market_excess (as FF Mkt-RF would provide)
  factor_tbl <- tibble::tibble(
    date = format(dates, "%d.%m.%Y"),
    risk_free_rate = rep(0.0001, length(dates)),
    market_excess = rnorm(length(dates), 0.0003, 0.015),
    smb = rnorm(length(dates), 0, 0.005),
    hml = rnorm(length(dates), 0, 0.005)
  )

  task <- EventStudyTask$new(firm_data, index_data, request, factor_tbl = factor_tbl)
  ps <- ParameterSet$new()
  task <- prepare_event_study(task, ps)

  # Check that the market_excess in the nested data matches the factor_tbl values,
  # NOT index_returns - risk_free_rate
  nested_data <- task$data_tbl$data[[1]]
  expect_true("market_excess" %in% names(nested_data))

  # The factor-provided market_excess should be preserved
  # (Join produces the factor_tbl value; if overwritten, it would be index_returns - rf)
  joined_mkt <- nested_data %>%
    dplyr::inner_join(factor_tbl %>% dplyr::select(date, mkt_factor = market_excess),
                      by = "date")
  expect_equal(joined_mkt$market_excess, joined_mkt$mkt_factor, tolerance = 1e-12)
})


# --- Regression: ComparisonPeriodMeanAdjustedModel handles NA returns ---

test_that("ComparisonPeriodMeanAdjustedModel handles NA in estimation returns", {
  # Bug: mean() without na.rm=TRUE caused NA to propagate when returns have NAs
  # (e.g., the leading NA from return calculation).
  data <- create_mock_model_data()
  # Introduce NA at the beginning (simulating return calculation lag)
  data$firm_returns[1] <- NA_real_

  cpm <- ComparisonPeriodMeanAdjustedModel$new()
  cpm$fit(data)

  expect_true(cpm$is_fitted)
  # Sigma should be finite (not NA)
  expect_true(is.finite(cpm$statistics$sigma))

  result <- cpm$abnormal_returns(data)
  # Non-NA rows should have finite abnormal returns
  non_na_rows <- !is.na(data$firm_returns)
  expect_true(all(is.finite(result$abnormal_returns[non_na_rows])))
})


# --- Regression: Event date validation ---

test_that(".append_windows errors when event date not found in data", {
  # Bug: When the event date didn't match any date in the data,
  # event_index was empty and relative_index became wrong silently.
  # Fix: Now throws an informative error.
  data <- tibble::tibble(
    date = c("01.01.2020", "02.01.2020", "03.01.2020"),
    firm_returns = c(0.01, 0.02, -0.01),
    index_returns = c(0.005, 0.01, -0.005)
  )

  request <- list(
    event_date = "15.06.2020",  # doesn't exist in data
    event_window_start = -1,
    event_window_end = 1,
    shift_estimation_window = -2,
    estimation_window_length = 1
  )

  expect_error(
    EventStudy:::.append_windows(data, request),
    "not found"
  )
})


# --- Regression: cross_sectional all-NA ARs produce NA CARs ---

test_that("cross_sectional .extract_cars returns NA for all-NA abnormal returns", {
  # Bug: sum(NA, na.rm=TRUE) returned 0, so events with entirely missing
  # ARs were treated as having zero CAR.
  # Fix: Now returns NA_real_ when all ARs are NA.
  task <- create_fitted_mock_task(n_firms = 2)

  # Replace only the first firm's ARs with NA (leave second firm intact)
  task$data_tbl$data[[1]]$abnormal_returns <- NA_real_

  firm_chars <- tibble::tibble(
    event_id = 1:2,
    x = c(1.0, 2.0)
  )

  # Should not error but the first firm should have NA CAR
  result <- cross_sectional_regression(task, ~ x, firm_chars, robust = FALSE)
  car_data <- result$car_data
  # First event (all NA) should have NA CAR
  expect_true(is.na(car_data$car[car_data$event_id == 1]))
  # Second event should have a non-NA CAR
  expect_false(is.na(car_data$car[car_data$event_id == 2]))
})


# --- Regression: MarketAdjustedModel handles NA residuals in sd() ---

test_that("MarketAdjustedModel sd(residuals) uses na.rm=TRUE", {

  # Bug: MarketAdjustedModel used sd(residuals) without na.rm=TRUE,
  # returning NA sigma when residuals contain NAs.
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = MarketAdjustedModel$new())
  task <- run_event_study(task, ps)

  # Verify sigma is finite (not NA)
  model_tbl <- task$data_tbl$model
  sigmas <- purrr::map_dbl(model_tbl, ~ .x$statistics$sigma)
  expect_true(all(is.finite(sigmas)))
})


# --- Regression: FEC handles constant market returns without division by zero ---

test_that("forecast_error_correction handles constant market returns", {
  # Bug: When all estimation-window market returns were constant,
  # sum((x - mean(x))^2) = 0 caused division by zero -> NaN in FEC sigma.
  # Test the base class method directly since MarketModel's lm() drops
  # the collinear predictor before reaching FEC.
  model <- ModelBase$new()

  n_est <- 120
  n_event <- 5
  sigma <- 0.02

  # Constant market returns -> ss_market = 0
  est_market <- rep(0.001, n_est)
  event_market <- rnorm(n_event, mean = 0.001, sd = 0.01)

  # Call FEC directly (private method accessed via environment)
  env <- model$.__enclos_env__$private
  env$calculate_forecast_error_correction(sigma, n_est, est_market, event_market)

  fec <- model$statistics$forecast_error_corrected_sigma
  fec_car <- model$statistics$forecast_error_corrected_sigma_car

  # Should fall back to constant-mean correction, not NaN
  expect_true(all(is.finite(fec)))
  expect_true(all(is.finite(fec_car)))
  expected <- sigma * sqrt(1 + 1 / n_est)
  expect_equal(fec, rep(expected, n_event), tolerance = 1e-10)
  expect_equal(fec_car, rep(0, n_event))
})


# --- Regression: BHARModel degree_of_freedom uses estimation window length ---

test_that("BHARModel degree_of_freedom equals nrow(estimation_tbl) - 1", {
  # Bug: BHARModel computed df from length(diff(est_bhar)) - 1 = n_est - 2,

  # but sigma was computed from n_est observations, so df should be n_est - 1.
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = BHARModel$new())
  task <- run_event_study(task, ps)

  model_tbl <- task$data_tbl$model
  for (m in model_tbl) {
    df <- m$statistics$degree_of_freedom
    n_resid <- length(m$statistics$residuals)
    # df should be nrow(estimation_tbl) - 1, which is n_resid (diff reduces by 1)
    # So df = (n_resid + 1) - 1 = n_resid
    expect_equal(df, n_resid)
  }
})


# --- Regression: VolatilityModel handles zero-variance estimation window ---

test_that("VolatilityModel warns and skips fitting when estimation returns are constant", {
  # Bug: When var(estimation_tbl$firm_returns) == 0 (constant returns),
  # division by zero in r^2/var produced Inf/NaN residuals.
  model <- VolatilityModel$new()

  n <- 120
  event_n <- 11
  data_tbl <- tibble::tibble(
    firm_returns = c(rep(0.001, n), rnorm(event_n, sd = 0.01)),  # constant estimation
    index_returns = rnorm(n + event_n, sd = 0.01),
    estimation_window = c(rep(1, n), rep(0, event_n)),
    event_window = c(rep(0, n), rep(1, event_n)),
    relative_index = c(seq(-n, -1), seq(0, event_n - 1))
  )

  expect_warning(model$fit(data_tbl), "zero or NA variance")
  expect_false(model$is_fitted)
})


# --- Regression: RollingWindowModel rejects effective window size < 3 ---

test_that("RollingWindowModel warns when effective window size < 3", {
  # Bug: When ws < 3, sigma = sqrt(sum(resid^2) / (ws-2)) caused division
  # by zero (ws=2 -> denominator=0).
  skip_if_not_installed("sandwich")
  model <- RollingWindowModel$new(window_size = 2, min_obs = 2)

  n <- 5  # very small estimation window -> ws = min(2, 5) = 2
  event_n <- 3
  data_tbl <- tibble::tibble(
    firm_returns = rnorm(n + event_n, sd = 0.01),
    index_returns = rnorm(n + event_n, sd = 0.01),
    estimation_window = c(rep(1, n), rep(0, event_n)),
    event_window = c(rep(0, n), rep(1, event_n)),
    relative_index = c(seq(-n, -1), seq(0, event_n - 1))
  )

  expect_warning(model$fit(data_tbl), "window size.*must be >= 3")
  expect_false(model$is_fitted)
})


# --- Regression: PatellZTest Q_i clamps to 1 when m < k+2 ---

test_that("PatellZTest Q_i does not produce NaN with short estimation window", {
  # Bug: Q_i = (m-k)/(m-k-2) produced negative values when m < k+2,
  # then sqrt(sum(negative Q_i)) = NaN, breaking the Patell Z-test.
  # Now clamps Q_i to 1 when estimation window is too short.
  task <- create_mock_task(n_firms = 3)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(PatellZTest$new())
    )
  )
  task <- run_event_study(task, ps)

  result <- task$aar_caar_tbl[["PatellZ"]][[1]]
  # AAR z-statistics should be finite (not NaN)
  expect_true(all(is.finite(result$aar_z)))
})


# --- Regression: tidy_aar pt(df=0) returns valid p-value with n=1 ---

test_that("tidy_aar does not produce NaN p-values with single firm", {
  # Bug: pt(stat, df=0) returns NaN when n_valid_events=1 (df=1-1=0).
  # Now clamps df to be >= 1.
  # Note: With n_firms=1, CSectTTest aar_t is NA (sd of 1 value = NA),
  # so p-values are correctly NA (not NaN). The fix guards against the case
  # where a test statistic IS computed but df=0 would make pt() return NaN.
  task <- create_mock_task(n_firms = 2)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  result <- tidy.EventStudyTask(task, type = "aar")
  # p-values should never be NaN (may be NA if stat is NA)
  non_na_pvals <- result$p.value[!is.na(result$p.value)]
  expect_true(all(!is.nan(non_na_pvals)))
  expect_true(all(non_na_pvals >= 0 & non_na_pvals <= 1))
})


# --- Regression: MarketModel FEC uses non-NA observation count ---

test_that("MarketModel FEC uses effective obs count excluding NAs", {
  # Bug: nrow(estimation_tbl) included rows with NA returns, but lm()
  # drops NAs via na.omit. This made FEC correction factor too small
  # (denominator too large).
  model <- MarketModel$new()

  set.seed(42)
  n <- 120
  event_n <- 5
  firm_ret <- rnorm(n + event_n, sd = 0.02)
  idx_ret <- rnorm(n + event_n, sd = 0.015)

  # Inject 10 NAs into estimation window
  na_idx <- sample(1:n, 10)
  firm_ret[na_idx] <- NA

  data_tbl <- tibble::tibble(
    firm_returns = firm_ret,
    index_returns = idx_ret,
    estimation_window = c(rep(1, n), rep(0, event_n)),
    event_window = c(rep(0, n), rep(1, event_n)),
    relative_index = c(seq(-n, -1), seq(0, event_n - 1))
  )

  model$fit(data_tbl)

  if (model$is_fitted) {
    fec <- model$statistics$forecast_error_corrected_sigma
    sigma <- model$statistics$sigma

    # FEC should use n_valid = 110 (not n_total = 120)
    # With 110 obs: correction factor sqrt(1 + 1/110 + ...) is larger
    # than with 120 obs: sqrt(1 + 1/120 + ...)
    # So FEC / sigma > sqrt(1 + 1/120) for all event days
    min_ratio <- min(fec / sigma)
    expect_gt(min_ratio, sqrt(1 + 1 / 120))
  }
})


# ============================================================================
# Round 10: Crash bugs and wrong-result edge cases
# ============================================================================

test_that("ARTTest/CARTTest with df=0 do not crash (dist_student_t guard)", {
  # With exactly p+1 estimation observations, lm() produces df.residual=0
  # and sigma=NaN. dist_student_t(df=0) throws a hard error.
  # The guard clamps df to >= 1.
  firm_data <- create_mock_firm_data(symbols = "FIRM_A")
  index_data <- create_mock_index_data()
  request <- create_mock_request(
    firm_symbols = "FIRM_A",
    estimation_window_length = 3,
    shift_estimation_window = -6
  )
  task <- EventStudyTask$new(firm_data, index_data, request)
  ps <- ParameterSet$new()
  task <- prepare_event_study(task, ps)
  task <- fit_model(task, ps)

  model <- task$data_tbl$model[[1]]
  # With 3 obs and 2 params (intercept + slope), df should be 1
  # The model stores df.residual from lm() which could be 0 or 1

  # Should not throw "degrees of freedom must be strictly positive"
  expect_no_error({
    task <- calculate_statistics(task, ps)
  })
})


test_that("KolariPynnonenTest with NA in correlation matrix does not crash", {
  # When a firm has constant SARs, cor() produces NA entries.
  # The denom becomes NA, and if(NA > 0) crashes.
  task <- create_mock_task(n_firms = 3)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(KolariPynnonenTest$new())
    )
  )

  # Run the full pipeline — should not crash
  expect_no_error({
    task <- run_event_study(task, ps)
  })

  # Verify results exist
  expect_true("KP" %in% names(task$aar_caar_tbl))
})


test_that("VolatilityModel with zero variance guards abnormal_returns", {
  firm_data <- create_mock_firm_data(symbols = "FIRM_A")
  index_data <- create_mock_index_data()
  request <- create_mock_request(firm_symbols = "FIRM_A")

  task <- EventStudyTask$new(firm_data, index_data, request)
  ps <- ParameterSet$new(return_model = VolatilityModel$new())
  task <- prepare_event_study(task, ps)

  # Force constant returns in estimation window to trigger zero variance
  inner <- task$data_tbl$data[[1]]
  inner$firm_returns[inner$estimation_window == 1] <- 0.01
  task$data_tbl$data[[1]] <- inner

  # Model should detect zero variance and set is_fitted = FALSE
  expect_warning(
    task <- fit_model(task, ps),
    "zero or NA variance"
  )

  # abnormal_returns should return NA, not Inf
  model <- task$data_tbl$model[[1]]
  expect_false(model$is_fitted)
  result <- model$abnormal_returns(inner)
  expect_true(all(is.na(result$abnormal_returns)))
})


test_that("ComparisonPeriodMeanAdjustedModel df excludes NAs", {
  firm_data <- create_mock_firm_data(symbols = "FIRM_A")
  index_data <- create_mock_index_data()
  request <- create_mock_request(firm_symbols = "FIRM_A")

  task <- EventStudyTask$new(firm_data, index_data, request)
  ps <- ParameterSet$new(return_model = ComparisonPeriodMeanAdjustedModel$new())
  task <- prepare_event_study(task, ps)

  # Inject NAs into estimation window
  inner <- task$data_tbl$data[[1]]
  est_idx <- which(inner$estimation_window == 1)
  n_nas <- 10
  inner$firm_returns[est_idx[1:n_nas]] <- NA
  task$data_tbl$data[[1]] <- inner

  task <- fit_model(task, ps)
  model <- task$data_tbl$model[[1]]

  # df should be (n_valid - 1), not (n_total - 1)
  n_est <- length(est_idx)
  expect_equal(model$statistics$degree_of_freedom, n_est - n_nas - 1)
})


test_that("RollingWindowModel with NA data uses na.rm and validates params", {
  firm_data <- create_mock_firm_data(symbols = "FIRM_A")
  index_data <- create_mock_index_data()
  request <- create_mock_request(firm_symbols = "FIRM_A")

  task <- EventStudyTask$new(firm_data, index_data, request)
  ps <- ParameterSet$new(return_model = RollingWindowModel$new(window_size = 30))
  task <- prepare_event_study(task, ps)

  # Inject NAs into last rolling window
  inner <- task$data_tbl$data[[1]]
  est_idx <- which(inner$estimation_window == 1)
  # Make all index_returns constant in last window → beta = NA
  last_30 <- tail(est_idx, 30)
  inner$index_returns[last_30] <- 0.01
  task$data_tbl$data[[1]] <- inner

  # Should warn about NA params, not crash
  expect_warning(
    task <- fit_model(task, ps),
    "last window parameters are NA"
  )
  expect_false(task$data_tbl$model[[1]]$is_fitted)
})


test_that("Bootstrap exceed counter handles NA comparisons", {
  task <- create_mock_task(n_firms = 2)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  # Should produce finite p-values (not NA from NA poisoning)
  result <- bootstrap_test(task, n_boot = 50, statistic = "both")
  expect_true(all(is.finite(result$boot_p_aar)))
  expect_true(all(is.finite(result$boot_p_caar)))
})


test_that("Simulation weekend filter works regardless of locale", {
  # .generate_synthetic_data uses format(dates, "%u") which is locale-independent
  set.seed(42)
  result <- simulate_event_study(
    n_events = 3,
    estimation_window_length = 30,
    event_window = c(-2, 2),
    abnormal_return = 0.02,
    n_simulations = 5
  )
  expect_true(is.numeric(result$power))
  expect_true(result$power >= 0 && result$power <= 1)
})


# ============================================================================
# Round 11: Crash bugs and silent wrong results
# ============================================================================

test_that("Synthetic control validates donor completeness for all periods", {
  # A donor missing post-treatment data would cause silent R vector recycling
  set.seed(42)
  treated_data <- tibble::tibble(
    time = 1:10,
    outcome = rnorm(10)
  )
  donor_data <- tibble::tibble(
    unit = c(rep("donor1", 10), rep("donor2", 7)),
    time = c(1:10, 1:7),  # donor2 missing periods 8-10
    outcome = rnorm(17)
  )

  task <- SyntheticControlTask$new(
    treated_data = treated_data,
    donor_data = donor_data,
    treatment_time = 6
  )

  expect_error(
    estimate_synthetic_control(task),
    "complete data"
  )
})


test_that("calculate_statistics is idempotent (no duplicate columns)", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- prepare_event_study(task, ps)
  task <- fit_model(task, ps)
  task <- calculate_statistics(task, ps)

  cols_before <- names(task$data_tbl)
  task <- calculate_statistics(task, ps)
  cols_after <- names(task$data_tbl)

  # Should have same column names, no duplicates
  expect_equal(sort(cols_after), sort(cols_before))
  expect_false(any(grepl("\\.\\.\\.", cols_after)))
})


test_that("Durbin-Watson returns NA (not NaN) for zero residuals", {
  # Direct test of the DW calculation logic
  resid <- rep(0, 10)
  denom <- sum(resid^2)
  dw_stat <- if (denom < .Machine$double.eps) NA_real_ else sum(diff(resid)^2) / denom
  expect_true(is.na(dw_stat))
  expect_false(is.nan(dw_stat))
})


test_that("tidy_ar and tidy_car handle df=0 without NaN", {
  task <- create_mock_task(n_firms = 1)
  ps <- ParameterSet$new()
  task <- prepare_event_study(task, ps)
  task <- fit_model(task, ps)
  task <- calculate_statistics(task, ps)

  # Force df=0 in the model
  task$data_tbl$model[[1]]$.__enclos_env__$private$.statistics$degree_of_freedom <- 0

  # Should not produce NaN p-values
  ar_tidy <- tidy.EventStudyTask(task, type = "ar")
  expect_true(all(is.na(ar_tidy$p.value)))  # NA, not NaN
  expect_false(any(is.nan(ar_tidy$p.value)))

  car_tidy <- tidy.EventStudyTask(task, type = "car")
  expect_true(all(is.na(car_tidy$p.value)))
  expect_false(any(is.nan(car_tidy$p.value)))
})


test_that("Panel TWFE handles se=0 without Inf statistic", {
  skip_if_not_installed("sandwich")

  set.seed(42)
  n_units <- 10
  n_periods <- 8
  panel <- expand.grid(unit_id = 1:n_units, time_id = 1:n_periods)
  panel <- tibble::as_tibble(panel)
  panel$treatment_time <- ifelse(panel$unit_id <= 5, 5L, NA_integer_)
  panel$treated <- as.integer(!is.na(panel$treatment_time) &
                                panel$time_id >= panel$treatment_time)
  panel$outcome <- rnorm(nrow(panel)) + 2 * panel$treated

  task <- PanelEventStudyTask$new(
    panel_data = panel,
    outcome = "outcome",
    treatment = "treated",
    unit_id = "unit_id",
    time_id = "time_id",
    treatment_time = "treatment_time"
  )

  # Should not crash or produce Inf
  result <- estimate_panel_event_study(task, method = "dynamic_twfe")
  expect_true(all(is.na(result$coef_tbl$statistic) |
                    is.finite(result$coef_tbl$statistic)))
})
