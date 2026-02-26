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

test_that("Forecast error correction with constant market returns in MarketAdjustedModel", {
  data = create_mock_model_data()
  # Constant index returns → sum((x - mean)^2) = 0 → division by zero
  data$index_returns = 0.001

  # Use MarketAdjustedModel which doesn't do lm() regression
  mam = MarketAdjustedModel$new()
  mam$fit(data)

  expect_true(mam$is_fitted)
  # Forecast error sigma will be Inf due to zero-variance market returns
  fec = mam$statistics$forecast_error_corrected_sigma
  expect_true(!is.null(fec))
  expect_true(all(is.infinite(fec) | is.nan(fec)))
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

test_that("SimpleReturn with zero price produces Inf", {
  sr = SimpleReturn$new()
  tbl = tibble::tibble(p = c(100, 0, 50))
  result = sr$calculate_return(tbl, "p", "r")
  # (0 - 100) / 0 is -Inf or NaN
  expect_true(is.infinite(result$r[2]) || is.nan(result$r[2]))
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

test_that("cross_sectional_regression with all-NA abnormal returns gives CAR=0", {
  task = create_fitted_mock_task(n_firms = 3)

  # Replace all abnormal returns with NA
  for (i in seq_len(nrow(task$data_tbl))) {
    task$data_tbl$data[[i]]$abnormal_returns = NA_real_
  }

  firm_chars = tibble::tibble(
    event_id = 1:2,
    x = c(1.0, 2.0)
  )

  # sum(NA, na.rm=TRUE) = 0, so CARs will be 0 (not NA)
  # This documents the current behavior
  result = cross_sectional_regression(task, ~ x, firm_chars, robust = FALSE)
  expect_true(all(result$car_data$car == 0))
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
  # ts_sd = sd(constant) = 0 → aar_t = value/0 = Inf
  expect_true(all(is.infinite(result$aar_t) | is.nan(result$aar_t)))
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
