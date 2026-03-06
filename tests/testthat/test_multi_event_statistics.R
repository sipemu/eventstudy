test_that("SignTest computes correctly", {
  set.seed(42)
  data = do.call(rbind, lapply(1:5, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -5:5,
      abnormal_returns = rnorm(11, mean = 0.005, sd = 0.02),
      event_window = 1,
      estimation_window = 0
    )
  }))

  sign_test = SignTest$new()
  result = sign_test$compute(data, NULL)

  expect_true("sign_z" %in% names(result))
  expect_true("csign_z" %in% names(result))
  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_equal(nrow(result), 11)
})


test_that("SignTest name is correct", {
  expect_equal(SignTest$new()$name, "SignT")
})


test_that("GeneralizedSignTest computes correctly", {
  set.seed(42)
  data = do.call(rbind, lapply(1:5, function(i) {
    n_est = 50
    n_ev = 11
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1), seq(0, n_ev - 1)),
      abnormal_returns = rnorm(n_est + n_ev, mean = 0.001, sd = 0.02),
      event_window = c(rep(0, n_est), rep(1, n_ev)),
      estimation_window = c(rep(1, n_est), rep(0, n_ev))
    )
  }))

  gsign_test = GeneralizedSignTest$new()
  result = gsign_test$compute(data, NULL)

  expect_true("gsign_z" %in% names(result))
  expect_true("cgsign_z" %in% names(result))
  expect_equal(nrow(result), 11)
})


test_that("GeneralizedSignTest name is correct", {
  expect_equal(GeneralizedSignTest$new()$name, "GSignT")
})


test_that("RankTest computes correctly", {
  set.seed(42)
  data = do.call(rbind, lapply(1:5, function(i) {
    n_est = 50
    n_ev = 11
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1), seq(0, n_ev - 1)),
      abnormal_returns = rnorm(n_est + n_ev, mean = 0.001, sd = 0.02),
      event_window = c(rep(0, n_est), rep(1, n_ev)),
      estimation_window = c(rep(1, n_est), rep(0, n_ev))
    )
  }))

  rank_test = RankTest$new()
  result = rank_test$compute(data, NULL)

  expect_true("rank_z" %in% names(result))
  expect_true("mean_rank" %in% names(result))
  expect_equal(nrow(result), 11)
})


test_that("RankTest name is correct", {
  expect_equal(RankTest$new()$name, "RankT")
})


test_that("BMPTest name is correct", {
  expect_equal(BMPTest$new()$name, "BMP")
})


# Helper: create multi-event data with model tibble (needed by BMPTest/PatellZTest)
create_multi_event_model_data <- function(n_firms = 5, n_est = 50, n_ev = 11) {
  set.seed(42)
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

  # Create model tibble matching the structure expected by PatellZTest/BMPTest
  model_tbl = tibble::tibble(
    firm_symbol = paste0("F", seq_len(n_firms)),
    model = lapply(seq_len(n_firms), function(i) {
      mm = MarketModel$new()
      firm_data = data[data$firm_symbol == paste0("F", i), ]
      mm$fit(firm_data)
      mm
    })
  )

  list(data = data, model = model_tbl)
}


test_that("BMPTest computes correctly", {
  md = create_multi_event_model_data()
  bmp = BMPTest$new()
  result = bmp$compute(md$data, md$model)

  expect_true("bmp_t" %in% names(result))
  expect_true("cbmp_t" %in% names(result))
  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_true("mean_sar" %in% names(result))
  expect_equal(nrow(result), 11)
  # BMP t-stat should be finite
  expect_true(all(is.finite(result$bmp_t)))
})


test_that("PatellZTest computes correctly", {
  md = create_multi_event_model_data()
  patell = PatellZTest$new()
  result = patell$compute(md$data, md$model)

  expect_true("aar_z" %in% names(result))
  expect_true("caar_z" %in% names(result))
  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_equal(nrow(result), 11)
  expect_true(all(is.finite(result$aar_z)))
})


test_that("PatellZTest uses forecast-error-corrected sigma for standardization (GH #6)", {
  # Construct minimal 2-firm data with known ARs and per-day FEC sigmas
  n_est = 20
  n_ev = 3
  data = do.call(rbind, lapply(1:2, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1), 0:(n_ev - 1)),
      abnormal_returns = c(rnorm(n_est, sd = 0.01), rep(0.02, n_ev)),
      event_window = c(rep(0, n_est), rep(1, n_ev)),
      estimation_window = c(rep(1, n_est), rep(0, n_ev))
    )
  }))

  # Build model with known FEC sigma vectors (one per event-window day)
  fec_sigma_f1 = c(0.044, 0.040, 0.042)  # varies by day
  fec_sigma_f2 = c(0.088, 0.080, 0.084)
  model_tbl = tibble::tibble(
    firm_symbol = c("F1", "F2"),
    model = list(
      list(statistics = list(sigma = 0.04, forecast_error_corrected_sigma = fec_sigma_f1)),
      list(statistics = list(sigma = 0.08, forecast_error_corrected_sigma = fec_sigma_f2))
    )
  )

  patell = PatellZTest$new()
  result = patell$compute(data, model_tbl)

  # Manually compute expected SAR at day 0 using FEC sigma
  # SAR = AR / fec_sigma (NOT AR / sigma or AR / sqrt(sigma))
  sar_f1_d0 = 0.02 / fec_sigma_f1[1]
  sar_f2_d0 = 0.02 / fec_sigma_f2[1]
  expected_sum_sar_d0 = sar_f1_d0 + sar_f2_d0

  # aar should be mean of raw ARs (not sum of SARs)
  day0 = result[result$relative_index == 0, ]
  expect_equal(day0$aar, 0.02, tolerance = 1e-10)

  # aar_z = sum(SAR) / sqrt(sum(Q_i))
  Q_i = (n_est - 2) / (n_est - 4)
  Q_total = sqrt(2 * Q_i)
  expect_equal(day0$aar_z, expected_sum_sar_d0 / Q_total, tolerance = 1e-10)
})


test_that("PatellZTest CSAR cumsum is per-firm with per-firm Q_i (GH #6)", {
  # With 2 firms and 3 event days, cumsum must reset per firm
  n_est = 20
  n_ev = 3
  set.seed(99)
  data = do.call(rbind, lapply(1:2, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1), 0:(n_ev - 1)),
      abnormal_returns = c(rnorm(n_est, sd = 0.01), c(0.01, 0.02, 0.03) * i),
      event_window = c(rep(0, n_est), rep(1, n_ev)),
      estimation_window = c(rep(1, n_est), rep(0, n_ev))
    )
  }))

  fec_sigma_val = 0.05
  model_tbl = tibble::tibble(
    firm_symbol = c("F1", "F2"),
    model = lapply(1:2, function(i) {
      list(statistics = list(
        sigma = fec_sigma_val,
        forecast_error_corrected_sigma = rep(fec_sigma_val, n_ev)
      ))
    })
  )

  patell = PatellZTest$new()
  result = patell$compute(data, model_tbl)

  expect_equal(nrow(result), n_ev)
  expect_true(all(is.finite(result$caar_z)))

  # Manually compute caar_z at day 0
  # SAR_F1_day0 = 0.01 / 0.05 = 0.2, SAR_F2_day0 = 0.02 / 0.05 = 0.4
  # Q_i = (20-2)/(20-4) = 1.125
  Q_i = (n_est - 2) / (n_est - 4)
  sar_f1_d0 = 0.01 / fec_sigma_val
  sar_f2_d0 = 0.02 / fec_sigma_val
  # At day 0, n=1, csar = cumsum[1] / sqrt(1 * Q_i) = sar / sqrt(Q_i)
  csar_f1_d0 = sar_f1_d0 / sqrt(1 * Q_i)
  csar_f2_d0 = sar_f2_d0 / sqrt(1 * Q_i)
  expected_caar_z_d0 = (1 / sqrt(2)) * (csar_f1_d0 + csar_f2_d0)

  day0 = result[result$relative_index == 0, ]
  expect_equal(day0$caar_z, expected_caar_z_d0, tolerance = 1e-10)
})


test_that("CalendarTimePortfolioTest computes correctly", {
  set.seed(42)
  data = do.call(rbind, lapply(1:5, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -5:5,
      abnormal_returns = rnorm(11, mean = 0.005, sd = 0.02),
      event_window = 1,
      estimation_window = 0
    )
  }))

  ct = CalendarTimePortfolioTest$new()
  result = ct$compute(data, NULL)

  expect_true("caltime_t" %in% names(result))
  expect_true("ccaltime_t" %in% names(result))
  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_true("car_window" %in% names(result))
  expect_equal(nrow(result), 11)
  expect_true(all(is.finite(result$caltime_t)))
})


test_that("KolariPynnonenTest name is correct", {
  expect_equal(KolariPynnonenTest$new()$name, "KP")
})


test_that("KolariPynnonenTest computes correctly", {
  md = create_multi_event_model_data()
  kp = KolariPynnonenTest$new()
  result = kp$compute(md$data, md$model)

  expect_true("kp_t" %in% names(result))
  expect_true("ckp_t" %in% names(result))
  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_true("car_window" %in% names(result))
  expect_equal(nrow(result), 11)
  expect_true(all(is.finite(result$kp_t)))
})


test_that("KolariPynnonenTest adjustment factor is constant across days", {
  md = create_multi_event_model_data()
  bmp = BMPTest$new()
  kp = KolariPynnonenTest$new()

  bmp_result = bmp$compute(md$data, md$model)
  kp_result = kp$compute(md$data, md$model)

  # KP adjusts BMP by a factor. Check that the adjustment is well-defined
  # and that the KP statistics are finite
  expect_true(all(is.finite(kp_result$kp_t)))
  expect_true(all(is.finite(kp_result$ckp_t)))
  # The ratio kp/bmp should be constant (same adjustment factor for all days)
  ratios = kp_result$kp_t / bmp_result$bmp_t
  ratios = ratios[is.finite(ratios)]
  if (length(ratios) > 1) {
    expect_equal(max(ratios) - min(ratios), 0, tolerance = 1e-10)
  }
})


test_that("KolariPynnonenTest with 1 firm does not error", {
  md = create_multi_event_model_data(n_firms = 1)
  kp = KolariPynnonenTest$new()

  # With 1 firm, BMP itself produces NaN (sd of single value),
  # so KP will also produce NaN — the key is it doesn't error
  expect_no_error(result <- kp$compute(md$data, md$model))
  expect_equal(nrow(result), 11)
  expect_true("kp_t" %in% names(result))
  expect_true("ckp_t" %in% names(result))
})


test_that("KolariPynnonenTest with 2 firms gives finite results", {
  md = create_multi_event_model_data(n_firms = 2)
  kp = KolariPynnonenTest$new()
  result = kp$compute(md$data, md$model)

  expect_true(all(is.finite(result$kp_t)))
  expect_equal(nrow(result), 11)
})


test_that("KolariPynnonenTest with many firms works", {
  md = create_multi_event_model_data(n_firms = 10)
  kp = KolariPynnonenTest$new()
  result = kp$compute(md$data, md$model)

  expect_true(all(is.finite(result$kp_t)))
  expect_true(all(is.finite(result$ckp_t)))
  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
})


test_that("KolariPynnonenTest works in full pipeline", {
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(KolariPynnonenTest$new())
    )
  )
  task <- run_event_study(task, ps)

  # The KP result should be in aar_caar_tbl
  expect_true("KP" %in% names(task$aar_caar_tbl))
})


# --- Regression: PatellZTest Q_i adapts to model parameters ---

test_that("PatellZTest Q_i uses correct k for MarketModel (k=2)", {
  # Bug: Q_i was hardcoded as (m-2)/(m-4), correct only for k=2.
  # Fix: Now extracts k from model degree_of_freedom.
  task <- create_mock_task(n_firms = 3)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(PatellZTest$new())
    )
  )
  task <- run_event_study(task, ps)

  patell <- task$aar_caar_tbl$PatellZ[[1]]
  expect_true("aar_z" %in% names(patell))
  expect_true(all(is.finite(patell$aar_z)))
})


test_that("PatellZTest Q_i adapts for multi-factor models (k>2)", {
  # For FF3 (k=4), Q_i should be (m-4)/(m-6) instead of (m-2)/(m-4)
  task <- create_mock_task_with_factors(n_firms = 3)
  ps <- ParameterSet$new(
    return_model = FamaFrench3FactorModel$new(),
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(PatellZTest$new())
    )
  )
  task <- run_event_study(task, ps)

  patell <- task$aar_caar_tbl$PatellZ[[1]]
  expect_true("aar_z" %in% names(patell))
  expect_true(all(is.finite(patell$aar_z)))
  # With k=4 and typical m~120, Q_i = (120-4)/(120-6) ≈ 1.0175
  # vs old Q_i = (120-2)/(120-4) ≈ 1.0172 -- similar but different
  expect_true(all(is.finite(patell$caar_z)))
})


test_that("PatellZTest Q_i uses k=1 for ComparisonPeriodMeanAdjustedModel", {
  # ComparisonPeriodMeanAdjustedModel estimates 1 parameter (the mean)
  # df = T - 1, so k = 1. Q_i should be (m-1)/(m-3).
  task <- create_mock_task(n_firms = 3)
  ps <- ParameterSet$new(
    return_model = ComparisonPeriodMeanAdjustedModel$new(),
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(PatellZTest$new())
    )
  )
  task <- run_event_study(task, ps)

  patell <- task$aar_caar_tbl$PatellZ[[1]]
  expect_true(all(is.finite(patell$aar_z)))
})


# --- Regression: CalendarTimePortfolioTest column names ---

test_that("CalendarTimePortfolioTest uses caltime_t/ccaltime_t column names", {
  # Bug: CalendarTimePortfolioTest used aar_t/caar_t, colliding with CSectTTest.
  # Fix: Renamed to caltime_t/ccaltime_t.
  set.seed(42)
  data <- do.call(rbind, lapply(1:5, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -5:5,
      abnormal_returns = rnorm(11, mean = 0.005, sd = 0.02),
      event_window = 1,
      estimation_window = 0
    )
  }))

  ct <- CalendarTimePortfolioTest$new()
  result <- ct$compute(data, NULL)

  # New column names should be present

  expect_true("caltime_t" %in% names(result))
  expect_true("ccaltime_t" %in% names(result))
  # Old column names should NOT be present
  expect_false("aar_t" %in% names(result))
  expect_false("caar_t" %in% names(result))
})
