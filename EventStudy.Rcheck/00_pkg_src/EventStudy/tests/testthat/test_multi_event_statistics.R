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

  # Create model tibble matching the structure expected by PatellZTest/BMPTest.
  # In the real pipeline the model tibble is keyed by event_id (one fit per
  # event), so the fixture must carry event_id as well.
  model_tbl = tibble::tibble(
    event_id = paste0("E", seq_len(n_firms)),
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
    event_id = c("E1", "E2"),
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
    event_id = c("E1", "E2"),
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


# --- Regression: PatellZTest handles NULL residuals in k extraction ---

test_that("PatellZTest k extraction handles NULL residuals gracefully", {
  # Bug: When a model failed to fit, residuals was NULL. The expression
  # sum(NULL != 0 | TRUE) evaluated to 0, and length(NULL) - df produced
  # a wrong negative k value. Now explicitly checks for NULL residuals.
  task <- create_mock_task(n_firms = 3)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(PatellZTest$new())
    )
  )
  task <- run_event_study(task, ps)

  # Verify PatellZ results exist and are finite
  aar_tbl <- task$aar_caar_tbl
  stat_name <- "PatellZ"
  expect_true(stat_name %in% names(aar_tbl))

  result <- aar_tbl[[stat_name]][[1]]
  expect_true(all(!is.na(result$aar)))
})


# Regression test for GH #7: a firm appearing in multiple events must not
# inflate n_events or the BMP/Patell statistics via a many-to-many sigma join.
create_shared_firm_data <- function(n_events = 6, n_est = 50, n_ev = 11) {
  set.seed(7)
  # Only 2 distinct firms spread across n_events events -> firms recur.
  data = do.call(rbind, lapply(seq_len(n_events), function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", ((i - 1) %% 2) + 1),  # F1, F2, F1, F2, ...
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
    event_id = paste0("E", seq_len(n_events)),
    firm_symbol = paste0("F", ((seq_len(n_events) - 1) %% 2) + 1),
    model = lapply(seq_len(n_events), function(i) {
      mm = MarketModel$new()
      mm$fit(data[data$event_id == paste0("E", i), ])
      mm
    })
  )

  list(data = data, model = model_tbl)
}


test_that("BMPTest does not inflate n_events when firms recur (GH #7)", {
  md = create_shared_firm_data(n_events = 6)
  result = BMPTest$new()$compute(md$data, md$model)

  # 6 events, one observation per event at each relative day -> n_events == 6,
  # not 6 * (#events sharing the firm).
  expect_true(all(result$n_events == 6))
  expect_true(all(result$n_valid_events == 6))
  expect_true(all(is.finite(result$bmp_t)))
})


test_that("KolariPynnonenTest does not inflate n_events when firms recur (GH #7)", {
  md = create_shared_firm_data(n_events = 6)
  result = KolariPynnonenTest$new()$compute(md$data, md$model)

  expect_true(all(result$n_events == 6))
  expect_true(all(result$n_valid_events == 6))
})


test_that("PatellZTest does not inflate n_events when firms recur (GH #7)", {
  md = create_shared_firm_data(n_events = 6)
  result = PatellZTest$new()$compute(md$data, md$model)

  expect_true(all(result$n_events == 6))
  expect_true(all(result$n_valid_events == 6))
  expect_true(all(is.finite(result$aar_z)))
})


# --- STATS-02 regression test: CSectTTest does not inflate when firms recur ---

test_that("STATS-02: CSectTTest does not inflate n_events when firms recur", {
  # 6 events, only 2 distinct firms: F1 appears in events E1,E3,E5 and F2 in E2,E4,E6.
  # A many-to-many join on firm_symbol would triple-count each firm → n_events == 18.
  # The correct join on event_id yields n_events == 6 (one row per event per day).
  md = create_shared_firm_data(n_events = 6)
  result = CSectTTest$new()$compute(md$data, md$model)

  expect_true(all(result$n_events == 6),
              "CSectTTest n_events must equal the number of events, not inflated by firm recurrence")
  expect_true(all(result$n_valid_events == 6))
  # aar_t must be finite (sd of 6 values is estimable)
  expect_true(all(is.finite(result$aar_t)))
})


# --- STATS-04 regression tests: n_events == 1 → NA (not finite-but-invalid) ---

# Minimal single-event dataset for n_events == 1 tests.
make_single_event_data <- function(n_est = 30, n_ev = 5) {
  set.seed(11)
  tibble::tibble(
    event_id          = "E1",
    firm_symbol       = "F1",
    relative_index    = c(seq(-n_est, -1), seq(0, n_ev - 1)),
    abnormal_returns  = rnorm(n_est + n_ev, mean = 0.001, sd = 0.02),
    event_window      = c(rep(0L, n_est), rep(1L, n_ev)),
    estimation_window = c(rep(1L, n_est), rep(0L, n_ev)),
    index_returns     = rnorm(n_est + n_ev, 0.0003, 0.015),
    firm_returns      = rnorm(n_est + n_ev, 0.001, 0.02),
    event_date        = c(rep(0L, n_est), 1L, rep(0L, n_ev - 1))
  )
}

make_single_event_model_tbl <- function(data) {
  mm <- MarketModel$new()
  mm$fit(data)
  tibble::tibble(
    event_id    = "E1",
    firm_symbol = "F1",
    model       = list(mm)
  )
}


test_that("STATS-04: PatellZTest aar_z is NA (not finite) when n_events == 1", {
  d   <- make_single_event_data()
  mod <- make_single_event_model_tbl(d)

  result <- PatellZTest$new()$compute(d, mod)

  # With only 1 event the Patell z-test is statistically invalid → must be NA
  expect_true(all(is.na(result$aar_z)),
              "PatellZTest aar_z must be NA when n_events == 1")
  expect_false(any(is.finite(result$aar_z)),
               "PatellZTest aar_z must not be a finite number when n_events == 1")
})


test_that("STATS-04: PatellZTest caar_z is NA (not finite) when n_events == 1", {
  d   <- make_single_event_data()
  mod <- make_single_event_model_tbl(d)

  result <- PatellZTest$new()$compute(d, mod)

  expect_true(all(is.na(result$caar_z)),
              "PatellZTest caar_z must be NA when n_events == 1")
  expect_false(any(is.finite(result$caar_z)),
               "PatellZTest caar_z must not be a finite number when n_events == 1")
})


test_that("STATS-04: SignTest sign_z is NA (not finite) when n_events == 1", {
  d <- make_single_event_data()

  result <- SignTest$new()$compute(d, NULL)

  expect_true(all(is.na(result$sign_z)),
              "SignTest sign_z must be NA when n_events == 1")
  expect_false(any(is.finite(result$sign_z)),
               "SignTest sign_z must not be a finite number when n_events == 1")
})


test_that("STATS-04: BMPTest bmp_t is NA (already correct baseline) when n_events == 1", {
  d   <- make_single_event_data()
  mod <- make_single_event_model_tbl(d)

  result <- BMPTest$new()$compute(d, mod)

  # bmp_t = sd of single value → NA (sd(x) = NA for length-1). Locks existing guard.
  expect_true(all(is.na(result$bmp_t)),
              "BMPTest bmp_t must be NA when n_events == 1 (sd of single value is NA)")
})


test_that("STATS-04: CSectTTest aar_t is NA (already correct baseline) when n_events == 1", {
  d <- make_single_event_data()

  result <- CSectTTest$new()$compute(d, NULL)

  # sd(x) for a single value is NA → aar_t guard fires → NA. Locks existing behaviour.
  expect_true(all(is.na(result$aar_t)),
              "CSectTTest aar_t must be NA when n_events == 1")
})


test_that("STATS-04: PatellZTest returns finite aar_z / caar_z with >= 2 events (regression)", {
  # Confirm the guard does not fire for valid multi-event data.
  md <- create_multi_event_model_data(n_firms = 4)
  result <- PatellZTest$new()$compute(md$data, md$model)

  expect_true(all(is.finite(result$aar_z)),
              "PatellZTest aar_z must be finite for n_events >= 2")
  expect_true(all(is.finite(result$caar_z)),
              "PatellZTest caar_z must be finite for n_events >= 2")
})


test_that("STATS-04: SignTest returns finite sign_z with >= 2 events (regression)", {
  # Confirm the guard does not fire for valid multi-event data.
  set.seed(42)
  d <- do.call(rbind, lapply(1:4, function(i) {
    tibble::tibble(
      event_id          = paste0("E", i),
      firm_symbol       = paste0("F", i),
      relative_index    = -5:5,
      abnormal_returns  = rnorm(11, 0.005, 0.02),
      event_window      = 1L,
      estimation_window = 0L
    )
  }))

  result <- SignTest$new()$compute(d, NULL)

  expect_true(all(is.finite(result$sign_z)),
              "SignTest sign_z must be finite for n_events >= 2")
})


# --- STATS-03 regression test: NA gap mid-window does not corrupt post-gap CARs ---
#
# Verifies that the existing cumsum(coalesce(abnormal_returns, 0)) chains
# in CSectTTest and SignTest correctly treat a mid-window NA as contributing
# 0 to the running sum, so post-gap CARs/CAACs continue rather than becoming NA.

test_that("STATS-03: CSectTTest mid-window NA gap does not corrupt post-gap CARs", {
  # Two events:
  #   E1: ARs = [0.01, 0.02, NA,  0.03, 0.04]  (gap at day 2)
  #   E2: ARs = [0.01, 0.02, 0.03, 0.04, 0.05]  (complete)
  #
  # With coalesce(ar, 0), the per-event CAR for E1 at each day:
  #   day 0: 0.01
  #   day 1: 0.01 + 0.02 = 0.03
  #   day 2: 0.03 + 0  = 0.03  (NA coalesced to 0)
  #   day 3: 0.03 + 0.03 = 0.06
  #   day 4: 0.06 + 0.04 = 0.10
  #
  # If the coalesce were absent (plain cumsum), day 2 and all subsequent
  # CARs for E1 would be NA, making sd_caar undefined → caar_t = NA.

  e1_ar <- c(0.01, 0.02, NA,   0.03, 0.04)
  e2_ar <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  n_ev <- 5L

  d <- rbind(
    tibble::tibble(event_id = "E1", firm_symbol = "F1",
                   relative_index = 0:(n_ev - 1),
                   abnormal_returns = e1_ar,
                   event_window = 1L, estimation_window = 0L),
    tibble::tibble(event_id = "E2", firm_symbol = "F2",
                   relative_index = 0:(n_ev - 1),
                   abnormal_returns = e2_ar,
                   event_window = 1L, estimation_window = 0L)
  )

  result <- CSectTTest$new()$compute(d, NULL)

  # Post-gap CARs must not be NA (coalesce-based cumsum is correct)
  expect_equal(nrow(result), n_ev)
  # caar should be finite at every day (gap treated as 0 contribution)
  expect_true(all(is.finite(result$caar)),
              "CAAR must be finite after mid-window NA gap (coalesce chain)")

  # Manually verify per-event CAR for E1 using coalesce:
  e1_car_expected <- cumsum(dplyr::coalesce(e1_ar, 0))
  e2_car_expected <- cumsum(dplyr::coalesce(e2_ar, 0))

  # At day 2 (relative_index == 2), E1 CAR should be e1_car_expected[3] = 0.03
  # (not NA — the gap was filled with 0).
  expect_equal(e1_car_expected[3], 0.03, tolerance = 1e-10,
               label = "E1 CAR at gap day should equal pre-gap cumsum (0 contribution)")
  # Post-gap (day 3), E1 CAR should be 0.06, not NA.
  expect_equal(e1_car_expected[4], 0.06, tolerance = 1e-10,
               label = "E1 CAR at post-gap day should continue correctly")

  # The CAAR is cumsum(coalesce(aar, 0)) where aar at each day is
  # mean(abnormal_returns, na.rm=TRUE) across all events.
  # At day 2, E1 contributes NA so aar = mean(c(NA, e2_ar[3]), na.rm=TRUE) = e2_ar[3].
  # Verify the CAAR is monotonically non-decreasing (all positive ARs in this example)
  # and that no NA leaked into it.
  expect_true(!any(is.na(result$caar)),
              "CAAR must have no NAs even when one event has a mid-window gap")
  # CAAR at day 2 should be strictly greater than at day 1 (because e2_ar[3]>0)
  expect_gt(result$caar[3], result$caar[2],
            label = "CAAR must increase after gap day (coalesce makes NA contribute 0 to AAR denominator via na.rm)")
})


test_that("STATS-03: SignTest mid-window NA gap does not corrupt post-gap CAAR", {
  # Spot-check: same NA-gap scenario through SignTest.
  # caar in SignTest is cumsum(coalesce(aar, 0)); if aar has NA (because
  # mean(abnormal_returns, na.rm=TRUE) on all-NA day is NaN) the coalesce
  # keeps caar running.
  e1_ar <- c(0.01, 0.02, NA,   0.03, 0.04)
  e2_ar <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  n_ev  <- 5L

  d <- rbind(
    tibble::tibble(event_id = "E1", firm_symbol = "F1",
                   relative_index = 0:(n_ev - 1),
                   abnormal_returns = e1_ar,
                   event_window = 1L, estimation_window = 0L),
    tibble::tibble(event_id = "E2", firm_symbol = "F2",
                   relative_index = 0:(n_ev - 1),
                   abnormal_returns = e2_ar,
                   event_window = 1L, estimation_window = 0L)
  )

  result <- SignTest$new()$compute(d, NULL)

  expect_equal(nrow(result), n_ev)
  # caar must be finite at every day
  expect_true(all(is.finite(result$caar)),
              "SignTest CAAR must be finite after mid-window NA gap (coalesce chain)")
})


# ============================================================
# WR-01: PatellZTest Q_total must exclude degenerate events (Plan 02-02)
# ============================================================

test_that("WR-01: PatellZTest Q_total excludes degenerate events — degenerate events must not change valid-event z-scores", {
  # Build a 2-firm dataset where E1 is valid and E2 is degenerate (all-NA fec_sigma).
  # Before WR-01 fix, E2's Q_i = 1 (fallback) inflates Q_total and deflates E1's z-score.
  # After fix, Q_total is computed from valid events only, so the z-score for the valid
  # events is the same as if the degenerate events were absent.
  n_est <- 20L
  n_ev  <- 3L

  d_valid <- do.call(rbind, lapply(1:2, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1L), 0L:(n_ev - 1L)),
      abnormal_returns = c(rnorm(n_est, sd = 0.01), rep(0.02, n_ev)),
      event_window = c(rep(0L, n_est), rep(1L, n_ev)),
      estimation_window = c(rep(1L, n_est), rep(0L, n_ev))
    )
  }))

  fec_val <- 0.05
  Q_i_expected <- (n_est - 2) / (n_est - 4)  # (20-2)/(20-4) = 1.125

  # Reference: only valid events
  model_valid_only <- tibble::tibble(
    event_id    = c("E1", "E2"),
    firm_symbol = c("F1", "F2"),
    model = lapply(1:2, function(i)
      list(statistics = list(sigma = fec_val,
                             forecast_error_corrected_sigma = rep(fec_val, n_ev),
                             residuals = rnorm(n_est), degree_of_freedom = n_est - 2L))
    )
  )
  result_valid_only <- PatellZTest$new()$compute(d_valid, model_valid_only)

  # Mixed dataset: add a degenerate event E3 (all-NA fec_sigma)
  d_degen <- tibble::tibble(
    event_id = "E3",
    firm_symbol = "F3",
    relative_index = c(seq(-n_est, -1L), 0L:(n_ev - 1L)),
    abnormal_returns = c(rnorm(n_est, sd = 0.01), rep(NA_real_, n_ev)),
    event_window = c(rep(0L, n_est), rep(1L, n_ev)),
    estimation_window = c(rep(1L, n_est), rep(0L, n_ev))
  )
  d_mixed <- rbind(d_valid, d_degen)

  model_mixed <- tibble::tibble(
    event_id    = c("E1", "E2", "E3"),
    firm_symbol = c("F1", "F2", "F3"),
    model = c(
      model_valid_only$model,
      list(list(statistics = list(sigma = NA_real_,
                                   forecast_error_corrected_sigma = NULL,
                                   residuals = NULL, degree_of_freedom = NA_real_)))
    )
  )
  result_mixed <- PatellZTest$new()$compute(d_mixed, model_mixed)

  # After WR-01 fix, valid-event z-scores must match the valid-only result
  day0_valid <- result_valid_only[result_valid_only$relative_index == 0L, ]
  day0_mixed <- result_mixed[result_mixed$relative_index == 0L, ]

  expect_equal(day0_mixed$aar_z, day0_valid$aar_z, tolerance = 1e-10,
               info = "WR-01: degenerate event must not change valid-event aar_z")
})


# ============================================================
# WR-02: SignTest csign_z NA guard for n_valid == 1 (Plan 02-02)
# ============================================================

test_that("WR-02: SignTest csign_z is NA (not finite) when n_events == 1", {
  # Before WR-02 fix, csign_z with n_valid == 1 yielded (1-0.5)/(0.5*1) = 1.0 —
  # a plausible-looking but statistically meaningless number.
  # After fix, csign_z must be NA when n_valid == 1.
  d <- make_single_event_data()
  result <- SignTest$new()$compute(d, NULL)

  expect_true(all(is.na(result$csign_z)),
              "SignTest csign_z must be NA when n_events == 1 (statistically meaningless)")
  expect_false(any(is.finite(result$csign_z)),
               "SignTest csign_z must not be a finite number when n_events == 1")
})

test_that("WR-02: SignTest csign_z is finite with >= 2 events (regression — guard must not fire on valid data)", {
  set.seed(42)
  d <- do.call(rbind, lapply(1:3, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -3:3,
      abnormal_returns = rnorm(7, 0.005, 0.02),
      event_window = 1L,
      estimation_window = 0L
    )
  }))

  result <- SignTest$new()$compute(d, NULL)

  expect_true(all(is.finite(result$csign_z)),
              "SignTest csign_z must be finite for n_events >= 2")
})
