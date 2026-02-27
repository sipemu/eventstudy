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

  expect_true("aar_t" %in% names(result))
  expect_true("caar_t" %in% names(result))
  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_true("car_window" %in% names(result))
  expect_equal(nrow(result), 11)
  expect_true(all(is.finite(result$aar_t)))
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
