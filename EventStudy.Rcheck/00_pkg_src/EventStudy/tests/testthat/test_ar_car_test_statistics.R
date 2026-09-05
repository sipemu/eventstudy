test_that("ARTTest computes correctly", {
  data = create_mock_model_data()
  mm = MarketModel$new()
  mm$fit(data)

  # Calculate abnormal returns first
  data = mm$abnormal_returns(data)

  art = ARTTest$new()
  result = art$compute(data, mm)

  expect_true("relative_index" %in% names(result))
  expect_true("abnormal_returns" %in% names(result))
  expect_true("ar_t" %in% names(result))
  expect_true("ar_t_dist" %in% names(result))

  # Only event window rows
  expect_equal(nrow(result), sum(data$event_window == 1))

  # ar_t = abnormal_returns / sigma
  sigma = mm$statistics$sigma
  expect_equal(result$ar_t, result$abnormal_returns / sigma)
})


test_that("CARTTest computes correctly", {
  data = create_mock_model_data()
  mm = MarketModel$new()
  mm$fit(data)
  data = mm$abnormal_returns(data)

  cart = CARTTest$new()
  result = cart$compute(data, mm)

  expect_true("car" %in% names(result))
  expect_true("car_t" %in% names(result))
  expect_true("car_window" %in% names(result))

  # CAR should be cumulative sum of AR
  event_ar = data$abnormal_returns[data$event_window == 1]
  expect_equal(result$car, cumsum(event_ar))

  # car_window should be formatted like "[-5, -5]", "[-5, -4]", etc.
  expect_true(all(grepl("\\[.*,.*\\]", result$car_window)))
})


test_that("TestStatisticBase initializes with defaults", {
  ts = TestStatisticBase$new()
  expect_equal(ts$confidence_level, 0.95)
  expect_equal(ts$confidence_type, "two-sided")
})


test_that("TestStatisticBase initializes with custom parameters", {
  ts = TestStatisticBase$new(confidence_level = 0.99, confidence_type = "less")
  expect_equal(ts$confidence_level, 0.99)
  expect_equal(ts$confidence_type, "less")
})


test_that("ARTTest name is correct", {
  art = ARTTest$new()
  expect_equal(art$name, "ART")
})


test_that("CARTTest name is correct", {
  cart = CARTTest$new()
  expect_equal(cart$name, "CART")
})


test_that("CARTTest car_t_dist scale grows with sqrt(event_window_length)", {
  data = create_mock_model_data()
  mm = MarketModel$new()
  mm$fit(data)
  data = mm$abnormal_returns(data)

  cart = CARTTest$new()
  result = cart$compute(data, mm)

  sigma = mm$statistics$sigma
  # The distribution's scale should be sqrt(L) * sigma at each row
  for (i in seq_len(nrow(result))) {
    L = result$event_window_length[i]
    dist_params = distributional::parameters(result$car_t_dist[i])
    expected_scale = sqrt(L) * sigma
    expect_equal(dist_params$sigma, expected_scale, tolerance = 1e-10,
                 info = paste("Day", i, ": dist scale should be sqrt(L)*sigma"))
  }
})


# --- STATS-01 regression tests: sigma==0 → NA (never Inf/NaN) ---

# Minimal stub model with a controllable sigma value.
make_sigma_stub_model <- function(sigma_val) {
  structure(
    list(
      statistics = list(
        sigma             = sigma_val,
        degree_of_freedom = 100,
        residuals         = NULL,
        forecast_error_corrected_sigma = rep(sigma_val, 11)
      )
    ),
    class = "stub_model"
  )
}

# Event-window data with known, non-degenerate abnormal returns.
make_event_data <- function(n_ev = 11) {
  set.seed(77)
  tibble::tibble(
    relative_index    = seq(0, n_ev - 1),
    firm_returns      = rnorm(n_ev, 0.001, 0.02),
    index_returns     = rnorm(n_ev, 0.0003, 0.015),
    abnormal_returns  = rnorm(n_ev, 0.001, 0.015),
    event_window      = 1L,
    estimation_window = 0L
  )
}


test_that("STATS-01: ARTTest returns NA (not Inf/NaN) when sigma == 0", {
  # Degenerate path: sigma exactly 0
  mod0 <- make_sigma_stub_model(0)
  d    <- make_event_data()
  result <- ARTTest$new()$compute(d, mod0)

  expect_true(all(is.na(result$ar_t)),
              "ar_t must be NA when sigma == 0")
  expect_false(any(is.infinite(result$ar_t)),
               "ar_t must not be Inf when sigma == 0")
  expect_false(any(is.nan(result$ar_t)),
               "ar_t must not be NaN when sigma == 0")
})


test_that("STATS-01: ARTTest returns finite t-stats on valid sigma (regression)", {
  # Valid path: normal fit must still yield finite ar_t
  data <- create_mock_model_data()
  mm   <- MarketModel$new()
  mm$fit(data)
  data <- mm$abnormal_returns(data)

  result <- ARTTest$new()$compute(data, mm)
  expect_true(all(is.finite(result$ar_t)),
              "ar_t must be finite on valid (non-degenerate) input")
})


test_that("STATS-01: CARTTest returns NA (not Inf/NaN) in car_t when sigma == 0", {
  mod0   <- make_sigma_stub_model(0)
  d      <- make_event_data()
  result <- CARTTest$new()$compute(d, mod0)

  expect_true(all(is.na(result$car_t)),
              "car_t must be NA when sigma == 0")
  expect_false(any(is.infinite(result$car_t)),
               "car_t must not be Inf when sigma == 0")
  expect_false(any(is.nan(result$car_t)),
               "car_t must not be NaN when sigma == 0")
})


test_that("STATS-01: CARTTest returns finite car_t on valid sigma (regression)", {
  data <- create_mock_model_data()
  mm   <- MarketModel$new()
  mm$fit(data)
  data <- mm$abnormal_returns(data)

  result <- CARTTest$new()$compute(data, mm)
  expect_true(all(is.finite(result$car_t)),
              "car_t must be finite on valid (non-degenerate) input")
})


test_that("STATS-01: BHARTTest returns NA (not Inf/NaN) in bhar_t when sigma == 0", {
  mod0   <- make_sigma_stub_model(0)
  d      <- make_event_data()
  result <- BHARTTest$new()$compute(d, mod0)

  expect_true(all(is.na(result$bhar_t)),
              "bhar_t must be NA when sigma == 0")
  expect_false(any(is.infinite(result$bhar_t)),
               "bhar_t must not be Inf when sigma == 0")
  expect_false(any(is.nan(result$bhar_t)),
               "bhar_t must not be NaN when sigma == 0")
})


test_that("STATS-01: BHARTTest returns finite bhar_t on valid sigma (regression)", {
  data       <- create_mock_model_data()
  bhar_model <- BHARModel$new()
  bhar_model$fit(data)
  data       <- bhar_model$abnormal_returns(data)

  result <- BHARTTest$new()$compute(data, bhar_model)
  expect_true(all(is.finite(result$bhar_t)),
              "bhar_t must be finite on valid (non-degenerate) input")
})


test_that("STATS-01: ARTTest returns NA when sigma is NA (not just zero)", {
  mod_na <- make_sigma_stub_model(NA_real_)
  d      <- make_event_data()
  result <- ARTTest$new()$compute(d, mod_na)

  expect_true(all(is.na(result$ar_t)),
              "ar_t must be NA when model sigma is NA")
})
