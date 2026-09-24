test_that("CSectTTest computes AAR and CAAR correctly", {
  # Create multi-event data
  set.seed(42)
  n_events = 3
  event_window_size = 11  # -5 to +5

  data = do.call(rbind, lapply(1:n_events, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -5:5,
      abnormal_returns = rnorm(event_window_size, mean = 0.001, sd = 0.02),
      event_window = 1,
      estimation_window = 0
    )
  }))

  csect = CSectTTest$new()
  result = csect$compute(data, NULL)

  expect_true("aar" %in% names(result))
  expect_true("aar_t" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_true("caar_t" %in% names(result))
  expect_true("n_events" %in% names(result))
  expect_true("car_window" %in% names(result))

  expect_equal(nrow(result), event_window_size)
  expect_equal(result$n_events[1], n_events)

  # CAAR should be cumsum of AAR
  expect_equal(result$caar, cumsum(result$aar))
})


test_that("CSectTTest name is correct", {
  csect = CSectTTest$new()
  expect_equal(csect$name, "CSectT")
})


test_that("PatellZTest name is PatellZ (bug fix)", {
  patell = PatellZTest$new()
  expect_equal(patell$name, "PatellZ")
})


test_that("PatellZTest$compute() runs without interaction and matches a hand computation (C9, 2026-09-24)", {
  # Behavioural replacement for the old deparse()+grepl("browser()") source
  # check: run compute() on a small fixture (which would HANG if browser()
  # were reintroduced, since testthat runs non-interactively) and assert the
  # resulting aar_z values against an independent hand computation.
  n_est <- 20L
  n_ev  <- 3L
  set.seed(261201)
  data <- do.call(rbind, lapply(1:3, function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = c(seq(-n_est, -1L), 0L:(n_ev - 1L)),
      abnormal_returns = c(rnorm(n_est, sd = 0.01), c(0.01, 0.02, 0.03) * i),
      event_window = c(rep(0L, n_est), rep(1L, n_ev)),
      estimation_window = c(rep(1L, n_est), rep(0L, n_ev))
    )
  }))

  fec_val <- 0.05
  model_tbl <- tibble::tibble(
    event_id = c("E1", "E2", "E3"),
    firm_symbol = c("F1", "F2", "F3"),
    model = lapply(1:3, function(i) {
      list(statistics = list(sigma = fec_val,
                             forecast_error_corrected_sigma = rep(fec_val, n_ev),
                             n_params = 2))
    })
  )

  result <- PatellZTest$new()$compute(data, model_tbl)
  expect_true(all(is.finite(result$aar_z)))

  # Hand computation: k=2, m=20 -> Q_i = (20-2)/(20-4) = 1.125 for every event.
  Q_i <- (n_est - 2) / (n_est - 4)
  Q_total <- sqrt(3 * Q_i)
  ev <- data[data$event_window == 1, ]
  sar <- ev$abnormal_returns / fec_val
  expected_aar_z <- vapply(0:(n_ev - 1), function(ri) {
    sum(sar[ev$relative_index == ri]) / Q_total
  }, numeric(1))

  expect_equal(result$aar_z[order(result$relative_index)], expected_aar_z, tolerance = 1e-10)
})
