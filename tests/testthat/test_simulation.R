test_that("simulate_event_study runs with small params", {
  skip_on_cran()
  result <- simulate_event_study(
    n_events = 3,
    event_window = c(-2, 2),
    estimation_window_length = 50,
    abnormal_return = 0,
    n_simulations = 5,
    seed = 42
  )

  expect_s3_class(result, "es_simulation")
  expect_true(result$power >= 0 && result$power <= 1)
  expect_equal(nrow(result$rejection_by_day), 5)  # -2 to 2
  expect_equal(length(result$test_stats), 5)
})


test_that("simulate_event_study power in [0,1]", {
  skip_on_cran()
  result <- simulate_event_study(
    n_events = 3,
    event_window = c(-2, 2),
    estimation_window_length = 50,
    abnormal_return = 0.05,
    n_simulations = 5,
    seed = 42
  )

  expect_true(all(result$rejection_by_day$rejection_rate >= 0))
  expect_true(all(result$rejection_by_day$rejection_rate <= 1))
})


test_that("simulate_event_study seed reproducibility", {
  skip_on_cran()
  r1 <- simulate_event_study(n_events = 3, n_simulations = 3,
                              event_window = c(-2, 2),
                              estimation_window_length = 50,
                              seed = 123)
  r2 <- simulate_event_study(n_events = 3, n_simulations = 3,
                              event_window = c(-2, 2),
                              estimation_window_length = 50,
                              seed = 123)
  expect_equal(r1$test_stats, r2$test_stats)
  expect_equal(r1$power, r2$power)
})


test_that("print.es_simulation works", {
  skip_on_cran()
  result <- simulate_event_study(n_events = 3, n_simulations = 3,
                                  event_window = c(-2, 2),
                                  estimation_window_length = 50,
                                  seed = 42)
  expect_output(print(result), "Event Study Simulation")
  expect_output(print(result), "Power")
})


test_that("simulate_event_study with custom DGP params", {
  skip_on_cran()
  result <- simulate_event_study(
    n_events = 3,
    event_window = c(-2, 2),
    estimation_window_length = 50,
    abnormal_return = 0,
    n_simulations = 3,
    dgp_params = list(alpha = 0.001, beta = 1.5, sigma_firm = 0.02,
                       sigma_market = 0.02),
    seed = 42
  )

  expect_s3_class(result, "es_simulation")
  expect_equal(result$params$dgp_params$beta, 1.5)
  expect_equal(result$params$dgp_params$sigma_firm, 0.02)
})


test_that("simulate_event_study with PatellZ test statistic", {
  skip_on_cran()
  result <- simulate_event_study(
    n_events = 3,
    event_window = c(-2, 2),
    estimation_window_length = 50,
    n_simulations = 3,
    test_statistic = "PatellZ",
    seed = 42
  )

  expect_s3_class(result, "es_simulation")
  expect_equal(result$params$test_statistic, "PatellZ")
  expect_true(all(result$rejection_by_day$rejection_rate >= 0))
})


test_that("simulate_event_study with BMP test statistic", {
  skip_on_cran()
  result <- simulate_event_study(
    n_events = 3,
    event_window = c(-2, 2),
    estimation_window_length = 50,
    n_simulations = 3,
    test_statistic = "BMP",
    seed = 42
  )

  expect_s3_class(result, "es_simulation")
  expect_equal(result$params$test_statistic, "BMP")
})


test_that("simulate_event_study with SignT test statistic", {
  skip_on_cran()
  result <- simulate_event_study(
    n_events = 5,
    event_window = c(-2, 2),
    estimation_window_length = 50,
    n_simulations = 3,
    test_statistic = "SignT",
    seed = 42
  )

  expect_s3_class(result, "es_simulation")
  expect_true(all(result$rejection_by_day$rejection_rate >= 0))
})


test_that("simulate_event_study params stored correctly", {
  skip_on_cran()
  result <- simulate_event_study(
    n_events = 5,
    event_window = c(-3, 3),
    estimation_window_length = 80,
    abnormal_return = 0.02,
    alpha = 0.10,
    n_simulations = 3,
    seed = 42
  )

  expect_equal(result$params$n_events, 5)
  expect_equal(result$params$event_window, c(-3, 3))
  expect_equal(result$params$estimation_window_length, 80)
  expect_equal(result$params$abnormal_return, 0.02)
  expect_equal(result$params$alpha, 0.10)
  expect_equal(result$params$n_simulations, 3)
  expect_equal(nrow(result$rejection_by_day), 7)  # -3 to 3
})


test_that("simulate_event_study with different seeds gives different results", {
  skip_on_cran()
  r1 <- simulate_event_study(n_events = 3, n_simulations = 5,
                              event_window = c(-2, 2),
                              estimation_window_length = 50,
                              seed = 1)
  r2 <- simulate_event_study(n_events = 3, n_simulations = 5,
                              event_window = c(-2, 2),
                              estimation_window_length = 50,
                              seed = 999)
  expect_false(identical(r1$test_stats, r2$test_stats))
})


test_that("simulate_event_study with unknown test_statistic produces NA results", {
  skip_on_cran()
  # The tryCatch in the simulation loop catches the error, resulting in NA stats
  result <- simulate_event_study(
    n_events = 2, n_simulations = 2,
    event_window = c(-1, 1),
    estimation_window_length = 30,
    test_statistic = "NonExistent",
    seed = 42
  )

  expect_true(all(is.na(result$test_stats)))
})


test_that("simulation price injection propagates to post-event days", {
  skip_on_cran()
  # After injecting abnormal_return at event day, the price level shift should
  # persist for all subsequent days (no reversal in returns)
  syn <- EventStudy:::.generate_synthetic_data(
    n_events = 1,
    estimation_window_length = 50,
    event_window = c(-2, 2),
    dgp = list(alpha = 0, beta = 1.0, sigma_firm = 0.01, sigma_market = 0.015),
    abnormal_return = 0.10,
    n_days_total = 100
  )

  # Get raw firm prices from the generated data
  firm_data <- syn$task$data_tbl$data[[1]]
  prices <- firm_data$firm_adjusted

  # Find event date from the request
  event_date <- syn$task$data_tbl$request[[1]]$event_date
  event_idx <- which(firm_data$date == event_date)

  if (length(event_idx) > 0 && event_idx > 1 && event_idx < length(prices)) {
    # Return on event day should include the 0.10 injection
    event_ret <- prices[event_idx] / prices[event_idx - 1] - 1
    # Return on day after should be normal (not a -0.10 reversal)
    post_ret <- prices[event_idx + 1] / prices[event_idx] - 1
    # Event day return should be larger than typical (~0.01 sd)
    expect_true(abs(event_ret) > 0.05,
                info = "Event day should show injected abnormal return")
    # Post-event return should be normal sized, not a large reversal
    expect_true(abs(post_ret) < 0.05,
                info = "Post-event return should not show spurious reversal")
  }
})


test_that("simulation failed sims excluded from rejection rate", {
  skip_on_cran()
  # Use unknown test_statistic to force all sims to fail
  result <- simulate_event_study(
    n_events = 2, n_simulations = 3,
    event_window = c(-1, 1),
    estimation_window_length = 30,
    test_statistic = "NonExistent",
    seed = 42
  )

  # All sims failed -> rejection rates should be NaN (0/0), not 0
  expect_true(all(is.nan(result$rejection_by_day$rejection_rate)))
})
