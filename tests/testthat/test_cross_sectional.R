test_that("cross_sectional_regression fits OLS on CARs", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  # Create firm characteristics
  firm_chars <- tibble::tibble(
    event_id = 1:4,
    log_market_cap = c(10, 11, 12, 10.5),
    leverage = c(0.3, 0.5, 0.2, 0.4)
  )

  result <- cross_sectional_regression(
    task,
    formula = ~ log_market_cap + leverage,
    data = firm_chars,
    robust = FALSE
  )

  expect_s3_class(result, "es_cross_sectional")
  expect_true("coefficients" %in% names(result))
  expect_true("r_squared" %in% names(result))
  expect_equal(result$n_obs, 4)
  expect_true(nrow(result$coefficients) == 3)  # intercept + 2 vars
})

test_that("cross_sectional_regression errors on missing event_id", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  bad_data <- tibble::tibble(x = 1:2)
  expect_error(
    cross_sectional_regression(task, ~ x, bad_data),
    "event_id"
  )
})

test_that("cross_sectional_regression errors on unfitted task", {
  task <- create_mock_task()
  data <- tibble::tibble(event_id = 1:2, x = c(1, 2))
  expect_error(
    cross_sectional_regression(task, ~ x, data),
    "not computed"
  )
})

test_that("car_by_group returns summary and test", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  result <- car_by_group(task)

  expect_true("summary" %in% names(result))
  expect_true("test_name" %in% names(result))
  expect_true("mean_car" %in% names(result$summary))
})

test_that("car_quantiles returns quantile vector", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  result <- car_quantiles(task)
  expect_equal(length(result), 5)
  expect_true(all(!is.na(result)))
  # Quantiles should be monotonically increasing
  expect_true(all(diff(result) >= 0))
})

test_that("car_quantiles respects car_window", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  q_full <- car_quantiles(task)
  q_narrow <- car_quantiles(task, car_window = c(0, 2))

  # Narrow window should generally produce smaller CARs
  expect_equal(length(q_narrow), 5)
})

test_that("plot_car_distribution returns ggplot", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  p <- plot_car_distribution(task)
  expect_s3_class(p, "gg")
})

test_that("plot_car_distribution with by_group", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  p <- plot_car_distribution(task, by_group = TRUE)
  expect_s3_class(p, "gg")
})

test_that("print.es_cross_sectional works", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  firm_chars <- tibble::tibble(
    event_id = 1:4,
    x = rnorm(4)
  )

  result <- cross_sectional_regression(task, ~ x, firm_chars, robust = FALSE)
  expect_output(print(result), "Cross-Sectional Regression")
})
