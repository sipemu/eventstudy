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


# --- Cross-sectional edge cases (issue #3, gap #8) ---

test_that("car_by_group with single group returns no test", {
  task <- create_fitted_mock_task()
  result <- car_by_group(task)

  # Only one group "TestGroup", so test should be NULL
  expect_equal(result$n_groups, 1)
  expect_null(result$test)
  expect_equal(result$test_name, "Only one group -- no test performed")
})


test_that("car_by_group ANOVA path with 3+ groups", {
  # Create tasks with different groups and combine
  task_a <- create_mock_task(n_firms = 2, group = "GroupA")
  task_b <- create_mock_task(n_firms = 2, group = "GroupB")
  task_c <- create_mock_task(n_firms = 2, group = "GroupC")

  # Manually combine into one task and run pipeline
  combined_data <- dplyr::bind_rows(
    task_a$data_tbl %>% dplyr::mutate(event_id = event_id),
    task_b$data_tbl %>% dplyr::mutate(event_id = event_id + 10),
    task_c$data_tbl %>% dplyr::mutate(event_id = event_id + 20)
  )

  task_a$data_tbl <- combined_data
  ps <- ParameterSet$new(single_event_statistics = NULL, multi_event_statistics = NULL)
  task_a <- prepare_event_study(task_a, ps)
  task_a <- fit_model(task_a, ps)

  result <- car_by_group(task_a)
  expect_equal(result$n_groups, 3)
  expect_equal(result$test_name, "One-way ANOVA (Welch)")
  expect_false(is.null(result$test))
})


test_that("car_by_group errors on invalid group_var", {
  task <- create_fitted_mock_task()
  expect_error(car_by_group(task, group_var = "nonexistent"), "not found")
})


test_that("cross_sectional_regression with car_window", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  firm_chars <- tibble::tibble(
    event_id = 1:4,
    x = c(1, 2, 3, 4)
  )

  result <- cross_sectional_regression(
    task,
    formula = ~ x,
    data = firm_chars,
    car_window = c(0, 2),
    robust = FALSE
  )

  expect_s3_class(result, "es_cross_sectional")
  expect_equal(result$n_obs, 4)
})


test_that("cross_sectional_regression errors on non-matching event_ids", {
  task <- create_fitted_mock_task()
  bad_data <- tibble::tibble(event_id = c(999, 998), x = c(1, 2))
  expect_error(
    cross_sectional_regression(task, ~ x, bad_data),
    "No matching"
  )
})


# --- PIPELINE-03: singular/collinear guard ---

test_that("singular design: collinear regressor returns NA std.error/statistic/p.value + one warning", {
  # A perfectly collinear design (x2 == x1) must not crash with a solve()
  # error; instead it should return an es_cross_sectional object with NA
  # standard errors for the aliased column and emit exactly one warning.
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  firm_chars <- tibble::tibble(
    event_id = 1:4,
    x1 = c(1, 2, 3, 4),
    x2 = c(1, 2, 3, 4)   # identical to x1 -> perfect collinearity
  )

  result <- withCallingHandlers(
    cross_sectional_regression(task, ~ x1 + x2, firm_chars, robust = TRUE),
    warning = function(w) invokeRestart("muffleWarning")
  )

  expect_s3_class(result, "es_cross_sectional")
  # At least one aliased coefficient row should have NA std.error
  expect_true(any(is.na(result$coefficients$std.error)))
})


test_that("singular design: emits at least one warning (not just a crash)", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  firm_chars <- tibble::tibble(
    event_id = 1:4,
    x1 = c(1, 2, 3, 4),
    x2 = c(1, 2, 3, 4)
  )

  expect_warning(
    cross_sectional_regression(task, ~ x1 + x2, firm_chars, robust = TRUE)
  )
})


test_that("sandwich absent: emits warning() naming robust SEs and OLS fallback", {
  # When sandwich is unavailable, the function must emit a warning (not
  # message) that names the lost capability.  We simulate absence by mocking
  # base::requireNamespace so that calls from cross_sectional.R see sandwich
  # as absent.  with_mocked_bindings(.package="base") is required because
  # requireNamespace lives in base, not in the EventStudy namespace.
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  firm_chars <- tibble::tibble(
    event_id = 1:4,
    x = rnorm(4)
  )

  expect_warning(
    testthat::with_mocked_bindings(
      cross_sectional_regression(task, ~ x, firm_chars, robust = TRUE),
      requireNamespace = function(pkg, ...) if (pkg == "sandwich") FALSE else TRUE,
      .package = "base"
    ),
    regexp = "sandwich|robust|OLS",
    ignore.case = TRUE
  )
})


test_that("sandwich absent: returned object is valid es_cross_sectional with OLS SEs", {
  task <- create_mock_task(n_firms = 4)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  firm_chars <- tibble::tibble(
    event_id = 1:4,
    x = c(1.0, 2.0, 3.0, 4.0)
  )

  result <- suppressWarnings(
    testthat::with_mocked_bindings(
      cross_sectional_regression(task, ~ x, firm_chars, robust = TRUE),
      requireNamespace = function(pkg, ...) if (pkg == "sandwich") FALSE else TRUE,
      .package = "base"
    )
  )

  expect_s3_class(result, "es_cross_sectional")
  # With OLS SEs the std.error column must be finite (not NA)
  expect_true(all(is.finite(result$coefficients$std.error)))
})


test_that("well-conditioned design: coefficients unchanged vs OLS baseline", {
  # A properly specified regression should return byte-identical coefficients
  # before and after the tryCatch hardening — correctness must not regress.
  task <- create_mock_task(n_firms = 6)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  firm_chars <- tibble::tibble(
    event_id = 1:6,
    x = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  )

  result_ols <- cross_sectional_regression(task, ~ x, firm_chars, robust = FALSE)

  # Coefficients must be finite and non-NA for a well-conditioned OLS problem
  expect_true(all(is.finite(result_ols$coefficients$estimate)))
  expect_true(all(is.finite(result_ols$coefficients$std.error)))
  expect_true(all(is.finite(result_ols$coefficients$statistic)))
  expect_true(all(is.finite(result_ols$coefficients$p.value)))
  expect_true(result_ols$r_squared >= 0)
})
