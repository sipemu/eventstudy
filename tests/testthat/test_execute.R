test_that("fit_model works end-to-end", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  # Check that models are fitted
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  # Check that AR is calculated
  expect_true(all(purrr::map_lgl(task$data_tbl$data,
                                  ~"abnormal_returns" %in% names(.x))))
})


test_that("calculate_statistics works for single event stats", {
  task = create_mock_task()
  ps = ParameterSet$new(
    multi_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)
  task = calculate_statistics(task, ps)

  # Should have ART and CART columns
  expect_true("ART" %in% names(task$data_tbl))
  expect_true("CART" %in% names(task$data_tbl))
})


test_that("calculate_statistics works for multi event stats", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)
  task = calculate_statistics(task, ps)

  expect_false(is.null(task$aar_caar_tbl))
  expect_true("CSectT" %in% names(task$aar_caar_tbl))
})


test_that("run_event_study convenience wrapper works", {
  task = create_mock_task()
  ps = ParameterSet$new()
  task = run_event_study(task, ps)

  # Should have everything
  expect_true("model" %in% names(task$data_tbl))
  expect_true("ART" %in% names(task$data_tbl))
  expect_true("CART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
})


test_that("run_event_study uses default ParameterSet", {
  task = create_mock_task()
  # Should work without explicit parameter_set
  task = run_event_study(task)

  expect_true("model" %in% names(task$data_tbl))
})


test_that("est_task bug is fixed (uses task not est_task)", {
  # This test verifies the bug fix in calculate_statistics
  # where est_task was referenced instead of task
  task = create_mock_task()
  ps = ParameterSet$new()

  # If the bug were still present, this would error with
  # "object 'est_task' not found"
  expect_no_error({
    task = prepare_event_study(task, ps)
    task = fit_model(task, ps)
    task = calculate_statistics(task, ps)
  })
})


# --- End-to-end pipeline tests for all models (issue #3, gap #4) ---

test_that("MarketAdjustedModel works through full pipeline", {
  task = create_mock_task()
  ps = ParameterSet$new(return_model = MarketAdjustedModel$new())
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  expect_true("ART" %in% names(task$data_tbl))
  expect_true("CART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
  expect_false(is.null(task$data_tbl$model[[1]]$statistics$sigma))
  expect_false(is.null(task$data_tbl$model[[1]]$statistics$degree_of_freedom))
})


test_that("ComparisonPeriodMeanAdjustedModel works through full pipeline", {
  task = create_mock_task()
  ps = ParameterSet$new(return_model = ComparisonPeriodMeanAdjustedModel$new())
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  expect_true("ART" %in% names(task$data_tbl))
  expect_true("CART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
  expect_false(is.null(task$data_tbl$model[[1]]$statistics$sigma))
  expect_false(is.null(task$data_tbl$model[[1]]$statistics$degree_of_freedom))
})


test_that("BHARModel works through full pipeline", {
  task = create_mock_task()
  ps = ParameterSet$new(return_model = BHARModel$new())
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  expect_true("ART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
})


test_that("VolatilityModel works through full pipeline", {
  task = create_mock_task()
  ps = ParameterSet$new(return_model = VolatilityModel$new())
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  expect_true("ART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
})


test_that("FamaFrench3FactorModel works through full pipeline", {
  task = create_mock_task_with_factors()
  ps = ParameterSet$new(return_model = FamaFrench3FactorModel$new())
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  expect_true("ART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
})


test_that("FamaFrench5FactorModel works through full pipeline", {
  task = create_mock_task_with_factors()
  ps = ParameterSet$new(return_model = FamaFrench5FactorModel$new())
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  expect_true("ART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
})


test_that("Carhart4FactorModel works through full pipeline", {
  task = create_mock_task_with_factors()
  ps = ParameterSet$new(return_model = Carhart4FactorModel$new())
  task = run_event_study(task, ps)

  expect_true("model" %in% names(task$data_tbl))
  expect_true(all(purrr::map_lgl(task$data_tbl$model, ~.x$is_fitted)))
  expect_true("ART" %in% names(task$data_tbl))
  expect_false(is.null(task$aar_caar_tbl))
})


# ============================================================
# C8 (2026-09-24): numeric assertions against a direct computation for the
# key return models exercised through the full pipeline.
# ============================================================

test_that("C8: MarketModel (default) alpha/beta/AR match a direct lm() computation", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  d <- task$data_tbl$data[[1]]
  mdl <- task$data_tbl$model[[1]]

  est <- d[d$estimation_window == 1, ]
  fit <- lm(firm_returns ~ index_returns, data = est)
  expect_equal(unname(mdl$statistics$alpha), unname(coef(fit)[1]), tolerance = 1e-10)
  expect_equal(unname(mdl$statistics$beta), unname(coef(fit)[2]), tolerance = 1e-10)

  ev <- d[d$event_window == 1, ]
  expected_ar1 <- ev$firm_returns[1] -
    (mdl$statistics$alpha + mdl$statistics$beta * ev$index_returns[1])
  expect_equal(ev$abnormal_returns[1], expected_ar1, tolerance = 1e-10)
})


test_that("C8: MarketAdjustedModel AR equals firm_returns - index_returns exactly", {
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = MarketAdjustedModel$new())
  task <- run_event_study(task, ps)

  d <- task$data_tbl$data[[1]]
  ev <- d[d$event_window == 1, ]
  expect_equal(ev$abnormal_returns, ev$firm_returns - ev$index_returns, tolerance = 1e-12)
})


test_that("C8: ComparisonPeriodMeanAdjustedModel AR equals firm_returns - estimation mean exactly", {
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = ComparisonPeriodMeanAdjustedModel$new())
  task <- run_event_study(task, ps)

  d <- task$data_tbl$data[[1]]
  est_mean <- mean(d$firm_returns[d$estimation_window == 1], na.rm = TRUE)
  ev <- d[d$event_window == 1, ]
  expect_equal(ev$abnormal_returns, ev$firm_returns - est_mean, tolerance = 1e-12)
})


test_that("C8: FamaFrench3FactorModel AR matches a direct lm() computation on the factors", {
  task <- create_mock_task_with_factors()
  ps <- ParameterSet$new(return_model = FamaFrench3FactorModel$new())
  task <- run_event_study(task, ps)

  d <- task$data_tbl$data[[1]]
  est <- d[d$estimation_window == 1, ]
  fit <- lm(excess_return ~ market_excess + smb + hml, data = est)

  ev <- d[d$event_window == 1, ]
  predicted <- predict(fit, newdata = ev)
  expected_ar <- unname(ev$excess_return - predicted)
  expect_equal(unname(ev$abnormal_returns), expected_ar, tolerance = 1e-10)
})


test_that("C10: fit_model() collapses per-event short-window warnings into exactly ONE, listing event ids", {
  # A6/C10 (2026-09-24): N short-window events must produce ONE summary
  # warning (class eventstudy_short_estimation_window), not N.
  symbols <- paste0("FIRM_", LETTERS[1:4])
  firm_data <- create_mock_firm_data(symbols = symbols)
  index_data <- create_mock_index_data()
  request <- create_mock_request(firm_symbols = symbols,
                                  estimation_window_length = 20)
  task <- EventStudyTask$new(firm_data, index_data, request)
  ps <- ParameterSet$new()
  task <- prepare_event_study(task, ps)

  captured <- list()
  task2 <- withCallingHandlers(
    fit_model(task, ps),
    eventstudy_short_estimation_window = function(w) {
      captured[[length(captured) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_length(captured, 1L)
  msg <- captured[[1L]]$message
  expect_match(msg, "4 event\\(s\\)")
  for (id in task$data_tbl$event_id) {
    expect_match(msg, as.character(id), fixed = TRUE)
  }
  expect_true(all(purrr::map_lgl(task2$data_tbl$model, ~.x$is_fitted)))
})
