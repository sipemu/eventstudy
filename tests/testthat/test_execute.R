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
