# --- plot_event_study tests ---

test_that("plot_event_study returns ggplot for AR type", {
  task = create_fitted_mock_task()
  p = plot_event_study(task, type = "ar")
  expect_s3_class(p, "gg")
})


test_that("plot_event_study returns ggplot for CAR type", {
  task = create_fitted_mock_task()
  p = plot_event_study(task, type = "car")
  expect_s3_class(p, "gg")
})


test_that("plot_event_study returns ggplot for AAR type", {
  task = create_fitted_mock_task()
  p = plot_event_study(task, type = "aar")
  expect_s3_class(p, "gg")
})


test_that("plot_event_study returns ggplot for CAAR type", {
  task = create_fitted_mock_task()
  p = plot_event_study(task, type = "caar")
  expect_s3_class(p, "gg")
})


test_that("plot_event_study errors on invalid type", {
  task = create_fitted_mock_task()
  expect_error(plot_event_study(task, type = "invalid"), "must be one of")
})


test_that("plot_event_study errors on invalid event_id", {
  task = create_fitted_mock_task()
  expect_error(plot_event_study(task, type = "ar", event_id = 9999), "not found")
})


test_that("plot_event_study errors before fitting", {
  task = create_mock_task()
  # Unfitted task doesn't have model column; also lacks event_window before prepare
  expect_error(plot_event_study(task, type = "ar"))
})


test_that("plot_event_study errors before multi-event stats", {
  task = create_mock_task()
  ps = ParameterSet$new(multi_event_statistics = NULL)
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)
  expect_error(plot_event_study(task, type = "aar"), "Run calculate_statistics")
})


test_that("plot_event_study accepts custom title", {
  task = create_fitted_mock_task()
  p = plot_event_study(task, type = "car", title = "Custom Title")
  expect_s3_class(p, "gg")
  expect_equal(p$labels$title, "Custom Title")
})


# --- plot_diagnostics tests ---

test_that("plot_diagnostics runs without error", {
  task = create_fitted_mock_task()
  # grid.arrange returns a gtable, but also draws; just check no error
  expect_no_error(
    invisible(capture.output(plot_diagnostics(task)))
  )
})


test_that("plot_diagnostics errors before fitting", {
  task = create_mock_task()
  expect_error(plot_diagnostics(task), "not been fitted")
})


test_that("plot_diagnostics errors on invalid event_id", {
  task = create_fitted_mock_task()
  expect_error(plot_diagnostics(task, event_id = 9999), "not found")
})


# --- plot_stocks tests ---

test_that("plot_stocks runs without error", {
  task = create_mock_task()
  ps = ParameterSet$new(single_event_statistics = NULL, multi_event_statistics = NULL)
  task = prepare_event_study(task, ps)
  expect_no_error(plot_stocks(task))
})


test_that("plot_stocks respects max_symbols", {
  task = create_mock_task(n_firms = 4)
  ps = ParameterSet$new(single_event_statistics = NULL, multi_event_statistics = NULL)
  task = prepare_event_study(task, ps)
  # Should not error even with fewer than max_symbols
  expect_no_error(plot_stocks(task, max_symbols = 2, do_sample = FALSE))
})


test_that("plot_stocks with add_event_date", {
  task = create_mock_task()
  ps = ParameterSet$new(single_event_statistics = NULL, multi_event_statistics = NULL)
  task = prepare_event_study(task, ps)
  expect_no_error(plot_stocks(task, add_event_date = TRUE))
})
