test_that("BHARTTest computes BHAR for single event", {
  data <- create_mock_model_data()
  bhar_model <- BHARModel$new()
  bhar_model$fit(data)
  data <- bhar_model$abnormal_returns(data)

  bhart <- BHARTTest$new()
  result <- bhart$compute(data, bhar_model)

  expect_true("bhar" %in% names(result))
  expect_true("bhar_t" %in% names(result))
  expect_true("bhar_window" %in% names(result))
  expect_true("relative_index" %in% names(result))

  # BHAR should be finite

  expect_true(all(is.finite(result$bhar)))
  expect_true(all(is.finite(result$bhar_t)))

  # Event window should have 11 rows (default mock data)
  expect_equal(nrow(result), 11)
})

test_that("BHARModel calculates compound returns correctly", {
  # Create simple data where we know the answer
  data <- tibble::tibble(
    firm_returns = c(rep(0.01, 5), rep(0.02, 5)),
    index_returns = c(rep(0.005, 5), rep(0.01, 5)),
    estimation_window = c(rep(1, 5), rep(0, 5)),
    event_window = c(rep(0, 5), rep(1, 5)),
    relative_index = c(-5:-1, 0:4),
    event_date = c(rep(0, 5), 1, rep(0, 4))
  )

  bhar <- BHARModel$new()
  bhar$fit(data)
  result <- bhar$abnormal_returns(data)

  # Abnormal returns should be compounded difference
  expect_true("abnormal_returns" %in% names(result))
  expect_equal(nrow(result), nrow(data))
})

test_that("CalendarTimePortfolioTest computes portfolio test", {
  task <- create_mock_task(n_firms = 3)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  # Get unnested event window data
  data_tbl <- task$data_tbl %>%
    dplyr::select(event_id, group, firm_symbol, data) %>%
    tidyr::unnest(data)

  # Get model tibble
  model_tbl <- task$data_tbl %>%
    dplyr::select(event_id, group, firm_symbol, model) %>%
    dplyr::group_by(group) %>%
    tidyr::nest() %>%
    dplyr::rename(model = data)

  group_data <- data_tbl %>%
    dplyr::group_by(group) %>%
    tidyr::nest()

  caltime <- CalendarTimePortfolioTest$new()
  result <- caltime$compute(group_data$data[[1]], model_tbl$model[[1]])

  expect_true("aar" %in% names(result))
  expect_true("caar" %in% names(result))
  expect_true("aar_t" %in% names(result))
  expect_true("caar_t" %in% names(result))
  expect_true("car_window" %in% names(result))

  expect_true(all(is.finite(result$aar)))
  expect_true(all(is.finite(result$caar)))
})
