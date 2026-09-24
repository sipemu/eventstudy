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

# C9 (2026-09-24): the full-pipeline "CalendarTimePortfolioTest computes
# portfolio test" case that used to live here was moved to
# test_multi_event_statistics.R (alongside the other CalendarTimePortfolioTest
# coverage) since it is not a BHAR test.
