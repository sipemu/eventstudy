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
