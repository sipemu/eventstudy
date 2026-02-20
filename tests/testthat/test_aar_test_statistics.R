test_that("CSectTTest computes AAR and CAAR correctly", {
  # Create multi-event data
  set.seed(42)
  n_events = 3
  event_window_size = 11  # -5 to +5

  data = do.call(rbind, lapply(1:n_events, function(i) {
    tibble(
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


test_that("PatellZTest does not call browser() (bug fix)", {
  # If browser() were still present, this test would hang
  # We verify by checking the compute method source
  compute_body = body(PatellZTest$new()$compute)
  compute_str = deparse(compute_body)
  expect_false(any(grepl("browser\\(\\)", compute_str)))
})
