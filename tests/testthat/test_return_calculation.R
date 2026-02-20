test_that("SimpleReturn calculates correctly", {
  simple_return = SimpleReturn$new()
  tbl = tibble(x = c(0.1, 0.102, 0.104, 0.102, 0.101))

  tbl = simple_return$calculate_return(tbl, in_column = "x", out_column = "pct")
  expect_true("pct" %in% names(tbl))
  expect_true(is.na(tbl$pct[1]))
  expect_equal(length(tbl$pct), 5)
  # Check approximate values
  mean_diff = mean(tbl$pct - c(NA, 0.0196, 0.0192, -0.0196, -0.00990), na.rm = TRUE)
  expect_lt(abs(mean_diff), 1e-3)
})


test_that("LogReturn calculates correctly", {
  log_return = LogReturn$new()
  tbl = tibble(x = c(0.1, 0.102, 0.104, 0.102, 0.101))

  tbl = log_return$calculate_return(tbl, in_column = "x", out_column = "pct")
  expect_true("pct" %in% names(tbl))
  expect_true(is.na(tbl$pct[1]))
  # Log return should be close to simple return for small values
  expect_lt(abs(tbl$pct[2] - log(0.102 / 0.1)), 1e-10)
})


test_that("ReturnCalculation base class exists", {
  rc = ReturnCalculation$new()
  expect_true(inherits(rc, "ReturnCalculation"))
  expect_equal(rc$name, "")
})
