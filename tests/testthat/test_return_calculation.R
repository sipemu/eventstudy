test_that("SimpleReturn calculates correctly", {
  simple_return = SimpleReturn$new()
  tbl = tibble::tibble(x = c(0.1, 0.102, 0.104, 0.102, 0.101))

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
  tbl = tibble::tibble(x = c(0.1, 0.102, 0.104, 0.102, 0.101))

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


# --- Return calculation edge cases (issue #3, gap #11) ---

test_that("SimpleReturn with custom column names", {
  sr = SimpleReturn$new()
  tbl = tibble::tibble(my_price = c(100, 102, 104, 102))
  tbl = sr$calculate_return(tbl, in_column = "my_price", out_column = "my_ret")
  expect_true("my_ret" %in% names(tbl))
  expect_true(is.na(tbl$my_ret[1]))
  expect_equal(length(tbl$my_ret), 4)
})


test_that("LogReturn with custom column names", {
  lr = LogReturn$new()
  tbl = tibble::tibble(price = c(50, 55, 52, 53))
  tbl = lr$calculate_return(tbl, in_column = "price", out_column = "log_ret")
  expect_true("log_ret" %in% names(tbl))
  expect_true(is.na(tbl$log_ret[1]))
  expect_lt(abs(tbl$log_ret[2] - log(55 / 50)), 1e-10)
})


test_that("SimpleReturn name is correct", {
  expect_equal(SimpleReturn$new()$name, "simple return")
})


test_that("LogReturn name is correct", {
  expect_equal(LogReturn$new()$name, "log return")
})


test_that("LogReturn and SimpleReturn agree for small returns", {
  sr = SimpleReturn$new()
  lr = LogReturn$new()
  tbl = tibble::tibble(p = c(100, 100.01, 100.02, 100.03))
  simple = sr$calculate_return(tbl, "p", "r")$r
  logg = lr$calculate_return(tbl, "p", "r")$r
  # For very small returns, log and simple should be close
  expect_lt(max(abs(simple[-1] - logg[-1])), 1e-4)
})
