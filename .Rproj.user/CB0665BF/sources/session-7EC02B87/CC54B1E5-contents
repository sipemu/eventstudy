testthat::context("Return Calculation")


testthat::test_that("Simple Return", {
  simple_return = SimpleReturn$new()
  tbl = tibble(x=c(.1, .102, .104, .102, .101))

  tbl = simple_return$calculate_return(tbl, in_column = "x", out_column = "pct")
  mean_diff = mean(tbl$pct - c(NA, 0.0196, 0.0192, -0.0196, -0.00990), na.rm=T)
  testthat::expect_lt(mean_diff, 1e-5)
})


testthat::test_that("Log Return", {
  log_return = LogReturn$new()
  tbl = tibble(x=c(.1, .102, .104, .102, .101))

  tbl = log_return$calculate_return(tbl, in_column = "x", out_column = "pct")
  mean_diff = mean(tbl$pct - c(NA, 0.0198, 0.0194, -0.0194, -0.00985), na.rm=T)
  testthat::expect_lt(mean_diff, 1e-5)
})


testthat::test_that("Log Return", {
  log_return = LogReturn$new()
  tbl = tibble(x=c(.1, .102, .104, .102, .101))

  tbl = log_return$calculate_return(tbl, in_column = "x", out_column = "pct")
  mean_diff = mean(tbl$pct - c(NA, 0.0198, 0.0194, -0.0194, -0.00985), na.rm=T)
  testthat::expect_lt(mean_diff, 1e-5)
})
