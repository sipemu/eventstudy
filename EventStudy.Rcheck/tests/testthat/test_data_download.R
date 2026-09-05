# Network download tests. skip_if_offline() only checks generic connectivity,
# not the specific data source (Yahoo Finance / Ken French library), which are
# frequently unreachable or rate-limited from CI. .try_download() skips the
# test when the source is down instead of failing on a transient outage.
.try_download <- function(expr) {
  tryCatch(
    expr,
    error = function(e)
      skip(paste("data source unreachable:", conditionMessage(e)))
  )
}

test_that("download_stock_data errors without tidyquant or quantmod", {
  # This test verifies the error message when neither package is available
  # Skip if either is installed (they'd mask the error)
  skip_if(requireNamespace("tidyquant", quietly = TRUE) ||
            requireNamespace("quantmod", quietly = TRUE))

  expect_error(
    download_stock_data("AAPL", from = "2024-01-01"),
    "tidyquant.*quantmod"
  )
})


test_that("download_stock_data returns formatted data", {
  skip_if_not_installed("tidyquant")
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_stock_data("AAPL", from = "2024-01-01",
                                to = "2024-01-31", format_for_task = TRUE))

  expect_true("symbol" %in% names(data))
  expect_true("date" %in% names(data))
  expect_true("adjusted" %in% names(data))
  # Date format should be dd.mm.yyyy
  expect_true(grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", data$date[1]))
})


test_that("download_stock_data with format_for_task=FALSE", {
  skip_if_not_installed("tidyquant")
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_stock_data("AAPL", from = "2024-01-01",
                                to = "2024-01-31", format_for_task = FALSE))

  expect_true(nrow(data) > 0)
  # Should still have data but possibly in original format
  expect_true(is.data.frame(data))
})


test_that("download_stock_data with multiple symbols", {
  skip_if_not_installed("tidyquant")
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_stock_data(c("AAPL", "MSFT"), from = "2024-01-01",
                                to = "2024-01-31", format_for_task = TRUE))

  expect_true(nrow(data) > 0)
  expect_true("symbol" %in% names(data))
  expect_equal(length(unique(data$symbol)), 2)
})


test_that("download_stock_data errors clearly when the source returns no data", {
  # Regression: when tq_get returns a bare logical (source unreachable /
  # rate-limited), download_stock_data must fail with a clear message rather
  # than a cryptic "no applicable method for 'transmute'" dispatch error.
  skip_if_not_installed("tidyquant")

  testthat::local_mocked_bindings(
    tq_get = function(...) NA,
    .package = "tidyquant"
  )

  expect_error(
    download_stock_data("AAPL", from = "2024-01-01", to = "2024-01-31"),
    "returned no data"
  )
})


test_that("download_factor_data downloads FF3 daily", {
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_factor_data(model = "ff3", frequency = "daily",
                                 format_for_task = TRUE))

  expect_true("date" %in% names(data))
  expect_true("market_excess" %in% names(data) ||
                "Mkt-RF" %in% names(data) ||
                "smb" %in% names(data))
  expect_true(nrow(data) > 0)
})


test_that("download_factor_data downloads FF5 daily", {
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_factor_data(model = "ff5", frequency = "daily",
                                 format_for_task = TRUE))

  expect_true("date" %in% names(data))
  expect_true(nrow(data) > 0)
  # FF5 should have rmw and cma
  if ("rmw" %in% names(data)) {
    expect_true("cma" %in% names(data))
  }
})


test_that("download_factor_data with format_for_task=FALSE", {
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_factor_data(model = "ff3", frequency = "daily",
                                 format_for_task = FALSE))

  expect_true(nrow(data) > 0)
  expect_true(is.data.frame(data))
})


test_that("download_risk_free_rate returns date and rate", {
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_risk_free_rate(frequency = "daily",
                                    format_for_task = TRUE))

  expect_true("date" %in% names(data))
  expect_true("risk_free_rate" %in% names(data))
  expect_true(nrow(data) > 0)
})


test_that("download_risk_free_rate with format_for_task=FALSE", {
  skip_on_cran()
  skip_if_offline()

  data <- .try_download(download_risk_free_rate(frequency = "daily",
                                    format_for_task = FALSE))

  expect_true(nrow(data) > 0)
  expect_true(is.data.frame(data))
})
