# Offline format conversion tests (don't need internet)

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

  data <- download_stock_data("AAPL", from = "2024-01-01",
                                to = "2024-01-31", format_for_task = TRUE)

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

  data <- download_stock_data("AAPL", from = "2024-01-01",
                                to = "2024-01-31", format_for_task = FALSE)

  expect_true(nrow(data) > 0)
  # Should still have data but possibly in original format
  expect_true(is.data.frame(data))
})


test_that("download_stock_data with multiple symbols", {
  skip_if_not_installed("tidyquant")
  skip_on_cran()
  skip_if_offline()

  data <- download_stock_data(c("AAPL", "MSFT"), from = "2024-01-01",
                                to = "2024-01-31", format_for_task = TRUE)

  expect_true(nrow(data) > 0)
  expect_true("symbol" %in% names(data))
  expect_equal(length(unique(data$symbol)), 2)
})


test_that("download_factor_data downloads FF3 daily", {
  skip_on_cran()
  skip_if_offline()

  data <- download_factor_data(model = "ff3", frequency = "daily",
                                 format_for_task = TRUE)

  expect_true("date" %in% names(data))
  expect_true("market_excess" %in% names(data) ||
                "Mkt-RF" %in% names(data) ||
                "smb" %in% names(data))
  expect_true(nrow(data) > 0)
})


test_that("download_factor_data downloads FF5 daily", {
  skip_on_cran()
  skip_if_offline()

  data <- download_factor_data(model = "ff5", frequency = "daily",
                                 format_for_task = TRUE)

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

  data <- download_factor_data(model = "ff3", frequency = "daily",
                                 format_for_task = FALSE)

  expect_true(nrow(data) > 0)
  expect_true(is.data.frame(data))
})


test_that("download_risk_free_rate returns date and rate", {
  skip_on_cran()
  skip_if_offline()

  data <- download_risk_free_rate(frequency = "daily",
                                    format_for_task = TRUE)

  expect_true("date" %in% names(data))
  expect_true("risk_free_rate" %in% names(data))
  expect_true(nrow(data) > 0)
})


test_that("download_risk_free_rate with format_for_task=FALSE", {
  skip_on_cran()
  skip_if_offline()

  data <- download_risk_free_rate(frequency = "daily",
                                    format_for_task = FALSE)

  expect_true(nrow(data) > 0)
  expect_true(is.data.frame(data))
})
