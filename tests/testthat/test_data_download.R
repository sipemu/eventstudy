# Network download tests. skip_if_offline() only checks generic connectivity,
# not the specific data source (Yahoo Finance / Ken French library), which are
# frequently unreachable or rate-limited from CI. .try_download() skips the
# test when the source is down instead of failing on a transient outage.
#
# C6 (2026-09-24): only NETWORK/HTTP failures convert to skip() -- matched by
# condition class (curl/httr2, when present) or by a narrow message pattern.
# Every other error (a parse/logic bug in the package) is re-signalled so the
# test fails instead of being silently skipped.
.NETWORK_ERROR_PATTERN <- paste0(
  "(could not resolve host|timed? ?out|timeout|http (error )?[45][0-9]{2}|",
  "cannot open url|cannot open connection|ssl|unreachable|rate.?limit|",
  "failed to download|connection (refused|reset)|could not connect|",
  "network is unreachable|gateway timeout|service unavailable)"
)
.try_download <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      is_network_class <- inherits(e, c("curl_error", "httr2_http", "httr2_failure",
                                          "httr_error", "http_error"))
      is_network_msg <- grepl(.NETWORK_ERROR_PATTERN, conditionMessage(e), ignore.case = TRUE)
      if (is_network_class || is_network_msg) {
        skip(paste("data source unreachable:", conditionMessage(e)))
      }
      stop(e)
    }
  )
}


test_that(".try_download skips on a network-style condition (C6, 2026-09-24)", {
  net_err <- simpleError("Failed to download: could not resolve host www.example.com")
  expect_condition(
    .try_download(stop(net_err)),
    class = "skip"
  )
})


test_that(".try_download re-signals a non-network (logic) error (C6, 2026-09-24)", {
  expect_error(
    .try_download(stop("subscript out of bounds")),
    "subscript out of bounds"
  )
})

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
