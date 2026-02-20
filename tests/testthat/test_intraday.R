# Helper to create mock intraday data
create_mock_intraday_data <- function(symbol = "FIRM_A",
                                       n_bars = 500,
                                       start_time = as.POSIXct("2020-06-15 09:30:00",
                                                                tz = "UTC")) {
  set.seed(42)
  timestamps <- seq(start_time, by = "1 min", length.out = n_bars)

  returns <- rnorm(n_bars, mean = 0, sd = 0.001)
  prices <- 100 * cumprod(1 + returns)


  tibble::tibble(
    symbol = symbol,
    timestamp = timestamps,
    price = prices
  )
}

create_mock_intraday_index <- function(index_symbol = "INDEX_1",
                                        n_bars = 500,
                                        start_time = as.POSIXct("2020-06-15 09:30:00",
                                                                 tz = "UTC")) {
  set.seed(123)
  timestamps <- seq(start_time, by = "1 min", length.out = n_bars)

  returns <- rnorm(n_bars, mean = 0, sd = 0.0008)
  prices <- 1000 * cumprod(1 + returns)

  tibble::tibble(
    symbol = index_symbol,
    timestamp = timestamps,
    price = prices
  )
}

create_mock_intraday_request <- function(firm_symbol = "FIRM_A",
                                          index_symbol = "INDEX_1",
                                          start_time = as.POSIXct("2020-06-15 09:30:00",
                                                                   tz = "UTC")) {
  event_time <- start_time + 300 * 60  # 300 bars in

  tibble::tibble(
    event_id = 1L,
    firm_symbol = firm_symbol,
    index_symbol = index_symbol,
    event_timestamp = event_time,
    group = "TestGroup",
    event_window_start = -30L,
    event_window_end = 30L,
    shift_estimation_window = -31L,
    estimation_window_length = 120L
  )
}


test_that("IntradayEventStudyTask creates correctly", {
  firm <- create_mock_intraday_data()
  index <- create_mock_intraday_index()
  request <- create_mock_intraday_request()

  task <- IntradayEventStudyTask$new(firm, index, request)

  expect_s3_class(task, "IntradayEventStudyTask")
  expect_s3_class(task, "EventStudyTask")
  expect_equal(nrow(task$data_tbl), 1)
})

test_that("IntradayEventStudyTask errors on missing timestamp", {
  firm <- tibble::tibble(
    symbol = "FIRM_A",
    date = Sys.time() + 1:10,
    price = 100 + rnorm(10)
  )
  index <- create_mock_intraday_index()
  request <- create_mock_intraday_request()

  expect_error(
    IntradayEventStudyTask$new(firm, index, request),
    "timestamp"
  )
})

test_that("IntradayEventStudyTask errors on non-POSIXct timestamp", {
  firm <- tibble::tibble(
    symbol = "FIRM_A",
    timestamp = as.character(Sys.time() + 1:10),
    price = 100 + rnorm(10)
  )
  index <- create_mock_intraday_index()
  request <- create_mock_intraday_request()

  expect_error(
    IntradayEventStudyTask$new(firm, index, request),
    "POSIXct"
  )
})

test_that("prepare_intraday_event_study computes returns and windows", {
  firm <- create_mock_intraday_data()
  index <- create_mock_intraday_index()
  request <- create_mock_intraday_request()

  task <- IntradayEventStudyTask$new(firm, index, request)
  ps <- ParameterSet$new()

  task <- prepare_intraday_event_study(task, ps)

  d <- task$data_tbl$data[[1]]
  expect_true("firm_returns" %in% names(d))
  expect_true("index_returns" %in% names(d))
  expect_true("event_window" %in% names(d))
  expect_true("estimation_window" %in% names(d))
  expect_true("relative_index" %in% names(d))

  # Check windows are assigned

  expect_gt(sum(d$event_window == 1, na.rm = TRUE), 0)
  expect_gt(sum(d$estimation_window == 1, na.rm = TRUE), 0)
})

test_that("IntradayEventStudyTask print works", {
  firm <- create_mock_intraday_data()
  index <- create_mock_intraday_index()
  request <- create_mock_intraday_request()

  task <- IntradayEventStudyTask$new(firm, index, request)
  expect_output(print(task), "IntradayEventStudyTask")
})
