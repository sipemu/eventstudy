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


# ---------------------------------------------------------------------------
# Helpers for nonparametric_intraday_test
# ---------------------------------------------------------------------------

#' Create a mock estimation window spanning multiple days with identical
#' intraday time grids.
create_mock_np_estimation_window <- function(n_days = 20, n_bars_per_day = 50,
                                              seed = 42) {
  set.seed(seed)
  times <- sprintf("%02d:%02d", 10L + seq_len(n_bars_per_day) %/% 60L,
                   seq_len(n_bars_per_day) %% 60L)
  days <- seq_len(n_days)
  tibble::tibble(
    day  = rep(days, each = n_bars_per_day),
    time = rep(times, n_days),
    abnormalReturn = rnorm(n_days * n_bars_per_day, 0, 0.001)
  )
}

#' Create a single event-day window with the same time grid.
create_mock_np_event_window <- function(n_bars = 50, seed = 123,
                                         inject_signal = FALSE) {
  set.seed(seed)
  times <- sprintf("%02d:%02d", 10L + seq_len(n_bars) %/% 60L,
                   seq_len(n_bars) %% 60L)
  ar <- rnorm(n_bars, 0, 0.001)

  if (inject_signal) {
    # Inject a large negative signal starting at bar 10 to ensure significance
    ar[10:25] <- ar[10:25] - 0.02
  }

  tibble::tibble(
    time = times,
    abnormalReturn = ar
  )
}


# ---------------------------------------------------------------------------
# Tests for nonparametric_intraday_test
# ---------------------------------------------------------------------------

test_that("nonparametric_intraday_test returns list of tibbles with expected columns", {
  est <- create_mock_np_estimation_window()
  ew  <- create_mock_np_event_window()
  event_times <- ew$time[c(1, 20)]

  res <- nonparametric_intraday_test(est, ew, event_times)

  expect_type(res, "list")
  expect_equal(length(res), 2)
  expect_equal(names(res), event_times)

  for (r in res) {
    expect_s3_class(r, "tbl_df")
    expect_true(all(c("id", "CAR", "CI", "fitCI") %in% names(r)))
  }
})

test_that("nonparametric_intraday_test validates p in (0,1)", {
  est <- create_mock_np_estimation_window()
  ew  <- create_mock_np_event_window()
  et  <- ew$time[1]

  expect_error(nonparametric_intraday_test(est, ew, et, p = 0),
               "`p` must be")
  expect_error(nonparametric_intraday_test(est, ew, et, p = 1),
               "`p` must be")
  expect_error(nonparametric_intraday_test(est, ew, et, p = -0.1),
               "`p` must be")
})

test_that("nonparametric_intraday_test validates input data frames", {
  est <- create_mock_np_estimation_window()
  ew  <- create_mock_np_event_window()
  et  <- ew$time[1]

  # estimation_window missing column
  bad_est <- est[, c("day", "time")]
  expect_error(nonparametric_intraday_test(bad_est, ew, et),
               "abnormalReturn")

  # event_window missing column
  bad_ew <- ew[, "time", drop = FALSE]
  expect_error(nonparametric_intraday_test(est, bad_ew, et),
               "abnormalReturn")

  # event_times not character
  expect_error(nonparametric_intraday_test(est, ew, 123),
               "`event_times`")
})

test_that("nonparametric_intraday_test returns CAR=0/CI=0 for non-significant events", {
  est <- create_mock_np_estimation_window(n_days = 20, seed = 42)
  ew  <- create_mock_np_event_window(seed = 42, inject_signal = FALSE)
  et  <- ew$time[1]

  res <- nonparametric_intraday_test(est, ew, et, p = 0.05, init_window = 5L)
  r <- res[[1]]

  # Non-significant: single row with zeros
  expect_equal(nrow(r), 1)
  expect_equal(r$CAR[1], 0)
  expect_equal(r$CI[1], 0)
  expect_equal(r$fitCI[1], 0)
})

test_that("nonparametric_intraday_test detects significant event with injected signal", {
  est <- create_mock_np_estimation_window(n_days = 20, seed = 42)
  ew  <- create_mock_np_event_window(seed = 42, inject_signal = TRUE)
  # Event time at bar 10 where the signal starts
  et  <- ew$time[10]

  res <- nonparametric_intraday_test(est, ew, et, p = 0.05, init_window = 5L)
  r <- res[[1]]

  # Should detect significance: more than 1 row

  expect_gt(nrow(r), 1)
  # CAR should be negative (injected negative signal)
  expect_true(all(r$CAR < 0))
  # fitCI should have same length as CAR
  expect_equal(length(r$fitCI), length(r$CAR))
})

test_that("nonparametric_intraday_test upper tail works", {
  est <- create_mock_np_estimation_window(n_days = 20, seed = 42)
  ew  <- create_mock_np_event_window(seed = 42, inject_signal = FALSE)
  et  <- ew$time[1]

  # Upper tail on random data: should also be non-significant
  res <- nonparametric_intraday_test(est, ew, et, p = 0.05,
                                     init_window = 5L, upper = TRUE)
  r <- res[[1]]
  expect_equal(nrow(r), 1)
  expect_equal(r$CAR[1], 0)
})

test_that("nonparametric_intraday_test fitCI is polynomial-smoothed", {
  est <- create_mock_np_estimation_window(n_days = 20, seed = 42)
  ew  <- create_mock_np_event_window(seed = 42, inject_signal = TRUE)
  et  <- ew$time[10]

  res <- nonparametric_intraday_test(est, ew, et, p = 0.05, init_window = 5L)
  r <- res[[1]]

  if (nrow(r) > 1) {
    # fitCI should differ from raw CI (smoothed)
    # They may be close but not identical for >4 points
    if (nrow(r) > 5) {
      expect_false(all(r$fitCI == r$CI))
    }
    # fitCI should be numeric with no NAs
    expect_true(is.numeric(r$fitCI))
    expect_false(any(is.na(r$fitCI)))
  }
})
