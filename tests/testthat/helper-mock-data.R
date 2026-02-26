# Helper functions for creating mock event study data
#
# These are automatically loaded by testthat before running tests.

library(dplyr)

#' Create mock stock price data for testing
#'
#' Generates synthetic stock price data for a given set of symbols.
#' Prices follow a random walk with drift.
create_mock_firm_data <- function(symbols = c("FIRM_A", "FIRM_B"),
                                  n_days = 300,
                                  start_date = as.Date("2020-01-01")) {
  set.seed(42)
  dates = seq(start_date, by = "day", length.out = n_days)
  # Remove weekends
  dates = dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

  firm_data = do.call(rbind, lapply(symbols, function(sym) {
    n = length(dates)
    # Random walk with small drift
    returns = rnorm(n, mean = 0.0002, sd = 0.02)
    prices = 100 * cumprod(1 + returns)
    tibble::tibble(
      symbol = sym,
      date = format(dates, "%d.%m.%Y"),
      adjusted = prices
    )
  }))

  firm_data
}


#' Create mock index/reference data for testing
create_mock_index_data <- function(index_symbols = c("INDEX_1"),
                                    n_days = 300,
                                    start_date = as.Date("2020-01-01")) {
  set.seed(123)
  dates = seq(start_date, by = "day", length.out = n_days)
  dates = dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

  index_data = do.call(rbind, lapply(index_symbols, function(sym) {
    n = length(dates)
    returns = rnorm(n, mean = 0.0003, sd = 0.015)
    prices = 1000 * cumprod(1 + returns)
    tibble::tibble(
      symbol = sym,
      date = format(dates, "%d.%m.%Y"),
      adjusted = prices
    )
  }))

  index_data
}


#' Create mock request table for testing
create_mock_request <- function(firm_symbols = c("FIRM_A", "FIRM_B"),
                                 index_symbol = "INDEX_1",
                                 event_dates = NULL,
                                 group = "TestGroup",
                                 event_window_start = -5,
                                 event_window_end = 5,
                                 shift_estimation_window = -6,
                                 estimation_window_length = 120) {
  n_days = 300
  start_date = as.Date("2020-01-01")
  dates = seq(start_date, by = "day", length.out = n_days)
  dates = dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

  if (is.null(event_dates)) {
    # Pick event dates roughly in the middle of the date range
    event_dates = format(dates[180], "%d.%m.%Y")
    event_dates = rep(event_dates, length(firm_symbols))
  }

  tibble::tibble(
    event_id = seq_along(firm_symbols),
    firm_symbol = firm_symbols,
    index_symbol = index_symbol,
    event_date = event_dates,
    group = group,
    event_window_start = event_window_start,
    event_window_end = event_window_end,
    shift_estimation_window = shift_estimation_window,
    estimation_window_length = estimation_window_length
  )
}


#' Create a complete mock event study task
create_mock_task <- function(n_firms = 2, group = "TestGroup") {
  symbols = paste0("FIRM_", LETTERS[1:n_firms])
  firm_data = create_mock_firm_data(symbols = symbols)
  index_data = create_mock_index_data()
  request = create_mock_request(firm_symbols = symbols, group = group)
  EventStudyTask$new(firm_data, index_data, request)
}


#' Create a complete mock event study task with factor data
create_mock_task_with_factors <- function(n_firms = 2, group = "TestGroup") {
  symbols = paste0("FIRM_", LETTERS[1:n_firms])
  firm_data = create_mock_firm_data(symbols = symbols)
  index_data = create_mock_index_data()
  request = create_mock_request(firm_symbols = symbols, group = group)

  n_days = 300
  start_date = as.Date("2020-01-01")
  dates = seq(start_date, by = "day", length.out = n_days)
  dates = dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

  set.seed(99)
  factor_tbl = tibble::tibble(
    date = format(dates, "%d.%m.%Y"),
    risk_free_rate = rep(0.0001, length(dates)),
    smb = rnorm(length(dates), 0, 0.005),
    hml = rnorm(length(dates), 0, 0.005),
    mom = rnorm(length(dates), 0, 0.005),
    rmw = rnorm(length(dates), 0, 0.004),
    cma = rnorm(length(dates), 0, 0.004)
  )

  EventStudyTask$new(firm_data, index_data, request, factor_tbl = factor_tbl)
}


#' Create a fitted mock task (convenience for tests that need a completed pipeline)
create_fitted_mock_task <- function(n_firms = 2, group = "TestGroup") {
  task = create_mock_task(n_firms = n_firms, group = group)
  ps = ParameterSet$new()
  run_event_study(task, ps)
}


#' Create estimation-window data for testing models directly
create_mock_model_data <- function(n_estimation = 120, n_event = 11) {
  set.seed(42)
  n_total = n_estimation + n_event
  index_returns = rnorm(n_total, mean = 0.0003, sd = 0.015)
  firm_returns = 0.001 + 1.2 * index_returns + rnorm(n_total, sd = 0.01)

  tibble::tibble(
    firm_returns = firm_returns,
    index_returns = index_returns,
    estimation_window = c(rep(1, n_estimation), rep(0, n_event)),
    event_window = c(rep(0, n_estimation), rep(1, n_event)),
    relative_index = c(seq(-n_estimation, -1), seq(0, n_event - 1)),
    event_date = c(rep(0, n_estimation), 1, rep(0, n_event - 1))
  )
}
