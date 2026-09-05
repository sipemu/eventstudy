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


#' Create degenerate model data with insufficient estimation observations
#'
#' Returns a data set derived from create_mock_model_data() where all but
#' the first \code{n_valid} estimation-window rows have both firm_returns
#' and index_returns set to NA_real_, producing a degenerate estimation
#' window with fewer than 2 valid observations.
#'
#' @param n_valid Integer. Number of estimation rows to leave non-NA (default 1).
#' @param n_event Integer. Number of event-window rows (default 11).
create_degenerate_model_data_insufficient <- function(n_valid = 1, n_event = 11) {
  d <- create_mock_model_data(n_estimation = 120, n_event = n_event)
  # Identify estimation rows
  est_rows <- which(d$estimation_window == 1)
  # Set all estimation rows beyond the first n_valid to NA
  if (length(est_rows) > n_valid) {
    rows_to_na <- est_rows[(n_valid + 1):length(est_rows)]
    d$firm_returns[rows_to_na]  <- NA_real_
    d$index_returns[rows_to_na] <- NA_real_
  }
  d
}


#' Create degenerate model data with zero variance in index returns
#'
#' Returns a data set derived from create_mock_model_data() where all
#' estimation-window index_returns are set to a single constant (0.001),
#' making sd(index_returns) == 0 in the estimation window and OLS undefined.
#'
#' @param n_event Integer. Number of event-window rows (default 11).
create_degenerate_model_data_zero_variance <- function(n_event = 11) {
  d <- create_mock_model_data(n_estimation = 120, n_event = n_event)
  # Set all estimation-window index_returns to a constant
  est_rows <- which(d$estimation_window == 1)
  d$index_returns[est_rows] <- 0.001
  d
}


#' Create degenerate volume model data with insufficient valid observations
#'
#' Returns a data set with a firm_volume column where all but the first
#' n_valid estimation-window rows have NA volume, producing fewer than 2
#' finite entries for the VolumeModel estimation guard.
#'
#' @param n_valid Integer. Number of estimation rows to leave non-NA (default 1).
#' @param n_event Integer. Number of event-window rows (default 11).
create_degenerate_volume_model_data_insufficient <- function(n_valid = 1, n_event = 11) {
  d <- create_mock_model_data(n_estimation = 120, n_event = n_event)
  set.seed(7)
  n_total <- nrow(d)
  # Add a firm_volume column (realistic positive values)
  d$firm_volume <- abs(rnorm(n_total, mean = 1e6, sd = 2e5))
  # NA out all but n_valid estimation rows
  est_rows <- which(d$estimation_window == 1)
  if (length(est_rows) > n_valid) {
    rows_to_na <- est_rows[(n_valid + 1):length(est_rows)]
    d$firm_volume[rows_to_na] <- NA_real_
  }
  d
}


#' Create degenerate volume model data with zero variance after log-transform
#'
#' Returns a data set with a firm_volume column where all estimation-window
#' rows have the same constant volume, making sd(log(vol + 1)) == 0 and
#' triggering the VolumeModel zero-variance guard.
#'
#' @param n_event Integer. Number of event-window rows (default 11).
create_degenerate_volume_model_data_zero_variance <- function(n_event = 11) {
  d <- create_mock_model_data(n_estimation = 120, n_event = n_event)
  set.seed(8)
  n_total <- nrow(d)
  d$firm_volume <- abs(rnorm(n_total, mean = 1e6, sd = 2e5))
  # Set all estimation-window volumes to a constant so sd(log(vol+1)) == 0
  est_rows <- which(d$estimation_window == 1)
  d$firm_volume[est_rows] <- 1000.0
  d
}


#' Create degenerate volatility model data with zero variance in firm returns
#'
#' Returns a data set where all estimation-window firm_returns are set to a
#' single constant (0.001), making var(firm_returns) == 0 in the estimation
#' window and triggering the VolatilityModel zero-variance guard.
#'
#' @param n_event Integer. Number of event-window rows (default 11).
create_degenerate_volatility_model_data_zero_var <- function(n_event = 11) {
  d <- create_mock_model_data(n_estimation = 120, n_event = n_event)
  # Set all estimation-window firm_returns to a constant
  est_rows <- which(d$estimation_window == 1)
  d$firm_returns[est_rows] <- 0.001
  d
}


#' Create factor model data for testing LinearFactorModel / FF3 / FF5 / Carhart4
#'
#' Returns a flat estimation + event-window tibble with all columns that the
#' factor model formulas read: firm_returns, index_returns (base columns kept
#' for compatibility), excess_return, market_excess, smb, hml, mom, rmw, cma,
#' risk_free_rate, plus the window-tracking columns used by fit().
#'
#' @param n_estimation Integer. Estimation window rows (default 120).
#' @param n_event Integer. Event window rows (default 11).
create_mock_factor_model_data <- function(n_estimation = 120, n_event = 11) {
  set.seed(55)
  n_total <- n_estimation + n_event

  # Common factor returns (small magnitude, mimicking Fama-French data)
  market_excess  <- rnorm(n_total, mean = 0.0003, sd = 0.015)
  smb            <- rnorm(n_total, mean = 0.0001, sd = 0.005)
  hml            <- rnorm(n_total, mean = 0.0001, sd = 0.005)
  mom            <- rnorm(n_total, mean = 0.0000, sd = 0.005)
  rmw            <- rnorm(n_total, mean = 0.0001, sd = 0.004)
  cma            <- rnorm(n_total, mean = 0.0001, sd = 0.004)
  risk_free_rate <- rep(0.0001, n_total)

  # Firm returns as a function of factors + idiosyncratic noise
  firm_returns   <- risk_free_rate +
    0.9 * market_excess +
    0.3 * smb +
    0.2 * hml +
    0.1 * mom +
    rnorm(n_total, sd = 0.008)

  # Excess returns (firm minus risk-free) — the LHS used by factor models
  excess_return  <- firm_returns - risk_free_rate
  # index_returns kept for compatibility (same as market_excess + risk_free)
  index_returns  <- market_excess + risk_free_rate

  tibble::tibble(
    firm_returns   = firm_returns,
    index_returns  = index_returns,
    excess_return  = excess_return,
    market_excess  = market_excess,
    smb            = smb,
    hml            = hml,
    mom            = mom,
    rmw            = rmw,
    cma            = cma,
    risk_free_rate = risk_free_rate,
    estimation_window = c(rep(1L, n_estimation), rep(0L, n_event)),
    event_window      = c(rep(0L, n_estimation), rep(1L, n_event)),
    relative_index    = c(seq(-n_estimation, -1L), seq(0L, n_event - 1L)),
    event_date        = c(rep(0L, n_estimation), 1L, rep(0L, n_event - 1L))
  )
}
