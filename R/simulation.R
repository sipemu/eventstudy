#' Simulate Event Study for Power Analysis
#'
#' Runs Monte Carlo simulations to estimate the statistical power (rejection
#' rate) of an event study design. Each simulation generates synthetic data
#' from a user-specified DGP, runs the full event study pipeline, and records
#' whether the null hypothesis is rejected.
#'
#' @param n_events Number of events (firms) per simulation. Default 20.
#' @param event_window Event window as \code{c(start, end)}. Default \code{c(-5, 5)}.
#' @param estimation_window_length Length of the estimation window. Default 120.
#' @param abnormal_return Injected abnormal return on the event day (day 0).
#'   Set to 0 for size analysis, positive for power analysis. Default 0.
#' @param return_model An initialized return model object. Default \code{MarketModel$new()}.
#' @param test_statistic Name of the multi-event test statistic to evaluate.
#'   Default \code{"CSectT"}.
#' @param alpha Significance level. Default 0.05.
#' @param n_simulations Number of Monte Carlo replications. Default 1000.
#' @param dgp_params List of DGP parameters: \code{alpha} (drift), \code{beta}
#'   (market exposure), \code{sigma_firm} (idiosyncratic volatility),
#'   \code{sigma_market} (market volatility).
#' @param seed Optional seed for reproducibility.
#'
#' @return An S3 object of class \code{es_simulation} with components:
#'   \describe{
#'     \item{power}{Rejection rate at the event day}
#'     \item{rejection_by_day}{Tibble of rejection rates for each event-window day}
#'     \item{test_stats}{Vector of test statistics at the event day from each sim}
#'     \item{params}{List of simulation parameters}
#'   }
#'
#' @export
simulate_event_study <- function(n_events = 20,
                                  event_window = c(-5, 5),
                                  estimation_window_length = 120,
                                  abnormal_return = 0,
                                  return_model = MarketModel$new(),
                                  test_statistic = "CSectT",
                                  alpha = 0.05,
                                  n_simulations = 1000,
                                  dgp_params = list(),
                                  seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # DGP defaults
  dgp <- list(
    alpha = dgp_params$alpha %||% 0.0002,
    beta = dgp_params$beta %||% 1.0,
    sigma_firm = dgp_params$sigma_firm %||% 0.01,
    sigma_market = dgp_params$sigma_market %||% 0.015
  )

  event_window_length <- event_window[2] - event_window[1] + 1
  n_days_total <- estimation_window_length + event_window_length + 50
  shift <- event_window[1] - 1  # gap between estimation and event

  # Storage
  event_day_stats <- numeric(n_simulations)
  rejection_matrix <- matrix(0, nrow = n_simulations, ncol = event_window_length)

  for (sim in seq_len(n_simulations)) {
    tryCatch({
      # Generate synthetic data
      syn <- .generate_synthetic_data(
        n_events = n_events,
        estimation_window_length = estimation_window_length,
        event_window = event_window,
        dgp = dgp,
        abnormal_return = abnormal_return,
        n_days_total = n_days_total
      )

      task <- syn$task
      ps <- ParameterSet$new(
        return_model = return_model$clone(deep = TRUE),
        single_event_statistics = NULL,
        multi_event_statistics = MultiEventStatisticsSet$new(
          tests = list(.resolve_test_stat(test_statistic))
        )
      )

      task <- run_event_study(task, ps)

      # Extract test statistics
      stat_tbl <- task$aar_caar_tbl[[test_statistic]][[1]]

      # Detect the t/z column
      t_col <- .detect_stat_column(stat_tbl)
      t_vals <- stat_tbl[[t_col]]
      n_valid <- stat_tbl$n_valid_events

      # Compute p-values
      if (grepl("_t$", t_col)) {
        p_vals <- 2 * stats::pt(abs(t_vals), df = n_valid - 1, lower.tail = FALSE)
      } else {
        p_vals <- 2 * stats::pnorm(abs(t_vals), lower.tail = FALSE)
      }

      # Record rejections
      rejection_matrix[sim, ] <- as.integer(p_vals < alpha)

      # Event day (relative_index == 0) statistic
      event_day_idx <- which(stat_tbl$relative_index == 0)
      if (length(event_day_idx) > 0) {
        event_day_stats[sim] <- t_vals[event_day_idx]
      }
    }, error = function(e) {
      # Failed simulations contribute NA
      event_day_stats[sim] <<- NA_real_
    })
  }

  # Compute rejection rates
  rel_indices <- seq(event_window[1], event_window[2])
  rejection_rates <- colMeans(rejection_matrix, na.rm = TRUE)

  rejection_by_day <- tibble::tibble(
    relative_index = rel_indices,
    rejection_rate = rejection_rates
  )

  event_day_idx <- which(rel_indices == 0)
  power <- if (length(event_day_idx) > 0) rejection_rates[event_day_idx] else NA_real_

  result <- list(
    power = power,
    rejection_by_day = rejection_by_day,
    test_stats = event_day_stats,
    params = list(
      n_events = n_events,
      event_window = event_window,
      estimation_window_length = estimation_window_length,
      abnormal_return = abnormal_return,
      test_statistic = test_statistic,
      alpha = alpha,
      n_simulations = n_simulations,
      dgp_params = dgp
    )
  )
  class(result) <- "es_simulation"
  result
}


#' @export
print.es_simulation <- function(x, ...) {
  cat("Event Study Simulation\n")
  cat("  N events:       ", x$params$n_events, "\n")
  cat("  Event window:   [", x$params$event_window[1], ",",
      x$params$event_window[2], "]\n")
  cat("  Abnormal return:", x$params$abnormal_return, "\n")
  cat("  Test statistic: ", x$params$test_statistic, "\n")
  cat("  Alpha:          ", x$params$alpha, "\n")
  cat("  N simulations:  ", x$params$n_simulations, "\n")
  cat("  Power (day 0):  ", round(x$power, 4), "\n")
  invisible(x)
}


#' Generate synthetic event study data
#' @noRd
.generate_synthetic_data <- function(n_events, estimation_window_length,
                                      event_window, dgp, abnormal_return,
                                      n_days_total) {
  symbols <- paste0("SIM_", seq_len(n_events))
  start_date <- as.Date("2020-01-01")
  dates <- seq(start_date, by = "day", length.out = n_days_total * 1.5)
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  dates <- dates[seq_len(n_days_total)]

  # Market returns
  market_returns <- stats::rnorm(n_days_total, mean = 0.0003,
                                  sd = dgp$sigma_market)
  market_prices <- 1000 * cumprod(1 + market_returns)

  index_data <- tibble::tibble(
    symbol = "SIM_INDEX",
    date = format(dates, "%d.%m.%Y"),
    adjusted = market_prices
  )

  # Firm returns
  firm_data_list <- lapply(symbols, function(sym) {
    eps <- stats::rnorm(n_days_total, sd = dgp$sigma_firm)
    firm_ret <- dgp$alpha + dgp$beta * market_returns + eps
    firm_prices <- 100 * cumprod(1 + firm_ret)
    tibble::tibble(
      symbol = sym,
      date = format(dates, "%d.%m.%Y"),
      adjusted = firm_prices
    )
  })
  firm_data <- do.call(rbind, firm_data_list)

  # Pick event date in the middle (after estimation window)
  event_day_pos <- estimation_window_length + abs(event_window[1]) + 20
  event_date_str <- format(dates[event_day_pos], "%d.%m.%Y")

  # Inject abnormal return on event day for each firm
  if (abnormal_return != 0) {
    for (sym in symbols) {
      idx <- which(firm_data$symbol == sym & firm_data$date == event_date_str)
      if (length(idx) > 0) {
        old_price <- firm_data$adjusted[idx]
        if (idx > 1) {
          prev_price <- firm_data$adjusted[idx - 1]
          # Adjust price to inject additional return
          new_price <- prev_price * (1 + (old_price / prev_price - 1) + abnormal_return)
          firm_data$adjusted[idx] <- new_price
        }
      }
    }
  }

  request <- tibble::tibble(
    event_id = seq_len(n_events),
    firm_symbol = symbols,
    index_symbol = "SIM_INDEX",
    event_date = event_date_str,
    group = "Simulation",
    event_window_start = event_window[1],
    event_window_end = event_window[2],
    shift_estimation_window = event_window[1] - 1,
    estimation_window_length = estimation_window_length
  )

  task <- EventStudyTask$new(firm_data, index_data, request)

  list(task = task)
}


#' Resolve test statistic name to R6 object
#' @noRd
.resolve_test_stat <- function(name) {
  switch(name,
    "CSectT"   = CSectTTest$new(),
    "PatellZ"  = PatellZTest$new(),
    "SignT"    = SignTest$new(),
    "GSignT"   = GeneralizedSignTest$new(),
    "RankT"    = RankTest$new(),
    "BMP"      = BMPTest$new(),
    "KP"       = KolariPynnonenTest$new(),
    "CalTimeT" = CalendarTimePortfolioTest$new(),
    stop("Unknown test statistic: ", name)
  )
}


#' Detect the primary test statistic column in a result tibble
#' @noRd
.detect_stat_column <- function(tbl) {
  candidates <- c("aar_t", "aar_z", "bmp_t", "kp_t", "sign_z",
                   "gsign_z", "rank_z")
  hit <- intersect(candidates, names(tbl))
  if (length(hit) == 0) stop("Cannot detect test statistic column.")
  hit[1]
}
