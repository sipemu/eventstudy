#' Intraday Event Study Task
#'
#' Extension of \code{\link{EventStudyTask}} for intraday (high-frequency)
#' event studies. Supports POSIXct timestamps and time-based windows
#' specified in minutes or hours.
#'
#' @section Differences from EventStudyTask:
#' \itemize{
#'   \item Date column contains POSIXct timestamps instead of date strings
#'   \item Event windows specified as time offsets (e.g., \code{"-30min"}, \code{"+60min"})
#'   \item Handles trading session boundaries
#' }
#'
#' @export
IntradayEventStudyTask <- R6::R6Class(
  classname = "IntradayEventStudyTask",
  inherit = EventStudyTask,
  public = list(
    #' @field .index The time column name (POSIXct).
    .index = "timestamp",
    #' @field .target The price column name.
    .target = "price",
    #' @field .request_file_columns Required columns for intraday requests.
    .request_file_columns = c("event_id", "firm_symbol", "index_symbol",
                               "event_timestamp", "group",
                               "event_window_start", "event_window_end",
                               "shift_estimation_window",
                               "estimation_window_length"),

    #' @description
    #' Create a new IntradayEventStudyTask.
    #'
    #' @param firm_stock_data_tbl Dataframe with firm intraday data.
    #'   Must contain columns: \code{symbol}, \code{timestamp} (POSIXct),
    #'   and \code{price}.
    #' @param reference_tbl Dataframe with reference market intraday data.
    #' @param request_tbl Request dataframe with \code{event_timestamp}
    #'   (POSIXct), and window specifications in minutes.
    #' @param factor_tbl Optional factor data.
    initialize = function(firm_stock_data_tbl,
                          reference_tbl,
                          request_tbl,
                          factor_tbl = NULL) {
      # Validate timestamps
      private$check_intraday_input(firm_stock_data_tbl, "Firm table")
      private$check_intraday_input(reference_tbl, "Reference table")
      private$check_intraday_request(request_tbl)

      # Rename columns for internal consistency
      firm_stock_data_tbl <- firm_stock_data_tbl %>%
        dplyr::rename(firm_symbol = symbol, firm_price = price)
      reference_tbl <- reference_tbl %>%
        dplyr::rename(index_symbol = symbol, index_price = price)

      # Join index data to firm data via request
      request_join <- request_tbl %>%
        dplyr::select(event_id, group, firm_symbol, index_symbol)

      firm_stock_data_tbl <- firm_stock_data_tbl %>%
        dplyr::left_join(request_join, by = "firm_symbol") %>%
        dplyr::left_join(reference_tbl, by = c("index_symbol", "timestamp"))

      # Nest by event
      self$data_tbl <- firm_stock_data_tbl %>%
        dplyr::group_by(event_id, group, firm_symbol) %>%
        tidyr::nest()

      # Nest request
      request_nested <- request_tbl %>%
        dplyr::group_by(event_id, group, firm_symbol) %>%
        tidyr::nest() %>%
        dplyr::rename("request" = "data")

      self$data_tbl <- self$data_tbl %>%
        dplyr::left_join(request_nested,
                         by = c("event_id", "group", "firm_symbol"))

      if (!is.null(factor_tbl)) {
        self$factor_tbl <- factor_tbl
      }
    },

    #' @description
    #' Print summary.
    print = function(...) {
      cat("IntradayEventStudyTask\n")
      n_events <- nrow(self$data_tbl)
      cat("  Events:  ", n_events, "\n")
      groups <- unique(self$data_tbl$group)
      cat("  Groups:  ", length(groups), "(", paste(groups, collapse = ", "), ")\n")
      symbols <- unique(self$data_tbl$firm_symbol)
      cat("  Symbols: ", length(symbols), "\n")
      cat("  Type:     Intraday\n")
      invisible(self)
    }
  ),
  private = list(
    check_intraday_input = function(tbl, tbl_name) {
      if (!"symbol" %in% names(tbl)) {
        stop(tbl_name, ": Missing 'symbol' column.")
      }
      if (!"timestamp" %in% names(tbl)) {
        stop(tbl_name, ": Missing 'timestamp' column.")
      }
      if (!inherits(tbl$timestamp, "POSIXct")) {
        stop(tbl_name, ": 'timestamp' must be POSIXct.")
      }
      if (!"price" %in% names(tbl)) {
        stop(tbl_name, ": Missing 'price' column.")
      }
    },

    check_intraday_request = function(tbl) {
      required <- self$.request_file_columns
      missing_cols <- setdiff(required, names(tbl))
      if (length(missing_cols) > 0) {
        stop("Request missing columns: ", paste(missing_cols, collapse = ", "))
      }
      if (!"event_timestamp" %in% names(tbl)) {
        stop("Request must contain 'event_timestamp' column.")
      }
    }
  )
)


#' Prepare Intraday Event Study
#'
#' Compute returns and assign estimation/event windows for intraday data.
#' Windows are specified in number of observations (bars) rather than days.
#'
#' @param task An IntradayEventStudyTask.
#' @param parameter_set A ParameterSet.
#'
#' @return The task with returns and windows appended.
#'
#' @export
prepare_intraday_event_study <- function(task, parameter_set) {
  if (!inherits(task, "IntradayEventStudyTask")) {
    stop("task must be an IntradayEventStudyTask.")
  }

  # Calculate returns
  task$data_tbl <- task$data_tbl %>%
    dplyr::mutate(data = purrr::map(data, function(d) {
      d %>%
        dplyr::arrange(timestamp) %>%
        dplyr::mutate(
          firm_returns = (firm_price - dplyr::lag(firm_price)) / dplyr::lag(firm_price),
          index_returns = (index_price - dplyr::lag(index_price)) / dplyr::lag(index_price)
        )
    }))

  # Assign windows based on observation count from event timestamp
  task$data_tbl <- task$data_tbl %>%
    dplyr::mutate(data = purrr::map2(data, request, function(d, r) {
      event_ts <- r$event_timestamp
      event_window_start <- as.integer(r$event_window_start)
      event_window_end <- as.integer(r$event_window_end)
      est_shift <- as.integer(r$shift_estimation_window)
      est_length <- as.integer(r$estimation_window_length)

      d <- d %>% dplyr::arrange(timestamp)

      # Find event observation index
      event_idx <- which.min(abs(difftime(d$timestamp, event_ts, units = "secs")))

      d %>%
        dplyr::mutate(
          tmp_index = seq_len(dplyr::n()),
          relative_index = tmp_index - event_idx,
          event_date = ifelse(relative_index == 0, 1, 0),
          event_window = ifelse(relative_index >= event_window_start &
                                  relative_index <= event_window_end, 1, 0),
          estimation_window = ifelse(
            relative_index >= (est_shift - est_length) &
              relative_index <= est_shift, 1, 0
          )
        ) %>%
        dplyr::select(-tmp_index)
    }))

  task
}


#' Pure R implementation of the non-parametric intraday CAR test
#'
#' Ports the C++ `calc_T` algorithm: for each event time, accumulates CARs on
#' the event day and on each estimation day, then checks whether the event-day
#' CAR falls outside the empirical percentile of the estimation-day CARs.
#'
#' @param event_window Data frame with columns \code{time} and
#'   \code{abnormalReturn} for the single event day.
#' @param event_time_vector Character vector of event start times to test.
#' @param estimation_window Data frame with columns \code{day}, \code{time},
#'   and \code{abnormalReturn} for multiple estimation days.
#' @param initial_window Integer; number of observations to accumulate before
#'   the first significance check (default 5).
#' @param p Numeric; significance level in (0, 1) (default 0.05).
#' @param upper Logical; if \code{TRUE} test the upper tail, otherwise lower
#'   tail (default \code{FALSE}).
#'
#' @return A list (one element per event time) of lists with components
#'   \code{CAR} and \code{CI}.
#'
#' @noRd
.calc_T_r <- function(event_window, event_time_vector, estimation_window,
                      initial_window = 5L, p = 0.05, upper = FALSE) {
  ew_time <- as.character(event_window$time)
  ew_ret  <- event_window$abnormalReturn
  est_time <- as.character(estimation_window$time)
  est_ret  <- estimation_window$abnormalReturn

  results <- vector("list", length(event_time_vector))

  for (ei in seq_along(event_time_vector)) {
    event_start_time <- event_time_vector[ei]

    # Find the start index on the event day
    event_start_idx <- which(ew_time == event_start_time)[1]
    # Find all matching start indices across estimation days
    est_start_indices <- which(est_time == event_start_time)

    # End index: next event time or end of day
    if (ei == length(event_time_vector)) {
      event_end_idx <- length(ew_time)
    } else {
      event_end_idx <- which(ew_time == event_time_vector[ei + 1])[1] - 1L
    }

    # Initialize CARs
    event_car <- 0
    est_cars <- rep(0, length(est_start_indices))

    # Accumulate initial window
    tt <- 0L
    while (tt <= initial_window && (event_start_idx + tt) <= event_end_idx) {
      event_car <- event_car + ew_ret[event_start_idx + tt]
      for (i in seq_along(est_start_indices)) {
        est_cars[i] <- est_cars[i] + est_ret[est_start_indices[i] + tt]
      }
      tt <- tt + 1L
    }

    # Percentile function matching the C++ sort-based approach
    pct_val <- function(vec, prob, up) {
      q <- if (up) 1 - prob else prob
      sort(vec)[max(1L, floor(length(vec) * q))]
    }

    pctl <- pct_val(est_cars, p, upper)
    sig <- if (upper) event_car > pctl else event_car < pctl

    if (tt <= initial_window || !sig) {
      results[[ei]] <- list(CAR = 0, CI = 0)
    } else {
      car_result <- event_car
      ci_result  <- pctl

      while ((event_start_idx + tt) <= event_end_idx) {
        event_car <- event_car + ew_ret[event_start_idx + tt]
        for (i in seq_along(est_start_indices)) {
          est_cars[i] <- est_cars[i] + est_ret[est_start_indices[i] + tt]
        }

        pctl <- pct_val(est_cars, p, upper)
        sig <- if (upper) event_car > pctl else event_car < pctl

        if (!sig) break

        ci_result  <- c(ci_result, pctl)
        car_result <- c(car_result, event_car)
        tt <- tt + 1L
      }

      results[[ei]] <- list(CAR = car_result, CI = ci_result)
    }
  }

  results
}


#' Non-Parametric Intraday Event Study Test
#'
#' Implements the non-parametric intraday event study methodology of
#' Rinaudo & Saha (2014). For each event time, cumulative abnormal returns
#' (CARs) on the event day are compared to the empirical distribution of CARs
#' from estimation-period days. Significance is assessed without distributional
#' assumptions.
#'
#' @param estimation_window Data frame with columns \code{day}, \code{time},
#'   and \code{abnormalReturn} containing intraday abnormal returns for
#'   multiple estimation days.
#' @param event_window Data frame with columns \code{time} and
#'   \code{abnormalReturn} for the single event day.
#' @param event_times Character vector of intraday times at which events
#'   occurred (e.g., \code{c("10:00", "14:30")}).
#' @param p Numeric; significance level in (0, 1). Default 0.05.
#' @param init_window Integer; number of initial observations to accumulate
#'   before testing significance. Default 5.
#' @param upper Logical; if \code{TRUE} test for positive abnormal returns
#'   (upper tail), otherwise test for negative abnormal returns (lower tail).
#'   Default \code{FALSE}.
#'
#' @return A named list of tibbles, one per event time. Each tibble contains:
#'   \describe{
#'     \item{id}{Observation index within the significant window.}
#'     \item{CAR}{Cumulative abnormal return on the event day.}
#'     \item{CI}{Empirical confidence interval boundary from estimation days.}
#'     \item{fitCI}{Polynomial-smoothed CI (degree-4 polynomial fit).}
#'   }
#'   When an event time is not significant, returns a single-row tibble with
#'   all values equal to zero.
#'
#' @references
#' Rinaudo, J.B. & Saha, A. (2014). Non-parametric intraday event studies.
#'
#' @examples
#' \dontrun{
#' results <- nonparametric_intraday_test(
#'   estimation_window = est_data,
#'   event_window = event_data,
#'   event_times = c("10:00", "14:30"),
#'   p = 0.05,
#'   init_window = 5
#' )
#' }
#'
#' @export
nonparametric_intraday_test <- function(estimation_window, event_window,
                                        event_times, p = 0.05,
                                        init_window = 5L, upper = FALSE) {
  # --- Input validation ---
  if (!is.data.frame(estimation_window)) {
    stop("`estimation_window` must be a data frame.")
  }
  required_est <- c("day", "time", "abnormalReturn")
  missing_est <- setdiff(required_est, names(estimation_window))
  if (length(missing_est) > 0) {
    stop("`estimation_window` missing columns: ",
         paste(missing_est, collapse = ", "))
  }

  if (!is.data.frame(event_window)) {
    stop("`event_window` must be a data frame.")
  }
  required_ew <- c("time", "abnormalReturn")
  missing_ew <- setdiff(required_ew, names(event_window))
  if (length(missing_ew) > 0) {
    stop("`event_window` missing columns: ",
         paste(missing_ew, collapse = ", "))
  }

  if (!is.character(event_times) || length(event_times) == 0) {
    stop("`event_times` must be a non-empty character vector.")
  }

  if (!is.numeric(p) || length(p) != 1 || p <= 0 || p >= 1) {
    stop("`p` must be a single numeric value in (0, 1).")
  }

  if (!is.numeric(init_window) || length(init_window) != 1 || init_window < 1) {
    stop("`init_window` must be a positive integer.")
  }
  init_window <- as.integer(init_window)

  # --- Core computation ---
  raw <- .calc_T_r(
    event_window      = event_window,
    event_time_vector = event_times,
    estimation_window = estimation_window,
    initial_window    = init_window,
    p                 = p,
    upper             = upper
  )

  # --- Post-process into tidy tibbles ---
  out <- stats::setNames(vector("list", length(event_times)), event_times)

  for (i in seq_along(raw)) {
    car_vec <- raw[[i]]$CAR
    ci_vec  <- raw[[i]]$CI

    if (length(car_vec) == 1 && car_vec[1] == 0) {
      out[[i]] <- tibble::tibble(id = 1L, CAR = 0, CI = 0, fitCI = 0)
    } else {
      n <- length(car_vec)
      ids <- seq_len(n)

      # Polynomial-smoothed CI (degree 4, capped at n-1)
      degree <- min(4L, n - 1L)
      if (degree >= 1 && n >= 2) {
        fit <- stats::lm(ci_vec ~ stats::poly(ids, degree))
        fit_ci <- stats::predict(fit)
      } else {
        fit_ci <- ci_vec
      }

      out[[i]] <- tibble::tibble(
        id    = ids,
        CAR   = car_vec,
        CI    = ci_vec,
        fitCI = as.numeric(fit_ci)
      )
    }
  }

  out
}
