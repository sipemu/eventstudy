#' Validate an Event Study experiment
#'
#' Performs validation checks on the event study task:
#' 1. Does the event date exist in the stock data for each event?
#' 2. Are there sufficient observations in the estimation window?
#' 3. Are there any gaps in the time series?
#' 4. Do estimation and event windows overlap?
#'
#' @param task The event study task.
#' @param parameter_set The parameter set that defines the event study.
#' @param min_estimation_obs Minimum number of observations required in the
#'   estimation window. Default is 30.
#'
#' @return The task object (invisibly). Warnings are issued for each problem found.
#'
#' @export
validate_task <- function(task, parameter_set = NULL, min_estimation_obs = 30) {
  if (!inherits(task, "EventStudyTask")) {
    stop("The task must be an EventStudyTask!")
  }

  n_events = nrow(task$data_tbl)
  n_issues = 0

  for (i in seq_len(n_events)) {
    event_id = task$data_tbl$event_id[i]
    symbol = task$data_tbl$firm_symbol[i]
    data = task$data_tbl$data[[i]]
    request = task$data_tbl$request[[i]]
    label = paste0("[Event ", event_id, " / ", symbol, "]")

    # 1. Check if event date exists in data
    if (!"event_date" %in% names(data)) {
      # Before prepare_event_study, check if the request date exists
      event_date_val = request$event_date
      if (!event_date_val %in% data$date) {
        warning(label, " Event date '", event_date_val, "' not found in data.")
        n_issues = n_issues + 1
      }
    } else {
      if (sum(data$event_date == 1) == 0) {
        warning(label, " Event date not found in data.")
        n_issues = n_issues + 1
      } else if (sum(data$event_date == 1) > 1) {
        warning(label, " Multiple event dates found in data.")
        n_issues = n_issues + 1
      }
    }

    # 2. Check estimation window has sufficient observations
    if ("estimation_window" %in% names(data)) {
      n_est = sum(data$estimation_window == 1, na.rm = TRUE)
      if (n_est < min_estimation_obs) {
        warning(label, " Estimation window has only ", n_est,
                " observations (minimum: ", min_estimation_obs, ").")
        n_issues = n_issues + 1
      }
    }

    # 3. Check for gaps in time series (missing trading days)
    if ("date" %in% names(data) && nrow(data) > 1) {
      dates = sort(as.Date(data$date, format = "%d.%m.%Y"))
      # Check for gaps > 5 days (accounting for weekends)
      diffs = as.integer(diff(dates))
      large_gaps = which(diffs > 5)
      if (length(large_gaps) > 0) {
        warning(label, " ", length(large_gaps), " gap(s) > 5 days found in time series.")
        n_issues = n_issues + 1
      }
    }

    # 4. Check estimation and event windows don't overlap
    if (all(c("estimation_window", "event_window") %in% names(data))) {
      overlap = sum(data$estimation_window == 1 & data$event_window == 1, na.rm = TRUE)
      if (overlap > 0) {
        warning(label, " Estimation and event windows overlap by ", overlap, " observation(s).")
        n_issues = n_issues + 1
      }
    }

    # 5. Check for thin trading (many zero returns)
    if ("firm_returns" %in% names(data) && "estimation_window" %in% names(data)) {
      est_returns = data$firm_returns[data$estimation_window == 1]
      est_returns = est_returns[!is.na(est_returns)]
      if (length(est_returns) > 0) {
        pct_zero = sum(est_returns == 0) / length(est_returns)
        if (pct_zero > 0.1) {
          warning(label, " ", round(pct_zero * 100, 1),
                  "% zero returns in estimation window (possible thin trading).")
          n_issues = n_issues + 1
        }
      }
    }
  }

  if (n_issues == 0) {
    message("Validation passed: no issues found across ", n_events, " event(s).")
  } else {
    message("Validation complete: ", n_issues, " issue(s) found across ", n_events, " event(s).")
  }

  invisible(task)
}
