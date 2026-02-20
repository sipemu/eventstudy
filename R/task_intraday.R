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
