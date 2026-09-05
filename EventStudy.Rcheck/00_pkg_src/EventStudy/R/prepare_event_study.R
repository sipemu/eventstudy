#' Prepare data for an Event Study
#'
#' Perform return calculation for each stock and the corresponding reference
#' market defined in the task and the parameter set.
#'
#' @param task An Event Study task.
#' @param parameter_set A parameter set that defines the Event Study.
#'
#' @return The task object with returns and windows appended.
#'
#' @export
prepare_event_study <- function(task, parameter_set) {
  if (!inherits(task, "EventStudyTask")) {
    stop("The task must be a EventStudyTask!")
  }

  if (!inherits(parameter_set, "ParameterSet")) {
    stop("The parameter_set must be a ParameterSet!")
  }

  # Resolve degenerate mode once here so the closure captures a stable value
  # rather than re-resolving per row (consistent with the Phase 1 pattern in
  # fit_model(), which also resolves mode before the row-indexed map).
  mode <- .resolve_degenerate_mode(parameter_set$degenerate_handling)

  # Calculate returns
  task$data_tbl = task$data_tbl %>%
    mutate(data = purrr::map(.x=data,
                             .f=.append_returns,
                             return_calculation=parameter_set$return_calculation,
                             in_column=task$.target))

  # Append estimation window using an explicit row-indexed map so that
  # mode, event_id, and firm_symbol can be threaded into .append_windows()
  # without NSE ambiguity (per D-01 from Phase 1: no pmap-inside-mutate).
  task$data_tbl$data <- purrr::map(
    seq_len(nrow(task$data_tbl)),
    function(i) {
      .append_windows(
        task$data_tbl$data[[i]],
        task$data_tbl$request[[i]],
        mode        = mode,
        event_id    = task$data_tbl$event_id[[i]],
        firm_symbol = task$data_tbl$firm_symbol[[i]]
      )
    }
  )

  # Join factor data if provided (for Fama-French, Carhart models)
  if (!is.null(task$factor_tbl)) {
    factor_tbl = task$factor_tbl
    task$data_tbl = task$data_tbl %>%
      dplyr::mutate(data = purrr::map(data, function(d) {
        d %>% dplyr::left_join(factor_tbl, by = "date")
      }))

    # Compute excess returns if risk_free_rate is available
    has_rf = "risk_free_rate" %in% names(factor_tbl)
    if (has_rf) {
      task$data_tbl = task$data_tbl %>%
        dplyr::mutate(data = purrr::map(data, function(d) {
          if ("risk_free_rate" %in% names(d)) {
            d <- d %>% dplyr::mutate(excess_return = firm_returns - risk_free_rate)
            # Only compute market_excess if not already provided by factor data (e.g., FF Mkt-RF)
            if (!"market_excess" %in% names(d)) {
              d <- d %>% dplyr::mutate(market_excess = index_returns - risk_free_rate)
            }
            d
          } else {
            d
          }
        }))
    }
  }

  task
}


#' Append returns (simple or log) to given input dataframe
#'
#' @param data_tbl A dataframe of a single stock index with reference market data.
#' @param return_calculation An initialized ReturnCalculation class.
#' @param in_column String identifier for the price data.
#'
#' @noRd
.append_returns = function(data_tbl, return_calculation, in_column='adjusted') {
  in_cols = colnames(data_tbl)[stringr::str_detect(colnames(data_tbl), in_column)]
  out_cols = stringr::str_replace(in_cols, in_column, "returns")

  data_tbl %>%
    return_calculation$calculate_return(in_column=in_cols[1], out_column=out_cols[1]) %>%
    return_calculation$calculate_return(in_column=in_cols[2], out_column=out_cols[2])
}


#' Create event and estimation window, and relative index columns
#'
#' This method adds the columns event and estimation window and the relative
#' index to the given input data frame according to the single Event Study
#' request definition.
#'
#' @param data_tbl A dataframe of a single stock index with reference market data.
#' @param request The specification of the Event Study for the given stock.
#' @param mode Degenerate-handling mode: "lenient" (default) or "strict".
#' @param event_id Event identifier, passed to .handle_degenerate for informative messages.
#' @param firm_symbol Firm identifier, passed to .handle_degenerate for informative messages.
#'
#' @noRd
.append_windows = function(data_tbl, request, mode = "lenient",
                             event_id = NULL, firm_symbol = NULL) {
  # Add event date & temporary index
  data_tbl = data_tbl %>%
    mutate(event_date = ifelse(date == request$event_date, 1, 0)) %>%
    mutate(tmp_index  = 1:n())

  # Create columns event_window and estimation_window
  event_window_start = as.integer(request$event_window_start)
  event_window_end = as.integer(request$event_window_end)
  estimation_window_start = as.integer(request$shift_estimation_window) - as.integer(request$estimation_window_length) + 1L
  estimation_window_end = as.integer(request$shift_estimation_window)

  # Extract index of event
  event_index = data_tbl %>%
    filter(event_date == 1) %>%
    .[['tmp_index']]

  if (length(event_index) != 1) {
    # Route through the shared contract dispatch point so strict/lenient
    # behavior is controlled by the caller, not hard-coded here.
    .handle_degenerate(
      mode        = mode,
      condition   = paste0("event date '", request$event_date,
                           "' not found in trading data (or found multiple times)"),
      component   = "prepare_event_study",
      event_id    = event_id,
      firm_symbol = firm_symbol
      # no private_env: this is a free function, not an R6 method
    )
    # Return data with all-zero windows so the model layer's n_valid guard
    # fires cleanly (filter(estimation_window == 1) yields 0 rows) rather
    # than crashing on a missing column or unexpected shape.
    return(
      data_tbl %>%
        dplyr::mutate(
          relative_index    = NA_real_,
          event_window      = 0L,
          estimation_window = 0L
        ) %>%
        dplyr::select(-tmp_index)
    )
  }

  data_tbl = data_tbl %>%
    mutate(relative_index    = tmp_index - event_index,
           event_window      = ifelse((relative_index >= event_window_start) & (relative_index <= event_window_end), 1, 0),
           estimation_window = ifelse((relative_index >= estimation_window_start) & (relative_index <= estimation_window_end), 1, 0)) %>%
    select(-tmp_index)

  data_tbl
}
