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

  # Calculate returns
  task$data_tbl = task$data_tbl %>%
    mutate(data = purrr::map(.x=data,
                             .f=.append_returns,
                             return_calculation=parameter_set$return_calculation,
                             in_column=task$.target))

  # Append estimation window
  task$data_tbl = task$data_tbl %>%
    mutate(data = purrr::map2(.x=data,
                              .y=request,
                              .f=.append_windows))

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
  out_cols = stringr::str_replace(in_cols, "adjusted", "returns")

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
#'
#' @noRd
.append_windows = function(data_tbl, request) {
  # Add event date & temporary index
  data_tbl = data_tbl %>%
    mutate(event_date = ifelse(date == request$event_date, 1, 0)) %>%
    mutate(tmp_index  = 1:n())

  # Create columns event_window and estimation_window
  event_window_start = as.integer(request$event_window_start)
  event_window_end = as.integer(request$event_window_end)
  estimation_window_start = as.integer(request$shift_estimation_window) - as.integer(request$estimation_window_length)
  estimation_window_end = as.integer(request$shift_estimation_window)

  # Extract index of event
  event_index = data_tbl %>%
    filter(event_date == 1) %>%
    .[['tmp_index']]

  data_tbl = data_tbl %>%
    mutate(relative_index    = tmp_index - event_index,
           event_window      = ifelse((relative_index >= event_window_start) & (relative_index <= event_window_end), 1, 0),
           estimation_window = ifelse((relative_index >= estimation_window_start) & (relative_index <= estimation_window_end), 1, 0)) %>%
    select(-tmp_index)

  data_tbl
}
