#' Prepare data for an Event Study
#'
#' Perform return calculation for each stock and the corresponding reference
#' market defined in the task and the parameter set.
#'
#' @param task An Event Study task.
#' @param parameter_set A parmeter set that defines the Event Study.
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
  task$firm_tbl = task$firm_tbl %>%
    mutate(data = furrr::future_map(.x=data,
                                    .f=.append_returns,
                                    return_calculation=parameter_set$return_calculation,
                                    in_column=task$.target))

  # TBD: Correct event date

  # Append estimation window
  task$firm_tbl = task$firm_tbl %>%
    mutate(data = furrr::future_map2(.x=data,
                                     .y=request,
                                     .f=.append_windows))

  task
}



#' Append returns (simple of log) to given inüput dataframe
#'
#' @param data_tbl A dataframe of a single stock index with reference market data.
#' @param return_calculation An initialized ReturnCalculation class.
#' @param in_column String identifier for the price data.
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
#' request definition. The relative index is according to the specified event
#' date. The index is negative before and positive after the event date. The
#' estimation and event index are used for training the models and perform the
#' test statistic calculations.
#'
#' @param data_tbl A dataframe of a single stock index with reference market data.
#' @param The specification of the Event Study for the given stock in the input
#'        data.
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
