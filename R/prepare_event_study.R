#' Prepare data for an Event Study
#'
prepare_event_study <- function(task, parameter_set) {

  # Calculate returns
  task$firm_tbl = task$firm_tbl %>%
    mutate(data = furrr::future_map(.x=data,
                                    .f=.append_returns,
                                    return_calculation=parameter_set$return_calculation,
                                    in_column=task$.target))

  # Correctevent date

  # Append estimation window
  task$firm_tbl = task$firm_tbl %>%
    mutate(data = furrr::future_map2(.x=data,
                                     .y=request,
                                     .f=.append_windows))

  task
}


validate_task <- function(task, parameter_set) {

}



.append_returns = function(data_tbl, return_calculation, in_column='adjusted') {
  in_cols = colnames(data_tbl)[stringr::str_detect(colnames(data_tbl), in_column)]
  out_cols = stringr::str_replace(in_cols, "adjusted", "returns")

  data_tbl %>%
    return_calculation$calculate_return(in_column=in_cols[1], out_column=out_cols[1]) %>%
    return_calculation$calculate_return(in_column=in_cols[2], out_column=out_cols[2])
}


.validate_event_date = function(data_tbl, request) {
  request$event_date
  data_tbl = data_tbl |>
    mutate(event_date = ifelse(date == request$event_date, 1, 0))

  sum(data_tbl$event_date) == 1
}


.append_windows = function(data_tbl, request) {
  # Add event date & temporary index
  data_tbl = data_tbl |>
    mutate(event_date = ifelse(date == request$event_date, 1, 0)) |>
    mutate(tmp_index  = 1:n())

  # Create columns event_window and estimation_window
  event_window_start = as.integer(request$event_window_start)
  event_window_end = as.integer(request$event_window_end)
  estimation_window_start = as.integer(request$shift_estimation_window) - as.integer(request$estimation_window_length)
  estimation_window_end = as.integer(request$shift_estimation_window)

  # Extract index of event
  event_index = data_tbl |>
    filter(event_date == 1) %>%
    .[['tmp_index']]

  data_tbl = data_tbl |>
    mutate(relative_index    = tmp_index - event_index,
           event_window      = ifelse((relative_index >= event_window_start) & (relative_index <= event_window_end), 1, 0),
           estimation_window = ifelse((relative_index >= estimation_window_start) & (relative_index <= estimation_window_end), 1, 0)) |>
    select(-tmp_index)

  data_tbl
}
