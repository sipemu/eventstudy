#' Train the defined model on each event and calculate the abnormal return.
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @export
execute_model = function(task, parameter_set) {
  # TODO: Check input & check data initialization

  task$data_tbl = task$data_tbl %>%
    mutate(model = furrr::future_map(.x=data,
                                     .f=.initialize_and_fit_model,
                                     return_model=parameter_set$return_model))

  task$data_tbl = task$data_tbl %>%
    mutate(data = furrr::future_map2(.x=data,
                                     .y=model,
                                     .f=.calculate_abnormal_returns))
  task
}


#' Calculate event test statistics
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @export
execute_single_event_statistics = function(task, parameter_set) {
  # TODO: Check input, check data initialization, and modell fitting

  # Single event statistic calculation
  task$data_tbl = task$data_tbl %>%
    dplyr::mutate(ar_statistics = furrr::future_map2(.x=data,
                                                     .y=model,
                                                     .f=.calculate_ar_test_statistics,
                                                     ar_statistics=parameter_set$ar_test_statistics))

  # Multi event test calculation
  task$data_tbl = task$data_tbl %>%
    dplyr::mutate(car_statistics = furrr::future_map2(.x=data,
                                                      .y=model,
                                                      .f=.calculate_car_test_statistics,
                                                      car_statistics=parameter_set$car_test_statistics))
  task
}


.initialize_and_fit_model <- function(data_tbl, return_model) {
  # Each event needs its own model
  market_model = return_model$clone(deep=TRUE)
  market_model$fit(data_tbl)
  market_model
}


.calculate_abnormal_returns <- function(data_tbl, return_model) {
  return_model$abnormal_returns(data_tbl)
}


.calculate_ar_test_statistics = function(data_tbl, return_model, ar_statistics) {
  ar_statistics$compute(data_tbl, return_model)
}

.calculate_car_test_statistics = function(data_tbl, return_model, car_statistics) {
  car_statistics$compute(data_tbl, return_model)
}
