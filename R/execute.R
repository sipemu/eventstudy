#' Train the defined model on each event and calculate the abnormal return.
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @return task
#'
#' @export
fit_model = function(task, parameter_set) {
  # TODO: Check input & check data initialization

  # Fit a return model for each event
  task$data_tbl = task$data_tbl %>%
    mutate(model = furrr::future_map(.x=data,
                                     .f=.initialize_and_fit_model,
                                     return_model=parameter_set$return_model))

  # Calculate abnormal returns for each event
  task$data_tbl = task$data_tbl %>%
    dplyr::mutate(data = furrr::future_map2(.x=data,
                                            .y=model,
                                            .f=.calculate_abnormal_returns))
  task
}


#'
.initialize_and_fit_model <- function(data_tbl, return_model) {
  # Each event needs its own model, therefore a deep clone is necessary
  cloned_return_model = return_model$clone(deep=TRUE)
  cloned_return_model$fit(data_tbl)
  cloned_return_model
}


.calculate_abnormal_returns <- function(data_tbl, return_model) {
  return_model$abnormal_returns(data_tbl)
}


#' Calculate test statistics
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @export
calculate_statistics = function(task, parameter_set) {
  # TODO: Check input, check data initialization, and modell fitting

  # Single event statistic calculation
  if (!is.null(parameter_set$single_event_statistics)) {
    task$data_tbl = task$data_tbl %>%
      dplyr::mutate(statistics = furrr::future_map2(.x=data,
                                                    .y=model,
                                                    .f=.calculate_single_event_test_statistics,
                                                    statistic_set=parameter_set$single_event_statistics))

    # Transpose results such that each test statistic result has its own column
    est_task$data_tbl$statistics %>%
      purrr::transpose() %>%
      as_tibble() -> stats_tbl
    est_task$data_tbl = cbind(est_task$data_tbl, stats_tbl) %>%
      dplyr::select(-statistics)
  }

  # Multiple events test statistic calculation
  if (!is.null(parameter_set$multi_event_statistics)) {
    # The data must be reshaped for these calculations as we need to consider the
    # grouping of the events.
    task$aar_caar_tbl = task$data_tbl %>%
      dplyr::select(est_task$.keys, data) %>%
      tidyr::unnest(data) %>%
      dplyr::group_by(group) %>%
      tidyr::nest() %>%
      dplyr::mutate(statistics = furrr::future_map(.x=data,
                                                   .f = .calculate_multiple_event_test_statistics,
                                                   statistic_set=parameter_set$multi_event_statistics))

    # Transpose results such that each test statistic result has its own column
    est_task$aar_caar_tbl$statistics %>%
      purrr::transpose() %>%
      as_tibble() -> stats_tbl
    est_task$aar_caar_tbl = cbind(est_task$aar_caar_tbl, stats_tbl) %>%
      dplyr::select(-statistics)
  }

  task
}


#' Internal method for calculating test statistics for a single event
#'
#'
.calculate_single_event_test_statistics = function(data_tbl, fitted_model, statistic_set) {
  # test if model is fitted and abnormal returns are calculated

  # Calculate test statistics
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic, data_tbl, fitted_model) {
      data_tbl %>%
        test_statistic$compute(fitted_model)
    }, data_tbl=data_tbl, fitted_model=fitted_model) -> res

  # Extract names
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic) {
      test_statistic$name
    }) -> stat_names

  names(res) = stat_names
  res
}



#' Internal method for calculating test statistics for a multiple event
#'
#'
.calculate_multiple_event_test_statistics = function(data_tbl, statistic_set) {
  # Calculate test statistics
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic, data_tbl) {
      data_tbl %>%
        test_statistic$compute(NULL)
    }, data_tbl=data_tbl) -> res

  # Extract names
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic) {
      print(test_statistic$name)
      test_statistic$name
    }) -> stat_names

  names(res) = stat_names
  res
}
