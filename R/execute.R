#' Run a Complete Event Study
#'
#' Convenience wrapper that runs the full event study pipeline: prepare data,
#' fit models, and calculate test statistics in a single call.
#'
#' @param task An EventStudyTask object.
#' @param parameter_set A ParameterSet object defining the event study.
#'   Defaults to a new ParameterSet with default settings.
#'
#' @return The task object with all results computed.
#'
#' @export
run_event_study = function(task, parameter_set = ParameterSet$new()) {
  task = prepare_event_study(task, parameter_set)
  task = fit_model(task, parameter_set)
  task = calculate_statistics(task, parameter_set)
  task
}


#' Train the defined model on each event and calculate the abnormal return.
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @return task
#'
#' @export
fit_model = function(task, parameter_set) {
  # Fit a return model for each event
  task$data_tbl = task$data_tbl %>%
    mutate(model = purrr::map(.x=data,
                              .f=.initialize_and_fit_model,
                              return_model=parameter_set$return_model))

  # Calculate abnormal returns for each event
  task$data_tbl = task$data_tbl %>%
    dplyr::mutate(data = purrr::map2(.x=data,
                                     .y=model,
                                     .f=.calculate_abnormal_returns))
  task
}


#' @noRd
.initialize_and_fit_model <- function(data_tbl, return_model) {
  # Each event needs its own model, therefore a deep clone is necessary
  cloned_return_model = return_model$clone(deep=TRUE)
  cloned_return_model$fit(data_tbl)
  cloned_return_model
}


#' @noRd
.calculate_abnormal_returns <- function(data_tbl, return_model) {
  return_model$abnormal_returns(data_tbl)
}


#' Calculate test statistics
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @return task
#'
#' @export
calculate_statistics = function(task, parameter_set) {
  # Single event statistic calculation
  if (!is.null(parameter_set$single_event_statistics)) {
    task$data_tbl = task$data_tbl %>%
      dplyr::mutate(statistics = purrr::map2(.x=data,
                                             .y=model,
                                             .f=.calculate_single_event_test_statistics,
                                             statistic_set=parameter_set$single_event_statistics))

    # Transpose results such that each test statistic result has its own column
    task$data_tbl$statistics %>%
      purrr::transpose() %>%
      as_tibble() -> stats_tbl
    task$data_tbl = cbind(task$data_tbl, stats_tbl) %>%
      dplyr::select(-statistics)
  }

  # Multiple events test statistic calculation
  if (!is.null(parameter_set$multi_event_statistics)) {
    # The data must be reshaped for these calculations as we need to consider the
    # grouping of the events.
    task$data_tbl %>%
      dplyr::select(task$.keys, model) %>%
      dplyr::group_by(group) %>%
      tidyr::nest() %>%
      dplyr::rename(model = data) -> model_tbl

    task$aar_caar_tbl = task$data_tbl %>%
      dplyr::select(task$.keys, data) %>%
      tidyr::unnest(data) %>%
      dplyr::group_by(group) %>%
      tidyr::nest() %>%
      dplyr::left_join(model_tbl, by="group") %>%
      dplyr::mutate(statistics = purrr::map2(.x=data,
                                             .y=model,
                                             .f = .calculate_multiple_event_test_statistics,
                                             statistic_set=parameter_set$multi_event_statistics))

    # Transpose results such that each test statistic result has its own column
    task$aar_caar_tbl$statistics %>%
      purrr::transpose() %>%
      as_tibble() -> stats_tbl
    task$aar_caar_tbl = cbind(task$aar_caar_tbl, stats_tbl) %>%
      dplyr::select(-statistics)
  }

  task
}


#' @noRd
.calculate_single_event_test_statistics = function(data_tbl, fitted_model, statistic_set) {
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


#' @noRd
.calculate_multiple_event_test_statistics = function(data_tbl, model, statistic_set) {
  # Calculate test statistics
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic, data_tbl) {
      data_tbl %>%
        test_statistic$compute(model)
    }, data_tbl=data_tbl) -> res

  # Extract names
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic) {
      test_statistic$name
    }) -> stat_names

  names(res) = stat_names
  res
}
