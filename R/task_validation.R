#' Validate an Event Study experiment
#'
#' Validation is done according to:
#'
#' 1. Does the data fit to the request, e.g., is the event date in the stock data.
#' 2. Does the task contains all data in either the estimation and the event window.
#' 3. Does the reference data fits to each defined event and corresponding stock data.
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @export
validate_task <- function(task, parameter_set) {

}


#' Sub-routine for validating the event date
.validate_event_date = function(data_tbl, request) {
  request$event_date
  data_tbl = data_tbl %>%
    dplyr::mutate(event_date = ifelse(date == request$event_date, 1, 0))

  sum(data_tbl$event_date) == 1
}
