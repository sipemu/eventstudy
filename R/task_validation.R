validate_task <- function(task, parameter_set) {

}

.validate_event_date = function(data_tbl, request) {
  request$event_date
  data_tbl = data_tbl %>%
    mutate(event_date = ifelse(date == request$event_date, 1, 0))

  sum(data_tbl$event_date) == 1
}
