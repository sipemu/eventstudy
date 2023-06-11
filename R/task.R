EventStudyTask = R6::R6Class(classname = "EventStudyTask",
                             public = list(
                               firm_tbl = NULL,
                               .keys = c('event_id', 'group', 'firm_symbol'),
                               .index = 'date',
                               .target = 'adjusted',
                               initialize = function(firm_tbl,
                                                     reference_tbl,
                                                     request_tbl) {
                                 # validate input
                                 firm_tbl %>% private$check_data_input()
                                 reference_tbl %>% private$check_data_input()
                                 request_tbl %>% private$check_request_input()

                                 # Prepare tables
                                 firm_tbl = firm_tbl %>% private$rename_columns(id="firm")
                                 reference_tbl = reference_tbl %>% private$rename_columns(id="index")

                                 # Join index data to firm data
                                 firm_tbl = firm_tbl %>%
                                   private$append_index_tbl(reference_tbl, request_tbl)

                                 # Nest data
                                 self$firm_tbl = firm_tbl %>%
                                   group_by(event_id, group, firm_symbol) %>%
                                   tidyr::nest()

                                 request_tbl = request_tbl %>%
                                   group_by(event_id, group, firm_symbol) %>%
                                   tidyr::nest() %>%
                                   rename("request" = "data")

                                 self$firm_tbl = self$firm_tbl %>%
                                   left_join(request_tbl, by=self$.keys)
                               }
                             ),
                             private = list(
                               check_request_input = function(tbl) {

                               },
                               check_data_input = function(tbl) {

                               },
                               rename_columns = function(tbl, id="firm") {
                                 symbol_name = stringr::str_c(id, "_symbol")
                                 price_name = stringr::str_c(id, "_", self$.target)
                                 tbl %>%
                                   rename(!!symbol_name := "symbol",
                                          !!price_name := self$.target)
                               },
                               validate_request = function() {

                               }
                             )
)


EventStudyRequest <- R6Class("EventStudyRequest",
                             public = list(
                               initialize = function(request_tbl) {
                                 private$check_cols(request_tbl)
                                 private$request_tbl = request_tbl


                               }
                             ),
                             active = list(
                               request_tbl = function(value) {
                                 private$.request_tbl
                               }
                             ),
                             private = list(
                               .request_tbl = NULL,
                               .request_tbl_columns = c("event_id", "firm_symbol", "index_symbol", "event_date",
                                                        "group", "event_window_start", "event_window_end",
                                                        "shift_estimation_window", "estimation_window_length"),
                               check_cols = function(request_tbl) {

                               }
                             )
)

