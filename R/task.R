EventStudyTask = R6::R6Class(classname = "EventStudyTask",
                             public = list(
                               data_tbl = NULL,
                               .keys = c('event_id', 'group', 'firm_symbol'),
                               .index = 'date',
                               .target = 'adjusted',
                               initialize = function(firm_stock_data_tbl,
                                                     reference_tbl,
                                                     request_tbl) {
                                 # Validate input:
                                 #
                                 firm_stock_data_tbl %>% private$check_data_input()
                                 reference_tbl %>% private$check_data_input()
                                 request_tbl %>% private$check_request_input()

                                 # Rename symbol and price identifier. This is
                                 # necessary as we want to have both price
                                 # data in the same dataframe.
                                 # firm stock data:
                                 #  symbol -> firm_symbol,
                                 #  adjusted -> firm_adjusted
                                 # reference stock
                                 #  data: symbol -> index_symbol,
                                 #  adjusted -> index_adjusted
                                 firm_stock_data_tbl = firm_stock_data_tbl %>% private$rename_columns(id="firm")
                                 reference_tbl = reference_tbl %>% private$rename_columns(id="index")

                                 # Join index data to firm data
                                 # TODO: Non-unique mapping between firm and
                                 # reference must be tested, e.g. multiple
                                 # events per firm with different reference
                                 # markets.
                                 firm_stock_data_tbl = firm_stock_data_tbl %>%
                                   private$append_index_tbl(reference_tbl, request_tbl)

                                 # Nest stock data: the identifier is the event
                                 # id, the group, and the firm symbol. The data
                                 # is saved in.
                                 self$data_tbl = firm_stock_data_tbl %>%
                                   dplyr::group_by(event_id, group, firm_symbol) %>%
                                   tidyr::nest()

                                 # Nest request data similarly as the stock data
                                 request_tbl = request_tbl %>%
                                   dplyr::group_by(event_id, group, firm_symbol) %>%
                                   tidyr::nest() %>%
                                   rename("request" = "data")

                                 self$data_tbl = self$data_tbl %>%
                                   dplyr::left_join(request_tbl, by=self$.keys)
                               }
                             ),
                             private = list(
                               check_request_input = function(tbl) {

                               },
                               check_data_input = function(tbl) {
                                 # Check if keys are in the dataframe
                                 col_names = names(tbl)

                                 # Check if date variable is in the dataframe and
                                 # is a date variable

                                 # Check target variable

                               },
                               rename_columns = function(tbl, id="firm") {
                                 symbol_name = stringr::str_c(id, "_symbol")
                                 price_name = stringr::str_c(id, "_", self$.target)
                                 tbl %>%
                                   rename(!!symbol_name := "symbol",
                                          !!price_name := self$.target)
                               },
                               append_index_tbl = function(firm_tbl, reference_tbl, request_tbl) {
                                 # Link between stock data and reference data is
                                 # given in the request file
                                 # Multiple reference data sources are not
                                 # tested yet.
                                 request_tbl %>%
                                   dplyr::select(event_id, group, firm_symbol, index_symbol) -> request_join_idx_tbl

                                 firm_tbl %>%
                                   dplyr::left_join(request_join_idx_tbl, by = "firm_symbol") %>%
                                   dplyr::left_join(reference_tbl, by = c("index_symbol", "date"))
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

