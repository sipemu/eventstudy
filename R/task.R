#' Event Study Task
#'
#' The Event Study task contains all necessary data for performing an event
#' study. Furthermore, all calculations are saved in the internal dataframe
#' named data_tbl.
#'
#' @export
EventStudyTask = R6::R6Class(classname = "EventStudyTask",
                             public = list(
                               #' @field data_tbl All calculations are saved in
                               #' this dataframe. The dataframe is a nested
                               #' object with each row is one event, identified
                               #' by the event id, the group, and the firm
                               #' symbol.
                               data_tbl = NULL,
                               #' @field .keys event identifier. Do not change.
                               .keys = c('event_id', 'group', 'firm_symbol'),
                               #' @field .index The time column name
                               .index = 'date',
                               #' @field .target The price column name.
                               .target = 'adjusted',
                               #' @field .request_file_columns The necessary
                               #'   column names of the request dataframe.
                               .request_file_columns = c("event_id", "firm_symbol", "index_symbol", "event_date",
                                                         "group", "event_window_start", "event_window_end",
                                                         "shift_estimation_window", "estimation_window_length"),
                               #' @title Initialization of an Event Study task.
                               #'
                               #' @description desc
                               #'
                               #' @param firm_stock_data_tbl Dataframe with firm
                               #'   stock data. This dataframe must contain the
                               #'   stock (col: symbol) the date (col: date)
                               #'   and the price (col: adjusted) column.
                               #' @param reference_tbl Dataframe with firm
                               #'   reference data.
                               #' @param request_tbl The requst dataframe for
                               #'   each event.
                               initialize = function(firm_stock_data_tbl,
                                                     reference_tbl,
                                                     request_tbl) {
                                 # Validate input:
                                 # Check if nexessary columns are in the dataframes.
                                 firm_stock_data_tbl %>% private$check_data_input("Firm table")
                                 reference_tbl %>% private$check_data_input("Reference table")
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
                                   dplyr::rename("request" = "data")

                                 self$data_tbl = self$data_tbl %>%
                                   dplyr::left_join(request_tbl, by=self$.keys)
                               }
                             ),
                             private = list(
                               check_request_input = function(tbl) {
                                 if (any(! self$.request_file_columns %in% names(tbl))) {
                                   stop("Request file column names are not properly defined!")
                                 }
                               },
                               check_data_input = function(tbl, tbl_name="firm data") {
                                 # Validate
                                 col_names = names(tbl)
                                 # Check if date variable is in the dataframe and
                                 # is a date variable
                                 if (! "symbol" %in% col_names) {
                                   stop(stringr::str_c(tbl_name, ": Input dataframe do not contain the stock identifier 'symbol' column."))
                                 }

                                 # Check if date variable is in the dataframe and
                                 # is a date variable
                                 if (! self$.index %in% col_names) {
                                   stop(stringr::str_c(tbl_name, ": Input dataframe do not contain the date '", self$.index, "' column."))
                                 }

                                 # Check target variable
                                 if (! self$.target %in% col_names) {
                                   stop(stringr::str_c(tbl_name, ": Input dataframe do not contain the price '", self$.target, "' column."))
                                 }
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
