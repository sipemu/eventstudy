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
                               #' @field aar_caar_tbl Placeholder for AAR and
                               #' CAAR test statistics.
                               aar_caar_tbl = NULL,
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
                               #' @description Create a new EventStudyTask from stock data, reference
                               #' market data, and an event request specification.
                               #'
                               #' @param firm_stock_data_tbl Dataframe with firm
                               #'   stock data. This dataframe must contain the
                               #'   stock (col: symbol) the date (col: date)
                               #'   and the price (col: adjusted) column.
                               #' @param reference_tbl Dataframe with firm
                               #'   reference data.
                               #' @param request_tbl The request dataframe for
                               #'   each event.
                               initialize = function(firm_stock_data_tbl,
                                                     reference_tbl,
                                                     request_tbl) {
                                 # Validate input:
                                 # Check if necessary columns are in the dataframes.
                                 firm_stock_data_tbl %>% private$check_data_input("Firm table")
                                 reference_tbl %>% private$check_data_input("Reference table")
                                 request_tbl %>% private$check_request_input()

                                 # Rename symbol and price identifier. This is
                                 # necessary as we want to have both price
                                 # data in the same dataframe.
                                 firm_stock_data_tbl = firm_stock_data_tbl %>% private$rename_columns(id="firm")
                                 reference_tbl = reference_tbl %>% private$rename_columns(id="index")

                                 # Join index data to firm data
                                 firm_stock_data_tbl = firm_stock_data_tbl %>%
                                   private$append_index_tbl(reference_tbl, request_tbl)

                                 # Nest stock data: the identifier is the event
                                 # id, the group, and the firm symbol.
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
                               },

                               #' @description
                               #' Print a summary of the EventStudyTask.
                               print = function(...) {
                                 cat("EventStudyTask\n")
                                 n_events = nrow(self$data_tbl)
                                 cat("  Events:  ", n_events, "\n")

                                 groups = unique(self$data_tbl$group)
                                 cat("  Groups:  ", length(groups), "(", paste(groups, collapse=", "), ")\n")

                                 symbols = unique(self$data_tbl$firm_symbol)
                                 cat("  Symbols: ", length(symbols), "\n")

                                 # Check if models have been fitted
                                 has_model = "model" %in% names(self$data_tbl)
                                 cat("  Fitted:  ", has_model, "\n")

                                 # Check if AR has been calculated
                                 if (has_model) {
                                   has_ar = any(purrr::map_lgl(self$data_tbl$data, ~"abnormal_returns" %in% names(.x)))
                                   cat("  AR calc: ", has_ar, "\n")
                                 }

                                 # Check if multi-event stats exist
                                 has_aar = !is.null(self$aar_caar_tbl)
                                 cat("  AAR/CAAR:", has_aar, "\n")

                                 invisible(self)
                               },

                               #' @description
                               #' Summarize the event study results.
                               #'
                               #' @return A list with summary information about the event study results.
                               summary = function() {
                                 result = list()
                                 result$n_events = nrow(self$data_tbl)
                                 result$groups = unique(self$data_tbl$group)
                                 result$symbols = unique(self$data_tbl$firm_symbol)

                                 has_model = "model" %in% names(self$data_tbl)

                                 if (has_model) {
                                   # Model fit summary
                                   result$model_stats = purrr::map(self$data_tbl$model, function(m) {
                                     stats = m$statistics
                                     list(
                                       is_fitted = m$is_fitted,
                                       sigma = stats$sigma,
                                       r2 = stats$r2,
                                       alpha = stats$alpha,
                                       beta = stats$beta
                                     )
                                   })
                                   names(result$model_stats) = self$data_tbl$firm_symbol
                                 }

                                 if (!is.null(self$aar_caar_tbl)) {
                                   result$aar_caar = self$aar_caar_tbl
                                 }

                                 class(result) = "EventStudySummary"
                                 result
                               },

                               #' @description
                               #' Extract abnormal returns for a single event.
                               #'
                               #' @param event_id The event identifier.
                               #'
                               #' @return A tibble with abnormal returns for the event window.
                               get_ar = function(event_id = NULL) {
                                 data = private$get_event_data(event_id)
                                 if (!"abnormal_returns" %in% names(data)) {
                                   stop("Abnormal returns have not been calculated yet. Run fit_model() first.")
                                 }
                                 data %>%
                                   dplyr::filter(event_window == 1) %>%
                                   dplyr::select(relative_index, abnormal_returns)
                               },

                               #' @description
                               #' Extract cumulative abnormal returns for a single event.
                               #'
                               #' @param event_id The event identifier.
                               #'
                               #' @return A tibble with cumulative abnormal returns for the event window.
                               get_car = function(event_id = NULL) {
                                 ar = self$get_ar(event_id)
                                 ar %>%
                                   dplyr::mutate(car = cumsum(abnormal_returns))
                               },

                               #' @description
                               #' Extract average abnormal returns across events.
                               #'
                               #' @param group Optional group name to filter by.
                               #' @param stat_name Name of the multi-event test statistic
                               #'   to extract. Defaults to "CSectT".
                               #'
                               #' @return A tibble with AAR/CAAR results.
                               get_aar = function(group = NULL, stat_name = "CSectT") {
                                 if (is.null(self$aar_caar_tbl)) {
                                   stop("AAR/CAAR have not been calculated yet. Run calculate_statistics() first.")
                                 }
                                 tbl = self$aar_caar_tbl
                                 if (!is.null(group)) {
                                   tbl = tbl %>% dplyr::filter(group == !!group)
                                 }
                                 if (stat_name %in% names(tbl)) {
                                   tbl[[stat_name]]
                                 } else {
                                   stop("Statistic '", stat_name, "' not found. Available: ",
                                        paste(setdiff(names(tbl), c("group", "data", "model")), collapse = ", "))
                                 }
                               },

                               #' @description
                               #' Extract model statistics for a single event.
                               #'
                               #' @param event_id The event identifier.
                               #'
                               #' @return A list with model statistics.
                               get_model_stats = function(event_id = NULL) {
                                 if (!"model" %in% names(self$data_tbl)) {
                                   stop("Models have not been fitted yet. Run fit_model() first.")
                                 }
                                 if (is.null(event_id)) {
                                   event_id = self$data_tbl$event_id[1]
                                 }
                                 row = self$data_tbl %>%
                                   dplyr::filter(event_id == !!event_id)
                                 if (nrow(row) == 0) {
                                   stop("Event ID '", event_id, "' not found.")
                                 }
                                 row$model[[1]]$statistics
                               }
                             ),
                             active = list(
                               #' @field symbols Read-only field to get the firm symbols.
                               symbols = function(value) {
                                 if (missing(value)) {
                                   self$data_tbl$firm_symbol
                                 } else {
                                   stop("`$symbols` is read only", call. = FALSE)
                                 }
                               },
                               #' @field symbol_data Read-only field to get the firm symbol data.
                               symbol_data = function(value) {
                                 if (missing(value)) {
                                   self$data_tbl
                                 } else {
                                   stop("`$symbol_data` is read only", call. = FALSE)
                                 }
                               },
                               #' @field group_level_data Read-only field to get the group data.
                               group_level_data = function(value) {
                                 if (missing(value)) {
                                   self$data_tbl %>%
                                     dplyr::select(self$.keys, data) %>%
                                     tidyr::unnest(data) %>%
                                     dplyr::group_by(group) %>%
                                     tidyr::nest()
                                 } else {
                                   stop("`$group_level_data` is read only", call. = FALSE)
                                 }
                               }
                             ),
                             private = list(
                               get_event_data = function(event_id = NULL) {
                                 if (is.null(event_id)) {
                                   event_id = self$data_tbl$event_id[1]
                                 }
                                 row = self$data_tbl %>%
                                   dplyr::filter(event_id == !!event_id)
                                 if (nrow(row) == 0) {
                                   stop("Event ID '", event_id, "' not found.")
                                 }
                                 row$data[[1]]
                               },
                               check_request_input = function(tbl) {
                                 if (any(! self$.request_file_columns %in% names(tbl))) {
                                   missing_cols = self$.request_file_columns[!self$.request_file_columns %in% names(tbl)]
                                   stop("Request file missing columns: ", paste(missing_cols, collapse = ", "))
                                 }
                               },
                               check_data_input = function(tbl, tbl_name="firm data") {
                                 col_names = names(tbl)
                                 if (! "symbol" %in% col_names) {
                                   stop(stringr::str_c(tbl_name, ": Input dataframe does not contain the stock identifier 'symbol' column."))
                                 }
                                 if (! self$.index %in% col_names) {
                                   stop(stringr::str_c(tbl_name, ": Input dataframe does not contain the date '", self$.index, "' column."))
                                 }
                                 if (! self$.target %in% col_names) {
                                   stop(stringr::str_c(tbl_name, ": Input dataframe does not contain the price '", self$.target, "' column."))
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
                                 request_tbl %>%
                                   dplyr::select(event_id, group, firm_symbol, index_symbol) -> request_join_idx_tbl

                                 firm_tbl %>%
                                   dplyr::left_join(request_join_idx_tbl, by = "firm_symbol") %>%
                                   dplyr::left_join(reference_tbl, by = c("index_symbol", "date"))
                               }
                             )
)


#' Print method for EventStudySummary
#'
#' @param x An EventStudySummary object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.EventStudySummary = function(x, ...) {
  cat("Event Study Summary\n")
  cat("===================\n")
  cat("Events: ", x$n_events, "\n")
  cat("Groups: ", paste(x$groups, collapse = ", "), "\n")
  cat("Symbols:", paste(x$symbols, collapse = ", "), "\n\n")

  if (!is.null(x$model_stats)) {
    cat("Model Statistics:\n")
    for (sym in names(x$model_stats)) {
      s = x$model_stats[[sym]]
      if (isTRUE(s$is_fitted)) {
        cat("  ", sym, ": alpha=", round(s$alpha, 6),
            " beta=", round(s$beta, 4),
            " sigma=", round(s$sigma, 6),
            " R2=", round(s$r2, 4), "\n")
      } else {
        cat("  ", sym, ": NOT FITTED\n")
      }
    }
  }

  invisible(x)
}
