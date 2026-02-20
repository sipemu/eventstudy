#' Event Study Parameter Set
#'
#' The parameter set defines the Event Study, e.g, the return
#' calculation, the event study model, the AR, CAR, AAR, and CAAR test
#' statistics that should be applied.
#'
#' @export
ParameterSet = R6::R6Class(classname = "ParameterSet",
                           lock_objects = FALSE,
                           public = list(
                             #' @field return_calculation A R6 object for calculating returns.
                             return_calculation = NULL,
                             #' @field return_model A R6 object for fitting the desired model.
                             return_model = NULL,
                             #' @field single_event_statistics single event test statistic R6 object.
                             single_event_statistics = NULL,
                             #' @field multi_event_statistics multi event test statistic R6 object.
                             multi_event_statistics = NULL,
                             #' @field study_type Type of event study: "return" (default),
                             #'   "volume", or "volatility". Affects axis labels in plots.
                             study_type = "return",
                             #' @description
                             #' Initialize the parameters that defines the Event Study that should be applied.
                             #'
                             #' @param return_calculation An initialized return calculation class.
                             #'   Defaults to SimpleReturn.
                             #' @param return_model An initialized event study model.
                             #'   Defaults to MarketModel.
                             #' @param single_event_statistics Definition of single event test statistics.
                             #'   Defaults to SingleEventStatisticsSet (AR T and CAR T tests).
                             #' @param multi_event_statistics Definition of multiple event test statistics.
                             #'   Defaults to MultiEventStatisticsSet (CSect T test).
                             initialize = function(return_calculation = SimpleReturn$new(),
                                                   return_model = MarketModel$new(),
                                                   single_event_statistics = SingleEventStatisticsSet$new(),
                                                   multi_event_statistics = MultiEventStatisticsSet$new()) {
                               # Validate return_calculation object
                               private$validate_object(return_calculation, "ReturnCalculation")
                               self$return_calculation = return_calculation

                               # Validate return_model object
                               private$validate_object(return_model, "ModelBase")
                               self$return_model = return_model

                               # Validate single event statistics; Can be null
                               if (!is.null(single_event_statistics)) {
                                 private$validate_object(single_event_statistics, "StatisticsSetBase")
                                 self$single_event_statistics = single_event_statistics
                               }

                               if (!is.null(multi_event_statistics)) {
                                 private$validate_object(multi_event_statistics, "StatisticsSetBase")
                                 self$multi_event_statistics = multi_event_statistics
                               }
                             },
                             #' @description
                             #' Print a summary of the parameter set.
                             print = function(...) {
                               cat("EventStudy ParameterSet\n")
                               cat("  Return calculation:", self$return_calculation$name, "\n")
                               cat("  Return model:     ", self$return_model$model_name, "\n")
                               if (!is.null(self$single_event_statistics)) {
                                 stat_names = purrr::map_chr(self$single_event_statistics$tests, ~.x$name)
                                 cat("  Single event tests:", paste(stat_names, collapse = ", "), "\n")
                               } else {
                                 cat("  Single event tests: none\n")
                               }
                               if (!is.null(self$multi_event_statistics)) {
                                 stat_names = purrr::map_chr(self$multi_event_statistics$tests, ~.x$name)
                                 cat("  Multi event tests: ", paste(stat_names, collapse = ", "), "\n")
                               } else {
                                 cat("  Multi event tests:  none\n")
                               }
                               invisible(self)
                             }
                           ),
                           private = list(
                             validate_object = function(obj, class_name) {
                               is_valid = inherits(obj, class_name)
                               if (!is_valid) {
                                 object_name <- deparse(substitute(obj))
                                 stop(stringr::str_c("The given object:", object_name, " is not of type:", class_name, "!"))
                               }
                             }
                           )
)
