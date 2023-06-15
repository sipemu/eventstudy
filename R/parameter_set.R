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
                             #' @description
                             #' Initialize the parameters that defines the Event Study that should be applied.
                             #'
                             #' @param return_calculation An initialized return calculation class.
                             #' @param return_model An initialized event study model.
                             #' @param single_event_statistics Definition of single event test statistics.
                             #' @param multi_event_statistics Definition of multiple event test statistics.
                             initialize = function(return_calculation = SimpleReturn$new(),
                                                   return_model = MarketModel$new(),
                                                   single_event_statistics=NULL,
                                                   multi_event_statistics=NULL) {
                               # Validate return_calculation object
                               private$validate_object(return_calculation, "ReturnCalculation")
                               self$return_calculation = return_calculation

                               # Validate return_model object
                               private$validate_object(return_model, "ModelBase")
                               self$return_model = return_model

                               private$validate_object(single_event_statistics, "SingleEventStatisticsSet")
                               self$single_event_statistics = single_event_statistics

                               private$validate_object(multi_event_statistics, "StatisticsSetBase")
                               self$multi_event_statistics = multi_event_statistics
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
