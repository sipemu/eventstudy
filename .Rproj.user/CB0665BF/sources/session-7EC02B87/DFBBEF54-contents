#' ParameterSet
#'
#' @description
#'
#'
#'
ParameterSet = R6::R6Class(classname = "ParameterSet",
                           lock_objects = FALSE,
                           public = list(
                             return_calculation = NULL,
                             return_model = NULL,
                             ar_test_statistics = NULL,
                             car_test_statistics = NULL,
                             aar_test_statistics = NULL,
                             caar_test_statistics = NULL,
                             initialize = function(return_calculation = SimpleReturn$new(),
                                                   return_model = MarketModel$new(),
                                                   ar_test_statistics=NULL,
                                                   car_test_statistics=NULL,
                                                   cumulative_test_statistics=NULL) {

                               private$validate_object(return_calculation, "ReturnCalculation")
                               self$return_calculation = return_calculation

                               private$validate_object(return_model, "ModelBase")
                               self$return_model = return_model

                               self$ar_test_statistics = ar_test_statistics
                               self$car_test_statistics = car_test_statistics
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
