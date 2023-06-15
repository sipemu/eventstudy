#' Base class for test statistic container
#'
#' @export
StatisticsSetBase <- R6Class("StatisticsSetBase",
                             public = list(
                               #' @field tests Container with the statistical tests.
                               tests = list(),
                               #' Test container initialization
                               #'
                               #' @description Initializes a test statistic
                               #' container. Single event and multiple event
                               #' test statistics are collected in separated
                               #' containers.
                               #'
                               #' @param tests List of test statistics.
                               initialize = function(tests=NULL) {
                                 if (is.null(tests))
                                   return(NULL)

                                 if (!inherits(tests, "list")) {
                                   stop("The input must be a list!")
                                 }
                                 # Validate objects
                                 for (test in tests) {
                                   private$validate(test)
                                 }
                                 selftests = tests
                               },
                               #' @title Adds a test statistic to this container.
                               #'
                               #' @param test A single event study tests.
                               add_test = function(test) {
                                 private$validate(test)
                                 self$tests = c(self$tests, test)
                               }
                             ),
                             private = list(
                               test_parent_class = "",
                               validate = function(test_obj) {
                                 if (! inherits(test_obj, private$test_parent_class)) {
                                   stop(stringr::str_c("Test object must be of class: ", private$test_parent_class))
                                 }
                               }
                             )
)



#' Single Event Statistic Set
#'
#' Contains a set of single event test statistics as, e.g., AR and CAR t test.
#' These are the default test statistics.
#'
#' @export
SingleEventStatisticsSet <- R6Class("SingleEventStatisticsSet",
                                    inherit = StatisticsSetBase,
                                    public = list(
                                      #' @field tests Container with the statistical tests.
                                      tests = list(ARTTest$new(), CARTTest$new())
                                    ),
                                    private = list(
                                      test_parent_class = "SingleEventTestStatistic"
                                    )
)


#' Multi Event Statistic Set
#'
#' Contains a set of single event test statistics as, e.g., AR and CAR t test.
#' These are the default test statistics.
#'
#' @export
MultiEventStatisticsSet <- R6Class("MultiEventStatisticsSet",
                                   inherit = StatisticsSetBase,
                                   private = list(
                                     test_parent_class = "MultiEventTestStatistic"
                                   )
)

