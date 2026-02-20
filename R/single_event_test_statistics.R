#' Base class for Event Study test statistics
TestStatisticBase <- R6Class("TestStatisticBase",
                                    public = list(
                                      #' @field name Short code of the test statistic.
                                      name = 'TestStatistics',
                                      #' @field confidence_level The chosen confidence
                                      #' level.
                                      confidence_level = 0.95,
                                      #' @field confidence_type Type of the test. Defaults
                                      #' to 'two-sided'. Alternatives are 'less' or
                                      #' greater'.
                                      confidence_type = 'two-sided',
                                      #' @description
                                      #' Initializes the test statistic. This includes the
                                      #' confidence level and the type of the test ('less',
                                      #' greater' or 'two-sided')
                                      #'
                                      #' @param confidence_level The confidence level for
                                      #' the confidence band. Must be anumber between 0
                                      #' and 1.
                                      #' @param confidence_type Side of the test statistic.
                                      initialize = function(confidence_level=0.95, confidence_type='two-sided') {
                                        self$confidence_level = confidence_level
                                        self$confidence_type = confidence_type
                                      },
                                      #' @description
                                      #' Computes the test test statistics for a single event.
                                      #'
                                      #' @param data_tbl The data for a single event with
                                      #' calculated abnormal returns.
                                      #' @param model The fitted model that includes the
                                      #' necessary information for calculating the test
                                      #' statistic.
                                      compute = function(data_tbl, model) {

                                      }
                                    )
)


#' Abnormal Return T Statistic (ART)
#'
#' The AR t-test is a statistical method used to determine whether the abnormal
#' return of a security on a specific day is significantly different from zero.
#' This test helps researchers identify whether the event of interest has a
#' significant impact on the security’s return at a particular point in time.
#'
#' See also \url{https://eventstudy.de/statistics/ar_car_statistics.html}
#'
#' @export
ARTTest <- R6Class("ARTTest",
                   inherit = TestStatisticBase,
                   public = list(
                     #' @field name Short code of the test statistic.
                     name = 'ART',
                     #' @description
                     #' Computes the test AR test statistics for a single event.
                     #'
                     #' @param data_tbl The data for a single event with
                     #' calculated abnormal returns.
                     #' @param model The fitted model that includes the
                     #' necessary information for calculating the test
                     #' statistic.
                     compute = function(data_tbl, model) {
                       statistics = model$statistics
                       sigma = statistics$sigma
                       degree_of_freedom = statistics$degree_of_freedom

                       res = data_tbl %>%
                         dplyr::filter(event_window == 1) %>%
                         dplyr::select(relative_index, abnormal_returns) %>%
                         dplyr::mutate(ar_t      = abnormal_returns / sigma,
                                       ar_t_dist = distributional::dist_student_t(degree_of_freedom))
                       res
                     }
                   )
)


#' Cumulative Abnormal Return T Statistic (CART)
#'
#' The CAR t-test is a statistical method used to determine whether the
#' cumulative abnormal return of a security over an event window is
#' significantly different from zero. This test helps researchers identify
#' whether the event of interest has a significant impact on the security’s
#' return over the entire event window, considering the cumulative effects of
#' the event.
#'
#' See also \url{https://eventstudy.de/statistics/ar_car_statistics.html}
#'
#' @export
CARTTest <- R6Class("CARTTest",
                    inherit = TestStatisticBase,
                    public = list(
                      #' @field name Short code of the test statistic.
                      name = 'CART',
                      #' @description
                      #' Computes the test CAR test statistics for a single event.
                      #'
                      #' @param data_tbl The data for a single event with
                      #' calculated abnormal returns.
                      #' @param model The fitted model that includes the
                      #' necessary information for calculating the test
                      #' statistic.
                      compute = function(data_tbl, model) {
                        statistics = model$statistics
                        sigma = statistics$sigma
                        degree_of_freedom = statistics$degree_of_freedom

                        res = data_tbl %>%
                          dplyr::filter(event_window == 1) %>%
                          dplyr::select(relative_index, abnormal_returns) %>%
                          dplyr::mutate(event_window_length = 1:dplyr::n(),
                                        car_window          = "",
                                        car                 = cumsum(abnormal_returns),
                                        corrected_car       = car / sigma,
                                        car_t               = car / (sqrt(event_window_length) * sigma),
                                        car_t_dist          = distributional::dist_student_t(df=degree_of_freedom, mu=car, sigma=sigma))
                        res$car_window = stringr::str_c("[", res$relative_index[1], ", ", res$relative_index, "]")
                        res
                      }
                    )
)


PermutationTest <- R6Class("PermutationTest",
                            inherit = TestStatisticBase,
                            public = list(
                              #' @field name Short code of the test statistic.
                              name = 'PermutationTest',
                              #' @description
                              #' Computes the test CAR test statistics for a single event.
                              #'
                              #' @param data_tbl The data for a single event with
                              #' calculated abnormal returns.
                              #' @param model The fitted model that includes the
                              #' necessary information for calculating the test
                              #' statistic.
                              compute = function(data_tbl, model) {

                              })
)


#' Buy-and-Hold Abnormal Return T Test (BHARTTest)
#'
#' Tests whether the BHAR for a single event is significantly different from
#' zero. The BHAR is the difference between compounded firm returns and
#' compounded benchmark returns over the event window.
#'
#' @export
BHARTTest <- R6Class("BHARTTest",
                      inherit = TestStatisticBase,
                      public = list(
                        #' @field name Short code of the test statistic.
                        name = 'BHART',
                        #' @description
                        #' Computes the BHAR t test for a single event.
                        #'
                        #' @param data_tbl The data for a single event with
                        #' calculated abnormal returns.
                        #' @param model The fitted model.
                        compute = function(data_tbl, model) {
                          statistics <- model$statistics
                          sigma <- statistics$sigma

                          event_data <- data_tbl %>%
                            dplyr::filter(event_window == 1)

                          # Compound returns
                          cum_firm <- cumprod(1 + dplyr::coalesce(event_data$firm_returns, 0))
                          cum_index <- cumprod(1 + dplyr::coalesce(event_data$index_returns, 0))
                          bhar <- cum_firm - cum_index

                          n <- seq_len(nrow(event_data))
                          # Under simple approximation, sigma of BHAR grows with sqrt(n)
                          bhar_se <- sigma * sqrt(n)

                          res <- event_data %>%
                            dplyr::select(relative_index) %>%
                            dplyr::mutate(
                              bhar = bhar,
                              bhar_window = stringr::str_c("[", relative_index[1], ", ",
                                                           relative_index, "]"),
                              bhar_se = bhar_se,
                              bhar_t = bhar / bhar_se
                            )
                          res
                        }
                      )
)


