#' @title TestStatisticBase
#' @description Base (abstract) class for single-event and multi-event test
#' statistics. Subclass this to plug a custom test statistic into
#' \code{calculate_statistics()} / \code{run_event_study()}.
#'
#' A subclass must implement \code{compute(data_tbl, model)}:
#' \itemize{
#'   \item \code{data_tbl} -- a single event's rows with an
#'     \code{abnormal_returns} column already populated by the fitted model.
#'   \item \code{model} -- the fitted \code{ModelBase} (or list-mock)
#'     instance for that event; its \code{statistics} field carries
#'     \code{sigma}, \code{degree_of_freedom}, \code{residuals} and (since
#'     2026-09-24) \code{n_params}.
#' }
#' \code{compute()} returns a tibble with the statistic's result columns for
#' that event; multi-event subclasses (see \code{R/multi_event_test_statistics.R})
#' aggregate across all events in a group.
#'
#' \code{confidence_level} and \code{confidence_type} are validated and
#' stored at construction (\code{confidence_type} must be one of
#' \code{"two-sided"} (default), \code{"less"} or \code{"greater"}); every
#' p-value computed anywhere in EventStudy is currently two-sided, so a
#' non-default \code{confidence_type} is accepted but ignored, with one
#' warning at construction (A10, 2026-09-24).
#'
#' @family eventstudy-statistics
#' @export
TestStatisticBase <- R6Class("TestStatisticBase",
                                    public = list(
                                      #' @field name Short code of the test statistic.
                                      name = 'TestStatistics',
                                      #' @field confidence_level The chosen confidence
                                      #' level.
                                      confidence_level = 0.95,
                                      #' @field confidence_type Side of the test. One of
                                      #' \code{"two-sided"} (default), \code{"less"} or
                                      #' \code{"greater"}. A10 (2026-09-24): every p-value
                                      #' computed anywhere in EventStudy is currently
                                      #' two-sided; a non-default value is validated but
                                      #' has NO effect on any computed statistic -- it is
                                      #' stored and a one-time warning is emitted at
                                      #' construction. One-sided p-values are not
                                      #' implemented.
                                      confidence_type = 'two-sided',
                                      #' @description
                                      #' Initializes the test statistic. This includes the
                                      #' confidence level and the type of the test ('less',
                                      #' greater' or 'two-sided')
                                      #'
                                      #' @param confidence_level The confidence level for
                                      #' the confidence band. Must be anumber between 0
                                      #' and 1.
                                      #' @param confidence_type Side of the test statistic:
                                      #' \code{"two-sided"} (default), \code{"less"} or
                                      #' \code{"greater"}. Every p-value in EventStudy is
                                      #' currently two-sided; supplying \code{"less"} or
                                      #' \code{"greater"} emits one warning that the value
                                      #' is stored but ignored by every compute() method.
                                      initialize = function(confidence_level=0.95, confidence_type='two-sided') {
                                        confidence_type <- match.arg(confidence_type,
                                                                      c("two-sided", "less", "greater"))
                                        if (confidence_type != "two-sided") {
                                          warning(
                                            "confidence_type = \"", confidence_type, "\" is currently ",
                                            "ignored: all p-values in EventStudy are two-sided.",
                                            call. = FALSE
                                          )
                                        }
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
#' significant impact on the security's return at a particular point in time.
#'
#' See also \url{https://eventstudy.de/statistics/ar_car_statistics.html}
#'
#' @family eventstudy-statistics
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
                       sigma = statistics$sigma %||% NA_real_
                       degree_of_freedom = max(statistics$degree_of_freedom %||% 1, 1)

                       # Guard: sigma == 0 or NA \u2192 ar_t is NA (not Inf/NaN).
                       # The %||% above handles NULL; this handles zero/near-zero.
                       sigma_degenerate <- is.na(sigma) || sigma < .Machine$double.eps

                       res = data_tbl %>%
                         dplyr::filter(event_window == 1) %>%
                         dplyr::select(relative_index, abnormal_returns) %>%
                         dplyr::mutate(ar_t      = if (sigma_degenerate) NA_real_
                                                    else abnormal_returns / sigma,
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
#' whether the event of interest has a significant impact on the security's
#' return over the entire event window, considering the cumulative effects of
#' the event.
#'
#' See also \url{https://eventstudy.de/statistics/ar_car_statistics.html}
#'
#' @family eventstudy-statistics
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
                        sigma = statistics$sigma %||% NA_real_
                        degree_of_freedom = max(statistics$degree_of_freedom %||% 1, 1)

                        # Guard: sigma == 0 or NA \u2192 car_t is NA (not Inf/NaN).
                        # cumsum(abnormal_returns) NA cascade on degenerate model
                        # is intentional and correct \u2014 do not coalesce it here.
                        # The distributional sigma uses pmax(...) to prevent its
                        # own crash; car_t itself is guarded separately.
                        sigma_degenerate <- is.na(sigma) || sigma < .Machine$double.eps

                        res = data_tbl %>%
                          dplyr::filter(event_window == 1) %>%
                          dplyr::select(relative_index, abnormal_returns) %>%
                          dplyr::mutate(event_window_length = 1:dplyr::n(),
                                        car_window          = "",
                                        car                 = cumsum(abnormal_returns),
                                        corrected_car       = if (sigma_degenerate) NA_real_
                                                               else car / sigma,
                                        car_t               = if (sigma_degenerate) NA_real_
                                                               else car / (sqrt(event_window_length) * sigma),
                                        car_t_dist          = distributional::dist_student_t(
                                          df    = degree_of_freedom,
                                          mu    = car,
                                          sigma = pmax(sqrt(event_window_length) * sigma, .Machine$double.eps)
                                        ))

                        # Guard: long-window CAR cumulation OVERFLOW. When the running
                        # cumsum exceeds the representable range it becomes Inf/NaN, and
                        # car_t would report a MISLEADING infinite test statistic. On an
                        # OVERFLOWED CAR entry (Inf or NaN only), emit exactly one warning
                        # (the statistics-layer NA+one-warning contract) and NA out the
                        # derived statistics for those entries.
                        # IMPORTANT: plain NA is EXCLUDED -- an NA CAR is the legitimate
                        # degenerate-model propagation (an unfitted model returns all-NA
                        # abnormal returns, already warned once at fit time). Firing here on
                        # NA would emit a SECOND contract warning, violating the one-warning
                        # contract (CONTRACT-04). Valid finite windows are untouched (SC5).
                        car_overflow <- is.infinite(res$car) | is.nan(res$car)
                        if (any(car_overflow)) {
                          warning(
                            "CARTest: CAR cumulation overflow -- ",
                            sum(car_overflow),
                            " cumulative abnormal return(s) are non-finite; ",
                            "car_t set to NA (rather than a misleading Inf).",
                            call. = FALSE
                          )
                          res$corrected_car[car_overflow] <- NA_real_
                          res$car_t[car_overflow]         <- NA_real_
                        }

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
#' @family eventstudy-statistics
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
                          sigma <- statistics$sigma %||% NA_real_

                          event_data <- data_tbl %>%
                            dplyr::filter(event_window == 1)

                          # Compound returns
                          cum_firm <- cumprod(1 + dplyr::coalesce(event_data$firm_returns, 0))
                          cum_index <- cumprod(1 + dplyr::coalesce(event_data$index_returns, 0))
                          bhar <- cum_firm - cum_index

                          n <- seq_len(nrow(event_data))
                          # Under simple approximation, sigma of BHAR grows with sqrt(n)
                          bhar_se <- sigma * sqrt(n)

                          # Guard: sigma == 0 or NA \u2192 bhar_se is 0/NA \u2192 bhar_t
                          # would be Inf/NaN. Return NA_real_ instead.
                          bhar_se_degenerate <- is.na(bhar_se) | bhar_se < .Machine$double.eps

                          res <- event_data %>%
                            dplyr::select(relative_index) %>%
                            dplyr::mutate(
                              bhar = bhar,
                              bhar_window = stringr::str_c("[", relative_index[1], ", ",
                                                           relative_index, "]"),
                              bhar_se = bhar_se,
                              bhar_t = ifelse(bhar_se_degenerate, NA_real_, bhar / bhar_se)
                            )
                          res
                        }
                      )
)


