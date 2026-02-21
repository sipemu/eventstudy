#' Cross-Sectional T Test (CSectTTest)
#'
#' The Cross-Sectional Test (CSect T) is a statistical tool employed in event
#' studies to evaluate the null hypothesis that the average abnormal return at
#' the event date is zero. The test statistic is distributed as \eqn{t_{N-1}}, where
#' N is the number of events in that group.
#'
#' See also \url{https://eventstudy.de/statistics/aar_caar_statistics.html}
#'
#' @export
CSectTTest <- R6Class("CSectTTest",
                   inherit = TestStatisticBase,
                   public = list(
                     #' @field name Short code of the test statistic.
                     name = 'CSectT',
                     #' @description
                     #' Computes the AAR/CAAR cross-sectional t test statistics.
                     #'
                     #' @param data_tbl The data for a multiple event with
                     #' calculated abnormal returns.
                     #' @param model The fitted model: not necessary for this
                     #' test statistics.
                     compute = function(data_tbl, model) {
                       # AAR & AAR T Test
                       aar_caar_stats = data_tbl %>%
                         dplyr::filter(event_window == 1) %>%
                         dplyr::group_by(relative_index) %>%
                         dplyr::summarise(aar            = mean(abnormal_returns, na.rm = TRUE),
                                          n_events       = dplyr::n(),
                                          n_valid_events = sum(!is.na(abnormal_returns)),
                                          n_pos          = sum(abnormal_returns >= 0, na.rm = TRUE),
                                          n_neg          = sum(abnormal_returns < 0, na.rm = TRUE),
                                          aar_t          = sqrt(n_valid_events) * aar / sd(abnormal_returns, na.rm=TRUE),
                                          .groups = "drop")

                       # Calculate SD_CAAR for each window
                       sd_caar = data_tbl %>%
                         dplyr::filter(event_window == 1) %>%
                         dplyr::select(event_id, relative_index, abnormal_returns) %>%
                         dplyr::group_by(event_id) %>%
                         dplyr::mutate(car = cumsum(abnormal_returns)) %>%
                         dplyr::group_by(relative_index) %>%
                         dplyr::summarise(sd_caar = sd(car, na.rm=TRUE))

                       # Finally, the t statistic for CAAR and each window is calculated
                       aar_caar_stats = aar_caar_stats %>%
                         dplyr::mutate(caar = cumsum(aar)) %>%
                         dplyr::left_join(sd_caar, by="relative_index") %>%
                         dplyr::mutate(caar_t = sqrt(n_valid_events) * caar / sd_caar) %>%
                         dplyr::select(-sd_caar)

                       aar_caar_stats$car_window = stringr::str_c("[", aar_caar_stats$relative_index[1], ", ", aar_caar_stats$relative_index, "]")

                       aar_caar_stats
                     }
                   )
)


#' Patell or Standardized Residual Test (PatellZTest)
#'
#' The Patell or Standardized Residual Test is a statistical tool employed in event
#' studies to evaluate the null hypothesis that the average abnormal return at
#' the event date is zero. The test statistics is approximately distributed as
#' N(0, 1).
#'
#' See also \url{https://eventstudy.de/statistics/aar_caar_statistics.html}
#'
#' @export
PatellZTest <- R6Class("PatellZTest",
                      inherit = TestStatisticBase,
                      public = list(
                        #' @field name Short code of the test statistic.
                        name = 'PatellZ',
                        #' @description
                        #' Computes the Patell Z test statistics for multiple events.
                        #'
                        #' @param data_tbl The data for a multiple event with
                        #' calculated abnormal returns.
                        #' @param model The fitted model.
                        compute = function(data_tbl, model) {
                          sd_asar = data_tbl %>%
                            dplyr::filter(estimation_window == 1) %>%
                            dplyr::group_by(firm_symbol) %>%
                            dplyr::summarise(m = dplyr::n(), .groups = "drop") %>%
                            dplyr::mutate(Q = (m - 2) / (m - 4))

                          # Extract sigma from the model object
                          model = model %>%
                            dplyr::mutate(sigma = purrr::map(model, .f=function(x) {
                              x$statistics$sigma
                            }) %>% purrr::reduce(c))

                          # Extract forecast error corrected sigma from the model object
                          model = model %>%
                            dplyr::mutate(forecast_error_corrected_sigma = purrr::map(model, .f=function(x) {
                              x$statistics$forecast_error_corrected_sigma
                            }))

                          # Standardize abnormal returns by forecast corrected sigma
                          aar_caar_stats_tmp = data_tbl %>%
                            dplyr::filter(event_window == 1) %>%
                            dplyr::left_join(model %>% dplyr::select(firm_symbol, sigma), by="firm_symbol") %>%
                            dplyr::mutate(standardized_abnormal_returns = abnormal_returns / sqrt(sigma))

                          # AAR & AAR Z Test
                          Q = sd_asar %>%
                            .[["Q"]] %>%
                            sum() %>%
                            sqrt()

                          aar_caar_stats = aar_caar_stats_tmp %>%
                            dplyr::group_by(relative_index) %>%
                            dplyr::summarise(aar            = sum(standardized_abnormal_returns, na.rm = TRUE),
                                             n_events       = dplyr::n(),
                                             n_valid_events = sum(!is.na(abnormal_returns)),
                                             n_pos          = sum(abnormal_returns >= 0, na.rm = TRUE),
                                             n_neg          = sum(abnormal_returns < 0, na.rm = TRUE),
                                             aar_z          = aar / Q,
                                             .groups = "drop")

                          sd_caar = aar_caar_stats_tmp %>%
                            dplyr::mutate(csar = cumsum(standardized_abnormal_returns)) %>%
                            dplyr::left_join(sd_asar, by="firm_symbol") %>%
                            dplyr::group_by(firm_symbol) %>%
                            dplyr::mutate(n      = 1:dplyr::n(),
                                          csar   = csar / (sqrt(n * Q))) %>%
                            dplyr::group_by(relative_index) %>%
                            dplyr::summarise(caar_z = 1 / sqrt(dplyr::n()) * sum(csar))

                          aar_caar_stats = aar_caar_stats %>%
                            dplyr::mutate(caar = cumsum(aar)) %>%
                            dplyr::left_join(sd_caar, by="relative_index")

                          aar_caar_stats$car_window = stringr::str_c("[", aar_caar_stats$relative_index[1], ", ", aar_caar_stats$relative_index, "]")
                          aar_caar_stats
                        }
                      )
)


#' Sign Test for Multiple Events
#'
#' Tests whether the proportion of positive abnormal returns differs
#' significantly from 0.5 under the null hypothesis. The test statistic
#' is approximately distributed as N(0, 1).
#'
#' @export
SignTest <- R6Class("SignTest",
                    inherit = TestStatisticBase,
                    public = list(
                      #' @field name Short code of the test statistic.
                      name = 'SignT',
                      #' @description
                      #' Computes the sign test for multiple events.
                      #'
                      #' @param data_tbl The data for a multiple event with
                      #' calculated abnormal returns.
                      #' @param model The fitted model (unused).
                      compute = function(data_tbl, model) {
                        aar_stats = data_tbl %>%
                          dplyr::filter(event_window == 1) %>%
                          dplyr::group_by(relative_index) %>%
                          dplyr::summarise(
                            aar            = mean(abnormal_returns, na.rm = TRUE),
                            n_events       = dplyr::n(),
                            n_valid_events = sum(!is.na(abnormal_returns)),
                            n_pos          = sum(abnormal_returns > 0, na.rm = TRUE),
                            n_neg          = sum(abnormal_returns <= 0, na.rm = TRUE),
                            .groups = "drop"
                          ) %>%
                          dplyr::mutate(
                            # Sign test: (n_pos - 0.5*N) / (0.5*sqrt(N))
                            sign_z = (n_pos - 0.5 * n_valid_events) / (0.5 * sqrt(n_valid_events)),
                            caar   = cumsum(aar)
                          )

                        # Cumulative sign test
                        cum_sign = data_tbl %>%
                          dplyr::filter(event_window == 1) %>%
                          dplyr::select(event_id, relative_index, abnormal_returns) %>%
                          dplyr::group_by(event_id) %>%
                          dplyr::mutate(car = cumsum(abnormal_returns)) %>%
                          dplyr::group_by(relative_index) %>%
                          dplyr::summarise(
                            n_pos_car = sum(car > 0, na.rm = TRUE),
                            n_valid   = sum(!is.na(car)),
                            csign_z   = (n_pos_car - 0.5 * n_valid) / (0.5 * sqrt(n_valid)),
                            .groups = "drop"
                          )

                        aar_stats = aar_stats %>%
                          dplyr::left_join(cum_sign %>% dplyr::select(relative_index, csign_z),
                                           by = "relative_index")

                        aar_stats$car_window = stringr::str_c("[", aar_stats$relative_index[1], ", ", aar_stats$relative_index, "]")
                        aar_stats
                      }
                    )
)


#' Generalized Sign Test (Cowan 1992)
#'
#' Adjusts the sign test for the expected proportion of positive abnormal
#' returns estimated from the estimation window, rather than assuming 0.5.
#' This accounts for asymmetry in the return distribution.
#'
#' @export
GeneralizedSignTest <- R6Class("GeneralizedSignTest",
                               inherit = TestStatisticBase,
                               public = list(
                                 #' @field name Short code of the test statistic.
                                 name = 'GSignT',
                                 #' @description
                                 #' Computes the generalized sign test for multiple events.
                                 #'
                                 #' @param data_tbl The data for a multiple event with
                                 #' calculated abnormal returns.
                                 #' @param model The fitted model (unused).
                                 compute = function(data_tbl, model) {
                                   # Estimate p_hat from estimation window
                                   p_hat_by_firm = data_tbl %>%
                                     dplyr::filter(estimation_window == 1) %>%
                                     dplyr::group_by(firm_symbol) %>%
                                     dplyr::summarise(
                                       p_hat = mean(abnormal_returns > 0, na.rm = TRUE),
                                       .groups = "drop"
                                     )

                                   # Average p_hat across firms
                                   p_hat = mean(p_hat_by_firm$p_hat, na.rm = TRUE)

                                   aar_stats = data_tbl %>%
                                     dplyr::filter(event_window == 1) %>%
                                     dplyr::group_by(relative_index) %>%
                                     dplyr::summarise(
                                       aar            = mean(abnormal_returns, na.rm = TRUE),
                                       n_events       = dplyr::n(),
                                       n_valid_events = sum(!is.na(abnormal_returns)),
                                       n_pos          = sum(abnormal_returns > 0, na.rm = TRUE),
                                       n_neg          = sum(abnormal_returns <= 0, na.rm = TRUE),
                                       .groups = "drop"
                                     ) %>%
                                     dplyr::mutate(
                                       gsign_z = (n_pos - n_valid_events * p_hat) /
                                         sqrt(n_valid_events * p_hat * (1 - p_hat)),
                                       caar = cumsum(aar)
                                     )

                                   # Cumulative generalized sign test
                                   cum_gsign = data_tbl %>%
                                     dplyr::filter(event_window == 1) %>%
                                     dplyr::select(event_id, relative_index, abnormal_returns) %>%
                                     dplyr::group_by(event_id) %>%
                                     dplyr::mutate(car = cumsum(abnormal_returns)) %>%
                                     dplyr::group_by(relative_index) %>%
                                     dplyr::summarise(
                                       n_pos_car = sum(car > 0, na.rm = TRUE),
                                       n_valid   = sum(!is.na(car)),
                                       cgsign_z  = (n_pos_car - n_valid * p_hat) /
                                         sqrt(n_valid * p_hat * (1 - p_hat)),
                                       .groups = "drop"
                                     )

                                   aar_stats = aar_stats %>%
                                     dplyr::left_join(cum_gsign %>% dplyr::select(relative_index, cgsign_z),
                                                      by = "relative_index")

                                   aar_stats$car_window = stringr::str_c("[", aar_stats$relative_index[1], ", ", aar_stats$relative_index, "]")
                                   aar_stats
                                 }
                               )
)


#' Rank Test (Corrado 1989)
#'
#' Non-parametric rank test for event studies. Ranks abnormal returns
#' across the combined estimation and event windows, which is robust
#' to non-normality of abnormal returns.
#'
#' @export
RankTest <- R6Class("RankTest",
                    inherit = TestStatisticBase,
                    public = list(
                      #' @field name Short code of the test statistic.
                      name = 'RankT',
                      #' @description
                      #' Computes the Corrado rank test for multiple events.
                      #'
                      #' @param data_tbl The data for a multiple event with
                      #' calculated abnormal returns.
                      #' @param model The fitted model (unused).
                      compute = function(data_tbl, model) {
                        # Rank abnormal returns within each firm across combined windows
                        ranked_data = data_tbl %>%
                          dplyr::filter(estimation_window == 1 | event_window == 1) %>%
                          dplyr::group_by(firm_symbol) %>%
                          dplyr::mutate(
                            total_obs = dplyr::n(),
                            ar_rank   = rank(abnormal_returns, na.last = "keep"),
                            # Center ranks: K_it = rank / (T+1) - 0.5
                            centered_rank = ar_rank / (total_obs + 1) - 0.5
                          ) %>%
                          dplyr::ungroup()

                        # Compute standard deviation of centered ranks across estimation window
                        sd_rank = ranked_data %>%
                          dplyr::group_by(relative_index) %>%
                          dplyr::summarise(
                            mean_centered_rank = mean(centered_rank, na.rm = TRUE),
                            .groups = "drop"
                          )

                        S_rank = sd(sd_rank$mean_centered_rank, na.rm = TRUE)

                        # Event window statistics
                        aar_stats = ranked_data %>%
                          dplyr::filter(event_window == 1) %>%
                          dplyr::group_by(relative_index) %>%
                          dplyr::summarise(
                            aar            = mean(abnormal_returns, na.rm = TRUE),
                            n_events       = dplyr::n(),
                            n_valid_events = sum(!is.na(abnormal_returns)),
                            n_pos          = sum(abnormal_returns > 0, na.rm = TRUE),
                            n_neg          = sum(abnormal_returns <= 0, na.rm = TRUE),
                            mean_rank      = mean(centered_rank, na.rm = TRUE),
                            .groups = "drop"
                          ) %>%
                          dplyr::mutate(
                            rank_z = mean_rank / S_rank,
                            caar   = cumsum(aar)
                          )

                        aar_stats$car_window = stringr::str_c("[", aar_stats$relative_index[1], ", ", aar_stats$relative_index, "]")
                        aar_stats
                      }
                    )
)


#' BMP Test (Boehmer, Musumeci, Poulsen 1991)
#'
#' Standardized cross-sectional test that is robust to event-induced variance
#' increases. The BMP test standardizes abnormal returns by their forecast
#' error corrected standard deviation, then applies a cross-sectional
#' t-test to these standardized residuals.
#'
#' @export
BMPTest <- R6Class("BMPTest",
                   inherit = TestStatisticBase,
                   public = list(
                     #' @field name Short code of the test statistic.
                     name = 'BMP',
                     #' @description
                     #' Computes the BMP standardized cross-sectional test.
                     #'
                     #' @param data_tbl The data for a multiple event with
                     #' calculated abnormal returns.
                     #' @param model The fitted model containing sigma estimates.
                     compute = function(data_tbl, model) {
                       # Extract sigma from each model
                       model = model %>%
                         dplyr::mutate(sigma = purrr::map_dbl(model, .f=function(x) {
                           x$statistics$sigma
                         }))

                       # Standardize abnormal returns by model sigma
                       sar_data = data_tbl %>%
                         dplyr::filter(event_window == 1) %>%
                         dplyr::left_join(model %>% dplyr::select(firm_symbol, sigma),
                                          by = "firm_symbol") %>%
                         dplyr::mutate(sar = abnormal_returns / sigma)

                       # BMP test: cross-sectional t-test of SARs
                       aar_stats = sar_data %>%
                         dplyr::group_by(relative_index) %>%
                         dplyr::summarise(
                           aar            = mean(abnormal_returns, na.rm = TRUE),
                           n_events       = dplyr::n(),
                           n_valid_events = sum(!is.na(abnormal_returns)),
                           n_pos          = sum(abnormal_returns > 0, na.rm = TRUE),
                           n_neg          = sum(abnormal_returns <= 0, na.rm = TRUE),
                           mean_sar       = mean(sar, na.rm = TRUE),
                           sd_sar         = sd(sar, na.rm = TRUE),
                           bmp_t          = sqrt(n_valid_events) * mean_sar / sd_sar,
                           .groups = "drop"
                         ) %>%
                         dplyr::mutate(caar = cumsum(aar))

                       # Cumulative BMP
                       cum_sar = sar_data %>%
                         dplyr::select(event_id, relative_index, sar) %>%
                         dplyr::group_by(event_id) %>%
                         dplyr::mutate(csar = cumsum(sar)) %>%
                         dplyr::group_by(relative_index) %>%
                         dplyr::summarise(
                           mean_csar = mean(csar, na.rm = TRUE),
                           sd_csar   = sd(csar, na.rm = TRUE),
                           n_valid   = sum(!is.na(csar)),
                           cbmp_t    = sqrt(n_valid) * mean_csar / sd_csar,
                           .groups = "drop"
                         )

                       aar_stats = aar_stats %>%
                         dplyr::left_join(cum_sar %>% dplyr::select(relative_index, cbmp_t),
                                          by = "relative_index")

                       aar_stats$car_window = stringr::str_c("[", aar_stats$relative_index[1], ", ", aar_stats$relative_index, "]")
                       aar_stats
                     }
                   )
)


#' Calendar-Time Portfolio Test
#'
#' Aggregates event-firm returns into calendar-time portfolios and tests
#' whether the portfolio intercept (alpha) is significantly different from
#' zero. This approach naturally handles cross-sectional dependence that
#' arises when events cluster in calendar time.
#'
#' For each relative event day, the test forms an equal-weighted portfolio
#' of all event firms' abnormal returns and computes a t-statistic of the
#' mean portfolio return.
#'
#' @export
CalendarTimePortfolioTest <- R6Class("CalendarTimePortfolioTest",
                                      inherit = TestStatisticBase,
                                      public = list(
                                        #' @field name Short code of the test statistic.
                                        name = 'CalTimeT',
                                        #' @description
                                        #' Computes the calendar-time portfolio test.
                                        #'
                                        #' @param data_tbl The data for multiple events with
                                        #' calculated abnormal returns.
                                        #' @param model The fitted models (unused directly).
                                        compute = function(data_tbl, model) {
                                          # Portfolio approach: for each relative event day,
                                          # form equal-weighted portfolio of ARs
                                          portfolio <- data_tbl %>%
                                            dplyr::filter(event_window == 1) %>%
                                            dplyr::group_by(relative_index) %>%
                                            dplyr::summarise(
                                              aar = mean(abnormal_returns, na.rm = TRUE),
                                              n_events = dplyr::n(),
                                              n_valid_events = sum(!is.na(abnormal_returns)),
                                              n_pos = sum(abnormal_returns >= 0, na.rm = TRUE),
                                              n_neg = sum(abnormal_returns < 0, na.rm = TRUE),
                                              port_sd = sd(abnormal_returns, na.rm = TRUE),
                                              .groups = "drop"
                                            )

                                          # Compute time-series t-stat of portfolio returns
                                          # under H0: E[AAR] = 0
                                          ts_sd <- sd(portfolio$aar, na.rm = TRUE)
                                          n_periods <- nrow(portfolio)

                                          portfolio <- portfolio %>%
                                            dplyr::mutate(
                                              caar = cumsum(aar),
                                              # Time-series t-stat: AAR_t / sd(AAR) * sqrt(T)
                                              aar_t = aar / ts_sd,
                                              # CAAR t-stat: CAAR / (sd * sqrt(L))
                                              caar_t = caar / (ts_sd * sqrt(seq_len(n_periods)))
                                            )

                                          portfolio$car_window <- stringr::str_c(
                                            "[", portfolio$relative_index[1], ", ",
                                            portfolio$relative_index, "]"
                                          )

                                          portfolio %>%
                                            dplyr::select(-port_sd)
                                        }
                                      )
)
