#' Cross-Sectional T Test (CSectTTest)
#'
#' The Cross-Sectional Test (CSect T) is a statistical tool employed in event
#' studies to evaluate the null hypothesis that the average abnormal return at
#' the event date is zero. The test statistic is distributed as t_{N-1}, where
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
                     #' Computes the test AR test statistics for a single event.
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
#' The  Patell or Standardized Residual Test is a statistical tool employed in event
#' studies to evaluate the null hypothesis that the average abnormal return at
#' the event date is zero. The test statistics is approximatelly distributed as
#' N(0, 1).
#'
#' See also \url{https://eventstudy.de/statistics/aar_caar_statistics.html}
#'
#' @export
PatellZTest <- R6Class("PatellZTest",
                      inherit = TestStatisticBase,
                      public = list(
                        #' @field name Short code of the test statistic.
                        name = 'CSectT',
                        #' @description
                        #' Computes the test AR test statistics for a single event.
                        #'
                        #' @param data_tbl The data for a multiple event with
                        #' calculated abnormal returns.
                        #' @param model The fitted model: not necessary for this
                        #' test statistics.
                        compute = function(data_tbl, model) {
                          browser()
                          #
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

                          # AAR & AAR T Test
                          # Calculate AAR and AAR z for each index in the event window
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
                            # Calculate CSAR
                            dplyr::mutate(csar = cumsum(standardized_abnormal_returns)) %>%
                            # Correct for SD
                            dplyr::left_join(sd_asar, by="firm_symbol") %>%
                            dplyr::group_by(firm_symbol) %>%
                            dplyr::mutate(n      = 1:dplyr::n(),
                                          csar   = csar / (sqrt(n * Q))) %>%
                            # Calculate cumulative CAAR Z statistics
                            dplyr::group_by(relative_index) %>%
                            dplyr::summarise(caar_z = 1 / sqrt(dplyr::n()) * sum(csar))


                          # Finally, the t statistic for CAAR and each window is calculated
                          aar_caar_stats = aar_caar_stats %>%
                            dplyr::mutate(caar = cumsum(aar)) %>%
                            dplyr::left_join(sd_caar, by="relative_index")

                          aar_caar_stats$car_window = stringr::str_c("[", aar_caar_stats$relative_index[1], ", ", aar_caar_stats$relative_index, "]")
                          aar_caar_stats
                        }
                      )
)
