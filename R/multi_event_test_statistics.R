#' Cross-Sectional T Test (CSectTTest)
#'
#' The Cross-Sectional Test (CSect T) is a statistical tool employed in event
#' studies to evaluate the null hypothesis that the average abnormal return at
#' the event date is zero.
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
