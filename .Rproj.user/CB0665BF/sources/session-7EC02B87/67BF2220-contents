library(distributional)

TestStatistic <- R6Class("TestStatistic",
                         public = list(
                           name = 'TestStatistics',
                           confidence_level = 0.95,
                           confidence_type = 'two-sided',
                           initialize = function(confidence_level=0.95, confidence_type='two-sided') {
                             self$confidence_level = confidence_level
                             self$confidence_type = confidence_type
                           },
                           compute = function(data_tbl, model) {

                           }
                         )
)


ARTTest <- R6Class("ARTTest",
                   inherit = TestStatistic,
                   public = list(
                     name = 'ART',
                     compute = function(data_tbl, model) {
                       statistics = model$statistics
                       sigma = statistics$sigma
                       degree_of_freedom = statistics$degree_of_freedom

                       res = data_tbl %>%
                         filter(event_window == 1) %>%
                         select(relative_index, abnormal_returns) %>%
                         mutate(ar_t      = abnormal_returns / sigma,
                                ar_t_dist = distributional::dist_student_t(degree_of_freedom))
                       res
                     }
                   )
)


CARTTest <- R6Class("CARTTest",
                    inherit = TestStatistic,
                    public = list(
                      name = 'CART',
                      compute = function(data_tbl, model) {
                        statistics = model$statistics
                        sigma = statistics$sigma
                        degree_of_freedom = statistics$degree_of_freedom

                        res = data_tbl %>%
                          filter(event_window == 1) %>%
                          select(relative_index, abnormal_returns) %>%
                          mutate(event_window_length = 1:n(),
                                 car_window          = "",
                                 car                 = cumsum(abnormal_returns),
                                 corrected_car       = car / sigma,
                                 car_t               = car / (sqrt(event_window_length) * sigma),
                                 car_t_dist          = distributional::dist_student_t(degree_of_freedom))
                        res$car_window = stringr::str_c("[", res$relative_index[1], ", ", res$relative_index, "]")
                        res
                      }
                    )
)
