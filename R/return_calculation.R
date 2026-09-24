#' @title ReturnCalculation
#' @description Base (abstract) class for return calculation strategies.
#' Subclass this to plug a custom return calculation into
#' \code{prepare_event_study()} / \code{EventStudyTask$new()} via
#' \code{ParameterSet$return_calculation}.
#'
#' A subclass must implement \code{calculate_return(tbl, in_column, out_column)}:
#' given a tibble of prices in \code{in_column}, add a column named
#' \code{out_column} with the calculated return (e.g. simple or log return).
#' See \code{SimpleReturn} and \code{LogReturn} for reference implementations.
#'
#' @family eventstudy-models
#' @export
ReturnCalculation <- R6Class("ReturnCalculation",
                             public = list(
                               #' @field name Name of the return calculation.
                               name = "",
                               #' @description Calculates the return for a
                               #' single stock.
                               #'
                               #' @param tbl The dataframe with the stock price.
                               #' @param in_column The column name of the price
                               #' infromation.
                               #' @param out_column The column name were the
                               #' return will be saved.
                               calculate_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {

                               }
                             )
)


#' R6 class for log return calculation
#'
#' @family eventstudy-models
#' @export
LogReturn <- R6Class("LogReturn",
                     inherit = ReturnCalculation,
                     public = list(
                       #' @field name Name of the log return calculation.
                       name = "log return",
                       #' @description Calculates the return for a
                       #' single stock.
                       #'
                       #' @param tbl The dataframe with the stock price.
                       #' @param in_column The column name of the price
                       #' infromation.
                       #' @param out_column The column name were the
                       #' return will be saved.
                       calculate_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {
                         tbl %>%
                           mutate(!!rlang::sym(out_column) := {
                             price <- !!rlang::sym(in_column)
                             lagged <- lag(price)
                             ratio <- price / lagged
                             # Guard: log(0) = -Inf, log(negative) = NaN
                             ifelse(is.finite(ratio) & ratio > 0, log(ratio), NA_real_)
                           })
                       }
                     )
)


#' R6 class for simple return calculation
#'
#' @family eventstudy-models
#' @export
SimpleReturn <- R6Class("SimpleReturn",
                        inherit = ReturnCalculation,
                        public = list(
                          #' @field name Name of the return calculation.
                          name = "simple return",
                          #' @description Calculates the simple return for a
                          #' single stock.
                          #'
                          #' @param tbl The dataframe with the stock price.
                          #' @param in_column The column name of the price
                          #' infromation.
                          #' @param out_column The column name were the
                          #' return will be saved.
                          calculate_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {
                            tbl %>%
                              mutate(!!rlang::sym(out_column) := {
                                price <- !!rlang::sym(in_column)
                                lagged <- lag(price)
                                ifelse(is.finite(lagged) & lagged != 0,
                                       (price - lagged) / lagged,
                                       NA_real_)
                              })
                          }
                        )
)
