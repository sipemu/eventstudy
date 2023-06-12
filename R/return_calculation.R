#' R6 base class for return calculation
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
                           mutate(!!rlang::sym(out_column) := log(!!rlang::sym(in_column) / lag(!!rlang::sym(in_column))))
                       }
                     )
)


#' R6 class for simple return calculation
#'
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
                              mutate(!!rlang::sym(out_column) := (!!rlang::sym(in_column) - lag(!!rlang::sym(in_column))) / !!rlang::sym(in_column))
                          }
                        )
)
