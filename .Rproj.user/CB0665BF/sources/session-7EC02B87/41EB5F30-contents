ReturnCalculation <- R6Class("ReturnCalculation",
                             public = list(
                               name = "",
                               calculate_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {

                               }
                             )
)


LogReturn <- R6Class("LogReturn",
                     inherit = ReturnCalculation,
                     public = list(
                       name = "log_return",
                       calculate_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {
                         tbl %>%
                           mutate(!!rlang::sym(out_column) := log(!!rlang::sym(in_column) / lag(!!rlang::sym(in_column))))
                       }
                     )
)


SimpleReturn <- R6Class("SimpleReturn",
                        inherit = ReturnCalculation,
                        public = list(
                          name = "log_return",
                          calculate_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {
                            tbl %>%
                              mutate(!!rlang::sym(out_column) := (!!rlang::sym(in_column) - lag(!!rlang::sym(in_column))) / !!rlang::sym(in_column))
                          }
                        )
)


calculate_log_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {
  tbl %>%
    mutate(!!rlang::sym(out_column) := log(!!rlang::sym(in_column) / lag(!!rlang::sym(in_column))))
}

calculate_simple_return = function(tbl, in_column = "adjusted", out_column="adjusted_return") {
  tbl %>%
    mutate(!!rlang::sym(out_column) := (!!rlang::sym(in_column) - lag(!!rlang::sym(in_column))) / !!rlang::sym(in_column))
}
