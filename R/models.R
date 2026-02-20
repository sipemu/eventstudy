#' @title ModelBase
#' @description Base model class for event study. Each single event study will
#' get its own model initialization and fitting. Therefore, the input DataFrame
#' contains the data for a single Event Study. For custom models, except the
#' child modles of the market model, several statistics must be included, namely
#' sigma, degree_of_freedom, first_order_auto_correlation, residuals,
#' forecast_error_corrected_sigma, and forecast_error_corrected_sigma_car. Part
#' of these statistics are necessary for calculating the Event Study statistics.
ModelBase <- R6Class("ModelBase",
                     public = list(
                       #' @field model_name Name of the model.
                       model_name = "",
                       #' @description
                       #' Fits the model with given data.
                       #'
                       #' @param data_tbl A data frame or tibble containing the data to fit.
                       fit = function(data_tbl) {

                       },
                       #' @description
                       #' Calculate the abnormal returns with given data and fitted model.
                       #'
                       #' @param data_tbl Data frame or tibble containing the data to calculate abnormal returns.
                       abnormal_returns = function(data_tbl) {

                       }
                     ),
                     active = list(
                       #' @field statistics Read-only field to get statistics.
                       statistics = function(value) {
                         if (missing(value)) {
                           private$.statistics
                         } else {
                           stop("`$statistics` is read only", call. = FALSE)
                         }
                       },
                       #' @field model Read-only field to get the fitted model.
                       model = function(value) {
                         if (missing(value)) {
                           private$.fitted_model
                         } else {
                           stop("`$model` is read only", call. = FALSE)
                         }
                       },
                       #' @field is_fitted Read-only field to check if the model is fitted.
                       is_fitted = function(value) {
                         if (missing(value)) {
                           private$.is_fitted
                         } else {
                           stop("`$is_fitted` is read only", call. = FALSE)
                         }
                       }
                     ),
                     private = list(
                       .is_fitted = FALSE,
                       .fitted_model = NULL,
                       .error = NULL,
                       #' Statistics object contains different model specific KPIs
                       #' that describes the fitted model.
                       .statistics = list(sigma=NULL,
                                          degree_of_freedom=NULL,
                                          first_order_auto_correlation=NULL,
                                          residuals=NULL,
                                          forecast_error_corrected_sigma=NULL,
                                          forecast_error_corrected_sigma_car=NULL),
                       calculate_statistics = function(data_tbl) {


                       },
                       calculate_forecast_error_correction = function(sigma,
                                                                      estimation_window_length,
                                                                      estimation_market_returns,
                                                                      event_market_returns) {
                         meanMREstW <- mean(estimation_market_returns)
                         forecast_error_corrected_sigma <- sigma *
                           sqrt(1 + 1 / estimation_window_length +
                                  (event_market_returns - meanMREstW)^2 /
                                  sum((estimation_market_returns - meanMREstW)^2))

                         forecast_error_corrected_sigma_car = (event_market_returns - meanMREstW) /
                           sqrt(sum((estimation_market_returns - meanMREstW)^2, na.rm=TRUE))

                         private$.statistics$forecast_error_corrected_sigma = forecast_error_corrected_sigma
                         private$.statistics$forecast_error_corrected_sigma_car = forecast_error_corrected_sigma_car
                       },
                       first_order_autocorrelation = function(residuals) {
                         first_order_acf = acf(c(na.omit(residuals)), plot=F, 1, type="correlation")
                         first_order_auto_correlation = first_order_acf[[1]][, , 1][2]
                         private$.statistics$first_order_auto_correlation = first_order_auto_correlation
                       },
                       add_residuals = function(residuals) {
                         private$.statistics$residuals = residuals
                       }
                     )
)


#' Market Model
#'
#' The Market Model is a widely used method in event studies to estimate the
#' expected returns of a stock and calculate its abnormal returns during an
#' event window. The model is based on a simple linear regression framework and
#' captures the relationship between a stock’s return and the return of a market
#' index, such as the S&P 500 or the Dow Jones Industrial Average. The
#' underlying assumption of the Market Model is that a stock’s return is
#' primarily influenced by market movements, along with a stock-specific
#' idiosyncratic component.
#'
#' @export
MarketModel <- R6Class("MarketModel",
                       inherit = ModelBase,
                       public = list(
                         #' @field model_name Name of the model.
                         model_name = "MarketModel",
                         #' @field formula The formula applied for calculating the market model
                         formula = as.formula("firm_returns ~ index_returns"),
                         #' @description
                         #' Set the formula
                         #'
                         #' @param formula A formula.
                         set_formula = function(formula) {
                           if (!inherits(formula, "formula")) {
                             stop("Input must be a formula")
                           }
                           self$formula = formula
                         },
                         #' @description
                         #' Fit the model with given data.
                         #'
                         #' @param data_tbl Data frame or tibble containing the data to fit.
                         fit = function(data_tbl) {
                           data_tbl %>%
                             filter(estimation_window == 1) -> estimation_tbl

                           # safe execution
                           safe_mm = purrr::safely(.f=.estimate_mm_model)
                           res = safe_mm(self$formula, estimation_tbl)
                           if (is.null(res$error)) {
                             private$.fitted_model = res$result
                             private$.is_fitted = TRUE

                             # Calculate statistics
                             private$calculate_statistics(data_tbl)
                           } else {
                             private$.is_fitted = FALSE
                             private$.error = res$error
                             warning("Model fitting failed: ", conditionMessage(res$error))
                           }
                         },
                         #' @description
                         #' Calculate the abnormal returns with given data.
                         #'
                         #' @param data_tbl Data frame or tibble containing the data to calculate abnormal returns.
                         abnormal_returns = function(data_tbl) {
                           if (private$.is_fitted) {
                             # Calculate abnormal returns
                             alpha = private$.statistics$alpha
                             beta = private$.statistics$beta
                             data_tbl %>%
                               mutate(abnormal_returns = firm_returns - (alpha + beta * index_returns))
                           } else {
                             warning("MarketModel is not fitted. Returning NA abnormal returns.")
                             data_tbl %>%
                               mutate(abnormal_returns = NA_real_)
                           }
                         }
                       ),
                       private = list(
                         calculate_statistics = function(data_tbl) {
                           # abnormal return calculation
                           alpha = private$.fitted_model$coefficients[1]
                           names(alpha) <- NULL
                           beta = private$.fitted_model$coefficients[2]
                           names(beta) <- NULL

                           # Calculate statistics
                           # TBD: Make more abstract for general usage.
                           modell_summary = summary(private$.fitted_model)
                           private$.statistics$alpha = alpha
                           private$.statistics$pval_alpha = modell_summary$coefficients[1, 4]
                           private$.statistics$beta = beta
                           private$.statistics$pval_beta = modell_summary$coefficients[2, 4]

                           private$.statistics$sigma = modell_summary$sigma
                           private$.statistics$r2 = modell_summary$r.squared
                           f_stat <- modell_summary$fstatistic[1]
                           names(f_stat) <- NULL
                           private$.statistics$f_stat = f_stat
                           private$.statistics$degree_of_freedom = private$.fitted_model$df.residual

                           # residuals & first-order autocorrelation for
                           # diagnostics
                           residuals = private$.fitted_model$residuals
                           private$add_residuals(residuals)
                           private$first_order_autocorrelation(residuals)

                           # forecast correction term
                           estimation_tbl = data_tbl %>% filter(estimation_window == 1)
                           event_window_tbl = data_tbl %>% filter(event_window == 1)
                           event_market_returns = event_window_tbl$index_returns
                           estimation_window_length = nrow(estimation_tbl)
                           estimation_market_returns = estimation_tbl$index_returns

                           private$calculate_forecast_error_correction(modell_summary$sigma,
                                                                       estimation_window_length,
                                                                       estimation_market_returns,
                                                                       event_market_returns)
                         }
                       )
)



#' Market Adjusted Model
#'
#' The Market Adjusted Model is another simple approach used in event studies
#' to estimate the expected returns of a stock and calculate its abnormal
#' returns during an event window. This model is less complex than the Market
#' Model, as it assumes that a stock’s expected return is equal to the market
#' return, without considering any stock-specific factors. The Market
#' Adjusted Model is particularly useful in situations where the estimation of
#' individual stock parameters (such as alpha and beta) is not feasible or
#' desired, and a basic benchmark for comparison is needed.
#'
#' @export
MarketAdjustedModel <- R6Class("MarketAdjustedModel",
                               inherit = ModelBase,
                               public = list(
                                 #' @field model_name Name of the model.
                                 model_name = "MarketAdjustedModel",
                                 #' @description
                                 #' fit Fit the model with given data.
                                 #' @param data_tbl Data frame or tibble containing the data to fit.
                                 fit = function(data_tbl) {
                                   # do nothing
                                   private$.is_fitted = TRUE

                                   # Calculate statistics
                                   private$calculate_statistics(data_tbl)
                                 },
                                 #' @description
                                 #' abnormal_returns Calculate the abnormal returns with given data.
                                 #'
                                 #' @param data_tbl Data frame or tibble containing the data to calculate abnormal returns.
                                 abnormal_returns = function(data_tbl) {
                                   data_tbl %>%
                                     mutate(abnormal_returns = firm_returns - index_returns)
                                 }
                               ),
                               private = list(
                                 calculate_statistics = function(data_tbl) {
                                   estimation_tbl = data_tbl %>%
                                     dplyr::filter(estimation_window == 1)

                                   # residuals & first-order autocorrelation for
                                   # diagnostics
                                   residuals = estimation_tbl$firm_returns - estimation_tbl$index_returns
                                   private$add_residuals(residuals)
                                   private$first_order_autocorrelation(residuals)

                                   # forecast correction term
                                   sigma = sd(residuals)
                                   event_window_tbl = data_tbl %>% filter(event_window == 1)
                                   event_market_returns = event_window_tbl$index_returns
                                   estimation_window_length = nrow(estimation_tbl)
                                   estimation_market_returns = estimation_tbl$index_returns

                                   private$calculate_forecast_error_correction(sigma,
                                                                               estimation_window_length,
                                                                               estimation_market_returns,
                                                                               event_market_returns)
                                 }
                               )
)


#' Comparison Period Mean Adjusted Model
#'
#' The Comparison Period Mean Adjusted Model is another relatively simple
#' approach used in event studies to estimate the expected returns of a stock
#' and calculate its abnormal returns during an event window. This model is
#' based on the assumption that a stock’s expected return during the event
#' window is equal to its average return during a comparison period (typically
#' a pre-event period). This model is particularly useful when researchers want
#' to control for a stock’s historical performance and do not wish to rely on
#' market return data.
#'
#' @export
ComparisonPeriodMeanAdjustedModel <- R6Class("ComparisonPeriodMeanAdjustedModel",
                                             inherit = ModelBase,
                                             public = list(
                                               #' @field model_name Name of the model.
                                               model_name = "ComparisonPeriodMeanAdjustedModel",
                                               #' @description
                                               #' Fit the model with given data.
                                               #'
                                               #' @param data_tbl Data frame or tibble containing the data to fit.
                                               fit = function(data_tbl) {
                                                 data_tbl %>%
                                                   filter(estimation_window == 1) %>%
                                                   .[['firm_returns']] %>%
                                                   mean(.) -> reference_mean

                                                 private$.fitted_model = reference_mean
                                                 private$.is_fitted = TRUE

                                                 # Calculate statistics
                                                 private$calculate_statistics(data_tbl)
                                               },
                                               #' @description
                                               #' Calculate the abnormal returns with given data.
                                               #'
                                               #' @param data_tbl Data frame or tibble containing the data to calculate abnormal returns.
                                               abnormal_returns = function(data_tbl) {
                                                 data_tbl %>%
                                                   mutate(abnormal_returns = firm_returns - private$.fitted_model)
                                               }
                                             ),
                                             private = list(
                                               calculate_statistics = function(data_tbl) {
                                                 estimation_tbl = data_tbl %>%
                                                   dplyr::filter(estimation_window == 1)

                                                 # residuals & first-order autocorrelation for
                                                 # diagnostics
                                                 residuals = estimation_tbl$firm_returns - mean(estimation_tbl$firm_returns)
                                                 private$add_residuals(residuals)
                                                 private$first_order_autocorrelation(residuals)

                                                 # forecast correction term
                                                 sigma = sd(residuals)
                                                 event_window_tbl = data_tbl %>% filter(event_window == 1)
                                                 event_market_returns = event_window_tbl$index_returns
                                                 estimation_window_length = nrow(estimation_tbl)
                                                 estimation_market_returns = estimation_tbl$index_returns

                                                 private$calculate_forecast_error_correction(sigma,
                                                                                             estimation_window_length,
                                                                                             estimation_market_returns,
                                                                                             event_market_returns)
                                               }
                                             )
)



CustomModel <- R6Class("CustomModel",
                       inherit = MarketModel,
                       public = list(
                         model_name = "CustomModel",
                         abnormal_returns = function(data_tbl) {
                           # Calculate abnormal returns
                           mm_model = private$.fitted_model
                           data_tbl %>%
                             mutate(abnormal_returns = firm_returns - predict(mm_model, data_tbl),
                                    abnormal_returns = ifelse(event_date == 1, abnormal_returns + loss_market_cap, abnormal_returns))
                         }
                       )
)



.estimate_garch_model <- function(x=NULL, y, dt=NULL, garch_order=c(1, 1), varModel=F) {
  .estimate = function(x=NULL, y, dt=NULL, garch_order=c(1, 1), varModel=F) {
    varMod <- NULL
    if (varModel) {
      varMod <- list(model               = "sGARCH",
                     garchOrder          = orGarch,
                     submodel            = NULL,
                     external.regressors = cbind(dt))
    }

    # model specification
    model_specification <-
      rugarch::ugarchspec(
        variance.model = varMod,
        mean.model = list(armaOrder           = c(0, 0),
                          include.mean        = TRUE,
                          archm               = FALSE,
                          external.regressors = cbind(x, dt)),
        distribution.model="norm",
        start.pars=list())

    # model fitting
    garch_model <- rugarch::ugarchfit(spec           = modelSpec,
                                      data           = y,
                                      solver         = "nloptr",
                                      solver.control = list(trace=0, solver=10))
  }

  .estimate_safely = purrr::safely(.estimate, otherwise = NULL)
  .estimate_safely(x=NULL, y, dt=NULL, garch_order=c(1, 1), varModel=F)
}


.estimate_mm_model <- function(formula, data) {
  lm(formula, data=data)
}
