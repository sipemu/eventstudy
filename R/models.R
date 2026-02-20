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



#' Linear Factor Model Base
#'
#' Base class for multi-factor OLS models used in event studies. All factor
#' models (Market Model, Fama-French, Carhart) share the same estimation
#' approach: OLS regression of excess returns on factor returns during the
#' estimation window.
#'
#' @export
LinearFactorModel <- R6Class("LinearFactorModel",
                              inherit = ModelBase,
                              public = list(
                                #' @field model_name Name of the model.
                                model_name = "LinearFactorModel",
                                #' @field formula The regression formula.
                                formula = NULL,
                                #' @field required_columns Columns required in the data.
                                required_columns = c("firm_returns", "index_returns"),
                                #' @description
                                #' Fit the linear factor model via OLS on the estimation window.
                                #'
                                #' @param data_tbl Data frame or tibble containing the data to fit.
                                fit = function(data_tbl) {
                                  # Validate required columns
                                  missing_cols <- setdiff(self$required_columns, names(data_tbl))
                                  if (length(missing_cols) > 0) {
                                    stop(self$model_name, " requires columns: ",
                                         paste(missing_cols, collapse = ", "))
                                  }

                                  estimation_tbl <- data_tbl %>%
                                    dplyr::filter(estimation_window == 1)

                                  safe_lm <- purrr::safely(.f = .estimate_mm_model)
                                  res <- safe_lm(self$formula, estimation_tbl)

                                  if (is.null(res$error)) {
                                    private$.fitted_model <- res$result
                                    private$.is_fitted <- TRUE
                                    private$calculate_statistics(data_tbl)
                                  } else {
                                    private$.is_fitted <- FALSE
                                    private$.error <- res$error
                                    warning("Model fitting failed: ", conditionMessage(res$error))
                                  }
                                },
                                #' @description
                                #' Calculate abnormal returns as observed minus predicted.
                                #'
                                #' @param data_tbl Data frame or tibble.
                                abnormal_returns = function(data_tbl) {
                                  if (private$.is_fitted) {
                                    predicted <- predict(private$.fitted_model, newdata = data_tbl)
                                    data_tbl %>%
                                      dplyr::mutate(abnormal_returns = firm_returns - predicted)
                                  } else {
                                    warning(self$model_name, " is not fitted. Returning NA abnormal returns.")
                                    data_tbl %>%
                                      dplyr::mutate(abnormal_returns = NA_real_)
                                  }
                                }
                              ),
                              private = list(
                                calculate_statistics = function(data_tbl) {
                                  mod <- private$.fitted_model
                                  mod_summary <- summary(mod)

                                  # Store all coefficients
                                  coefs <- mod$coefficients
                                  coef_names <- names(coefs)
                                  for (i in seq_along(coefs)) {
                                    val <- coefs[i]
                                    names(val) <- NULL
                                    private$.statistics[[coef_names[i]]] <- val
                                    if (nrow(mod_summary$coefficients) >= i) {
                                      private$.statistics[[paste0("pval_", coef_names[i])]] <-
                                        mod_summary$coefficients[i, 4]
                                    }
                                  }

                                  # Map intercept to alpha, first factor to beta for compatibility
                                  private$.statistics$alpha <- coefs[1]
                                  names(private$.statistics$alpha) <- NULL
                                  private$.statistics$pval_alpha <- mod_summary$coefficients[1, 4]
                                  if (length(coefs) >= 2) {
                                    private$.statistics$beta <- coefs[2]
                                    names(private$.statistics$beta) <- NULL
                                    private$.statistics$pval_beta <- mod_summary$coefficients[2, 4]
                                  }

                                  private$.statistics$sigma <- mod_summary$sigma
                                  private$.statistics$r2 <- mod_summary$r.squared
                                  f_stat <- mod_summary$fstatistic[1]
                                  names(f_stat) <- NULL
                                  private$.statistics$f_stat <- f_stat
                                  private$.statistics$degree_of_freedom <- mod$df.residual

                                  residuals <- mod$residuals
                                  private$add_residuals(residuals)
                                  private$first_order_autocorrelation(residuals)

                                  # Forecast error correction
                                  estimation_tbl <- data_tbl %>% dplyr::filter(estimation_window == 1)
                                  event_window_tbl <- data_tbl %>% dplyr::filter(event_window == 1)
                                  private$calculate_forecast_error_correction(
                                    mod_summary$sigma,
                                    nrow(estimation_tbl),
                                    estimation_tbl$index_returns,
                                    event_window_tbl$index_returns
                                  )
                                }
                              )
)


#' Fama-French Three-Factor Model
#'
#' Implements the Fama and French (1993) three-factor model:
#' \deqn{R_i - R_f = \alpha + \beta_m (R_m - R_f) + \beta_s SMB + \beta_h HML + \epsilon}
#'
#' The data must contain columns: \code{excess_return} (firm return minus risk-free),
#' \code{market_excess} (market return minus risk-free), \code{smb}, and \code{hml}.
#' These can be joined via a factor table in \code{EventStudyTask}.
#'
#' @export
FamaFrench3FactorModel <- R6Class("FamaFrench3FactorModel",
                                   inherit = LinearFactorModel,
                                   public = list(
                                     #' @field model_name Name of the model.
                                     model_name = "FamaFrench3FactorModel",
                                     #' @field formula The three-factor regression formula.
                                     formula = stats::as.formula(
                                       "excess_return ~ market_excess + smb + hml"
                                     ),
                                     #' @field required_columns Required data columns.
                                     required_columns = c("excess_return", "market_excess",
                                                          "smb", "hml"),
                                     #' @description
                                     #' Calculate abnormal returns using the three-factor model.
                                     #'
                                     #' @param data_tbl Data frame or tibble.
                                     abnormal_returns = function(data_tbl) {
                                       if (private$.is_fitted) {
                                         predicted <- predict(private$.fitted_model, newdata = data_tbl)
                                         data_tbl %>%
                                           dplyr::mutate(abnormal_returns = excess_return - predicted)
                                       } else {
                                         warning(self$model_name, " is not fitted.")
                                         data_tbl %>%
                                           dplyr::mutate(abnormal_returns = NA_real_)
                                       }
                                     }
                                   )
)


#' Fama-French Five-Factor Model
#'
#' Implements the Fama and French (2015) five-factor model:
#' \deqn{R_i - R_f = \alpha + \beta_m (R_m - R_f) + \beta_s SMB + \beta_h HML + \beta_r RMW + \beta_c CMA + \epsilon}
#'
#' Requires columns: \code{excess_return}, \code{market_excess}, \code{smb},
#' \code{hml}, \code{rmw}, \code{cma}.
#'
#' @export
FamaFrench5FactorModel <- R6Class("FamaFrench5FactorModel",
                                   inherit = LinearFactorModel,
                                   public = list(
                                     #' @field model_name Name of the model.
                                     model_name = "FamaFrench5FactorModel",
                                     #' @field formula The five-factor regression formula.
                                     formula = stats::as.formula(
                                       "excess_return ~ market_excess + smb + hml + rmw + cma"
                                     ),
                                     #' @field required_columns Required data columns.
                                     required_columns = c("excess_return", "market_excess",
                                                          "smb", "hml", "rmw", "cma"),
                                     #' @description
                                     #' Calculate abnormal returns using the five-factor model.
                                     #'
                                     #' @param data_tbl Data frame or tibble.
                                     abnormal_returns = function(data_tbl) {
                                       if (private$.is_fitted) {
                                         predicted <- predict(private$.fitted_model, newdata = data_tbl)
                                         data_tbl %>%
                                           dplyr::mutate(abnormal_returns = excess_return - predicted)
                                       } else {
                                         warning(self$model_name, " is not fitted.")
                                         data_tbl %>%
                                           dplyr::mutate(abnormal_returns = NA_real_)
                                       }
                                     }
                                   )
)


#' Carhart Four-Factor Model
#'
#' Implements the Carhart (1997) four-factor model, which extends the
#' Fama-French three-factor model with a momentum factor:
#' \deqn{R_i - R_f = \alpha + \beta_m (R_m - R_f) + \beta_s SMB + \beta_h HML + \beta_{mom} MOM + \epsilon}
#'
#' Requires columns: \code{excess_return}, \code{market_excess}, \code{smb},
#' \code{hml}, \code{mom}.
#'
#' @export
Carhart4FactorModel <- R6Class("Carhart4FactorModel",
                                inherit = LinearFactorModel,
                                public = list(
                                  #' @field model_name Name of the model.
                                  model_name = "Carhart4FactorModel",
                                  #' @field formula The four-factor regression formula.
                                  formula = stats::as.formula(
                                    "excess_return ~ market_excess + smb + hml + mom"
                                  ),
                                  #' @field required_columns Required data columns.
                                  required_columns = c("excess_return", "market_excess",
                                                       "smb", "hml", "mom"),
                                  #' @description
                                  #' Calculate abnormal returns using the four-factor model.
                                  #'
                                  #' @param data_tbl Data frame or tibble.
                                  abnormal_returns = function(data_tbl) {
                                    if (private$.is_fitted) {
                                      predicted <- predict(private$.fitted_model, newdata = data_tbl)
                                      data_tbl %>%
                                        dplyr::mutate(abnormal_returns = excess_return - predicted)
                                    } else {
                                      warning(self$model_name, " is not fitted.")
                                      data_tbl %>%
                                        dplyr::mutate(abnormal_returns = NA_real_)
                                    }
                                  }
                                )
)


#' GARCH Model
#'
#' Event study model using GARCH(1,1) for time-varying volatility estimation.
#' Uses the \pkg{rugarch} package to fit a GARCH(1,1) model with a market
#' return regressor in the mean equation during the estimation window.
#' Abnormal returns are computed as the difference between observed returns
#' and the GARCH conditional mean. The time-varying sigma from GARCH can
#' be used for standardized test statistics.
#'
#' @export
GARCHModel <- R6Class("GARCHModel",
                       inherit = ModelBase,
                       public = list(
                         #' @field model_name Name of the model.
                         model_name = "GARCHModel",
                         #' @field garch_order GARCH order as c(p, q). Default c(1,1).
                         garch_order = c(1, 1),
                         #' @description
                         #' Fit the GARCH model on the estimation window.
                         #'
                         #' @param data_tbl Data frame or tibble with firm_returns,
                         #'   index_returns, estimation_window, event_window columns.
                         fit = function(data_tbl) {
                           if (!requireNamespace("rugarch", quietly = TRUE)) {
                             stop("Package 'rugarch' is required for GARCHModel. ",
                                  "Install it with: install.packages('rugarch')")
                           }

                           estimation_tbl <- data_tbl %>%
                             dplyr::filter(estimation_window == 1)

                           spec <- rugarch::ugarchspec(
                             variance.model = list(
                               model = "sGARCH",
                               garchOrder = self$garch_order
                             ),
                             mean.model = list(
                               armaOrder = c(0, 0),
                               include.mean = TRUE,
                               external.regressors = as.matrix(estimation_tbl$index_returns)
                             ),
                             distribution.model = "norm"
                           )

                           safe_fit <- purrr::safely(rugarch::ugarchfit)
                           res <- safe_fit(
                             spec = spec,
                             data = estimation_tbl$firm_returns,
                             solver = "hybrid",
                             solver.control = list(trace = 0)
                           )

                           if (is.null(res$error)) {
                             private$.fitted_model <- res$result
                             private$.is_fitted <- TRUE
                             private$calculate_statistics(data_tbl)
                           } else {
                             private$.is_fitted <- FALSE
                             private$.error <- res$error
                             warning("GARCH model fitting failed: ", conditionMessage(res$error))
                           }
                         },
                         #' @description
                         #' Calculate abnormal returns from the GARCH model.
                         #'
                         #' @param data_tbl Data frame or tibble.
                         abnormal_returns = function(data_tbl) {
                           if (!private$.is_fitted) {
                             warning("GARCHModel is not fitted. Returning NA abnormal returns.")
                             return(data_tbl %>% dplyr::mutate(abnormal_returns = NA_real_))
                           }

                           garch_fit <- private$.fitted_model
                           coefs <- rugarch::coef(garch_fit)
                           mu <- coefs["mu"]
                           mxreg1 <- coefs["mxreg1"]

                           data_tbl %>%
                             dplyr::mutate(
                               expected_return = mu + mxreg1 * index_returns,
                               abnormal_returns = firm_returns - expected_return
                             ) %>%
                             dplyr::select(-expected_return)
                         }
                       ),
                       private = list(
                         calculate_statistics = function(data_tbl) {
                           garch_fit <- private$.fitted_model

                           # Extract conditional sigma from estimation window
                           cond_sigma <- as.numeric(rugarch::sigma(garch_fit))
                           avg_sigma <- mean(cond_sigma, na.rm = TRUE)

                           coefs <- rugarch::coef(garch_fit)
                           private$.statistics$alpha <- coefs["mu"]
                           private$.statistics$beta <- coefs["mxreg1"]
                           private$.statistics$sigma <- avg_sigma
                           private$.statistics$garch_sigma <- cond_sigma
                           private$.statistics$degree_of_freedom <-
                             length(cond_sigma) - length(coefs)

                           # Residuals
                           residuals <- as.numeric(rugarch::residuals(garch_fit))
                           private$add_residuals(residuals)
                           private$first_order_autocorrelation(residuals)

                           # Forecast error correction (using average sigma)
                           estimation_tbl <- data_tbl %>% dplyr::filter(estimation_window == 1)
                           event_window_tbl <- data_tbl %>% dplyr::filter(event_window == 1)
                           private$calculate_forecast_error_correction(
                             avg_sigma,
                             nrow(estimation_tbl),
                             estimation_tbl$index_returns,
                             event_window_tbl$index_returns
                           )
                         }
                       )
)


#' Buy-and-Hold Abnormal Returns (BHAR) Model
#'
#' Implements Buy-and-Hold Abnormal Returns for long-horizon event studies.
#' BHAR compounds returns over the event window instead of summing:
#' \deqn{BHAR_i = \prod(1 + R_{i,t}) - \prod(1 + R_{benchmark,t})}
#'
#' The benchmark is the market/index return by default. This model is
#' appropriate for long-horizon studies (months/years) where compounding
#' effects matter.
#'
#' @export
BHARModel <- R6Class("BHARModel",
                      inherit = ModelBase,
                      public = list(
                        #' @field model_name Name of the model.
                        model_name = "BHARModel",
                        #' @description
                        #' Fit the BHAR model. Computes estimation window statistics.
                        #'
                        #' @param data_tbl Data frame or tibble.
                        fit = function(data_tbl) {
                          private$.is_fitted <- TRUE
                          private$calculate_statistics(data_tbl)
                        },
                        #' @description
                        #' Calculate abnormal returns using buy-and-hold compounding.
                        #'
                        #' @param data_tbl Data frame or tibble.
                        abnormal_returns = function(data_tbl) {
                          data_tbl %>%
                            dplyr::mutate(
                              # Running compounded returns
                              cum_firm = cumprod(1 + dplyr::coalesce(firm_returns, 0)),
                              cum_index = cumprod(1 + dplyr::coalesce(index_returns, 0)),
                              # BHAR at each point
                              abnormal_returns = cum_firm - cum_index
                            ) %>%
                            dplyr::select(-cum_firm, -cum_index)
                        }
                      ),
                      private = list(
                        calculate_statistics = function(data_tbl) {
                          estimation_tbl <- data_tbl %>%
                            dplyr::filter(estimation_window == 1)

                          # Compute estimation-window BHAR residuals
                          est_bhar <- cumprod(1 + estimation_tbl$firm_returns) -
                            cumprod(1 + estimation_tbl$index_returns)
                          # Use incremental differences as residual proxy
                          residuals <- diff(est_bhar)
                          private$add_residuals(residuals)
                          if (length(residuals) >= 2) {
                            private$first_order_autocorrelation(residuals)
                          }

                          sigma <- sd(estimation_tbl$firm_returns -
                                        estimation_tbl$index_returns, na.rm = TRUE)
                          private$.statistics$sigma <- sigma
                          private$.statistics$degree_of_freedom <- length(residuals) - 1

                          # Forecast error correction
                          event_window_tbl <- data_tbl %>% dplyr::filter(event_window == 1)
                          private$calculate_forecast_error_correction(
                            sigma, nrow(estimation_tbl),
                            estimation_tbl$index_returns,
                            event_window_tbl$index_returns
                          )
                        }
                      )
)


#' Volume Event Study Model
#'
#' Model for volume-based event studies. Computes abnormal volume as the
#' difference between observed volume and expected volume from the estimation
#' window mean. The data must contain a \code{firm_volume} column (and
#' optionally \code{index_volume} for market-adjusted volume).
#'
#' The existing test statistics infrastructure works on the
#' \code{abnormal_returns} column, so this model writes abnormal volume
#' to that same column for compatibility.
#'
#' @export
VolumeModel <- R6Class("VolumeModel",
                        inherit = ModelBase,
                        public = list(
                          #' @field model_name Name of the model.
                          model_name = "VolumeModel",
                          #' @field log_transform Whether to log-transform volume. Default TRUE.
                          log_transform = TRUE,
                          #' @description
                          #' Create a new VolumeModel.
                          #'
                          #' @param log_transform Whether to log-transform volume before analysis.
                          initialize = function(log_transform = TRUE) {
                            self$log_transform <- log_transform
                          },
                          #' @description
                          #' Fit the volume model. Computes expected volume from estimation window.
                          #'
                          #' @param data_tbl Data frame or tibble with firm_volume column.
                          fit = function(data_tbl) {
                            if (!"firm_volume" %in% names(data_tbl)) {
                              stop("VolumeModel requires a 'firm_volume' column.")
                            }

                            estimation_tbl <- data_tbl %>%
                              dplyr::filter(estimation_window == 1)

                            vol <- estimation_tbl$firm_volume
                            if (self$log_transform) vol <- log(vol + 1)

                            private$.fitted_model <- mean(vol, na.rm = TRUE)
                            private$.is_fitted <- TRUE
                            private$calculate_statistics(data_tbl)
                          },
                          #' @description
                          #' Calculate abnormal volume.
                          #'
                          #' @param data_tbl Data frame or tibble.
                          abnormal_returns = function(data_tbl) {
                            expected <- private$.fitted_model
                            data_tbl %>%
                              dplyr::mutate(
                                .vol = if (self$log_transform) log(firm_volume + 1) else firm_volume,
                                abnormal_returns = .vol - expected
                              ) %>%
                              dplyr::select(-.vol)
                          }
                        ),
                        private = list(
                          calculate_statistics = function(data_tbl) {
                            estimation_tbl <- data_tbl %>%
                              dplyr::filter(estimation_window == 1)

                            vol <- estimation_tbl$firm_volume
                            if (self$log_transform) vol <- log(vol + 1)

                            expected <- mean(vol, na.rm = TRUE)
                            residuals <- vol - expected
                            private$add_residuals(residuals)
                            if (length(residuals) >= 2) {
                              private$first_order_autocorrelation(residuals)
                            }

                            sigma <- sd(residuals, na.rm = TRUE)
                            private$.statistics$sigma <- sigma
                            private$.statistics$degree_of_freedom <- length(residuals) - 1

                            event_window_tbl <- data_tbl %>% dplyr::filter(event_window == 1)
                            private$calculate_forecast_error_correction(
                              sigma, nrow(estimation_tbl),
                              estimation_tbl$index_returns,
                              event_window_tbl$index_returns
                            )
                          }
                        )
)


#' Volatility Event Study Model
#'
#' Model for volatility-based event studies. Computes abnormal volatility
#' as the ratio of event-window squared returns to estimation-window variance.
#' The abnormal measure is written to the \code{abnormal_returns} column
#' for compatibility with existing test statistics.
#'
#' @export
VolatilityModel <- R6Class("VolatilityModel",
                            inherit = ModelBase,
                            public = list(
                              #' @field model_name Name of the model.
                              model_name = "VolatilityModel",
                              #' @description
                              #' Fit the volatility model. Estimates expected variance from
                              #' estimation window.
                              #'
                              #' @param data_tbl Data frame or tibble.
                              fit = function(data_tbl) {
                                estimation_tbl <- data_tbl %>%
                                  dplyr::filter(estimation_window == 1)

                                est_var <- var(estimation_tbl$firm_returns, na.rm = TRUE)
                                private$.fitted_model <- est_var
                                private$.is_fitted <- TRUE
                                private$calculate_statistics(data_tbl)
                              },
                              #' @description
                              #' Calculate abnormal volatility (squared returns / expected variance - 1).
                              #'
                              #' @param data_tbl Data frame or tibble.
                              abnormal_returns = function(data_tbl) {
                                est_var <- private$.fitted_model
                                data_tbl %>%
                                  dplyr::mutate(
                                    abnormal_returns = (firm_returns^2 / est_var) - 1
                                  )
                              }
                            ),
                            private = list(
                              calculate_statistics = function(data_tbl) {
                                estimation_tbl <- data_tbl %>%
                                  dplyr::filter(estimation_window == 1)

                                est_var <- var(estimation_tbl$firm_returns, na.rm = TRUE)
                                # Residuals: squared returns minus expected variance
                                residuals <- estimation_tbl$firm_returns^2 - est_var
                                private$add_residuals(residuals)
                                if (length(residuals) >= 2) {
                                  private$first_order_autocorrelation(residuals)
                                }

                                sigma <- sd(residuals, na.rm = TRUE)
                                private$.statistics$sigma <- sigma
                                private$.statistics$degree_of_freedom <- length(residuals) - 1

                                event_window_tbl <- data_tbl %>% dplyr::filter(event_window == 1)
                                private$calculate_forecast_error_correction(
                                  sigma, nrow(estimation_tbl),
                                  estimation_tbl$index_returns,
                                  event_window_tbl$index_returns
                                )
                              }
                            )
)


.estimate_mm_model <- function(formula, data) {
  lm(formula, data=data)
}
