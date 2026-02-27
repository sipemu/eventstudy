#' Rolling Window Model
#'
#' Event study model with time-varying parameters estimated via a rolling
#' OLS window over the estimation period. The last rolling window's
#' parameters are used for event-window prediction. This captures parameter
#' instability that is common in financial return data.
#'
#' @export
RollingWindowModel <- R6Class("RollingWindowModel",
                               inherit = ModelBase,
                               public = list(
                                 #' @field model_name Name of the model.
                                 model_name = "RollingWindowModel",
                                 #' @field window_size Rolling window size. Default 60.
                                 window_size = 60L,
                                 #' @field min_obs Minimum observations required. Default 30.
                                 min_obs = 30L,
                                 #' @description
                                 #' Create a new RollingWindowModel.
                                 #'
                                 #' @param window_size Size of the rolling window.
                                 #' @param min_obs Minimum observations for a valid window.
                                 initialize = function(window_size = 60L, min_obs = 30L) {
                                   self$window_size <- as.integer(window_size)
                                   self$min_obs <- as.integer(min_obs)
                                 },
                                 #' @description
                                 #' Fit the rolling window model on the estimation window.
                                 #'
                                 #' @param data_tbl Data frame or tibble with firm_returns,
                                 #'   index_returns, estimation_window, event_window columns.
                                 fit = function(data_tbl) {
                                   estimation_tbl <- data_tbl %>%
                                     dplyr::filter(estimation_window == 1)

                                   n_est <- nrow(estimation_tbl)
                                   if (n_est < self$min_obs) {
                                     private$.is_fitted <- FALSE
                                     warning("RollingWindowModel: insufficient estimation data (",
                                             n_est, " < ", self$min_obs, ")")
                                     return(invisible(self))
                                   }

                                   ws <- min(self$window_size, n_est)
                                   firm_ret <- estimation_tbl$firm_returns
                                   idx_ret <- estimation_tbl$index_returns

                                   # Rolling OLS: y = alpha + beta * x
                                   n_windows <- n_est - ws + 1
                                   alphas <- numeric(n_windows)
                                   betas <- numeric(n_windows)
                                   sigmas <- numeric(n_windows)

                                   for (i in seq_len(n_windows)) {
                                     start <- i
                                     end <- i + ws - 1
                                     y <- firm_ret[start:end]
                                     x <- idx_ret[start:end]

                                     x_bar <- mean(x)
                                     y_bar <- mean(y)
                                     ss_xx <- sum((x - x_bar)^2)
                                     ss_xy <- sum((x - x_bar) * (y - y_bar))

                                     if (ss_xx > 0) {
                                       betas[i] <- ss_xy / ss_xx
                                       alphas[i] <- y_bar - betas[i] * x_bar
                                       resid <- y - (alphas[i] + betas[i] * x)
                                       sigmas[i] <- sqrt(sum(resid^2) / (ws - 2))
                                     } else {
                                       betas[i] <- NA_real_
                                       alphas[i] <- y_bar
                                       sigmas[i] <- stats::sd(y)
                                     }
                                   }

                                   private$.rolling_params <- list(
                                     alphas = alphas, betas = betas, sigmas = sigmas
                                   )
                                   private$.is_fitted <- TRUE
                                   private$calculate_statistics(data_tbl)
                                 },
                                 #' @description
                                 #' Calculate abnormal returns using the last rolling window parameters.
                                 #'
                                 #' @param data_tbl Data frame or tibble.
                                 abnormal_returns = function(data_tbl) {
                                   if (!private$.is_fitted) {
                                     warning("RollingWindowModel is not fitted. Returning NA.")
                                     return(data_tbl %>%
                                              dplyr::mutate(abnormal_returns = NA_real_))
                                   }

                                   alpha_last <- utils::tail(private$.rolling_params$alphas, 1)
                                   beta_last <- utils::tail(private$.rolling_params$betas, 1)

                                   data_tbl %>%
                                     dplyr::mutate(
                                       abnormal_returns = firm_returns -
                                         (alpha_last + beta_last * index_returns)
                                     )
                                 }
                               ),
                               private = list(
                                 .rolling_params = NULL,
                                 calculate_statistics = function(data_tbl) {
                                   alphas <- private$.rolling_params$alphas
                                   betas <- private$.rolling_params$betas
                                   sigmas <- private$.rolling_params$sigmas

                                   alpha_last <- utils::tail(alphas, 1)
                                   beta_last <- utils::tail(betas, 1)
                                   sigma_last <- utils::tail(sigmas, 1)

                                   private$.statistics$alpha <- alpha_last
                                   private$.statistics$beta <- beta_last
                                   private$.statistics$sigma <- sigma_last
                                   private$.statistics$degree_of_freedom <- self$window_size - 2

                                   # Residuals from last window
                                   estimation_tbl <- data_tbl %>%
                                     dplyr::filter(estimation_window == 1)
                                   n_est <- nrow(estimation_tbl)
                                   ws <- min(self$window_size, n_est)
                                   last_window <- utils::tail(estimation_tbl, ws)
                                   residuals <- last_window$firm_returns -
                                     (alpha_last + beta_last * last_window$index_returns)
                                   private$add_residuals(residuals)
                                   private$first_order_autocorrelation(residuals)

                                   # Forecast error correction
                                   event_window_tbl <- data_tbl %>%
                                     dplyr::filter(event_window == 1)
                                   private$calculate_forecast_error_correction(
                                     sigma_last, ws,
                                     last_window$index_returns,
                                     event_window_tbl$index_returns
                                   )

                                   # Store rolling time series for diagnostics
                                   private$.statistics$rolling_alphas <- alphas
                                   private$.statistics$rolling_betas <- betas
                                   private$.statistics$rolling_sigmas <- sigmas
                                 }
                               )
)


#' DCC-GARCH Model
#'
#' Event study model using Dynamic Conditional Correlation GARCH for
#' time-varying beta estimation. Requires the \pkg{rmgarch} package.
#' The bivariate DCC-GARCH model captures both time-varying volatility
#' and time-varying correlation between firm and market returns, yielding
#' a time-varying beta: \eqn{\beta_t = Cov(R_{firm}, R_{market})_t / Var(R_{market})_t}.
#'
#' @export
DCCGARCHModel <- R6Class("DCCGARCHModel",
                           inherit = ModelBase,
                           public = list(
                             #' @field model_name Name of the model.
                             model_name = "DCCGARCHModel",
                             #' @field garch_order GARCH order for each univariate model. Default c(1,1).
                             garch_order = c(1, 1),
                             #' @field dcc_order DCC order. Default c(1,1).
                             dcc_order = c(1, 1),
                             #' @description
                             #' Create a new DCCGARCHModel.
                             #'
                             #' @param garch_order GARCH(p,q) order for univariate models.
                             #' @param dcc_order DCC(a,b) order.
                             initialize = function(garch_order = c(1, 1),
                                                    dcc_order = c(1, 1)) {
                               self$garch_order <- garch_order
                               self$dcc_order <- dcc_order
                             },
                             #' @description
                             #' Fit the DCC-GARCH model on the estimation window.
                             #'
                             #' @param data_tbl Data frame or tibble with firm_returns,
                             #'   index_returns, estimation_window, event_window columns.
                             fit = function(data_tbl) {
                               if (!requireNamespace("rmgarch", quietly = TRUE)) {
                                 stop("Package 'rmgarch' is required for DCCGARCHModel. ",
                                      "Install it with: install.packages('rmgarch')")
                               }
                               if (!requireNamespace("rugarch", quietly = TRUE)) {
                                 stop("Package 'rugarch' is required for DCCGARCHModel. ",
                                      "Install it with: install.packages('rugarch')")
                               }

                               estimation_tbl <- data_tbl %>%
                                 dplyr::filter(estimation_window == 1)

                               returns_mat <- cbind(estimation_tbl$firm_returns,
                                                     estimation_tbl$index_returns)

                               # Univariate GARCH specs
                               uspec <- rugarch::ugarchspec(
                                 variance.model = list(model = "sGARCH",
                                                        garchOrder = self$garch_order),
                                 mean.model = list(armaOrder = c(0, 0),
                                                    include.mean = TRUE),
                                 distribution.model = "norm"
                               )
                               multi_uspec <- rugarch::multispec(
                                 replicate(2, uspec)
                               )

                               # DCC spec
                               dcc_spec <- rmgarch::dccspec(
                                 uspec = multi_uspec,
                                 dccOrder = self$dcc_order,
                                 distribution = "mvnorm"
                               )

                               safe_fit <- purrr::safely(rmgarch::dccfit)
                               res <- safe_fit(dcc_spec, data = returns_mat)

                               if (is.null(res$error)) {
                                 private$.fitted_model <- res$result
                                 private$.is_fitted <- TRUE
                                 private$calculate_statistics(data_tbl)
                               } else {
                                 private$.is_fitted <- FALSE
                                 private$.error <- res$error
                                 warning("DCC-GARCH fitting failed: ",
                                         conditionMessage(res$error))
                               }
                             },
                             #' @description
                             #' Calculate abnormal returns using the last conditional beta.
                             #'
                             #' @param data_tbl Data frame or tibble.
                             abnormal_returns = function(data_tbl) {
                               if (!private$.is_fitted) {
                                 warning("DCCGARCHModel is not fitted. Returning NA.")
                                 return(data_tbl %>%
                                          dplyr::mutate(abnormal_returns = NA_real_))
                               }

                               alpha_last <- private$.statistics$alpha
                               beta_last <- private$.statistics$beta

                               data_tbl %>%
                                 dplyr::mutate(
                                   abnormal_returns = firm_returns -
                                     (alpha_last + beta_last * index_returns)
                                 )
                             }
                           ),
                           private = list(
                             calculate_statistics = function(data_tbl) {
                               dcc_fit <- private$.fitted_model

                               # Extract conditional covariance matrices
                               # rcov returns an array: [2, 2, T]
                               H <- rmgarch::rcov(dcc_fit)
                               n_t <- dim(H)[3]

                               # Time-varying beta: Cov(firm, market) / Var(market)
                               beta_t <- numeric(n_t)
                               for (t in seq_len(n_t)) {
                                 beta_t[t] <- H[1, 2, t] / H[2, 2, t]
                               }

                               # Use last beta for prediction
                               beta_last <- beta_t[n_t]

                               # Extract conditional sigma for firm
                               sigma_t <- sqrt(H[1, 1, ])
                               sigma_last <- sigma_t[n_t]

                               # Mean equation intercept
                               estimation_tbl <- data_tbl %>%
                                 dplyr::filter(estimation_window == 1)
                               alpha_last <- mean(estimation_tbl$firm_returns -
                                                    beta_last * estimation_tbl$index_returns,
                                                  na.rm = TRUE)

                               private$.statistics$alpha <- alpha_last
                               private$.statistics$beta <- beta_last
                               private$.statistics$sigma <- mean(sigma_t, na.rm = TRUE)
                               private$.statistics$degree_of_freedom <- n_t - 4
                               private$.statistics$beta_t <- beta_t
                               private$.statistics$sigma_t <- sigma_t

                               # Residuals from estimation window
                               residuals <- estimation_tbl$firm_returns -
                                 (alpha_last + beta_last * estimation_tbl$index_returns)
                               private$add_residuals(residuals)
                               if (length(residuals) >= 2) {
                                 private$first_order_autocorrelation(residuals)
                               }

                               # Forecast error correction
                               event_window_tbl <- data_tbl %>%
                                 dplyr::filter(event_window == 1)
                               private$calculate_forecast_error_correction(
                                 mean(sigma_t, na.rm = TRUE),
                                 nrow(estimation_tbl),
                                 estimation_tbl$index_returns,
                                 event_window_tbl$index_returns
                               )
                             }
                           )
)
