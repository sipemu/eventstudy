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

                                   # Resolve degenerate mode once per fit call
                                   mode <- .resolve_degenerate_mode(self$degenerate_mode)

                                   # Guard 1: insufficient finite observations
                                   # Use finite-pair count (not nrow) to catch NA-heavy windows
                                   n_valid <- sum(!is.na(estimation_tbl$firm_returns) &
                                                    !is.na(estimation_tbl$index_returns))
                                   if (n_valid < self$min_obs) {
                                     .handle_degenerate(
                                       mode        = mode,
                                       condition   = paste0("insufficient estimation observations (",
                                                            n_valid, " valid, need ", self$min_obs, ")"),
                                       component   = self$model_name,
                                       event_id    = self$event_id,
                                       firm_symbol = self$firm_symbol,
                                       private_env = private
                                     )
                                     private$.is_fitted <- FALSE
                                     return(invisible(self))
                                   }

                                   n_est <- nrow(estimation_tbl)
                                   ws <- min(self$window_size, n_est)

                                   # Guard 2: effective window size too small for OLS
                                   if (ws < 3) {
                                     .handle_degenerate(
                                       mode        = mode,
                                       condition   = paste0("effective window size (", ws,
                                                            ") must be >= 3 for OLS with 2 parameters"),
                                       component   = self$model_name,
                                       event_id    = self$event_id,
                                       firm_symbol = self$firm_symbol,
                                       private_env = private
                                     )
                                     private$.is_fitted <- FALSE
                                     return(invisible(self))
                                   }

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

                                     x_bar <- mean(x, na.rm = TRUE)
                                     y_bar <- mean(y, na.rm = TRUE)
                                     ss_xx <- sum((x - x_bar)^2, na.rm = TRUE)
                                     ss_xy <- sum((x - x_bar) * (y - y_bar), na.rm = TRUE)

                                     if (ss_xx > 0) {
                                       betas[i] <- ss_xy / ss_xx
                                       alphas[i] <- y_bar - betas[i] * x_bar
                                       resid <- y - (alphas[i] + betas[i] * x)
                                       n_valid_resid <- sum(!is.na(resid))
                                       denom <- max(n_valid_resid - 2, 1)
                                       sigmas[i] <- sqrt(sum(resid^2, na.rm = TRUE) / denom)
                                     } else {
                                       betas[i] <- NA_real_
                                       alphas[i] <- y_bar
                                       sigmas[i] <- stats::sd(y, na.rm = TRUE)
                                     }
                                   }

                                   private$.rolling_params <- list(
                                     alphas = alphas, betas = betas, sigmas = sigmas
                                   )

                                   alpha_last <- utils::tail(alphas, 1)
                                   beta_last <- utils::tail(betas, 1)

                                   # Guard 3: last-window parameters NA (e.g. zero-variance window)
                                   if (is.na(alpha_last) || is.na(beta_last)) {
                                     .handle_degenerate(
                                       mode        = mode,
                                       condition   = "last window parameters are NA; model cannot produce valid predictions",
                                       component   = self$model_name,
                                       event_id    = self$event_id,
                                       firm_symbol = self$firm_symbol,
                                       private_env = private
                                     )
                                     private$.is_fitted <- FALSE
                                     return(invisible(self))
                                   }

                                   private$.is_fitted <- TRUE
                                   private$calculate_statistics(data_tbl)
                                 },
                                 #' @description
                                 #' Calculate abnormal returns using the last rolling window parameters.
                                 #'
                                 #' @param data_tbl Data frame or tibble.
                                 abnormal_returns = function(data_tbl) {
                                   if (private$.is_fitted) {
                                     alpha_last <- utils::tail(private$.rolling_params$alphas, 1)
                                     beta_last <- utils::tail(private$.rolling_params$betas, 1)
                                     data_tbl %>%
                                       dplyr::mutate(
                                         abnormal_returns = firm_returns -
                                           (alpha_last + beta_last * index_returns)
                                       )
                                   } else if (private$.degenerate_handled) {
                                     # fit() already emitted one contract-formatted warning;
                                     # return NA silently to honour the one-warning guarantee.
                                     data_tbl %>%
                                       dplyr::mutate(abnormal_returns = NA_real_)
                                   } else {
                                     warning(self$model_name, " is not fitted. Returning NA abnormal returns.")
                                     data_tbl %>%
                                       dplyr::mutate(abnormal_returns = NA_real_)
                                   }
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
                                   # Residuals from last window
                                   estimation_tbl <- data_tbl %>%
                                     dplyr::filter(estimation_window == 1)
                                   # Use finite pair count (not nrow) so NA-heavy estimation windows
                                   # do not inflate df and produce overly permissive t-statistics.
                                   # fit() already uses n_valid for the min-obs guard; calculate_statistics()
                                   # must match to stay consistent — WR-04 fix.
                                   n_valid_est <- sum(!is.na(estimation_tbl$firm_returns) &
                                                        !is.na(estimation_tbl$index_returns))
                                   ws <- min(self$window_size, n_valid_est)
                                   private$.statistics$degree_of_freedom <- max(ws - 2L, 1L)
                                   last_window <- utils::tail(estimation_tbl, ws)
                                   residuals <- last_window$firm_returns -
                                     (alpha_last + beta_last * last_window$index_returns)
                                   private$add_residuals(residuals)
                                   if (length(stats::na.omit(residuals)) >= 2) {
                                     private$first_order_autocorrelation(residuals)
                                   }

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

                               # --- PRE-CALL contract guards (Phase 2) ---
                               # Inserted BEFORE returns_mat <- cbind(...).
                               # The existing purrr::safely(dccfit) / rcov / warning
                               # failure-handling below is Phase 3 and left untouched.

                               # Resolve degenerate mode once per fit call
                               mode <- .resolve_degenerate_mode(self$degenerate_mode)

                               # Guard 1: insufficient finite (firm, index) pairs
                               n_valid <- sum(!is.na(estimation_tbl$firm_returns) &
                                               !is.na(estimation_tbl$index_returns))
                               if (n_valid < 2) {
                                 .handle_degenerate(
                                   mode        = mode,
                                   condition   = paste0("insufficient estimation observations (",
                                                        n_valid, " valid, need 2)"),
                                   component   = self$model_name,
                                   event_id    = self$event_id,
                                   firm_symbol = self$firm_symbol,
                                   private_env = private
                                 )
                                 private$.is_fitted <- FALSE
                                 return(invisible(self))
                               }

                               # Guard 2: zero or near-zero variance in either series
                               # DCC-GARCH requires BOTH series to have variance; zero variance
                               # in either series makes the univariate GARCH sub-model degenerate.
                               sd_firm  <- stats::sd(estimation_tbl$firm_returns,  na.rm = TRUE)
                               sd_index <- stats::sd(estimation_tbl$index_returns, na.rm = TRUE)
                               if (sd_firm < .Machine$double.eps) {
                                 .handle_degenerate(
                                   mode        = mode,
                                   condition   = "zero or near-zero variance in firm_returns",
                                   component   = self$model_name,
                                   event_id    = self$event_id,
                                   firm_symbol = self$firm_symbol,
                                   private_env = private
                                 )
                                 private$.is_fitted <- FALSE
                                 return(invisible(self))
                               }
                               if (sd_index < .Machine$double.eps) {
                                 .handle_degenerate(
                                   mode        = mode,
                                   condition   = "zero or near-zero variance in index_returns",
                                   component   = self$model_name,
                                   event_id    = self$event_id,
                                   firm_symbol = self$firm_symbol,
                                   private_env = private
                                 )
                                 private$.is_fitted <- FALSE
                                 return(invisible(self))
                               }
                               # --- end PRE-CALL guards ---

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
                                 # Check convergence of underlying GARCH fits.
                                 # rmgarch does not expose a single convergence flag; we probe
                                 # the conditional covariance matrices via rcov. Distinguish two
                                 # outcomes so failures route to the correct handler:
                                 #   - rcov ERRORS            -> statistics-extraction failure
                                 #   - rcov returns non-finite -> genuine non-convergence
                                 conv_probe <- tryCatch(
                                   list(ok = all(is.finite(rmgarch::rcov(res$result))),
                                        err = NULL),
                                   error = function(e) list(ok = NA, err = e)
                                 )
                                 if (!is.null(conv_probe$err)) {
                                   # rcov (or covariance extraction) failed -> treat as a
                                   # calculate_statistics failure so the message + one-warning
                                   # contract match the statistics-failure path below.
                                   private$.is_fitted <- FALSE
                                   private$.degenerate_handled <- TRUE  # suppress second warning in abnormal_returns()
                                   warning("DCC-GARCH model statistics computation failed: ",
                                           conditionMessage(conv_probe$err),
                                           ". Returning NA abnormal returns.", call. = FALSE)
                                 } else if (isTRUE(conv_probe$ok)) {
                                   private$.fitted_model <- res$result
                                   private$.is_fitted <- TRUE
                                   tryCatch(
                                     private$calculate_statistics(data_tbl),
                                     error = function(e) {
                                       private$.is_fitted <- FALSE
                                       private$.degenerate_handled <- TRUE  # suppress second warning in abnormal_returns()
                                       warning("DCC-GARCH model statistics computation failed: ",
                                               conditionMessage(e),
                                               ". Returning NA abnormal returns.", call. = FALSE)
                                     }
                                   )
                                 } else {
                                   private$.is_fitted <- FALSE
                                   private$.degenerate_handled <- TRUE  # suppress second warning in abnormal_returns()
                                   warning("DCC-GARCH model produced non-finite covariance. Returning NA.")
                                 }
                               } else {
                                 private$.is_fitted <- FALSE
                                 private$.degenerate_handled <- TRUE  # suppress second warning in abnormal_returns()
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
                               if (private$.is_fitted) {
                                 alpha_last <- private$.statistics$alpha
                                 beta_last <- private$.statistics$beta
                                 data_tbl %>%
                                   dplyr::mutate(
                                     abnormal_returns = firm_returns -
                                       (alpha_last + beta_last * index_returns)
                                   )
                               } else if (private$.degenerate_handled) {
                                 # fit() already emitted one contract-formatted warning;
                                 # return NA silently to honour the one-warning guarantee.
                                 data_tbl %>%
                                   dplyr::mutate(abnormal_returns = NA_real_)
                               } else {
                                 warning(self$model_name, " is not fitted. Returning NA abnormal returns.")
                                 data_tbl %>%
                                   dplyr::mutate(abnormal_returns = NA_real_)
                               }
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
                                 mkt_var <- H[2, 2, t]
                                 if (is.finite(mkt_var) && mkt_var > .Machine$double.eps) {
                                   beta_t[t] <- H[1, 2, t] / mkt_var
                                 } else {
                                   beta_t[t] <- NA_real_
                                 }
                               }

                               # Use last non-NA beta for prediction
                               beta_last <- beta_t[n_t]
                               if (is.na(beta_last)) {
                                 non_na <- which(!is.na(beta_t))
                                 beta_last <- if (length(non_na) > 0) beta_t[non_na[length(non_na)]] else 0
                               }

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
                               private$.statistics$degree_of_freedom <- max(n_t - 4, 1)
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
                               # Use n_valid (finite obs) not nrow — MODELS-04 FEC fix
                               event_window_tbl <- data_tbl %>%
                                 dplyr::filter(event_window == 1)
                               n_valid_fec <- sum(!is.na(estimation_tbl$firm_returns) &
                                                    !is.na(estimation_tbl$index_returns))
                               private$calculate_forecast_error_correction(
                                 mean(sigma_t, na.rm = TRUE),
                                 n_valid_fec,
                                 estimation_tbl$index_returns,
                                 event_window_tbl$index_returns
                               )
                             }
                           )
)
