# Helper fixtures for golden-value regression tests (Phase 26).
#
# These builders produce SMALL, fully deterministic data_tbl objects whose
# expected statistics can be derived in closed form by hand / by an
# independent R computation (see data-raw/derive-golden-values.R). They feed
# the SAME model$fit() and TestStatistic$compute() code paths the package uses
# in production, so a golden pin locks the real formula, not a re-implementation.
#
# Convention (mirrors helper-mock-data.R and the model fit() contract):
#   data_tbl columns: firm_returns, index_returns, estimation_window,
#   event_window, relative_index, event_date.

#' Deterministic single-event fixture for the Market Model tracer.
#'
#' Fixed numeric vectors (no set.seed) so the OLS fit, residual sigma, and the
#' AR/CAR t-statistics are exactly reproducible closed-form constants. The
#' estimation window has m = 6 observations (df = m - 2 = 4); the event window
#' has L = 3 days.
#'
#' The index/firm returns are chosen so that alpha and beta are simple: with
#' firm = 0.5 + 2 * index + residual over the estimation window, OLS recovers
#' alpha ~ 0.5, beta ~ 2 up to the injected residual pattern. The exact
#' constants are pinned in test_golden_values.R from an independent lm() fit.
#'
#' @return A tibble suitable for MarketModel$fit() / ARTTest$compute() /
#'   CARTTest$compute().
golden_market_model_fixture <- function() {
  # Estimation window: 6 fixed observations.
  est_index <- c(-0.02, -0.01, 0.00, 0.01, 0.02, 0.03)
  # firm = 0.005 + 1.5 * index + a small fixed residual pattern that sums to
  # zero and is orthogonal-ish to index, keeping alpha/beta clean but sigma > 0.
  est_resid <- c(0.001, -0.001, 0.002, -0.002, 0.001, -0.001)
  est_firm  <- 0.005 + 1.5 * est_index + est_resid

  # Event window: 3 fixed days.
  evt_index <- c(0.015, -0.005, 0.010)
  evt_firm  <- c(0.040, 0.000, 0.030)

  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = c(est_index, evt_index),
    estimation_window = c(rep(1, length(est_firm)), rep(0, length(evt_firm))),
    event_window      = c(rep(0, length(est_firm)), rep(1, length(evt_firm))),
    relative_index    = c(seq(-length(est_firm), -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, length(est_firm)), 1, rep(0, length(evt_firm) - 1))
  )
}

#' Deterministic single-event fixture for the Market Adjusted Model.
#'
#' AR = firm_returns - index_returns (the index IS the benchmark; no OLS
#' parameters estimated). The estimation-window index is held constant so the
#' residual series firm - index equals firm - const and varies, giving a
#' well-defined sigma = sd(firm - index) with df = m - 1 = 5. Constants are
#' pinned in test_golden_values.R from the closed-form subtraction.
#'
#' @return A tibble suitable for MarketAdjustedModel$fit() / abnormal_returns().
golden_market_adjusted_fixture <- function() {
  # Estimation window: m = 6. Index constant so firm - index has real variance.
  est_firm  <- c(0.012, 0.008, 0.015, 0.006, 0.011, 0.009)
  est_index <- c(0.010, 0.010, 0.010, 0.010, 0.010, 0.010)
  # Event window: L = 3.
  evt_firm  <- c(0.030, -0.010, 0.020)
  evt_index <- c(0.012, -0.004, 0.008)

  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = c(est_index, evt_index),
    estimation_window = c(rep(1, length(est_firm)), rep(0, length(evt_firm))),
    event_window      = c(rep(0, length(est_firm)), rep(1, length(evt_firm))),
    relative_index    = c(seq(-length(est_firm), -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, length(est_firm)), 1, rep(0, length(evt_firm) - 1))
  )
}

#' Deterministic single-event fixture for the Comparison Period Mean model.
#'
#' AR = firm_returns - mean(estimation firm_returns). The estimation-window
#' returns are chosen so the mean is exactly 0.01; residuals firm - mean vary,
#' giving sigma = sd(residuals) with df = m - 1 = 5. Constants are pinned in
#' test_golden_values.R from the closed-form subtraction against the mean.
#'
#' @return A tibble for ComparisonPeriodMeanAdjustedModel$fit() /
#'   abnormal_returns().
golden_comparison_mean_fixture <- function() {
  # Estimation window: m = 6, mean = 0.01 exactly.
  est_firm  <- c(0.010, 0.014, 0.008, 0.012, 0.006, 0.010)
  # Event window: L = 3.
  evt_firm  <- c(0.030, -0.005, 0.018)
  # Index column present for pipeline shape; unused by this model.
  est_index <- c(0.008, 0.012, 0.006, 0.010, 0.004, 0.008)
  evt_index <- c(0.011, -0.002, 0.009)

  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = c(est_index, evt_index),
    estimation_window = c(rep(1, length(est_firm)), rep(0, length(evt_firm))),
    event_window      = c(rep(0, length(est_firm)), rep(1, length(evt_firm))),
    relative_index    = c(seq(-length(est_firm), -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, length(est_firm)), 1, rep(0, length(evt_firm) - 1))
  )
}

#' Deterministic single-event fixture for the multi-factor (OLS) models.
#'
#' A fully fixed factor design over an m = 8 estimation window plus an L = 3
#' event window. The firm excess return is built as a known linear combination
#' of the factors plus a fixed residual pattern, so the OLS fit, residual sigma,
#' and event-window abnormal returns (excess_return - predicted) are exact
#' closed-form constants. The SAME tibble carries every factor column, so it
#' feeds FamaFrench3/5 and Carhart4 unchanged -- each model selects the columns
#' its formula names. Degrees of freedom differ by factor count: FF3 df = 4
#' (m - 4), Carhart4 df = 3 (m - 5), FF5 df = 2 (m - 6). Constants are pinned in
#' test_golden_values.R from an independent lm()/predict() derivation.
#'
#' @return A tibble for FamaFrench3FactorModel / FamaFrench5FactorModel /
#'   Carhart4FactorModel fit() + abnormal_returns().
golden_factor_model_fixture <- function() {
  # Estimation window: m = 8, all factor columns fixed.
  me  <- c(0.010, -0.005, 0.008, 0.002, -0.010, 0.006, 0.004, -0.003)
  smb <- c(0.002, 0.001, -0.003, 0.004, 0.000, 0.002, -0.001, 0.003)
  hml <- c(-0.001, 0.002, 0.001, -0.002, 0.003, 0.000, 0.002, -0.001)
  mom <- c(0.003, -0.002, 0.001, 0.002, -0.001, 0.004, 0.000, 0.001)
  rmw <- c(0.001, 0.000, 0.002, -0.001, 0.002, -0.001, 0.003, 0.000)
  cma <- c(-0.002, 0.001, 0.000, 0.002, -0.001, 0.001, -0.002, 0.003)
  # Fixed residual pattern (kept small, both signs) so sigma > 0 but tiny.
  resid <- c(0.0010, -0.0008, 0.0012, -0.0009, 0.0007, -0.0011, 0.0006, -0.0007)

  # A single firm-excess series drives all three models. It intentionally
  # contains ALL factor loadings (mom, rmw, cma). For a model that omits a
  # factor, that factor's contribution folds into the residual -- the golden
  # pin still locks the exact fitted number for THAT model's formula.
  alpha0 <- 0.0005
  excess_est <- alpha0 + 1.1 * me + 0.5 * smb - 0.3 * hml +
    0.2 * mom + 0.15 * rmw - 0.1 * cma + resid

  # Event window: L = 3, fixed factors and fixed observed firm excess.
  me_e  <- c(0.012, -0.004, 0.006)
  smb_e <- c(0.001, -0.002, 0.003)
  hml_e <- c(0.002, 0.001, -0.001)
  mom_e <- c(0.001, 0.002, 0.000)
  rmw_e <- c(0.002, -0.001, 0.001)
  cma_e <- c(-0.001, 0.000, 0.002)
  excess_evt <- c(0.020, -0.008, 0.014)

  tibble::tibble(
    excess_return     = c(excess_est, excess_evt),
    market_excess     = c(me, me_e),
    smb               = c(smb, smb_e),
    hml               = c(hml, hml_e),
    mom               = c(mom, mom_e),
    rmw               = c(rmw, rmw_e),
    cma               = c(cma, cma_e),
    # firm_returns / index_returns present for pipeline shape (risk-free = 0).
    firm_returns      = c(excess_est, excess_evt),
    index_returns     = c(me, me_e),
    estimation_window = c(rep(1, 8), rep(0, 3)),
    event_window      = c(rep(0, 8), rep(1, 3)),
    relative_index    = c(seq(-8, -1), 0:2),
    event_date        = c(rep(0, 8), 1, 0, 0)
  )
}
