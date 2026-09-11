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
