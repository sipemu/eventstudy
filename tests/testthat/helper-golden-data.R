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

#' Deterministic single-event fixture for the BHAR (buy-and-hold) model.
#'
#' Estimation window m = 6, event window L = 3, all returns fixed. BHAR compounds
#' returns within each window: AR = cumprod(1 + firm) - cumprod(1 + index)
#' (Barber-Lyon 1997). The model's residual sigma is sd(firm - index) over the
#' estimation window with df = m - 1 = 5, and BHARTTest scales the compounded
#' BHAR by sigma * sqrt(n) (Lyon-Barber-Tsai 1999). Constants are pinned in
#' test_golden_values.R from the closed-form compounding.
#'
#' @return A tibble for BHARModel$fit() / abnormal_returns() / BHARTTest$compute().
golden_bhar_fixture <- function() {
  # Estimation window: m = 6.
  est_firm  <- c(0.020, 0.010, 0.030, 0.005, 0.015, 0.025)
  est_index <- c(0.010, 0.012, 0.020, 0.008, 0.010, 0.015)
  # Event window: L = 3.
  evt_firm  <- c(0.040, -0.010, 0.030)
  evt_index <- c(0.015, -0.005, 0.010)

  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = c(est_index, evt_index),
    estimation_window = c(rep(1, length(est_firm)), rep(0, length(evt_firm))),
    event_window      = c(rep(0, length(est_firm)), rep(1, length(evt_firm))),
    relative_index    = c(seq(-length(est_firm), -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, length(est_firm)), 1, rep(0, length(evt_firm) - 1))
  )
}

#' Deterministic single-event fixture for the Volume model.
#'
#' Abnormal volume convention: AR = log(firm_volume + 1) - mean(log(est volume + 1))
#' (log_transform = TRUE default). sigma = sd(log-volume residuals) over the
#' estimation window with df = m - 1 = 5. firm_returns / index_returns are held
#' constant (unused by the volume convention) so the pipeline shape is valid.
#' Constants are pinned in test_golden_values.R from the closed-form log-mean
#' subtraction.
#'
#' @return A tibble for VolumeModel$fit() / abnormal_returns().
golden_volume_fixture <- function() {
  # Estimation window: m = 6 volume levels.
  est_vol <- c(1000, 1200, 900, 1100, 1050, 950)
  # Event window: L = 3 volume levels (a spike, a drop, a rise).
  evt_vol <- c(2000, 800, 1500)

  tibble::tibble(
    firm_volume       = c(est_vol, evt_vol),
    firm_returns      = rep(0.01, length(est_vol) + length(evt_vol)),
    index_returns     = rep(0.01, length(est_vol) + length(evt_vol)),
    estimation_window = c(rep(1, length(est_vol)), rep(0, length(evt_vol))),
    event_window      = c(rep(0, length(est_vol)), rep(1, length(evt_vol))),
    relative_index    = c(seq(-length(est_vol), -1), seq(0, length(evt_vol) - 1)),
    event_date        = c(rep(0, length(est_vol)), 1, rep(0, length(evt_vol) - 1))
  )
}

#' Deterministic single-event fixture for the Volatility model.
#'
#' Abnormal volatility convention: AR = firm_returns^2 / est_var - 1, where
#' est_var = var(estimation firm_returns). sigma = sd(ratio residuals) over the
#' estimation window with df = m - 1 = 5. index_returns held constant (unused).
#' Constants are pinned in test_golden_values.R from the closed-form ratio.
#'
#' @return A tibble for VolatilityModel$fit() / abnormal_returns().
golden_volatility_fixture <- function() {
  # Estimation window: m = 6 returns with non-zero variance.
  est_firm <- c(0.02, -0.01, 0.03, -0.02, 0.015, -0.005)
  # Event window: L = 3 (a big move, a big drop, a small move).
  evt_firm <- c(0.05, -0.04, 0.01)

  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = rep(0.01, length(est_firm) + length(evt_firm)),
    estimation_window = c(rep(1, length(est_firm)), rep(0, length(evt_firm))),
    event_window      = c(rep(0, length(est_firm)), rep(1, length(evt_firm))),
    relative_index    = c(seq(-length(est_firm), -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, length(est_firm)), 1, rep(0, length(evt_firm) - 1))
  )
}

#' Deterministic single-event fixture for the Rolling-Window model.
#'
#' RollingWindowModel$new() defaults are window_size = 60, min_obs = 30, so the
#' estimation window MUST have at least 30 observations. With m = 30 and
#' window_size = 60, the effective window is ws = min(60, 30) = 30 -- a SINGLE
#' rolling window equal to the full estimation sample, so the "time-varying" fit
#' reduces to one closed-form OLS whose alpha/beta/sigma are exactly reproducible.
#' The event-window AR = firm - (alpha_last + beta_last * index). df = ws - 2 = 28.
#' Constants are pinned in test_golden_values.R from the closed-form single-window
#' OLS.
#'
#' @return A tibble for RollingWindowModel$fit() / abnormal_returns().
golden_rolling_window_fixture <- function() {
  # Estimation window: m = 30 (meets the default min_obs = 30). Fixed index grid
  # and a repeating fixed residual pattern keep alpha/beta/sigma exact.
  n_est <- 30L
  est_index <- seq(-0.03, 0.03, length.out = n_est)
  resid_pat <- rep(c(0.001, -0.001, 0.0015, -0.0015, 0.0005, -0.0005),
                   length.out = n_est)
  est_firm  <- 0.004 + 1.2 * est_index + resid_pat
  # Event window: L = 3.
  evt_index <- c(0.02, -0.01, 0.015)
  evt_firm  <- c(0.050, 0.000, 0.040)

  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = c(est_index, evt_index),
    estimation_window = c(rep(1, n_est), rep(0, length(evt_firm))),
    event_window      = c(rep(0, n_est), rep(1, length(evt_firm))),
    relative_index    = c(seq(-n_est, -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, n_est), 1, rep(0, length(evt_firm) - 1))
  )
}

#' Deterministic MULTI-EVENT fixture for the cross-sectional statistics (Task 4).
#'
#' Three events (E1, E2, E3), each fit by its own Market Model over an m = 8
#' estimation window, with an L = 3 event window. Every index/firm return is a
#' fixed literal (no set.seed), so each per-event OLS fit -- and therefore the
#' cross-sectional AAR/CAAR t (CSectTTest), the standardized-residual Patell Z
#' (PatellZTest), and the standardized cross-sectional BMP t (BMPTest) -- reduces
#' to an exact closed-form constant. Each event's firm return is built as
#' alpha + beta * index + a fixed residual pattern, so alpha/beta are clean and
#' the residual sigma is small but positive (df = m - 2 = 6, k = 2).
#'
#' The builder returns BOTH the abnormal-return data_tbl (produced by the real
#' MarketModel$abnormal_returns() path, keyed by event_id) AND the per-event
#' model tibble (event_id, firm_symbol, model) that PatellZTest / BMPTest consume.
#' Feeding these to the production compute() methods locks the real formulas, not
#' a re-implementation. Constants are pinned in test_golden_values.R from the
#' independent closed-form derivation in data-raw/derive-golden-values.R.
#'
#' @return A list with `data` (abnormal-return tibble across all three events)
#'   and `model` (per-event model tibble) for CSectTTest / PatellZTest / BMPTest.
golden_multi_event_fixture <- function() {
  # Per-event specification: fixed estimation index + residual pattern, clean
  # alpha/beta, and a fixed event window. m = 8, L = 3 for every event.
  spec <- list(
    E1 = list(
      est_index = c(-0.02, -0.01, 0.00, 0.01, 0.02, 0.03, -0.015, 0.005),
      est_resid = c(0.001, -0.001, 0.002, -0.002, 0.0015, -0.0015, 0.0005, -0.0005),
      alpha = 0.004, beta = 1.2,
      evt_index = c(0.015, -0.005, 0.010), evt_firm = c(0.040, 0.000, 0.030)
    ),
    E2 = list(
      est_index = c(-0.018, -0.008, 0.002, 0.012, 0.022, 0.028, -0.012, 0.008),
      est_resid = c(0.0012, -0.0008, 0.0018, -0.0016, 0.0010, -0.0012, 0.0006, -0.0010),
      alpha = 0.003, beta = 1.0,
      evt_index = c(0.012, -0.004, 0.009), evt_firm = c(0.030, -0.010, 0.020)
    ),
    E3 = list(
      est_index = c(-0.025, -0.012, 0.001, 0.010, 0.020, 0.030, -0.010, 0.006),
      est_resid = c(0.0008, -0.0012, 0.0016, -0.0014, 0.0012, -0.0010, 0.0004, -0.0004),
      alpha = 0.005, beta = 1.4,
      evt_index = c(0.018, -0.006, 0.011), evt_firm = c(0.050, -0.005, 0.035)
    )
  )
  n_est <- 8L
  n_ev  <- 3L

  base <- do.call(rbind, lapply(names(spec), function(nm) {
    s <- spec[[nm]]
    est_firm <- s$alpha + s$beta * s$est_index + s$est_resid
    tibble::tibble(
      event_id          = nm,
      firm_symbol       = sub("E", "F", nm),
      relative_index    = c(seq(-n_est, -1), seq(0, n_ev - 1)),
      index_returns     = c(s$est_index, s$evt_index),
      firm_returns      = c(est_firm, s$evt_firm),
      estimation_window = c(rep(1, n_est), rep(0, n_ev)),
      event_window      = c(rep(0, n_est), rep(1, n_ev)),
      event_date        = c(rep(0, n_est), 1, rep(0, n_ev - 1))
    )
  }))

  # Per-event model tibble (one MarketModel fit per event), keyed by event_id --
  # the exact shape PatellZTest / BMPTest expect.
  model_tbl <- tibble::tibble(
    event_id    = names(spec),
    firm_symbol = sub("E", "F", names(spec)),
    model = lapply(names(spec), function(nm) {
      mm <- MarketModel$new()
      # C10 (2026-09-24): m = 8 estimation observations is INTENTIONALLY
      # short (kept small for closed-form tractability); muffle only the
      # advisory eventstudy_short_estimation_window warning it triggers.
      muffle_short_window(mm$fit(base[base$event_id == nm, ]))
      mm
    })
  )

  # Attach abnormal_returns via the real per-event path (pipeline behavior).
  data_ar <- do.call(rbind, lapply(names(spec), function(nm) {
    mm <- model_tbl$model[[which(model_tbl$event_id == nm)]]
    mm$abnormal_returns(base[base$event_id == nm, ])
  }))

  list(data = data_ar, model = model_tbl)
}
