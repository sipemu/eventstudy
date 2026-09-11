# Helper fixtures + model registry for the cross-cutting invariant and
# numerical-stability tests (Phase 27).
#
# Design goals (mirroring helper-golden-data.R and test_contract_matrix.R):
#   * Deterministic fixtures only -- fixed numeric literals, no set.seed, no
#     generative randomness (SC3: no CI flakiness).
#   * Fixtures feed the REAL prepare/fit/abnormal_returns pipeline, so an
#     invariant locks the production formula, not a re-implementation.
#   * A table-driven `invariant_model_registry`: every model is a DATA ROW
#     (name + zero-arg constructor thunk + fixture builder), so later tasks
#     append models as rows, never as new code paths.

# --------------------------------------------------------------------------
# Invariant fixture builders
# --------------------------------------------------------------------------

#' Deterministic single-event fixture for the Market Model invariant loop.
#'
#' Reuses the golden Market Model fixture (a fully fixed OLS design over an
#' m = 6 estimation window, L = 3 event window) so the invariant loop exercises
#' the same well-conditioned path the golden pins lock -- the CAR==cumsum(AR)
#' identity must hold there exactly.
#'
#' @return A tibble for MarketModel$fit() / abnormal_returns().
invariant_market_model_fixture <- function() {
  # Delegate to the golden builder (loaded from helper-golden-data.R) so the
  # invariant and golden layers share one canonical Market Model design.
  golden_market_model_fixture()
}

#' Market-Adjusted / Comparison-Mean invariant fixtures (reuse golden builders).
invariant_market_adjusted_fixture <- function() golden_market_adjusted_fixture()
invariant_comparison_mean_fixture <- function() golden_comparison_mean_fixture()

#' Custom-model invariant fixture.
#'
#' Reuses the golden Market Model design but adds loss_market_cap = 0 so the
#' CustomModel event-date adjustment is neutral and its AR reduces exactly to
#' the Market Model AR (see test_golden_values.R Custom Model). With a neutral
#' adjustment the per-period AR are ordinary additive abnormal returns, so the
#' universal CAR==cumsum(AR) identity holds.
invariant_custom_model_fixture <- function() {
  d <- golden_market_model_fixture()
  d$loss_market_cap <- 0
  d
}

#' Factor-model invariant fixture (LinearFactorModel / FF3 / FF5 / Carhart4).
#'
#' Reuses the golden multi-factor design (excess_return + all factor columns);
#' each concrete model selects the columns its own formula names. AR =
#' excess_return - predicted, ordinary additive abnormal returns, so
#' CAR==cumsum(AR) holds.
invariant_factor_model_fixture <- function() golden_factor_model_fixture()

#' BHAR invariant fixture (reuse golden buy-and-hold design).
invariant_bhar_fixture <- function() golden_bhar_fixture()

#' Volume / Volatility / Rolling-Window invariant fixtures (reuse golden).
invariant_volume_fixture <- function() golden_volume_fixture()
invariant_volatility_fixture <- function() golden_volatility_fixture()
invariant_rolling_window_fixture <- function() golden_rolling_window_fixture()

#' Constructor thunk for LinearFactorModel with a concrete FF3 formula.
#'
#' The abstract LinearFactorModel ships with formula = NULL and cannot fit
#' standalone; we set a concrete three-factor formula (the same one FF3 uses) so
#' the base class participates in the universal identity loop on the shared
#' factor fixture. This exercises the base OLS fit/predict path directly.
invariant_make_linear_factor_model <- function() {
  m <- LinearFactorModel$new()
  m$formula <- stats::as.formula(
    "excess_return ~ market_excess + smb + hml"
  )
  m$required_columns <- c("excess_return", "market_excess", "smb", "hml")
  m
}

# --------------------------------------------------------------------------
# Model registry (table-driven; Task 2 appends the remaining models as rows)
# --------------------------------------------------------------------------

#' Registry of return models exercised by the universal invariant loop.
#'
#' Each element is a list with:
#'   name        : human-readable model name (for test labels)
#'   constructor : a zero-arg thunk returning a fresh model instance
#'   fixture     : a zero-arg thunk returning the model's invariant data_tbl
#'
#' Structured as a list of data rows so Task 2 appends models by adding rows,
#' NOT by editing the loop in test_invariants.R (mirrors the table-driven
#' registry idiom in test_contract_matrix.R).
#' Each row additionally carries:
#'   identity : "additive" (default) -> CAR == cumsum(AR); or "bhar" -> the
#'              reported abnormal_returns ARE the compounded BHAR path itself
#'              (cumprod(1+firm) - cumprod(1+index)), so the model/statistic
#'              CAR equals that path directly, NOT a naive additive cumsum.
#'   skip_pkg : optional character vector of packages that must be installed
#'              (skip_if_not_installed) before the row runs (GARCH/DCC).
invariant_model_registry <- list(
  list(
    name        = "MarketModel",
    constructor = function() MarketModel$new(),
    fixture     = invariant_market_model_fixture,
    identity    = "additive"
  ),
  list(
    name        = "MarketAdjustedModel",
    constructor = function() MarketAdjustedModel$new(),
    fixture     = invariant_market_adjusted_fixture,
    identity    = "additive"
  ),
  list(
    name        = "ComparisonPeriodMeanAdjustedModel",
    constructor = function() ComparisonPeriodMeanAdjustedModel$new(),
    fixture     = invariant_comparison_mean_fixture,
    identity    = "additive"
  ),
  list(
    name        = "CustomModel",
    constructor = function() CustomModel$new(),
    fixture     = invariant_custom_model_fixture,
    identity    = "additive"
  ),
  list(
    name        = "LinearFactorModel",
    constructor = invariant_make_linear_factor_model,
    fixture     = invariant_factor_model_fixture,
    identity    = "additive"
  ),
  list(
    name        = "FamaFrench3FactorModel",
    constructor = function() FamaFrench3FactorModel$new(),
    fixture     = invariant_factor_model_fixture,
    identity    = "additive"
  ),
  list(
    name        = "FamaFrench5FactorModel",
    constructor = function() FamaFrench5FactorModel$new(),
    fixture     = invariant_factor_model_fixture,
    identity    = "additive"
  ),
  list(
    name        = "Carhart4FactorModel",
    constructor = function() Carhart4FactorModel$new(),
    fixture     = invariant_factor_model_fixture,
    identity    = "additive"
  ),
  list(
    name        = "BHARModel",
    constructor = function() BHARModel$new(),
    fixture     = invariant_bhar_fixture,
    identity    = "bhar"
  ),
  list(
    name        = "VolumeModel",
    constructor = function() VolumeModel$new(),
    fixture     = invariant_volume_fixture,
    identity    = "additive"
  ),
  list(
    name        = "VolatilityModel",
    constructor = function() VolatilityModel$new(),
    fixture     = invariant_volatility_fixture,
    identity    = "additive"
  ),
  list(
    name        = "RollingWindowModel",
    constructor = function() RollingWindowModel$new(),
    fixture     = invariant_rolling_window_fixture,
    identity    = "additive"
  ),
  list(
    name        = "GARCHModel",
    constructor = function() GARCHModel$new(),
    fixture     = invariant_market_model_fixture,
    identity    = "additive",
    skip_pkg    = "rugarch"
  ),
  list(
    name        = "DCCGARCHModel",
    constructor = function() DCCGARCHModel$new(),
    fixture     = invariant_market_model_fixture,
    identity    = "additive",
    skip_pkg    = c("rugarch", "rmgarch")
  )
)

#' Fit a registered model on its fixture and return the event-window AR/CAR.
#'
#' Runs the real fit() + abnormal_returns() path, restricts to the event
#' window, and returns a tibble ordered by relative_index with an `ar` column
#' (per-period abnormal returns) and a `car` column (running cumulative sum as
#' the model/statistic layer would produce it). Used by the universal
#' CAR==cumsum(AR) invariant.
#'
#' @param reg_row One element of invariant_model_registry.
#' @return A tibble with columns relative_index, ar, car (event window only).
invariant_fit_event_ar <- function(reg_row) {
  model <- reg_row$constructor()
  data  <- reg_row$fixture()
  model$fit(data)
  ar_tbl <- model$abnormal_returns(data)
  ev <- ar_tbl[ar_tbl$event_window == 1, , drop = FALSE]
  ev <- ev[order(ev$relative_index), , drop = FALSE]

  identity <- if (is.null(reg_row$identity)) "additive" else reg_row$identity
  if (identity == "bhar") {
    # BHAR's abnormal_returns column is ALREADY the compounded buy-and-hold
    # path (cumprod(1+firm) - cumprod(1+index)); it is inherently cumulative.
    # The model/statistic-layer CAR at each index IS that reported path, so the
    # BHAR-specific form of the identity is car == abnormal_returns, NOT a naive
    # additive cumsum of increments. `ar` here is the per-step increment of the
    # BHAR path (diff), retained only so tests can reason about increments.
    # unname(): factor fixtures carry named numeric vectors; names would leak
    # into the recurrence comparison (offset labels) without affecting the math.
    path <- unname(ev$abnormal_returns)
    increments <- c(path[1], diff(path))
    tibble::tibble(
      relative_index = ev$relative_index,
      ar             = increments,
      car            = path
    )
  } else {
    ar <- unname(ev$abnormal_returns)
    tibble::tibble(
      relative_index = ev$relative_index,
      ar             = ar,
      car            = cumsum(ar)
    )
  }
}

#' Compute expected CAR under a row's identity form (for the universal loop).
#'
#' Additive models: cumsum of per-period AR. BHAR: the reported path itself
#' (already cumulative), so the "expected" cumulation equals the AR path
#' reconstructed from its own increments -- i.e. cumsum of the increments,
#' which by construction equals the path. This keeps the loop uniform while
#' honouring the BHAR-specific identity documented above.
invariant_expected_car <- function(ev) {
  cumsum(ev$ar)
}

# --------------------------------------------------------------------------
# Ill-conditioned (near-collinear) design fixture for the stability guard
# --------------------------------------------------------------------------

#' Deterministic near-collinear Market Model estimation design.
#'
#' The estimation index_returns have genuine (non-zero) variance -- so the
#' pre-existing sd()<double.eps guard does NOT fire -- but are constructed to be
#' almost perfectly collinear with the intercept: a constant offset plus a tiny
#' fixed perturbation, so rcond(X'X) falls far below the sqrt(double.eps)
#' threshold. This is the "finite-variance yet severely ill-conditioned" gap
#' the Phase 27 guard closes.
#'
#' @param scale Perturbation scale controlling conditioning. The default 1e-9
#'   yields rcond well below sqrt(.Machine$double.eps) (~1.5e-8). A larger scale
#'   (e.g. 1e-2) produces a well-conditioned positive control.
#' @return A tibble for MarketModel$fit().
invariant_ill_conditioned_fixture <- function(scale = 1e-9) {
  n_est <- 8L
  # A fixed deterministic perturbation pattern (both signs, sums to ~0) so the
  # index has non-zero sd but is dominated by a constant near-collinear term.
  pert <- c(1, -1, 2, -2, 1, -1, 2, -2) * scale
  est_index <- 0.01 + pert                     # sd > double.eps, near-constant
  # A clean firm series so any instability comes from the design, not the LHS.
  est_firm  <- 0.002 + 1.5 * est_index + c(1, -1, 1, -1, 1, -1, 1, -1) * 1e-4

  evt_index <- c(0.015, -0.005, 0.010)
  evt_firm  <- c(0.040, 0.000, 0.030)

  tibble::tibble(
    firm_returns      = c(est_firm, evt_firm),
    index_returns     = c(est_index, evt_index),
    estimation_window = c(rep(1, n_est), rep(0, length(evt_firm))),
    event_window      = c(rep(0, n_est), rep(1, length(evt_firm))),
    relative_index    = c(seq(-n_est, -1), seq(0, length(evt_firm) - 1)),
    event_date        = c(rep(0, n_est), 1, rep(0, length(evt_firm) - 1))
  )
}
