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
invariant_model_registry <- list(
  list(
    name        = "MarketModel",
    constructor = function() MarketModel$new(),
    fixture     = invariant_market_model_fixture
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
  tibble::tibble(
    relative_index = ev$relative_index,
    ar             = ev$abnormal_returns,
    car            = cumsum(ev$abnormal_returns)
  )
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
