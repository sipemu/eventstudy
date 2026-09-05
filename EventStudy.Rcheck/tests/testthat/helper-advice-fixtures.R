# =============================================================================
# helper-advice-fixtures.R — test fixtures for test_advise.R
# =============================================================================
#
# Provides:
#   .make_test_diag()         — a minimal es_diagnostics object for guard tests
#   CANNED_JSON_THREE_RECS    — canned JSON with one grounded + two ungrounded recs
#
# The fixture is deterministic (no random state) so guard regression tests
# always produce the same result regardless of run order.

#' Build a minimal es_diagnostics object for testing
#'
#' Values are set so that the guard test can probe all three outcomes:
#'   - Rec A (cross_sectional.car_iqr = 0.025): key exists, value within tolerance -> KEPT
#'   - Rec B (cross_sectional.kurtosis = 4.2): key ABSENT from diagnostics -> DROPPED
#'   - Rec C (estimation_window.r2 = 0.99 vs actual mean ~0.45): value mismatch -> DROPPED
#'
#' @noRd
.make_test_diag <- function() {
  structure(
    list(
      meta              = list(
        n_events_total      = 5L,
        n_events_shown      = 5L,
        n_events_summarized = 0L,
        event_ids_shown     = 1:5
      ),
      estimation_window = list(
        r2                = c(0.4, 0.5, 0.3, 0.6, 0.45),
        sigma             = c(0.01, 0.01, 0.01, 0.01, 0.01),
        degree_of_freedom = c(120L, 120L, 120L, 120L, 120L),
        acf1              = c(0.01, 0.02, -0.01, 0.03, 0.01),
        shapiro_p         = c(0.12, 0.08, 0.15, 0.20, 0.10),
        dw_stat           = c(1.9, 2.1, 2.0, 1.8, 2.2),
        ljung_box_p       = c(0.3, 0.4, 0.5, 0.3, 0.4)
      ),
      event_window      = list(
        ar_t      = c(1.2, 0.8, 2.1, -0.3, 1.5),
        ar_p      = c(0.23, 0.42, 0.04, 0.77, 0.13),
        car_t     = c(2.1, 1.5, 3.0, 0.2, 2.5),
        car_p     = c(0.04, 0.13, 0.003, 0.84, 0.01),
        final_car = c(0.03, 0.02, 0.05, 0.001, 0.04)
      ),
      cross_sectional   = list(
        n_events        = 5L,
        n_valid_events  = 5L,
        car_iqr         = 0.025,
        car_sd          = 0.018,
        n_overlap_pairs = 0L,
        any_overlap     = FALSE
      ),
      contract_state    = list(
        is_fitted       = c(TRUE, TRUE, TRUE, TRUE, TRUE),
        na_ar_count     = c(0L, 0L, 0L, 0L, 0L),
        na_est_count    = c(0L, 0L, 0L, 0L, 0L),
        insufficient_obs = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        zero_var_index  = c(FALSE, FALSE, FALSE, FALSE, FALSE)
      ),
      aggregate_summary = NULL
    ),
    class = "es_diagnostics"
  )
}

#' Canned JSON with three recommendations: one valid, two invalid
#'
#' Rec A — VALID: key "cross_sectional.car_iqr" exists in diagnostics, value 0.025
#'         matches exactly (within tolerance) -> KEPT
#' Rec B — INVALID: key "cross_sectional.kurtosis" is ABSENT from diagnostics -> DROPPED
#' Rec C — INVALID: key "estimation_window.r2" exists but reported value 0.99
#'         vs actual mean ~0.45 (way beyond tolerance) -> DROPPED
#'
#' Guard result: n_dropped == 2L, one "Grounding guard" warning, one rec (Rec A) kept.
#'
#' @noRd
CANNED_JSON_THREE_RECS <- if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::toJSON(
    list(
      interpretation  = "The study shows significant abnormal returns.",
      recommendations = list(
        # Rec A: VALID — key exists, value within tolerance
        list(
          action          = "Use BMP test",
          kind            = "stat_choice",
          rationale       = "High CAR dispersion.",
          expected_effect = "Robust p-values.",
          evidence        = list(
            list(
              diagnostic_key = "cross_sectional.car_iqr",
              value          = 0.025,
              threshold      = 0.10,
              direction      = "below"
            )
          )
        ),
        # Rec B: INVALID — key does not exist in diagnostics
        list(
          action          = "Check tail risk",
          kind            = "robustness",
          rationale       = "Heavy tails.",
          expected_effect = "Better size control.",
          evidence        = list(
            list(
              diagnostic_key = "cross_sectional.kurtosis",  # ABSENT key
              value          = 4.2,
              threshold      = 3.0,
              direction      = "above"
            )
          )
        ),
        # Rec C: INVALID — value mismatches beyond tolerance
        list(
          action          = "Switch to FF3",
          kind            = "recommend_model",
          rationale       = "Low R2.",
          expected_effect = "Better fit.",
          evidence        = list(
            list(
              diagnostic_key = "estimation_window.r2",
              value          = 0.99,   # ACTUAL mean is ~0.45, way off
              threshold      = 0.05,
              direction      = "above"
            )
          )
        )
      ),
      caveats = list("This is AI-generated advice.")
    ),
    auto_unbox = TRUE
  )
} else {
  # Fallback for environments without jsonlite (hand-rolled JSON)
  paste0(
    '{"interpretation":"The study shows significant abnormal returns.",',
    '"recommendations":[',
    '{"action":"Use BMP test","kind":"stat_choice","rationale":"High CAR dispersion.",',
    '"expected_effect":"Robust p-values.",',
    '"evidence":[{"diagnostic_key":"cross_sectional.car_iqr","value":0.025,"threshold":0.10,"direction":"below"}]},',
    '{"action":"Check tail risk","kind":"robustness","rationale":"Heavy tails.",',
    '"expected_effect":"Better size control.",',
    '"evidence":[{"diagnostic_key":"cross_sectional.kurtosis","value":4.2,"threshold":3.0,"direction":"above"}]},',
    '{"action":"Switch to FF3","kind":"recommend_model","rationale":"Low R2.",',
    '"expected_effect":"Better fit.",',
    '"evidence":[{"diagnostic_key":"estimation_window.r2","value":0.99,"threshold":0.05,"direction":"above"}]}',
    '],"caveats":["This is AI-generated advice."]}'
  )
}
