# test_invariants.R
#
# Cross-cutting statistical invariants that must hold for ANY correct return
# model, asserted by looping over invariant_model_registry (Phase 27, CORR-03).
#
# Registry-driven so a model is covered by ADDING A ROW to the registry in
# helper-invariant-data.R -- Task 2 appends the remaining models; a missing row
# silently drops coverage, which the Phase 27 coverage gate (Task 6) checks.
#
# Tolerance policy (planning_constraints 4): CAR==cumsum(AR) is an EXACT
# algebraic identity, so it is asserted at absolute tolerance 1e-10.

test_that("CAR == cumsum(AR) holds for every registered return model", {
  for (reg_row in invariant_model_registry) {
    ev <- invariant_fit_event_ar(reg_row)

    # Universal identity: the cumulative abnormal return at each relative index
    # equals the running sum of per-period abnormal returns. This must hold
    # exactly for any additive-cumulation model -- an algebraic identity, so we
    # use a tight ABSOLUTE tolerance of 1e-10 (not the testthat default).
    expect_equal(
      ev$car,
      cumsum(ev$ar),
      tolerance = 1e-10,           # exact algebraic identity -> tight absolute
      info = paste0("CAR==cumsum(AR) for ", reg_row$name)
    )
  }
})
