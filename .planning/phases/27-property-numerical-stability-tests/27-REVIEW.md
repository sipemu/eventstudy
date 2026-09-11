---
phase: 27-property-numerical-stability-tests
reviewed: 2026-09-11T00:00:00Z
depth: deep
files_reviewed: 7
files_reviewed_list:
  - R/models.R
  - R/single_event_test_statistics.R
  - R/multi_event_test_statistics.R
  - tests/testthat/helper-invariant-data.R
  - tests/testthat/test_invariants.R
  - tests/testthat/test_numerical_stability.R
  - DESCRIPTION
findings:
  critical: 0
  warning: 2
  info: 3
  total: 5
status: issues_found
---

# Phase 27: Code Review Report

**Reviewed:** 2026-09-11
**Depth:** deep
**Files Reviewed:** 7
**Status:** issues_found

## Summary

Phase 27 adds property/invariant tests and three additive numerical-stability
guards (ill-conditioned OLS design, long-window CAR overflow, STATS-04
uniformity for GeneralizedSign and CalendarTimePortfolio). The overall
implementation is sound: guards route correctly through the existing contract,
the overflow guard's exclusion of plain NA is correct and well-motivated, and
no new hard Imports are introduced. The two warnings below are real correctness
gaps that survive in the current diff; the info items are cosmetic or coverage
gaps with no correctness consequence.

---

## Warnings

### WR-01: CARTest overflow guard does not NA out `car_t_dist`

**File:** `R/single_event_test_statistics.R:134,160-161`

**Issue:** The overflow guard (lines 160-161) correctly NAs `corrected_car`
and `car_t` for rows where `res$car` is Inf/NaN. However, `car_t_dist` — the
`distributional::dist_student_t(mu = car, ...)` column computed on line 134
— is built directly from `res$car` before the guard fires. When `car` is Inf,
`car_t_dist` will be a Student-t distribution centred on Inf, which is not
meaningfully NA'd. Any downstream consumer that reads `car_t_dist` on an
overflowed row therefore still receives a misleading non-finite distribution
object, contrary to the guard's stated intent.

The `car_t` scalar NA is correct and protects the primary output path used by
`calculate_statistics()`. `car_t_dist` is only read in the test
`test_ar_car_test_statistics.R:74` for scale verification and is not currently
consumed by any export or plotting path, so this does not cause a silently
wrong result at present. However it is an inconsistency the overflow comment
promises to fix and does not: "car_t set to NA" implies the whole test result
for that row is NA'd, not just one column.

**Fix:** After the guard fires, also clear `car_t_dist` for overflowed rows:

```r
if (any(car_overflow)) {
  warning(...)
  res$corrected_car[car_overflow] <- NA_real_
  res$car_t[car_overflow]         <- NA_real_
  # car_t_dist is also derived from car; NA it for consistency so no
  # overflowed distribution object leaks to any downstream consumer.
  res$car_t_dist[car_overflow]    <- distributional::dist_student_t(
    df = degree_of_freedom, mu = NA_real_, sigma = 1
  )
}
```

Alternatively, build `car_t_dist` after the overflow guard so it never sees
Inf/NaN `car` values in the first place.

---

### WR-02: `invariant_alpha0_beta1_fixture` relies on `suppressWarnings` hiding the ill-conditioned-design guard

**File:** `tests/testthat/test_invariants.R:811-815` / `R/models.R:234`

**Issue:** The cross-method-consistency test uses an alpha=0/beta=1 fixture
with `est_firm == est_index` (estimation-window residuals are identically
zero). When `MarketModel$fit()` runs on this design, `rcond(X'X)` for the
model matrix `[1 | index_returns]` with a non-constant index (mean-zero,
sd=0.017) is perfectly fine — the design is NOT ill-conditioned in the
near-constant sense. However, the zero estimation-window residuals cause `lm()`
itself to emit a "essentially perfect fit" message (`summary.lm` warns about
aliased coefficients if sigma==0), which the test correctly suppresses with
`suppressWarnings(model$fit(d))`.

The subtler issue is that `sigma` from a zero-residual fit is exactly 0. The
`CARTTest$new()$compute()` path then hits `sigma_degenerate <- TRUE` (line 122:
`sigma < .Machine$double.eps`), so `car_t` is **all-NA** for MarketModel on
this fixture. This means the cross-method consistency test at lines 816-829
only compares `ar_mm` vs `ar_maj` and `ar_cpm` at the `abnormal_returns` level
— which is valid — but **does not** exercise the statistic layer as the test
comment implies it does. The scale-invariance test (line 865) uses a
different fixture so it is unaffected.

The test is not wrong about what it asserts (abnormal returns agree) but the
comment "cross-method invariant" overstates the scope: the statistic layer
falls back to NA on this fixture and is not compared. A reader may conclude
statistic-layer agreement is tested here when it is not.

**Fix (preferred):** Add a small non-zero noise term to `est_firm` (e.g.
`est_firm <- est_index + c(1,-1,1,-1,1,-1,1,-1) * 1e-5`) so sigma is finite
and `car_t` is comparable across all three models. Adjust the comment to
reflect that both AR-layer and statistic-layer cross-method agreement are now
covered. The alpha~0/beta~1 approximation remains valid at this perturbation
scale. Alternatively, add an explicit statistic-layer assertion using the
separate `golden_market_model_fixture()` (already used by the scale-invariance
test) and keep the current alpha=0/beta=1 fixture solely for AR-layer
comparison with an updated comment.

---

## Info

### IN-01: `.design_rcond` called on the full `data_tbl` (estimation + event rows), not just `estimation_tbl`

**File:** `R/models.R:226`

**Issue:** The call `rc <- .design_rcond(self$formula, estimation_tbl)` is
correct — `estimation_tbl` is the already-filtered estimation-window slice
passed in, so only estimation-window rows are used. The function itself applies
`na.action = na.omit`, which is consistent with how `lm()` sees the data.
No bug here. However, the function signature comment says "Builds the same
model matrix `lm()` would use" — and `lm()` is called on `estimation_tbl`
a few lines later (via `.estimate_mm_model`). This is accurate. Purely a
clarity note: the comment in `.design_rcond`'s roxygen block says the function
takes `data` (not `estimation_tbl`) which is generic and could mislead a
future caller into passing the full data frame. A one-line usage note would
help.

**Fix:** Add `#' @param data The estimation-window tibble (filtered rows only).` to the `.design_rcond` roxygen block to prevent future misuse.

---

### IN-02: `invariant_fit_event_ar` builds `car` by `cumsum(ar)` in the helper, then the universal loop asserts `ev$car == invariant_expected_car(ev)` which also calls `cumsum(ev$ar)` — the identity is tautological for additive models

**File:** `tests/testthat/helper-invariant-data.R:416-422` / `tests/testthat/test_invariants.R:652-658`

**Issue:** For additive-identity models, `invariant_fit_event_ar` assigns
`car = cumsum(ar)` (line 421) and `invariant_expected_car` returns `cumsum(ev$ar)`
(line 434). The universal-identity `expect_equal(ev$car, invariant_expected_car(ev))` is
therefore always trivially true by construction — it compares `cumsum(ar)` to
`cumsum(ar)`. The test does NOT verify that the model/statistic layer's own
reported CAR matches the AR-based cumsum; it only verifies that a re-computed
cumsum equals itself.

To genuinely lock the invariant, `ev$car` should come from the model/statistic
layer's output (e.g., from `CARTTest$compute()$car`), not be constructed in
the helper as `cumsum(ar)`.

This is a test-coverage gap, not a production bug. The BHAR row is correctly
handled (car = path from the model output, ar = diff(path), and
`cumsum(increments) == path` is genuinely non-trivial). Only the "additive"
rows suffer from the tautology.

**Fix:** In `invariant_fit_event_ar`, for additive rows, return `car` from the
model/statistic layer's own cumulation (e.g., run `CARTTest$new()$compute()`
on the fitted abnormal returns and extract its `car` column). Then the
`expect_equal(ev$car, cumsum(ev$ar))` assertion genuinely cross-checks two
independent computations.

---

### IN-03: `withr` already in DESCRIPTION Suggests — no new dependency introduced (confirmation note)

**File:** `DESCRIPTION:47`

**Issue:** `withr` was already present as a Suggests dependency before this
phase. No action required. Confirmed: `grep -c "patrick|hedgehog" DESCRIPTION`
== 0. CRAN dependency constraint is satisfied.

---

## Structural Findings (fallow)

No structural pre-pass was provided for this phase.

---

_Reviewed: 2026-09-11_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: deep_
