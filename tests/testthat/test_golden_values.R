# Golden-value regression tests (Phase 26).
#
# Each block pins a key statistic to a PUBLISHED-FIRST reference constant:
# either a value from a published table or a closed-form algebraic derivation
# on a small deterministic fixture (see helper-golden-data.R). The constants
# are literals with an inline provenance / assumed-conventions / tolerance
# rationale so a wrong number can never be silently enshrined, and a correct
# number can never be falsely failed by a convention mismatch.
#
# The suite NEVER depends on estudy2 / eventstudies being installed: every
# constant here is derived from published sources or closed-form R algebra.
# data-raw/derive-golden-values.R documents how each constant was obtained.
#
# Tolerance policy (matches the suite norm and Phase 26 CONTEXT.md):
#   - absolute 1e-10 : exact algebraic identities (closed-form == pipeline)
#   - absolute 1e-8  : general default
#   - relative 1e-6  : cross-implementation comparisons (estudy2-derived)

test_that("Market Model AR/CAR t-test matches MacKinlay (1997) closed form", {
  # Provenance: constants derived from an independent lm() fit on the fixed
  #   estimation fixture (golden_market_model_fixture), reproduced in
  #   data-raw/derive-golden-values.R. This is the MacKinlay (1997) canonical
  #   OLS market-model case.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Market Model):
  #   AR = R - (alpha + beta*Rm); sigma = residual SE with df = m - 2 = 4;
  #   ar_t = AR/sigma; car_t = CAR/(sqrt(L)*sigma), constant-sigma approximation;
  #   two-sided.
  # Tolerance: absolute 1e-10 -- this is an exact algebraic identity between the
  #   package pipeline and an independent closed-form OLS fit, so the tight
  #   identity tolerance applies (not the looser cross-impl relative tolerance).
  skip_if_not_installed("distributional")

  d <- golden_market_model_fixture()
  m <- MarketModel$new()
  m$fit(d)

  # Model-level pins.
  expect_equal(m$statistics$sigma, 0.00166476081508083, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 4L)

  ar_res  <- ARTTest$new()$compute(m$abnormal_returns(d), m)
  car_res <- CARTTest$new()$compute(m$abnormal_returns(d), m)

  # AR t per event day (L = 3 event days).
  expect_equal(
    ar_res$ar_t,
    c(7.64588601153099, 1.36441736906108, 6.07551885091351),
    tolerance = 1e-10
  )

  # CAR t cumulative over the 3-day event window.
  expect_equal(
    car_res$car_t,
    c(7.64588601153099, 6.37124662096473, 8.70980352630659),
    tolerance = 1e-10
  )
})
