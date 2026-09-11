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

test_that("Market Adjusted Model AR/sigma matches closed form (MacKinlay 1997)", {
  # Provenance: closed-form subtraction on golden_market_adjusted_fixture --
  #   AR = firm_returns - index_returns, sigma = sd(firm - index) over the
  #   estimation window. Cross-checked against an independent hand computation
  #   in data-raw/derive-golden-values.R. The index IS the benchmark
  #   (market-adjusted / index model: alpha = 0, beta = 1), MacKinlay (1997)
  #   sec. 4.4.2.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Market
  #   Adjusted Model): AR = R - Rm; sigma = sd(residuals); df = m - 1 = 5;
  #   constant-mean FEC sigma*sqrt(1 + 1/m); two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and a direct subtraction, so the tight identity tolerance applies.
  d <- golden_market_adjusted_fixture()
  m <- MarketAdjustedModel$new()
  m$fit(d)

  expect_equal(m$statistics$sigma, 0.0031885210782848315, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 5L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns[ar$event_window == 1],
    c(0.018, -0.006, 0.012),
    tolerance = 1e-10
  )
})

test_that("Comparison Period Mean model AR/sigma matches closed form (Brown-Warner 1985)", {
  # Provenance: closed-form on golden_comparison_mean_fixture -- AR = firm minus
  #   the estimation-window mean (constructed to be exactly 0.01), sigma =
  #   sd(firm - mean). Reproduced in data-raw/derive-golden-values.R. This is the
  #   constant-mean-return model of Brown & Warner (1985) / MacKinlay (1997).
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Comparison
  #   Period Mean Adjusted Model): AR = R - mean(R_est); sigma = sd(residuals);
  #   df = m - 1 = 5; constant-mean FEC sigma*sqrt(1 + 1/m); two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity (subtraction against a
  #   fixed mean), so the tight identity tolerance applies.
  d <- golden_comparison_mean_fixture()
  m <- ComparisonPeriodMeanAdjustedModel$new()
  m$fit(d)

  expect_equal(m$model, 0.01, tolerance = 1e-10)  # estimation-window mean
  expect_equal(m$statistics$sigma, 0.0028284271247461901, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 5L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns[ar$event_window == 1],
    c(0.02, -0.015, 0.008),
    tolerance = 1e-10
  )
})

test_that("Fama-French 3-factor AR/sigma/df matches closed-form OLS (Fama-French 1993)", {
  # Provenance: independent lm(excess_return ~ market_excess + smb + hml) fit on
  #   golden_factor_model_fixture with predict() for the event window, giving
  #   AR = excess_return - predicted. Reproduced in
  #   data-raw/derive-golden-values.R. This is the Fama & French (1993)
  #   three-factor OLS convention.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Fama-French
  #   Three-Factor Model): AR = excess_return - predicted; sigma = residual SE;
  #   df = m - (k + 1) = 8 - 4 = 4; multi-factor hat-value FEC; two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent lm()/predict() fit on the same fixture.
  d <- golden_factor_model_fixture()
  m <- FamaFrench3FactorModel$new()
  m$fit(d)

  expect_equal(m$statistics$sigma, 0.0011487290143446527, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 4L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    unname(ar$abnormal_returns[ar$event_window == 1]),
    c(0.0052322898643110804, -0.0036006681996175191, 0.005127563518805207),
    tolerance = 1e-10
  )
})

test_that("Fama-French 5-factor AR/sigma/df matches closed-form OLS (Fama-French 2015)", {
  # Provenance: independent lm(excess_return ~ market_excess + smb + hml + rmw +
  #   cma) fit on golden_factor_model_fixture with predict() for the event
  #   window. Reproduced in data-raw/derive-golden-values.R. Fama & French (2015)
  #   five-factor OLS convention.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Fama-French
  #   Five-Factor Model): AR = excess_return - predicted; sigma = residual SE;
  #   df = m - (k + 1) = 8 - 6 = 2; multi-factor hat-value FEC; two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent lm()/predict() fit on the same fixture.
  d <- golden_factor_model_fixture()
  m <- FamaFrench5FactorModel$new()
  m$fit(d)

  expect_equal(m$statistics$sigma, 0.00049531882326697056, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 2L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    unname(ar$abnormal_returns[ar$event_window == 1]),
    c(0.0077819653514609628, -0.0052661577349010259, 0.0064095759008179626),
    tolerance = 1e-10
  )
})

test_that("Carhart 4-factor AR/sigma/df matches closed-form OLS (Carhart 1997)", {
  # Provenance: independent lm(excess_return ~ market_excess + smb + hml + mom)
  #   fit on golden_factor_model_fixture with predict() for the event window.
  #   Reproduced in data-raw/derive-golden-values.R. Carhart (1997) four-factor
  #   (FF3 + momentum) OLS convention.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Carhart
  #   Four-Factor Model): AR = excess_return - predicted; sigma = residual SE;
  #   df = m - (k + 1) = 8 - 5 = 3; multi-factor hat-value FEC; two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent lm()/predict() fit on the same fixture.
  d <- golden_factor_model_fixture()
  m <- Carhart4FactorModel$new()
  m$fit(d)

  expect_equal(m$statistics$sigma, 0.0013089355539710223, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 3L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    unname(ar$abnormal_returns[ar$event_window == 1]),
    c(0.0055584815456853481, -0.0040944050121481806, 0.0055390294422756683),
    tolerance = 1e-10
  )
})

test_that("Custom Model uses the user-supplied prediction (documented convention)", {
  # Provenance: CustomModel inherits MarketModel's OLS fit but defines
  #   AR = firm_returns - predict(fitted_model) + (loss_market_cap on the event
  #   date). There is no published closed-form constant for a user-supplied
  #   model, so this pins the DOCUMENTED convention: with loss_market_cap = 0 the
  #   Custom Model reduces exactly to the Market Model AR. See
  #   vignettes/statistical-conventions.Rmd (Custom Model).
  # Assumed conventions: AR = R - predict(user model), plus an optional
  #   event-date loss_market_cap adjustment (0 here); sigma/df inherited from the
  #   underlying OLS fit (Market Model, df = m - 2 = 4).
  # Tolerance: absolute 1e-10 -- exact identity with the Market Model when
  #   loss_market_cap = 0.
  d <- golden_market_model_fixture()
  d$loss_market_cap <- 0  # neutralize the event-date adjustment

  cm <- CustomModel$new()
  cm$fit(d)
  mm <- MarketModel$new()
  mm$fit(d)

  ar_custom <- cm$abnormal_returns(d)
  ar_market <- mm$abnormal_returns(d)

  # With loss_market_cap = 0 the Custom Model equals the Market Model exactly.
  expect_equal(
    ar_custom$abnormal_returns,
    ar_market$abnormal_returns,
    tolerance = 1e-10
  )
  expect_equal(cm$statistics$sigma, mm$statistics$sigma, tolerance = 1e-10)
  expect_equal(cm$statistics$degree_of_freedom, 4L)
})
