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
  muffle_short_window(m$fit(d))

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
  muffle_short_window(m$fit(d))

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
  muffle_short_window(m$fit(d))

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
  muffle_short_window(m$fit(d))

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
  muffle_short_window(m$fit(d))

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
  muffle_short_window(m$fit(d))

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
  muffle_short_window(cm$fit(d))
  mm <- MarketModel$new()
  muffle_short_window(mm$fit(d))

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

# --------------------------------------------------------------------------
# Specialized / time-varying return models (Task 3).
# --------------------------------------------------------------------------

test_that("BHAR model + BHARTTest match the compounded closed form (Barber-Lyon 1997)", {
  # Provenance: closed-form buy-and-hold compounding on golden_bhar_fixture --
  #   BHAR = cumprod(1 + firm) - cumprod(1 + index) over the event window; sigma
  #   = sd(firm - index) over the estimation window; bhar_se = sigma * sqrt(n).
  #   Reproduced in data-raw/derive-golden-values.R. This is the buy-and-hold
  #   abnormal-return convention of Barber & Lyon (1997) with the sqrt(n)
  #   standard-error scaling of Lyon, Barber & Tsai (1999).
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, BHAR Model):
  #   AR = cumprod(1 + firm) - cumprod(1 + index) (compounded, not summed);
  #   sigma = sd(firm - index); df = m - 1 = 5; bhar_se = sigma * sqrt(n) where
  #   n is the day index within the event window; two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent closed-form compounding on the same fixture.
  d <- golden_bhar_fixture()
  m <- BHARModel$new()
  muffle_short_window(m$fit(d))

  expect_equal(m$statistics$sigma, 0.0061318838867023568, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 5L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns[ar$event_window == 1],
    c(0.025000000000000133, 0.019675000000000109, 0.040463750000000243),
    tolerance = 1e-10
  )

  bt <- BHARTTest$new()$compute(ar, m)
  expect_equal(
    bt$bhar_se,
    c(0.0061318838867023568, 0.0086717933554715208, 0.0106207344378814027),
    tolerance = 1e-10
  )
  expect_equal(
    bt$bhar_t,
    c(4.0770504565840353, 2.2688501897460513, 3.8098824743867570),
    tolerance = 1e-10
  )
})

test_that("Volume model abnormal volume matches the log-mean closed form", {
  # Provenance: closed-form on golden_volume_fixture -- with log_transform = TRUE
  #   the expected volume is mean(log(estimation firm_volume + 1)) and the
  #   abnormal volume is log(firm_volume + 1) - expected. Reproduced in
  #   data-raw/derive-golden-values.R. Standard abnormal-trading-volume
  #   convention (log volume relative to the estimation-window mean).
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Volume
  #   Model): measure = log(volume + 1); expected = estimation-window log-mean;
  #   AR = log(volume + 1) - expected; sigma = sd(log residuals); df = m - 1 = 5;
  #   two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and a direct log-mean subtraction.
  d <- golden_volume_fixture()
  m <- VolumeModel$new()
  muffle_short_window(m$fit(d))

  expect_equal(m$model, 6.9370259048580936, tolerance = 1e-10)  # log-volume mean
  expect_equal(m$statistics$sigma, 0.10347034309378118, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 5L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns[ar$event_window == 1],
    c(0.66437642972563982, -0.25116495778973391, 0.37686092677536820),
    tolerance = 1e-10
  )
})

test_that("Volatility model abnormal volatility matches the ratio closed form", {
  # Provenance: closed-form on golden_volatility_fixture -- est_var =
  #   var(estimation firm_returns) and AR = firm_returns^2 / est_var - 1.
  #   Reproduced in data-raw/derive-golden-values.R. Standard abnormal-volatility
  #   convention (squared return relative to the estimation-window variance).
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Volatility
  #   Model): est_var = var(estimation firm_returns) (df = m - 1 sample
  #   variance); AR = firm_returns^2 / est_var - 1; sigma = sd(ratio residuals);
  #   df = m - 1 = 5; two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and a direct ratio computation.
  d <- golden_volatility_fixture()
  m <- VolatilityModel$new()
  muffle_short_window(m$fit(d))

  expect_equal(m$model, 0.00038000000000000002, tolerance = 1e-10)  # est_var
  expect_equal(m$statistics$sigma, 0.82451474298735783, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 5L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns[ar$event_window == 1],
    c(5.57894736842105399, 3.21052631578947345, -0.73684210526315796),
    tolerance = 1e-10
  )
})

test_that("Rolling-Window model reduces to a single closed-form OLS on a 30-obs fixture", {
  # Provenance: closed-form on golden_rolling_window_fixture. The model defaults
  #   are window_size = 60, min_obs = 30; with exactly m = 30 estimation
  #   observations the effective window ws = min(60, 30) = 30 is a SINGLE window
  #   equal to the full sample, so the rolling fit reduces to one OLS whose
  #   alpha/beta/sigma are exact. AR = firm - (alpha_last + beta_last * index).
  #   Reproduced in data-raw/derive-golden-values.R. The time-varying-beta
  #   convention (last-window parameters used for event prediction) is documented
  #   in the vignette; here the deterministic single-window case pins the OLS.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Rolling-Window
  #   Model): rolling OLS firm ~ index; last window's parameters predict the event
  #   window; sigma = last-window residual SE with denom = ws - 2; df =
  #   max(ws - 2, 1) = 28; two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent single-window OLS on the same fixture.
  d <- golden_rolling_window_fixture()
  m <- RollingWindowModel$new()
  muffle_short_window(m$fit(d))

  expect_equal(m$statistics$alpha, 0.0039999999999999992, tolerance = 1e-10)
  expect_equal(m$statistics$beta, 1.1967741935483869, tolerance = 1e-10)
  expect_equal(m$statistics$sigma, 0.0011164338756776096, tolerance = 1e-10)
  expect_equal(m$statistics$degree_of_freedom, 28L)

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns[ar$event_window == 1],
    c(0.0220645161290322633, 0.0079677419354838713, 0.0180483870967741988),
    tolerance = 1e-10
  )
})

test_that("GARCH model abnormal return is the mean-equation residual identity (Engle 1982; skip-guarded)", {
  # Provenance: GARCH/DCC-GARCH depend on rugarch/rmgarch, which are OPTIONAL and
  #   whose fitted coefficients vary across package/solver versions. Pinning a raw
  #   fitted constant would flake across versions (threat T-26-02), so -- as the
  #   documented golden-value choice (see vignettes/statistical-conventions.Rmd,
  #   GARCH Model) -- we pin only the STABLE ALGEBRAIC IDENTITY that the model's
  #   abnormal_returns() satisfies for ANY fitted coefficients:
  #     AR = firm_returns - (mu + mxreg1 * index_returns),
  #   where mu, mxreg1 are the fitted mean-equation coefficients. This is exact
  #   regardless of the fitted numbers, so it locks the AR *formula* (Engle 1982
  #   ARCH / sGARCH mean-equation convention) without a version-fragile constant.
  # Assumed conventions: mean equation firm ~ mu + mxreg1 * index (index as an
  #   external regressor); AR = firm - fitted mean; sigma = mean conditional
  #   sigma; two-sided.
  # Tolerance: absolute 1e-10 -- this is an exact identity, not a cross-impl
  #   comparison.
  skip_if_not_installed("rugarch")

  d <- golden_market_model_fixture()  # any fixture with firm/index returns works
  m <- GARCHModel$new()
  muffle_short_window(m$fit(d))
  skip_if_not(isTRUE(m$is_fitted), "GARCH fit did not converge on the fixture")

  coefs  <- rugarch::coef(m$model)
  mu     <- unname(coefs["mu"])
  mxreg1 <- unname(coefs["mxreg1"])

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns,
    d$firm_returns - (mu + mxreg1 * d$index_returns),
    tolerance = 1e-10
  )
})

test_that("DCC-GARCH model abnormal return is the time-varying-beta residual identity (Bollerslev 1990; skip-guarded)", {
  # Provenance: like GARCH above, DCC-GARCH (rmgarch) is optional and
  #   non-deterministic across versions, so we pin only the stable algebraic
  #   IDENTITY its abnormal_returns() satisfies for ANY fitted parameters:
  #     AR = firm_returns - (alpha_last + beta_last * index_returns),
  #   where beta_last is the last conditional beta = Cov(firm, mkt)_t / Var(mkt)_t
  #   and alpha_last is the mean-equation intercept. Documented as the golden-value
  #   choice in vignettes/statistical-conventions.Rmd (DCC-GARCH Model). Cites the
  #   dynamic-conditional-correlation / time-varying-beta convention (Bollerslev
  #   1990; Engle 2002).
  # Assumed conventions: time-varying beta from the DCC conditional covariance;
  #   last conditional beta used for event prediction; AR = firm - (alpha + beta *
  #   index); two-sided.
  # Tolerance: absolute 1e-10 -- exact identity, not a cross-impl comparison.
  skip_if_not_installed("rmgarch")
  skip_if_not_installed("rugarch")

  d <- golden_market_model_fixture()
  m <- DCCGARCHModel$new()
  muffle_short_window(m$fit(d))
  skip_if_not(isTRUE(m$is_fitted), "DCC-GARCH fit did not converge on the fixture")

  alpha_last <- m$statistics$alpha
  beta_last  <- m$statistics$beta

  ar <- m$abnormal_returns(d)
  expect_equal(
    ar$abnormal_returns,
    d$firm_returns - (alpha_last + beta_last * d$index_returns),
    tolerance = 1e-10
  )
})

# --------------------------------------------------------------------------
# Single-event and cross-sectional test statistics (Task 4).
# --------------------------------------------------------------------------

test_that("Cross-sectional AAR/CAAR t matches the closed form (Brown-Warner 1985)", {
  # Provenance: closed-form on golden_multi_event_fixture (3 Market-Model events,
  #   m = 8, L = 3). Per event day the abnormal returns across events give
  #   aar = mean(AR), sd_ar = sd(AR) (sample SD, N - 1), and the cross-sectional t
  #   is aar_t = sqrt(N) * aar / sd_ar; the cumulative version uses per-event CARs:
  #   caar = mean(CAR), sd_caar = sd(CAR), caar_t = sqrt(N) * caar / sd_caar.
  #   Reproduced in data-raw/derive-golden-values.R. This is the cross-sectional
  #   t-test of Brown & Warner (1985), distributed t_{N-1}.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd,
  #   Cross-Sectional t-test): aar_t = sqrt(N) * aar / sd(AR); caar_t =
  #   sqrt(N) * caar / sd(CAR); N = number of valid events; sd is the sample SD
  #   (denominator N - 1); distributed t_{N-1}; two-sided by default.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent closed-form cross-sectional computation.
  fx <- golden_multi_event_fixture()
  res <- CSectTTest$new()$compute(fx$data, NULL)

  expect_equal(res$n_events, c(3L, 3L, 3L))
  expect_equal(
    res$aar,
    c(0.017858427448395411, -0.003055141803282022, 0.012352491200872388),
    tolerance = 1e-10
  )
  expect_equal(
    res$aar_t,
    c(12.785674729349529, -0.93664440956979689, 5.8589693453114027),
    tolerance = 1e-10
  )
  expect_equal(
    res$caar,
    c(0.017858427448395411, 0.014803285645113389, 0.027155776845985774),
    tolerance = 1e-10
  )
  expect_equal(
    res$caar_t,
    c(12.785674729349529, 3.3332365393903789, 4.1627875088073241),
    tolerance = 1e-10
  )
})

test_that("Patell standardized-residual Z matches the closed form (Patell 1976)", {
  # Provenance: closed-form on golden_multi_event_fixture. Each event's abnormal
  #   returns are standardized by its forecast-error-corrected sigma,
  #   SAR = AR / fec_sigma; the Patell denominator is Q_total = sqrt(sum Q_i) with
  #   Q_i = (m - k) / (m - k - 2) (here m = 8, k = 2 -> Q_i = 6/4 = 1.5); the AAR
  #   Z is aar_z = sum(SAR) / Q_total. The cumulative version standardizes the
  #   per-event cumulative SAR by sqrt(n * Q_i) and averages: caar_z =
  #   (1/sqrt(N)) * sum(cumsum(SAR) / sqrt(n * Q_i)). Reproduced in
  #   data-raw/derive-golden-values.R. Patell (1976) standardized-residual test.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Patell
  #   Z-test): SAR = AR / forecast-error-corrected sigma; Q_i = (m - k)/(m - k - 2);
  #   Q_total = sqrt(sum Q_i); aar_z = sum(SAR)/Q_total; approximately N(0, 1);
  #   N >= 2 guard (aar_z / caar_z are NA for a single valid event); two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent closed-form Patell computation.
  fx <- golden_multi_event_fixture()
  res <- PatellZTest$new()$compute(fx$data, fx$model)

  expect_equal(
    res$aar_z,
    c(17.404798533168357, -3.239085545200092, 12.231088522816789),
    tolerance = 1e-10
  )
  expect_equal(
    res$caar_z,
    c(17.404798533168361, 10.016671714134711, 15.240200457996872),
    tolerance = 1e-10
  )
})

test_that("Patell Z returns NA for a single-event group (N >= 2 guard, STATS-04)", {
  # Provenance: documented convention, not a published table. The Patell
  #   approximation requires N >= 2 for the cross-event variance to be estimable;
  #   with a single valid event aar_z / caar_z are statistically invalid, so the
  #   package returns NA rather than a finite-but-wrong number. This pins that
  #   guard (see vignettes/statistical-conventions.Rmd, Patell Z-test).
  # Tolerance: exact (NA identity) -- no numeric tolerance needed.
  fx <- golden_multi_event_fixture()
  one <- list(
    data  = fx$data[fx$data$event_id == "E1", ],
    model = fx$model[fx$model$event_id == "E1", ]
  )
  res <- PatellZTest$new()$compute(one$data, one$model)
  expect_true(all(is.na(res$aar_z)))
  expect_true(all(is.na(res$caar_z)))
})

test_that("BMP standardized cross-sectional t matches the closed form (Boehmer-Musumeci-Poulsen 1991)", {
  # Provenance: closed-form on golden_multi_event_fixture. Each event's abnormal
  #   returns are standardized by its MODEL sigma (not the FEC sigma),
  #   SAR = AR / sigma; the BMP statistic is the cross-sectional t of the SARs:
  #   bmp_t = sqrt(N) * mean(SAR) / sd(SAR) (sample SD, N - 1). The cumulative
  #   version uses per-event cumulative SAR: cbmp_t = sqrt(N) * mean(cumsum(SAR)) /
  #   sd(cumsum(SAR)). Reproduced in data-raw/derive-golden-values.R.
  #   Boehmer, Musumeci & Poulsen (1991) standardized cross-sectional test,
  #   robust to event-induced variance.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, BMP test):
  #   SAR = AR / model sigma; bmp_t = sqrt(N) * mean(SAR) / sd(SAR); distributed
  #   approximately t_{N-1}; two-sided by default.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent closed-form BMP computation.
  fx <- golden_multi_event_fixture()
  res <- BMPTest$new()$compute(fx$data, fx$model)

  expect_equal(
    res$mean_sar,
    c(13.478448984553861, -2.4691314518574581, 9.2773617814074072),
    tolerance = 1e-10
  )
  expect_equal(
    res$bmp_t,
    c(9.262881651878164, -0.99419205506945918, 5.6244379647718095),
    tolerance = 1e-10
  )
  expect_equal(
    res$cbmp_t,
    c(9.262881651878164, 3.431336898228547, 4.2274213480688871),
    tolerance = 1e-10
  )
})

# --------------------------------------------------------------------------
# Nonparametric and correlation-robust statistics (Task 5).
# --------------------------------------------------------------------------

test_that("Sign test z matches the binomial-normal closed form (N >= 2 guard)", {
  # Provenance: closed-form on golden_multi_event_fixture (3 Market-Model events,
  #   L = 3). The point sign test counts strictly positive abnormal returns
  #   (n_pos = sum(AR > 0); zeros count as NON-positive) and computes
  #   sign_z = (n_pos - 0.5 N) / (0.5 sqrt(N)); the cumulative version counts
  #   strictly positive per-event CARs. Reproduced in
  #   data-raw/derive-golden-values.R. On this fixture all three per-event CARs
  #   stay positive on every day, so csign_z = 1.7320508 throughout is correct
  #   (not a broadcast artifact -- the point sign_z varies: +1.73, -0.58, +1.73).
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Sign test):
  #   n_pos = sum(AR > 0) (zero is non-positive -- the >0 vs >=0 convention);
  #   z = (n_pos - 0.5 N) / (0.5 sqrt(N)); N >= 2 guard (n_valid_events < 2 -> NA);
  #   approximately N(0, 1); two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and a direct binomial-normal computation.
  fx <- golden_multi_event_fixture()
  res <- SignTest$new()$compute(fx$data, NULL)

  expect_equal(res$n_pos, c(3L, 1L, 3L))
  expect_equal(res$n_neg, c(0L, 2L, 0L))
  expect_equal(
    res$sign_z,
    c(1.7320508075688774, -0.57735026918962584, 1.7320508075688774),
    tolerance = 1e-10
  )
  expect_equal(
    res$csign_z,
    c(1.7320508075688774, 1.7320508075688774, 1.7320508075688774),
    tolerance = 1e-10
  )
})

test_that("Sign test returns NA for a single-event group (N >= 2 guard, STATS-04)", {
  # Provenance: documented convention, not a published table. With a single
  #   valid event the sign test's normal approximation is invalid, so the package
  #   returns NA rather than a finite-but-meaningless z (see
  #   vignettes/statistical-conventions.Rmd, Sign test).
  # Tolerance: exact (NA identity) -- no numeric tolerance needed.
  fx <- golden_multi_event_fixture()
  one <- fx$data[fx$data$event_id == "E1", ]
  res <- SignTest$new()$compute(one, NULL)
  expect_true(all(is.na(res$sign_z)))
  expect_true(all(is.na(res$csign_z)))
})

test_that("Generalized sign test z matches the closed form (Cowan 1992)", {
  # Provenance: closed-form on golden_multi_event_fixture. p_hat is the average,
  #   across firms, of the estimation-window fraction of positive abnormal
  #   returns; here every firm's estimation residuals are exactly balanced so
  #   p_hat = 0.5 and the generalized sign test reduces to the ordinary sign test
  #   on this fixture (gsign_z equals sign_z). z = (n_pos - N p_hat) /
  #   sqrt(N p_hat (1 - p_hat)). Reproduced in data-raw/derive-golden-values.R.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Generalized
  #   Sign test): p_hat from the estimation window (per firm, then averaged);
  #   n_pos = sum(AR > 0); z = (n_pos - N p_hat)/sqrt(N p_hat (1 - p_hat));
  #   approximately N(0, 1); two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent closed-form Cowan (1992) computation.
  fx <- golden_multi_event_fixture()
  res <- GeneralizedSignTest$new()$compute(fx$data, NULL)

  expect_equal(
    res$gsign_z,
    c(1.7320508075688774, -0.57735026918962584, 1.7320508075688774),
    tolerance = 1e-10
  )
  expect_equal(
    res$cgsign_z,
    c(1.7320508075688774, 1.7320508075688774, 1.7320508075688774),
    tolerance = 1e-10
  )
})

test_that("Corrado rank test z matches the closed form and varies per day (Corrado 1989)", {
  # Provenance: closed-form on golden_multi_event_fixture. Within each firm the
  #   abnormal returns are ranked over the COMBINED estimation + event window and
  #   centered as K = rank / (T + 1) - 0.5; per relative day the cross-firm mean
  #   centered rank is taken; S_rank is the SD of the per-day mean centered rank
  #   across all combined days; rank_z = mean_rank_day / S_rank. Reproduced in
  #   data-raw/derive-golden-values.R.
  # Regression note (Task 5 audit fix): rank_z previously used base ifelse() with
  #   a SCALAR condition on S_rank, silently collapsing the per-day vector to its
  #   first element and recycling it across every event day (all three days
  #   reported the day-0 value 1.5864). The fix divides the vector directly,
  #   guarding the scalar denominator with a plain if(). This test locks the
  #   corrected per-day values (+1.586, -0.952, +1.269), so the broadcast bug
  #   cannot silently return.
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Rank test):
  #   combined-window centered ranks K = rank/(T+1) - 0.5; rank_z =
  #   mean_rank_day / S_rank; approximately N(0, 1); two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent closed-form Corrado (1989) computation.
  fx <- golden_multi_event_fixture()
  res <- RankTest$new()$compute(fx$data, NULL)

  expect_equal(
    res$mean_rank,
    c(0.41666666666666663, -0.25, 0.33333333333333337),
    tolerance = 1e-10
  )
  expect_equal(
    res$rank_z,
    c(1.586435792425881, -0.95186147545552868, 1.269148633940705),
    tolerance = 1e-10
  )
  # The three per-day z values are distinct -- proves the broadcast bug is fixed.
  expect_false(res$rank_z[1] == res$rank_z[2])
})

test_that("Calendar-time portfolio t matches the closed form and varies per day", {
  # Provenance: closed-form on golden_multi_event_fixture. For each relative day
  #   an equal-weight portfolio of event-firm abnormal returns gives the AAR; the
  #   time-series t is AAR / sd(AAR) and the cumulative t is CAAR /
  #   (sd(AAR) sqrt(L)), with sd taken over the L portfolio days. Reproduced in
  #   data-raw/derive-golden-values.R.
  # Regression note (Task 5 audit fix): caltime_t / ccaltime_t previously used
  #   base ifelse() with a SCALAR condition on ts_sd, silently collapsing the
  #   per-day t vectors to their first element and recycling it across every day
  #   (all three days reported the day-0 value 1.6474). The fix divides the
  #   vectors directly, guarding the scalar denominator with a plain if(). This
  #   test locks the corrected per-day values so the broadcast bug cannot return.
  # Re-pinned 2026-09-24: A7 -- the denominator is now the Brown-Warner (1980,
  #   1985) ESTIMATION-window AAR standard deviation (ts_sd = sd(AAR over the
  #   8 estimation days)), not sd(AAR over the 3 event-window days) as before.
  #   caltime_t = AAR_t / ts_sd; ccaltime_t = CAAR_t / (ts_sd * sqrt(L));
  #   df = n_estimation_days - 1 = 7, exposed as attr(res, "caltime_df").
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Calendar-Time
  #   Portfolio test): equal-weight portfolio AAR; time-series t = AAR / ts_sd,
  #   CAAR t = CAAR / (ts_sd * sqrt(L)); ts_sd over the ESTIMATION-window days;
  #   two-sided.
  # Tolerance: absolute 1e-10 -- exact algebraic identity between the package
  #   pipeline and an independent closed-form portfolio computation.
  fx <- golden_multi_event_fixture()
  res <- suppressWarnings(CalendarTimePortfolioTest$new()$compute(fx$data, NULL))

  expect_equal(
    res$aar,
    c(0.017858427448395411, -0.003055141803282022, 0.012352491200872388),
    tolerance = 1e-10
  )
  expect_equal(
    res$caltime_t,
    c(14.5159150457412, -2.48331937385221, 10.0405096357597),
    tolerance = 1e-10
  )
  expect_equal(
    res$ccaltime_t,
    c(14.5159150457412, 8.50832999486859, 12.7439132912219),
    tolerance = 1e-10
  )
  expect_equal(attr(res, "caltime_df"), 7)
  # The three per-day t values are distinct -- proves the broadcast bug is fixed.
  expect_false(res$caltime_t[1] == res$caltime_t[2])
})

test_that("Kolari-Pynnonen adjusted BMP matches the closed form (Kolari-Pynnonen 2010)", {
  # Provenance: closed-form on golden_multi_event_fixture. The KP statistic is the
  #   BMP cross-sectional t scaled by kp_adj = sqrt((1 - r_bar) / (1 + (N - 1)
  #   r_bar)), where r_bar is the average pairwise correlation of the
  #   estimation-window standardized abnormal returns (SAR = AR / model sigma).
  #   On this fixture the three events' estimation SARs are highly correlated by
  #   construction (r_bar ~ 0.983), so kp_adj ~ 0.0764 shrinks the BMP t sharply --
  #   exactly the cross-sectional-correlation penalty the KP adjustment applies.
  #   Reproduced in data-raw/derive-golden-values.R; the independent derivation
  #   agrees with the pinned pipeline values to ~1e-11 (the ~13th significant
  #   digit differs only because r_bar is accumulated over a slightly different
  #   floating-point path).
  # Assumed conventions (see vignettes/statistical-conventions.Rmd, Kolari-
  #   Pynnonen test): SAR = AR / model sigma (as BMP); r_bar = average off-diagonal
  #   pairwise correlation of estimation-window SARs; kp_adj =
  #   sqrt((1 - r_bar)/(1 + (N - 1) r_bar)); kp_t = bmp_t * kp_adj; two-sided.
  # Tolerance: absolute 1e-10 -- the pinned values are the package pipeline's own
  #   output; the closed-form cross-check agrees to ~1e-11, comfortably inside the
  #   identity tolerance.
  fx <- golden_multi_event_fixture()
  res <- KolariPynnonenTest$new()$compute(fx$data, fx$model)

  expect_equal(
    res$kp_t,
    c(0.70742152433092598, -0.075928084316215713, 0.42954758876108962),
    tolerance = 1e-10
  )
  expect_equal(
    res$ckp_t,
    c(0.70742152433092598, 0.26205684907413279, 0.32285512936825822),
    tolerance = 1e-10
  )
})
