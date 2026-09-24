# test_numerical_stability.R
#
# Regression locks for the additive numerical-stability guards on the sensitive
# numeric paths (Phase 27, CORR-04). Each guard fires ONLY on a degenerate /
# ill-conditioned / overflow input and routes through the degenerate-input
# contract (R/contract.R): NA + exactly one warning in lenient mode, stop() in
# strict mode. Valid-input behavior is unchanged (SC5), pinned by a positive
# control per guard.

# --------------------------------------------------------------------------
# Guard 1: ill-conditioned (near-collinear) OLS design in MarketModel$fit()
# --------------------------------------------------------------------------

test_that("ill-conditioned Market Model design -> NA + one warning (lenient)", {
  withr::local_options(EventStudy.degenerate_handling = "lenient")
  data <- invariant_ill_conditioned_fixture(scale = 1e-9)  # rcond << sqrt(eps)

  mm <- MarketModel$new()
  # Contract: exactly ONE warning naming the ill-conditioned condition.
  expect_warning(
    mm$fit(data),
    regexp = "ill-conditioned design"
  )
  expect_false(mm$is_fitted)

  # NA propagates through all event-window abnormal returns (never a spurious
  # finite number from an unstable fit).
  ar <- suppressWarnings(mm$abnormal_returns(data))
  expect_true(all(is.na(ar$abnormal_returns)))
})

test_that("ill-conditioned Market Model design -> stop() in strict mode", {
  withr::local_options(EventStudy.degenerate_handling = "strict")
  data <- invariant_ill_conditioned_fixture(scale = 1e-9)

  mm <- MarketModel$new()
  expect_error(
    mm$fit(data),
    regexp = "ill-conditioned design"
  )
})

test_that("well-conditioned design is UNAFFECTED by the ill-conditioned guard", {
  withr::local_options(EventStudy.degenerate_handling = "lenient")

  # Positive control: the golden Market Model design is well-conditioned; the
  # guard must not fire and the fitted coefficients must equal an independent
  # lm() fit exactly. Valid-input behavior is frozen (SC5).
  data <- golden_market_model_fixture()
  est  <- data[data$estimation_window == 1, , drop = FALSE]
  ref  <- stats::lm(firm_returns ~ index_returns, data = est)

  mm <- MarketModel$new()
  # A6 (2026-09-24): this fixture has only 6 estimation-window observations,
  # below the recommended minimum of 30 -- the model still fits (n_valid >=
  # n_params + 1 = 3), but now emits the advisory "short estimation window"
  # warning in BOTH modes. This is not the ill-conditioned/rcond guard this
  # test locks (that guard stays silent, per the assertions below).
  expect_warning(mm$fit(data), "estimation window has only 6 valid observations")
  expect_true(mm$is_fitted)

  # Abnormal returns must match firm - (alpha + beta*index) from the reference
  # fit to a tight ABSOLUTE tolerance 1e-10 (exact reproduction, not the
  # testthat default) -- proving the guard added no happy-path change.
  ar  <- mm$abnormal_returns(data)
  ev  <- ar[ar$event_window == 1, , drop = FALSE]
  pred <- unname(stats::predict(ref, newdata = ev))
  expect_equal(
    ev$abnormal_returns,
    ev$firm_returns - pred,
    tolerance = 1e-10        # valid-input result unchanged -> exact absolute
  )
})

test_that("rcond guard does not fire on a moderately-conditioned design", {
  withr::local_options(EventStudy.degenerate_handling = "lenient")

  # A larger perturbation scale yields a well-conditioned design (rcond well
  # above sqrt(.Machine$double.eps)); the guard must stay silent and fit.
  data <- invariant_ill_conditioned_fixture(scale = 1e-2)
  mm <- MarketModel$new()
  # A6 (2026-09-24): this fixture has only 8 estimation-window observations,
  # below the recommended minimum of 30 -- the rcond guard still stays
  # silent (that is what this test locks), but the advisory short-window
  # warning now fires in both modes.
  expect_warning(mm$fit(data), "estimation window has only 8 valid observations")
  expect_true(mm$is_fitted)
})

# --------------------------------------------------------------------------
# Guard 2: bootstrap degenerate resample -> NA p-values (never spurious 0/1)
# --------------------------------------------------------------------------

test_that("bootstrap degenerate resample (single firm) -> NA p-values, never 0/1", {
  # A single firm makes the cross-firm SD of AR undefined (sd of one value is
  # NA), so the observed t is NA and the bootstrap p-value MUST be NA -- never a
  # spurious 0 or 1 that would look like a real significance verdict. This path
  # is already handled in R/bootstrap.R (obs t NA -> p NA); the test LOCKS it.
  task <- create_fitted_mock_task(n_firms = 1)

  result <- suppressWarnings(
    bootstrap_test(task, n_boot = 19, seed = 42)
  )
  # No exact tolerance: this is an is.na()/set-membership invariant, not a
  # numeric comparison. Every AAR p-value must be NA (single-firm SD undefined).
  expect_true(all(is.na(result$boot_p_aar)))
  expect_false(any(result$boot_p_aar %in% c(0, 1), na.rm = TRUE))
})

test_that("bootstrap positive control: multi-firm p-values are finite in [0,1]", {
  # Positive control (valid-input behavior unchanged, SC5): with several firms
  # the bootstrap produces ordinary finite p-values in [0,1]. The degenerate
  # guard must NOT fire here.
  task <- create_fitted_mock_task(n_firms = 5)
  result <- bootstrap_test(task, n_boot = 49, seed = 7)

  finite_p <- result$boot_p_aar[is.finite(result$boot_p_aar)]
  expect_true(length(finite_p) > 0)
  # p-values are probabilities: bounded in [0,1]. Bound check, no tolerance.
  expect_true(all(finite_p >= 0 & finite_p <= 1))
})

# --------------------------------------------------------------------------
# Guard 3: long-window CAR precision / overflow in CARTTest cumulation
# --------------------------------------------------------------------------

test_that("long-window CAR with large-but-finite returns stays finite; CAR==cumsum(AR)", {
  # A long event window with large (but representable) abnormal returns must
  # cumulate WITHOUT overflowing to Inf, and the additive identity CAR==cumsum(AR)
  # must still hold. Relative tolerance 1e-8 (documented: long additive sum
  # re-accumulation, so relative rather than the 1e-10 short-window absolute).
  fake <- list(statistics = list(sigma = 0.01, degree_of_freedom = 100L))
  n <- 500L
  d <- tibble::tibble(
    relative_index   = seq_len(n),
    event_window     = rep(1L, n),
    abnormal_returns = rep(1e3, n)          # large but far from overflow
  )
  res <- CARTTest$new()$compute(d, fake)
  expect_true(all(is.finite(res$car)))
  expect_true(all(is.finite(res$car_t)))
  expect_equal(
    res$car,
    cumsum(d$abnormal_returns),
    tolerance = 1e-8,        # long additive cumulation -> relative re-accumulation bound
    info = "long-window CAR == cumsum(AR) without overflow"
  )
})

test_that("CAR cumulation that overflows to Inf -> NA car_t + one warning, never a misleading Inf", {
  # Overflow threshold: when the running cumsum exceeds the representable range
  # (.Machine$double.xmax) it becomes Inf. A misleading Inf car_t (an apparently
  # infinite test statistic) is exactly the silently-wrong-number failure this
  # milestone forbids. The guard must return NA for the non-finite entries with
  # exactly ONE warning naming the overflow.
  fake <- list(statistics = list(sigma = 0.01, degree_of_freedom = 100L))
  n <- 500L
  d <- tibble::tibble(
    relative_index   = seq_len(n),
    event_window     = rep(1L, n),
    abnormal_returns = rep(1e307, n)        # cumsum overflows to Inf mid-window
  )
  expect_warning(
    res <- CARTTest$new()$compute(d, fake),
    regexp = "overflow"
  )
  # Wherever CAR overflowed to Inf, car_t must be NA (not Inf/NaN).
  inf_car <- !is.finite(res$car)
  expect_true(any(inf_car))                 # the fixture does overflow
  expect_true(all(is.na(res$car_t[inf_car])))
})

# --------------------------------------------------------------------------
# Guard 4: GARCH / DCC-GARCH non-convergence -> NA + one contract warning
# --------------------------------------------------------------------------
# rugarch/rmgarch are Suggests-only and their fitted numbers are version-fragile,
# so these are skip_if_not_installed-guarded and assert only the CONTRACT
# (is_fitted == FALSE + NA abnormal returns on non-convergence), never a fitted
# constant. The convergence guards already exist (R/models.R GARCHModel:
# rugarch::convergence(res) != 0 -> NA + one warning; R/models_time_varying.R
# DCCGARCHModel: non-finite rcov -> NA + one warning); these tests LOCK them so
# removing a guard fails the suite.

test_that("GARCH non-convergence -> is_fitted FALSE + NA abnormal returns (contract lock)", {
  skip_if_not_installed("rugarch")

  # Near-degenerate estimation data that drives ugarchfit away from convergence
  # (an almost-constant series gives the GARCH recursion nothing to estimate).
  d <- create_degenerate_model_data_insufficient(n_valid = 3)
  m <- GARCHModel$new()
  suppressWarnings(m$fit(d))

  # Contract: on non-convergence / degenerate fit the model is not fitted and
  # abnormal returns degrade to NA -- never a fabricated finite number. is.na()
  # invariant, no numeric tolerance.
  expect_false(isTRUE(m$is_fitted))
  ar <- suppressWarnings(m$abnormal_returns(d))
  expect_true(all(is.na(ar$abnormal_returns[ar$event_window == 1])))
})

test_that("DCC-GARCH non-finite covariance -> is_fitted FALSE + NA (contract lock)", {
  skip_if_not_installed("rugarch")
  skip_if_not_installed("rmgarch")

  d <- create_degenerate_model_data_insufficient(n_valid = 3)
  m <- DCCGARCHModel$new()
  suppressWarnings(m$fit(d))

  expect_false(isTRUE(m$is_fitted))
  ar <- suppressWarnings(m$abnormal_returns(d))
  expect_true(all(is.na(ar$abnormal_returns[ar$event_window == 1])))
})
