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
  expect_silent(mm$fit(data))
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
  expect_silent(mm$fit(data))
  expect_true(mm$is_fitted)
})
