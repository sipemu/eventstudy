# test_invariants.R
#
# Cross-cutting statistical invariants that must hold for ANY correct return
# model, asserted by looping over invariant_model_registry (Phase 27, CORR-03).
#
# Registry-driven so a model is covered by ADDING A ROW to the registry in
# helper-invariant-data.R -- a missing row silently drops coverage, which the
# Phase 27 coverage gate (Task 6) checks.
#
# Tolerance policy (planning_constraints 4): the algebraic identities below
# (CAR==cumsum(AR), single-period edge, monotonic growth) are EXACT, so they
# are asserted at absolute tolerance 1e-10 (never the testthat default).
#
# GARCH/DCC rows are skip_if_not_installed-guarded (their fitted numbers are
# version-fragile); they assert the IDENTITY form only, never a fitted constant.

# Helper: fit a registry row, skipping if its optional upstream is absent.
.invariant_ev_or_skip <- function(reg_row) {
  if (!is.null(reg_row$skip_pkg)) {
    for (pkg in reg_row$skip_pkg) testthat::skip_if_not_installed(pkg)
  }
  invariant_fit_event_ar(reg_row)
}

test_that("CAR == cumsum(AR) holds for every registered return model", {
  for (reg_row in invariant_model_registry) {
    ev <- .invariant_ev_or_skip(reg_row)

    # Universal identity: the cumulative abnormal return at each relative index
    # equals the running sum of per-period abnormal returns. For additive models
    # this is the ordinary CAR==cumsum(AR); for BHAR the reported path IS already
    # cumulative and `ar` holds its increments, so cumsum(increments) == path by
    # construction (the BHAR-specific form documented in helper-invariant-data.R).
    # An algebraic identity, so tight ABSOLUTE tolerance 1e-10.
    expect_equal(
      ev$car,
      invariant_expected_car(ev),
      tolerance = 1e-10,           # exact algebraic identity -> tight absolute
      info = paste0("CAR==cumsum(AR) for ", reg_row$name)
    )
  }
})

test_that("boundary window identity: length-1 window yields CAR == AR", {
  for (reg_row in invariant_model_registry) {
    ev <- .invariant_ev_or_skip(reg_row)

    # Single-period edge: restricting the event window to its first day makes the
    # cumulative return equal the single-period abnormal return exactly. This is
    # the k=1 boundary of the cumulation identity and must hold for every model.
    # Exact algebraic edge -> absolute tolerance 1e-10.
    first_ar  <- ev$ar[1]
    first_car <- ev$car[1]
    expect_equal(
      first_car,
      first_ar,
      tolerance = 1e-10,           # length-1 window: CAR collapses to AR
      info = paste0("length-1 window CAR==AR for ", reg_row$name)
    )
  }
})

test_that("empty/all-NA event window yields all-NA CAR with no crash", {
  for (reg_row in invariant_model_registry) {
    if (!is.null(reg_row$skip_pkg)) {
      for (pkg in reg_row$skip_pkg) testthat::skip_if_not_installed(pkg)
    }
    model <- reg_row$constructor()
    data  <- reg_row$fixture()
    model$fit(data)

    # NA out every event-window observation of the primary input columns so the
    # window is effectively empty. cumsum/cumprod over all-NA must degrade to NA,
    # never crash and never fabricate a finite number.
    ev_rows <- which(data$event_window == 1)
    na_data <- data
    for (col in intersect(
      c("firm_returns", "index_returns", "excess_return", "firm_volume"),
      names(na_data)
    )) {
      na_data[[col]][ev_rows] <- NA_real_
    }

    ar_tbl <- tryCatch(
      model$abnormal_returns(na_data),
      error = function(e) {
        fail(paste0("abnormal_returns crashed on all-NA window for ",
                    reg_row$name, ": ", conditionMessage(e)))
        NULL
      }
    )
    ev <- ar_tbl[ar_tbl$event_window == 1, , drop = FALSE]
    car_na <- cumsum(ifelse(is.na(ev$abnormal_returns), NA_real_,
                            ev$abnormal_returns))
    # Every cumulative entry over an all-NA window must itself be NA. No exact
    # tolerance needed -- this is an is.na() invariant, not a numeric comparison.
    expect_true(
      all(is.na(car_na)),
      info = paste0("all-NA window -> all-NA CAR for ", reg_row$name)
    )
  }
})

test_that("monotonic window growth: CAR_k == CAR_{k-1} + AR_k for every model", {
  for (reg_row in invariant_model_registry) {
    ev <- .invariant_ev_or_skip(reg_row)
    if (nrow(ev) < 2) next

    # Growing the window one step at a time, the CAR at step k must equal the
    # CAR at step k-1 plus the incremental AR_k. This is the per-step form of the
    # cumulation identity (`ar` holds the BHAR path increments for BHAR rows), so
    # it holds uniformly. Exact recurrence -> absolute tolerance 1e-10.
    car_prev <- ev$car[-nrow(ev)]
    ar_k     <- ev$ar[-1]
    car_k    <- ev$car[-1]
    expect_equal(
      car_k,
      car_prev + ar_k,
      tolerance = 1e-10,           # exact cumulation recurrence
      info = paste0("monotonic window growth for ", reg_row$name)
    )
  }
})

# ===========================================================================
# Cross-method-consistency invariants (Phase 27 Task 3, CORR-03)
# ===========================================================================
#
# REPRESENTATIVE SUBSET + RATIONALE (per planning_constraints 5):
# The universal identities above run on EVERY registered model. The heavier
# cross-method invariants below run on a documented subset -- forcing every
# model x method pairing would exercise invalid combinations (e.g. a factor
# model has no meaningful "subtract the index" reduction). The subset is:
#   * Return-strategy consistency: LogReturn vs SimpleReturn on a fixed price
#     series -- the two strategies are only comparable on the SAME price input,
#     so this is a return-calculation-layer invariant, not a model pairing.
#   * OLS/adjusted family: {MarketModel, MarketAdjustedModel,
#     ComparisonPeriodMeanAdjustedModel} -- the family for which a common
#     "benchmark == fitted line" reduction is well-defined. On the alpha=0,
#     beta=1, index==0 fixture all three MUST coincide.
#   * Statistic side: {ARTTest/CARTTest, CSectTTest} -- the statistics for which
#     the AAR = mean(AR) and CAR = cumsum(AR) reductions are defined. Rank/Sign
#     tests have no such linear reduction and are excluded here (covered by the
#     universal + contract layers).
# Tolerances: relative for cross-method comparisons (cross-implementation
# differences accumulate), tight absolute for exact algebraic identities.

test_that("return-strategy consistency: log ~ simple to first order on small returns", {
  prices <- invariant_price_series_fixture()

  log_ret <- LogReturn$new()$calculate_return(
    prices, in_column = "adjusted", out_column = "r"
  )$r
  simple_ret <- SimpleReturn$new()$calculate_return(
    prices, in_column = "adjusted", out_column = "r"
  )$r

  # First lag is NA for both; compare the finite tail. log(1+r) = r - r^2/2 + ...
  # so the ABSOLUTE approximation error is ~r^2/2 (~1.8e-5 on these <0.6% moves)
  # but the RELATIVE error is ~r/2, i.e. ~0.3% of the return itself. testthat's
  # relative comparison divides by magnitude, so the correct cross-method bound
  # here is RELATIVE 1e-2 (documented as the first-order log~simple relative
  # approximation bound for sub-1% returns, NOT an exact identity). The exact
  # bridge below (1e-10) is what pins the strategies precisely.
  ok <- is.finite(log_ret) & is.finite(simple_ret)
  expect_equal(
    log_ret[ok],
    simple_ret[ok],
    tolerance = 1e-2,            # first-order log~simple RELATIVE bound (~r/2) on <1% moves
    info = "LogReturn ~ SimpleReturn to first order on small returns"
  )

  # Internal consistency: an exact algebraic bridge, log_ret == log(1+simple_ret)
  # holds to machine precision regardless of return size (this is the DEFINING
  # relationship between the two strategies, not an approximation).
  expect_equal(
    log_ret[ok],
    log1p(simple_ret[ok]),
    tolerance = 1e-10,           # exact log/simple bridge -> tight absolute
    info = "log_ret == log1p(simple_ret) exactly"
  )
})

test_that("OLS/adjusted family coincides when benchmark == fitted line", {
  # On the alpha=0, beta=1, index==0 fixture the abnormal return under
  # MarketModel (fits alpha~0,beta~1), MarketAdjustedModel (imposes alpha=0,
  # beta=1) and ComparisonPeriodMeanAdjustedModel (subtracts mean(est)==0) all
  # reduce to the same value. Cross-method comparison -> RELATIVE tolerance 1e-6.
  d <- invariant_alpha0_beta1_fixture()

  ar_of <- function(model) {
    # The zero-residual estimation design triggers lm()'s benign "essentially
    # perfect fit" note (the fit IS exact by construction) -- suppress it; it is
    # not a degenerate-input warning, just lm() flagging the perfect fit.
    suppressWarnings(model$fit(d))
    a <- model$abnormal_returns(d)
    unname(a$abnormal_returns[a$event_window == 1])
  }
  ar_mm  <- ar_of(MarketModel$new())
  ar_maj <- ar_of(MarketAdjustedModel$new())
  ar_cpm <- ar_of(ComparisonPeriodMeanAdjustedModel$new())

  expect_equal(
    ar_mm, ar_maj,
    tolerance = 1e-6,            # cross-method: MarketModel vs Market-Adjusted
    info = "MarketModel == MarketAdjustedModel on alpha=0/beta=1 design"
  )
  expect_equal(
    ar_mm, ar_cpm,
    tolerance = 1e-6,            # cross-method: MarketModel vs Comparison-Mean
    info = "MarketModel == ComparisonPeriodMeanAdjustedModel on this design"
  )
})

test_that("statistic-side consistency: AAR==mean(AR) and CAR==cumsum(AR)", {
  fx <- golden_multi_event_fixture()

  # CSectTTest AAR is, by definition, the cross-event mean of per-event AR at
  # each relative index. Reconstruct the expected AAR by hand from the fixture's
  # abnormal returns and compare -- an exact algebraic identity -> 1e-10.
  res <- CSectTTest$new()$compute(fx$data, NULL)
  ar_by_idx <- tapply(
    fx$data$abnormal_returns,
    fx$data$relative_index,
    mean
  )
  expected_aar <- as.numeric(ar_by_idx[order(as.numeric(names(ar_by_idx)))])
  # CSectT reports the event window only; align to its relative_index order.
  res <- res[order(res$relative_index), , drop = FALSE]
  ev_aar <- expected_aar[match(res$relative_index,
                               sort(unique(fx$data$relative_index)))]
  expect_equal(
    res$aar,
    ev_aar,
    tolerance = 1e-10,           # AAR == mean(AR) is an exact definition
    info = "CSectT AAR == cross-event mean of AR"
  )

  # CAAR == cumsum(AAR) over the event window -> exact cumulation, 1e-10.
  expect_equal(
    res$caar,
    cumsum(res$aar),
    tolerance = 1e-10,           # CAAR == cumsum(AAR) exact cumulation
    info = "CSectT CAAR == cumsum(AAR)"
  )
})

test_that("t-statistic is invariant to a positive rescale of returns (MarketModel)", {
  # Scale invariance: multiplying firm AND index returns by a positive constant
  # c rescales AR and sigma by the same c, so the t = AR/sigma ratio is
  # unchanged. Asserted at absolute tolerance 1e-8 (a ratio identity that is
  # exact up to floating-point re-accumulation under the rescale).
  d  <- golden_market_model_fixture()
  cc <- 3.0

  m1 <- MarketModel$new(); m1$fit(d)
  t1 <- CARTTest$new()$compute(m1$abnormal_returns(d), m1)$car_t

  d2 <- d
  d2$firm_returns  <- d2$firm_returns  * cc
  d2$index_returns <- d2$index_returns * cc
  m2 <- MarketModel$new(); m2$fit(d2)
  t2 <- CARTTest$new()$compute(m2$abnormal_returns(d2), m2)$car_t

  expect_equal(
    t1, t2,
    tolerance = 1e-8,            # t = AR/sigma invariant to positive rescale
    info = "CAR t-stat invariant to positive return rescale"
  )
})
