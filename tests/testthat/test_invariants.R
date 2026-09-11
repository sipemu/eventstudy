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
