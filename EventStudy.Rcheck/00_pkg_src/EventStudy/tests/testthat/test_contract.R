# test_contract.R
#
# Regression net for the degenerate-input contract (Phase 1, Plan 02).
# Covers CONTRACT-01 through CONTRACT-05 as defined in R/contract.R and
# the plan's must_haves.  Do NOT hard-code numeric literals for
# CONTRACT-05 — read them from the committed .rds baseline.

# ---------------------------------------------------------------------------
# CONTRACT-02: ParameterSet degenerate_handling field resolution
# ---------------------------------------------------------------------------

test_that("CONTRACT-02: ParameterSet default resolves to lenient", {
  ps <- ParameterSet$new()
  expect_null(ps$degenerate_handling)
  # Resolved mode (via .resolve_degenerate_mode) is "lenient"
  withr::with_options(list(EventStudy.degenerate_handling = NULL), {
    expect_equal(.resolve_degenerate_mode(ps$degenerate_handling), "lenient")
  })
})

test_that("CONTRACT-02: ParameterSet strict field resolves to strict", {
  ps <- ParameterSet$new(degenerate_handling = "strict")
  expect_equal(ps$degenerate_handling, "strict")
  expect_equal(.resolve_degenerate_mode(ps$degenerate_handling), "strict")
})

test_that("CONTRACT-02: package option overrides default when field is NULL", {
  withr::with_options(list(EventStudy.degenerate_handling = "strict"), {
    ps <- ParameterSet$new()
    expect_null(ps$degenerate_handling)
    expect_equal(.resolve_degenerate_mode(ps$degenerate_handling), "strict")
  })
})

test_that("CONTRACT-02: ParameterSet field takes precedence over package option", {
  withr::with_options(list(EventStudy.degenerate_handling = "strict"), {
    ps <- ParameterSet$new(degenerate_handling = "lenient")
    expect_equal(.resolve_degenerate_mode(ps$degenerate_handling), "lenient")
  })
})

test_that("CONTRACT-02: invalid degenerate_handling value raises match.arg error", {
  expect_error(
    ParameterSet$new(degenerate_handling = "bogus"),
    "arg"  # match.arg error message
  )
})

test_that("CONTRACT-02: mode switching within one session works without reload", {
  # Switch strict -> lenient via field — both must resolve correctly
  ps_strict  <- ParameterSet$new(degenerate_handling = "strict")
  ps_lenient <- ParameterSet$new(degenerate_handling = "lenient")
  expect_equal(.resolve_degenerate_mode(ps_strict$degenerate_handling),  "strict")
  expect_equal(.resolve_degenerate_mode(ps_lenient$degenerate_handling), "lenient")

  # Toggling the option live also works
  withr::with_options(list(EventStudy.degenerate_handling = "strict"), {
    expect_equal(.resolve_degenerate_mode(NULL), "strict")
  })
  withr::with_options(list(EventStudy.degenerate_handling = "lenient"), {
    expect_equal(.resolve_degenerate_mode(NULL), "lenient")
  })
})


# ---------------------------------------------------------------------------
# CONTRACT-03 (strict) — insufficient observations
# ---------------------------------------------------------------------------

test_that("CONTRACT-03: strict mode errors on insufficient obs — names event_id, firm_symbol, MarketModel", {
  d  <- create_degenerate_model_data_insufficient(n_valid = 1)
  m  <- MarketModel$new()
  m$degenerate_mode <- "strict"
  m$event_id        <- "EVT_K"
  m$firm_symbol     <- "FIRM_K"

  expect_error(
    m$fit(d),
    regexp = "EVT_K"
  )
  expect_error(
    {
      m2 <- MarketModel$new()
      m2$degenerate_mode <- "strict"
      m2$event_id        <- "EVT_K"
      m2$firm_symbol     <- "FIRM_K"
      m2$fit(d)
    },
    regexp = "FIRM_K"
  )
  expect_error(
    {
      m3 <- MarketModel$new()
      m3$degenerate_mode <- "strict"
      m3$event_id        <- "EVT_K"
      m3$firm_symbol     <- "FIRM_K"
      m3$fit(d)
    },
    regexp = "MarketModel"
  )
})


# ---------------------------------------------------------------------------
# CONTRACT-03 (strict) — zero variance
# ---------------------------------------------------------------------------

test_that("CONTRACT-03: strict mode errors on zero-variance — names event_id, firm_symbol, MarketModel", {
  d  <- create_degenerate_model_data_zero_variance()
  m  <- MarketModel$new()
  m$degenerate_mode <- "strict"
  m$event_id        <- "EVT_K"
  m$firm_symbol     <- "FIRM_K"

  expect_error(
    m$fit(d),
    regexp = "EVT_K"
  )
  expect_error(
    {
      m2 <- MarketModel$new()
      m2$degenerate_mode <- "strict"
      m2$event_id        <- "EVT_K"
      m2$firm_symbol     <- "FIRM_K"
      m2$fit(d)
    },
    regexp = "FIRM_K"
  )
  expect_error(
    {
      m3 <- MarketModel$new()
      m3$degenerate_mode <- "strict"
      m3$event_id        <- "EVT_K"
      m3$firm_symbol     <- "FIRM_K"
      m3$fit(d)
    },
    regexp = "MarketModel"
  )
})


# ---------------------------------------------------------------------------
# CONTRACT-04 (lenient, unit) — insufficient observations
# ---------------------------------------------------------------------------

test_that("CONTRACT-04: lenient mode on insufficient obs sets is_fitted FALSE + exactly 1 warning", {
  d <- create_degenerate_model_data_insufficient(n_valid = 1)
  m <- MarketModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id        <- "EVT_L"
  m$firm_symbol     <- "FIRM_L"

  warnings_collected <- character(0)
  withCallingHandlers(
    m$fit(d),
    warning = function(w) {
      warnings_collected <<- c(warnings_collected, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(m$is_fitted)
  expect_length(warnings_collected, 1L)
  # After a contract-handled degenerate fit(), abnormal_returns() must NOT
  # emit any additional warning — the contract guarantees exactly one total
  # warning across the full fit() + abnormal_returns() call pair.
  extra_warnings <- character(0)
  withCallingHandlers(
    ar_tbl <- m$abnormal_returns(d),
    warning = function(w) {
      extra_warnings <<- c(extra_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(extra_warnings, 0L)
  expect_true(all(is.na(ar_tbl$abnormal_returns)))
})


# ---------------------------------------------------------------------------
# CONTRACT-04 (lenient, unit) — zero variance
# ---------------------------------------------------------------------------

test_that("CONTRACT-04: lenient mode on zero-variance sets is_fitted FALSE + exactly 1 warning", {
  d <- create_degenerate_model_data_zero_variance()
  m <- MarketModel$new()
  m$degenerate_mode <- "lenient"
  m$event_id        <- "EVT_L"
  m$firm_symbol     <- "FIRM_L"

  warnings_collected <- character(0)
  withCallingHandlers(
    m$fit(d),
    warning = function(w) {
      warnings_collected <<- c(warnings_collected, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(m$is_fitted)
  expect_length(warnings_collected, 1L)
  # After a contract-handled degenerate fit(), abnormal_returns() must NOT
  # emit any additional warning — the contract guarantees exactly one total
  # warning across the full fit() + abnormal_returns() call pair.
  extra_warnings <- character(0)
  withCallingHandlers(
    ar_tbl <- m$abnormal_returns(d),
    warning = function(w) {
      extra_warnings <<- c(extra_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(extra_warnings, 0L)
  expect_true(all(is.na(ar_tbl$abnormal_returns)))
})


# ---------------------------------------------------------------------------
# CONTRACT-04 (pipeline level, WARNING-8) — one warning per degenerate firm
# ---------------------------------------------------------------------------

test_that("CONTRACT-04 (pipeline): run_event_study emits exactly ONE warning per degenerate (event_id, firm_symbol)", {
  # Build a 2-firm task: FIRM_A is valid, FIRM_B will be degenerate.
  # We create a full task and then manually make one firm's estimation window
  # insufficient so the pipeline sees it.

  # Use the mock task factory and then corrupt one firm's data
  task <- create_mock_task(n_firms = 2)
  ps   <- ParameterSet$new(degenerate_handling = "lenient")

  # Prepare the task so data_tbl$data has estimation_window rows
  task <- prepare_event_study(task, ps)

  # Find the row for FIRM_B (index 2) and corrupt its estimation window
  firm_b_row <- which(task$data_tbl$firm_symbol == "FIRM_B")
  if (length(firm_b_row) > 0) {
    d_b <- task$data_tbl$data[[firm_b_row]]
    est_rows <- which(d_b$estimation_window == 1)
    # Keep 0 valid obs — NA out all estimation rows
    d_b$firm_returns[est_rows]  <- NA_real_
    d_b$index_returns[est_rows] <- NA_real_
    task$data_tbl$data[[firm_b_row]] <- d_b
  }

  # Collect ALL warnings emitted during fit_model() + calculate_statistics()
  all_warnings <- character(0)
  withCallingHandlers(
    {
      task <- fit_model(task, ps)
      task <- calculate_statistics(task, ps)
    },
    warning = function(w) {
      all_warnings <<- c(all_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Verify exactly ONE contract-formatted warning names FIRM_B.
  # The contract handler formats as "[firm=<symbol>]".
  firm_b_contract_warnings <- all_warnings[grepl("[firm=FIRM_B]", all_warnings, fixed = TRUE)]
  expect_length(firm_b_contract_warnings, 1L)

  # Strengthen: the TOTAL warning count across the entire pipeline for FIRM_B
  # must also be exactly 1 — i.e. abnormal_returns() must NOT emit a second
  # "not fitted" warning for a degenerate event that fit() already warned about.
  # Any warning containing "FIRM_B" (regardless of format) counts.
  firm_b_all_warnings <- all_warnings[grepl("FIRM_B", all_warnings, fixed = TRUE)]
  # Contract guarantees exactly one warning per degenerate (event_id, firm_symbol)
  # across the full fit_model() + calculate_statistics() pipeline call.
  expect_length(firm_b_all_warnings, 1L)
})


# ---------------------------------------------------------------------------
# CONTRACT-05: regression against committed baseline (no numeric literals)
# ---------------------------------------------------------------------------

test_that("CONTRACT-05: valid-input MarketModel statistics match committed baseline within 1e-8", {
  # Load the frozen pre-refactor baseline created by Plan 01 Task 1.
  # The test MUST NOT hard-code numeric literals — the .rds IS the reference.
  baseline <- readRDS(testthat::test_path("fixtures", "contract05_baseline.rds"))

  # Re-fit MarketModel on the same data as was used for the baseline
  d  <- create_mock_model_data()  # seed=42, n_estimation=120, n_event=11
  m  <- MarketModel$new()
  m$degenerate_mode <- "lenient"
  m$fit(d)

  expect_true(m$is_fitted)
  s <- m$statistics

  expect_equal(s$alpha,              baseline$alpha,      tolerance = 1e-8)
  expect_equal(s$beta,               baseline$beta,       tolerance = 1e-8)
  expect_equal(s$sigma,              baseline$sigma,      tolerance = 1e-8)
  expect_equal(s$degree_of_freedom,  baseline$df)        # exact integer
  expect_equal(s$r2,                 baseline$r2,         tolerance = 1e-8)
  expect_equal(s$f_stat,             baseline$f_stat,     tolerance = 1e-8)
  expect_equal(s$pval_alpha,         baseline$pval_alpha, tolerance = 1e-8)
  expect_equal(s$pval_beta,          baseline$pval_beta,  tolerance = 1e-8)

  # First 5 FEC and AR values.
  # The baseline captures head(ar_tbl$abnormal_returns, 5) — the first 5 rows
  # of the full AR vector, which correspond to the first 5 estimation-window
  # rows (consistent with how the baseline was frozen in Plan 01 Task 1).
  ar_tbl <- m$abnormal_returns(d)

  expect_equal(
    head(s$forecast_error_corrected_sigma, 5),
    baseline$fec_head,
    tolerance = 1e-8
  )
  expect_equal(
    head(ar_tbl$abnormal_returns, 5),
    baseline$ar_head,
    tolerance = 1e-8
  )
})
