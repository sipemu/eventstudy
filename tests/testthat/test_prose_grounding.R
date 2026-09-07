# =============================================================================
# test_prose_grounding.R — GROUND-01/02/03 regression tests
#
# Covers:
#   - GROUND-01: .extract_numeric_literals() contract (spike block)
#   - GROUND-01: .build_prose_value_registry() construction from es_diagnostics
#   - GROUND-01: .is_grounded_literal() exemption + tolerance logic
#   - GROUND-02: .scan_prose_grounding() drop-and-keep with single warning
#   - GROUND-03: report-path regression: fabricated literal never rendered
# =============================================================================

# ---------------------------------------------------------------------------
# SPIKE BLOCK — Task 1: .extract_numeric_literals() contract
# Defines the extractor contract; tests start RED until Task 2 implements
# the internals.
# ---------------------------------------------------------------------------

test_that(".extract_numeric_literals extracts standard decimal literal", {
  # "The median CAR t-statistic was 2.35 (p < 0.001)."
  vals <- EventStudy:::.extract_numeric_literals(
    "The median CAR t-statistic was 2.35 (p < 0.001)."
  )
  expect_true(2.35 %in% vals)
  expect_true(0.001 %in% vals)
})

test_that(".extract_numeric_literals extracts integer and short decimal", {
  # "Across 42 events the mean R2 was 0.87."
  vals <- EventStudy:::.extract_numeric_literals(
    "Across 42 events the mean R2 was 0.87."
  )
  expect_true(42 %in% vals)
  expect_true(0.87 %in% vals)
})

test_that(".extract_numeric_literals extracts citation year (not exempt at extractor level)", {
  # "MacKinlay (1997) shows..." — 1997 IS extracted; exemption is scanner's job
  vals <- EventStudy:::.extract_numeric_literals("MacKinlay (1997) shows the market model is standard.")
  expect_true(1997 %in% vals)
})

test_that(".extract_numeric_literals extracts percentage numeral", {
  # "returns fell 12.5% over the window"
  vals <- EventStudy:::.extract_numeric_literals("returns fell 12.5% over the window")
  expect_true(12.5 %in% vals)
})

test_that(".extract_numeric_literals extracts two literals from range prose", {
  # "between 0.30 and 0.70"
  vals <- EventStudy:::.extract_numeric_literals("The R-squared varied between 0.30 and 0.70.")
  expect_true(0.30 %in% vals)
  expect_true(0.70 %in% vals)
})

test_that(".extract_numeric_literals strips thousands separator and returns correct value", {
  # "1,234.56 observations" -> 1234.56
  vals <- EventStudy:::.extract_numeric_literals("The dataset had 1,234.56 observations.")
  expect_true(any(abs(vals - 1234.56) < 1e-9))
})

test_that(".extract_numeric_literals handles scientific notation", {
  # "p-value was 2.5e-4" -> 2.5e-4 = 0.00025
  vals <- EventStudy:::.extract_numeric_literals("The p-value was 2.5e-4 for this event.")
  # Should contain the scientific notation value
  expect_true(length(vals) >= 1L)
  expect_true(any(abs(vals - 2.5e-4) < 1e-10))
})

test_that(".extract_numeric_literals returns numeric (not character)", {
  vals <- EventStudy:::.extract_numeric_literals("The CAR was 3.14.")
  expect_type(vals, "double")
})

test_that(".extract_numeric_literals returns empty numeric on prose with no numbers", {
  vals <- EventStudy:::.extract_numeric_literals("No numbers here at all.")
  expect_length(vals, 0L)
})

# ---------------------------------------------------------------------------
# Task 2 GREEN block — .build_prose_value_registry()
# ---------------------------------------------------------------------------

test_that(".build_prose_value_registry returns list with scalars and structural_ints", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_type(reg, "list")
  expect_true("scalars" %in% names(reg))
  expect_true("structural_ints" %in% names(reg))
})

test_that(".build_prose_value_registry scalars are all finite", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_true(all(is.finite(reg$scalars)))
})

test_that(".build_prose_value_registry summarises estimation_window r2 to mean", {
  diag <- .make_test_diag()
  # r2 = c(0.4, 0.5, 0.3, 0.6, 0.45) -> mean = 0.45
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_true(any(abs(reg$scalars - 0.45) < 1e-9))
})

test_that(".build_prose_value_registry structural_ints contains n_events_total", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  # n_events_total = 5L
  expect_true(5L %in% reg$structural_ints)
})

# ---------------------------------------------------------------------------
# Task 2 GREEN block — .is_grounded_literal() exemptions
# ---------------------------------------------------------------------------

test_that(".is_grounded_literal: structural integer is exempt", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  # 5 is n_events_total — always exempt
  expect_true(
    EventStudy:::.is_grounded_literal(5, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: citation year 1997 is exempt", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_true(
    EventStudy:::.is_grounded_literal(1997, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: significance constant 0.05 is exempt", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_true(
    EventStudy:::.is_grounded_literal(0.05, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: significance constant 0.01 is exempt", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_true(
    EventStudy:::.is_grounded_literal(0.01, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: significance constant 0.10 is exempt", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_true(
    EventStudy:::.is_grounded_literal(0.10, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: significance constant 0.001 is exempt", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  expect_true(
    EventStudy:::.is_grounded_literal(0.001, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: exact registry value is grounded (tolerance match)", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  # car_iqr = 0.025 is a scalar in the registry
  expect_true(
    EventStudy:::.is_grounded_literal(0.025, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: rounded value 2.35 grounded against 2.3456 (rounding-aware KEEP)", {
  # 2.3456 rounded to 2 decimal places = 2.35 — must be KEPT
  scalars        <- c(2.3456)
  structural_ints <- integer(0)
  expect_true(
    EventStudy:::.is_grounded_literal(2.35, scalars, structural_ints, 1e-6, 1e-4)
  )
})

test_that(".is_grounded_literal: fabricated literal 99.99 is NOT grounded", {
  diag <- .make_test_diag()
  reg  <- EventStudy:::.build_prose_value_registry(diag)
  # 99.99 is absent from diagnostics and is not a year/constant/structural
  expect_false(
    EventStudy:::.is_grounded_literal(99.99, reg$scalars, reg$structural_ints, 1e-6, 1e-4)
  )
})

# ---------------------------------------------------------------------------
# Task 2 GREEN block — .scan_prose_grounding() single-warning discipline
# ---------------------------------------------------------------------------

test_that(".scan_prose_grounding: fabricated literal drops section with exactly one warning", {
  diag <- .make_test_diag()
  prose_fields <- list(
    exec_summary = "The CAR was 99.99 according to the analysis.",
    data_methods = "We used 5 events in the estimation window."
  )
  result <- NULL
  expect_warning(
    result <- EventStudy:::.scan_prose_grounding(prose_fields, diag),
    regexp = "Prose grounding guard",
    fixed  = FALSE
  )
  # Fabricated section must be dropped (empty string)
  expect_equal(result$sections$exec_summary, "")
  # n_dropped must be 1
  expect_equal(result$n_dropped, 1L)
})

test_that(".scan_prose_grounding: exactly ONE warning even when two sections are fabricated", {
  diag <- .make_test_diag()
  prose_fields <- list(
    exec_summary = "The CAR was 99.99.",
    data_methods = "The sigma was 88.88.",
    results      = "We used 5 events."  # grounded (5 is structural_int)
  )
  warnings_seen <- character(0)
  withCallingHandlers(
    {
      result <- EventStudy:::.scan_prose_grounding(prose_fields, diag)
    },
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(warnings_seen), 1L)
  expect_equal(result$n_dropped, 2L)
})

test_that(".scan_prose_grounding: clean prose passes with zero warnings and no drops", {
  diag <- .make_test_diag()
  # Build prose using only values actually in the registry
  # mean r2 = 0.45; n_events = 5 (structural)
  prose_fields <- list(
    exec_summary = "The study included 5 events.",
    data_methods = "The mean R-squared was 0.45."
  )
  expect_no_warning(
    result <- EventStudy:::.scan_prose_grounding(prose_fields, diag)
  )
  expect_equal(result$n_dropped, 0L)
  expect_equal(result$sections$exec_summary, prose_fields$exec_summary)
  expect_equal(result$sections$data_methods, prose_fields$data_methods)
})

test_that(".scan_prose_grounding: rounded value 2.35 for actual 2.3456 is KEPT (rounding-aware)", {
  # Build a minimal diag with car_t mean of 2.3456
  diag <- .make_test_diag()
  # Override event_window car_t to produce mean 2.3456
  diag$event_window$car_t <- c(2.3456, 2.3456, 2.3456, 2.3456, 2.3456)

  prose_fields <- list(
    results = "The median CAR t-statistic was 2.35 (p < 0.05).",
    data_methods = "We used 5 events."
  )
  # 0.05 is exempt; 5 is structural; 2.35 rounds from 2.3456 -> KEEP
  expect_no_warning(
    result <- EventStudy:::.scan_prose_grounding(prose_fields, diag)
  )
  expect_equal(result$n_dropped, 0L)
  expect_equal(result$sections$results, prose_fields$results)
})

test_that(".scan_prose_grounding: year 1997 exempt (no false positive)", {
  diag <- .make_test_diag()
  prose_fields <- list(
    exec_summary = "As shown by MacKinlay (1997) and using 5 events."
  )
  expect_no_warning(
    result <- EventStudy:::.scan_prose_grounding(prose_fields, diag)
  )
  expect_equal(result$n_dropped, 0L)
})

test_that(".scan_prose_grounding: empty prose field passes through unchanged", {
  diag <- .make_test_diag()
  prose_fields <- list(
    exec_summary = "",
    data_methods = "We used 5 events."
  )
  expect_no_warning(
    result <- EventStudy:::.scan_prose_grounding(prose_fields, diag)
  )
  expect_equal(result$sections$exec_summary, "")
  expect_equal(result$n_dropped, 0L)
})

# ---------------------------------------------------------------------------
# GROUND-03: Report-path regression — fabricated number never rendered
# ---------------------------------------------------------------------------

test_that("GROUND-03: fabricated literal absent from diagnostics is never present in kept sections", {
  diag <- .make_test_diag()
  # Simulate OfflineNarrative-style prose_fields where one section is fabricated
  prose_fields <- list(
    exec_summary = "The CAR was 99.99 in this analysis.",
    data_methods = "We analysed 5 events over the estimation window.",
    results      = "Mean R-squared was 0.45 across events.",
    robustness   = "As shown by MacKinlay (1997), the study covered 5 events."
  )
  warnings_seen <- character(0)
  withCallingHandlers(
    {
      result <- EventStudy:::.scan_prose_grounding(prose_fields, diag)
    },
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # (a) Exactly one warning
  expect_equal(length(warnings_seen), 1L)
  expect_match(warnings_seen[[1L]], "Prose grounding guard")

  # (b) Fabricated section is dropped (empty string)
  expect_equal(result$sections$exec_summary, "")

  # (c) The string "99.99" does NOT appear in any kept section
  kept_text <- paste(unlist(result$sections), collapse = " ")
  expect_false(grepl("99.99", kept_text, fixed = TRUE))

  # (d) n_dropped = 1
  expect_equal(result$n_dropped, 1L)
})

test_that("GROUND-03: all-grounded prose passes with zero warnings — no false positives (Pitfall 1)", {
  diag <- .make_test_diag()
  # Build prose entirely from real diag values
  # mean r2=0.45, mean sigma=0.01, n_events=5, car_iqr=0.025, year exemption
  prose_fields <- list(
    exec_summary = "The analysis covered 5 events with a mean R-squared of 0.45.",
    data_methods = "Mean residual sigma was 0.01 across the 5 estimation windows.",
    results      = "The cross-sectional IQR of CARs was 0.025.",
    robustness   = "As in MacKinlay (1997), threshold 0.05 was used for the 5-event set."
  )
  expect_no_warning(
    result <- EventStudy:::.scan_prose_grounding(prose_fields, diag)
  )
  expect_equal(result$n_dropped, 0L)
  # All sections retained
  for (nm in names(prose_fields)) {
    expect_equal(result$sections[[nm]], prose_fields[[nm]])
  }
})
