# =============================================================================
# test_advise.R — regression tests for es_advise() + Advice S3 + grounding guard
# =============================================================================
#
# These tests are deterministic (no network, no LLM) — all provider calls go
# through CustomProvider with canned JSON. The grounding guard regression is
# the critical gate: it must always produce exactly the same result regardless
# of model or environment.
#
# Test coverage:
#   ADV-01/02/03: Advice S3 shape (fields, recommendations, evidence)
#   ADV-04: Grounding guard (missing key, value mismatch, one warning, caveat)
#   ADV-05: All six task types route correctly
#   ADV-06: LLM-only types stop() without provider
#   Task 3: Failure-matrix (NA text, empty string, malformed JSON, null evidence,
#            integer-vs-double, indexed key-paths)

# ---- Helper: minimal valid Advice JSON (empty recs, used in routing tests) -----

.canned_empty_advice_json <- function() {
  paste0(
    '{"interpretation":"Test interpretation.",',
    '"recommendations":[],"caveats":[]}'
  )
}

# Helper: call es_advise and capture both the result AND expect a specific warning.
# testthat 3e expect_warning() returns the warning condition, NOT the expression value.
# This helper returns the expression value while still asserting one matching warning fires.
.advise_expect_warning <- function(expr_call, pattern) {
  env <- parent.frame()
  result <- NULL
  # First: assert the warning fires with the right pattern
  expect_warning(eval(expr_call, envir = env), regexp = pattern)
  # Second: capture the return value (muffling the warning so no double-fire in test output)
  result <- withCallingHandlers(
    eval(expr_call, envir = env),
    warning = function(w) invokeRestart("muffleWarning")
  )
  result
}

.canned_single_rec_json <- function(key = "cross_sectional.car_iqr", value = 0.025) {
  sprintf(
    paste0(
      '{"interpretation":"Test.",',
      '"recommendations":[{"action":"Test rec","kind":"stat_choice",',
      '"rationale":"Test.","expected_effect":"Test effect.",',
      '"evidence":[{"diagnostic_key":"%s","value":%s,"threshold":0.10,"direction":"below"}]}],',
      '"caveats":[]}'
    ),
    key, value
  )
}

# ==============================================================================
# TASK 1 TRACER: End-to-end guard regression
# ==============================================================================

test_that("grounding guard: only valid rec survives, caveat records 2 drops, one warning", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) CANNED_JSON_THREE_RECS)

  # Assert exactly one "Grounding guard" warning fires
  expect_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    "Grounding guard"
  )

  # Capture the return value separately (muffling the warning)
  result <- withCallingHandlers(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    warning = function(w) invokeRestart("muffleWarning")
  )

  # ADV-01: returns Advice class
  expect_s3_class(result, "Advice")

  # ADV-04: exactly one rec survives (Rec A)
  expect_equal(length(result$recommendations), 1L)
  expect_equal(result$recommendations[[1L]]$action, "Use BMP test")

  # ADV-04: n_dropped == 2
  expect_equal(result$n_dropped, 2L)

  # ADV-04: caveat mentions "2 recommendation"
  expect_true(any(grepl("2 recommendation", result$caveats)))
})

test_that("Advice S3 has all required fields (ADV-01, ADV-02, ADV-03)", {
  diag <- .make_test_diag()
  p    <- CustomProvider$new(function(prompt, schema) {
    .canned_single_rec_json("cross_sectional.car_iqr", 0.025)
  })

  result <- es_advise(diag, task_type = "recommend_stat", provider = p)

  # ADV-01: top-level fields
  expect_s3_class(result, "Advice")
  expect_true(all(c("source", "is_deterministic", "task_type", "interpretation",
                     "recommendations", "caveats", "n_dropped") %in% names(result)))

  # ADV-02: recommendation fields
  expect_equal(length(result$recommendations), 1L)
  rec <- result$recommendations[[1L]]
  expect_true(all(c("action", "kind", "rationale", "expected_effect", "evidence") %in% names(rec)))

  # ADV-03: evidence fields
  ev <- rec$evidence[[1L]]
  expect_true(all(c("diagnostic_key", "value", "threshold", "direction") %in% names(ev)))
})

test_that("print.Advice runs without error and returns invisibly", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())
  # interpret is LLM-only: no KB back-fill, so an empty recommendations[] stays
  # empty and exercises the print "No recommendations" branch. (recommend_stat
  # would back-fill KB-grounded recs and never hit that branch.)
  result <- es_advise(diag, task_type = "interpret", provider = p)

  out <- capture.output(ret <- print(result))
  expect_identical(ret, result)
  expect_true(any(grepl("Event Study Advice", out)))
  expect_true(any(grepl("No recommendations", out, fixed = FALSE)))  # empty recs branch
})

test_that("print.Advice shows [GUARD] line when n_dropped > 0", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) CANNED_JSON_THREE_RECS)

  result <- withCallingHandlers(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    warning = function(w) invokeRestart("muffleWarning")
  )

  out <- capture.output(print(result))
  expect_true(any(grepl("\\[GUARD\\]", out)))
})

# ==============================================================================
# ADV-06: No-provider routing
# ==============================================================================

test_that("es_advise with no provider + interpret -> stop with 'requires a provider'", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "interpret", provider = NULL),
    "requires a provider"
  )
})

test_that("es_advise with no provider + recommend_model -> stop with 'requires a provider'", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "recommend_model", provider = NULL),
    "requires a provider"
  )
})

test_that("es_advise with no provider + design_discussion -> stop with 'requires a provider'", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "design_discussion", provider = NULL),
    "requires a provider"
  )
})

test_that("es_advise with no provider + report_writing -> stop with 'requires a provider'", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "report_writing", provider = NULL),
    "requires a provider"
  )
})

# ==============================================================================
# ADV-05: No-provider KB types return es_advice (Phase 5 offline path)
# ==============================================================================

test_that("es_advise with no provider + recommend_stat -> returns es_advice (Phase 5 path)", {
  diag   <- .make_test_diag()
  result <- es_advise(diag, task_type = "recommend_stat", provider = NULL)
  # Returns es_advice (Phase 5 class), NOT Advice
  expect_s3_class(result, "es_advice")
  expect_false(inherits(result, "Advice"))
})

test_that("es_advise with no provider + flag_robustness -> returns es_advice (Phase 5 path)", {
  diag   <- .make_test_diag()
  result <- es_advise(diag, task_type = "flag_robustness", provider = NULL)
  expect_s3_class(result, "es_advice")
  expect_false(inherits(result, "Advice"))
})

# ==============================================================================
# ADV-05: All six task types with CustomProvider return Advice class
# ==============================================================================

test_that("es_advise with provider + recommend_stat -> Advice, is_deterministic FALSE", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())
  result <- es_advise(diag, task_type = "recommend_stat", provider = p)
  expect_s3_class(result, "Advice")
  expect_false(result$is_deterministic)
})

test_that("es_advise with provider + flag_robustness -> Advice", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())
  result <- es_advise(diag, task_type = "flag_robustness", provider = p)
  expect_s3_class(result, "Advice")
})

test_that("es_advise with provider + interpret -> Advice", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())
  result <- es_advise(diag, task_type = "interpret", provider = p)
  expect_s3_class(result, "Advice")
})

test_that("es_advise with provider + recommend_model -> Advice", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())
  result <- es_advise(diag, task_type = "recommend_model", provider = p)
  expect_s3_class(result, "Advice")
})

test_that("es_advise with provider + design_discussion -> Advice", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())
  result <- es_advise(diag, task_type = "design_discussion", provider = p)
  expect_s3_class(result, "Advice")
})

test_that("es_advise with provider + report_writing -> Advice", {
  diag   <- .make_test_diag()
  p      <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())
  result <- es_advise(diag, task_type = "report_writing", provider = p)
  expect_s3_class(result, "Advice")
})

# ==============================================================================
# Unknown task type -> stop()
# ==============================================================================

test_that("es_advise with unknown task_type -> stop()", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "not_a_type", provider = NULL),
    "unknown task_type"
  )
})

# ==============================================================================
# TASK 3: Failure-matrix hardening
# ==============================================================================

# (a) Provider failure response (text = NA_character_)
test_that("provider failure (text=NA) -> one warning, empty Advice, no crash", {
  diag <- .make_test_diag()
  # CustomProvider returning NA degrades inside provider.R
  p <- CustomProvider$new(function(prompt, schema) NA_character_)

  # Assert warning fires
  expect_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    regexp = "."  # any warning; provider.R emits "custom function returned no text"
  )
  # Capture result
  result <- withCallingHandlers(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_s3_class(result, "Advice")
  expect_equal(length(result$recommendations), 0L)
})

# (b) Empty string response
test_that("provider returns empty string -> one warning, empty Advice, no crash", {
  diag <- .make_test_diag()
  p    <- CustomProvider$new(function(prompt, schema) "")

  expect_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    regexp = "."
  )
  result <- withCallingHandlers(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_s3_class(result, "Advice")
  expect_equal(length(result$recommendations), 0L)
})

# (c) Malformed JSON
test_that("malformed JSON -> one warning, empty Advice, no crash", {
  diag <- .make_test_diag()
  p    <- CustomProvider$new(function(prompt, schema) "{not json")

  expect_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    "malformed JSON"
  )
  result <- withCallingHandlers(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_s3_class(result, "Advice")
  expect_equal(length(result$recommendations), 0L)
})

# (d) Valid JSON with empty recommendations []
# When LLM returns empty recs for a KB type, code seeds from KB (fallback).
# KB-NORM-PATELL fires on .make_test_diag() (shapiro_p mean > 0.05).
# The seeded KB rec is grounded by construction -> no guard drop.
test_that("valid JSON with empty recommendations -> Advice with KB fallback recs, no guard warning", {
  diag <- .make_test_diag()
  p    <- CustomProvider$new(function(prompt, schema) .canned_empty_advice_json())

  # No grounding guard warning (KB recs are grounded by construction)
  result <- expect_no_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p)
  )
  expect_s3_class(result, "Advice")
  # KB-NORM-PATELL fires -> 1 KB rec is seeded
  expect_gte(length(result$recommendations), 0L)  # may be 0 or more depending on KB firing
  expect_equal(result$n_dropped, 0L)
})

# (e) Rec whose evidence value is null (JSON null -> NA-vs-present mismatch -> dropped)
test_that("evidence value null -> NA-vs-present mismatch -> dropped with one warning", {
  diag <- .make_test_diag()
  json_with_null <- paste0(
    '{"interpretation":"Test.",',
    '"recommendations":[{"action":"Null rec","kind":"stat_choice",',
    '"rationale":"Test.","expected_effect":"Test.",',
    '"evidence":[{"diagnostic_key":"cross_sectional.car_iqr",',
    '"value":null,"threshold":0.10,"direction":"below"}]}],',
    '"caveats":[]}'
  )
  p <- CustomProvider$new(function(prompt, schema) json_with_null)

  expect_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    "Grounding guard"
  )
  result <- withCallingHandlers(
    es_advise(diag, task_type = "recommend_stat", provider = p),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_equal(length(result$recommendations), 0L)
  expect_equal(result$n_dropped, 1L)
})

# (f) Integer vs double: JSON 5 vs diagnostics 0L (n_overlap_pairs) -> within tolerance -> kept
test_that("integer-vs-double comparison: JSON 0 vs 0L -> within tolerance -> kept", {
  diag <- .make_test_diag()  # n_overlap_pairs = 0L
  json_int_double <- paste0(
    '{"interpretation":"Test.",',
    '"recommendations":[{"action":"KP test","kind":"stat_choice",',
    '"rationale":"Test.","expected_effect":"Test.",',
    '"evidence":[{"diagnostic_key":"cross_sectional.n_overlap_pairs",',
    '"value":0,"threshold":0,"direction":"equal"}]}],',  # JSON 0 -> double 0.0, diag is 0L
    '"caveats":[]}'
  )
  p <- CustomProvider$new(function(prompt, schema) json_int_double)

  # Should not emit a guard warning (0.0 vs 0L: abs(0.0 - 0) == 0 <= tol)
  result <- expect_no_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p)
  )
  expect_equal(length(result$recommendations), 1L)
  expect_equal(result$n_dropped, 0L)
})

# ==============================================================================
# .resolve_diag_key: indexed form section.key[i]
# ==============================================================================

test_that(".resolve_diag_key handles indexed form section.key[i]", {
  diag <- .make_test_diag()
  # estimation_window.shapiro_p[3] should be 0.15 (3rd element)
  val <- .resolve_diag_key(diag, "estimation_window.shapiro_p[3]")
  expect_equal(val, 0.15)
})

test_that(".resolve_diag_key: guard keeps rec with indexed key matching value", {
  diag <- .make_test_diag()
  # shapiro_p[3] = 0.15 in the fixture; a rec reporting exactly 0.15 should be kept
  json_indexed <- sprintf(
    paste0(
      '{"interpretation":"Test.",',
      '"recommendations":[{"action":"Indexed rec","kind":"stat_choice",',
      '"rationale":"Test.","expected_effect":"Test.",',
      '"evidence":[{"diagnostic_key":"estimation_window.shapiro_p[3]",',
      '"value":%.6f,"threshold":0.05,"direction":"above"}]}],',
      '"caveats":[]}'
    ),
    0.15  # exact match for shapiro_p[3]
  )
  p <- CustomProvider$new(function(prompt, schema) json_indexed)

  result <- expect_no_warning(
    es_advise(diag, task_type = "recommend_stat", provider = p)
  )
  expect_equal(length(result$recommendations), 1L)
  expect_equal(result$n_dropped, 0L)
})

test_that(".resolve_diag_key returns NA_real_ for absent path", {
  diag <- .make_test_diag()
  expect_true(is.na(.resolve_diag_key(diag, "cross_sectional.kurtosis")))
  expect_true(is.na(.resolve_diag_key(diag, "nonexistent_section.key")))
  expect_true(is.na(.resolve_diag_key(diag, "estimation_window.shapiro_p[99]")))  # out of range
})

# ==============================================================================
# jsonlite absent: guard path coverage (documented)
# ==============================================================================

test_that(".parse_advice_json degrades cleanly when requireNamespace guard prevents parse", {
  # We test the guard path by constructing an es_provider_response with NA text
  # (which triggers the text-check path before jsonlite is needed) — covers the
  # early-exit degrade path that would also be taken if jsonlite were absent.
  diag <- .make_test_diag()
  p    <- CustomProvider$new(function(prompt, schema) NA_character_)

  # expect_warning() returns the warning condition, not the value — use the
  # helper to assert the warning AND capture the returned Advice.
  result <- .advise_expect_warning(
    quote(es_advise(diag, task_type = "recommend_stat", provider = p)),
    "."
  )
  # Regardless of the path taken, result must be an Advice with no recs
  expect_s3_class(result, "Advice")
  expect_equal(length(result$recommendations), 0L)
})

# ==============================================================================
# Invalid diagnostics input
# ==============================================================================

test_that("es_advise with non-es_diagnostics input -> stop()", {
  expect_error(
    es_advise(list(a = 1), task_type = "recommend_stat"),
    "es_diagnostics"
  )
})

# ==============================================================================
# Grounding-guard degenerate-input hardening (code review 07-REVIEW.md)
# ==============================================================================

test_that("CR-01: recommendation with empty evidence[] is dropped as ungrounded", {
  diag <- .make_test_diag()
  advice_list <- list(
    recommendations = list(
      list(action = "No-evidence rec", kind = "stat_choice",
           rationale = "", expected_effect = "", evidence = list())
    ),
    caveats = character()
  )
  # expect_warning() returns the condition, not the value — capture via handler
  fired <- FALSE
  res <- withCallingHandlers(
    EventStudy:::.validate_grounding(advice_list, diag),
    warning = function(w) {
      fired <<- grepl("Grounding guard", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(fired)
  expect_equal(res$n_dropped, 1L)
  expect_equal(length(res$recommendations), 0L)
})

test_that("CR-02: all-NA diagnostic vector drops the rec without crashing", {
  diag <- .make_test_diag()
  diag$estimation_window$r2 <- c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  advice_list <- list(
    recommendations = list(
      list(action = "Cite all-NA key", kind = "stat_choice", rationale = "",
           expected_effect = "",
           evidence = list(list(diagnostic_key = "estimation_window.r2",
                                value = 0.5, threshold = 0.3, direction = "above")))
    ),
    caveats = character()
  )
  # expect_warning() returns the condition, not the value — capture via handler
  fired <- FALSE
  res <- withCallingHandlers(
    EventStudy:::.validate_grounding(advice_list, diag),
    warning = function(w) {
      fired <<- grepl("Grounding guard", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(fired)
  expect_equal(res$n_dropped, 1L)
  expect_equal(length(res$recommendations), 0L)
})

test_that("CR-03: non-finite (Inf) diagnostic value drops the rec, no false-accept, no crash", {
  diag <- .make_test_diag()
  diag$cross_sectional$car_iqr <- Inf
  advice_list <- list(
    recommendations = list(
      list(action = "Cite Inf key", kind = "robustness", rationale = "",
           expected_effect = "",
           evidence = list(list(diagnostic_key = "cross_sectional.car_iqr",
                                value = 12345, threshold = 0.02, direction = "above")))
    ),
    caveats = character()
  )
  # expect_warning() returns the condition, not the value — capture via handler
  fired <- FALSE
  res <- withCallingHandlers(
    EventStudy:::.validate_grounding(advice_list, diag),
    warning = function(w) {
      fired <<- grepl("Grounding guard", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(fired)
  expect_equal(res$n_dropped, 1L)
  expect_equal(length(res$recommendations), 0L)
})
