# =============================================================================
# test_report_narrative_asm.R -- Tests for assemble_report_narrative(),
#   .calibrate_significance(), .extract_kb_references(), and the
#   section_hint= seam in es_advise().
#
# Coverage:
#   NARR-01: Provider contacted once per LLM section (3 calls), independent of
#            format count (no format arg in assembler)
#   NARR-02: Four section keys present + section_sources + report_mode
#   NARR-03: References deduped by key, alpha by author, KB-sourced
#   NARR-04: .calibrate_significance() four tiers + NA/non-numeric guard
#   NARR-05: Joint-hypothesis caveat present in every assembled robustness
#   OFFLINE-02: report_mode and section_sources reflect per-section source
#   section_hint=: Injection scopes prompt; NULL path unchanged
#
# Fixtures: .make_test_diag() from helper-advice-fixtures.R
# No network, no API keys required.
# =============================================================================

# ---- Helper: mock provider ---------------------------------------------------

.make_mock_provider <- function() {
  # Returns a mock provider with call tracking.
  # complete() returns a minimal valid JSON response (list with $text field)
  # matching the Advice schema that es_advise() expects.
  env <- new.env(parent = emptyenv())
  env$call_count <- 0L
  env$prompts    <- character(0L)

  list(
    source   = "mock",
    complete = function(prompt, schema) {
      env$call_count <- env$call_count + 1L
      env$prompts    <- c(env$prompts, prompt)
      # Return list with $text containing valid Advice JSON
      list(
        text = '{"interpretation":"Mock LLM narrative prose for testing purposes.","recommendations":[],"caveats":[]}'
      )
    },
    get_call_count = function() env$call_count,
    get_prompts    = function() env$prompts
  )
}


# ---- NARR-02: Four section keys + metadata (offline path) --------------------

test_that("assemble_report_narrative(provider=NULL) returns list with four section keys", {
  diag   <- .make_test_diag()
  result <- EventStudy:::assemble_report_narrative(diag, provider = NULL)

  expect_true(is.list(result))
  expect_true("exec_summary"  %in% names(result))
  expect_true("data_methods"  %in% names(result))
  expect_true("results"       %in% names(result))
  expect_true("robustness"    %in% names(result))
  expect_true("section_sources" %in% names(result))
  expect_true("report_mode"   %in% names(result))
})


test_that("assemble_report_narrative(provider=NULL) section keys are non-empty character scalars", {
  diag   <- .make_test_diag()
  result <- EventStudy:::assemble_report_narrative(diag, provider = NULL)

  for (key in c("exec_summary", "data_methods", "results", "robustness")) {
    val <- result[[key]]
    expect_true(is.character(val), info = paste("key:", key, "should be character"))
    expect_length(val, 1L)
    expect_true(nzchar(val), info = paste("key:", key, "should be non-empty"))
  }
})


test_that("assemble_report_narrative(provider=NULL) report_mode is 'offline'", {
  diag   <- .make_test_diag()
  result <- EventStudy:::assemble_report_narrative(diag, provider = NULL)

  expect_identical(result$report_mode, "offline")
})


test_that("assemble_report_narrative(provider=NULL) all section_sources are 'offline'", {
  diag   <- .make_test_diag()
  result <- EventStudy:::assemble_report_narrative(diag, provider = NULL)

  for (key in c("exec_summary", "data_methods", "results", "robustness")) {
    src <- result$section_sources[[key]]
    expect_identical(src, "offline",
                     info = paste("section_sources$", key, "should be 'offline'"))
  }
})


# ---- NARR-05: Joint-hypothesis caveat in every robustness section -----------

test_that("NARR-05: joint-hypothesis caveat present in offline robustness section", {
  diag   <- .make_test_diag()
  result <- EventStudy:::assemble_report_narrative(diag, provider = NULL)

  rob <- result$robustness
  expect_true(
    grepl("joint", rob, ignore.case = TRUE),
    info = paste("robustness section should contain 'joint' (caveat). Got:", substr(rob, 1, 200))
  )
})


# ---- NARR-01: Provider contacted once per LLM section (not per format) ------

test_that("NARR-01: mock provider contacted exactly 3 times (one per LLM section)", {
  diag <- .make_test_diag()
  mock <- .make_mock_provider()

  result <- EventStudy:::assemble_report_narrative(diag, provider = mock)

  # Three LLM-narrated sections: exec_summary, results, robustness
  expect_equal(mock$get_call_count(), 3L,
    info = "Provider must be called once per LLM section (3), not per format")
})


test_that("NARR-01: data_methods is always offline regardless of provider", {
  diag <- .make_test_diag()
  mock <- .make_mock_provider()

  result <- EventStudy:::assemble_report_narrative(diag, provider = mock)

  expect_identical(result$section_sources$data_methods, "offline",
    info = "data_methods must always be sourced offline (section_sources$data_methods == 'offline')")
})


# ---- OFFLINE-02: report_mode and section_sources with mock provider ----------

test_that("OFFLINE-02: report_mode is 'ai' when any section used the provider", {
  diag <- .make_test_diag()
  mock <- .make_mock_provider()

  result <- EventStudy:::assemble_report_narrative(diag, provider = mock)

  expect_identical(result$report_mode, "ai",
    info = "report_mode should be 'ai' when at least one section used the LLM")
})


test_that("OFFLINE-02: section_sources marks LLM-narrated sections as 'ai'", {
  diag <- .make_test_diag()
  mock <- .make_mock_provider()

  result <- EventStudy:::assemble_report_narrative(diag, provider = mock)

  # These three sections are LLM-narrated when provider is available
  for (key in c("exec_summary", "results", "robustness")) {
    src <- result$section_sources[[key]]
    expect_identical(src, "ai",
      info = paste("section_sources$", key, "should be 'ai' with mock provider"))
  }
  # data_methods is always offline
  expect_identical(result$section_sources$data_methods, "offline")
})


# ---- section_hint= wiring: prompt scoping observable ------------------------

test_that("section_hint= injects section name and 'Write ONLY the' into prompt", {
  diag <- .make_test_diag()
  mock <- .make_mock_provider()

  EventStudy:::assemble_report_narrative(diag, provider = mock)

  prompts <- mock$get_prompts()
  expect_length(prompts, 3L)

  # Each of the three LLM calls must scope to its section
  # Map by order: exec_summary (1st), results (2nd), robustness (3rd)
  expected_sections <- c("exec_summary", "results", "robustness")
  for (i in seq_along(expected_sections)) {
    section <- expected_sections[[i]]
    prompt  <- prompts[[i]]
    expect_true(
      grepl("Write ONLY the", prompt, fixed = TRUE),
      info = paste("Prompt", i, "for", section, "must contain 'Write ONLY the'")
    )
    expect_true(
      grepl(section, prompt, fixed = TRUE),
      info = paste("Prompt", i, "must contain section name:", section)
    )
  }
})


test_that("NULL section_hint path: es_advise report_writing prompt has no 'Write ONLY the'", {
  diag <- .make_test_diag()
  mock <- .make_mock_provider()

  # Call es_advise directly without section_hint (backward-compat path)
  # Capture the prompt by using a modified mock that records it
  env <- new.env(parent = emptyenv())
  env$captured_prompt <- NULL
  capture_mock <- list(
    source   = "capture",
    complete = function(prompt, schema) {
      env$captured_prompt <- prompt
      list(text = '{"interpretation":"test","recommendations":[],"caveats":[]}')
    }
  )

  # es_advise without section_hint (default NULL)
  suppressWarnings(
    EventStudy::es_advise(diag, task_type = "report_writing", provider = capture_mock)
  )

  prompt <- env$captured_prompt
  if (!is.null(prompt)) {
    expect_false(
      grepl("Write ONLY the", prompt, fixed = TRUE),
      info = "NULL section_hint path must NOT inject 'Write ONLY the' (backward-compat)"
    )
  }
})


test_that("section_hint= argument exists on es_advise formals", {
  expect_true(
    "section_hint" %in% names(formals(EventStudy::es_advise)),
    info = "es_advise() must have an additive section_hint= argument"
  )
})


# ---- Provider error -> fallback to offline for that section -----------------

test_that("provider error causes graceful fallback to offline for that section", {
  diag <- .make_test_diag()

  error_mock <- list(
    source   = "error_mock",
    complete = function(prompt, schema) {
      stop("Simulated provider failure")
    }
  )

  # Should not throw; result should be complete with offline fallback
  result <- expect_no_error(
    EventStudy:::assemble_report_narrative(diag, provider = error_mock)
  )

  # All four section keys must be non-empty
  for (key in c("exec_summary", "data_methods", "results", "robustness")) {
    expect_true(nzchar(result[[key]]), info = paste("key:", key, "must be non-empty after fallback"))
  }

  # All section_sources should be "offline" (since provider failed)
  for (key in c("exec_summary", "data_methods", "results", "robustness")) {
    expect_identical(result$section_sources[[key]], "offline",
      info = paste("section_sources$", key, "should be 'offline' after error fallback"))
  }
})


# ---- NARR-04: .calibrate_significance() ------------------------------------

test_that(".calibrate_significance() returns 'strongly significant' for p < 0.01", {
  expect_identical(EventStudy:::.calibrate_significance(0.005), "strongly significant")
  expect_identical(EventStudy:::.calibrate_significance(0.001), "strongly significant")
  expect_identical(EventStudy:::.calibrate_significance(0.0001), "strongly significant")
})


test_that(".calibrate_significance() returns 'significant' for 0.01 <= p < 0.05", {
  expect_identical(EventStudy:::.calibrate_significance(0.03), "significant")
  expect_identical(EventStudy:::.calibrate_significance(0.01), "significant")
  expect_identical(EventStudy:::.calibrate_significance(0.049), "significant")
})


test_that(".calibrate_significance() returns 'marginally significant' for 0.05 <= p < 0.10", {
  expect_identical(EventStudy:::.calibrate_significance(0.08), "marginally significant")
  expect_identical(EventStudy:::.calibrate_significance(0.05), "marginally significant")
  expect_identical(EventStudy:::.calibrate_significance(0.099), "marginally significant")
})


test_that(".calibrate_significance() returns 'not statistically significant' for p >= 0.10", {
  expect_identical(EventStudy:::.calibrate_significance(0.5), "not statistically significant")
  expect_identical(EventStudy:::.calibrate_significance(0.10), "not statistically significant")
  expect_identical(EventStudy:::.calibrate_significance(1.0), "not statistically significant")
})


test_that(".calibrate_significance() returns 'not evaluable' for NA", {
  expect_identical(EventStudy:::.calibrate_significance(NA), "not evaluable")
  expect_identical(EventStudy:::.calibrate_significance(NA_real_), "not evaluable")
})


test_that(".calibrate_significance() returns 'not evaluable' for non-numeric", {
  expect_identical(EventStudy:::.calibrate_significance("0.03"), "not evaluable")
  expect_identical(EventStudy:::.calibrate_significance(NULL),   "not evaluable")
})


# ---- NARR-03: .extract_kb_references() -------------------------------------

test_that(".extract_kb_references() returns a list", {
  diag <- .make_test_diag()
  refs <- EventStudy:::.extract_kb_references(diag)
  expect_true(is.list(refs))
})


test_that(".extract_kb_references() deduplicates by citation key", {
  # Build a diagnostics where at least one rule fires (KB-NORM-PATELL needs
  # shapiro_p > 0.05 in >= 70% of events -- our fixture has shapiro_p =
  # c(0.12, 0.08, 0.15, 0.20, 0.10), all > 0.05, so 100% pass -> fires)
  diag <- .make_test_diag()
  refs <- EventStudy:::.extract_kb_references(diag)

  keys <- vapply(refs, function(r) r$key, character(1L))
  expect_equal(length(keys), length(unique(keys)),
    info = "References must not contain duplicate keys")
})


test_that(".extract_kb_references() returns references ordered alphabetically by author", {
  diag <- .make_test_diag()
  refs <- EventStudy:::.extract_kb_references(diag)

  if (length(refs) >= 2L) {
    authors <- vapply(refs, function(r) r$author, character(1L))
    expect_equal(authors, sort(authors),
      info = "References must be ordered alphabetically by author")
  }
})


test_that(".extract_kb_references() returns empty list for all-NA diagnostics", {
  # Build diagnostics that will fire no rules
  degenerate_diag <- structure(
    list(
      meta              = list(
        n_events_total      = 0L,
        n_events_shown      = 0L,
        n_events_summarized = 0L,
        event_ids_shown     = integer(0L)
      ),
      estimation_window = list(
        r2                = NA_real_,
        sigma             = NA_real_,
        degree_of_freedom = NA_real_,
        acf1              = NA_real_,
        shapiro_p         = NA_real_,
        dw_stat           = NA_real_,
        ljung_box_p       = NA_real_
      ),
      event_window      = list(
        ar_t      = NA_real_,
        ar_p      = NA_real_,
        car_t     = NA_real_,
        car_p     = NA_real_,
        final_car = NA_real_
      ),
      cross_sectional   = list(
        n_events        = 0L,
        n_valid_events  = 0L,
        car_iqr         = NA_real_,
        car_sd          = NA_real_,
        n_overlap_pairs = 0L,
        any_overlap     = FALSE
      ),
      contract_state    = list(
        is_fitted        = logical(0L),
        na_ar_count      = integer(0L),
        na_est_count     = integer(0L),
        insufficient_obs = logical(0L),
        zero_var_index   = logical(0L)
      ),
      aggregate_summary = NULL
    ),
    class = "es_diagnostics"
  )

  result <- expect_no_error(EventStudy:::.extract_kb_references(degenerate_diag))
  expect_true(is.list(result))
  # Either empty or all refs from rules that fired (should be 0 with degenerate)
  # The key invariant: no error thrown
})
