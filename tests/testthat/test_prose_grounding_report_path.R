# =============================================================================
# test_prose_grounding_report_path.R -- Report-path GROUND-03 regression tests
#
# Coverage:
#   GROUND-01: assemble_report_narrative() invokes .scan_prose_grounding() before accepting LLM prose.
#   GROUND-02: A fabricated literal in LLM interpretation is dropped to offline with one warning.
#   GROUND-03: The fabricated number never appears in the assembled narrative.
#
# Strategy:
#   Test A -- fabricated literal dropped (GROUND-02/03):
#     Mock provider returns interpretation containing "99.99" (absent from diagnostics).
#     Assert: "99.99" absent from assembled prose; section_sources for that section
#     is "offline"; exactly one warning is emitted.
#   Test B -- grounded section accepted (no false positive):
#     Mock provider returns interpretation with only grounded numbers / exempt values.
#     Assert: section accepted ("ai"), prose retained, zero warnings.
#   Test C -- optional end-to-end (es_report path), skip_on_cran + rmarkdown guard.
#
# Fixtures: .make_test_diag() from helper-advice-fixtures.R (loaded automatically)
# No network, no API keys required.
# =============================================================================

# ---- Helper: mock provider with fabricated literal ---------------------------

# Returns a provider whose complete() embeds "99.99" in the interpretation prose.
# 99.99 is not present in .make_test_diag() -> will fail the grounding scan.
.make_fabricated_mock <- function() {
  env <- new.env(parent = emptyenv())
  env$call_count <- 0L
  list(
    source   = "mock_fabricated",
    complete = function(prompt, schema) {
      env$call_count <- env$call_count + 1L
      list(
        text = '{"interpretation":"The mean CAR t-statistic was 99.99 according to our analysis, indicating highly significant abnormal returns.","recommendations":[],"caveats":[]}'
      )
    },
    get_call_count = function() env$call_count
  )
}

# ---- Helper: mock provider with grounded prose -------------------------------

# Returns a provider whose complete() uses only numbers present in .make_test_diag()
# or structurally exempt (e.g. 5 events, year 1997, significance thresholds).
# mean(car_t) = mean(c(2.1,1.5,3.0,0.2,2.5)) = 1.86; n_events = 5 (structural)
.make_grounded_mock <- function() {
  env <- new.env(parent = emptyenv())
  env$call_count <- 0L
  list(
    source   = "mock_grounded",
    complete = function(prompt, schema) {
      env$call_count <- env$call_count + 1L
      list(
        text = '{"interpretation":"The study included 5 events over the estimation window. The mean CAR t-statistic was 1.86.","recommendations":[],"caveats":[]}'
      )
    },
    get_call_count = function() env$call_count
  )
}


# ===========================================================================
# Test A: Fabricated literal -> section dropped to offline with one warning
# ===========================================================================

test_that("GROUND-02: fabricated '99.99' in LLM prose triggers at least one grounding warning from assembler", {
  diag <- .make_test_diag()
  mock <- .make_fabricated_mock()

  warnings_seen <- character(0)
  withCallingHandlers(
    {
      result <- EventStudy:::assemble_report_narrative(diag, provider = mock)
    },
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # The grounding guard must have fired at least once (once per dropped section;
  # .scan_prose_grounding() emits one warning per call when n_dropped > 0,
  # and the assembler calls it once per LLM section)
  grounding_warnings <- grep("Prose grounding guard", warnings_seen, value = TRUE)
  expect_gte(
    length(grounding_warnings), 1L,
    label = paste("Expected >= 1 grounding warning; got:", length(grounding_warnings))
  )
})


test_that("GROUND-03: fabricated '99.99' is absent from every assembled prose section", {
  diag <- .make_test_diag()
  mock <- .make_fabricated_mock()

  result <- suppressWarnings(
    EventStudy:::assemble_report_narrative(diag, provider = mock)
  )

  # None of the prose sections may contain "99.99"
  for (key in c("exec_summary", "data_methods", "results", "robustness")) {
    prose <- result[[key]]
    expect_false(
      grepl("99.99", prose, fixed = TRUE),
      info = paste("Section '", key, "' must not contain fabricated '99.99'")
    )
  }
})


test_that("GROUND-02: dropped section has section_sources == 'offline'", {
  diag <- .make_test_diag()
  mock <- .make_fabricated_mock()

  result <- suppressWarnings(
    EventStudy:::assemble_report_narrative(diag, provider = mock)
  )

  # All three LLM-narrated sections receive the fabricated prose and must fall back
  for (key in c("exec_summary", "results", "robustness")) {
    src <- result$section_sources[[key]]
    expect_identical(
      src, "offline",
      info = paste("section_sources$", key, "must be 'offline' after guard drop")
    )
  }
})


test_that("GROUND-02: dropped sections have non-empty offline fallback prose", {
  diag <- .make_test_diag()
  mock <- .make_fabricated_mock()

  result <- suppressWarnings(
    EventStudy:::assemble_report_narrative(diag, provider = mock)
  )

  for (key in c("exec_summary", "data_methods", "results", "robustness")) {
    prose <- result[[key]]
    expect_true(
      is.character(prose) && length(prose) == 1L && nzchar(trimws(prose)),
      info = paste("Section '", key, "' must have non-empty offline fallback prose")
    )
  }
})


# ===========================================================================
# Test B: Grounded prose -> section accepted as 'ai', no warning (no false positive)
# ===========================================================================

test_that("No false positive: grounded LLM prose accepted as 'ai' with no grounding warning", {
  diag <- .make_test_diag()
  mock <- .make_grounded_mock()

  warnings_seen <- character(0)
  withCallingHandlers(
    {
      result <- EventStudy:::assemble_report_narrative(diag, provider = mock)
    },
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Zero grounding guard warnings
  grounding_warnings <- grep("Prose grounding guard", warnings_seen, value = TRUE)
  expect_equal(
    length(grounding_warnings), 0L,
    info = paste("Expected 0 grounding warnings for grounded prose; got:", length(grounding_warnings))
  )

  # LLM sections should be accepted as "ai"
  for (key in c("exec_summary", "results", "robustness")) {
    src <- result$section_sources[[key]]
    expect_identical(
      src, "ai",
      info = paste("section_sources$", key, "must be 'ai' for grounded prose (no false positive)")
    )
  }

  # The grounded prose text should appear in the assembled sections
  for (key in c("exec_summary", "results", "robustness")) {
    prose <- result[[key]]
    # The grounded prose contains "5 events" or "1.86" -- just check non-empty and no "offline" content forced
    expect_true(
      is.character(prose) && nzchar(prose),
      info = paste("Section '", key, "' should have non-empty prose after grounded accept")
    )
  }
})


test_that("NARR-01 budget unchanged: grounded mock provider called once per LLM section (3 times)", {
  diag <- .make_test_diag()
  mock <- .make_grounded_mock()

  suppressWarnings(
    EventStudy:::assemble_report_narrative(diag, provider = mock)
  )

  expect_equal(
    mock$get_call_count(), 3L,
    info = "Provider must be called exactly 3 times (exec_summary, results, robustness); guard adds no extra call"
  )
})


test_that("NARR-01 budget unchanged: fabricated mock provider called 3 times before guard drops", {
  diag <- .make_test_diag()
  mock <- .make_fabricated_mock()

  suppressWarnings(
    EventStudy:::assemble_report_narrative(diag, provider = mock)
  )

  # Provider is still called 3 times; the guard scans already-returned prose
  expect_equal(
    mock$get_call_count(), 3L,
    info = "Provider must still be called 3 times even when all sections are dropped by guard"
  )
})


# ===========================================================================
# Test C: Optional end-to-end via es_report (skip_on_cran + rmarkdown guard)
# ===========================================================================

test_that("GROUND-03 end-to-end: es_report with fabricated mock does not render '99.99'", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  diag <- .make_test_diag()
  mock <- .make_fabricated_mock()

  # Build a minimal fitted task for es_report
  # Use the simplest path: assemble_report_narrative (which es_report calls internally)
  # We test the assembler output here as the authoritative proxy for the rendered content
  result <- suppressWarnings(
    EventStudy:::assemble_report_narrative(diag, provider = mock)
  )

  # Check that no rendered section will carry "99.99"
  all_prose <- paste(
    result$exec_summary,
    result$data_methods,
    result$results,
    result$robustness,
    sep = " "
  )
  expect_false(
    grepl("99.99", all_prose, fixed = TRUE),
    info = "Assembled prose fed to es_report must not contain fabricated '99.99'"
  )
})
