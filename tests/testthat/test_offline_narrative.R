# =============================================================================
# test_offline_narrative.R — OFFLINE-01 tests for .build_offline_narrative()
#   and es_advise() routing surgery (Phase 17, Plan 01)
# =============================================================================
#
# Coverage:
#   OFFLINE-01: provider=NULL + report_writing -> OfflineNarrative (no stop())
#   OFFLINE-01: all four section keys present and non-empty
#   OFFLINE-01: section prose is a character scalar
#   REGRESSION: the three remaining LLM-only types still stop() without provider
#   REPORT-03: generate_report() end-to-end offline render (Task 3 addition)
#
# Fixtures: .make_test_diag() from helper-advice-fixtures.R
# No network, no LLM provider, no API keys required.

# ---- Task 1 tests -----------------------------------------------------------

test_that("es_advise(report_writing, provider=NULL) returns OfflineNarrative, no stop()", {
  diag <- .make_test_diag()

  result <- es_advise(diag, task_type = "report_writing", provider = NULL)

  expect_s3_class(result, "OfflineNarrative")
})


test_that("OfflineNarrative has all four required section keys", {
  diag   <- .make_test_diag()
  result <- es_advise(diag, task_type = "report_writing", provider = NULL)

  expect_true(all(c("exec_summary", "data_methods", "results", "robustness") %in% names(result)))
})


test_that("OfflineNarrative section keys are non-empty character scalars", {
  diag   <- .make_test_diag()
  result <- es_advise(diag, task_type = "report_writing", provider = NULL)

  for (key in c("exec_summary", "data_methods", "results", "robustness")) {
    val <- result[[key]]
    expect_true(is.character(val), info = paste("key:", key, "should be character"))
    expect_length(val, 1L)
    expect_true(nzchar(val), info = paste("key:", key, "should be non-empty"))
  }
})


test_that("OfflineNarrative source and is_deterministic fields are correct", {
  diag   <- .make_test_diag()
  result <- es_advise(diag, task_type = "report_writing", provider = NULL)

  expect_identical(result$source, "offline_kb")
  expect_true(isTRUE(result$is_deterministic))
})


# ---- REGRESSION: three remaining LLM-only types still stop() ----------------

test_that("es_advise(interpret, provider=NULL) still stop()s with ADV-06 message", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "interpret", provider = NULL),
    regexp = "requires a provider",
    fixed  = FALSE
  )
})


test_that("es_advise(recommend_model, provider=NULL) still stop()s with ADV-06 message", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "recommend_model", provider = NULL),
    regexp = "requires a provider",
    fixed  = FALSE
  )
})


test_that("es_advise(design_discussion, provider=NULL) still stop()s with ADV-06 message", {
  diag <- .make_test_diag()
  expect_error(
    es_advise(diag, task_type = "design_discussion", provider = NULL),
    regexp = "requires a provider",
    fixed  = FALSE
  )
})


# ---- LLM_ONLY_TYPES / KB_TYPES membership assertions -----------------------

test_that("report_writing is NOT in LLM_ONLY_TYPES (OFFLINE-01 acceptance)", {
  # Access via package namespace
  llm_only <- getFromNamespace("LLM_ONLY_TYPES", "EventStudy")
  expect_false("report_writing" %in% llm_only)
})


test_that("report_writing IS in KB_TYPES (OFFLINE-01 acceptance)", {
  kb_types <- getFromNamespace("KB_TYPES", "EventStudy")
  expect_true("report_writing" %in% kb_types)
})


test_that("LLM_ONLY_TYPES still contains interpret, recommend_model, design_discussion", {
  llm_only <- getFromNamespace("LLM_ONLY_TYPES", "EventStudy")
  expect_true("interpret"          %in% llm_only)
  expect_true("recommend_model"    %in% llm_only)
  expect_true("design_discussion"  %in% llm_only)
})


# ---- Task 3: End-to-end offline report render (tracer proof) ----------------

test_that("full offline pipeline renders a non-empty HTML file (tracer proof)", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  # Build a fitted task via the standard mock fixture
  task <- create_fitted_mock_task()

  # Step 1: harvest diagnostics
  diag <- es_diagnostics(task)

  # Step 2: offline report_writing narrative
  narrative_obj <- es_advise(diag, task_type = "report_writing", provider = NULL)
  expect_s3_class(narrative_obj, "OfflineNarrative")

  # Step 3: coerce OfflineNarrative to a plain named list of the four section strings
  #         (what generate_report() accepts as narrative=)
  narrative_list <- list(
    exec_summary = narrative_obj$exec_summary,
    data_methods = narrative_obj$data_methods,
    results      = narrative_obj$results,
    robustness   = narrative_obj$robustness
  )

  # Step 4: render via generate_report()
  tmp_file <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_file), add = TRUE)

  result <- generate_report(
    task,
    narrative   = narrative_list,
    output_file = tmp_file,
    format      = "html",
    sections    = c("summary", "appendix")
  )

  # Step 5: assert output
  expect_true(file.exists(result))
  expect_gt(file.size(result), 0L)

  # Check the rendered HTML contains at least one offline section string
  html_lines <- readLines(result, warn = FALSE)
  html_text  <- paste(html_lines, collapse = " ")
  # Any non-empty section string should appear somewhere in the HTML
  found_any <- any(vapply(narrative_list, function(s) {
    nzchar(s) && grepl(substr(s, 1L, 40L), html_text, fixed = TRUE)
  }, logical(1L)))
  expect_true(found_any, info = "Rendered HTML should contain offline narrative prose")
})
