# =============================================================================
# test_report_narrative.R -- REPORT-03 backward-compat + independent-seam tests
#                            for generate_report(narrative = ...)
# =============================================================================
#
# Covers:
#   A. FORMALS (fast, no skip): narrative formal exists, default NULL, position
#      after advice and before ... -- mirrors test_report_advice.R:29-35.
#   B. GOLDEN-FILE (skip-guarded): narrative=NULL byte-identical to baseline;
#      this is the REPORT-03 success-criterion-3 proof.
#   C. INDEPENDENT SEAMS: invalid narrative degrades with one warning; narrative
#      and advice are independently NULLable.

# ---- A. FORMALS (fast, no skip) -----------------------------------------------

test_that("narrative is a formal of generate_report, default NULL", {
  f <- formals(generate_report)

  expect_true("narrative" %in% names(f),
              info = "narrative must be a formal parameter of generate_report")
  expect_null(f$narrative,
              info = "narrative default must be NULL")
})

test_that("narrative formal appears after advice and before ...", {
  param_names <- names(formals(generate_report))
  idx_advice    <- which(param_names == "advice")
  idx_narrative <- which(param_names == "narrative")
  idx_dots      <- which(param_names == "...")

  expect_true(length(idx_advice)    > 0L, info = "advice formal must exist")
  expect_true(length(idx_narrative) > 0L, info = "narrative formal must exist")
  expect_true(length(idx_dots)      > 0L, info = "... formal must exist")

  expect_true(idx_narrative > idx_advice,
              info = "narrative must come after advice")
  expect_true(idx_narrative < idx_dots,
              info = "narrative must come before ...")
})

test_that("skeleton.Rmd declares narrative: NULL in params block", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found (package not installed)")

  lines <- readLines(skeleton_path, warn = FALSE)

  has_narrative_param <- any(grepl("^  narrative: NULL", lines))
  expect_true(has_narrative_param,
              info = 'skeleton.Rmd params block must include "  narrative: NULL"')
})

test_that("skeleton.Rmd contains eval-guarded narrative-section chunk", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found (package not installed)")

  lines <- readLines(skeleton_path, warn = FALSE)

  has_chunk <- any(grepl("narrative-section", lines, fixed = TRUE))
  expect_true(has_chunk,
              info = "skeleton.Rmd must contain a narrative-section chunk")

  has_eval_guard <- any(grepl("is.null(params$narrative)", lines, fixed = TRUE))
  expect_true(has_eval_guard,
              info = "narrative-section chunk must have eval= guard checking is.null(params$narrative)")
})

# ---- B. GOLDEN-FILE byte-identical (skip-guarded) ----------------------------
# REPORT-03 proof: narrative=NULL render is byte-identical to the pre-narrative
# call shape (no narrative arg at all). Both calls use advice=NULL and identical
# sections. Timestamps are stripped before comparison (Pitfall 5 guard).

strip_ts <- function(x) {
  # Strip any line containing an ISO date/time (rmarkdown may embed render date)
  x[!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", x)]
}

test_that("narrative=NULL path is byte-identical to pre-narrative baseline (REPORT-03)", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  baseline <- tempfile(fileext = ".html")
  new_out  <- tempfile(fileext = ".html")

  # Baseline: pre-narrative call shape (no narrative arg, advice=NULL)
  generate_report(task, output_file = baseline, format = "html",
                  advice = NULL, sections = c("summary", "appendix"))

  # New: explicit narrative = NULL
  generate_report(task, output_file = new_out, format = "html",
                  advice = NULL, sections = c("summary", "appendix"),
                  narrative = NULL)

  base_lines <- readLines(baseline, warn = FALSE)
  new_lines  <- readLines(new_out,  warn = FALSE)

  expect_identical(strip_ts(base_lines), strip_ts(new_lines),
                   info = "narrative=NULL must produce byte-identical (timestamp-stripped) output to the pre-narrative call")

  unlink(c(baseline, new_out))
})

test_that("advice=NULL and narrative=NULL together byte-identical to all-defaults baseline", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  baseline <- tempfile(fileext = ".html")
  new_out  <- tempfile(fileext = ".html")

  # Baseline: default call (both advice and narrative absent)
  generate_report(task, output_file = baseline, format = "html",
                  sections = c("summary", "appendix"))

  # Explicit NULL for both seams
  generate_report(task, output_file = new_out, format = "html",
                  sections = c("summary", "appendix"),
                  advice = NULL, narrative = NULL)

  base_lines <- readLines(baseline, warn = FALSE)
  new_lines  <- readLines(new_out,  warn = FALSE)

  expect_identical(strip_ts(base_lines), strip_ts(new_lines),
                   info = "Explicit advice=NULL + narrative=NULL must equal all-defaults baseline")

  unlink(c(baseline, new_out))
})

# ---- C. INDEPENDENT SEAMS (fast where possible; render group skip-guarded) ---

test_that("invalid narrative (not a list) -> exactly one warning, degrades to NULL", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  expect_warning(
    generate_report(
      task,
      output_file = tmp_file,
      format      = "html",
      sections    = c("summary", "appendix"),
      narrative   = "not a list"  # invalid: must be a list or NULL
    ),
    regexp = "named list or NULL",
    fixed  = FALSE
  )

  if (file.exists(tmp_file)) unlink(tmp_file)
})

test_that("invalid narrative (integer) -> exactly one warning, report not broken", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  expect_warning(
    {
      result <- generate_report(
        task,
        output_file = tmp_file,
        format      = "html",
        sections    = c("summary", "appendix"),
        narrative   = 42L
      )
      expect_true(file.exists(result))
    },
    regexp = "named list or NULL"
  )

  if (file.exists(tmp_file)) unlink(tmp_file)
})

test_that("narrative=NULL with a valid advice does not interfere with advice section", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()

  # Build a minimal grounded Advice (reuse CustomProvider pattern from test_report_advice.R)
  diag <- .make_test_diag()
  p    <- CustomProvider$new(function(prompt, schema) {
    paste0(
      '{"interpretation":"Test narrative independence.",',
      '"recommendations":[{"action":"Use BMP test","kind":"stat_choice",',
      '"rationale":"High cross-event variance.","expected_effect":"More robust p-values.",',
      '"evidence":[{"diagnostic_key":"cross_sectional.car_iqr",',
      '"value":0.025,"threshold":0.10,"direction":"below"}]}],',
      '"caveats":["AI-generated."]}'
    )
  })
  advice <- es_advise(diag, task_type = "report_writing", provider = p)
  expect_s3_class(advice, "Advice")

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # narrative=NULL + valid advice: advice section should still render
  result <- generate_report(
    task,
    output_file = tmp_file,
    format      = "html",
    sections    = c("summary", "appendix"),
    advice      = advice,
    narrative   = NULL
  )

  expect_true(file.exists(result))
  content <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_true(grepl("AI Advisor Interpretation", content),
              info = "narrative=NULL must not suppress the advice section when a valid Advice is supplied")
  unlink(result)
})

test_that("advice=NULL with a valid narrative list does not error", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # A valid named list for narrative (no numeric literals that would be
  # false-dropped if the grounding scanner were applied -- WR-04).
  # This test exercises generate_report()'s type validation and render path,
  # not the grounding scanner (scanner is invoked upstream in es_advise()).
  narrative_list <- list(
    exec_summary = "Five events were analyzed.",
    data_methods = "Market model estimated over 120-day window.",
    results      = "The cumulative abnormal returns were examined.",
    robustness   = "No overlapping event windows detected."
  )

  # Should render without error or warning
  result <- generate_report(
    task,
    output_file = tmp_file,
    format      = "html",
    sections    = c("summary", "appendix"),
    advice      = NULL,
    narrative   = narrative_list
  )

  expect_true(file.exists(result))
  unlink(result)
})
