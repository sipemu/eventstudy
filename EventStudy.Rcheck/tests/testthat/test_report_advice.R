# =============================================================================
# test_report_advice.R — backward-compat + degrade + render integration tests
#                         for generate_report(advice = ...) (ADV-07, 07-2)
# =============================================================================
#
# Covers:
#   1. Backward-compat: `advice` is a trailing formal defaulting NULL; all
#      pre-existing param keys are present and unaffected.
#   2. Degrade: a non-Advice value -> exactly ONE "not an Advice object" warning;
#      the section is skipped; the report is not broken.
#   3. Render integration (behind skip guards): a supplied grounded Advice renders
#      the "AI Advisor Interpretation" section; NULL-advice omits it.
#   4. skeleton.Rmd structural assertion: the guarded chunk eval= condition is
#      present in the template (deterministic, no render required).

# ---- 1. Backward-compat: formals inspection -----------------------------------

test_that("advice is a trailing formal defaulting NULL (ADV-07 backward-compat)", {
  f <- formals(generate_report)

  # 'advice' must be present
  expect_true("advice" %in% names(f))

  # default must be NULL
  expect_null(f$advice)

  # 'advice' must appear after 'interactive' and before '...'
  param_names <- names(f)
  idx_interactive <- which(param_names == "interactive")
  idx_advice      <- which(param_names == "advice")
  idx_dots        <- which(param_names == "...")
  expect_true(idx_advice > idx_interactive,
              info = "advice must come after interactive")
  expect_true(idx_advice < idx_dots,
              info = "advice must come before ...")
})

test_that("all pre-existing param keys are still present in generate_report", {
  f <- formals(generate_report)
  expected_keys <- c("task", "output_file", "format", "title", "author",
                     "sections", "cross_sectional", "confidence_level",
                     "interactive", "...")
  for (k in expected_keys) {
    expect_true(k %in% names(f),
                info = paste("pre-existing param", k, "must still be present"))
  }
})

# ---- 2. Degrade: non-Advice advice -> exactly one warning, no crash -----------

test_that("generate_report with non-Advice advice -> one 'not an Advice object' warning", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  # We test only the validation branch — not the full render — so we don't need
  # a real task. Inject the invalid advice BEFORE the task-class check by
  # calling the validation inline via the source body.
  #
  # The cleanest way: pass a valid task + invalid advice and assert the warning
  # fires. The render itself is skipped-guarded; here we only prove the coerce
  # happens without a second render call.
  task <- create_fitted_mock_task()

  # expect_warning() in testthat 3e: the warning must match the regexp, exactly
  # once. A second warning would cause the test to fail.
  tmp_file <- tempfile(fileext = ".html")
  expect_warning(
    generate_report(
      task,
      output_file = tmp_file,
      format      = "html",
      sections    = c("summary", "appendix"),
      advice      = list(not = "an advice object")
    ),
    regexp  = "not an Advice object",
    fixed   = FALSE
  )
  # Clean up if the report was generated (advice coerced to NULL -> normal render)
  if (file.exists(tmp_file)) unlink(tmp_file)
})

test_that("generate_report with non-Advice advice -> report still generated (no stop)", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  # Suppress the expected warning; assert report is produced
  withCallingHandlers(
    {
      result <- generate_report(
        task,
        output_file = tmp_file,
        format      = "html",
        sections    = c("summary", "appendix"),
        advice      = list(not = "an advice object")
      )
      expect_true(file.exists(result))
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  if (file.exists(tmp_file)) unlink(tmp_file)
})

test_that("generate_report with non-list advice (integer) -> one warning, no crash", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")
  expect_warning(
    generate_report(
      task,
      output_file = tmp_file,
      format      = "html",
      sections    = c("summary", "appendix"),
      advice      = 42L   # not an Advice, not a list
    ),
    regexp = "not an Advice object"
  )
  if (file.exists(tmp_file)) unlink(tmp_file)
})

# ---- 3. Render integration: skeleton.Rmd structural assertion -----------------
# Deterministic: no render, no task fixture — just assert the eval= condition
# string is present in the skeleton file. This proves the guard is wired without
# requiring a full render environment.

test_that("skeleton.Rmd contains guarded advice-section chunk eval condition", {
  skeleton_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skeleton_path == "", "skeleton.Rmd not found (package not installed)")

  lines <- readLines(skeleton_path, warn = FALSE)

  # The advice-section chunk header must be present
  has_chunk <- any(grepl("advice-section", lines, fixed = TRUE))
  expect_true(has_chunk, info = "skeleton.Rmd must contain an advice-section chunk")

  # The eval= guard must reference params$advice and inherits(..., "Advice")
  has_eval_guard <- any(grepl("is.null(params$advice)", lines, fixed = TRUE))
  expect_true(has_eval_guard,
              info = "advice-section chunk must have eval= guard checking is.null(params$advice)")

  has_inherits_guard <- any(grepl('inherits(params$advice', lines, fixed = TRUE))
  expect_true(has_inherits_guard,
              info = 'eval= guard must also check inherits(params$advice, "Advice")')

  # The params block must declare advice: NULL
  has_advice_param <- any(grepl("^  advice: NULL", lines))
  expect_true(has_advice_param,
              info = 'skeleton.Rmd params block must include "advice: NULL"')

  # Chunk must appear BEFORE appendix-section
  chunk_idx    <- which(grepl("advice-section", lines, fixed = TRUE))
  appendix_idx <- which(grepl("appendix-section", lines, fixed = TRUE))
  expect_true(length(chunk_idx) > 0L, info = "advice-section chunk must exist")
  expect_true(length(appendix_idx) > 0L, info = "appendix-section chunk must exist")
  expect_true(min(chunk_idx) < min(appendix_idx),
              info = "advice-section chunk must precede appendix-section chunk")
})

# ---- 4. Render integration: NULL advice -> section absent (behind skip guard) ----

test_that("NULL advice render path omits AI Advisor Interpretation section", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- generate_report(
    task,
    output_file = tmp_file,
    format      = "html",
    sections    = c("summary", "appendix"),
    advice      = NULL   # explicit NULL — same as default
  )

  expect_true(file.exists(result))
  content <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_false(grepl("AI Advisor Interpretation", content),
               info = "NULL advice must not render the AI Advisor Interpretation section")
  unlink(result)
})

# ---- 5. Render integration: grounded Advice -> section present ----------------
# Uses CustomProvider + .make_test_diag() from helper-advice-fixtures.R
# to build a grounded Advice without network (deterministic).

test_that("grounded Advice renders AI Advisor Interpretation section in HTML output", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()

  # Build a minimal grounded Advice using the CustomProvider + fixture
  diag <- .make_test_diag()
  p    <- CustomProvider$new(function(prompt, schema) {
    # A single grounded rec: car_iqr = 0.025 matches fixture exactly
    paste0(
      '{"interpretation":"Event returns are statistically significant.",',
      '"recommendations":[{"action":"Use BMP test","kind":"stat_choice",',
      '"rationale":"High variance across events.","expected_effect":"More robust p-values.",',
      '"evidence":[{"diagnostic_key":"cross_sectional.car_iqr",',
      '"value":0.025,"threshold":0.10,"direction":"below"}]}],',
      '"caveats":["AI-generated — verify with domain expert."]}'
    )
  })

  advice <- es_advise(diag, task_type = "report_writing", provider = p)
  expect_s3_class(advice, "Advice")

  task     <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- generate_report(
    task,
    output_file = tmp_file,
    format      = "html",
    sections    = c("summary", "appendix"),
    advice      = advice
  )

  expect_true(file.exists(result))
  content <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_true(grepl("AI Advisor Interpretation", content),
              info = "Grounded Advice must render the AI Advisor Interpretation section")
  unlink(result)
})
