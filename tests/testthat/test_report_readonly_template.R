# =============================================================================
# test_report_readonly_template.R -- generate_report() must not write into the
# installed package library (CRAN policy; Debian checks mount it read-only).
# =============================================================================
#
# Deliberately NOT skip_on_cran(): this is the CRAN failure mode it guards.

test_that("generate_report() renders without writing next to the installed template", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  template_dir <- system.file(
    "rmarkdown/templates/event_study_report/skeleton",
    package = "EventStudy"
  )
  before <- list.files(template_dir, all.files = TRUE, recursive = TRUE)

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_file), add = TRUE)

  suppressWarnings(generate_report(
    task,
    output_file = tmp_file,
    format      = "html",
    sections    = c("summary", "appendix"),
    verbose     = FALSE
  ))

  expect_true(file.exists(tmp_file))
  expect_identical(
    list.files(template_dir, all.files = TRUE, recursive = TRUE),
    before
  )
})
