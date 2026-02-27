test_that("generate_report errors without rmarkdown", {
  # Only run if rmarkdown is not installed
  skip_if(requireNamespace("rmarkdown", quietly = TRUE))

  task <- create_fitted_mock_task()
  expect_error(generate_report(task), "rmarkdown")
})


test_that("generate_report errors on wrong task type", {
  skip_if_not_installed("rmarkdown")
  expect_error(generate_report(list()), "EventStudyTask")
})


test_that("generate_report produces HTML file", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- generate_report(
    task,
    output_file = tmp_file,
    format = "html",
    title = "Test Report",
    sections = c("summary", "appendix")
  )

  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
  unlink(result)
})


test_that("generate_report works with multiple sections", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- generate_report(
    task,
    output_file = tmp_file,
    format = "html",
    sections = c("summary", "data", "multi_event", "appendix")
  )

  expect_true(file.exists(result))
  unlink(result)
})


test_that("generate_report with summary only", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- generate_report(
    task,
    output_file = tmp_file,
    format = "html",
    sections = c("summary")
  )

  expect_true(file.exists(result))
  unlink(result)
})


test_that("generate_report with custom title and author", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task <- create_fitted_mock_task()
  tmp_file <- tempfile(fileext = ".html")

  result <- generate_report(
    task,
    output_file = tmp_file,
    format = "html",
    title = "Custom Title",
    author = "Test Author",
    sections = c("summary")
  )

  expect_true(file.exists(result))
  # Read file and check that title is present
  content <- readLines(result)
  expect_true(any(grepl("Custom Title", content)))
  unlink(result)
})
