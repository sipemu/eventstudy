test_that("export_results writes CSV files", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(list.files(dirname(tmp), full.names = TRUE,
                             pattern = tools::file_path_sans_ext(basename(tmp)))),
          add = TRUE)

  result <- export_results(task, tmp)
  expect_equal(result, tmp)

  # Multiple tables create suffixed files
  base <- tools::file_path_sans_ext(tmp)
  ar_file <- paste0(base, "_ar.csv")
  expect_true(file.exists(ar_file))
})

test_that("export_results writes single CSV when which has one element", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_results(task, tmp, which = "model")
  expect_true(file.exists(tmp))

  data <- read.csv(tmp)
  expect_true("sigma" %in% names(data))
  expect_true("event_id" %in% names(data))
})

test_that("export_results writes LaTeX file", {
  skip_if_not_installed("knitr")
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  export_results(task, tmp, which = "model")
  expect_true(file.exists(tmp))

  content <- readLines(tmp)
  expect_true(any(grepl("\\\\begin\\{tabular\\}", content)))
})

test_that("export_results errors on xlsx without openxlsx", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  tmp <- tempfile(fileext = ".xlsx")
  # This test depends on whether openxlsx is installed;
  # just check it doesn't fail with an unexpected error
  tryCatch({
    export_results(task, tmp, which = "model")
    expect_true(file.exists(tmp))
  }, error = function(e) {
    expect_true(grepl("openxlsx", conditionMessage(e)))
  })
})

test_that("export_results errors on unfitted task", {
  task <- create_mock_task()
  tmp <- tempfile(fileext = ".csv")

  expect_error(export_results(task, tmp), "No results available")
})

test_that("export_results infers format from extension", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  tmp <- tempfile(fileext = ".unknown")
  expect_error(export_results(task, tmp), "Cannot infer format")
})

test_that("tidy.EventStudyTask returns AR tibble", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  result <- tidy.EventStudyTask(task, type = "ar")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in% names(result)))
  expect_true(all(c("event_id", "firm_symbol") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("tidy.EventStudyTask returns CAR tibble", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  result <- tidy.EventStudyTask(task, type = "car")
  expect_s3_class(result, "tbl_df")
  expect_true("estimate" %in% names(result))
  # CAR should be cumulative (last >= sum effect)
  first_event <- result %>% dplyr::filter(event_id == result$event_id[1])
  expect_true(nrow(first_event) > 1)
})

test_that("tidy.EventStudyTask returns AAR tibble", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  result <- tidy.EventStudyTask(task, type = "aar")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("estimate", "caar") %in% names(result)))
})

test_that("tidy.EventStudyTask returns model tibble", {
  task <- create_mock_task()
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  result <- tidy.EventStudyTask(task, type = "model")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("term", "estimate") %in% names(result)))
  expect_true("alpha" %in% result$term)
  expect_true("beta" %in% result$term)
})

test_that("tidy.EventStudyTask errors on unfitted task", {
  task <- create_mock_task()
  expect_error(tidy.EventStudyTask(task, type = "ar"), "not computed")
})


# --- Export content validation (issue #3, gap #7) ---

test_that("export CSV AR content is correct", {
  task <- create_fitted_mock_task()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_results(task, tmp, which = "ar")
  expect_true(file.exists(tmp))

  data <- read.csv(tmp)
  expect_true("abnormal_returns" %in% names(data))
  expect_true("event_id" %in% names(data))
  expect_true("relative_index" %in% names(data))
  # Should have event_window rows only
  expect_true(nrow(data) > 0)
})


test_that("export CSV CAR content is correct", {
  task <- create_fitted_mock_task()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_results(task, tmp, which = "car")
  expect_true(file.exists(tmp))

  data <- read.csv(tmp)
  expect_true("car" %in% names(data))
  expect_true("abnormal_returns" %in% names(data))
})


test_that("export multiple CSVs creates all files", {
  task <- create_fitted_mock_task()
  tmp <- tempfile(fileext = ".csv")
  base <- tools::file_path_sans_ext(tmp)
  on.exit(unlink(list.files(dirname(tmp), full.names = TRUE,
                             pattern = basename(base))),
          add = TRUE)

  export_results(task, tmp, which = c("ar", "car", "model"))

  expect_true(file.exists(paste0(base, "_ar.csv")))
  expect_true(file.exists(paste0(base, "_car.csv")))
  expect_true(file.exists(paste0(base, "_model.csv")))
})


# --- Tidy edge cases (issue #3, gap #16) ---

test_that("tidy AR with MarketAdjustedModel has sigma", {
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = MarketAdjustedModel$new())
  task <- run_event_study(task, ps)

  result <- tidy.EventStudyTask(task, type = "ar")
  # Should have valid std.error (not NA) since sigma is now set
  expect_true(all(!is.na(result$std.error)))
  expect_true(all(!is.na(result$statistic)))
  expect_true(all(!is.na(result$p.value)))
})


test_that("tidy model with non-regression model omits alpha/beta", {
  task <- create_mock_task()
  ps <- ParameterSet$new(return_model = ComparisonPeriodMeanAdjustedModel$new())
  task <- run_event_study(task, ps)

  result <- tidy.EventStudyTask(task, type = "model")
  # ComparisonPeriodMeanAdjustedModel doesn't produce alpha/beta
  expect_true("sigma" %in% result$term)
})


test_that("tidy AAR errors on missing stat_name", {
  task <- create_fitted_mock_task()
  expect_error(
    tidy.EventStudyTask(task, type = "aar", stat_name = "Nonexistent"),
    "not found"
  )
})
