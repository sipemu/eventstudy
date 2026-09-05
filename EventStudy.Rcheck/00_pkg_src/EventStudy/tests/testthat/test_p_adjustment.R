test_that("adjust_p_values returns correct columns", {
  task <- create_fitted_mock_task()
  result <- adjust_p_values(task, method = "BH", stat_name = "CSectT")

  expect_true("p_raw_aar" %in% names(result))
  expect_true("p_adj_aar" %in% names(result))
  expect_true("p_raw_caar" %in% names(result))
  expect_true("p_adj_caar" %in% names(result))
  expect_true("group" %in% names(result))
  # Original stat columns should also be present
  expect_true("aar" %in% names(result))
  expect_true("relative_index" %in% names(result))
})


test_that("adjusted p-values >= raw p-values for all methods", {
  task <- create_fitted_mock_task()

  for (m in c("BH", "bonferroni", "holm", "hochberg")) {
    result <- adjust_p_values(task, method = m, stat_name = "CSectT")
    expect_true(all(result$p_adj_aar >= result$p_raw_aar - 1e-10),
                info = paste("method:", m))
    expect_true(all(result$p_adj_caar >= result$p_raw_caar - 1e-10, na.rm = TRUE),
                info = paste("method:", m))
  }
})


test_that("Bonferroni p = min(1, raw * n)", {
  task <- create_fitted_mock_task()
  result <- adjust_p_values(task, method = "bonferroni", stat_name = "CSectT")

  n <- nrow(result)
  expected <- pmin(1, result$p_raw_aar * n)
  expect_equal(result$p_adj_aar, expected, tolerance = 1e-10)
})


test_that("method='none' returns unadjusted p-values", {
  task <- create_fitted_mock_task()
  result <- adjust_p_values(task, method = "none", stat_name = "CSectT")

  expect_equal(result$p_adj_aar, result$p_raw_aar, tolerance = 1e-10)
  expect_equal(result$p_adj_caar, result$p_raw_caar, tolerance = 1e-10)
})


test_that("all p-values are in [0, 1]", {
  task <- create_fitted_mock_task()
  result <- adjust_p_values(task, method = "BH", stat_name = "CSectT")

  expect_true(all(result$p_raw_aar >= 0 & result$p_raw_aar <= 1))
  expect_true(all(result$p_adj_aar >= 0 & result$p_adj_aar <= 1))
  expect_true(all(result$p_raw_caar >= 0 & result$p_raw_caar <= 1))
  expect_true(all(result$p_adj_caar >= 0 & result$p_adj_caar <= 1))
})


test_that("adjust_p_values works with PatellZ (z-statistic type)", {
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(PatellZTest$new())
    )
  )
  task <- run_event_study(task, ps)
  result <- adjust_p_values(task, method = "BH", stat_name = "PatellZ")

  expect_true("p_raw_aar" %in% names(result))
  expect_true("p_adj_aar" %in% names(result))
  expect_true(all(result$p_raw_aar >= 0 & result$p_raw_aar <= 1))
})


test_that("adjust_p_values works with BMP (bmp_t type)", {
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(BMPTest$new())
    )
  )
  task <- run_event_study(task, ps)
  result <- adjust_p_values(task, method = "BH", stat_name = "BMP")

  expect_true("p_raw_aar" %in% names(result))
  expect_true(all(result$p_raw_aar >= 0 & result$p_raw_aar <= 1))
})


test_that("adjust_p_values works with SignTest (sign_z type)", {
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(SignTest$new())
    )
  )
  task <- run_event_study(task, ps)
  result <- adjust_p_values(task, method = "bonferroni", stat_name = "SignT")

  expect_true("p_raw_aar" %in% names(result))
  expect_true(all(result$p_adj_aar >= result$p_raw_aar - 1e-10))
})


test_that("adjust_p_values group filtering works", {
  task <- create_fitted_mock_task()
  all_result <- adjust_p_values(task, method = "BH", stat_name = "CSectT")
  grp_result <- adjust_p_values(task, method = "BH", stat_name = "CSectT",
                                  group = "TestGroup")

  # Both should return data, group result should be subset
  expect_true(nrow(grp_result) > 0)
  expect_true(all(grp_result$group == "TestGroup"))
})


test_that("adjust_p_values errors on non-EventStudyTask", {
  expect_error(adjust_p_values(list()), "EventStudyTask")
})


test_that("adjust_p_values errors when no results", {
  task <- create_mock_task()
  expect_error(adjust_p_values(task), "Run calculate_statistics")
})


test_that("adjust_p_values errors on unknown stat_name", {
  task <- create_fitted_mock_task()
  expect_error(adjust_p_values(task, stat_name = "NonExistent"), "not found")
})
