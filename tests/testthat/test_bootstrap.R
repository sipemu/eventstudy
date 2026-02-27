test_that("bootstrap_test returns correct columns", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 19, seed = 42)

  expect_true("relative_index" %in% names(result))
  expect_true("observed_aar" %in% names(result))
  expect_true("observed_caar" %in% names(result))
  expect_true("boot_p_aar" %in% names(result))
  expect_true("boot_p_caar" %in% names(result))
  # Number of rows should match event window length
  expect_equal(nrow(result), 11)  # default event window is -5 to 5
})


test_that("bootstrap p-values are in [0, 1]", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 19, seed = 42)

  expect_true(all(result$boot_p_aar >= 0 & result$boot_p_aar <= 1))
  expect_true(all(result$boot_p_caar >= 0 & result$boot_p_caar <= 1))
})


test_that("bootstrap seed reproducibility", {
  task <- create_fitted_mock_task()
  r1 <- bootstrap_test(task, n_boot = 19, seed = 123)
  r2 <- bootstrap_test(task, n_boot = 19, seed = 123)

  expect_equal(r1$boot_p_aar, r2$boot_p_aar)
  expect_equal(r1$boot_p_caar, r2$boot_p_caar)
  expect_equal(r1$observed_aar, r2$observed_aar)
  expect_equal(r1$observed_caar, r2$observed_caar)
})


test_that("different seeds produce different results", {
  task <- create_fitted_mock_task()
  r1 <- bootstrap_test(task, n_boot = 99, seed = 1)
  r2 <- bootstrap_test(task, n_boot = 99, seed = 999)

  # With enough replications and different seeds, p-values should generally differ
  # (not guaranteed per-element, but overall vector should differ)
  expect_false(identical(r1$boot_p_aar, r2$boot_p_aar))
})


test_that("bootstrap with rademacher weights works", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 19, weight_type = "rademacher",
                            seed = 42)

  expect_true(all(result$boot_p_aar >= 0 & result$boot_p_aar <= 1))
  expect_true(all(result$boot_p_caar >= 0 & result$boot_p_caar <= 1))
})


test_that("bootstrap with mammen weights works", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 19, weight_type = "mammen", seed = 42)

  expect_true(all(result$boot_p_aar >= 0 & result$boot_p_aar <= 1))
  expect_true(all(result$boot_p_caar >= 0 & result$boot_p_caar <= 1))
})


test_that("bootstrap statistic='aar' only computes AAR p-values", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 19, statistic = "aar", seed = 42)

  # AAR p-values should be meaningful
  expect_true(all(result$boot_p_aar >= 0 & result$boot_p_aar <= 1))
  # CAAR should still be present (column always returned) but bootstrap
  # loop for CAAR is skipped, so p-values may differ
  expect_true("boot_p_caar" %in% names(result))
})


test_that("bootstrap statistic='caar' computes CAAR p-values", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 19, statistic = "caar", seed = 42)

  expect_true(all(result$boot_p_caar >= 0 & result$boot_p_caar <= 1))
})


test_that("bootstrap with n_boot=1 returns valid p-values", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 1, seed = 42)

  # With 1 bootstrap, p-values should be 0.5 or 1 (since (count+1)/(1+1))
  expect_true(all(result$boot_p_aar >= 0 & result$boot_p_aar <= 1))
  expect_true(all(result$boot_p_caar >= 0 & result$boot_p_caar <= 1))
})


test_that("bootstrap observed values match cross-sectional means", {
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 5, seed = 42)

  # observed_aar should be the mean AR across firms at each relative_index
  ar_data <- task$data_tbl %>%
    dplyr::select(event_id, data) %>%
    tidyr::unnest(data) %>%
    dplyr::filter(event_window == 1) %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(aar = mean(abnormal_returns, na.rm = TRUE),
                     .groups = "drop")

  expect_equal(result$observed_aar, ar_data$aar, tolerance = 1e-10)
})


test_that("bootstrap with more firms gives more stable results", {
  task5 <- create_fitted_mock_task(n_firms = 5)
  result5 <- bootstrap_test(task5, n_boot = 49, seed = 42)

  # Should still produce valid results
  expect_true(all(result5$boot_p_aar >= 0 & result5$boot_p_aar <= 1))
  expect_equal(nrow(result5), 11)
})


test_that("bootstrap errors on non-EventStudyTask", {
  expect_error(bootstrap_test(list()), "EventStudyTask")
})


test_that("bootstrap errors on unprepared task", {
  task <- create_mock_task()
  # Task without running the pipeline — nested data lacks event_window column
  expect_error(bootstrap_test(task, n_boot = 5, seed = 42))
})
