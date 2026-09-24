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


# ============================================================
# C1 (2026-09-24): explicit-formula tests for the KP, generalized-sign,
# rank and calendar-time branches of adjust_p_values().
# ============================================================

test_that("C1: adjust_p_values KP branch matches explicit 2*pt(-abs(t), df)", {
  set.seed(261101)
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(KolariPynnonenTest$new())
    )
  )
  task <- run_event_study(task, ps)
  result <- adjust_p_values(task, method = "BH", stat_name = "KP")

  df <- pmax(result$n_valid_events - 1, 1)
  expected_p_raw_aar <- 2 * stats::pt(-abs(result$kp_t), df = df)
  expected_p_raw_caar <- 2 * stats::pt(-abs(result$ckp_t), df = df)

  expect_equal(result$p_raw_aar, expected_p_raw_aar, tolerance = 1e-12)
  expect_equal(result$p_raw_caar, expected_p_raw_caar, tolerance = 1e-12)
  expect_equal(result$p_adj_aar, stats::p.adjust(expected_p_raw_aar, method = "BH"),
               tolerance = 1e-12)
  expect_equal(result$p_adj_caar, stats::p.adjust(expected_p_raw_caar, method = "BH"),
               tolerance = 1e-12)
})


test_that("C1: adjust_p_values GeneralizedSignTest branch matches explicit 2*pnorm(-abs(z))", {
  set.seed(261102)
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(GeneralizedSignTest$new())
    )
  )
  task <- run_event_study(task, ps)
  result <- adjust_p_values(task, method = "BH", stat_name = "GSignT")

  expected_p_raw_aar <- 2 * stats::pnorm(-abs(result$gsign_z))
  expected_p_raw_caar <- 2 * stats::pnorm(-abs(result$cgsign_z))

  expect_equal(result$p_raw_aar, expected_p_raw_aar, tolerance = 1e-12)
  expect_equal(result$p_raw_caar, expected_p_raw_caar, tolerance = 1e-12)
  expect_equal(result$p_adj_aar, stats::p.adjust(expected_p_raw_aar, method = "BH"),
               tolerance = 1e-12)
  expect_equal(result$p_adj_caar, stats::p.adjust(expected_p_raw_caar, method = "BH"),
               tolerance = 1e-12)
})


test_that("C1: adjust_p_values RankTest branch matches explicit 2*pnorm(-abs(z)); p_raw_caar is NA", {
  set.seed(261103)
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(RankTest$new())
    )
  )
  task <- run_event_study(task, ps)
  result <- adjust_p_values(task, method = "BH", stat_name = "RankT")

  expected_p_raw_aar <- 2 * stats::pnorm(-abs(result$rank_z))

  expect_equal(result$p_raw_aar, expected_p_raw_aar, tolerance = 1e-12)
  expect_true(all(is.na(result$p_raw_caar)))
  expect_equal(result$p_adj_aar, stats::p.adjust(expected_p_raw_aar, method = "BH"),
               tolerance = 1e-12)
})


test_that("C1: adjust_p_values CalendarTimePortfolioTest branch matches explicit 2*pt(-abs(t), caltime_df)", {
  set.seed(261104)
  task <- create_mock_task(n_firms = 5)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(
      tests = list(CalendarTimePortfolioTest$new())
    )
  )
  task <- run_event_study(task, ps)

  # A7 (2026-09-24): caltime_df comes from attr(stat_tbl, "caltime_df"), the
  # Brown-Warner estimation-window degrees of freedom -- read it directly
  # from the un-adjusted CalTimeT result stored on the task, not re-derived.
  caltime_tbl <- task$aar_caar_tbl$CalTimeT[[1]]
  df <- max(attr(caltime_tbl, "caltime_df"), 1)

  result <- adjust_p_values(task, method = "BH", stat_name = "CalTimeT")

  expected_p_raw_aar <- 2 * stats::pt(-abs(result$caltime_t), df = df)
  expected_p_raw_caar <- 2 * stats::pt(-abs(result$ccaltime_t), df = df)

  expect_equal(result$p_raw_aar, expected_p_raw_aar, tolerance = 1e-12)
  expect_equal(result$p_raw_caar, expected_p_raw_caar, tolerance = 1e-12)
  expect_equal(result$p_adj_aar, stats::p.adjust(expected_p_raw_aar, method = "BH"),
               tolerance = 1e-12)
  expect_equal(result$p_adj_caar, stats::p.adjust(expected_p_raw_caar, method = "BH"),
               tolerance = 1e-12)
})
