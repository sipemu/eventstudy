test_that("CSectTTest AAR/CAAR integration with full pipeline", {
  task = create_mock_task()
  ps = ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = MultiEventStatisticsSet$new()
  )
  task = prepare_event_study(task, ps)
  task = fit_model(task, ps)
  task = calculate_statistics(task, ps)

  # CAAR table should exist
  expect_false(is.null(task$aar_caar_tbl))

  # Extract AAR/CAAR results
  aar_result = task$get_aar()
  expect_true(is.list(aar_result) || inherits(aar_result, "tbl_df"))
})


# C9 (2026-09-24): folded in as a real numeric CSectTTest test (the file
# previously had only the shape-only test above).
test_that("C9: CSectTTest aar/caar/caar_t match a direct hand computation", {
  set.seed(261202)
  n_events <- 4L
  ev_n <- 11L  # -5..5
  data <- do.call(rbind, lapply(seq_len(n_events), function(i) {
    tibble::tibble(
      event_id = paste0("E", i),
      firm_symbol = paste0("F", i),
      relative_index = -5:5,
      abnormal_returns = rnorm(ev_n, mean = 0.003, sd = 0.02),
      event_window = 1L,
      estimation_window = 0L
    )
  }))

  result <- CSectTTest$new()$compute(data, NULL)

  # aar = mean AR per day across events
  expected_aar <- vapply(-5:5, function(ri) {
    mean(data$abnormal_returns[data$relative_index == ri])
  }, numeric(1))
  # caar = cumsum(aar)
  expected_caar <- cumsum(expected_aar)
  # caar_t = sqrt(n) * caar / sd(per-event CAR), hand-computed per-event CAR
  per_event_car <- data %>%
    dplyr::group_by(event_id) %>%
    dplyr::arrange(relative_index, .by_group = TRUE) %>%
    dplyr::mutate(car = cumsum(abnormal_returns)) %>%
    dplyr::ungroup()
  expected_caar_t <- vapply(-5:5, function(ri) {
    cars <- per_event_car$car[per_event_car$relative_index == ri]
    sqrt(length(cars)) * expected_caar[ri + 6] / stats::sd(cars)
  }, numeric(1))

  ordered <- result[order(result$relative_index), ]
  expect_equal(ordered$aar, expected_aar, tolerance = 1e-10)
  expect_equal(ordered$caar, expected_caar, tolerance = 1e-10)
  expect_equal(ordered$caar_t, expected_caar_t, tolerance = 1e-10)
})
