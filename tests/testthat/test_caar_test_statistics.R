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
