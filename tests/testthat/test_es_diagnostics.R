## tests/testthat/test_es_diagnostics.R
## Testthat 3e tests for es_diagnostics() — DIAG-01 through DIAG-07

# Task 1: Tracer — basic S3 class, meta, estimation_window, print
test_that("DIAG-01: es_diagnostics returns an es_diagnostics S3 object", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  expect_true(inherits(result, "es_diagnostics"))
})

test_that("DIAG-01: meta$n_events_total equals nrow(task$data_tbl)", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  expect_equal(result$meta$n_events_total, nrow(task$data_tbl))
})

test_that("DIAG-02: estimation_window$r2 is a plain numeric vector with finite or NA entries", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  r2 <- result$estimation_window$r2
  expect_true(is.numeric(r2))
  expect_false(is.null(r2))
  # Each entry must be finite or NA (never a list, never a dist object)
  expect_true(all(is.finite(r2) | is.na(r2)))
})

test_that("DIAG-01: print.es_diagnostics produces non-empty output containing 'Event Study Diagnostics'", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  out <- capture.output(print(result))
  expect_true(length(out) > 0)
  expect_true(any(grepl("Event Study Diagnostics", out)))
})

# Task 2: Full signal set
test_that("DIAG-02: estimation_window carries all required signal vectors", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  ew <- result$estimation_window
  expect_true(is.numeric(ew$shapiro_p))
  expect_true(is.numeric(ew$dw_stat))
  expect_true(is.numeric(ew$ljung_box_p))
  expect_true(is.numeric(ew$acf1))
  expect_true(is.numeric(ew$sigma))
  expect_true(is.numeric(ew$degree_of_freedom))
  # All must be plain numeric (finite or NA — never NULL or list)
  for (field in c("shapiro_p", "dw_stat", "ljung_box_p", "acf1", "sigma", "degree_of_freedom")) {
    vals <- ew[[field]]
    expect_true(all(is.finite(vals) | is.na(vals)),
                info = paste("Field", field, "must be finite or NA"))
  }
})

test_that("DIAG-03: event_window carries scalar ar_t/car_t VALUES as plain numerics (no distributional objects)", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  ew <- result$event_window
  # ar_t and car_t should be plain numeric vectors
  expect_true(is.numeric(ew$ar_t))
  expect_true(is.numeric(ew$car_t))
  # p-values should also be plain numeric
  expect_true(is.numeric(ew$ar_p))
  expect_true(is.numeric(ew$car_p))
  # p-values in [0,1] where not NA
  ar_p_vals <- ew$ar_p[!is.na(ew$ar_p)]
  car_p_vals <- ew$car_p[!is.na(ew$car_p)]
  expect_true(all(ar_p_vals >= 0 & ar_p_vals <= 1))
  expect_true(all(car_p_vals >= 0 & car_p_vals <= 1))
})

test_that("DIAG-03: es_diagnostics output is JSON-serializable (no distributional objects)", {
  skip_if_not_installed("jsonlite")
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  # unclass() strips the S3 class so jsonlite uses its default list handler;
  # the inner data must be plain R atomic vectors (no dist_student_t objects).
  expect_no_error(jsonlite::toJSON(unclass(result), null = "null"))
})

test_that("DIAG-04: cross_sectional carries n_events, n_valid_events, car_iqr, car_sd, n_overlap_pairs, any_overlap", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  cs <- result$cross_sectional
  expect_true(!is.null(cs$n_events))
  expect_true(!is.null(cs$n_valid_events))
  expect_true(is.numeric(cs$car_iqr) || is.na(cs$car_iqr))
  expect_true(is.numeric(cs$car_sd) || is.na(cs$car_sd))
  expect_true(is.numeric(cs$n_overlap_pairs) || is.na(cs$n_overlap_pairs))
  expect_true(is.logical(cs$any_overlap) || is.na(cs$any_overlap))
})

test_that("DIAG-05: contract_state carries per-event is_fitted, na_ar_count, na_est_count, insufficient_obs, zero_var_index", {
  task <- create_fitted_mock_task()
  result <- es_diagnostics(task)
  cs <- result$contract_state
  expect_true(is.logical(cs$is_fitted))
  expect_true(is.numeric(cs$na_ar_count))
  expect_true(is.numeric(cs$na_est_count))
  expect_true(is.logical(cs$insufficient_obs))
  expect_true(is.logical(cs$zero_var_index))
})

test_that("DIAG-07: es_diagnostics does not error when aar_caar_tbl is NULL (no calculate_statistics)", {
  task <- create_mock_task()
  ps <- ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  task <- prepare_event_study(task, ps)
  task <- fit_model(task, ps)
  # aar_caar_tbl should be NULL here
  expect_null(task$aar_caar_tbl)
  # es_diagnostics should not error and cross-sectional multi-event fields should be NA
  result <- es_diagnostics(task)
  expect_true(inherits(result, "es_diagnostics"))
  # With no CART column, car_iqr degrades to NA
  expect_true(is.na(result$cross_sectional$car_iqr) || is.numeric(result$cross_sectional$car_iqr))
})

# Task 3: Anomaly ranking + top-N cap + aggregate remainder
test_that("DIAG-06: with max_events < n_events, meta$n_events_shown is capped", {
  # Create a 4-firm task
  task <- create_fitted_mock_task(n_firms = 4)
  result <- es_diagnostics(task, max_events = 2L)
  expect_equal(result$meta$n_events_shown, 2L)
  expect_equal(result$meta$n_events_summarized, 2L)
  # Per-event vectors have length max_events
  expect_equal(length(result$estimation_window$r2), 2L)
})

test_that("DIAG-06: aggregate_summary is non-NULL when events are truncated", {
  task <- create_fitted_mock_task(n_firms = 4)
  result <- es_diagnostics(task, max_events = 2L)
  expect_false(is.null(result$aggregate_summary))
  expect_true(!is.null(result$aggregate_summary$n_summarized))
})

test_that("DIAG-06: default max_events=20 on small task shows all events with NULL aggregate_summary", {
  task <- create_fitted_mock_task(n_firms = 2)
  result <- es_diagnostics(task)
  expect_equal(result$meta$n_events_shown, nrow(task$data_tbl))
  expect_null(result$aggregate_summary)
})

test_that("DIAG-06: degenerate (unfitted) event always appears in shown events regardless of CAR", {
  # C4 (2026-09-24): real test of .rank_events_for_cap -- build a 4-event
  # task, then re-fit event 1's model on a degenerate (insufficient
  # observations) estimation window so it is genuinely unfitted, expecting
  # its single contract warning explicitly. The unfitted event's anomaly
  # score must be Inf, so it is ALWAYS included in a capped top-N selection
  # regardless of its (NA) CAR magnitude.
  task <- create_fitted_mock_task(n_firms = 4)

  degenerate_data <- create_degenerate_model_data_insufficient(n_valid = 1, n_event = 11)
  mm <- MarketModel$new()
  expect_warning(mm$fit(degenerate_data), "insufficient estimation observations")
  expect_false(mm$is_fitted)

  task$data_tbl$model[[1]] <- mm
  # Its abnormal returns propagate to NA, matching a genuinely degenerate event.
  task$data_tbl$data[[1]]$abnormal_returns <- NA_real_

  ranking <- EventStudy:::.rank_events_for_cap(task)
  expect_equal(ranking$anomaly_score[1], Inf)
  expect_equal(ranking$row_idx[1], 1L)

  # With a cap smaller than n_events, the unfitted event must still be
  # included in the top-N selection.
  cap <- 2L
  top_idx <- ranking$row_idx[seq_len(cap)]
  expect_true(1L %in% top_idx)

  # And es_diagnostics() itself surfaces this via meta$event_ids_shown.
  result <- es_diagnostics(task, max_events = cap)
  expect_true(task$data_tbl$event_id[1] %in% result$meta$event_ids_shown)
})

test_that("DIAG-06: n_events_summarized equals n_total - n_events_shown", {
  task <- create_fitted_mock_task(n_firms = 4)
  result <- es_diagnostics(task, max_events = 3L)
  expect_equal(result$meta$n_events_summarized,
               result$meta$n_events_total - result$meta$n_events_shown)
})
