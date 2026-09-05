## tests/testthat/test_advise_offline.R
## Tests for recommend_stat() and flag_robustness() offline advice layer (Plan 05-3)

# ---- Task 1: basic structure, dispatch, category filtering, no-provider ------

test_that("recommend_stat on fitted task returns an es_advice object", {
  task   <- create_fitted_mock_task()
  advice <- recommend_stat(task)

  expect_s3_class(advice, "es_advice")
  expect_equal(advice$source, "offline_kb")
  expect_true(advice$is_deterministic)
  expect_type(advice$rules_matched, "list")
})

test_that("recommend_stat on es_diagnostics returns same shape as task input path", {
  task   <- create_fitted_mock_task()
  diag   <- es_diagnostics(task)

  advice_task <- recommend_stat(task)
  advice_diag <- recommend_stat(diag)

  # Both paths return es_advice with identical structural fields
  expect_s3_class(advice_diag, "es_advice")
  expect_equal(advice_diag$source,           advice_task$source)
  expect_equal(advice_diag$is_deterministic, advice_task$is_deterministic)
  expect_type(advice_diag$rules_matched,     "list")
})

test_that("flag_robustness returns only robustness-category rules", {
  task   <- create_fitted_mock_task()
  advice <- flag_robustness(task)

  expect_s3_class(advice, "es_advice")
  categories <- vapply(advice$rules_matched, `[[`, character(1L), "category")
  expect_true(all(categories == "robustness"))
})

test_that("recommend_stat returns only stat_choice-category rules", {
  task   <- create_fitted_mock_task()
  advice <- recommend_stat(task)

  categories <- vapply(advice$rules_matched, `[[`, character(1L), "category")
  expect_true(all(categories == "stat_choice"))
})

test_that("recommend_stat and flag_robustness do NOT error with no provider (NULL)", {
  task <- create_fitted_mock_task()

  # Explicit NULL provider — offline guarantee (ADV-08)
  expect_no_error(recommend_stat(task,    provider = NULL))
  expect_no_error(flag_robustness(task,   provider = NULL))

  diag <- es_diagnostics(task)
  expect_no_error(recommend_stat(diag,    provider = NULL))
  expect_no_error(flag_robustness(diag,   provider = NULL))
})

test_that("print.es_advice produces non-empty output listing id/severity/citation key", {
  task   <- create_fitted_mock_task()
  advice <- recommend_stat(task)

  output <- capture.output(print(advice))
  expect_gt(length(output), 0L)

  # Header lines present
  expect_true(any(grepl("Offline Event Study Advice", output)))
  expect_true(any(grepl("Source.*offline_kb",         output)))
  expect_true(any(grepl("Deterministic.*TRUE",         output)))

  # If any rules matched, each one should list id and severity bracket
  if (length(advice$rules_matched) > 0L) {
    expect_true(any(grepl("\\[INFO\\]|\\[WARNING\\]|\\[ERROR\\]", output)))
  }
})

test_that("rules_matched entries contain id, recommendation, citation, severity, category", {
  task   <- create_fitted_mock_task()
  diag   <- es_diagnostics(task)
  advice <- recommend_stat(diag)

  for (rule in advice$rules_matched) {
    expect_true("id"             %in% names(rule))
    expect_true("recommendation" %in% names(rule))
    expect_true("citation"       %in% names(rule))
    expect_true("severity"       %in% names(rule))
    expect_true("category"       %in% names(rule))
    expect_type(rule$citation, "list")
    expect_true("key" %in% names(rule$citation))
  }
})

test_that("matched rules are severity-ranked: error before warning before info", {
  task   <- create_fitted_mock_task()
  diag   <- es_diagnostics(task)
  # Use flag_robustness which has error-severity KB-DEGEN-EVENTS available
  advice <- flag_robustness(diag)

  if (length(advice$rules_matched) < 2L) skip("Need at least 2 matched rules to test ordering")

  sev_order <- c("error" = 1L, "warning" = 2L, "info" = 3L)
  sev_vals  <- vapply(advice$rules_matched, function(r) sev_order[[r$severity]], integer(1L))
  expect_true(all(diff(sev_vals) >= 0L), info = "severity order must be non-decreasing (error < warning < info)")
})


# ---- Task 2: grounded-steering integration tests + degenerate safety + JSON ----

test_that("non-normality diagnostics steer recommend_stat toward non-parametric rule", {
  # Build synthetic es_diagnostics where shapiro_p < 0.05 for all events
  diag <- structure(
    list(
      meta = list(
        n_events_total      = 5L,
        n_events_shown      = 5L,
        n_events_summarized = 0L,
        event_ids_shown     = 1:5
      ),
      estimation_window = list(
        r2                = rep(0.15, 5L),
        sigma             = rep(0.01, 5L),
        degree_of_freedom = rep(100L, 5L),
        acf1              = rep(0.05, 5L),
        shapiro_p         = rep(0.01, 5L),   # <0.05 for ALL events → 100% non-normal
        dw_stat           = rep(2.0,  5L),
        ljung_box_p       = rep(0.30, 5L)
      ),
      event_window = list(
        ar_t      = rep(NA_real_, 5L),
        ar_p      = rep(NA_real_, 5L),
        car_t     = rep(NA_real_, 5L),
        car_p     = rep(NA_real_, 5L),
        final_car = rep(NA_real_, 5L)
      ),
      cross_sectional = list(
        n_events        = 5L,
        n_valid_events  = 5L,
        car_iqr         = 0.05,
        car_sd          = 0.07,
        n_overlap_pairs = 0L,
        any_overlap     = FALSE
      ),
      contract_state = list(
        is_fitted        = rep(TRUE, 5L),
        na_ar_count      = rep(0L,   5L),
        na_est_count     = rep(0L,   5L),
        insufficient_obs = rep(FALSE, 5L),
        zero_var_index   = rep(FALSE, 5L)
      ),
      aggregate_summary = NULL
    ),
    class = "es_diagnostics"
  )

  advice <- recommend_stat(diag)
  ids    <- vapply(advice$rules_matched, `[[`, character(1L), "id")

  # KB-NONNORM-NONPAR must fire (>=50% non-normal)
  expect_true("KB-NONNORM-NONPAR" %in% ids,
              info = "Non-normality should steer toward non-parametric rule")

  # Recommendation text should name a non-parametric alternative
  nonnorm_rule <- advice$rules_matched[[which(ids == "KB-NONNORM-NONPAR")]]
  expect_true(grepl("Sign|Rank|Corrado|non-parametric", nonnorm_rule$recommendation,
                    ignore.case = TRUE),
              info = "Recommendation must name non-parametric alternative")
})

test_that("overlap diagnostics steer recommend_stat toward Kolari-Pynnonen rule", {
  # Synthetic diagnostics with n_overlap_pairs > 0
  diag <- structure(
    list(
      meta = list(
        n_events_total      = 5L,
        n_events_shown      = 5L,
        n_events_summarized = 0L,
        event_ids_shown     = 1:5
      ),
      estimation_window = list(
        r2                = rep(0.20, 5L),
        sigma             = rep(0.01, 5L),
        degree_of_freedom = rep(100L, 5L),
        acf1              = rep(0.05, 5L),
        shapiro_p         = rep(0.30, 5L),  # mostly normal
        dw_stat           = rep(2.0,  5L),
        ljung_box_p       = rep(0.30, 5L)
      ),
      event_window = list(
        ar_t      = rep(NA_real_, 5L),
        ar_p      = rep(NA_real_, 5L),
        car_t     = rep(NA_real_, 5L),
        car_p     = rep(NA_real_, 5L),
        final_car = rep(NA_real_, 5L)
      ),
      cross_sectional = list(
        n_events        = 5L,
        n_valid_events  = 5L,
        car_iqr         = 0.05,
        car_sd          = 0.07,
        n_overlap_pairs = 3L,   # overlap present
        any_overlap     = TRUE
      ),
      contract_state = list(
        is_fitted        = rep(TRUE,  5L),
        na_ar_count      = rep(0L,    5L),
        na_est_count     = rep(0L,    5L),
        insufficient_obs = rep(FALSE, 5L),
        zero_var_index   = rep(FALSE, 5L)
      ),
      aggregate_summary = NULL
    ),
    class = "es_diagnostics"
  )

  advice <- recommend_stat(diag)
  ids    <- vapply(advice$rules_matched, `[[`, character(1L), "id")

  expect_true("KB-OVERLAP-KP" %in% ids,
              info = "Event-window overlap should fire Kolari-Pynnonen rule")

  kp_rule <- advice$rules_matched[[which(ids == "KB-OVERLAP-KP")]]
  expect_equal(kp_rule$citation$key, "KolariPynnonen2010")
})

test_that("degenerate task with no API key returns valid es_advice without error", {
  # Create a task that includes at least one degenerate event by forcing
  # its model to an unfitted state via a mock degenerate diagnostics object
  diag_degen <- structure(
    list(
      meta = list(
        n_events_total      = 3L,
        n_events_shown      = 3L,
        n_events_summarized = 0L,
        event_ids_shown     = 1:3
      ),
      estimation_window = list(
        r2                = c(NA_real_, NA_real_, 0.1),
        sigma             = c(NA_real_, NA_real_, 0.01),
        degree_of_freedom = c(NA_real_, NA_real_, 100L),
        acf1              = c(NA_real_, NA_real_, 0.05),
        shapiro_p         = c(NA_real_, NA_real_, 0.30),
        dw_stat           = c(NA_real_, NA_real_, 2.0),
        ljung_box_p       = c(NA_real_, NA_real_, 0.30)
      ),
      event_window = list(
        ar_t      = c(NA_real_, NA_real_, NA_real_),
        ar_p      = c(NA_real_, NA_real_, NA_real_),
        car_t     = c(NA_real_, NA_real_, NA_real_),
        car_p     = c(NA_real_, NA_real_, NA_real_),
        final_car = c(NA_real_, NA_real_, NA_real_)
      ),
      cross_sectional = list(
        n_events        = 3L,
        n_valid_events  = 1L,   # only 1 of 3 fitted → < 80%, KB-DEGEN-EVENTS fires
        car_iqr         = NA_real_,
        car_sd          = NA_real_,
        n_overlap_pairs = 0L,
        any_overlap     = FALSE
      ),
      contract_state = list(
        is_fitted        = c(FALSE, FALSE, TRUE),
        na_ar_count      = c(NA_integer_, NA_integer_, 0L),
        na_est_count     = c(NA_integer_, NA_integer_, 0L),
        insufficient_obs = c(TRUE,  TRUE,  FALSE),
        zero_var_index   = c(FALSE, FALSE, FALSE)
      ),
      aggregate_summary = NULL
    ),
    class = "es_diagnostics"
  )

  # No API key needed — offline guarantee (provider=NULL explicit)
  expect_no_error({
    adv_stat  <- recommend_stat(diag_degen,  provider = NULL)
    adv_robust <- flag_robustness(diag_degen, provider = NULL)
  })

  expect_s3_class(adv_stat,   "es_advice")
  expect_s3_class(adv_robust, "es_advice")

  # KB-DEGEN-EVENTS should fire: 1/3 fitted < 80%
  ids_robust <- vapply(adv_robust$rules_matched, `[[`, character(1L), "id")
  expect_true("KB-DEGEN-EVENTS" %in% ids_robust,
              info = "Degenerate events should trigger KB-DEGEN-EVENTS robustness rule")
})

test_that("rules_matched is JSON-serializable (no environment/function objects)", {
  skip_if_not_installed("jsonlite")

  task   <- create_fitted_mock_task()
  advice <- recommend_stat(task)

  # Strip diagnostics_ref (which contains the full task data) before serializing
  # rules_matched alone must be JSON-safe
  expect_no_error(
    jsonlite::toJSON(advice$rules_matched, null = "null", auto_unbox = TRUE)
  )
})
