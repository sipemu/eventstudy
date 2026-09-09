# Snapshot regression net for the 6 print.* methods (API-01 / API-02).
#
# These snapshots lock the exact console bytes of every print method BEFORE
# the print/format refactor. The format.*/print.* split in this phase must
# reproduce these bytes exactly (git diff --quiet on the _snaps file); a
# changed snapshot means the refactor drifted and is wrong.
#
# All fixtures are hand-assembled deterministic constants: no download, no
# model fit, no RNG. ASCII-only (CRAN non-ASCII guardrail).

test_that("print.EventStudySummary snapshot", {
  x <- list(
    n_events = 3L,
    groups = c("treated", "control"),
    symbols = c("AAA", "BBB"),
    model_stats = list(
      AAA = list(is_fitted = TRUE, alpha = 0.0012345, beta = 1.234567,
                 sigma = 0.0098765, r2 = 0.876543),
      BBB = list(is_fitted = FALSE)
    )
  )
  class(x) <- "EventStudySummary"
  expect_snapshot(print(x))
})

test_that("print.es_diagnostics snapshot (fitted)", {
  x <- list(
    meta = list(n_events_total = 5L, n_events_shown = 5L,
                n_events_summarized = 0L),
    estimation_window = list(
      r2 = c(0.81, 0.76, 0.90),
      shapiro_p = c(0.42, 0.15, 0.60),
      dw_stat = c(1.98, 2.05, 1.87)
    ),
    cross_sectional = list(
      car_iqr = 0.0234567,
      n_overlap_pairs = 4L,
      n_valid_events = 5L
    ),
    contract_state = list(is_fitted = c(TRUE, TRUE, TRUE, TRUE, TRUE))
  )
  class(x) <- "es_diagnostics"
  expect_snapshot(print(x))
})

test_that("print.es_diagnostics snapshot (degenerate WARN branch)", {
  x <- list(
    meta = list(n_events_total = 3L, n_events_shown = 3L,
                n_events_summarized = 2L),
    estimation_window = list(
      r2 = c(0.55, NA, 0.60),
      shapiro_p = c(0.10, 0.20, NA),
      dw_stat = c(2.10, 1.90, 2.00)
    ),
    cross_sectional = list(
      car_iqr = NA_real_,
      n_overlap_pairs = NA_integer_,
      n_valid_events = NA_integer_
    ),
    contract_state = list(is_fitted = c(TRUE, FALSE, FALSE))
  )
  class(x) <- "es_diagnostics"
  expect_snapshot(print(x))
})

test_that("print.es_simulation snapshot", {
  x <- list(
    params = list(
      n_events = 50L,
      event_window = c(-5L, 5L),
      abnormal_return = 0.01,
      test_statistic = "car_t",
      alpha = 0.05,
      n_simulations = 1000L
    ),
    power = 0.837421
  )
  class(x) <- "es_simulation"
  expect_snapshot(print(x))
})

test_that("print.es_cross_sectional snapshot", {
  x <- list(
    n_obs = 42L,
    r_squared = 0.234567,
    adj_r_squared = 0.198765,
    coefficients = data.frame(
      estimate = c(0.0012345, -0.0234567),
      std_error = c(0.0005432, 0.0109876),
      t_value = c(2.273456, -2.134567),
      p_value = c(0.028765, 0.039876),
      row.names = c("(Intercept)", "size")
    )
  )
  class(x) <- "es_cross_sectional"
  expect_snapshot(print(x))
})

test_that("print.Advice snapshot (with recommendations + guard drop)", {
  x <- list(
    source = "llm",
    task_type = "recommend_stat",
    is_deterministic = FALSE,
    interpretation = "CARs are significantly positive around the event.",
    n_dropped = 2L,
    recommendations = list(
      list(
        action = "Use BMP test",
        kind = "statistic",
        expected_effect = "more robust to variance inflation",
        evidence = list(
          list(diagnostic_key = "event_var_ratio", value = "3.2",
               threshold = "1.5", direction = "above")
        )
      )
    ),
    caveats = c("Small sample.", "Overlapping windows.")
  )
  class(x) <- "Advice"
  expect_snapshot(print(x))
})

test_that("print.es_advice snapshot (zero rules)", {
  x <- list(
    source = "offline_kb",
    is_deterministic = TRUE,
    rules_matched = list()
  )
  class(x) <- "es_advice"
  expect_snapshot(print(x))
})

test_that("print.es_advice snapshot (>=1 rule)", {
  x <- list(
    source = "offline_kb",
    is_deterministic = TRUE,
    rules_matched = list(
      list(severity = "warning", id = "R001",
           citation = list(key = "MacKinlay1997"),
           recommendation = "Prefer BMP over plain t-test under variance inflation.")
    )
  )
  class(x) <- "es_advice"
  expect_snapshot(print(x))
})
