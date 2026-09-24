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


test_that("bootstrap works with non-sequential event IDs via group filter", {
  # Create a task with 5 firms across 2 groups
  symbols = paste0("FIRM_", LETTERS[1:5])
  firm_data = create_mock_firm_data(symbols = symbols)
  index_data = create_mock_index_data()

  n_days = 300
  start_date = as.Date("2020-01-01")
  dates = seq(start_date, by = "day", length.out = n_days)
  dates = dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  event_date = format(dates[180], "%d.%m.%Y")

  request = tibble::tibble(
    event_id = 1:5,
    firm_symbol = symbols,
    index_symbol = "INDEX_1",
    event_date = event_date,
    group = c("A", "B", "A", "B", "A"),
    event_window_start = -5,
    event_window_end = 5,
    shift_estimation_window = -6,
    estimation_window_length = 120
  )

  task = EventStudyTask$new(firm_data, index_data, request)
  ps = ParameterSet$new()
  task = run_event_study(task, ps)

  # Filtering group "B" gives event_ids 2 and 4 (non-sequential)
  result = bootstrap_test(task, n_boot = 19, group = "B", seed = 42)
  expect_true(all(result$boot_p_aar >= 0 & result$boot_p_aar <= 1))
  expect_equal(nrow(result), 11)
})


# --- Regression: boot_p_caar is NA when statistic="aar" ---

test_that("bootstrap_test returns NA boot_p_caar when statistic='aar'", {
  # Bug: When statistic="aar", boot_caar_exceed was never updated (stayed 0),

  # so boot_p_caar = 1/(n_boot+1) -- the minimum possible p-value regardless
  # of the actual data. Now correctly returns NA.
  task <- create_fitted_mock_task()
  result <- bootstrap_test(task, n_boot = 19, seed = 42, statistic = "aar")

  expect_true(all(is.na(result$boot_p_caar)))
  expect_true(all(!is.na(result$boot_p_aar)))
})


# --- Regression: bootstrap clusters by firm_symbol, not event_id ---

test_that("bootstrap_test clusters weights by firm_symbol (hand replication, C2/A12c 2026-09-24)", {
  # A12c (REVISED 2026-09-24): weights STAY clustered by firm_symbol
  # (deliberate: coarser firm clusters are robust to cross-event correlation
  # of a recurring firm). Lock this numerically: build a task with a firm
  # that recurs across two events, reproduce the exact wild-bootstrap
  # algorithm by hand using ONE weight per unique firm_symbol shared by all
  # of that firm's events, and assert it reproduces the package's output
  # exactly. Then show that an event_id-level clustering (one weight per
  # event) would generally have produced a DIFFERENT result on the same
  # seed -- confirming the package really does cluster by firm, not by event.
  symbols <- c("FIRM_A", "FIRM_A", "FIRM_B", "FIRM_B")
  firm_data <- create_mock_firm_data(symbols = unique(symbols))
  index_data <- create_mock_index_data()

  n_days <- 300
  start_date <- as.Date("2020-01-01")
  dates <- seq(start_date, by = "day", length.out = n_days)
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  event_date <- format(dates[180], "%d.%m.%Y")

  request <- tibble::tibble(
    event_id = 1:4,
    firm_symbol = symbols,
    index_symbol = "INDEX_1",
    event_date = event_date,
    group = "TestGroup",
    event_window_start = -5,
    event_window_end = 5,
    shift_estimation_window = -6,
    estimation_window_length = 120
  )

  task <- EventStudyTask$new(firm_data, index_data, request)
  ps <- ParameterSet$new()
  task <- run_event_study(task, ps)

  n_boot <- 25L
  seed <- 777L
  result <- bootstrap_test(task, n_boot = n_boot, seed = seed,
                            weight_type = "rademacher", statistic = "aar")

  ar_data <- task$data_tbl %>%
    dplyr::select(event_id, firm_symbol, data) %>%
    tidyr::unnest(data) %>%
    dplyr::filter(event_window == 1) %>%
    dplyr::select(event_id, firm_symbol, relative_index, abnormal_returns)

  firm_ids <- unique(ar_data$firm_symbol)
  expect_equal(length(firm_ids), 2L)  # FIRM_A, FIRM_B -- the clustering unit

  observed <- ar_data %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(aar = mean(abnormal_returns, na.rm = TRUE),
                     sd_aar = stats::sd(abnormal_returns, na.rm = TRUE),
                     n = sum(!is.na(abnormal_returns)), .groups = "drop") %>%
    dplyr::mutate(aar_t = ifelse(is.finite(sd_aar) & sd_aar > 0,
                                 sqrt(n) * aar / sd_aar, NA_real_))
  obs_aar_t <- observed$aar_t

  .replicate_boot_p_aar <- function(cluster_ids) {
    set.seed(seed)
    n_cluster <- length(cluster_ids)
    exceed <- rep(0L, nrow(observed))
    valid  <- rep(0L, nrow(observed))
    id_col <- if (identical(sort(cluster_ids), sort(unique(ar_data$firm_symbol)))) {
      "firm_symbol"
    } else {
      "event_id"
    }
    for (b in seq_len(n_boot)) {
      w <- sample(c(-1, 1), n_cluster, replace = TRUE)
      names(w) <- as.character(cluster_ids)
      boot_ar <- ar_data %>%
        dplyr::mutate(boot_ar = abnormal_returns * w[as.character(.data[[id_col]])])
      boot_stats <- boot_ar %>%
        dplyr::group_by(relative_index) %>%
        dplyr::summarise(boot_aar = mean(boot_ar, na.rm = TRUE),
                         sd_boot = stats::sd(boot_ar, na.rm = TRUE),
                         n = sum(!is.na(boot_ar)), .groups = "drop") %>%
        dplyr::mutate(boot_aar_t = ifelse(is.finite(sd_boot) & sd_boot > 0,
                                           sqrt(n) * boot_aar / sd_boot, NA_real_))
      draw_finite <- is.finite(boot_stats$boot_aar_t)
      comparison <- draw_finite & (abs(boot_stats$boot_aar_t) >= abs(obs_aar_t))
      comparison[is.na(comparison)] <- FALSE
      exceed <- exceed + as.integer(comparison)
      valid  <- valid + as.integer(draw_finite)
    }
    p <- (exceed + 1) / (valid + 1)
    p[is.na(obs_aar_t) | valid == 0] <- NA_real_
    p
  }

  boot_p_aar_firm  <- .replicate_boot_p_aar(firm_ids)
  event_ids <- unique(ar_data$event_id)
  boot_p_aar_event <- .replicate_boot_p_aar(event_ids)

  # The package's output matches FIRM-level clustering exactly.
  expect_equal(result$boot_p_aar, boot_p_aar_firm, tolerance = 1e-12)
  # ...and differs from what event-level clustering would have produced
  # (same seed, same data, different weight-assignment granularity).
  expect_false(isTRUE(all.equal(boot_p_aar_firm, boot_p_aar_event)))
})
