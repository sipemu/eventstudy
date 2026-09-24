# Regression tests for the 2026-09-24 package re-evaluation, Section A
# (statistical silent-wrong defects A1-A12). Every test_that() description
# is prefixed with its item id. Each test FAILS on HEAD 92a1882 (pre-fix)
# for the reason described in its comment, and passes after the fix.

# --- Shared fixture helper -------------------------------------------------
# Builds a multi-event data_tbl + list-mock model_tbl pair, directly usable
# with `SomeTest$new()$compute(data_tbl, model_tbl)` (the pattern used by the
# 2026-09-24 audit repro scripts). `model` list elements are plain lists
# (list(statistics = list(...))), NOT ModelBase R6 instances -- matching the
# audit's a_funcs.R fixture pattern.
.me_fixture <- function(event_ids, firm_symbols = NULL, n_est = 30,
                         event_window = -5:5, seed = 1001,
                         sigma_by_event = NULL, n_params_by_event = NULL,
                         all_na_event_ids = integer(0), ar_gen = NULL) {
  set.seed(seed)
  if (is.null(firm_symbols)) firm_symbols <- paste0("F", event_ids)
  n_ew <- length(event_window)

  data_tbl <- purrr::map2_dfr(event_ids, firm_symbols, function(eid, fs) {
    est_idx <- seq(-(n_est + 5), -6)
    if (!is.null(ar_gen)) {
      vals <- ar_gen(eid)
      ar_est <- vals$est
      ar_evt <- vals$evt
    } else {
      ar_est <- rnorm(n_est, sd = 0.01)
      ar_evt <- rnorm(n_ew, sd = 0.01)
    }
    if (eid %in% all_na_event_ids) ar_evt <- rep(NA_real_, n_ew)
    tibble::tibble(
      event_id = eid, firm_symbol = fs, group = "g",
      relative_index = c(est_idx, event_window),
      estimation_window = c(rep(1L, n_est), rep(0L, n_ew)),
      event_window = c(rep(0L, n_est), rep(1L, n_ew)),
      abnormal_returns = c(ar_est, ar_evt)
    )
  })

  model_tbl <- tibble::tibble(
    event_id = event_ids,
    model = purrr::map(event_ids, function(eid) {
      sg <- if (!is.null(sigma_by_event)) sigma_by_event[[as.character(eid)]] else 0.01
      kp <- if (!is.null(n_params_by_event)) n_params_by_event[[as.character(eid)]] else 2
      list(statistics = list(
        sigma = sg,
        degree_of_freedom = n_est - 2,
        residuals = rnorm(n_est, sd = sg),
        n_params = kp,
        forecast_error_corrected_sigma = rep(sg, n_ew)
      ))
    })
  )
  list(data = data_tbl, model = model_tbl)
}

.count_warnings <- function(expr) {
  n <- 0L
  result <- withCallingHandlers(
    expr,
    warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") }
  )
  list(result = result, n = n)
}


# --- A1 ----------------------------------------------------------------

test_that("A1: RollingWindowModel beta/alpha/sigma match lm() on complete pairs", {
  set.seed(101)
  n_est <- 150
  index_returns <- rnorm(n_est, sd = 0.01)
  firm_returns <- 0.001 + 1.2 * index_returns + rnorm(n_est, sd = 0.005)
  na_days <- seq(3, n_est, by = 3)
  firm_returns[na_days] <- NA_real_
  # Index outliers ONLY on the NA-firm days -- if fit() computed x_bar/ss_xx
  # from ALL index values (including these outlier days), beta would be
  # biased away from what lm() on the complete-pair subset produces.
  index_returns[na_days] <- index_returns[na_days] + 0.2

  d <- tibble::tibble(
    firm_returns = firm_returns,
    index_returns = index_returns,
    estimation_window = 1L,
    event_window = 0L,
    relative_index = seq(-n_est, -1)
  )
  d <- dplyr::bind_rows(d, tibble::tibble(
    firm_returns = NA_real_, index_returns = 0.001,
    estimation_window = 0L, event_window = 1L, relative_index = 0L
  ))

  model <- RollingWindowModel$new(window_size = 150, min_obs = 30)
  expect_no_warning(model$fit(d))
  expect_true(model$is_fitted)

  complete <- is.finite(firm_returns) & is.finite(index_returns)
  fit_lm <- lm(firm_returns[complete] ~ index_returns[complete])

  expect_equal(unname(model$statistics$alpha), unname(coef(fit_lm)[1]), tolerance = 1e-10)
  expect_equal(unname(model$statistics$beta), unname(coef(fit_lm)[2]), tolerance = 1e-10)
  expect_equal(model$statistics$sigma, summary(fit_lm)$sigma, tolerance = 1e-10)
  expect_equal(model$statistics$degree_of_freedom, sum(complete) - 2)
  expect_equal(model$statistics$n_params, 2)
})


# --- A2 ----------------------------------------------------------------

test_that("A2: all-NA event excluded identically across CSectT/Patell/BMP/Sign/GSign/KP", {
  fx6 <- .me_fixture(event_ids = 1:6, all_na_event_ids = 6, seed = 2001)
  fx5 <- list(
    data = dplyr::filter(fx6$data, event_id != 6),
    model = dplyr::filter(fx6$model, event_id != 6)
  )

  checks <- list(
    list(cls = CSectTTest, col = "caar_t"),
    list(cls = PatellZTest, col = "caar_z"),
    list(cls = BMPTest, col = "cbmp_t"),
    list(cls = SignTest, col = "csign_z"),
    list(cls = GeneralizedSignTest, col = "cgsign_z"),
    list(cls = KolariPynnonenTest, col = "ckp_t")
  )

  for (chk in checks) {
    out <- .count_warnings(chk$cls$new()$compute(fx6$data, fx6$model))
    expect_equal(out$n, 1, info = chk$col)
    r5 <- chk$cls$new()$compute(fx5$data, fx5$model)
    expect_equal(out$result[[chk$col]], r5[[chk$col]], info = chk$col)
  }
})

test_that("A2: strict mode errors naming the excluded event_id", {
  fx6 <- .me_fixture(event_ids = 1:6, all_na_event_ids = 6, seed = 2002)
  withr::local_options(EventStudy.degenerate_handling = "strict")
  expect_error(CSectTTest$new()$compute(fx6$data, fx6$model), "6")
})

test_that("A2: pipeline excludes an unfitted model's event without a second warning", {
  symbols <- paste0("FIRM_", LETTERS[1:4])
  firm_data <- create_mock_firm_data(symbols = symbols)
  index_data <- create_mock_index_data()
  request <- create_mock_request(firm_symbols = symbols)
  firm_data$adjusted[firm_data$symbol == "FIRM_D"] <- NA_real_

  task <- EventStudyTask$new(firm_data, index_data, request)
  ps <- ParameterSet$new(
    multi_event_statistics = MultiEventStatisticsSet$new(list(
      CSectTTest$new(), KolariPynnonenTest$new(), PatellZTest$new()
    ))
  )

  out <- .count_warnings(run_event_study(task, ps))
  expect_equal(out$n, 1)
  task_full <- out$result

  symbols3 <- symbols[1:3]
  firm_data3 <- create_mock_firm_data(symbols = symbols3)
  request3 <- create_mock_request(firm_symbols = symbols3)
  task3 <- EventStudyTask$new(firm_data3, index_data, request3)
  task3 <- suppressWarnings(run_event_study(task3, ps))

  cs_full <- task_full$aar_caar_tbl$CSectT[[1]]
  cs_3    <- task3$aar_caar_tbl$CSectT[[1]]
  last_full <- nrow(cs_full)
  last_3    <- nrow(cs_3)
  expect_equal(cs_full$caar_t[last_full], cs_3$caar_t[last_3])

  kp_full <- task_full$aar_caar_tbl$KP[[1]]
  kp_3    <- task3$aar_caar_tbl$KP[[1]]
  expect_equal(kp_full$ckp_t[nrow(kp_full)], kp_3$ckp_t[nrow(kp_3)])

  pz_full <- task_full$aar_caar_tbl$PatellZ[[1]]
  pz_3    <- task3$aar_caar_tbl$PatellZ[[1]]
  expect_equal(pz_full$caar_z[nrow(pz_full)], pz_3$caar_z[nrow(pz_3)])
})

test_that("A2: STATS-03 partial gap still contributes 0 to that event's CAR (unchanged)", {
  fx <- .me_fixture(event_ids = 1:3, seed = 2003, n_est = 15, event_window = -2:2)
  fx$data$abnormal_returns[fx$data$event_id == 1 & fx$data$relative_index == 0] <- NA_real_

  result <- CSectTTest$new()$compute(fx$data, fx$model)

  # STATS-03: the missing AR contributes 0 to THAT EVENT's own CAR (used for
  # the caar_t denominator, sd_caar) -- the aar/caar POINT ESTIMATE itself
  # uses na.rm=TRUE across events (unaffected by this convention). Hand-
  # replicate sd_caar's per-event coalesced-to-0 cumsum and confirm caar_t
  # (not caar) matches -- i.e. the convention is unchanged by the A2 fix.
  ew <- fx$data %>% dplyr::filter(event_window == 1)
  aar_hand <- ew %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(aar = mean(abnormal_returns, na.rm = TRUE),
                      n_valid_events = sum(!is.na(abnormal_returns)), .groups = "drop") %>%
    dplyr::mutate(caar = cumsum(dplyr::coalesce(aar, 0)))
  sd_caar_hand <- ew %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(car = cumsum(dplyr::coalesce(abnormal_returns, 0))) %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(sd_caar = sd(car, na.rm = TRUE), .groups = "drop")
  hand <- aar_hand %>%
    dplyr::left_join(sd_caar_hand, by = "relative_index") %>%
    dplyr::mutate(caar_t_hand = sqrt(n_valid_events) * caar / sd_caar) %>%
    dplyr::arrange(relative_index)

  result <- dplyr::arrange(result, relative_index)
  expect_equal(result$caar_t, hand$caar_t_hand, tolerance = 1e-12)
})


# --- A3 ----------------------------------------------------------------

test_that("A3: KP excludes a constant-SAR event from r_bar and uses per-day n_valid_events", {
  n_est <- 30
  event_window <- -5:5
  n_ew <- length(event_window)
  set.seed(3001)
  base <- rnorm(n_est, sd = 0.01)

  ar_gen <- function(eid) {
    if (eid <= 5) {
      est <- base + rnorm(n_est, sd = 0.002)
    } else {
      est <- rep(0.005, n_est)  # constant SAR -> sd == 0, excluded from r_bar
    }
    list(est = est, evt = rnorm(n_ew, sd = 0.01))
  }
  sigma_by_event <- as.list(setNames(c(rep(0.01, 5), 0.02), as.character(1:6)))

  fx <- .me_fixture(event_ids = 1:6, n_est = n_est, event_window = event_window,
                     seed = 3002, ar_gen = ar_gen, sigma_by_event = sigma_by_event)

  out <- .count_warnings(KolariPynnonenTest$new()$compute(fx$data, fx$model))
  expect_equal(out$n, 1)
  result <- out$result

  bmp_result <- BMPTest$new()$compute(fx$data, fx$model)
  expect_false(isTRUE(all.equal(result$kp_t, bmp_result$bmp_t)))

  est_sar <- fx$data %>%
    dplyr::filter(estimation_window == 1, event_id <= 5) %>%
    dplyr::left_join(tibble::tibble(event_id = 1:5, sigma = 0.01), by = "event_id") %>%
    dplyr::mutate(sar = abnormal_returns / sigma) %>%
    dplyr::select(relative_index, event_id, sar) %>%
    tidyr::pivot_wider(names_from = event_id, values_from = sar)
  cor_mat <- stats::cor(as.matrix(est_sar[, -1]))
  r5 <- (sum(cor_mat) - 5) / (5 * 4)

  n_t <- result$n_valid_events
  expect_true(all(n_t == 6))
  expected_kp_t <- bmp_result$bmp_t * sqrt((1 - r5) / (1 + (n_t - 1) * r5))
  expect_equal(result$kp_t, expected_kp_t, tolerance = 1e-8)
})

test_that("A3: KP sets kp_t/ckp_t to NA with a single usable event (never falls back to 1)", {
  n_est <- 30
  event_window <- -2:2
  ar_gen <- function(eid) {
    est <- if (eid == 1) rnorm(n_est, sd = 0.01) else rep(0.003, n_est)
    list(est = est, evt = rnorm(length(event_window), sd = 0.01))
  }
  sigma_by_event <- as.list(setNames(c(0.01, 0.02), c("1", "2")))
  fx <- .me_fixture(event_ids = 1:2, n_est = n_est, event_window = event_window,
                     seed = 3011, ar_gen = ar_gen, sigma_by_event = sigma_by_event)

  out <- .count_warnings(KolariPynnonenTest$new()$compute(fx$data, fx$model))
  expect_equal(out$n, 1)
  expect_true(all(is.na(out$result$kp_t)))
  expect_true(all(is.na(out$result$ckp_t)))
})

test_that("A3: KP per-day adjustment uses that day's n_valid_events", {
  n_est <- 30
  event_window <- -3:3
  n_ew <- length(event_window)
  set.seed(3020)
  base <- rnorm(n_est, sd = 0.01)
  ar_gen <- function(eid) list(est = base + rnorm(n_est, sd = 0.002),
                                evt = rnorm(n_ew, sd = 0.01))
  fx <- .me_fixture(event_ids = 1:5, n_est = n_est, event_window = event_window,
                     seed = 3021, ar_gen = ar_gen)
  fx$data$abnormal_returns[fx$data$event_id == 1 & fx$data$relative_index == 0] <- NA_real_

  result <- suppressWarnings(KolariPynnonenTest$new()$compute(fx$data, fx$model))
  day0 <- result[result$relative_index == 0, ]
  expect_equal(day0$n_valid_events, 4)

  bmp_result <- suppressWarnings(BMPTest$new()$compute(fx$data, fx$model))
  est_sar <- fx$data %>%
    dplyr::filter(estimation_window == 1) %>%
    dplyr::left_join(tibble::tibble(event_id = 1:5, sigma = 0.01), by = "event_id") %>%
    dplyr::mutate(sar = abnormal_returns / sigma) %>%
    dplyr::select(relative_index, event_id, sar) %>%
    tidyr::pivot_wider(names_from = event_id, values_from = sar)
  cor_mat <- stats::cor(as.matrix(est_sar[, -1]))
  r_bar <- (sum(cor_mat) - 5) / (5 * 4)
  expected <- bmp_result$bmp_t[bmp_result$relative_index == 0] *
    sqrt((1 - r_bar) / (1 + (4 - 1) * r_bar))
  expect_equal(day0$kp_t, expected, tolerance = 1e-8)
})


# --- A4 ----------------------------------------------------------------

test_that("A4: RankTest and GeneralizedSignTest group by event_id, not firm_symbol", {
  fx_distinct <- .me_fixture(event_ids = 1:4, firm_symbols = paste0("F", 1:4),
                              seed = 4001, n_est = 20, event_window = -3:3)
  fx_recur <- fx_distinct
  fx_recur$data <- fx_recur$data %>%
    dplyr::mutate(firm_symbol = c("F1", "F1", "F2", "F2")[match(event_id, 1:4)])

  r_rank_distinct <- RankTest$new()$compute(fx_distinct$data, fx_distinct$model)
  r_rank_recur    <- RankTest$new()$compute(fx_recur$data, fx_recur$model)
  expect_equal(r_rank_distinct$rank_z, r_rank_recur$rank_z)

  r_gsign_distinct <- GeneralizedSignTest$new()$compute(fx_distinct$data, fx_distinct$model)
  r_gsign_recur    <- GeneralizedSignTest$new()$compute(fx_recur$data, fx_recur$model)
  expect_equal(r_gsign_distinct$gsign_z, r_gsign_recur$gsign_z)
  expect_equal(r_gsign_distinct$cgsign_z, r_gsign_recur$cgsign_z)
})


# --- A5 ----------------------------------------------------------------

test_that("A5: cross_sectional_regression errors on duplicate event_id in data", {
  task <- create_fitted_mock_task(n_firms = 4)
  data <- tibble::tibble(event_id = c(1, 1, 2, 3), x = rnorm(4))
  err <- tryCatch(cross_sectional_regression(task, ~x, data), error = function(e) e)
  expect_true(inherits(err, "eventstudy_error_bad_argument"))
  expect_match(conditionMessage(err), "1")
})

test_that("A5: cross_sectional_regression warns once on unmatched task events", {
  task <- create_fitted_mock_task(n_firms = 4)
  data <- tibble::tibble(event_id = 1:2, x = rnorm(2))
  # robust = FALSE: with n_obs == n_params (2 events, intercept + x), HC1
  # is singular and sandwich emits its own (unrelated) warning -- robust
  # SEs are irrelevant to what this test checks (the unmatched-event warning).
  expect_warning(result <- cross_sectional_regression(task, ~x, data, robust = FALSE),
                  "task event")
  expect_equal(result$n_obs, 2)
})

test_that("A5: cross_sectional_regression errors when car_window exceeds the event window", {
  task <- create_fitted_mock_task(n_firms = 4)
  data <- tibble::tibble(event_id = 1:4, x = rnorm(4))
  err <- tryCatch(
    cross_sectional_regression(task, ~x, data, car_window = c(-20, 2)),
    error = function(e) e
  )
  expect_true(inherits(err, "eventstudy_error_bad_argument"))
})

test_that("A5: cross_sectional_regression excludes an event with a missing AR inside car_window", {
  task <- create_fitted_mock_task(n_firms = 4)
  ew_rows <- which(task$data_tbl$data[[1]]$event_window == 1)
  task$data_tbl$data[[1]]$abnormal_returns[ew_rows[1]] <- NA_real_
  data <- tibble::tibble(event_id = 1:4, x = rnorm(4))

  expect_warning(result <- cross_sectional_regression(task, ~x, data), "missing")
  expect_equal(result$n_obs, 3)
  expect_false(1 %in% result$car_data$event_id)
})


# --- A6 / A8 -------------------------------------------------------------

test_that("A6: MarketModel routes insufficient obs (n_valid < n_params + 1) through the contract", {
  d2 <- create_mock_model_data(n_estimation = 2)
  m <- MarketModel$new()
  expect_warning(m$fit(d2), "insufficient estimation observations")
  expect_false(m$is_fitted)

  m_strict <- MarketModel$new()
  m_strict$degenerate_mode <- "strict"
  expect_error(m_strict$fit(d2))
})

test_that("A6: MarketModel with 20 valid obs fits and warns about the short window", {
  d20 <- create_mock_model_data(n_estimation = 20)
  m <- MarketModel$new()
  expect_warning(m$fit(d20), "estimation window has only 20 valid observations")
  expect_true(m$is_fitted)
})

test_that("A6: MarketModel with 120 obs fits without warning", {
  d120 <- create_mock_model_data(n_estimation = 120)
  m <- MarketModel$new()
  expect_no_warning(m$fit(d120))
  expect_true(m$is_fitted)
})

test_that("A6: FamaFrench3FactorModel routes insufficient obs through the contract", {
  d4 <- create_mock_factor_model_data(n_estimation = 4)
  m <- FamaFrench3FactorModel$new()
  expect_warning(m$fit(d4), "insufficient estimation observations")
  expect_false(m$is_fitted)
})

test_that("A6: MarketModel FEC uses complete pairs only", {
  set.seed(6001)
  n_est <- 100
  index_returns <- rnorm(n_est, sd = 0.01)
  firm_returns <- 0.001 + 1.1 * index_returns + rnorm(n_est, sd = 0.005)
  extreme <- order(abs(index_returns), decreasing = TRUE)[1:10]
  firm_returns[extreme] <- NA_real_
  n_event <- 5
  event_index <- rnorm(n_event, sd = 0.01)
  d <- tibble::tibble(
    firm_returns = c(firm_returns, rep(NA_real_, n_event)),
    index_returns = c(index_returns, event_index),
    estimation_window = c(rep(1L, n_est), rep(0L, n_event)),
    event_window = c(rep(0L, n_est), rep(1L, n_event)),
    relative_index = c(seq(-n_est, -1), seq(0, n_event - 1))
  )
  m <- MarketModel$new()
  expect_no_warning(m$fit(d))

  complete <- !is.na(firm_returns) & !is.na(index_returns)
  meanRm <- mean(index_returns[complete])
  ssRm <- sum((index_returns[complete] - meanRm)^2)
  expected_fec <- m$statistics$sigma *
    sqrt(1 + 1 / sum(complete) + (event_index - meanRm)^2 / ssRm)
  expect_equal(m$statistics$forecast_error_corrected_sigma, expected_fec, tolerance = 1e-12)
})

test_that("A6/A8: statistics$n_params matches the documented value per model class", {
  d <- create_mock_model_data(n_estimation = 120)
  fd <- create_mock_factor_model_data(n_estimation = 120)

  mm <- MarketModel$new(); mm$fit(d)
  expect_equal(mm$statistics$n_params, 2)

  ff3 <- FamaFrench3FactorModel$new(); ff3$fit(fd)
  expect_equal(ff3$statistics$n_params, 4)

  carhart <- Carhart4FactorModel$new(); carhart$fit(fd)
  expect_equal(carhart$statistics$n_params, 5)

  ff5 <- FamaFrench5FactorModel$new(); ff5$fit(fd)
  expect_equal(ff5$statistics$n_params, 6)

  cpma <- ComparisonPeriodMeanAdjustedModel$new(); cpma$fit(d)
  expect_equal(cpma$statistics$n_params, 1)

  ma <- MarketAdjustedModel$new(); ma$fit(d)
  expect_equal(ma$statistics$n_params, 0)
})

test_that("A6: PatellZTest aar_z uses m = valid estimation obs and k = model n_params", {
  fx <- .me_fixture(event_ids = 1:3, n_est = 30, event_window = -3:3, seed = 6010,
                     n_params_by_event = as.list(setNames(rep(4, 3), as.character(1:3))))
  na_rows <- which(fx$data$event_id == 1 & fx$data$estimation_window == 1)[1:10]
  fx$data$abnormal_returns[na_rows] <- NA_real_

  result <- PatellZTest$new()$compute(fx$data, fx$model)

  sd_asar <- fx$data %>%
    dplyr::filter(estimation_window == 1) %>%
    dplyr::group_by(event_id) %>%
    dplyr::summarise(m = sum(is.finite(abnormal_returns)), .groups = "drop") %>%
    dplyr::mutate(k = 4, Q_i = ifelse(m > k + 2, (m - k) / (m - k - 2), 1))
  Q_total <- sqrt(sum(sd_asar$Q_i))

  sigma_by_event <- purrr::map_dbl(fx$model$model, ~.x$statistics$sigma)
  evt <- fx$data %>%
    dplyr::filter(event_window == 1) %>%
    dplyr::left_join(tibble::tibble(event_id = fx$model$event_id, sigma = sigma_by_event),
                      by = "event_id") %>%
    dplyr::mutate(sar = abnormal_returns / sigma)
  hand <- evt %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(sum_sar = sum(sar, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(aar_z = sum_sar / Q_total) %>%
    dplyr::arrange(relative_index)

  result <- dplyr::arrange(result, relative_index)
  expect_equal(result$aar_z, hand$aar_z, tolerance = 1e-10)
})


# --- A7 ----------------------------------------------------------------

test_that("A7: caltime_t doubles when the event-day shock doubles; df is estimation-window based", {
  n_events <- 20
  n_est <- 40
  event_window <- -2:2
  shock <- 0.01

  build_fixture <- function(shock_val) {
    purrr::map_dfr(seq_len(n_events), function(i) {
      ar_est <- rnorm(n_est, sd = 0.01)
      ar_evt <- rnorm(length(event_window), sd = 0.01)
      ar_evt[event_window == 0] <- shock_val
      tibble::tibble(
        event_id = i, firm_symbol = paste0("F", i), group = "g",
        relative_index = c(seq(-(n_est + 5), -6), event_window),
        estimation_window = c(rep(1L, n_est), rep(0L, length(event_window))),
        event_window = c(rep(0L, n_est), rep(1L, length(event_window))),
        abnormal_returns = c(ar_est, ar_evt)
      )
    })
  }
  set.seed(7002); d1 <- build_fixture(shock)
  set.seed(7002); d2 <- build_fixture(2 * shock)
  model_tbl <- tibble::tibble(event_id = 1:n_events, model = vector("list", n_events))

  r1 <- CalendarTimePortfolioTest$new()$compute(d1, model_tbl)
  r2 <- CalendarTimePortfolioTest$new()$compute(d2, model_tbl)

  day0_1 <- r1$caltime_t[r1$relative_index == 0]
  day0_2 <- r2$caltime_t[r2$relative_index == 0]
  expect_equal(day0_2, 2 * day0_1, tolerance = 1e-10)

  est_aar <- d1 %>%
    dplyr::filter(estimation_window == 1) %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(aar = mean(abnormal_returns), .groups = "drop")
  ts_sd <- sd(est_aar$aar)
  n_estimation_days <- nrow(est_aar)
  expect_equal(attr(r1, "caltime_df"), n_estimation_days - 1)

  evt_aar <- d1 %>%
    dplyr::filter(event_window == 1, relative_index == 0) %>%
    dplyr::summarise(aar = mean(abnormal_returns)) %>%
    dplyr::pull(aar)
  expect_equal(day0_1, evt_aar / ts_sd, tolerance = 1e-10)

  task_stub <- structure(
    list(aar_caar_tbl = tibble::tibble(group = "g", CalTimeT = list(r1))),
    class = "EventStudyTask"
  )
  p <- adjust_p_values(task_stub, stat_name = "CalTimeT", method = "none")
  expected_p <- 2 * stats::pt(-abs(r1$caltime_t), df = n_estimation_days - 1)
  expect_equal(p$p_raw_aar, expected_p, tolerance = 1e-10)
})


# --- A8 (also covered by the A6/A8 n_params test above) -----------------

test_that("A8: MarketAdjustedModel FEC sigma equals sigma exactly (correction factor 1)", {
  d <- create_mock_model_data(n_estimation = 120)
  m <- MarketAdjustedModel$new()
  m$fit(d)
  n_event <- sum(d$event_window == 1)
  expect_equal(m$statistics$forecast_error_corrected_sigma, rep(m$statistics$sigma, n_event))

  cpma <- ComparisonPeriodMeanAdjustedModel$new()
  cpma$fit(d)
  n_valid_fec <- sum(!is.na(d$firm_returns[d$estimation_window == 1]))
  expect_equal(
    cpma$statistics$forecast_error_corrected_sigma,
    rep(cpma$statistics$sigma * sqrt(1 + 1 / n_valid_fec), n_event),
    tolerance = 1e-12
  )
})


# --- A9 ----------------------------------------------------------------

test_that("A9: n_pos counts strictly positive ARs; a zero AR is counted in n_neg", {
  fx <- .me_fixture(event_ids = 1:4, n_est = 20, event_window = -1:1, seed = 9001)
  fx$data$abnormal_returns[fx$data$event_id == 1 & fx$data$relative_index == 0] <- 0

  r_cs <- CSectTTest$new()$compute(fx$data, fx$model)
  r_patell <- PatellZTest$new()$compute(fx$data, fx$model)
  model_tbl2 <- tibble::tibble(event_id = 1:4, model = vector("list", 4))
  r_cal <- CalendarTimePortfolioTest$new()$compute(fx$data, model_tbl2)

  ars_day0 <- fx$data$abnormal_returns[fx$data$event_window == 1 & fx$data$relative_index == 0]
  expect_equal(r_cs$n_pos[r_cs$relative_index == 0], sum(ars_day0 > 0))
  expect_equal(r_cs$n_neg[r_cs$relative_index == 0], sum(ars_day0 <= 0))
  expect_equal(r_patell$n_pos[r_patell$relative_index == 0], sum(ars_day0 > 0))
  expect_equal(r_patell$n_neg[r_patell$relative_index == 0], sum(ars_day0 <= 0))
  expect_equal(r_cal$n_pos[r_cal$relative_index == 0], sum(ars_day0 > 0))
  expect_equal(r_cal$n_neg[r_cal$relative_index == 0], sum(ars_day0 <= 0))
})


# --- A10 -----------------------------------------------------------------

test_that("A10: TestStatisticBase validates confidence_type and warns exactly once when non-default", {
  expect_error(CSectTTest$new(confidence_type = "bogus"))

  expect_warning(t1 <- CSectTTest$new(confidence_type = "less"), "currently ignored")
  expect_equal(t1$confidence_type, "less")

  expect_warning(t2 <- CSectTTest$new(confidence_type = "greater"), "currently ignored")
  expect_equal(t2$confidence_type, "greater")

  expect_no_warning(CSectTTest$new())
  expect_no_warning(CSectTTest$new(confidence_type = "two-sided"))
})


# --- A12 -----------------------------------------------------------------

test_that("A12: (a) bootstrap observed_caar matches the mean of per-event CAR; equals cumsum(aar) when NA-free", {
  task <- create_fitted_mock_task(n_firms = 4)
  ew_rows <- which(task$data_tbl$data[[1]]$event_window == 1)
  task$data_tbl$data[[1]]$abnormal_returns[ew_rows[1]] <- NA_real_
  result <- bootstrap_test(task, n_boot = 5, seed = 42)

  ar_data <- task$data_tbl %>%
    dplyr::select(event_id, firm_symbol, data) %>%
    tidyr::unnest(data) %>%
    dplyr::filter(event_window == 1) %>%
    dplyr::select(event_id, relative_index, abnormal_returns)
  hand <- ar_data %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(car = cumsum(dplyr::coalesce(abnormal_returns, 0))) %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(observed_caar = mean(car), .groups = "drop") %>%
    dplyr::arrange(relative_index)

  result <- dplyr::arrange(result, relative_index)
  expect_equal(result$observed_caar, hand$observed_caar, tolerance = 1e-12)

  task2 <- create_fitted_mock_task(n_firms = 4)
  result2 <- bootstrap_test(task2, n_boot = 5, seed = 42)
  result2 <- dplyr::arrange(result2, relative_index)
  expect_equal(result2$observed_caar, cumsum(result2$observed_aar), tolerance = 1e-12)
})

test_that("A12: (b) NA bootstrap draws are excluded from the p-value denominator", {
  set.seed(1234)
  n_est <- 30
  event_window <- 0

  build_event <- function(eid, ar_k) {
    tibble::tibble(
      event_id = eid, firm_symbol = paste0("F", eid), group = "g",
      relative_index = c(seq(-(n_est + 5), -6), event_window),
      estimation_window = c(rep(1L, n_est), 0L),
      event_window = c(rep(0L, n_est), 1L),
      abnormal_returns = c(rnorm(n_est, sd = 0.01), ar_k)
    )
  }
  d <- dplyr::bind_rows(build_event(1, 0.01), build_event(2, -0.01))
  nested <- d %>%
    dplyr::group_by(event_id, firm_symbol, group) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  task <- structure(list(data_tbl = nested), class = "EventStudyTask")

  result <- bootstrap_test(task, n_boot = 200, weight_type = "rademacher", seed = 55)
  expect_equal(result$boot_p_aar[result$relative_index == 0], 1)
})
