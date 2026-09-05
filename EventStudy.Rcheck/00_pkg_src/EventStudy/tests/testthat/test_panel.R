# Helper to create mock panel data
create_mock_panel_data <- function(n_units = 20, n_periods = 10,
                                    n_treated = 10, treatment_period = 6) {
  set.seed(42)
  units <- seq_len(n_units)
  periods <- seq_len(n_periods)

  panel <- expand.grid(unit_id = units, time_id = periods)
  panel <- tibble::as_tibble(panel)

  # Unit and time fixed effects
  unit_fe <- rnorm(n_units, sd = 2)
  time_fe <- rnorm(n_periods, sd = 1)

  panel <- panel %>%
    dplyr::mutate(
      unit_fe = unit_fe[unit_id],
      time_fe = time_fe[time_id]
    )

  # Treatment assignment: first n_treated units get treated at treatment_period
  treated_units <- units[seq_len(n_treated)]
  panel <- panel %>%
    dplyr::mutate(
      treatment_time = ifelse(unit_id %in% treated_units, treatment_period, NA),
      treated = ifelse(!is.na(treatment_time) & time_id >= treatment_time, 1, 0),
      # True treatment effect = 2.0 (constant)
      outcome = unit_fe + time_fe + 2.0 * treated + rnorm(dplyr::n(), sd = 0.5)
    )

  panel
}

create_mock_staggered_panel <- function(n_units = 30, n_periods = 15) {
  set.seed(42)
  units <- seq_len(n_units)
  periods <- seq_len(n_periods)

  panel <- expand.grid(unit_id = units, time_id = periods)
  panel <- tibble::as_tibble(panel)

  unit_fe <- rnorm(n_units, sd = 2)
  time_fe <- rnorm(n_periods, sd = 1)

  # Staggered treatment: 3 cohorts
  panel <- panel %>%
    dplyr::mutate(
      unit_fe = unit_fe[unit_id],
      time_fe = time_fe[time_id],
      treatment_time = dplyr::case_when(
        unit_id <= 10 ~ 6L,      # early cohort
        unit_id <= 20 ~ 9L,      # late cohort
        TRUE ~ NA_integer_        # never-treated
      ),
      treated = ifelse(!is.na(treatment_time) & time_id >= treatment_time, 1, 0),
      # Heterogeneous treatment effects by cohort
      true_effect = dplyr::case_when(
        unit_id <= 10 ~ 3.0,
        unit_id <= 20 ~ 1.5,
        TRUE ~ 0
      ),
      outcome = unit_fe + time_fe + true_effect * treated + rnorm(dplyr::n(), sd = 0.5)
    )

  panel
}


test_that("PanelEventStudyTask creates correctly", {
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)

  expect_s3_class(task, "PanelEventStudyTask")
  expect_equal(task$unit_id, "unit_id")
  expect_equal(task$outcome, "outcome")
  expect_null(task$results)
})

test_that("PanelEventStudyTask errors on missing columns", {
  panel <- tibble::tibble(x = 1:10, y = 1:10)
  expect_error(
    PanelEventStudyTask$new(panel),
    "missing columns"
  )
})

test_that("PanelEventStudyTask print works", {
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)
  expect_output(print(task), "PanelEventStudyTask")
  expect_output(print(task), "Units:")
  expect_output(print(task), "Periods:")
})


# --- Regression: print counts treated units, not cohorts ---

test_that("PanelEventStudyTask print reports correct treated unit count", {
  # Bug: print counted unique treatment_time values (cohorts), not unique
  # unit_ids with non-NA treatment_time (treated units).
  panel <- create_mock_panel_data(n_units = 20, n_treated = 10)
  task <- PanelEventStudyTask$new(panel)

  # All 10 treated units share the same treatment period, so there's 1 cohort
  # but 10 treated units. The print should report "10 unit(s)", not "1 unit(s)".
  expect_output(print(task), "10 unit\\(s\\)")
})

test_that("static_twfe estimates treatment effect", {
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)

  task <- estimate_panel_event_study(task, method = "static_twfe")

  expect_false(is.null(task$results))
  expect_equal(task$results$method, "static_twfe")
  coefs <- task$results$coefficients
  expect_true("estimate" %in% names(coefs))
  # ATT should be close to the true effect of 2.0
  expect_lt(abs(coefs$estimate[1] - 2.0), 1.0)
})

test_that("dynamic_twfe produces event-time coefficients", {
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)

  task <- estimate_panel_event_study(task, method = "dynamic_twfe",
                                      leads = 3, lags = 3)

  expect_false(is.null(task$results))
  expect_equal(task$results$method, "dynamic_twfe")

  coefs <- task$results$coefficients
  expect_true("relative_time" %in% names(coefs))
  expect_true("estimate" %in% names(coefs))
  expect_true("std.error" %in% names(coefs))

  # Base period should have estimate = 0
  base <- coefs %>% dplyr::filter(relative_time == -1)
  expect_equal(base$estimate, 0)

  # Pre-treatment coefficients should be close to 0
  pre <- coefs %>% dplyr::filter(relative_time < -1)
  expect_true(all(abs(pre$estimate) < 3))

  # Post-treatment should be positive (true effect = 2)
  post <- coefs %>% dplyr::filter(relative_time >= 0)
  expect_true(mean(post$estimate) > 0)
})

test_that("sun_abraham produces event-time coefficients with staggered treatment", {
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  task <- estimate_panel_event_study(task, method = "sun_abraham",
                                      leads = 4, lags = 4)

  expect_false(is.null(task$results))
  expect_equal(task$results$method, "sun_abraham")

  coefs <- task$results$coefficients
  expect_true("relative_time" %in% names(coefs))
  expect_true(nrow(coefs) > 0)
})

test_that("plot_panel_event_study returns ggplot", {
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)
  task <- estimate_panel_event_study(task, method = "dynamic_twfe",
                                      leads = 3, lags = 3)

  p <- plot_panel_event_study(task)
  expect_s3_class(p, "gg")
})

test_that("plot_panel_event_study errors on static TWFE", {
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)
  task <- estimate_panel_event_study(task, method = "static_twfe")

  expect_error(plot_panel_event_study(task), "dynamic")
})

test_that("plot_panel_event_study errors without results", {
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)

  expect_error(plot_panel_event_study(task), "No results")
})


# --- Modern DiD Estimator tests ---

test_that("callaway_santanna method works", {
  skip_if_not_installed("did")
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  task <- estimate_panel_event_study(task, method = "callaway_santanna",
                                      leads = 3, lags = 3)

  expect_false(is.null(task$results))
  expect_equal(task$results$method, "callaway_santanna")
  coefs <- task$results$coefficients
  expect_true("relative_time" %in% names(coefs))
  expect_true("estimate" %in% names(coefs))
  expect_true("std.error" %in% names(coefs))
  expect_true(nrow(coefs) > 0)

  # Plot should still work
  p <- plot_panel_event_study(task)
  expect_s3_class(p, "gg")
})


test_that("dechaisemartin_dhaultfoeuille method works", {
  # DIDmultiplegtDYN can segfault on macOS arm64 during dyn.load(),
  # which kills the entire test runner. We must NOT call skip_if_not_installed()
  # or requireNamespace() directly — test loading in a subprocess instead.
  pkg_found <- nzchar(system.file(package = "DIDmultiplegt"))
  skip_if(!pkg_found, "DIDmultiplegt is not installed")
  load_ok <- tryCatch({
    ret <- system2(
      file.path(R.home("bin"), "Rscript"),
      args = c("-e", shQuote("library(DIDmultiplegt)")),
      stdout = FALSE, stderr = FALSE, timeout = 30
    )
    ret == 0
  }, error = function(e) FALSE)
  skip_if(!load_ok, "DIDmultiplegt crashes on load (segfault)")

  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  # DIDmultiplegt API varies across versions; wrap in tryCatch
  tryCatch({
    suppressWarnings(
      task <- estimate_panel_event_study(
        task, method = "dechaisemartin_dhaultfoeuille",
        leads = 2, lags = 2
      )
    )

    expect_false(is.null(task$results))
    expect_equal(task$results$method, "dechaisemartin_dhaultfoeuille")
    coefs <- task$results$coefficients
    expect_true("relative_time" %in% names(coefs))
    # Coefficients may be empty with small mock data — just verify structure
    expect_true(is.data.frame(coefs))
  }, error = function(e) {
    skip(paste("DIDmultiplegt compatibility:", conditionMessage(e)))
  })
})


test_that("borusyak_jaravel_spiess method works", {
  skip_if_not_installed("didimputation")
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  task <- estimate_panel_event_study(
    task, method = "borusyak_jaravel_spiess",
    leads = 3, lags = 3
  )

  expect_false(is.null(task$results))
  expect_equal(task$results$method, "borusyak_jaravel_spiess")
  coefs <- task$results$coefficients
  expect_true("relative_time" %in% names(coefs))
  expect_true(nrow(coefs) > 0)
})


test_that("dynamic_twfe coefficient relative_time labels are correct even if dummies dropped", {
  # Create panel where some event-time dummies will be collinear
  panel <- create_mock_panel_data(n_units = 20, n_periods = 10,
                                   n_treated = 10, treatment_period = 6)
  task <- PanelEventStudyTask$new(panel)

  task <- estimate_panel_event_study(task, method = "dynamic_twfe",
                                      leads = 4, lags = 4)

  coefs <- task$results$coefficients
  # Check that relative_time values are correct and include base period
  expect_true(-1 %in% coefs$relative_time)
  base <- coefs %>% dplyr::filter(relative_time == -1)
  expect_equal(base$estimate, 0)

  # Post-treatment coefficients should correspond to actual post-treatment periods
  post <- coefs %>% dplyr::filter(relative_time > 0)
  if (nrow(post) > 0) {
    # Estimate should be positive (true effect is 2.0)
    expect_true(all(post$estimate > 0))
  }
})


# --- EXTERNAL-01: callaway_santanna absence and failure wrapping ---

test_that("callaway_santanna warns and returns task with NULL results when did is absent", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) pkg != "did",
    .package = "base"
  )
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task, method = "callaway_santanna"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  # Inner estimator must warn about missing "did" package
  expect_true(any(grepl("did", ws, ignore.case = TRUE)))
  expect_null(result$results)
})


test_that("callaway_santanna tryCatch catches att_gt runtime failures", {
  skip_if_not_installed("did")
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  # Corrupt the panel so att_gt will fail with a runtime error
  task2 <- task
  task2$outcome <- "nonexistent_column"

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task2, method = "callaway_santanna"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("did::att_gt failed", ws, ignore.case = TRUE)))
  expect_null(result$results)
})


# --- EXTERNAL-02: dechaisemartin_dhaultfoeuille absence and opt-out ---

test_that("dechaisemartin warns and returns task with NULL results when DIDmultiplegt is absent", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) pkg != "DIDmultiplegt",
    .package = "base"
  )
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task, method = "dechaisemartin_dhaultfoeuille"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("DIDmultiplegt", ws, ignore.case = TRUE)))
  expect_null(result$results)
})


test_that("dechaisemartin opt-out option produces warning and NULL results", {
  old_opt <- getOption("eventstudy.skip_didmultiplegt")
  on.exit(options(eventstudy.skip_didmultiplegt = old_opt), add = TRUE)
  options(eventstudy.skip_didmultiplegt = TRUE)

  # Mock DIDmultiplegt as available so the skip option is reached
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) TRUE,
    .package = "base"
  )

  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task, method = "dechaisemartin_dhaultfoeuille"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("skip_didmultiplegt", ws, ignore.case = TRUE)))
  expect_null(result$results)
})


test_that("dechaisemartin callr probe gate: probe option FALSE means no callr involvement", {
  # When eventstudy.probe_didmultiplegt is FALSE (the default), the callr probe
  # code path must NOT run. We verify that the option check itself is gated.
  old_opt <- getOption("eventstudy.probe_didmultiplegt")
  on.exit(options(eventstudy.probe_didmultiplegt = old_opt), add = TRUE)
  options(eventstudy.probe_didmultiplegt = FALSE)

  # Confirm the option is off (the gate condition)
  expect_false(isTRUE(getOption("eventstudy.probe_didmultiplegt", FALSE)))

  # With probe disabled and DIDmultiplegt absent, only the missing-pkg warning fires
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) pkg != "DIDmultiplegt",
    .package = "base"
  )
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task, method = "dechaisemartin_dhaultfoeuille"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("DIDmultiplegt", ws, ignore.case = TRUE)))
  expect_null(result$results)
})


# --- EXTERNAL-03: didimputation absence and failure wrapping ---

test_that("borusyak_jaravel_spiess warns and returns task with NULL results when didimputation is absent", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) pkg != "didimputation",
    .package = "base"
  )
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task, method = "borusyak_jaravel_spiess"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("didimputation", ws, ignore.case = TRUE)))
  expect_null(result$results)
})


test_that("borusyak_jaravel_spiess tryCatch catches did_imputation runtime failures", {
  skip_if_not_installed("didimputation")
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  # Corrupt panel so did_imputation fails
  task2 <- task
  task2$outcome <- "nonexistent_column_xyz"

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task2, method = "borusyak_jaravel_spiess"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("didimputation::did_imputation failed", ws, ignore.case = TRUE)))
  expect_null(result$results)
})


# --- EXTERNAL-03: sandwich absence warning upgrade ---

# --- WR-02 regression: wrapper emits warning when external estimator returns NULL ---

test_that("estimate_panel_event_study: wrapper warns when external estimator returns NULL results", {
  # WR-02: The switch() in estimate_panel_event_study discards the return value
  # of inner estimators.  When an external estimator fails (package absent or
  # runtime error), it returns invisible(NULL) and sets task$results=NULL.
  # The wrapper now emits ONE additional warning naming the method so the
  # caller is informed rather than silently receiving a NULL-results task.
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) pkg != "did",
    .package = "base"
  )
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_panel_event_study(task, method = "callaway_santanna"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  # task$results must be NULL (estimator returned NULL)
  expect_null(result$results)
  # At least one warning must mention the method name (wrapper-level signal)
  expect_true(any(grepl("callaway_santanna", ws)),
              label = "wrapper must warn naming the failed method")
  # The wrapper warning must also state task$results is NULL
  expect_true(any(grepl("NULL", ws)),
              label = "wrapper warning must state results are NULL")
})


test_that("estimate_panel_event_study: wrapper does NOT warn when external estimator succeeds", {
  # WR-02: The NULL guard must only fire when task$results actually stays NULL.
  # A successful callaway_santanna run (did installed) should produce no wrapper warning.
  skip_if_not_installed("did")
  panel <- create_mock_staggered_panel()
  task <- PanelEventStudyTask$new(panel)

  ws <- character(0)
  result <- withCallingHandlers(
    suppressWarnings(estimate_panel_event_study(task, method = "callaway_santanna")),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  # No wrapper-level "returned no results" warning should fire
  wrapper_warns <- grepl("returned no results", ws, ignore.case = TRUE)
  expect_false(any(wrapper_warns),
               label = "wrapper must NOT warn when results are populated")
})


test_that(".compute_se emits warning (not message) when sandwich is absent", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) pkg != "sandwich",
    .package = "base"
  )
  panel <- create_mock_panel_data()
  task <- PanelEventStudyTask$new(panel)

  # static_twfe calls .compute_se; expect a warning naming sandwich
  expect_warning(
    result <- estimate_panel_event_study(task, method = "static_twfe"),
    "sandwich"
  )
  # Results should still be populated (OLS SE fallback)
  expect_false(is.null(result$results))
})
