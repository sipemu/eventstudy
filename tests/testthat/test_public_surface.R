# Evaluated (non eval=FALSE, non-skipped) tests exercising the public
# extension surface added/locked by the 2026-09-24 re-evaluation (B1, B5):
#   - ModelBase, TestStatisticBase, ReturnCalculation are exported and
#     subclassable end-to-end through the real pipeline.
#   - generics::tidy(task) dispatches to tidy.EventStudyTask().

test_that("B1: ModelBase, TestStatisticBase, ReturnCalculation are exported", {
  exports <- getNamespaceExports("EventStudy")
  expect_true("ModelBase" %in% exports)
  expect_true("TestStatisticBase" %in% exports)
  expect_true("ReturnCalculation" %in% exports)
})

test_that("B1: a ModelBase subclass fits and produces abnormal returns matching a direct computation", {
  # Minimal constant-mean demo model: expected return is the estimation-window
  # mean firm return (same shape as ComparisonPeriodMeanAdjustedModel, kept
  # deliberately simple here to exercise only the documented ModelBase contract).
  ConstantMeanDemoModel <- R6::R6Class(
    "ConstantMeanDemoModel",
    inherit = ModelBase,
    public = list(
      model_name = "ConstantMeanDemoModel",
      fit = function(data_tbl) {
        est <- data_tbl[data_tbl$estimation_window == 1, ]
        ref_mean <- mean(est$firm_returns, na.rm = TRUE)
        residuals <- est$firm_returns - ref_mean
        m <- sum(!is.na(est$firm_returns))
        sigma <- sd(residuals, na.rm = TRUE)

        private$.fitted_model <- ref_mean
        private$.is_fitted <- TRUE
        private$.statistics$sigma <- sigma
        private$.statistics$degree_of_freedom <- m - 1
        private$.statistics$residuals <- residuals
        # n_params = 1: the estimation-window mean is the one estimated
        # parameter (documented optional statistics$n_params field, A6/B1).
        private$.statistics$n_params <- 1

        n_event <- sum(data_tbl$event_window == 1)
        private$.statistics$forecast_error_corrected_sigma <-
          rep(sigma * sqrt(1 + 1 / m), n_event)
        private$.statistics$forecast_error_corrected_sigma_car <- rep(0, n_event)

        invisible(self)
      },
      abnormal_returns = function(data_tbl) {
        dplyr::mutate(data_tbl, abnormal_returns = firm_returns - private$.fitted_model)
      }
    )
  )

  task <- create_mock_task(n_firms = 2)
  ps <- ParameterSet$new(return_model = ConstantMeanDemoModel$new())
  task <- run_event_study(task, ps)

  for (i in seq_len(nrow(task$data_tbl))) {
    raw <- task$data_tbl$data[[i]]
    est <- raw[raw$estimation_window == 1, ]
    ref_mean <- mean(est$firm_returns, na.rm = TRUE)
    expected_ar <- raw$firm_returns - ref_mean

    expect_equal(raw$abnormal_returns, expected_ar, tolerance = 1e-12)

    model_i <- task$data_tbl$model[[i]]
    expect_true(model_i$is_fitted)
    expect_identical(model_i$statistics$n_params, 1)
  }
})

test_that("B1: a TestStatisticBase subclass computes multi-event results matching a direct computation", {
  # Minimal multi-event statistic: mean AR per relative day (no t-stat), to
  # exercise the documented TestStatisticBase compute(data_tbl, model) contract
  # for the multi-event dispatch path.
  MeanARTest <- R6::R6Class(
    "MeanARTest",
    inherit = TestStatisticBase,
    public = list(
      name = "MeanAR",
      compute = function(data_tbl, model) {
        data_tbl %>%
          dplyr::filter(event_window == 1) %>%
          dplyr::group_by(relative_index) %>%
          dplyr::summarise(mean_ar = mean(abnormal_returns, na.rm = TRUE),
                            .groups = "drop")
      }
    )
  )

  task <- create_mock_task(n_firms = 3, group = "TestGroup")
  ps <- ParameterSet$new(
    single_event_statistics = NULL,
    multi_event_statistics = StatisticsSetBase$new(list(MeanARTest$new()))
  )
  task <- run_event_study(task, ps)

  expect_true("MeanAR" %in% names(task$aar_caar_tbl))

  # Direct computation from the task's raw event-window data, independent of
  # the custom test statistic class.
  raw <- task$data_tbl %>%
    dplyr::select(task$.keys, data) %>%
    tidyr::unnest(data) %>%
    dplyr::filter(event_window == 1) %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(mean_ar = mean(abnormal_returns, na.rm = TRUE), .groups = "drop")

  computed <- task$aar_caar_tbl$MeanAR[[1]]
  expect_equal(
    computed[order(computed$relative_index), ]$mean_ar,
    raw[order(raw$relative_index), ]$mean_ar,
    tolerance = 1e-12
  )
})

test_that("B1: a ReturnCalculation subclass produces output identical to SimpleReturn", {
  DemoSimpleReturn <- R6::R6Class(
    "DemoSimpleReturn",
    inherit = ReturnCalculation,
    public = list(
      name = "demo simple return",
      calculate_return = function(tbl, in_column = "adjusted", out_column = "adjusted_return") {
        tbl %>%
          dplyr::mutate(!!rlang::sym(out_column) := {
            price <- !!rlang::sym(in_column)
            lagged <- dplyr::lag(price)
            ifelse(is.finite(lagged) & lagged != 0,
                   (price - lagged) / lagged,
                   NA_real_)
          })
      }
    )
  )

  set.seed(3001)
  prices_tbl <- tibble::tibble(
    symbol = "FIRM_A",
    date = as.character(seq(as.Date("2024-01-01"), by = "day", length.out = 30)),
    adjusted = 100 * cumprod(1 + rnorm(30, 0, 0.01))
  )

  demo_result <- DemoSimpleReturn$new()$calculate_return(prices_tbl)
  real_result <- SimpleReturn$new()$calculate_return(prices_tbl)

  expect_identical(demo_result, real_result)
})

test_that("B5: generics::tidy(task) dispatches to tidy.EventStudyTask()", {
  skip_if_not_installed("generics")

  task <- create_fitted_mock_task()

  direct <- tidy.EventStudyTask(task)
  via_generic <- generics::tidy(task)

  expect_identical(via_generic, direct)
})
