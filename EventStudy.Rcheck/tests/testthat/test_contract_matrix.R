# test_contract_matrix.R
#
# Table-driven degenerate-input contract matrix (Phase 4, Plan 01).
#
# Exercises every covered component in both strict and lenient modes on >= 1
# degenerate input, asserting the degenerate-input contract (CONTRACT-03/04).
#
# EXCLUDED from contract matrix (not migrated onto .handle_degenerate() this
# milestone):
#   PermutationTest (resampling test — degenerate behavior inherent to
#     permutation arithmetic; covered by test_multi_event_statistics.R +
#     test_edge_cases.R)
#   RankTest (rank-based test — same rationale; covered by
#     test_multi_event_statistics.R)
# Contract does not apply to these two of the 12 stat classes.

# Machine-readable exclusion set (assertable in exhaustiveness test below).
contract_matrix_excluded <- c("PermutationTest", "RankTest")

# ---------------------------------------------------------------------------
# Registry: 25 rows = 14 models + 10 contract-covered stats + 1 pipeline
# Breakdown:
#   Models (14): MarketModel, MarketAdjustedModel,
#     ComparisonPeriodMeanAdjustedModel, CustomModel, LinearFactorModel,
#     FamaFrench3FactorModel, FamaFrench5FactorModel, Carhart4FactorModel,
#     BHARModel, VolumeModel, VolatilityModel, RollingWindowModel,
#     GARCHModel, DCCGARCHModel
#   Stats (10): ARTTest, CARTTest, BHARTTest, CSectTTest, PatellZTest,
#     BMPTest, KolariPynnonenTest, SignTest, GeneralizedSignTest,
#     CalendarTimePortfolioTest
#   Pipeline (1): prepare_event_study missing-date path (PIPELINE-01)
# ---------------------------------------------------------------------------

# Helper: degenerate factor model data (insufficient obs — NA out all but 1
# estimation row from a valid factor tibble).
create_degenerate_factor_model_data_insufficient <- function(n_valid = 1) {
  d <- create_mock_factor_model_data(n_estimation = 120, n_event = 11)
  est_rows <- which(d$estimation_window == 1L)
  if (length(est_rows) > n_valid) {
    rows_to_na <- est_rows[(n_valid + 1):length(est_rows)]
    d$excess_return[rows_to_na]  <- NA_real_
    d$market_excess[rows_to_na] <- NA_real_
    d$smb[rows_to_na]           <- NA_real_
    d$hml[rows_to_na]           <- NA_real_
    d$mom[rows_to_na]           <- NA_real_
    d$rmw[rows_to_na]           <- NA_real_
    d$cma[rows_to_na]           <- NA_real_
    d$firm_returns[rows_to_na]  <- NA_real_
    d$index_returns[rows_to_na] <- NA_real_
  }
  d
}

# Helper: zero-variance factor model data (constant market_excess in estimation
# window so the factor regression is undefined).
create_degenerate_factor_model_data_zero_var <- function() {
  d <- create_mock_factor_model_data(n_estimation = 120, n_event = 11)
  est_rows <- which(d$estimation_window == 1L)
  d$market_excess[est_rows] <- 0.001
  d$excess_return[est_rows] <- 0.001  # keep LHS also constant
  d
}

# Helper: single-event data (one event_id) for stat degenerate rows.
# This is the standard "n_events == 1" degenerate input used in STATS-04 tests.
make_matrix_single_event_data <- function(n_est = 30, n_ev = 5) {
  set.seed(11)
  tibble::tibble(
    event_id          = "E1",
    firm_symbol       = "F1",
    relative_index    = c(seq(-n_est, -1L), seq(0L, n_ev - 1L)),
    abnormal_returns  = rnorm(n_est + n_ev, mean = 0.001, sd = 0.02),
    event_window      = c(rep(0L, n_est), rep(1L, n_ev)),
    estimation_window = c(rep(1L, n_est), rep(0L, n_ev)),
    index_returns     = rnorm(n_est + n_ev, 0.0003, 0.015),
    firm_returns      = rnorm(n_est + n_ev, 0.001, 0.02),
    event_date        = c(rep(0L, n_est), 1L, rep(0L, n_ev - 1L))
  )
}

# Helper: single-event model tibble for stat rows that require a model arg.
make_matrix_single_event_model_tbl <- function() {
  d  <- make_matrix_single_event_data()
  mm <- MarketModel$new()
  mm$fit(d)
  tibble::tibble(
    event_id    = "E1",
    firm_symbol = "F1",
    model       = list(mm)
  )
}

# Helper: sigma-zero stub model for single-event-stat degenerate input.
make_matrix_sigma_zero_model <- function() {
  structure(
    list(
      statistics = list(
        sigma                          = 0,
        degree_of_freedom              = 100,
        residuals                      = NULL,
        forecast_error_corrected_sigma = rep(0, 5)
      )
    ),
    class = "stub_model"
  )
}

# ---------------------------------------------------------------------------
# The registry
# ---------------------------------------------------------------------------

contract_matrix_components <- list(

  # --- MODELS (14) ---

  list(
    label = "MarketModel",
    kind  = "model",
    make  = function() MarketModel$new(),
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "MarketAdjustedModel",
    kind  = "model",
    make  = function() MarketAdjustedModel$new(),
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "ComparisonPeriodMeanAdjustedModel",
    kind  = "model",
    make  = function() ComparisonPeriodMeanAdjustedModel$new(),
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "CustomModel",
    kind  = "model",
    make  = function() CustomModel$new(),
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "LinearFactorModel",
    kind  = "model",
    make  = function() LinearFactorModel$new(),
    data  = function() create_degenerate_factor_model_data_insufficient()
  ),

  list(
    label = "FamaFrench3FactorModel",
    kind  = "model",
    make  = function() FamaFrench3FactorModel$new(),
    data  = function() create_degenerate_factor_model_data_insufficient()
  ),

  list(
    label = "FamaFrench5FactorModel",
    kind  = "model",
    make  = function() FamaFrench5FactorModel$new(),
    data  = function() create_degenerate_factor_model_data_insufficient()
  ),

  list(
    label = "Carhart4FactorModel",
    kind  = "model",
    make  = function() Carhart4FactorModel$new(),
    data  = function() create_degenerate_factor_model_data_insufficient()
  ),

  list(
    label = "BHARModel",
    kind  = "model",
    make  = function() BHARModel$new(),
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "VolumeModel",
    kind  = "model",
    make  = function() VolumeModel$new(),
    data  = function() create_degenerate_volume_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "VolatilityModel",
    kind  = "model",
    make  = function() VolatilityModel$new(),
    data  = function() create_degenerate_volatility_model_data_zero_var()
  ),

  list(
    label = "RollingWindowModel",
    kind  = "model",
    make  = function() RollingWindowModel$new(window_size = 30L, min_obs = 10L),
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "GARCHModel",
    kind  = "model",
    make  = function() {
      testthat::skip_if_not_installed("rugarch")
      GARCHModel$new()
    },
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  list(
    label = "DCCGARCHModel",
    kind  = "model",
    make  = function() {
      testthat::skip_if_not_installed("rugarch")
      testthat::skip_if_not_installed("rmgarch")
      DCCGARCHModel$new()
    },
    data  = function() create_degenerate_model_data_insufficient(n_valid = 1)
  ),

  # --- STATS (10): lenient-only (NA-safety invariant) ---
  # These stats have no strict-error contract; only assert NA not Inf/NaN.

  list(
    label      = "ARTTest",
    kind       = "stat",
    make_stat  = function() ARTTest$new(),
    stat_col   = "ar_t",
    stat_data  = function() {
      d  <- make_matrix_single_event_data()
      m  <- make_matrix_sigma_zero_model()
      list(data = d, model = m)
    }
  ),

  list(
    label      = "CARTTest",
    kind       = "stat",
    make_stat  = function() CARTTest$new(),
    stat_col   = "car_t",
    stat_data  = function() {
      d  <- make_matrix_single_event_data()
      m  <- make_matrix_sigma_zero_model()
      list(data = d, model = m)
    }
  ),

  list(
    label      = "BHARTTest",
    kind       = "stat",
    make_stat  = function() BHARTTest$new(),
    stat_col   = "bhar_t",
    stat_data  = function() {
      d  <- make_matrix_single_event_data()
      m  <- make_matrix_sigma_zero_model()
      list(data = d, model = m)
    }
  ),

  list(
    label      = "CSectTTest",
    kind       = "stat",
    make_stat  = function() CSectTTest$new(),
    stat_col   = "aar_t",
    stat_data  = function() {
      d <- make_matrix_single_event_data()
      list(data = d, model = NULL)
    }
  ),

  list(
    label      = "PatellZTest",
    kind       = "stat",
    make_stat  = function() PatellZTest$new(),
    stat_col   = "aar_z",
    stat_data  = function() {
      d   <- make_matrix_single_event_data()
      mod <- make_matrix_single_event_model_tbl()
      list(data = d, model = mod)
    }
  ),

  list(
    label      = "BMPTest",
    kind       = "stat",
    make_stat  = function() BMPTest$new(),
    stat_col   = "bmp_t",
    stat_data  = function() {
      d   <- make_matrix_single_event_data()
      mod <- make_matrix_single_event_model_tbl()
      list(data = d, model = mod)
    }
  ),

  list(
    label      = "KolariPynnonenTest",
    kind       = "stat",
    make_stat  = function() KolariPynnonenTest$new(),
    stat_col   = "kp_t",
    stat_data  = function() {
      d   <- make_matrix_single_event_data()
      mod <- make_matrix_single_event_model_tbl()
      list(data = d, model = mod)
    }
  ),

  list(
    label      = "SignTest",
    kind       = "stat",
    make_stat  = function() SignTest$new(),
    stat_col   = "sign_z",
    stat_data  = function() {
      d <- make_matrix_single_event_data()
      list(data = d, model = NULL)
    }
  ),

  list(
    label      = "GeneralizedSignTest",
    kind       = "stat",
    make_stat  = function() GeneralizedSignTest$new(),
    stat_col   = "gsign_z",
    stat_data  = function() {
      # Degenerate: all-positive ARs → p_hat = 1 → denom = sqrt(n*1*0) = 0 → NA
      set.seed(11)
      n_est <- 30; n_ev <- 5
      d <- tibble::tibble(
        event_id          = "E1",
        firm_symbol       = "F1",
        relative_index    = c(seq(-n_est, -1L), seq(0L, n_ev - 1L)),
        abnormal_returns  = abs(rnorm(n_est + n_ev, mean = 0.05, sd = 0.01)),
        event_window      = c(rep(0L, n_est), rep(1L, n_ev)),
        estimation_window = c(rep(1L, n_est), rep(0L, n_ev)),
        index_returns     = rnorm(n_est + n_ev, 0.0003, 0.015),
        firm_returns      = abs(rnorm(n_est + n_ev, 0.001, 0.02)),
        event_date        = c(rep(0L, n_est), 1L, rep(0L, n_ev - 1L))
      )
      list(data = d, model = NULL)
    }
  ),

  list(
    label      = "CalendarTimePortfolioTest",
    kind       = "stat",
    make_stat  = function() CalendarTimePortfolioTest$new(),
    stat_col   = "caltime_t",
    # Degenerate: single event-window day → sd(portfolio$aar) = NA (sd of 1 value) → caltime_t = NA
    stat_data  = function() {
      d <- tibble::tibble(
        event_id          = "E1",
        firm_symbol       = "F1",
        relative_index    = 0L,
        abnormal_returns  = 0.01,
        event_window      = 1L,
        estimation_window = 0L,
        index_returns     = 0.0003,
        firm_returns      = 0.001,
        event_date        = 1L
      )
      list(data = d, model = NULL)
    }
  ),

  # --- PIPELINE (1): prepare_event_study missing-date path ---

  list(
    label     = "prepare_event_study_missing_date",
    kind      = "pipeline",
    make_task = function() {
      task <- create_mock_task(n_firms = 1)
      task$data_tbl$request[[1]]$event_date <- "99.99.9999"
      task
    }
  )

)  # end contract_matrix_components


# ---------------------------------------------------------------------------
# Driver: iterate registry and generate test_that blocks per kind
# ---------------------------------------------------------------------------

for (.entry in contract_matrix_components) {

  if (.entry$kind == "model") {
    local({
      entry <- .entry

      # Strict row
      test_that(
        paste0("contract-matrix [strict] ", entry$label,
               " — errors naming event_id/firm_symbol/component"),
        {
          m <- entry$make()
          m$degenerate_mode <- "strict"
          m$event_id        <- "EVT_MX"
          m$firm_symbol     <- "FIRM_MX"
          d <- entry$data()
          expect_error(m$fit(d), regexp = "EVT_MX")

          m2 <- entry$make()
          m2$degenerate_mode <- "strict"
          m2$event_id        <- "EVT_MX"
          m2$firm_symbol     <- "FIRM_MX"
          expect_error(m2$fit(d), regexp = "FIRM_MX")

          m3 <- entry$make()
          m3$degenerate_mode <- "strict"
          m3$event_id        <- "EVT_MX"
          m3$firm_symbol     <- "FIRM_MX"
          expect_error(m3$fit(d), regexp = entry$label)
        }
      )

      # Lenient row
      test_that(
        paste0("contract-matrix [lenient] ", entry$label,
               " — is_fitted FALSE, NA downstream, one warning, no Inf/NaN"),
        {
          m <- entry$make()
          m$degenerate_mode <- "lenient"
          m$event_id        <- "EVT_MX"
          m$firm_symbol     <- "FIRM_MX"
          d <- entry$data()

          # Collect warnings during fit()
          fit_warnings <- character(0)
          withCallingHandlers(
            m$fit(d),
            warning = function(w) {
              fit_warnings <<- c(fit_warnings, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )

          expect_false(m$is_fitted,
                       info = paste(entry$label, "must not be fitted after degenerate input"))
          expect_length(fit_warnings, 1L)

          # abnormal_returns() must produce NA results with zero additional warnings
          ar_warnings <- character(0)
          ar_tbl <- withCallingHandlers(
            m$abnormal_returns(d),
            warning = function(w) {
              ar_warnings <<- c(ar_warnings, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )

          expect_length(ar_warnings, 0L)
          expect_true(all(is.na(ar_tbl$abnormal_returns)),
                      info = paste(entry$label, "abnormal_returns must all be NA"))

          # Guard against Inf/NaN in the non-NA subset (empty here, but checks general contract)
          non_na <- ar_tbl$abnormal_returns[!is.na(ar_tbl$abnormal_returns)]
          expect_true(
            all(!is.infinite(non_na) & !is.nan(non_na)),
            info = paste(entry$label, "abnormal_returns must not contain Inf/NaN")
          )
        }
      )
    })

  } else if (.entry$kind == "stat") {
    local({
      entry <- .entry

      # Lenient-only (NA-safety invariant): stat is NA, no Inf/NaN
      test_that(
        paste0("contract-matrix [lenient] ", entry$label,
               " — NA statistic on degenerate input, no Inf/NaN"),
        {
          inputs <- entry$stat_data()
          result <- entry$make_stat()$compute(inputs$data, inputs$model)

          stat_vals <- result[[entry$stat_col]]
          expect_true(
            all(is.na(stat_vals)),
            info = paste(entry$label, entry$stat_col,
                         "must be NA on degenerate input")
          )
          expect_false(
            any(is.infinite(stat_vals)),
            info = paste(entry$label, entry$stat_col, "must not be Inf")
          )
          expect_false(
            any(is.nan(stat_vals)),
            info = paste(entry$label, entry$stat_col, "must not be NaN")
          )
        }
      )
    })

  } else if (.entry$kind == "pipeline") {
    local({
      entry <- .entry

      # Strict: prepare_event_study raises error naming component/event/firm
      test_that(
        paste0("contract-matrix [strict] ", entry$label,
               " — errors naming event_id/firm_symbol/component"),
        {
          task <- entry$make_task()
          ps   <- ParameterSet$new(degenerate_handling = "strict")
          expect_error(
            prepare_event_study(task, ps),
            regexp = "prepare_event_study"
          )
        }
      )

      # Lenient: one warning emitted, no crash, pipeline degrades
      test_that(
        paste0("contract-matrix [lenient] ", entry$label,
               " — one warning, graceful degradation, no crash"),
        {
          task <- entry$make_task()
          ps   <- ParameterSet$new(degenerate_handling = "lenient")

          warnings_seen <- character(0)
          result <- withCallingHandlers(
            prepare_event_study(task, ps),
            warning = function(w) {
              warnings_seen <<- c(warnings_seen, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )

          expect_length(warnings_seen, 1L)
          # Degraded row must not have valid windows (all-zero)
          d <- result$data_tbl$data[[1]]
          expect_true(all(d$event_window      == 0L))
          expect_true(all(d$estimation_window == 0L))
        }
      )
    })
  }

}  # end for loop over registry


# ---------------------------------------------------------------------------
# EXHAUSTIVENESS — assert registry completeness explicitly
# ---------------------------------------------------------------------------

test_that("contract-matrix registry is exhaustive", {
  # Total: 14 models + 10 stats + 1 pipeline = 25
  expect_equal(length(contract_matrix_components), 25L,
               info = "Registry must have exactly 25 rows (14 models + 10 stats + 1 pipeline)")

  # Sub-counts
  kinds     <- vapply(contract_matrix_components, function(x) x$kind, character(1))
  n_models  <- sum(kinds == "model")
  n_stats   <- sum(kinds == "stat")
  n_pipe    <- sum(kinds == "pipeline")
  expect_equal(n_models, 14L, info = "Must have exactly 14 model rows")
  expect_equal(n_stats,  10L, info = "Must have exactly 10 stat rows")
  expect_equal(n_pipe,   1L,  info = "Must have exactly 1 pipeline row")

  # All labels unique
  labels <- vapply(contract_matrix_components, function(x) x$label, character(1))
  expect_false(any(duplicated(labels)),
               info = "All registry labels must be unique")

  # Excluded classes are NOT in the registry
  expect_false(any(contract_matrix_excluded %in% labels),
               info = "Excluded classes must not appear in the registry")

  # Excluded classes are defined in contract_matrix_excluded
  expect_true(setequal(contract_matrix_excluded, c("PermutationTest", "RankTest")),
              info = "contract_matrix_excluded must be exactly {PermutationTest, RankTest}")

  # 10 covered stats + 2 excluded = 12 concrete stat classes total
  covered_stats <- labels[kinds == "stat"]
  expect_equal(
    length(covered_stats) + length(contract_matrix_excluded),
    12L,
    info = "covered stats + excluded must equal 12 concrete stat classes"
  )
})
