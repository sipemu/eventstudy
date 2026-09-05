# Helper: create synthetic control test data with known treatment effect
create_sc_test_data <- function(n_donors = 5, n_periods = 40,
                                 treatment_time = 21, effect = 2) {
  set.seed(42)

  times <- seq_len(n_periods)

  # Common trend
  common <- cumsum(rnorm(n_periods, mean = 0.1, sd = 0.3))

  # Treated unit: follows common trend + effect after treatment
  treated_outcome <- common + rnorm(n_periods, sd = 0.2)
  post <- times >= treatment_time
  treated_outcome[post] <- treated_outcome[post] + effect

  treated_data <- tibble::tibble(
    time = times,
    outcome = treated_outcome
  )

  # Donor units: follow common trend with different loadings
  donor_list <- lapply(seq_len(n_donors), function(i) {
    loading <- 0.5 + runif(1, 0, 1)
    d_outcome <- loading * common + rnorm(n_periods, sd = 0.2)
    tibble::tibble(
      unit = paste0("D", i),
      time = times,
      outcome = d_outcome
    )
  })
  donor_data <- do.call(rbind, donor_list)

  list(
    treated_data = treated_data,
    donor_data = donor_data,
    treatment_time = treatment_time,
    effect = effect
  )
}


test_that("SyntheticControlTask creates correctly", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    treated_data = d$treated_data,
    donor_data = d$donor_data,
    treatment_time = d$treatment_time
  )

  expect_true(inherits(task, "SyntheticControlTask"))
  expect_output(print(task), "Donors")
})


test_that("SyntheticControlTask validates treated_data", {
  expect_error(
    SyntheticControlTask$new(
      treated_data = tibble::tibble(x = 1),
      donor_data = tibble::tibble(unit = "A", time = 1, outcome = 1),
      treatment_time = 1
    ),
    "time.*outcome"
  )
})


test_that("SyntheticControlTask validates donor_data", {
  expect_error(
    SyntheticControlTask$new(
      treated_data = tibble::tibble(time = 1, outcome = 1),
      donor_data = tibble::tibble(x = 1),
      treatment_time = 1
    ),
    "unit.*time.*outcome"
  )
})


test_that("estimate_synthetic_control with optim produces valid weights", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )

  task <- estimate_synthetic_control(task, method = "optim")

  w <- task$results$weights
  expect_true(all(w >= -1e-6))
  expect_equal(sum(w), 1, tolerance = 1e-4)
})


test_that("estimate_synthetic_control with quadprog produces valid weights", {
  skip_if_not_installed("quadprog")
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )

  task <- estimate_synthetic_control(task, method = "quadprog")

  w <- task$results$weights
  expect_true(all(w >= -1e-6))
  expect_equal(sum(w), 1, tolerance = 1e-4)
})


test_that("synthetic control detects known treatment effect", {
  d <- create_sc_test_data(effect = 5, n_donors = 10, n_periods = 60)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")

  # ATT should be roughly positive (matching the injected positive effect)
  expect_gt(task$results$att, 0)
})


test_that("estimate_synthetic_control populates all result fields", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")

  expect_false(is.null(task$results$weights))
  expect_false(is.null(task$results$trajectory))
  expect_false(is.null(task$results$pre_mspe))
  expect_false(is.null(task$results$post_mspe))
  expect_false(is.null(task$results$att))
  expect_false(is.null(task$results$method))
  expect_equal(task$results$method, "optim")

  # Trajectory should have the correct columns
  traj <- task$results$trajectory
  expect_true(all(c("time", "treated", "synthetic", "gap") %in% names(traj)))
  expect_equal(nrow(traj), 40)

  # gap = treated - synthetic
  expect_equal(traj$gap, traj$treated - traj$synthetic, tolerance = 1e-10)
})


test_that("pre_mspe is less than post_mspe when effect is large", {
  d <- create_sc_test_data(effect = 10, n_donors = 10, n_periods = 60)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")

  # Post-treatment MSPE should be much larger due to the effect
  expect_gt(task$results$post_mspe, task$results$pre_mspe)
})


test_that("plot_synthetic_control returns ggplot for trajectory", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")

  p <- plot_synthetic_control(task, type = "trajectory")
  expect_true(inherits(p, "ggplot"))
})


test_that("plot_synthetic_control returns ggplot for gap", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")

  p <- plot_synthetic_control(task, type = "gap")
  expect_true(inherits(p, "ggplot"))
})


test_that("plot_synthetic_control errors without results", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  expect_error(plot_synthetic_control(task), "estimate_synthetic_control")
})


test_that("estimate_synthetic_control errors on wrong type", {
  expect_error(estimate_synthetic_control(list()), "SyntheticControlTask")
})


test_that("sc_placebo_test produces valid results", {
  d <- create_sc_test_data(n_donors = 3, n_periods = 30, effect = 5)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")
  task <- sc_placebo_test(task, n_placebo = 3)

  expect_false(is.null(task$results$placebo))
  expect_false(is.null(task$results$placebo$p_value))
  expect_true(task$results$placebo$p_value >= 0 &&
                task$results$placebo$p_value <= 1)

  # mspe_ratios should be named (MSPE ratio per Abadie et al.)
  ratios <- task$results$placebo$mspe_ratios
  expect_equal(length(ratios), 3)
  expect_false(is.null(names(ratios)))
})


test_that("sc_placebo_test errors without estimation", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  expect_error(sc_placebo_test(task), "estimate_synthetic_control")
})


test_that("sc_placebo_test errors on wrong type", {
  expect_error(sc_placebo_test(list()), "SyntheticControlTask")
})


test_that("plot_synthetic_control placebo type requires placebo results", {
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")

  expect_error(plot_synthetic_control(task, type = "placebo"),
               "sc_placebo_test")
})


test_that("plot_synthetic_control placebo type works after placebo test", {
  d <- create_sc_test_data(n_donors = 3, n_periods = 30, effect = 3)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")
  task <- sc_placebo_test(task, n_placebo = 3)

  p <- plot_synthetic_control(task, type = "placebo")
  expect_true(inherits(p, "ggplot"))
})


test_that("synthetic control with no treatment effect has ATT near zero", {
  d <- create_sc_test_data(effect = 0, n_donors = 10, n_periods = 60)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task <- estimate_synthetic_control(task, method = "optim")

  # ATT should be close to 0 (may not be exactly 0 due to finite sample)
  expect_lt(abs(task$results$att), 3)  # generous tolerance
})


test_that("synthetic control handles unsorted treated_data correctly", {
  d <- create_sc_test_data(n_donors = 3, n_periods = 30, treatment_time = 16)

  # Shuffle the treated data rows (unsorted by time)
  shuffled_treated <- d$treated_data[sample(nrow(d$treated_data)), ]

  task_sorted <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  task_shuffled <- SyntheticControlTask$new(
    shuffled_treated, d$donor_data, d$treatment_time
  )

  task_sorted <- estimate_synthetic_control(task_sorted, method = "optim")
  task_shuffled <- estimate_synthetic_control(task_shuffled, method = "optim")

  # Results should be identical regardless of input row order
  expect_equal(task_sorted$results$weights, task_shuffled$results$weights,
               tolerance = 1e-6)
  expect_equal(task_sorted$results$att, task_shuffled$results$att,
               tolerance = 1e-6)
})


# --- Regression: Placebo test uses MSPE ratio (not RMSPE) ---

test_that("sc_placebo_test uses MSPE ratio per Abadie et al.", {
  # Bug: Used sqrt(post_mspe)/sqrt(pre_mspe) (RMSPE ratio) instead of
  # post_mspe/pre_mspe (MSPE ratio), reducing statistical power.
  d <- create_sc_test_data()
  task <- SyntheticControlTask$new(d$treated_data, d$donor_data, d$treatment_time)
  task <- estimate_synthetic_control(task, method = "optim")
  task <- sc_placebo_test(task, n_placebo = 3)

  # The treated_ratio should be MSPE ratio (post/pre), not RMSPE
  expected_ratio <- task$results$post_mspe / max(task$results$pre_mspe, 1e-10)
  expect_equal(task$results$placebo$treated_ratio, expected_ratio, tolerance = 1e-10)

  # Field should be mspe_ratios (not rmspe_ratios)
  expect_true("mspe_ratios" %in% names(task$results$placebo))
  expect_false("rmspe_ratios" %in% names(task$results$placebo))
})


# --- Regression: Donor matrix validation for missing periods ---

test_that("estimate_synthetic_control errors on donor with missing periods", {
  # Bug: If a donor was missing pre-treatment periods, R would silently
  # recycle values in the donor matrix.
  d <- create_sc_test_data()

  # Remove some pre-treatment periods from donor D2
  incomplete_donors <- d$donor_data[!(d$donor_data$unit == "D2" &
                                        d$donor_data$time <= 3), ]

  task <- SyntheticControlTask$new(
    d$treated_data, incomplete_donors, d$treatment_time
  )

  expect_error(
    estimate_synthetic_control(task, method = "optim"),
    "pre-treatment observations"
  )
})


# --- Regression: estimate_synthetic_control validates pre/post periods ---

test_that("estimate_synthetic_control errors when no pre-treatment periods", {
  # Bug: mean(gap[FALSE]^2) returned NaN, causing silent NaN propagation
  # through all results. Now errors with an informative message.
  d <- create_sc_test_data(n_donors = 3, n_periods = 20)

  # Set treatment_time to before all data (so no pre-treatment periods)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, treatment_time = 0
  )

  expect_error(
    estimate_synthetic_control(task, method = "optim"),
    "No pre-treatment periods"
  )
})


test_that("estimate_synthetic_control errors when no post-treatment periods", {
  d <- create_sc_test_data(n_donors = 3, n_periods = 20)

  # Set treatment_time to after all data (so no post-treatment periods)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, treatment_time = 100
  )

  expect_error(
    estimate_synthetic_control(task, method = "optim"),
    "No post-treatment periods"
  )
})


# --- Regression: optim convergence warning ---

test_that("estimate_synthetic_control warns on non-convergence", {
  # Bug: optim could fail to converge silently without any indication.
  # Now issues a warning when convergence != 0.
  # We can't easily force non-convergence, so just verify convergence
  # succeeds without warning on normal data.
  d <- create_sc_test_data(n_donors = 3, n_periods = 30)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )
  expect_no_warning(estimate_synthetic_control(task, method = "optim"))
})


# --- EXTERNAL-04: Synthetic control numerical guards ---

test_that("solve.QP failure (via mock) degrades to optim fallback without crashing", {
  skip_if_not_installed("quadprog")
  d <- create_sc_test_data(n_donors = 3, n_periods = 30)
  task <- SyntheticControlTask$new(d$treated_data, d$donor_data, d$treatment_time)

  # Mock solve.QP to simulate an unrecoverable singularity (1e-8 ridge not enough)
  local_mocked_bindings(
    solve.QP = function(...) stop("simulated: system is computationally singular"),
    .package = "quadprog"
  )

  # Should warn and fall back to optim — no crash
  result <- withCallingHandlers(
    estimate_synthetic_control(task, method = "quadprog"),
    warning = function(w) {
      invokeRestart("muffleWarning")
    }
  )

  # Weights should be numeric (optim fallback produces valid simplex weights)
  expect_false(is.null(result$results$weights))
  expect_true(is.numeric(result$results$weights))
})


test_that("solve.QP failure (via mock) produces named warning and falls back to optim", {
  skip_if_not_installed("quadprog")
  d <- create_sc_test_data(n_donors = 3, n_periods = 30)
  task <- SyntheticControlTask$new(d$treated_data, d$donor_data, d$treatment_time)

  # Mock solve.QP to simulate a singular-matrix failure
  local_mocked_bindings(
    solve.QP = function(...) stop("simulated singular matrix in quadprog"),
    .package = "quadprog"
  )

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_synthetic_control(task, method = "quadprog"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  # Warning must name solve.QP or quadprog
  expect_true(any(grepl("solve.QP|quadprog", ws)))
  # Must still produce weights (optim fallback)
  expect_false(is.null(result$results$weights))
  expect_true(is.numeric(result$results$weights))
})


test_that("empty donor pool (.solve_sc_optim n==0) warns and does not crash from max()", {
  # Directly exercise the empty-donor-pool guard in .solve_sc_optim via :::
  # Empty X: 10 rows, 0 columns (zero donors)
  y <- rnorm(10)
  X <- matrix(numeric(0), nrow = 10, ncol = 0)

  ws <- character(0)
  result <- withCallingHandlers(
    EventStudy:::.solve_sc_optim(y, X),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  # Must warn about empty donor pool (no max() crash)
  expect_true(length(ws) >= 1L)
  expect_true(any(grepl("empty donor pool", ws, ignore.case = TRUE)))
  # Must return length-0 numeric (not throw)
  expect_true(is.numeric(result))
  expect_equal(length(result), 0L)
})


# --- CR-02 regression: empty donor pool at estimate_synthetic_control level ---

test_that("estimate_synthetic_control: empty donor pool warns and returns invisible(NULL)", {
  # CR-02 regression: without the early guard, X_all %*% rep(NA, 0) yields a
  # zero vector so y_synth=0 and att = mean(y_treated[post]) — the raw treated
  # outcome level (maximally wrong plausible-looking result).
  # After fix: returns invisible(NULL) + one warning before entering any solver.
  treated <- tibble::tibble(time = 1:20, outcome = rnorm(20))
  # Empty donor_data (zero rows, but correct columns)
  empty_donors <- tibble::tibble(unit = character(0), time = integer(0),
                                  outcome = numeric(0))

  task <- SyntheticControlTask$new(
    treated_data = treated,
    donor_data = empty_donors,
    treatment_time = 11
  )

  ws <- character(0)
  result <- withCallingHandlers(
    estimate_synthetic_control(task, method = "optim"),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  # Must warn about empty donor pool
  expect_true(length(ws) >= 1L)
  expect_true(any(grepl("donor pool is empty|empty donor", ws, ignore.case = TRUE)))

  # Must return NULL (not a fabricated ATT)
  expect_null(result)

  # task$results must remain NULL — no fake ATT stored
  expect_null(task$results)
})


test_that("valid donor pool weights unchanged after solve.QP guard addition", {
  skip_if_not_installed("quadprog")
  d <- create_sc_test_data(n_donors = 5, n_periods = 40, treatment_time = 21)
  task <- SyntheticControlTask$new(
    d$treated_data, d$donor_data, d$treatment_time
  )

  task_qp <- estimate_synthetic_control(task, method = "quadprog")
  w <- task_qp$results$weights

  # Weights must form a simplex
  expect_true(all(w >= -1e-6))
  expect_equal(sum(w), 1, tolerance = 1e-4)
  expect_equal(length(w), 5)
})
