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

  # rmspe_ratios should be named
  ratios <- task$results$placebo$rmspe_ratios
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
