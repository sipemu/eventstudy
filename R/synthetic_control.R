#' Synthetic Control Task
#'
#' Task container for synthetic control analysis. Holds treated unit data,
#' donor pool data, treatment time, and results after estimation.
#'
#' @export
SyntheticControlTask <- R6::R6Class(
  classname = "SyntheticControlTask",
  public = list(
    #' @field treated_data Tibble with \code{time} and \code{outcome} columns
    #'   for the treated unit.
    treated_data = NULL,
    #' @field donor_data Tibble in long format with \code{unit}, \code{time},
    #'   and \code{outcome} columns for donor units.
    donor_data = NULL,
    #' @field treatment_time The time period when treatment begins.
    treatment_time = NULL,
    #' @field results Estimation results (populated after estimation).
    results = NULL,

    #' @description
    #' Create a new SyntheticControlTask.
    #'
    #' @param treated_data Tibble with \code{time} and \code{outcome}.
    #' @param donor_data Long-format tibble with \code{unit}, \code{time},
    #'   \code{outcome}.
    #' @param treatment_time Period when treatment begins.
    initialize = function(treated_data, donor_data, treatment_time) {
      if (!all(c("time", "outcome") %in% names(treated_data))) {
        stop("treated_data must have 'time' and 'outcome' columns.")
      }
      if (!all(c("unit", "time", "outcome") %in% names(donor_data))) {
        stop("donor_data must have 'unit', 'time', and 'outcome' columns.")
      }

      self$treated_data <- treated_data
      self$donor_data <- donor_data
      self$treatment_time <- treatment_time
    },

    #' @description Print summary.
    print = function(...) {
      cat("SyntheticControlTask\n")
      n_donors <- length(unique(self$donor_data$unit))
      n_periods <- length(unique(self$treated_data$time))
      n_pre <- sum(self$treated_data$time < self$treatment_time)
      n_post <- sum(self$treated_data$time >= self$treatment_time)

      cat("  Donors:     ", n_donors, "\n")
      cat("  Periods:    ", n_periods, "\n")
      cat("  Pre-treat:  ", n_pre, "\n")
      cat("  Post-treat: ", n_post, "\n")
      cat("  Treatment:  ", self$treatment_time, "\n")

      if (!is.null(self$results)) {
        cat("  Estimated:   TRUE\n")
      } else {
        cat("  Estimated:   FALSE\n")
      }
      invisible(self)
    }
  )
)


#' Estimate Synthetic Control
#'
#' Estimate synthetic control weights by minimizing the pre-treatment
#' mean squared prediction error. Weights are non-negative and sum to one.
#'
#' @param task A \code{SyntheticControlTask}.
#' @param method Optimization method: \code{"quadprog"} (default, requires
#'   \pkg{quadprog}) or \code{"optim"} (uses \code{stats::optim} L-BFGS-B).
#' @param covariates Optional character vector of covariate column names
#'   in both treated and donor data to include in the matching.
#'
#' @return The task with \code{results} populated.
#'
#' @export
estimate_synthetic_control <- function(task, method = c("quadprog", "optim"),
                                        covariates = NULL) {
  if (!inherits(task, "SyntheticControlTask")) {
    stop("task must be a SyntheticControlTask.")
  }

  method <- match.arg(method)

  treated <- task$treated_data
  donors <- task$donor_data
  t0 <- task$treatment_time

  # Pre-treatment data
  y_pre <- treated$outcome[treated$time < t0]
  donor_units <- sort(unique(donors$unit))
  n_donors <- length(donor_units)

  # Build donor matrix: rows = pre-treatment periods, cols = donors
  X_pre <- matrix(NA_real_, nrow = length(y_pre), ncol = n_donors)
  pre_times <- treated$time[treated$time < t0]

  for (j in seq_along(donor_units)) {
    d <- donors[donors$unit == donor_units[j], ]
    d <- d[d$time %in% pre_times, ]
    d <- d[order(d$time), ]
    X_pre[, j] <- d$outcome
  }

  # Solve for weights W: min ||y_pre - X_pre %*% W||^2 s.t. W >= 0, sum(W) = 1
  if (method == "quadprog" && requireNamespace("quadprog", quietly = TRUE)) {
    weights <- .solve_sc_quadprog(y_pre, X_pre)
  } else {
    if (method == "quadprog") {
      message("Package 'quadprog' not available. Falling back to optim.")
    }
    weights <- .solve_sc_optim(y_pre, X_pre)
  }

  names(weights) <- donor_units

  # Full trajectory
  all_times <- sort(unique(treated$time))
  y_all <- treated$outcome[order(treated$time)]

  X_all <- matrix(NA_real_, nrow = length(all_times), ncol = n_donors)
  for (j in seq_along(donor_units)) {
    d <- donors[donors$unit == donor_units[j], ]
    d <- d[d$time %in% all_times, ]
    d <- d[order(d$time), ]
    X_all[, j] <- d$outcome
  }

  y_synth <- as.numeric(X_all %*% weights)
  gap <- y_all - y_synth

  # Pre/post MSPE
  pre_idx <- all_times < t0
  post_idx <- all_times >= t0

  pre_mspe <- mean(gap[pre_idx]^2)
  post_mspe <- mean(gap[post_idx]^2)
  att <- mean(gap[post_idx])

  task$results <- list(
    weights = weights,
    trajectory = tibble::tibble(
      time = all_times,
      treated = y_all,
      synthetic = y_synth,
      gap = gap
    ),
    pre_mspe = pre_mspe,
    post_mspe = post_mspe,
    att = att,
    method = method
  )

  task
}


#' Solve synthetic control via quadprog
#' @noRd
.solve_sc_quadprog <- function(y, X) {
  n <- ncol(X)
  Dmat <- t(X) %*% X
  # Ensure positive definiteness
  Dmat <- Dmat + diag(1e-8, n)
  dvec <- t(X) %*% y

  # Constraints: sum(W) = 1, W_i >= 0
  # quadprog wants: Amat' %*% b >= bvec
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  meq <- 1  # first constraint is equality

  res <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
  pmax(res$solution, 0)  # numerical cleanup
}


#' Solve synthetic control via optim (L-BFGS-B)
#' @noRd
.solve_sc_optim <- function(y, X) {
  n <- ncol(X)

  # Objective: ||y - X %*% w||^2 where w is simplex-constrained
  # Use log-ratio transform: w = softmax(theta)
  obj_fn <- function(theta) {
    w <- exp(theta) / sum(exp(theta))
    sum((y - X %*% w)^2)
  }

  init <- rep(0, n)
  res <- stats::optim(init, obj_fn, method = "L-BFGS-B")

  w <- exp(res$par) / sum(exp(res$par))
  as.numeric(w)
}


#' Placebo Test for Synthetic Control
#'
#' Performs a placebo (permutation) test by re-estimating the synthetic
#' control for each donor unit as a pseudo-treated unit. The p-value is
#' the rank of the treated unit's RMSPE ratio among all units.
#'
#' @param task A \code{SyntheticControlTask} with results.
#' @param n_placebo Number of placebo units to use. Default NULL (all donors).
#'
#' @return The task with \code{results$placebo} populated, a list containing
#'   \code{rmspe_ratios}, \code{p_value}, and \code{placebo_gaps}.
#'
#' @export
sc_placebo_test <- function(task, n_placebo = NULL) {
  if (!inherits(task, "SyntheticControlTask")) {
    stop("task must be a SyntheticControlTask.")
  }
  if (is.null(task$results)) {
    stop("Run estimate_synthetic_control() first.")
  }

  treated <- task$treated_data
  donors <- task$donor_data
  t0 <- task$treatment_time
  donor_units <- sort(unique(donors$unit))

  if (!is.null(n_placebo) && n_placebo < length(donor_units)) {
    donor_units <- sample(donor_units, n_placebo)
  }

  # Treated unit RMSPE ratio
  traj <- task$results$trajectory
  pre_rmspe_treated <- sqrt(task$results$pre_mspe)
  post_rmspe_treated <- sqrt(task$results$post_mspe)
  ratio_treated <- post_rmspe_treated / max(pre_rmspe_treated, 1e-10)

  all_times <- sort(unique(treated$time))
  placebo_gaps <- list()
  rmspe_ratios <- numeric(length(donor_units))

  for (i in seq_along(donor_units)) {
    du <- donor_units[i]
    tryCatch({
      # Pseudo-treated = this donor
      pseudo_treated <- donors[donors$unit == du, ]
      pseudo_treated <- pseudo_treated[order(pseudo_treated$time), ]
      names(pseudo_treated)[names(pseudo_treated) == "unit"] <- "pseudo_unit"

      # Remaining donors = all other donors + the real treated unit as a donor
      pseudo_donors <- donors[donors$unit != du, ]
      # Add actual treated as donor
      treated_as_donor <- treated
      treated_as_donor$unit <- "treated"
      pseudo_donors <- dplyr::bind_rows(pseudo_donors, treated_as_donor)

      pseudo_task <- SyntheticControlTask$new(
        treated_data = tibble::tibble(
          time = pseudo_treated$time,
          outcome = pseudo_treated$outcome
        ),
        donor_data = pseudo_donors,
        treatment_time = t0
      )

      # Use optim to avoid quadprog dependency issues
      pseudo_task <- estimate_synthetic_control(pseudo_task, method = "optim")

      placebo_gaps[[du]] <- pseudo_task$results$trajectory$gap
      pre_rmspe <- sqrt(pseudo_task$results$pre_mspe)
      post_rmspe <- sqrt(pseudo_task$results$post_mspe)
      rmspe_ratios[i] <- post_rmspe / max(pre_rmspe, 1e-10)
    }, error = function(e) {
      rmspe_ratios[i] <<- NA_real_
    })
  }

  # P-value: proportion of donor ratios >= treated ratio
  valid_ratios <- rmspe_ratios[!is.na(rmspe_ratios)]
  p_value <- (sum(valid_ratios >= ratio_treated) + 1) /
    (length(valid_ratios) + 1)

  task$results$placebo <- list(
    rmspe_ratios = stats::setNames(rmspe_ratios, donor_units),
    treated_ratio = ratio_treated,
    p_value = p_value,
    placebo_gaps = placebo_gaps
  )

  task
}


#' Plot Synthetic Control Results
#'
#' Create plots for synthetic control analysis: trajectory comparison,
#' treatment effect gap, or placebo test results.
#'
#' @param task A \code{SyntheticControlTask} with results.
#' @param type Plot type: \code{"trajectory"} (default), \code{"gap"}, or
#'   \code{"placebo"}.
#'
#' @return A ggplot2 object.
#'
#' @export
plot_synthetic_control <- function(task, type = c("trajectory", "gap", "placebo")) {
  if (!inherits(task, "SyntheticControlTask")) {
    stop("task must be a SyntheticControlTask.")
  }
  if (is.null(task$results)) {
    stop("Run estimate_synthetic_control() first.")
  }

  type <- match.arg(type)
  traj <- task$results$trajectory
  t0 <- task$treatment_time

  if (type == "trajectory") {
    plot_data <- tidyr::pivot_longer(
      traj, cols = c("treated", "synthetic"),
      names_to = "series", values_to = "value"
    )

    ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = value,
                                             color = series,
                                             linetype = series)) +
      ggplot2::geom_vline(xintercept = t0, linetype = "dashed",
                           color = "grey40") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_color_manual(values = c(treated = "black",
                                              synthetic = "steelblue")) +
      ggplot2::labs(title = "Synthetic Control: Treated vs. Synthetic",
                     x = "Time", y = "Outcome", color = "", linetype = "") +
      ggplot2::theme_minimal()

  } else if (type == "gap") {
    ggplot2::ggplot(traj, ggplot2::aes(x = time, y = gap)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                           color = "grey40") +
      ggplot2::geom_vline(xintercept = t0, linetype = "dashed",
                           color = "red", alpha = 0.6) +
      ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
      ggplot2::geom_area(alpha = 0.2, fill = "steelblue") +
      ggplot2::labs(title = "Synthetic Control: Treatment Effect (Gap)",
                     x = "Time", y = "Gap (Treated - Synthetic)") +
      ggplot2::theme_minimal()

  } else {
    # Placebo plot
    if (is.null(task$results$placebo)) {
      stop("Run sc_placebo_test() first.")
    }

    placebo_gaps <- task$results$placebo$placebo_gaps
    all_times <- traj$time
    treated_gap <- traj$gap

    # Build tibble of all placebo gaps
    placebo_tbl <- do.call(rbind, lapply(names(placebo_gaps), function(u) {
      tibble::tibble(
        time = all_times,
        gap = placebo_gaps[[u]],
        unit = u
      )
    }))

    ggplot2::ggplot() +
      ggplot2::geom_line(data = placebo_tbl,
                          ggplot2::aes(x = time, y = gap, group = unit),
                          color = "grey70", alpha = 0.5) +
      ggplot2::geom_line(data = traj,
                          ggplot2::aes(x = time, y = gap),
                          color = "black", linewidth = 1) +
      ggplot2::geom_vline(xintercept = t0, linetype = "dashed",
                           color = "red", alpha = 0.6) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                           color = "grey40") +
      ggplot2::labs(title = "Synthetic Control: Placebo Test",
                     x = "Time", y = "Gap") +
      ggplot2::theme_minimal()
  }
}
