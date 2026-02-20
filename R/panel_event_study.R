#' Panel Event Study Task
#'
#' Task container for panel (difference-in-differences style) event studies.
#' This is designed for settings where units receive treatment at potentially
#' different times, and we observe outcomes over multiple periods.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(panel_data, unit_id, time_id, outcome, treatment, treatment_time)}}{
#'     Create a new panel event study task.}
#'   \item{\code{print()}}{Print summary.}
#' }
#'
#' @export
PanelEventStudyTask <- R6::R6Class(
  classname = "PanelEventStudyTask",
  public = list(
    #' @field panel_data The panel dataset in long format.
    panel_data = NULL,
    #' @field unit_id Name of the unit identifier column.
    unit_id = NULL,
    #' @field time_id Name of the time identifier column.
    time_id = NULL,
    #' @field outcome Name of the outcome column.
    outcome = NULL,
    #' @field treatment Name of the treatment indicator column (0/1).
    treatment = NULL,
    #' @field treatment_time Name of the treatment timing column
    #'   (period when unit first treated; NA for never-treated).
    treatment_time = NULL,
    #' @field results Estimation results (populated after estimation).
    results = NULL,

    #' @description
    #' Create a new PanelEventStudyTask.
    #'
    #' @param panel_data Long-format panel data frame.
    #' @param unit_id Name of the unit ID column.
    #' @param time_id Name of the time period column.
    #' @param outcome Name of the outcome variable column.
    #' @param treatment Name of the treatment indicator column (0/1).
    #' @param treatment_time Name of column indicating when each unit is
    #'   first treated (NA for never-treated controls).
    initialize = function(panel_data,
                          unit_id = "unit_id",
                          time_id = "time_id",
                          outcome = "outcome",
                          treatment = "treated",
                          treatment_time = "treatment_time") {
      # Validate columns exist
      required <- c(unit_id, time_id, outcome, treatment, treatment_time)
      missing <- setdiff(required, names(panel_data))
      if (length(missing) > 0) {
        stop("Panel data missing columns: ", paste(missing, collapse = ", "))
      }

      self$panel_data <- panel_data
      self$unit_id <- unit_id
      self$time_id <- time_id
      self$outcome <- outcome
      self$treatment <- treatment
      self$treatment_time <- treatment_time
    },

    #' @description Print summary.
    print = function(...) {
      cat("PanelEventStudyTask\n")
      n_units <- length(unique(self$panel_data[[self$unit_id]]))
      n_periods <- length(unique(self$panel_data[[self$time_id]]))
      n_treated <- sum(!is.na(unique(
        self$panel_data[[self$treatment_time]][self$panel_data[[self$treatment]] == 1]
      )))

      cat("  Units:   ", n_units, "\n")
      cat("  Periods: ", n_periods, "\n")
      cat("  Treated: ", n_treated, "unit(s)\n")
      cat("  Outcome: ", self$outcome, "\n")

      if (!is.null(self$results)) {
        cat("  Estimated: TRUE\n")
      } else {
        cat("  Estimated: FALSE\n")
      }
      invisible(self)
    }
  )
)


#' Estimate Panel Event Study
#'
#' Estimate a panel event study using two-way fixed effects (TWFE) or
#' dynamic TWFE. Returns event-time coefficient estimates suitable for
#' event study plots.
#'
#' @param task A \code{PanelEventStudyTask}.
#' @param method Estimation method. One of \code{"static_twfe"},
#'   \code{"dynamic_twfe"}, or \code{"sun_abraham"}.
#' @param leads Number of pre-treatment periods to include. Default 5.
#' @param lags Number of post-treatment periods to include. Default 5.
#' @param base_period The reference period (relative to treatment) to omit.
#'   Default -1 (the period just before treatment).
#' @param cluster Name of the clustering variable for standard errors.
#'   Defaults to the unit ID.
#'
#' @return The task with \code{results} populated, containing:
#'   \describe{
#'     \item{coefficients}{Tibble of event-time coefficients with std errors}
#'     \item{model}{The fitted model object}
#'     \item{method}{The estimation method used}
#'   }
#'
#' @export
estimate_panel_event_study <- function(task,
                                        method = c("static_twfe",
                                                   "dynamic_twfe",
                                                   "sun_abraham"),
                                        leads = 5,
                                        lags = 5,
                                        base_period = -1,
                                        cluster = NULL) {
  if (!inherits(task, "PanelEventStudyTask")) {
    stop("task must be a PanelEventStudyTask.")
  }

  method <- match.arg(method)

  if (is.null(cluster)) {
    cluster <- task$unit_id
  }

  panel <- task$panel_data

  # Compute relative time to treatment
  panel$.rel_time <- panel[[task$time_id]] - panel[[task$treatment_time]]
  # Never-treated units get NA for rel_time
  panel$.rel_time[is.na(panel[[task$treatment_time]])] <- NA

  switch(method,
    static_twfe   = .estimate_static_twfe(task, panel, cluster),
    dynamic_twfe  = .estimate_dynamic_twfe(task, panel, leads, lags,
                                            base_period, cluster),
    sun_abraham   = .estimate_sun_abraham(task, panel, leads, lags,
                                           base_period, cluster)
  )

  task
}


#' Static TWFE
#' @noRd
.estimate_static_twfe <- function(task, panel, cluster) {
  # Y_it = alpha_i + gamma_t + beta * D_it + eps
  fml <- stats::as.formula(
    paste(task$outcome, "~", task$treatment,
          "+ factor(", task$unit_id, ") + factor(", task$time_id, ")")
  )

  fit <- stats::lm(fml, data = panel)
  fit_summary <- summary(fit)

  # Extract treatment coefficient
  treat_coef <- stats::coef(fit)[task$treatment]
  treat_se <- fit_summary$coefficients[task$treatment, 2]
  treat_t <- treat_coef / treat_se
  treat_p <- 2 * stats::pt(abs(treat_t), df = fit$df.residual, lower.tail = FALSE)

  coef_tbl <- tibble::tibble(
    term = "ATT",
    estimate = treat_coef,
    std.error = treat_se,
    statistic = treat_t,
    p.value = treat_p
  )

  task$results <- list(
    coefficients = coef_tbl,
    model = fit,
    method = "static_twfe"
  )
}


#' Dynamic TWFE
#' @noRd
.estimate_dynamic_twfe <- function(task, panel, leads, lags,
                                    base_period, cluster) {
  # Create event-time indicators
  # Bin endpoints: aggregate leads < -leads and lags > lags
  panel$.rel_time_binned <- dplyr::case_when(
    is.na(panel$.rel_time) ~ NA_real_,
    panel$.rel_time < -leads ~ -leads,
    panel$.rel_time > lags ~ lags,
    TRUE ~ panel$.rel_time
  )

  # Create factor, dropping base period
  event_times <- sort(unique(panel$.rel_time_binned[!is.na(panel$.rel_time_binned)]))
  event_times <- setdiff(event_times, base_period)

  # Create dummies manually
  for (k in event_times) {
    col_name <- paste0(".D_", ifelse(k < 0, "m", "p"), abs(k))
    panel[[col_name]] <- ifelse(!is.na(panel$.rel_time_binned) &
                                  panel$.rel_time_binned == k, 1, 0)
  }

  # Build formula
  dummy_names <- paste0(".D_", ifelse(event_times < 0, "m", "p"), abs(event_times))
  rhs <- paste(c(dummy_names,
                  paste0("factor(", task$unit_id, ")"),
                  paste0("factor(", task$time_id, ")")),
               collapse = " + ")
  fml <- stats::as.formula(paste(task$outcome, "~", rhs))

  fit <- stats::lm(fml, data = panel)
  fit_summary <- summary(fit)

  # Extract event-time coefficients
  coef_names <- names(stats::coef(fit))
  idx <- match(dummy_names, coef_names)
  idx <- idx[!is.na(idx)]

  coefs <- stats::coef(fit)[idx]
  se <- fit_summary$coefficients[idx, 2]

  coef_tbl <- tibble::tibble(
    relative_time = event_times[seq_along(idx)],
    estimate = as.numeric(coefs),
    std.error = as.numeric(se),
    statistic = as.numeric(coefs) / as.numeric(se),
    p.value = 2 * stats::pt(abs(as.numeric(coefs) / as.numeric(se)),
                              df = fit$df.residual, lower.tail = FALSE)
  )

  # Add the base period as zero
  coef_tbl <- dplyr::bind_rows(
    coef_tbl,
    tibble::tibble(
      relative_time = base_period,
      estimate = 0, std.error = 0,
      statistic = NA_real_, p.value = NA_real_
    )
  ) %>%
    dplyr::arrange(relative_time)

  task$results <- list(
    coefficients = coef_tbl,
    model = fit,
    method = "dynamic_twfe"
  )
}


#' Sun & Abraham (2021) Interaction-Weighted Estimator
#' @noRd
.estimate_sun_abraham <- function(task, panel, leads, lags,
                                   base_period, cluster) {
  # Sun & Abraham approach: interact cohort indicators with rel_time indicators
  # Cohort = treatment_time value

  # Identify cohorts
  cohorts <- sort(unique(panel[[task$treatment_time]][!is.na(panel[[task$treatment_time]])]))

  if (length(cohorts) < 2) {
    # Fall back to dynamic TWFE for single-cohort designs
    message("Only one treatment cohort found. Falling back to dynamic TWFE.")
    return(.estimate_dynamic_twfe(task, panel, leads, lags, base_period, cluster))
  }

  # Bin relative time
  panel$.rel_time_binned <- dplyr::case_when(
    is.na(panel$.rel_time) ~ NA_real_,
    panel$.rel_time < -leads ~ -leads,
    panel$.rel_time > lags ~ lags,
    TRUE ~ panel$.rel_time
  )

  event_times <- sort(unique(panel$.rel_time_binned[!is.na(panel$.rel_time_binned)]))
  event_times <- setdiff(event_times, base_period)

  # Create cohort x rel_time interactions
  dummy_names <- character(0)
  for (g in cohorts) {
    for (k in event_times) {
      col_name <- paste0(".IW_g", g, "_k", ifelse(k < 0, "m", "p"), abs(k))
      panel[[col_name]] <- ifelse(
        !is.na(panel[[task$treatment_time]]) &
          panel[[task$treatment_time]] == g &
          !is.na(panel$.rel_time_binned) &
          panel$.rel_time_binned == k,
        1, 0
      )
      dummy_names <- c(dummy_names, col_name)
    }
  }

  rhs <- paste(c(dummy_names,
                  paste0("factor(", task$unit_id, ")"),
                  paste0("factor(", task$time_id, ")")),
               collapse = " + ")
  fml <- stats::as.formula(paste(task$outcome, "~", rhs))

  fit <- stats::lm(fml, data = panel)
  fit_summary <- summary(fit)

  # Aggregate cohort-specific estimates to event-time estimates (IW weights)
  # Weight by cohort size
  cohort_sizes <- table(panel[[task$treatment_time]][!is.na(panel[[task$treatment_time]])])
  cohort_weights <- as.numeric(cohort_sizes) / sum(cohort_sizes)
  names(cohort_weights) <- names(cohort_sizes)

  coef_tbl <- tibble::tibble(relative_time = numeric(0), estimate = numeric(0),
                              std.error = numeric(0))

  for (k in event_times) {
    k_coefs <- numeric(0)
    k_names <- character(0)
    for (g in cohorts) {
      col_name <- paste0(".IW_g", g, "_k", ifelse(k < 0, "m", "p"), abs(k))
      if (col_name %in% names(stats::coef(fit))) {
        k_coefs <- c(k_coefs, stats::coef(fit)[col_name])
        k_names <- c(k_names, as.character(g))
      }
    }

    if (length(k_coefs) > 0) {
      # Weighted average
      w <- cohort_weights[k_names]
      w <- w / sum(w)
      est <- sum(k_coefs * w)
      # Approximate SE via delta method (diagonal)
      k_se <- fit_summary$coefficients[names(k_coefs), 2]
      se <- sqrt(sum((w * k_se)^2))

      coef_tbl <- dplyr::bind_rows(coef_tbl, tibble::tibble(
        relative_time = k, estimate = est, std.error = se
      ))
    }
  }

  coef_tbl <- coef_tbl %>%
    dplyr::mutate(
      statistic = estimate / std.error,
      p.value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    )

  # Add base period
  coef_tbl <- dplyr::bind_rows(
    coef_tbl,
    tibble::tibble(
      relative_time = base_period,
      estimate = 0, std.error = 0,
      statistic = NA_real_, p.value = NA_real_
    )
  ) %>%
    dplyr::arrange(relative_time)

  task$results <- list(
    coefficients = coef_tbl,
    model = fit,
    method = "sun_abraham"
  )
}


#' Plot Panel Event Study Results
#'
#' Create an event study plot from panel estimation results. Can also be
#' called via \code{plot_event_study()} when given a PanelEventStudyTask.
#'
#' @param task A PanelEventStudyTask with results.
#' @param confidence_level Confidence level for error bars. Default 0.95.
#' @param title Optional plot title.
#'
#' @return A ggplot2 object.
#'
#' @export
plot_panel_event_study <- function(task, confidence_level = 0.95,
                                    title = NULL) {
  if (!inherits(task, "PanelEventStudyTask")) {
    stop("task must be a PanelEventStudyTask.")
  }
  if (is.null(task$results)) {
    stop("No results found. Run estimate_panel_event_study() first.")
  }

  coefs <- task$results$coefficients

  if (!"relative_time" %in% names(coefs)) {
    stop("Results do not contain dynamic coefficients. Use method = 'dynamic_twfe' or 'sun_abraham'.")
  }

  z_val <- stats::qnorm(1 - (1 - confidence_level) / 2)

  coefs <- coefs %>%
    dplyr::mutate(
      ci_lower = estimate - z_val * std.error,
      ci_upper = estimate + z_val * std.error
    )

  if (is.null(title)) {
    title <- paste("Panel Event Study -", task$results$method)
  }

  ggplot2::ggplot(coefs, ggplot2::aes(x = relative_time, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted", color = "red",
                         alpha = 0.6) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                          fill = "steelblue", alpha = 0.2) +
    ggplot2::geom_point(color = "steelblue", size = 2) +
    ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
    ggplot2::labs(title = title, x = "Relative Time to Treatment",
                   y = "Estimate") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
