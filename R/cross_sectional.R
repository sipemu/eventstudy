#' Cross-Sectional Regression of CARs
#'
#' Regress cumulative abnormal returns (CARs) on firm characteristics to
#' explain cross-sectional variation in event effects. Supports OLS with
#' heteroskedasticity-consistent (HC) standard errors.
#'
#' @param task A fitted EventStudyTask with abnormal returns computed.
#' @param formula A formula with the response on the left (ignored; CAR is
#'   always the dependent variable) and explanatory variables on the right,
#'   e.g., \code{~ log_market_cap + leverage}.
#' @param data A data frame of firm characteristics. Must contain an
#'   \code{event_id} column to merge with CARs.
#' @param car_window Two-element integer vector specifying the CAR window
#'   as \code{c(start, end)} relative indices. Default is the full event window.
#' @param robust Logical. If TRUE and the \pkg{sandwich} package is available,
#'   compute HC1 robust standard errors. Default TRUE.
#'
#' @return A list with class \code{"es_cross_sectional"} containing:
#'   \describe{
#'     \item{model}{The fitted \code{lm} object}
#'     \item{coefficients}{Coefficient table with (robust) standard errors}
#'     \item{r_squared}{R-squared of the regression}
#'     \item{n_obs}{Number of observations}
#'     \item{car_data}{The merged CAR + characteristics data}
#'   }
#'
#' @export
cross_sectional_regression <- function(task, formula, data,
                                        car_window = NULL,
                                        robust = TRUE) {
  if (!inherits(task, "EventStudyTask")) {
    stop("task must be an EventStudyTask object.")
  }
  if (!"model" %in% names(task$data_tbl)) {
    stop("Abnormal returns not computed. Run fit_model() first.")
  }
  if (!"event_id" %in% names(data)) {
    stop("data must contain an 'event_id' column for merging.")
  }

  # Extract CARs for each event
  cars <- .extract_cars(task, car_window)

  # Merge with firm characteristics
  merged <- dplyr::inner_join(cars, data, by = "event_id")

  if (nrow(merged) == 0) {
    stop("No matching event_id values between task and data.")
  }

  # Build regression formula: car ~ <user formula RHS>
  rhs <- as.character(formula)[length(as.character(formula))]
  reg_formula <- stats::as.formula(paste("car ~", rhs))

  # Fit OLS
  fit <- stats::lm(reg_formula, data = merged)
  fit_summary <- summary(fit)

  # Robust standard errors
  if (robust && requireNamespace("sandwich", quietly = TRUE)) {
    vcov_hc <- sandwich::vcovHC(fit, type = "HC1")
    se <- sqrt(diag(vcov_hc))
    coef_tbl <- data.frame(
      estimate  = stats::coef(fit),
      std.error = se,
      statistic = stats::coef(fit) / se,
      p.value   = 2 * stats::pt(abs(stats::coef(fit) / se),
                                  df = fit$df.residual, lower.tail = FALSE),
      row.names = names(stats::coef(fit))
    )
  } else {
    coef_tbl <- as.data.frame(fit_summary$coefficients)
    names(coef_tbl) <- c("estimate", "std.error", "statistic", "p.value")
    if (robust) {
      message("Package 'sandwich' not available. Using OLS standard errors.")
    }
  }

  result <- list(
    model = fit,
    coefficients = coef_tbl,
    r_squared = fit_summary$r.squared,
    adj_r_squared = fit_summary$adj.r.squared,
    n_obs = nrow(merged),
    car_data = merged
  )
  class(result) <- "es_cross_sectional"
  result
}


#' Extract CARs from Task
#' @noRd
.extract_cars <- function(task, car_window) {
  purrr::pmap_dfr(
    list(task$data_tbl$event_id, task$data_tbl$firm_symbol,
         task$data_tbl$group, task$data_tbl$data),
    function(eid, sym, grp, d) {
      event_data <- d %>%
        dplyr::filter(event_window == 1)

      if (!"abnormal_returns" %in% names(event_data)) {
        return(tibble::tibble(
          event_id = eid, firm_symbol = sym, group = grp, car = NA_real_
        ))
      }

      if (!is.null(car_window)) {
        event_data <- event_data %>%
          dplyr::filter(relative_index >= car_window[1],
                        relative_index <= car_window[2])
      }

      tibble::tibble(
        event_id = eid,
        firm_symbol = sym,
        group = grp,
        car = sum(event_data$abnormal_returns, na.rm = TRUE)
      )
    }
  )
}


#' Print Cross-Sectional Regression Results
#'
#' @param x An \code{es_cross_sectional} object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.es_cross_sectional <- function(x, ...) {
  cat("Cross-Sectional Regression of CARs\n")
  cat("===================================\n")
  cat("N:", x$n_obs, "\n")
  cat("R-squared:", round(x$r_squared, 4), "\n")
  cat("Adj. R-squared:", round(x$adj_r_squared, 4), "\n\n")
  cat("Coefficients:\n")
  base::print.data.frame(round(x$coefficients, 6))
  invisible(x)
}


#' Compare CARs Across Groups
#'
#' Performs a t-test (two groups) or ANOVA (3+ groups) to test whether
#' CARs differ significantly across groups.
#'
#' @param task A fitted EventStudyTask.
#' @param group_var Name of the grouping variable. Defaults to "group"
#'   (the standard group column in EventStudyTask).
#' @param car_window Optional two-element vector for CAR window.
#'
#' @return A list with test results and group-level summary statistics.
#'
#' @export
car_by_group <- function(task, group_var = "group", car_window = NULL) {
  cars <- .extract_cars(task, car_window)

  if (!group_var %in% names(cars)) {
    stop("Group variable '", group_var, "' not found.")
  }

  groups <- unique(cars[[group_var]])
  n_groups <- length(groups)

  # Summary by group
  group_summary <- cars %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_car = mean(car, na.rm = TRUE),
      sd_car = sd(car, na.rm = TRUE),
      median_car = stats::median(car, na.rm = TRUE),
      min_car = min(car, na.rm = TRUE),
      max_car = max(car, na.rm = TRUE),
      .groups = "drop"
    )

  # Test
  if (n_groups == 2) {
    g1 <- cars$car[cars[[group_var]] == groups[1]]
    g2 <- cars$car[cars[[group_var]] == groups[2]]
    test <- stats::t.test(g1, g2)
    test_name <- "Welch Two-Sample t-test"
  } else if (n_groups > 2) {
    fml <- stats::as.formula(paste("car ~", group_var))
    test <- stats::oneway.test(fml, data = cars)
    test_name <- "One-way ANOVA (Welch)"
  } else {
    test <- NULL
    test_name <- "Only one group -- no test performed"
  }

  list(
    summary = group_summary,
    test = test,
    test_name = test_name,
    n_groups = n_groups
  )
}


#' CAR Quantiles
#'
#' Compute quantiles of the CAR distribution across events.
#'
#' @param task A fitted EventStudyTask.
#' @param probs Probability vector for quantiles. Default
#'   \code{c(0.05, 0.25, 0.5, 0.75, 0.95)}.
#' @param car_window Optional two-element vector for CAR window.
#'
#' @return A named numeric vector of quantiles.
#'
#' @export
car_quantiles <- function(task, probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                           car_window = NULL) {
  cars <- .extract_cars(task, car_window)
  stats::quantile(cars$car, probs = probs, na.rm = TRUE)
}


#' Plot CAR Distribution
#'
#' Create a histogram of CARs across events, optionally colored by group.
#'
#' @param task A fitted EventStudyTask.
#' @param car_window Optional two-element vector for CAR window.
#' @param bins Number of histogram bins. Default 30.
#' @param by_group Logical. If TRUE, color histogram by group.
#' @param title Optional plot title.
#'
#' @return A ggplot2 object.
#'
#' @export
plot_car_distribution <- function(task, car_window = NULL,
                                   bins = 30, by_group = FALSE,
                                   title = NULL) {
  cars <- .extract_cars(task, car_window)

  if (is.null(title)) title <- "Distribution of Cumulative Abnormal Returns"

  p <- ggplot2::ggplot(cars, ggplot2::aes(x = car))

  if (by_group && length(unique(cars$group)) > 1) {
    p <- p +
      ggplot2::geom_histogram(ggplot2::aes(fill = group),
                               bins = bins, alpha = 0.7,
                               position = "identity") +
      ggplot2::scale_fill_brewer(palette = "Set2")
  } else {
    p <- p +
      ggplot2::geom_histogram(fill = "steelblue", alpha = 0.7, bins = bins)
  }

  p +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = "CAR", y = "Count") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
