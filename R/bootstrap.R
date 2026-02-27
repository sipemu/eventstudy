#' Wild Bootstrap Inference for Event Studies
#'
#' Computes bootstrap p-values for AAR and CAAR test statistics using
#' the wild bootstrap approach. Per-firm random weights preserve the
#' cross-sectional dependence structure while randomizing the sign of
#' abnormal returns under the null hypothesis.
#'
#' @param task A fitted EventStudyTask with abnormal returns computed.
#' @param n_boot Number of bootstrap replications. Default 999.
#' @param weight_type Type of bootstrap weights: \code{"rademacher"} (default,
#'   +1/-1 with equal probability) or \code{"mammen"} (Mammen two-point
#'   distribution).
#' @param statistic Which statistic to bootstrap: \code{"aar"}, \code{"caar"},
#'   or \code{"both"} (default).
#' @param group Optional group name to filter.
#' @param seed Optional seed for reproducibility.
#'
#' @return A tibble with columns: \code{relative_index}, \code{observed_aar},
#'   \code{observed_caar}, \code{boot_p_aar}, \code{boot_p_caar}.
#'
#' @export
bootstrap_test <- function(task, n_boot = 999L, weight_type = "rademacher",
                            statistic = "both", group = NULL, seed = NULL) {
  if (!inherits(task, "EventStudyTask")) {
    stop("task must be an EventStudyTask.")
  }
  if (is.null(task$data_tbl)) {
    stop("Task has no data. Run the event study pipeline first.")
  }

  weight_type <- match.arg(weight_type, c("rademacher", "mammen"))
  statistic <- match.arg(statistic, c("aar", "caar", "both"))

  if (!is.null(seed)) set.seed(seed)

  # Extract event-window abnormal returns from all events
  ar_data <- task$data_tbl %>%
    dplyr::select(event_id, firm_symbol, data) %>%
    tidyr::unnest(data) %>%
    dplyr::filter(event_window == 1) %>%
    dplyr::select(event_id, firm_symbol, relative_index, abnormal_returns)

  if (!is.null(group)) {
    group_events <- task$data_tbl %>%
      dplyr::filter(group == !!group) %>%
      dplyr::pull(event_id)
    ar_data <- ar_data %>% dplyr::filter(event_id %in% group_events)
  }

  firm_ids <- unique(ar_data$event_id)
  n_firms <- length(firm_ids)

  # Observed AAR and t-statistics
  observed <- ar_data %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(
      aar = mean(abnormal_returns, na.rm = TRUE),
      sd_aar = stats::sd(abnormal_returns, na.rm = TRUE),
      n = sum(!is.na(abnormal_returns)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      aar_t = sqrt(n) * aar / sd_aar,
      caar = cumsum(aar)
    )

  # CAAR t-stat: cumulative SD via CAR per firm
  car_data <- ar_data %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(car = cumsum(abnormal_returns)) %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(
      sd_caar = stats::sd(car, na.rm = TRUE),
      n = sum(!is.na(car)),
      .groups = "drop"
    )

  observed <- observed %>%
    dplyr::left_join(car_data, by = "relative_index", suffix = c("", "_car")) %>%
    dplyr::mutate(caar_t = sqrt(n) * caar / sd_caar)

  obs_aar_t <- observed$aar_t
  obs_caar_t <- observed$caar_t

  # Bootstrap loop
  boot_aar_exceed <- rep(0L, nrow(observed))
  boot_caar_exceed <- rep(0L, nrow(observed))

  for (b in seq_len(n_boot)) {
    # Generate per-firm weights
    if (weight_type == "rademacher") {
      w <- sample(c(-1, 1), n_firms, replace = TRUE)
    } else {
      # Mammen two-point distribution
      p <- (sqrt(5) + 1) / (2 * sqrt(5))
      w <- ifelse(stats::runif(n_firms) < p,
                   -(sqrt(5) - 1) / 2,
                   (sqrt(5) + 1) / 2)
    }
    names(w) <- firm_ids

    # Weighted abnormal returns
    boot_ar <- ar_data %>%
      dplyr::mutate(boot_ar = abnormal_returns * w[event_id])

    boot_stats <- boot_ar %>%
      dplyr::group_by(relative_index) %>%
      dplyr::summarise(
        boot_aar = mean(boot_ar, na.rm = TRUE),
        sd_boot = stats::sd(boot_ar, na.rm = TRUE),
        n = sum(!is.na(boot_ar)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(boot_aar_t = sqrt(n) * boot_aar / sd_boot)

    boot_aar_exceed <- boot_aar_exceed +
      as.integer(abs(boot_stats$boot_aar_t) >= abs(obs_aar_t))

    if (statistic %in% c("caar", "both")) {
      boot_car <- boot_ar %>%
        dplyr::group_by(event_id) %>%
        dplyr::mutate(boot_car = cumsum(boot_ar)) %>%
        dplyr::group_by(relative_index) %>%
        dplyr::summarise(
          boot_caar = mean(boot_car, na.rm = TRUE),
          sd_boot_caar = stats::sd(boot_car, na.rm = TRUE),
          n = sum(!is.na(boot_car)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(boot_caar_t = sqrt(n) * boot_caar / sd_boot_caar)

      boot_caar_exceed <- boot_caar_exceed +
        as.integer(abs(boot_car$boot_caar_t) >= abs(obs_caar_t))
    }
  }

  # Bootstrap p-values
  result <- tibble::tibble(
    relative_index = observed$relative_index,
    observed_aar = observed$aar,
    observed_caar = observed$caar,
    boot_p_aar = (boot_aar_exceed + 1) / (n_boot + 1),
    boot_p_caar = (boot_caar_exceed + 1) / (n_boot + 1)
  )

  result
}
