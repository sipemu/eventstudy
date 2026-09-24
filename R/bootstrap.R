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
#' @family eventstudy-statistics
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

  firm_ids <- unique(ar_data$firm_symbol)
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
      aar_t = ifelse(is.finite(sd_aar) & sd_aar > 0,
                      sqrt(n) * aar / sd_aar, NA_real_)
    )

  # A12a (2026-09-24): observed CAAR and CAAR t must be built from the SAME
  # per-event CAR construction the bootstrap draws use below (mean over
  # events of each event's own coalesced-to-0 cumulative CAR), not from
  # cumsum() of the cross-event daily AAR mean. The two only coincide when
  # every event has a fully-observed event window; with a partial gap in
  # ONE event (but not all), cumsum(daily-mean-AAR) and mean(per-event-CAR)
  # diverge, and only the latter matches what the draws compute (STATS-03:
  # a missing AR mid-window contributes 0 to that event's own CAR).
  car_data <- ar_data %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(car = cumsum(dplyr::coalesce(abnormal_returns, 0))) %>%
    dplyr::ungroup()

  observed_car_stats <- car_data %>%
    dplyr::group_by(relative_index) %>%
    dplyr::summarise(
      observed_caar = mean(car, na.rm = TRUE),
      sd_caar = stats::sd(car, na.rm = TRUE),
      n_car = sum(!is.na(car)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      caar_t = ifelse(is.finite(sd_caar) & sd_caar > 0,
                       sqrt(n_car) * observed_caar / sd_caar, NA_real_)
    )

  observed <- observed %>%
    dplyr::left_join(
      observed_car_stats %>% dplyr::select(relative_index, observed_caar, caar_t),
      by = "relative_index"
    )

  obs_aar_t <- observed$aar_t
  obs_caar_t <- observed$caar_t

  # Bootstrap loop
  boot_aar_exceed <- rep(0L, nrow(observed))
  boot_caar_exceed <- rep(0L, nrow(observed))
  # A12b (2026-09-24): track the number of draws with a FINITE bootstrap
  # statistic per day, so a draw whose statistic is NA (e.g. a Rademacher
  # sign-flip that zeroes the cross-sectional sd) is excluded from BOTH the
  # numerator and the denominator, rather than silently counted as "did not
  # exceed" while still inflating the denominator.
  boot_aar_valid <- rep(0L, nrow(observed))
  boot_caar_valid <- rep(0L, nrow(observed))

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
    names(w) <- as.character(firm_ids)

    # Weighted abnormal returns
    boot_ar <- ar_data %>%
      dplyr::mutate(boot_ar = abnormal_returns * w[as.character(firm_symbol)])

    boot_stats <- boot_ar %>%
      dplyr::group_by(relative_index) %>%
      dplyr::summarise(
        boot_aar = mean(boot_ar, na.rm = TRUE),
        sd_boot = stats::sd(boot_ar, na.rm = TRUE),
        n = sum(!is.na(boot_ar)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(boot_aar_t = ifelse(is.finite(sd_boot) & sd_boot > 0,
                                         sqrt(n) * boot_aar / sd_boot, NA_real_))

    # A12b: a draw's statistic must be FINITE to count at all -- towards
    # either the exceedance numerator or the valid-draw denominator.
    draw_finite_aar <- is.finite(boot_stats$boot_aar_t)
    comparison_aar <- draw_finite_aar & (abs(boot_stats$boot_aar_t) >= abs(obs_aar_t))
    comparison_aar[is.na(comparison_aar)] <- FALSE
    boot_aar_exceed <- boot_aar_exceed + as.integer(comparison_aar)
    boot_aar_valid  <- boot_aar_valid + as.integer(draw_finite_aar)

    if (statistic %in% c("caar", "both")) {
      boot_car <- boot_ar %>%
        dplyr::group_by(event_id) %>%
        dplyr::mutate(boot_car = cumsum(dplyr::coalesce(boot_ar, 0))) %>%
        dplyr::group_by(relative_index) %>%
        dplyr::summarise(
          boot_caar = mean(boot_car, na.rm = TRUE),
          sd_boot_caar = stats::sd(boot_car, na.rm = TRUE),
          n = sum(!is.na(boot_car)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(boot_caar_t = ifelse(is.finite(sd_boot_caar) & sd_boot_caar > 0,
                                             sqrt(n) * boot_caar / sd_boot_caar,
                                             NA_real_))

      draw_finite_caar <- is.finite(boot_car$boot_caar_t)
      comparison_caar <- draw_finite_caar & (abs(boot_car$boot_caar_t) >= abs(obs_caar_t))
      comparison_caar[is.na(comparison_caar)] <- FALSE
      boot_caar_exceed <- boot_caar_exceed + as.integer(comparison_caar)
      boot_caar_valid  <- boot_caar_valid + as.integer(draw_finite_caar)
    }
  }

  # Bootstrap p-values; NA where the observed stat is NA (degenerate, e.g.
  # single firm) OR no draw produced a finite bootstrap statistic (A12b:
  # the denominator counts only VALID draws, not n_boot).
  boot_p_aar_raw <- (boot_aar_exceed + 1) / (boot_aar_valid + 1)
  boot_p_aar_raw[is.na(obs_aar_t) | boot_aar_valid == 0] <- NA_real_

  boot_p_caar_raw <- if (statistic %in% c("caar", "both")) {
    p <- (boot_caar_exceed + 1) / (boot_caar_valid + 1)
    p[is.na(obs_caar_t) | boot_caar_valid == 0] <- NA_real_
    p
  } else {
    NA_real_
  }

  result <- tibble::tibble(
    relative_index = observed$relative_index,
    observed_aar = observed$aar,
    observed_caar = observed$observed_caar,
    boot_p_aar = boot_p_aar_raw,
    boot_p_caar = boot_p_caar_raw
  )

  result
}
