#' Model Diagnostics for Event Study
#'
#' Run diagnostic tests on fitted event study models. Tests include
#' normality of residuals, autocorrelation, and basic model fit quality.
#'
#' @param task A fitted EventStudyTask (after fit_model has been called).
#' @param event_id Optional event identifier. If NULL, runs diagnostics
#'   for all events.
#'
#' @return A tibble with diagnostic test results for each event.
#'
#' @export
model_diagnostics = function(task, event_id = NULL) {
  if (!"model" %in% names(task$data_tbl)) {
    stop("Models have not been fitted yet. Run fit_model() first.")
  }

  if (!is.null(event_id)) {
    rows = task$data_tbl %>% dplyr::filter(event_id == !!event_id)
  } else {
    rows = task$data_tbl
  }

  results = purrr::pmap(
    list(rows$event_id, rows$firm_symbol, rows$model, rows$data),
    function(eid, sym, model, data) {
      if (!model$is_fitted) {
        return(tibble::tibble(
          event_id = eid,
          firm_symbol = sym,
          is_fitted = FALSE,
          shapiro_p = NA_real_,
          dw_stat = NA_real_,
          ljung_box_p = NA_real_,
          acf1 = NA_real_,
          sigma = NA_real_,
          r2 = NA_real_
        ))
      }

      stats = model$statistics
      residuals = stats$residuals

      # Shapiro-Wilk normality test
      shapiro_p = tryCatch({
        # Shapiro-Wilk requires 3 <= n <= 5000
        n = length(na.omit(residuals))
        if (n >= 3 && n <= 5000) {
          shapiro.test(na.omit(residuals))$p.value
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_)

      # Durbin-Watson statistic (approximate)
      dw_stat = tryCatch({
        resid = na.omit(residuals)
        sum(diff(resid)^2) / sum(resid^2)
      }, error = function(e) NA_real_)

      # Ljung-Box test for autocorrelation
      ljung_box_p = tryCatch({
        resid = na.omit(residuals)
        if (length(resid) > 10) {
          Box.test(resid, lag = min(10, floor(length(resid) / 5)),
                   type = "Ljung-Box")$p.value
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_)

      tibble::tibble(
        event_id    = eid,
        firm_symbol = sym,
        is_fitted   = TRUE,
        shapiro_p   = shapiro_p,
        dw_stat     = dw_stat,
        ljung_box_p = ljung_box_p,
        acf1        = stats$first_order_auto_correlation,
        sigma       = stats$sigma,
        r2          = stats$r2
      )
    }
  )

  dplyr::bind_rows(results)
}


#' Pre-trend Test for Event Study
#'
#' Performs a joint F-test of pre-event abnormal returns to assess whether
#' there are statistically significant pre-event effects, which would
#' indicate potential model misspecification or confounding.
#'
#' @param task A fitted EventStudyTask with abnormal returns computed.
#' @param group Optional group to filter by.
#'
#' @return A tibble with pre-trend test results for each group.
#'
#' @export
pretrend_test = function(task, group = NULL) {
  if (!"model" %in% names(task$data_tbl)) {
    stop("Models have not been fitted. Run fit_model() first.")
  }

  data = task$data_tbl %>%
    dplyr::select(task$.keys, data) %>%
    tidyr::unnest(data)

  if (!is.null(group)) {
    data = data %>% dplyr::filter(group == !!group)
  }

  # Get pre-event abnormal returns (relative_index < 0 within event window)
  pre_event = data %>%
    dplyr::filter(event_window == 1, relative_index < 0)

  if (nrow(pre_event) == 0) {
    stop("No pre-event observations found in event window.")
  }

  # Test by group
  results = pre_event %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n_pre_periods  = dplyr::n_distinct(relative_index),
      n_events       = dplyr::n_distinct(event_id),
      mean_pre_ar    = mean(abnormal_returns, na.rm = TRUE),
      sd_pre_ar      = sd(abnormal_returns, na.rm = TRUE),
      # Joint test: H0: all pre-event AARs = 0
      # Using F = (mean(AR)^2 / var(AR)) * N approximation
      t_stat         = sqrt(sum(!is.na(abnormal_returns))) *
                         mean(abnormal_returns, na.rm = TRUE) /
                         sd(abnormal_returns, na.rm = TRUE),
      p_value        = 2 * (1 - stats::pt(abs(t_stat), df = sum(!is.na(abnormal_returns)) - 1)),
      .groups = "drop"
    )

  results
}
