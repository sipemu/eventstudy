#' Plot Stocks
#'
#' Visualize the adjusted close prices or abnormal returns of selected stocks
#' around a specified event date.
#'
#' @param task An EventStudyTask object.
#' @param target_variable A character string specifying the target variable
#'   to plot. Default is "firm_adjusted".
#' @param add_event_date Add vertical line for event date (TRUE/FALSE).
#' @param max_symbols An integer specifying the maximum number of symbols
#'   to display. Default is 6.
#' @param do_sample A boolean specifying whether to randomly sample the
#'   symbols if the number exceeds max_symbols. Default is TRUE.
#'
#' @return A plotly plot object.
#' @export
plot_stocks <- function(task,
                        target_variable="firm_adjusted",
                        add_event_date=FALSE,
                        max_symbols=6,
                        do_sample=TRUE) {

  # Sample symbols
  symbols = task$symbols
  if (length(symbols) > max_symbols) {
    if (do_sample) {
      symbols = sample(symbols, max_symbols)
    } else {
      symbols = symbols[1:max_symbols]
    }
  }

  # Helper function for event date line
  vline <- function(x = 0, color = "grey") {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      text = "Event Date",
      line = list(color = color, dash="dot")
    )
  }

  plots_list = list()
  symbols_tbl = task$symbol_data
  for (symbol in symbols) {
    symbol_data_tbl = symbols_tbl %>%
      dplyr::filter(firm_symbol == !!symbol) %>%
      .[["data"]] %>%
      .[[1]]

    request_tbl = symbols_tbl %>%
      dplyr::filter(firm_symbol == !!symbol) %>%
      .[["request"]] %>%
      .[[1]]
    event_date = as.Date(request_tbl$event_date, format="%d.%m.%Y")

    symbol_data_tbl$date <- as.Date(symbol_data_tbl$date, format="%d.%m.%Y")

    v_shape = NULL
    if (add_event_date) {
      v_shape = list(vline(as.Date(event_date)))
    }

    plot <- plot_ly(data = symbol_data_tbl) %>%
      add_trace(x    = ~date,
                y    = ~get(target_variable),
                type = 'scatter',
                mode = 'lines',
                name = symbol) %>%
      layout(shapes = v_shape,
             xaxis  = list(title = "Date"),
             yaxis  = list(title = target_variable),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5))

    plots_list[[symbol]] <- plot
  }

  # Combine individual plots into a single plot
  subplot(plots_list, nrows = max(1, length(plots_list) / 2), margin = 0.05)
}


#' Plot Event Study Results
#'
#' Create an event study plot showing abnormal returns (AR), cumulative
#' abnormal returns (CAR), average abnormal returns (AAR), or cumulative
#' average abnormal returns (CAAR) with confidence bands.
#'
#' @param task A fitted EventStudyTask with statistics computed.
#' @param type Type of plot: "ar", "car", "aar", or "caar".
#' @param event_id Event identifier for single-event plots ("ar", "car").
#' @param group Group name for multi-event plots ("aar", "caar").
#' @param stat_name Name of the multi-event test statistic to use
#'   for AAR/CAAR plots. Defaults to "CSectT".
#' @param confidence_level Confidence level for the bands. Default is 0.95.
#' @param title Optional plot title.
#'
#' @return A ggplot2 plot object.
#'
#' @export
plot_event_study <- function(task,
                             type = "car",
                             event_id = NULL,
                             group = NULL,
                             stat_name = "CSectT",
                             confidence_level = 0.95,
                             title = NULL) {

  if (type %in% c("ar", "car")) {
    return(.plot_single_event(task, type, event_id, confidence_level, title))
  } else if (type %in% c("aar", "caar")) {
    return(.plot_multi_event(task, type, group, stat_name, confidence_level, title))
  } else {
    stop("type must be one of: 'ar', 'car', 'aar', 'caar'")
  }
}


#' @noRd
.plot_single_event <- function(task, type, event_id, confidence_level, title) {
  if (is.null(event_id)) {
    event_id = task$data_tbl$event_id[1]
  }

  row = task$data_tbl %>%
    dplyr::filter(event_id == !!event_id)

  if (nrow(row) == 0) {
    stop("Event ID '", event_id, "' not found.")
  }

  data = row$data[[1]] %>%
    dplyr::filter(event_window == 1)

  if (!"abnormal_returns" %in% names(data)) {
    stop("Abnormal returns not computed. Run fit_model() first.")
  }

  model = row$model[[1]]
  sigma = model$statistics$sigma

  z_val = stats::qnorm(1 - (1 - confidence_level) / 2)

  if (type == "ar") {
    data = data %>%
      dplyr::mutate(
        value = abnormal_returns,
        ci_lower = -z_val * sigma,
        ci_upper = z_val * sigma
      )
    y_label = "Abnormal Return"
    if (is.null(title)) title = paste("Abnormal Returns - Event", event_id)
  } else {
    data = data %>%
      dplyr::mutate(
        value = cumsum(abnormal_returns),
        n = 1:dplyr::n(),
        ci_lower = -z_val * sqrt(n) * sigma,
        ci_upper = z_val * sqrt(n) * sigma
      )
    y_label = "Cumulative Abnormal Return"
    if (is.null(title)) title = paste("Cumulative Abnormal Returns - Event", event_id)
  }

  ggplot2::ggplot(data, ggplot2::aes(x = relative_index)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                         fill = "steelblue", alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = value), color = "steelblue", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = value), color = "steelblue", size = 1.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", alpha = 0.6) +
    ggplot2::labs(title = title, x = "Event Time", y = y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


#' @noRd
.plot_multi_event <- function(task, type, group, stat_name, confidence_level, title) {
  if (is.null(task$aar_caar_tbl)) {
    stop("AAR/CAAR not computed. Run calculate_statistics() first.")
  }

  tbl = task$aar_caar_tbl
  if (!is.null(group)) {
    tbl = tbl %>% dplyr::filter(group == !!group)
  }

  if (nrow(tbl) == 0) {
    stop("No data found for group '", group, "'.")
  }

  if (!stat_name %in% names(tbl)) {
    stop("Statistic '", stat_name, "' not found.")
  }

  # Get the stats tibble (first group if multiple)
  stats_data = tbl[[stat_name]][[1]]
  grp_name = tbl$group[1]

  z_val = stats::qnorm(1 - (1 - confidence_level) / 2)

  if (type == "aar") {
    if (!"aar" %in% names(stats_data) || !"aar_t" %in% names(stats_data)) {
      stop("AAR or AAR t-stat not found in results.")
    }
    # Compute CI from the implicit standard error
    stats_data = stats_data %>%
      dplyr::mutate(
        value = aar,
        se = ifelse(aar_t != 0, abs(aar / aar_t), NA_real_),
        ci_lower = aar - z_val * se,
        ci_upper = aar + z_val * se
      )
    y_label = "Average Abnormal Return"
    if (is.null(title)) title = paste("AAR -", grp_name)
  } else {
    if (!"caar" %in% names(stats_data)) {
      stop("CAAR not found in results.")
    }
    # Use caar_t if available, otherwise approximate
    t_col = intersect(c("caar_t", "caar_z"), names(stats_data))
    stats_data = stats_data %>%
      dplyr::mutate(
        value = caar,
        se = if (length(t_col) > 0 && any(!is.na(.data[[t_col[1]]]))) {
          ifelse(.data[[t_col[1]]] != 0, abs(caar / .data[[t_col[1]]]), NA_real_)
        } else {
          NA_real_
        },
        ci_lower = caar - z_val * se,
        ci_upper = caar + z_val * se
      )
    y_label = "Cumulative Average Abnormal Return"
    if (is.null(title)) title = paste("CAAR -", grp_name)
  }

  p = ggplot2::ggplot(stats_data, ggplot2::aes(x = relative_index))

  # Only add ribbon if we have valid CIs
  if (any(!is.na(stats_data$ci_lower))) {
    p = p + ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                                 fill = "steelblue", alpha = 0.2)
  }

  p +
    ggplot2::geom_line(ggplot2::aes(y = value), color = "steelblue", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = value), color = "steelblue", size = 1.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", alpha = 0.6) +
    ggplot2::labs(title = title, x = "Event Time", y = y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


#' Plot Model Diagnostics
#'
#' Create diagnostic plots for fitted event study models, including
#' residual plots and Q-Q plots.
#'
#' @param task A fitted EventStudyTask.
#' @param event_id The event identifier to plot diagnostics for.
#'
#' @return A ggplot2 plot arranged with patchwork-style layout.
#'
#' @export
plot_diagnostics <- function(task, event_id = NULL) {
  if (!"model" %in% names(task$data_tbl)) {
    stop("Models have not been fitted yet. Run fit_model() first.")
  }

  if (is.null(event_id)) {
    event_id = task$data_tbl$event_id[1]
  }

  row = task$data_tbl %>%
    dplyr::filter(event_id == !!event_id)

  if (nrow(row) == 0) {
    stop("Event ID '", event_id, "' not found.")
  }

  model = row$model[[1]]
  if (!model$is_fitted) {
    stop("Model for event ", event_id, " is not fitted.")
  }

  residuals = model$statistics$residuals
  resid_df = tibble::tibble(
    index = seq_along(residuals),
    residuals = residuals
  )

  # Residuals vs index plot
  p1 = ggplot2::ggplot(resid_df, ggplot2::aes(x = index, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Residuals", x = "Observation", y = "Residual") +
    ggplot2::theme_minimal()

  # Q-Q plot
  p2 = ggplot2::ggplot(resid_df, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq(color = "steelblue", alpha = 0.5) +
    ggplot2::stat_qq_line(color = "red", linetype = "dashed") +
    ggplot2::labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggplot2::theme_minimal()

  # Residual histogram
  p3 = ggplot2::ggplot(resid_df, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(fill = "steelblue", alpha = 0.7, bins = 30) +
    ggplot2::labs(title = "Residual Distribution", x = "Residual", y = "Count") +
    ggplot2::theme_minimal()

  # ACF plot (manual since ggplot2 doesn't have native ACF)
  acf_vals = acf(na.omit(residuals), plot = FALSE, lag.max = 20)
  acf_df = tibble::tibble(
    lag = acf_vals$lag[-1, , 1],
    acf = acf_vals$acf[-1, , 1]
  )
  ci = stats::qnorm(0.975) / sqrt(length(na.omit(residuals)))

  p4 = ggplot2::ggplot(acf_df, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "blue", alpha = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), color = "steelblue") +
    ggplot2::geom_point(color = "steelblue") +
    ggplot2::labs(title = "ACF of Residuals", x = "Lag", y = "Autocorrelation") +
    ggplot2::theme_minimal()

  # Combine plots in a 2x2 grid
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}
