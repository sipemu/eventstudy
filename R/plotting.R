#' Plot Stocks
#'
#' Visualize the adjusted close prices or abnormal returns of selected stocks around a specified event date.
#' Allows for customization such as setting a maximum number of symbols to display and the option to
#' sample the symbols randomly.
#'
#' @param symbols_tbl A data frame containing stock data with columns 'symbols', 'date', 'adjusted', and
#'   'abnormal_return'. The 'symbols' column should contain stock symbols, and the 'date' column should
#'   contain dates in the format "dd.mm.yyyy".
#' @param target_variable A character string specifying the target variable to plot. Default is "firm_adjusted",
#'   which corresponds to adjusted close prices. Alternatively, you can use "abnormal_return" to plot
#'   abnormal returns.
#' @param add_event_date Add vertical line for event date (TRUE/FALSE).
#' @param max_symbols An integer specifying the maximum number of symbols to display. Default is 6.
#' @param do_sample A boolean specifying whether to randomly sample the symbols if the number of unique
#'   symbols is greater than `max_symbols`. Default is TRUE.
#'
#' @return A plotly plot object with the specified stock events.
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

  # Helper function for event Data line
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
  subplot(plots_list, nrows = length(plots_list) / 2, margin = 0.05)
}
