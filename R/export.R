#' Export Event Study Results
#'
#' Export event study results to CSV, Excel, or LaTeX formats.
#'
#' @param task A fitted EventStudyTask with statistics computed.
#' @param file Path to the output file. The file extension determines the format
#'   unless \code{format} is explicitly specified.
#' @param format Output format: "csv", "xlsx", or "latex". If NULL, inferred
#'   from the file extension.
#' @param which Which results to export. One or more of "ar", "car", "aar",
#'   "model". Defaults to all available.
#' @param stat_name Name of the multi-event test statistic to use for AAR/CAAR
#'   tables. Defaults to "CSectT".
#' @param ... Additional arguments passed to the format-specific writer.
#'
#' @return The file path (invisibly).
#'
#' @export
export_results <- function(task,
                           file,
                           format = NULL,
                           which = c("ar", "car", "aar", "model"),
                           stat_name = "CSectT",
                           ...) {
  if (!inherits(task, "EventStudyTask")) {
    stop("task must be an EventStudyTask object.")
  }

  # Infer format from file extension if not specified
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext,
      csv  = "csv",
      xlsx = "xlsx",
      tex  = "latex",
      stop("Cannot infer format from extension '.", ext,
           "'. Specify format explicitly.")
    )
  }
  format <- match.arg(format, c("csv", "xlsx", "latex"))

  # Build result tables
  tables <- .build_export_tables(task, which, stat_name)

  switch(format,
    csv   = .export_csv(tables, file, ...),
    xlsx  = .export_xlsx(tables, file, ...),
    latex = .export_latex(tables, file, ...)
  )

  invisible(file)
}


#' Build Export Tables
#' @noRd
.build_export_tables <- function(task, which, stat_name) {
  tables <- list()

  has_model <- "model" %in% names(task$data_tbl)
  has_ar <- has_model && any(purrr::map_lgl(
    task$data_tbl$data, ~"abnormal_returns" %in% names(.x)
  ))
  has_aar <- !is.null(task$aar_caar_tbl)

  if ("ar" %in% which && has_ar) {
    tables$ar <- task$data_tbl %>%
      dplyr::select(event_id, group, firm_symbol, data) %>%
      dplyr::mutate(data = purrr::map(data, function(d) {
        d %>%
          dplyr::filter(event_window == 1) %>%
          dplyr::select(dplyr::any_of(c(
            "relative_index", "abnormal_returns", "date",
            "firm_returns", "index_returns"
          )))
      })) %>%
      tidyr::unnest(data)
  }

  if ("car" %in% which && has_ar) {
    tables$car <- task$data_tbl %>%
      dplyr::select(event_id, group, firm_symbol, data) %>%
      dplyr::mutate(data = purrr::map(data, function(d) {
        d %>%
          dplyr::filter(event_window == 1) %>%
          dplyr::select(relative_index, abnormal_returns) %>%
          dplyr::mutate(car = cumsum(abnormal_returns))
      })) %>%
      tidyr::unnest(data)
  }

  if ("aar" %in% which && has_aar) {
    if (stat_name %in% names(task$aar_caar_tbl)) {
      aar_list <- purrr::map2(
        task$aar_caar_tbl$group,
        task$aar_caar_tbl[[stat_name]],
        function(grp, tbl) {
          tbl %>% dplyr::mutate(group = grp)
        }
      )
      tables$aar <- dplyr::bind_rows(aar_list)
    }
  }

  if ("model" %in% which && has_model) {
    tables$model <- purrr::pmap_dfr(
      list(task$data_tbl$event_id, task$data_tbl$firm_symbol,
           task$data_tbl$group, task$data_tbl$model),
      function(eid, sym, grp, model) {
        stats <- model$statistics
        tibble::tibble(
          event_id    = eid,
          firm_symbol = sym,
          group       = grp,
          is_fitted   = model$is_fitted,
          alpha       = stats$alpha %||% NA_real_,
          beta        = stats$beta %||% NA_real_,
          sigma       = stats$sigma %||% NA_real_,
          r2          = stats$r2 %||% NA_real_,
          degree_of_freedom = stats$degree_of_freedom %||% NA_real_,
          acf1        = stats$first_order_auto_correlation %||% NA_real_
        )
      }
    )
  }

  if (length(tables) == 0) {
    stop("No results available to export. Run the event study pipeline first.")
  }

  tables
}


#' Export to CSV
#' @noRd
.export_csv <- function(tables, file, ...) {
  if (length(tables) == 1) {
    utils::write.csv(tables[[1]], file = file, row.names = FALSE, ...)
  } else {
    # Multiple tables: write each with a suffix
    base <- tools::file_path_sans_ext(file)
    ext <- tools::file_ext(file)
    for (name in names(tables)) {
      out_file <- paste0(base, "_", name, ".", ext)
      utils::write.csv(tables[[name]], file = out_file, row.names = FALSE, ...)
    }
  }
}


#' Export to Excel
#' @noRd
.export_xlsx <- function(tables, file, ...) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel export. ",
         "Install it with: install.packages('openxlsx')")
  }

  wb <- openxlsx::createWorkbook()

  for (name in names(tables)) {
    sheet_name <- switch(name,
      ar    = "Abnormal Returns",
      car   = "Cumulative AR",
      aar   = "AAR & CAAR",
      model = "Model Statistics",
      name
    )
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeDataTable(wb, sheet_name, tables[[name]])

    # Auto-size columns
    openxlsx::setColWidths(wb, sheet_name,
                           cols = seq_along(tables[[name]]),
                           widths = "auto")
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}


#' Export to LaTeX
#' @noRd
.export_latex <- function(tables, file, ...) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for LaTeX export. ",
         "Install it with: install.packages('knitr')")
  }

  lines <- character(0)

  for (name in names(tables)) {
    caption <- switch(name,
      ar    = "Abnormal Returns",
      car   = "Cumulative Abnormal Returns",
      aar   = "Average Abnormal Returns and CAAR",
      model = "Model Statistics",
      name
    )

    tbl <- tables[[name]]
    # Round numeric columns for cleaner output
    num_cols <- vapply(tbl, is.numeric, logical(1))
    tbl[num_cols] <- lapply(tbl[num_cols], round, digits = 6)

    latex_tbl <- knitr::kable(tbl, format = "latex",
                               caption = caption,
                               booktabs = TRUE, ...)
    lines <- c(lines, "", latex_tbl, "")
  }

  writeLines(lines, file)
}


#' Tidy Event Study Results
#'
#' Extract event study results in a tidy (long-format) tibble compatible
#' with broom conventions. This enables integration with standard tidyverse
#' workflows.
#'
#' @param x An EventStudyTask object.
#' @param type Type of results to extract: "ar" (abnormal returns),
#'   "car" (cumulative abnormal returns), "aar" (average abnormal returns
#'   with test statistics), or "model" (model fit statistics).
#' @param stat_name For type "aar", the name of the multi-event test statistic.
#'   Defaults to "CSectT".
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with columns following broom conventions:
#'   \describe{
#'     \item{term}{Identifier for the observation (e.g., relative_index)}
#'     \item{estimate}{The estimated value (AR, CAR, AAR, or coefficient)}
#'     \item{std.error}{Standard error where available}
#'     \item{statistic}{Test statistic value}
#'     \item{p.value}{p-value where available}
#'   }
#'
#' @export
tidy.EventStudyTask <- function(x, type = c("ar", "car", "aar", "model"),
                                stat_name = "CSectT", ...) {
  type <- match.arg(type)

  switch(type,
    ar    = .tidy_ar(x),
    car   = .tidy_car(x),
    aar   = .tidy_aar(x, stat_name),
    model = .tidy_model(x)
  )
}


#' @noRd
.tidy_ar <- function(task) {
  if (!"model" %in% names(task$data_tbl)) {
    stop("Abnormal returns not computed. Run fit_model() first.")
  }

  task$data_tbl %>%
    dplyr::select(event_id, group, firm_symbol, data, model) %>%
    dplyr::mutate(tidy_data = purrr::map2(data, model, function(d, m) {
      sigma <- m$statistics$sigma
      df <- m$statistics$degree_of_freedom
      d %>%
        dplyr::filter(event_window == 1) %>%
        dplyr::transmute(
          term      = as.character(relative_index),
          estimate  = abnormal_returns,
          std.error = if (!is.null(sigma)) sigma else NA_real_,
          statistic = if (!is.null(sigma)) abnormal_returns / sigma else NA_real_,
          p.value   = if (!is.null(df) && !is.null(sigma)) {
            2 * stats::pt(abs(abnormal_returns / sigma), df = df, lower.tail = FALSE)
          } else NA_real_
        )
    })) %>%
    dplyr::select(event_id, group, firm_symbol, tidy_data) %>%
    tidyr::unnest(tidy_data)
}


#' @noRd
.tidy_car <- function(task) {
  if (!"model" %in% names(task$data_tbl)) {
    stop("Abnormal returns not computed. Run fit_model() first.")
  }

  task$data_tbl %>%
    dplyr::select(event_id, group, firm_symbol, data, model) %>%
    dplyr::mutate(tidy_data = purrr::map2(data, model, function(d, m) {
      sigma <- m$statistics$sigma
      df <- m$statistics$degree_of_freedom
      d %>%
        dplyr::filter(event_window == 1) %>%
        dplyr::transmute(
          term      = paste0("[", relative_index[1], ",", relative_index, "]"),
          estimate  = cumsum(abnormal_returns),
          n         = seq_len(dplyr::n()),
          std.error = if (!is.null(sigma)) sqrt(n) * sigma else NA_real_,
          statistic = if (!is.null(sigma)) estimate / (sqrt(n) * sigma) else NA_real_,
          p.value   = if (!is.null(df) && !is.null(sigma)) {
            2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
          } else NA_real_
        ) %>%
        dplyr::select(-n)
    })) %>%
    dplyr::select(event_id, group, firm_symbol, tidy_data) %>%
    tidyr::unnest(tidy_data)
}


#' @noRd
.tidy_aar <- function(task, stat_name) {
  if (is.null(task$aar_caar_tbl)) {
    stop("AAR/CAAR not computed. Run calculate_statistics() first.")
  }

  if (!stat_name %in% names(task$aar_caar_tbl)) {
    stop("Statistic '", stat_name, "' not found.")
  }

  purrr::map2_dfr(
    task$aar_caar_tbl$group,
    task$aar_caar_tbl[[stat_name]],
    function(grp, tbl) {
      # Detect which t/z column is available
      t_col <- intersect(c("aar_t", "aar_z"), names(tbl))
      ct_col <- intersect(c("caar_t", "caar_z"), names(tbl))

      tbl %>%
        dplyr::transmute(
          group     = grp,
          term      = as.character(relative_index),
          estimate  = aar,
          std.error = if (length(t_col) > 0) {
            ifelse(.data[[t_col[1]]] != 0, abs(aar / .data[[t_col[1]]]), NA_real_)
          } else NA_real_,
          statistic = if (length(t_col) > 0) .data[[t_col[1]]] else NA_real_,
          p.value   = if (length(t_col) > 0) {
            2 * stats::pnorm(abs(.data[[t_col[1]]]), lower.tail = FALSE)
          } else NA_real_,
          caar      = caar,
          caar_statistic = if (length(ct_col) > 0) .data[[ct_col[1]]] else NA_real_,
          caar_p.value   = if (length(ct_col) > 0) {
            2 * stats::pnorm(abs(.data[[ct_col[1]]]), lower.tail = FALSE)
          } else NA_real_
        )
    }
  )
}


#' @noRd
.tidy_model <- function(task) {
  if (!"model" %in% names(task$data_tbl)) {
    stop("Models not fitted. Run fit_model() first.")
  }

  purrr::pmap_dfr(
    list(task$data_tbl$event_id, task$data_tbl$firm_symbol,
         task$data_tbl$group, task$data_tbl$model),
    function(eid, sym, grp, model) {
      stats <- model$statistics
      rows <- list()

      if (!is.null(stats$alpha)) {
        rows$alpha <- tibble::tibble(
          event_id = eid, firm_symbol = sym, group = grp,
          term = "alpha", estimate = stats$alpha,
          p.value = stats$pval_alpha %||% NA_real_
        )
      }
      if (!is.null(stats$beta)) {
        rows$beta <- tibble::tibble(
          event_id = eid, firm_symbol = sym, group = grp,
          term = "beta", estimate = stats$beta,
          p.value = stats$pval_beta %||% NA_real_
        )
      }
      if (!is.null(stats$sigma)) {
        rows$sigma <- tibble::tibble(
          event_id = eid, firm_symbol = sym, group = grp,
          term = "sigma", estimate = stats$sigma,
          p.value = NA_real_
        )
      }
      if (!is.null(stats$r2)) {
        rows$r2 <- tibble::tibble(
          event_id = eid, firm_symbol = sym, group = grp,
          term = "r.squared", estimate = stats$r2,
          p.value = NA_real_
        )
      }

      dplyr::bind_rows(rows)
    }
  )
}
