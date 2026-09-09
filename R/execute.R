#' Run a Complete Event Study
#'
#' Convenience wrapper that runs the full event study pipeline: prepare data,
#' fit models, and calculate test statistics in a single call.
#'
#' @param task An EventStudyTask object.
#' @param parameter_set A ParameterSet object defining the event study.
#'   Defaults to a new ParameterSet with default settings.
#' @param report Logical. When \code{FALSE} (default), the function behaves
#'   exactly as before this parameter was added. When \code{TRUE}, also renders
#'   a report via \code{\link{es_report}()} after the pipeline completes and
#'   attaches the rendered path(s) as \code{attr(task, "report_path")}.
#' @param report_args Named list forwarded to \code{\link{es_report}()} when
#'   \code{report = TRUE}. May include \code{output_file}, \code{format},
#'   \code{provider}, and any other argument accepted by \code{es_report()}.
#'   Defaults to \code{list()} (uses \code{es_report()} defaults, which render
#'   \code{"event_study_report.html"} in the working directory).
#' @param verbose Logical; if FALSE, suppress informational messages. Default
#'   \code{getOption("eventstudy.verbose", TRUE)}.
#'
#' @return The task object with all results computed. When \code{report = TRUE},
#'   the returned task additionally carries \code{attr(task, "report_path")}
#'   with the path(s) of the rendered report file(s). The return type is always
#'   a fitted \code{EventStudyTask} in both paths.
#'
#' @examples
#' \dontrun{
#' task <- run_event_study(my_task, ParameterSet$new(), report = TRUE)
#' attr(task, "report_path")  # path to the rendered HTML
#' }
#'
#' @family eventstudy-pipeline
#' @seealso
#'   \code{\link{prepare_event_study}}, \code{\link{fit_model}},
#'   \code{\link{calculate_statistics}}, \code{\link{MarketModel}},
#'   \code{\link{ARTTest}}
#'
#' @export
run_event_study = function(task,
                           parameter_set = ParameterSet$new(),
                           report = FALSE,
                           report_args = list(),
                           verbose = getOption("eventstudy.verbose", TRUE)) {
  task = prepare_event_study(task, parameter_set)
  task = fit_model(task, parameter_set)
  task = calculate_statistics(task, parameter_set)

  if (isTRUE(report)) {
    paths <- do.call(es_report, c(list(task = task), report_args))
    attr(task, "report_path") <- paths
    .inform(paste0("Report written to: ", paste(paths, collapse = ", ")), verbose)
  }

  task
}


#' Train the defined model on each event and calculate the abnormal return.
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @return task
#'
#' @family eventstudy-pipeline
#' @seealso
#'   \code{\link{run_event_study}}, \code{\link{prepare_event_study}},
#'   \code{\link{calculate_statistics}}
#'
#' @export
fit_model = function(task, parameter_set) {
  # Resolve the degenerate-input handling mode once for the entire fit pass
  mode <- .resolve_degenerate_mode(parameter_set$degenerate_handling)

  # Fit a return model for each event using an explicit row-indexed map so that
  # the outer row keys (event_id, firm_symbol) are available inside each fit
  # call.  purrr::pmap-inside-dplyr::mutate is forbidden per plan (NSE
  # evaluation is untested in this context); the explicit seq_len approach is
  # deterministic and avoids NSE ambiguity.
  task$data_tbl$model <- purrr::map(
    seq_len(nrow(task$data_tbl)),
    function(i) {
      .initialize_and_fit_model(
        task$data_tbl$data[[i]],
        parameter_set$return_model,
        degenerate_mode = mode,
        event_id        = task$data_tbl$event_id[[i]],
        firm_symbol     = task$data_tbl$firm_symbol[[i]]
      )
    }
  )

  # Calculate abnormal returns for each event
  task$data_tbl = task$data_tbl %>%
    dplyr::mutate(data = purrr::map2(.x=data,
                                     .y=model,
                                     .f=.calculate_abnormal_returns))
  task
}


#' @noRd
.initialize_and_fit_model <- function(data_tbl, return_model,
                                       degenerate_mode = "lenient",
                                       event_id = NULL,
                                       firm_symbol = NULL) {
  # Each event needs its own model, therefore a deep clone is necessary
  cloned_return_model = return_model$clone(deep=TRUE)
  # Thread contract context — models read these fields inside fit()
  cloned_return_model$degenerate_mode <- degenerate_mode
  cloned_return_model$event_id        <- event_id
  cloned_return_model$firm_symbol     <- firm_symbol
  cloned_return_model$fit(data_tbl)
  cloned_return_model
}


#' @noRd
.calculate_abnormal_returns <- function(data_tbl, return_model) {
  return_model$abnormal_returns(data_tbl)
}


#' Calculate test statistics
#'
#' @param task The event study task
#' @param parameter_set The parameter set that defines the event study.
#'
#' @return task
#'
#' @family eventstudy-pipeline
#' @seealso
#'   \code{\link{run_event_study}}, \code{\link{prepare_event_study}},
#'   \code{\link{fit_model}}
#'
#' @export
calculate_statistics = function(task, parameter_set) {
  # Single event statistic calculation
  if (!is.null(parameter_set$single_event_statistics)) {
    task$data_tbl = task$data_tbl %>%
      dplyr::mutate(statistics = purrr::map2(.x=data,
                                             .y=model,
                                             .f=.calculate_single_event_test_statistics,
                                             statistic_set=parameter_set$single_event_statistics))

    # Transpose results such that each test statistic result has its own column
    task$data_tbl$statistics %>%
      purrr::transpose() %>%
      as_tibble() -> stats_tbl

    # Remove existing statistic columns to allow idempotent re-runs
    existing_stat_cols <- intersect(names(stats_tbl), names(task$data_tbl))
    if (length(existing_stat_cols) > 0) {
      task$data_tbl <- task$data_tbl[, !names(task$data_tbl) %in% existing_stat_cols,
                                      drop = FALSE]
    }

    task$data_tbl = cbind(task$data_tbl, stats_tbl) %>%
      dplyr::select(-statistics)
  }

  # Multiple events test statistic calculation
  if (!is.null(parameter_set$multi_event_statistics)) {
    # The data must be reshaped for these calculations as we need to consider the
    # grouping of the events.
    task$data_tbl %>%
      dplyr::select(task$.keys, model) %>%
      dplyr::group_by(group) %>%
      tidyr::nest() %>%
      dplyr::rename(model = data) -> model_tbl

    task$aar_caar_tbl = task$data_tbl %>%
      dplyr::select(task$.keys, data) %>%
      tidyr::unnest(data) %>%
      dplyr::group_by(group) %>%
      tidyr::nest() %>%
      dplyr::left_join(model_tbl, by="group") %>%
      dplyr::mutate(statistics = purrr::map2(.x=data,
                                             .y=model,
                                             .f = .calculate_multiple_event_test_statistics,
                                             statistic_set=parameter_set$multi_event_statistics))

    # Transpose results such that each test statistic result has its own column
    task$aar_caar_tbl$statistics %>%
      purrr::transpose() %>%
      as_tibble() -> stats_tbl
    task$aar_caar_tbl = cbind(task$aar_caar_tbl, stats_tbl) %>%
      dplyr::select(-statistics)
  }

  task
}


#' @noRd
.calculate_single_event_test_statistics = function(data_tbl, fitted_model, statistic_set) {
  # Calculate test statistics
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic, data_tbl, fitted_model) {
      data_tbl %>%
        test_statistic$compute(fitted_model)
    }, data_tbl=data_tbl, fitted_model=fitted_model) -> res

  # Extract names
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic) {
      test_statistic$name
    }) -> stat_names

  names(res) = stat_names
  res
}


#' @noRd
.calculate_multiple_event_test_statistics = function(data_tbl, model, statistic_set) {
  # Calculate test statistics
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic, data_tbl) {
      data_tbl %>%
        test_statistic$compute(model)
    }, data_tbl=data_tbl) -> res

  # Extract names
  statistic_set$tests %>%
    purrr::map(.f = function(test_statistic) {
      test_statistic$name
    }) -> stat_names

  names(res) = stat_names
  res
}
