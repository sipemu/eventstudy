#' Degenerate-Input Contract for EventStudy Models
#'
#' @name degenerate-input-contract
#' @title Degenerate-Input Contract for EventStudy Models
#'
#' @description
#' The degenerate-input contract defines how all EventStudy return models
#' behave when the estimation data is degenerate. Two modes are supported:
#'
#' \strong{Lenient (default):} The model sets \code{is_fitted = FALSE},
#' emits exactly one \code{warning()} per \code{(event_id, firm_symbol)}
#' per fit call, and propagates \code{NA} through all abnormal returns
#' and downstream statistics. No event is silently dropped; zeros are
#' never substituted for \code{NA}.
#'
#' \strong{Strict:} The model raises a descriptive \code{stop()} error
#' naming the component, \code{event_id}, and \code{firm_symbol}, plus
#' the specific reason for the degeneracy.
#'
#' \strong{Configuration:}
#' \itemize{
#'   \item Via \code{ParameterSet}: \code{ParameterSet$new(degenerate_handling = "strict")}
#'   \item Via package option: \code{options(EventStudy.degenerate_handling = "strict")}
#' }
#' The ParameterSet field takes precedence over the package option; if
#' neither is set, the default \code{"lenient"} mode is used.
#'
#' \strong{Degenerate conditions covered:}
#' \itemize{
#'   \item Fewer than 2 finite observations in the estimation window
#'     (insufficient observations for OLS).
#'   \item Zero or near-zero variance in index returns
#'     (\code{sd < .Machine$double.eps}), making OLS estimation undefined.
#'   \item Single-event group (relevant for multi-event statistics;
#'     applied in Phase 2).
#'   \item \code{NA} propagation from upstream pipeline steps.
#' }
#'
#' \strong{NA propagation semantics:}
#' When \code{is_fitted = FALSE}, \code{model$abnormal_returns()} returns
#' a tibble with \code{abnormal_returns = NA_real_} for all rows. All
#' downstream test statistics that depend on fitted models then receive
#' \code{NA} inputs and propagate \code{NA} to their outputs. The event
#' is retained in the output tibble -- it is never silently dropped.
#'
#' \strong{Multi-event exclusion (2026-09-24 re-evaluation, item A2):}
#' A single-event's \code{NA} propagation above is not enough for
#' cumulative (CAR-based) multi-event statistics (CSectT, PatellZ, Sign,
#' GeneralizedSign, BMP, KolariPynnonen): an event with ZERO finite
#' event-window abnormal returns must be EXCLUDED from every such
#' statistic's group, not merely NA-propagated, because a cumulative sum
#' that \code{coalesce()}s a fully-degenerate event's abnormal returns to
#' 0 would silently understate the true cross-sectional dispersion. The
#' exclusion is applied once per group (see \code{.exclude_all_na_events})
#' before any statistic is computed, and reported exactly once: silently
#' if the excluded event's model already emitted the one contract warning
#' guaranteed by CONTRACT-04 (an unfitted \code{ModelBase} instance), or
#' via a single additional warning naming the excluded \code{event_id}(s)
#' otherwise. Strict mode errors instead. This exclusion rule leaves the
#' pre-existing STATS-03 convention for a PARTIAL gap unchanged: an event
#' with at least one finite event-window abnormal return, but a missing
#' value on some individual day, still contributes 0 to that event's CAR
#' on the missing day(s) via \code{coalesce(abnormal_returns, 0)}.
#'
#' @seealso \code{\link{ParameterSet}}, \code{\link{MarketModel}}
NULL


#' Recommended minimum estimation-window observation count
#'
#' Advisory threshold used by OLS-based return models (MarketModel,
#' LinearFactorModel and subclasses) to emit a one-time "small estimation
#' window" warning when a model fits with fewer than this many valid
#' observations. This mirrors (but does not read from) the
#' \code{min_estimation_obs} default of \code{\link{validate_task}}
#' (currently also 30); the two are intentionally independent constants so
#' that changing one does not silently change the other's formals/behavior.
#'
#' @noRd
.MIN_ESTIMATION_OBS_RECOMMENDED <- 30L


#' Number of estimated parameters implied by an OLS formula
#'
#' For a formula of the form \code{y ~ x1 + x2 + ...} with an implicit
#' intercept, \code{length(all.vars(formula))} equals 1 (intercept) plus the
#' number of right-hand-side terms, i.e. exactly the number of coefficients
#' \code{lm()} estimates (response + predictors, since the intercept has no
#' named variable but contributes 1 for each formula term this counts is
#' already implicit in the coefficient count: response variable name is not
#' itself a coefficient, but its count offsets the +1 for the intercept).
#'
#' @noRd
.formula_n_params <- function(formula) {
  length(all.vars(formula))
}


#' Exclude all-NA events from a multi-event statistics group
#'
#' Finds \code{event_id}s with ZERO finite event-window \code{abnormal_returns}
#' and removes their rows from \code{data_tbl} and (if supplied) from the
#' nested \code{model} tibble. Events whose model object is a
#' \code{ModelBase} instance with \code{is_fitted == FALSE} were already
#' reported once by \code{fit()} (CONTRACT-04 one-warning invariant) and are
#' excluded silently here. Any other excluded event is reported exactly once
#' through \code{.handle_degenerate()}: strict mode errors, lenient mode
#' warns once naming all newly-excluded \code{event_id}s.
#'
#' @param data_tbl A tibble with columns including \code{event_id},
#'   \code{event_window}, \code{abnormal_returns}.
#' @param model A nested tibble with columns \code{event_id}, \code{model}
#'   (list-column of fitted model objects), or \code{NULL}.
#' @param mode \code{"lenient"} or \code{"strict"}.
#' @param component Name of the calling statistic/dispatcher, used in the
#'   warning/error message.
#'
#' @return A list with elements \code{data_tbl} and \code{model}, both with
#'   the all-NA events removed.
#' @noRd
.exclude_all_na_events <- function(data_tbl, model, mode, component) {
  if (!"event_id" %in% names(data_tbl) || !"event_window" %in% names(data_tbl) ||
      !"abnormal_returns" %in% names(data_tbl)) {
    return(list(data_tbl = data_tbl, model = model))
  }

  event_status <- data_tbl %>%
    dplyr::filter(event_window == 1) %>%
    dplyr::group_by(event_id) %>%
    dplyr::summarise(.has_finite = any(is.finite(abnormal_returns)), .groups = "drop")

  all_na_events <- event_status$event_id[!event_status$.has_finite]

  if (length(all_na_events) == 0) {
    return(list(data_tbl = data_tbl, model = model))
  }

  # Events already reported once by fit() (unfitted ModelBase instance) are
  # excluded silently -- CONTRACT-04 guarantees exactly one warning per event.
  already_reported <- character(0)
  if (!is.null(model) && "event_id" %in% names(model) && "model" %in% names(model)) {
    reported_tbl <- model %>%
      dplyr::filter(event_id %in% all_na_events)
    if (nrow(reported_tbl) > 0) {
      is_reported <- vapply(reported_tbl$model, function(m) {
        inherits(m, "ModelBase") && isTRUE(!m$is_fitted)
      }, logical(1))
      already_reported <- reported_tbl$event_id[is_reported]
    }
  }

  to_report <- setdiff(all_na_events, already_reported)

  if (length(to_report) > 0) {
    .handle_degenerate(
      mode        = mode,
      condition   = paste0(
        length(to_report), " event(s) with no finite event-window abnormal ",
        "returns excluded from multi-event statistics (event_id: ",
        paste(to_report, collapse = ", "), ")"
      ),
      component   = component,
      private_env = NULL
    )
  }

  data_tbl <- data_tbl %>% dplyr::filter(!event_id %in% all_na_events)
  if (!is.null(model) && "event_id" %in% names(model)) {
    model <- model %>% dplyr::filter(!event_id %in% all_na_events)
  }

  list(data_tbl = data_tbl, model = model)
}


#' @noRd
.resolve_degenerate_mode <- function(ps_value = NULL) {
  # Priority: ParameterSet field > package option > default "lenient"
  if (!is.null(ps_value)) {
    return(match.arg(ps_value, c("lenient", "strict")))
  }
  opt <- getOption("EventStudy.degenerate_handling", default = NULL)
  if (!is.null(opt)) {
    return(match.arg(opt, c("lenient", "strict")))
  }
  "lenient"
}


#' @noRd
.finite_residual_df <- function(residuals, n_params = 1L) {
  # Returns the number of finite residuals minus n_params, floored at 1.
  # Used by models whose df should reflect only finite (non-NA, non-Inf)
  # residuals rather than the total row count in the estimation window.
  max(sum(is.finite(residuals)) - as.integer(n_params), 1L)
}


#' @noRd
.handle_degenerate <- function(mode, condition, component,
                                event_id = NULL, firm_symbol = NULL,
                                private_env = NULL) {
  # Build context string from component and optional identifying keys
  ctx <- component
  if (!is.null(event_id))    ctx <- paste0(ctx, " [event_id=", event_id, "]")
  if (!is.null(firm_symbol)) ctx <- paste0(ctx, " [firm=", firm_symbol, "]")

  msg <- paste0(ctx, ": ", condition)

  if (mode == "strict") {
    stop(msg, call. = FALSE)
  } else {
    warning(msg, call. = FALSE)
    if (!is.null(private_env)) {
      private_env$.is_fitted <- FALSE
      # Mark that a contract warning has already been emitted for this model
      # instance so abnormal_returns() does not emit a second "not fitted"
      # warning (contract guarantees exactly one warning per degenerate event).
      private_env$.degenerate_handled <- TRUE
    }
    invisible(FALSE)
  }
}
