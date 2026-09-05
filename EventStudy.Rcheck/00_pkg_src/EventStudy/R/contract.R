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
#' is retained in the output tibble — it is never silently dropped.
#'
#' @seealso \code{\link{ParameterSet}}, \code{\link{MarketModel}}
NULL


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
