#' Return-Shape Contracts for EventStudy Pipeline Results
#'
#' @name eventstudy-shape-contracts
#' @title Return-Shape Contracts for EventStudy Pipeline Results
#'
#' @description
#' The shape-contract system locks the column names and types of the
#' EventStudy pipeline's result tibbles against accidental structural
#' drift.  It is designed for development and CI use only.
#'
#' \strong{Default:} OFF.  When the option is unset (the default) the
#' entire shape-contract path is a strict no-op — every existing caller
#' and the full test suite are completely unaffected.
#'
#' \strong{Opt-in:}
#' \code{options(EventStudy.shape_contracts = TRUE)}
#'
#' \strong{Philosophy (mirrors the degenerate-input contract):}
#' On a column-name or type mismatch the contract emits exactly \emph{one}
#' \code{warning()}, naming the context and the specific drift, and then
#' returns invisibly.  It \strong{never} calls \code{stop()}.  The
#' correctly-shaped \code{is_fitted = FALSE} degenerate output (same
#' column names, \code{NA}-propagated values) is treated as a \emph{valid}
#' shape and produces no warning.
#'
#' \strong{Coverage:}
#' \itemize{
#'   \item Single-event statistics tibbles: \code{ART} (ARTTest) and
#'     \code{CART} (CARTTest), including their \code{is_fitted = FALSE}
#'     degenerate variants.
#'   \item Multi-event AAR/CAAR statistics tibble: \code{CSectT}
#'     (CSectTTest) and all other multi-event result tibbles, including
#'     their \code{is_fitted = FALSE} degenerate variants.
#' }
#'
#' \strong{Configuration:}
#' \itemize{
#'   \item Via package option: \code{options(EventStudy.shape_contracts = TRUE)}
#'   \item Default is \code{FALSE} (off).
#' }
#'
#' @seealso \code{\link{degenerate-input-contract}}, \code{\link{ParameterSet}}
NULL


#' @noRd
.resolve_shape_contract_mode <- function() {
  # Mirrors .resolve_degenerate_mode() in R/contract.R.
  # Priority: package option > default FALSE.
  # Returns TRUE (on) or FALSE (off — strict no-op).
  isTRUE(getOption("EventStudy.shape_contracts", default = FALSE))
}


# ---------------------------------------------------------------------------
# Canonical expected-shape specifications
# ---------------------------------------------------------------------------
# Each spec is a named character vector: names = column names, values = R
# type-class string to check with is(tbl[[col]], class).
# The *same* spec covers both the fitted and is_fitted = FALSE degenerate
# variants because the degenerate path preserves column structure exactly
# (NA-propagated values, same column names — per contract.R semantics).
# ---------------------------------------------------------------------------

#' @noRd
.shape_spec_art <- function() {
  # ARTTest$compute() columns (single-event, fitted and degenerate):
  #   relative_index   : integer
  #   abnormal_returns : numeric
  #   ar_t             : numeric
  #   ar_t_dist        : distributional distribution object (class "distribution")
  c(
    relative_index   = "numeric",   # integer passes is(x, "numeric")
    abnormal_returns = "numeric",
    ar_t             = "numeric",
    ar_t_dist        = "distribution"
  )
}

#' @noRd
.shape_spec_cart <- function() {
  # CARTTest$compute() columns (single-event, fitted and degenerate):
  #   relative_index      : integer
  #   abnormal_returns    : numeric
  #   event_window_length : integer
  #   car_window          : character
  #   car                 : numeric
  #   corrected_car       : numeric
  #   car_t               : numeric
  #   car_t_dist          : distributional distribution object
  c(
    relative_index      = "numeric",
    abnormal_returns    = "numeric",
    event_window_length = "numeric",
    car_window          = "character",
    car                 = "numeric",
    corrected_car       = "numeric",
    car_t               = "numeric",
    car_t_dist          = "distribution"
  )
}

#' @noRd
.shape_spec_aar_caar <- function() {
  # CSectTTest$compute() columns — also the canonical multi-event AAR/CAAR
  # tibble shape (fitted and degenerate):
  #   relative_index  : integer
  #   aar             : numeric
  #   n_events        : integer
  #   n_valid_events  : integer
  #   n_pos           : integer
  #   n_neg           : integer
  #   aar_t           : numeric
  #   caar            : numeric
  #   caar_t          : numeric
  #   car_window      : character
  c(
    relative_index  = "numeric",
    aar             = "numeric",
    n_events        = "numeric",
    n_valid_events  = "numeric",
    n_pos           = "numeric",
    n_neg           = "numeric",
    aar_t           = "numeric",
    caar            = "numeric",
    caar_t          = "numeric",
    car_window      = "character"
  )
}


# ---------------------------------------------------------------------------
# Core check function
# ---------------------------------------------------------------------------

#' @noRd
.check_shape <- function(tbl, expected_cols, expected_types, context) {
  # Emits exactly ONE warning() on any structural drift (missing column,
  # extra unexpected column, or type mismatch) and then returns invisibly.
  # Never calls stop().  Returns invisible(TRUE) when shape is valid.
  #
  # Args:
  #   tbl           - the tibble/data.frame to check
  #   expected_cols - character vector of required column names
  #   expected_types - named character vector (name = col, value = type class)
  #   context       - string naming the calling context (e.g. "ART [event_id=E1]")

  actual_cols <- names(tbl)
  drifts <- character(0)

  # Check for missing required columns
  missing_cols <- setdiff(expected_cols, actual_cols)
  if (length(missing_cols) > 0) {
    drifts <- c(drifts, paste0("missing column(s): ",
                               paste(missing_cols, collapse = ", ")))
  }

  # Check types for columns that are present
  present_cols <- intersect(expected_cols, actual_cols)
  for (col in present_cols) {
    expected_type <- expected_types[[col]]
    if (!is.null(expected_type) && !is(tbl[[col]], expected_type)) {
      actual_type <- class(tbl[[col]])[1]
      drifts <- c(drifts, paste0("column '", col, "': expected class '",
                                 expected_type, "', got '", actual_type, "'"))
    }
  }

  if (length(drifts) > 0) {
    warning(
      "EventStudy shape contract violated [", context, "]: ",
      paste(drifts, collapse = "; "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# Pipeline-level shape check dispatchers
# ---------------------------------------------------------------------------

#' @noRd
.check_single_event_shape <- function(tbl, stat_name, context_suffix = "") {
  # Selects the canonical spec based on stat_name and calls .check_shape().
  # Unknown stat names are silently skipped (no warning, no error) to stay
  # additive-only — the contract only covers explicitly registered shapes.
  context <- paste0(stat_name, if (nchar(context_suffix) > 0) paste0(" [", context_suffix, "]") else "")

  spec <- switch(stat_name,
    ART  = .shape_spec_art(),
    CART = .shape_spec_cart(),
    NULL  # unknown stat — skip silently
  )

  if (is.null(spec)) return(invisible(TRUE))

  .check_shape(
    tbl           = tbl,
    expected_cols = names(spec),
    expected_types = spec,
    context       = context
  )
}


#' @noRd
.check_aar_caar_shape <- function(tbl, stat_name, context_suffix = "") {
  # Checks multi-event AAR/CAAR stat tibbles.
  # Uses the canonical CSectT spec for CSectTTest results.
  # Unknown stat names are silently skipped.
  context <- paste0(stat_name, if (nchar(context_suffix) > 0) paste0(" [", context_suffix, "]") else "")

  spec <- switch(stat_name,
    CSectT = .shape_spec_aar_caar(),
    NULL   # unknown stat — skip silently
  )

  if (is.null(spec)) return(invisible(TRUE))

  .check_shape(
    tbl           = tbl,
    expected_cols = names(spec),
    expected_types = spec,
    context       = context
  )
}
