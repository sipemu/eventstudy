#' @name eventstudy-deprecation
#' @title EventStudy Deprecation Policy
#'
#' @description
#' \strong{Policy: warn, never silently break.}
#'
#' Deprecated argument names and function names in EventStudy continue to work
#' and emit exactly one deprecation warning per call, pointing the user to the
#' replacement. Deprecated surfaces are never removed in a patch or minor
#' release; removal (the "defunct" stage) is reserved for a future major version
#' and will be announced in NEWS.md.
#'
#' \strong{Mechanism:}
#' Every renamed public argument has an explicit old-name shim that calls
#' \code{.deprecate_arg()} and then forwards the supplied value under the new
#' name. The shim fires before any computation, so the old name is
#' fully-equivalent to the new name for all valid inputs.
#'
#' \strong{Warning source:}
#' Deprecation warnings always use base \code{\link[base]{.Deprecated}()} so
#' they work without any additional package dependency. When the optional
#' \pkg{lifecycle} package is installed (Suggests), the warning is also routed
#' through \code{lifecycle::deprecate_warn()} for richer IDE integration;
#' \pkg{lifecycle} is never a hard \code{Imports} dependency.
#'
#' \strong{NEWS discipline:}
#' Every deprecation and rename is documented in \code{NEWS.md} under the
#' release heading in which it was introduced.
#'
#' @seealso \code{\link{run_event_study}}, \code{\link{plot_stocks}}
NULL


#' @noRd
#'
#' Emit a deprecation warning for a renamed function argument, optionally
#' routing through \pkg{lifecycle} when available, and return the supplied
#' value so call sites can forward it in one expression.
#'
#' @param old   Character scalar — the old argument name (as the user typed it).
#' @param new   Character scalar — the replacement argument name.
#' @param value The value supplied by the user under the old name. Returned
#'   invisibly so callers can write \code{new_arg <- .deprecate_arg(...)}.
#' @param fn    Character scalar — name of the function where the rename lives,
#'   used to produce a precise warning message (e.g. \code{"plot_stocks"}).
#' @param when  Character scalar — version string when the argument was first
#'   deprecated (e.g. \code{"0.66.0"}).
#'
#' @return \code{value}, invisibly.
.deprecate_arg <- function(old, new, value, fn = NULL, when = "0.66.0") {
  msg <- paste0(
    if (!is.null(fn)) paste0("In `", fn, "()`: ") else "",
    "argument `", old, "` is deprecated as of EventStudy ", when, "; ",
    "please use `", new, "` instead."
  )

  # Base .Deprecated() is always available — no external dependency required.
  # It routes through warning() so existing withCallingHandlers() / tryCatch()
  # patterns continue to work.
  .Deprecated(msg = msg)

  # When lifecycle is installed (Suggests-only, never hard Imports), also
  # emit via lifecycle::deprecate_warn() for richer IDE and CLI integration.
  # The requireNamespace() guard ensures absence of lifecycle is a no-op.
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      when  = when,
      what  = paste0(if (!is.null(fn)) paste0(fn, "(") else "", old, if (!is.null(fn)) ")" else ""),
      with  = paste0(if (!is.null(fn)) paste0(fn, "(") else "", new, if (!is.null(fn)) ")" else "")
    )
  }

  invisible(value)
}
