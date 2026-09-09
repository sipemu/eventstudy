# ---------------------------------------------------------------------------
# .inform() -- gated informational messaging (API-05)
#
# Routes informational chatter through a single quiet-mode gate. When
# `verbose` is truthy the message is emitted verbatim via message(); when
# FALSE the message is suppressed. Only informational message() sites route
# through this helper -- warnings, errors, and the exactly-one degenerate
# warning (contract.R) are NEVER gated.
#
# Default is getOption("eventstudy.verbose", TRUE) so that default console
# output is byte-identical to prior behaviour: with `verbose` omitted every
# gated site still emits exactly as it did before.
#
# @param msg A single already-assembled message string. Callers that used
#   `message(a, b, c)` should pass `paste0(a, b, c)`.
# @param verbose Logical; if FALSE, suppress the message. Defaults to
#   getOption("eventstudy.verbose", TRUE).
# @return invisible(NULL)
# @noRd
# ---------------------------------------------------------------------------
.inform <- function(msg, verbose = getOption("eventstudy.verbose", TRUE)) {
  if (isTRUE(verbose)) {
    message(msg)
  }
  invisible(NULL)
}
