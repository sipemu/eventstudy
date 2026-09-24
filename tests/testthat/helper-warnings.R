# C10 (2026-09-24): helper to muffle ONLY the advisory
# eventstudy_short_estimation_window warning (commit c4d0ea9 made the A6
# advisory a classed condition) for tests whose fixtures are INTENTIONALLY
# short (golden/invariant/numerical-stability fixtures built to keep the
# examples small and readable, not to exercise the short-window advisory
# itself). Never use suppressWarnings() here -- that would also hide a
# genuine, unexpected warning the fixture should have surfaced.
muffle_short_window <- function(expr) {
  withCallingHandlers(
    expr,
    eventstudy_short_estimation_window = function(w) invokeRestart("muffleWarning")
  )
}
