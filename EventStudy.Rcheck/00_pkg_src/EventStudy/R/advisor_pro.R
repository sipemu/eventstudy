# =============================================================================
# advisor_pro.R — Advisor Pro waitlist doc topic + opt-in footer helper
# =============================================================================

#' Advisor Pro — Future Retrieval-Grounded Paid Tier
#'
#' \strong{Advisor Pro} is a planned premium add-on for the EventStudy package
#' that will provide retrieval-augmented, evidence-grounded AI advice backed by
#' a curated academic knowledge base.  It is not yet available; this help topic
#' documents the waitlist mechanism.
#'
#' @section What Advisor Pro will include:
#' \itemize{
#'   \item Retrieval-grounded recommendations sourced from peer-reviewed
#'     methodology (MacKinlay 1997, Fama 1991, Boehmer et al. 1991, and more)
#'   \item Automatic context assembly from \code{\link{es_diagnostics}} output
#'   \item Structured \code{Advice} objects with traceable evidence chains
#'   \item Priority access to new KB rules and model integrations
#' }
#'
#' @section Current offline tier:
#' The current package already ships an offline advice layer based on a
#' deterministic knowledge base:
#' \itemize{
#'   \item \code{\link{recommend_stat}()} — KB-based test-statistic recommendation
#'   \item \code{\link{flag_robustness}()} — KB-based robustness flags
#'   \item \code{\link{es_advise}()} — LLM-backed advice (requires provider)
#' }
#'
#' @section Waitlist:
#' To join the Advisor Pro waitlist, visit:
#' \url{https://github.com/sipemu/eventstudy#advisor-pro-waitlist}
#'
#' To enable an optional footer reminder after printing advice objects:
#' \preformatted{options(eventstudy.advisor_pro_footer = TRUE)}
#' The footer is silent by default; enabling it appends a static URL — no
#' network connection is made.
#'
#' @name advisor_pro
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# Internal: .advisor_pro_footer — opt-in, static, zero-network footer
# ---------------------------------------------------------------------------

# BIZ-02 / CRAN no-phone-home: this helper is purely presentational.
# It cat()s a constant string and opens no connections or sockets.
# It is called only from print.Advice and print.es_advice immediately
# before their invisible(x) returns.  Default option value is FALSE
# (silent); users opt in explicitly with options(eventstudy.advisor_pro_footer = TRUE).

#' @noRd
.advisor_pro_footer <- function(con = stdout()) {
  if (!isTRUE(getOption("eventstudy.advisor_pro_footer", default = FALSE))) {
    return(invisible(NULL))
  }
  cat(
    "\n-- Advisor Pro (Waitlist) --\n",
    "Retrieval-grounded premium advice: https://github.com/sipemu/eventstudy#advisor-pro-waitlist\n",
    sep = "",
    file = con
  )
  invisible(NULL)
}
