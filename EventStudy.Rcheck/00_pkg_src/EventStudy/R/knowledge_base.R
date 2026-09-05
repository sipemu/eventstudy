#' Grounding Knowledge Base for Event Study Diagnostics
#'
#' \code{EVENTSTUDY_KB} is a pure-R list of rule records that maps diagnostic
#' conditions (read from an \code{\link{es_diagnostics}} object) to grounded
#' methodological recommendations, each carrying a structured academic citation
#' and a severity level.
#'
#' The KB is built at package-load time from calls to \code{.kb_rule()} and
#' never depends on a network connection or LLM provider. It is the
#' correctness-critical layer that the offline advice functions
#' (\code{\link{recommend_stat}}, \code{\link{flag_robustness}}) and the
#' Phase 7 LLM layer both evaluate against.
#'
#' \strong{KB-04 scope note:} Phase 5 delivers the KB data structure
#' only — exported and serializable, ready for Phase 7 system-prompt
#' injection. The actual prompt-injection behavior is a Phase 7
#' deliverable and is deliberately out of scope here.
#'
#' @section Rule record fields:
#' Each element of \code{EVENTSTUDY_KB} is a list with:
#' \describe{
#'   \item{\code{id}}{Unique character identifier (e.g. \code{"KB-NORM-PATELL"}).}
#'   \item{\code{category}}{Either \code{"stat_choice"} (steers which test to
#'     use) or \code{"robustness"} (data-quality / reliability warning).}
#'   \item{\code{condition}}{A \code{function(diag)} that accepts an
#'     \code{es_diagnostics} list and returns a length-1 logical. Returns
#'     \code{FALSE} when any required field is \code{NA} (NA-safe; never
#'     errors on missing data).}
#'   \item{\code{recommendation}}{Character. The methodological action to take.}
#'   \item{\code{citation}}{Named list: \code{author} (character),
#'     \code{year} (integer), \code{key} (character), \code{venue} (character).}
#'   \item{\code{severity}}{One of \code{"info"}, \code{"warning"},
#'     \code{"error"}.}
#' }
#'
#' @section Threshold notes:
#' The following thresholds are literature-informed defaults and are marked
#' \strong{[ASSUMED]} — they can be tuned without breaking the KB contract:
#' \itemize{
#'   \item Shapiro-Wilk p-value threshold: 0.05
#'   \item Proportion for normality-holds rule: \>= 70\% of events
#'   \item Proportion for non-normality rule: \>= 50\% of events
#'   \item Durbin-Watson bounds: [1.5, 2.5]
#'   \item R-squared low-fit threshold: < 0.05
#'   \item Small-N threshold: < 10 valid events
#'   \item Degenerate-event threshold: < 80\% of events fitted
#'   \item High-dispersion CAR IQR threshold: > 0.10 or SD > 0.15
#' }
#'
#' @name EVENTSTUDY_KB
#' @keywords internal
NULL


# ---- Internal rule validator (.kb_rule) --------------------------------------

#' Construct and validate a single KB rule record
#'
#' Validates that all required fields are present and correctly typed,
#' then returns the rule as a plain named list. Stops with a clear message
#' if any field is malformed.
#'
#' @param id Character. Unique rule identifier.
#' @param category Character. One of \code{"stat_choice"} or
#'   \code{"robustness"}.
#' @param condition Function of one argument (an \code{es_diagnostics} list)
#'   returning a length-1 logical.
#' @param recommendation Character. Methodological recommendation text.
#' @param citation Named list with fields \code{author} (character),
#'   \code{year} (numeric or integer), \code{key} (character), and
#'   optionally \code{venue} (character).
#' @param severity Character. One of \code{"info"}, \code{"warning"},
#'   \code{"error"}.
#'
#' @return A validated named list suitable for inclusion in
#'   \code{EVENTSTUDY_KB}.
#' @noRd
.kb_rule <- function(id, category, condition, recommendation, citation, severity) {
  # --- id ---
  if (!is.character(id) || length(id) != 1L || nchar(id) == 0L) {
    stop(".kb_rule: 'id' must be a non-empty length-1 character string.", call. = FALSE)
  }
  # --- category ---
  valid_categories <- c("stat_choice", "robustness")
  if (!is.character(category) || length(category) != 1L ||
      !category %in% valid_categories) {
    stop(".kb_rule (", id, "): 'category' must be one of ",
         paste(valid_categories, collapse = "/"), ".", call. = FALSE)
  }
  # --- condition ---
  if (!is.function(condition)) {
    stop(".kb_rule (", id, "): 'condition' must be a function.", call. = FALSE)
  }
  if (length(formals(condition)) < 1L) {
    stop(".kb_rule (", id, "): 'condition' must accept at least one argument (diag).",
         call. = FALSE)
  }
  # --- recommendation ---
  if (!is.character(recommendation) || length(recommendation) != 1L ||
      nchar(recommendation) == 0L) {
    stop(".kb_rule (", id, "): 'recommendation' must be a non-empty character string.",
         call. = FALSE)
  }
  # --- citation ---
  required_citation_fields <- c("author", "year", "key")
  for (cf in required_citation_fields) {
    if (!cf %in% names(citation)) {
      stop(".kb_rule (", id, "): citation must have field '", cf, "'.", call. = FALSE)
    }
  }
  if (!is.character(citation$author) || nchar(citation$author) == 0L) {
    stop(".kb_rule (", id, "): citation$author must be a non-empty character string.",
         call. = FALSE)
  }
  if (!is.numeric(citation$year) && !is.integer(citation$year)) {
    stop(".kb_rule (", id, "): citation$year must be numeric.", call. = FALSE)
  }
  if (!is.character(citation$key) || nchar(citation$key) == 0L) {
    stop(".kb_rule (", id, "): citation$key must be a non-empty character string.",
         call. = FALSE)
  }
  # --- severity ---
  valid_severities <- c("info", "warning", "error")
  if (!is.character(severity) || length(severity) != 1L ||
      !severity %in% valid_severities) {
    stop(".kb_rule (", id, "): 'severity' must be one of ",
         paste(valid_severities, collapse = "/"), ".", call. = FALSE)
  }

  list(
    id             = id,
    category       = category,
    condition      = condition,
    recommendation = recommendation,
    citation       = citation,
    severity       = severity
  )
}


# ---- EVENTSTUDY_KB: package-level list of rule records -----------------------

#' @title Grounding Knowledge Base
#' @description Package-level list; use \code{\link{es_kb}()} to access.
#' @keywords internal
EVENTSTUDY_KB <- list(

  # 1. KB-NORM-PATELL -----------------------------------------------------------
  # Condition: estimation-window residuals appear approximately normal
  # (Shapiro-Wilk p > 0.05 in >= 70% of shown events).
  # Steers toward: Patell Z (valid under normality assumption).
  # [ASSUMED] proportion threshold 0.70; direction [CITED: MacKinlay 1997 §4]
  .kb_rule(
    id       = "KB-NORM-PATELL",
    category = "stat_choice",
    condition = function(diag) {
      sw <- diag$estimation_window$shapiro_p
      if (all(is.na(sw))) return(FALSE)
      isTRUE(mean(sw > 0.05, na.rm = TRUE) >= 0.70)
    },
    recommendation = paste0(
      "Estimation-window residuals appear approximately normal (Shapiro-Wilk p > 0.05 ",
      "in >= 70% of events). Patell Z is appropriate under the normality assumption. ",
      "Note: estimation-window residual normality is a proxy for\u2014not equivalent to\u2014the ",
      "cross-sectional normality of standardized abnormal returns that Patell Z formally ",
      "assumes. ",
      "[Threshold: 70% of events, p = 0.05 \u2014 ASSUMED, adjustable]"
    ),
    citation = list(
      author = "Patell, J.M.",
      year   = 1976L,
      key    = "Patell1976",
      venue  = "Journal of Accounting Research, 14(2), 246-276"
    ),
    severity = "info"
  ),

  # 2. KB-NONNORM-NONPAR --------------------------------------------------------
  # Condition: normality rejected in >= 50% of events (Shapiro-Wilk p < 0.05).
  # Steers toward: Sign Test or Rank Test (Corrado 1989) as non-parametric alt.
  # [ASSUMED] proportion threshold 0.50; direction Brown & Warner (1985) §3
  .kb_rule(
    id       = "KB-NONNORM-NONPAR",
    category = "stat_choice",
    condition = function(diag) {
      sw <- diag$estimation_window$shapiro_p
      if (all(is.na(sw))) return(FALSE)
      isTRUE(mean(sw < 0.05, na.rm = TRUE) >= 0.50)
    },
    recommendation = paste0(
      "Non-normality detected in estimation-window residuals for >= 50% of events ",
      "(Shapiro-Wilk p < 0.05). Consider non-parametric alternatives: ",
      "Sign Test or Rank Test (Corrado 1989) are robust to departures from normality. ",
      "Brown & Warner (1985) document that parametric tests lose size control under ",
      "non-normal return distributions. [Threshold: 50% of events \u2014 ASSUMED, adjustable]"
    ),
    citation = list(
      author = "Brown, S.J. and Warner, J.B.",
      year   = 1985L,
      key    = "BrownWarner1985",
      venue  = "Journal of Financial Economics, 14(1), 3-31"
    ),
    severity = "warning"
  ),

  # 3. KB-VAR-INCREASE-BMP ------------------------------------------------------
  # Condition: high CAR dispersion suggests event-induced variance increase.
  # CAR IQR > 0.10 or CAR SD > 0.15 (heuristic thresholds, [ASSUMED]).
  # Steers toward: BMP (Boehmer, Musumeci & Poulsen 1991) over Patell Z.
  # BMP standardizes by event-window variance, making it robust to variance shifts.
  .kb_rule(
    id       = "KB-VAR-INCREASE-BMP",
    category = "stat_choice",
    condition = function(diag) {
      iqr <- diag$cross_sectional$car_iqr
      sdd <- diag$cross_sectional$car_sd
      if (is.na(iqr) && is.na(sdd)) return(FALSE)
      isTRUE(
        (!is.na(iqr) && iqr > 0.10) ||
        (!is.na(sdd) && sdd > 0.15)
      )
    },
    recommendation = paste0(
      "High CAR dispersion (IQR > 0.10 or SD > 0.15) suggests event-induced ",
      "variance increase, which inflates Patell Z rejection rates. Use the ",
      "BMP (Boehmer-Musumeci-Poulsen) test, which standardizes by the ",
      "event-window variance and is specifically designed for this case. ",
      "[Thresholds: IQR > 0.10, SD > 0.15 \u2014 ASSUMED, adjustable]"
    ),
    citation = list(
      author = "Boehmer, E., Musumeci, J. and Poulsen, A.B.",
      year   = 1991L,
      key    = "BMP1991",
      venue  = "Journal of Financial Economics, 30(2), 253-272"
    ),
    severity = "warning"
  ),

  # 4. KB-OVERLAP-KP ------------------------------------------------------------
  # Condition: n_overlap_pairs > 0 (calendar-time event-window overlap detected).
  # Steers toward: Kolari-Pynnonen adjusted BMP (KP test).
  # KP corrects for cross-sectional correlation of abnormal returns arising from
  # overlapping event windows (clustered events).
  # [CITED: Kolari & Pynnonen 2010; referenced in package KolariPynnonenTest roxygen]
  .kb_rule(
    id       = "KB-OVERLAP-KP",
    category = "stat_choice",
    condition = function(diag) {
      n_op <- diag$cross_sectional$n_overlap_pairs
      if (is.na(n_op)) return(FALSE)
      isTRUE(n_op > 0L)
    },
    recommendation = paste0(
      "Event windows overlap in calendar time (cross-sectional correlation of ",
      "abnormal returns is likely). Standard BMP and Patell Z assume independence ",
      "across events \u2014 this assumption is violated when windows cluster. ",
      "Use the Kolari-Pynnonen (KP) adjusted BMP test, which corrects for ",
      "cross-sectional correlation arising from overlapping event windows."
    ),
    citation = list(
      author = "Kolari, J.W. and Pynnonen, S.",
      year   = 2010L,
      key    = "KolariPynnonen2010",
      venue  = "Review of Financial Studies, 23(11), 3996-4025"
    ),
    severity = "warning"
  ),

  # 5. KB-AC-WARN ---------------------------------------------------------------
  # Condition: Durbin-Watson statistic outside [1.5, 2.5] in the majority of events
  # (< 1.5 indicates positive autocorrelation; > 2.5 indicates negative).
  # Recommendation: Use HAC standard errors or note the caveat.
  # [ASSUMED] DW bounds 1.5/2.5; direction Brown & Warner (1985) §2
  .kb_rule(
    id       = "KB-AC-WARN",
    category = "robustness",
    condition = function(diag) {
      dw <- diag$estimation_window$dw_stat
      if (all(is.na(dw))) return(FALSE)
      # Majority: >= 50% of non-NA values outside [1.5, 2.5]
      n_out <- sum(!is.na(dw) & (dw < 1.5 | dw > 2.5))
      n_nonNA <- sum(!is.na(dw))
      isTRUE(n_nonNA > 0L && (n_out / n_nonNA) >= 0.50)
    },
    recommendation = paste0(
      "Serial autocorrelation detected in estimation-window residuals ",
      "(Durbin-Watson statistic outside [1.5, 2.5] in >= 50% of events). ",
      "OLS standard errors are understated under autocorrelation. Consider ",
      "using HAC-robust standard errors (MarketModel use_hac = TRUE) or ",
      "interpreting significance levels conservatively. ",
      "Brown & Warner (1985) document that autocorrelation inflates test size. ",
      "[DW bounds: [1.5, 2.5] \u2014 ASSUMED heuristic, adjustable]"
    ),
    citation = list(
      author = "Brown, S.J. and Warner, J.B.",
      year   = 1985L,
      key    = "BrownWarner1985",
      venue  = "Journal of Financial Economics, 14(1), 3-31"
    ),
    severity = "warning"
  ),

  # 6. KB-LOWFIT-WARN -----------------------------------------------------------
  # Condition: R-squared < 0.05 in the majority of events (>= 50%).
  # Low R² inflates the standard error of abnormal returns (MacKinlay 1997 §3.1).
  # [ASSUMED] threshold R² < 0.05; direction [CITED: MacKinlay 1997]
  .kb_rule(
    id       = "KB-LOWFIT-WARN",
    category = "robustness",
    condition = function(diag) {
      r2 <- diag$estimation_window$r2
      if (all(is.na(r2))) return(FALSE)
      n_low   <- sum(!is.na(r2) & r2 < 0.05)
      n_nonNA <- sum(!is.na(r2))
      isTRUE(n_nonNA > 0L && (n_low / n_nonNA) >= 0.50)
    },
    recommendation = paste0(
      "Market model fit is poor: R-squared < 0.05 in >= 50% of events. ",
      "Low R\u00b2 inflates the standard error of abnormal returns, reducing ",
      "test power. Consider a multi-factor model (Fama-French 3/5, Carhart 4) ",
      "or a Market-Adjusted (index-return) model to improve fit. ",
      "MacKinlay (1997) \u00a73.1 documents this relationship. ",
      "[Threshold: R\u00b2 < 0.05 \u2014 ASSUMED, adjustable]"
    ),
    citation = list(
      author = "MacKinlay, A.C.",
      year   = 1997L,
      key    = "MacKinlay1997",
      venue  = "Journal of Economic Literature, 35(1), 13-39"
    ),
    severity = "warning"
  ),

  # 7. KB-DEGEN-EVENTS ----------------------------------------------------------
  # Condition: n_valid_events / n_events_total < 0.8 (>20% of events not fitted).
  # Degenerate events produce NA abnormal returns and inflate uncertainty.
  # [VERIFIED: R/contract.R degenerate-input contract documentation]
  .kb_rule(
    id       = "KB-DEGEN-EVENTS",
    category = "robustness",
    condition = function(diag) {
      n_total  <- diag$meta$n_events_total
      n_fitted <- diag$cross_sectional$n_valid_events
      if (is.na(n_total) || is.na(n_fitted) || n_total == 0L) return(FALSE)
      isTRUE((n_fitted / n_total) < 0.8)
    },
    recommendation = paste0(
      "More than 20% of events have degenerate estimation windows ",
      "(insufficient data or zero variance in index returns). ",
      "These events contribute NA abnormal returns, potentially biasing ",
      "cross-sectional test statistics. Inspect contract_state for details; ",
      "consider extending the estimation window or filtering events with ",
      "insufficient data. MacKinlay (1997) \u00a73 requires adequate estimation window length. ",
      "[Threshold: < 80% fitted \u2014 ASSUMED, adjustable]"
    ),
    citation = list(
      author = "MacKinlay, A.C.",
      year   = 1997L,
      key    = "MacKinlay1997",
      venue  = "Journal of Economic Literature, 35(1), 13-39"
    ),
    severity = "error"
  ),

  # 8. KB-SMALL-N ---------------------------------------------------------------
  # Condition: n_valid_events < 10.
  # Cross-sectional t-test loses power with very few events; the Central Limit
  # Theorem approximation underlying CSectT is weak below N ~ 10.
  # Non-parametric tests are preferred in small samples.
  # [ASSUMED] threshold N < 10; direction Brown & Warner (1985)
  .kb_rule(
    id       = "KB-SMALL-N",
    category = "robustness",
    condition = function(diag) {
      n_valid <- diag$cross_sectional$n_valid_events
      if (is.na(n_valid)) return(FALSE)
      isTRUE(n_valid < 10L)
    },
    recommendation = paste0(
      "Very few valid events (n < 10). The cross-sectional t-test relies on ",
      "a large-sample normal approximation that is unreliable with fewer than ~10 events. ",
      "Non-parametric tests (Sign Test, Rank Test) are preferred in small samples, ",
      "as documented by Brown & Warner (1985). Interpret parametric p-values cautiously. ",
      "[Threshold: n < 10 \u2014 ASSUMED, adjustable]"
    ),
    citation = list(
      author = "Brown, S.J. and Warner, J.B.",
      year   = 1985L,
      key    = "BrownWarner1985",
      venue  = "Journal of Financial Economics, 14(1), 3-31"
    ),
    severity = "warning"
  )
)


# ---- Exported accessor -------------------------------------------------------

#' Access the EventStudy Grounding Knowledge Base
#'
#' Returns \code{EVENTSTUDY_KB}, the package-level pure-R list of rule records
#' that maps diagnostic conditions to grounded methodological recommendations.
#' Each rule carries a structured academic citation and can be evaluated against
#' an \code{\link{es_diagnostics}} object.
#'
#' The KB is the correctness-critical layer consumed by
#' \code{\link{recommend_stat}} and \code{\link{flag_robustness}}, and
#' exported here so that Phase 7 can inject its contents into an LLM
#' system prompt without accessing an internal package object.
#'
#' \strong{KB-04 note:} This function delivers the \emph{structure} only —
#' exported and serializable, ready for Phase 7 system-prompt injection.
#' The actual prompt-injection behavior is a Phase 7 deliverable and is
#' deliberately out of scope here.
#'
#' @return A named list of rule records, each with fields:
#'   \code{id}, \code{category}, \code{condition}, \code{recommendation},
#'   \code{citation} (list of \code{author}/\code{year}/\code{key}/\code{venue}),
#'   and \code{severity}.
#'
#' @note Each rule's \code{condition} field is an R function closure and is
#'   therefore \strong{not} JSON-serializable. Any consumer that serializes
#'   the KB (e.g. Phase 7 system-prompt injection) must drop the
#'   \code{condition} field from each rule record before encoding to JSON.
#'
#' @seealso \code{\link{recommend_stat}}, \code{\link{flag_robustness}},
#'   \code{\link{es_diagnostics}}
#'
#' @examples
#' kb <- es_kb()
#' length(kb)             # number of rules
#' kb[[1]]$id             # id of first rule
#' kb[[1]]$citation       # citation list
#'
#' @export
es_kb <- function() {
  EVENTSTUDY_KB
}
