#' Recommend Test Statistics via Offline KB Matching
#'
#' Evaluates the KB decision table (category \code{"stat_choice"} rules) against
#' the diagnostic signals extracted from a fitted \code{EventStudyTask} or a
#' precomputed \code{es_diagnostics} object, and returns a severity-ranked
#' \code{es_advice} S3 object.
#'
#' No LLM provider, network connection, or API key is required. Both functions
#' are the always-available offline grounding layer (ADV-08). The returned
#' \code{es_advice} object has the same shape as the Phase 7 Advice contract,
#' flagged \code{is_deterministic = TRUE} and \code{source = "offline_kb"}.
#'
#' @param x A fitted \code{EventStudyTask} (after \code{fit_model()}) or a
#'   precomputed \code{es_diagnostics} object returned by \code{es_diagnostics()}.
#' @param provider Accepted but ignored in the offline path — present only so
#'   the Phase 7 call shape is forward-compatible. Default \code{NULL}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An S3 object of class \code{"es_advice"} — a named list with:
#' \describe{
#'   \item{\code{source}}{\code{"offline_kb"} (character).}
#'   \item{\code{is_deterministic}}{\code{TRUE} — advice is rule-based, not LLM-generated.}
#'   \item{\code{rules_matched}}{Named list of matched rule records (severity-ranked:
#'     \code{"error"} first, then \code{"warning"}, then \code{"info"}), each with
#'     fields \code{id}, \code{recommendation}, \code{citation} (list of
#'     \code{author}/\code{year}/\code{key}/\code{venue}), \code{severity},
#'     \code{category}.}
#'   \item{\code{diagnostics_ref}}{The \code{es_diagnostics} list that was evaluated
#'     (possibly computed on-the-fly from the task).}
#' }
#'
#' @seealso \code{\link{flag_robustness}}, \code{\link{es_diagnostics}},
#'   \code{\link{es_kb}}
#'
#' @examples
#' \dontrun{
#' task <- run_event_study(my_task, ParameterSet$new())
#' advice <- recommend_stat(task)
#' print(advice)
#' }
#'
#' @export
recommend_stat <- function(x, provider = NULL, ...) {
  UseMethod("recommend_stat")
}


#' @rdname recommend_stat
#' @export
recommend_stat.default <- function(x, provider = NULL, ...) {
  stop("recommend_stat() requires an EventStudyTask or es_diagnostics object.",
       call. = FALSE)
}


#' @rdname recommend_stat
#' @export
recommend_stat.EventStudyTask <- function(x, provider = NULL, ...) {
  diag <- es_diagnostics(x)
  recommend_stat.es_diagnostics(diag, provider = provider, ...)
}


#' @rdname recommend_stat
#' @export
recommend_stat.es_diagnostics <- function(x, provider = NULL, ...) {
  rules <- Filter(function(r) r$category == "stat_choice", es_kb())
  .build_offline_advice(x, rules)
}


#' Flag Robustness Issues via Offline KB Matching
#'
#' Evaluates the KB decision table (category \code{"robustness"} rules) against
#' the diagnostic signals extracted from a fitted \code{EventStudyTask} or a
#' precomputed \code{es_diagnostics} object, and returns a severity-ranked
#' \code{es_advice} S3 object.
#'
#' No LLM provider, network connection, or API key is required. The returned
#' \code{es_advice} object has the same shape as the Phase 7 Advice contract,
#' flagged \code{is_deterministic = TRUE} and \code{source = "offline_kb"}.
#'
#' @param x A fitted \code{EventStudyTask} (after \code{fit_model()}) or a
#'   precomputed \code{es_diagnostics} object returned by \code{es_diagnostics()}.
#' @param provider Accepted but ignored in the offline path — present only so
#'   the Phase 7 call shape is forward-compatible. Default \code{NULL}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An S3 object of class \code{"es_advice"} — see \code{\link{recommend_stat}}
#'   for field descriptions. Rules are filtered to \code{category == "robustness"}.
#'
#' @seealso \code{\link{recommend_stat}}, \code{\link{es_diagnostics}},
#'   \code{\link{es_kb}}
#'
#' @examples
#' \dontrun{
#' task <- run_event_study(my_task, ParameterSet$new())
#' advice <- flag_robustness(task)
#' print(advice)
#' }
#'
#' @export
flag_robustness <- function(x, provider = NULL, ...) {
  UseMethod("flag_robustness")
}


#' @rdname flag_robustness
#' @export
flag_robustness.default <- function(x, provider = NULL, ...) {
  stop("flag_robustness() requires an EventStudyTask or es_diagnostics object.",
       call. = FALSE)
}


#' @rdname flag_robustness
#' @export
flag_robustness.EventStudyTask <- function(x, provider = NULL, ...) {
  diag <- es_diagnostics(x)
  flag_robustness.es_diagnostics(diag, provider = provider, ...)
}


#' @rdname flag_robustness
#' @export
flag_robustness.es_diagnostics <- function(x, provider = NULL, ...) {
  rules <- Filter(function(r) r$category == "robustness", es_kb())
  .build_offline_advice(x, rules)
}


#' Print method for es_advice objects
#'
#' Prints a structured summary of the offline advice, listing each matched rule
#' with its severity, citation key, and recommendation. Follows the package
#' convention of \code{print.es_diagnostics} and \code{print.es_simulation}
#' (cat-based, invisible return).
#'
#' @param x An object of class \code{"es_advice"}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.es_advice <- function(x, ...) {
  cat("Offline Event Study Advice\n")
  cat("==========================\n")
  cat("Source:          ", x$source, "\n")
  cat("Deterministic:   ", x$is_deterministic, "\n")
  cat("Rules matched:   ", length(x$rules_matched), "\n")

  if (length(x$rules_matched) == 0L) {
    cat("\n(No rules fired on these diagnostics.)\n")
  } else {
    cat("\n")
    for (i in seq_along(x$rules_matched)) {
      rule <- x$rules_matched[[i]]
      cat(sprintf("[%s] %s  (citation: %s)\n",
                  toupper(rule$severity),
                  rule$id,
                  rule$citation$key))
      # Wrap recommendation text at ~72 chars for readability
      rec <- rule$recommendation
      cat("  Recommendation:", rec, "\n\n")
    }
  }

  .advisor_pro_footer()
  invisible(x)
}


# ---- Internal engine (.build_offline_advice) ----------------------------------

#' Build an offline es_advice object by evaluating rules against diagnostics
#'
#' Evaluates each rule's condition function against the provided diagnostics
#' inside tryCatch so a malformed predicate never crashes the advice call
#' (ADV-08 never-error guarantee). Keeps all matching rules (not first-match);
#' severity-ranks them error > warning > info.
#'
#' @param diag An \code{es_diagnostics} object (a named list with class
#'   \code{"es_diagnostics"}) already extracted from a fitted task.
#' @param rules A list of KB rule records (filtered to a single category),
#'   each having fields \code{id}, \code{category}, \code{condition},
#'   \code{recommendation}, \code{citation}, \code{severity}.
#'
#' @return A named list of class \code{"es_advice"} with fields
#'   \code{source}, \code{is_deterministic}, \code{rules_matched},
#'   \code{diagnostics_ref}.
#'
#' @noRd
.build_offline_advice <- function(diag, rules) {
  severity_order <- c("error" = 1L, "warning" = 2L, "info" = 3L)

  matched <- list()
  for (rule in rules) {
    fires <- tryCatch(
      isTRUE(rule$condition(diag)),
      error = function(e) FALSE
    )
    if (isTRUE(fires)) {
      # Surface only the plain-scalar fields (no function objects)
      matched[[length(matched) + 1L]] <- list(
        id             = rule$id,
        category       = rule$category,
        recommendation = rule$recommendation,
        citation       = rule$citation,
        severity       = rule$severity
      )
    }
  }

  # Severity-rank: error first, then warning, then info
  if (length(matched) > 1L) {
    sev_vals <- vapply(matched, function(r) {
      severity_order[[r$severity]] %||% 99L
    }, integer(1L))
    matched <- matched[order(sev_vals)]
  }

  structure(
    list(
      source          = "offline_kb",
      is_deterministic = TRUE,
      rules_matched   = matched,
      diagnostics_ref = diag
    ),
    class = "es_advice"
  )
}


# ---- Offline narrative engine (OFFLINE-01) ------------------------------------

#' Build an offline OfflineNarrative for report_writing task type
#'
#' Synthesises per-section prose from KB rules and diagnostics without any LLM
#' provider. Returns an \code{OfflineNarrative} S3 object with four section
#' keys: \code{exec_summary}, \code{data_methods}, \code{results},
#' \code{robustness}. These key names are a contract locked by Phase 17 ---
#' Phase 18 renderer depends on them.
#'
#' No-fabrication rule: every helper builds sentences exclusively from values
#' in \code{diag} at runtime via \code{sprintf()}. No hard-coded statistical
#' decimals in helper bodies.
#'
#' @param diag An \code{es_diagnostics} object.
#' @return An S3 object of class \code{"OfflineNarrative"} with fields
#'   \code{source}, \code{is_deterministic}, \code{exec_summary},
#'   \code{data_methods}, \code{results}, \code{robustness}.
#' @noRd
.build_offline_narrative <- function(diag) {
  # Run both KB rule categories against diagnostics
  all_rules  <- es_kb()
  stat_rules <- Filter(function(r) r$category == "stat_choice", all_rules)
  rob_rules  <- Filter(function(r) r$category == "robustness",  all_rules)

  stat_advice <- .build_offline_advice(diag, stat_rules)
  rob_advice  <- .build_offline_advice(diag, rob_rules)

  # Synthesise prose from matched rules + diagnostics scalars
  exec_summary <- .narrative_exec_summary(diag, stat_advice, rob_advice)
  data_methods <- .narrative_data_methods(diag)
  results_sec  <- .narrative_results(diag)
  robustness   <- .narrative_robustness(rob_advice, diag)

  structure(
    list(
      source           = "offline_kb",
      is_deterministic = TRUE,
      exec_summary     = exec_summary,
      data_methods     = data_methods,
      results          = results_sec,
      robustness       = robustness
    ),
    class = "OfflineNarrative"
  )
}


#' Executive summary prose section for offline narrative
#' @noRd
.narrative_exec_summary <- function(diag, stat_advice, rob_advice) {
  n_valid  <- diag$cross_sectional$n_valid_events %||% diag$meta$n_events_total
  n_total  <- diag$meta$n_events_total

  # Median CAR t-statistic across events
  car_t_vals <- diag$event_window$car_t
  med_car_t  <- if (length(car_t_vals) > 0L) {
    median(car_t_vals, na.rm = TRUE)
  } else {
    NA_real_
  }

  # Count fired stat-choice rules
  n_stat_rules <- length(stat_advice$rules_matched)
  n_rob_rules  <- length(rob_advice$rules_matched)

  if (is.finite(med_car_t)) {
    sprintf(
      paste0(
        "This offline rule-based event study report summarises %d events (%d fitted). ",
        "The median cumulative abnormal return (CAR) t-statistic across fitted events ",
        "was %.3f. The offline KB matched %d test-statistic recommendation(s) and ",
        "%d robustness concern(s) based on the computed diagnostics."
      ),
      n_total, n_valid, med_car_t, n_stat_rules, n_rob_rules
    )
  } else {
    sprintf(
      paste0(
        "This offline rule-based event study report summarises %d events (%d fitted). ",
        "The offline KB matched %d test-statistic recommendation(s) and ",
        "%d robustness concern(s) based on the computed diagnostics."
      ),
      n_total, n_valid, n_stat_rules, n_rob_rules
    )
  }
}


#' Data / methods prose section for offline narrative
#' @noRd
.narrative_data_methods <- function(diag) {
  n_total  <- diag$meta$n_events_total
  n_shown  <- diag$meta$n_events_shown
  n_valid  <- diag$cross_sectional$n_valid_events %||% n_shown

  # Mean R-squared across estimation windows
  r2_vals  <- diag$estimation_window$r2
  mean_r2  <- if (length(r2_vals) > 0L) mean(r2_vals, na.rm = TRUE) else NA_real_

  # Mean sigma
  sig_vals <- diag$estimation_window$sigma
  mean_sig <- if (length(sig_vals) > 0L) mean(sig_vals, na.rm = TRUE) else NA_real_

  r2_clause <- if (is.finite(mean_r2)) {
    sprintf("Mean estimation-window R-squared was %.3f", mean_r2)
  } else {
    "Estimation-window R-squared data were not available"
  }

  sig_clause <- if (is.finite(mean_sig)) {
    sprintf("mean residual sigma %.4f", mean_sig)
  } else {
    "residual sigma not available"
  }

  sprintf(
    paste0(
      "The analysis covers %d event(s) in total; %d event(s) were included in the ",
      "detailed diagnostic output and %d event(s) had sufficient data for inference. ",
      "%s, with %s. ",
      "These model-fit metrics are computed entirely from package-calculated diagnostics ",
      "with no external data sources."
    ),
    n_total, n_shown, n_valid,
    r2_clause, sig_clause
  )
}


#' Results prose section for offline narrative
#' @noRd
.narrative_results <- function(diag) {
  n_valid   <- diag$cross_sectional$n_valid_events %||% diag$meta$n_events_total

  car_t_vals <- diag$event_window$car_t
  car_p_vals <- diag$event_window$car_p

  med_car_t <- if (length(car_t_vals) > 0L) median(car_t_vals, na.rm = TRUE) else NA_real_
  med_car_p <- if (length(car_p_vals) > 0L) median(car_p_vals, na.rm = TRUE) else NA_real_

  final_car_vals <- diag$event_window$final_car
  mean_final_car <- if (length(final_car_vals) > 0L) {
    mean(final_car_vals, na.rm = TRUE)
  } else {
    NA_real_
  }

  t_clause <- if (is.finite(med_car_t)) {
    sprintf("median CAR t-statistic of %.3f", med_car_t)
  } else {
    "CAR t-statistic data not available"
  }

  p_clause <- if (is.finite(med_car_p)) {
    sprintf("median CAR p-value %.4f", med_car_p)
  } else {
    "p-value data not available"
  }

  car_clause <- if (is.finite(mean_final_car)) {
    sprintf("mean final CAR of %.4f", mean_final_car)
  } else {
    "final CAR data not available"
  }

  sprintf(
    paste0(
      "Across %d fitted event(s), the event-window analysis yielded a %s ",
      "(%s). The %s across all fitted events. ",
      "[Offline rule-based narrative — all values sourced from package-computed diagnostics.]"
    ),
    n_valid, t_clause, p_clause, car_clause
  )
}


#' Robustness / caveats prose section for offline narrative
#' @noRd
.narrative_robustness <- function(rob_advice, diag) {
  n_rob   <- length(rob_advice$rules_matched)
  n_valid <- diag$cross_sectional$n_valid_events %||% diag$meta$n_events_total

  # Overlap indicator
  any_overlap <- isTRUE(diag$cross_sectional$any_overlap)

  # Durbin-Watson mean
  dw_vals  <- diag$estimation_window$dw_stat
  mean_dw  <- if (length(dw_vals) > 0L) mean(dw_vals, na.rm = TRUE) else NA_real_

  overlap_clause <- if (any_overlap) {
    "Event-window overlap was detected; clustered or portfolio-based inference is advisable."
  } else {
    "No event-window overlap was detected among the fitted events."
  }

  dw_clause <- if (is.finite(mean_dw)) {
    sprintf(
      "The mean Durbin-Watson statistic across estimation windows was %.3f.",
      mean_dw
    )
  } else {
    ""
  }

  if (n_rob == 0L) {
    base_msg <- sprintf(
      paste0(
        "No robustness concerns were flagged by the offline KB for %d fitted event(s). ",
        "%s %s",
        "[Offline rule-based narrative — all values sourced from package-computed diagnostics.]"
      ),
      n_valid, overlap_clause,
      if (nzchar(dw_clause)) paste0(dw_clause, " ") else ""
    )
  } else {
    # Collect rule IDs and recommendations for prose
    rule_texts <- vapply(rob_advice$rules_matched, function(r) {
      sprintf("%s: %s", r$id, r$recommendation)
    }, character(1L))
    rules_str <- paste(rule_texts, collapse = " ")

    base_msg <- sprintf(
      paste0(
        "The offline KB flagged %d robustness concern(s) for %d fitted event(s): %s ",
        "%s %s",
        "[Offline rule-based narrative — all values sourced from package-computed diagnostics.]"
      ),
      n_rob, n_valid, rules_str, overlap_clause,
      if (nzchar(dw_clause)) paste0(dw_clause, " ") else ""
    )
  }

  base_msg
}
