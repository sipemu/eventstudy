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
