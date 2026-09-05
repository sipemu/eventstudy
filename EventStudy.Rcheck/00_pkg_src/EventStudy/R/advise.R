# =============================================================================
# Grounded Advise Engine — es_advise() + Advice S3 + runtime grounding guard
# =============================================================================
#
# This file delivers the Phase 7 grounding guarantee: every claim returned by
# es_advise() is provably tied to a diagnostic the package actually computed,
# enforced by a runtime R grounding guard (.validate_grounding()) independent of
# the prompt. The guard follows the drop-and-keep contract:
#   - Drop only the ungrounded recommendation, keep the rest.
#   - Append one caveat recording N drops.
#   - Emit exactly ONE warning(msg, call.=FALSE).
#   - Never stop() on a guard failure (only on missing provider for LLM-only types).
#
# Failure discipline mirrors .handle_degenerate() in R/contract.R and
# .provider_failure() in R/provider.R: a single function is the sole
# warning-emitting point; never more than one warning per call.
#
# All jsonlite usage is guarded by requireNamespace("jsonlite", quietly=TRUE)
# (Suggests discipline — package loads and tests cleanly with jsonlite uninstalled).

# ---------------------------------------------------------------------------
# Task types and routing constants
# ---------------------------------------------------------------------------

# LLM-only types: no offline fallback; require a provider (ADV-06)
LLM_ONLY_TYPES <- c("interpret", "recommend_model", "design_discussion", "report_writing")

# KB-grounded types: offline fallback via Phase 5 path; provider optional
KB_TYPES <- c("recommend_stat", "flag_robustness")

# All valid types
VALID_TASK_TYPES <- c(LLM_ONLY_TYPES, KB_TYPES)

# ---------------------------------------------------------------------------
# Internal: KB key-path map
# ---------------------------------------------------------------------------

# KB_KEY_MAP: maps each KB rule ID to the diagnostic key(s) it reads.
# Derived verbatim from Reading condition bodies in knowledge_base.R:153-393.
# [VERIFIED: 07-RESEARCH.md Finding 4]
KB_KEY_MAP <- list(
  "KB-NORM-PATELL"       = list(list(key = "estimation_window.shapiro_p", threshold = 0.05, direction = "above")),
  "KB-NONNORM-NONPAR"    = list(list(key = "estimation_window.shapiro_p", threshold = 0.05, direction = "below")),
  "KB-VAR-INCREASE-BMP"  = list(
    list(key = "cross_sectional.car_iqr", threshold = 0.10, direction = "above"),
    list(key = "cross_sectional.car_sd",  threshold = 0.15, direction = "above")
  ),
  "KB-OVERLAP-KP"        = list(list(key = "cross_sectional.n_overlap_pairs", threshold = 0L, direction = "above")),
  "KB-AC-WARN"           = list(list(key = "estimation_window.dw_stat", threshold = 1.5, direction = "below")),
  "KB-LOWFIT-WARN"       = list(list(key = "estimation_window.r2", threshold = 0.05, direction = "below")),
  "KB-DEGEN-EVENTS"      = list(list(key = "cross_sectional.n_valid_events", threshold = 0.8, direction = "below")),
  "KB-SMALL-N"           = list(list(key = "cross_sectional.n_valid_events", threshold = 10L, direction = "below"))
)

# ---------------------------------------------------------------------------
# Internal: Advice S3 constructors
# ---------------------------------------------------------------------------

#' Build an empty Advice S3 object
#'
#' Used as the degrade result on any parse/provider failure.
#'
#' @param source_label Character. Provider label or "offline_kb".
#' @param task_type Character. One of the six valid task types.
#' @return An S3 object of class "Advice".
#' @noRd
.empty_advice <- function(source_label, task_type = "") {
  structure(
    list(
      source           = source_label,
      is_deterministic = FALSE,
      task_type        = task_type,
      interpretation   = "",
      recommendations  = list(),
      caveats          = character(),
      n_dropped        = 0L
    ),
    class = "Advice"
  )
}

# ---------------------------------------------------------------------------
# Internal: Diagnostic key-path resolver
# ---------------------------------------------------------------------------

#' Resolve a dotted key-path into the es_diagnostics object
#'
#' Parses "section.key" and "section.key[i]" (1-based index) forms.
#' Navigates diag[[section]][[key]]; applies [[idx]] when an index suffix
#' is present. Returns NA_real_ when section/key/index is absent/invalid.
#'
#' Implementation follows 07-RESEARCH.md Finding 3 verbatim.
#'
#' @param diag An es_diagnostics object (named list with six sections).
#' @param key_path Character. Dotted path, e.g. "cross_sectional.car_iqr"
#'   or "estimation_window.shapiro_p[3]".
#' @return The resolved value, or NA_real_ if any part of the path is absent.
#' @noRd
.resolve_diag_key <- function(diag, key_path) {
  # Validate the format: must match "section.key" or "section.key[i]"
  m <- regmatches(key_path, regexpr("^([^.]+)\\.([^\\[]+)(?:\\[(\\d+)\\])?$", key_path, perl = TRUE))
  if (length(m) == 0L || nchar(m) == 0L) return(NA_real_)

  parts   <- strsplit(key_path, "\\.")[[1L]]
  section <- parts[[1L]]
  rest    <- paste(parts[-1L], collapse = ".")

  # Extract index if present (e.g. "shapiro_p[3]" -> idx=3, rest="shapiro_p")
  idx <- NULL
  idx_match <- regmatches(rest, regexpr("\\[(\\d+)\\]$", rest, perl = TRUE))
  if (length(idx_match) > 0L && nchar(idx_match) > 0L) {
    idx  <- as.integer(gsub(".*\\[(\\d+)\\].*", "\\1", idx_match))
    rest <- sub("\\[\\d+\\]$", "", rest)
  }

  # Navigate section
  sec_val <- diag[[section]]
  if (is.null(sec_val)) return(NA_real_)

  val <- sec_val[[rest]]
  if (is.null(val)) return(NA_real_)

  if (!is.null(idx)) {
    if (idx < 1L || idx > length(val)) return(NA_real_)
    val <- val[[idx]]
  }
  val
}

# ---------------------------------------------------------------------------
# Internal: JSON parsing with requireNamespace guard
# ---------------------------------------------------------------------------

#' Parse provider JSON text into an advice list
#'
#' Guards requireNamespace("jsonlite"); degrades to one warning + .empty_advice
#' on any failure (absent jsonlite, NA/empty text, invalid JSON, non-list result).
#' Uses simplifyVector=FALSE to preserve list-of-lists structure.
#'
#' This is NOT the warning-emitting point for the grounding guard — only for
#' parse failures. .validate_grounding() is the single guard-drop warning point.
#'
#' @param resp An es_provider_response with a $text field.
#' @param source_label Character. Used in warning messages.
#' @param task_type Character. Passed to .empty_advice on failure.
#' @return A parsed list (on success) or an Advice S3 object (on failure).
#' @noRd
.parse_advice_json <- function(resp, source_label, task_type) {
  # If the provider already failed, it emitted its single warning via
  # .provider_failure() (error field set, text = NA). Degrade SILENTLY here —
  # warning again would violate the exactly-one-warning contract.
  if (!is.null(resp$error)) {
    return(.empty_advice(source_label, task_type))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning(
      "jsonlite is required to parse LLM advice but is not installed. Returning empty Advice.",
      call. = FALSE
    )
    return(.empty_advice(source_label, task_type))
  }

  txt <- resp$text %||% NA_character_
  if (length(txt) == 0L || is.na(txt) || !nzchar(txt)) {
    warning(
      sprintf("Provider '%s' returned no text. Returning empty Advice.", source_label),
      call. = FALSE
    )
    return(.empty_advice(source_label, task_type))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !is.list(parsed)) {
    warning(
      sprintf("Provider '%s' returned malformed JSON. Returning empty Advice.", source_label),
      call. = FALSE
    )
    return(.empty_advice(source_label, task_type))
  }

  parsed
}

# ---------------------------------------------------------------------------
# Internal: Grounding guard (the SINGLE warning-emitting point for guard drops)
# ---------------------------------------------------------------------------

#' Validate grounding of an advice list against computed diagnostics
#'
#' For each recommendation, checks every evidence entry:
#'   - key must resolve in the diagnostics (absent -> mismatch)
#'   - reported value must match actual value within tolerance (numeric) or
#'     be identical (non-numeric)
#'   - NA-vs-present in either direction is a mismatch
#'
#' Drops the entire recommendation on any bad evidence entry.
#' Emits exactly ONE warning when n_drop > 0 (this is the sole warning point
#' for guard failures — mirrors .provider_failure() discipline).
#' Appends a caveat recording N drops.
#'
#' Tolerance: abs(reported - actual) <= max(abs_tol, rel_tol * abs(actual)).
#' Configurable via package options "EventStudy.guard_abs_tol" and
#' "EventStudy.guard_rel_tol".
#'
#' Implementation follows 07-RESEARCH.md Finding 3 verbatim.
#'
#' @param advice_list A list with $recommendations (list-of-lists) and $caveats.
#' @param diagnostics An es_diagnostics object.
#' @param abs_tol Absolute tolerance for numeric comparison.
#' @param rel_tol Relative tolerance for numeric comparison.
#' @return The modified advice_list with dropped recs removed and caveat appended.
#' @noRd
.validate_grounding <- function(advice_list, diagnostics,
                                abs_tol = getOption("EventStudy.guard_abs_tol", 1e-6),
                                rel_tol = getOption("EventStudy.guard_rel_tol", 1e-4)) {
  recs   <- advice_list$recommendations %||% list()
  kept   <- list()
  n_drop <- 0L

  for (rec in recs) {
    ev_list  <- rec$evidence %||% list()
    # A recommendation with NO evidence is ungrounded by definition — it cites
    # nothing the package computed. Empty evidence[] must be DROPPED, never kept
    # (an empty inner loop would otherwise leave all_good = TRUE). (CR-01)
    all_good <- length(ev_list) > 0L

    for (ev in ev_list) {
      key <- ev$diagnostic_key %||% ""
      if (!nzchar(key)) {
        all_good <- FALSE
        break
      }

      actual   <- .resolve_diag_key(diagnostics, key)
      reported <- ev$value

      # NA handling — all(is.na()) catches an ALL-NA vector too, not just a
      # scalar NA (e.g. every event failed model fitting). Without this a
      # length>1 all-NA vector slips through to mean(na.rm=TRUE) -> NaN and
      # crashes the tolerance compare with no warning. (CR-02)
      actual_na   <- length(actual) == 0L || is.null(actual) ||
                     all(is.na(actual))
      reported_na <- is.null(reported) ||
                     (length(reported) == 1L && is.na(reported))

      if (actual_na && !reported_na) { all_good <- FALSE; break }  # key absent, LLM has value
      if (!actual_na && reported_na) { all_good <- FALSE; break }  # key present, LLM says NA
      if (actual_na && reported_na)  next                          # both NA — consistent

      # Summarize vector-valued actuals to a scalar for comparison
      # (Vector keys without an index suffix return the whole vector;
      #  the LLM must cite a scalar. Summarize to mean, matching KB logic.)
      if (length(actual) > 1L && is.numeric(actual)) {
        actual <- mean(actual, na.rm = TRUE)
      }

      # Numeric tolerance comparison
      if (is.numeric(actual) && is.numeric(reported)) {
        # Guard: reported must also be scalar
        if (length(reported) != 1L) {
          all_good <- FALSE
          break
        }
        # Non-finite (Inf/-Inf/NaN) on either side is a mismatch, never a
        # match: with actual = Inf the relative tolerance becomes Inf and would
        # accept ANY reported value; abs(Inf - Inf) = NaN would also crash the
        # comparison. Drop the recommendation instead. (CR-03)
        if (!is.finite(actual) || !is.finite(reported)) {
          all_good <- FALSE
          break
        }
        tol <- max(abs_tol, rel_tol * abs(actual))
        if (abs(reported - actual) > tol) {
          all_good <- FALSE
          break
        }
      } else {
        # Non-numeric (logical, character): exact match only
        if (!identical(actual, reported)) {
          all_good <- FALSE
          break
        }
      }
    }

    if (all_good) {
      kept[[length(kept) + 1L]] <- rec
    } else {
      n_drop <- n_drop + 1L
    }
  }

  # Exactly ONE warning when any recs are dropped (this is the sole warning point)
  if (n_drop > 0L) {
    warning(
      sprintf(
        "Grounding guard: %d recommendation(s) dropped \u2014 evidence cited absent or mismatched diagnostic values.",
        n_drop
      ),
      call. = FALSE
    )
  }

  advice_list$recommendations <- kept
  advice_list$n_dropped       <- n_drop

  if (n_drop > 0L) {
    drop_caveat <- sprintf(
      "%d recommendation(s) were dropped by the grounding guard: evidence cited a diagnostic key absent from the computed diagnostics or a value mismatching beyond tolerance.",
      n_drop
    )
    advice_list$caveats <- c(advice_list$caveats %||% character(), drop_caveat)
  }

  advice_list
}

# ---------------------------------------------------------------------------
# Internal: Build Advice S3 from parsed JSON
# ---------------------------------------------------------------------------

#' Assemble an Advice S3 object from parsed JSON, then run the grounding guard
#'
#' @param parsed A parsed list from .parse_advice_json.
#' @param diagnostics An es_diagnostics object.
#' @param source_label Character. Provider label.
#' @param task_type Character. One of the six valid task types.
#' @return An S3 object of class "Advice", post-guard.
#' @noRd
.build_advice_from_parsed <- function(parsed, diagnostics, source_label, task_type) {
  advice_list <- list(
    source           = source_label,
    is_deterministic = FALSE,
    task_type        = task_type,
    interpretation   = parsed$interpretation %||% "",
    recommendations  = parsed$recommendations %||% list(),
    caveats          = if (is.null(parsed$caveats)) character()
                       else unlist(parsed$caveats, use.names = FALSE),
    n_dropped        = 0L
  )

  # Run the grounding guard (the single warning-emitting point)
  advice_list <- .validate_grounding(advice_list, diagnostics)

  structure(advice_list, class = "Advice")
}

# ---------------------------------------------------------------------------
# Internal: JSON schema for structured output
# ---------------------------------------------------------------------------

#' Build the JSON schema for the Advice contract
#'
#' This schema is passed to provider$complete(prompt, schema) so the LLM
#' produces structured JSON. The schema is a JSON Schema draft-7 compatible
#' R list. Verbatim from 07-RESEARCH.md Finding 2.
#'
#' @return A named list representing the JSON Schema.
#' @noRd
.advice_schema <- function() {
  list(
    type = "object",
    properties = list(
      interpretation = list(type = "string"),
      recommendations = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            action          = list(type = "string"),
            kind            = list(type = "string"),
            rationale       = list(type = "string"),
            expected_effect = list(type = "string"),
            evidence = list(
              type = "array",
              minItems = 1L,
              items = list(
                type = "object",
                properties = list(
                  diagnostic_key = list(type = "string"),
                  value          = list(type = "number"),
                  threshold      = list(type = "number"),
                  direction      = list(
                    type = "string",
                    enum = list("above", "below", "equal", "na")
                  )
                ),
                required = list("diagnostic_key", "value", "threshold", "direction")
              )
            )
          ),
          required = list("action", "kind", "rationale", "expected_effect", "evidence")
        )
      ),
      caveats = list(type = "array", items = list(type = "string"))
    ),
    required = list("interpretation", "recommendations", "caveats"),
    additionalProperties = FALSE
  )
}

# ---------------------------------------------------------------------------
# Internal: KB helpers for prompt injection
# ---------------------------------------------------------------------------

#' Extract KB rules for prompt injection (strips non-serializable condition)
#'
#' Filters es_kb() by category (NULL = all), then maps each rule to a
#' serializable list (OMITS the condition closure per knowledge_base.R:420-421).
#'
#' @param category Optional character category filter ("stat_choice" or "robustness").
#' @return A list of serializable rule records.
#' @noRd
.kb_for_prompt <- function(category = NULL) {
  rules <- es_kb()
  if (!is.null(category)) {
    rules <- Filter(function(r) r$category == category, rules)
  }
  lapply(rules, function(r) {
    list(
      id             = r$id,
      category       = r$category,
      recommendation = r$recommendation,
      citation       = r$citation,
      severity       = r$severity
      # condition OMITTED — not serializable (knowledge_base.R:420-421)
    )
  })
}

#' Convert a fired KB rule into a structured evidence list
#'
#' Uses KB_KEY_MAP to find the diagnostic key(s) each rule reads, resolves the
#' actual value from the diagnostics object, and produces evidence entries in
#' the {diagnostic_key, value, threshold, direction} format.
#'
#' Because the value is resolved FROM the diagnostics, these evidence entries
#' are grounded by construction and pass the guard.
#'
#' Vector-valued keys (e.g. estimation_window.shapiro_p) are summarized to
#' a scalar (mean with na.rm=TRUE) matching the KB rule's aggregate logic.
#'
#' @param kb_rule A single KB rule record (from es_kb()).
#' @param diagnostics An es_diagnostics object.
#' @return A list of evidence entry lists.
#' @noRd
.kb_to_evidence <- function(kb_rule, diagnostics) {
  key_specs <- KB_KEY_MAP[[kb_rule$id]] %||% list()
  lapply(key_specs, function(spec) {
    actual_val <- .resolve_diag_key(diagnostics, spec$key)
    list(
      diagnostic_key = spec$key,
      value          = if (length(actual_val) == 0L || is.null(actual_val)) {
                         NA_real_
                       } else if (is.numeric(actual_val) && length(actual_val) > 1L) {
                         mean(actual_val, na.rm = TRUE)
                       } else {
                         actual_val
                       },
      threshold      = spec$threshold,
      direction      = spec$direction
    )
  })
}

# ---------------------------------------------------------------------------
# Internal: Prompt builder
# ---------------------------------------------------------------------------

#' Build a prompt for the LLM provider
#'
#' Assembles a structured prompt with:
#'   1. System context (cite only provided diagnostics)
#'   2. Diagnostics block (JSON via jsonlite, or minimal text fallback)
#'   3. KB context (recommend_stat/flag_robustness only)
#'   4. KB-pre-grounded recommendations (when kb_recs supplied)
#'   5. Task instruction
#'   6. Schema reminder
#'
#' For KB-grounded types (recommend_stat/flag_robustness with provider), the
#' task instruction explicitly bounds the LLM to prose only — it must not invent
#' new diagnostic keys, add/remove recommendations, or modify any evidence entry.
#' This is the intent-preservation instruction; the guard is the enforcement backstop.
#'
#' @param task_type Character. One of the six valid task types.
#' @param diagnostics An es_diagnostics object.
#' @param kb_context Optional list of KB rule records (from .kb_for_prompt).
#' @param kb_recs Optional list of pre-grounded recommendation lists (from KB).
#' @return A character string prompt.
#' @noRd
.build_prompt <- function(task_type, diagnostics, kb_context = NULL, kb_recs = NULL) {
  # 1. System context
  system_ctx <- paste0(
    "You are an event-study methodology expert. ",
    "Cite ONLY diagnostic values provided below. ",
    "Do not invent numbers or reference diagnostics not present in the data. ",
    "Return ONLY valid JSON matching the provided schema."
  )

  # 2. Diagnostics block (JSON if jsonlite available, else minimal text)
  diag_block <- if (requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(
      jsonlite::toJSON(unclass(diagnostics), auto_unbox = TRUE, null = "null"),
      error = function(e) paste("Diagnostics:", paste(names(diagnostics), collapse = ", "))
    )
  } else {
    paste("Diagnostics sections:", paste(names(diagnostics), collapse = ", "))
  }

  # 3. KB context block (KB types only)
  kb_block <- ""
  if (!is.null(kb_context) && length(kb_context) > 0L) {
    kb_text <- if (requireNamespace("jsonlite", quietly = TRUE)) {
      tryCatch(
        jsonlite::toJSON(kb_context, auto_unbox = TRUE),
        error = function(e) ""
      )
    } else {
      ""
    }
    if (nzchar(kb_text)) {
      kb_block <- paste0("\n\nKnowledge base rules (peer-reviewed methodology):\n", kb_text)
    }
  }

  # 4. Pre-grounded recommendations block (KB+provider path)
  kb_recs_block <- ""
  if (!is.null(kb_recs) && length(kb_recs) > 0L) {
    recs_text <- if (requireNamespace("jsonlite", quietly = TRUE)) {
      tryCatch(
        jsonlite::toJSON(kb_recs, auto_unbox = TRUE),
        error = function(e) ""
      )
    } else {
      ""
    }
    if (nzchar(recs_text)) {
      kb_recs_block <- paste0(
        "\n\nPre-grounded recommendations (evidence[] already set from KB \u2014 DO NOT modify):\n",
        recs_text
      )
    }
  }

  # 5. Task instruction
  task_instruction <- switch(task_type,
    "recommend_stat" = if (!is.null(kb_recs) && length(kb_recs) > 0L) {
      paste0(
        "\n\nTask: Fill in the rationale, expected_effect, and interpretation prose fields for the ",
        "pre-grounded recommendations above. ",
        "The recommendations array is pre-populated with evidence[] derived from peer-reviewed ",
        "event-study methodology rules. Do NOT invent new diagnostic keys, do NOT add or remove ",
        "recommendations, and do NOT modify any existing evidence entry \u2014 fill only the rationale, ",
        "expected_effect, and top-level interpretation prose fields."
      )
    } else {
      "\n\nTask: Recommend appropriate statistical tests for this event study, citing only the diagnostics above."
    },
    "flag_robustness" = if (!is.null(kb_recs) && length(kb_recs) > 0L) {
      paste0(
        "\n\nTask: Fill in the rationale, expected_effect, and interpretation prose fields for the ",
        "pre-grounded robustness recommendations above. ",
        "The recommendations array is pre-populated with evidence[] derived from peer-reviewed ",
        "event-study methodology rules. Do NOT invent new diagnostic keys, do NOT add or remove ",
        "recommendations, and do NOT modify any existing evidence entry \u2014 fill only the rationale, ",
        "expected_effect, and top-level interpretation prose fields."
      )
    } else {
      "\n\nTask: Flag robustness issues for this event study, citing only the diagnostics above."
    },
    "interpret" = paste0(
      "\n\nTask: Provide an interpretation of these event study results for a financial researcher. ",
      "Cite only the provided diagnostic values."
    ),
    "recommend_model" = paste0(
      "\n\nTask: Recommend an appropriate return model for this event study based on the diagnostics above."
    ),
    "design_discussion" = paste0(
      "\n\nTask: Discuss the event study design choices appropriate given these diagnostics."
    ),
    "report_writing" = paste0(
      "\n\nTask: Write Methods/Results prose suitable for an academic event study paper, ",
      "based on the provided diagnostics. Each recommendation should be a paragraph-form narrative. ",
      "Caveats should flag statistical limitations."
    ),
    paste0("\n\nTask: Provide advice for task type '", task_type, "' based on the diagnostics above.")
  )

  # Assemble prompt
  paste0(
    system_ctx,
    "\n\nDiagnostics (JSON):\n", diag_block,
    kb_block,
    kb_recs_block,
    task_instruction,
    "\n\nReturn ONLY valid JSON matching the provided schema."
  )
}

# ---------------------------------------------------------------------------
# Public: print.Advice S3 method
# ---------------------------------------------------------------------------

#' Print method for Advice objects
#'
#' Prints a structured summary of the grounded AI advice, including source,
#' task type, grounding guard status, interpretation, recommendations with
#' evidence, and caveats. Follows the package convention of
#' \code{print.es_advice} (cat-based, invisible return).
#'
#' @param x An object of class \code{"Advice"}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.Advice <- function(x, ...) {
  cat("Event Study Advice\n")
  cat("==================\n")
  cat("Source:        ", x$source, "\n")
  cat("Task type:     ", x$task_type, "\n")
  cat("Deterministic: ", x$is_deterministic, "\n")
  n_recs <- length(x$recommendations)
  cat("Recommendations:", n_recs, "\n")
  if (!is.null(x$n_dropped) && x$n_dropped > 0L) {
    cat("[GUARD]", x$n_dropped, "recommendation(s) dropped as ungrounded.\n")
  }
  cat("\n")
  if (nzchar(x$interpretation %||% "")) {
    cat("Interpretation:\n ", x$interpretation, "\n\n")
  }
  if (n_recs == 0L) {
    cat("(No recommendations.)\n")
  } else {
    for (i in seq_along(x$recommendations)) {
      r <- x$recommendations[[i]]
      cat(sprintf("[%d] %s\n", i, r$action %||% ""))
      cat("    Kind:   ", r$kind %||% "", "\n")
      cat("    Effect: ", r$expected_effect %||% "", "\n")
      cat("    Evidence:\n")
      for (ev in r$evidence %||% list()) {
        cat(sprintf("      %s = %s (threshold %s, %s)\n",
                    ev$diagnostic_key %||% "",
                    ev$value %||% "NA",
                    ev$threshold %||% "NA",
                    ev$direction %||% ""))
      }
      cat("\n")
    }
  }
  if (length(x$caveats) > 0L) {
    cat("Caveats:\n")
    for (cv in x$caveats) cat(" -", cv, "\n")
  }
  .advisor_pro_footer()
  invisible(x)
}

# ---------------------------------------------------------------------------
# Public: es_advise() — the grounded advise engine
# ---------------------------------------------------------------------------

#' Grounded AI Advice for Event Study Results
#'
#' Produces a grounded \code{Advice} S3 object by routing through a task-type
#' dispatch, calling an optional LLM provider, parsing the JSON response, and
#' running the runtime grounding guard — which drops any recommendation whose
#' \code{evidence[]} cites a diagnostic key absent from the computed diagnostics
#' or a value mismatching beyond numeric tolerance.
#'
#' \strong{Grounding guarantee:} Every returned recommendation is provably tied
#' to a value the package actually computed (\code{es_diagnostics()}). The guard
#' is enforced in R, independent of the prompt (ADV-04).
#'
#' \strong{Task type routing:}
#' \describe{
#'   \item{\code{recommend_stat}, \code{flag_robustness}}{
#'     No provider: returns the Phase 5 \code{es_advice} object (offline KB
#'     path — deterministic, \code{is_deterministic = TRUE}).
#'     With provider: KB produces grounded evidence[], LLM adds prose;
#'     returns an \code{Advice} object (\code{is_deterministic = FALSE}).
#'   }
#'   \item{\code{interpret}, \code{recommend_model}, \code{design_discussion},
#'     \code{report_writing}}{
#'     LLM-required: \code{stop()} when \code{provider = NULL} (ADV-06).
#'     With provider: LLM produces full advice; guard runs.
#'   }
#' }
#'
#' \strong{Failure discipline:} Any provider failure, malformed JSON, or empty
#' response degrades to one \code{warning()} + an empty \code{Advice} object —
#' never a crash, never a fabricated result (mirrors \code{.handle_degenerate()}).
#'
#' @param diagnostics An \code{es_diagnostics} object returned by
#'   \code{\link{es_diagnostics}()}.
#' @param task_type Character. One of \code{"interpret"}, \code{"recommend_stat"},
#'   \code{"recommend_model"}, \code{"flag_robustness"}, \code{"design_discussion"},
#'   \code{"report_writing"}.
#' @param provider An optional provider R6 object (from \code{\link{provider}()}).
#'   Required for LLM-only task types. When \code{NULL} and task type is KB-based,
#'   falls back to the Phase 5 offline path.
#' @param model Optional character model identifier. Reserved for forward
#'   compatibility: the effective model is the one the provider was constructed
#'   with (see \code{\link{provider}()}), so set the model there. Accepted here
#'   without error so calling code can pass it, but it does not override the
#'   provider's configured model.
#' @param ... Additional arguments (currently ignored; reserved for future use).
#'
#' @return For KB task types without a provider: an \code{es_advice} S3 object
#'   (Phase 5 offline path, \code{is_deterministic = TRUE}).
#'   For all other paths: an \code{Advice} S3 object with fields
#'   \code{source}, \code{is_deterministic}, \code{task_type},
#'   \code{interpretation}, \code{recommendations}, \code{caveats},
#'   \code{n_dropped}.
#'
#' @seealso \code{\link{es_diagnostics}}, \code{\link{recommend_stat}},
#'   \code{\link{flag_robustness}}, \code{\link{provider}}
#'
#' @examples
#' \dontrun{
#' task    <- run_event_study(my_task, ParameterSet$new())
#' diag    <- es_diagnostics(task)
#'
#' # Offline KB path (no LLM):
#' advice_kb <- es_advise(diag, task_type = "recommend_stat")
#' print(advice_kb)   # es_advice S3
#'
#' # LLM-grounded path:
#' p         <- provider("openai")
#' advice    <- es_advise(diag, task_type = "recommend_stat", provider = p)
#' print(advice)      # Advice S3 with grounding guarantee
#' }
#'
#' @export
es_advise <- function(diagnostics, task_type, provider = NULL, model = NULL, ...) {

  # --- Input validation ---
  if (!inherits(diagnostics, "es_diagnostics")) {
    stop("es_advise(): 'diagnostics' must be an es_diagnostics object.", call. = FALSE)
  }

  # Validate task_type (unknown type -> stop with list of valid types)
  if (!task_type %in% VALID_TASK_TYPES) {
    stop(
      sprintf(
        "es_advise(): unknown task_type '%s'. Valid types: %s.",
        task_type,
        paste(VALID_TASK_TYPES, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # --- No-provider routing ---
  if (is.null(provider)) {
    if (task_type %in% LLM_ONLY_TYPES) {
      # LLM-only types require a provider (ADV-06) — stop(), not warning
      stop(
        sprintf(
          "es_advise(): task_type '%s' requires a provider. Supply provider= or use recommend_stat()/flag_robustness() for the offline path.",
          task_type
        ),
        call. = FALSE
      )
    }
    # KB types with no provider -> Phase 5 offline path (unchanged es_advice)
    if (task_type == "recommend_stat") {
      return(recommend_stat.es_diagnostics(diagnostics, provider = NULL))
    }
    if (task_type == "flag_robustness") {
      return(flag_robustness.es_diagnostics(diagnostics, provider = NULL))
    }
  }

  # --- Provider path (all task types) ---
  source_label <- if (!is.null(provider$source)) provider$source else "custom"

  # For KB types: build KB-grounded evidence first, then call LLM for prose
  kb_context <- NULL
  kb_recs    <- NULL

  if (task_type %in% KB_TYPES) {
    # Run offline KB match to get fired rules
    category <- if (task_type == "recommend_stat") "stat_choice" else "robustness"
    kb_context <- .kb_for_prompt(category)

    # Convert fired rules to pre-grounded recommendations
    all_rules <- es_kb()
    filtered_rules <- Filter(function(r) r$category == category, all_rules)

    kb_recs <- list()
    for (rule in filtered_rules) {
      fires <- tryCatch(
        isTRUE(rule$condition(diagnostics)),
        error = function(e) FALSE
      )
      if (isTRUE(fires)) {
        evidence_list <- .kb_to_evidence(rule, diagnostics)
        kb_recs[[length(kb_recs) + 1L]] <- list(
          action          = rule$recommendation,
          kind            = rule$category,
          rationale       = "",    # LLM fills this
          expected_effect = "",    # LLM fills this
          evidence        = evidence_list
        )
      }
    }
  }

  # Build prompt
  prompt <- .build_prompt(task_type, diagnostics,
                          kb_context = kb_context,
                          kb_recs    = kb_recs)

  # Call provider
  resp <- tryCatch(
    provider$complete(prompt, .advice_schema()),
    error = function(e) {
      warning(
        sprintf("Provider call failed for task_type '%s': %s. Returning empty Advice.",
                task_type, conditionMessage(e)),
        call. = FALSE
      )
      NULL
    }
  )

  # Handle provider error (NULL means complete() itself threw, already warned above)
  if (is.null(resp)) {
    return(.empty_advice(source_label, task_type))
  }

  # Parse JSON (handles NA text, empty string, malformed JSON — each emits one warning)
  parsed <- .parse_advice_json(resp, source_label, task_type)

  # If parse failed, parsed is already an Advice S3 (returned by .parse_advice_json)
  if (inherits(parsed, "Advice")) {
    return(parsed)
  }

  # For KB+provider path: seed recommendations from KB-grounded set
  # LLM response should carry the KB recs with prose filled in,
  # but if it returns new recs, the guard will catch ungrounded evidence.
  # If kb_recs were supplied and LLM recommendations are empty/fewer, use KB recs.
  if (!is.null(kb_recs) && length(kb_recs) > 0L) {
    llm_recs <- parsed$recommendations %||% list()
    # If LLM returned recommendations, use those (guard will validate).
    # If LLM returned nothing (e.g. []), fall back to KB recs (already grounded).
    if (length(llm_recs) == 0L) {
      parsed$recommendations <- kb_recs
    }
  }

  # Build Advice S3 and run grounding guard
  .build_advice_from_parsed(parsed, diagnostics, source_label, task_type)
}
