# =============================================================================
# report_narrative.R -- Section-by-section grounded narrative assembler
#
# Delivers NARR-01..05, FORMAT-04, OFFLINE-02 (Phase 18, Plan 01).
#
# Public-facing assembler (consumed by generate_report() via the narrative=
# seam added in Phase 17):
#   assemble_report_narrative()
#
# Internal helpers (@noRd):
#   .calibrate_significance()
#   .extract_kb_references()
#   .sanitise_prose()
#   .sanitise_universal()
#   .sanitise_for_pdf()
#   .sanitise_for_word()
#   JOINT_HYPOTHESIS_CAVEAT (package-internal constant)
# =============================================================================

# ---------------------------------------------------------------------------
# Package-internal constant: joint-hypothesis caveat (NARR-05)
#
# This fixed text is appended to the robustness section of EVERY assembled
# narrative, regardless of whether the section was LLM-sourced or offline.
# Wording follows MacKinlay (1997) -- no numeric literals beyond significance
# constants (which are grounding-guard-exempt).
# ASCII-only (CRAN discipline).
# ---------------------------------------------------------------------------

JOINT_HYPOTHESIS_CAVEAT <- paste0(
  "Note: statistical significance of abnormal returns is a joint test of ",
  "the event effect and the correctness of the return model. ",
  "Rejection of the null may reflect model misspecification rather than a ",
  "true event effect (MacKinlay 1997)."
)


# ---------------------------------------------------------------------------
# .calibrate_significance() -- static four-tier significance calibrator
# (NARR-04)
#
# Boundaries (locked by CONTEXT.md):
#   NA / non-numeric          -> "not evaluable"
#   p < 0.01                  -> "strongly significant"
#   0.01 <= p < 0.05          -> "significant"
#   0.05 <= p < 0.10          -> "marginally significant"
#   p >= 0.10                 -> "not statistically significant"
#
# The constants 0.001 / 0.01 / 0.05 / 0.10 are already exempt from the
# prose grounding guard (R/advise.R:.is_grounded_literal(), line 442).
#
# @param p_value Numeric scalar p-value, or NA, or non-numeric.
# @return A character scalar label.
# @noRd
# ---------------------------------------------------------------------------

.calibrate_significance <- function(p_value) {
  if (length(p_value) == 0L || !is.numeric(p_value) || is.na(p_value)) {
    return("not evaluable")
  }
  if (p_value < 0.01) return("strongly significant")
  if (p_value < 0.05) return("significant")
  if (p_value < 0.10) return("marginally significant")
  "not statistically significant"
}


# ---------------------------------------------------------------------------
# .extract_kb_references() -- KB-sourced citation extraction (NARR-03)
#
# Fires every rule in the package knowledge base against `diagnostics`,
# collects the citation records of fired rules, deduplicates by citation$key,
# and returns the deduplicated list ordered alphabetically by citation$author.
#
# References are NEVER produced by the LLM -- they come exclusively from
# es_kb() rule records (KB-sourced).
#
# @param diagnostics An es_diagnostics object.
# @return A (possibly empty) list of citation record lists, each having at
#   least the fields: author, year, key (and optionally venue).
# @noRd
# ---------------------------------------------------------------------------

.extract_kb_references <- function(diagnostics) {
  all_rules <- tryCatch(es_kb(), error = function(e) list())

  if (length(all_rules) == 0L) return(list())

  # Fire each rule and collect citations of rules that fire
  citations <- list()
  for (rule in all_rules) {
    fires <- tryCatch(
      isTRUE(rule$condition(diagnostics)),
      error = function(e) FALSE
    )
    if (isTRUE(fires)) {
      citations[[length(citations) + 1L]] <- rule$citation
    }
  }

  if (length(citations) == 0L) return(list())

  # Deduplicate by citation$key (keep first occurrence)
  keys  <- vapply(citations, function(c) c$key, character(1L))
  citations <- citations[!duplicated(keys)]

  # Sort alphabetically by author
  authors <- vapply(citations, function(c) c$author, character(1L))
  citations[order(authors)]
}


# ---------------------------------------------------------------------------
# assemble_report_narrative() -- section-by-section grounded narrative (NARR-01/02)
#
# Builds a format-agnostic narrative list with the four locked section keys
# (exec_summary, data_methods, results, robustness) plus metadata fields
# section_sources and report_mode. This function is the LLM-call budget
# enforcer: it is called ONCE before any format-render loop; the same list
# is passed into every format render. LLM contacts = one per LLM-narrated
# section (exec_summary, results, robustness), never one per format.
#
# Algorithm:
#   1. Compute the offline baseline once via .build_offline_narrative() --
#      supplies data_methods always, and per-section fallback text.
#   2. For each key in sections_to_narrate: if provider is NULL, use offline.
#      If provider is non-NULL, call es_advise(..., section_hint=key) inside
#      tryCatch; on error or empty/dropped prose -> fall back to offline for
#      that section only.
#   3. data_methods is always sourced from the offline baseline (never LLM).
#   4. Append JOINT_HYPOTHESIS_CAVEAT to robustness unconditionally (NARR-05).
#   5. report_mode <- "ai" iff any section_sources value == "ai", else "offline".
#   6. Return list(exec_summary=, data_methods=, results=, robustness=,
#                  section_sources=, report_mode=).
#
# @param diagnostics An es_diagnostics object.
# @param provider Optional LLM provider. NULL -> fully offline narrative.
# @param sections_to_narrate Character vector of keys for which to request LLM
#   prose when provider is non-NULL. data_methods is excluded by design --
#   it is always sourced offline regardless of this argument.
# @return A named list with keys exec_summary, data_methods, results,
#   robustness (all character scalars), section_sources (named list of
#   "ai"/"offline" per section), and report_mode (scalar "ai"/"offline").
# @noRd
# ---------------------------------------------------------------------------

assemble_report_narrative <- function(
    diagnostics,
    provider            = NULL,
    sections_to_narrate = c("exec_summary", "results", "robustness")
) {

  # Step 1: Compute the offline baseline once (data_methods + per-section
  #         fallback text). .build_offline_narrative() is in advise_offline.R.
  offline_base <- .build_offline_narrative(diagnostics)

  # Step 2: Assemble per-section prose
  section_keys  <- c("exec_summary", "data_methods", "results", "robustness")
  prose         <- list()
  sources       <- list()

  for (key in section_keys) {
    # data_methods is always offline (deterministic auto-fill)
    if (key == "data_methods" || !key %in% sections_to_narrate || is.null(provider)) {
      prose[[key]]   <- offline_base[[key]]
      sources[[key]] <- "offline"
      next
    }

    # Try LLM path for this section.
    # .extract_llm_section_prose() returns NULL when the LLM returned nothing
    # usable (empty interpretation, failed grounding guard, etc.).  NULL here
    # means "fall back to offline"; a non-empty character scalar means "use AI".
    llm_prose <- tryCatch({
      advice <- es_advise(
        diagnostics,
        task_type    = "report_writing",
        provider     = provider,
        section_hint = key
      )
      # Extract prose: returns NULL on empty/failed result, non-empty string on success
      .extract_llm_section_prose(advice)
    }, error = function(e) {
      NULL
    })

    if (!is.null(llm_prose) && is.character(llm_prose) && nzchar(trimws(llm_prose))) {
      # Accept LLM prose only if it is non-empty
      prose[[key]]   <- llm_prose
      sources[[key]] <- "ai"
    } else {
      # Fall back to offline for this section
      prose[[key]]   <- offline_base[[key]]
      sources[[key]] <- "offline"
    }
  }

  # Step 4: Append joint-hypothesis caveat to robustness (NARR-05)
  prose[["robustness"]] <- paste0(prose[["robustness"]], " ", JOINT_HYPOTHESIS_CAVEAT)

  # Step 5: Determine report_mode
  report_mode <- if (any(unlist(sources) == "ai")) "ai" else "offline"

  # Step 6: Return assembled list
  list(
    exec_summary    = prose[["exec_summary"]],
    data_methods    = prose[["data_methods"]],
    results         = prose[["results"]],
    robustness      = prose[["robustness"]],
    section_sources = sources,
    report_mode     = report_mode
  )
}


# ---------------------------------------------------------------------------
# .extract_llm_section_prose() -- extract LLM prose from es_advise result
#
# The LLM path for report_writing returns an Advice S3 whose `interpretation`
# field holds the prose. Returns NULL if the result is empty, failed, or an
# OfflineNarrative (indicating the LLM was not actually used for this call).
# Returning NULL signals to the assembler to fall back to offline for that
# section.
#
# @param advice Result of es_advise() with a provider (Advice S3 or NULL).
# @return A non-empty character scalar on success, or NULL on failure/empty.
# @noRd
# ---------------------------------------------------------------------------

.extract_llm_section_prose <- function(advice) {
  if (is.null(advice)) return(NULL)

  # If advice is an OfflineNarrative, the LLM was not used -> fall back
  if (inherits(advice, "OfflineNarrative")) return(NULL)

  # If advice is an Advice S3 (LLM path), use interpretation field as prose
  if (inherits(advice, "Advice")) {
    interp <- advice$interpretation
    if (!is.null(interp) && is.character(interp) && nzchar(trimws(interp))) {
      return(interp)
    }
    return(NULL)  # Empty Advice (guard dropped or parse failed) -> fall back
  }

  # If advice is a plain character scalar (e.g. from a simplified mock)
  if (is.character(advice) && length(advice) == 1L && nzchar(trimws(advice))) {
    return(advice)
  }

  NULL
}


# ===========================================================================
# Per-format prose sanitiser (FORMAT-04)
# ===========================================================================

# ---------------------------------------------------------------------------
# .sanitise_universal() -- normalise smart-quotes and typographic dashes
#
# Converts Unicode smart-quotes and typographic dashes to their ASCII
# equivalents. Applied before format-specific escaping so that a LaTeX
# sanitiser never sees a raw em-dash byte (which would survive as a LaTeX
# error).
#
# Note: the match patterns use \u Unicode escape sequences so no raw non-ASCII
# bytes appear in the source file (CRAN non-ASCII hygiene).
#
# @param text Character scalar.
# @return Character scalar with smart-quotes/dashes normalised to ASCII.
# @noRd
# ---------------------------------------------------------------------------

.sanitise_universal <- function(text) {
  if (!is.character(text)) return(text)
  # Left double quote  (U+201C) -> straight "
  text <- gsub("\u201c", "\"", text, fixed = TRUE)
  # Right double quote (U+201D) -> straight "
  text <- gsub("\u201d", "\"", text, fixed = TRUE)
  # Left single quote  (U+2018) -> straight '
  text <- gsub("\u2018", "'",  text, fixed = TRUE)
  # Right single quote (U+2019) -> straight '
  text <- gsub("\u2019", "'",  text, fixed = TRUE)
  # Em dash            (U+2014) -> --
  text <- gsub("\u2014", "--", text, fixed = TRUE)
  # En dash            (U+2013) -> -
  text <- gsub("\u2013", "-",  text, fixed = TRUE)
  text
}


# ---------------------------------------------------------------------------
# .sanitise_for_pdf() -- escape LaTeX special characters
#
# Escapes the 10 LaTeX special characters that break pdflatex if emitted raw
# into a results='asis' knitr chunk. Backslash must be replaced FIRST to
# avoid double-escaping the escape sequences introduced for other specials.
#
# Order: \\ { } $ % # _ & ~ ^
#
# @param text Character scalar (after .sanitise_universal has been applied).
# @return Character scalar with LaTeX specials escaped.
# @noRd
# ---------------------------------------------------------------------------

.sanitise_for_pdf <- function(text) {
  if (!is.character(text)) return(text)
  # 1. Replace backslash with a placeholder BEFORE brace-escaping so that the
  #    curly braces introduced in step 10 below (\textbackslash{}) are not
  #    themselves escaped by step 2.  The placeholder must contain no LaTeX
  #    specials (no backslash, braces, $, %, #, _, &, ~, ^).
  #    (CR-01 fix: original code emitted \textbackslash\{\} -- three
  #    rendered characters -- instead of the correct \textbackslash{} -- one.)
  text <- gsub("\\\\", "BSPH7F3A", text, fixed = FALSE)
  # 2. Curly braces (now safe: placeholder has no braces)
  text <- gsub("{", "\\{", text, fixed = TRUE)
  text <- gsub("}", "\\}", text, fixed = TRUE)
  # 3. Dollar sign
  text <- gsub("$", "\\$", text, fixed = TRUE)
  # 4. Percent
  text <- gsub("%", "\\%", text, fixed = TRUE)
  # 5. Hash
  text <- gsub("#", "\\#", text, fixed = TRUE)
  # 6. Underscore
  text <- gsub("_", "\\_", text, fixed = TRUE)
  # 7. Ampersand
  text <- gsub("&", "\\&", text, fixed = TRUE)
  # 8. Tilde
  text <- gsub("~", "\\textasciitilde{}", text, fixed = TRUE)
  # 9. Caret
  text <- gsub("^", "\\textasciicircum{}", text, fixed = TRUE)
  # 10. Restore backslash placeholder as correct LaTeX macro (braces here are
  #     safe because brace-escaping in step 2 is already done)
  text <- gsub("BSPH7F3A", "\\textbackslash{}", text, fixed = TRUE)
  text
}


# ---------------------------------------------------------------------------
# .sanitise_for_word() -- escape XML/HTML entities for Word DOCX
#
# Escapes the 5 XML special characters that corrupt a Word document if
# emitted raw into the underlying XML markup. Ampersand must be replaced
# FIRST to avoid double-encoding the entity sequences introduced for
# other specials.
#
# @param text Character scalar (after .sanitise_universal has been applied).
# @return Character scalar with XML entities substituted.
# @noRd
# ---------------------------------------------------------------------------

.sanitise_for_word <- function(text) {
  if (!is.character(text)) return(text)
  # 1. Ampersand first (avoid double-encoding)
  text <- gsub("&",  "&amp;",  text, fixed = TRUE)
  text <- gsub("<",  "&lt;",   text, fixed = TRUE)
  text <- gsub(">",  "&gt;",   text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'",  "&#39;",  text, fixed = TRUE)
  text
}


# ---------------------------------------------------------------------------
# .sanitise_prose() -- per-format prose sanitiser dispatcher (FORMAT-04)
#
# Applies universal smart-quote/dash normalisation first (all formats), then
# format-specific escaping. Passes through unchanged for unknown formats
# after universal normalisation (safe default).
#
# Security note: LaTeX \input / \write18 injection and Word field-code
# control sequences from LLM prose are rendered inert because:
#   - pdf: the leading backslash is escaped to \textbackslash{}, so
#     \input{/etc/passwd} becomes \textbackslash{}input\{/etc/passwd\}
#   - word: < > & are escaped to entities, neutralising control sequences
#   - html/md: universal normalisation only; pandoc handles further escaping
#
# @param text Character scalar (raw prose from LLM or offline engine).
# @param format Character scalar: "pdf", "word", "html", or "md".
# @return Sanitised character scalar.
# @noRd
# ---------------------------------------------------------------------------

.sanitise_prose <- function(text, format) {
  if (!is.character(text)) return(text)
  text <- .sanitise_universal(text)
  if (identical(format, "pdf"))  text <- .sanitise_for_pdf(text)
  if (identical(format, "word")) text <- .sanitise_for_word(text)
  # html / md: universal normalisation only
  text
}
