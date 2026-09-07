# Phase 17: Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline — Research

**Researched:** 2026-09-07
**Domain:** R package internals — runtime grounding guard, offline advice engine, generate_report() seam, R CMD check hygiene
**Confidence:** HIGH (all claims derived from direct file reads this session)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

**Prose Grounding Scanner (GROUND-01..03)**
- Detect claims by extracting every numeric literal from each free-text narrative field via regex and checking each against the `es_diagnostics()` value registry.
- On a fabricated number: drop the offending section and emit exactly one warning — mirrors the existing drop-and-keep contract; the unverified number is never emitted into rendered output.
- Tolerance model reuses the existing `EventStudy.guard_abs_tol` / `EventStudy.guard_rel_tol` options, plus a rounding-aware match: a literal that is a correct rounding of an actual diagnostic value at the literal's displayed precision counts as grounded (handles "2.35" for 2.3456).
- Exempt structural integers that match metadata already present in diagnostics (N observations, window indices/bounds, event counts); scrutinize only statistical decimals as claims.

**Offline `report_writing` Fallback (OFFLINE-01)**
- Produce offline narrative by extending the existing rule-based offline engine (`advise_offline.R`) to synthesize per-section prose from KB rules + diagnostics.
- Remove `report_writing` from `LLM_ONLY_TYPES` and move it into the KB-grounded / offline-capable set so `provider = NULL` no longer `stop()`s.
- Offline narrative uses the same section keys as the LLM path (exec summary · data/methods · results · robustness/caveats).
- When a provider IS configured but a section fails or is guard-dropped, fall back to offline text per-section so the report is always complete.

**`generate_report()` `narrative=` Seam (REPORT-03)**
- Parameter shape: `narrative = NULL` default, accepting a named list / S3 object keyed by section; the NULL path is byte-identical to the v0.63.x baseline.
- `narrative` and the existing `advice` param are independent seams: `narrative` = section prose, `advice` = structured recommendations block; both independently NULLable.
- Backward compatibility proven by a golden-file diff test locking the `narrative = NULL` / `advice = NULL` output byte-identical to the baseline.
- `generate_report()` only accepts narrative — it stays a pure renderer. The Phase 19 `es_report()` orchestrator produces the narrative and passes it in.

**CRAN Hygiene Discipline (REPORT-03)**
- Wrap all `render()`-touching examples in `\dontrun{}`.
- Every render / PDF / Word / toolchain test uses `skip_on_cran()` + `skip_if_not_installed()`.
- PDF/Word/LaTeX dependencies stay Suggests-only, `requireNamespace()`-guarded (`tinytex` added to Suggests); zero new hard deps.
- Snapshot the current `R CMD check` NOTEs/WARNINGs up front as the diff baseline every downstream phase is measured against.

### Claude's Discretion
(None explicitly listed — all decisions are locked.)

### Deferred Ideas (OUT OF SCOPE)
- Multi-format renderer, fixed template, section-by-section narrative assembler — Phase 18.
- `es_report()` one-call orchestrator, `run_event_study(..., report=TRUE)`, final CRAN release gate — Phase 19.
- officedown rich Word (RPTX-03), custom templates (RPTC-01), panel/intraday/synthetic report support (RPTX-01), bootstrap-CI reporting (RPTX-02).
</user_constraints>

---

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| GROUND-01 | Grounding guard scans narrative prose for numeric literals absent from `es_diagnostics()` (within tolerance), extending coverage beyond structured `evidence[]` arrays | Regex design, value registry construction, rounding-aware match — all in §Architecture Patterns below |
| GROUND-02 | A grounding failure degrades safely — offending section dropped/flagged with one warning; unverified number never emitted silently | Drop-and-keep pattern is already implemented in `.validate_grounding()` at `advise.R:218-321`; prose scanner reuses the same contract |
| GROUND-03 | Regression tests lock the report-path grounding invariant (a fabricated number in prose is caught, not rendered) | testthat 3e patterns in §Validation Architecture |
| OFFLINE-01 | With no LLM provider, `es_report()` still renders a complete report via rule-based offline advice engine | Exact surgery on `LLM_ONLY_TYPES` at `advise.R:26`; offline narrative extension in `advise_offline.R` |
| REPORT-03 | `generate_report()` gains `narrative = NULL` parameter; existing `narrative=NULL`/`advice=NULL` output stays byte-identical | Exact signature change, golden-file test pattern — §Code Examples |
</phase_requirements>

---

## Summary

Phase 17 delivers four surgical changes to existing files — no new R files are required. The ground-truth of each change is in three source files already fully read: `R/advise.R`, `R/advise_offline.R`, and `R/report.R`.

The critical seam is the runtime grounding guard (`.validate_grounding()` at `advise.R:218-321`). It currently validates only structured `evidence[]` arrays. Phase 17 adds a companion function — `.scan_prose_grounding()` — that extracts numeric literals from free-text prose strings and verifies each against the diagnostics value registry using the same tolerance model already wired into `.validate_grounding()`. The prose scanner is called from `es_advise()` after `LLM_ONLY_TYPES` surgery allows `report_writing` to reach the offline path.

The `LLM_ONLY_TYPES` surgery (`advise.R:26`) is one line: remove `"report_writing"` from the vector. The ADV-06 `stop()` block at `advise.R:761-769` guards `task_type %in% LLM_ONLY_TYPES` — after surgery, `report_writing` with `provider=NULL` routes to the KB/offline path instead of stopping. The three remaining LLM-only types (`interpret`, `recommend_model`, `design_discussion`) are unaffected.

`generate_report()` (`report.R:30-127`) gets a single new trailing parameter `narrative = NULL` inserted before `...`. The NULL path is byte-identical: the parameter is never read when NULL (the template params list passes it through, the skeleton's existing eval= guards handle NULL). The golden-file backward-compatibility test captures the rendered bytes before the change and diffs after.

**Primary recommendation:** Implement in this order — (1) prose scanner function, (2) `LLM_ONLY_TYPES` surgery + offline narrative engine extension, (3) `generate_report()` `narrative=` seam, (4) CRAN baseline snapshot. Each is independently testable with no network access.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| Prose grounding scanner | R package (runtime) | — | Pure R function; no I/O; operates on a string + diagnostics object already in memory |
| Offline narrative synthesis | R package (offline engine) | — | KB rules + diagnostics → structured prose; no LLM, no network |
| `generate_report()` narrative seam | R package (renderer) | Report template (skeleton.Rmd) | Renderer passes `narrative` to template via `params`; template eval= guards control rendering |
| CRAN hygiene | R package (examples + tests) | CI (R CMD check) | `\dontrun{}` in roxygen examples; `skip_on_cran()` + `skip_if_not_installed()` in testthat files |

---

## Standard Stack

### Core (no new deps — everything below is already in DESCRIPTION)

| Library | In DESCRIPTION | Purpose in Phase 17 | Why Standard |
|---------|---------------|---------------------|--------------|
| R6 | Imports | `CustomProvider` fixture in tests | Existing class system |
| testthat ≥ 3.0.0 | Suggests | All new tests | Existing test framework |
| rmarkdown | Suggests | `generate_report()` render (skip-guarded in tests) | Already in Suggests |
| knitr | Suggests | Render engine (skip-guarded in tests) | Already in Suggests |
| jsonlite | Suggests | Advice JSON parse (already requireNamespace-guarded) | Already in Suggests |

**New Suggests addition: `tinytex`**
`tinytex` must be added to the `Suggests` field in `DESCRIPTION`. It is not `requireNamespace()`-called in Phase 17 code (no PDF render in this phase) but the CRAN discipline baseline requires it be declared before Phase 18 uses it.

[VERIFIED: DESCRIPTION:63-66] — current Suggests block ends at `DIDmultiplegt`, `didimputation`. `tinytex` is absent and must be added.

**Installation (dev, no new runtime deps):**
```r
# No new runtime deps this phase.
# For test environment:
install.packages("tinytex")   # Suggests only — verified via requireNamespace() before use
```

---

## Package Legitimacy Audit

No new external packages are installed in Phase 17. `tinytex` is added to `Suggests` (existing CRAN package, well-established). No legitimacy gate needed.

| Package | Action | Notes |
|---------|--------|-------|
| tinytex | Add to DESCRIPTION Suggests | Well-established CRAN package; not called in Phase 17 code |

---

## Architecture Patterns

### Key Existing Signatures (read this session — the exact contracts to extend)

**`LLM_ONLY_TYPES` constant** `[VERIFIED: R/advise.R:26]`
```r
LLM_ONLY_TYPES <- c("interpret", "recommend_model", "design_discussion", "report_writing")
```
Phase 17 surgery: remove `"report_writing"` → `c("interpret", "recommend_model", "design_discussion")`.

**`KB_TYPES` constant** `[VERIFIED: R/advise.R:29]`
```r
KB_TYPES <- c("recommend_stat", "flag_robustness")
```
Phase 17 adds `"report_writing"` → `c("recommend_stat", "flag_robustness", "report_writing")`.

**ADV-06 `stop()` block** `[VERIFIED: R/advise.R:761-769]`
```r
if (is.null(provider)) {
  if (task_type %in% LLM_ONLY_TYPES) {
    stop(
      sprintf(
        "es_advise(): task_type '%s' requires a provider. ...",
        task_type
      ),
      call. = FALSE
    )
  }
  # KB types with no provider -> Phase 5 offline path
  if (task_type == "recommend_stat") { ... }
  if (task_type == "flag_robustness") { ... }
}
```
After surgery: add a third `if (task_type == "report_writing")` branch routing to the new offline narrative engine. The `stop()` block is unchanged for the three remaining LLM-only types.

**Grounding guard tolerance model** `[VERIFIED: R/advise.R:218-221]`
```r
.validate_grounding <- function(advice_list, diagnostics,
                                abs_tol = getOption("EventStudy.guard_abs_tol", 1e-6),
                                rel_tol = getOption("EventStudy.guard_rel_tol", 1e-4)) {
```
Prose scanner must use identical defaults: `abs_tol = getOption("EventStudy.guard_abs_tol", 1e-6)`, `rel_tol = getOption("EventStudy.guard_rel_tol", 1e-4)`.

**Tolerance formula** `[VERIFIED: R/advise.R:277]`
```r
tol <- max(abs_tol, rel_tol * abs(actual))
if (abs(reported - actual) > tol) { all_good <- FALSE; break }
```

**`generate_report()` current signature** `[VERIFIED: R/report.R:30-42]`
```r
generate_report <- function(task,
                              output_file = "event_study_report.html",
                              format = c("html", "pdf"),
                              title = "Event Study Report",
                              author = NULL,
                              sections = c("summary", "data", "diagnostics",
                                           "single_event", "multi_event",
                                           "cross_sectional", "appendix"),
                              cross_sectional = NULL,
                              confidence_level = 0.95,
                              interactive = TRUE,
                              advice = NULL,
                              ...) {
```
Phase 17 adds `narrative = NULL` between `advice = NULL` and `...`.

**`generate_report()` advice validation pattern** `[VERIFIED: R/report.R:94-101]`
```r
if (!is.null(advice) && !inherits(advice, "Advice")) {
  warning(
    "generate_report(): 'advice' is not an Advice object — advice section will be skipped.",
    call. = FALSE
  )
  advice <- NULL
}
```
`narrative=` validation follows the identical pattern: check class of narrative when non-NULL, degrade to NULL with one warning if wrong type.

**`generate_report()` params passthrough** `[VERIFIED: R/report.R:104-122]`
```r
rmarkdown::render(
  input = template_path,
  ...
  params = list(
    task = task,
    title = title,
    author = author %||% "",
    sections = sections,
    cross_sectional = cross_sectional,
    confidence_level = confidence_level,
    interactive = interactive,
    advice = advice
  ),
  ...
)
```
Phase 17 adds `narrative = narrative` to this params list. The skeleton.Rmd params block must declare `narrative: NULL`. When `narrative` is NULL, the template eval= guard suppresses the narrative section — byte-identical to baseline.

**`es_advice` S3 shape from offline engine** `[VERIFIED: R/advise_offline.R:193-231]`
```r
structure(
  list(
    source          = "offline_kb",
    is_deterministic = TRUE,
    rules_matched   = matched,
    diagnostics_ref = diag
  ),
  class = "es_advice"
)
```
The offline `report_writing` path needs a different (or extended) return shape — it must produce narrative prose sections, not just matched rules. Two options (planner decides):
- Return a new S3 class `"OfflineNarrative"` — a named list of section keys → prose strings.
- Extend `es_advice` with an additional `narrative` field (list of section → prose).

The Phase 18 renderer is agnostic about source as long as the section keys are consistent. Option A (new S3) is cleaner and easier to test in isolation.

**`.build_offline_advice()` engine** `[VERIFIED: R/advise_offline.R:193-231]`
The engine evaluates `rule$condition(diag)` for each rule and collects matched rules. The new `report_writing` offline path calls this logic for both `stat_choice` and `robustness` rules, then synthesizes per-section prose from the matched rules' `recommendation` and `citation` fields.

### Diagnostics Value Registry — What's Available for Prose Scanning

`[VERIFIED: R/es_diagnostics.R:83-97]` — the six sections and their keys:

```
meta:
  n_events_total, n_events_shown, n_events_summarized, event_ids_shown

estimation_window (vectors, length n_events_shown):
  r2, sigma, degree_of_freedom, acf1, shapiro_p, dw_stat, ljung_box_p

event_window (vectors, length n_events_shown):
  ar_t, ar_p, car_t, car_p, final_car

cross_sectional (scalars):
  n_events, n_valid_events, car_iqr, car_sd, n_overlap_pairs, any_overlap

contract_state (vectors):
  is_fitted, na_ar_count, na_est_count, insufficient_obs, zero_var_index

aggregate_summary (scalars, NULL when no truncation):
  n_summarized, mean_r2, median_r2, mean_final_car, n_fitted, n_degenerate
```

**Value registry construction for the prose scanner:** Flatten the diagnostics object into a single numeric vector — one entry per scalar value across all sections. Vector-valued keys (estimation_window, event_window, contract_state) are summarised to their `mean(na.rm=TRUE)` — matching the existing guard's summarization logic at `advise.R:258-260`. Structural integers (`meta$n_events_total`, `meta$n_events_shown`, `cross_sectional$n_events`, `cross_sectional$n_valid_events`) go into a separate "structural integers" set that is exempt from the prose scan.

### Pattern 1: Prose Scanner (`.scan_prose_grounding()`)

**What:** Extract all numeric literals from a character string and verify each is either (a) in the diagnostics value registry within tolerance, or (b) a correct rounding of a registry value at the literal's displayed decimal precision, or (c) in the structural-integers exempt set.

**When to use:** Called from `es_advise()` after LLM returns narrative prose for `report_writing`, and from any phase-18 section assembler that produces free-text containing numbers.

**Numeric literal regex (R):** [ASSUMED — validated against R regex engine conventions, not externally cited]
```r
# Matches: -3.14  0.001  2.35e-4  95%  1,234.56  <0.001  >0.05
# Captures the numeric part only (strips % and comparison operators)
.extract_numeric_literals <- function(text) {
  # Remove common non-numeric prefixes (<, >, ~, ≈) and suffixes (%, points, pp)
  # Pattern: optional sign, digits with optional decimal and/or scientific notation
  pattern <- "-?(?:\\d{1,3}(?:,\\d{3})*|\\d+)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?"
  m <- gregexpr(pattern, text, perl = TRUE)
  raw <- regmatches(text, m)[[1L]]
  # Strip thousands separators before as.numeric
  raw <- gsub(",", "", raw)
  vals <- suppressWarnings(as.numeric(raw))
  vals[!is.na(vals)]
}
```

**Rounding-aware match:** [ASSUMED — derived from display-precision logic; no external citation]
```r
.is_grounded_literal <- function(literal, registry_vals, structural_ints,
                                  abs_tol, rel_tol) {
  # 1. Structural integer exempt?
  if (literal == round(literal) && as.integer(literal) %in% structural_ints) return(TRUE)
  # 2. Direct tolerance match against any registry value?
  for (v in registry_vals) {
    if (!is.finite(v) || !is.finite(literal)) next
    tol <- max(abs_tol, rel_tol * abs(v))
    if (abs(literal - v) <= tol) return(TRUE)
  }
  # 3. Rounding-aware: is literal a correctly rounded form of any registry value?
  # Determine displayed decimal precision from the literal's string form
  dec_places <- nchar(sub(".*\\.", "", format(literal, scientific = FALSE)))
  for (v in registry_vals) {
    if (!is.finite(v)) next
    if (abs(literal - round(v, dec_places)) <= abs_tol) return(TRUE)
  }
  FALSE
}
```

**False-positive risks:**
- **Years** (e.g., "2023", "1997"): years appear in citations and are NOT in diagnostics. The structural-integers exempt set must NOT include years — years should be handled by a minimum-value threshold (exempt integers < 1000 are counts; integers ≥ 1900 and ≤ 2100 are candidate years → also exempt). [ASSUMED]
- **p-values written as "<0.001"**: the `<` prefix makes the literal 0.001. This is a valid diagnostic-range value and should match against `shapiro_p`, `ar_p`, `car_p` etc. No special handling needed — the stripped literal is 0.001 and the tolerance check handles it.
- **Ranges** ("between 0.3 and 0.7"): each bound is extracted separately and each is individually checked. A fabricated range generates two literals, both of which must be grounded. This is correct behavior.
- **Percentages** ("5% significance"): "5" is extracted. This is a structural constant (the significance level), not a computed diagnostic. Recommend adding `0.05`, `0.01`, `0.10`, `0.001` to a small allowlist of universal statistical constants that are always exempt. [ASSUMED]

**Section-level drop contract:** When `.scan_prose_grounding()` finds any ungrounded literal in a section's prose string, the entire section string is replaced by `""` (empty) and a single warning is emitted. The `n_dropped` count on the parent object increments by 1. This mirrors `.validate_grounding()` dropping an entire recommendation (not just one evidence entry) when any evidence entry fails.

### Pattern 2: Offline Narrative Engine Extension (OFFLINE-01)

**What:** Add a `report_writing` offline path to `advise_offline.R` that produces per-section narrative prose from KB rules + diagnostics.

**Section keys (same as LLM path):** [ASSUMED — derive from CONTEXT.md decisions; not yet present in code]
```
"exec_summary"    — 1-2 sentence findings overview
"data_methods"    — estimation window, model choice, N events
"results"         — AR/CAR significance, CAR magnitude
"robustness"      — caveats, fired KB robustness rules
```

**New function:** `.build_offline_narrative(diag)` in `advise_offline.R`:
```r
.build_offline_narrative <- function(diag) {
  # 1. Run both rule categories
  stat_rules <- Filter(function(r) r$category == "stat_choice", es_kb())
  rob_rules  <- Filter(function(r) r$category == "robustness",  es_kb())
  stat_advice <- .build_offline_advice(diag, stat_rules)
  rob_advice  <- .build_offline_advice(diag, rob_rules)

  # 2. Synthesize prose from matched rules + diagnostics scalars
  exec_summary <- .narrative_exec_summary(diag, stat_advice, rob_advice)
  data_methods <- .narrative_data_methods(diag)
  results_sec  <- .narrative_results(diag)
  robustness   <- .narrative_robustness(rob_advice, diag)

  structure(
    list(
      source          = "offline_kb",
      is_deterministic = TRUE,
      exec_summary    = exec_summary,
      data_methods    = data_methods,
      results         = results_sec,
      robustness      = robustness
    ),
    class = "OfflineNarrative"
  )
}
```

**No-fabrication rule for offline prose:** Each helper (`.narrative_exec_summary()` etc.) must construct sentences exclusively from values in `diag`. No literal numbers may appear in the prose helpers that are not read from `diag` at runtime — the prose scanner will catch them anyway, but defensive coding is the right approach. Use `sprintf()` with `diag$...` values directly.

**Example results prose (defensively grounded):**
```r
.narrative_results <- function(diag) {
  n_valid <- diag$cross_sectional$n_valid_events
  med_car_t <- median(diag$event_window$car_t, na.rm = TRUE)
  sprintf(
    "Across %d fitted events, the median CAR t-statistic was %.3f. [Rule-based offline narrative]",
    n_valid, med_car_t
  )
}
```

### Pattern 3: `generate_report()` `narrative=` Seam (REPORT-03)

**Exact change to `R/report.R`:**

1. Add `narrative = NULL` between `advice = NULL` and `...` in the signature (`report.R:41`).
2. Add validation block after the existing `advice` validation block (`report.R:94-101`):
```r
if (!is.null(narrative) && !is.list(narrative)) {
  warning(
    "generate_report(): 'narrative' must be a named list or NULL — narrative will be skipped.",
    call. = FALSE
  )
  narrative <- NULL
}
```
3. Add `narrative = narrative` to the `params` list in `rmarkdown::render()` call.
4. Add `narrative: NULL` to the skeleton.Rmd `params:` block.
5. Add eval= guard in skeleton.Rmd for the narrative section: `eval = !is.null(params$narrative)`.

**What does NOT change:** The existing `advice` param, its validation, its position in the signature, and its template handling are all untouched. The skeleton.Rmd advice-section chunk (`advice-section`) remains exactly as is.

**Backward-compat test (golden-file approach):**
The golden-file approach for testthat 3e without `expect_snapshot()` (which is for console output, not file bytes):
```r
test_that("narrative=NULL path is byte-identical to baseline", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()

  task     <- create_fitted_mock_task()
  baseline <- tempfile(fileext = ".html")
  new_out  <- tempfile(fileext = ".html")

  # Baseline: no narrative param (pre-Phase-17 call shape)
  generate_report(task, output_file = baseline, format = "html",
                  sections = c("summary", "appendix"))
  # New: explicit narrative = NULL
  generate_report(task, output_file = new_out, format = "html",
                  sections = c("summary", "appendix"), narrative = NULL)

  # Compare byte-by-byte (strip timestamps if rmarkdown embeds them)
  base_lines <- readLines(baseline, warn = FALSE)
  new_lines  <- readLines(new_out, warn = FALSE)
  # Exclude any lines with ISO timestamps (rmarkdown may embed date)
  strip_ts <- function(x) x[!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T", x)]
  expect_identical(strip_ts(base_lines), strip_ts(new_lines))

  unlink(c(baseline, new_out))
})
```

### Pattern 4: CRAN Hygiene — `\dontrun{}` vs `\donttest{}`

[VERIFIED: R/report.R:30 shows existing `generate_report()` has no `\examples{}` block currently — examples are absent, so no `\dontrun{}` migration is needed for this function in Phase 17]

[VERIFIED: R/advise.R:725] — `es_advise()` already uses `\dontrun{}`:
```r
#' \dontrun{
#' task    <- run_event_study(my_task, ParameterSet$new())
```

**`\dontrun{}` vs `\donttest{}` semantics under `R CMD check --as-cran`:** [ASSUMED — based on R documentation conventions; not verified against current R-devel docs this session]
- `\dontrun{}`: code is NOT run during `R CMD check` at all (not even `--run-dontrun`); use for examples requiring network, API keys, or render toolchains.
- `\donttest{}`: code is skipped during standard `R CMD check` but IS run with `--run-donttest`; use for slow but locally-runnable examples.
- **Rule for this phase:** All `generate_report()`, `es_advise()` with provider, and any narrative-rendering examples → `\dontrun{}`. Pure offline examples (no render, no provider) → no guard needed.

**testthat patterns for render tests:** [VERIFIED: R/tests/testthat/test_report.R:18-19]
```r
skip_if_not_installed("rmarkdown")
skip_if_not_installed("knitr")
skip_on_cran()
```
All three guards together are the pattern for any test that calls `rmarkdown::render()`. Tests that only inspect formals or file structure (like the skeleton.Rmd structural assertion at `test_report_advice.R:131-165`) need no skip guards — they are fast and offline.

**CRAN baseline snapshot:** Run `R CMD check --as-cran` and redirect output to a committed file:
```bash
R CMD build . --no-build-vignettes && \
R CMD check --as-cran --no-vignettes EventStudy_*.tar.gz 2>&1 | \
  grep -E "^(NOTE|WARNING|ERROR|Status)" > .planning/phases/17-grounding-prose-hardening-offline-report-fallback-cran-basel/cran-check-baseline.txt
```
Commit `cran-check-baseline.txt` so Phase 18 and 19 can diff against it.

### Recommended File-Change Map

```
R/advise.R          — Line 26: remove "report_writing" from LLM_ONLY_TYPES
                    — Line 29: add "report_writing" to KB_TYPES
                    — Lines 761-778: add report_writing branch in no-provider routing
                    — New internal: .scan_prose_grounding() + .extract_numeric_literals()
                      + .build_prose_value_registry()
                    — es_advise(): call .scan_prose_grounding() on narrative prose
                      after .build_advice_from_parsed()

R/advise_offline.R  — New internal: .build_offline_narrative(), .narrative_exec_summary(),
                      .narrative_data_methods(), .narrative_results(), .narrative_robustness()
                    — New export (or internal): offline_report_writing() function
                      (called from the new report_writing branch in es_advise())

R/report.R          — Line 41: add narrative = NULL before ...
                    — Lines 94-101 area: add narrative validation block
                    — Lines 104-122: add narrative = narrative to params list
                    — Roxygen: document narrative= param

inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd
                    — params block: add narrative: NULL
                    — Add narrative-section chunk with eval = !is.null(params$narrative)

DESCRIPTION         — Suggests block: add tinytex

tests/testthat/test_prose_grounding.R   — NEW: GROUND-01/02/03 regression tests
tests/testthat/test_offline_narrative.R — NEW: OFFLINE-01 tests
tests/testthat/test_report_narrative.R  — NEW: REPORT-03 backward-compat + seam tests
```

### Anti-Patterns to Avoid

- **Scanning structured `evidence[]` values from prose:** The evidence array is already validated by `.validate_grounding()`. The prose scanner must target only the free-text string fields (`interpretation`, `narrative prose`, per-section text) — not the JSON-structured fields.
- **Emitting more than one warning per prose scan:** The prose scanner must aggregate all ungrounded literals in a section into one `warning()` call, just as `.validate_grounding()` emits one warning for all dropped recommendations.
- **Modifying the `stop()` error message for remaining LLM-only types:** The existing message (`advise.R:763-768`) is tested by `test_advise.R`. Do not change it — only the routing vector changes.
- **Adding `narrative=` before `advice=` in the signature:** The existing backward-compat tests at `test_report_advice.R:17-46` check that `advice` appears after `interactive` and before `...`. `narrative` must also follow this order: `..., advice = NULL, narrative = NULL, ...` or `..., narrative = NULL, advice = NULL, ...`. Either is valid — but the test at line 29-35 only checks `advice`'s position, so placing `narrative` after `advice` and before `...` is safest.
- **Changing the `advice` validation warning message:** The message "not an Advice object" is tested by exact regexp at `test_report_advice.R:72`. Do not alter it.

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Numeric extraction from strings | Custom parser | Base R `gregexpr()` + `regmatches()` + `as.numeric()` | Zero deps; handles scientific notation, negatives; already used throughout the codebase |
| Tolerance comparison | Custom epsilon logic | Reuse `.validate_grounding()`'s formula verbatim: `max(abs_tol, rel_tol * abs(actual))` | Options already wired; single source of truth for tolerance |
| Test fixtures for diagnostics | New mock creation | Reuse `.make_test_diag()` from `tests/testthat/helper-advice-fixtures.R` (already used in `test_advise.R:63`) | Consistent fixture; avoids fixture drift |
| Provider stub for tests | New mock provider class | Reuse `CustomProvider$new(function(prompt, schema) canned_json)` pattern from `test_advise.R:64` | Already tested and working |
| Snapshot comparison for HTML output | Custom diff tool | `readLines()` + `expect_identical()` after stripping timestamps | testthat 3e native; no extra deps |

---

## Common Pitfalls

### Pitfall 1: Prose Scanner Fires on Non-Prose Fields
**What goes wrong:** The scanner is applied to the entire `Advice` S3 object, hitting `n_dropped` (an integer) or `is_deterministic` (logical) and extracting "0", "1" as literals.
**Why it happens:** The scanner function receives the whole object rather than only the free-text strings.
**How to avoid:** Define the set of fields to scan explicitly: for a `report_writing` Advice, scan only `$interpretation` and each element of `$recommendations$rationale` and `$recommendations$expected_effect`. Never scan `$n_dropped`, `$source`, evidence numeric values, or structural fields.
**Warning signs:** Test emitting spurious "Grounding guard" warnings on a correctly grounded Advice.

### Pitfall 2: Rounding-Aware Match Accepts Years and Statistical Constants
**What goes wrong:** The literal "1997" (a citation year in prose) or "0.05" (significance level) is checked against diagnostics and — if a registry value happens to round to 1997 or 0.05 at the right precision — is wrongly declared grounded (or, if no match, wrongly dropped).
**Why it happens:** The exempt sets for years and universal constants are not defined.
**How to avoid:** Before running the tolerance check, test: (a) integer ≥ 1900 and ≤ 2100 → year exempt; (b) member of `c(0.001, 0.01, 0.05, 0.10)` → significance-level exempt; (c) integer in structural-integers set → exempt. Apply these tests in order, before the registry search.
**Warning signs:** A test with prose containing "MacKinlay (1997)" raises a grounding warning.

### Pitfall 3: `report_writing` Offline Branch Returns Wrong S3 Class
**What goes wrong:** The new `report_writing` offline branch returns an `es_advice` object (the existing `recommend_stat`/`flag_robustness` shape), but the Phase 18 renderer expects a named-list keyed by section.
**Why it happens:** Reusing `.build_offline_advice()` directly without wrapping into the new `OfflineNarrative` shape.
**How to avoid:** Wrap or extend: call `.build_offline_advice()` internally for rule matching, but return a new `structure(..., class = "OfflineNarrative")` with `exec_summary`, `data_methods`, `results`, `robustness` as named character fields.
**Warning signs:** Phase 18 code calling `narrative$exec_summary` returns `NULL`.

### Pitfall 4: Skeleton.Rmd `params` Block Not Updated
**What goes wrong:** `generate_report()` passes `narrative = narrative` in `params`, but skeleton.Rmd has no `narrative: NULL` in its YAML `params:` block, causing `rmarkdown::render()` to error with "unknown parameter".
**Why it happens:** The params block in the skeleton must be updated to match any new parameter.
**How to avoid:** Add `narrative: NULL` to the YAML params block in `skeleton.Rmd` at the same time as the `R/report.R` change. Test together.
**Warning signs:** `rmarkdown::render()` errors with "unknown parameter 'narrative'" when `narrative` is not NULL.

### Pitfall 5: Golden-File Test Nondeterminism
**What goes wrong:** The byte-identical comparison fails because rmarkdown embeds a render timestamp or random element in the output.
**Why it happens:** HTML output from rmarkdown may include `<meta name="date" content="...">` or similar.
**How to avoid:** Use `readLines()` and filter out known variable lines before `expect_identical()`. Alternatively, compare only the content between specific HTML markers (e.g., the `<body>` contents) rather than the full file.
**Warning signs:** The golden-file test passes locally but fails in CI (different timestamp format).

---

## Code Examples

### GROUND-01: Building the diagnostics value registry
```r
# Source: R/es_diagnostics.R — section structure verified this session
.build_prose_value_registry <- function(diag) {
  # Structural integers: exempt from prose scanning
  structural_ints <- c(
    diag$meta$n_events_total,
    diag$meta$n_events_shown,
    diag$meta$n_events_summarized,
    diag$cross_sectional$n_events,
    diag$cross_sectional$n_valid_events,
    diag$cross_sectional$n_overlap_pairs
  )
  structural_ints <- as.integer(na.omit(structural_ints))

  # Scalar registry: all numeric values from diagnostics (vectors summarised to mean)
  registry <- c(
    # estimation_window vectors -> summarise to mean
    mean(diag$estimation_window$r2, na.rm = TRUE),
    mean(diag$estimation_window$sigma, na.rm = TRUE),
    mean(diag$estimation_window$shapiro_p, na.rm = TRUE),
    mean(diag$estimation_window$dw_stat, na.rm = TRUE),
    mean(diag$estimation_window$ljung_box_p, na.rm = TRUE),
    mean(diag$estimation_window$acf1, na.rm = TRUE),
    # event_window vectors -> summarise to mean
    mean(diag$event_window$ar_t, na.rm = TRUE),
    mean(diag$event_window$ar_p, na.rm = TRUE),
    mean(diag$event_window$car_t, na.rm = TRUE),
    mean(diag$event_window$car_p, na.rm = TRUE),
    mean(diag$event_window$final_car, na.rm = TRUE),
    # cross_sectional scalars -> direct
    diag$cross_sectional$car_iqr,
    diag$cross_sectional$car_sd
  )
  registry <- registry[is.finite(registry)]

  list(scalars = registry, structural_ints = structural_ints)
}
```

### GROUND-01: Prose scanner function
```r
# Internal — call after LLM returns report_writing prose
.scan_prose_grounding <- function(prose_fields, diag,
                                   abs_tol = getOption("EventStudy.guard_abs_tol", 1e-6),
                                   rel_tol = getOption("EventStudy.guard_rel_tol", 1e-4)) {
  registry <- .build_prose_value_registry(diag)
  sections_kept <- list()
  n_drop <- 0L

  for (nm in names(prose_fields)) {
    text <- prose_fields[[nm]] %||% ""
    if (!nzchar(text)) { sections_kept[[nm]] <- text; next }

    literals <- .extract_numeric_literals(text)

    all_grounded <- all(vapply(literals, function(lit) {
      .is_grounded_literal(lit, registry$scalars, registry$structural_ints,
                           abs_tol, rel_tol)
    }, logical(1L)))

    if (all_grounded) {
      sections_kept[[nm]] <- text
    } else {
      sections_kept[[nm]] <- ""
      n_drop <- n_drop + 1L
    }
  }

  if (n_drop > 0L) {
    warning(
      sprintf(
        "Prose grounding guard: %d section(s) dropped — narrative contained numeric literal(s) absent from computed diagnostics.",
        n_drop
      ),
      call. = FALSE
    )
  }
  list(sections = sections_kept, n_dropped = n_drop)
}
```

### OFFLINE-01: New routing branch in `es_advise()` no-provider block
```r
# Insert after the flag_robustness branch at advise.R:776
if (task_type == "report_writing") {
  return(.build_offline_narrative(diagnostics))
}
```

### REPORT-03: `narrative=` seam in `generate_report()`
```r
# Updated signature (report.R:30-42 area):
generate_report <- function(task,
                              output_file = "event_study_report.html",
                              format = c("html", "pdf"),
                              title = "Event Study Report",
                              author = NULL,
                              sections = c("summary", "data", "diagnostics",
                                           "single_event", "multi_event",
                                           "cross_sectional", "appendix"),
                              cross_sectional = NULL,
                              confidence_level = 0.95,
                              interactive = TRUE,
                              advice = NULL,
                              narrative = NULL,   # NEW — Phase 17
                              ...) {
```

### CRAN Hygiene: test pattern for render tests
```r
# All render-touching tests must have all three guards:
test_that("narrative section renders when narrative is supplied", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_cran()
  # ... test body
})
```

---

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | testthat 3.0.0+ |
| Config file | `tests/testthat.R` |
| Quick run command | `Rscript -e "testthat::test_file('tests/testthat/test_prose_grounding.R')"` |
| Full suite command | `R CMD check --no-vignettes --no-manual .` |

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|-------------|
| GROUND-01 | Numeric literal extraction from prose | unit | `testthat::test_file('tests/testthat/test_prose_grounding.R')` | ❌ Wave 0 |
| GROUND-01 | Registry construction from es_diagnostics | unit | same file | ❌ Wave 0 |
| GROUND-02 | Fabricated literal → section dropped, one warning | unit | same file | ❌ Wave 0 |
| GROUND-02 | Rounding-aware match: "2.35" for 2.3456 → kept | unit | same file | ❌ Wave 0 |
| GROUND-02 | Year "1997" → exempt (no false positive) | unit | same file | ❌ Wave 0 |
| GROUND-03 | report_writing prose with fabricated number is caught | regression | `testthat::test_file('tests/testthat/test_prose_grounding.R')` | ❌ Wave 0 |
| OFFLINE-01 | provider=NULL + task_type="report_writing" → no stop() | unit | `testthat::test_file('tests/testthat/test_offline_narrative.R')` | ❌ Wave 0 |
| OFFLINE-01 | Returned OfflineNarrative has all 4 section keys | unit | same file | ❌ Wave 0 |
| OFFLINE-01 | Section prose is non-empty when KB rules fire | unit | same file | ❌ Wave 0 |
| OFFLINE-01 | Remaining LLM-only types still stop() without provider | regression | same file | ❌ Wave 0 |
| REPORT-03 | `narrative` formal exists, defaults NULL, after `advice` | unit (formals) | `testthat::test_file('tests/testthat/test_report_narrative.R')` | ❌ Wave 0 |
| REPORT-03 | narrative=NULL path byte-identical to baseline | golden-file | same file (skip_on_cran) | ❌ Wave 0 |
| REPORT-03 | narrative= and advice= are independent seams | unit | same file | ❌ Wave 0 |

### Sampling Rate
- **Per task commit:** `Rscript -e "testthat::test_file('tests/testthat/<test_file>.R')"`
- **Per wave merge:** `R CMD check --no-vignettes --no-manual .` (full suite green)
- **Phase gate:** Full suite green + CRAN baseline diff shows no new NOTEs/WARNINGs

### Wave 0 Gaps
- [ ] `tests/testthat/test_prose_grounding.R` — covers GROUND-01/02/03
- [ ] `tests/testthat/test_offline_narrative.R` — covers OFFLINE-01
- [ ] `tests/testthat/test_report_narrative.R` — covers REPORT-03

---

## Security Domain

Security enforcement: not applicable to this phase. Phase 17 changes are purely internal R function logic (no I/O, no network, no auth). The grounding guard is a correctness control, not a security control.

---

## Environment Availability

| Dependency | Required By | Available | Notes |
|------------|------------|-----------|-------|
| R ≥ 4.1.0 | All | ✓ (runtime) | — |
| rmarkdown | golden-file test (skip-guarded) | In Suggests | Tests skip if absent |
| knitr | golden-file test (skip-guarded) | In Suggests | Tests skip if absent |
| jsonlite | Advice JSON parse | In Suggests | requireNamespace-guarded |
| tinytex | Future PDF render (Phase 18) | Not required yet | Add to DESCRIPTION Suggests now |
| R CMD check | CRAN baseline capture | ✓ (dev toolchain) | Run manually once |

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | Numeric literal regex pattern (including thousands separators, scientific notation, percentages) correctly covers real LLM output samples | Architecture Patterns §Pattern 1 | False negatives (fabricated number not extracted) or false positives (non-number extracted); spike test before locking |
| A2 | Year-detection exempt rule (integer ≥ 1900 ≤ 2100) correctly covers all citation years without false-positiving on valid event counts | Architecture Patterns §Pattern 1 Pitfalls | Could exempt large event counts near 1900-2100 range (very unlikely for n_events; guard with max(n_events_total) sanity check) |
| A3 | Universal statistical constants exempt set (`c(0.001, 0.01, 0.05, 0.10)`) is sufficient | Architecture Patterns §Pattern 1 Pitfalls | A fabricated "0.05" passes the guard — low risk since 0.05 is always a threshold, never a computed result |
| A4 | `\dontrun{}` vs `\donttest{}` semantics described are current for R-devel | CRAN Hygiene | Could differ from current R CMD check behavior; verify with `R CMD check --help` |
| A5 | `narrative=` must appear after `advice=` and before `...` in generate_report() signature | Code Examples §Pattern 3 | If placed before `advice=`, existing positional-arg callers who pass advice as arg 11 are unaffected (advice is passed by name, always); risk is low but test the formals order |
| A6 | Section keys for offline narrative ("exec_summary", "data_methods", "results", "robustness") match the Phase 18 renderer's expected keys | Architecture Patterns §Pattern 2 | Phase 18 cannot consume narrative sections; requires coordination with Phase 18 planning |
| A7 | `OfflineNarrative` as a new S3 class (vs extending `es_advice`) is the right abstraction | Architecture Patterns §Pattern 2 | If Phase 18 wants to consume both types uniformly, a single class with optional `narrative` field may be cleaner — deferred decision for Phase 18 planner |

---

## Open Questions

1. **Prose scanner scope: per-section or per-Advice?**
   - What we know: `es_advise()` returns one `Advice` object with `$interpretation` (one string) and per-recommendation `$rationale` / `$expected_effect` strings.
   - What's unclear: For `report_writing`, are there 4 separate `es_advise()` calls (one per section) or one call returning all sections? Phase 18 decides this. Phase 17 must build the scanner to handle either shape.
   - Recommendation: Build `.scan_prose_grounding()` to accept a named `list(section_name = prose_string)` — this works for both single-section and multi-section inputs.

2. **OfflineNarrative vs es_advice extension**
   - What we know: `.build_offline_advice()` returns `es_advice` S3; narrative synthesis is structurally different (prose strings, not rule records).
   - What's unclear: Phase 18 may want a unified type that can be `print()`-ed and `inherits()`-checked uniformly.
   - Recommendation: New `OfflineNarrative` S3 for Phase 17; Phase 18 planner can unify if needed. Flag as assumption A7.

---

## Sources

### Primary (HIGH confidence — read directly this session)
- `R/advise.R:1-862` — complete grounding guard implementation, `LLM_ONLY_TYPES`, tolerance model
- `R/advise_offline.R:1-231` — offline KB engine, `es_advice` S3 shape, `.build_offline_advice()`
- `R/es_diagnostics.R:1-98` — all six diagnostic sections and their keys
- `R/report.R:1-127` — `generate_report()` complete signature and render call
- `tests/testthat/test_report_advice.R:1-235` — existing backward-compat test patterns
- `tests/testthat/test_advise.R:1-139` — existing guard regression patterns
- `tests/testthat/test_report.R:1-100` — existing render test patterns with skip guards
- `DESCRIPTION:1-66` — current Suggests block, confirming `tinytex` is absent

### Secondary (MEDIUM confidence)
- CONTEXT.md — locked decisions for all four work areas
- REQUIREMENTS.md — formal requirement definitions for GROUND-01/02/03, OFFLINE-01, REPORT-03

### Tertiary (LOW confidence)
- `\dontrun{}` vs `\donttest{}` semantics (A4 — based on R documentation conventions)
- Numeric regex design for LLM prose (A1 — derived from general R regex knowledge)

---

## Metadata

**Confidence breakdown:**
- Existing code seams: HIGH — all signatures, line numbers, and behavior verified by direct Read this session
- Prose scanner design: MEDIUM — regex and rounding logic is sound but unvalidated against real LLM samples (spike required per STATE.md pending todos)
- Section key names for offline narrative: LOW — not yet present in code; coordinated assumption with Phase 18

**Research date:** 2026-09-07
**Valid until:** 2026-10-07 (stable codebase, no external deps changing)
