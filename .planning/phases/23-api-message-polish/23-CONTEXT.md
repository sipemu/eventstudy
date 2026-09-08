# Phase 23: API & Message Polish - Context

**Gathered:** 2026-09-09
**Status:** Ready for planning

<domain>
## Phase Boundary

This phase makes EventStudy's *surface feel* consistent and scriptable without changing what valid inputs produce. It delivers four polish surfaces over the existing package code:

1. **Print/format contract** — every `print.*` S3 method returns `invisible(x)` with consistent formatting, and a `format.*` method is added wherever a class has `print()` but no `format()`. Snapshot tests are established *before* any change so byte-identical output is proven.
2. **Classed conditions** — selected high-value `stop()`/`warning()` sites migrate to classed `rlang::abort()`/`rlang::warn()` (rlang is already an Import; NO `cli`, NO new dependency), each naming the offending argument and its value.
3. **Message quality** — a consistent convention for naming the offending argument + value across the migrated call sites.
4. **`verbose=` quiet mode + deprecation audit** — a `verbose=` flag that quiets informational chatter with a default byte-identical to current behavior; plus a deprecation audit that is a documented verified-no-op (NO `lifecycle` dependency) unless a genuine rename surfaces, in which case a back-compat shim + warning ships.

**Hard boundary:** This is polish, not a redesign. Valid-input behavior and default console/message output are byte-identical (snapshot-locked). The exactly-one-warning degenerate-input discipline (`.handle_degenerate` in `contract.R`, the `.validate_grounding` guard-drop in `advise.R`) is preserved untouched. No new `R CMD check` findings; existing 2300+ tests stay green.

</domain>

<decisions>
## Implementation Decisions

### Area 1: Print / Format Contract
- **`format.*` coverage**: Add a `format.*` method for each of the six classes that currently have `print()` but no `format()` — `Advice`, `EventStudySummary`, `es_advice`, `es_cross_sectional`, `es_diagnostics`, `es_simulation`. Each `print.*` method is refactored to `cat(format(x), sep = "\n"); invisible(x)` so print and format share one rendering path and cannot drift. (Recommended over adding `format.*` selectively — uniform coverage is the success-criterion.)
- **`format()` return type**: `format.*` returns a **character vector** (one element per output line), the idiomatic R contract (`base::format` returns character; `print` does the `cat`). Not a single newline-joined string.
- **`print.*` return value**: Uniformly `invisible(x)` — return the object, not the formatted string, matching the existing `print.es_diagnostics`/`print.es_advice`/`print.es_cross_sectional` convention. Audit `print.EventStudySummary` (task.R:313) and `print.Advice` (advise.R:848) / `print.es_simulation` (simulation.R:146) to confirm each ends in `invisible(x)`.
- **Console width / formatting**: Preserve current formatting exactly — no reflow, no `getOption("width")` wrapping newly introduced, no rounding changes. The refactor is a pure extract-render-to-`format`; snapshot tests written FIRST lock the current bytes, then the refactor must reproduce them.

### Area 2: Classed Conditions (scope + class scheme)
- **Scope**: Selective migration of **high-value user-facing validation sites**, NOT a wholesale rewrite. Target the argument-validation `stop()`s in the public entry surface (e.g. `task.R` request/column checks, `models.R` required-column and formula checks, export/report/cross-sectional argument guards). Leave hot-path / deep-internal / read-only-active-binding `stop("... is read only")` sites and the degenerate-contract path on base `stop()`/`warning()`.
- **Class naming scheme**: `eventstudy_error_<kind>` for errors and `eventstudy_warning_<kind>` for warnings, where `<kind>` is a short snake_case tag (e.g. `eventstudy_error_missing_column`, `eventstudy_error_bad_argument`, `eventstudy_error_not_fitted`). Every classed condition also carries the generic parent class `eventstudy_error` / `eventstudy_warning` so callers can catch broadly. Passed via `rlang::abort(msg, class = c("eventstudy_error_<kind>", "eventstudy_error"))`.
- **What stays base**: The exactly-one-warning invariants (`.handle_degenerate` at contract.R:87 and every `warning()` in `.validate_grounding` / the grounding guard in advise.R) stay **exactly as-is** — plain `warning(msg, call. = FALSE)`, untouched. Simple internal `stop()` in tight loops and R6 active-binding read-only guards also stay base (low value, high churn risk).
- **Message parity**: When a base `stop()` becomes `rlang::abort()`, the *rendered message text* stays as close as practical to current wording (extended only to add arg+value where missing). Snapshot/expect_error tests assert both the class and the message substring so the migration is provably additive.

### Area 3: Message Quality Convention
- **Naming convention**: Errors/warnings name the offending argument in backticks and show its offending value, e.g. `` "`model_name` must be a formula, not a string (got \"market\")." `` or `` "Request file missing required columns: `event_date`, `event_window_start`." `` Argument names in backticks; string values in double-quotes; column lists comma-separated in backticks.
- **Value truncation**: Long or vector values are truncated for readability — show up to the first few offending elements then `…` (e.g. first 5 of a long missing-column vector), so a pathological input cannot produce a multi-KB error string.
- **Consistency**: Apply the convention uniformly across the migrated sites only (Area 2 scope). Non-migrated base `stop()`/`warning()` sites are left as-is this phase to keep the diff bounded and the default output byte-identical where not explicitly targeted.
- **No `sprintf` locale traps**: Build messages with `paste0()`/`rlang` glue-in-message style consistent with the existing codebase (which uses `stop("...", var, "...")` concatenation), not `gettextf`, to avoid changing rendered bytes.

### Area 4: verbose= Quiet Mode + Deprecation Audit
- **`verbose=` default**: Default value is chosen so the **default output is byte-identical to today** — i.e. `verbose = TRUE` (or `getOption("eventstudy.verbose", TRUE)`) wherever current behavior emits the informational `message()`/chatter. Setting `verbose = FALSE` suppresses only *informational* `message()` output — never warnings, never errors, never the exactly-one degenerate warning.
- **Which functions gain `verbose=`**: The user-facing orchestrators/reporters that currently emit informational `message()` chatter — primarily `es_report()` / `run_event_study(report=…)` / the report + advise + download paths that surface progress or fallback notices. Scope is limited to functions that *today* print informational messages; pure-compute functions do not gain a no-op flag.
- **Suppression mechanism**: Route informational messages through a small internal helper (e.g. `.inform(msg, verbose)`) that emits `message()` only when `verbose` is truthy. Warnings and the grounding/degenerate one-warning paths do NOT go through this helper and are unaffected.
- **Deprecation audit (API-06 / CRAN-06)**: Audit is a **documented verified no-op** — no public function/argument has actually been renamed this milestone, so no shim and explicitly NO `lifecycle` dependency is added. If the audit surfaces a genuine rename, ship a thin back-compat shim (old name → new name) that warns once via a classed `eventstudy_warning_deprecated`; otherwise record the no-op finding in the plan.

### Claude's Discretion
- Exact set of `stop()`/`warning()` sites chosen for migration within the "high-value user-facing validation" scope (bounded by: name arg+value, don't touch one-warning invariants, don't touch read-only active-binding guards).
- Precise `<kind>` tags in the `eventstudy_error_<kind>` scheme.
- Exact truncation cutoff for long values in messages.
- Whether `verbose=` reads a per-call argument, a `getOption()` default, or both — provided the default is byte-identical.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- **Six `print.*` S3 methods, zero `format.*` methods** (confirmed via NAMESPACE `S3method(print,…)` lines 6–11): `print.Advice` (advise.R:848), `print.EventStudySummary` (task.R:313), `print.es_advice` (advise_offline.R:145), `print.es_cross_sectional` (cross_sectional.R:182), `print.es_diagnostics` (es_diagnostics.R:112), `print.es_simulation` (simulation.R:146). `print.es_diagnostics`/`print.es_advice`/`print.es_cross_sectional` already return `invisible(x)` — the pattern to replicate.
- **rlang is already an Import** (`%||%`, `.data` used throughout) — `rlang::abort()`/`rlang::warn()` add zero dependency. No `rlang::abort`/`rlang::warn` currently used anywhere (grep returned empty) — this phase introduces classed conditions for the first time.
- **`.sanitise_prose()` / `.sanitise_universal()` / `.sanitise_for_pdf()` / `.sanitise_for_word()`** (report_narrative.R:270+) — the prose sanitiser the snapshot tests must cover; ampersand-first ordering is LOCKED (v0.64.0 invariant), CSS/table additions already respected it.
- **No snapshot tests exist** (`tests/testthat/_snaps/` absent, no `expect_snapshot` usages) — this phase establishes the first snapshot suite (print methods + prose sanitiser), which becomes the byte-identical guarantee mechanism.

### Established Patterns
- **Existing `stop()` idiom** names context by concatenation, e.g. `task.R:202` `stop("Statistic '", stat_name, "' not found. Available: ", …)`, `models.R:622` `stop(self$model_name, " requires columns: ", …)`, `task.R:273` `stop("Request file missing columns: ", paste(missing_cols, collapse=", "))`. Several already show the offending value — the convention formalises + backticks it and adds the class.
- **`call. = FALSE`** is used on user-facing `stop()`/`warning()` (e.g. read-only active bindings, contract.R:87) — `rlang::abort()` suppresses the call by default, consistent.
- **Highest `stop()`/`warning()` density**: models.R (26), synthetic_control.R (16), panel_event_study.R (16), advise.R (15), task_intraday.R (14), report.R (14), task.R (13). Migration is selective — density ≠ target; the target is *user-facing argument validation*, concentrated in task.R / models.R / export.R / cross_sectional.R / report.R argument guards.

### Integration Points
- **LOCKED one-warning invariants — DO NOT TOUCH**: `.handle_degenerate()` (contract.R:74–87, `warning(msg, call.=FALSE)`) and the `.validate_grounding()` guard-drop warnings (advise.R:159/168/181/302). These carry a "exactly one warning per degenerate event / per grounding drop" contract locked since v0.50.0/v0.64.0.
- **Report render path**: `verbose=` informational suppression must not alter the report's grounding guard, `JOINT_HYPOTHESIS_CAVEAT`, `knitr::is_html_output()` switch, or per-format sanitiser (all v0.64.0/Phase 22 invariants).
- **NAMESPACE**: New `format.*` methods need `@export` + roxygen `S3method(format,<Class>)` entries; regenerate with roxygen2 (RoxygenNote 7.3.3) — no manual NAMESPACE edits.

</code_context>

<specifics>
## Specific Ideas

- **LOCKED decision (from STATE Decisions)**: "API polish uses classed `rlang` conditions (already imported), not `cli`; `verbose=` default is byte-identical; `lifecycle` added only if a real rename appears (else API-06 is a verified no-op)." All four decision areas above conform.
- **Snapshot-first discipline**: Success Criterion 1 explicitly requires snapshot tests "established *before* any change" — the plan must sequence snapshot capture BEFORE the print/format refactor so the refactor is proven byte-identical, not merely asserted.
- **Dependency guardrail (STATE Decisions)**: Explicitly NOT adding `cli` or `lifecycle`. Suggests additions this milestone were `tinytable`/`patchwork`/`ragg` (Phase 21/22) only — Phase 23 adds no dependency.
- **Carry-in tech-debt candidate (from Phase 21 review, noted for Phase 23 guard pass)**: `gridExtra::grid.arrange` used unguarded in R/plotting.R (~L351) + a stale `@return` "patchwork-style" doc. Verify gridExtra is guarded/declared before CRAN submission — in scope only if it touches the message/guard surface; otherwise flag for docs/next phase.

</specifics>

<deferred>
## Deferred Ideas

- **Wholesale error-taxonomy rewrite** — migrating *all* ~200 `stop()`/`warning()` sites to classed conditions. Out of scope; this phase is selective high-value migration only. A full taxonomy could be a future hardening ticket.
- **`cli`-based rich condition formatting** — explicitly rejected (dependency guardrail). Deferred indefinitely.
- **`lifecycle`-managed deprecation lifecycle badges** — deferred unless a genuine rename appears; API-06 is a verified no-op this phase.
- **README count reconciliation** ("13 Return Models"/"11 Test Statistics" stale prose) — belongs to Phase 24 Docs & Site Polish, already tracked there.

</deferred>
