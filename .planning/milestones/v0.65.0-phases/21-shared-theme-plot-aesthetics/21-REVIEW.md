---
phase: 21-shared-theme-plot-aesthetics
reviewed: 2026-09-08T21:57:01Z
depth: standard
files_reviewed: 2
files_reviewed_list:
  - R/theme.R
  - R/plotting.R
findings:
  critical: 0
  warning: 0
  info: 4
  total: 4
status: issues_found
fix_disposition: no_fixes_applied
---

# Phase 21: Code Review Report

**Reviewed:** 2026-09-08T21:57:01Z
**Depth:** standard
**Files Reviewed:** 2
**Status:** issues_found (all findings dispositioned as out-of-scope / cosmetic — no fixes applied)

## Summary

Phase 21 introduced `R/theme.R` (the `es_colours` palette, exported `theme_eventstudy()`, and
internal `.style_plotly()`) and rewired colour literals throughout `R/plotting.R`. The palette
definition is sound — all 12 names exist, all hex values are valid six-digit hex, and the
`@format` length-12 claim matches the actual vector. The ggplot2 theme is well-structured.
Palette name cross-references between `theme.R` and `plotting.R` are consistent: every key
subscripted in `plotting.R` (`primary`, `event`, `reference`, `ci_band`, `group1`) is defined in
`es_colours`, so there is **no NA-colour risk** from a name typo.

The initial pass raised one Critical (CR-01) and two Warnings (WR-01/WR-02). On verification
against the pre-phase baseline (`daf50c7`) and the phase's own design decisions, **all four
findings are either false positives or pre-existing/cosmetic**, and none violates the
aesthetics-only invariant. No fixes were applied. Each is retained below as Info with its
disposition.

The plot-structure invariant holds: no ggplot geom/layer or plotly trace was added, removed, or
reordered; data mappings are unchanged; and the horizontal bottom legend that the pre-phase
`plot_stocks` set via `layout(legend = ...)` is preserved by `.style_plotly()`
(`legend = list(orientation = "h", xanchor = "center", x = 0.5)`).

---

## Info (dispositioned — no fixes applied)

### IN-01 (was CR-01, downgraded — FALSE POSITIVE): `plot_stocks` per-symbol line colour

**File:** `R/plotting.R:74`

**Original claim:** Adding `line = list(color = es_colours["group1"])` pins every trace to the
same blue, making multi-symbol plots indistinguishable.

**Disposition — false positive.** `plot_stocks` builds **one `plot_ly` object per symbol**
(`plots_list[[symbol]] <- plot`) and combines them with `subplot()`. Each panel therefore
contains exactly one line — symbols are shown in separate facets, never overlaid — so a single
fixed colour does not collide two series in one panel. Pre-phase, plotly's auto-cycle assigned
colour index 1 to that lone trace anyway, so every panel was already the same default blue; the
change makes it the palette blue `#2563eb`. This is the explicit phase decision recorded in the
SUMMARY: *"plot_stocks add_trace uses fixed es_colours[\"group1\"] (coarse granularity
acceptable)."* Plot structure (one trace per panel) is unchanged. No fix.

### IN-02 (was WR-01 — PRE-EXISTING, OUT OF SCOPE): `gridExtra::grid.arrange()` unguarded

**File:** `R/plotting.R:348`

`plot_diagnostics()` calls `gridExtra::grid.arrange()` without a `requireNamespace()` guard, and
`gridExtra` is in `Suggests`, not `Imports`. This is a real CRAN-hygiene gap, but it is
**pre-existing** — identical at the pre-phase baseline `daf50c7:R/plotting.R:351` — and is
explicitly logged as out-of-scope in the SUMMARY decisions: *"gridExtra unguarded call left as-is
(pre-existing tech debt, out of scope)."* Not introduced by Phase 21; not fixed here to honour the
aesthetics-only scope. Recommend a follow-up hardening ticket.

### IN-03 (was WR-02 — COSMETIC / DEFENSIVE IDIOM): named-vector subscripting into plotly slots

**File:** `R/plotting.R:74` (and `.style_plotly`/`vline` colour slots)

`es_colours["group1"]` returns a length-1 **named** character (`c(group1 = "#2563eb")`). Passed
into a plotly `list(color = ...)` slot the name rides along; plotly strips it silently, so the
rendered colour is correct today. `es_colours[["group1"]]` / `unname()` would be the more
defensive idiom, but the current code is functionally correct and ggplot2 usages are unaffected
(ggplot2 discards names). Cosmetic/subjective — recorded, not auto-fixed per scope.

### IN-04 (INFO — PRE-EXISTING): `@return` on `plot_diagnostics()` says "patchwork-style layout"

**File:** `R/plotting.R:276`

The roxygen `@return` reads "A ggplot2 plot arranged with patchwork-style layout," while the
implementation uses `gridExtra::grid.arrange()` (returns a `gtable`). Inaccurate, but the text is
**pre-existing** (identical at `daf50c7:R/plotting.R:279`) and Phase 21 did not touch it. Out of
scope for an aesthetics-only phase; bundle with the IN-02 hardening follow-up if desired.

---

## Verdict

No genuine Phase-21-introduced defects. Aesthetics-only invariant upheld; plot structure and
data mappings unchanged; palette integrity verified. **No fixes applied.** IN-02 and IN-04 point
at pre-existing tech debt worth a separate hardening ticket, outside this phase's scope.

---

_Reviewed: 2026-09-08T21:57:01Z_
_Reviewer: Claude (gsd-code-reviewer) + orchestrator verification_
_Depth: standard_
