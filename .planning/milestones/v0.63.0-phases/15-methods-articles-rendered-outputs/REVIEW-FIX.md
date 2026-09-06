---
phase: 15-methods-articles-rendered-outputs
fixed_at: 2026-09-06T00:00:00Z
review_path: .planning/phases/15-methods-articles-rendered-outputs/REVIEW.md
iteration: 1
findings_in_scope: 3
fixed: 3
skipped: 0
status: all_fixed
---

# Phase 15: Code Review Fix Report

**Fixed at:** 2026-09-06
**Source review:** .planning/phases/15-methods-articles-rendered-outputs/REVIEW.md
**Iteration:** 1

**Summary:**
- Findings in scope: 3 (WR-01, WR-02, IN-01; IN-02 was note-only with no code action)
- Fixed: 3
- Skipped: 0

## Fixed Issues

### WR-01: AI-advisor article shows non-existent function `advise_llm()`

**Files modified:** `vignettes/articles/methods-ai-advisor.Rmd`
**Commit:** 0131d75
**Applied fix:** Replaced `advise_llm(task, provider = "anthropic")` with
`es_advise(diag, task_type = "interpret", provider = provider("anthropic"))`.
Confirmed against `R/advise.R:740`: `es_advise(diagnostics, task_type, provider = NULL, model = NULL, ...)`;
`task_type = "interpret"` is in `LLM_ONLY_TYPES` so it correctly requires a provider.
The `diag` object is already computed in the chunk immediately above. Chunk remains `eval=FALSE`.

### WR-02: Synthetic-control article calls `method = "optim"` the default

**Files modified:** `vignettes/articles/methods-synthetic-control.Rmd`
**Commit:** c5593ee
**Applied fix:** Rewrote two prose passages that described `"optim"` as the default:
- §1 abstract: "using the base-R `method = \"optim\"` solver" → "explicitly choosing `method = \"optim\"` (pure base-R `stats::optim`; the package default is `\"quadprog\"` when that optional package is installed)"
- §6 intro: "We default to `method = \"optim\"` ..." → "We explicitly pass `method = \"optim\"` (pure `stats::optim` L-BFGS-B, no extra dependency); the package default is `\"quadprog\"` when that optional package is available."
Confirmed against `R/synthetic_control.R:80`: `method = c("quadprog", "optim")`, so `match.arg` selects `"quadprog"` as default. Live code chunk (`method = "optim"` explicit) left unchanged.

### IN-01: BorusyakJaravelSpiess2024 uncertainty comment removed

**Files modified:** `vignettes/articles/references.bib`
**Commit:** 93dfe81
**Applied fix:** Deleted the two-line comment "% BorusyakJaravelSpiess2024: vol 91 / issue 6 / pages 3253-3285 [ASSUMED, lower % confidence — 2024 publication most likely to drift; conservative if uncertain]." Entry fields (author, title, journal, year, volume, number, pages) left fully intact.

## Skipped Issues

None.

---

## MacKinlay1997 / Brown1985 confirmation (IN-02)

Both keys are present and correct in `vignettes/articles/references.bib`:
- `MacKinlay1997`: A. Craig MacKinlay, "Event Studies in Economics and Finance", Journal of Economic Literature, 1997, vol 35, no 1, pp 13-39. Matches the known metadata exactly.
- `Brown1985`: Stephen J. Brown and Jerold B. Warner, "Using Daily Stock Returns: The Case of Event Studies", Journal of Financial Economics, 1985, vol 14, no 1, pp 3-31. Matches the known metadata exactly.
No additions were needed.

## Re-render Results

Both edited articles were re-rendered via `pkgdown::build_article()` in the main checkout (workflow.use_worktrees=false):

- `methods-ai-advisor`: renders cleanly to HTML; `es_advise` present in output, `advise_llm` absent, zero unresolved `[@`, live table and plot present.
- `methods-synthetic-control`: renders cleanly to HTML; corrected prose present, stale "We default to" absent, zero unresolved `[@`, live table (10-row trajectory) and plot present.

The pre-existing title/VignetteIndexEntry mismatch warning appeared for both articles — this is unrelated to these changes and pre-dates this phase.

## Constraint Verification

- `R/`, `DESCRIPTION`, `NAMESPACE`, `data/`: zero diff (confirmed via `git diff HEAD~3 -- R/ DESCRIPTION data/` returning empty).
- Top-level CRAN vignettes (`vignettes/*.Rmd`): zero diff (only `vignettes/articles/` files changed).

---

_Fixed: 2026-09-06_
_Fixer: Claude (gsd-code-fixer)_
_Iteration: 1_
