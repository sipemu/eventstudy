---
phase: 16-worked-examples-gallery-build-release-integrity
fixed_at: 2026-09-06T22:00:00Z
review_path: .planning/phases/16-worked-examples-gallery-build-release-integrity/REVIEW.md
iteration: 1
findings_in_scope: 5
fixed: 5
skipped: 0
status: all_fixed
---

# Phase 16: Code Review Fix Report

**Fixed at:** 2026-09-06
**Source review:** `.planning/phases/16-worked-examples-gallery-build-release-integrity/REVIEW.md`
**Iteration:** 1

**Summary:**
- Findings in scope: 5
- Fixed: 5
- Skipped: 0

## Fixed Issues

### WR-01: Corrado (1989) cited for sign test — that paper introduced the rank test

**Files modified:** `vignettes/articles/example-regulatory.Rmd`
**Commit:** `fc6f183`
**Applied fix:** Replaced `[@Corrado1989]` with `[@Brown1985]`.

**Citation reasoning:** The `SignTest` class in `R/multi_event_test_statistics.R` (lines 222-228)
implements the plain binomial formula `(n_pos - 0.5*N) / (0.5*sqrt(N))` — exactly the sign test
described in Brown & Warner (1985). The `GeneralizedSignTest` class (line 270) is the Cowan (1992)
variant that adjusts for the estimation-period fraction of positive ARs. Since the regulatory
example uses `SignTest$new()` (not `GeneralizedSignTest`), the correct citation is `@Brown1985`
(key already present in `vignettes/articles/references.bib`). No new BibTeX entry was needed.

**Re-render result:** `pkgdown::build_article("articles/example-regulatory")` succeeded. Rendered
HTML shows "Brown and Warner 1985" in the bibliography; `@Corrado1989` does not appear.
Zero unresolved `[@` citations.

---

### WR-02: M&A power sweep uses n_simulations=200 but interprets results as if precise

**Files modified:** `vignettes/articles/example-ma.Rmd`
**Commit:** `2c3a335`
**Applied fix:** Added a blockquote caveat directly after the power interpretation paragraph:
"power estimates above are Monte Carlo averages from 200 simulation runs. At p ≈ 0.50 the
sampling error is roughly ±3–5 percentage points (95% interval), so small differences between
adjacent grid points are within simulation noise and should not be over-interpreted."
The simulation count was not changed (per instructions: disclose, don't re-run).

**Re-render result:** `pkgdown::build_article("articles/example-ma")` succeeded (BUILD OK).
Rendered HTML (line 249) contains the caveat text. Power curve plot still renders via Plotly.
Zero unresolved `[@` citations.

---

### IN-01: Earnings null hypothesis is stated at the individual-AR level, not the AAR level

**Files modified:** `vignettes/articles/example-earnings.Rmd`
**Commit:** `bf1a66d`
**Applied fix:** Changed `H_0: E[AR_t] = 0` to `H_0: E[AAR_t] = 0` in the LaTeX display and
simplified the follow-on sentence to remove the redundant restatement ("average abnormal return
(AAR) and the cumulative average abnormal return (CAAR)") since CAAR is now sufficient.

**Note:** IN-01 and IN-02 edits were applied to `example-earnings.Rmd` in a single edit session
(both changes together in one file), so they share commit `bf1a66d`. The commit message
references IN-01; IN-02 is covered by the same SHA.

**Re-render result:** `pkgdown::build_article("articles/example-earnings")` succeeded. Rendered
HTML shows `H_0: E[AAR_t] = 0` in the math display. Zero unresolved `[@` citations.

---

### IN-02: Earnings stat-values chunk does not report p-values for Patell Z or BMP

**Files modified:** `vignettes/articles/example-earnings.Rmd`
**Commit:** `bf1a66d` (same commit as IN-01; both applied to the same file in one pass)
**Applied fix:** Extended the `stat-values` chunk with:
- `df_bmp <- utils::tail(bmp$n_valid_events, 1L) - 1L` (BMP degrees of freedom)
- `p_patell <- round(2 * pnorm(abs(z_patell), lower.tail = FALSE), 4)` (Patell Z ~ N(0,1), two-sided)
- `p_bmp <- round(2 * pt(abs(t_bmp), df = df_bmp, lower.tail = FALSE), 4)` (BMP ~ t(df), two-sided)

Updated prose to inline the p-values: "Patell Z = `r z_patell` (two-sided p = `r p_patell`) and
the BMP statistic is `r t_bmp` (two-sided p = `r p_bmp`)."

**Re-render result:** Rendered HTML shows concrete values: Patell Z = 1.664 (p = 0.0961),
BMP = 2.248 (p = 0.1536). P-values derive from computed statistics — not fabricated.

---

### IN-03: _pkgdown.yml uses articles/ prefix for Worked Examples but not for other groups

**Files modified:** `_pkgdown.yml`
**Commit:** `2fc1d9f`
**Applied fix:** Added a 5-line YAML comment block immediately before the "Worked Examples"
group explaining that `example-*.Rmd` files live in `vignettes/articles/` (not `vignettes/`),
so the `articles/` prefix is required in slugs, and that future contributors must include it.
Slugs themselves were not modified.

---

## Skipped Issues

None.

---

## Verification notes

- Verification ran in the **main checkout** (not an isolated worktree — `workflow.use_worktrees = false`).
- Tier 1 (re-read) applied to all five findings.
- Tier 2 applied where available: `pkgdown::build_article()` executed for both modified articles
  (earnings, regulatory, M&A) — all three built successfully with no errors, only the expected
  `VignetteIndexEntry` title-mismatch warning (pre-existing, not introduced by these fixes).
- Protected files confirmed untouched: R/, DESCRIPTION, NAMESPACE, data/, .github/workflows/.

---

_Fixed: 2026-09-06_
_Fixer: Claude (gsd-code-fixer)_
_Iteration: 1_
