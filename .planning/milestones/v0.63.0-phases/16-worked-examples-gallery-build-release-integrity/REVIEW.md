---
phase: 16-worked-examples-gallery-build-release-integrity
reviewed: 2026-09-06T00:00:00Z
depth: deep
files_reviewed: 9
files_reviewed_list:
  - vignettes/articles/example-earnings.Rmd
  - vignettes/articles/example-regulatory.Rmd
  - vignettes/articles/example-ma.Rmd
  - vignettes/gallery.Rmd
  - pkgdown/extra.css
  - _pkgdown.yml
  - man/figures/card-example-earnings.svg
  - man/figures/card-example-regulatory.svg
  - man/figures/card-example-ma.svg
findings:
  critical: 0
  warning: 2
  info: 3
  total: 5
status: issues_found
---

# Phase 16: Code Review Report

**Reviewed:** 2026-09-06
**Depth:** deep (cross-file + API call chain tracing)
**Files Reviewed:** 9
**Status:** issues_found

## Summary

Three worked-example articles (earnings, regulatory, M&A), gallery card infrastructure, CSS, and cran-comments baseline reviewed against the actual R source in `R/`. No CRITICAL findings. The executor correctly adapted all five documented deviations from the plan. Column access patterns in stat-values chunks are verified against live source. Citation and link targets are mostly sound with two exceptions noted below.

The highest-priority finding is a citation misattribution in the regulatory example that falsely credits a nonparametric sign test to the Corrado (1989) rank-test paper — a citable statistical error that a peer reviewer would flag immediately. The second warning is a methodological honesty gap in the M&A power sweep: only 200 simulations are used and the interpretation presents power estimates as if they were precise, without acknowledging the ±3–5 pp simulation noise. Neither finding causes silently-wrong statistical results in computed output; both affect the accuracy of the surrounding prose.

## Narrative Findings (AI reviewer)

## Warnings

### WR-01: Corrado (1989) cited for sign test — that paper introduced the rank test

**File:** `vignettes/articles/example-regulatory.Rmd:138-139`
**Issue:** The interpretation prose justifies using `SignTest` when the two-firm normality assumption is fragile with `[@Corrado1989]`. Corrado (1989) — "A Nonparametric Test for Abnormal Security-Price Performance in Event Studies" (JFE 23:2) — introduced the **rank** test for abnormal returns, not the sign test. The sign test in event studies is attributed to Cowan (1992) ("Nonparametric Event Study Tests", *Review of Quantitative Finance and Accounting*). Citing the wrong paper to justify the sign test is a substantive misattribution that will be caught in peer review or by any reader who looks up the reference.
**Fix:** Replace `[@Corrado1989]` with the correct sign-test citation. Add a `@Cowan1992` entry to `vignettes/articles/references.bib`:

```bibtex
@article{Cowan1992,
  author  = {Arnold R. Cowan},
  title   = {Nonparametric Event Study Tests},
  journal = {Review of Quantitative Finance and Accounting},
  year    = {1992},
  volume  = {2},
  number  = {4},
  pages   = {343--358}
}
```

And change the prose from:
```
the sign statistic points the same way as the parametric test, which matters when
a two-firm group makes the normality assumption of the *t*-test fragile [@Corrado1989].
```
to:
```
the sign statistic points the same way as the parametric test, which matters when
a two-firm group makes the normality assumption of the *t*-test fragile [@Cowan1992].
```

---

### WR-02: M&A power sweep uses n_simulations=200 but interprets results as if precise

**File:** `vignettes/articles/example-ma.Rmd:50,76,117-121`
**Issue:** Both the warm-up simulation (`sim_result`) and the power-sweep grid use `n_simulations=200`. At alpha=0.05, the Monte Carlo SE of a power estimate p is sqrt(p*(1-p)/200). At p≈0.50 this is ±0.035 (95% CI spans 14 pp). The interpretation at line 117 states "essentially blind (power near the nominal 5% false-positive floor)" and "the test detects the effect in the large majority of samples" — both phrased as established facts rather than noisy estimates. The plan originally implied n_simulations=1000 (the `simulate_event_study()` default). Rendering the article with seed=42 and 200 simulations will produce a reproducible but imprecise power curve, and the prose does not inform readers that the estimates carry substantial simulation uncertainty.

**Fix:** Either increase `n_simulations` to at least 500 (preferably 1000) for tighter estimates, or add a one-sentence caveat in the interpretation acknowledging simulation imprecision:

```r
sim_result <- simulate_event_study(
  n_events         = 15,
  event_window     = c(-5, 5),
  abnormal_return  = 0.01,
  n_simulations    = 1000,   # 1000 reps: SE < 1 pp across the full power range
  seed             = 42
)
```

And in the grid chunk, match `n_simulations = 1000`. If build time is a concern, keep 200 but add to the interpretation: "Note: estimates shown are based on 200 simulation runs; Monte Carlo standard error is roughly ±3–5 percentage points."

---

## Info

### IN-01: Earnings null hypothesis is stated at the individual-AR level, not the AAR level

**File:** `vignettes/articles/example-earnings.Rmd:28`
**Issue:** The formal null `H_0: E[AR_t] = 0 for all t` uses individual firm AR notation, but the statistics computed (Patell Z, BMP) test H0: E[AAR_t] = 0 (the cross-sectional average). The very next sentence ("against the alternative that the average abnormal return (AAR) and CAAR differ from zero") partially corrects this, but the mathematical display and the statistics are misaligned. A technically rigorous article would state `H_0: E[AAR_t] = 0` directly, since Patell/BMP are aggregate tests. This is a pedagogically common shorthand but is not fully precise for a worked example intended as a reference.

**Fix:** Change line 28 from:
```
$$H_0:\ \mathbb{E}[AR_t] = 0 \quad \text{for all } t \text{ in the event window},$$
```
to:
```
$$H_0:\ \mathbb{E}[AAR_t] = 0 \quad \text{for all } t \text{ in the event window},$$
```
and simplify the following line to remove the redundant AAR/CAAR restatement.

---

### IN-02: Earnings stat-values chunk does not report p-values for Patell Z or BMP

**File:** `vignettes/articles/example-earnings.Rmd:132-148`
**Issue:** The stat-values chunk extracts `z_patell` (Patell Z statistic) and `t_bmp` (cumulative BMP t-statistic) but never computes or reports p-values. The interpretation says "at a 5% level we [reject / fail to reject] H0" as a template placeholder — the actual rejection decision is left implicit. A reader cannot determine significance without doing external arithmetic on the reported z/t statistics. The Patell Z is a standard normal so p is easy to calculate; the BMP t uses n_valid_events degrees of freedom.

**Fix:** Extend the stat-values chunk to compute and inline-reference p-values:
```r
p_patell <- round(2 * pnorm(abs(z_patell), lower.tail = FALSE), 4)
df_bmp   <- tail(pz$n_valid_events, 1L)
p_bmp    <- round(2 * pt(abs(t_bmp), df = df_bmp - 1, lower.tail = FALSE), 4)
```
Then update the interpretation prose to state concrete decisions: "Patell Z = `r z_patell` (p = `r p_patell`); BMP t = `r t_bmp` (p = `r p_bmp`)."

---

### IN-03: _pkgdown.yml uses articles/ prefix for Worked Examples but not for Core Workflow / Return Models groups

**File:** `_pkgdown.yml:290-297`
**Issue:** The "Methods" and "Worked Examples" article groups use `articles/example-*` slugs with the `articles/` prefix, while all other groups (Core Workflow, Return Models, etc.) use bare slugs (e.g., `result-extraction`, not `articles/result-extraction`). This inconsistency in slug format is harmless since pkgdown accepted both (BUILD-04 passed), but it is a silent deviation from the convention used in every other group in the file. Future maintainers adding articles to the Worked Examples group may not notice that the prefix is needed (because the vignettes live in `vignettes/articles/` rather than `vignettes/` directly), and adding a bare slug for a future example-*.Rmd would silently fail or produce a misleading build warning.

**Fix:** Add a comment in `_pkgdown.yml` before the "Worked Examples" group explaining the prefix requirement:
```yaml
  # Note: example-* articles live in vignettes/articles/ (not vignettes/), so
  # the articles/ prefix is required here. Core Workflow etc. use bare slugs
  # because those Rmds live directly in vignettes/.
  - title: "Worked Examples"
```

---

_Reviewed: 2026-09-06_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: deep_
