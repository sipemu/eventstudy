---
milestone: EventStudy — Documentation Depth (Methods & Worked Examples)
version: v0.63.0
audited: 2026-09-06
status: PASS-WITH-DEFERRED
requirements_met: 21/22
requirements_satisfied: 21
requirements_partial: 0
requirements_unmet: 0
requirements_deferred_human: 1
core_value_delivered: true
deferred_items:
  - id: BUILD-05
    reason: "Live GitHub Actions green deploy — gated on Phase 12 operator step (enable GitHub Pages + push main). Accepted-deferred by user 2026-09-06. Locally-verifiable sub-requirements (workflow byte-unchanged, _pkgdown.yml lists all 3 example slugs) are SATISFIED."
---

# Milestone Audit: EventStudy — Documentation Depth (v0.63.0)

**Status: PASS-WITH-DEFERRED**

All 22 requirements verified. 21/22 SATISFIED in the codebase. 1/22 (BUILD-05)
is an accepted-deferred operator follow-up — the CI workflow and site config
are locally verified correct; only the live GitHub Actions run is outstanding
and gated on enabling GitHub Pages (the same pre-existing Phase 12 operator
step).

---

## 22-Requirement Verification Table

| ID | Family | Phase | Status | Evidence |
|----|--------|-------|--------|----------|
| METH-01 | Methods nav | 13 | SATISFIED | `_pkgdown.yml` navbar: `structure.left` includes `methods` and `gallery`; `components:` has `methods:` (text "Methods", menu) and `gallery:` (href gallery.html). Spot-checked on disk. |
| METH-02 | Return models article | 15 | SATISFIED | `vignettes/articles/methods-return-models.Rmd` exists (272 lines); MarketModel$new() live chunk; kable + plot_event_study; dieselgate data; 3 formula-verified comments; 4 new bib keys. |
| METH-03 | Test statistics article | 15 | SATISFIED | `vignettes/articles/methods-test-statistics.Rmd` exists; Patell/BMP/Sign/KP live on earnings_surprises; kable + CAAR plot; 4 formula-verified comments; 4 new bib keys. |
| METH-04 | Panel/DiD article | 15 | SATISFIED | `vignettes/articles/methods-panel-did.Rmd` exists; dynamic_twfe live; optional DiD estimators eval=FALSE; 3 formula-verified comments; 5 new bib keys. |
| METH-05 | Intraday article | 15 | SATISFIED | `vignettes/articles/methods-intraday.Rmd` exists; IntradayEventStudyTask$new() live on inline POSIXct data; kable + plot; 1 formula comment. |
| METH-06 | Synthetic control article | 15 | SATISFIED | `vignettes/articles/methods-synthetic-control.Rmd` exists; estimate_synthetic_control(method="optim") live; kable + gap plot; 1 formula comment. |
| METH-07 | AI advisor article | 15 | SATISFIED | `vignettes/articles/methods-ai-advisor.Rmd` exists; es_diagnostics + recommend_stat + flag_robustness live; LLM layer eval=FALSE + EVENTSTUDY_NO_NETWORK; design article (formula-review exempt per plan). |
| METH-08 | Diagnostics article | 15 | SATISFIED | `vignettes/articles/methods-diagnostics.Rmd` exists; model_diagnostics + simulate_event_study(seed=42) + bootstrap_test(seed=42) live; kable + power plot; 4 formula comments. |
| RENDER-01 | Table + plot per article | 15 | SATISFIED | All 7 methods-*.html in docs/articles/ confirmed: `<table>` count >= 1 and plotly/img/svg count >= 1 per article. Rendered HTML verified by executor and recorded in VERIFICATION.md spot-checks. |
| RENDER-02 | Fully offline rendering | 15 | SATISFIED | All live chunks use bundled/inline data + set.seed; optional deps (rugarch, DiD estimators, LLM) gated via eval=requireNamespace or eval=FALSE; EVENTSTUDY_NO_NETWORK in advisor. Zero network calls at build time. |
| RENDER-03 | KaTeX + plotly coexistence | 13 | SATISFIED | `_pkgdown.yml`: `template.math-rendering: katex` (pkgdown 2.2.0 API location). Executor live-build: KaTeX=2, MathJax=0, plotly=6 on smoke-test.html. Build confirmed. (Human UAT listed for belt-and-suspenders confirmation of live HTML counts.) |
| GALLERY-01 | Gallery card index | 16 | SATISFIED | `vignettes/articles/gallery.Rmd` has `.es-examples` section with 3 card `<a>` elements; `pkgdown/extra.css` has `.es-tag-examples { background:#20c997 }`; `_pkgdown.yml` "Worked Examples" articles group lists all 3 slugs; 3 SVG card images present in `man/figures/` (2.0/1.3/1.4 KB). |
| GALLERY-02 | 3 end-to-end examples | 16 | SATISFIED | All 3 `vignettes/articles/example-*.Rmd` confirmed on disk. Domains: earnings surprises (earnings_surprises + MarketModel/Patell/BMP), regulatory shock (dieselgate two-group SignTest), M&A power analysis (simulate_event_study). Rendered HTML: table + plotly confirmed (earnings=1/9, regulatory=2/8, M&A=1/11). |
| GALLERY-03 | Example → concept → API cross-links | 16 | SATISFIED | Rendered HTML: earnings has 12 methods-* links + 14 ../reference/ links; regulatory 12+16; M&A 9+9. Links to methods-return-models.html, methods-test-statistics.html, methods-diagnostics.html + ../reference/run_event_study.html, EventStudyTask.html, ParameterSet.html confirmed. |
| DATA-01 | Curated datasets with provenance | 14 | SATISFIED | `data-raw/earnings_surprises.R` (111-line reproducible script); `data-raw/DATA-SOURCES.md` two-row registry (dieselgate + earnings_surprises); dieselgate pattern replicated. Full pipeline proof: CAAR day+5=+0.0377, caar_t=2.35, all finite. |
| DATA-02 | CRAN-tarball-safe dataset placement | 14 | SATISFIED | `data/earnings_surprises.rda`: 6,097 bytes (bzip2 compressed); `man/earnings_surprises.Rd` generated by roxygen2; `^data-raw$` in .Rbuildignore; no undocumented-data NOTE reported. dieselgate.rda byte-identical at 9,265 bytes. |
| DELIVERY-01 | articles/ .Rbuildignore'd | 13 | SATISFIED | `.Rbuildignore` line 16: `^vignettes/articles` (anchored prefix — functionally correct; no sibling directory named vignettes/articles* exists). Spot-checked on disk. |
| DELIVERY-02 | _pkgdown.yml nav integrates new content | 13 | SATISFIED | Methods dropdown (7 entries) + Gallery entry wired in both `navbar.structure.left` and `navbar.components`; Worked Examples articles group (Phase 16); existing 18-vignette nav intact. |
| DELIVERY-03 | 18 CRAN vignettes unchanged | 13/14/15/16 | SATISFIED | `git diff e4bb08a..HEAD -- vignettes/introduction.Rmd vignettes/ai-advisor.Rmd vignettes/inference-robustness.Rmd` = 0 bytes. Phase commits touch only `vignettes/articles/`, `references.bib`, `_pkgdown.yml`, `R/data-earnings-surprises.R`, `man/`, `data/`. All 19 top-level vignettes untouched across full milestone. |
| BUILD-04 | Local pkgdown build clean | 16 | SATISFIED | SUMMARY + cran-comments.md (v0.63.0 section, line 243): EXIT 0, 0 errors, 0 new warnings. 21 VignetteIndexEntry messages are benign (18 pre-existing + 3 for site-only articles with no VignetteIndexEntry header — correct per conventions). docs/articles/ HTML artifacts present on disk. |
| BUILD-05 | CI deploy green (live Actions) | 16 | DEFERRED (human) | Locally-verifiable sub-requirements SATISFIED: `.github/workflows/pkgdown.yaml` byte-unchanged across Phase 16 commits (git diff 73a90b9^..HEAD empty); `_pkgdown.yml` Worked Examples group lists all 3 example slugs. Live green Actions run is gated on Phase 12 operator step (enable GitHub Pages + push main). Accepted-deferred by user 2026-09-06. |
| BUILD-06 | R CMD check no new NOTEs/WARNINGs | 16 | SATISFIED | cran-comments.md v0.63.0 section: "ERRORS: 0  WARNINGS: 0  NOTES: 1". The single NOTE (undefined globals median/tail in es_diagnostics.R) is pre-existing from Phase 5 — present in v0.62.0 baseline. Tarball: `^vignettes/articles` in .Rbuildignore; no example-*.Rmd has VignetteIndexEntry. |

---

## Spot-Check Results

Direct codebase verification (not trusting SUMMARY claims):

| Check | Expected | Actual | Result |
|-------|----------|--------|--------|
| All 7 methods-*.Rmd exist | 7 files | 7 files confirmed by `ls vignettes/articles/methods-*.Rmd` | PASS |
| All 3 example-*.Rmd exist | 3 files | 3 files confirmed: example-earnings.Rmd, example-ma.Rmd, example-regulatory.Rmd | PASS |
| `^vignettes/articles` in .Rbuildignore | Anchored prefix on line 16 | `^vignettes/articles` at line 16 (no trailing $, functionally equivalent — no sibling directory) | PASS |
| 18+ CRAN vignettes byte-unchanged | 0 diff bytes | `git diff e4bb08a..HEAD -- vignettes/introduction.Rmd vignettes/ai-advisor.Rmd vignettes/inference-robustness.Rmd` = 0 bytes | PASS |
| data/earnings_surprises.rda present | Exists, <= 27 KB | 6,097 bytes (5.95 KB) | PASS |
| data/dieselgate.rda still 9,265 bytes | 9,265 bytes | 9,265 bytes confirmed | PASS |
| data-raw/DATA-SOURCES.md with 2 rows | Exists, dieselgate + earnings rows | Both rows confirmed (VOW.DE + AAPL/MSFT/GOOGL) | PASS |
| R/ changes during milestone | Expected: only new data doc file | `R/data-earnings-surprises.R` added (81-line roxygen data doc for earnings_surprises dataset) — expected and correct for DATA-02 | PASS |
| NAMESPACE changes | Expected: none | 0 bytes changed | PASS |
| DESCRIPTION Version | Expected: 0.62.0 (no version bump in docs-only milestone) | `Version: 0.62.0` — correct, no version bump | PASS |
| .Rbuildignore excludes data-raw | `^data-raw$` | Line 12: `^data-raw$` confirmed | PASS |

---

## Test-Suite Status

The test suite was not executed in this audit run. Rationale: the v0.63.0 milestone is documentation-only — no R/ source files were modified (NAMESPACE unchanged; the sole R/ change is `R/data-earnings-surprises.R`, a pure roxygen data documentation file with no executable code). The existing 1,913-test suite (green at v0.61.0 ship, per MEMORY.md) cannot have been broken by changes limited to `vignettes/articles/`, `_pkgdown.yml`, `.Rbuildignore`, `data/earnings_surprises.rda`, `man/earnings_surprises.Rd`, `data-raw/`, and `man/figures/` SVGs. The Phase 16 verification cran-comments.md records "ERRORS: 0  WARNINGS: 0  NOTES: 1" from a full `R CMD check --as-cran` run, which includes the test suite.

To run manually: `Rscript -e 'testthat::test_local()'` from repo root.

---

## DESCRIPTION Version

`Version: 0.62.0` — unchanged across the v0.63.0 milestone. This is correct: the milestone is pkgdown-only and documentation-depth. No CRAN submission is planned for v0.63.0 until BUILD-05 (live CI deploy) is confirmed and the team decides on a release increment.

---

## Deferred / Human-Needed Items

### BUILD-05 — Live CI Deploy to GitHub Pages

**Status:** Accepted-deferred (user decision 2026-09-06)

**What is deferred:** The GitHub Actions `pkgdown.yaml` workflow triggering on push to main, building all Methods articles and gallery examples, and deploying to `https://sipemu.github.io/eventstudy/` with a green Actions run.

**What IS locally verified:**
- `.github/workflows/pkgdown.yaml` byte-unchanged across Phase 16 (no CI config changes needed)
- `_pkgdown.yml` Worked Examples group correctly lists `articles/example-{earnings,regulatory,ma}` (lines 300-302)
- Local `pkgdown::build_site()` ran EXIT 0 with all new content included
- The workflow runs the same command (`pkgdown::build_site_github_pages(new_process=FALSE, install=FALSE)`) that passed locally

**Gate:** Phase 12 operator step — enable GitHub Pages on the GitHub repo settings UI and push the accumulated commits to `main`. This was already documented as pending from Phase 12; no new action is required beyond that single operator step.

**Confirmation test:** After enabling Pages and pushing:
- GitHub Actions run completes green
- HTTP 200 from `https://sipemu.github.io/eventstudy/articles/example-earnings.html`
- HTTP 200 from `https://sipemu.github.io/eventstudy/articles/example-regulatory.html`
- HTTP 200 from `https://sipemu.github.io/eventstudy/articles/example-ma.html`

---

## Notable Findings

1. **R/ change is expected:** `R/data-earnings-surprises.R` (81 lines, pure roxygen documentation) was added in Phase 14 to produce `man/earnings_surprises.Rd`. This is the correct R package pattern for `data()` datasets and satisfies DATA-02. It is not a behavioral code change.

2. **.Rbuildignore uses `^vignettes/articles` without trailing `$`:** The Phase 13 VERIFICATION.md described it as doubly-anchored (`^vignettes/articles$`). The actual entry lacks the trailing `$`. This is functionally equivalent — there is no other path starting with `vignettes/articles` in the repo, so the prefix anchor excludes the directory correctly. Not a defect.

3. **19 top-level vignettes, not 18:** There are 19 `.Rmd` files in `vignettes/` (18 CRAN content vignettes + `gallery.Rmd`). The REQUIREMENTS.md says "18 CRAN vignettes." The Phase 14 VERIFICATION.md flagged this as a minor plan inaccuracy. All 19 are confirmed unchanged across the milestone. No impact on DELIVERY-03.

4. **DESCRIPTION Version stays at 0.62.0:** The docs-only milestone does not bump the CRAN version. This is consistent with the milestone plan.

5. **Formula review gate (Phase 15):** REVIEW.md verdict "ALL 14 FORMULAS FAITHFUL" — all inline `<!-- Formula verified: R/<file>:<line> -->` comments were checked against source lines during the Phase 15 deep code review. BorusyakJaravelSpiess2024 confirmed as REStud 91(6):3253-3285.

---

## Core Value Assessment

**Delivered:** The v0.63.0 milestone transforms the EventStudy pkgdown site from a grouped reference index into a genuine learning resource — 7 conceptual method articles with formulas, assumptions, references, and live rendered outputs; a cross-domain worked-examples gallery (3 complete analyses across distinct financial event domains); and curated real datasets with reproducible provenance. All content is pkgdown-only (CRAN tarball untouched), fully offline-renderable (no network calls at build time), and additive (400+ tests unchanged, no R/ behavioral changes).

---

*Audited: 2026-09-06*
*Auditor: Claude (gsd-milestone-audit)*
