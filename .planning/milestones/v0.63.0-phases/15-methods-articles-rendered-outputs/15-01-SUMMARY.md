---
phase: 15-methods-articles-rendered-outputs
plan: 01
subsystem: docs-site
tags: [pkgdown, methods-articles, katex, citations, offline-render]
status: complete
requires:
  - "vignettes/articles infrastructure (_setup.Rmd, _article-skeleton.Rmd, smoke-test.Rmd) from Phase 13"
  - "bundled dieselgate.rda + earnings_surprises.rda"
provides:
  - "7 formula-bearing Methods articles under vignettes/articles/"
  - "~19 new BibTeX keys in vignettes/articles/references.bib"
  - "7 Methods-dropdown entries in _pkgdown.yml"
affects:
  - "_pkgdown.yml navbar Methods menu"
tech-stack:
  added: []
  patterns:
    - "pkgdown::build_article(name, pkg=as_pkgdown('.')) with 'articles/' name prefix — bare build_article(name) cannot find articles in this repo"
    - "tidy.EventStudyTask(task, type=...) called directly (broom tidy generic is not re-exported)"
    - "eval=requireNamespace(...) / eval=FALSE chunk gates for optional-dep and network paths"
key-files:
  created:
    - vignettes/articles/methods-return-models.Rmd
    - vignettes/articles/methods-test-statistics.Rmd
    - vignettes/articles/methods-panel-did.Rmd
    - vignettes/articles/methods-intraday.Rmd
    - vignettes/articles/methods-synthetic-control.Rmd
    - vignettes/articles/methods-ai-advisor.Rmd
    - vignettes/articles/methods-diagnostics.Rmd
  modified:
    - vignettes/articles/references.bib
    - _pkgdown.yml
decisions:
  - "Durbin-Watson LOCATED at R/diagnostics.R:56-61 — carries a real Formula-verified comment (not conceptual)"
  - "Intraday request tibble needs event_id + group columns (RESEARCH omitted them) — added to match .request_file_columns"
  - "tidy.EventStudyTask(task, type='car'/'aar') used instead of bare tidy(task) — the broom generic is not attached by library(EventStudy)"
metrics:
  duration: "~1 session"
  completed: 2026-09-06
  tasks: 8
  commits: 8
actuals:
  tokens: 11650
  tasks: 8
  commits: 8
---

# Phase 15 Plan 01: Methods Articles + Rendered Outputs Summary

Authored 7 formula-bearing pkgdown Methods articles (METH-02..08), each rendering live offline with a real kable table and a plot, KaTeX display equations carrying `R/<file>:<line>` provenance, resolved citations from ~19 atomically-added BibTeX keys, and wired all 7 into the `_pkgdown.yml` Methods dropdown.

## Per-Task Commits

| Task | Article / Change | Commit |
|------|------------------|--------|
| 1 (tracer) | methods-return-models.Rmd (METH-02) | a7feb6f |
| 2 | methods-test-statistics.Rmd (METH-03) | df014bb |
| 3 | methods-panel-did.Rmd (METH-04) | 9df1e5a |
| 4 | methods-intraday.Rmd (METH-05) | 3c1a5fb |
| 5 | methods-synthetic-control.Rmd (METH-06) | fb161bf |
| 6 | methods-ai-advisor.Rmd (METH-07) | 92beaba |
| 7 | methods-diagnostics.Rmd (METH-08) | 4106f25 |
| 8 | _pkgdown.yml Methods dropdown wiring | 87c70d3 |

## Per-Article Verification

All 7 articles rendered via `pkgdown::build_article("articles/<name>", pkg=as_pkgdown("."))` and the HTML was grepped: every one has `<table`, a plot element (`<img`/`plotly`/`<svg`), zero unresolved `[@`, zero raw `$$` passthrough, and KaTeX `class="math"` spans (advisor exempt — no equation).

| Article | Rendered clean | Table + plot | Citations resolved | Formula-verified comments | API deviation |
|---------|:---:|:---:|:---:|:---:|---|
| return-models | yes | yes (kable CAR + plot_event_study) | yes | 3 | tidy via `tidy.EventStudyTask(type="car")` not bare `tidy()` |
| test-statistics | yes | yes (kable AAR + caar plot) | yes | 4 | tidy via `type="aar"` |
| panel-did | yes | yes (kable coef + plot_panel_event_study) | yes | 3 | none (exact source method strings used) |
| intraday | yes | yes (kable CAR + plot_event_study) | yes | 1 | request needs `event_id`+`group` cols (added vs RESEARCH) |
| synthetic-control | yes | yes (kable trajectory + gap plot) | yes | 1 | none (method="optim", result shape as documented) |
| ai-advisor | yes | yes (kable est-window diag + plot_diagnostics) | n/a (no cites) | 0 (design article, exempt) | `estimation_window` field order is r2,sigma,dof,acf1,shapiro_p,dw_stat,ljung_box_p |
| diagnostics | yes | yes (kable diag + power plot) | yes | 4 | none (`model_diagnostics` is the real export name) |

## Durbin-Watson Resolution

**LOCATED** at `R/diagnostics.R:56-61` — the package computes the approximate DW statistic directly as `sum(diff(resid)^2) / sum(resid^2)`. RESEARCH flagged DW as possibly unimplemented (grep miss); it is in fact implemented, so methods-diagnostics.Rmd carries a genuine `<!-- Formula verified: R/diagnostics.R:56-61 matches DurbinWatson1950 -->` comment rather than presenting DW conceptually without provenance.

## BibTeX Fields Left Uncertain

All ~19 added entries are `[ASSUMED]` (training-sourced, not fetched from a DOI registry this session). Every cited `[@Key]` resolves in the rendered HTML (verified: 15 distinct keys, all resolve; zero `[@` literals in any article HTML). Uncertainty flagged in-file via comments:
- **BorusyakJaravelSpiess2024** (Review of Economic Studies, vol 91 / no 6 / pp 3253-3285) — flagged in references.bib as lower-confidence (2024 publication most likely to drift). Kept the entry so the citation resolves; a reviewer should confirm vol/no/pages against the DOI.
- SunAbraham2021, CallawaySantAnna2021, GoodmanBacon2021 — same J. Econometrics vol.225 special issue (high confidence).
- Remaining entries (Fama-French, Carhart, Patell, BMP, Corrado, Kolari-Pynnönen, Barclay-Warner, Abadie-Diamond-Hainmueller, Box-Ljung, Durbin-Watson) sanity-checked and consistent with standard metadata.

## Regression Guards (all pass)

- **19 top-level CRAN vignettes byte-unchanged:** `git diff -- 'vignettes/*.Rmd'` is empty (articles live in `vignettes/articles/`, not matched by that glob).
- **Bundled data untouched:** `git status --porcelain data/` empty; `dieselgate.rda` = 9265 B, `earnings_surprises.rda` = 6097 B (both exact).
- **R/ DESCRIPTION NAMESPACE untouched:** `git status --porcelain R/ DESCRIPTION NAMESPACE` empty.
- **_pkgdown.yml:** only the Methods `menu:` expanded (Overview relabelled "Methods Overview" + 7 new entries); YAML valid, `navbar$structure$left` intact, 8 total menu entries.

## Deviations from Plan

### API adjustments (documented, not bugs)

**1. [Rule 3 - Blocking] `tidy(task)` → `tidy.EventStudyTask(task, type=...)`**
- **Found during:** Task 1 tracer render (`could not find function "tidy"`).
- **Cause:** EventStudy exports the S3 *method* `tidy.EventStudyTask` but does not re-export the broom/generics `tidy` generic; `library(EventStudy)` does not attach it. RESEARCH's example used bare `tidy(task)` which errors at render.
- **Fix:** Call the method directly with an explicit `type` argument (`"car"` for single-event, `"aar"` for multi-event). Applied in return-models, test-statistics, intraday.
- **Files:** methods-return-models.Rmd, methods-test-statistics.Rmd, methods-intraday.Rmd.

**2. [Rule 3 - Blocking] Intraday request tibble requires `event_id` + `group`**
- **Found during:** Task 4 live pipeline check (`Request missing columns: event_id, group`).
- **Cause:** `IntradayEventStudyTask`'s `.request_file_columns` requires `event_id` and `group` in addition to the columns RESEARCH listed.
- **Fix:** Added `event_id = 1L, group = "Intraday"` to the synthetic request tibble; matched the rest of the schema to the working `vignettes/intraday-event-study.Rmd` (firm/reference use `symbol/timestamp/price`; reference uses `symbol`, not `index_symbol`).
- **Files:** methods-intraday.Rmd.

**3. [Non-blocking] `pkgdown::build_article` invocation**
- Bare `build_article("methods-return-models")` (as written in plan verify blocks) fails with "Can't find article" for *every* article including the pre-existing smoke-test — pkgdown discovers articles under the `articles/` prefix and needs a `pkg` context. Used `build_article("articles/<name>", pkg = pkgdown::as_pkgdown("."))` throughout. This is an invocation detail, not an article defect; all renders succeeded.

**4. [Non-blocking] `estimate_synthetic_control` / `estimate_panel_event_study` return the mutated task**
- Both populate `task$results` and return the task (not a bare result list). Confirmed against source and used `res$results$...` accessors accordingly. Matches RESEARCH result-shape notes.

### None else — the 7-article structure, dataset assignments, offline gating, and formula provenance followed the plan exactly.

## Known Stubs

None. Every article's live path uses real package code on bundled or inline-synthetic data; no placeholder tables, no hardcoded empty values, no unwired plots.

## Self-Check: PASSED

Created files (all exist):
- vignettes/articles/methods-return-models.Rmd, methods-test-statistics.Rmd, methods-panel-did.Rmd, methods-intraday.Rmd, methods-synthetic-control.Rmd, methods-ai-advisor.Rmd, methods-diagnostics.Rmd — FOUND
- vignettes/articles/references.bib (modified), _pkgdown.yml (modified) — FOUND

Commits (all exist in git log): a7feb6f, df014bb, 9df1e5a, 3c1a5fb, fb161bf, 92beaba, 4106f25, 87c70d3 — FOUND

Rendered HTML for all 7 articles: table + plot + resolved citations + KaTeX + no raw math — VERIFIED.
Regression guards: 19 vignettes byte-unchanged, data/R/DESCRIPTION/NAMESPACE untouched — VERIFIED.
