---
phase: 16-worked-examples-gallery-build-release-integrity
plan: 01
subsystem: docs-site
tags: [pkgdown, worked-examples, gallery, cran-check, build-gate, offline-render]
status: complete
requires:
  - Phase 15 methods-* articles + article infrastructure (KaTeX, citations, _setup.Rmd)
  - Phase 14 bundled datasets (earnings_surprises, dieselgate)
provides:
  - 3 worked-example articles (earnings, regulatory, M&A)
  - Worked Examples gallery section + navbar menu + _pkgdown.yml articles group
  - v0.62.0/v0.63.0 cran-comments baseline sections
affects:
  - _pkgdown.yml (methods-* articles wired into index — pre-existing build defect fix)
tech-stack:
  added: []
  patterns:
    - "Site-only articles in vignettes/articles/ (no VignetteIndexEntry, tarball-excluded)"
    - "plotly::ggplotly() wrap of plot_event_study() ggplot to satisfy RENDER-01 plotly gate"
    - "es_simulation power sweep rendered as a native plotly::plot_ly power curve"
key-files:
  created:
    - vignettes/articles/example-earnings.Rmd
    - vignettes/articles/example-regulatory.Rmd
    - vignettes/articles/example-ma.Rmd
    - man/figures/card-example-earnings.svg
    - man/figures/card-example-regulatory.svg
    - man/figures/card-example-ma.svg
  modified:
    - pkgdown/extra.css
    - vignettes/gallery.Rmd
    - _pkgdown.yml
    - cran-comments.md
decisions:
  - "Patell/BMP/Sign are multi-event statistics (plan prose put them in single_event_statistics — corrected)"
  - "tidy.EventStudyTask has no 'caar' type; type='aar' output carries caar/caar_statistic/caar_p.value columns"
  - "simulate_event_study returns an es_simulation summary (power/rejection_by_day), not a fitted task"
metrics:
  duration: ~55m
  completed: 2026-09-06
actuals:
  tokens: 41000
  tasks: 4
  commits: 4
---

# Phase 16 Plan 01: Worked-Examples Gallery + Build & Release Integrity Summary

Delivered three offline, end-to-end worked-example pkgdown articles (earnings
surprise, dieselgate regulatory shock, synthetic M&A power analysis) with teal
gallery-card infrastructure, and proved the entire v0.63.0 documentation
addition builds and checks clean: full `build_site_github_pages()` with 0 errors
and `R CMD check` with 0 new NOTEs/WARNINGs vs the v0.62.0 baseline.

## Tasks completed

| Task | Type | Name | Commit |
|------|------|------|--------|
| 1 | tracer | Earnings example + CSS slot + gallery card + _pkgdown.yml group | `73a90b9` |
| 2 | auto | Regulatory (dieselgate two-group) example + card | `c81f6c3` |
| 3 | auto | M&A power-analysis (synthetic) example + card | `688e1c7` |
| 4 | auto | Build & release gate: full site build, CRAN check, cran-comments | `1d443be` |

## Per-example render verification (RENDER-01)

All three verified in the fresh full-site build (`docs/articles/*.html`):

| Article | `<table>` | plotly/htmlwidget | methods-* link | ../reference/ link | Extra |
|---------|-----------|-------------------|----------------|--------------------|-------|
| example-earnings   | YES | YES | YES | YES | CAR kable + ggplotly CAR plot |
| example-regulatory | YES | YES | YES | YES | CAAR kable + car_by_group summary + ggplotly |
| example-ma         | YES | YES | YES | YES | power kable + plotly power curve; SYNTHETIC marker YES |

## Build gate results

- **BUILD-04** `pkgdown::build_site_github_pages(new_process=FALSE, install=FALSE)`:
  **EXIT 0, 0 errors.** 21 benign `VignetteIndexEntry` title-check messages
  (18 pre-existing from Phase 15/earlier; 3 for the new site-only example
  articles, which follow the established no-VignetteIndexEntry convention of the
  Phase 15 methods-* articles). No new *class* of warning.
- **BUILD-06** `devtools::check(--no-manual --no-build-vignettes --no-vignettes)`:
  **ERRORS: 0 | WARNINGS: 0 | NOTES: 1.** The single NOTE is the pre-existing
  `median`/`tail` undefined-globals finding in `R/es_diagnostics.R` (documented
  in the v0.60.0 baseline, unchanged since Phase 5). **Gate PASSED** — strict
  subset of v0.62.0 baseline; zero new findings.
- **GALLERY-01/02/03**: gallery.html contains the `es-examples` Worked Examples
  section with all 3 teal cards; each article has data output + kable + plotly +
  interpretation; each cross-links to methods-* and ../reference/ pages.

### Phase 16 Verification script output

```
PHASE 16 VERIFICATION: ALL CHECKS PASSED
```

## Isolation confirmation (BUILD-06 scope, `git diff 73a90b9^..HEAD`)

Only the 10 allowed files changed across all Phase 16 commits:
`_pkgdown.yml`, `pkgdown/extra.css`, `vignettes/gallery.Rmd`, `cran-comments.md`,
the 3 `vignettes/articles/example-*.Rmd`, and the 3 `man/figures/card-example-*.svg`.

- R/ NAMESPACE DESCRIPTION data/ changes: **NONE** (empty)
- non-gallery / non-example vignettes changed: **NONE** (empty)
- `.github/workflows/pkgdown.yaml`: **byte-unchanged**
- `.Rbuildignore` still contains `^vignettes/articles`: **YES**
- `docs/` was rebuilt but **NOT committed** (build artifact; plan instructs source-only commits)

## Deviations from Plan

### Auto-fixed / adapted issues

**1. [Rule 1 - Bug] Patell/BMP/Sign belong to multi_event_statistics, not single**
- **Found during:** Task 1 (inspect-before-write end-to-end smoke test).
- **Issue:** The plan prose placed `PatellZTest`/`BMPTest` inside
  `SingleEventStatisticsSet`. Running that errors (`mutate` dispatched on a
  MarketModel R6 object) because Patell/BMP/Sign are multi-event statistics
  (`R/multi_event_test_statistics.R`).
- **Fix:** Attached them via `MultiEventStatisticsSet$new(tests=list(...))`,
  matching the canonical usage in `vignettes/articles/methods-test-statistics.Rmd`
  and `ai-advisor.Rmd`.

**2. [Rule 1 - Bug] Datasets are named lists, not flat data frames**
- **Issue:** Plan prose assumed `earnings_surprises`/`dieselgate` were flat
  frames with a `data`/`request` arg. They are lists of `$firm`/`$index`/
  `$request`/`$meta` tibbles. `EventStudyTask$new()` takes three positional
  tibbles `(firm_stock_data_tbl, reference_tbl, request_tbl)`.
- **Fix:** `EventStudyTask$new(ds$firm, ds$index, ds$request)` — the pattern used
  by the shipped ai-advisor vignette.

**3. [Rule 1 - Bug] `tidy.EventStudyTask` has no `type="caar"`; broom generic not re-exported**
- **Issue:** Plan used `tidy.EventStudyTask(result, type="caar")` via a bare
  `tidy()`. Valid types are `c("ar","car","aar","model")`; the `aar` output
  already carries `caar`, `caar_statistic`, `caar_p.value` columns. `tidy` is not
  attached, so it must be called as `EventStudy::tidy.EventStudyTask(x, ...)`.
- **Fix:** Regulatory CAAR table uses `EventStudy::tidy.EventStudyTask(result,
  type="aar")` sliced to the last window per group, plus `car_by_group()` summary.

**4. [Rule 1 - Bug] `simulate_event_study` signature and return type differ from plan**
- **Issue:** Plan used args `true_car`/`event_window`/`seed` and
  `plot_event_study(sim_result)`. The real signature uses `abnormal_return`
  (not `true_car`), `n_simulations`, `test_statistic`; the return is an
  `es_simulation` list (`power`, `rejection_by_day`, `test_stats`, `params`),
  NOT a fitted EventStudyTask — so `plot_event_study()` cannot consume it.
- **Fix:** Section 4 power sweep maps over `abnormal_return` and reads `$power`
  (real extracted values, verified gradient 0.035 → 0.96 at N=15). The RENDER-01
  plotly plot is built directly with `plotly::plot_ly()` as a power curve with an
  80% reference line. Extraction approach documented inline in the chunk.

**5. [Rule 1 - Bug] Gallery card href prefix would 404**
- **Issue:** Plan specified `href="articles/example-earnings.html"`. gallery.html
  renders at `docs/articles/gallery.html`, so a sibling article is reached by the
  bare `example-earnings.html` (matching every existing card in gallery.Rmd).
  The `articles/` prefix would produce `docs/articles/articles/...` (404).
- **Fix:** Used bare `href="example-<slug>.html"`.

**6. [Rule 3 - Blocking build fix] Wired 7 methods-* articles into _pkgdown.yml index**
- **Found during:** Task 4 Step A (first-ever full `build_site_github_pages`).
- **Issue:** Build aborted with "7 vignettes missing from index" — the Phase 15
  methods-* articles were only in the navbar dropdown menu, never in the
  `articles:` index. Phase 15 only ran per-article `build_article()`, so the full
  build never exercised the index validation; this latent defect surfaced at
  Phase 16's BUILD-04 gate.
- **Fix:** Added the 7 `articles/methods-*` slugs to the "Methods" articles-index
  group in `_pkgdown.yml` (an allowed file). Rebuild then completed with 0 errors.

**7. [Rule 1 - Bibliography key] Used real BMP citation key `BMP1991`**
- The plan template implied `@boehmer1991event`; the real key in
  `vignettes/articles/references.bib` is `@BMP1991`. Corrected so citations resolve.

### Plotly-gate note (non-deviation, design choice)

`plot_event_study(type="car"/"aar")` returns a **ggplot** (renders as `<img>`),
which would fail the RENDER-01 `plotly|htmlwidget` grep. Wrapping with
`plotly::ggplotly()` produces a genuine interactive plotly htmlwidget from the
real package output — satisfying both the letter of the gate and the intent
(interactive plots). `plotly` is already a hard dependency.

## Known Stubs

None. All live chunks execute real package API against bundled data /
`simulate_event_study()`; the M&A power table contains real extracted `$power`
values (not placeholders).

## BUILD-05 status: HUMAN_NEEDED

- **Verified locally:** `.github/workflows/pkgdown.yaml` is byte-unchanged and
  runs the identical `pkgdown::build_site_github_pages(new_process=FALSE,
  install=FALSE)` command that passed locally with 0 errors. `_pkgdown.yml` lists
  all three example slugs in the Worked Examples group, and the navbar `examples`
  menu links all three. The workflow discovers new articles automatically.
- **HUMAN_NEEDED (cannot be observed from here):** the live green GitHub Actions
  deploy requires the Phase 12 operator step (enable GitHub Pages + push to main).
  After that, confirm the pkgdown Actions run is green and the three articles are
  live:
  - https://sipemu.github.io/eventstudy/articles/example-earnings.html
  - https://sipemu.github.io/eventstudy/articles/example-regulatory.html
  - https://sipemu.github.io/eventstudy/articles/example-ma.html

## Deferred (out of scope)

- `EventStudy.Rcheck/` is tracked in git (committed in a prior phase) and its
  copies of DESCRIPTION/NAMESPACE/data appear in any diff against the
  milestone-start commit. It is a build artifact, not real source; not touched
  by Phase 16 and left as-is. Consider gitignoring it in a future cleanup.

## Self-Check: PASSED

- Commits `73a90b9`, `c81f6c3`, `688e1c7`, `1d443be` all present in git log.
- All 3 example Rmds and all 3 card SVGs (2048/1324/1419 bytes) exist on disk.
- Phase 16 Verification script: ALL CHECKS PASSED.
