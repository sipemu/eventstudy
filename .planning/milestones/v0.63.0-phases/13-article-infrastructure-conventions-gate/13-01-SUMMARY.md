---
phase: 13-article-infrastructure-conventions-gate
plan: "01"
subsystem: documentation
tags: [pkgdown, katex, plotly, citation, vignettes, article-infrastructure]
status: complete

dependency_graph:
  requires: []
  provides:
    - vignettes/articles/_setup.Rmd
    - vignettes/articles/references.bib
    - vignettes/articles/_article-skeleton.Rmd
    - vignettes/articles/smoke-test.Rmd
    - _pkgdown.yml (math-rendering: katex, Methods/Gallery navbar, Methods articles index)
    - .Rbuildignore (^vignettes/articles$)
  affects:
    - pkgdown site build
    - CRAN source tarball

tech_stack:
  patterns:
    - "pkgdown 2.2.0 template.math-rendering: katex (NOT top-level) → KaTeX JS injected, no MathJax"
    - "knitr child chunk pattern: {r child='_setup.Rmd'} with include=FALSE in child"
    - "pandoc-citeproc resolves [@Key] when .bib is co-located in same directory as .Rmd"
    - "plotly::ggplotly() wraps ggplot2 output since plot_event_study() now returns ggplot, not plotly"

key_files:
  created:
    - vignettes/articles/_setup.Rmd
    - vignettes/articles/references.bib
    - vignettes/articles/_article-skeleton.Rmd
    - vignettes/articles/smoke-test.Rmd
  modified:
    - _pkgdown.yml
    - .Rbuildignore

decisions:
  - "template.math-rendering: katex goes under template: in _pkgdown.yml (not top-level) — pkgdown 2.2.0 reads it via config_math_rendering() from template.math-rendering key"
  - "plot_event_study() returns ggplot2 object in current codebase; used plotly::ggplotly() to satisfy plotly-presence requirement"
  - "smoke-test.Rmd uses event_id=1 for CAR plot (VW, the primary Dieselgate event)"

metrics:
  duration: "~45 minutes"
  completed: "2026-09-05"
  tasks_completed: 3
  commits: 1

actuals:
  tokens: 14000
  tasks: 3
  commits: 1
---

# Phase 13 Plan 01: Article Infrastructure & Conventions Gate Summary

Article infrastructure for pkgdown-delivered Methods articles established: shared child chunk, co-located bibliography, 10-section skeleton, and smoke-test article with KaTeX math + resolved citation + plotly figure on one page — no MathJax collision.

## Tasks Completed

| Task | Name | Commit | Files |
|------|------|--------|-------|
| 1 (tracer) | Smoke-test article renders with KaTeX + citation + plotly | 9c84b60 | smoke-test.Rmd, _setup.Rmd, references.bib, _pkgdown.yml |
| 2 | Wire Methods dropdown + Gallery navbar + articles index | 9c84b60 | _pkgdown.yml, _article-skeleton.Rmd |
| 3 | Tarball-exclusion gate + CRAN-vignette regression guard | 9c84b60 | .Rbuildignore |

All three tasks committed atomically in one commit (DELIVERY-01 requirement).

## Verification Results

### Task 1 (Tracer)
- `docs/articles/smoke-test.html` exists: PASS
- KaTeX assets in smoke-test.html (`grep -c 'katex'`): 2 — PASS
- No raw `$...$` math passthrough: PASS (math rendered as MathML by pandoc, then KaTeX via site)
- Citation resolved: "(MacKinlay 1997)" in body; only `id="ref-MacKinlay1997"` anchor in bib section — PASS
- MathJax scripts: 0 — PASS
- Plotly count: 6 — PASS

### Task 2
- `pkgdown::build_site()` completed with no missing-topic/orphan/error lines — PASS
- Methods and Gallery in navbar (`docs/index.html`): PASS
- Get Started / Reference / Articles intact (count=3): PASS
- `_setup.html` and `_article-skeleton.html` NOT rendered: SKELETON_EXCLUDED — PASS

### Task 3
- `R CMD build` tarball excludes `vignettes/articles/`: ARTICLES_EXCLUDED OK — PASS
- CRAN vignettes in tarball (`cran_vignettes`): 19 — PASS
- `git diff -- 'vignettes/*.Rmd'` empty: VIGNETTES_UNCHANGED — PASS
- `.Rbuildignore` contains exact `^vignettes/articles$`: RULE_PRESENT_ANCHORED — PASS

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] math-rendering key location**
- **Found during:** Task 1 verification (KaTeX count = 0 after build)
- **Issue:** RESEARCH.md stated `math-rendering: katex` as a top-level `_pkgdown.yml` key, but pkgdown 2.2.0 reads it from `template.math-rendering` (via `config_math_rendering()` which calls `config_pluck_string(pkg, "template.math-rendering")`)
- **Fix:** Moved key under `template:` block: `template.math-rendering: katex`
- **Files modified:** `_pkgdown.yml`
- **Commit:** 9c84b60

**2. [Rule 2 - Missing functionality] plotly::ggplotly() wrapper**
- **Found during:** Task 1 verification (plotly count = 0)
- **Issue:** RESEARCH draft assumed `plot_event_study()` returns a plotly object, but current codebase returns ggplot2 for all types (ar/car/aar/caar)
- **Fix:** Wrapped `plot_event_study()` result with `plotly::ggplotly()` in smoke-test.Rmd
- **Files modified:** `vignettes/articles/smoke-test.Rmd`
- **Commit:** 9c84b60

## Known Stubs

None — smoke-test drives real dieselgate data through the full pipeline.

## Self-Check: PASSED

- `vignettes/articles/_setup.Rmd`: FOUND
- `vignettes/articles/references.bib`: FOUND
- `vignettes/articles/_article-skeleton.Rmd`: FOUND
- `vignettes/articles/smoke-test.Rmd`: FOUND
- `docs/articles/smoke-test.html`: FOUND
- Commit `9c84b60`: FOUND
- `.Rbuildignore` contains `^vignettes/articles$`: FOUND
