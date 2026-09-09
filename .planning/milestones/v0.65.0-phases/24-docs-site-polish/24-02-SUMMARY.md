---
phase: 24-docs-site-polish
plan: 02
subsystem: docs
tags: [readme, pkgdown, vignette, ci, ecosystem]
requires: ["24-01"]
provides:
  - "README Ecosystem section + honest 15+/12 counts + pkgdown home markers (DOCS-02)"
  - "_pkgdown.yml format.* index fix + CI check_pkgdown step before Build site (DOCS-03)"
  - "introduction.Rmd getting-started flow with Quick Start + Next Steps cross-links (DOCS-04)"
affects:
  - README.md
  - _pkgdown.yml
  - vignettes/introduction.Rmd
  - .github/workflows/pkgdown.yaml
tech-stack:
  added: []
  patterns:
    - "pkgdown internal reference section catches starts_with(\"format.\") alongside print.*"
    - "CI validates cross-references via pkgdown::check_pkgdown() before the expensive site build"
    - "pkgdown home-marker HTML comments are inert on pkgdown 2.2.0 but document intent"
key-files:
  created:
    - .planning/phases/24-docs-site-polish/deferred-items.md
  modified:
    - README.md
    - _pkgdown.yml
    - vignettes/introduction.Rmd
    - .github/workflows/pkgdown.yaml
decisions:
  - "New README content kept strictly ASCII; pre-existing non-ASCII badge/prose bytes (6 lines) left untouched (out of scope, not CI-guarded)"
  - "Kolari-Pynnonen ASCII spelling used as the 12th test statistic to avoid the non-ASCII guard"
  - "Next Steps heading set to `## Next Steps` (h2) per acceptance criterion, replacing the h1 Roadmap section"
metrics:
  duration: ~12m
  completed: 2026-09-09
actuals:
  tokens: 2263
  tasks: 3
  commits: 3
status: complete
---

# Phase 24 Plan 02: Docs & Site Polish (Wave 2) Summary

Non-roxygen docs polish: README placed inside the three-tool eventstudy.de ecosystem with honest 15+/12 counts, the pre-existing `check_pkgdown()` failure fixed and wired into CI before the site build, and `introduction.Rmd` tightened into a self-navigating getting-started flow with a one-call Quick Start and Next Steps vignette cross-links. Docs-only; zero R/*.R and zero DESCRIPTION changes.

## What Was Built

### Task 1 (tracer): `_pkgdown.yml` format.* index fix — commit `5ac8a29`
Added `- starts_with("format.")` to the `internal` reference section immediately after `starts_with("print.")`. This routes the 6 exported `format.*` methods from Phase 23 (`format.Advice`, `format.EventStudySummary`, `format.es_advice`, `format.es_cross_sectional`, `format.es_diagnostics`, `format.es_simulation`) into the internal index. `pkgdown::check_pkgdown()` went from an ERROR (6 missing topics) to "No problems found." `home: sidebar: false` untouched. Verified end-to-end before expanding (tracer gate): committed and re-ran check_pkgdown clean.

### Task 2 (auto): README Ecosystem + honest counts — commit `1a5eb6c`
- Inserted an `## Ecosystem` section between the description paragraph and `## Installation`, wrapped in `<!-- pkgdown-home-start -->` / `<!-- pkgdown-home-end -->`, with the anchor "Event Study Analysis Made Simple", a link to https://eventstudy.de, and three ASCII-only bullets (Google Sheets Template / R Package / WebAssembly App).
- Reconciled stale counts: `**13 Return Models**` -> `**15+ Return Models**` (adding DCC-GARCH, Rolling-Window to the list) and `**11 Test Statistics**` -> `**12 Test Statistics**` (adding Kolari-Pynnonen, ASCII spelling).
- New content is ASCII-clean; the `+` keeps "15+" honest.

### Task 3 (auto): introduction.Rmd + CI check_pkgdown — commit `0587806`
- Retitled to "Getting Started with EventStudy"; collapsed the "Why Event Study?" / "Key Features" marketing sections into a two-paragraph intro.
- Added a `# Quick Start` section demonstrating the one-call `run_event_study()` shortcut before the manual 3-step walkthrough.
- Removed the stale "More a coming soon." sentence and rewrote the surrounding line to name the default multi-event statistics.
- Replaced the stale `# Roadmap` section with `## Next Steps` carrying `vignette()` cross-links to factor-models-bhar, inference-robustness, result-extraction, ai-advisor, and gallery.
- All code chunks remain `eval=FALSE`; the Rmd knits/parses cleanly.
- Added a `Check pkgdown configuration` step running `pkgdown::check_pkgdown()` with `shell: Rscript {0}`, positioned immediately before `Build site` (line 36 vs 39). Safe because Task 1 already fixed the format.* gap; `any::pkgdown` already in extra-packages (no dependency change).

## Verification

- `pkgdown::check_pkgdown()`: "No problems found." (0 missing topics, 0 broken references)
- README: `**15+ Return Models**` and `**12 Test Statistics**` present; `13 Return Models` / `11 Test Statistics` absent; Ecosystem/eventstudy.de/home-markers present; new content ASCII-clean
- introduction.Rmd: title updated, "More a coming soon" gone, `## Next Steps` + 5 cross-links present, `run_event_study(` Quick Start present, knits without error
- CI: `check_pkgdown()` step is before `Build site` (36 < 39)
- Full testthat suite: PASS=2359, FAIL=0 (4 warnings, 29 skips — pre-existing baseline; the "provider error" line is an expected message from a deliberate-failure test)
- `git diff DESCRIPTION`: 0 lines (no new Imports/Suggests)
- Changed files: exactly the 4 planned disjoint files; no R/*.R touched

## Deviations from Plan

### Scope note (not a code deviation)
The plan's Task 2 `<verify>` used `! grep -rlP "[^\x00-\x7F]" README.md` (whole-file ASCII check). README already carries 6 pre-existing non-ASCII lines (R-badge `>=` glyph, em-dashes, smart quotes) from earlier phases. The actual CI non-ASCII guard lives in `.github/workflows/R-CMD-check.yaml` and is scoped to `R/`, `man/figures/`, `inst/` only — it does NOT cover README.md. Per SCOPE BOUNDARY, pre-existing non-ASCII is out of scope and stripping badge glyphs could regress rendering. Resolution: kept all NEW README content strictly ASCII (Ecosystem section + edited count lines verified byte-clean) and left the pre-existing bytes untouched. All acceptance-criteria intent (ASCII-safe new content, honest counts, Ecosystem block) is met.

### Auto-fixed Issues
None — no Rule 1/2/3 fixes were needed; all files behaved as the plan described.

## Deferred Issues

- **Test-artifact hygiene (out of scope):** The report-template render path writes transient `file*.log` files into `inst/rmarkdown/templates/event_study_report/skeleton/` during the test suite; these are not gitignored. Two such files appeared after the regression run and were removed before finishing (never committed). Logged to `deferred-items.md`. Fixing this requires a `.gitignore` or template change, out of scope for a docs-only plan.

## Known Stubs

None.

## Threat Flags

None — docs-only; no new network endpoints, auth paths, file-access patterns, or schema changes. T-24-03 (CI DoS) mitigated by delivering the `_pkgdown.yml` fix (Task 1) before the CI check step (Task 3). T-24-04 (README encoding) mitigated by ASCII-only new content + Kolari-Pynnonen ASCII spelling.

## Self-Check: PASSED
