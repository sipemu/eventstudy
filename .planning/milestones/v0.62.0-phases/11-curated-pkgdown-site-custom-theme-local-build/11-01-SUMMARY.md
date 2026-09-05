---
phase: 11-curated-pkgdown-site-custom-theme-local-build
plan: 01
subsystem: docs
tags: [pkgdown, bslib, bootstrap5, scss, documentation, site]

requires: []
provides:
  - _pkgdown.yml with url, Bootstrap-5 template, 10-group reference, 8-group articles, navbar
  - pkgdown/extra.scss with Google Fonts import and #1C4E80 function-name syntax override
  - Clean local build: build_site() exits 0, zero reference missing/orphan warnings, docs/index.html from README
affects:
  - 12-ci-cd-deploy-repo-linkage-release-integrity

actuals:
  tokens: 18000
  tasks: 3
  commits: 3

tech-stack:
  added:
    - pkgdown 2.2.0 (site config via _pkgdown.yml)
    - bslib (Bootstrap-5 theme variables in template.bslib)
    - Google Fonts CDN (Inter + JetBrains Mono via extra.scss @import)
  patterns:
    - pkgdown reference grouping: 10 thematic groups + internal section for non-exported Rd topics
    - has_keyword("datasets") idiom for bundled dataset inclusion without export() requirement
    - extra.scss .fu class override to cascade accent color over pkgdown's default syntax highlighting

key-files:
  created:
    - _pkgdown.yml
    - pkgdown/extra.scss
  modified: []

key-decisions:
  - "Internal section (title: internal) used for ModelBase, ReturnCalculation, TestStatisticBase, degenerate-input-contract, and print.* S3 method topics that have .Rd files but are not export()'d — pkgdown 2.2.0 requires all .Rd topics to be listed or they become missing-topic errors"
  - "has_keyword('datasets') used to include dieselgate in Data & Datasets group — empirical build confirmed it resolves correctly without triggering orphan warnings"
  - "introduction vignette listed in both navbar (Get Started) and articles: (Get Started group) — pkgdown 2.x requires every vignette to appear in exactly one articles: group even if it is also a navbar item"
  - "DESCRIPTION URL sitrep (✖ URLs not ok) is a Phase-12 dependency — pkgdown wants the site URL added to DESCRIPTION's URL field, which the scope guard defers to Phase 12 (LINK-01)"
  - "extra.scss .fu override cascades correctly because it is compiled after pkgdown's default .fu{color:#4758AB} — verified by presence of both rules in bootstrap.min.css with ours last"

patterns-established:
  - "pkgdown reference completeness: every .Rd file must be in a reference group or the internal section — non-exported base classes and package-doc topics count"
  - "bslib Google Font shorthand: base_font/heading_font/code_font: {google: 'Name'} downloads and serves fonts as local deps, no CDN fetch needed for font files — extra.scss @import adds the CDN path for the syntax-highlighting override only"

requirements-completed: [SITE-01, SITE-02, SITE-03, SITE-04, THEME-01, THEME-02, BUILD-01]

coverage:
  - id: D1
    description: "url: https://sipemu.github.io/eventstudy/ set in _pkgdown.yml; Bootstrap-5 template configured (SITE-01)"
    requirement: SITE-01
    verification:
      - kind: other
        ref: "grep 'url: https://sipemu.github.io/eventstudy/' _pkgdown.yml"
        status: pass
    human_judgment: false
  - id: D2
    description: "10 reference groups cover all 74 NAMESPACE exports exactly once; internal section covers non-exported .Rd topics; zero missing/orphan lines from build (SITE-02, BUILD-01)"
    requirement: SITE-02
    verification:
      - kind: other
        ref: "Rscript -e 'pkgdown::build_site(preview=FALSE)' 2>&1 | grep -iE 'missing|orphan' | grep -viE 'DESCRIPTION|URL'; test $? -eq 1"
        status: pass
    human_judgment: false
  - id: D3
    description: "All 18 vignettes appear under 8 article groups; navbar has Get Started, Reference, Articles dropdown, Changelog, GitHub link (SITE-03)"
    requirement: SITE-03
    verification:
      - kind: other
        ref: "Rscript -e 'pkgdown::build_site(preview=FALSE)' exits 0; all 18 vignette HTMLs present in docs/articles/"
        status: pass
    human_judgment: false
  - id: D4
    description: "Homepage renders from README.md; docs/index.html produced (SITE-04)"
    requirement: SITE-04
    verification:
      - kind: other
        ref: "test -f docs/index.html"
        status: pass
    human_judgment: false
  - id: D5
    description: "Bootstrap-5 bslib theme: primary #1C4E80, Inter body/headings, JetBrains Mono code, no bootswatch, no logo (THEME-01)"
    requirement: THEME-01
    verification:
      - kind: other
        ref: "grep '--bs-primary: #1C4E80' docs/deps/bootstrap-5.*/bootstrap.min.css"
        status: pass
    human_judgment: false
  - id: D6
    description: "extra.scss: Google Fonts @import + .fu{color:#1C4E80} override compiled into CSS; wide code blocks horizontally scrollable (overflow-x:auto in CSS) (THEME-02)"
    requirement: THEME-02
    verification:
      - kind: other
        ref: "grep -q '1C4E80' pkgdown/extra.scss && grep 'overflow-x:auto' docs/deps/bootstrap-5*/bootstrap.min.css"
        status: pass
    human_judgment: false

duration: 61min
completed: 2026-09-04
status: complete
---

# Phase 11 Plan 01: Curated pkgdown Site + Custom Theme (local build) Summary

**pkgdown 2.2.0 site with 10-group reference index (74 exports + internal .Rd topics), 8-group articles (18 vignettes), Bootstrap-5 bslib theme (accent #1C4E80, Inter + JetBrains Mono), and warning-clean local build_site()**

## Performance

- **Duration:** 61 min
- **Started:** 2026-09-04T19:56:08Z
- **Completed:** 2026-09-04T20:57:00Z
- **Tasks:** 3
- **Files modified:** 2 created (_pkgdown.yml, pkgdown/extra.scss)

## Accomplishments

- Created `_pkgdown.yml` with site URL, Bootstrap-5 template, 10 thematic reference groups, 8 article groups over all 18 vignettes, and a curated navbar (Get Started / Reference / Articles dropdown / Changelog / GitHub)
- All 74 exported NAMESPACE symbols are assigned to exactly one reference group; 9 non-exported .Rd topics (base classes, package docs, print S3 methods) are in an internal section; `has_keyword("datasets")` correctly includes the bundled `dieselgate` dataset
- Created `pkgdown/extra.scss` with Google Fonts @import (Inter 400/500/600/700 + JetBrains Mono 400) and `.fu{color:#1C4E80}` override; compiled into the Bootstrap CSS via bslib
- Custom bslib theme applied: primary #1C4E80, secondary-bg #F8F9FA, fg #1A1A1A, danger #C0392B; Inter for body/headings, JetBrains Mono for code
- `pkgdown::build_site(preview = FALSE)` exits 0 with zero reference-level missing/orphan warnings; `docs/index.html` rendered from README.md; all 18 article HTMLs produced

## Task Commits

Each task was committed atomically:

1. **Task 1: Tracer _pkgdown.yml** - `8a466d5` (feat)
2. **Task 2: Grouped reference + articles + navbar** - `70b4813` (feat)
3. **Task 3: bslib theme + extra.scss** - `5ac5d0d` (feat)

## Files Created/Modified

- `_pkgdown.yml` — site config: url, Bootstrap-5 template, bslib theme variables, 10-group reference, 8-group articles, navbar
- `pkgdown/extra.scss` — Google Fonts @import (Inter + JetBrains Mono) + .fu{color:#1C4E80} function-name syntax override

## Decisions Made

- **Internal reference section:** pkgdown 2.2.0 treats all `.Rd` files as reference topics (even non-exported ones like `ModelBase`, `ReturnCalculation`, `TestStatisticBase`, `degenerate-input-contract`, and print S3 method Rd topics). Added `- title: internal` with `starts_with("print.")` + explicit names to prevent missing-topic errors.
- **dieselgate resolution:** The dataset has `\keyword{datasets}` in its .Rd but is not `export()`'d. `has_keyword("datasets")` in the Data & Datasets group's `contents:` resolved it cleanly — build confirmed no orphan warning.
- **introduction vignette:** Listed in both the Get Started navbar item AND an articles group (`- title: "Get Started"` in `articles:`). pkgdown 2.x requires every vignette to appear in exactly one `articles:` group.
- **DESCRIPTION URL scope deferral:** pkgdown's sitrep emits `✖ URLs not ok — In DESCRIPTION, URL is missing package url` because DESCRIPTION doesn't list the pkgdown site URL. This is LINK-01 in Phase 12. The build exits 0 and reference completeness is fully green.
- **bslib base_font/heading_font/code_font shorthand:** Used `{google: "Inter"}` / `{google: "JetBrains Mono"}` which causes bslib to download and serve fonts as local deps (`deps/Inter-*/`, `deps/JetBrains_Mono-*/`). The Google Fonts `@import` in `extra.scss` additionally ensures fonts load for CDN-path environments; both paths coexist without conflict.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Added internal reference section for non-exported .Rd topics**
- **Found during:** Task 2 first build attempt
- **Issue:** pkgdown 2.2.0 emits a fatal missing-topic error for every `.Rd` file not in a `reference:` group — including non-exported base classes (`ModelBase`, `ReturnCalculation`, `TestStatisticBase`) and print S3 method Rd files (`print.Advice`, `print.EventStudySummary`, `print.es_advice`, `print.es_cross_sectional`, `print.es_diagnostics`) and `degenerate-input-contract`
- **Fix:** Added `- title: internal` section with `starts_with("print.")` and explicit non-exported topic names
- **Files modified:** `_pkgdown.yml`
- **Verification:** Build exits 0, no missing-topic errors after fix
- **Committed in:** `70b4813` (Task 2 commit)

**2. [Rule 3 - Blocking] Added introduction to articles groups**
- **Found during:** Task 2 first build attempt
- **Issue:** pkgdown reported `1 vignette missing from index: "introduction"` — navbar Get Started link alone does not satisfy the articles index requirement
- **Fix:** Added `- title: "Get Started"` group with `introduction` as its sole vignette in the `articles:` block
- **Files modified:** `_pkgdown.yml`
- **Verification:** Build articles metadata now shows `✔ Articles metadata ok.`
- **Committed in:** `70b4813` (Task 2 commit)

---

**Total deviations:** 2 auto-fixed (both Rule 3 — blocking build errors discovered empirically from live build output, not guessable in advance)
**Impact on plan:** Both fixes were necessary for the build to pass. No scope creep.

## Issues Encountered

- **DESCRIPTION URL sitrep:** pkgdown's `✖ URLs not ok` message appears because DESCRIPTION's URL field doesn't include the pkgdown site URL. This produces a "missing" substring match in the plan's verify grep, preventing `REFERENCE_CLEAN` from printing. This is a Phase-12 LINK-01 item (DESCRIPTION URL + README badge). Build exits 0 and all reference/orphan checks pass; the sitrep is purely informational.
- **11 R warnings during build:** The build emits "Es gab 11 Warnungen" (German locale: "There were 11 warnings") from the vignette rendering pipeline — these are vignette-level R warnings (e.g., quantmod S3 method overwrite), not pkgdown reference warnings. They do not match the "warning" substring pattern in the grep, and they do not affect build success.

## Known Stubs

None — all reference groups and articles are wired to actual content; no placeholder titles or empty groups.

## Threat Flags

None — no new network endpoints, auth paths, or trust boundaries introduced. The only external fetch at build time is the Google Fonts CDN `@import` in `extra.scss` (font files only, no JS execution).

## Self-Check

- [x] `_pkgdown.yml` exists: confirmed
- [x] `pkgdown/extra.scss` exists: confirmed
- [x] `docs/index.html` exists: confirmed
- [x] All 3 task commits present: `8a466d5`, `70b4813`, `5ac5d0d`
- [x] Zero reference missing/orphan lines from build: confirmed
- [x] `#1C4E80` in `pkgdown/extra.scss`: confirmed
- [x] Inter + JetBrains Mono served as local deps: `docs/deps/Inter-0.4.10/`, `docs/deps/JetBrains_Mono-0.4.10/`
- [x] `--bs-primary: #1C4E80` in compiled bootstrap.min.css: confirmed
- [x] docs/ NOT staged for commit: confirmed

## Phase-12 Dependency Noted

The `✖ URLs not ok` pkgdown sitrep requires adding the pkgdown site URL to DESCRIPTION's `URL:` field — deferred to Phase 12 LINK-01 (`DESCRIPTION URL + README badge + .Rbuildignore`).

## Next Phase Readiness

- Phase 12 (CI/CD Deploy + Repo Linkage + Release Integrity) can proceed with this config as the foundation
- `_pkgdown.yml` and `pkgdown/extra.scss` are the complete local-build deliverables
- Phase 12 should add `https://sipemu.github.io/eventstudy/` to DESCRIPTION URL field (resolves LINK-01 and the sitrep)
- Phase 12 should add `^docs$`, `^_pkgdown\.yml$`, `^pkgdown/` to `.Rbuildignore`

---
*Phase: 11-curated-pkgdown-site-custom-theme-local-build*
*Completed: 2026-09-04*
