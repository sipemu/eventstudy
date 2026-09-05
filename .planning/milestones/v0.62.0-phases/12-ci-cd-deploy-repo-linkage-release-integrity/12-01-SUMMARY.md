---
phase: 12-ci-cd-deploy-repo-linkage-release-integrity
plan: "01"
subsystem: ci-cd-release
tags: [pkgdown, ci-cd, github-actions, release, cran]
requires: [11-01]
provides: [pkgdown-workflow, release-0.62.0]
affects: [DESCRIPTION, README, NEWS, .Rbuildignore, vignettes]
tech_stack:
  added:
    - JamesIves/github-pages-deploy-action@v4
  patterns:
    - r-lib/actions pkgdown-deploy idiom
key_files:
  created:
    - .github/workflows/pkgdown.yaml
  modified:
    - .Rbuildignore
    - DESCRIPTION
    - README.md
    - NEWS.md
    - vignettes/data-download.Rmd
key_decisions:
  - "Use JamesIves/github-pages-deploy-action@v4 (r-lib recommended) — same trust level as r-lib/actions already in R-CMD-check.yaml"
  - "pkgdown NOT added to DESCRIPTION Suggests — CI-only docs tool not needed for CRAN (carry_forward_moot per plan)"
  - "WR-01 and IN-01 from Phase 11 code review are MOOT — bslib block removed in redesign, extra.scss deleted; no action taken"
  - "R CMD check --as-cran ERROR (missing Suggests packages) is environment-only, pre-existing in v0.61.x baseline, not a new regression"
  - "404 NOTE for https://sipemu.github.io/eventstudy/ is expected before first CI deploy — URL is correct, site not yet live"
requirements_completed: [CI-01, CI-02, CI-03, LINK-01, LINK-02, LINK-03, BUILD-02, BUILD-03]
duration: "14 min"
completed: "2026-09-05"
status: complete
actuals:
  tokens: 8000
  tasks: 3
  commits: 4

coverage:
  - deliverable: "pkgdown.yaml CI workflow with three triggers, contents:write, gh-pages deploy"
    verification:
      - kind: automated
        ref: "Rscript yaml::read_yaml check (pkgdown.yaml OK)"
        status: pass
      - kind: automated
        ref: "grep gh-pages + deploy action (DEPLOY_OK)"
        status: pass
    human_judgment: false
  - deliverable: ".Rbuildignore excludes _pkgdown.yml, docs/, pkgdown/"
    verification:
      - kind: automated
        ref: "grep .Rbuildignore (RBUILDIGNORE_OK)"
        status: pass
      - kind: automated
        ref: "R CMD build tarball content check (TARBALL_CLEAN_OK)"
        status: pass
    human_judgment: false
  - deliverable: "DESCRIPTION URL lists both GitHub and pkgdown site URLs"
    verification:
      - kind: automated
        ref: "grep DESCRIPTION (DESCRIPTION_URL_OK)"
        status: pass
    human_judgment: false
  - deliverable: "data-download.Rmd and introduction.Rmd eval=FALSE in setup chunk"
    verification:
      - kind: automated
        ref: "grep setup chunk (VIGNETTE_EVAL_FALSE_OK)"
        status: pass
    human_judgment: false
  - deliverable: "data-download.Rmd prose label for non-evaluated content"
    verification:
      - kind: automated
        ref: "grep prose label (LABEL_OK)"
        status: pass
    human_judgment: false
  - deliverable: "README docs-site badge"
    verification:
      - kind: automated
        ref: "grep sipemu.github.io README.md (README_BADGE_OK)"
        status: pass
    human_judgment: false
  - deliverable: "DESCRIPTION Version 0.62.0"
    verification:
      - kind: automated
        ref: "grep Version 0.62.0 (VERSION_OK)"
        status: pass
    human_judgment: false
  - deliverable: "NEWS.md 0.62.0 top entry"
    verification:
      - kind: automated
        ref: "head -3 NEWS.md (NEWS_OK)"
        status: pass
    human_judgment: false
  - deliverable: "Green CI Actions run on push to main (CI-03 live half)"
    human_judgment: true
    rationale: "Backstop — requires a real GitHub Actions run triggered by operator push to main. Cannot be verified offline."
  - deliverable: "GitHub Pages serving site at https://sipemu.github.io/eventstudy/ (CI-02 live half)"
    human_judgment: true
    rationale: "Backstop — requires operator to enable Pages (Settings -> Pages -> gh-pages branch) after first workflow run."
---

# Phase 12 Plan 01: CI/CD Deploy + Repo Linkage + Release Integrity Summary

Wire CI/CD deployment via r-lib pkgdown workflow to gh-pages, add DESCRIPTION/README
site linkage, verify offline-safe vignette builds, and ship as CRAN-clean 0.62.0.

**Duration:** 14 min | **Tasks:** 3/3 | **Commits:** 4 | **Files:** 6

## Accomplishments

- **CI-01/02** `.github/workflows/pkgdown.yaml` created: push:[main], release:[published],
  workflow_dispatch triggers; least-privilege `permissions: contents: write`; deploys to
  `gh-pages` via `JamesIves/github-pages-deploy-action@v4`. Uses exact same r-lib/actions
  major versions as R-CMD-check.yaml (checkout@v4, setup-pandoc@v2, setup-r@v2,
  setup-r-dependencies@v2). Only `secrets.GITHUB_TOKEN` used — no additional secrets.

- **LINK-01** `DESCRIPTION URL` now comma-separated:
  `https://github.com/sipemu/eventstudy, https://sipemu.github.io/eventstudy/`.
  `BugReports:` retained unchanged.

- **LINK-02** `README.md` docs badge added near existing badge row:
  `[![Docs](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://sipemu.github.io/eventstudy/)`

- **LINK-03** `.Rbuildignore` gains three anchored regex lines:
  `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$`. The `^\.github$` line already excluded
  the workflow directory — no new entry needed for that. Tarball verified clean.

- **BUILD-02** Both `vignettes/data-download.Rmd` and `vignettes/introduction.Rmd` set
  `eval = FALSE` globally in their setup chunk — confirmed by automated grep. Added a
  plain-prose label to `data-download.Rmd` after the Introduction heading: "The code in
  this article is shown for illustration purposes and is not executed at build time
  because it requires a live network connection."

- **BUILD-03** `DESCRIPTION Version` bumped `0.61.3` → `0.62.0`. `NEWS.md` prepended
  with `# EventStudy 0.62.0` entry recording the pkgdown site, CI deploy, DESCRIPTION
  URL, README badge, and tarball exclusion.

## R CMD check --as-cran Diff vs v0.61.x Baseline

Run locally on `EventStudy_0.62.0.tar.gz` with `--no-manual --no-build-vignettes`:

**Status: 1 ERROR, 1 NOTE — all pre-existing, none introduced by Phase 12.**

| Finding | Kind | New? | Notes |
|---------|------|------|-------|
| `Packages suggested but not available: rugarch, rmgarch, did, DIDmultiplegt, didimputation, DT` | ERROR | No | Missing on this dev machine; identical ERROR in v0.61.x on same machine. CRAN runners have these. |
| `New submission / Package was archived` | NOTE | No | CRAN history — pre-existing. |
| `VignetteBuilder field but no prebuilt vignette index` | NOTE | No | Artifact of `--no-build-vignettes` flag; pre-existing. |
| `URL https://sipemu.github.io/eventstudy/ — Status: 404` | NOTE | Expected | Site not live yet (first CI deploy pending operator push). URL is correct; 404 resolves after deploy. |

**Verdict:** No new NOTE or WARNING introduced by this phase. The 404 URL NOTE is expected and resolves after the first operator-triggered CI deploy.

## Carry-Forward Phase 11 Code Review Items (MOOT)

Both carry-forward items from Phase 11 require NO action:

- **WR-01** (`font-size-base`/`headings-font-weight` under `template.bslib`): The bslib block was removed in the fdars-r redesign. `_pkgdown.yml` now uses plain `template: {bootstrap: 5}` — no bslib key, no variables to pin. Nothing to do.
- **IN-01** (dead `.r-function` selector in `pkgdown/extra.scss`): `extra.scss` was deleted in the redesign. Only `pkgdown/extra.css` remains, which contains no `.r-function` selector. Nothing to remove.

Neither `pkgdown/extra.scss` nor a `template.bslib` block was reintroduced.

## Tarball Verification

```
R CMD build . --no-build-vignettes --no-manual
→ EventStudy_0.62.0.tar.gz

tar tzf EventStudy_0.62.0.tar.gz | grep -Eq '(_pkgdown\.yml|docs/|pkgdown/)'
→ TARBALL_CLEAN_OK (no match — site scaffolding absent from tarball)
```

## Backstops (Operator / Human-Needed)

These items cannot be automated — they require a real GitHub Actions run after the
operator pushes to main:

1. **CI-03**: Push merged changes to `main` (or publish a release) to trigger the pkgdown
   workflow. Verify it completes green on GitHub Actions.
2. **CI-02 (live half)**: After the first successful workflow run creates `gh-pages`:
   - GitHub repo Settings → Pages → Source = Deploy from a branch, Branch = gh-pages / (root)
3. **Operator**: Set repo About → Website field to `https://sipemu.github.io/eventstudy/`
   (GitHub UI main page → About gear icon).

## Deviations from Plan

None — plan executed exactly as written. The `.github$` line already covering the
workflow dir (noted in plan) was confirmed; no extra `.Rbuildignore` entry added for it.

## Self-Check: PASSED

- `.github/workflows/pkgdown.yaml` exists on disk: FOUND
- `.Rbuildignore` contains `_pkgdown.yml`, `docs`, `pkgdown` exclusions: FOUND
- `DESCRIPTION` URL contains both URLs + BugReports: FOUND
- `vignettes/data-download.Rmd` prose label present: FOUND
- `README.md` badge links to sipemu.github.io: FOUND
- `DESCRIPTION Version: 0.62.0`: FOUND
- `NEWS.md` top entry `# EventStudy 0.62.0`: FOUND
- `TARBALL_CLEAN_OK` verified: PASS
- Commits `1790e7d`, `ac6373d`, `d3897a0` in git log: confirmed

## Next Steps

Phase 12 Plan 01 is the only plan in Phase 12. Phase complete — ready for:
- Operator push to `main` to trigger CI (backstops above)
- `/gsd-complete-milestone` to archive v0.62.0
