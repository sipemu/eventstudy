---
phase: 12-ci-cd-deploy-repo-linkage-release-integrity
verified: 2026-09-05T00:00:00Z
status: human_needed
score: 7/9 must-haves verified
behavior_unverified: 0
overrides_applied: 0
backstop_items:
  - truth: "A push to main triggers the pkgdown workflow, which installs the package with its Suggests, builds articles, and completes green on GitHub Actions (CI-01/CI-03 live half)"
    verification: backstop
    why: "Requires a real GitHub Actions run after operator pushes to main — not verifiable from local repo"
  - truth: "GitHub Pages serves the site at https://sipemu.github.io/eventstudy/ after first successful deploy (CI-02 live half)"
    verification: backstop
    why: "Requires operator to enable Pages (Settings -> Pages -> gh-pages branch) after first workflow run"
human_verification:
  - test: "Push to main and confirm GitHub Actions pkgdown workflow run completes green"
    expected: "All steps pass — checkout, pandoc, R setup, dependency install (with Suggests), pkgdown::build_site_github_pages, JamesIves deploy to gh-pages"
    why_human: "CI execution cannot be verified from local repo state"
  - test: "Enable GitHub Pages (Settings -> Pages -> Source = gh-pages branch) and confirm site loads at https://sipemu.github.io/eventstudy/"
    expected: "Site renders the pkgdown homepage, navbar, Reference, and Articles sections"
    why_human: "Requires operator GitHub UI action + live network check after first deploy"
  - test: "Confirm R CMD check --as-cran on EventStudy_0.62.0.tar.gz shows no new NOTEs/WARNINGs vs v0.61.x baseline and test suite is green"
    expected: "No new findings beyond the pre-existing ERROR (missing optional Suggests on this dev machine) and pre-existing NOTEs"
    why_human: "R CMD check cannot run in this verifier environment; SUMMARY records a plausible diff but it must be confirmed on a clean CI runner or a machine with all Suggests installed"
---

# Phase 12: CI/CD Deploy + Repo Linkage + Release Integrity — Verification Report

**Phase Goal:** The site that builds locally in Phase 11 is now built and deployed to GitHub Pages by CI on every push to `main` and on releases, the repo links to the live site, network-touching vignettes build reproducibly in Actions, and the whole change ships as a CRAN-clean 0.62.0 release that leaves the source tarball unchanged.

**Verified:** 2026-09-05
**Status:** human_needed
**Re-verification:** No — initial verification

---

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
|---|-------|--------|----------|
| 1 | pkgdown.yaml exists, is valid YAML, declares push:[main] + release:[published] + workflow_dispatch triggers (CI-01) | VERIFIED | File present at `.github/workflows/pkgdown.yaml`; all three trigger blocks confirmed by direct read |
| 2 | Workflow declares least-privilege `permissions: contents: write` and a gh-pages deploy step (CI-02 local half) | VERIFIED | Line 10-11: `permissions: contents: write`; lines 39-46: `JamesIves/github-pages-deploy-action@v4` with `branch: gh-pages` |
| 3 | A push to main triggers the workflow, completes green on GitHub Actions (CI-03 live half) | BACKSTOP | Cannot verify without a real Actions run — operator must push and confirm |
| 4 | GitHub Pages serves the site at https://sipemu.github.io/eventstudy/ (CI-02 live half) | BACKSTOP | Cannot verify without live deployment — operator must enable Pages after first run |
| 5 | DESCRIPTION URL contains both GitHub URL and https://sipemu.github.io/eventstudy/; BugReports retained (LINK-01) | VERIFIED | Line 26: `URL: https://github.com/sipemu/eventstudy, https://sipemu.github.io/eventstudy/`; line 27: `BugReports: https://github.com/sipemu/eventstudy/issues` |
| 6 | README shows a docs-site badge/link near the badge row (LINK-02) | VERIFIED | Line 11 of README.md: `[![Docs](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://sipemu.github.io/eventstudy/)` — in the badge row |
| 7 | .Rbuildignore excludes `_pkgdown.yml`, `docs/`, `pkgdown/` (LINK-03 local half) | VERIFIED | Lines 13-15 of .Rbuildignore: `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$` — all three anchored regex entries present |
| 8 | Network-touching vignettes set eval=FALSE in setup chunk; data-download.Rmd has a prose non-execution label (BUILD-02) | VERIFIED | `data-download.Rmd` setup chunk (lines 12-18): `eval = FALSE`; prose label line 24: "The code in this article is shown for illustration purposes and is not executed at build time because it requires a live network connection." `introduction.Rmd` setup chunk (lines 12-18): `eval = FALSE` |
| 9 | DESCRIPTION Version is 0.62.0; NEWS.md top entry is `# EventStudy 0.62.0` recording the docs site (BUILD-03 local half) | VERIFIED | DESCRIPTION line 4: `Version: 0.62.0`; NEWS.md lines 1-3: `# EventStudy 0.62.0` + `## Documentation site + CI/CD deploy` |

**Score:** 7/9 truths verified in-repo; 2 are backstops requiring operator action (not failures — by design).

---

## Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `.github/workflows/pkgdown.yaml` | NEW — r-lib pkgdown deploy workflow | VERIFIED | Exists; valid YAML; correct triggers, permissions, steps |
| `DESCRIPTION` (URL + Version 0.62.0) | Two-URL comma-separated URL field; version bumped | VERIFIED | URL line 26 has both; Version line 4 is 0.62.0 |
| `README.md` (docs-site badge) | Badge linking to https://sipemu.github.io/eventstudy/ | VERIFIED | Line 11; shield.io badge with correct link target |
| `.Rbuildignore` (+3 site exclusion lines) | `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$` | VERIFIED | Lines 13-15; all three anchored entries present |
| `NEWS.md` (0.62.0 top entry) | `# EventStudy 0.62.0` with docs-site bullet | VERIFIED | Lines 1-11; correct heading + three bullets describing CI, URL, and tarball |

---

## Key Link Verification

| From | To | Via | Status | Details |
|------|----|----|--------|---------|
| `pkgdown.yaml` deploy step | `gh-pages` branch | `JamesIves/github-pages-deploy-action@v4 branch: gh-pages` | VERIFIED (local) | Wiring present in workflow YAML; live execution is a backstop |
| `.Rbuildignore` entries | CRAN source tarball (site paths absent) | `R CMD build` respects anchored regex exclusions | VERIFIED (local) | Three anchored entries confirmed; SUMMARY records `TARBALL_CLEAN_OK` from executor's `R CMD build` run. Tarball inspection by verifier not possible (R not available in this environment) — see human verification item 3 |
| `DESCRIPTION URL` | README badge | Both point to `https://sipemu.github.io/eventstudy/` | VERIFIED | Consistent across both files |

---

## Workflow Structure Spot-Check (pkgdown.yaml)

Direct read of `.github/workflows/pkgdown.yaml` confirms:

- **Triggers (CI-01):** `push: branches: [main]`, `release: types: [published]`, `workflow_dispatch` — all three present.
- **Permissions (CI-02):** `permissions: contents: write` at top level — least-privilege; no pages, id-token, or packages write.
- **Steps in order:** `actions/checkout@v4` → `r-lib/actions/setup-pandoc@v2` → `r-lib/actions/setup-r@v2` (with `use-public-rspm: true`) → `r-lib/actions/setup-r-dependencies@v2` (with `extra-packages: any::pkgdown, local::.` and `needs: website`, which installs the package with Suggests — CI-03) → `pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)` → `JamesIves/github-pages-deploy-action@v4` (`branch: gh-pages`, `folder: docs`, guarded by `if: github.event_name != 'pull_request'`).
- **Action versions:** `checkout@v4`, `setup-pandoc@v2`, `setup-r@v2`, `setup-r-dependencies@v2` — match the r-lib/actions major versions used in `R-CMD-check.yaml` (per plan requirement).
- **Secrets:** Only `secrets.GITHUB_TOKEN` — no additional secrets.
- **Concurrency:** `group: pkgdown`, `cancel-in-progress: true` — present.

No structural issues found.

---

## BUILD-02 Vignette Safety Detail

`vignettes/data-download.Rmd`:
- Setup chunk lines 12-18: `knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)` — globally disables evaluation.
- Prose label at line 24 (immediately after Introduction heading): "The code in this article is shown for illustration purposes and is not executed at build time because it requires a live network connection." — satisfies the "clearly labelled" requirement.

`vignettes/introduction.Rmd`:
- Setup chunk lines 12-18: `knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)` — globally disables evaluation.
- No prose label required (plan only required it for data-download.Rmd).

Both vignettes: SAFE. A transient network outage cannot fail the CI docs build.

---

## Anti-Patterns Scan

Files modified in this phase: `.github/workflows/pkgdown.yaml`, `.Rbuildignore`, `DESCRIPTION`, `README.md`, `NEWS.md`, `vignettes/data-download.Rmd`.

No TBD/FIXME/XXX/TODO/HACK/PLACEHOLDER markers found in any of the modified files. The workflow YAML is idiomatic r-lib/actions structure with no stubs or empty implementations. The prose label in `data-download.Rmd` is substantive, not a placeholder.

No blockers found.

---

## Requirements Coverage

| Requirement | Description | Status | Evidence |
|-------------|-------------|--------|----------|
| CI-01 | pkgdown.yaml with push:[main] + release:[published] + workflow_dispatch | VERIFIED (in-repo) | All three triggers confirmed in `.github/workflows/pkgdown.yaml` |
| CI-02 | Deploy to gh-pages with least-privilege contents:write (live serving = backstop) | VERIFIED (local half) / BACKSTOP (live) | `permissions: contents: write` + JamesIves deploy step present; live Pages serving requires operator action |
| CI-03 | Installs pkg + Suggests + builds articles; green Actions run = backstop | VERIFIED (wiring) / BACKSTOP (run) | `needs: website` + `extra-packages: any::pkgdown, local::.` in setup-r-dependencies step; green run requires operator push |
| LINK-01 | DESCRIPTION URL includes site URL; BugReports retained | VERIFIED | Both URLs on line 26; BugReports on line 27 |
| LINK-02 | README docs-site badge/link near badge row | VERIFIED | Line 11 of README.md; in the badge row |
| LINK-03 | .Rbuildignore excludes _pkgdown.yml, docs/, pkgdown/ | VERIFIED (entries) | Three anchored entries confirmed; tarball verification is human/CI-confirmable |
| BUILD-02 | Network-touching vignettes offline-safe and labelled | VERIFIED | eval=FALSE in both setup chunks; prose label in data-download.Rmd |
| BUILD-03 | Version 0.62.0 + NEWS entry; no new R CMD check issues vs v0.61.x; suite green | VERIFIED (version + NEWS) / HUMAN (check diff) | Version and NEWS confirmed in-repo; R CMD check diff plausible per SUMMARY but requires CI/clean-machine confirmation |

**Coverage:** 8/8 requirement IDs claimed by this phase are addressed. All can be confirmed in the local repo for their static/structural components. Three have live/runtime backstop halves that require operator action.

---

## Carry-Forward Items (MOOT — no action required)

- **WR-01** (`font-size-base`/`headings-font-weight` under `template.bslib`): `_pkgdown.yml` confirmed to use plain `template: {bootstrap: 5}` after the fdars-r redesign; no bslib block exists. Nothing to pin.
- **IN-01** (dead `.r-function` selector in `pkgdown/extra.scss`): `extra.scss` was deleted in the redesign; only `pkgdown/extra.css` remains. Nothing to remove.

Neither file was reintroduced in Phase 12. Both findings are correctly recorded as moot.

---

## Human Verification Required

### 1. Green GitHub Actions Run (CI-01/CI-03 live half)

**Test:** Push the Phase 12 changes to `main` (or publish a release) and watch the pkgdown workflow in the GitHub Actions tab.
**Expected:** All steps complete green — checkout, pandoc setup, R setup, dependency install (includes Suggests via `needs: website`), `pkgdown::build_site_github_pages()`, and the JamesIves deploy step writing to the `gh-pages` branch.
**Why human:** CI execution is not verifiable from the local repository state.

### 2. GitHub Pages Live Serving (CI-02 live half)

**Test:** After the first successful workflow run creates the `gh-pages` branch: navigate to GitHub repo Settings -> Pages, set Source = "Deploy from a branch", Branch = gh-pages / (root). Then visit `https://sipemu.github.io/eventstudy/`.
**Expected:** The pkgdown site loads — homepage renders the README, navbar shows Get Started / Reference / Articles / News, all vignette links resolve.
**Why human:** Requires a GitHub UI operator action and a live network check — not automatable from local state.

### 3. R CMD check --as-cran Baseline Diff Confirmation (BUILD-03 runtime half)

**Test:** On a machine with all Suggests packages installed (or on a CRAN-equivalent runner), run `R CMD check --as-cran EventStudy_0.62.0.tar.gz` and compare NOTEs/WARNINGs against the v0.61.x baseline.
**Expected:** No new NOTE or WARNING introduced by Phase 12. The SUMMARY records the expected diff: one pre-existing ERROR (missing optional Suggests on this dev machine), two pre-existing NOTEs (`VignetteBuilder` + `New submission`), and one expected-transient NOTE (404 for the site URL before first deploy). The 404 NOTE should resolve after the first successful deploy in item 1.
**Why human:** R is not available in the verifier environment; the executor ran this check but the verifier cannot independently reproduce it.

---

## Gaps Summary

No gaps. All in-repo verifiable components pass. The three human verification items above are all backstop/operator-dependent outcomes that the plan explicitly designated as non-automatable — they are not failures.

The SUMMARY records one notable finding: a `URL: Status: 404` NOTE in `R CMD check --as-cran` for `https://sipemu.github.io/eventstudy/`. This is expected before the first CI deploy and resolves automatically once the site is live. It is not a new regression introduced by Phase 12.

---

_Verified: 2026-09-05_
_Verifier: Claude (gsd-verifier)_
