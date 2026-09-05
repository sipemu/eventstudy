# Phase 12 Planning Brief — CI/CD Deploy + Repo Linkage + Release Integrity

**Read this in full before planning.** It captures ground-truth repo state (verified
2026-09-05) and locked decisions the orchestrator holds in memory that you cannot see.
Requirements + success criteria live in ROADMAP.md (Phase 12) and REQUIREMENTS.md
(CI-01..03, LINK-01..03, BUILD-02, BUILD-03) — this brief adds the concrete current
state so your tasks reference real values, not guesses.

## Ground truth (verified in the repo right now)

- **Package name:** `EventStudy` (CRAN/DESCRIPTION `Package:`). **GitHub repo:** `sipemu/eventstudy`.
- **DESCRIPTION `Version:` is `0.61.3`** (not 0.61.0 — there have been patch releases). Bump to `0.62.0`.
- **DESCRIPTION `URL:`** currently `https://github.com/sipemu/eventstudy` only. Add the pkgdown site URL
  `https://sipemu.github.io/eventstudy/` alongside it (comma-separated). `BugReports:` already set — retain it.
- **`.Rbuildignore`** currently excludes: `.Rproj`, `.Rproj.user`, `^\.github$`, `cran-comments.md`,
  `README.md`, `LICENSE(.md)`, `.gitignore`, `^\.claude$`, `^\.planning$`, `^\.gsd$`, `data-raw`.
  It does **NOT** yet exclude `_pkgdown.yml`, `docs/`, or `pkgdown/`. Add those three (the `^\.github$`
  exclusion already covers the workflow, so the tarball stays unchanged).
- **`.github/workflows/`** currently has `R-CMD-check.yaml` only. Add `pkgdown.yaml` (r-lib/actions standard).
- **README.md** already has a badge row (lines 3–10): CRAN status, CRAN downloads, monthly downloads,
  License AGPL-3, R ≥ 4.1.0, Lifecycle experimental, R-CMD-check, Codecov. Add a **docs-site badge/link**
  near this row (e.g. a pkgdown/"docs: online" shield linking to `https://sipemu.github.io/eventstudy/`).
- **NEWS.md** top entry is `# EventStudy 0.61.3`. Prepend a `# EventStudy 0.62.0` entry recording the
  documentation site (pkgdown site + CI deploy).
- **pkgdown site config:** `_pkgdown.yml` is now **plain Bootstrap 5** (`template: {bootstrap: 5}`,
  `home: {sidebar: false}`, minimal navbar, `url: https://sipemu.github.io/eventstudy/` already set).
  A `pkgdown/extra.css` gallery stylesheet exists; `pkgdown/extra.scss` was **deleted** in the redesign.
  There are 18 `man/figures/card-*.svg` and a `vignettes/gallery.Rmd` hub. The site builds warning-clean
  locally (`pkgdown::build_site(preview = FALSE)` exits 0).

## Locked decisions (orchestrator memory — honor these)

1. **Redesign already shipped** (fdars-r style). Phase 12 must NOT touch `_pkgdown.yml` design,
   `pkgdown/extra.css`, `vignettes/gallery.Rmd`, or the card SVGs except as strictly required for CI/release.
2. Keep `url:` as `https://sipemu.github.io/eventstudy/` (already set — do not change).
3. **Carry-forward findings from Phase 11 review are now MOOT** and require NO action:
   - **WR-01** (`font-size-base`/`headings-font-weight` under `template.bslib`): the bslib block was
     removed in the redesign — `_pkgdown.yml` is plain `bootstrap: 5`, so these vars no longer exist.
   - **IN-01** (dead `.r-function` selector in `extra.scss`): `extra.scss` was deleted; `extra.css` has
     no `.r-function`. Verified absent. State both as moot in the plan; do not reintroduce files to "fix" them.
   Pinning `pkgdown` in DESCRIPTION `Suggests:` is OPTIONAL (pkgdown is a docs tool run in CI, not needed
   for CRAN). Currently pkgdown is NOT in DESCRIPTION at all. Only add a pin if you have a concrete reason.
4. Commit trailer for any commits: `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.

## Requirements → work mapping (all 8 IDs must appear in a plan's `requirements` field)

- **CI-01/02/03** — `.github/workflows/pkgdown.yaml` (r-lib/actions): triggers on `push: [main]`,
  `release: [published]`, `workflow_dispatch`; installs pkg + Suggests; builds articles; deploys to
  `gh-pages` with least-privilege `permissions: {contents: write}`; site served at the Pages URL.
- **LINK-01** — DESCRIPTION `URL` adds the pkgdown site URL (retain BugReports).
- **LINK-02** — README docs-site badge/link near the existing badge row.
- **LINK-03** — `.Rbuildignore` excludes `_pkgdown.yml`, `docs/`, `pkgdown/` (workflow already covered by `^\.github$`).
- **BUILD-02** — network-touching vignettes (e.g. `data-download`) build reproducibly in CI:
  offline-safe / cached / gated so a transient network failure does not break the docs build; any
  non-evaluated content clearly labelled. (Check how `data-download.Rmd` currently guards network calls —
  it likely already uses `eval=FALSE`/`purl=FALSE` chunks or a bundled-data fallback; verify and align CI.)
- **BUILD-03** — `R CMD check --as-cran` shows no new NOTEs/WARNINGs vs the v0.61.x baseline, suite stays
  green, version bumped to `0.62.0`, NEWS.md `v0.62.0` entry added.

## Scope guard

- **IN:** `.github/workflows/pkgdown.yaml`, DESCRIPTION (`URL`, `Version`), README badge row,
  `.Rbuildignore`, NEWS.md, any minimal vignette CI-safety adjustment for BUILD-02.
- **OUT:** the site's visual design (done in the redesign), any R/ source behavior change, new features.
- **Operator step (cannot be automated):** setting GitHub repo "About → Website" field — call it out in the
  plan as a post-deploy manual action, not an automated requirement.

## Proven verify commands (reuse verbatim where the build story matches)

Local pkgdown build gate (from Phase 11, still valid):
```
Rscript -e 'pkgdown::build_site(preview = FALSE)' 2>&1 | grep -viE '^#' | grep -iE 'missing|orphan|warning'; test $? -eq 1
```
CRAN check gate (BUILD-03): `R CMD check --as-cran` on the built tarball, diff NOTEs/WARNINGs against
the v0.61.x baseline. Note: the pkgdown deploy itself can only be fully verified by a real GitHub Actions
run on `main` after push — the plan should treat "green Actions run" as an operator/post-merge verification,
and make everything locally checkable (YAML lint/parse, `.Rbuildignore` effect on `R CMD build` tarball
contents, DESCRIPTION/README/NEWS assertions) verifiable offline.
