# Roadmap: EventStudy — Grounded AI Advisor

## Milestones

- ✅ **v0.50.0 Robustness Hardening** — Phases 1-4 (shipped 2026-09-02)
- ✅ **v0.60.0 Grounded AI Advisor** — Phases 5-8 (shipped 2026-09-04)
- ✅ **v0.61.0 Advisor Vignette + Dieselgate Walkthrough** — Phases 9-10 (shipped 2026-09-04)
- 🚧 **v0.62.0 Documentation Site (pkgdown + CI/CD)** — Phases 11-12 (in progress)
- 🚧 **v0.63.0 Documentation Depth — Methods & Worked Examples** — Phases 13-16 (in progress)

## Phases

**Phase Numbering:**

- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Numbering is continuous across milestones. Full per-phase detail for shipped
milestones lives in `.planning/milestones/v{X.Y}-ROADMAP.md`.

<details>
<summary>✅ v0.50.0 Robustness Hardening (Phases 1-4) — SHIPPED 2026-09-02</summary>

- [x] **Phase 1: Contract Foundation** — degenerate-input contract + strict/lenient mode (completed 2026-09-02)
- [x] **Phase 2: Model and Stats Sweep** — contract applied across all 13 models + all test statistics (completed 2026-09-02)
- [x] **Phase 3: Pipeline and External Hardening** — hardened prepare/export + wrapped external-package areas (completed 2026-09-02)
- [x] **Phase 4: Regression Net and Check Gate** — per-fix regression tests + contract matrix + green R CMD check (completed 2026-09-02)

Archive: `.planning/milestones/v0.50.0-ROADMAP.md`

</details>

<details>
<summary>✅ v0.60.0 Grounded AI Advisor (Phases 5-8) — SHIPPED 2026-09-04</summary>

**Milestone Goal:** Add an LLM-agnostic AI advisor that guides users through an entire event study — grounded so it interprets only package-computed numbers and never fabricates results.

- [x] **Phase 5: Offline Diagnostics + Grounding Knowledge Base** — deterministic zero-dependency `es_diagnostics()`, pure-R assumption→test KB, non-LLM rule-based advice fallback (completed 2026-09-03)
- [x] **Phase 6: Provider Abstraction + CRAN-Safe HTTP Harness** — LLM-agnostic `AdvisorProvider` R6 hierarchy (Anthropic, OpenAI-compatible, custom), 3-tier precedence, graceful degradation, offline-tested (completed 2026-09-04)
- [x] **Phase 7: Grounded Advise Layer + Grounding Guard** — `es_advise()` `Advice` object with runtime grounding guard, all six advice modes, `generate_report()` integration (completed 2026-09-04)
- [x] **Phase 8: Agent Skill + Waitlist + Green Check Gate** — Claude Code Agent Skill, "Advisor Pro" waitlist surface, green `R CMD check --as-cran` with suite green (completed 2026-09-04)

Archive: `.planning/milestones/v0.60.0-ROADMAP.md` · Audit: `.planning/milestones/v0.60.0-MILESTONE-AUDIT.md`

</details>

<details>
<summary>✅ v0.61.0 Advisor Vignette + Dieselgate Walkthrough (Phases 9-10) — SHIPPED 2026-09-04</summary>

**Milestone Goal:** Ship a CRAN vignette that explains the AI advisor's idea and mechanics, anchored by a real Volkswagen dieselgate worked example, and align the package's other doc entry points around that story.

- [x] **Phase 9: Bundled Dieselgate Dataset + Provenance** — reproducible `data-raw/` fetch of VW + benchmark returns around the Sept 2015 EPA disclosure, frozen into `data/` with documented provenance and `.Rd`, proven to drive a valid end-to-end event study (completed 2026-09-04)
- [x] **Phase 10: Advisor Vignette + Offline-Safe Build + Docs + Release** — the two-layer advisor vignette (deterministic layer live, LLM layer static/labelled) on the bundled dieselgate data, offline-safe build, aligned README/pkgdown/NEWS, and a CRAN-clean 0.61.0 release (completed 2026-09-04)

Archive: `.planning/milestones/v0.61.0-ROADMAP.md`

</details>

### 🚧 v0.62.0 Documentation Site (pkgdown + CI/CD) (Phases 11-12)

**Milestone Goal:** Ship a curated, professionally-themed pkgdown documentation website for EventStudy — auto-built and deployed to GitHub Pages by CI on every push to `main` and on releases — and link it prominently from the repo, matching the `fdars-r` documentation pattern.

- [x] **Phase 11: Curated pkgdown Site + Custom Theme (local build)** — `_pkgdown.yml` with grouped reference index, organized Articles nav over the 18 vignettes, README homepage, custom Bootstrap-5 theme, verified with a clean local `build_site()` (completed 2026-09-04)
- [~] **Phase 12: CI/CD Deploy + Repo Linkage + Release Integrity** — r-lib `pkgdown.yaml` workflow deploying to gh-pages, DESCRIPTION URL / README badge / `.Rbuildignore`, network-safe article build, green Actions run, and a CRAN-clean 0.62.0 release (executed 2026-09-05, verification `human_needed`: all 8 reqs wired & verified in-repo; awaits operator: push→green Actions run, enable Pages, full-toolchain `R CMD check --as-cran` confirm)

### 🚧 v0.63.0 Documentation Depth — Methods & Worked Examples (Phases 13-16)

**Milestone Goal:** Transform the v0.62.0 pkgdown site from a grouped reference into a pyfda-caliber learning resource — a Learn/Methods track (one conceptual, formula-bearing, rendered article per method family), a cross-domain worked-examples gallery powered by curated real datasets, all pkgdown-only (`vignettes/articles/`, `.Rbuildignore`d) and rendered fully offline in the existing CI build with no CRAN/`R CMD check` regressions.

- [x] **Phase 13: Article Infrastructure & Conventions Gate** — `vignettes/articles/` (`.Rbuildignore`d same commit), shared `_setup.Rmd` (seed + `options`), co-located `references.bib`, `math-rendering: katex` (resolves the plotly/MathJax conflict), reusable 10-section article skeleton, Learn/Methods + Gallery nav slots wired, existing 18 CRAN vignettes left untouched, verified by a CI dry-run rendering a smoke-test formula + citation + plotly figure on one page (completed 2026-09-05)
- [ ] **Phase 14: Curated Per-Domain Datasets** — curated real datasets (dieselgate pattern) for the gallery domains, each with reproducible `data-raw/` provenance + `DATA-SOURCES.md`, `simulate_event_study()` preferred over scraped data, placed and sized (compressed documented `data()` `.rda` or `.Rbuildignore`d site-only `.rds`) so the CRAN tarball stays clean, placement recorded per dataset
- [ ] **Phase 15: Methods Articles + Rendered Outputs** — all 8 conceptual Methods articles (return models, test statistics, panel/DiD, intraday, synthetic control, AI advisor, diagnostics/robustness), each with formulas, assumptions, when-to-use, primary academic references, and at least one build-time-executed rendered table + plot, all offline/`set.seed`, formula-reviewed against primary literature + package source
- [ ] **Phase 16: Worked-Examples Gallery + Build & Release Integrity** — pyfda-style gallery landing card index + ≥3 complete cross-domain worked examples (data → fit → statistics → plots → interpretation), each cross-linked to Methods articles and reference pages, closed out by a green `pkgdown::build_site()`, a green CI `pkgdown.yaml` deploy, and `R CMD check --as-cran` with no new NOTEs/WARNINGs and an unbloated tarball

## Phase Details

### Phase 11: Curated pkgdown Site + Custom Theme (local build)

**Goal**: A curated, professionally-themed pkgdown site builds cleanly on a developer's machine — every exported symbol is grouped, all 18 vignettes are reachable and organized, the homepage renders from the README, and the custom Bootstrap-5 theme reads as professional — before any CI is introduced.
**Depends on**: Phase 10 (v0.61.x exports + 18 vignettes exist)
**Requirements**: SITE-01, SITE-02, SITE-03, SITE-04, THEME-01, THEME-02, BUILD-01
**Success Criteria** (what must be TRUE):

  1. `pkgdown::build_site()` completes locally with zero errors and zero missing-topic/orphaned-reference warnings — every exported symbol appears in exactly one grouped Reference section (Pipeline & tasks, Return models, Test statistics, Panel/intraday/synthetic, AI advisor, Diagnostics, Cross-sectional & simulation, Export & reporting, Plotting, Data & datasets).
  2. A visitor to the built site sees a navbar with Get Started, Reference, an Articles dropdown, and News/Changelog, and can reach all 18 vignettes grouped meaningfully under Articles.
  3. The homepage renders the README as the landing page without build error, and `url:` is set to `https://sipemu.github.io/eventstudy/` so cross-references and canonical links resolve.
  4. The site applies a custom Bootstrap-5 theme (accent color + font pairing via `template.bootswatch`/bslib variables, no logo), and reference/article typography, code blocks, and syntax highlighting render cleanly with no contrast or layout regressions in the local build.

**Plans**: 1/1 plans executed

- [x] 11-01-PLAN.md — curated `_pkgdown.yml` (grouped reference + Articles + navbar) and custom Bootstrap-5 bslib theme, verified warning-clean via local `build_site()`

**UI hint**: yes

### Phase 12: CI/CD Deploy + Repo Linkage + Release Integrity

**Goal**: The site that builds locally in Phase 11 is now built and deployed to GitHub Pages by CI on every push to `main` and on releases, the repo links to the live site, network-touching vignettes build reproducibly in Actions, and the whole change ships as a CRAN-clean 0.62.0 release that leaves the source tarball unchanged.
**Depends on**: Phase 11 (a locally-building site config must exist before CI can deploy it)
**Requirements**: CI-01, CI-02, CI-03, LINK-01, LINK-02, LINK-03, BUILD-02, BUILD-03
**Success Criteria** (what must be TRUE):

  1. A push to `main` triggers the `.github/workflows/pkgdown.yaml` workflow (also on `release: [published]` and manual `workflow_dispatch`), which installs the package with its Suggests, builds articles, and completes green on GitHub Actions — verified by a successful run on the default branch.
  2. The workflow deploys the built site to the `gh-pages` branch with least-privilege `contents: write` permissions, and GitHub Pages serves the site at `https://sipemu.github.io/eventstudy/`.
  3. Network-touching vignettes (e.g. `data-download`) build reproducibly in CI — offline-safe, cached, or gated so a transient network failure does not break the docs build, with any non-evaluated content clearly labelled.
  4. DESCRIPTION `URL` includes the pkgdown site URL alongside the GitHub URL (BugReports retained), the README shows a documentation-site badge/link near the existing badge row, and `.Rbuildignore` excludes `_pkgdown.yml`, `docs/`, `pkgdown/`, and the workflow so the CRAN source tarball is unchanged.
  5. `R CMD check --as-cran` shows no new NOTEs/WARNINGs vs the v0.61.x baseline, the existing test suite stays green, and the package is bumped to `0.62.0` with a NEWS.md `v0.62.0` entry recording the documentation site.

**Plans**: 1 plan

- [ ] 12-01-PLAN.md — r-lib `pkgdown.yaml` gh-pages deploy workflow, DESCRIPTION URL + README docs badge + `.Rbuildignore` site exclusions, network-safe vignette build, and CRAN-clean 0.62.0 release (version bump + NEWS)

**Operator step**: Setting the GitHub repo "About → Website" field to the live site URL is a manual GitHub UI action — CI cannot set it. This is an operator task to perform after the first successful deploy, not an automated requirement.
**UI hint**: no

### Phase 13: Article Infrastructure & Conventions Gate

**Goal**: Every documentation-integrity pitfall (determinism, math escaping, citation resolution, tarball exclusion, plotly/MathJax coexistence, content duplication) is prevented once — in a shared, reusable article skeleton and site config — before any content article is written, with the Learn/Methods track and Gallery visible in the navigation and the existing 18 CRAN vignettes untouched.
**Depends on**: Phase 12 (a building, CI-deployed v0.62.0 pkgdown site config must exist)
**Requirements**: METH-01, RENDER-03, DELIVERY-01, DELIVERY-02, DELIVERY-03
**Success Criteria** (what must be TRUE):

  1. A site visitor sees a dedicated "Learn / Methods" navigation section and a "Gallery" entry in the navbar (wired in both `navbar: structure:` and `navbar: components:`), coherent alongside — and not breaking — the existing Articles listing over the 18 vignettes or the grouped Reference index.
  2. The new rich content lives under `vignettes/articles/` and `^vignettes/articles$` is in `.Rbuildignore` (added in the same commit that creates the directory) — so articles render on the site but are absent from the CRAN source tarball, and the 18 existing CRAN vignettes are byte-unchanged and still CRAN-shipped.
  3. A CI dry-run renders one smoke-test article that shows a KaTeX-rendered formula, a resolved academic citation (from the co-located filename-only `references.bib`), and a plotly figure all on the same page with no broken layout, no raw `[@Key]`/`$LaTeX$`, and no plotly/MathJax JS conflict (`math-rendering: katex` set).
  4. A reusable article skeleton (`_setup.Rmd` child with `set.seed`, `options(scipen, digits)`, knitr opts + the 10-section template) exists and is used by the smoke-test article, so downstream authors inherit determinism and structure by default.

**Plans**: 1/1 plans executed

- [x] 13-01-PLAN.md — shared article infra (`_setup.Rmd`, `references.bib`, 10-section skeleton), `_pkgdown.yml` `math-rendering: katex` + Methods/Gallery navbar, `.Rbuildignore` `^vignettes/articles$`, verified end-to-end by a dieselgate smoke-test article

**UI hint**: yes

### Phase 14: Curated Per-Domain Datasets

**Goal**: Every gallery domain example has a curated dataset whose provenance is reproducible, whose licensing posture is defensible, and whose placement keeps the CRAN source tarball clean — with the size/documentation discipline established on the first dataset, not deferred to pre-release cleanup.
**Depends on**: Phase 13 (article infra + delivery conventions define where site-only data lives)
**Requirements**: DATA-01, DATA-02
**Success Criteria** (what must be TRUE):

  1. Each gallery domain dataset has documented `data-raw/` provenance — a reproducible fetch/build script plus a frozen snapshot and a `DATA-SOURCES.md`/`license_note` — following the existing dieselgate pattern, with `simulate_event_study()` + `set.seed()` preferred over scraped data wherever it serves the example.
  2. Each dataset is placed and sized to keep the CRAN tarball clean — either a compressed, documented `data()` dataset with an `.Rd` (`@format`/`@source`) shipped in the same commit as the `.rda`, or an `.Rbuildignore`d site-only `.rds` under `vignettes/articles/data/` — with the placement choice recorded per dataset and the `data/` size budget respected.
  3. Each curated dataset drives a valid end-to-end event study (loads, fits, produces finite statistics) so it is proven usable by a gallery example before that example is authored.

**Plans**: 1/1 plans executed
Plans:

- [x] 14-01-PLAN.md — Build earnings_surprises dataset (tracer), roxygen doc, DATA-SOURCES.md registry, CRAN safety gates

**UI hint**: no

### Phase 15: Methods Articles + Rendered Outputs

**Goal**: A reader can learn every EventStudy method family from a conceptual, formula-bearing article that not only explains the method (estimation/null, assumptions, when-to-use, primary references) but demonstrates it with real package code rendered at build time — no method described without a shown output, all reproducible offline.
**Depends on**: Phase 13 (article skeleton + conventions); Phase 14 (datasets for rendered examples)
**Requirements**: METH-02, METH-03, METH-04, METH-05, METH-06, METH-07, METH-08, RENDER-01, RENDER-02
**Success Criteria** (what must be TRUE):

  1. There is one conceptual article per method family — return models, test statistics, panel/DiD, intraday, synthetic control, AI advisor, and diagnostics/robustness (7 articles covering all 8 METH requirements) — each stating the method's estimation/null, assumptions, when-to-use guidance, and primary academic references.
  2. Every Methods article executes real package code at build time and renders at least one results table (`knitr::kable`/printed result) and at least one plot for the method(s) it covers.
  3. All article code runs fully offline — bundled/simulated data plus `set.seed()` for any stochastic step — with zero network calls, so building twice produces zero numeric diffs.
  4. Each article's key formulas are verified against the primary source paper AND the package source implementation (formula-review gate), so the package's own docs are not subtly wrong.

**Plans**: TBD
**UI hint**: yes

### Phase 16: Worked-Examples Gallery + Build & Release Integrity

**Goal**: A cross-domain gallery of complete, rendered worked examples makes the package tangible end-to-end and connects example → concept → API, and the entire v0.63.0 documentation-depth addition ships proven clean: local build, CI deploy, and CRAN check all green.
**Depends on**: Phase 15 (Methods articles to cross-link to); Phase 14 (datasets that power the examples)
**Requirements**: GALLERY-01, GALLERY-02, GALLERY-03, BUILD-04, BUILD-05, BUILD-06
**Success Criteria** (what must be TRUE):

  1. A pyfda-style gallery landing page presents the cross-domain worked examples as a browsable card/thumbnail index with short descriptions, reachable from the navbar.
  2. At least three complete end-to-end worked examples across distinct domains (e.g. earnings surprises, M&A announcements, regulatory/enforcement shocks) each render a full analysis: data → model fit → test statistics → plots → written interpretation.
  3. Each gallery example cross-links to the relevant Methods articles and to the exported functions it uses (reference pages), so a reader can move example → concept → API.
  4. `pkgdown::build_site()` completes locally with zero errors and zero new warnings (all Methods articles + gallery included), and the existing CI `pkgdown.yaml` builds the new articles offline and deploys them to GitHub Pages green with no CI changes beyond content/config.
  5. `R CMD check --as-cran` shows no new NOTEs/WARNINGs versus the v0.62.0 baseline, the source tarball is not bloated by site-only content, and the existing testthat suite stays green.

**Plans**: TBD
**UI hint**: yes

## Progress

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 1. Contract Foundation | v0.50.0 | 2/2 | Complete | 2026-09-02 |
| 2. Model and Stats Sweep | v0.50.0 | 4/4 | Complete | 2026-09-02 |
| 3. Pipeline and External Hardening | v0.50.0 | 2/2 | Complete | 2026-09-02 |
| 4. Regression Net and Check Gate | v0.50.0 | 2/2 | Complete | 2026-09-02 |
| 5. Offline Diagnostics + Grounding KB | v0.60.0 | 3/3 | Complete | 2026-09-03 |
| 6. Provider Abstraction + HTTP Harness | v0.60.0 | 3/3 | Complete | 2026-09-04 |
| 7. Grounded Advise Layer + Guard | v0.60.0 | 2/2 | Complete | 2026-09-04 |
| 8. Agent Skill + Waitlist + Check Gate | v0.60.0 | 2/2 | Complete | 2026-09-04 |
| 9. Bundled Dieselgate Dataset + Provenance | v0.61.0 | 1/1 | Complete | 2026-09-04 |
| 10. Advisor Vignette + Offline-Safe Build + Docs + Release | v0.61.0 | 1/1 | Complete | 2026-09-04 |
| 11. Curated pkgdown Site + Custom Theme (local build) | v0.62.0 | 1/1 | Complete    | 2026-09-04 |
| 12. CI/CD Deploy + Repo Linkage + Release Integrity | v0.62.0 | 0/1 | Planned | - |
| 13. Article Infrastructure & Conventions Gate | v0.63.0 | 1/1 | Complete    | 2026-09-05 |
| 14. Curated Per-Domain Datasets | v0.63.0 | 1/1 | In Progress|  |
| 15. Methods Articles + Rendered Outputs | v0.63.0 | 0/? | Not started | - |
| 16. Worked-Examples Gallery + Build & Release Integrity | v0.63.0 | 0/? | Not started | - |
