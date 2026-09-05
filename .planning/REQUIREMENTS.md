# Requirements: EventStudy — Documentation Depth (Methods & Worked Examples)

**Defined:** 2026-09-05
**Milestone:** v0.63.0
**Core Value:** Trustworthy numbers, trustworthy interpretation — now made *teachable*. The docs site becomes a pyfda-caliber learning resource (reference: `https://sipemu.github.io/pyfda/`): every method family explained with real statistical exposition + academic references, every method demonstrated with rendered outputs, plus a cross-domain gallery of complete worked examples — all pkgdown-only and CRAN-/CI-clean.

## v1 Requirements

Requirements for milestone v0.63.0. Each maps to exactly one roadmap phase. All new rich content is **pkgdown-only** (`vignettes/articles/`, `.Rbuildignore`d) and must render **fully offline** in the existing v0.62.0 CI pkgdown build.

### Learn / Methods articles (METH)

- [ ] **METH-01**: A "Learn / Methods" section is exposed in the pkgdown navigation (a dedicated navbar menu or Articles group) that collects the conceptual method articles as a coherent learning track.
- [ ] **METH-02**: A conceptual article for **return models** (market, Fama-French 3/5, Carhart, GARCH/DCC, rolling-window, BHAR, comparison-period-mean, volume/volatility) explains each model's estimation, assumptions, and when-to-use, and cites the primary academic references.
- [ ] **METH-03**: A conceptual article for **test statistics** (AR/CAR t-tests, Patell Z, BMP, Sign, Kolari-Pynnönen, Calendar-Time Portfolio, cross-sectional t) explains each statistic's null hypothesis, formula, assumptions, and references.
- [ ] **METH-04**: A conceptual article for **panel / DiD estimators** (TWFE, Sun-Abraham, Callaway-Sant'Anna, Borusyak-Jaravel-Spiess, de Chaisemartin-D'Haultfoeuille) explains staggered-treatment bias, each estimator's approach, and references.
- [ ] **METH-05**: A conceptual article for **intraday event studies** explains POSIXct/minute-second windows, microstructure considerations, and references.
- [ ] **METH-06**: A conceptual article for **synthetic control** explains the method, identifying assumptions, donor-pool/weighting mechanics, and references.
- [ ] **METH-07**: A conceptual article for the **AI advisor** explains the two-layer design (deterministic offline diagnostics + grounded LLM advise), the grounding invariant, provider precedence, and usage.
- [ ] **METH-08**: A conceptual article for **diagnostics & robustness** (normality, autocorrelation, pre-trend, bootstrap, power) explains each check, how to interpret it, and references.

### Rendered outputs (RENDER)

- [ ] **RENDER-01**: Every Methods article executes real package code at build time and renders at least one results table (e.g. `knitr::kable`/printed result) and at least one plot for the method(s) it covers — no method is described without a demonstrated output.
- [ ] **RENDER-02**: All article code executes fully offline — bundled data plus `set.seed()` for any stochastic step — with zero network calls at build time, so articles render reproducibly on a CI runner.
- [ ] **RENDER-03**: Interactive/HTML plot output (plotly) and MathJax formula rendering coexist on the same page without breaking layout or math (the known pkgdown plotly/MathJax conflict is handled).

### Worked-examples gallery (GALLERY)

- [ ] **GALLERY-01**: A pyfda-style gallery landing page presents the cross-domain worked examples as a browsable index (cards/thumbnails with short descriptions), reachable from the navbar.
- [ ] **GALLERY-02**: At least three complete end-to-end worked examples across distinct domains (e.g. earnings surprises, M&A announcements, regulatory/enforcement shocks) each render a full analysis: data → model fit → test statistics → plots → written interpretation.
- [ ] **GALLERY-03**: Each gallery example cross-links to the relevant Methods articles and to the exported functions it uses (reference pages), so readers can move from example → concept → API.

### Curated datasets (DATA)

- [ ] **DATA-01**: Each gallery domain example is powered by a curated real dataset with documented `data-raw/` provenance (a reproducible fetch/build script plus a frozen snapshot), following the existing dieselgate dataset pattern.
- [ ] **DATA-02**: Datasets are sized and placed so the CRAN source tarball stays clean — either compressed, documented `data()` datasets (with `.Rd`) or `.Rbuildignore`d site-only data — with the choice recorded per dataset.

### pkgdown-only delivery (DELIVERY)

- [ ] **DELIVERY-01**: The new Methods articles and gallery live under `vignettes/articles/` and are `.Rbuildignore`d so they render on the site but are absent from the CRAN source tarball.
- [ ] **DELIVERY-02**: `_pkgdown.yml` navigation integrates the new Learn/Methods track and the Gallery alongside the existing Articles without breaking the existing 18-vignette navigation or the grouped Reference index.
- [ ] **DELIVERY-03**: The existing 18 CRAN-shipped vignettes remain unchanged and CRAN-shipped — the new rich content is strictly additive.

### Build & release integrity (BUILD)

- [ ] **BUILD-04**: `pkgdown::build_site()` completes locally with zero errors and zero new warnings, including all new Methods articles and the gallery.
- [ ] **BUILD-05**: The existing v0.62.0 CI `pkgdown.yaml` workflow builds the new articles offline and deploys them to GitHub Pages (green Actions run), with no CI changes required beyond content/config.
- [ ] **BUILD-06**: `R CMD check --as-cran` shows no new NOTEs/WARNINGs versus the v0.62.0 baseline, the source tarball is not bloated by site-only content, and the existing testthat suite stays green.

## Future Requirements

Deferred beyond v0.63.0.

### Docs (DOCS-FUTURE)

- **Package logo / hex sticker** — deferred (v0.62.0 decision).
- **Versioned docs (multi-version pkgdown)** — deferred.
- **Video / screencast walkthroughs** — out of scope for a static pkgdown site.

## Out of Scope

Explicitly excluded. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| New statistical methods, models, or estimators | v0.63.0 is documentation depth only — no new compute surface. |
| Changing the statistical intent of any existing method | Behavior on valid input stays unchanged. |
| Rewriting the existing 18 CRAN vignettes | They stay concise and CRAN-shipped; new content is additive and pkgdown-only. |
| Shipping the rich Methods articles / gallery inside the CRAN tarball | Deliberately pkgdown-only to keep the tarball small and `R CMD check` fast. |
| Full retrieval-corpus (RAG) "Advisor Pro" | The commercial tier; validated via waitlist first, not built here. |
| MCP server surface | Deferred; the Agent Skill delivers the full loop with less surface area. |
| Performance / scaling work (streaming, data.table, sparse FE) | Orthogonal; deferred. |

## Traceability

Which phases cover which requirements. Populated during roadmap creation.

| Requirement | Phase | Status |
|-------------|-------|--------|
| METH-01 | TBD | Pending |
| METH-02 | TBD | Pending |
| METH-03 | TBD | Pending |
| METH-04 | TBD | Pending |
| METH-05 | TBD | Pending |
| METH-06 | TBD | Pending |
| METH-07 | TBD | Pending |
| METH-08 | TBD | Pending |
| RENDER-01 | TBD | Pending |
| RENDER-02 | TBD | Pending |
| RENDER-03 | TBD | Pending |
| GALLERY-01 | TBD | Pending |
| GALLERY-02 | TBD | Pending |
| GALLERY-03 | TBD | Pending |
| DATA-01 | TBD | Pending |
| DATA-02 | TBD | Pending |
| DELIVERY-01 | TBD | Pending |
| DELIVERY-02 | TBD | Pending |
| DELIVERY-03 | TBD | Pending |
| BUILD-04 | TBD | Pending |
| BUILD-05 | TBD | Pending |
| BUILD-06 | TBD | Pending |

**Coverage:**
- v1 requirements: 22 total
- Mapped to phases: 0 (roadmap pending)
- Unmapped: 22 ⚠️

---
*Requirements defined: 2026-09-05*
*Last updated: 2026-09-05 after initial definition*
