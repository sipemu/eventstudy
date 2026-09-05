# Phase 15: Methods Articles + Rendered Outputs — Context

**Gathered:** 2026-09-06
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous)

<domain>
## Phase Boundary

Author the 7 conceptual Methods articles (METH-02..08) under
`vignettes/articles/` — one per method family — each following the 10-section
skeleton, bearing KaTeX formulas, rendering at least one `knitr::kable` table
and one plot from live package code executed at build time, and running fully
offline. Wire all 7 articles into the Methods navbar dropdown in `_pkgdown.yml`.
Verify formulas against the primary source paper AND the package source line.
Add missing bib entries to `vignettes/articles/references.bib`.

In scope:
- 7 new `vignettes/articles/*.Rmd` files, one per method family.
- 7 new `_pkgdown.yml` Methods menu entries (one per article).
- bib additions to `vignettes/articles/references.bib` for each family's
  primary academic references.
- Inline provenance comments in each Rmd (`<!-- Formula verified: R/<file>:<line> -->`).
- Per-chunk offline safety: `eval=requireNamespace("rugarch", quietly=TRUE)` for
  GARCH; `eval=FALSE` with static output block for LLM advisor layer and optional
  DiD estimators (callaway_santanna / dchaisemartin / bjs); inline synthetic data
  for panel/intraday/synthetic-control articles.

Out of scope:
- New R source code, new models, or new test statistics.
- New bundled datasets (Phase 14 decision: Methods articles reuse dieselgate +
  simulate; no new datasets).
- Changes to the existing CRAN vignettes (byte-unchanged, DELIVERY-03).
- Gallery worked-example Rmd files (Phase 16).
- Any _setup.Rmd, references.bib structure, or skeleton structural changes beyond
  content additions.

</domain>

<decisions>
## Implementation Decisions

**Both grey-area batches ACCEPTED by user (Accept all), 2026-09-06.**

### Article Count and Mapping
- **7 articles, 7 METH requirements (METH-02..08)** — no splitting or merging.
  METH-02 (return models) is one article with sub-sections per model family,
  structured around the two source files (R/models.R OLS-based,
  R/models_time_varying.R time-varying).
- Article-to-requirement mapping:
  | Article filename | METH req | Method family |
  |-----------------|----------|---------------|
  | `methods-return-models.Rmd` | METH-02 | Return models (all 9 classes) |
  | `methods-test-statistics.Rmd` | METH-03 | Test statistics (all 8) |
  | `methods-panel-did.Rmd` | METH-04 | Panel / DiD estimators |
  | `methods-intraday.Rmd` | METH-05 | Intraday event studies |
  | `methods-synthetic-control.Rmd` | METH-06 | Synthetic control |
  | `methods-ai-advisor.Rmd` | METH-07 | AI advisor two-layer design |
  | `methods-diagnostics.Rmd` | METH-08 | Diagnostics & robustness |

### Dataset Assignment per Article
- **methods-return-models.Rmd**: `data("dieselgate")` for OLS-based models
  (MarketModel, FamaFrench3, Carhart, RollingWindow, BHAR, ComparisonPeriodMean,
  MarketAdjusted). GARCH/DCC-GARCH chunks gated with
  `eval=requireNamespace("rugarch", quietly=TRUE)` — fall-through to a prose
  fallback paragraph explaining the time-varying volatility motivation.
- **methods-test-statistics.Rmd**: `data("earnings_surprises")` (3-firm
  multi-event panel produces AAR/CAAR output, natural for Patell/BMP/Sign/KP).
  Single-event statistics (AR/CAR t-test) also demonstrated via earnings_surprises
  event_id=1.
- **methods-panel-did.Rmd**: Inline synthetic staggered panel (hand-constructed
  tibble, ~100 rows, 10 units, 3 cohorts) using base R — no external packages.
  TWFE, dynamic_twfe, sun_abraham rendered live (all use `stats::lm()`).
  callaway_santanna / dchaisemartin / bjs chunks marked `eval=FALSE` with
  static captured output and installation instructions.
- **methods-intraday.Rmd**: Inline synthetic intraday tibble (POSIXct timestamps,
  ~200 rows, 2 firms, single trading session). Hand-constructed in a setup chunk.
- **methods-synthetic-control.Rmd**: Inline synthetic treated_data + donor_data
  (~80 periods, 5 donors). estimate_synthetic_control() with method="optim"
  (base R only — quadprog optional path gated if pkg absent).
- **methods-ai-advisor.Rmd**: `data("dieselgate")` for deterministic layer
  (es_diagnostics, recommend_stat, flag_robustness — fully offline). LLM layer
  `eval=FALSE` with static captured output block, identical pattern to
  `vignettes/ai-advisor.Rmd`.
- **methods-diagnostics.Rmd**: `data("earnings_surprises")` for model_diagnostics,
  pretrend_test. `simulate_event_study(seed=42)` for bootstrap_test and power
  (simulate_event_study is zero-network, seed-stable per R/simulation.R).

### Offline Rendering Hazard Resolution
- GARCH (rugarch): chunk-level `eval=requireNamespace("rugarch", quietly=TRUE)`.
  If rugarch absent, chunk is skipped; a fallback prose block explains the model
  conceptually. RENDER-01 (>=1 table + plot per article) is satisfied via the
  OLS-model sections which are always evaluated.
- Panel DiD optional estimators (did/DIDmultiplegt/didimputation): `eval=FALSE`
  per chunk; base-R estimators (TWFE/sun_abraham) provide the live-rendered output
  satisfying RENDER-01.
- AI advisor LLM layer: `eval=FALSE` + `Sys.setenv(EVENTSTUDY_NO_NETWORK="1")`
  in setup, consistent with vignettes/ai-advisor.Rmd.
- Synthetic control quadprog path: article defaults to method="optim" (stats::optim,
  base R). quadprog path shown as `eval=requireNamespace("quadprog", quietly=TRUE)`.

### Formula-Review Gate Mechanics
- Each Rmd's formula section (Section 4 of skeleton) contains an inline HTML
  comment: `<!-- Formula verified: R/<source_file>:<line_range> matches
  <BibKey> eq. <N> -->`.
- Planner tasks the executor to read the implementation line(s) BEFORE writing
  each formula, citing the file:line as evidence.
- References.bib additions are atomic with each article's plan task — no separate
  bib task.

### Article Math Depth
- Proportional: simple models (MarketModel, ComparisonPeriodMean) get 1-2 display
  equations; complex statistics (Patell Z, BMP) get formula + standardization
  equation + distribution statement; DCC-GARCH gets the bivariate GARCH spec.
- Cap: no article exceeds 5 display equations. Deeper math cross-links to the
  primary reference.

### _pkgdown.yml Nav Wiring
- All 7 articles added to the Methods dropdown under `navbar: components: methods:`
  in one atomic commit after all article Rmd files exist.
- The existing `smoke-test` entry is retained as "Overview" (or relabelled
  "Methods Overview").

### Plan Decomposition
- **Single plan (15-01-PLAN.md)** with 7 article tasks (parallelizable),
  bib additions atomic per article, and 1 nav-wiring task (sequenced after
  articles exist, touches only _pkgdown.yml Methods dropdown section).

### references.bib Additions Required
Current keys: `MacKinlay1997`, `Brown1985` (only these two in references.bib).
Articles must add (grouped by family):

| Family | Keys to add |
|--------|------------|
| Return models | Fama/French 1993 (FF3), Fama/French 2015 (FF5), Carhart 1997, Barber/Lyon 1997 (BHAR) |
| Test statistics | Patell 1976, Boehmer/Musumeci/Poulsen 1991 (BMP), Corrado 1989 (Rank/Sign), Kolari/Pynnönen 2010 |
| Panel/DiD | Callaway/Sant'Anna 2021, Sun/Abraham 2021, Borusyak/Jaravel/Spiess 2024, de Chaisemartin/D'Haultfoeuille 2020, Goodman-Bacon 2021 |
| Intraday | Barclay/Warner 1993 |
| Synthetic control | Abadie/Diamond/Hainmueller 2010 |
| Diagnostics | Box/Ljung 1978, Durbin/Watson 1950 |
| AI advisor | no academic bib needed (describes package design) |

### Claude's Discretion
- Exact prose, section wording, and which specific models/statistics get the
  fullest worked example within each article, provided each article covers all
  members of its family (conceptually) and renders >=1 live table + plot.
- Exact synthetic-data parameters for panel/intraday/SC inline construction.

</decisions>

<code_context>
## Existing Code Insights

### Article Infrastructure (Phase 13)
- `vignettes/articles/_setup.Rmd`: set.seed(42), options(scipen=999, digits=4),
  knitr defaults. All articles inherit determinism via `child="_setup.Rmd"`.
- `vignettes/articles/_article-skeleton.Rmd`: 10-section structure with
  bibliography: references.bib (co-located). Sections 6-8 require a live code
  chunk, a kable table, and a plot respectively.
- `vignettes/articles/smoke-test.Rmd`: working proof that KaTeX + plotly +
  @citation coexist. Uses dieselgate data and the standard pipeline.
- `_pkgdown.yml` Methods dropdown: currently holds only the smoke-test Overview
  entry. Seven entries to add.

### Optional-Dependency Footprint
- GARCHModel (`R/models.R`): hard stop if rugarch absent.
  DCCGARCHModel (`R/models_time_varying.R`): hard stop if rmgarch OR rugarch absent.
  RollingWindowModel: pure base R, no optional dep.
- Panel estimators: TWFE/dynamic_twfe/sun_abraham use `stats::lm()` (always available).
  callaway_santanna, dchaisemartin, bjs (`R/panel_event_study.R`): requireNamespace
  guard, warn+return NULL when absent.
- Synthetic control: `estimate_synthetic_control()` defaults to "quadprog" but
  falls back to "optim" (stats::optim) when quadprog absent (`R/synthetic_control.R`).
  Safe to default to method="optim" in articles.
- AI advisor LLM layer: no R package requirement; requires API key + network.
  Established offline pattern: `Sys.setenv(EVENTSTUDY_NO_NETWORK="1")` in setup,
  `eval=FALSE` on LLM chunk (`vignettes/ai-advisor.Rmd`).

### Bundled Datasets
- `data/dieselgate.rda`: 4 firms, DAX, event 2015-09-18, format dieselgate$firm /
  $index / $request / $meta. 9.1 KB. Best for multi-group single-event examples
  (return models, advisor).
- `data/earnings_surprises.rda`: AAPL/MSFT/GOOGL, S&P500, April-May 2023, single
  group "Earnings Beat". Best for multi-event (test statistics, diagnostics).
  Format earnings_surprises$firm / $index / $request / $meta
  (`R/data-earnings-surprises.R`).

### Simulation Capability
- `simulate_event_study()` (`R/simulation.R`): fully zero-network, accepts
  `seed` param, returns a valid EventStudyTask. NOT shaped as a panel, intraday,
  or synthetic control task. Suitable for power analysis only.

### Existing CRAN Vignette Overlap (19 vignettes exist)
- There are 19 Rmd files under `vignettes/` (including gallery.Rmd). ROADMAP/Phase
  13 CONTEXT say 18 — the discrepancy is gallery.Rmd. All 19 are CRAN-shipped and
  must remain byte-unchanged.
- Overlap to avoid: vignettes/factor-models-bhar.Rmd covers FF3/FF5/Carhart/GARCH/BHAR
  descriptively with global eval=FALSE (no rendered outputs). Methods article MUST
  complement (formula-bearing, live-rendered) rather than duplicate. Same for
  vignettes/modern-did-estimators.Rmd (eval=FALSE globally), vignettes/ai-advisor.Rmd
  (already has rendered deterministic layer), vignettes/diagnostics-validation.Rmd,
  vignettes/inference-robustness.Rmd.

</code_context>

<integration>
## Integration Points

- `vignettes/articles/` — 7 new Rmd files added (all already .Rbuildignored).
- `vignettes/articles/references.bib` — ~15 new bib entries added.
- `_pkgdown.yml` — Methods navbar dropdown expanded: 7 new menu entries added
  under `navbar: components: methods: menu:` (currently only smoke-test entry).
  No structural changes to Reference, Articles, or Gallery sections.
- `data/` — no changes (dieselgate.rda and earnings_surprises.rda already in place).
- `R/` — no changes (documentation-only phase).
- No NAMESPACE, DESCRIPTION, or CRAN vignette changes.

</integration>

<deferred>
## Deferred

- Gallery worked-example Rmd files (Phase 16).
- Any new bundled dataset (Phase 14 closed with dieselgate + earnings_surprises;
  Methods articles use simulation/inline synthetic data for remaining families).
- Volume/Volatility model articles: niche, and their datasets require firm_volume
  columns not present in bundled data; they appear in METH-02 as a brief conceptual
  section with eval=FALSE demonstration referencing
  vignettes/volume-volatility-event-study.Rmd for worked usage.
- Versioned docs, logo/hex sticker — deferred beyond v0.63.0.

</deferred>
