# Feature Research

**Domain:** Documentation depth for a financial event-study R package (pkgdown-only Methods articles + worked-examples gallery)
**Researched:** 2026-09-05
**Confidence:** HIGH

---

## Context: What Already Exists

The 18 existing CRAN vignettes are concise how-to guides with `eval = FALSE` code and no rendered outputs. They answer "how do I call this function?" They do not answer "what is this method, when should I use it, how does it behave on real data, and what does the output look like?" The v0.63.0 articles are additive and pkgdown-only — they must complement, not duplicate, the existing 18.

**Existing vignette coverage (complement-not-duplicate map):**

| Existing Vignette | What It Covers | What a Methods Article Adds |
|---|---|---|
| `introduction.Rmd` | Pipeline API walkthrough, Dieselgate setup (eval=FALSE) | Rendered pipeline output; statistical foundations of the event-study framework |
| `factor-models-bhar.Rmd` | API for FF3/FF5/Carhart/GARCH/BHAR, factor table setup | Formulas for each model, estimation assumptions, when-to-use decision tree, rendered CAR comparison across models |
| `time-varying-models.Rmd` | RollingWindow + GARCH API | GARCH(p,q) / DCC-GARCH math, volatility-clustering intuition, rendered sigma-over-time plots |
| `custom-models.Rmd` | How to subclass ModelBase | Nothing — this is an extension guide, not a method page |
| `custom-test-statistics.Rmd` | How to subclass TestStatisticBase | Nothing — extension guide |
| `diagnostics-validation.Rmd` | API for validate_task/model_diagnostics/pretrend_test | What each diagnostic detects statistically; rendered residual plots; decision rules |
| `inference-robustness.Rmd` | HAC, KP-test, bootstrap, p-adjustment API | HAC/Newey-West theory; KP correction math; rendered bootstrap CI bands |
| `panel-event-study.Rmd` | TWFE setup, Miller (2023) data structure | Rendered event-time plots; pre-trend test output; TWFE bias illustration |
| `modern-did-estimators.Rmd` | Sun-Abraham / CS / BJS / de CH API | Heterogeneous-effects theory; rendered group x time estimates; comparison across estimators |
| `intraday-event-study.Rmd` | POSIXct setup, nonparametric test API | Microstructure contamination; VWAP benchmark; rendered 5-min CAR plot |
| `synthetic-control.Rmd` | SyntheticControlTask API, placebo | Abadie (2010) theory; rendered gap plot with placebo p-values |
| `result-extraction.Rmd` | tidy/export/LaTeX API | Nothing — output guide, not a method page |
| `cross-sectional-analysis.Rmd` | cross_sectional_regression API | Fama-MacBeth structure; rendered coefficient table with CARs on firm chars |
| `simulation-power-analysis.Rmd` | simulate_event_study API | Power curves rendered; how event-window length affects Type-I error |
| `volume-volatility-event-study.Rmd` | VolumeModel/VolatilityModel API | Rendered volume-AR and vol-AR plots; interpretation |
| `ai-advisor.Rmd` | es_diagnostics + es_advise walkthrough with Dieselgate | Nothing — walkthrough, not a method page |
| `data-download.Rmd` | download_stock_data/factor_data API | Nothing — utility vignette |
| `automated-reports.Rmd` | generate_report API | Nothing — utility vignette |

---

## Feature Landscape

### Table Stakes (Users Expect These)

Features that must be present for the docs to feel thorough rather than auto-generated.

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Rendered code output on every article | Without actual output, readers cannot verify the code does what is claimed | MEDIUM | Requires bundled offline data + `set.seed`; existing vignettes are all `eval=FALSE` |
| LaTeX / MathJax formula rendering for every method | An econometrics package without math in its docs feels incomplete to an academic audience | LOW | pkgdown supports `math-rendering: mathjax` in `_pkgdown.yml`; only requires YAML change + proper `$$` syntax |
| When-to-use decision guidance per method family | Users need to know which model/test to choose; currently no guidance | MEDIUM | One comparison table + decision tree per family; minimal prose, high value |
| Academic references per method | Users cite these methods in papers; they need the original papers | LOW | Static bibliography section; known papers (Patell 1976, BMP 1991, Fama-French 1993/1996, etc.) |
| A Learn/Methods section in the navbar | Without a Methods section, the site reads as a reference-only API dump | LOW | `_pkgdown.yml` navbar + `articles:` grouping; already partially done in the current grouping |
| Worked examples with real (or realistic) data | Code-only articles without output look like untested stubs | HIGH | Requires bundled datasets or fully offline simulated data with `set.seed` |
| Gallery landing page with domain cards | Users arrive at the examples section and need quick orientation | MEDIUM | `gallery.Rmd` exists; needs to become a real visual index (card layout + domain labels) |

### Differentiators (Competitive Advantage)

Features that go beyond what any other R event-study package offers.

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| pyfda-style method page template with assumption checklist | Makes the docs feel like a textbook chapter, not a function reference | MEDIUM | A repeatable template lowers per-article marginal cost after the first two are done |
| Cross-method comparison tables within a family | Lets users see Patell vs BMP vs KP vs Sign in one place instead of reading four pages | MEDIUM | One well-designed table per family (return models, test statistics); decision logic embedded |
| Advisor tie-in callouts | At the end of each method page, a "What es_advise() checks for this method" note connects learning content to the package's unique feature | LOW | Static prose block; requires no new code |
| Cross-domain gallery (8 examples, different sectors) | Makes the package feel production-grade by showing it works on real research questions, not just toy examples | HIGH | Depends on dataset availability; highest user value item in the milestone |
| Rendered interactive Plotly output embedded in articles | Shows the actual interactive experience users will get, not just static screenshots | MEDIUM | Plotly htmlwidgets render natively in pkgdown articles via `htmltools`; requires `eval=TRUE` chunks |
| Method-page assumption checklist callout box | A formatted callout box listing "This method assumes: (1) …" — scannable, not buried in prose | LOW | Pure Rmd formatting; no code required |
| Offline-safe build with bundled datasets | Zero network dependency at `pkgdown::build_site()` time — site builds in CI without API keys or internet access | HIGH | Each gallery example needs a bundled dataset (or `simulate_event_study()` output with `set.seed`) |

### Anti-Features (Commonly Requested, Often Problematic)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Re-implementing methods theory already covered in academic papers | Completeness instinct | Scope-creeps into a textbook; maintenance burden when theory is stable | Write a short summary + link to the original paper; use LaTeX for the key formula only |
| Live network calls at build time (Yahoo Finance, French library) | Want real live data | Breaks CI behind firewalls; nondeterministic outputs; Yahoo Finance ToS prohibits redistribution | Bundle pre-downloaded snapshots with `data-raw/` provenance scripts; use `simulate_event_study()` for examples that only need plausible structure |
| Interactive R Shiny widgets in articles | Interactivity appeal | Shiny requires a server; pkgdown articles are static HTML | Use Plotly htmlwidgets (client-side JS only; no server needed); they embed fine in pkgdown |
| Bundling real stock price data with no clear open license | Realistic examples | Yahoo Finance data has ToS restrictions on redistribution; French library data is copyright Fama and French with no explicit redistribution license | Use simulated data generated by `simulate_event_study()` with `set.seed()`; or use the one verified-clear dataset (VW dieselgate, already bundled); for gallery examples requiring event dates, embed only the event-date list as a small vector |
| One mega-vignette covering all methods | One-stop shop desire | Unnavigable; breaks pkgdown's section structure; cannot link to a specific section | One article per method family; cross-link liberally |
| Pixel-perfect article layouts with custom CSS per page | Design polish | CSS scope issues in pkgdown; breaks on pkgdown upgrades | Use standard BS5 callout divs and pkgdown's native card layout; rely on the existing `extra.css` from v0.62.0 |

---

## The pyfda Method-Page Template

Based on detailed study of `sipemu.github.io/pyfda` — specifically the Smoothing page (the most fully-realized example, approximately 4,500 words, 8+ rendered figures, 15+ code blocks, 12 sections) and the Introduction page.

### What makes pyfda pages thorough (not stubs)

1. **Conceptual intro before any code** — answers "what problem does this method solve?" in 2–3 sentences with a real-world motivation
2. **Mathematical notation alongside code** — not a textbook derivation, but the key formula written in LaTeX so readers can match it to academic references
3. **Reproducible examples with fixed seeds** — every code block runs and produces the shown output
4. **Side-by-side visualizations comparing methods** — a table or overlay plot showing all variants at once (not one plot per section)
5. **A "When to Use" decision table** — the single most referenced element; scannable, action-oriented
6. **Progressive complexity** — definition to simple example to advanced variant to comparative summary
7. **References section** — 3–5 citations, no more (avoids becoming a literature survey)
8. **Cross-links to related articles** — "See Also" section directs readers to dependent or complementary topics

### Concrete Page Template (reusable for every EventStudy Methods article)

Every new Methods article in `vignettes/articles/` must use this section structure:

```
## Overview
2–3 sentences: what problem this family solves, where it fits in the pipeline,
and the key invariant the user must understand.

## Methods in This Family
Quick-reference table: method name | class | one-line distinguishing characteristic.

## Core Formula
$$key formula in LaTeX$$
Plain-English gloss of each symbol. No derivation — just identification.

## Assumptions
Callout box (use a div or blockquote styled as a note):
  "This method assumes: (1) ... (2) ... (3) ..."
  "Violated when: [most common real-world violation and what to use instead]"

## When to Use Each Method
Table: scenario | recommended method | one-line reason.
Followed by one bold rule-of-thumb sentence.

## Complete Worked Example
2–3 sentence description of the event/dataset/what output demonstrates.
Code chunks (eval=TRUE, set.seed fixed):
  - Load bundled dataset
  - Run pipeline (prepare -> fit -> calculate_statistics)
  - plot_event_study() with rendered Plotly output
  - tidy() or print() showing a result table
2–3 sentences interpreting the rendered output.

## Comparing Methods in This Family
One comparative plot or table showing all variants on the same data.

## Diagnostics and Robustness Checks
Table: diagnostic flag | meaning | action.

## AI Advisor Connection
Bullet list: which es_diagnostics() fields this method surfaces;
what the KB rule recommends when each flag fires.

## References
3–5 citations in author (year) format.

## See Also
Links to related Methods articles and reference functions.
```

**Length target:** 1,500–2,500 words of prose plus all code chunks. The assumption checklist, when-to-use table, and comparative output are the three non-negotiable elements in every article.

---

## Per-Method-Family Required Content Sections

### 1. Return Models Family

**Article title:** "Return Models — Estimating Normal Returns"

**Must contain:**
- Overview table: all 13 models with class name and one-line description
- Market model OLS formula plus abnormal return definition: `AR_it = R_it - (alpha_i + beta_i * R_mt)`
- Factor model formula (generic: `AR = R - (alpha + beta_1*F_1 + ... + beta_k*F_k)`) with factor table requirements
- GARCH(1,1) variance equation; DCC extension one-liner; when volatility-clustering invalidates OLS sigma
- Rolling-Window: rolling beta concept, window length tradeoff (stability vs. responsiveness)
- BHAR compound-return formula and the rebalancing-bias argument (why summing daily ARs overstates long-run effects)
- When-to-use decision table (estimation window < 60 days → rolling window; long horizon > 6 months → BHAR; volatile event period → GARCH; multi-factor risk → FF3/FF5/Carhart)
- Rendered: one example running MarketModel + FamaFrench3FactorModel on the same events, outputting a CAR comparison table
- Rendered: `plot_event_study()` with confidence bands
- Assumption checklist for OLS-based models (i.i.d. residuals, stationarity, no event-induced variance change)

**What the existing vignette does NOT cover:** formulas, assumptions, rendered output, cross-model comparison

### 2. Test Statistics Family

**Article title:** "Test Statistics — Measuring Statistical Significance"

**Must contain:**
- Classification diagram: Single-event (ARTTest, CARTTest) vs Multi-event (CSectTTest, PatellZ, BMP, Sign, GenSign, Rank, KP, CalTimePF)
- AR t-test formula including the forecast error correction term `(1/T_e + (R_m - R_m_bar)^2 / sum(...))` — the most commonly misunderstood detail, responsible for the difference between OLS and event-study standard errors
- Patell standardization formula; key assumption: the event does not change return variance
- BMP: Patell plus cross-sectional variance correction; assumption: cross-sectional independence
- KP (Kolari-Pynnonen): BMP plus Scholes-Williams cross-correlation adjustment; when mandatory (economy-wide events, clustered dates)
- Sign test: non-parametric, does not require normality; fraction of positive ARs vs expected 0.5
- Calendar-Time Portfolio: Fama-MacBeth monthly portfolio regression; clusters correlation automatically
- When-to-use table: clustering calendar dates → KP; non-normal residuals → Sign/Rank; few events → bootstrap; long window → CalTimePF
- Rendered: all multi-event statistics on one dataset, printed as a comparison table showing how test values diverge when assumptions are violated
- Power and size discussion (cross-reference simulation vignette)

**What the existing vignette does NOT cover:** formulas, full taxonomy, when-to-use, rendered output

### 3. Panel DiD Family

**Article title:** "Panel Event Studies and DiD Estimators"

**Must contain:**
- TWFE estimating equation; relative-time indicator definition; reference period normalization (why t = -1 is the standard reference)
- Staggered adoption: why TWFE is biased with heterogeneous effects (Sun-Abraham 2021, Callaway-Sant'Anna 2021); the "negative weights" intuition
- Sun-Abraham: interaction-weighted estimator formula (schematic); aggregation to ATT
- Callaway-Sant'Anna: group x time ATT definition; doubly-robust estimation
- BJS (Borusyak-Jaravel-Spiess): imputation estimator one-liner
- de Chaisemartin-D'Haultfoeuille: `did_multiplegt` approach; when to use over CS
- When-to-use table: single cohort → TWFE; staggered + homogeneous effects → TWFE with pre-trend test; staggered + heterogeneous → CS or SA; single-treated unit → synthetic control
- Rendered: event-time plot with pre-trend and post-treatment estimates plus confidence bands (the primary visual payoff of the panel article)
- Rendered: pre-trend test table (p-values, joint F-test)
- Endpoint binning note (what happens at the first and last relative-time period)

**What the existing vignettes DO cover:** API setup (panel-event-study.Rmd), modern estimators (modern-did-estimators.Rmd). **What they do NOT cover:** rendered outputs, theory formulas, estimator comparison table.

### 4. Intraday Family

**Article title:** "Intraday Event Studies — Minute and Second Windows"

**Must contain:**
- Why intraday differs: bid-ask bounce, microstructure contamination, VWAP benchmark, non-normality of high-frequency returns
- POSIXct window definition; how `relative_index` maps to minutes; estimation window requirements at intraday frequency
- Nonparametric test justification (intraday return distributions have fat tails and are not normal)
- Rendered: 5-minute AR plot around a simulated announcement event
- When intraday is necessary vs daily: earnings call replay (intraday); central bank press conference (intraday); daily close-to-close for all other uses
- Assumption: data must be synchronous; how to handle pre-market and after-hours contamination

**What the existing vignette does NOT cover:** rendered output, microstructure theory, when intraday is necessary vs daily

### 5. Synthetic Control Family

**Article title:** "Synthetic Control — Counterfactuals for Single Treated Units"

**Must contain:**
- Abadie-Gardeazabal (2003) / Abadie-Diamond-Hainmueller (2010) setup: donor pool, outcome variable, predictor matching objective
- Optimization objective: minimize pre-treatment MSPE (mean squared prediction error)
- Placebo inference: permute treatment assignment across donor units; compute MSPE ratio for each placebo; p-value = fraction with ratio >= treated unit
- When to use: single treated unit; long pre-treatment period (at least 2x the event window); no parallel-trends exclusion restriction needed
- Rendered: gap plot (actual minus synthetic) with placebo lines overlaid
- Rendered: predictor balance table (pre-treatment outcomes: treated vs synthetic vs donors)
- When it fails: donor pool too small (< 5 units); poor pre-treatment fit (MSPE ratio < 2)

**What the existing vignette does NOT cover:** rendered output, theory, placebo inference table

### 6. Diagnostics Family

**Article title:** "Model Diagnostics — Validating Event Study Assumptions"

**Must contain:**
- Shapiro-Wilk: null hypothesis, decision rule (p < 0.05 → reject normality), consequence → Switch to Sign test or Rank test or bootstrap
- Durbin-Watson / Ljung-Box: what autocorrelation in estimation-window residuals implies for standard errors; consequence → HAC (Newey-West) SEs
- Pre-trend test: test whether ARs in pre-event window [-k, -1] are jointly zero; Miller (2023) recommendation for k
- `validate_task()`: what each check catches (missing event dates, window overlap, insufficient estimation observations, all-NA returns)
- Rendered: `plot_diagnostics()` output with residual QQ plot and ACF plot (deterministic with `set.seed`)
- Decision table: each diagnostic flag → recommended action → which package function implements the action
- AI advisor integration: which diagnostics flow directly into `es_diagnostics()` and what KB rule fires

**What the existing vignette does NOT cover:** rendered output (all `eval=FALSE`), theory behind each test, the action table mapping flags to remedies

### 7. AI Advisor Family

**Article title:** "The Grounded AI Advisor — Architecture and Usage"

**Must contain:**
- The grounding invariant (never fabricate a number) and why it matters for scientific reproducibility
- Two-layer architecture: `es_diagnostics()` (deterministic, zero-dependency, always available) then `es_advise()` (LLM-grounded, optional)
- Diagnostics schema: what fields `es_diagnostics()` harvests and why each one is included
- Six advice modes and what each returns (model selection, test selection, result interpretation, robustness, reporting, comparison)
- Provider configuration: arg to env var to default precedence; how to configure Anthropic vs OpenAI-compatible vs Ollama
- Offline mode: what happens when no API key is configured (rule-based KB output from `es_kb`)
- `recommend_stat()` and `flag_robustness()` as standalone utilities
- Rendered: `es_diagnostics()` output printed (deterministic, always evaluable without API key)
- Rendered: `es_advise()` offline KB response (no LLM needed; uses rule-based engine)

**What the existing vignette DOES cover:** the walkthrough with Dieselgate data and both layers. **What a Methods article adds:** the architectural explanation (why two layers), full decision logic for each advice mode, provider setup guide, and `recommend_stat()` / `flag_robustness()` standalone use.

---

## Cross-Domain Gallery: 8 Concrete Example Proposals

Each gallery example is a complete end-to-end rendered analysis in `vignettes/articles/`. The selection covers the breadth of event study applications and showcases specific method families distinctly from one another.

### G-1: Corporate Scandal — Emissions Manipulation (Dieselgate)

**Domain:** Corporate governance / environmental
**Event:** VW Dieselgate announcement, September 18 2015
**Model:** MarketModel + FamaFrench3FactorModel (compare both on same events)
**Tests:** CARTTest, PatellZ, BMPTest
**Dataset:** `dieselgate` — already bundled; multi-automaker (VW, BMW, Daimler, Peugeot)
**Rendered output:** AAR + CAAR plot with 95% CI bands; cross-automaker CAAR comparison table showing spillover effects; Patell vs BMP result comparison showing BMP correction matters for the event-induced variance spike
**What it demonstrates:** Market model vs FF3 in a volatile-event setting; BMP correction; multi-group comparison with `car_by_group()`
**Data licensing:** CLEARED. Already bundled with provenance in `data-raw/`. No new licensing issue.

### G-2: Earnings Surprise — Cross-Sectional Regression on SUE

**Domain:** Accounting / corporate finance
**Event:** Quarterly earnings announcements; positive vs negative surprise
**Model:** MarketModel (standard; followed by cross-sectional regression of CARs on SUE magnitude)
**Tests:** CSectTTest; `cross_sectional_regression()` with SUE as regressor; `car_by_group()` for quintile splits
**Dataset:** Simulated via `simulate_event_study(n_events = 60, seed = 42)` with earnings-surprise magnitude assigned as a firm characteristic column
**Rendered output:** CAAR plot split by surprise quintile; cross-sectional regression table (CAR ~ SUE + log_size) with coefficient estimates and standard errors
**What it demonstrates:** Cross-sectional regression as the natural follow-on to abnormal return estimation; `car_quantiles()` for distributional view
**Data licensing:** CLEARED. Fully simulated by the package. Note in article that real SUE data can be sourced from IBES/Compustat via institutional access.

### G-3: Monetary Policy — FOMC Rate Decisions

**Domain:** Macroeconomics / monetary economics
**Event:** FOMC announcement dates — rate hike vs hold vs cut
**Model:** MarketModel with event window [-1, +1]; multi-firm (diversified sector portfolio proxies)
**Tests:** PatellZ, KolariPynnonenTest (mandatory when all firms move together), bootstrap_test
**Dataset:** Simulated panel with 20 portfolio-level series + a small vector of FOMC announcement dates (dates are public information from the Federal Reserve website — only dates embedded, no proprietary price data)
**Rendered output:** AAR plot showing announcement-day spike; KP vs Patell comparison table (KP delivers much wider CIs due to cross-sectional correlation); bootstrap confidence bands
**What it demonstrates:** Why KP test is mandatory for economy-wide events; how calendar clustering destroys Patell/BMP independence assumption; bootstrap as the non-parametric alternative
**Data licensing:** CLEARED. FOMC dates are public (Federal Reserve website). Price data fully simulated. Provenance note in article: the U.S. Monetary Policy Event-Study Database (FRBSF USMPD) provides real event-time surprises for researchers who want the real-data version.

### G-4: Drug Approval — FDA Phase 3 Outcomes

**Domain:** Life sciences / pharmaceutical
**Event:** FDA approval vs rejection decisions (Phase 3 trial outcomes for biotech firms)
**Model:** MarketModel; event window [-2, +2] to capture pre-announcement leakage; single-event focus
**Tests:** ARTTest, CARTTest; `plot_car_distribution()` to show bimodal distribution
**Dataset:** Simulated 15-firm biotech panel with event outcome label (approval vs rejection); designed to replicate the distributional property (large positive CAR on approval, large negative on rejection)
**Rendered output:** Forest plot of per-event CARs sorted by outcome; histogram of abnormal returns showing bimodal distribution; `plot_car_distribution()` output
**What it demonstrates:** Single-event focus vs multi-event aggregation; the `plot_car_distribution()` function; interpreting individual-event CARs in small samples; pre-event leakage window choice
**Data licensing:** CLEARED. Fully simulated. Provenance note: real FDA NDA/BLA approval dates are public at FDA.gov; real prices require institutional data access.

### G-5: Staggered Regulatory Shock — GDPR Enforcement Actions

**Domain:** Regulation / technology / privacy
**Event:** GDPR enforcement decisions (staggered across 2019–2022; different firms penalized at different times)
**Model:** Callaway-Sant'Anna panel estimator (staggered adoption design); comparison against biased TWFE
**Tests:** `estimate_panel_event_study()` with `estimator = "callaway_santanna"`; pre-trend test; TWFE comparison to show the negative-weights bias
**Dataset:** Simulated staggered-treatment panel (40 firms, 3 treatment cohorts: 2019, 2020, 2022)
**Rendered output:** Event-time plot showing TWFE aggregated estimate vs CS group-x-time estimates; pre-trend test table; TWFE bias visualization showing the sign reversal that heterogeneous effects produce
**What it demonstrates:** The heterogeneous-treatment-effect bias in TWFE; when CS/SA is necessary vs sufficient; practical use of `modern-did-estimators` in a regulatory context; the "negative weights" diagnostic
**Data licensing:** CLEARED. Fully synthetic. Provenance note: real GDPR fines are public (enforcementtracker.com); stock price data for named firms available via `download_stock_data()` for those who want to replicate with real data.

### G-6: Dividend Announcement — Payout Policy and Non-Parametric Tests

**Domain:** Corporate payout policy
**Event:** Cash dividend initiation vs share buyback announcement
**Model:** Carhart4FactorModel (momentum factor important for payout studies)
**Tests:** CSectTTest, SignTest, GeneralizedSignTest; comparison of parametric vs non-parametric outcomes
**Dataset:** Simulated 30-firm panel; two groups (dividend initiation vs buyback)
**Rendered output:** CAAR plot for both groups on same axes; Sign test vs t-test comparison table; `car_by_group()` output; discussion of when parametric and non-parametric results diverge
**What it demonstrates:** Non-parametric tests in practice; group comparison; Carhart model applied to payout events; what divergence between Sign and t-test implies about the return distribution
**Data licensing:** CLEARED. Fully simulated.

### G-7: Intraday — Central Bank Press Conference

**Domain:** Macroeconomics / intraday microstructure
**Event:** Simulated central bank press conference starting at 14:30 (ECB/Fed style)
**Model:** IntradayEventStudyTask; 1-minute windows; VWAP benchmark
**Tests:** `nonparametric_intraday_test()`
**Dataset:** Simulated 1-minute OHLC data for 5 broad-market ETF proxies across a 6.5-hour trading day
**Rendered output:** 5-minute CAR plot showing the announcement spike with pre-event flat region and post-event drift; printed nonparametric test result with p-value
**What it demonstrates:** POSIXct-based pipeline from raw setup to result; intraday windows and how the CAR plot differs visually from daily studies; microstructure contamination note (bid-ask bounce in the minutes immediately before/after)
**Data licensing:** CLEARED. Fully simulated at 1-minute frequency.

### G-8: Synthetic Control — Single Firm vs Donor Pool (Dieselgate Subset)

**Domain:** Single-unit causal inference / corporate governance
**Event:** VW emissions scandal as a single-treated-unit problem (VW vs. the European automotive sector donor pool)
**Model:** SyntheticControlTask; `estimate_synthetic_control()`; `sc_placebo_test()`
**Tests:** Placebo MSPE ratio; `plot_synthetic_control()`
**Dataset:** Subset of the bundled `dieselgate` dataset: VW as the treated unit, BMW + Daimler + Peugeot as donor pool (no new data required)
**Rendered output:** Gap plot (VW actual minus synthetic VW) with placebo lines overlaid for each donor; predictor balance table (pre-treatment outcomes: treated vs synthetic vs each donor); MSPE ratio for inference (p-value interpretation)
**What it demonstrates:** Synthetic control as the alternative to standard event study when there is only one treated entity; donor pool selection; how the placebo test yields a p-value without distributional assumptions; contrast with G-1 which uses the same dataset under the multi-event paradigm
**Data licensing:** CLEARED. Uses existing bundled `dieselgate` dataset. No new dataset required.

---

## Dataset Strategy and Licensing Analysis

### The Core Problem

Real production-quality financial event study data (CRSP, Compustat, IBES, WRDS) is proprietary and cannot be bundled. Yahoo Finance data has ToS restrictions on redistribution (ToS states data is for personal use only). Ken French factor data is copyrighted by Fama and French with no explicit redistribution license. The FRBSF USMPD database has no stated license; contact required before bundling.

### Resolution: Three Tiers

**Tier 1 — Already Bundled (use as-is)**
- `dieselgate`: already in the package, `data-raw/` provenance documented. Covers G-1 and G-8 with no new licensing work.

**Tier 2 — Simulate with `simulate_event_study()` and `set.seed()`**
Use the package's own `simulate_event_study()` function with documented seeds for G-2 (earnings), G-3 (FOMC), G-4 (FDA), G-5 (GDPR), G-6 (dividends), G-7 (intraday). Simulated data is:
- Zero licensing risk — generated by the package itself
- Offline-safe — no network at build time
- Reproducible — `set.seed()` makes it deterministic across machines
- Methodologically honest — each article notes that real data requires institutional access and points to the appropriate source

Precedent: the `eventstudies` CRAN package (nipfpmf) bundles simulated `SplitDates` + `StockPriceReturns` data. This is the accepted CRAN pattern for event-study packages.

**Tier 3 — Provenance-only scripts in `data-raw/` (for reproducibility, not bundled)**
For gallery examples where real data would be scientifically preferable, include a `data-raw/make_[name].R` script that downloads and processes the data. This lets a researcher with institutional access reproduce the real-data version, while the bundled version uses simulated data. This follows the same pattern as the `dieselgate` dataset established in v0.61.0.

### Dataset Decision Table

| Gallery Example | Data Strategy | What Gets Bundled | Licensing Status |
|---|---|---|---|
| G-1 Dieselgate | Existing bundled `dieselgate` | `dieselgate` (existing) | CLEARED — simulated/curated with public event dates |
| G-2 Earnings Surprise | Simulate inline via `simulate_event_study(seed=42)` | `earnings_sim` small Rda (< 50 KB) | CLEARED — package-generated |
| G-3 FOMC | Simulate prices; embed FOMC date vector (12 dates) | `fomc_sim` (dates + simulated prices) | CLEARED — Fed dates are public; prices simulated |
| G-4 FDA Approvals | Simulate; embed event outcome lookup (15 events) | `fda_sim` | CLEARED — FDA approval dates are public; prices simulated |
| G-5 GDPR | Fully simulated staggered panel | `gdpr_sim` | CLEARED — fully synthetic |
| G-6 Dividend/Buyback | Fully simulated | `payout_sim` | CLEARED — fully synthetic |
| G-7 Intraday | Fully simulated 1-minute OHLC | `intraday_sim` | CLEARED — fully synthetic |
| G-8 Synthetic Control | Subset of `dieselgate` (no new data) | No new dataset needed | CLEARED — existing bundled data |

**CRAN tarball note:** All datasets used only by `vignettes/articles/` files (which are `.Rbuildignore`d). Under the pkgdown-only strategy, the simulation code runs inline at article build time, or small Rda files live in `vignettes/articles/data/` and are never installed by `R CMD INSTALL`. Either way the CRAN tarball is not affected. Estimated size per simulated dataset: under 50 KB uncompressed.

---

## Feature Dependencies

```
LaTeX/MathJax formula rendering
    requires: `_pkgdown.yml` math-rendering: mathjax config change (one line)

Rendered Outputs (eval=TRUE chunks)
    requires: bundled datasets or inline simulate_event_study() with set.seed()
    requires: vignettes/articles/ delivery path (.Rbuildignore'd)

Methods Articles
    requires: Rendered Outputs (otherwise they are just duplicating existing vignettes)
    enables:  Advisor Tie-in Callouts (can reference specific diagnostic flags)

Gallery Examples
    requires: bundled/simulated datasets
    requires: Methods Articles (gallery links to method pages for theory)
    enables:  Gallery Landing Page (needs real content to link to)

Gallery Landing Page
    requires: Gallery Examples (needs examples to display as cards)
    enables:  Navbar Articles link to become meaningful
```

---

## MVP Definition

### Phase 1 — Foundation (deliver first, unblocks everything else)

- [ ] `_pkgdown.yml` math-rendering: mathjax — enables LaTeX in all articles; one YAML line; zero risk
- [ ] Reusable Rmd method-article skeleton with all 8 sections templated — lowers per-article marginal cost dramatically
- [ ] Return Models Methods article (rendered) — highest-traffic concept; `factor-models-bhar` is the most complete existing vignette to build from
- [ ] Test Statistics Methods article (rendered) — second most requested; unique formula content not present in any existing vignette
- [ ] G-1 Dieselgate gallery example (rendered) — uses existing data; proof-of-concept that the gallery format works

### Phase 2 — Method Coverage + Core Gallery

- [ ] Diagnostics Methods article (advisor integration callout is the unique value-add)
- [ ] Panel DiD Methods article (rendered event-time plot is the visual centerpiece)
- [ ] Synthetic Control Methods article (rendered gap plot with placebo)
- [ ] G-8 Synthetic control gallery example (uses existing data; exercises `plot_synthetic_control()`)
- [ ] G-3 FOMC monetary policy gallery (high academic interest; showcases KP test necessity)
- [ ] G-5 GDPR staggered panel gallery (highest-differentiation example; CS estimator showcase)

### Phase 3 — Complete Gallery + Minor Articles

- [ ] Intraday Methods article (shortest; new microstructure content)
- [ ] AI Advisor Methods article (architectural depth beyond the existing walkthrough vignette)
- [ ] G-2 Earnings + cross-sectional regression gallery
- [ ] G-4 FDA drug approval gallery (exercises `plot_car_distribution()`)
- [ ] G-6 Dividend/buyback gallery (non-parametric test showcase)
- [ ] G-7 Intraday press conference gallery (exercises `IntradayEventStudyTask`)
- [ ] Gallery landing page updated to real card index with domain labels

### Defer to v0.64.0

- [ ] Printed PDF export of articles — beyond pkgdown's native capability
- [ ] Full Shiny interactivity — requires a server; incompatible with static pkgdown delivery
- [ ] Cross-article search index — pkgdown already provides site-level search

---

## Feature Prioritization Matrix

| Feature | User Value | Implementation Cost | Priority |
|---------|------------|---------------------|----------|
| LaTeX formula rendering (one config line) | HIGH | LOW | P1 |
| Reusable Rmd article template skeleton | HIGH | LOW | P1 |
| Return Models Methods article (rendered) | HIGH | MEDIUM | P1 |
| Test Statistics Methods article (rendered) | HIGH | MEDIUM | P1 |
| G-1 Dieselgate gallery example (rendered) | HIGH | LOW | P1 |
| Diagnostics Methods article | MEDIUM | MEDIUM | P2 |
| Panel DiD Methods article (rendered event-time plot) | HIGH | MEDIUM | P2 |
| Synthetic Control Methods article (rendered gap plot) | MEDIUM | MEDIUM | P2 |
| G-8 Synthetic control gallery | MEDIUM | LOW | P2 |
| G-3 FOMC monetary policy gallery | HIGH | MEDIUM | P2 |
| G-5 GDPR staggered panel gallery | HIGH | MEDIUM | P2 |
| G-2 Earnings + cross-sectional gallery | HIGH | MEDIUM | P2 |
| G-4 FDA approval gallery | MEDIUM | LOW | P2 |
| Advisor tie-in callout in every Methods article | HIGH | LOW | P2 |
| Intraday Methods article | LOW | MEDIUM | P3 |
| AI Advisor Methods article | MEDIUM | LOW | P3 |
| G-6 Dividend/buyback gallery | MEDIUM | LOW | P3 |
| G-7 Intraday press conference gallery | MEDIUM | MEDIUM | P3 |
| Gallery landing page card index redesign | HIGH | MEDIUM | P2 |

**Priority key:** P1 = must have for milestone; P2 = should have, same milestone; P3 = nice-to-have, can slip to v0.64.0

---

## Competitor Documentation Analysis

| Feature | `estudy2` (irudnyts) | `eventstudies` (nipfpmf) | EventStudy current (v0.62.0) | v0.63.0 target |
|---------|---|---|---|---|
| Method formulas in docs | None — reference pages only | None | None | Full LaTeX per method family |
| Rendered outputs in articles | None | PDF vignette (eval=TRUE) | None (all eval=FALSE) | Rendered plots and tables in every article |
| Decision guidance / when-to-use | None | None | None | When-to-use table in every Methods article |
| Cross-domain gallery | None | None | Dieselgate intro only | 8 cross-domain examples |
| Academic references | Function-level only | Yes (Kothari-Warner cites) | Minimal | Full per-method bibliography |
| AI advisor connection | None | None | Yes (v0.60.0, separate vignette) | Callout box in every method page |
| Non-parametric tests documented | Partial | Yes | Yes (API) | Theory + when-to-use + rendered |

---

## Sources

- `sipemu.github.io/pyfda` — reference standard; Smoothing page structure analyzed in detail (approximately 4,500 words, 8+ figures, 15+ code blocks, 12 sections including decision table, formula sections, and references)
- `sipemu.github.io/pyfda/learn/introduction` — Introduction page structure confirming progressive complexity pattern
- Patell (1976), "Corporate Forecasts of Earnings per Share," *Journal of Accounting Research*, 14(2):246-276
- Boehmer, Musumeci, Poulsen (1991), "Event-Study Methodology Under Conditions of Event-Induced Variance," *Journal of Financial Economics*, 30(2):253-272
- Fama, French (1993), "Common Risk Factors in the Returns on Stocks and Bonds," *Journal of Financial Economics*, 33(1):3-56
- Kolari, Pynnonen (2010), "Event Study Testing with Cross-Sectional Correlation of Abnormal Returns," *Review of Financial Studies*, 23(11):3996-4025
- Abadie, Diamond, Hainmueller (2010), "Synthetic Control Methods for Comparative Case Studies," *Journal of the American Statistical Association*, 105(490):493-505
- Miller (2023), "An Introductory Guide to Event Study Models," *Journal of Economic Perspectives*, 37(2):203-230
- Callaway, Sant'Anna (2021), "Difference-in-Differences with Multiple Time Periods," *Journal of Econometrics*, 225(2):200-230
- U.S. Monetary Policy Event-Study Database (FRBSF USMPD) — publicly downloadable; no explicit redistribution license stated; contact cmr@sf.frb.org before bundling
- Yahoo Finance ToS — data intended for personal use only; redistribution not permitted; do not bundle
- Ken French Data Library (mba.tuck.dartmouth.edu) — Copyright Fama and French; no explicit redistribution license; do not bundle
- `eventstudies` CRAN package (nipfpmf) — bundles `SplitDates` and `StockPriceReturns` as the accepted CRAN pattern for simulated event-study data
- pkgdown documentation — `math-rendering: mathjax` configuration; articles rendered from `vignettes/articles/` with `.Rbuildignore` exclusion

---
*Feature research for: EventStudy v0.63.0 — Documentation Depth: Methods and Worked Examples*
*Researched: 2026-09-05*
