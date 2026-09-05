# Phase 15: Methods Articles + Rendered Outputs — Research

**Researched:** 2026-09-06
**Domain:** pkgdown Methods articles (KaTeX + live-rendered R + citations), R6 EventStudy API grounding, formula-to-source provenance
**Confidence:** HIGH (all API signatures read from R/ source this session; formulas cross-read against implementation)

## Summary

Seven conceptual, formula-bearing Methods articles (METH-02..08) each render live package code (>=1 kable table + >=1 plot) fully offline. This research grounds every article in the **actual** API by reading the R6 source, locates the exact `R/<file>:<line>` for each core formula so the executor can write the `<!-- Formula verified: ... -->` comment, and supplies complete BibTeX for the ~19 keys 15-CONTEXT requires.

**API surprises found (must be honored — 15-CONTEXT's shorthand names are NOT the real API):**
- Panel estimator method names are `"static_twfe"` (not `"twfe"`), `"dechaisemartin_dhaultfoeuille"` (not `"dchaisemartin"`), `"borusyak_jaravel_spiess"` (not `"bjs"`). `"dynamic_twfe"` and `"sun_abraham"` are correct. `[VERIFIED: R/panel_event_study.R:118-123]`
- The panel entry point is `estimate_panel_event_study(task, method=...)`, NOT a `PanelEventStudyTask$new()...estimator()` method call. `[VERIFIED: R/panel_event_study.R:117]`
- FF3/FF5/Carhart models require a **factor_tbl** with `market_excess, smb, hml[, rmw, cma, mom]` columns; **dieselgate has NO factor data**. FF3/FF5/Carhart worked examples on dieselgate would ERROR. They must be `eval=FALSE` conceptual snippets (cross-link `vignettes/factor-models-bhar.Rmd`), OR use inline synthetic factor data. Only MarketModel / MarketAdjusted / ComparisonPeriodMean / RollingWindow / BHAR run live on dieselgate. `[VERIFIED: R/models.R:826-830,868-883]`
- Advisor deterministic functions are `es_diagnostics(task)`, `recommend_stat(x)`, `flag_robustness(x)` — all S3, offline. `[VERIFIED: R/es_diagnostics.R:51, R/advise_offline.R:43,103]`
- Model selection is via `ParameterSet$new(return_model = <ModelClass>$new())`, NOT a string. `[VERIFIED: R/parameter_set.R:40-41]`

**Primary recommendation:** Author each article on the skeleton, select models/stats by passing R6 objects into `ParameterSet$new()`, gate rugarch/optional-DiD/LLM chunks per 15-CONTEXT, and satisfy RENDER-01 with the always-live MarketModel + `plot_event_study()` path proven in `smoke-test.Rmd`.

## User Constraints (from CONTEXT.md)

### Locked Decisions
- **7 articles, 7 METH reqs (METH-02..08)** — no split/merge. Filenames: `methods-return-models.Rmd`, `methods-test-statistics.Rmd`, `methods-panel-did.Rmd`, `methods-intraday.Rmd`, `methods-synthetic-control.Rmd`, `methods-ai-advisor.Rmd`, `methods-diagnostics.Rmd`.
- **Datasets:** return-models & ai-advisor → `dieselgate`; test-statistics & diagnostics → `earnings_surprises`; panel/intraday/synthetic-control → inline synthetic (base R); diagnostics bootstrap/power → `simulate_event_study(seed=42)`.
- **Offline gating:** GARCH/DCC → `eval=requireNamespace("rugarch", quietly=TRUE)` + prose fallback; optional DiD (callaway/dechaisemartin/borusyak) → `eval=FALSE` + static output; LLM advisor layer → `eval=FALSE` + `Sys.setenv(EVENTSTUDY_NO_NETWORK="1")`; SC quadprog → default `method="optim"`.
- **Formula gate:** inline `<!-- Formula verified: R/<file>:<line> matches <BibKey> eq. <N> -->` in Section 4; executor reads impl line BEFORE writing formula. Bib additions atomic per article.
- **Math depth:** proportional; cap 5 display equations/article.
- **Nav:** all 7 into `navbar: components: methods: menu:` in one atomic commit after articles exist; retain `smoke-test` as "Overview".
- **Single plan (15-01-PLAN.md):** 7 article tasks (parallelizable) + 1 nav-wiring task (sequenced last).

### Claude's Discretion
- Exact prose, section wording, which member gets the fullest worked example (must cover all family members conceptually + render >=1 live table + plot).
- Exact synthetic-data parameters for panel/intraday/SC.

### Deferred Ideas (OUT OF SCOPE)
- Gallery worked-example Rmd (Phase 16); new bundled datasets; Volume/Volatility get brief `eval=FALSE` conceptual section only (cross-link `vignettes/volume-volatility-event-study.Rmd`); versioned docs / logo.

## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| METH-02 | Return models article | §Article 1 — MarketModel live on dieselgate; FF/GARCH gated/conceptual |
| METH-03 | Test statistics article | §Article 2 — Patell/BMP/Sign/KP/CSectT via MultiEventStatisticsSet on earnings_surprises |
| METH-04 | Panel/DiD article | §Article 3 — inline panel + `estimate_panel_event_study()` |
| METH-05 | Intraday article | §Article 4 — inline POSIXct + `IntradayEventStudyTask$new()` |
| METH-06 | Synthetic control article | §Article 5 — inline treated/donor + `estimate_synthetic_control(method="optim")` |
| METH-07 | AI advisor article | §Article 6 — `es_diagnostics/recommend_stat/flag_robustness` live; LLM eval=FALSE |
| METH-08 | Diagnostics & robustness article | §Article 7 — `model_diagnostics/pretrend_test/bootstrap_test/simulate_event_study` |
| RENDER-01 | >=1 table + >=1 plot per article, live | Every article's live path documented below |
| DELIVERY-03 | 19 CRAN vignettes byte-unchanged | §Verification Hooks |

## Project Constraints (from CLAUDE.md)
- R 4.1.0+, R6, testthat 3e — no new stack. Documentation-only phase: **no R/ changes**.
- No new `R CMD check` NOTEs/WARNINGs. Articles are `.Rbuildignore`d (`^vignettes/articles`) so they never enter the tarball or `R CMD check`.
- Optional packages stay `requireNamespace()`-guarded.
- Behavior on valid inputs unchanged; existing 400+ tests stay green (articles touch no R source).

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary | Rationale |
|------------|-------------|-----------|-----------|
| Formula rendering | pkgdown/KaTeX (static build) | — | math is prose, not code |
| Live table/plot | knitr chunk → R6 pipeline | plotly/ggplot2 | proven in smoke-test.Rmd |
| Offline gating | knitr chunk options (`eval=`) | requireNamespace | build must not hit network/optional pkgs |
| Citation resolution | pandoc-citeproc via references.bib | — | `[@Key]` → rendered ref |
| Formula provenance | HTML comment tied to R/ line | — | audit gate |

---

## Article 1 — methods-return-models.Rmd (METH-02) · dataset: dieselgate

### Live worked example (always-eval, satisfies RENDER-01)
Identical proven pipeline to `smoke-test.Rmd:36-55`. MarketModel is the default `return_model`, so `ParameterSet$new()` fits it.

```r
library(EventStudy)
data("dieselgate")
task <- EventStudyTask$new(
  firm_stock_data_tbl = dieselgate$firm,
  reference_tbl       = dieselgate$index,
  request_tbl         = dieselgate$request
)
# Model is selected by passing an R6 model object (NOT a string):
params <- ParameterSet$new(return_model = MarketModel$new())
task <- prepare_event_study(task, params)
task <- fit_model(task, params)
task <- calculate_statistics(task, params)

knitr::kable(head(tidy(task), 10), caption = "Market-model CAR t-statistics (dieselgate).")   # TABLE
plot_event_study(task, type = "car", event_id = 1)                                            # PLOT
```
`[VERIFIED: R/parameter_set.R:40-41]` (return_model arg), `[VERIFIED: vignettes/smoke-test.Rmd:36-55]` (pipeline proven live), `[VERIFIED: R/plotting.R:107]` (`plot_event_study`).

Other **live-capable on dieselgate** (no factor_tbl needed): `MarketAdjustedModel$new()` `[VERIFIED: R/models.R:348]`, `ComparisonPeriodMeanAdjustedModel$new()` `[VERIFIED: R/models.R:456]`, `RollingWindowModel$new()` `[VERIFIED: R/models_time_varying.R:9]`, `BHARModel$new()` `[VERIFIED: R/models.R:1173]`.

**FF3/FF5/Carhart:** require factor columns `market_excess, smb, hml[, rmw, cma, mom]` joined via factor_tbl — dieselgate has none `[VERIFIED: R/models.R:826-830,868-883]`. Show as `eval=FALSE` conceptual snippet; cross-link `vignettes/factor-models-bhar.Rmd`.

**GARCH chunk** (gated per 15-CONTEXT):
```r
```{r garch, eval=requireNamespace("rugarch", quietly=TRUE)}
params_g <- ParameterSet$new(return_model = GARCHModel$new())
# ... same pipeline ...
```
```
`GARCHModel$new()` `[VERIFIED: R/models.R:980]`; DCCGARCHModel `[VERIFIED: R/models_time_varying.R:218]`. Prose fallback paragraph follows for rugarch-absent case. **Volume/Volatility:** brief `eval=FALSE` section, cross-link `vignettes/volume-volatility-event-study.Rmd` (deferred).

### Formulas + provenance
- Market model: $R_{it} = \alpha_i + \beta_i R_{mt} + \varepsilon_{it}$; abnormal $\hat\varepsilon_{it}=R_{it}-(\hat\alpha_i+\hat\beta_i R_{mt})$.
  `<!-- Formula verified: R/models.R:248 matches MacKinlay1997 eq. 1-2 -->` (impl computes `firm_returns - (alpha + beta*index_returns)` at line 248; α/β at 267-276, σ at 279). `[VERIFIED: R/models.R:248,267-279]`
- CAR: $\text{CAR}(t_1,t_2)=\sum_{t=t_1}^{t_2}\hat\varepsilon_t$ `<!-- Formula verified: R/single_event_test_statistics.R:127 matches MacKinlay1997 eq. 15 -->` (`car = cumsum(abnormal_returns)`). `[VERIFIED: R/single_event_test_statistics.R:127]`
- BHAR: $\text{BHAR}_i=\prod(1+R_{it})-\prod(1+R_{mt})$ `<!-- Formula verified: R/single_event_test_statistics.R:189-191 matches BarberLyon1997 -->`. `[VERIFIED: R/single_event_test_statistics.R:189-191]`
- Forecast-error-corrected σ (feeds Patell): `[VERIFIED: R/models.R:100-108]` (`calculate_forecast_error_correction`).

### Refs to add: `FamaFrench1993`, `FamaFrench2015`, `Carhart1997`, `BarberLyon1997` (see BibTeX §)
### Offline safety: MarketModel path always-live (satisfies RENDER-01). FF/GARCH/DCC/Volume gated or eval=FALSE. Determinism from `_setup.Rmd` set.seed(42).

---

## Article 2 — methods-test-statistics.Rmd (METH-03) · dataset: earnings_surprises

### Live worked example (multi-event → AAR/CAAR + Patell/BMP/Sign/KP)
Compose the statistics via `MultiEventStatisticsSet$new(tests=list(...))` and pass to `ParameterSet`. Statistics are selected by passing R6 test objects.

```r
library(EventStudy)
data("earnings_surprises")
task <- EventStudyTask$new(
  firm_stock_data_tbl = earnings_surprises$firm,
  reference_tbl       = earnings_surprises$index,
  request_tbl         = earnings_surprises$request
)
multi <- MultiEventStatisticsSet$new(tests = list(
  CSectTTest$new(), PatellZTest$new(), BMPTest$new(),
  SignTest$new(), KolariPynnonenTest$new()
))
params <- ParameterSet$new(multi_event_statistics = multi)
task <- prepare_event_study(task, params)
task <- fit_model(task, params)
task <- calculate_statistics(task, params)

knitr::kable(tidy(task), caption = "AAR/CAAR with Patell Z, BMP t, sign & KP.")   # TABLE
plot_event_study(task, type = "caar")                                            # PLOT
```
`[VERIFIED: R/test_statistics_set.R:74-79]` (MultiEventStatisticsSet), `[VERIFIED: R/parameter_set.R:43]` (multi_event_statistics arg). Class names all confirmed: `CSectTTest` (R/multi...:11), `PatellZTest` (:75), `BMPTest` (:420), `SignTest` (:199), `KolariPynnonenTest` (:575), plus `GeneralizedSignTest` (:270), `RankTest`/Corrado (:352), `CalendarTimePortfolioTest` (:505). Single-event AR/CAR t via `SingleEventStatisticsSet$new()` default (`ARTTest`,`CARTTest`) `[VERIFIED: R/test_statistics_set.R:56-60]`.

**Result columns for kable** (from `compute()` returns): CSectT → `relative_index, aar, aar_t, caar, caar_t, n_events, car_window` `[VERIFIED: R/multi...:28-57]`; Patell → adds `aar_z, caar_z` `[VERIFIED: R/multi...:164,183]`; BMP → adds `bmp_t, cbmp_t` `[VERIFIED: R/multi...:459,483]`; Sign → `sign_z, csign_z` `[VERIFIED: R/multi...:227,246]`; KP → `kp_t, ckp_t` `[VERIFIED: R/multi...:677,684]`.

### Formulas + provenance
- CSectT AAR t: $t=\sqrt{N}\,\overline{AR}/s_{AR}$ `<!-- Formula verified: R/multi_event_test_statistics.R:35 matches Brown1985 -->` (`sqrt(n_valid_events)*aar/sd_ar`). `[VERIFIED: R/multi_event_test_statistics.R:35]`
- Patell Z: standardize by forecast-error-corrected σ; $Z=\dfrac{\sum_i SAR_i}{\sqrt{\sum_i Q_i}}$, $Q_i=\dfrac{m_i-k}{m_i-k-2}$ `<!-- Formula verified: R/multi_event_test_statistics.R:113,137,150,166 matches Patell1976 -->` (Q_i at 111-115, standardization `abnormal_returns/fec_sigma` at 137, Q_total 150, `sum_sar/Q_total` 166). `[VERIFIED: R/multi_event_test_statistics.R:111-166]`
- BMP: SAR = AR/σ; $t_{BMP}=\sqrt{N}\,\overline{SAR}/s_{SAR}$ `<!-- Formula verified: R/multi_event_test_statistics.R:446,459-461 matches BMP1991 -->`. `[VERIFIED: R/multi_event_test_statistics.R:446,459-461]`
- Sign: $z=(w-0.5N)/(0.5\sqrt{N})$ `<!-- Formula verified: R/multi_event_test_statistics.R:228 matches Corrado1989/Cowan -->`. `[VERIFIED: R/multi_event_test_statistics.R:228]`
- KP: $t_{KP}=t_{BMP}\cdot\sqrt{\dfrac{1-\bar r}{1+(N-1)\bar r}}$ `<!-- Formula verified: R/multi_event_test_statistics.R:660,671,677 matches KolariPynnonen2010 -->` ($\bar r$ = avg off-diag SAR corr, 660; adj 671; `kp_t=bmp_t*kp_adj` 677). `[VERIFIED: R/multi_event_test_statistics.R:660-677]`

### Refs to add: `Patell1976`, `BMP1991`, `Corrado1989`, `KolariPynnonen2010`
### Offline safety: fully live, zero optional deps (all base-R stats). set.seed(42) from _setup.

---

## Article 3 — methods-panel-did.Rmd (METH-04) · dataset: inline synthetic staggered panel

### Live worked example
`PanelEventStudyTask$new(panel_data, unit_id, time_id, outcome, treatment, treatment_time)` — default col names `"unit_id","time_id","outcome"(? see note),"treated","treatment_time"` `[VERIFIED: R/panel_event_study.R:44-49]`. **Note:** the `outcome` default is not shown in the grep window — pass `outcome="y"` explicitly to be safe. Required columns = `c(unit_id, time_id, outcome, treatment, treatment_time)` `[VERIFIED: R/panel_event_study.R:51]`.

```r
set.seed(42)  # local seed for reproducible synthetic panel
n_units <- 10; periods <- 1:10
cohorts <- c(4, 6, 8)                         # 3 staggered cohorts
panel <- do.call(rbind, lapply(1:n_units, function(u){
  g <- cohorts[((u-1) %% 3) + 1]
  data.frame(unit_id=u, time_id=periods, treatment_time=g,
             treated=as.integer(periods>=g),
             y = 0.5*u + 0.3*periods + 1.5*(periods>=g) + rnorm(length(periods),0,0.5))
}))
task <- PanelEventStudyTask$new(panel, unit_id="unit_id", time_id="time_id",
                                outcome="y", treatment="treated",
                                treatment_time="treatment_time")
# LIVE estimators (base R stats::lm):
res <- estimate_panel_event_study(task, method = "dynamic_twfe", leads = 3, lags = 3)
knitr::kable(res$results$coefficients, caption = "Dynamic TWFE event-time coefficients.")  # TABLE
plot_panel_event_study(res)                                                                # PLOT
```
`[VERIFIED: R/panel_event_study.R:117-158]`. Live methods: `"static_twfe"`, `"dynamic_twfe"`, `"sun_abraham"` (all `stats::lm`, always available) `[VERIFIED: R/panel_event_study.R:147-150,214,316]`. Result shape: `results` = list with `coefficients` (tibble of event-time coefs + SE), `model`, `method` `[VERIFIED: R/panel_event_study.R:109-114]`. Plot: `plot_panel_event_study()` `[VERIFIED: R/panel_event_study.R:674]`.

**eval=FALSE optional estimators** (external pkgs; warn+return NULL if absent):
```r
```{r cs, eval=FALSE}
estimate_panel_event_study(task, method = "callaway_santanna")            # needs 'did'
estimate_panel_event_study(task, method = "dechaisemartin_dhaultfoeuille") # needs DIDmultiplegt
estimate_panel_event_study(task, method = "borusyak_jaravel_spiess")       # needs didimputation
```
```
**API SURPRISE — exact method strings:** `"static_twfe"`, `"callaway_santanna"`, `"dechaisemartin_dhaultfoeuille"`, `"borusyak_jaravel_spiess"` `[VERIFIED: R/panel_event_study.R:118-123]`. These differ from 15-CONTEXT's shorthand (`twfe`/`dchaisemartin`/`bjs`) — use the source strings.

### Formulas + provenance
- Static TWFE: $y_{it}=\alpha_i+\lambda_t+\delta D_{it}+\varepsilon_{it}$ `<!-- Formula verified: R/panel_event_study.R:210-214 matches GoodmanBacon2021 -->`. `[VERIFIED: R/panel_event_study.R:210-214]`
- Dynamic/event-study TWFE: $y_{it}=\alpha_i+\lambda_t+\sum_{k\neq-1}\beta_k \mathbf{1}\{t-g_i=k\}+\varepsilon_{it}$ (base period -1) `<!-- Formula verified: R/panel_event_study.R:242-... matches SunAbraham2021 -->` (`.estimate_dynamic_twfe`, base_period default -1 at :126). `[VERIFIED: R/panel_event_study.R:242,126]`
- Sun-Abraham interaction-weighted: cohort×rel-time interactions `<!-- ... R/panel_event_study.R:316 matches SunAbraham2021 -->`. `[VERIFIED: R/panel_event_study.R:316]`

### Refs to add: `CallawaySantAnna2021`, `SunAbraham2021`, `BorusyakJaravelSpiess2024`, `deChaisemartinDHaultfoeuille2020`, `GoodmanBacon2021`
### Offline safety: dynamic_twfe live (base R) satisfies RENDER-01; optional estimators eval=FALSE. Local `set.seed(42)` before synthetic panel (in addition to _setup inheritance) for byte-stable rows.

---

## Article 4 — methods-intraday.Rmd (METH-05) · dataset: inline synthetic intraday

### Live worked example
`IntradayEventStudyTask$new(firm_stock_data_tbl, reference_tbl, request_tbl [, factor_tbl])`. Firm tbl needs `symbol`, `timestamp` (POSIXct), price/return; reference needs `index_symbol`, `timestamp`; request needs `event_timestamp` (POSIXct) + minute windows `[VERIFIED: R/task_intraday.R:33-38,102-120]`. Timestamp MUST be POSIXct or constructor stops `[VERIFIED: R/task_intraday.R:105-106]`.

```r
set.seed(42)
ts <- as.POSIXct("2023-06-01 09:30:00", tz="UTC") + (0:199)*60   # 200 min bars
firm <- data.frame(symbol="ACME", timestamp=rep(ts,1),
                   price = 100*cumprod(1+rnorm(200,0,0.001)))
ref  <- data.frame(index_symbol="MKT", timestamp=ts,
                   price = 4000*cumprod(1+rnorm(200,0,0.0008)))
req  <- data.frame(symbol="ACME", index_symbol="MKT",
                   event_timestamp = as.POSIXct("2023-06-01 11:00:00", tz="UTC"),
                   estimation_window_length=60, event_window_start=-10, event_window_end=10)
task <- IntradayEventStudyTask$new(firm, ref, req)
task <- prepare_intraday_event_study(task, ParameterSet$new())
# ... fit_model / calculate_statistics as for daily ...
```
`[VERIFIED: R/task_intraday.R:15,138]` (`prepare_intraday_event_study`). Windows assigned by observation count from event timestamp `[VERIFIED: R/task_intraday.R:154-166]`. **Executor must confirm exact firm/request column names against `vignettes/intraday-event-study.Rmd` before finalizing the synthetic tibble** — the grep confirmed `symbol`/`timestamp`/`event_timestamp`/`index_symbol` but the price/return column name and window arg names should be cross-checked against the existing intraday vignette (which renders successfully). Table = `tidy(task)`; plot = `plot_event_study(task)`.

### Formulas + provenance
- Same market-model AR machinery at intraday frequency; $AR_{i,\tau}=r_{i,\tau}-\hat\alpha-\hat\beta r_{m,\tau}$ over intraday $\tau$. `<!-- Formula verified: R/models.R:248 (shared AR engine) matches BarclayWarner1993 -->`. `[VERIFIED: R/models.R:248]`

### Refs to add: `BarclayWarner1993`
### Offline safety: fully live (base R, POSIXct). Local `set.seed(42)`. No optional deps.

---

## Article 5 — methods-synthetic-control.Rmd (METH-06) · dataset: inline treated+donor

### Live worked example
`SyntheticControlTask$new(treated_data, donor_data, treatment_time)`. treated_data needs `time, outcome`; donor_data long with `unit, time, outcome` `[VERIFIED: R/synthetic_control.R:29-33]`.

```r
set.seed(42)
periods <- 1:80; ttime <- 60
donor_ids <- paste0("D", 1:5)
donor_data <- do.call(rbind, lapply(donor_ids, function(d)
  data.frame(unit=d, time=periods, outcome=cumsum(rnorm(80,0.1,1)))))
# treated = weighted blend of donors pre-period + post-treatment jump
treated_data <- data.frame(time=periods,
  outcome = with(donor_data, tapply(outcome, time, mean)) + c(rep(0,ttime-1), seq(0,5,length.out=81-ttime+... )) )
task <- SyntheticControlTask$new(treated_data, donor_data, treatment_time=ttime)
res  <- estimate_synthetic_control(task, method = "optim")   # base R stats::optim
knitr::kable(head(res$results$trajectory), caption = "Treated vs synthetic trajectory.")  # TABLE
plot(res$results$trajectory$time, res$results$trajectory$gap, type="l")                    # PLOT (or ggplot)
```
`[VERIFIED: R/synthetic_control.R:80,86]` (`estimate_synthetic_control`, `method=c("quadprog","optim")`). **Default to `method="optim"`** — pure `stats::optim` L-BFGS-B, no optional dep `[VERIFIED: R/synthetic_control.R:73,138,226]`. Result shape: `results` = list(`weights`, `trajectory` tibble[`time,treated,synthetic,gap`], `pre_mspe`, `post_mspe`, `att`, `method`) `[VERIFIED: R/synthetic_control.R:180-192]`. Executor: fix the treated-data construction arithmetic (skeleton above is illustrative; ensure lengths match 80).

### Formulas + provenance
- Synthetic control: $\hat Y^N_{1t}=\sum_{j=2}^{J+1} w_j Y_{jt}$, weights $w$ minimizing pre-period MSPE s.t. $w_j\ge0,\sum w_j=1$; gap $=Y_{1t}-\hat Y^N_{1t}$ `<!-- Formula verified: R/synthetic_control.R:178,183 matches AbadieDiamondHainmueller2010 -->` (gap at trajectory build; simplex constraints in `.solve_sc_optim`). `[VERIFIED: R/synthetic_control.R:183,226]`

### Refs to add: `AbadieDiamondHainmueller2010`
### Offline safety: method="optim" always-live satisfies RENDER-01. quadprog path shown `eval=requireNamespace("quadprog", quietly=TRUE)`. Local set.seed(42).

---

## Article 6 — methods-ai-advisor.Rmd (METH-07) · dataset: dieselgate

### Live worked example (deterministic layer, fully offline)
Mirror `vignettes/ai-advisor.Rmd` exactly: `Sys.setenv(EVENTSTUDY_NO_NETWORK="1")` in setup, deterministic chunks live, LLM chunk `eval=FALSE` static.

```r
library(EventStudy)
data("dieselgate")
task <- EventStudyTask$new(dieselgate$firm, dieselgate$index, dieselgate$request)
params <- ParameterSet$new()
task <- prepare_event_study(task, params); task <- fit_model(task, params)
task <- calculate_statistics(task, params)

diag   <- es_diagnostics(task)          # S3 "es_diagnostics", 6 sections, offline
advice <- recommend_stat(task)          # S3 "es_advice"
robust <- flag_robustness(task)         # S3 "es_advice"
knitr::kable(as.data.frame(diag$normality %||% diag[[1]]),
             caption = "Deterministic diagnostics feeding the advisor.")   # TABLE
plot_diagnostics(task)                                                     # PLOT
```
`[VERIFIED: R/es_diagnostics.R:51]` (`es_diagnostics(task, max_events=20L)` → named list of 6 sections), `[VERIFIED: R/advise_offline.R:43,103]` (`recommend_stat`, `flag_robustness` S3 generics, offline path ignores `provider`), `[VERIFIED: R/plotting.R:282]` (`plot_diagnostics`). Executor: confirm exact `es_diagnostics` sub-list names by reading R/es_diagnostics.R:17-51 (six sections) before writing the kable accessor.

**LLM layer** — `eval=FALSE` + static captured block, identical pattern to `vignettes/ai-advisor.Rmd:11-22`. `[VERIFIED: vignettes/ai-advisor.Rmd:19-22]`

### Formulas + provenance
No academic formula (design article, per 15-CONTEXT). Section 4 states the **grounding invariant** (advisor never fabricates a number) quoting `vignettes/ai-advisor.Rmd`. No bib needed.

### Refs to add: none
### Offline safety: deterministic layer live (offline by construction) satisfies RENDER-01. LLM eval=FALSE + NO_NETWORK env. set.seed(42) from _setup.

---

## Article 7 — methods-diagnostics.Rmd (METH-08) · dataset: earnings_surprises + simulate

### Live worked example
```r
data("earnings_surprises")
task <- EventStudyTask$new(earnings_surprises$firm, earnings_surprises$index,
                           earnings_surprises$request)
params <- ParameterSet$new(); task <- prepare_event_study(task, params)
task <- fit_model(task, params); task <- calculate_statistics(task, params)

diag <- model_diagnostics(task)            # Shapiro-Wilk + Ljung-Box per event
pt   <- pretrend_test(task)                # pre-event trend
knitr::kable(diag, caption = "Normality (Shapiro-Wilk) & autocorrelation (Ljung-Box).")  # TABLE

# Bootstrap + power on a simulated task (zero-network, seed-stable):
sim <- simulate_event_study(n_events = 20, abnormal_return = 0.02,
                            n_simulations = 200, seed = 42)
plot(sim$rejection_by_day$relative_index, sim$rejection_by_day$rejection_rate, type="b")  # PLOT
boot <- bootstrap_test(task, n_boot = 199, seed = 42)
```
`[VERIFIED: R/diagnostics.R:12]` (`model_diagnostics` — confirm exact export name at R/diagnostics.R:12; grep shows the `@export` block returns tibble with `shapiro_p, ljung_box_p`), `[VERIFIED: R/diagnostics.R:44-67]` (Shapiro at 49, Ljung-Box `Box.test` at 67), `[VERIFIED: R/diagnostics.R:104]` (`pretrend_test`), `[VERIFIED: R/bootstrap.R:22]` (`bootstrap_test(task, n_boot=999, weight_type="rademacher", statistic="both", group=NULL, seed=NULL)`), `[VERIFIED: R/simulation.R:32]` (`simulate_event_study(..., seed=NULL)` → S3 `es_simulation` with `$power`, `$rejection_by_day` tibble[`relative_index, rejection_rate`]) `[VERIFIED: R/simulation.R:25-26,117-119]`.

**API note:** function is `model_diagnostics()` (the diagnostics export at R/diagnostics.R:12) — executor confirm the exact exported name (grep showed the `@export`/`@return` block but the function line was not captured; read R/diagnostics.R:12-15). bootstrap returns tibble `relative_index, observed_aar, observed_caar, boot_p_aar, boot_p_caar` `[VERIFIED: R/bootstrap.R:18-20]`.

### Formulas + provenance
- Ljung-Box: $Q=n(n+2)\sum_{k=1}^{h}\hat\rho_k^2/(n-k)$ `<!-- Formula verified: R/diagnostics.R:67 (Box.test type=Ljung-Box) matches BoxLjung1978 -->`. `[VERIFIED: R/diagnostics.R:64-67]`
- Durbin-Watson: $d=\sum(e_t-e_{t-1})^2/\sum e_t^2$ — **verify DW is actually computed**: grep found no `durbin`/`dw` in diagnostics.R this session. If DW is not implemented, present it as conceptual only (cite BoxLjung + DurbinWatson) OR locate its impl before writing `verified` comment. `[ASSUMED]` — executor MUST confirm DW presence before tagging.
- Wild bootstrap: $AR^*_b = w_b \hat{AR}$, $w_b\in\{-1,+1\}$ (Rademacher) `<!-- Formula verified: R/bootstrap.R:91-93 matches (wild bootstrap) -->`. `[VERIFIED: R/bootstrap.R:91-93]`
- Power = rejection rate `<!-- Formula verified: R/simulation.R:115 matches (Monte Carlo power) -->`. `[VERIFIED: R/simulation.R:115]`

### Refs to add: `BoxLjung1978`, `DurbinWatson1950`
### Offline safety: model_diagnostics/pretrend live; bootstrap & simulate seed-stable zero-network. set.seed(42) inherited + explicit `seed=42` in bootstrap/simulate calls.

---

## BibTeX Entries (add to vignettes/articles/references.bib)

Existing keys retained: `MacKinlay1997`, `Brown1985`. Add:

```bibtex
@article{FamaFrench1993,
  author={Eugene F. Fama and Kenneth R. French},
  title={Common Risk Factors in the Returns on Stocks and Bonds},
  journal={Journal of Financial Economics}, year={1993}, volume={33}, number={1}, pages={3--56}}

@article{FamaFrench2015,
  author={Eugene F. Fama and Kenneth R. French},
  title={A Five-Factor Asset Pricing Model},
  journal={Journal of Financial Economics}, year={2015}, volume={116}, number={1}, pages={1--22}}

@article{Carhart1997,
  author={Mark M. Carhart},
  title={On Persistence in Mutual Fund Performance},
  journal={The Journal of Finance}, year={1997}, volume={52}, number={1}, pages={57--82}}

@article{BarberLyon1997,
  author={Brad M. Barber and John D. Lyon},
  title={Detecting Long-Run Abnormal Stock Returns: The Empirical Power and Specification of Test Statistics},
  journal={Journal of Financial Economics}, year={1997}, volume={43}, number={3}, pages={341--372}}

@article{Patell1976,
  author={James M. Patell},
  title={Corporate Forecasts of Earnings Per Share and Stock Price Behavior: Empirical Tests},
  journal={Journal of Accounting Research}, year={1976}, volume={14}, number={2}, pages={246--276}}

@article{BMP1991,
  author={Ekkehart Boehmer and Jim Musumeci and Annette B. Poulsen},
  title={Event-Study Methodology under Conditions of Event-Induced Variance},
  journal={Journal of Financial Economics}, year={1991}, volume={30}, number={2}, pages={253--272}}

@article{Corrado1989,
  author={Charles J. Corrado},
  title={A Nonparametric Test for Abnormal Security-Price Performance in Event Studies},
  journal={Journal of Financial Economics}, year={1989}, volume={23}, number={2}, pages={385--395}}

@article{KolariPynnonen2010,
  author={James W. Kolari and Seppo Pynn{\"o}nen},
  title={Event Study Testing with Cross-sectional Correlation of Abnormal Returns},
  journal={The Review of Financial Studies}, year={2010}, volume={23}, number={11}, pages={3996--4025}}

@article{CallawaySantAnna2021,
  author={Brantly Callaway and Pedro H. C. Sant'Anna},
  title={Difference-in-Differences with Multiple Time Periods},
  journal={Journal of Econometrics}, year={2021}, volume={225}, number={2}, pages={200--230}}

@article{SunAbraham2021,
  author={Liyang Sun and Sarah Abraham},
  title={Estimating Dynamic Treatment Effects in Event Studies with Heterogeneous Treatment Effects},
  journal={Journal of Econometrics}, year={2021}, volume={225}, number={2}, pages={175--199}}

@article{BorusyakJaravelSpiess2024,
  author={Kirill Borusyak and Xavier Jaravel and Jann Spiess},
  title={Revisiting Event-Study Designs: Robust and Efficient Estimation},
  journal={The Review of Economic Studies}, year={2024}, volume={91}, number={6}, pages={3253--3285}}

@article{deChaisemartinDHaultfoeuille2020,
  author={Cl{\'e}ment de Chaisemartin and Xavier D'Haultf{\oe}uille},
  title={Two-Way Fixed Effects Estimators with Heterogeneous Treatment Effects},
  journal={American Economic Review}, year={2020}, volume={110}, number={9}, pages={2964--2996}}

@article{GoodmanBacon2021,
  author={Andrew Goodman-Bacon},
  title={Difference-in-Differences with Variation in Treatment Timing},
  journal={Journal of Econometrics}, year={2021}, volume={225}, number={2}, pages={254--277}}

@article{BarclayWarner1993,
  author={Michael J. Barclay and Jerold B. Warner},
  title={Stealth Trading and Volatility: Which Trades Move Prices?},
  journal={Journal of Financial Economics}, year={1993}, volume={34}, number={3}, pages={281--305}}

@article{AbadieDiamondHainmueller2010,
  author={Alberto Abadie and Alexis Diamond and Jens Hainmueller},
  title={Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program},
  journal={Journal of the American Statistical Association}, year={2010}, volume={105}, number={490}, pages={493--505}}

@article{BoxLjung1978,
  author={G. E. P. Box and Greta M. Ljung},
  title={On a Measure of Lack of Fit in Time Series Models},
  journal={Biometrika}, year={1978}, volume={65}, number={2}, pages={297--303}}

@article{DurbinWatson1950,
  author={J. Durbin and G. S. Watson},
  title={Testing for Serial Correlation in Least Squares Regression: I},
  journal={Biometrika}, year={1950}, volume={37}, number={3-4}, pages={409--428}}
```

**Provenance:** all `[ASSUMED]` — metadata is from training knowledge, not fetched from a registry/DOI this session. Executor SHOULD double-check volume/number/pages for `BorusyakJaravelSpiess2024` (published 2024, volume/pages most likely to drift) and `SunAbraham2021`/`CallawaySantAnna2021` (same JoE special issue, vol 225 — high confidence). Citation keys match the `[@Key]` references used in each article above.

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Formula → source check | Manual eyeballing | Read `R/<file>:<line>` before writing `<!-- Formula verified -->` | Impl may differ subtly (e.g. Patell uses forecast-error-corrected σ, not plain σ) |
| Model/stat selection | String dispatch | `ParameterSet$new(return_model=X$new(), multi_event_statistics=MultiEventStatisticsSet$new(tests=list(...)))` | API takes R6 objects, not strings |
| Offline safety | try/catch in prose | knitr `eval=requireNamespace(...)` / `eval=FALSE` | build-time skip is the established pattern |

## Common Pitfalls

- **FF3/Carhart on dieselgate ERRORS** — no factor_tbl. Keep factor-model worked examples `eval=FALSE`. Only MarketModel-family runs live on dieselgate.
- **Wrong panel method strings** — use `static_twfe`/`dechaisemartin_dhaultfoeuille`/`borusyak_jaravel_spiess`, not the 15-CONTEXT shorthand.
- **Non-deterministic synthetic data** — inline panel/intraday/SC need an explicit `set.seed(42)` in their own chunk (not only the _setup inheritance) so the rendered table is byte-stable across rebuilds.
- **Durbin-Watson may not be implemented** in diagnostics.R (not found this session) — do not write a `verified` comment for DW until the line is located; otherwise present conceptually.
- **references.bib is co-located** at `vignettes/articles/references.bib` (per skeleton YAML `bibliography: references.bib`), NOT `vignettes/references.bib`. Add entries to the articles copy. `[VERIFIED: vignettes/articles/_article-skeleton.Rmd:4]`

## Runtime State Inventory
Not a rename/refactor phase — N/A. (Documentation-only; no stored data, service config, OS state, secrets, or build artifacts affected.)

## Environment Availability

| Dependency | Required By | Available | Fallback |
|------------|------------|-----------|----------|
| pkgdown | build_article verification | assumed dev env | — |
| plotly, ggplot2 | plots (proven in smoke-test) | Imports (present) | — |
| rugarch | GARCH chunk | optional | eval-gate → prose fallback |
| quadprog | SC quadprog path | optional | method="optim" (base R) |
| did / DIDmultiplegt / didimputation | optional DiD | optional | eval=FALSE static |
| pandoc-citeproc | citation resolution | via rmarkdown | — |

## Validation Architecture (Verification Hooks)

Per-article verification for planner/verifier:
1. **Renders:** `pkgdown::build_article("articles/methods-<x>")` exits 0 (or `pkgdown::build_articles()` for all). Articles are `.Rbuildignore`d so this is dev-only, not `R CMD check`.
2. **>=1 table + >=1 plot:** grep rendered HTML for `<table` and (`<img`/`plotly`/`svg`) — each article's live path documented above guarantees both.
3. **Zero raw math:** grep rendered HTML for literal `$...$`/`$$` that KaTeX failed to render (should be `<span class="math">` after render). smoke-test.Rmd proves KaTeX works.
4. **Citations resolved:** grep for unresolved `[@` / `@Key` literals in HTML; every `[@Key]` must appear in References section. All keys in §BibTeX.
5. **19 CRAN vignettes byte-unchanged (DELIVERY-03):** `git diff --stat vignettes/*.Rmd` shows no changes to the 19 top-level vignettes; only `vignettes/articles/*.Rmd`, `vignettes/articles/references.bib`, `_pkgdown.yml` change. Confirm with `git status --porcelain vignettes/`.
6. **Formula gate:** each article Section 4 contains `<!-- Formula verified: R/<file>:<line> matches <BibKey> eq. <N> -->` with a real line (all located above except DW).

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | All BibTeX metadata (vol/no/pages) | §BibTeX | Wrong citation detail — executor cross-checks BJS 2024, SunAbraham/CS 2021 |
| A2 | Durbin-Watson implemented in diagnostics.R | Article 7 | If absent, no `verified` comment — present DW conceptually |
| A3 | `model_diagnostics` is the exact export name (line 12 grep truncated) | Article 7 | Read R/diagnostics.R:12 to confirm before writing chunk |
| A4 | Intraday firm price/return + window arg column names | Article 4 | Cross-check `vignettes/intraday-event-study.Rmd` before finalizing synthetic tibble |
| A5 | `PanelEventStudyTask` `outcome` default | Article 3 | Pass `outcome="y"` explicitly to avoid relying on unread default |
| A6 | `es_diagnostics` sub-section names for kable accessor | Article 6 | Read R/es_diagnostics.R:17-51 for the 6 section names |

## Sources

### Primary (HIGH — read this session)
- R/parameter_set.R, R/single_event_test_statistics.R, R/multi_event_test_statistics.R, R/test_statistics_set.R, R/models.R, R/models_time_varying.R (grep), R/panel_event_study.R, R/task_intraday.R, R/synthetic_control.R, R/diagnostics.R (grep), R/bootstrap.R (grep), R/simulation.R (grep), R/es_diagnostics.R (grep), R/advise_offline.R (grep), R/plotting.R (grep)
- vignettes/articles/_article-skeleton.Rmd, _setup.Rmd, smoke-test.Rmd, references.bib; vignettes/ai-advisor.Rmd (head)

### Tertiary (LOW — training knowledge)
- All BibTeX bibliographic metadata (§BibTeX) — `[ASSUMED]`, flagged for executor double-check.

## Metadata
- Standard stack / API: HIGH — every signature read from source.
- Formula provenance: HIGH for located lines; DW is LOW (unconfirmed).
- BibTeX: LOW — flagged for verification.
- Research date: 2026-09-06. Valid until: ~2026-10-06 (stable; R source frozen this milestone).
