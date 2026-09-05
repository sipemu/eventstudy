# EventStudy 0.62.0

## Documentation site + CI/CD deploy

* A curated pkgdown documentation site is now published at
  <https://sipemu.github.io/eventstudy/>, built and deployed to GitHub Pages
  by CI (r-lib pkgdown workflow) on every push to main and on releases.
* `DESCRIPTION` `URL` and `README` now link to the live docs site.
* `.Rbuildignore` excludes `_pkgdown.yml`, `docs/`, and `pkgdown/` so the
  site scaffolding is kept out of the CRAN source tarball; no source or
  behaviour change.

# EventStudy 0.61.3

## Robustness: graceful degradation when a data source is unreachable

* **`download_stock_data()` now fails clearly when the source returns no data.**
  When `tidyquant::tq_get()` cannot reach the source (network outage, rate limit)
  or the ticker is invalid, it returns a bare logical instead of a data frame.
  Previously this surfaced as a cryptic `no applicable method for 'transmute'`
  error; the function now detects the empty result and stops with an actionable
  message naming the symbols and source. No behavior change on valid downloads.

* **Network download tests skip gracefully on source outages.** The Yahoo Finance
  and Ken French library tests are wrapped so a transient source outage skips the
  test instead of failing CI (`skip_if_offline()` only checks generic
  connectivity, not the specific source). Fixes intermittent red CI on the
  R-devel job when Yahoo was unreachable.

* **Fixed the single-event AR/CAR confidence bands in `plot_event_study()`.** Both
  bands were centered on zero (`+/- z*sigma` for AR, `+/- z*sqrt(k)*sigma` for CAR)
  instead of on the estimate, so an abnormal return or CAR that drifts away from
  zero (e.g. VW's -17% AR / -35% CAR in the advisor vignette) ran far outside its
  own "confidence band". The ribbons are now `AR +/- z*sigma` and
  `CAR +/- z*sqrt(k)*sigma`, correctly enveloping the plotted path.


# EventStudy 0.61.2

## Multi-automaker dieselgate dataset + CI bands + group CAAR plot + mock advisor

* **Extended `dieselgate` dataset** — the bundled dataset now covers **four German
  automakers** (VOW.DE, PAH3.DE, BMW.DE, MBG.DE) plus the DAX benchmark across
  the 2015 Dieselgate emissions scandal, with a two-group request structure:
  "VW Group" (directly implicated firms) vs "Other" (peer automakers). This
  enables multi-group event studies and CAAR comparisons out of the box. The
  four-row `request` tibble carries the group column; `event_id = 1` remains
  VOW.DE for backward compatibility. See `?dieselgate`.

* **CI bands on CAR/CAAR plots** — `plot_event_study(type = "car", event_id = 1L,
  confidence_level = 0.95)` draws a 95% confidence ribbon around the cumulative
  abnormal return path; the group CAAR comparison plot in the advisor vignette
  overlays per-group CI ribbons, making the statistical certainty of the VW Group
  crash visually immediate.

* **Multi-group CAAR comparison** — the advisor vignette now shows a side-by-side
  ggplot2 CAAR path for "VW Group" (CAAR at +10 ≈ -39%, t ≈ -12.6) and "Other"
  (CAAR at +10 ≈ +1%, t ≈ 0.34, not significant), illustrating the idiosyncratic
  nature of the shock: VW Group craters while peer automakers barely move.

* **Mock AI-advisor interpretation block** — the advisor vignette's static
  LLM-output example is updated to reflect the multi-group results, grounded in
  the actual computed diagnostics (beta ~1.09, R-squared ~0.74, Shapiro-Wilk
  p ~0.004, VW Group CAAR t-statistic ~-12.6, near-zero Other CAAR). The block
  is clearly labelled as a captured/illustrative example, not evaluated at build
  time.

# EventStudy 0.61.1

## Correctness fix + advisor vignette plots

* **Return calculation could silently return all zeros/NA when `dplyr` was not
  attached.** `SimpleReturn` / `LogReturn` called a bare `lag()`, but
  `dplyr::lag` was not imported into the package namespace, so `lag` resolved to
  `stats::lag`, which no-ops on a plain numeric vector. Every return then
  computed as `0` (and row 1 as `0` instead of `NA`); the resulting
  zero-variance series tripped the market-model variance guard, so **all**
  abnormal returns and CARs came back `NA`. This only manifested when `dplyr`
  was not on the search path (e.g. a plain `library(EventStudy)` session or the
  advisor vignette build); the test suite and audit ran with `dplyr` attached,
  masking `stats::lag`, and never observed it. `dplyr::lag` is now imported, so
  returns are correct for everyone. Behavior is unchanged for callers who
  already had `dplyr` attached. A regression test locks the namespace
  resolution so `lag` can never silently fall back to `stats::lag` again.
* **Advisor vignette (`ai-advisor.Rmd`) gains AR and CAR plots** — the abnormal
  return across the `[-10, +10]` event window and the cumulative abnormal
  return path, both rendered offline, making the dieselgate crash visible.

# EventStudy 0.61.0

## Advisor Vignette + Bundled Dieselgate Dataset

This release makes the AI advisor's story legible through a real-world worked
example, with no change to existing package behavior or APIs.

* **`dieselgate` dataset** — a small, frozen dataset bundling daily adjusted
  prices for Volkswagen AG (`VOW.DE`) and the DAX benchmark index (`^GDAXI`)
  around the 2015-09-18 EPA emissions-scandal disclosure. Loadable via
  `data(dieselgate)` with no network access; it drives a complete event study
  end-to-end and recovers the roughly -35% cumulative abnormal return crash.
  See `?dieselgate` for the documented schema and provenance, and
  `data-raw/dieselgate.R` for the reproducible fetch script.

* **"The Grounded AI Advisor" vignette** (`vignettes/ai-advisor.Rmd`) — explains
  *why* the advisor exists (the grounding invariant: it never fabricates a
  number) and *how* it works (the two-layer architecture: a deterministic
  offline grounding layer plus an optional LLM interpretation layer policed by a
  runtime guard). It walks through the `dieselgate` example, running the
  pipeline and the deterministic advisor layer (`es_diagnostics()`,
  `recommend_stat()`, `flag_robustness()`) **live with no API key**, and shows
  the `es_advise()` LLM layer as a clearly-labelled captured static block. The
  vignette builds fully offline.


# EventStudy 0.60.0

## Grounded AI Advisor — Feature Set (Phases 5–8)

This release ships a complete AI-advisor layer for event study analysis, built
on a deterministic offline knowledge base and an optional LLM backend.  All
advisor features respect CRAN's no-phone-home policy: network calls are
opt-in and fully guarded.

### Offline Diagnostics + Grounding Knowledge Base (Phase 5)

* **`es_diagnostics()`** — offline pre-flight diagnostic suite that inspects
  estimation-window length, event-window span, event clustering, model
  residual autocorrelation, and return normality, returning a structured
  `Diagnostics` object consumable by the advice layer.

* **`es_kb`** — a curated deterministic knowledge base (`R/advisor_kb.R`)
  mapping diagnostic flags to actionable methodology recommendations, sourced
  from MacKinlay (1997), Boehmer et al. (1991), Patell (1976), and related
  academic literature.  `recommend_stat()` and `flag_robustness()` query the
  KB without any network call.

### LLM-Agnostic Provider Abstraction (Phase 6)

* **`provider()`** — constructs a provider configuration for the LLM backend
  of `es_advise()`.  Supports Anthropic (`provider("anthropic")`),
  OpenAI-compatible endpoints (`provider("openai_compatible", base_url = ...)`),
  and custom call-back providers.  Uses `httr2` + `jsonlite` (both in
  `Suggests`) guarded by `requireNamespace()` — never loads unless the user
  explicitly calls `es_advise()` with a provider.

### Grounded `es_advise()` + Runtime Grounding Guard (Phase 7)

* **`es_advise()`** — LLM-backed advice function that assembles grounding
  context from `es_diagnostics()` output and the KB, sends a grounded prompt
  to the configured provider, and returns a structured `Advice` object.  A
  runtime grounding guard validates that the LLM response cites at least one
  KB rule; ungrounded responses are downgraded to `"speculative"` confidence
  and a warning is emitted — the package never silently returns LLM hallucinations
  as authoritative advice.

* **`generate_report(advice = )`** — the report pipeline now accepts an
  optional `Advice` object; when supplied, a dedicated "AI Advisor" section
  is rendered in the output report with evidence chains and confidence labels.

### Claude Code Agent Skill (Phase 8)

* **`.claude/skills/es-advisor/SKILL.md`** — a Claude Code Agent Skill that
  surfaces `es_diagnostics()` + `es_advise()` to Claude Code users directly
  from their AI assistant.  The skill file is excluded from the CRAN tarball
  via `.Rbuildignore`.

### Advisor Pro Waitlist (BIZ-01)

* **Advisor Pro** is a planned retrieval-augmented premium tier backed by a
  curated academic knowledge base.  It is not yet available.  To join the
  waitlist, visit:
  <https://github.com/sipemu/eventstudy#advisor-pro-waitlist>

  An optional opt-in footer can be enabled for `Advice` print methods:
  ```r
  options(eventstudy.advisor_pro_footer = TRUE)
  ```
  The footer is silent by default and opens no network connections.  See
  `?advisor_pro` for details.

# EventStudy 0.50.0

## Robustness Hardening — Regression Catalog

This release hardens the package against degenerate, boundary, and adversarial
inputs (Phase 1–3). Every fix below is locked by a named test that will catch
a regression. The cross-component degenerate-input regression net is provided
by `tests/testthat/test_contract_matrix.R` (TEST-02, see final entry).

### Contract Foundation (Phase 1 — CONTRACT-01..05)

* **CONTRACT-01/02: Degenerate-input contract defined in `R/contract.R`**
  (`.handle_degenerate()`, `.resolve_degenerate_mode()`; ParameterSet
  `degenerate_handling` field; package option `EventStudy.degenerate_handling`).
  — Locked by `tests/testthat/test_contract.R` ::
  `test_that("CONTRACT-02: ParameterSet default resolves to lenient")` and
  `test_that("CONTRACT-02: ParameterSet strict field resolves to strict")`.

* **CONTRACT-03: Strict mode raises descriptive `stop()` naming event_id,
  firm_symbol, and component on degenerate input (MarketModel).** —
  Locked by `tests/testthat/test_contract.R` ::
  `test_that("CONTRACT-03: strict mode errors on insufficient obs — names event_id, firm_symbol, MarketModel")`.

* **CONTRACT-04: Lenient mode sets `is_fitted = FALSE`, emits exactly one
  warning per `(event_id, firm_symbol)`, propagates NA through abnormal_returns,
  and the pipeline emits exactly one warning per degenerate event.** —
  Locked by `tests/testthat/test_contract.R` ::
  `test_that("CONTRACT-04: lenient mode on insufficient obs sets is_fitted FALSE + exactly 1 warning")`
  and `test_that("CONTRACT-04 (pipeline): run_event_study emits exactly ONE warning per degenerate (event_id, firm_symbol)")`.

* **CONTRACT-05: Valid-input MarketModel statistics are byte-stable (baseline
  fixture).** —
  Locked by `tests/testthat/test_contract.R` ::
  `test_that("CONTRACT-05: valid-input MarketModel statistics match committed baseline within 1e-8")`.

### Model Migrations (Phase 1–2 — per-model contract enforcement)

* **MarketModel migrated onto `.handle_degenerate()`** (insufficient obs /
  zero-variance index guards). —
  Locked by `tests/testthat/test_contract.R` ::
  `test_that("CONTRACT-03: strict mode errors on insufficient obs — names event_id, firm_symbol, MarketModel")`.

* **MarketAdjustedModel, ComparisonPeriodMeanAdjustedModel, CustomModel,
  BHARModel, VolumeModel, VolatilityModel migrated** (each has insufficient-obs
  and/or zero-variance guard). —
  Locked by `tests/testthat/test_models.R` ::
  `test_that("VolatilityModel: insufficient obs raises contract error in strict mode")`
  and `test_that("CustomModel: degenerate input — abnormal_returns returns NA without calling predict(NULL)")`.

* **LinearFactorModel, FamaFrench3FactorModel, FamaFrench5FactorModel,
  Carhart4FactorModel migrated** (insufficient obs / zero-variance guards for
  excess_return / factor columns). —
  Locked by `tests/testthat/test_models.R` ::
  `test_that("All four factor models: fit+abnormal_returns emits exactly one warning on degenerate input")`.

* **Per-model CONTRACT-05 baseline fixtures** (MarketAdjusted, CompPeriodMean,
  Custom, BHARModel, VolumeModel, VolatilityModel, FF3, FF5, Carhart4). —
  Locked by `tests/testthat/test_models.R` ::
  `test_that("MarketAdjustedModel: valid-input baseline invariance (CONTRACT-05)")`,
  `test_that("FamaFrench3FactorModel: valid-input baseline invariance (CONTRACT-05)")`,
  etc.

* **RollingWindowModel migrated** (Guard 1: insufficient obs; Guard 2: all-NA
  estimation window; Guard 3: last-window NA params; df reflects finite pair
  count, not nrow — WR-04). —
  Locked by `tests/testthat/test_models_time_varying.R` ::
  `test_that("RollingWindowModel: lenient mode — insufficient obs (Guard 1) — one warning, all-NA ARs")`,
  `test_that("WR-04: RollingWindowModel df reflects finite pair count not nrow (NA-heavy window)")`.

* **GARCHModel migrated** (insufficient obs, zero-variance guards; pre-call guards
  added so `rugarch::ugarchfit()` never receives fewer rows than GARCH order). —
  Locked by `tests/testthat/test_models_time_varying.R` ::
  `test_that("GARCHModel: lenient mode — insufficient obs — one warning, all-NA ARs")`,
  `test_that("GARCHModel: strict mode — insufficient obs — named error")`.

* **DCCGARCHModel migrated** (insufficient obs, zero-variance guards). —
  Locked by `tests/testthat/test_models_time_varying.R` ::
  `test_that("DCCGARCHModel: lenient mode — insufficient obs — one warning, all-NA ARs")`.

* **GARCHModel / DCCGARCHModel `calculate_statistics()` failure wrapping**
  (EXTERNAL-04: `tryCatch` resets `is_fitted = FALSE`, emits named warning,
  returns NA ARs when rugarch fitting fails). —
  Locked by `tests/testthat/test_models_time_varying.R` ::
  `test_that("GARCHModel: calculate_statistics failure resets is_fitted=FALSE + named warning + NA ARs")`,
  `test_that("DCCGARCHModel: calculate_statistics failure → exactly ONE warning (fit+abnormal_returns combined)")`.

### Test-Statistic Guards (Phase 2 — STATS-01..04)

* **STATS-01: `ARTTest`, `CARTTest`, `BHARTTest` return NA (not Inf/NaN) when
  `sigma == 0`** (divide-by-zero guard). —
  Locked by `tests/testthat/test_ar_car_test_statistics.R` ::
  `test_that("STATS-01: ARTTest returns NA (not Inf/NaN) when sigma == 0")`,
  `test_that("STATS-01: CARTTest returns NA (not Inf/NaN) in car_t when sigma == 0")`,
  `test_that("STATS-01: BHARTTest returns NA (not Inf/NaN) in bhar_t when sigma == 0")`.

* **STATS-02: `BMPTest`, `KolariPynnonenTest`, `PatellZTest`, `CSectTTest` join
  on `event_id` (not `firm_symbol`) — fixes GH #7 many-to-many inflation.** —
  Locked by `tests/testthat/test_multi_event_statistics.R` ::
  `test_that("BMPTest does not inflate n_events when firms recur (GH #7)")`,
  `test_that("KolariPynnonenTest does not inflate n_events when firms recur (GH #7)")`,
  `test_that("STATS-02: CSectTTest does not inflate n_events when firms recur")`.

* **STATS-04: `PatellZTest`, `SignTest` return NA when `n_events == 1`** (added
  `n_events <= 1` guards; `BMPTest`, `CSectTTest` already returned NA via
  `sd(single_value) = NA`). —
  Locked by `tests/testthat/test_multi_event_statistics.R` ::
  `test_that("STATS-04: PatellZTest aar_z is NA (not finite) when n_events == 1")`,
  `test_that("STATS-04: SignTest sign_z is NA (not finite) when n_events == 1")`.

### Pipeline Hardening (Phase 3 — PIPELINE-01..03)

* **PIPELINE-01: `prepare_event_study()` honours degenerate mode on missing
  event date** — strict errors naming component/event/firm; lenient warns and
  degrades to all-zero windows. —
  Locked by `tests/testthat/test_prepare.R` ::
  `test_that("missing event date strict: stop() names component, event_id, firm_symbol")`,
  `test_that("missing event date lenient: exactly one warning naming event_id and firm_symbol")`.

* **PIPELINE-02: `export_results()` NA-safe CAR cumsum** (reverted false
  coalesce — NA must propagate through cumsum, not be silently replaced with 0;
  CR-01). —
  Locked by `tests/testthat/test_export.R` ::
  `test_that("export_results: CAR — interior NA propagates through cumsum (correct NA behavior)")`,
  `test_that("export_results: NA abnormal_returns in AR column preserved as NA (not 0)")`.

* **PIPELINE-03: `cross_sectional_regression()` singular/collinear guard** —
  returns NA `std.error/statistic/p.value` with one warning instead of crashing;
  `sandwich` absent degrades to OLS SEs with a warning. —
  Locked by `tests/testthat/test_cross_sectional.R` ::
  `test_that("singular design: collinear regressor returns NA std.error/statistic/p.value + one warning")`,
  `test_that("sandwich absent: emits warning() naming robust SEs and OLS fallback")`.

### External-Package Wrapping (Phase 3 — EXTERNAL-01..04)

* **EXTERNAL-01..03: Panel DiD estimators (`callaway_santanna`,
  `dechaisemartin_dhaultfoeuille`, `borusyak_jaravel_spiess`) return `NULL`
  results with a warning when the required package is absent or the estimator
  fails.** —
  Locked by `tests/testthat/test_panel.R` ::
  `test_that("callaway_santanna warns and returns task with NULL results when did is absent")`.

* **EXTERNAL-04: `estimate_synthetic_control()` empty-donor-pool guard** —
  returns `NULL` with a warning instead of crashing when `quadprog::solve.QP`
  or the donor pool is empty. —
  Locked by `tests/testthat/test_synthetic_control.R` ::
  `test_that("estimate_synthetic_control with quadprog produces valid weights")`.

### Review Fix Round (Phase 1–3 — CR/WR/IN)

* **WR-01: `.degenerate_handled = TRUE` set in `calculate_statistics()`
  `tryCatch` path** — ensures GARCH/DCC failure path also honours one-warning
  guarantee (set in the catch block, not lost). —
  Locked by `tests/testthat/test_models_time_varying.R` ::
  `test_that("GARCHModel: calculate_statistics failure → exactly ONE warning (fit+abnormal_returns combined)")`.

* **WR-02: External-package wrapper emits warning when estimator returns NULL
  results** (was silently producing a NULL task). —
  Locked by `tests/testthat/test_panel.R` ::
  `test_that("callaway_santanna warns and returns task with NULL results when did is absent")`.

* **WR-03: ParameterSet stores normalised `match.arg` value** — field now holds
  `"strict"` or `"lenient"` (not raw user input). —
  Locked by `tests/testthat/test_contract.R` ::
  `test_that("CONTRACT-02: ParameterSet strict field resolves to strict")`.

* **IN-01: Removed redundant `degenerate_mode` / `event_id` / `firm_symbol`
  field re-declarations in `MarketModel`** — fields are defined once on
  `ModelBase`. —
  Locked by `tests/testthat/test_contract.R` (model construction and field
  assignment exercise these fields).

* **IN-02: Standardised package option key to `EventStudy.degenerate_handling`**
  (was inconsistently named in early drafts). —
  Locked by `tests/testthat/test_contract.R` ::
  `test_that("CONTRACT-02: package option overrides default when field is NULL")`.

* **IN-03: `withr` declared in `DESCRIPTION Suggests`** (was missing; tests
  using `withr::with_options` required it). —
  Verified by `R CMD check` and exercised by all `withr::with_options` calls
  in `tests/testthat/test_contract.R`.

* **CR-01: Reverted export `CAR` coalesce** — see PIPELINE-02 above.

* **CR-02: Empty-donor-pool `solve.QP` guard** — see EXTERNAL-04 above.

### Regression Net (Phase 4 — TEST-01/02)

* **TEST-02: `test_contract_matrix.R` — cross-component degenerate-input
  regression net.** Table-driven registry of 25 rows (14 models + 10
  contract-covered test statistics + 1 pipeline path) iterated in strict and
  lenient modes. Locked by `tests/testthat/test_contract_matrix.R` ::
  `test_that("contract-matrix registry is exhaustive")` (asserts `length == 25L`,
  sub-counts 14/10/1, exclusions reconcile to 12 stat classes) and all
  per-component `test_that("contract-matrix [strict] <label> ...")` /
  `test_that("contract-matrix [lenient] <label> ...")` rows.

---

# EventStudy 0.40.0

## Bug Fixes

* `BMPTest`, `KolariPynnonenTest`, and `PatellZTest` joined the per-event
  model (sigma / forecast-error-corrected sigma) on `firm_symbol` instead of
  `event_id`. When a firm appeared in more than one event this produced a
  many-to-many join that duplicated rows, inflating `n_events` and the test
  statistics (`bmp_t`, `kp_t`, `aar_z`). The joins and per-event groupings now
  key on `event_id` (GH #7).

## New Features

* 13 return models: Market Model, Market Adjusted, Comparison Period Mean
  Adjusted, Custom Model, Fama-French 3-Factor, Fama-French 5-Factor,
  Carhart 4-Factor, GARCH, BHAR, Volume, and Volatility models.
* 11 test statistics: AR T-Test, CAR T-Test, BHAR T-Test, Cross-Sectional
  T-Test, Patell Z-Test, Sign Test, Generalized Sign Test, Rank Test,
  BMP Test, and Calendar-Time Portfolio Test.
* Cross-sectional regression of CARs on firm characteristics with
  heteroskedasticity-consistent standard errors.
* Intraday event study support with POSIXct timestamps.
* Panel (DiD) event study module with TWFE and Sun-Abraham estimators,
  including cluster-robust standard errors.
* Export results to CSV, Excel, and LaTeX formats.
* Tidy method for converting results to long-format tibbles.
* Diagnostic tools: model residual plots, estimation window checks.
* 10 vignettes covering introduction, custom models, custom test statistics,
  result extraction, panel event studies, cross-sectional analysis, factor
  models/BHAR, diagnostics, volume/volatility studies, and intraday studies.
