---
name: es-capabilities
description: |
  Discover the full EventStudy R package capability surface: list every exported
  function and R6 class, get its call signature and one-line purpose, enumerate the
  available return models, test statistics, and task types, and find the right
  function or model for a task. Trigger this skill whenever a user asks "what can
  EventStudy do?", "which model/test should I use for X?", "what's the signature of
  Y?", or otherwise needs API discovery. Capability discovery only — parameter
  tuning and result diagnostics are handled by the es-advisor skill.
---

# EventStudy Capabilities Skill

This skill answers **"what exists and how do I call it?"** for the EventStudy R
package. It is grounded in the package's `NAMESPACE` (the single source of truth for
exports) — it never invents a function name. For parameter tuning, diagnosing a
fitted result, or grounded recommendations, route to the **es-advisor** skill.

## Skill Boundary

- **In scope:** listing exported symbols, call signatures, one-line purpose, the
  model/statistic/task-type inventory, and method-for-task routing.
- **Out of scope (→ es-advisor):** which test statistic best fits *these*
  diagnostics, how to tune `n_boot`/windows, reading `es_diagnostics()` output.

## Source of Truth — always verify before answering

The package surface changes across versions. Confirm against the installed package,
not memory:

```r
# All exported symbols (functions + R6 generators)
Rscript -e 'library(EventStudy); print(ls("package:EventStudy"))'

# Signature of any exported function
Rscript -e 'library(EventStudy); print(args(run_event_study))'

# R6 class: list public methods/fields
Rscript -e 'library(EventStudy); print(names(MarketModel$new()))'
```

Offline (no attach), read `NAMESPACE` directly:

```bash
grep -E "^export\(" NAMESPACE | sed -E 's/export\((.*)\)/\1/' | sort
```

Online reference: the pkgdown site function index at
<https://sipemu.github.io/eventstudy/reference/index.html>.

## The Pipeline (entry points)

The whole package composes three steps; `run_event_study()` runs all three.

| Function | Signature | Purpose |
|----------|-----------|---------|
| `prepare_event_study` | `(task, parameter_set)` | Returns, windows, factor joins |
| `fit_model` | `(task, parameter_set)` | Estimate return model, compute abnormal returns |
| `calculate_statistics` | `(task, parameter_set)` | Single- + multi-event test statistics |
| `run_event_study` | `(task, parameter_set = ParameterSet$new(), report = FALSE, ...)` | All three, end to end |
| `validate_task` | `(task)` | Structural validation of a task |

**Task constructors** (choose by study design):

| Class | Use for |
|-------|---------|
| `EventStudyTask$new(...)` | Standard single/multi-firm daily event study |
| `IntradayEventStudyTask` / `prepare_intraday_event_study` | POSIXct intraday windows |
| `PanelEventStudyTask` / `estimate_panel_event_study` | Staggered-treatment panel (DiD) |
| `SyntheticControlTask` / `estimate_synthetic_control` | Synthetic control causal inference |

`ParameterSet$new()` composes the model, return calculation, and test-statistic
choices. `SimpleReturn` / `LogReturn` select the return type.

## Capability Domains

Reach for the reference files for the full grounded inventory:

- **`reference/model-map.md`** — all exported return models (name → what it estimates
  → required data → key notes). Use when the user asks "which model for X?".
- **`reference/statistic-map.md`** — all exported test statistics (name → null
  hypothesis → single vs multi-event → when to use). Use for "which test for X?".

Other domains (list via the Source-of-Truth commands above):

- **Diagnostics:** `es_diagnostics`, `model_diagnostics`, `pretrend_test`, `sc_placebo_test`
- **Inference / robustness:** `bootstrap_test`, `adjust_p_values`, `cross_sectional_regression`, `simulate_event_study`
- **Cross-sectional helpers:** `car_by_group`, `car_quantiles`, `plot_car_distribution`
- **Plotting:** `plot_event_study`, `plot_stocks`, `plot_diagnostics`, `plot_panel_event_study`, `plot_synthetic_control`, `theme_eventstudy`, `es_colours`
- **Export / reporting:** `export_results`, `tidy()` (broom), `es_report`, `report_table`, `generate_report`
- **Data download (optional deps):** `download_stock_data`, `download_factor_data`, `download_risk_free_rate`
- **AI advisor surface (→ es-advisor):** `es_advise`, `recommend_stat`, `flag_robustness`, `es_kb`, `provider`

## Method-for-Task Routing (quick answers)

| User goal | Point them at |
|-----------|---------------|
| "Run a basic market-model event study" | `EventStudyTask$new()` → `run_event_study()` (default `ParameterSet`) |
| "Control for size/value/momentum factors" | `FamaFrench3FactorModel` / `FamaFrench5FactorModel` / `Carhart4FactorModel` (+ `download_factor_data`) |
| "Long-horizon buy-and-hold abnormal returns" | `BHARModel` + `BHARTTest` |
| "Volatility/volume reaction, not price" | `VolatilityModel` / `VolumeModel` |
| "Few events, non-normal CARs" | `SignTest` / `GeneralizedSignTest` / `RankTest`, or `bootstrap_test` |
| "Cross-firm correlation / event clustering" | `KolariPynnonenTest`, `CalendarTimePortfolioTest` |
| "Explain CARs with firm characteristics" | `cross_sectional_regression` |
| "How much power do I have?" | `simulate_event_study` |
| "Staggered treatment / modern DiD" | `PanelEventStudyTask` + `estimate_panel_event_study` |

For anything past "which one" into "is it appropriate given my results" — hand off
to **es-advisor**.
