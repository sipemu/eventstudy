# Test Statistic Map (EventStudy v0.66.0)

Every statistic is an R6 generator exported from the package; instantiate with
`TestName$new(...)` and register it on a `ParameterSet` (single- vs multi-event
sets). This list mirrors `NAMESPACE` at v0.66.0 — **verify against the installed
package** with
`Rscript -e 'library(EventStudy); grep("Test$", ls("package:EventStudy"), value=TRUE)'`.

All statistics inherit `compute(data_tbl, model)` and return a tibble of results.

## Single-event statistics

Assess one event (or one firm) at a time.

| Statistic | Null hypothesis | Notes |
|-----------|-----------------|-------|
| `ARTTest` | Abnormal return on a given event day = 0 | Per-day AR t-test |
| `CARTTest` | Cumulative abnormal return over the window = 0 | Uses forecast-error-corrected variance |

## Multi-event (AAR / CAAR) statistics

Aggregate across events; these are the workhorses for a portfolio of events.

| Statistic | Family | Robust to | Use when |
|-----------|--------|-----------|----------|
| `CSectTTest` | Parametric (cross-sectional t) | event-induced variance | Default multi-event test |
| `PatellZTest` | Standardized-residual (Patell Z) | differing estimation-window variance | Homoskedastic event window |
| `BMPTest` | Standardized cross-sectional (Boehmer-Musumeci-Poulsen) | event-induced variance increase | Standard robust choice |
| `KolariPynnonenTest` | Adjusted-BMP | cross-sectional correlation of ARs | Overlapping/clustered event dates |
| `CalendarTimePortfolioTest` | Calendar-time portfolio | cross-sectional correlation, long horizon | Heavily clustered events |
| `SignTest` | Nonparametric (binomial sign) | non-normal CARs | Simple robustness cross-check |
| `GeneralizedSignTest` | Nonparametric (generalized sign) | skew via estimation-window sign ratio | Better than plain sign test |
| `RankTest` | Nonparametric (Corrado rank) | non-normality, outliers | Fat-tailed returns |
| `BHARTTest` | Long-horizon | — | Pair with `BHARModel` |

## Intraday

| Function | Purpose |
|----------|---------|
| `nonparametric_intraday_test` | Nonparametric test for intraday event studies |

## Statistic set containers

| Class | Role |
|-------|------|
| `SingleEventStatisticsSet` | Holds the single-event tests to run |
| `MultiEventStatisticsSet` | Holds the multi-event tests to run |
| `StatisticsSetBase` | Abstract base; `add_test()` to configure dynamically |

## Picking a statistic

- **Default:** `CSectTTest` (parametric) + `BMPTest` (robust to event-induced
  variance) reported together.
- **Non-normal / small samples:** add a nonparametric cross-check —
  `GeneralizedSignTest` or `RankTest` — or use `bootstrap_test`.
- **Clustered / overlapping event dates:** `KolariPynnonenTest` or
  `CalendarTimePortfolioTest` (both handle cross-sectional correlation).
- **Long-horizon returns:** `BHARTTest` with `BHARModel`.

Whether a statistic is *appropriate given your fitted diagnostics* (normality,
autocorrelation, event clustering actually present) is an **es-advisor** decision —
`recommend_stat()` / `flag_robustness()` there ground the choice in
`es_diagnostics()` output.
