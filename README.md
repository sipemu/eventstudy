# Event Study Analysis in R

[![CRAN status](https://www.r-pkg.org/badges/version/EventStudy)](https://CRAN.R-project.org/package=EventStudy)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/EventStudy)](https://CRAN.R-project.org/package=EventStudy)
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/EventStudy)](https://CRAN.R-project.org/package=EventStudy)
[![License: AGPL-3](https://img.shields.io/badge/License-AGPL--3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![R ≥ 4.1.0](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3.svg?logo=r)](https://cran.r-project.org/)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/sipemu/eventstudy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sipemu/eventstudy/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/sipemu/eventstudy/graph/badge.svg)](https://codecov.io/gh/sipemu/eventstudy)

A comprehensive, modular R package for financial event study analysis. Implements the classical methodology (MacKinlay 1997) and extends it with modern multi-factor models, long-horizon methods, panel (DiD) event studies, and more.

## Installation

#### GitHub

```r
install.packages("devtools")
devtools::install_github("sipemu/eventstudy")
```

## Features

- **13 Return Models**: Market Model, Market Adjusted, Mean Adjusted, Fama-French 3- and 5-factor, Carhart 4-factor, GARCH(1,1), Buy-and-Hold Abnormal Returns (BHAR), Volume, and Volatility models.

- **11 Test Statistics**: Parametric (AR T, CAR T, BHAR T, Cross-Sectional T, Patell Z, BMP) and non-parametric (Sign, Generalized Sign, Rank, Calendar-Time Portfolio).

- **Diagnostics**: Shapiro-Wilk normality, Durbin-Watson, Ljung-Box autocorrelation, pre-trend testing.

- **Visualization**: Event study plots (AR, CAR, AAR, CAAR) with confidence bands, diagnostic plots, CAR distribution histograms, panel event study plots.

- **Result Export**: CSV, Excel (multi-sheet), and LaTeX (publication-ready tables). Broom-compatible `tidy()` method for tidyverse workflows.

- **Cross-Sectional Analysis**: Regress CARs on firm characteristics with HC-robust standard errors. Group comparisons, quantiles, and distribution plots.

- **Intraday Event Studies**: POSIXct timestamp support with minute/second-level windows.

- **Panel Event Studies (DiD)**: Two-way fixed effects (static and dynamic), Sun & Abraham (2021) interaction-weighted estimator for staggered treatment.

- **Extensible Design**: Add custom models (inherit `ModelBase`) and test statistics (inherit `TestStatisticBase`).

## Quick Start

```r
library(EventStudy)

# Create task from your data
task <- EventStudyTask$new(firm_tbl, index_tbl, request_tbl)

# Run with default settings (Market Model, simple returns, all test statistics)
task <- run_event_study(task)

# Inspect results
print(task)
summary(task)
```

## Example: Dieselgate

Analyzing the 2015 Volkswagen emissions scandal and its impact on automotive stock prices.

### 1. Load Data

```r
library(tidyquant)
library(dplyr)
library(EventStudy)

index_symbol <- "^GDAXI"
firm_symbols <- c("VOW.DE", "PAH3.DE", "BMW.DE", "MBG.DE")

request_tbl <- tibble(
  event_id = 1:4,
  firm_symbol = firm_symbols,
  index_symbol = rep(index_symbol, 4),
  event_date = rep("18.09.2015", 4),
  group = c(rep("VW Group", 2), rep("Other", 2)),
  event_window_start = rep(-10, 4),
  event_window_end = rep(10, 4),
  shift_estimation_window = rep(-11, 4),
  estimation_window_length = rep(250, 4)
)

firm_tbl <- firm_symbols %>%
  tq_get(from = "2014-06-01", to = "2015-11-01") %>%
  mutate(date = format(date, "%d.%m.%Y")) %>%
  select(symbol, date, adjusted)

index_tbl <- index_symbol %>%
  tq_get(from = "2014-06-01", to = "2015-11-01") %>%
  mutate(date = format(date, "%d.%m.%Y")) %>%
  select(symbol, date, adjusted)
```

### 2. Run the Event Study

**One-liner with defaults:**

```r
task <- EventStudyTask$new(firm_tbl, index_tbl, request_tbl)
task <- run_event_study(task)
```

**Step-by-step with custom parameters:**

```r
task <- EventStudyTask$new(firm_tbl, index_tbl, request_tbl)

est_params <- ParameterSet$new(
  return_calculation = LogReturn$new(),
  return_model = MarketModel$new(),
  single_event_statistics = SingleEventStatisticsSet$new(),
  multi_event_statistics = MultiEventStatisticsSet$new()
)

task <- prepare_event_study(task, est_params)
task <- fit_model(task, est_params)
task <- calculate_statistics(task, est_params)
```

**Pipe-friendly:**

```r
task <- EventStudyTask$new(firm_tbl, index_tbl, request_tbl)
est_params <- ParameterSet$new(return_calculation = LogReturn$new())

task |>
  prepare_event_study(est_params) |>
  fit_model(est_params) |>
  calculate_statistics(est_params)
```

### 3. Extract Results

```r
# Overview
print(task)
summary(task)

# Single-event results
task$get_ar(event_id = 1)           # Abnormal returns
task$get_car(event_id = 1)          # Cumulative abnormal returns
task$get_model_stats(event_id = 1)  # Model fit statistics

# Multi-event results
task$get_aar(stat_name = "CSectT")  # AAR/CAAR table

# Broom-compatible tidy output
tidy.EventStudyTask(task, type = "ar")
tidy.EventStudyTask(task, type = "car")
tidy.EventStudyTask(task, type = "aar")
tidy.EventStudyTask(task, type = "model")
```

### 4. Export Results

```r
# CSV (one file per result type)
export_results(task, "results.csv")

# Excel (each result type on a separate sheet)
export_results(task, "results.xlsx")

# LaTeX (publication-ready tables)
export_results(task, "results.tex", which = c("model", "aar"))
```

### 5. Visualize

```r
# Event study plots with confidence bands
plot_event_study(task, type = "car", event_id = 1)
plot_event_study(task, type = "caar", group = "VW Group")

# Model diagnostics
plot_diagnostics(task, event_id = 1)

# CAR distribution across events
plot_car_distribution(task, by_group = TRUE)

# Raw price plots
plot_stocks(task, add_event_date = TRUE)
```

### 6. Diagnostics and Validation

```r
validate_task(task)
model_diagnostics(task)
pretrend_test(task)
```

## Available Models

| Model | Class | Description |
|-------|-------|-------------|
| Market Model | `MarketModel` | OLS: $R_i = \alpha + \beta R_m + \epsilon$ |
| Market Adjusted | `MarketAdjustedModel` | $AR_i = R_i - R_m$ |
| Mean Adjusted | `ComparisonPeriodMeanAdjustedModel` | $AR_i = R_i - \bar{R}_i^{est}$ |
| Fama-French 3-Factor | `FamaFrench3FactorModel` | $R_i - R_f = \alpha + \beta_m(R_m - R_f) + \beta_s SMB + \beta_h HML + \epsilon$ |
| Fama-French 5-Factor | `FamaFrench5FactorModel` | 3-factor + RMW + CMA |
| Carhart 4-Factor | `Carhart4FactorModel` | 3-factor + MOM |
| GARCH(1,1) | `GARCHModel` | Time-varying volatility (requires `rugarch`) |
| BHAR | `BHARModel` | Buy-and-hold: $\prod(1+R_i) - \prod(1+R_m)$ |
| Volume | `VolumeModel` | Abnormal trading volume |
| Volatility | `VolatilityModel` | Abnormal volatility (variance ratio) |
| Custom | `CustomModel` | Extend `MarketModel` with custom AR logic |
| Linear Factor | `LinearFactorModel` | Base class for all OLS factor models |

### Using Factor Models

Factor models require a `factor_tbl` with Fama-French data:

```r
factor_data <- tibble(
  date = dates,             # matching format as firm/index data
  smb = smb_returns,
  hml = hml_returns,
  risk_free_rate = rf_rate  # triggers automatic excess return computation
)

task <- EventStudyTask$new(firm_tbl, index_tbl, request_tbl,
                            factor_tbl = factor_data)
params <- ParameterSet$new(return_model = FamaFrench3FactorModel$new())
task <- run_event_study(task, params)
```

## Available Test Statistics

### Single Event (per-firm AR/CAR)

| Test | Class | Description |
|------|-------|-------------|
| AR T-Test | `ARTTest` | $t = AR_{i,t} / \sigma_i$ |
| CAR T-Test | `CARTTest` | $t = CAR_i / (\sqrt{L} \cdot \sigma_i)$ |
| BHAR T-Test | `BHARTTest` | t-test on buy-and-hold abnormal returns |

### Multiple Events (cross-sectional AAR/CAAR)

| Test | Class | Description |
|------|-------|-------------|
| Cross-Sectional T | `CSectTTest` | Standard cross-sectional t-test |
| Patell Z | `PatellZTest` | Standardized residual test (Patell 1976) |
| BMP Test | `BMPTest` | Standardized cross-sectional (Boehmer et al. 1991) |
| Sign Test | `SignTest` | Proportion of positive ARs vs 50% |
| Generalized Sign | `GeneralizedSignTest` | Adjusted for asymmetry (Cowan 1992) |
| Rank Test | `RankTest` | Non-parametric rank test (Corrado 1989) |
| Calendar-Time Portfolio | `CalendarTimePortfolioTest` | Portfolio approach for clustered events |

## Cross-Sectional Regression

Explain why some firms are affected more than others:

```r
firm_chars <- tibble(
  event_id = 1:4,
  log_market_cap = c(10, 11, 12, 10.5),
  leverage = c(0.3, 0.5, 0.2, 0.4)
)

result <- cross_sectional_regression(
  task, formula = ~ log_market_cap + leverage,
  data = firm_chars
)
print(result)

# Group comparisons
car_by_group(task)
car_quantiles(task)
```

## Intraday Event Studies

For high-frequency studies around earnings announcements, Fed decisions, etc.:

```r
task <- IntradayEventStudyTask$new(
  firm_intraday_tbl,    # symbol, timestamp (POSIXct), price
  index_intraday_tbl,
  request_tbl           # event_timestamp, windows in observation counts
)
task <- prepare_intraday_event_study(task, ParameterSet$new())
task <- fit_model(task, ParameterSet$new())
```

**Non-parametric significance test** (Rinaudo & Saha 2014): compares event-day CARs to the empirical distribution of estimation-day CARs to build confidence bands without distributional assumptions.

```r
results <- nonparametric_intraday_test(
  estimation_window = est_data,   # day, time, abnormalReturn
  event_window      = event_data, # time, abnormalReturn
  event_times       = c("10:00", "14:30"),
  p = 0.05
)
```

See `vignette("intraday-event-study")` for details.

## Panel Event Studies (DiD)

For difference-in-differences style analysis with staggered treatment:

```r
task <- PanelEventStudyTask$new(
  panel_data,
  unit_id = "firm_id",
  time_id = "year",
  outcome = "revenue",
  treatment = "policy_adopted",
  treatment_time = "adoption_year"
)

# Dynamic TWFE with event study plot
task <- estimate_panel_event_study(task, method = "dynamic_twfe",
                                    leads = 5, lags = 5)
plot_panel_event_study(task)

# Sun & Abraham for staggered treatment
task <- estimate_panel_event_study(task, method = "sun_abraham",
                                    leads = 5, lags = 5)
```

## Extending the Package

### Custom Models

```r
MyModel <- R6::R6Class("MyModel",
  inherit = ModelBase,
  public = list(
    model_name = "MyModel",
    fit = function(data_tbl) {
      # Estimate on estimation window, set private$.is_fitted = TRUE
      # Populate private$.statistics$sigma and $degree_of_freedom
    },
    abnormal_returns = function(data_tbl) {
      # Return data_tbl with an abnormal_returns column
    }
  )
)
```

### Custom Test Statistics

```r
MyTest <- R6::R6Class("MyTest",
  inherit = TestStatisticBase,
  public = list(
    name = "MyTest",
    compute = function(data_tbl, model) {
      # Return a tibble with your test results
    }
  )
)

multi_tests <- MultiEventStatisticsSet$new()
multi_tests$add_test(MyTest$new())
```

See `vignette("custom-models")` and `vignette("custom-test-statistics")` for detailed tutorials.

## Vignettes

| Vignette | Topic |
|----------|-------|
| `introduction` | Package overview with Dieselgate example |
| `custom-models` | Creating and using custom return models |
| `custom-test-statistics` | Creating custom test statistics |
| `result-extraction` | Extracting and exporting results |
| `factor-models-bhar` | Factor models (FF3, FF5, Carhart), GARCH, and BHAR |
| `cross-sectional-analysis` | Cross-sectional regression, group comparisons, CAR quantiles |
| `diagnostics-validation` | Data validation, model diagnostics, pre-trend testing |
| `volume-volatility-event-study` | Volume and volatility event studies |
| `intraday-event-study` | Non-parametric intraday event studies |
| `panel-event-study` | Panel DiD event studies |

## Roadmap

- [x] Return models (Market, Market Adjusted, Mean Adjusted)
- [x] Parametric test statistics (AR T, CAR T, CSect T, Patell Z, BMP)
- [x] Non-parametric test statistics (Sign, Generalized Sign, Rank)
- [x] Diagnostics and pre-trend testing
- [x] Event study visualization (AR, CAR, AAR, CAAR)
- [x] Task validation
- [x] Export to CSV, Excel, LaTeX
- [x] Broom-compatible tidy() method
- [x] Fama-French 3-factor, 5-factor, Carhart 4-factor models
- [x] GARCH(1,1) model
- [x] Buy-and-hold abnormal returns (BHAR)
- [x] Calendar-time portfolio test
- [x] Volume and volatility event studies
- [x] Cross-sectional regression analysis
- [x] Intraday event study support
- [x] Panel event study module (static/dynamic TWFE, Sun & Abraham 2021)
- [x] Vignettes for custom models, test statistics, result extraction, panel
- [ ] CRAN submission

## References

- MacKinlay, A. C. (1997). Event Studies in Economics and Finance. *Journal of Economic Literature*, 35(1), 13-39.
- Fama, E. F. & French, K. R. (1993). Common risk factors in the returns on stocks and bonds. *Journal of Financial Economics*, 33(1), 3-56.
- Fama, E. F. & French, K. R. (2015). A five-factor asset pricing model. *Journal of Financial Economics*, 116(1), 1-22.
- Carhart, M. M. (1997). On persistence in mutual fund performance. *Journal of Finance*, 52(1), 57-82.
- Boehmer, E., Musumeci, J. & Poulsen, A. B. (1991). Event-study methodology under conditions of event-induced variance. *Journal of Financial Economics*, 30(2), 253-272.
- Corrado, C. J. (1989). A nonparametric test for abnormal security-price performance in event studies. *Journal of Financial Economics*, 23(2), 385-395.
- Cowan, A. R. (1992). Nonparametric event study tests. *Review of Quantitative Finance and Accounting*, 2(4), 343-358.
- Patell, J. M. (1976). Corporate forecasts of earnings per share and stock price behavior. *Journal of Accounting Research*, 14(2), 246-276.
- Rinaudo, J. B. & Saha, A. (2014). Non-parametric intraday event studies.
- Sun, L. & Abraham, S. (2021). Estimating dynamic treatment effects in event studies with heterogeneous treatment effects. *Journal of Econometrics*, 225(2), 175-199.

## License

AGPL-3
