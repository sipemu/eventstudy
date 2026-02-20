# Event Study Analysis in R

**Package is under active development! The API may change.**

## Installation

#### CRAN

Not deployed yet.

#### GitHub

```r
install.packages("devtools")
devtools::install_github("sipemu/eventstudy")
```

## Features

The `EventStudy` package implements the classical financial event study methodology (MacKinlay 1997) with a modular, extensible architecture built on R6 classes.

- **Multiple Return Models**: Market Model, Market Adjusted Model, Comparison Period Mean Adjusted Model, and a Custom Model base class for your own specifications.

- **Parametric Test Statistics**: AR and CAR t-tests (single event), Cross-Sectional T test and Patell Z test (multiple events), and BMP test (Boehmer, Musumeci, Poulsen 1991) for event-induced variance robustness.

- **Non-Parametric Test Statistics**: Sign Test, Generalized Sign Test (Cowan 1992), and Rank Test (Corrado 1989) — robust to non-normality of abnormal returns.

- **Diagnostics**: Model diagnostics (Shapiro-Wilk normality, Durbin-Watson, Ljung-Box autocorrelation, ACF) and pre-trend testing for pre-event abnormal return detection.

- **Visualization**: Event study plots (AR, CAR, AAR, CAAR) with confidence bands, plus diagnostic plots (residuals, Q-Q, histogram, ACF).

- **Extensible Design**: Add your own models by inheriting from `ModelBase` and your own test statistics by inheriting from `TestStatisticBase`.

- **Result Extraction**: Convenient methods for extracting AR, CAR, AAR/CAAR, and model statistics from the task object.

## Quick Start

The fastest way to run an event study:

```r
library(EventStudy)

# Create task from your data
task = EventStudyTask$new(firm_tbl, index_tbl, request_tbl)

# Run with default settings (Market Model, simple returns, all test statistics)
task = run_event_study(task)

# Inspect results
print(task)
summary(task)
```

## Example: Dieselgate

This example demonstrates the `EventStudy` package by analyzing the "Dieselgate" scandal — Volkswagen's 2015 admission that it had installed software to cheat on diesel emissions tests. This event substantially affected stock prices across the automotive industry.

### 1. Load Data

```r
library(tidyquant)
library(dplyr)
library(EventStudy)

index_symbol = c("^GDAXI")
firm_symbols = c("VOW.DE", "PAH3.DE", "BMW.DE", "MBG.DE")

group <- c(rep("VW Group", 2), rep("Other", 2))
request_tbl <- tibble(
  event_id = 1:4,
  firm_symbol = firm_symbols,
  index_symbol = rep(index_symbol, 4),
  event_date = rep("18.09.2015", 4),
  group = group,
  event_window_start = rep(-10, 4),
  event_window_end = rep(10, 4),
  shift_estimation_window = rep(-11, 4),
  estimation_window_length = rep(250, 4)
)

firm_tbl <- firm_symbols %>%
  tidyquant::tq_get(from = "2014-06-01", to = "2015-11-01") %>%
  dplyr::mutate(date = format(date, "%d.%m.%Y")) %>%
  dplyr::select(symbol, date, adjusted)

index_tbl <- index_symbol %>%
  tidyquant::tq_get(from = "2014-06-01", to = "2015-11-01") %>%
  dplyr::mutate(date = format(date, "%d.%m.%Y")) %>%
  dplyr::select(symbol, date, adjusted)
```

### 2. Create Task and Run

**Option A — One-liner with defaults:**

```r
task = EventStudyTask$new(firm_tbl, index_tbl, request_tbl)
task = run_event_study(task)
```

**Option B — Step-by-step with custom parameters:**

```r
task = EventStudyTask$new(firm_tbl, index_tbl, request_tbl)

# Configure parameters
est_params = ParameterSet$new(
  return_calculation = LogReturn$new(),
  return_model = MarketModel$new(),
  single_event_statistics = SingleEventStatisticsSet$new(),
  multi_event_statistics = MultiEventStatisticsSet$new()
)

# Execute pipeline
task = prepare_event_study(task, est_params)
task = fit_model(task, est_params)
task = calculate_statistics(task, est_params)
```

**Option C — Pipe-friendly:**

```r
task = EventStudyTask$new(firm_tbl, index_tbl, request_tbl)
est_params = ParameterSet$new(return_calculation = LogReturn$new())

task |>
  prepare_event_study(est_params) |>
  fit_model(est_params) |>
  calculate_statistics(est_params)
```

### 3. Inspect Results

```r
# Overview
print(task)
summary(task)

# Extract single-event results
task$get_ar(event_id = 1)    # Abnormal returns
task$get_car(event_id = 1)   # Cumulative abnormal returns
task$get_model_stats(event_id = 1)  # Model fit statistics

# Extract multi-event results
task$get_aar(stat_name = "CSectT")  # AAR/CAAR table
```

### 4. Visualize

```r
# Event study plots with confidence bands
plot_event_study(task, type = "car", event_id = 1)
plot_event_study(task, type = "caar", group = "VW Group")

# Model diagnostic plots
plot_diagnostics(task, event_id = 1)

# Raw price plots
plot_stocks(task, add_event_date = TRUE)
```

### 5. Diagnostics and Validation

```r
# Validate the task before running
validate_task(task)

# Model diagnostics after fitting
model_diagnostics(task)

# Pre-trend test
pretrend_test(task)
```

## Available Models

| Model | Class | Description |
|-------|-------|-------------|
| Market Model | `MarketModel` | OLS regression: $R_i = \alpha + \beta R_m + \epsilon$ |
| Market Adjusted | `MarketAdjustedModel` | $AR_i = R_i - R_m$ (no estimation needed) |
| Comparison Period Mean | `ComparisonPeriodMeanAdjustedModel` | $AR_i = R_i - \bar{R}_i^{est}$ |
| Custom Model | `CustomModel` | Extend `MarketModel` with custom AR calculation |

## Available Test Statistics

### Single Event (per-firm AR/CAR tests)

| Test | Class | Statistic |
|------|-------|-----------|
| AR T-Test | `ARTTest` | $t = AR_{i,t} / \sigma_i$ |
| CAR T-Test | `CARTTest` | $t = CAR_i / (\sqrt{L} \cdot \sigma_i)$ |

### Multiple Events (cross-sectional AAR/CAAR tests)

| Test | Class | Description |
|------|-------|-------------|
| Cross-Sectional T | `CSectTTest` | Standard cross-sectional t-test |
| Patell Z | `PatellZTest` | Standardized residual test (Patell 1976) |
| BMP Test | `BMPTest` | Standardized cross-sectional test (Boehmer et al. 1991) |
| Sign Test | `SignTest` | Proportion of positive ARs vs 50% |
| Generalized Sign Test | `GeneralizedSignTest` | Sign test adjusted for asymmetry (Cowan 1992) |
| Rank Test | `RankTest` | Non-parametric rank test (Corrado 1989) |

## Adding Custom Test Statistics

```r
# Create a custom multi-event test by inheriting from TestStatisticBase
MyCustomTest <- R6::R6Class("MyCustomTest",
  inherit = TestStatisticBase,
  public = list(
    name = "MyTest",
    compute = function(data_tbl, model) {
      # Your test logic here
      # Must return a tibble with at least relative_index and your statistic
    }
  )
)

# Add to a statistics set
multi_tests = MultiEventStatisticsSet$new()
multi_tests$add_test(MyCustomTest$new())
```

## Roadmap

### Completed
- [x] Multiple return models (Market, Market Adjusted, Mean Adjusted)
- [x] Parametric test statistics (AR T, CAR T, CSect T, Patell Z)
- [x] Non-parametric test statistics (Sign, Generalized Sign, Rank, BMP)
- [x] Model diagnostics and pre-trend testing
- [x] Event study visualization (AR, CAR, AAR, CAAR plots)
- [x] Validation of task data
- [x] Comprehensive test suite

### Planned
1. Export results to Excel/LaTeX
2. CRAN submission
3. Vignettes for custom models, test statistics, and result extraction
4. Fama-French factor models (3-factor, 5-factor, Carhart 4-factor)
5. GARCH model integration
6. Long-term event study (BHAR)
7. Volatility and volume event study with test statistics
8. Intraday event study
9. Panel event study module (DiD-style, per Miller 2023)
10. Cross-sectional regression analysis
