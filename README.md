# Event Study Analysis in R

**Package is under active development! The API may change**

## Installation

#### CRAN

Not deployed yet.

#### GITHUB

    install.packages("devtools")
    devtools::install_github("sipemu/eventstudy")

## Features

- Apply common models and perform diagnostic tests on them.
- Modular event study calculations in R:
  - Apply your own market model (incl. external factors).
  - Apply your own test statistics (AR, AAR, CAR, and CAAR).
  - Extract results for further analysis as, e.g, [Cross-Sectional regression analysis](https://eventstudy.de/features/cross_sectional_regression.html)
- Extract confidence bands on each level and for each CAR and CAAR window.
- Parallel execution for large Event Studies.

## Example: Dieselgate

### Initialisation

```{r}
library(tidyquant)
library(dplyr)
library(purrr)
library(readr)
library(DT)

library(EventStudy)

index_symbol = c("^GDAXI")
firm_symbols = c("VOW.DE", "PAH3.DE", "BMW.DE", "MBG.DE")

group <- c(rep("VW Group", 2), rep("Other", 2))
request_tbl <- cbind(c(1:4), firm_symbols, rep(index_symbol, 4), 
                     rep("18.09.2015", 4), 
                     group, rep(-10, 4), rep(10, 4), rep(-11, 4), rep(250, 4)) %>% 
  as_tibble()

names(request_tbl) <- c("event_id", "firm_symbol", "index_symbol", "event_date", 
                        "group", "event_window_start", "event_window_end", 
                        "shift_estimation_window", "estimation_window_length")

firm_symbols %>%
  tidyquant::tq_get(from = "2014-06-01", to = "2015-11-01") %>%
  dplyr::mutate(date = format(date, "%d.%m.%Y")) %>%
  dplyr::select(symbol, date, adjusted) -> firm_tbl

index_symbol %>%
  tidyquant::tq_get(from = "2014-06-01", to = "2015-11-01") %>%
  dplyr::mutate(date = format(date, "%d.%m.%Y")) %>%
  dplyr::select(symbol, date, adjusted) -> index_tbl
```

### Define the Event Study

```{r}
# Parametrization of the Event Study
log_return = LogReturn$new()
market_model = MarketModel$new()
```

```{r}
# Define single event test statistics
# Per default AR and CAR T-Tests are applied
single_event_tests = SingleEventStatisticsSet$new()
```

```{r}
# Setup parameter set
# Setup parameter set
param_set = ParameterSet$new(return_calculation      = log_return, 
                             return_model            = market_model,
                             single_event_statistics = single_event_tests,
                             multi_event_statistics  = NULL)
```

### Execute the Event Study

```{r}
est_task = EventStudyTask$new(firm_tbl, index_tbl, request_tbl)
```

```{r}
est_task = prepare_event_study(est_task, param_set)
```

```{r}
est_task = fit_model(est_task, param_set)
```

```{r}
est_task = calculate_single_event_statistics(est_task, param_set)
```

### Extract Results

```{r}
library(ggplot2)
library(ggdist)

# Plot CAR statistic for the first event ID
est_task$data_tbl$CART[[1]] %>% 
  ggplot(aes(x=relative_index, y=car)) +
  stat_lineribbon(
    aes(ydist = car_t_dist),
    alpha = 1/2
  ) +
  scale_fill_brewer(palette = "Set2")
```

## Roadmap

1. Package documentation and validation.
2. Add Basic AAR and CAAR test statistic.
3. Create vignettes for 
  - performing an Event Study with this package.
  - add custom models 
  - add custom test statistics
  - extract results according to different research needs.
4. Diagnostics and resilient code.
5. Tests, tests, tests, ...
6. CRAN readiness
7. More test statistics.
8. Long term Event Study.
9. Volatility and volume Event Study with test statistics.
10. Intraday Event Study
