# Event Study Analysis in R

**Package is under active development! The API may change**

## Installation

#### CRAN

Not deployed yet.

#### GITHUB

    install.packages("devtools")
    devtools::install_github("sipemu/eventstudy")




## Features
The `EventStudy` package includes several features that make it a versatile tool for performing event study analyses:

- **Flexible Models and Diagnostic Tests**: You can apply common models and perform diagnostic tests on them. The package is designed to be modular and adaptable, which means you can easily extend it with your own models and tests.

- **Custom Models**: With EventStudy, you have the ability to apply your own market model. This means you can include external factors in your model that are specific to your study or industry.

- **Custom Test Statistics**: You can apply your own test statistics for abnormal returns (AR), average abnormal returns (AAR), cumulative abnormal returns (CAR), and cumulative average abnormal returns (CAAR). This gives you full control over how you want to measure the impact of the event.

- **Result Extraction**: You can extract the results of the event study for further analysis. For example, you might want to perform [Cross-Sectional regression analysis](https://eventstudy.de/features/cross_sectional_regression.html). The package provides convenient functions for extracting confidence bands at each level and for each CAR and CAAR window.

- **Parallel Execution**: If you are dealing with a large number of events, the package supports parallel execution. This allows you to take full advantage of your computer's processing power to speed up the calculations.


## Implementation Status

### Test Statistics

Single Firm:

- T test for abnormal returns: $H_0: E[AR_{i, t}] = 0$
- T test for cumulative abnormal return: $H_0: E[CAR_{i, t}] = 0$

Multiple Firms:

- Cross-Sectional T Test $H_0: E[AAR_{t}] = 0$ and $H_0: E[CAAR] = 0$
- Patell Z Test $H_0: E[AAR_{t}] = 0$ and $H_0: E[CAAR] = 0$


## Example: Dieselgate

This example demonstrates how to use the `EventStudy` package by conducting an event study analysis on the "Dieselgate" scandal. The scandal, which erupted in 2015, involved Volkswagen's admission that it had installed software on its diesel cars to cheat on emissions tests. This significant event had substantial effects on the stock prices of Volkswagen and other automotive companies. 

The following code will guide you through the steps of performing an event study analysis, from the initial setup to the extraction of results. 

### Initialisation

The first step is to load necessary packages and data. This includes market data for the companies of interest and the index during the event study period. 


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

Next, we define the event study. This involves specifying the return calculation method, the market model, and the test statistics to be used. In this example, we use logarithmic returns, the market model, and the default AR and CAR T-Tests.


```{r}
# Parametrization of the Event Study
log_return = LogReturn$new()
market_model = MarketModel$new()
```

```{r}
# Define single event test statistics
# Per default AR and CAR T-Tests are applied
single_event_tests = SingleEventStatisticsSet$new()
multiple_event_tests = MultiEventStatisticsSet$new()
```

```{r}
# Setup parameter set
param_set = ParameterSet$new(return_calculation      = log_return, 
                             return_model            = market_model,
                             single_event_statistics = single_event_tests,
                             multi_event_statistics  = multiple_event_tests)
```

### Execute the Event Study

Now, with everything set up, we can execute the event study. This involves preparing the event study, fitting the model, and calculating the statistics. 


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
est_task = calculate_statistics(est_task, param_set)
```

### Extract Results

Finally, after the event study has been executed, we can extract and visualize the results. In this case, we plot the cumulative abnormal return (CAR) for the first event ID. The shaded area represents the confidence interval around the CAR.


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

This example demonstrates the basic usage of the `EventStudy` package. However, the package is designed to be flexible and can be customized to suit different use cases. Refer to the package documentation for more detailed information on how to use the various features of the `EventStudy` package.


## Roadmap

1. Create vignettes for performing an Event Study with this package.
2. Export results to Excel.
3. Validation of statistics and models.
4. CRAN readiness
5. Diagnostics and resilient code.
6. Tests, tests, tests, ...
7. Create vignettes for 
  - add custom models 
  - add custom test statistics
  - extract results according to different research needs.
8. More test statistics.
9. Long term Event Study.
10. Volatility and volume Event Study with test statistics.
11. Intraday Event Study
