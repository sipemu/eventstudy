# Getting Started with EventStudy

## Introduction

An event study measures the impact of an event – a merger, earnings
announcement, or macroeconomic shock – on the value of a firm.
`EventStudy` offers a modular pipeline for conducting these analyses in
R: apply common return models, run diagnostic and test statistics, and
extract publication-ready results, all in a workflow that composes
cleanly with the tidyverse.

This vignette walks through the core flow: define a task, run the study,
inspect the results, and extract or plot them. It uses the Dieselgate
scandal as a running example.

## Quick Start

If you already have firm data, index data, and a request table (all
defined below), the entire pipeline runs in a single call:

``` r

library(EventStudy)

# One-call shortcut: prepare -> fit -> calculate, in one step
est_task <- run_event_study(
  EventStudyTask$new(firm_tbl, index_tbl, request_tbl),
  ParameterSet$new(
    return_calculation      = LogReturn$new(),
    return_model            = MarketModel$new(),
    single_event_statistics = SingleEventStatisticsSet$new(),
    multi_event_statistics  = MultiEventStatisticsSet$new()
  )
)

est_task$aar_caar_tbl
```

The rest of this vignette unpacks the same pipeline step by step, so you
can see – and customize – each stage.

## Example: Dieselgate

To demonstrate the functionality and ease of use of EventStudy, let’s
look at an example: an event study analysis of the Dieselgate scandal.
This example demonstrates how to use the EventStudy package by
conducting an event study analysis on the “Dieselgate” scandal. The
scandal, which erupted in 2015, involved Volkswagen’s admission that it
had installed software on its diesel cars to cheat on emissions tests.
This significant event had substantial effects on the stock prices of
Volkswagen and other automotive companies.

The following code will guide you through the steps of performing an
event study analysis, from the initial setup to the extraction of
results.

### Initialization

The first step is to load necessary packages and data. This includes
market data for the companies of interest and the index during the event
study period.

``` r

#' warnings: false
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

Both, the firm data as the index data should have the following
structure:

1.  **symbol**: Contains the symbol of the stock.
2.  **date**: The date of the price information.
3.  **adjusted**: The price of the stock at given date. The price column
    can be parametrized according to your needs when the task is
    defined. The default is `adjusted`.

``` r

DT::datatable(firm_tbl)
```

### Define the Event Study

Next, we define the event study. This involves specifying the return
calculation method, the market model, and the test statistics to be
used. In this example, we use logarithmic returns, the market model, and
the default AR and CAR T-Tests. Descriptions of the available models can
be found on our website: [Expected Return
Models](https://eventstudy.de/models/expected_return.html "Choosing the Right Approach: A Comprehensive Guide to Estimating Expected Returns.").

``` r

# Parametrization of the Event Study
log_return = LogReturn$new()
market_model = MarketModel$new()
```

For single events the standard [AR and CAR T
test](https://eventstudy.de/statistics/ar_car_statistics.html "A Comprehensive Guide to Analyzing Abnormal Returns in Event Studies.")
are applied. For performing an Event Study on multiple events, the
Cross-Sectional T Test (AAR and CAAR) is applied by default, alongside
Patell Z, BMP, sign, and rank tests.

``` r

# Define single event test statistics
# Per default AR and CAR T-Tests are applied
single_event_tests = SingleEventStatisticsSet$new()

# Per default CSEct T Test is applied (AAR & CAAR)
multiple_event_tests = MultiEventStatisticsSet$new()
```

Your Event Study is then defined in a parameter set:

``` r

# Setup parameter set
param_set = ParameterSet$new(return_calculation      = log_return, 
                             return_model            = market_model,
                             single_event_statistics = single_event_tests,
                             multi_event_statistics  = multiple_event_tests)
```

### Execute the Event Study

Now, with everything set up, we can execute the event study. This
involves preparing the event study, fitting the model, and calculating
the statistics. In the first step the task is defined. The firm and the
index data gathered before as also the request data is necessary for
performing the study.

``` r

est_task = EventStudyTask$new(firm_tbl, index_tbl, request_tbl)
```

Internally, firm and index data symbol and price column names are
renamed. Afterwards a join is applied and the data is collected in a
data frame with one event per row. Let’s have a look at the internal
data frame:

``` r

DT::datatable(est_task$data_tbl)
```

``` r

DT::datatable(est_task$data_tbl$data[[1]])
```

``` r

DT::datatable(est_task$data_tbl$request[[1]])
```

The internal data structure is important for you if you plan to develop
your own statistical or econometric model or test statistic.

``` r

est_task = prepare_event_study(est_task, param_set)
```

``` r

est_task$data_tbl$data[[1]]
```

``` r

est_task = fit_model(est_task, param_set)
```

``` r

est_task$data_tbl
```

``` r

est_task$data_tbl$model[[1]]
```

``` r

est_task = calculate_statistics(est_task, param_set)
```

``` r

est_task$data_tbl
```

``` r

est_task$data_tbl$ART[[1]]
```

``` r

est_task$data_tbl$CART[[1]]
```

``` r

est_task$aar_caar_tbl
```

``` r

est_task$aar_caar_tbl$CSectT[[1]]
```

### Next Steps

This vignette covered the core market-model pipeline. To go deeper:

- **Return models:** See
  [`vignette("factor-models-bhar")`](https://sipemu.github.io/eventstudy/articles/factor-models-bhar.md)
  for multi-factor and long-horizon models
- **Test statistics:** See
  [`vignette("inference-robustness")`](https://sipemu.github.io/eventstudy/articles/inference-robustness.md)
  for robust inference and bootstrap
- **Result extraction:** See
  [`vignette("result-extraction")`](https://sipemu.github.io/eventstudy/articles/result-extraction.md)
  for export, `tidy()`, and cross-sectional analysis
- **AI advisor:** See
  [`vignette("ai-advisor")`](https://sipemu.github.io/eventstudy/articles/ai-advisor.md)
  for deterministic diagnostics and LLM interpretation
- **Full gallery:** See
  [`vignette("gallery")`](https://sipemu.github.io/eventstudy/articles/gallery.md)
  for all available vignettes by topic

## Conclusion

Event Study offers a streamlined and intuitive interface for conducting
event study analysis in R, making it a valuable addition to the toolkit
of any researcher or analyst in finance. Whether you’re assessing the
impact of corporate events on stock prices or investigating the effects
of macroeconomic news, Event Study provides a flexible and efficient
solution.
