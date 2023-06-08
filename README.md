# Event Study Analysis in R

**Code is still experimental**

## Status

-   Simple and log return calculation.

-   Perform single event Event Study using standard models, e.g. market model, market adjusted model, and comparison period mean adjusted model (more to come (e)garch and custom model template).

-   Calculate simple ar and car test statistics on single events.

-   Parallel execution for a large scale Event Study.

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
index_symbol = c("SPY")
firm_symbols = c("VOW.DE", "PAH3.DE", "BMW.DE", "MBG.DE")
firm_symbols = c("DRMA")

group <- c(rep("VW Group", 2), rep("Other", 2))
request_tbl <- cbind(c(1:4), firm_symbols, rep(index_symbol, 4), 
                     rep("18.09.2015", 4), 
                     group, rep(-10, 4), rep(10, 4), rep(-11, 4), rep(250, 4)) |>
  as_tibble()

group <- c(rep("Other", 1))
request_tbl <- cbind(c(1), firm_symbols, rep(index_symbol, 1), 
                     rep("14.04.2023", 1), 
                     group, rep(-5, 1), rep(5, 1), rep(-11, 1), rep(250, 1)) |>
  as_tibble()

names(request_tbl) <- c("event_id", "firm_symbol", "index_symbol", "event_date", 
                        "group", "event_window_start", "event_window_end", 
                        "shift_estimation_window", "estimation_window_length")

firm_symbols |>
  tidyquant::tq_get(from = "2022-01-01", to = today()) |>
  dplyr::mutate(date = format(date, "%d.%m.%Y")) |>
  dplyr::select(symbol, date, adjusted) -> firm_tbl

index_symbol |>
  tidyquant::tq_get(from = "2022-01-01", to = today()) |>
  dplyr::mutate(date = format(date, "%d.%m.%Y")) |>
  dplyr::select(symbol, date, adjusted) -> index_tbl
```

### Define the Event Study

```{r}
# Parametrization of the Event Study
log_return = LogReturn$new()
market_model = MarketModel$new()

# Define single event test statistic
ar_test = ARTTest$new()
car_test = CARTTest$new()

# Setup parameter set
param_set = ParameterSet$new(return_calculation  = log_return, 
                             return_model        = market_model,
                             ar_test_statistics  = ar_test,
                             car_test_statistics = car_test)
```

### Execute the Event Study

```{r}
est_task = EventStudyTask$new(firm_tbl, index_tbl, request_tbl)
est_task = prepare_event_study(est_task, param_set)
est_task = execute_model(est_task, param_set)
est_task = execute_single_event_statistics(est_task, param_set)
```

### Extract Results

```{r}
est_task$firm_tbl$ar_statistics
est_task$firm_tbl$car_statistics

est_task$firm_tbl$car_statistics[[1]] %>% 
  ggplot(aes(x=relative_index, y=car)) +
  geom_line()
```

## Roadmap

-   Add documentation to methods.

-   Diagnostics and recilent code.

-   Tests, tests, tests, ...

-   Create vignettes.

-   AAR and CAAR test statistics and calculations.

-   Volatility and volume Event Study with test statistics.

-   Intraday Event Study
