# Result Extraction and Export

## Introduction

After running an event study, you need to extract and export results for
analysis, tables, and publication. This vignette covers all the ways to
get results out of an `EventStudyTask`.

## Running the Example

First, let’s set up a complete event study (using synthetic data for
reproducibility):

``` r

library(EventStudy)

# Create task with synthetic data
task <- create_mock_task(n_firms = 5)  # helper for testing
params <- ParameterSet$new()
task <- run_event_study(task, params)
```

## Extracting Results Programmatically

### Abnormal Returns

``` r

# Get AR for a specific event
ar <- task$get_ar(event_id = 1)
ar
# # A tibble: 11 x 2
#    relative_index abnormal_returns
#             <dbl>            <dbl>
#  1             -5         0.00234
#  ...
```

### Cumulative Abnormal Returns

``` r

# Get CAR for a specific event
car <- task$get_car(event_id = 1)
car
# # A tibble: 11 x 3
#    relative_index abnormal_returns      car
#             <dbl>            <dbl>    <dbl>
#  1             -5         0.00234  0.00234
#  ...
```

### Average Abnormal Returns (AAR/CAAR)

``` r

# Get AAR/CAAR from multi-event statistics
aar <- task$get_aar(stat_name = "CSectT")
aar
# Returns the cross-sectional t-test results tibble
```

### Model Statistics

``` r

# Get model fit statistics for a specific event
stats <- task$get_model_stats(event_id = 1)
stats$alpha
stats$beta
stats$sigma
stats$r2
```

## The tidy() Method

The
[`tidy.EventStudyTask()`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)
method returns results in broom-compatible long format, making it easy
to use with standard tidyverse workflows.

### Tidy AR

``` r

tidy_ar <- tidy.EventStudyTask(task, type = "ar")
tidy_ar
# # A tibble: 55 x 7
#    event_id group     firm_symbol term  estimate std.error statistic
#       <int> <chr>     <chr>       <chr>    <dbl>     <dbl>     <dbl>
#  1        1 TestGroup FIRM_A      -5     0.00234   0.0102     0.229
#  ...
```

### Tidy CAR

``` r

tidy_car <- tidy.EventStudyTask(task, type = "car")
# Includes cumulative AR with window labels as terms
```

### Tidy AAR

``` r

tidy_aar <- tidy.EventStudyTask(task, type = "aar")
# Includes AAR, CAAR, and their test statistics with p-values
```

### Tidy Model

``` r

tidy_model <- tidy.EventStudyTask(task, type = "model")
# Returns alpha, beta, sigma, r.squared for each event
```

## Exporting Results

### Export to CSV

``` r

# Export all results (creates separate files: results_ar.csv, results_car.csv, etc.)
export_results(task, "results.csv")

# Export specific results
export_results(task, "model_stats.csv", which = "model")
```

### Export to Excel

``` r

# Requires the openxlsx package
# Each result type gets its own worksheet
export_results(task, "results.xlsx")
```

### Export to LaTeX

``` r

# Produces publication-ready LaTeX tables
# Requires the knitr package
export_results(task, "results.tex", which = c("model", "aar"))
```

## Cross-Sectional Analysis of CARs

After extracting CARs, you can analyze what drives the cross-sectional
variation:

``` r

# Extract CARs for all events
all_cars <- purrr::map2_dfr(
  task$data_tbl$event_id,
  task$data_tbl$firm_symbol,
  function(eid, sym) {
    car <- task$get_car(event_id = eid)
    tibble::tibble(
      event_id = eid,
      firm_symbol = sym,
      car_total = tail(car$car, 1)
    )
  }
)

# Now you can regress CARs on firm characteristics
# (see the cross_sectional_regression() function)
```

## Visualization

``` r

# Single-event plots
plot_event_study(task, type = "ar", event_id = 1)
plot_event_study(task, type = "car", event_id = 1)

# Multi-event plots
plot_event_study(task, type = "aar")
plot_event_study(task, type = "caar")

# Model diagnostics
plot_diagnostics(task, event_id = 1)
```

## Summary

| Method | Returns | Format |
|----|----|----|
| `get_ar()` | AR for one event | tibble |
| `get_car()` | CAR for one event | tibble |
| `get_aar()` | AAR/CAAR for one group | tibble |
| `get_model_stats()` | Model fit stats | list |
| [`tidy.EventStudyTask()`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md) | Broom-compatible results | tibble |
| [`export_results()`](https://sipemu.github.io/eventstudy/reference/export_results.md) | Write to file | CSV/XLSX/LaTeX |
| [`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md) | Visualization | ggplot2 |
