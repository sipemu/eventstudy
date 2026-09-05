# Pre-trend Test for Event Study

Performs a joint F-test of pre-event abnormal returns to assess whether
there are statistically significant pre-event effects, which would
indicate potential model misspecification or confounding.

## Usage

``` r
pretrend_test(task, group = NULL)
```

## Arguments

- task:

  A fitted EventStudyTask with abnormal returns computed.

- group:

  Optional group to filter by.

## Value

A tibble with pre-trend test results for each group.
