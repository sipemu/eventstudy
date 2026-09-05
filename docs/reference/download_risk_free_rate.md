# Download Risk-Free Rate

Downloads risk-free rate data from the Kenneth French Data Library
(extracted from the Fama-French factor files).

## Usage

``` r
download_risk_free_rate(
  frequency = c("daily", "monthly"),
  source = "french",
  format_for_task = TRUE
)
```

## Arguments

- frequency:

  Data frequency: `"daily"` (default) or `"monthly"`.

- source:

  Data source: `"french"` (default).

- format_for_task:

  Logical. If TRUE, formats for EventStudyTask.

## Value

A tibble with `date` and `risk_free_rate` columns.
