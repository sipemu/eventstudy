# Download Factor Data

Downloads Fama-French factor data from the Kenneth French Data Library.
No external package dependencies required (uses base R download + CSV
parsing).

## Usage

``` r
download_factor_data(
  model = c("ff3", "ff5", "mom"),
  frequency = c("daily", "monthly"),
  format_for_task = TRUE
)
```

## Arguments

- model:

  Factor model: `"ff3"` (default), `"ff5"`, `"mom"` (momentum), or
  `"ff3_monthly"`, `"ff5_monthly"`.

- frequency:

  Data frequency: `"daily"` (default) or `"monthly"`.

- format_for_task:

  Logical. If TRUE, formats for EventStudyTask factor_tbl.

## Value

A tibble with factor data.
