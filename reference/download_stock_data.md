# Download Stock Data

Downloads historical stock price data for specified symbols. Uses
tidyquant if available, otherwise falls back to quantmod.

## Usage

``` r
download_stock_data(
  symbols,
  from,
  to = Sys.Date(),
  source = "yahoo",
  format_for_task = TRUE
)
```

## Arguments

- symbols:

  Character vector of stock ticker symbols.

- from:

  Start date (Date or character in YYYY-MM-DD format).

- to:

  End date. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- source:

  Data source: `"yahoo"` (default).

- format_for_task:

  Logical. If TRUE, formats output for `EventStudyTask`: columns
  `symbol`, `date` (dd.mm.yyyy), `adjusted`.

## Value

A tibble with stock data.
