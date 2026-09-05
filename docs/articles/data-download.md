# Downloading Stock and Factor Data

## Introduction

The EventStudy package includes helper functions to download stock price
data and Fama-French factor data, pre-formatted for use with
`EventStudyTask`. This eliminates the need for manual data preparation.

The code in this article is shown for illustration purposes and is not
executed at build time because it requires a live network connection.

## Downloading Stock Data

The
[`download_stock_data()`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)
function downloads adjusted close prices from Yahoo Finance. It requires
either the `tidyquant` or `quantmod` package.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``# Download a single stock`` ``aapl`` ``<-`` `[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)`(``"AAPL"``, from ``=`` ``"2023-01-01"``, to ``=`` ``"2024-12-31"``)`` `[`head`](https://rdrr.io/r/utils/head.html)`(``aapl``)`` ``#> # A tibble: 6 x 3`` ``#> symbol date adjusted`` ``#> <chr> <chr> <dbl>`` ``#> 1 AAPL 02.01.2023 130.`` ``#> 2 AAPL 03.01.2023 126.`` ``#> ...`

### Multiple Stocks

`stocks`` ``<-`` `[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)`(`` `` `[`c`](https://rdrr.io/r/base/c.html)`(``"AAPL"``, ``"MSFT"``, ``"GOOGL"``, ``"AMZN"``)``,`` `` from ``=`` ``"2023-01-01"``,`` `` to ``=`` ``"2024-12-31"`` ``)`

### Format for EventStudyTask

When `format_for_task = TRUE` (default), dates are converted to the
`dd.mm.yyyy` format required by `EventStudyTask`:

`# Formatted for direct use`` ``firm_data`` ``<-`` `[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)`(`` `` `[`c`](https://rdrr.io/r/base/c.html)`(``"AAPL"``, ``"MSFT"``)``,`` `` from ``=`` ``"2022-01-01"``,`` `` format_for_task ``=`` ``TRUE`` ``)`` `` ``# Download index data`` ``index_data`` ``<-`` `[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)`(`` `` ``"^GSPC"``, ``# S&P 500`` `` from ``=`` ``"2022-01-01"``,`` `` format_for_task ``=`` ``TRUE`` ``)`` `` ``# Create the task directly`` ``request`` ``<-`` ``tibble``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1``:``2``,`` `` firm_symbol ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AAPL"``, ``"MSFT"``)``,`` `` index_symbol ``=`` ``"^GSPC"``,`` `` event_date ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"15.06.2023"``, ``"15.06.2023"``)``,`` `` group ``=`` ``"Tech"``,`` `` event_window_start ``=`` ``-``5``,`` `` event_window_end ``=`` ``5``,`` `` shift_estimation_window ``=`` ``-``6``,`` `` estimation_window_length ``=`` ``120`` ``)`` `` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request``)`

### Raw Format

Set `format_for_task = FALSE` to get the original data without date
conversion:

`raw_data`` ``<-`` `[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)`(``"AAPL"``, from ``=`` ``"2023-01-01"``,`` `` format_for_task ``=`` ``FALSE``)`

## Downloading Factor Data

The
[`download_factor_data()`](https://sipemu.github.io/eventstudy/reference/download_factor_data.md)
function downloads Fama-French factor data directly from the Kenneth
French Data Library.

### Fama-French 3-Factor

`ff3`` ``<-`` `[`download_factor_data`](https://sipemu.github.io/eventstudy/reference/download_factor_data.md)`(``model ``=`` ``"ff3"``, frequency ``=`` ``"daily"``)`` `[`head`](https://rdrr.io/r/utils/head.html)`(``ff3``)`` ``#> # A tibble: 6 x 5`` ``#> date market_excess smb hml risk_free_rate`` ``#> <chr> <dbl> <dbl> <dbl> <dbl>`` ``#> 1 03.01.2023 0.0112 0.0023 -0.0045 0.0002`` ``#> ...`

### Fama-French 5-Factor

`ff5`` ``<-`` `[`download_factor_data`](https://sipemu.github.io/eventstudy/reference/download_factor_data.md)`(``model ``=`` ``"ff5"``, frequency ``=`` ``"daily"``)`` ``# Includes: market_excess, smb, hml, rmw, cma, risk_free_rate`

### Monthly Frequency

`ff3_monthly`` ``<-`` `[`download_factor_data`](https://sipemu.github.io/eventstudy/reference/download_factor_data.md)`(``model ``=`` ``"ff3"``, frequency ``=`` ``"monthly"``)`

### Using with Factor Models

`# Download data`` ``firm_data`` ``<-`` `[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"AAPL"``, ``"MSFT"``)``, from ``=`` ``"2022-01-01"``)`` ``index_data`` ``<-`` `[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)`(``"^GSPC"``, from ``=`` ``"2022-01-01"``)`` ``factor_tbl`` ``<-`` `[`download_factor_data`](https://sipemu.github.io/eventstudy/reference/download_factor_data.md)`(``model ``=`` ``"ff3"``, frequency ``=`` ``"daily"``)`` `` ``# Create task with factor data`` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request``,`` `` factor_tbl ``=`` ``factor_tbl``)`` `` ``# Use a factor model`` ``ps`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md)`$``new``(``)`` ``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``ps``)`

## Downloading Risk-Free Rate

The
[`download_risk_free_rate()`](https://sipemu.github.io/eventstudy/reference/download_risk_free_rate.md)
function extracts the risk-free rate from the FF3 data:

`rf`` ``<-`` `[`download_risk_free_rate`](https://sipemu.github.io/eventstudy/reference/download_risk_free_rate.md)`(``frequency ``=`` ``"daily"``)`` `[`head`](https://rdrr.io/r/utils/head.html)`(``rf``)`` ``#> # A tibble: 6 x 2`` ``#> date risk_free_rate`` ``#> <chr> <dbl>`` ``#> 1 03.01.2023 0.0002`` ``#> ...`

## Column Name Mapping

When `format_for_task = TRUE`, factor data columns are automatically
renamed:

| Original | Renamed        |
|----------|----------------|
| Mkt-RF   | market_excess  |
| SMB      | smb            |
| HML      | hml            |
| RMW      | rmw            |
| CMA      | cma            |
| Mom      | mom            |
| RF       | risk_free_rate |
