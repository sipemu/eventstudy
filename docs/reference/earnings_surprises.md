# Earnings Surprise Multi-Firm Event Study Dataset

A small, frozen dataset bundling daily prices for three U.S. large-cap
firms and the S&P 500 benchmark around the first-calendar-quarter 2023
(Jan-Mar) earnings announcements, reported late April / early May 2023,
ready to drive a complete event study pipeline
([`prepare_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)
-\>
[`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md)
-\>
[`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)).

## Usage

``` r
data(earnings_surprises)
```

## Format

A named `list` with four elements:

- firm:

  A tibble of daily prices for all three firms combined, with columns
  `symbol` (one of `"AAPL"`, `"MSFT"`, `"GOOGL"`), `date` (character,
  `"%d.%m.%Y"` format), and `adjusted` (numeric adjusted close). Rows
  for all firms are stacked (813 rows total).

- index:

  A tibble of S&P 500 (`"^GSPC"`) daily prices with the same `symbol` /
  `date` / `adjusted` columns, used as the benchmark for all three
  events.

- request:

  A three-row tibble giving the event-study request specifications, one
  row per firm, with the nine columns expected by
  [`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md):
  `event_id` (1L to 3L), `firm_symbol`, `index_symbol`, `event_date`
  (`"04.05.2023"` for AAPL, `"25.04.2023"` for MSFT and GOOGL), `group`
  (`"Earnings Beat"` for all firms), `event_window_start` (-5),
  `event_window_end` (5), `shift_estimation_window` (-6), and
  `estimation_window_length` (200).

- meta:

  A list of provenance metadata: `firm_tickers`, `index_ticker`,
  `event_dates`, `from`, `to`, `source`, `access_date`, and `note`.

## Source

Yahoo Finance daily adjusted prices, retrieved 2026-09-05 via the
package's own
[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md).
This is a small illustrative sample bundled for academic / demonstration
use only; see `data-raw/earnings_surprises.R` for the reproducible fetch
script.

## Details

All three firms beat consensus EPS estimates for their respective
January-to-March 2023 quarters: Apple Inc. reported on **2023-05-04**
(its fiscal Q2 FY2023, which runs Jan-Mar) and beat by approximately 8%,
driving a strong next-day return; Microsoft Corporation reported on
**2023-04-25** (its fiscal Q3 FY2023, which runs Jan-Mar), beating cloud
(Azure) estimates with a roughly +7% next-day response; Alphabet Inc.
reported on **2023-04-25** (Q1 CY2023, Jan-Mar), with advertising
revenue exceeding expectations. The bundled window layout uses a
200-trading-day estimation window ending 6 days before each event and an
event window of `[-5, +5]` trading days. Running a market model on this
single-group panel produces a positive cumulative average abnormal
return (CAAR) over the event window, reflecting the shared earnings-beat
signal across all three firms.

**Firms:**

- `AAPL` — Apple Inc. (NASDAQ), event_id = 1

- `MSFT` — Microsoft Corporation (NASDAQ), event_id = 2

- `GOOGL` — Alphabet Inc. Class A (NASDAQ), event_id = 3

**Group:** "Earnings Beat" (all firms). **Benchmark:** S&P 500 index
(ticker `^GSPC`). **Date range:** 2022-06-01 to 2023-06-30.

## Examples

``` r
# \donttest{
data(earnings_surprises)

# Build task and run the full pipeline
task <- EventStudyTask$new(earnings_surprises$firm,
                           earnings_surprises$index,
                           earnings_surprises$request)
task <- run_event_study(task, ParameterSet$new())

# Multi-event: AAR/CAAR across all firms
caar_tbl <- task$aar_caar_tbl$CSectT[[1]]
tail(caar_tbl[, c("relative_index", "caar", "caar_t")], 1)
#> # A tibble: 1 × 3
#>   relative_index   caar caar_t
#>            <int>  <dbl>  <dbl>
#> 1              5 0.0377   2.35
# }
```
