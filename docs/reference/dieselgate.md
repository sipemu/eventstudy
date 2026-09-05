# Volkswagen "Dieselgate" Multi-Automaker Event Study Dataset

A small, frozen dataset bundling daily prices for four German automakers
and the DAX benchmark index around the 2015 "dieselgate" emissions
scandal, ready to drive a complete multi-group event study
([`prepare_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)
-\>
[`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md)
-\>
[`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)).

## Usage

``` r
data(dieselgate)
```

## Format

A named `list` with four elements:

- firm:

  A tibble of daily prices for all four automakers combined, with
  columns `symbol` (one of `"VOW.DE"`, `"PAH3.DE"`, `"BMW.DE"`,
  `"MBG.DE"`), `date` (character, `"%d.%m.%Y"` format), and `adjusted`
  (numeric adjusted close). Rows for all firms are stacked (1 440 rows
  total).

- index:

  A tibble of DAX (`"^GDAXI"`) daily prices with the same `symbol` /
  `date` / `adjusted` columns, used as the benchmark / reference market
  for all four events.

- request:

  A four-row tibble giving the event-study request specifications, one
  row per firm, with the nine columns expected by
  [`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md):
  `event_id` (1L to 4L), `firm_symbol`, `index_symbol`, `event_date`
  (`"18.09.2015"`), `group` (`"VW Group"` for VOW.DE/PAH3.DE, `"Other"`
  for BMW.DE/MBG.DE), `event_window_start` (-10), `event_window_end`
  (10), `shift_estimation_window` (-11), and `estimation_window_length`
  (250). `event_id = 1` is VOW.DE for backward compatibility.

- meta:

  A list of provenance metadata: `firm_tickers` (character vector of all
  four tickers), `groups` (named list mapping group labels to tickers),
  `index_ticker`, `event_date`, `from`, `to`, `source`, `access_date`,
  and `note`.

## Source

Yahoo Finance daily adjusted prices, retrieved 2026-09-04 via the
package's own
[`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md).
This is a small illustrative sample bundled for academic / demonstration
use only; see `data-raw/dieselgate.R` for the reproducible fetch script.

## Details

The event is the U.S. Environmental Protection Agency's Notice of
Violation issued to Volkswagen on **2015-09-18** (a Friday); the
share-price crash lands on the following trading days. The dataset
covers two groups: the "VW Group" (VOW.DE, PAH3.DE — directly implicated
firms) and "Other" (BMW.DE, MBG.DE — peer automakers). The bundled
window layout uses a 250-trading-day estimation window ending 11 days
before the event and an event window of `[-10, +10]` trading days.

Fitting a market model on the "VW Group" events produces a strongly
negative cumulative average abnormal return (CAAR approximately -39
roughly -17% and -13% average abnormal returns on the first two trading
days after the disclosure, while the "Other" peer automakers show
near-zero CAAR (approximately +1%), illustrating the idiosyncratic
nature of the shock.

**Firms:**

- `VOW.DE` — Volkswagen AG ordinary shares (Xetra), event_id = 1

- `PAH3.DE` — Porsche Automobil Holding SE (Xetra), event_id = 2

- `BMW.DE` — BMW AG (Xetra), event_id = 3

- `MBG.DE` — Mercedes-Benz Group AG (Xetra), event_id = 4

**Groups:** "VW Group" (VOW.DE, PAH3.DE) vs "Other" (BMW.DE, MBG.DE).
**Benchmark:** DAX performance index (ticker `^GDAXI`). **Date range:**
2014-06-01 to 2015-11-01.

## Examples

``` r
# \donttest{
data(dieselgate)

# Build a multi-group task and run the full pipeline
task <- EventStudyTask$new(dieselgate$firm, dieselgate$index,
                           dieselgate$request)
task <- run_event_study(task, ParameterSet$new())

# Single-firm: VW crash (event_id = 1)
task$get_car(1L)
#> # A tibble: 21 × 3
#>    relative_index abnormal_returns     car
#>             <int>            <dbl>   <dbl>
#>  1            -10         0.00280  0.00280
#>  2             -9         0.000281 0.00308
#>  3             -8         0.00930  0.0124 
#>  4             -7         0.0224   0.0348 
#>  5             -6        -0.00573  0.0291 
#>  6             -5         0.00576  0.0349 
#>  7             -4        -0.00497  0.0299 
#>  8             -3         0.00253  0.0324 
#>  9             -2         0.000144 0.0326 
#> 10             -1        -0.000434 0.0321 
#> # ℹ 11 more rows

# Multi-group: CAAR comparison
vw_caar    <- task$aar_caar_tbl[task$aar_caar_tbl$group == "VW Group", ]$CSectT[[1]]
other_caar <- task$aar_caar_tbl[task$aar_caar_tbl$group == "Other", ]$CSectT[[1]]
tail(vw_caar[, c("relative_index", "caar", "caar_t")], 1)
#> # A tibble: 1 × 3
#>   relative_index   caar caar_t
#>            <int>  <dbl>  <dbl>
#> 1             10 -0.386  -12.6
tail(other_caar[, c("relative_index", "caar", "caar_t")], 1)
#> # A tibble: 1 × 3
#>   relative_index   caar caar_t
#>            <int>  <dbl>  <dbl>
#> 1             10 0.0132  0.338
# }
```
