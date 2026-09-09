# Methods: Intraday Event Studies

## 1. Title & Abstract

This article covers **intraday event studies** — abnormal-return
analysis at minute (or finer) frequency rather than daily. Many events
(earnings releases, regulatory announcements, central-bank
communications) resolve within minutes, and daily data averages that
signal away. EventStudy’s `IntradayEventStudyTask` runs the same
market-model machinery on POSIXct-timestamped high-frequency bars. Here
it is estimated live on an inline synthetic one-minute session.

## 2. When to Use This Method

Use an intraday study when the event’s effect is expected to materialise
and possibly decay **within a trading day**, and you have timestamped
bar or tick data. The frequency lets you locate the exact minute of
price impact and measure how quickly the market absorbs the news
(Barclay and Warner 1993) — detail that a daily study cannot resolve.

## 3. Intuition

The idea is identical to the daily market model, only the clock ticks
faster. Fit the firm’s minute returns on the market’s minute returns
over a pre-event estimation window, then measure minute-by-minute
abnormal returns around the event timestamp. Cumulating them across the
intraday event window traces the price-impact path in real time.

## 4. Model & Null Hypothesis

At intraday frequency \tau, the abnormal return is the market-model
residual computed by the same shared engine used for daily studies:

AR\_{i,\tau} = r\_{i,\tau} - \hat{\alpha}\_i - \hat{\beta}\_i\\
r\_{m,\tau}.

The null hypothesis is H_0: \mathbb{E}\[AR\_{i,\tau}\] = 0 across the
intraday event window — no abnormal minute-level return around the
event.

## 5. Assumptions

- **Timestamp alignment:** firm and reference bars share a common time
  grid; timestamps must be `POSIXct`.
- **Stable intraday beta** estimated in the pre-event window.
- **Microstructure caveats:** bid-ask bounce and thin trading inflate
  high-frequency noise; keep the bar size coarse enough to be
  meaningful.
- **Clean estimation window** free of overlapping intraday events.

## 6. Worked Example

We construct an inline synthetic intraday dataset in base R / `tibble`
with an explicit local `set.seed(42)` for byte-stable output. Column
names match the existing `intraday-event-study` vignette: firm and
reference tables carry `symbol`, `timestamp` (POSIXct), `price`; the
request carries `event_id`, `group`, `firm_symbol`, `index_symbol`,
`event_timestamp`, and minute windows.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`tibble`](https://tibble.tidyverse.org/)`)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` `` ``ts`` ``<-`` `[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)`(``"2023-06-01 09:30:00"``, tz ``=`` ``"UTC"``)``,`` `` by ``=`` ``60``, length.out ``=`` ``300``)`` ``# 300 one-minute bars`` `` ``firm_data`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"FIRM_A"``,`` `` timestamp ``=`` ``ts``, ``# MUST be POSIXct or the constructor stops`` `` price ``=`` ``100`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``, ``0``, ``0.001``)``)`` ``)`` ``index_data`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"INDEX_1"``,`` `` timestamp ``=`` ``ts``,`` `` price ``=`` ``1000`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``, ``0``, ``0.0008``)``)`` ``)`` ``request`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1L``,`` `` group ``=`` ``"Intraday"``,`` `` firm_symbol ``=`` ``"FIRM_A"``,`` `` index_symbol ``=`` ``"INDEX_1"``,`` `` event_timestamp ``=`` `[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)`(``"2023-06-01 12:30:00"``, tz ``=`` ``"UTC"``)``,`` `` event_window_start ``=`` ``-``30L``,`` `` event_window_end ``=`` ``30L``,`` `` shift_estimation_window ``=`` ``-``31L``,`` `` estimation_window_length ``=`` ``120L`` ``)`` `` ``task`` ``<-`` `[`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request``)`` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``)`` ``task`` ``<-`` `[`prepare_intraday_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md)`(``task``, ``params``)`` ``task`` ``<-`` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``task``, ``params``)`` ``task`` ``<-`` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``task``, ``params``)`

## 7. Rendered Table

`es_tt``(`` `` `[`head`](https://rdrr.io/r/utils/head.html)`(`[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task``, type ``=`` ``"car"``)``, ``10``)``,`` `` caption ``=`` ``"Intraday cumulative abnormal returns around the event minute (first 10 offsets)."`` ``)`

| event_id | group | firm_symbol | term | estimate | std.error | statistic | p.value |
|----|----|----|----|----|----|----|----|
| 1 | Intraday | FIRM_A | \[-30,-30\] | 0.00007718 | 0.0009403 | 0.08208 | 0.9347 |
| 1 | Intraday | FIRM_A | \[-30,-29\] | -0.00140952 | 0.0013298 | -1.05996 | 0.2913 |
| 1 | Intraday | FIRM_A | \[-30,-28\] | -0.00019876 | 0.0016287 | -0.12204 | 0.9031 |
| 1 | Intraday | FIRM_A | \[-30,-27\] | -0.00039988 | 0.0018806 | -0.21264 | 0.832 |
| 1 | Intraday | FIRM_A | \[-30,-26\] | -0.0008208 | 0.0021026 | -0.39038 | 0.697 |
| 1 | Intraday | FIRM_A | \[-30,-25\] | -0.00199079 | 0.0023033 | -0.86433 | 0.3892 |
| 1 | Intraday | FIRM_A | \[-30,-24\] | -0.00202787 | 0.0024878 | -0.81512 | 0.4166 |
| 1 | Intraday | FIRM_A | \[-30,-23\] | -0.00281193 | 0.0026596 | -1.05728 | 0.2925 |
| 1 | Intraday | FIRM_A | \[-30,-22\] | -0.00344262 | 0.0028209 | -1.22039 | 0.2247 |
| 1 | Intraday | FIRM_A | \[-30,-21\] | -0.00206269 | 0.0029735 | -0.69369 | 0.4892 |

Intraday cumulative abnormal returns around the event minute (first 10
offsets). {#tinytable_5qnmkssvfqzhj28rtlre .table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## 8. Rendered Plot

[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)`(``task``)`

![Intraday abnormal-return path around the event
timestamp.](methods-intraday_files/figure-html/results-plot-1.png)

Intraday abnormal-return path around the event timestamp.

## 9. Interpretation

The table in §7 reports minute-level cumulative abnormal returns
relative to the event timestamp; the corresponding t-statistics flag
which minutes carry a significant response. The plot in §8 shows the
intraday path — a sharp break at offset zero followed by a plateau is
the signature of near-instant price discovery, whereas a gradual drift
suggests slower information diffusion. Because the underlying
abnormal-return engine (§4) is shared with the daily pipeline, every
downstream statistic and plot behaves identically; only the clock
changes.

## References

Barclay, Michael J., and Jerold B. Warner. 1993. “Stealth Trading and
Volatility: Which Trades Move Prices?” *Journal of Financial Economics*
34 (3): 281–305.
