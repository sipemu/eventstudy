# EventStudy Colour Palette

A named character vector of hex colours for EventStudy plots, anchored
on the Okabe-Ito colorblind-safe (Color Universal Design) qualitative
set with the EventStudy brand primary blue as the leading colour.

## Usage

``` r
es_colours
```

## Format

A named character vector of length 12. Every value is a six-digit hex
colour (`^#[0-9A-Fa-f]{6}$`).

## Details

Semantic roles: `primary` (main series line/point/fill), `event`
(event-date marker), `reference` (zero lines, ACF baseline), `ci_band`
(confidence ribbon fill; apply alpha at the geom level), and
`group1`–`group8` (qualitative multi-series palette in Okabe-Ito order).

## See also

[`theme_eventstudy`](https://sipemu.github.io/eventstudy/reference/theme_eventstudy.md),
[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)

Other eventstudy-plots:
[`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md),
[`plot_diagnostics()`](https://sipemu.github.io/eventstudy/reference/plot_diagnostics.md),
[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md),
[`plot_stocks()`](https://sipemu.github.io/eventstudy/reference/plot_stocks.md),
[`theme_eventstudy()`](https://sipemu.github.io/eventstudy/reference/theme_eventstudy.md)
