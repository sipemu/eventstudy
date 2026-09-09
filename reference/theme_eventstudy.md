# EventStudy ggplot2 Theme

A clean, publication-ready ggplot2 theme for EventStudy plots. Built on
[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with a centred plot title, a bottom legend, and subtle gridlines styled
to the EventStudy palette.

## Usage

``` r
theme_eventstudy(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Base font size in points. Default is 11.

- base_family:

  Base font family. Default `""` uses the R device default, which works
  on all platforms without requiring installed fonts. Pass a family name
  (e.g. `"Inter"`) if that font is installed on the target device.

## Value

A [`theme`](https://ggplot2.tidyverse.org/reference/theme.html) object
that composes with `+`.

## See also

[`es_colours`](https://sipemu.github.io/eventstudy/reference/es_colours.md),
[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)

Other eventstudy-plots:
[`es_colours`](https://sipemu.github.io/eventstudy/reference/es_colours.md),
[`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md),
[`plot_diagnostics()`](https://sipemu.github.io/eventstudy/reference/plot_diagnostics.md),
[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md),
[`plot_stocks()`](https://sipemu.github.io/eventstudy/reference/plot_stocks.md)
