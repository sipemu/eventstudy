# Intraday Event Study Task

Extension of
[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)
for intraday (high-frequency) event studies. Supports POSIXct timestamps
and time-based windows specified in minutes or hours.

## Differences from EventStudyTask

- Date column contains POSIXct timestamps instead of date strings

- Event windows specified as time offsets (e.g., `"-30min"`, `"+60min"`)

- Handles trading session boundaries

## See also

Other eventstudy-tasks:
[`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md),
[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md),
[`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md),
[`estimate_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md),
[`nonparametric_intraday_test()`](https://sipemu.github.io/eventstudy/reference/nonparametric_intraday_test.md),
[`plot_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_panel_event_study.md),
[`plot_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md),
[`prepare_intraday_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md),
[`sc_placebo_test()`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)

## Super class

[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)
-\> `IntradayEventStudyTask`

## Public fields

- `.index`:

  The time column name (POSIXct).

- `.target`:

  The price column name.

- `.request_file_columns`:

  Required columns for intraday requests.

## Methods

### Public methods

- [`IntradayEventStudyTask$new()`](#method-IntradayEventStudyTask-initialize)

- [`IntradayEventStudyTask$print()`](#method-IntradayEventStudyTask-print)

- [`IntradayEventStudyTask$clone()`](#method-IntradayEventStudyTask-clone)

Inherited methods

- [`EventStudyTask$get_aar()`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.html#method-get_aar)
- [`EventStudyTask$get_ar()`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.html#method-get_ar)
- [`EventStudyTask$get_car()`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.html#method-get_car)
- [`EventStudyTask$get_model_stats()`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.html#method-get_model_stats)
- [`EventStudyTask$summary()`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.html#method-summary)

------------------------------------------------------------------------

### `IntradayEventStudyTask$new()`

Create a new IntradayEventStudyTask.

#### Usage

    IntradayEventStudyTask$new(
      firm_stock_data_tbl,
      reference_tbl,
      request_tbl,
      factor_tbl = NULL
    )

#### Arguments

- `firm_stock_data_tbl`:

  Dataframe with firm intraday data. Must contain columns: `symbol`,
  `timestamp` (POSIXct), and `price`.

- `reference_tbl`:

  Dataframe with reference market intraday data.

- `request_tbl`:

  Request dataframe with `event_timestamp` (POSIXct), and window
  specifications in minutes.

- `factor_tbl`:

  Optional factor data.

------------------------------------------------------------------------

### `IntradayEventStudyTask$print()`

Print summary.

#### Usage

    IntradayEventStudyTask$print(...)

------------------------------------------------------------------------

### `IntradayEventStudyTask$clone()`

The objects of this class are cloneable with this method.

#### Usage

    IntradayEventStudyTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
