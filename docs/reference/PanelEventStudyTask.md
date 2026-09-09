# Panel Event Study Task

Task container for panel (difference-in-differences style) event
studies. This is designed for settings where units receive treatment at
potentially different times, and we observe outcomes over multiple
periods.

## Methods

- `new(panel_data, unit_id, time_id, outcome, treatment, treatment_time)`:

  Create a new panel event study task.

- [`print()`](https://rdrr.io/r/base/print.html):

  Print summary.

## See also

Other eventstudy-tasks:
[`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md),
[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md),
[`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md),
[`estimate_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md),
[`nonparametric_intraday_test()`](https://sipemu.github.io/eventstudy/reference/nonparametric_intraday_test.md),
[`plot_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_panel_event_study.md),
[`plot_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md),
[`prepare_intraday_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md),
[`sc_placebo_test()`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)

## Public fields

- `panel_data`:

  The panel dataset in long format.

- `unit_id`:

  Name of the unit identifier column.

- `time_id`:

  Name of the time identifier column.

- `outcome`:

  Name of the outcome column.

- `treatment`:

  Name of the treatment indicator column (0/1).

- `treatment_time`:

  Name of the treatment timing column (period when unit first treated;
  NA for never-treated).

- `results`:

  Estimation results (populated after estimation).

## Methods

### Public methods

- [`PanelEventStudyTask$new()`](#method-PanelEventStudyTask-initialize)

- [`PanelEventStudyTask$print()`](#method-PanelEventStudyTask-print)

- [`PanelEventStudyTask$clone()`](#method-PanelEventStudyTask-clone)

------------------------------------------------------------------------

### `PanelEventStudyTask$new()`

Create a new PanelEventStudyTask.

#### Usage

    PanelEventStudyTask$new(
      panel_data,
      unit_id = "unit_id",
      time_id = "time_id",
      outcome = "outcome",
      treatment = "treated",
      treatment_time = "treatment_time"
    )

#### Arguments

- `panel_data`:

  Long-format panel data frame.

- `unit_id`:

  Name of the unit ID column.

- `time_id`:

  Name of the time period column.

- `outcome`:

  Name of the outcome variable column.

- `treatment`:

  Name of the treatment indicator column (0/1).

- `treatment_time`:

  Name of column indicating when each unit is first treated (NA for
  never-treated controls).

------------------------------------------------------------------------

### `PanelEventStudyTask$print()`

Print summary.

#### Usage

    PanelEventStudyTask$print(...)

------------------------------------------------------------------------

### `PanelEventStudyTask$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PanelEventStudyTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
