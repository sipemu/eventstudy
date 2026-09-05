# Initialization of an Event Study task.

Event Study Task

The Event Study task contains all necessary data for performing an event
study. Furthermore, all calculations are saved in the internal dataframe
named data_tbl.

## Public fields

- `data_tbl`:

  All calculations are saved in this dataframe. The dataframe is a
  nested object with each row is one event, identified by the event id,
  the group, and the firm symbol.

- `aar_caar_tbl`:

  Placeholder for AAR and CAAR test statistics.

- `.keys`:

  event identifier. Do not change.

- `.index`:

  The time column name

- `.target`:

  The price column name.

- `.request_file_columns`:

  The necessary column names of the request dataframe.

- `factor_tbl`:

  Optional factor data for multi-factor models (Fama-French, Carhart).
  Must contain a `date` column.

## Active bindings

- `symbols`:

  Read-only field to get the firm symbols.

- `symbol_data`:

  Read-only field to get the firm symbol data.

- `group_level_data`:

  Read-only field to get the group data.

## Methods

### Public methods

- [`EventStudyTask$new()`](#method-EventStudyTask-initialize)

- [`EventStudyTask$print()`](#method-EventStudyTask-print)

- [`EventStudyTask$summary()`](#method-EventStudyTask-summary)

- [`EventStudyTask$get_ar()`](#method-EventStudyTask-get_ar)

- [`EventStudyTask$get_car()`](#method-EventStudyTask-get_car)

- [`EventStudyTask$get_aar()`](#method-EventStudyTask-get_aar)

- [`EventStudyTask$get_model_stats()`](#method-EventStudyTask-get_model_stats)

- [`EventStudyTask$clone()`](#method-EventStudyTask-clone)

------------------------------------------------------------------------

### `EventStudyTask$new()`

Create a new EventStudyTask from stock data, reference market data, and
an event request specification.

#### Usage

    EventStudyTask$new(
      firm_stock_data_tbl,
      reference_tbl,
      request_tbl,
      factor_tbl = NULL
    )

#### Arguments

- `firm_stock_data_tbl`:

  Dataframe with firm stock data. This dataframe must contain the stock
  (col: symbol) the date (col: date) and the price (col: adjusted)
  column.

- `reference_tbl`:

  Dataframe with firm reference data.

- `request_tbl`:

  The request dataframe for each event.

- `factor_tbl`:

  Optional dataframe with factor data (e.g., Fama-French factors). Must
  contain a `date` column plus factor columns.

------------------------------------------------------------------------

### `EventStudyTask$print()`

Print a summary of the EventStudyTask.

#### Usage

    EventStudyTask$print(...)

------------------------------------------------------------------------

### `EventStudyTask$summary()`

Summarize the event study results.

#### Usage

    EventStudyTask$summary()

#### Returns

A list with summary information about the event study results.

------------------------------------------------------------------------

### `EventStudyTask$get_ar()`

Extract abnormal returns for a single event.

#### Usage

    EventStudyTask$get_ar(event_id = NULL)

#### Arguments

- `event_id`:

  The event identifier.

#### Returns

A tibble with abnormal returns for the event window.

------------------------------------------------------------------------

### `EventStudyTask$get_car()`

Extract cumulative abnormal returns for a single event.

#### Usage

    EventStudyTask$get_car(event_id = NULL)

#### Arguments

- `event_id`:

  The event identifier.

#### Returns

A tibble with cumulative abnormal returns for the event window.

------------------------------------------------------------------------

### `EventStudyTask$get_aar()`

Extract average abnormal returns across events.

#### Usage

    EventStudyTask$get_aar(group = NULL, stat_name = "CSectT")

#### Arguments

- `group`:

  Optional group name to filter by.

- `stat_name`:

  Name of the multi-event test statistic to extract. Defaults to
  "CSectT".

#### Returns

A tibble with AAR/CAAR results.

------------------------------------------------------------------------

### `EventStudyTask$get_model_stats()`

Extract model statistics for a single event.

#### Usage

    EventStudyTask$get_model_stats(event_id = NULL)

#### Arguments

- `event_id`:

  The event identifier.

#### Returns

A list with model statistics.

------------------------------------------------------------------------

### `EventStudyTask$clone()`

The objects of this class are cloneable with this method.

#### Usage

    EventStudyTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
