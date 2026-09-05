# Calendar-Time Portfolio Test

Aggregates event-firm returns into calendar-time portfolios and tests
whether the portfolio intercept (alpha) is significantly different from
zero. This approach naturally handles cross-sectional dependence that
arises when events cluster in calendar time.

For each relative event day, the test forms an equal-weighted portfolio
of all event firms' abnormal returns and computes a t-statistic of the
mean portfolio return.

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `CalendarTimePortfolioTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`CalendarTimePortfolioTest$compute()`](#method-CalendarTimePortfolioTest-compute)

- [`CalendarTimePortfolioTest$clone()`](#method-CalendarTimePortfolioTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `CalendarTimePortfolioTest$compute()`

Computes the calendar-time portfolio test.

#### Usage

    CalendarTimePortfolioTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for multiple events with calculated abnormal returns.

- `model`:

  The fitted models (unused directly).

------------------------------------------------------------------------

### `CalendarTimePortfolioTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CalendarTimePortfolioTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
