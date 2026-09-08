# Run a Complete Event Study

Convenience wrapper that runs the full event study pipeline: prepare
data, fit models, and calculate test statistics in a single call.

## Usage

``` r
run_event_study(
  task,
  parameter_set = ParameterSet$new(),
  report = FALSE,
  report_args = list()
)
```

## Arguments

- task:

  An EventStudyTask object.

- parameter_set:

  A ParameterSet object defining the event study. Defaults to a new
  ParameterSet with default settings.

- report:

  Logical. When `FALSE` (default), the function behaves exactly as
  before this parameter was added. When `TRUE`, also renders a report
  via
  [`es_report()`](https://sipemu.github.io/eventstudy/reference/es_report.md)
  after the pipeline completes and attaches the rendered path(s) as
  `attr(task, "report_path")`.

- report_args:

  Named list forwarded to
  [`es_report()`](https://sipemu.github.io/eventstudy/reference/es_report.md)
  when `report = TRUE`. May include `output_file`, `format`, `provider`,
  and any other argument accepted by
  [`es_report()`](https://sipemu.github.io/eventstudy/reference/es_report.md).
  Defaults to [`list()`](https://rdrr.io/r/base/list.html) (uses
  [`es_report()`](https://sipemu.github.io/eventstudy/reference/es_report.md)
  defaults, which render `"event_study_report.html"` in the working
  directory).

## Value

The task object with all results computed. When `report = TRUE`, the
returned task additionally carries `attr(task, "report_path")` with the
path(s) of the rendered report file(s). The return type is always a
fitted `EventStudyTask` in both paths.

## Examples

``` r
if (FALSE) { # \dontrun{
task <- run_event_study(my_task, ParameterSet$new(), report = TRUE)
attr(task, "report_path")  # path to the rendered HTML
} # }
```
