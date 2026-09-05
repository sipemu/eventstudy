# Tidy Event Study Results

Extract event study results in a tidy (long-format) tibble compatible
with broom conventions. This enables integration with standard tidyverse
workflows.

## Usage

``` r
tidy.EventStudyTask(
  x,
  type = c("ar", "car", "aar", "model"),
  stat_name = "CSectT",
  ...
)
```

## Arguments

- x:

  An EventStudyTask object.

- type:

  Type of results to extract: "ar" (abnormal returns), "car" (cumulative
  abnormal returns), "aar" (average abnormal returns with test
  statistics), or "model" (model fit statistics).

- stat_name:

  For type "aar", the name of the multi-event test statistic. Defaults
  to "CSectT".

- ...:

  Additional arguments (unused).

## Value

A tibble with columns following broom conventions:

- term:

  Identifier for the observation (e.g., relative_index)

- estimate:

  The estimated value (AR, CAR, AAR, or coefficient)

- std.error:

  Standard error where available

- statistic:

  Test statistic value

- p.value:

  p-value where available
