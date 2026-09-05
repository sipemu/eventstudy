# Compare CARs Across Groups

Performs a t-test (two groups) or ANOVA (3+ groups) to test whether CARs
differ significantly across groups.

## Usage

``` r
car_by_group(task, group_var = "group", car_window = NULL)
```

## Arguments

- task:

  A fitted EventStudyTask.

- group_var:

  Name of the grouping variable. Defaults to "group" (the standard group
  column in EventStudyTask).

- car_window:

  Optional two-element vector for CAR window.

## Value

A list with test results and group-level summary statistics.
