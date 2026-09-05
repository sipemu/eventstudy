# Plot CAR Distribution

Create a histogram of CARs across events, optionally colored by group.

## Usage

``` r
plot_car_distribution(
  task,
  car_window = NULL,
  bins = 30,
  by_group = FALSE,
  title = NULL
)
```

## Arguments

- task:

  A fitted EventStudyTask.

- car_window:

  Optional two-element vector for CAR window.

- bins:

  Number of histogram bins. Default 30.

- by_group:

  Logical. If TRUE, color histogram by group.

- title:

  Optional plot title.

## Value

A ggplot2 object.
