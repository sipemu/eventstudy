# Methods Gate: Smoke Test

## Cumulative Abnormal Return Formula

The Cumulative Abnormal Return (CAR) over the event window \[t_1, t_2\]
is defined as:

\text{CAR}(t_1, t_2) = \sum\_{t=t_1}^{t_2} \hat{\varepsilon}\_t

where \hat{\varepsilon}\_t = R\_{it} - (\hat{\alpha} + \hat{\beta}
R\_{mt}) is the abnormal return under the market model (MacKinlay 1997).

The market model regresses firm returns on an index during an estimation
window, then uses the out-of-sample residuals in the event window as
abnormal returns (Brown and Warner 1985).

## Worked Example: Dieselgate

This example uses the bundled `dieselgate` dataset (4 automakers, 2
groups) to demonstrate the full EventStudy pipeline.

``` r

library(EventStudy)
data("dieselgate")
```

``` r

task <- EventStudyTask$new(
  firm_stock_data_tbl = dieselgate$firm,
  reference_tbl       = dieselgate$index,
  request_tbl         = dieselgate$request
)
params <- ParameterSet$new()
task   <- prepare_event_study(task, params)
task   <- fit_model(task, params)
task   <- calculate_statistics(task, params)
```

The pipeline fits a market model for each firm against the DAX (^GDAXI)
using the estimation window, then computes CAR t-statistics for the
event window around 18 September 2015 (VW emissions revelation).

``` r

car_plot <- plot_event_study(task, type = "car", event_id = 1)
plotly::ggplotly(car_plot)
```

CAR over event window (dieselgate, event 1)

## References

Brown, Stephen J., and Jerold B. Warner. 1985. “Using Daily Stock
Returns: The Case of Event Studies.” *Journal of Financial Economics* 14
(1): 3–31.

MacKinlay, A. Craig. 1997. “Event Studies in Economics and Finance.”
*Journal of Economic Literature* 35 (1): 13–39.
