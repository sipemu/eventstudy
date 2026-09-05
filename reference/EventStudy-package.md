# EventStudy: Event Study Analysis in R

Perform financial event study analysis in R. Implements the classical
event study methodology (MacKinlay 1997) with multiple return models,
parametric and non-parametric test statistics, and visualization tools.

## Return Models

- [`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)
  – OLS market model (single factor, optional HAC SEs)

- [`MarketAdjustedModel`](https://sipemu.github.io/eventstudy/reference/MarketAdjustedModel.md)
  – Market-adjusted returns

- [`ComparisonPeriodMeanAdjustedModel`](https://sipemu.github.io/eventstudy/reference/ComparisonPeriodMeanAdjustedModel.md)
  – Mean-adjusted returns

- [`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md)
  – Fama-French 3-factor model

- [`FamaFrench5FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench5FactorModel.md)
  – Fama-French 5-factor model

- [`Carhart4FactorModel`](https://sipemu.github.io/eventstudy/reference/Carhart4FactorModel.md)
  – Carhart 4-factor model

- [`GARCHModel`](https://sipemu.github.io/eventstudy/reference/GARCHModel.md)
  – GARCH(1,1) model

- [`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md)
  – Rolling-window OLS (time-varying beta)

- [`DCCGARCHModel`](https://sipemu.github.io/eventstudy/reference/DCCGARCHModel.md)
  – DCC-GARCH (time-varying beta)

- [`BHARModel`](https://sipemu.github.io/eventstudy/reference/BHARModel.md)
  – Buy-and-hold abnormal returns

- [`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)
  – Volume event study model

- [`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md)
  – Volatility event study model

## Test Statistics

**Single-event:**

- [`ARTTest`](https://sipemu.github.io/eventstudy/reference/ARTTest.md)
  – Abnormal return t-test

- [`CARTTest`](https://sipemu.github.io/eventstudy/reference/CARTTest.md)
  – Cumulative abnormal return t-test

- [`BHARTTest`](https://sipemu.github.io/eventstudy/reference/BHARTTest.md)
  – BHAR t-test

**Multi-event:**

- [`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md)
  – Cross-sectional t-test

- [`PatellZTest`](https://sipemu.github.io/eventstudy/reference/PatellZTest.md)
  – Patell standardized residual test

- [`SignTest`](https://sipemu.github.io/eventstudy/reference/SignTest.md)
  – Sign test

- [`GeneralizedSignTest`](https://sipemu.github.io/eventstudy/reference/GeneralizedSignTest.md)
  – Generalized sign test (Cowan 1992)

- [`RankTest`](https://sipemu.github.io/eventstudy/reference/RankTest.md)
  – Rank test (Corrado 1989)

- [`BMPTest`](https://sipemu.github.io/eventstudy/reference/BMPTest.md)
  – Boehmer, Musumeci & Poulsen (1991) test

- [`KolariPynnonenTest`](https://sipemu.github.io/eventstudy/reference/KolariPynnonenTest.md)
  – Kolari-Pynnönen adjusted BMP test

- [`CalendarTimePortfolioTest`](https://sipemu.github.io/eventstudy/reference/CalendarTimePortfolioTest.md)
  – Calendar-time portfolio test

## Inference & Robustness

- [`adjust_p_values`](https://sipemu.github.io/eventstudy/reference/adjust_p_values.md)
  – Multiple testing corrections (BH, Bonferroni, etc.)

- [`bootstrap_test`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md)
  – Wild bootstrap inference

- HAC standard errors via `use_hac = TRUE` in model constructors

## Simulation

- [`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)
  – Monte Carlo power analysis

## Pipeline

The standard workflow is:

1.  Create an
    [`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)
    with data

2.  Define a
    [`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)
    with model and test choices

3.  Run
    [`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(task, parameter_set)`
    (or the individual steps:
    [`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md),
    [`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md),
    [`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md))

4.  Extract results with `get_ar()`, `get_car()`, `get_aar()`, or
    [`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)

5.  Export with
    [`export_results`](https://sipemu.github.io/eventstudy/reference/export_results.md)

6.  Visualize with
    [`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)

7.  Generate reports with
    [`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)

## Extensions

- [`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)
  – Explain CARs with firm characteristics

- [`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md)
  – Intraday event studies

- [`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md)
  – Panel DiD event studies (TWFE, Sun-Abraham, Callaway-Sant'Anna, de
  Chaisemartin-D'Haultfoeuille, Borusyak-Jaravel-Spiess)

- [`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md)
  – Synthetic control methods

- [`download_stock_data`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md),
  [`download_factor_data`](https://sipemu.github.io/eventstudy/reference/download_factor_data.md)
  – Data helpers

## References

MacKinlay, A. C. (1997). Event Studies in Economics and Finance.
*Journal of Economic Literature*, 35(1), 13–39.

Fama, E. F. and French, K. R. (1993). Common risk factors in the returns
on stocks and bonds. *Journal of Financial Economics*, 33(1), 3–56.

Carhart, M. M. (1997). On persistence in mutual fund performance. *The
Journal of Finance*, 52(1), 57–82.

## See also

Useful links:

- <https://github.com/sipemu/eventstudy>

- Report bugs at <https://github.com/sipemu/eventstudy/issues>

## Author

**Maintainer**: Simon Mueller <sm@data-zoo.de>

Authors:

- Simon Mueller <sm@data-zoo.de>
