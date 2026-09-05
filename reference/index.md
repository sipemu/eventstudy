# Package index

## Pipeline & Tasks

Core entry points and task containers — the objects and functions a user
touches first to set up and run an event study.

- [`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)
  : Initialization of an Event Study task.
- [`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)
  : Event Study Parameter Set
- [`run_event_study()`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)
  : Run a Complete Event Study
- [`prepare_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)
  : Prepare data for an Event Study
- [`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md)
  : Train the defined model on each event and calculate the abnormal
  return.
- [`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)
  : Calculate test statistics

## Return Models

R6 model classes for estimating normal returns and computing abnormal
returns, plus return-calculation strategy classes.

- [`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)
  : Market Model
- [`MarketAdjustedModel`](https://sipemu.github.io/eventstudy/reference/MarketAdjustedModel.md)
  : Market Adjusted Model
- [`ComparisonPeriodMeanAdjustedModel`](https://sipemu.github.io/eventstudy/reference/ComparisonPeriodMeanAdjustedModel.md)
  : Comparison Period Mean Adjusted Model
- [`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md)
  : Fama-French Three-Factor Model
- [`FamaFrench5FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench5FactorModel.md)
  : Fama-French Five-Factor Model
- [`Carhart4FactorModel`](https://sipemu.github.io/eventstudy/reference/Carhart4FactorModel.md)
  : Carhart Four-Factor Model
- [`LinearFactorModel`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.md)
  : Linear Factor Model Base
- [`GARCHModel`](https://sipemu.github.io/eventstudy/reference/GARCHModel.md)
  : GARCH Model
- [`DCCGARCHModel`](https://sipemu.github.io/eventstudy/reference/DCCGARCHModel.md)
  : DCC-GARCH Model
- [`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md)
  : Rolling Window Model
- [`BHARModel`](https://sipemu.github.io/eventstudy/reference/BHARModel.md)
  : Buy-and-Hold Abnormal Returns (BHAR) Model
- [`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)
  : Volume Event Study Model
- [`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md)
  : Volatility Event Study Model
- [`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md)
  : R6 class for simple return calculation
- [`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md)
  : R6 class for log return calculation

## Test Statistics

Single-event (AR/CAR) and multi-event (AAR/CAAR) test statistic classes
and their container sets.

- [`ARTTest`](https://sipemu.github.io/eventstudy/reference/ARTTest.md)
  : Abnormal Return T Statistic (ART)
- [`CARTTest`](https://sipemu.github.io/eventstudy/reference/CARTTest.md)
  : Cumulative Abnormal Return T Statistic (CART)
- [`BHARTTest`](https://sipemu.github.io/eventstudy/reference/BHARTTest.md)
  : Buy-and-Hold Abnormal Return T Test (BHARTTest)
- [`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md)
  : Cross-Sectional T Test (CSectTTest)
- [`PatellZTest`](https://sipemu.github.io/eventstudy/reference/PatellZTest.md)
  : Patell or Standardized Residual Test (PatellZTest)
- [`BMPTest`](https://sipemu.github.io/eventstudy/reference/BMPTest.md)
  : BMP Test (Boehmer, Musumeci, Poulsen 1991)
- [`SignTest`](https://sipemu.github.io/eventstudy/reference/SignTest.md)
  : Sign Test for Multiple Events
- [`GeneralizedSignTest`](https://sipemu.github.io/eventstudy/reference/GeneralizedSignTest.md)
  : Generalized Sign Test (Cowan 1992)
- [`RankTest`](https://sipemu.github.io/eventstudy/reference/RankTest.md)
  : Rank Test (Corrado 1989)
- [`KolariPynnonenTest`](https://sipemu.github.io/eventstudy/reference/KolariPynnonenTest.md)
  : Kolari-Pynnönen Adjusted BMP Test
- [`CalendarTimePortfolioTest`](https://sipemu.github.io/eventstudy/reference/CalendarTimePortfolioTest.md)
  : Calendar-Time Portfolio Test
- [`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)
  : Single Event Statistic Set
- [`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)
  : Multi Event Statistic Set
- [`StatisticsSetBase`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.md)
  : Adds a test statistic to this container.

## Panel, Intraday & Synthetic Control

Specialized task types and estimation functions for panel DiD, intraday,
and synthetic control event study designs.

- [`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md)
  : Panel Event Study Task
- [`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md)
  : Intraday Event Study Task
- [`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md)
  : Synthetic Control Task
- [`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md)
  : Estimate Panel Event Study
- [`prepare_intraday_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md)
  : Prepare Intraday Event Study
- [`nonparametric_intraday_test()`](https://sipemu.github.io/eventstudy/reference/nonparametric_intraday_test.md)
  : Non-Parametric Intraday Event Study Test
- [`estimate_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md)
  : Estimate Synthetic Control
- [`sc_placebo_test()`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)
  : Placebo Test for Synthetic Control

## AI Advisor

Grounded AI advisor layer — deterministic diagnostics harvester, LLM
advise function, knowledge base, and provider abstraction classes.

- [`es_advise()`](https://sipemu.github.io/eventstudy/reference/es_advise.md)
  : Grounded AI Advice for Event Study Results
- [`es_diagnostics()`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md)
  : Harvest Diagnostics from a Fitted EventStudyTask
- [`es_kb()`](https://sipemu.github.io/eventstudy/reference/es_kb.md) :
  Access the EventStudy Grounding Knowledge Base
- [`recommend_stat()`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md)
  : Recommend Test Statistics via Offline KB Matching
- [`flag_robustness()`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md)
  : Flag Robustness Issues via Offline KB Matching
- [`provider()`](https://sipemu.github.io/eventstudy/reference/provider.md)
  : Construct a Grounded AI Advisor provider
- [`ProviderBase`](https://sipemu.github.io/eventstudy/reference/ProviderBase.md)
  : ProviderBase
- [`AnthropicProvider`](https://sipemu.github.io/eventstudy/reference/AnthropicProvider.md)
  : AnthropicProvider
- [`OpenAICompatProvider`](https://sipemu.github.io/eventstudy/reference/OpenAICompatProvider.md)
  : OpenAICompatProvider
- [`CustomProvider`](https://sipemu.github.io/eventstudy/reference/CustomProvider.md)
  : CustomProvider

## Diagnostics

Model validation, pre-trend testing, and p-value adjustment functions.

- [`validate_task()`](https://sipemu.github.io/eventstudy/reference/validate_task.md)
  : Validate an Event Study experiment
- [`model_diagnostics()`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md)
  : Model Diagnostics for Event Study
- [`pretrend_test()`](https://sipemu.github.io/eventstudy/reference/pretrend_test.md)
  : Pre-trend Test for Event Study
- [`adjust_p_values()`](https://sipemu.github.io/eventstudy/reference/adjust_p_values.md)
  : Adjust P-Values for Multiple Testing

## Cross-Sectional & Simulation

CAR-level cross-sectional analysis, group comparisons, quantiles,
bootstrap inference, and Monte Carlo power simulation.

- [`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)
  : Cross-Sectional Regression of CARs
- [`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md)
  : Compare CARs Across Groups
- [`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md)
  : CAR Quantiles
- [`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md)
  : Wild Bootstrap Inference for Event Studies
- [`simulate_event_study()`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)
  : Simulate Event Study for Power Analysis

## Export & Reporting

Result extraction, multi-format export, broom-compatible tidying, and
automated RMarkdown report generation.

- [`export_results()`](https://sipemu.github.io/eventstudy/reference/export_results.md)
  : Export Event Study Results
- [`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md)
  : Generate Event Study Report
- [`tidy.EventStudyTask()`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)
  : Tidy Event Study Results

## Plotting

Interactive Plotly-based visualization functions for event study
results, stock prices, diagnostics, and specialized designs.

- [`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)
  : Plot Event Study Results
- [`plot_stocks()`](https://sipemu.github.io/eventstudy/reference/plot_stocks.md)
  : Plot Stocks
- [`plot_diagnostics()`](https://sipemu.github.io/eventstudy/reference/plot_diagnostics.md)
  : Plot Model Diagnostics
- [`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md)
  : Plot CAR Distribution
- [`plot_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_panel_event_study.md)
  : Plot Panel Event Study Results
- [`plot_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md)
  : Plot Synthetic Control Results

## Data & Datasets

Functions for downloading stock prices, factor data, and risk-free
rates, plus bundled example datasets.

- [`download_stock_data()`](https://sipemu.github.io/eventstudy/reference/download_stock_data.md)
  : Download Stock Data
- [`download_factor_data()`](https://sipemu.github.io/eventstudy/reference/download_factor_data.md)
  : Download Factor Data
- [`download_risk_free_rate()`](https://sipemu.github.io/eventstudy/reference/download_risk_free_rate.md)
  : Download Risk-Free Rate
- [`dieselgate`](https://sipemu.github.io/eventstudy/reference/dieselgate.md)
  : Volkswagen "Dieselgate" Multi-Automaker Event Study Dataset
