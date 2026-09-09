# Getting Started with EventStudy

## Introduction

An event study measures the impact of an event – a merger, earnings
announcement, or macroeconomic shock – on the value of a firm.
`EventStudy` offers a modular pipeline for conducting these analyses in
R: apply common return models, run diagnostic and test statistics, and
extract publication-ready results, all in a workflow that composes
cleanly with the tidyverse.

This vignette walks through the core flow: define a task, run the study,
inspect the results, and extract or plot them. It uses the Dieselgate
scandal as a running example.

## Quick Start

If you already have firm data, index data, and a request table (all
defined below), the entire pipeline runs in a single call:

`# Preview of the full pipeline — data objects are loaded and unpacked step-by-step below.`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``# One-call shortcut: prepare -> fit -> calculate, in one step`` ``est_task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(`` `` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)``,`` `` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_calculation ``=`` `[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md)`$``new``(``)``,`` `` return_model ``=`` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)``,`` `` single_event_statistics ``=`` `[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)`$``new``(``)``,`` `` multi_event_statistics ``=`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(``)`` `` ``)`` ``)`` `` ``est_task``$``aar_caar_tbl`

The rest of this vignette unpacks the same pipeline step by step, so you
can see – and customize – each stage.

## Example: Dieselgate

To demonstrate the functionality and ease of use of EventStudy, let’s
look at an example: an event study analysis of the Dieselgate scandal.
This example demonstrates how to use the EventStudy package by
conducting an event study analysis on the “Dieselgate” scandal. The
scandal, which erupted in 2015, involved Volkswagen’s admission that it
had installed software on its diesel cars to cheat on emissions tests.
This significant event had substantial effects on the stock prices of
Volkswagen and other automotive companies.

The following code will guide you through the steps of performing an
event study analysis, from the initial setup to the extraction of
results.

### Initialization

The first step is to load necessary packages and data. This includes
market data for the companies of interest and the index during the event
study period.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``# Load the bundled frozen Dieselgate dataset (replaces a live network download via`` ``# tidyquant so this vignette builds fully offline and reproducibly on CRAN/pkgdown).`` `[`data`](https://rdrr.io/r/utils/data.html)`(``dieselgate``)`` `` ``firm_tbl`` ``<-`` ``dieselgate``$``firm`` ``# symbol / date / adjusted (4 firms, DD-MM-YYYY dates)`` ``index_tbl`` ``<-`` ``dieselgate``$``index`` ``# symbol / date / adjusted (^GDAXI)`` ``request_tbl`` ``<-`` ``dieselgate``$``request`` ``# event_id / firm_symbol / index_symbol / event_date /`` `` ``# group / event_window_start / event_window_end /`` `` ``# shift_estimation_window / estimation_window_length`

Both, the firm data as the index data should have the following
structure:

1.  **symbol**: Contains the symbol of the stock.
2.  **date**: The date of the price information.
3.  **adjusted**: The price of the stock at given date. The price column
    can be parametrized according to your needs when the task is
    defined. The default is `adjusted`.

[`head`](https://rdrr.io/r/utils/head.html)`(``firm_tbl``)`` ``#> ``# A tibble: 6 × 3`` ``#> symbol date adjusted`` ``#> ``<chr>`` ``<chr>`` ``<dbl>`` ``#> ``1`` VOW.DE 02.06.2014 113.`` ``#> ``2`` VOW.DE 03.06.2014 112.`` ``#> ``3`` VOW.DE 04.06.2014 109.`` ``#> ``4`` VOW.DE 05.06.2014 112.`` ``#> ``5`` VOW.DE 06.06.2014 112.`` ``#> ``6`` VOW.DE 09.06.2014 112.`

### Define the Event Study

Next, we define the event study. This involves specifying the return
calculation method, the market model, and the test statistics to be
used. In this example, we use logarithmic returns, the market model, and
the default AR and CAR T-Tests. Descriptions of the available models can
be found on our website: [Expected Return
Models](https://eventstudy.de/models/expected_return.html "Choosing the Right Approach: A Comprehensive Guide to Estimating Expected Returns.").

`# Parametrization of the Event Study`` ``log_return`` ``=`` `[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md)`$``new``(``)`` ``market_model`` ``=`` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)`

For single events the standard [AR and CAR T
test](https://eventstudy.de/statistics/ar_car_statistics.html "A Comprehensive Guide to Analyzing Abnormal Returns in Event Studies.")
are applied. For performing an Event Study on multiple events, the
Cross-Sectional T Test (AAR and CAAR) is applied by default, alongside
Patell Z, BMP, sign, and rank tests.

`# Define single event test statistics`` ``# Per default AR and CAR T-Tests are applied`` ``single_event_tests`` ``=`` `[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)`$``new``(``)`` `` ``# Per default CSEct T Test is applied (AAR & CAAR)`` ``multiple_event_tests`` ``=`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(``)`

Your Event Study is then defined in a parameter set:

`# Setup parameter set`` ``param_set`` ``=`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_calculation ``=`` ``log_return``,`` `` return_model ``=`` ``market_model``,`` `` single_event_statistics ``=`` ``single_event_tests``,`` `` multi_event_statistics ``=`` ``multiple_event_tests``)`

### Execute the Event Study

Now, with everything set up, we can execute the event study. This
involves preparing the event study, fitting the model, and calculating
the statistics. In the first step the task is defined. The firm and the
index data gathered before as also the request data is necessary for
performing the study.

`est_task`` ``=`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`

Internally, firm and index data symbol and price column names are
renamed. Afterwards a join is applied and the data is collected in a
data frame with one event per row. Let’s have a look at the internal
data frame:

[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``)`` ``#> ``# A tibble: 4 × 5`` ``#> ``# Groups: event_id, group, firm_symbol [4]`` ``#> firm_symbol event_id group data request `` ``#> ``<chr>`` ``<int>`` ``<chr>`` ``<list>`` ``<list>`` `` ``#> ``1`` VOW.DE 1 VW Group ``<tibble [360 × 4]>`` ``<tibble [1 × 6]>`` ``#> ``2`` PAH3.DE 2 VW Group ``<tibble [360 × 4]>`` ``<tibble [1 × 6]>`` ``#> ``3`` BMW.DE 3 Other ``<tibble [360 × 4]>`` ``<tibble [1 × 6]>`` ``#> ``4`` MBG.DE 4 Other ``<tibble [360 × 4]>`` ``<tibble [1 × 6]>`

[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``$``data``[[``1``]``]``)`` ``#> ``# A tibble: 6 × 4`` ``#> date firm_adjusted index_symbol index_adjusted`` ``#> ``<chr>`` ``<dbl>`` ``<chr>`` ``<dbl>`` ``#> ``1`` 02.06.2014 113. ^GDAXI ``9``950.`` ``#> ``2`` 03.06.2014 112. ^GDAXI ``9``920.`` ``#> ``3`` 04.06.2014 109. ^GDAXI ``9``927.`` ``#> ``4`` 05.06.2014 112. ^GDAXI ``9``948.`` ``#> ``5`` 06.06.2014 112. ^GDAXI ``9``987.`` ``#> ``6`` 09.06.2014 112. ^GDAXI ``10``009.`

`est_task``$``data_tbl``$``request``[[``1``]``]`` ``#> ``# A tibble: 1 × 6`` ``#> index_symbol event_date event_window_start event_window_end`` ``#> ``<chr>`` ``<chr>`` ``<int>`` ``<int>`` ``#> ``1`` ^GDAXI 18.09.2015 -``10`` 10`` ``#> ``# ℹ 2 more variables: shift_estimation_window <int>,`` ``#> ``# estimation_window_length <int>`

The internal data structure is important for you if you plan to develop
your own statistical or econometric model or test statistic.

`est_task`` ``=`` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``est_task``, ``param_set``)`

[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``$``data``[[``1``]``]``)`` ``#> ``# A tibble: 6 × 10`` ``#> date firm_adjusted index_symbol index_adjusted firm_returns index_returns`` ``#> ``<chr>`` ``<dbl>`` ``<chr>`` ``<dbl>`` ``<dbl>`` ``<dbl>`` ``#> ``1`` 02.06.20… 113. ^GDAXI ``9``950. ``NA`` ``NA`` `` ``#> ``2`` 03.06.20… 112. ^GDAXI ``9``920. -``0.001``28`` -``0.003``06`` `` ``#> ``3`` 04.06.20… 109. ^GDAXI ``9``927. -``0.026``3`` 0.000``698`` ``#> ``4`` 05.06.20… 112. ^GDAXI ``9``948. 0.019``6`` 0.002``13`` `` ``#> ``5`` 06.06.20… 112. ^GDAXI ``9``987. 0.002``84`` 0.003``95`` `` ``#> ``6`` 09.06.20… 112. ^GDAXI ``10``009. 0.003``86`` 0.002``14`` `` ``#> ``# ℹ 4 more variables: event_date <dbl>, relative_index <int>,`` ``#> ``# event_window <dbl>, estimation_window <dbl>`

`est_task`` ``=`` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``est_task``, ``param_set``)`

[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``)`` ``#> ``# A tibble: 4 × 6`` ``#> ``# Groups: event_id, group, firm_symbol [4]`` ``#> firm_symbol event_id group data request model `` ``#> ``<chr>`` ``<int>`` ``<chr>`` ``<list>`` ``<list>`` ``<list>`` `` ``#> ``1`` VOW.DE 1 VW Group ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`` ``#> ``2`` PAH3.DE 2 VW Group ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`` ``#> ``3`` BMW.DE 3 Other ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`` ``#> ``4`` MBG.DE 4 Other ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`

`est_task``$``data_tbl``$``model``[[``1``]``]`` ``#> <MarketModel>`` ``#> Inherits from: <ModelBase>`` ``#> Public:`` ``#> abnormal_returns: function (data_tbl) `` ``#> clone: function (deep = FALSE) `` ``#> degenerate_mode: lenient`` ``#> event_id: 1`` ``#> firm_symbol: VOW.DE`` ``#> fit: function (data_tbl) `` ``#> formula: formula`` ``#> hac_lag: NULL`` ``#> initialize: function (use_hac = FALSE, hac_lag = NULL) `` ``#> is_fitted: active binding`` ``#> model: active binding`` ``#> model_name: MarketModel`` ``#> set_formula: function (formula) `` ``#> statistics: active binding`` ``#> use_hac: FALSE`` ``#> Private:`` ``#> .degenerate_handled: FALSE`` ``#> .error: NULL`` ``#> .fitted_model: lm`` ``#> .is_fitted: TRUE`` ``#> .statistics: list`` ``#> add_residuals: function (residuals) `` ``#> calculate_forecast_error_correction: function (sigma, estimation_window_length, estimation_market_returns, `` ``#> calculate_statistics: function (data_tbl) `` ``#> first_order_autocorrelation: function (residuals)`

`est_task`` ``=`` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``est_task``, ``param_set``)`

[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``)`` ``#> ``# A tibble: 4 × 8`` ``#> ``# Groups: event_id, group, firm_symbol [4]`` ``#> firm_symbol event_id group data request model ART CART `` ``#> ``<chr>`` ``<int>`` ``<chr>`` ``<list>`` ``<list>`` ``<list>`` ``<list>`` ``<list>`` `` ``#> ``1`` VOW.DE 1 VW Group ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`` ``#> ``2`` PAH3.DE 2 VW Group ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`` ``#> ``3`` BMW.DE 3 Other ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`` ``#> ``4`` MBG.DE 4 Other ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`

`est_task``$``data_tbl``$``ART``[[``1``]``]`` ``#> ``# A tibble: 21 × 4`` ``#> relative_index abnormal_returns ar_t`` ``#> ``<int>`` ``<dbl>`` ``<dbl>`` ``#> `` 1`` -``10`` 0.002``82`` 0.281 `` ``#> `` 2`` -``9`` 0.000``357`` 0.035``6`` ``#> `` 3`` -``8`` 0.009``20`` 0.918 `` ``#> `` 4`` -``7`` 0.022``2`` 2.21 `` ``#> `` 5`` -``6`` -``0.005``79`` -``0.577`` `` ``#> `` 6`` -``5`` 0.005``83`` 0.581 `` ``#> `` 7`` -``4`` -``0.004``92`` -``0.491`` `` ``#> `` 8`` -``3`` 0.002``58`` 0.258 `` ``#> `` 9`` -``2`` 0.000``213`` 0.021``3`` ``#> ``10`` -``1`` -``0.000``376`` -``0.037``5`` ``#> ``# ℹ 11 more rows`` ``#> ``# ℹ 1 more variable: ar_t_dist <dist>`

`est_task``$``data_tbl``$``CART``[[``1``]``]`` ``#> ``# A tibble: 21 × 8`` ``#> relative_index abnormal_returns event_window_length car_window car`` ``#> ``<int>`` ``<dbl>`` ``<int>`` ``<chr>`` ``<dbl>`` ``#> `` 1`` -``10`` 0.002``82`` 1 [-10, -10] 0.002``82`` ``#> `` 2`` -``9`` 0.000``357`` 2 [-10, -9] 0.003``17`` ``#> `` 3`` -``8`` 0.009``20`` 3 [-10, -8] 0.012``4`` `` ``#> `` 4`` -``7`` 0.022``2`` 4 [-10, -7] 0.034``6`` `` ``#> `` 5`` -``6`` -``0.005``79`` 5 [-10, -6] 0.028``8`` `` ``#> `` 6`` -``5`` 0.005``83`` 6 [-10, -5] 0.034``6`` `` ``#> `` 7`` -``4`` -``0.004``92`` 7 [-10, -4] 0.029``7`` `` ``#> `` 8`` -``3`` 0.002``58`` 8 [-10, -3] 0.032``3`` `` ``#> `` 9`` -``2`` 0.000``213`` 9 [-10, -2] 0.032``5`` `` ``#> ``10`` -``1`` -``0.000``376`` 10 [-10, -1] 0.032``1`` `` ``#> ``# ℹ 11 more rows`` ``#> ``# ℹ 3 more variables: corrected_car <dbl>, car_t <dbl>, car_t_dist <dist>`

`est_task``$``aar_caar_tbl`` ``#> ``# A tibble: 2 × 4`` ``#> ``# Groups: group [2]`` ``#> group data model CSectT `` ``#> ``<chr>`` ``<list>`` ``<list>`` ``<list>`` `` ``#> ``1`` VW Group ``<tibble [720 × 13]>`` ``<tibble [2 × 3]>`` ``<tibble [21 × 10]>`` ``#> ``2`` Other ``<tibble [720 × 13]>`` ``<tibble [2 × 3]>`` ``<tibble [21 × 10]>`

`est_task``$``aar_caar_tbl``$``CSectT``[[``1``]``]`` ``#> ``# A tibble: 21 × 10`` ``#> relative_index aar n_events n_valid_events n_pos n_neg aar_t caar`` ``#> ``<int>`` ``<dbl>`` ``<int>`` ``<int>`` ``<int>`` ``<int>`` ``<dbl>`` ``<dbl>`` ``#> `` 1`` -``10`` 0.004``18`` 2 2 2 0 3.07 0.004``18`` ``#> `` 2`` -``9`` -``0.001``22`` 2 2 1 1 -``0.774`` 0.002``95`` ``#> `` 3`` -``8`` 0.006``69`` 2 2 2 0 2.66 0.009``64`` ``#> `` 4`` -``7`` 0.015``8`` 2 2 2 0 2.46 0.025``4`` `` ``#> `` 5`` -``6`` -``0.002``61`` 2 2 1 1 -``0.819`` 0.022``8`` `` ``#> `` 6`` -``5`` 0.004``55`` 2 2 2 0 3.55 0.027``4`` `` ``#> `` 7`` -``4`` -``0.004``16`` 2 2 0 2 -``5.48`` 0.023``2`` `` ``#> `` 8`` -``3`` 0.004``71`` 2 2 2 0 2.21 0.027``9`` `` ``#> `` 9`` -``2`` 0.007``96`` 2 2 2 0 1.03 0.035``9`` `` ``#> ``10`` -``1`` -``0.004``83`` 2 2 0 2 -``1.08`` 0.031``0`` `` ``#> ``# ℹ 11 more rows`` ``#> ``# ℹ 2 more variables: caar_t <dbl>, car_window <chr>`

### Next Steps

This vignette covered the core market-model pipeline. To go deeper:

- **Return models:** See
  [`vignette("factor-models-bhar")`](https://sipemu.github.io/eventstudy/articles/factor-models-bhar.md)
  for multi-factor and long-horizon models
- **Test statistics:** See
  [`vignette("inference-robustness")`](https://sipemu.github.io/eventstudy/articles/inference-robustness.md)
  for robust inference and bootstrap
- **Result extraction:** See
  [`vignette("result-extraction")`](https://sipemu.github.io/eventstudy/articles/result-extraction.md)
  for export, `tidy()`, and cross-sectional analysis
- **AI advisor:** See
  [`vignette("ai-advisor")`](https://sipemu.github.io/eventstudy/articles/ai-advisor.md)
  for deterministic diagnostics and LLM interpretation
- **Full gallery:** See
  [`vignette("gallery")`](https://sipemu.github.io/eventstudy/articles/gallery.md)
  for all available vignettes by topic

## Conclusion

Event Study offers a streamlined and intuitive interface for conducting
event study analysis in R, making it a valuable addition to the toolkit
of any researcher or analyst in finance. Whether you’re assessing the
impact of corporate events on stock prices or investigating the effects
of macroeconomic news, Event Study provides a flexible and efficient
solution.
