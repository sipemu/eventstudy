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

`es_tt``(`[`head`](https://rdrr.io/r/utils/head.html)`(``firm_tbl``)``)`

| symbol | date       | adjusted |
|--------|------------|----------|
| VOW.DE | 02.06.2014 | 112.5    |
| VOW.DE | 03.06.2014 | 112.4    |
| VOW.DE | 04.06.2014 | 109.5    |
| VOW.DE | 05.06.2014 | 111.6    |
| VOW.DE | 06.06.2014 | 111.9    |
| VOW.DE | 09.06.2014 | 112.4    |

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

`es_tt``(`[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``$``data``[[``1``]``]``)``)`

| date       | firm_adjusted | index_symbol | index_adjusted |
|------------|---------------|--------------|----------------|
| 02.06.2014 | 112.5         | ^GDAXI       | 9950           |
| 03.06.2014 | 112.4         | ^GDAXI       | 9920           |
| 04.06.2014 | 109.5         | ^GDAXI       | 9927           |
| 05.06.2014 | 111.6         | ^GDAXI       | 9948           |
| 06.06.2014 | 111.9         | ^GDAXI       | 9987           |
| 09.06.2014 | 112.4         | ^GDAXI       | 10009          |

`es_tt``(``est_task``$``data_tbl``$``request``[[``1``]``]``)`

| index_symbol | event_date | event_window_start | event_window_end | shift_estimation_window | estimation_window_length |
|----|----|----|----|----|----|
| ^GDAXI | 18.09.2015 | -10 | 10 | -11 | 250 |

The internal data structure is important for you if you plan to develop
your own statistical or econometric model or test statistic.

`est_task`` ``=`` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``est_task``, ``param_set``)`

`es_tt``(`[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``$``data``[[``1``]``]``)``)`

| date | firm_adjusted | index_symbol | index_adjusted | firm_returns | index_returns | event_date | relative_index | event_window | estimation_window |
|----|----|----|----|----|----|----|----|----|----|
| 02.06.2014 | 112.5 | ^GDAXI | 9950 | NA | NA | 0 | -329 | 0 | 0 |
| 03.06.2014 | 112.4 | ^GDAXI | 9920 | -0.001284 | -0.0030579 | 0 | -328 | 0 | 0 |
| 04.06.2014 | 109.5 | ^GDAXI | 9927 | -0.026287 | 0.0006983 | 0 | -327 | 0 | 0 |
| 05.06.2014 | 111.6 | ^GDAXI | 9948 | 0.019586 | 0.0021294 | 0 | -326 | 0 | 0 |
| 06.06.2014 | 111.9 | ^GDAXI | 9987 | 0.00284 | 0.0039489 | 0 | -325 | 0 | 0 |
| 09.06.2014 | 112.4 | ^GDAXI | 10009 | 0.003861 | 0.0021444 | 0 | -324 | 0 | 0 |

`est_task`` ``=`` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``est_task``, ``param_set``)`

[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``)`` ``#> ``# A tibble: 4 × 6`` ``#> ``# Groups: event_id, group, firm_symbol [4]`` ``#> firm_symbol event_id group data request model `` ``#> ``<chr>`` ``<int>`` ``<chr>`` ``<list>`` ``<list>`` ``<list>`` `` ``#> ``1`` VOW.DE 1 VW Group ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`` ``#> ``2`` PAH3.DE 2 VW Group ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`` ``#> ``3`` BMW.DE 3 Other ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`` ``#> ``4`` MBG.DE 4 Other ``<tibble [360 × 11]>`` ``<tibble [1 × 6]>`` ``<MarktMdl>`

`est_task``$``data_tbl``$``model``[[``1``]``]`` ``#> <MarketModel>`` ``#> Inherits from: <ModelBase>`` ``#> Public:`` ``#> abnormal_returns: function (data_tbl) `` ``#> clone: function (deep = FALSE) `` ``#> degenerate_mode: lenient`` ``#> event_id: 1`` ``#> firm_symbol: VOW.DE`` ``#> fit: function (data_tbl) `` ``#> formula: formula`` ``#> hac_lag: NULL`` ``#> initialize: function (use_hac = FALSE, hac_lag = NULL) `` ``#> is_fitted: active binding`` ``#> model: active binding`` ``#> model_name: MarketModel`` ``#> set_formula: function (formula) `` ``#> statistics: active binding`` ``#> use_hac: FALSE`` ``#> Private:`` ``#> .degenerate_handled: FALSE`` ``#> .error: NULL`` ``#> .fitted_model: lm`` ``#> .is_fitted: TRUE`` ``#> .statistics: list`` ``#> add_residuals: function (residuals) `` ``#> calculate_forecast_error_correction: function (sigma, estimation_window_length, estimation_market_returns, `` ``#> calculate_statistics: function (data_tbl) `` ``#> first_order_autocorrelation: function (residuals)`

`est_task`` ``=`` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``est_task``, ``param_set``)`

[`head`](https://rdrr.io/r/utils/head.html)`(``est_task``$``data_tbl``)`` ``#> ``# A tibble: 4 × 8`` ``#> ``# Groups: event_id, group, firm_symbol [4]`` ``#> firm_symbol event_id group data request model ART CART `` ``#> ``<chr>`` ``<int>`` ``<chr>`` ``<list>`` ``<list>`` ``<list>`` ``<list>`` ``<list>`` `` ``#> ``1`` VOW.DE 1 VW Group ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`` ``#> ``2`` PAH3.DE 2 VW Group ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`` ``#> ``3`` BMW.DE 3 Other ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`` ``#> ``4`` MBG.DE 4 Other ``<tibble>`` ``<tibble>`` ``<MarktMdl>`` ``<tibble>`` ``<tibble>`

`es_tt``(``est_task``$``data_tbl``$``ART``[[``1``]``]``)`` ``` #> Warning in `[<-.data.frame`(`*tmp*`, idx, col, value = ``` ``#> structure(list(structure(list(: provided 21 variables to replace 1 variables`

| relative_index | abnormal_returns | ar_t      | ar_t_dist |
|----------------|------------------|-----------|-----------|
| -10            | 0.0028169        | 0.28098   | 248       |
| -9             | 0.0003573        | 0.03564   | 0         |
| -8             | 0.0091997        | 0.91765   | 1         |
| -7             | 0.0222041        | 2.21482   | NA        |
| -6             | -0.0057857       | -0.57712  | 248       |
| -5             | 0.0058288        | 0.58141   | 0         |
| -4             | -0.0049238       | -0.49114  | 1         |
| -3             | 0.0025836        | 0.2577    | NA        |
| -2             | 0.0002132        | 0.02127   | 248       |
| -1             | -0.0003757       | -0.03748  | 0         |
| 0              | -0.0026243       | -0.26177  | 1         |
| 1              | -0.1910336       | -19.05523 | NA        |
| 2              | -0.1418452       | -14.14879 | 248       |
| 3              | 0.0626856        | 6.25277   | 0         |
| 4              | 0.0215244        | 2.14702   | 1         |
| 5              | -0.0576803       | -5.7535   | NA        |
| 6              | -0.0523001       | -5.21684  | 248       |
| 7              | -0.0318594       | -3.17791  | 0         |
| 8              | -0.0074962       | -0.74773  | 1         |
| 9              | 0.0185153        | 1.84686   | NA        |
| 10             | -0.0423404       | -4.22337  | 248       |

`es_tt``(``est_task``$``data_tbl``$``CART``[[``1``]``]``)`` ``` #> Warning in `[<-.data.frame`(`*tmp*`, idx, col, value = ``` ``#> structure(list(structure(list(: provided 21 variables to replace 1 variables`

| relative_index | abnormal_returns | event_window_length | car_window | car | corrected_car | car_t | car_t_dist |
|----|----|----|----|----|----|----|----|
| -10 | 0.0028169 | 1 | \[-10, -10\] | 0.002817 | 0.281 | 0.281 | 248 |
| -9 | 0.0003573 | 2 | \[-10, -9\] | 0.003174 | 0.3166 | 0.2239 | 0.002816886 |
| -8 | 0.0091997 | 3 | \[-10, -8\] | 0.012374 | 1.2343 | 0.7126 | 0.01002526 |
| -7 | 0.0222041 | 4 | \[-10, -7\] | 0.034578 | 3.4491 | 1.7245 | NA |
| -6 | -0.0057857 | 5 | \[-10, -6\] | 0.028792 | 2.872 | 1.2844 | 248 |
| -5 | 0.0058288 | 6 | \[-10, -5\] | 0.034621 | 3.4534 | 1.4098 | 0.002816886 |
| -4 | -0.0049238 | 7 | \[-10, -4\] | 0.029697 | 2.9622 | 1.1196 | 0.01002526 |
| -3 | 0.0025836 | 8 | \[-10, -3\] | 0.032281 | 3.2199 | 1.1384 | NA |
| -2 | 0.0002132 | 9 | \[-10, -2\] | 0.032494 | 3.2412 | 1.0804 | 248 |
| -1 | -0.0003757 | 10 | \[-10, -1\] | 0.032118 | 3.2037 | 1.0131 | 0.002816886 |
| 0 | -0.0026243 | 11 | \[-10, 0\] | 0.029494 | 2.942 | 0.887 | 0.01002526 |
| 1 | -0.1910336 | 12 | \[-10, 1\] | -0.16154 | -16.1133 | -4.6515 | NA |
| 2 | -0.1418452 | 13 | \[-10, 2\] | -0.303385 | -30.2621 | -8.3932 | 248 |
| 3 | 0.0626856 | 14 | \[-10, 3\] | -0.240699 | -24.0093 | -6.4168 | 0.002816886 |
| 4 | 0.0215244 | 15 | \[-10, 4\] | -0.219175 | -21.8623 | -5.6448 | 0.01002526 |
| 5 | -0.0576803 | 16 | \[-10, 5\] | -0.276855 | -27.6158 | -6.9039 | NA |
| 6 | -0.0523001 | 17 | \[-10, 6\] | -0.329155 | -32.8326 | -7.9631 | 248 |
| 7 | -0.0318594 | 18 | \[-10, 7\] | -0.361015 | -36.0105 | -8.4878 | 0.002816886 |
| 8 | -0.0074962 | 19 | \[-10, 8\] | -0.368511 | -36.7583 | -8.4329 | 0.01002526 |
| 9 | 0.0185153 | 20 | \[-10, 9\] | -0.349996 | -34.9114 | -7.8064 | NA |
| 10 | -0.0423404 | 21 | \[-10, 10\] | -0.392336 | -39.1348 | -8.5399 | 248 |

`est_task``$``aar_caar_tbl`` ``#> ``# A tibble: 2 × 4`` ``#> ``# Groups: group [2]`` ``#> group data model CSectT `` ``#> ``<chr>`` ``<list>`` ``<list>`` ``<list>`` `` ``#> ``1`` VW Group ``<tibble [720 × 13]>`` ``<tibble [2 × 3]>`` ``<tibble [21 × 10]>`` ``#> ``2`` Other ``<tibble [720 × 13]>`` ``<tibble [2 × 3]>`` ``<tibble [21 × 10]>`

`es_tt``(``est_task``$``aar_caar_tbl``$``CSectT``[[``1``]``]``)`

| relative_index | aar | n_events | n_valid_events | n_pos | n_neg | aar_t | caar | caar_t | car_window |
|----|----|----|----|----|----|----|----|----|----|
| -10 | 0.004175 | 2 | 2 | 2 | 0 | 3.0734 | 0.004175 | 3.073 | \[-10, -10\] |
| -9 | -0.001221 | 2 | 2 | 1 | 1 | -0.7737 | 0.002954 | 13.431 | \[-10, -9\] |
| -8 | 0.006685 | 2 | 2 | 2 | 0 | 2.6587 | 0.009639 | 3.525 | \[-10, -8\] |
| -7 | 0.015783 | 2 | 2 | 2 | 0 | 2.458 | 0.025422 | 2.777 | \[-10, -7\] |
| -6 | -0.002606 | 2 | 2 | 1 | 1 | -0.8193 | 0.022817 | 3.819 | \[-10, -6\] |
| -5 | 0.004547 | 2 | 2 | 2 | 0 | 3.5468 | 0.027364 | 3.771 | \[-10, -5\] |
| -4 | -0.004164 | 2 | 2 | 0 | 2 | -5.4838 | 0.023199 | 3.57 | \[-10, -4\] |
| -3 | 0.004712 | 2 | 2 | 2 | 0 | 2.2139 | 0.027911 | 6.388 | \[-10, -3\] |
| -2 | 0.007963 | 2 | 2 | 2 | 0 | 1.0275 | 0.035874 | 10.612 | \[-10, -2\] |
| -1 | -0.004826 | 2 | 2 | 0 | 2 | -1.0844 | 0.031048 | 29.015 | \[-10, -1\] |
| 0 | -0.001426 | 2 | 2 | 0 | 2 | -1.1908 | 0.029622 | 231.743 | \[-10, 0\] |
| 1 | -0.191271 | 2 | 2 | 0 | 2 | -804.3893 | -0.16165 | -1470.036 | \[-10, 1\] |
| 2 | -0.145951 | 2 | 2 | 0 | 2 | -35.5468 | -0.307601 | -72.963 | \[-10, 2\] |
| 3 | 0.038745 | 2 | 2 | 2 | 0 | 1.6183 | -0.268856 | -9.549 | \[-10, 3\] |
| 4 | 0.017674 | 2 | 2 | 2 | 0 | 4.59 | -0.251182 | -7.848 | \[-10, 4\] |
| 5 | -0.058071 | 2 | 2 | 0 | 2 | -148.579 | -0.309253 | -9.545 | \[-10, 5\] |
| 6 | -0.051179 | 2 | 2 | 0 | 2 | -45.6437 | -0.360432 | -11.524 | \[-10, 6\] |
| 7 | -0.018909 | 2 | 2 | 0 | 2 | -1.46 | -0.379341 | -20.7 | \[-10, 7\] |
| 8 | -0.006953 | 2 | 2 | 0 | 2 | -12.8002 | -0.386294 | -21.723 | \[-10, 8\] |
| 9 | 0.009911 | 2 | 2 | 2 | 0 | 1.1519 | -0.376382 | -14.264 | \[-10, 9\] |
| 10 | -0.045807 | 2 | 2 | 0 | 2 | -13.2138 | -0.422189 | -14.142 | \[-10, 10\] |

### Visualize the Results

The
[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)
function returns a ggplot2 object; wrapping it in
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
converts it to an interactive widget – hover over data points to inspect
exact values, zoom into the announcement window, or click legend entries
to toggle series.

#### Cumulative Abnormal Return (CAR)

The chart below shows the cumulative abnormal return (CAR) path with
confidence band for the first event in the study (VW, September 2015
announcement).

The CAR is the running sum of abnormal returns from the start of the
event window to each day t. Abnormal returns are the firm’s actual
returns minus what the market model predicted – in other words, the
return in excess of what you would have expected given the index
movement that day. At day 0 (the event date) the cumulative sum resets
conceptually: positive values above the band reflect unexpectedly good
news; negative values below the band reflect unexpectedly bad news.

The shaded region is the confidence band. When the CAR path moves
outside the band the abnormal performance is statistically
distinguishable from zero – the market is responding to the event beyond
chance variation. When the path stays inside the band the evidence of
abnormal performance is not strong enough to rule out noise. For the VW
Dieselgate event you will notice a pronounced drop right around the
announcement window: the CAR falls sharply below the lower bound of the
confidence band, confirming a statistically significant negative market
reaction to the emissions disclosure.

Use the hover tooltip to read off the exact CAR value and
confidence-band limits at any relative day. The zoom and legend-toggle
controls let you focus on the announcement window or isolate individual
series.

`plotly``::`[`ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html)`(`[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)`(``est_task``, type ``=`` ``"car"``)``)`

#### Cumulative Average Abnormal Return (CAAR)

The next chart shows the cumulative average abnormal return (CAAR)
aggregated across all events in the study.

Where the single-event CAR reflects one firm’s reaction, the CAAR
averages abnormal returns across all events at each relative day.
Averaging smooths out firm-specific noise – idiosyncratic movements that
happen to coincide with the event window for a particular firm but are
unrelated to the event itself. When a clean CAAR signal emerges (a
sustained move outside the confidence band) it reflects a systematic
market reaction shared across firms, not just one company’s
idiosyncratic move. A narrow confidence band signals that the individual
event reactions were consistent with each other; a wide band suggests
heterogeneous reactions across the sample.

For the Dieselgate study the CAAR chart summarises the mean market
reaction across all four affected automotive companies, providing a
portfolio-level view of how the scandal reverberated through the sector.

`plotly``::`[`ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html)`(`[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)`(``est_task``, type ``=`` ``"caar"``)``)`

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
