# Introducing EventStudy: A Powerful Tool for Event Study Analysis in R

## Introduction

Event Study is a statistical method used to assess the impact of an
event on the value of a firm. This method has been widely adopted in
empirical finance and has been used to investigate the effects of
corporate events like mergers, earnings announcements, and macroeconomic
news on the value of firms.

While Event Study Analysis is a powerful tool, conducting these analyses
in R can often be complicated and time-consuming. Until now. I’m excited
to introduce EventStudy, a comprehensive and flexible R package designed
to streamline the process of conducting Event Study Analyses.

## Why Event Study?

Event Study is designed to offer a modular approach to conducting event
studies in R. It allows you to apply common models, perform diagnostic
tests, and extract results for further analysis. The package is designed
to work seamlessly with your existing R workflow and offers a range of
features that make conducting event studies in R a breeze.

## Key Features

### Features

The `EventStudy` package includes several features that make it a
versatile tool for performing event study analyses:

- **Flexible Models and Diagnostic Tests**: You can apply common models
  and perform diagnostic tests on them. The package is designed to be
  modular and adaptable, which means you can easily extend it with your
  own models and tests.

- **Custom Models**: With EventStudy, you have the ability to apply your
  own market model. This means you can include external factors in your
  model that are specific to your study or industry.

- **Custom Test Statistics**: You can apply your own test statistics for
  abnormal returns (AR), average abnormal returns (AAR), cumulative
  abnormal returns (CAR), and cumulative average abnormal returns
  (CAAR). This gives you full control over how you want to measure the
  impact of the event.

- **Result Extraction**: You can extract the results of the event study
  for further analysis. For example, you might want to perform
  [Cross-Sectional regression
  analysis](https://eventstudy.de/features/cross_sectional_regression.html).
  The package provides convenient functions for extracting confidence
  bands at each level and for each CAR and CAAR window.

- **Parallel Execution**: If you are dealing with a large number of
  events, the package supports parallel execution. This allows you to
  take full advantage of your computer’s processing power to speed up
  the calculations.

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

`#' warnings: false`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyquant`](https://business-science.github.io/tidyquant/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`purrr`](https://purrr.tidyverse.org/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`readr`](https://readr.tidyverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`DT`](https://github.com/rstudio/DT)`)`` `` `[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``index_symbol`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"^GDAXI"``)`` ``firm_symbols`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"VOW.DE"``, ``"PAH3.DE"``, ``"BMW.DE"``, ``"MBG.DE"``)`` `` ``group`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``"VW Group"``, ``2``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``"Other"``, ``2``)``)`` ``request_tbl`` ``<-`` `[`cbind`](https://rdrr.io/r/base/cbind.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``:``4``)``, ``firm_symbols``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``index_symbol``, ``4``)``, `` `` `[`rep`](https://rdrr.io/r/base/rep.html)`(``"18.09.2015"``, ``4``)``, `` `` ``group``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``-``10``, ``4``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``10``, ``4``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``-``11``, ``4``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``250``, ``4``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `` `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(``)`` `` `[`names`](https://rdrr.io/r/base/names.html)`(``request_tbl``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"event_id"``, ``"firm_symbol"``, ``"index_symbol"``, ``"event_date"``, `` `` ``"group"``, ``"event_window_start"``, ``"event_window_end"``, `` `` ``"shift_estimation_window"``, ``"estimation_window_length"``)`` `` ``firm_symbols`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``tidyquant``::`[`tq_get`](https://business-science.github.io/tidyquant/reference/tq_get.html)`(``from ``=`` ``"2014-06-01"``, to ``=`` ``"2015-11-01"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``date ``=`` `[`format`](https://rdrr.io/r/base/format.html)`(``date``, ``"%d.%m.%Y"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``symbol``, ``date``, ``adjusted``)`` ``->`` ``firm_tbl`` `` ``index_symbol`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``tidyquant``::`[`tq_get`](https://business-science.github.io/tidyquant/reference/tq_get.html)`(``from ``=`` ``"2014-06-01"``, to ``=`` ``"2015-11-01"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``date ``=`` `[`format`](https://rdrr.io/r/base/format.html)`(``date``, ``"%d.%m.%Y"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``symbol``, ``date``, ``adjusted``)`` ``->`` ``index_tbl`

Both, the firm data as the index data should have the following
structure:

1.  **symbol**: Contains the symbol of the stock.
2.  **date**: The date of the price information.
3.  **adjusted**: The price of the stock at given date. The price column
    can be parametrized according to your needs when the task is
    defined. The default is `adjusted`.

`DT``::``datatable``(``firm_tbl``)`

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
are applied. For performing an Event Study on multiple events, currently
the Cross-Sectional T Test (AAR and CAAR) is available. More a coming
soon.

`# Define single event test statistics`` ``# Per default AR and CAR T-Tests are applied`` ``single_event_tests`` ``=`` `[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)`$``new``(``)`` `` ``# Per default CSEct T Test is applied (AAR & CAAR)`` ``multiple_event_tests`` ``=`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(``)`

Your Event Study is then defined in a parameter set:

`# Setup parameter set`` ``param_set`` ``=`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_calculation ``=`` ``log_return``, `` `` return_model ``=`` ``market_model``,`` `` single_event_statistics ``=`` ``single_event_tests``,`` `` multi_event_statistics ``=`` ``multiple_event_tests``)`

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

`DT``::``datatable``(``est_task``$``data_tbl``)`

`DT``::``datatable``(``est_task``$``data_tbl``$``data``[[``1``]``]``)`

`DT``::``datatable``(``est_task``$``data_tbl``$``request``[[``1``]``]``)`

The internal data structure is important for you if you plan to develop
your own statistical or econometric model or test statistic.

`est_task`` ``=`` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``est_task``, ``param_set``)`

`est_task``$``data_tbl``$``data``[[``1``]``]`

`est_task`` ``=`` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``est_task``, ``param_set``)`

`est_task``$``data_tbl`

`est_task``$``data_tbl``$``model``[[``1``]``]`

`est_task`` ``=`` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``est_task``, ``param_set``)`

`est_task``$``data_tbl`

`est_task``$``data_tbl``$``ART``[[``1``]``]`

`est_task``$``data_tbl``$``CART``[[``1``]``]`

`est_task``$``aar_caar_tbl`

`est_task``$``aar_caar_tbl``$``CSectT``[[``1``]``]`

## Roadmap

While EventStudy already offers a powerful tool set for conducting event
study analysis in R, development is actively ongoing. The roadmap for
future features includes adding more test statistics, supporting
long-term event study, volatility and volume event study with test
statistics, and intraday event study. Stay tuned for more updates!

## Conclusion

Event Study offers a streamlined and intuitive interface for conducting
event study analysis in R, making it a valuable addition to the toolkit
of any researcher or analyst in finance. Whether you’re assessing the
impact of corporate events on stock prices or investigating the effects
of macroeconomic news, Event Study provides a flexible and efficient
solution.
