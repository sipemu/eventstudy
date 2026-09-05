# Creating Custom Models

## Introduction

The EventStudy package uses an object-oriented model system based on R6
classes. Every model inherits from `ModelBase`, which defines the
interface that the pipeline expects. This vignette shows how to create
your own custom model and plug it into the event study workflow.

## The ModelBase Interface

All models must implement two methods:

1.  **`fit(data_tbl)`** – Estimate model parameters from the estimation
    window.
2.  **`abnormal_returns(data_tbl)`** – Compute abnormal returns for all
    observations (estimation + event windows).

The `fit()` method should also populate `private$.statistics` with at
least `sigma` and `degree_of_freedom`, which are used by the test
statistics.

## Example: Industry-Adjusted Model

Suppose you want an industry-adjusted model where the expected return
equals the industry index return rather than the broad market index.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``IndustryAdjustedModel`` ``<-`` ``R6``::`[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)`(``"IndustryAdjustedModel"``,`` `` inherit ``=`` ``ModelBase``,`` `` public ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` model_name ``=`` ``"IndustryAdjustedModel"``,`` `` `` fit ``=`` ``function``(``data_tbl``)`` ``{`` `` ``# No estimation needed -- just compute statistics`` `` ``private``$``.is_fitted`` ``<-`` ``TRUE`` `` ``private``$``calculate_statistics``(``data_tbl``)`` `` ``}``,`` `` `` abnormal_returns ``=`` ``function``(``data_tbl``)`` ``{`` `` ``# AR = firm return - industry index return`` `` ``# Assumes data_tbl has an 'industry_return' column`` `` ``data_tbl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``abnormal_returns ``=`` ``firm_returns`` ``-`` ``industry_return``)`` `` ``}`` `` ``)``,`` `` private ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` calculate_statistics ``=`` ``function``(``data_tbl``)`` ``{`` `` ``estimation_tbl`` ``<-`` ``data_tbl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``estimation_window`` ``==`` ``1``)`` `` `` ``residuals`` ``<-`` ``estimation_tbl``$``firm_returns`` ``-`` ``estimation_tbl``$``industry_return`` `` ``private``$``add_residuals``(``residuals``)`` `` ``private``$``first_order_autocorrelation``(``residuals``)`` `` `` ``sigma`` ``<-`` `[`sd`](https://rdrr.io/r/stats/sd.html)`(``residuals``, na.rm ``=`` ``TRUE``)`` `` ``private``$``.statistics``$``sigma`` ``<-`` ``sigma`` `` ``private``$``.statistics``$``degree_of_freedom`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``residuals``)`` ``-`` ``1`` `` `` ``# Forecast error correction`` `` ``event_window_tbl`` ``<-`` ``data_tbl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``event_window`` ``==`` ``1``)`` `` ``private``$``calculate_forecast_error_correction``(`` `` ``sigma``, `[`nrow`](https://rdrr.io/r/base/nrow.html)`(``estimation_tbl``)``,`` `` ``estimation_tbl``$``index_returns``,`` `` ``event_window_tbl``$``index_returns`` `` ``)`` `` ``}`` `` ``)`` ``)`

## Using the Custom Model

Once defined, use your model exactly like any built-in model:

`# Create parameter set with custom model`` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` ``IndustryAdjustedModel``$``new``(``)`` ``)`` `` ``# Run the event study`` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request_data``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``params``)`

## Example: Excess Return Model

A model that simply computes excess returns over the risk-free rate:

`ExcessReturnModel`` ``<-`` ``R6``::`[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)`(``"ExcessReturnModel"``,`` `` inherit ``=`` ``ModelBase``,`` `` public ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` model_name ``=`` ``"ExcessReturnModel"``,`` `` risk_free_rate ``=`` ``0.0001``, ``# daily risk-free rate`` `` `` initialize ``=`` ``function``(``risk_free_rate`` ``=`` ``0.0001``)`` ``{`` `` ``self``$``risk_free_rate`` ``<-`` ``risk_free_rate`` `` ``}``,`` `` `` fit ``=`` ``function``(``data_tbl``)`` ``{`` `` ``private``$``.is_fitted`` ``<-`` ``TRUE`` `` ``estimation_tbl`` ``<-`` ``data_tbl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``estimation_window`` ``==`` ``1``)`` `` ``residuals`` ``<-`` ``estimation_tbl``$``firm_returns`` ``-`` ``self``$``risk_free_rate`` `` ``private``$``.statistics``$``sigma`` ``<-`` `[`sd`](https://rdrr.io/r/stats/sd.html)`(``residuals``, na.rm ``=`` ``TRUE``)`` `` ``private``$``.statistics``$``degree_of_freedom`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``residuals``)`` ``-`` ``1`` `` ``private``$``add_residuals``(``residuals``)`` `` ``private``$``first_order_autocorrelation``(``residuals``)`` `` ``}``,`` `` `` abnormal_returns ``=`` ``function``(``data_tbl``)`` ``{`` `` ``data_tbl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``abnormal_returns ``=`` ``firm_returns`` ``-`` ``self``$``risk_free_rate``)`` `` ``}`` `` ``)`` ``)`

## Using Factor Models

The package includes several built-in factor models that inherit from
`LinearFactorModel`:

- `FamaFrench3FactorModel` – SMB, HML, market excess
- `FamaFrench5FactorModel` – adds RMW, CMA
- `Carhart4FactorModel` – 3-factor + momentum

These require factor data to be provided via the `factor_tbl` argument
in `EventStudyTask`:

`# Factor data with date column matching firm/index data`` ``factor_data`` ``<-`` ``tibble``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` date ``=`` ``dates``,`` `` smb ``=`` ``smb_returns``,`` `` hml ``=`` ``hml_returns``,`` `` risk_free_rate ``=`` ``rf_rate`` ``)`` `` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request_data``,`` `` factor_tbl ``=`` ``factor_data``)`` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md)`$``new``(``)`` ``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``params``)`

## Key Points

- Always inherit from `ModelBase`
- Implement `fit()` and `abnormal_returns()`
- Populate `private$.statistics$sigma` and
  `private$.statistics$degree_of_freedom`
- Use `private$add_residuals()` and
  `private$first_order_autocorrelation()` for diagnostics compatibility
- Set `private$.is_fitted <- TRUE` after successful fitting
- The model is deep-cloned for each event, so you can store
  event-specific state
