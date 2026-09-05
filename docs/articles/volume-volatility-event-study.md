# Volume and Volatility Event Studies

## Introduction

Traditional event studies focus on price-based abnormal returns.
However, corporate events often affect **trading volume** and **return
volatility** even when the net price effect is ambiguous. For example,
an earnings announcement may trigger heavy trading and volatility spikes
without moving the price much in either direction. Volume and volatility
event studies capture these information-content effects that price-based
methods can miss.

The EventStudy package provides two dedicated models:

| Model | Class | Abnormal measure |
|----|----|----|
| Volume | `VolumeModel` | Deviation of (log-)volume from estimation-window mean |
| Volatility | `VolatilityModel` | Ratio of squared returns to estimation-window variance, minus 1 |

Both models plug into the standard pipeline
([`prepare_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)
→
[`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md)
→
[`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md))
and are compatible with all existing test statistics.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`

## Data Requirements

### Volume Event Study

The `VolumeModel` requires a **`firm_volume`** column in the firm data,
in addition to the standard `symbol`, `date`, and `adjusted` columns.
The volume column should contain raw trading volume (number of shares
traded):

`firm_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``"FIRM_A"``, ``300``)``,`` `` date ``=`` `[`format`](https://rdrr.io/r/base/format.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2014-01-01"``)``, by ``=`` ``"day"``, length.out ``=`` ``300``)``,`` `` ``"%d.%m.%Y"``)``,`` `` adjusted ``=`` ``100`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``, ``0.0003``, ``0.015``)``)``,`` `` firm_volume ``=`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``0``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``, mean ``=`` ``1e6``, sd ``=`` ``2e5``)``)`` ``)`

The `firm_volume` column is **preserved through the pipeline**
automatically. The standard `EventStudyTask` passes through any extra
columns present in the firm data.

### Volatility Event Study

The `VolatilityModel` does **not** require extra columns—it uses the
firm returns already computed by
[`prepare_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md).
Abnormal volatility is defined as:

AV\_{i,t} = \frac{R\_{i,t}^2}{\hat{\sigma}\_i^2} - 1

where \hat{\sigma}\_i^2 is the variance of firm returns in the
estimation window. Positive values indicate higher-than-expected
volatility.

## Volume Event Study

### Setup

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` `` ``# Firm data with volume`` ``n`` ``<-`` ``300`` ``firm_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"FIRM_A"``,`` `` date ``=`` `[`format`](https://rdrr.io/r/base/format.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2014-06-01"``)``, by ``=`` ``"day"``,`` `` length.out ``=`` ``n``)``, ``"%d.%m.%Y"``)``,`` `` adjusted ``=`` ``100`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0003``, ``0.015``)``)``,`` `` firm_volume ``=`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``0``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, mean ``=`` ``1e6``, sd ``=`` ``2e5``)``)`` ``)`` `` ``# Index data`` ``index_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"INDEX_1"``,`` `` date ``=`` ``firm_tbl``$``date``,`` `` adjusted ``=`` ``1000`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0002``, ``0.012``)``)`` ``)`` `` ``# Request table`` ``request_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1L``,`` `` firm_symbol ``=`` ``"FIRM_A"``,`` `` index_symbol ``=`` ``"INDEX_1"``,`` `` event_date ``=`` ``firm_tbl``$``date``[``200``]``,`` `` group ``=`` ``"Earnings"``,`` `` event_window_start ``=`` ``-``10L``,`` `` event_window_end ``=`` ``10L``,`` `` shift_estimation_window ``=`` ``-``11L``,`` `` estimation_window_length ``=`` ``150L`` ``)`

### Running the Volume Study

The key difference from a standard event study is passing
`VolumeModel$new()` as the return model. The `study_type` field on the
`ParameterSet` can be set to `"volume"` for appropriate axis labels in
plots.

`task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` `` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_calculation ``=`` `[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md)`$``new``(``)``,`` `` return_model ``=`` `[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)`$``new``(``log_transform ``=`` ``TRUE``)``,`` `` single_event_statistics ``=`` `[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)`$``new``(``)``,`` `` multi_event_statistics ``=`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(``)`` ``)`` ``params``$``study_type`` ``<-`` ``"volume"`` `` ``task`` ``<-`` ``task`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params``)`

### Log Transform

By default, `VolumeModel` log-transforms volume (`log(volume + 1)`)
before computing abnormal volume. This is standard practice because raw
volume is typically right-skewed. To disable the transform:

`vm`` ``<-`` `[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)`$``new``(``log_transform ``=`` ``FALSE``)`

### Interpreting Results

Abnormal volume is the difference between observed (log-)volume and the
estimation-window mean:

AVolume\_{i,t} = \log(V\_{i,t} + 1) - \overline{\log(V_i +
1)}^{\text{est}}

Positive values indicate higher-than-normal trading activity. The test
statistics (AR T-test, CAR T-test, etc.) are applied to these abnormal
volume measures in the same way as for abnormal returns.

`# Single-event results`` ``task``$``get_ar``(``event_id ``=`` ``1``)`` ``task``$``get_car``(``event_id ``=`` ``1``)`` `` ``# Tidy output`` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task``, type ``=`` ``"ar"``)`

### Visualization

[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)`(``task``, type ``=`` ``"ar"``, event_id ``=`` ``1``)`` ``+`` `` `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``title ``=`` ``"Abnormal Volume Around Earnings Announcement"``,`` `` y ``=`` ``"Abnormal Log-Volume"``)`

## Volatility Event Study

### Setup

The volatility study uses the same data structure as a standard event
study—no extra columns are needed.

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`` `` ``n`` ``<-`` ``300`` ``firm_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"FIRM_A"``,`` `` date ``=`` `[`format`](https://rdrr.io/r/base/format.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2014-06-01"``)``, by ``=`` ``"day"``,`` `` length.out ``=`` ``n``)``, ``"%d.%m.%Y"``)``,`` `` adjusted ``=`` ``100`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0003``, ``0.015``)``)`` ``)`` `` ``index_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"INDEX_1"``,`` `` date ``=`` ``firm_tbl``$``date``,`` `` adjusted ``=`` ``1000`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0002``, ``0.012``)``)`` ``)`` `` ``request_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1L``,`` `` firm_symbol ``=`` ``"FIRM_A"``,`` `` index_symbol ``=`` ``"INDEX_1"``,`` `` event_date ``=`` ``firm_tbl``$``date``[``200``]``,`` `` group ``=`` ``"Earnings"``,`` `` event_window_start ``=`` ``-``10L``,`` `` event_window_end ``=`` ``10L``,`` `` shift_estimation_window ``=`` ``-``11L``,`` `` estimation_window_length ``=`` ``150L`` ``)`

### Running the Volatility Study

`task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` `` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_calculation ``=`` `[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md)`$``new``(``)``,`` `` return_model ``=`` `[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md)`$``new``(``)``,`` `` single_event_statistics ``=`` `[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)`$``new``(``)``,`` `` multi_event_statistics ``=`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(``)`` ``)`` ``params``$``study_type`` ``<-`` ``"volatility"`` `` ``task`` ``<-`` ``task`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params``)`

### Interpreting Results

Abnormal volatility is a **variance ratio** minus 1:

AV\_{i,t} = \frac{R\_{i,t}^2}{\hat{\sigma}\_i^2} - 1

- AV = 0: volatility matches estimation-window expectations.
- AV \> 0: volatility is higher than expected (e.g., information
  arrival).
- AV \< 0: volatility is lower than expected (e.g., quiet trading).

Values around event dates are typically large and positive when events
carry new information, regardless of the direction of the price move.

`task``$``get_ar``(``event_id ``=`` ``1``)`` ``task``$``get_car``(``event_id ``=`` ``1``)`` `` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task``, type ``=`` ``"ar"``)`

### Visualization

[`plot_event_study`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)`(``task``, type ``=`` ``"ar"``, event_id ``=`` ``1``)`` ``+`` `` `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``title ``=`` ``"Abnormal Volatility Around Earnings Announcement"``,`` `` y ``=`` ``"Abnormal Volatility (Variance Ratio - 1)"``)`

## Combining Price, Volume, and Volatility

A comprehensive event study often examines all three dimensions. You can
run separate studies on the same data and compare results:

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` `` ``n`` ``<-`` ``300`` ``firm_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"FIRM_A"``,`` `` date ``=`` `[`format`](https://rdrr.io/r/base/format.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2014-06-01"``)``, by ``=`` ``"day"``,`` `` length.out ``=`` ``n``)``, ``"%d.%m.%Y"``)``,`` `` adjusted ``=`` ``100`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0003``, ``0.015``)``)``,`` `` firm_volume ``=`` `[`pmax`](https://rdrr.io/r/base/Extremes.html)`(``0``, `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, mean ``=`` ``1e6``, sd ``=`` ``2e5``)``)`` ``)`` `` ``index_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"INDEX_1"``,`` `` date ``=`` ``firm_tbl``$``date``,`` `` adjusted ``=`` ``1000`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0002``, ``0.012``)``)`` ``)`` `` ``request_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1L``, firm_symbol ``=`` ``"FIRM_A"``, index_symbol ``=`` ``"INDEX_1"``,`` `` event_date ``=`` ``firm_tbl``$``date``[``200``]``, group ``=`` ``"Earnings"``,`` `` event_window_start ``=`` ``-``10L``, event_window_end ``=`` ``10L``,`` `` shift_estimation_window ``=`` ``-``11L``, estimation_window_length ``=`` ``150L`` ``)`` `` ``# --- Price study ---`` ``task_price`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` ``params_price`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_model ``=`` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)``)`` ``task_price`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task_price``, ``params_price``)`` `` ``# --- Volume study ---`` ``task_vol`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` ``params_vol`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_model ``=`` `[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)`$``new``(``)``)`` ``params_vol``$``study_type`` ``<-`` ``"volume"`` ``task_vol`` ``<-`` ``task_vol`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params_vol``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params_vol``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params_vol``)`` `` ``# --- Volatility study ---`` ``task_volat`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` ``params_volat`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_model ``=`` `[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md)`$``new``(``)``)`` ``params_volat``$``study_type`` ``<-`` ``"volatility"`` ``task_volat`` ``<-`` ``task_volat`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params_volat``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params_volat``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params_volat``)`

`# Extract tidy ARs from each study`` ``ar_price`` ``<-`` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task_price``, type ``=`` ``"ar"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``study ``=`` ``"Price (AR)"``)`` ``ar_vol`` ``<-`` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task_vol``, type ``=`` ``"ar"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``study ``=`` ``"Volume (Abnormal Log-Volume)"``)`` ``ar_volat`` ``<-`` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task_volat``, type ``=`` ``"ar"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``study ``=`` ``"Volatility (Variance Ratio - 1)"``)`` `` ``combined`` ``<-`` `[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)`(``ar_price``, ``ar_vol``, ``ar_volat``)`` `` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``combined``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``relative_index``, y ``=`` ``abnormal_returns``)``)`` ``+`` `` `[`geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)`(``yintercept ``=`` ``0``, linetype ``=`` ``"dashed"``, colour ``=`` ``"grey40"``)`` ``+`` `` `[`geom_vline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)`(``xintercept ``=`` ``0``, linetype ``=`` ``"dotted"``, colour ``=`` ``"red"``, alpha ``=`` ``0.6``)`` ``+`` `` `[`geom_col`](https://ggplot2.tidyverse.org/reference/geom_bar.html)`(``fill ``=`` ``"steelblue"``, alpha ``=`` ``0.7``)`` ``+`` `` `[`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)`(``~`` ``study``, ncol ``=`` ``1``, scales ``=`` ``"free_y"``)`` ``+`` `` `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`` `` title ``=`` ``"Price, Volume, and Volatility Event Study"``,`` `` x ``=`` ``"Relative Time to Event"``,`` `` y ``=`` ``"Abnormal Measure"`` `` ``)`` ``+`` `` `[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`

This three-panel view reveals whether the event primarily affects
prices, trading activity, volatility, or some combination.

## Diagnostics

Model diagnostics work the same way as for price-based studies:

`# Volume model diagnostics`` `[`model_diagnostics`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md)`(``task_vol``)`` `[`plot_diagnostics`](https://sipemu.github.io/eventstudy/reference/plot_diagnostics.md)`(``task_vol``, event_id ``=`` ``1``)`` `` ``# Volatility model diagnostics`` `[`model_diagnostics`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md)`(``task_volat``)`` `[`plot_diagnostics`](https://sipemu.github.io/eventstudy/reference/plot_diagnostics.md)`(``task_volat``, event_id ``=`` ``1``)`

The diagnostics include estimation-window residual normality
(Shapiro-Wilk), autocorrelation (Durbin-Watson, Ljung-Box), and model
fit statistics.

## Export

Results can be exported in the same formats as any other study:

[`export_results`](https://sipemu.github.io/eventstudy/reference/export_results.md)`(``task_vol``, ``"volume_results.csv"``)`` `[`export_results`](https://sipemu.github.io/eventstudy/reference/export_results.md)`(``task_vol``, ``"volume_results.xlsx"``)`` `[`export_results`](https://sipemu.github.io/eventstudy/reference/export_results.md)`(``task_volat``, ``"volatility_results.tex"``, which ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"model"``, ``"ar"``)``)`

## References

- Campbell, J. Y. & Hentschel, L. (1992). No news is good news: An
  asymmetric model of changing volatility in stock returns. *Journal of
  Financial Economics*, 31(3), 281–318.
- Beaver, W. H. (1968). The information content of annual earnings
  announcements. *Journal of Accounting Research*, 6, 67–92.
- MacKinlay, A. C. (1997). Event Studies in Economics and Finance.
  *Journal of Economic Literature*, 35(1), 13–39.
