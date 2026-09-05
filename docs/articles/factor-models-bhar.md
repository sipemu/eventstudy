# Factor Models, GARCH, and Buy-and-Hold Abnormal Returns

## Introduction

The simple Market Model regresses firm returns on a single market index.
While widely used, it may miss systematic risk factors that explain
return variation. The EventStudy package provides several alternatives:

| Model | Class | Factors |
|----|----|----|
| Fama-French 3-Factor | `FamaFrench3FactorModel` | Market, SMB, HML |
| Fama-French 5-Factor | `FamaFrench5FactorModel` | Market, SMB, HML, RMW, CMA |
| Carhart 4-Factor | `Carhart4FactorModel` | Market, SMB, HML, MOM |
| GARCH(1,1) | `GARCHModel` | Market (time-varying volatility) |
| BHAR | `BHARModel` | Market (compounded returns) |

Factor models reduce estimation-window residual variance, yielding more
powerful tests. GARCH accounts for volatility clustering. BHAR avoids
the bias from summing daily abnormal returns over long horizons.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`tibble`](https://tibble.tidyverse.org/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`

## Factor Data: The `factor_tbl`

Factor models require a **factor table** passed to
`EventStudyTask$new()`. This table must contain a `date` column (same
format as firm/index data) plus the relevant factor columns.

### Required Columns by Model

| Model                | Required columns in `factor_tbl`             |
|----------------------|----------------------------------------------|
| Fama-French 3-Factor | `smb`, `hml`, `risk_free_rate`               |
| Fama-French 5-Factor | `smb`, `hml`, `rmw`, `cma`, `risk_free_rate` |
| Carhart 4-Factor     | `smb`, `hml`, `mom`, `risk_free_rate`        |

The `risk_free_rate` column is special: when present,
[`prepare_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)
automatically computes **excess returns**:

- `excess_return = firm_returns - risk_free_rate`
- `market_excess = index_returns - risk_free_rate`

These are the dependent and independent variables used by the factor
model regressions.

### Sourcing Factor Data

Fama-French factor data is publicly available from Kenneth French’s
website. In practice you might use the `frenchdata` or `tidyquant`
packages:

`# Example: constructing a factor_tbl manually`` ``# In practice, download from Kenneth French's data library`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` ``n`` ``<-`` ``300`` ``dates`` ``<-`` `[`format`](https://rdrr.io/r/base/format.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2014-06-01"``)``, by ``=`` ``"day"``, length.out ``=`` ``n``)``,`` `` ``"%d.%m.%Y"``)`` `` ``factor_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` date ``=`` ``dates``,`` `` smb ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0``, ``0.005``)``, ``# Small Minus Big`` `` hml ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0``, ``0.005``)``, ``# High Minus Low`` `` rmw ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0``, ``0.004``)``, ``# Robust Minus Weak`` `` cma ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0``, ``0.004``)``, ``# Conservative Minus Aggressive`` `` mom ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0``, ``0.006``)``, ``# Momentum`` `` risk_free_rate ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``0.0001``, ``n``)`` ``# Daily risk-free rate`` ``)`

## Fama-French Three-Factor Model

The three-factor model (Fama & French, 1993) explains stock returns
using market risk, size (SMB), and value (HML) factors:

R_i - R_f = \alpha + \beta_m (R_m - R_f) + \beta_s \text{SMB} + \beta_h
\text{HML} + \epsilon

### Running the Study

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` ``n`` ``<-`` ``300`` ``dates`` ``<-`` `[`format`](https://rdrr.io/r/base/format.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2014-06-01"``)``, by ``=`` ``"day"``, length.out ``=`` ``n``)``,`` `` ``"%d.%m.%Y"``)`` `` ``firm_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"FIRM_A"``,`` `` date ``=`` ``dates``,`` `` adjusted ``=`` ``100`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0003``, ``0.015``)``)`` ``)`` `` ``index_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"INDEX_1"``,`` `` date ``=`` ``dates``,`` `` adjusted ``=`` ``1000`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0002``, ``0.012``)``)`` ``)`` `` ``request_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1L``, firm_symbol ``=`` ``"FIRM_A"``, index_symbol ``=`` ``"INDEX_1"``,`` `` event_date ``=`` ``dates``[``200``]``, group ``=`` ``"Test"``,`` `` event_window_start ``=`` ``-``10L``, event_window_end ``=`` ``10L``,`` `` shift_estimation_window ``=`` ``-``11L``, estimation_window_length ``=`` ``150L`` ``)`` `` ``# Pass factor_tbl to the task`` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``,`` `` factor_tbl ``=`` ``factor_tbl``)`` `` ``params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_calculation ``=`` `[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md)`$``new``(``)``,`` `` return_model ``=`` `[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md)`$``new``(``)`` ``)`` `` ``task`` ``<-`` ``task`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params``)`

### Inspecting Factor Loadings

After fitting, the model stores all factor coefficients:

`# Model statistics for event 1`` ``stats`` ``<-`` ``task``$``data_tbl``$``model``[[``1``]``]``$``statistics`` `` ``# Factor loadings`` ``stats``$``alpha`` ``# Intercept (Jensen's alpha)`` ``stats``$``beta`` ``` # Market beta (same as `market_excess`) ``` ``stats``$``smb`` ``# SMB loading`` ``stats``$``hml`` ``# HML loading`` ``stats``$``r2`` ``# R-squared`` ``stats``$``sigma`` ``# Residual standard error`` `` ``# Full coefficient table`` ``task``$``get_model_stats``(``event_id ``=`` ``1``)`

## Fama-French Five-Factor Model

The five-factor model (Fama & French, 2015) adds profitability (RMW) and
investment (CMA) factors:

R_i - R_f = \alpha + \beta_m (R_m - R_f) + \beta_s \text{SMB} + \beta_h
\text{HML} + \beta_r \text{RMW} + \beta_c \text{CMA} + \epsilon

`task_ff5`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``,`` `` factor_tbl ``=`` ``factor_tbl``)`` `` ``params_ff5`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_calculation ``=`` `[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md)`$``new``(``)``,`` `` return_model ``=`` `[`FamaFrench5FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench5FactorModel.md)`$``new``(``)`` ``)`` `` ``task_ff5`` ``<-`` ``task_ff5`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params_ff5``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params_ff5``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params_ff5``)`

## Carhart Four-Factor Model

The Carhart (1997) model adds a momentum factor (MOM) to the
three-factor model:

R_i - R_f = \alpha + \beta_m (R_m - R_f) + \beta_s \text{SMB} + \beta_h
\text{HML} + \beta\_{mom} \text{MOM} + \epsilon

`task_c4`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``,`` `` factor_tbl ``=`` ``factor_tbl``)`` `` ``params_c4`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_calculation ``=`` `[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md)`$``new``(``)``,`` `` return_model ``=`` `[`Carhart4FactorModel`](https://sipemu.github.io/eventstudy/reference/Carhart4FactorModel.md)`$``new``(``)`` ``)`` `` ``task_c4`` ``<-`` ``task_c4`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params_c4``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params_c4``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params_c4``)`

## GARCH(1,1) Model

The GARCH model (Bollerslev, 1986) accounts for **volatility
clustering**—the tendency of large returns to follow large returns. The
mean equation includes the market return as a regressor (like the Market
Model), but the variance is modeled as time-varying:

R\_{i,t} = \mu + \beta R\_{m,t} + \epsilon_t, \quad \epsilon_t \sim N(0,
\sigma_t^2) \sigma_t^2 = \omega + \alpha_1 \epsilon\_{t-1}^2 + \beta_1
\sigma\_{t-1}^2

### Requirements

The GARCH model requires the `rugarch` package:

[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"rugarch"``)`

### Running the Study

`task_garch`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` `` ``params_garch`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`GARCHModel`](https://sipemu.github.io/eventstudy/reference/GARCHModel.md)`$``new``(``)`` ``)`` `` ``task_garch`` ``<-`` ``task_garch`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params_garch``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params_garch``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params_garch``)`

### GARCH-Specific Results

The GARCH model stores conditional volatility from the estimation
window:

`stats`` ``<-`` ``task_garch``$``data_tbl``$``model``[[``1``]``]``$``statistics`` `` ``stats``$``alpha`` ``# Mean equation intercept`` ``stats``$``beta`` ``# Market beta`` ``stats``$``sigma`` ``# Average conditional sigma`` ``stats``$``garch_sigma`` ``# Time series of conditional sigmas`

The time-varying conditional sigma can be particularly useful for
standardized test statistics, as it accounts for periods of high and low
volatility in the estimation window.

### When to Use GARCH

GARCH is most beneficial when:

- The estimation window spans periods of varying market conditions (calm
  and turbulent periods)
- The event itself may be associated with a volatility regime change
- You want to account for volatility clustering in the test statistics

For short estimation windows in stable markets, the simple Market Model
is often sufficient.

## Buy-and-Hold Abnormal Returns (BHAR)

### Motivation

Standard CARs sum daily abnormal returns:

CAR_i = \sum\_{t=1}^{T} AR\_{i,t}

For **long horizons** (months or years), this introduces a compounding
bias. BHAR instead compounds returns, which better reflects the actual
investor experience:

BHAR_i = \prod\_{t=1}^{T} (1 + R\_{i,t}) - \prod\_{t=1}^{T} (1 +
R\_{m,t})

### Running a BHAR Study

`task_bhar`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` `` ``params_bhar`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`BHARModel`](https://sipemu.github.io/eventstudy/reference/BHARModel.md)`$``new``(``)``,`` `` single_event_statistics ``=`` `[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)`$``new``(`` `` tests ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`BHARTTest`](https://sipemu.github.io/eventstudy/reference/BHARTTest.md)`$``new``(``)``)`` `` ``)`` ``)`` `` ``task_bhar`` ``<-`` ``task_bhar`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``params_bhar``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``params_bhar``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``params_bhar``)`

### Interpreting BHAR Results

Unlike the Market Model where abnormal returns are computed per
observation, BHAR computes **running compounded** differences. The
abnormal return at each point represents the cumulative buy-and-hold
difference up to that point:

`# Tidy output`` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task_bhar``, type ``=`` ``"ar"``)`` `` ``# BHAR test statistic`` ``task_bhar``$``data_tbl``$``BHART``[[``1``]``]`

### BHAR vs. CAR: When Does It Matter?

The difference between BHAR and CAR grows with the horizon and the
magnitude of returns:

`# Short window: CAR and BHAR are nearly identical`` ``task_short`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` ``params_short`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_model ``=`` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)``)`` ``task_short`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task_short``)`` `` ``car_short`` ``<-`` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task_short``, type ``=`` ``"car"``)`` ``bhar_short`` ``<-`` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``task_bhar``, type ``=`` ``"ar"``)`

For a 21-day event window (typical), the difference is usually
negligible. For horizons of 6–12 months or longer, BHAR is strongly
preferred.

## Comparing Models

A rigorous event study often compares results across multiple models to
assess robustness:

`models`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` ``"Market Model"`` ``=`` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)``,`` `` ``"FF3"`` ``=`` `[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md)`$``new``(``)``,`` `` ``"FF5"`` ``=`` `[`FamaFrench5FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench5FactorModel.md)`$``new``(``)``,`` `` ``"Carhart"`` ``=`` `[`Carhart4FactorModel`](https://sipemu.github.io/eventstudy/reference/Carhart4FactorModel.md)`$``new``(``)`` ``)`` `` ``results`` ``<-`` ``purrr``::`[`imap_dfr`](https://purrr.tidyverse.org/reference/map_dfr.html)`(``models``, ``function``(``model``, ``name``)`` ``{`` `` ``t`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``,`` `` factor_tbl ``=`` ``factor_tbl``)`` `` ``p`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_calculation ``=`` `[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md)`$``new``(``)``,`` `` return_model ``=`` ``model`` `` ``)`` `` ``t`` ``<-`` ``t`` ``|>`` `` `[`prepare_event_study`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)`(``p``)`` ``|>`` `` `[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md)`(``p``)`` ``|>`` `` `[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)`(``p``)`` `` `` `[`tidy.EventStudyTask`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)`(``t``, type ``=`` ``"car"``)`` ``|>`` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``model ``=`` ``name``)`` ``}``)`` `` ``# Plot CARs across models`` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``results``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``relative_index``, y ``=`` ``car``, color ``=`` ``model``)``)`` ``+`` `` `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(``linewidth ``=`` ``0.8``)`` ``+`` `` `[`geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)`(``yintercept ``=`` ``0``, linetype ``=`` ``"dashed"``, color ``=`` ``"grey40"``)`` ``+`` `` `[`geom_vline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)`(``xintercept ``=`` ``0``, linetype ``=`` ``"dotted"``, color ``=`` ``"red"``, alpha ``=`` ``0.6``)`` ``+`` `` `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`` `` title ``=`` ``"CAR Comparison Across Models"``,`` `` x ``=`` ``"Relative Time to Event"``,`` `` y ``=`` ``"Cumulative Abnormal Return"``,`` `` color ``=`` ``"Model"`` `` ``)`` ``+`` `` `[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`

If the main result holds across all models, you have strong evidence
that it is not an artifact of the return model specification.

## Summary

| Model | Best for | Extra data needed |
|----|----|----|
| Market Model | Short horizons, standard studies | None |
| FF3 | Controlling for size and value | `factor_tbl` with SMB, HML |
| FF5 | Adding profitability and investment | `factor_tbl` with SMB, HML, RMW, CMA |
| Carhart | Adding momentum exposure | `factor_tbl` with SMB, HML, MOM |
| GARCH | Volatile markets, heteroskedastic returns | None (requires `rugarch`) |
| BHAR | Long-horizon studies (6+ months) | None |

## References

- Fama, E. F. & French, K. R. (1993). Common risk factors in the returns
  on stocks and bonds. *Journal of Financial Economics*, 33(1), 3–56.
- Fama, E. F. & French, K. R. (2015). A five-factor asset pricing model.
  *Journal of Financial Economics*, 116(1), 1–22.
- Carhart, M. M. (1997). On persistence in mutual fund performance.
  *Journal of Finance*, 52(1), 57–82.
- Bollerslev, T. (1986). Generalized autoregressive conditional
  heteroskedasticity. *Journal of Econometrics*, 31(3), 307–327.
- Barber, B. M. & Lyon, J. D. (1997). Detecting long-run abnormal stock
  returns: The empirical power and specification of test statistics.
  *Journal of Financial Economics*, 43(3), 341–372.
- MacKinlay, A. C. (1997). Event Studies in Economics and Finance.
  *Journal of Economic Literature*, 35(1), 13–39.
