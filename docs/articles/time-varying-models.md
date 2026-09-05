# Time-Varying Beta Models: Rolling Window and DCC-GARCH

## Introduction

The standard market model assumes a constant beta over the estimation
window. In practice, betas can vary over time due to changing firm risk,
leverage, or market conditions. The EventStudy package provides two
time-varying beta models:

- **RollingWindowModel**: Rolling OLS estimation (no additional
  dependencies)
- **DCCGARCHModel**: Dynamic Conditional Correlation GARCH (requires
  `rmgarch`)

## Rolling Window Model

The `RollingWindowModel` estimates alpha and beta using a rolling OLS
window over the estimation period. The parameters from the last window
are used for event-window prediction.

### Basic Usage

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request``)`` `` ``ps`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md)`$``new``(``window_size ``=`` ``60``)`` ``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``ps``)`

### Custom Window Size

The `window_size` parameter controls the width of each rolling window. A
smaller window captures faster parameter changes but is noisier:

`# Shorter window: more responsive but noisier`` ``rw_short`` ``<-`` `[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md)`$``new``(``window_size ``=`` ``30``, min_obs ``=`` ``20``)`` `` ``# Longer window: smoother but slower to adapt`` ``rw_long`` ``<-`` `[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md)`$``new``(``window_size ``=`` ``120``, min_obs ``=`` ``60``)`

### Diagnostics

After fitting, you can inspect the time series of rolling parameters:

`ps`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md)`$``new``(``window_size ``=`` ``60``)`` ``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``ps``)`` `` ``# Access rolling parameters from the fitted model`` ``model_data`` ``<-`` ``task``$``data_tbl``$``data``[[``1``]``]`` ``model_obj`` ``<-`` ``task``$``data_tbl``$``model``[[``1``]``]`` ``stats`` ``<-`` ``model_obj``$``statistics`` `` ``# Rolling betas over the estimation window`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``stats``$``rolling_betas``, type ``=`` ``"l"``,`` `` main ``=`` ``"Rolling Beta over Estimation Window"``,`` `` xlab ``=`` ``"Window Index"``, ylab ``=`` ``"Beta"``)`` `[`abline`](https://rdrr.io/r/graphics/abline.html)`(``h ``=`` ``stats``$``beta``, lty ``=`` ``2``, col ``=`` ``"red"``)`` ``# final beta`

### How it Works

1.  For each position in the estimation window, a rolling OLS regression
    is fit: R\_{firm,t} = \alpha_w + \beta_w R\_{market,t} +
    \varepsilon_t where w indexes windows of size `window_size`.

2.  This produces time series of \alpha_t, \beta_t, and \sigma_t.

3.  The parameters from the **last** rolling window are used to predict
    expected returns in the event window.

4.  Abnormal returns are computed as: AR_t = R\_{firm,t} -
    (\hat{\alpha}\_{last} + \hat{\beta}\_{last} R\_{market,t})

## DCC-GARCH Model

The `DCCGARCHModel` uses bivariate DCC-GARCH to estimate time-varying
conditional correlations and covariances, yielding time-varying betas.

### Requirements

The DCC-GARCH model requires the `rmgarch` and `rugarch` packages:

[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"rmgarch"``)`

### Basic Usage

`ps`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` return_model ``=`` `[`DCCGARCHModel`](https://sipemu.github.io/eventstudy/reference/DCCGARCHModel.md)`$``new``(``)`` ``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``ps``)`

### Custom GARCH Orders

`# GARCH(2,1) with DCC(1,2)`` ``dcc`` ``<-`` `[`DCCGARCHModel`](https://sipemu.github.io/eventstudy/reference/DCCGARCHModel.md)`$``new``(`` `` garch_order ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2``, ``1``)``,`` `` dcc_order ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``)`` ``)`` `` ``ps`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_model ``=`` ``dcc``)`

### How it Works

1.  A bivariate DCC-GARCH model is fitted to the estimation-window
    returns:

    - Each return series gets a univariate GARCH(p,q) model for
      conditional variance
    - The DCC(a,b) layer models the time-varying correlation

2.  The time-varying beta is computed from the conditional covariance
    matrix H_t: \beta_t = \frac{H\_{12,t}}{H\_{22,t}} =
    \frac{Cov(R\_{firm}, R\_{market})\_t}{Var(R\_{market})\_t}

3.  The last \beta_t is used for event-window prediction.

### Convergence Notes

DCC-GARCH models can be sensitive to: - Very short estimation windows
(use at least 200 observations) - Extreme returns or near-constant
series - The package uses
[`purrr::safely()`](https://purrr.tidyverse.org/reference/safely.html)
internally so convergence failures produce a warning rather than an
error

## Comparison

| Feature               | RollingWindowModel | DCCGARCHModel        |
|-----------------------|--------------------|----------------------|
| Dependencies          | None (base R OLS)  | rmgarch, rugarch     |
| Speed                 | Fast               | Slower               |
| Volatility Clustering | No                 | Yes                  |
| Estimation Window     | 60+ obs            | 200+ obs recommended |
| Robustness            | Very robust        | May fail to converge |

For most applications, the `RollingWindowModel` provides a good balance
of flexibility and robustness. The `DCCGARCHModel` is preferred when
time-varying volatility clustering is important.
