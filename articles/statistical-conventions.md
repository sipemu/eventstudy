# Statistical Conventions and Formula Audit

## Purpose

Every event-study statistic embeds convention choices – the return type,
the forecast-error correction, the degrees of freedom, the p-value
sidedness, the Patell denominator, and so on. Two implementations of
“the same” test can differ in the last digits (or more) purely because
they made different, equally defensible choices. This vignette
documents, per return model and per test statistic, exactly which
convention EventStudy uses and cites the published source for each. It
is the reference that a golden-value regression test is pinned against:
a documented convention here matches the assumption annotated in
`tests/testthat/test_golden_values.R`.

## Audit methodology

Each model and statistic was audited against its published formula.
Findings are classified with a strict two-bucket policy:

- **Fix (genuine formula error).** If the code computes something no
  published convention supports, it is a bug. It is fixed in the R
  source, the fix is locked with a regression test, and the fix precedes
  the golden pin of that statistic. Such fixes are logged in the “Audit
  log” section below.
- **Document (defensible convention difference).** If the code matches a
  published convention that merely differs from another implementation
  (for example log vs simple returns, df = m - 2 vs m - 1, one- vs
  two-sided p), the behavior is *not* changed – it is documented here as
  the chosen convention with its citation. Behavior on valid inputs is
  frozen for this milestone.

When a discrepancy is ambiguous it is treated as a convention
(documented, not changed), because silently altering a valid-input
result is the higher-risk error.

Tolerance policy for the golden tests: absolute `1e-10` for exact
algebraic identities, absolute `1e-8` as the general default, and
relative `1e-6` for any cross-implementation comparison. Each pinned
assertion states its provenance, the conventions it assumes, and its
tolerance rationale inline.

## Return models

### Market Model (MacKinlay 1997)

The Market Model regresses firm returns on index returns over the
estimation window by OLS and defines the abnormal return as the residual
in the event window.

| Convention | Choice | Note |
|----|----|----|
| Return type | Arithmetic (simple) returns | The pipeline’s return-calculation strategy is orthogonal; the model consumes whatever `firm_returns` / `index_returns` contain. |
| Estimation | OLS: `firm_returns ~ index_returns` | `alpha`, `beta` from [`lm()`](https://rdrr.io/r/stats/lm.html). |
| Abnormal return | `AR = R - (alpha + beta * Rm)` | MacKinlay (1997) eq. (5). |
| Residual sigma | Residual standard error from `summary(lm)` | Uses `df = m - 2` (intercept + slope), `m` = COMPLETE-PAIR estimation observations. |
| Degrees of freedom | `m - 2` | `m` = estimation observations with both `firm_returns` and `index_returns` finite. |
| Minimum observations | `n_valid >= n_params + 1 = 3` | Fewer valid observations route through the degenerate-input contract (NA + one warning, or error in strict mode) rather than fitting an unstable/undefined `sigma` (needs `df >= 1`). |
| Short-window advisory | One plain warning when `2 < n_valid < 30` | Advisory only (not a degenerate/contract condition) – the model still fits; 30 is the same recommended minimum as [`validate_task()`](https://sipemu.github.io/eventstudy/reference/validate_task.md)’s default. |
| `statistics$n_params` | `2` (intercept + slope) | Consumed by `PatellZTest`’s `Q_i` adjustment (see below). |
| AR t-statistic | `AR / sigma` | Per-day, constant-sigma approximation. |
| CAR t-statistic | `CAR / (sqrt(L) * sigma)` | `L`-day cumulative, constant-sigma approximation, MacKinlay (1997). |
| Forecast-error correction | `sigma * sqrt(1 + 1/m + (Rm_evt - mean(Rm_est))^2 / SS_market)` | Patell/MacKinlay prediction-error variance; `mean(Rm_est)` and `SS_market` (the estimation-window sum of squares of the index) are computed over COMPLETE PAIRS ONLY, matching exactly the rows [`lm()`](https://rdrr.io/r/stats/lm.html) used (2026-09-24 re-evaluation item A6: previously included incomplete-pair rows, biasing the mean/SS). Falls back to the constant-mean form `sigma * sqrt(1 + 1/m)` when the index is constant. |
| p-value sidedness | Two-sided by default | `confidence_type = "two-sided"`. |

**Audit result: verified against MacKinlay (1997); two silent-wrong
defects fixed 2026-09-24 (items A6).** The core OLS/AR/CAR formulas
match MacKinlay. The 2026-09-24 re-evaluation found the
insufficient-observations guard used a flat `n_valid < 2` threshold (too
permissive for a 2-parameter OLS fit to be meaningful) and the
forecast-error correction’s `mean(Rm_est)`/`SS_market` included
estimation rows where `firm_returns` was NA but `index_returns` was not
– rows [`lm()`](https://rdrr.io/r/stats/lm.html) silently drops via
[`na.omit()`](https://rdrr.io/r/stats/na.fail.html) but which the
correction formula still summed over, biasing the correction away from
the fitted model’s own estimation sample. Both are fixed; pinned in
`test_golden_values.R` and `test_reeval_statistical_fixes.R` against an
independent closed-form [`lm()`](https://rdrr.io/r/stats/lm.html)
derivation restricted to complete pairs, at absolute tolerance
`1e-10`/`1e-12`.

*Citation:* MacKinlay, A. C. (1997). Event Studies in Economics and
Finance. *Journal of Economic Literature*, 35(1), 13–39.

### Market Adjusted Model

The Market Adjusted (index) model treats the market index itself as the
expected return: it fixes `alpha = 0` and `beta = 1` rather than
estimating them, so the abnormal return is simply the firm return minus
the index return. It is the restricted market model of MacKinlay (1997)
sec. 4.4.2.

| Convention | Choice | Note |
|----|----|----|
| Return type | Whatever `firm_returns` / `index_returns` contain | Return-calculation strategy is orthogonal. |
| Estimation | None – `alpha = 0`, `beta = 1` fixed | No OLS parameters estimated. |
| Abnormal return | `AR = R - Rm` | Firm return minus index return. |
| Residual sigma | `sd(firm - index)` over the estimation window | Sample standard deviation of the market-adjusted residual series. |
| Degrees of freedom | `m - 1` | One parameter consumed by the residual mean in [`sd()`](https://rdrr.io/r/stats/sd.html); `m` = estimation observations. |
| `statistics$n_params` | `0` | Nothing is estimated (alpha/beta are FIXED at 0/1, not fitted). |
| Forecast-error correction | `forecast_error_corrected_sigma == sigma` (correction factor exactly 1) | No regression, no leverage term, and – since nothing is estimated – no prediction-error inflation either (2026-09-24 re-evaluation item A8). |
| p-value sidedness | Two-sided by default | – |

**Audit result: one silent-wrong defect fixed 2026-09-24 (item A8).**
The subtraction `AR = R - Rm` matches the restricted market model. The
2026-09-24 re-evaluation found the forecast-error correction wrongly
reused the constant-MEAN formula `sigma * sqrt(1 + 1/m)` – which is only
valid when ONE parameter (a mean) is estimated from the estimation
window, as in the Comparison Period Mean Adjusted Model below.
MarketAdjustedModel estimates NOTHING (alpha/beta are fixed, not
fitted), so its correction factor is exactly 1:
`forecast_error_corrected_sigma == sigma`. Re-pinned in
`test_golden_values.R` and locked in `test_reeval_statistical_fixes.R`
at absolute tolerance `1e-10`/`1e-12`.

*Citation:* MacKinlay, A. C. (1997). Event Studies in Economics and
Finance. *Journal of Economic Literature*, 35(1), 13–39.

### Comparison Period Mean Adjusted Model

The Comparison Period Mean (constant-mean-return) model uses the firm’s
own average return over the estimation window as the expected return. It
is the constant-mean-return model of Brown & Warner (1985), also
described in MacKinlay (1997) sec. 4.4.1.

| Convention | Choice | Note |
|----|----|----|
| Return type | Whatever `firm_returns` contains | Index returns are unused by this model. |
| Estimation | `mean(firm_returns)` over the estimation window | One parameter (the mean) estimated. |
| Abnormal return | `AR = R - mean(R_est)` | Firm return minus estimation-window mean. |
| Residual sigma | `sd(firm - mean)` over the estimation window | Sample standard deviation of the demeaned residuals. |
| Degrees of freedom | `m - 1` | The mean consumes one degree of freedom. |
| `statistics$n_params` | `1` | One parameter estimated (the comparison-period mean). |
| Forecast-error correction | `sigma * sqrt(1 + 1/m)` | Constant-mean form. Uses the finite value count (NA rows do not inflate `m`). Unlike MarketAdjustedModel (A8), this model DOES estimate one parameter, so the constant-mean correction is the correct form here. |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change.** AR, sigma, df, and FEC match the
constant-mean-return model. Pinned in `test_golden_values.R` against a
closed-form subtraction against the fixed estimation mean at absolute
tolerance `1e-10`.

*Citation:* Brown, S. J., & Warner, J. B. (1985). Using daily stock
returns: The case of event studies. *Journal of Financial Economics*,
14(1), 3–31. MacKinlay (1997).

### Fama-French Three-Factor Model

Regresses firm excess returns on the three Fama-French factors – market
excess (`market_excess`), size (`smb`), and value (`hml`) – by OLS over
the estimation window; the abnormal return is the residual in the event
window. All factor models share the `LinearFactorModel` OLS core.

| Convention | Choice | Note |
|----|----|----|
| Return type | Excess returns (`excess_return` = firm minus risk-free) | Regressed on `market_excess + smb + hml`. |
| Estimation | OLS via `lm(excess_return ~ market_excess + smb + hml)` | `k = 3` factors plus intercept. |
| Abnormal return | `AR = excess_return - predicted` | `predicted` from the fitted OLS coefficients. |
| Residual sigma | Residual standard error from `summary(lm)` | – |
| Degrees of freedom | `m - (k + 1) = m - 4` | Intercept plus three factor slopes. |
| Minimum observations | `n_valid >= n_params + 1 = 5` | `n_params = length(all.vars(formula)) = 4` (response + 3 factors, i.e. intercept + 3 slopes); fewer valid observations route through the degenerate-input contract (2026-09-24 item A6). |
| Short-window advisory | One plain warning when `4 < n_valid < 30` | Same advisory convention as the Market Model. |
| `statistics$n_params` | `4` | Consumed by `PatellZTest`’s `Q_i` adjustment. |
| Forecast-error correction | `sigma * sqrt(1 + h_t)`, `h_t = x_t' (X'X)^{-1} x_t` | Full multi-factor hat-value (leverage) form using the estimation design matrix. |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change (guard strengthened 2026-09-24, item
A6).** The OLS fit and hat-value FEC match Fama-French (1993). Pinned in
`test_golden_values.R` against an independent
[`lm()`](https://rdrr.io/r/stats/lm.html)/[`predict()`](https://rdrr.io/r/stats/predict.html)
derivation at absolute tolerance `1e-10`.

*Citation:* Fama, E. F., & French, K. R. (1993). Common risk factors in
the returns on stocks and bonds. *Journal of Financial Economics*,
33(1), 3–56.

### Fama-French Five-Factor Model

Extends the three-factor model with profitability (`rmw`) and investment
(`cma`) factors – Fama & French (2015). Same OLS core, five factors.

| Convention | Choice | Note |
|----|----|----|
| Return type | Excess returns (`excess_return`) | Regressed on `market_excess + smb + hml + rmw + cma`. |
| Estimation | OLS on five factors plus intercept | `k = 5`. |
| Abnormal return | `AR = excess_return - predicted` | – |
| Residual sigma | Residual standard error | – |
| Degrees of freedom | `m - (k + 1) = m - 6` | – |
| Minimum observations | `n_valid >= n_params + 1 = 7` | `n_params = 6` (intercept + 5 factors); 2026-09-24 item A6. |
| `statistics$n_params` | `6` | Consumed by `PatellZTest`’s `Q_i` adjustment. |
| Forecast-error correction | `sigma * sqrt(1 + h_t)` | Multi-factor hat-value form. |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change (guard strengthened 2026-09-24, item
A6).** Pinned in `test_golden_values.R` against an independent
[`lm()`](https://rdrr.io/r/stats/lm.html)/[`predict()`](https://rdrr.io/r/stats/predict.html)
derivation at absolute tolerance `1e-10`.

*Citation:* Fama, E. F., & French, K. R. (2015). A five-factor asset
pricing model. *Journal of Financial Economics*, 116(1), 1–22.

### Carhart Four-Factor Model

Extends the three-factor model with a momentum factor (`mom`) – Carhart
(1997). Same OLS core, four factors.

| Convention | Choice | Note |
|----|----|----|
| Return type | Excess returns (`excess_return`) | Regressed on `market_excess + smb + hml + mom`. |
| Estimation | OLS on four factors plus intercept | `k = 4`. |
| Abnormal return | `AR = excess_return - predicted` | – |
| Residual sigma | Residual standard error | – |
| Degrees of freedom | `m - (k + 1) = m - 5` | – |
| Minimum observations | `n_valid >= n_params + 1 = 6` | `n_params = 5` (intercept + 4 factors); 2026-09-24 item A6. |
| `statistics$n_params` | `5` | Consumed by `PatellZTest`’s `Q_i` adjustment. |
| Forecast-error correction | `sigma * sqrt(1 + h_t)` | Multi-factor hat-value form. |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change (guard strengthened 2026-09-24, item
A6).** Pinned in `test_golden_values.R` against an independent
[`lm()`](https://rdrr.io/r/stats/lm.html)/[`predict()`](https://rdrr.io/r/stats/predict.html)
derivation at absolute tolerance `1e-10`.

*Citation:* Carhart, M. M. (1997). On persistence in mutual fund
performance. *Journal of Finance*, 52(1), 57–82.

### Custom Model

The Custom Model inherits the Market Model’s OLS estimation but defines
the abnormal return as `firm_returns - predict(user_model)`, plus an
optional event-date adjustment `loss_market_cap` added only on the event
date. There is no single published closed-form here – the convention is
deliberately “whatever the supplied model predicts.”

| Convention | Choice | Note |
|----|----|----|
| Return type | Whatever the supplied model consumes | User-defined. |
| Estimation | Inherited from the Market Model (OLS) | The fitted object is user-supplied via the pipeline. |
| Abnormal return | `AR = R - predict(model)` (+`loss_market_cap` on the event date) | The event-date term lets a user inject a known shock. |
| Residual sigma / df | Inherited from the underlying OLS fit (`df = m - 2`) | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: documented convention (no closed-form pin possible).**
With `loss_market_cap = 0` the Custom Model reduces exactly to the
Market Model; that identity is what `test_golden_values.R` pins
(absolute `1e-10`). A general user-supplied prediction has no published
reference value to pin against.

*Citation:* Convention is user-defined; the reduction case cites
MacKinlay (1997).

### BHAR Model (Barber-Lyon 1997)

The Buy-and-Hold Abnormal Return (BHAR) model is for long-horizon
studies where compounding matters. Instead of summing daily abnormal
returns it compounds firm and benchmark returns separately over the
event window and takes the difference.

| Convention | Choice | Note |
|----|----|----|
| Return type | Arithmetic returns compounded multiplicatively | `cumprod(1 + R)`. |
| Estimation | None estimated for the AR itself; the benchmark is the index | sigma is estimated from the estimation window for the t-test. |
| Abnormal return | `BHAR = cumprod(1 + firm) - cumprod(1 + index)` | Barber & Lyon (1997) buy-and-hold definition; compounded within each event window separately. |
| Residual sigma | `sd(firm - index)` over the estimation window | Sample SD of the one-period market-adjusted residual series. |
| Degrees of freedom | `m - 1` | Finite-pair count via `.finite_residual_df()` (NA rows do not inflate `m`). |
| BHAR standard error | `sigma * sqrt(n)` | `n` is the day index within the event window – the random-walk sqrt-of-horizon scaling of Lyon, Barber & Tsai (1999). |
| BHAR t-statistic | `BHAR / (sigma * sqrt(n))` | Returns NA when `sigma == 0` (degenerate guard), never Inf/NaN. |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change.** The compounded BHAR, the
`sigma * sqrt(n)` scaling, and the constant-mean forecast-error
correction match Barber-Lyon (1997) / Lyon-Barber-Tsai (1999). Pinned in
`test_golden_values.R` against an independent closed-form compounding at
absolute tolerance `1e-10`.

*Citation:* Barber, B. M., & Lyon, J. D. (1997). Detecting long-run
abnormal stock returns: The empirical power and specification of test
statistics. *Journal of Financial Economics*, 43(3), 341–372. Lyon, J.
D., Barber, B. M., & Tsai, C.-L. (1999). Improved methods for tests of
long-run abnormal stock returns. *Journal of Finance*, 54(1), 165–201.

### Volume Model

The Volume model runs an event study on trading volume rather than
returns. Expected volume is the estimation-window mean of (optionally
log-transformed) volume, and abnormal volume is the deviation from that
mean. It writes abnormal volume into the `abnormal_returns` column so
the standard test statistics apply.

| Convention | Choice | Note |
|----|----|----|
| Measure | `log(firm_volume + 1)` by default (`log_transform = TRUE`) | The `+1` avoids `log(0)`; set `log_transform = FALSE` for raw volume. |
| Estimation | `mean(log(volume + 1))` over the estimation window | One parameter (the mean). |
| Abnormal measure | `AR = log(volume + 1) - expected` | Deviation from the estimation-window log-mean. |
| Residual sigma | `sd(residuals)` over the estimation window | – |
| Degrees of freedom | `m - 1` | Finite value count (NA/non-finite rows excluded). |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change.** The log-mean expected volume, the
deviation-from-mean abnormal volume, and `df = m - 1` match the standard
abnormal-trading-volume convention. Pinned in `test_golden_values.R`
against a direct log-mean subtraction at absolute tolerance `1e-10`.

*Citation:* Convention follows the abnormal-trading-volume literature
(e.g. Campbell & Wasley 1996, log relative volume); the mean-adjusted
form is the volume analogue of the constant-mean-return model of Brown &
Warner (1985).

### Volatility Model

The Volatility model runs an event study on return variance. Expected
variance is the estimation-window sample variance, and abnormal
volatility is the ratio of each event-window squared return to that
variance, minus one (so zero means “as expected”). It writes the measure
into `abnormal_returns` for compatibility.

| Convention | Choice | Note |
|----|----|----|
| Measure | Squared return relative to estimation variance | – |
| Estimation | `est_var = var(estimation firm_returns)` | Sample variance, `df = m - 1`. |
| Abnormal measure | `AR = firm_returns^2 / est_var - 1` | Zero when the squared return equals the expected variance. |
| Residual sigma | `sd(residuals)` of the ratio series over the estimation window | – |
| Degrees of freedom | `m - 1` | Finite value count. |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change.** The ratio-to-expected-variance
abnormal measure and `df = m - 1` match the abnormal-volatility
convention. Pinned in `test_golden_values.R` against a direct ratio
computation at absolute tolerance `1e-10`.

*Citation:* The squared-return / expected-variance ratio follows the
event-induced-variance literature (Beaver 1968; Patell & Wolfson 1979),
adapted to the constant-variance benchmark of the estimation window.

### Rolling-Window Model

The Rolling-Window model captures parameter instability by estimating
the market model over a rolling OLS window and using the LAST window’s
parameters to predict the event window. This yields a time-varying beta
without a full GARCH model.

| Convention | Choice | Note |
|----|----|----|
| Estimation | Rolling OLS `firm ~ index` over windows of `window_size` (default 60), requiring `min_obs` (default 30) finite pairs | Last window’s `alpha`, `beta` used for prediction. |
| Effective window | `ws = min(window_size, n_est)` | When `n_est <= window_size` there is a single window equal to the full sample; `n_est` is the estimation window’s ROW span (not a valid-pair count). |
| Within-window pairs | Each `ws`-row window restricts `x_bar`/`y_bar`/`ss_xx`/`ss_xy`/residuals/sigma to COMPLETE PAIRS (both firm and index finite) | 2026-09-24 re-evaluation item A1: previously `x_bar`/`ss_xx` were computed from ALL index values in the window, including rows where the firm return was NA – if those NA-firm rows are not missing-at-random in the index (e.g. an index outlier co-occurring with a missing firm observation), this silently biases beta away from what [`lm()`](https://rdrr.io/r/stats/lm.html) on the complete-pair subset would produce. |
| Abnormal return | `AR = firm - (alpha_last + beta_last * index)` | Uses the most recent (last) rolling parameters. |
| Residual sigma | Last-window residual SE, `denom = max(n_complete_last - 2, 1)` | `n_complete_last` = complete pairs in the LAST window (not a freshly recomputed valid-count that could disagree with the actual last-window row span). |
| Degrees of freedom | `max(n_complete_last - 2, 1)` | `fit()` and [`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md) share the SAME complete-pair count (stored in `private$.rolling_params$n_completes`), so the reported df always matches the sigma that was actually estimated. |
| `statistics$n_params` | `2` (alpha, beta) | Consumed by `PatellZTest`’s `Q_i` adjustment. |
| p-value sidedness | Two-sided by default | – |

**Audit result: one silent-wrong defect fixed 2026-09-24 (item A1).**
The rolling OLS, last-window prediction, and `df = n_complete - 2` are
internally consistent once restricted to complete pairs. Pinned in
`test_golden_values.R` on a 30-observation fixture where
`ws = min(60, 30) = 30` collapses to a single closed-form OLS, and in
`test_reeval_statistical_fixes.R` on a 150-day window with firm-return
NAs co-occurring with index outliers every 3rd day (beta, alpha, sigma,
and df all match [`lm()`](https://rdrr.io/r/stats/lm.html) on the
complete-pair subset within `1e-10`).

*Citation:* Rolling-window / time-varying market-model estimation is
standard practice for capturing beta instability (e.g. Fama & MacBeth
1973 rolling regressions); the underlying market model is MacKinlay
(1997).

### GARCH Model

The GARCH model fits a GARCH(1,1) mean-variance model to firm returns
with the index return as an external regressor in the mean equation (via
the optional **rugarch** package). The abnormal return is the
mean-equation residual.

| Convention | Choice | Note |
|----|----|----|
| Backend | [`rugarch::ugarchfit`](https://rdrr.io/pkg/rugarch/man/ugarchfit-methods.html), `sGARCH`, `garchOrder` default `c(1, 1)`, normal errors | Optional dependency, guarded by `requireNamespace`. |
| Mean equation | `firm = mu + mxreg1 * index + e_t` | Index return as an external regressor. |
| Abnormal return | `AR = firm - (mu + mxreg1 * index)` | The fitted mean-equation residual. |
| Residual sigma | Mean conditional sigma from the GARCH fit | Falls back to `sd(residuals)` if non-finite. |
| Degrees of freedom | `max(length(cond_sigma) - length(coefs), 1)` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change (identity-pinned).** Because rugarch
is an optional dependency and its fitted coefficients vary across
package and solver versions, pinning a raw fitted constant would be
fragile (threat T-26-02). The documented golden-value choice is
therefore to pin the STABLE ALGEBRAIC IDENTITY
`AR = firm - (mu + mxreg1 * index)`, which holds for any fitted
coefficients, in `test_golden_values.R` guarded by
`skip_if_not_installed("rugarch")` – locking the AR formula without a
version-fragile number.

*Citation:* Engle, R. F. (1982). Autoregressive conditional
heteroscedasticity with estimates of the variance of United Kingdom
inflation. *Econometrica*, 50(4), 987–1007. Bollerslev, T. (1986).
Generalized autoregressive conditional heteroskedasticity. *Journal of
Econometrics*, 31(3), 307–327.

### DCC-GARCH Model

The DCC-GARCH model (via the optional **rmgarch** package) fits a
bivariate Dynamic Conditional Correlation GARCH to firm and market
returns, yielding a time-varying beta
`beta_t = Cov(firm, mkt)_t / Var(mkt)_t`. The last conditional beta
predicts the event window.

| Convention | Choice | Note |
|----|----|----|
| Backend | [`rmgarch::dccfit`](https://rdrr.io/pkg/rmgarch/man/dccfit-methods.html), univariate `sGARCH(1,1)`, DCC order default `c(1, 1)`, multivariate normal | Optional dependency, guarded by `requireNamespace`. |
| Time-varying beta | `beta_t = H[firm, mkt]_t / H[mkt, mkt]_t` | From the conditional covariance array; non-finite / zero-variance entries yield NA and fall back to the last valid beta. |
| Intercept | `alpha_last = mean(firm - beta_last * index)` over the estimation window | Mean-equation intercept implied by the last beta. |
| Abnormal return | `AR = firm - (alpha_last + beta_last * index)` | Uses the last conditional beta. |
| Residual sigma | `mean(conditional firm sigma)` | – |
| Degrees of freedom | `max(n_t - 4, 1)` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change (identity-pinned).** As with GARCH,
rmgarch is optional and non-deterministic across versions, so the
documented golden-value choice is to pin the stable identity
`AR = firm - (alpha_last + beta_last * index)` in `test_golden_values.R`
guarded by `skip_if_not_installed("rmgarch")` / `("rugarch")`, rather
than a fragile fitted constant.

*Citation:* Bollerslev, T. (1990). Modelling the coherence in short-run
nominal exchange rates: A multivariate generalized ARCH model. *Review
of Economics and Statistics*, 72(3), 498–505. Engle, R. F. (2002).
Dynamic conditional correlation. *Journal of Business & Economic
Statistics*, 20(3), 339–350.

## Test statistics

### Multi-event all-NA exclusion (2026-09-24 re-evaluation, item A2)

Every CAR-based multi-event statistic (`CSectTTest`, `PatellZTest`,
`SignTest`, `GeneralizedSignTest`, `BMPTest`, `KolariPynnonenTest`)
shares one exclusion rule, implemented once in `R/contract.R`
(`.exclude_all_na_events()`) and applied centrally by
[`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)
before any configured statistic runs (so N statistics in one group
produce AT MOST ONE warning, not N), and defensively again at the top of
each [`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
for direct/standalone calls: an event with **zero finite event-window
abnormal returns** is EXCLUDED from the group entirely – not
NA-propagated and coalesced to 0 – because cumulating a fully-degenerate
event’s abnormal returns as 0 would silently understate the true
cross-sectional dispersion (an entirely different failure mode from a
normal 0 abnormal return). Excluded events whose model already emitted
the one contract warning guaranteed by CONTRACT-04 (an unfitted
`ModelBase` instance) are dropped SILENTLY; any other excluded event is
reported once, naming the `event_id`(s) (strict mode errors instead).
This exclusion leaves the pre-existing STATS-03 convention for a PARTIAL
gap unchanged: an event with at least one finite event-window abnormal
return, but a missing value on some individual day, still contributes 0
to THAT EVENT’s own CAR on the missing day(s) via
`coalesce(abnormal_returns, 0)`.

### AR / CAR t-tests (ARTTest, CARTTest)

Documented above with the Market Model, since the tracer anchors them
together. The AR t-test divides each event-day abnormal return by the
model sigma; the CAR t-test divides the cumulative abnormal return by
`sqrt(L) * sigma`. Both are distributed as Student’s t with the model’s
residual degrees of freedom and are two-sided by default. Every return
model also computes `forecast_error_corrected_sigma_car` (the FEC’s
per-event covariance/hat-value term), but `ARTTest`/`CARTTest`
INTENTIONALLY do not use it – they keep the simpler constant-sigma
approximation (item A11, documented convention, no formula change);
`forecast_error_corrected_sigma_car` exists for consumers that need the
full prediction-error covariance structure. *Citation:* MacKinlay
(1997).

### BHAR t-test (BHARTTest)

The single-event buy-and-hold abnormal-return t-test. Documented above
with the BHAR Model, since the model and its t-test share the same
compounded definition and `sigma * sqrt(n)` scaling. In its general form
the statistic is `bhar_t = BHAR / (sigma * sqrt(n))`, where
`BHAR = cumprod(1 + firm) - cumprod(1 + index)` over the event window,
`sigma = sd(firm - index)` over the estimation window, and `n` is the
day index within the event window. It returns NA (never Inf/NaN) when
`sigma == 0`. Two-sided by default. *Citation:* Barber & Lyon (1997);
Lyon, Barber & Tsai (1999).

### Cross-Sectional t-test (CSectTTest)

The cross-sectional t-test evaluates, at each relative event day,
whether the average abnormal return across events is zero, using the
cross-event dispersion of abnormal returns as the standard error. It is
the workhorse parametric test of Brown & Warner (1985).

| Convention | Choice | Note |
|----|----|----|
| Point statistic | `aar_t = sqrt(N) * aar / sd(AR)` | `aar` = mean AR across events; `sd(AR)` = cross-event sample SD (denominator `N - 1`). |
| Cumulative statistic | `caar_t = sqrt(N) * caar / sd(CAR)` | `caar` = mean of per-event cumulative ARs; `sd(CAR)` = cross-event sample SD of the per-event CARs. |
| Sample size | `N` = number of valid (non-NA) events at that day | NA abnormal returns are excluded from both the mean and the SD. |
| Distribution | Student’s t with `N - 1` degrees of freedom | – |
| Standardization | None – raw abnormal returns | Unlike Patell/BMP, ARs are not divided by a per-event sigma; the cross-event SD is the only scale. |
| Positive count | `n_pos = sum(AR > 0)` – STRICT inequality | 2026-09-24 item A9: an AR exactly 0 counts as NON-positive (`n_neg = sum(AR <= 0)`), matching the `SignTest` convention. |
| All-NA event exclusion | An event with ZERO finite event-window ARs is excluded from every multi-event statistic (once per group, one contract warning/error) | 2026-09-24 item A2; see “Multi-event all-NA exclusion” below. A PARTIAL gap (one missing AR mid-window in an otherwise-valid event) still contributes 0 to that event’s own CAR (STATS-03, unchanged). |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified against Brown & Warner (1985); two silent-wrong
defects fixed 2026-09-24 (items A2, A9).** The core
`sqrt(n_valid_events) * aar / sd(AR)` / `caar / sd(CAR)` construction
matches Brown & Warner. The 2026-09-24 re-evaluation found `n_pos` used
`>= 0` (so a zero AR silently counted as “positive”) and that a
fully-degenerate event (all event-window ARs NA, e.g. an unfitted model)
was not excluded from the cumulative CAR-based statistics – its
`coalesce(AR, 0)` silently understated cross-event dispersion instead of
being excluded. Both are fixed; pinned in `test_golden_values.R` and
`test_reeval_statistical_fixes.R` against an independent closed-form
cross-sectional computation at absolute tolerance `1e-10`/`1e-12`.

*Citation:* Brown, S. J., & Warner, J. B. (1985). Using daily stock
returns: The case of event studies. *Journal of Financial Economics*,
14(1), 3–31.

### Patell Z-test (PatellZTest)

The Patell (standardized-residual) test standardizes each event’s
abnormal return by its own forecast-error-corrected standard deviation
before aggregating, which accounts for the fact that event-window
prediction errors have larger variance than estimation-window residuals.
The aggregate is approximately standard normal.

| Convention | Choice | Note |
|----|----|----|
| Standardization | `SAR = AR / forecast-error-corrected sigma` | Uses the per-event, per-day FEC sigma, not the raw model sigma (this is the defining feature vs. BMP). |
| Patell denominator | `Q_total = sqrt(sum_i Q_i)`, `Q_i = (m - k) / (m - k - 2)` | `m` = number of FINITE estimation-window abnormal returns for event `i` (2026-09-24 item A6: NOT the raw row count – NA rows in the estimation window must not inflate `m`). `k` = event `i`’s model’s `statistics$n_params` (MarketModel 2, FF3 4, Carhart4 5, FF5 6, CPMA 1, MarketAdjusted 0), read explicitly rather than derived as `length(resid) - df` (the pre-2026-09-24 derivation silently produced the WRONG `k` whenever `df` already reflected an unrelated correction, e.g. HAC). `Q_i` inflates the variance of the standardized sum for the estimation-error in each sigma. |
| Q_i fallback | `Q_i = 1` when `m <= k + 2` | Insufficient observations for the adjustment; avoids a negative/undefined denominator. |
| AAR statistic | `aar_z = sum_i SAR_i / Q_total` | Sum (not mean) of standardized abnormal returns over the Patell denominator. |
| CAAR statistic | `caar_z = (1 / sqrt(N)) * sum_i (cumsum(SAR_i) / sqrt(n * Q_i))` | Per-event cumulative SAR scaled by `sqrt(n * Q_i)` where `n` is the day count, then averaged. |
| Positive count | `n_pos = sum(AR > 0)` – STRICT inequality | 2026-09-24 item A9: matches `SignTest`/`CSectTTest`. |
| Degenerate-event handling | Events with all-NA FEC sigma are excluded from `Q_total` (WR-01); events with ZERO finite event-window ARs are excluded from the WHOLE statistic (2026-09-24 item A2) | Prevents a `Q_i = 1` fallback (or a coalesced-to-0 CAR contribution) from a degenerate event distorting valid-event z-scores. |
| N \>= 2 guard | `aar_z` / `caar_z` are NA when `n_valid_events <= 1` (STATS-04) | A single event gives a statistically invalid z; NA is returned rather than a finite-but-wrong number. |
| Distribution | Approximately `N(0, 1)` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified against Patell (1976); three silent-wrong
defects fixed 2026-09-24 (items A2, A6, A9).** The standardization by
the forecast-error-corrected sigma and the `Q_total = sqrt(sum Q_i)`
aggregation match Patell. The 2026-09-24 re-evaluation found: (1) `k`
was derived as `length(resid) - df` instead of read from each model’s
own `n_params`, silently using the WRONG `k` whenever `df` reflected an
unrelated adjustment; (2) `m` used the raw estimation-window row count
instead of the finite-observation count; (3) `n_pos` used `>= 0`; and
(4) an all-NA event was not excluded from the statistic. All four are
fixed; re-pinned in `test_golden_values.R` and locked in
`test_reeval_statistical_fixes.R` against an independent closed-form
Patell computation at absolute tolerance `1e-10`, plus a guard test
confirming NA for a single-event group.

*Citation:* Patell, J. M. (1976). Corporate forecasts of earnings per
share and stock price behavior: Empirical tests. *Journal of Accounting
Research*, 14(2), 246–276.

### BMP test (BMPTest)

The Boehmer, Musumeci & Poulsen (1991) standardized cross-sectional test
combines Patell-style standardization with a cross-sectional variance
estimate, making it robust to event-induced increases in return variance
(which the plain Patell test does not accommodate). It standardizes
abnormal returns by the model sigma, then applies a cross-sectional
t-test to the standardized values.

| Convention | Choice | Note |
|----|----|----|
| Standardization | `SAR = AR / model sigma` | Uses the per-event MODEL sigma (contrast: Patell uses the forecast-error-corrected sigma). |
| Point statistic | `bmp_t = sqrt(N) * mean(SAR) / sd(SAR)` | Cross-sectional t of the standardized abnormal returns; `sd(SAR)` is the cross-event sample SD (this cross-sectional SD is what makes BMP robust to event-induced variance). |
| Cumulative statistic | `cbmp_t = sqrt(N) * mean(cumsum(SAR)) / sd(cumsum(SAR))` | Same cross-sectional t applied to per-event cumulative SARs. |
| Join key | `event_id` (one sigma per event) | A `firm_symbol` join would be many-to-many when a firm recurs across events. |
| All-NA event exclusion | An event with ZERO finite event-window ARs is excluded (2026-09-24 item A2) | See “Multi-event all-NA exclusion” below. |
| Distribution | Approximately `t_{N-1}` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change (all-NA exclusion added 2026-09-24,
item A2; roxygen documentation corrected, item A11).** The
standardization by MODEL sigma (not the forecast-error-corrected sigma –
this is the defining contrast with `PatellZTest`/`KolariPynnonenTest`)
and the cross-sectional t `sqrt(N) * mean(SAR) / sd(SAR)` match Boehmer,
Musumeci & Poulsen (1991). The 2026-09-24 re-evaluation found the
roxygen documentation itself INCORRECTLY described the standardization
as using the forecast-error-corrected sigma; the formula was always
right, only the docstring was fixed (item A11, docs-only). Pinned in
`test_golden_values.R` against an independent closed-form BMP
computation on a fixed three-event fixture at absolute tolerance
`1e-10`.

*Citation:* Boehmer, E., Musumeci, J., & Poulsen, A. B. (1991).
Event-study methodology under conditions of event-induced variance.
*Journal of Financial Economics*, 30(2), 253–272.

### Sign test (SignTest)

The sign test is a nonparametric test of whether positive abnormal
returns occur more often than the 0.5 that would be expected by chance.
Under the null the count of positive abnormal returns is Binomial(N,
0.5), approximated by a normal.

| Convention | Choice | Note |
|----|----|----|
| Positive count | `n_pos = sum(AR > 0)` – STRICT inequality | A zero abnormal return counts as NON-positive (`n_neg = sum(AR <= 0)`). This is the `> 0` (not `>= 0`) convention; it is the conservative choice, treating an exact zero as evidence against a positive effect. |
| Point statistic | `sign_z = (n_pos - 0.5 N) / (0.5 sqrt(N))` | Binomial-normal approximation with `p = 0.5`; `N` = valid events at that day. |
| Cumulative statistic | `csign_z = (n_pos_car - 0.5 N) / (0.5 sqrt(N))` | Same form applied to the count of positive per-event CARs. |
| N \>= 2 guard | `sign_z` / `csign_z` are NA when `n_valid_events < 2` (STATS-04, WR-02) | A single observation gives a finite-but-meaningless z; NA is returned instead. |
| All-NA event exclusion | An event with ZERO finite event-window ARs is excluded (2026-09-24 item A2) | See “Multi-event all-NA exclusion” below. |
| Distribution | Approximately `N(0, 1)` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified, no change (all-NA exclusion added 2026-09-24,
item A2).** The `(n_pos - 0.5 N)/(0.5 sqrt(N))` statistic, the strict
`> 0` positive-count convention, and the `N >= 2` guard match the
standard sign test. Pinned in `test_golden_values.R` against a direct
binomial-normal computation on the fixed three-event fixture at absolute
tolerance `1e-10`, plus a guard test confirming NA for a single-event
group.

*Citation:* The binomial sign test for event studies is standard; see
Brown & Warner (1980, 1985) and Cowan (1992) for its use alongside the
generalized form.

### Generalized Sign test (GeneralizedSignTest)

The generalized sign test (Cowan 1992) relaxes the ordinary sign test’s
implicit `p = 0.5` by estimating the expected fraction of positive
abnormal returns from the estimation window, which corrects for
asymmetry (skewness) in the return distribution.

| Convention | Choice | Note |
|----|----|----|
| Expected proportion | `p_hat = mean over EVENTS of mean(estimation AR > 0)` | Estimated PER EVENT from the estimation window, then averaged (2026-09-24 item A4: previously grouped by `firm_symbol`, which silently pooled a recurring firm’s estimation windows ACROSS its distinct events – each event is fit independently, so this mixed unrelated data). Replaces the ordinary test’s fixed 0.5. |
| Positive count | `n_pos = sum(AR > 0)` | Same strict `> 0` convention as the sign test. |
| Point statistic | `gsign_z = (n_pos - N p_hat) / sqrt(N p_hat (1 - p_hat))` | Cowan (1992) generalized sign statistic. |
| Cumulative statistic | `cgsign_z = (n_pos_car - N p_hat) / sqrt(N p_hat (1 - p_hat))` | Same form on positive per-event CAR counts. |
| Degenerate guard | NA when the denominator is non-finite or zero (`p_hat` in {0, 1}) | – |
| All-NA event exclusion | An event with ZERO finite event-window ARs is excluded (2026-09-24 item A2) | See “Multi-event all-NA exclusion” below. |
| Distribution | Approximately `N(0, 1)` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: one silent-wrong defect fixed 2026-09-24 (item A4;
all-NA exclusion added, item A2).** The
`(n_pos - N p_hat)/sqrt(N p_hat (1 - p_hat))` statistic and the
degenerate- denominator guard match Cowan (1992). The 2026-09-24
re-evaluation found `p_hat` was estimated by grouping the estimation
window by `firm_symbol` rather than `event_id`: when a firm recurs
across multiple events, its estimation-window abnormal returns from
DIFFERENT events were silently pooled into one `p_hat`, instead of
estimating one `p_hat` per event and averaging those. Fixed to group by
`event_id`; pinned in `test_golden_values.R` against an independent
closed-form computation on the three-event fixture (where the balanced
estimation residuals give `p_hat = 0.5`, so the generalized test reduces
to the ordinary sign test) at absolute tolerance `1e-10`, and locked in
`test_reeval_statistical_fixes.R` (identical results whether a firm
recurs across events or every event has a distinct firm).

*Citation:* Cowan, A. R. (1992). Nonparametric event study tests.
*Review of Quantitative Finance and Accounting*, 2(4), 343–358.

### Rank test (RankTest)

The Corrado (1989) rank test is a nonparametric test that ranks each
firm’s abnormal returns across the combined estimation and event
windows, then tests whether the event-window ranks are unusually high or
low. Ranking makes it robust to non-normality and to event-induced
variance.

| Convention | Choice | Note |
|----|----|----|
| Ranking scope | Ranks computed within each EVENT over the COMBINED estimation + event window | `ar_rank = rank(AR)`, `T` = combined observations for that event (2026-09-24 item A4: previously grouped by `firm_symbol`, silently pooling a recurring firm’s ranks ACROSS its distinct events). |
| Centered rank | `K = rank / (T + 1) - 0.5` | Corrado’s mean-zero centered rank; `na.last = "keep"` so missing ARs do not receive a rank. |
| Rank SD | `S_rank = sd(per-day mean centered rank across ALL combined days)` | The scale is estimated from the full combined window, not just the event window. |
| Point statistic | `rank_z = mean_rank_day / S_rank` | Per event day, the cross-firm mean centered rank divided by `S_rank`. |
| Degenerate guard | `rank_z` is NA when `S_rank` is non-finite or zero | Guarded once on the scalar `S_rank`, then the per-day vector is divided directly. |
| Distribution | Approximately `N(0, 1)` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: FIXED (broadcast bug), then verified.** The
centered-rank construction and the `mean_rank / S_rank` statistic match
Corrado (1989). The audit found a genuine implementation error: `rank_z`
was computed with base
`ifelse(is.finite(S_rank) & S_rank > 0, mean_rank / S_rank, NA_real_)`.
Because `S_rank` is a scalar, `ifelse` returned a length-1 result (the
length of its condition), collapsing the per-day `mean_rank / S_rank`
vector to its first element, which
[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)
then recycled across every event day – so all event days reported the
day-0 z. No published convention makes the rank z identical on every
day, so this is a bug, not a convention. It was fixed to guard the
scalar `S_rank` with a plain `if()` and divide the vector directly; a
regression test (`test_golden_values.R`) locks the corrected per-day
values (`+1.586, -0.952, +1.269`) and asserts they are distinct,
preventing the broadcast from silently returning. The fix precedes the
golden pin.

**2026-09-24 re-evaluation (item A4):** ranking/centering was also found
to group by `firm_symbol` instead of `event_id`, silently pooling a
recurring firm’s ranks across its distinct events. Fixed to group by
`event_id`; locked in `test_reeval_statistical_fixes.R` (identical
results whether a firm recurs across events or every event has a
distinct firm).

*Citation:* Corrado, C. J. (1989). A nonparametric test for abnormal
security-price performance in event studies. *Journal of Financial
Economics*, 23(2), 385–395.

### Calendar-Time Portfolio test (CalendarTimePortfolioTest)

The calendar-time portfolio approach forms, for each relative event day,
an equal-weighted portfolio of all event firms’ abnormal returns and
tests whether the portfolio’s mean return differs from zero. Aggregating
into a portfolio naturally absorbs the cross-sectional dependence that
arises when events cluster in calendar time.

| Convention | Choice | Note |
|----|----|----|
| Portfolio | Equal-weighted mean abnormal return per relative day (`aar`) | – |
| Time-series scale | `ts_sd = sd(aar)` over the ESTIMATION-window days | 2026-09-24 item A7 (Brown-Warner crude-dependence adjustment): `ts_sd` is the standard deviation of the cross-event AAR series computed OVER THE ESTIMATION WINDOW, not over the (much shorter, event-specific) event window. This makes the denominator independent of the event window and of any event-day shock magnitude. |
| Estimation-window AAR days | `n_estimation_days` = number of FINITE estimation-window AAR observations | Exposed as `attr(result, "caltime_df")`. |
| Point statistic | `caltime_t = aar / ts_sd` | Time-series t of the portfolio AAR under `H0: E[AAR] = 0`; `aar` is still the EVENT-window per-day portfolio mean. |
| Cumulative statistic | `ccaltime_t = caar / (ts_sd sqrt(L))` | `L` = day index within the event window; random-walk sqrt-of-horizon scaling of the cumulative portfolio return. |
| Degrees of freedom | `n_estimation_days - 1` | Brown-Warner time-series df, NOT a cross-sectional `N - 1`; [`adjust_p_values()`](https://sipemu.github.io/eventstudy/reference/adjust_p_values.md) and `tidy()` both read `attr(result, "caltime_df")` rather than deriving a df of their own. |
| Positive count | `n_pos = sum(AR > 0)` – STRICT inequality | 2026-09-24 item A9: matches `SignTest`/`CSectTTest`. |
| Degenerate guard | Both t’s are NA when `ts_sd` is non-finite/zero or `n_estimation_days < 2` | One contract warning/error (not a silent NA) when the estimation-window AAR series cannot support a standard deviation. |
| p-value sidedness | Two-sided by default | – |

**Audit result: FIXED (broadcast bug; then Brown-Warner denominator
redefinition, item A7; then A9).** The equal-weight portfolio
construction matches the calendar-time portfolio approach. Three issues
were found across two audit rounds:

1.  **Broadcast bug (original audit).** `caltime_t` / `ccaltime_t` used
    base `ifelse(is.finite(ts_sd) & ts_sd > 0, ...)` with the scalar
    `ts_sd` as the condition, collapsing the per-day t vectors to their
    first element and recycling it across every event day. Fixed by
    guarding the scalar once with a plain `if()` and dividing the
    vectors directly.
2.  **Self-referential denominator (2026-09-24 re-evaluation, item
    A7).** `ts_sd` was computed from the EVENT-WINDOW portfolio AAR
    series itself – the same series `aar` that appears in the numerator
    – so a larger event-day shock inflated BOTH the numerator and (via
    the window’s own dispersion) the denominator, and the statistic was
    not independent of the event window’s length or contents. This is
    REDEFINED (not merely a bug fix in the narrow sense) to the
    Brown-Warner (1980, 1985) crude-dependence adjustment: `ts_sd` is
    now the ESTIMATION-window AAR series’s standard deviation, with
    `df = n_estimation_days - 1` exposed via
    `attr(result, "caltime_df")` and threaded through to
    [`adjust_p_values()`](https://sipemu.github.io/eventstudy/reference/adjust_p_values.md)
    and `tidy()`. This is a genuine convention change (not a
    silent-input-only fix) – valid-input `caltime_t`/`ccaltime_t` values
    differ from the prior release; see NEWS.md. Locked in
    `test_reeval_statistical_fixes.R`: doubling a pure event-day shock
    exactly doubles `caltime_t` on that day (proving the denominator no
    longer depends on the event window).
3.  **Sign convention (item A9).** `n_pos` used `>= 0`; fixed to strict
    `> 0`.

A regression test locks the corrected per-day broadcast values and
asserts they are distinct; the golden values were RE-PINNED for the A7
redefinition (see `test_golden_values.R`, “re-pinned 2026-09-24: A7”).

*Citation:* Brown, S. J., & Warner, J. B. (1980). Measuring security
price performance. *Journal of Financial Economics*, 8(3), 205–258.
Brown, S. J., & Warner, J. B. (1985). Using daily stock returns: The
case of event studies. *Journal of Financial Economics*, 14(1), 3–31.
The calendar-time portfolio approach is also due to Jaffe (1974) and
Mandelker (1974), popularized for long-horizon studies by Fama (1998)
and Mitchell & Stafford (2000) as a control for cross-sectional
dependence.

### Kolari-Pynnonen test (KolariPynnonenTest)

The Kolari & Pynnonen (2010) test corrects the BMP standardized
cross-sectional test for cross-sectional correlation of abnormal
returns. When events share calendar time, their abnormal returns are
positively correlated, which biases the BMP variance downward and
over-rejects the null; the KP adjustment scales the BMP statistic down
by a factor derived from the average pairwise correlation.

| Convention | Choice | Note |
|----|----|----|
| Base statistic | BMP cross-sectional t (`SAR = AR / model sigma`, `bmp_t = sqrt(N) mean(SAR)/sd(SAR)`) | Identical to `BMPTest`; KP scales it. |
| Usable events | An event contributes to `r_bar` only if its estimation-window SAR series has `>= 2` finite values, a finite model sigma, AND a strictly positive (non-constant) SAR standard deviation | 2026-09-24 item A3: a NON-usable event is excluded (once, with a contract warning/error naming it) – never silently folded into the correlation matrix as a degenerate (constant or all-NA) series. |
| Average correlation | `r_bar` = average off-diagonal pairwise correlation of estimation-window SARs, over the USABLE events only | `r_bar = (sum(cor) - n_usable) / (n_usable (n_usable - 1))`; events (not firms) are the cross-sectional units, so the SAR matrix is pivoted by `event_id`. Numerically identical to the pre-2026-09-24 formula when every event is usable. |
| Adjustment factor | `kp_adj_t = sqrt((1 - r_bar) / (1 + (n_t - 1) r_bar))` PER DAY | Kolari-Pynnonen (2010) correction; `n_t` is THAT DAY’s `n_valid_events` (2026-09-24 item A3: previously a single scalar `n <- aar_stats$n_valid_events[1]` – the FIRST day’s count – was applied uniformly to every day, silently wrong whenever the per-day valid-event count varies). `kp_adj < 1` when `r_bar > 0`, shrinking the statistic toward zero. |
| Point statistic | `kp_t = bmp_t * kp_adj_t` | – |
| Cumulative statistic | `ckp_t = cbmp_t * kp_adj_t'` | `kp_adj_t'` uses the cumulative SAR’s own per-day `n_valid` (from `cum_sar`), not the AAR-level `n_valid_events`. |
| Degenerate guard | `kp_t`/`ckp_t` are NA (NEVER `kp_adj = 1`) when fewer than 2 usable events remain, an off-diagonal correlation is non-finite, or the adjustment’s numerator/denominator is invalid | 2026-09-24 item A3: the pre-fix fallback silently substituted `kp_adj = 1` (i.e. the UNADJUSTED BMP value) whenever the correction was undefined – indistinguishable from “no correlation correction needed” when it actually meant “the correction could not be computed”. Exactly one warning/error per [`compute()`](https://dplyr.tidyverse.org/reference/compute.html) call covers both the usable-event exclusion and this NA substitution (never two). |
| All-NA event exclusion | An event with ZERO finite event-window ARs is excluded from the whole statistic (2026-09-24 item A2) | See “Multi-event all-NA exclusion” below; distinct from the estimation-window “usable events” check above (A3) – an event can have valid event-window ARs but a non-usable estimation-window SAR series, or vice versa. |
| Distribution | Approximately `t_{N-1}` | – |
| p-value sidedness | Two-sided by default | – |

**Audit result: verified against Kolari & Pynnonen (2010); two
silent-wrong defects fixed 2026-09-24 (items A2, A3).** The BMP base
statistic and the `sqrt((1 - r_bar)/(1 + (n_t - 1) r_bar))` adjustment
formula match Kolari-Pynnonen. The 2026-09-24 re-evaluation found: (1) a
degenerate (all-NA or constant) event’s estimation-window SAR series was
silently folded into the `r_bar` correlation matrix instead of being
excluded, and when the adjustment was undefined the code fell back to
`kp_adj = 1` – silently reporting the UNADJUSTED BMP value as if it were
KP-corrected; and (2) the per-day window-size term used only the FIRST
day’s `n_valid_events`, applied uniformly to every day, instead of that
day’s own count. Both are fixed; re-pinned in `test_golden_values.R`
against the package pipeline’s own output on the three-event fixture
(where the events’ estimation SARs are highly correlated by
construction, `r_bar ~ 0.983`, so `kp_adj ~ 0.076` shrinks the BMP t
sharply – unaffected here since all three events remain usable and
`n_valid_events` is constant across days in this fixture), and locked in
`test_reeval_statistical_fixes.R` against a 6-event fixture with one
constant-estimation-window (non-usable) event and a per-day varying
`n_valid_events`, at absolute tolerance `1e-8`.

*Citation:* Kolari, J. W., & Pynnonen, S. (2010). Event study testing
with cross-sectional correlation of abnormal returns. *Review of
Financial Studies*, 23(11), 3996–4025.

### PermutationTest (unwired stub)

`PermutationTest` is exported but its
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) body
is empty (returns `NULL`) and it is not wired into any statistics set or
parameter set – it is not on any pipeline path. It is recorded here for
completeness; a decision to implement or remove it is deferred (see the
audit log).

### TestStatisticBase `confidence_type` (2026-09-24 re-evaluation, item A10)

Every `TestStatisticBase` subclass accepts `confidence_level` and
`confidence_type` at construction, but `confidence_type` was never read
by any [`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
method – every p-value computed anywhere in EventStudy is two-sided.
`initialize()` now validates `confidence_type` with
`match.arg(c("two-sided", "less", "greater"))` (an invalid value errors)
and emits ONE warning at construction when a non-default value (“less”
or “greater”) is supplied, stating that it is currently ignored. The
value is still stored (for forward compatibility) but has no effect;
one-sided p-values are NOT implemented by this fix. This is a documented
convention (no p-value formula changed) with an added validation/warning
surface.

### Cross-sectional regression (2026-09-24 re-evaluation, item A5)

[`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)
regresses per-event CARs on user-supplied firm characteristics. Prior to
2026-09-24 it silently tolerated three sample- integrity hazards:

| Hazard | Behavior now |
|----|----|
| Duplicated `event_id` in the `data` argument | Errors (`eventstudy_error_bad_argument`) naming the duplicated id(s) – a duplicate would otherwise inflate the join many-to-many. |
| `car_window` extending outside the event window of any event | Errors (`eventstudy_error_bad_argument`) naming the window, the available range, and the offending event(s), instead of silently summing over rows that do not exist for that event. |
| A task event with no matching row in `data` | One warning listing the count/ids, then dropped (unchanged: an empty match set still errors). |
| A missing abnormal return inside `car_window` for an otherwise-matched event | That event’s CAR is `NA` (`.extract_cars()` no longer uses `na.rm = TRUE`, so ANY missing AR in the window makes the whole-window CAR `NA`, not the sum of the finite subset) and it is excluded from the regression with one warning listing the event(s). [`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md), [`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md), and [`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md) inherit the same NA-CAR semantics from the shared `.extract_cars()` helper WITHOUT an additional warning (their own `na.rm = TRUE` summary statistics already tolerate NA rows silently, as documented). |

This is a sample-integrity fix (T-sfl-02 in the threat register), not a
formula change: the OLS regression itself, robust-SE handling, and CAR
extraction for a fully-matched, non-degenerate window are unchanged.

### Wild bootstrap inference (2026-09-24 re-evaluation, item A12)

[`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md)
computes bootstrap p-values for AAR/CAAR test statistics. Two
silent-wrong defects were found and fixed:

- **A12a: observed-vs-draw construction mismatch.** `observed_caar` /
  `caar_t` were previously built as
  [`cumsum()`](https://rdrr.io/r/base/cumsum.html) of the cross-event
  DAILY mean AR (`na.rm = TRUE`), while each bootstrap draw built its
  CAAR as the MEAN over events of each event’s own
  `cumsum(coalesce(AR, 0))`. The two constructions coincide only when
  every event has a fully-observed event window; with a partial gap in
  ONE event (but not all), they diverge, and only the per-event-CAR-mean
  construction is the correct STATS-03-consistent comparison for what
  the bootstrap draws compute. Fixed so `observed_caar` and `caar_t` use
  the SAME per-event `coalesce`-to-0 CAR construction as the draws; on
  NA-free input this is numerically identical to the old `cumsum(aar)`
  form.
- **A12b: NA draws counted as “did not exceed”.** A bootstrap draw whose
  cross-sectional statistic is `NA` (e.g. a Rademacher sign-flip
  combination that zeroes the cross-sectional SD) was previously counted
  as “did not exceed the observed statistic” while still inflating the
  `n_boot + 1` denominator – silently biasing the p-value toward
  significance (too small) whenever NA draws are common. Fixed to track
  the number of VALID (finite) draws per day and use
  `p = (exceed + 1) / (n_valid_draws + 1)`, with `NA` when the observed
  statistic is `NA` or no draw produced a finite statistic.
- **A12c: bootstrap clustering unit (REVISED, no change).** The
  bootstrap weights are clustered by `firm_symbol` (one weight per
  unique firm per draw, shared across that firm’s events) – a deliberate
  prior fix; firm-level clusters are the coarser, correlation-robust
  choice when a firm recurs across multiple events. The 2026-09-24
  re-evaluation confirmed this is intentional and made NO change; it is
  locked with a hand-replication test (`test_bootstrap.R`).

## Audit log

| Component | Result | Citation |
|----|----|----|
| Market Model + AR/CAR t | **FIXED 2026-09-24** (A6: insufficient-obs guard now `n_params`-based; FEC restricted to complete pairs) | MacKinlay (1997) |
| Market Adjusted Model | **FIXED 2026-09-24** (A8: FEC == sigma exactly, correction factor 1) | MacKinlay (1997) |
| Comparison Period Mean Adjusted Model | verified, no change | Brown-Warner (1985); MacKinlay (1997) |
| Fama-French Three-Factor Model | **FIXED 2026-09-24** (A6: insufficient-obs guard strengthened) | Fama-French (1993) |
| Fama-French Five-Factor Model | **FIXED 2026-09-24** (A6: insufficient-obs guard strengthened) | Fama-French (2015) |
| Carhart Four-Factor Model | **FIXED 2026-09-24** (A6: insufficient-obs guard strengthened) | Carhart (1997) |
| Custom Model | documented convention (user-supplied prediction; reduces to Market Model when `loss_market_cap = 0`) | MacKinlay (1997) |
| BHAR Model + BHARTTest | verified, no change | Barber-Lyon (1997); Lyon-Barber-Tsai (1999) |
| Volume Model | verified, no change | Campbell-Wasley (1996); Brown-Warner (1985) |
| Volatility Model | verified, no change | Beaver (1968); Patell-Wolfson (1979) |
| Rolling-Window Model | **FIXED 2026-09-24** (A1: complete-pair restriction inside each rolling window) | Fama-MacBeth (1973); MacKinlay (1997) |
| GARCH Model | verified, no change (identity-pinned, skip-guarded on rugarch) | Engle (1982); Bollerslev (1986) |
| DCC-GARCH Model | verified, no change (identity-pinned, skip-guarded on rmgarch) | Bollerslev (1990); Engle (2002) |
| AR / CAR t-tests (ARTTest, CARTTest) | verified, no change (documented: `forecast_error_corrected_sigma_car` intentionally unused, A11) | MacKinlay (1997) |
| BHAR t-test (BHARTTest) | verified, no change | Barber-Lyon (1997); Lyon-Barber-Tsai (1999) |
| Multi-event all-NA exclusion (shared mechanism) | **FIXED 2026-09-24** (A2: all-NA events excluded from every CAR-based multi-event statistic, once per group) | – |
| Cross-Sectional t-test (CSectTTest) | **FIXED 2026-09-24** (A2 exclusion; A9: strict `n_pos > 0`) | Brown-Warner (1985) |
| Patell Z-test (PatellZTest) | **FIXED 2026-09-24** (A2 exclusion; A6: `m`/`k` redefinition; A9: strict `n_pos > 0`) | Patell (1976) |
| BMP test (BMPTest) | **FIXED 2026-09-24** (A2 exclusion; A11: roxygen corrected, docs-only) | Boehmer-Musumeci-Poulsen (1991) |
| Sign test (SignTest) | **FIXED 2026-09-24** (A2 exclusion) | Brown-Warner (1985); Cowan (1992) |
| Generalized Sign test (GeneralizedSignTest) | **FIXED 2026-09-24** (A2 exclusion; A4: `p_hat` grouped by `event_id`) | Cowan (1992) |
| Rank test (RankTest) | **FIXED** (original audit: `rank_z` broadcast); **FIXED 2026-09-24** (A4: grouped by `event_id`) | Corrado (1989) |
| Calendar-Time Portfolio test (CalendarTimePortfolioTest) | **FIXED** (original audit: `caltime_t`/`ccaltime_t` broadcast); **FIXED 2026-09-24** (A7: Brown-Warner estimation-window denominator, redefinition; A9: strict `n_pos > 0`) | Brown-Warner (1980, 1985); Jaffe (1974); Fama (1998) |
| Kolari-Pynnonen test (KolariPynnonenTest) | **FIXED 2026-09-24** (A2 exclusion; A3: usable-events `r_bar`, per-day adjustment, never `kp_adj = 1` fallback) | Kolari-Pynnonen (2010) |
| PermutationTest | unwired stub – empty [`compute()`](https://dplyr.tidyverse.org/reference/compute.html), not on pipeline path; deferred | – |
| [`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md) | **FIXED 2026-09-24** (A12a: observed/draw CAAR construction aligned; A12b: NA draws excluded from denominator; A12c: clustering unit confirmed, no change) | – |
| [`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md) | **FIXED 2026-09-24** (A5: duplicate-id error, out-of-range `car_window` error, unmatched/NA-CAR warnings+exclusion) | – |
| `TestStatisticBase$confidence_type` | **FIXED 2026-09-24** (A10: validated, warns once when non-default; no formula change) | – |

### Audit summary

The original audit covered all 13+ return models and all wired test
statistics and found two broadcast-bug implementation errors (RankTest,
CalendarTimePortfolioTest). The 2026-09-24 re-evaluation covered the
same surface plus
[`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md)
and
[`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md),
and found twelve additional silent-wrong-statistic defects (items A1-A9,
A11-A12; A10 is a validation/warning addition, no formula change; A11 is
docs-only). Every defect is fixed, locked by a regression test that
fails on the pre-fix code (`test_reeval_statistical_fixes.R`), and
golden values affected by a genuine redefinition (A7, A8, and the
Patell/KP/Rank/GSign pins touched by A2/A3/A4/A6/A9) are re-pinned in
`test_golden_values.R` with an explanatory comment. No valid-input
behavior changed for any component NOT listed above as fixed.

**2026-09-24 re-evaluation fixes (12 items, A1-A9 + A11-A12; A10 is a
validation addition):**

1.  **A1 – RollingWindowModel complete pairs.** Rolling-window OLS
    computed `x_bar`/`ss_xx` from ALL index values in a window,
    including rows where the firm return was NA, biasing beta when
    NA-firm rows co-occur with index outliers. Fixed to restrict each
    window to complete pairs.
2.  **A2 – Multi-event all-NA exclusion.** A fully-degenerate event (all
    event-window ARs NA) was coalesced to 0 in cumulative statistics
    instead of excluded, silently understating cross-event dispersion.
    Fixed with a shared exclusion helper applied once per group.
3.  **A3 – KolariPynnonenTest usable events.** A degenerate
    estimation-window SAR series was folded into `r_bar`, and an
    undefined adjustment silently fell back to `kp_adj = 1` (the
    unadjusted BMP value). Fixed to exclude non-usable events from
    `r_bar` and return NA (never a silent 1) when the adjustment cannot
    be computed; also fixed the per-day window-size term (previously the
    first day’s count applied to every day).
4.  **A4 – RankTest / GeneralizedSignTest grouping.** Ranking and
    `p_hat` estimation grouped by `firm_symbol`, silently pooling a
    recurring firm’s data across its distinct events. Fixed to group by
    `event_id`.
5.  **A5 – cross_sectional_regression sample integrity.** Duplicate
    `event_id`s, out-of-range `car_window`s, and NA-CAR events were
    silently tolerated. Fixed with explicit errors/warnings and
    exclusion.
6.  **A6 – OLS insufficient-observations guard and Patell m/k.** The
    insufficient-obs guard used a flat threshold instead of
    `n_params + 1`; Patell’s `k` was derived as `length(resid) - df`
    instead of read from the model; Patell’s `m` used the raw row count
    instead of the finite count; MarketModel’s FEC included
    incomplete-pair rows. All fixed.
7.  **A7 – CalendarTimePortfolioTest denominator.** `ts_sd` was computed
    from the event-window portfolio series itself (self-referential with
    the numerator). Redefined to the Brown-Warner (1980, 1985)
    estimation-window AAR standard deviation.
8.  **A8 – MarketAdjustedModel FEC.** Wrongly reused the constant-MEAN
    correction (`sigma * sqrt(1 + 1/m)`) for a model that estimates
    NOTHING. Fixed to FEC == sigma exactly.
9.  **A9 – Sign convention.** `n_pos` used `>= 0` in CSectT, Patell, and
    CalendarTime (inconsistent with SignTest’s `> 0`). Fixed to strict
    `> 0` everywhere.
10. **A11 – BMP/KP roxygen (docs-only).** The BMP docstring incorrectly
    claimed forecast-error-corrected-sigma standardization; both BMP and
    KP standardize by MODEL sigma. Fixed the docstring; no formula
    change.
11. **A12a/b – bootstrap_test() observed/draw mismatch and NA-draw
    denominator.** `observed_caar` used a different construction than
    the draws; NA draws were counted as “did not exceed” while still
    inflating the denominator. Both fixed.
12. **A12c – bootstrap clustering unit (confirmed, no change).**
    Firm-level clustering is deliberate and unchanged; locked with a
    hand-replication test.

A10 (TestStatisticBase `confidence_type` validation) adds a
construction-time check and a one-time warning; it changes no computed
statistic.
