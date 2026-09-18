# Return Model Map (EventStudy v0.66.0)

Every model is an R6 generator exported from the package; instantiate with
`ModelName$new(...)` and pass via `ParameterSet$new(return_model = ...)`. This list
mirrors `NAMESPACE` at v0.66.0 — **verify against the installed package** with
`Rscript -e 'library(EventStudy); grep("Model$", ls("package:EventStudy"), value=TRUE)'`
before relying on it.

All models inherit the `fit(data_tbl)` / `abnormal_returns(data_tbl)` interface and
populate `statistics` (sigma, df, residuals) used downstream by the test statistics.

## Benchmark / market models

| Model | What it estimates | Data needed | Notes |
|-------|-------------------|-------------|-------|
| `MarketModel` | OLS regression of firm return on index return over the estimation window | firm + index returns | The default; expected return = α + β·R_index |
| `MarketAdjustedModel` | Uses the index return directly as the expected return (β≡1, α≡0) | firm + index returns | No estimation window regression |
| `ComparisonPeriodMeanAdjustedModel` | Mean firm return over the estimation window as expected return | firm returns | "Constant mean return" model |

## Multi-factor models

Require **factor data joined by date** (see `download_factor_data`).

| Model | Factors | Data needed |
|-------|---------|-------------|
| `FamaFrench3FactorModel` | Market, SMB, HML | firm returns + FF3 factors + risk-free |
| `FamaFrench5FactorModel` | Market, SMB, HML, RMW, CMA | firm returns + FF5 factors + risk-free |
| `Carhart4FactorModel` | FF3 + momentum | firm returns + Carhart factors + risk-free |
| `LinearFactorModel` | Arbitrary user-supplied linear factors | firm returns + named factor columns |

## Time-varying / conditional models

| Model | What it captures | Optional dependency |
|-------|------------------|---------------------|
| `RollingWindowModel` | Time-varying α/β via a rolling estimation window | — |
| `GARCHModel` | Conditional heteroskedasticity in returns | `rugarch` (Suggests, `requireNamespace`-guarded) |
| `DCCGARCHModel` | Dynamic conditional correlation across series | `rmgarch` (Suggests-guarded) |

## Non-price / long-horizon models

| Model | Target quantity | Pair with |
|-------|-----------------|-----------|
| `BHARModel` | Buy-and-hold abnormal returns (long horizon) | `BHARTTest` |
| `VolumeModel` | Abnormal trading volume | volume-based statistics |
| `VolatilityModel` | Abnormal volatility | volatility-based statistics |

## Custom models

There is no separate `CustomModel` export — build one by subclassing the abstract
`ModelBase` (see the *custom-models* vignette). The base class defines the
`fit()` / `abnormal_returns()` contract every model above implements.

## Picking a model

- Start with `MarketModel` unless you have a reason not to.
- Add factor models when controlling for size/value/momentum exposure matters.
- Use `RollingWindowModel`/`GARCHModel` when the estimation-window constant-beta
  assumption is suspect.
- Switch to `BHARModel`/`VolumeModel`/`VolatilityModel` when the event's effect is
  on long-horizon returns, liquidity, or risk rather than short-horizon mean return.

Whether a chosen model is *appropriate for your fitted diagnostics* is an
**es-advisor** question, not a capability-discovery one.
