# EventStudy 0.3.0

## New Features

* 13 return models: Market Model, Market Adjusted, Comparison Period Mean
  Adjusted, Custom Model, Fama-French 3-Factor, Fama-French 5-Factor,
  Carhart 4-Factor, GARCH, BHAR, Volume, and Volatility models.
* 11 test statistics: AR T-Test, CAR T-Test, BHAR T-Test, Cross-Sectional
  T-Test, Patell Z-Test, Sign Test, Generalized Sign Test, Rank Test,
  BMP Test, and Calendar-Time Portfolio Test.
* Cross-sectional regression of CARs on firm characteristics with
  heteroskedasticity-consistent standard errors.
* Intraday event study support with POSIXct timestamps.
* Panel (DiD) event study module with TWFE and Sun-Abraham estimators,
  including cluster-robust standard errors.
* Export results to CSV, Excel, and LaTeX formats.
* Tidy method for converting results to long-format tibbles.
* Diagnostic tools: model residual plots, estimation window checks.
* 10 vignettes covering introduction, custom models, custom test statistics,
  result extraction, panel event studies, cross-sectional analysis, factor
  models/BHAR, diagnostics, volume/volatility studies, and intraday studies.
