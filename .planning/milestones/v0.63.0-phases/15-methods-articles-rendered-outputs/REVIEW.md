---
phase: 15-methods-articles-rendered-outputs
reviewed: 2026-09-06T00:00:00Z
depth: deep
files_reviewed: 9
files_reviewed_list:
  - vignettes/articles/methods-return-models.Rmd
  - vignettes/articles/methods-test-statistics.Rmd
  - vignettes/articles/methods-panel-did.Rmd
  - vignettes/articles/methods-intraday.Rmd
  - vignettes/articles/methods-synthetic-control.Rmd
  - vignettes/articles/methods-ai-advisor.Rmd
  - vignettes/articles/methods-diagnostics.Rmd
  - vignettes/articles/references.bib
  - _pkgdown.yml
findings:
  critical: 0
  warning: 2
  info: 2
  total: 4
status: findings
---

# Phase 15: Code Review Report

**Reviewed:** 2026-09-06
**Depth:** deep (cross-file: article LaTeX vs R source, bib vs known metadata, API vs NAMESPACE)
**Files Reviewed:** 9 (7 articles + references.bib + _pkgdown.yml)
**Status:** findings (2 warnings, 2 info — no blockers)

## Summary

The formula-review gate (SC-4) is the decisive clearance for this phase, and it PASSES cleanly:
every displayed LaTeX equation was traced to its cited source line and to the cited paper, and
all are faithful. Citation metadata (15 entries) is accurate — including the entry the SUMMARY
flagged as low-confidence, `BorusyakJaravelSpiess2024`, which is in fact correct (REStud 91(6):3253–3285).
RENDER-01 passes: every article's table + plot come from an always-live, ungated chunk. No edits
leaked into R/, DESCRIPTION, NAMESPACE, or the CRAN vignettes.

Two warnings concern API accuracy of shown code: (WR-01) the AI-advisor article's `eval=FALSE`
LLM chunk calls `advise_llm()`, which is not an exported function — the real exported name is
`es_advise()`; and (WR-02) the synthetic-control prose calls `method = "optim"` "the default"
when the source default is `"quadprog"`. Neither breaks the render (the live SC chunk explicitly
passes `method = "optim"`, and the advisor chunk is `eval=FALSE`), but both show users an API
that does not match the package.

## Formula-Review Gate (SC-4) — VERDICT: ALL FAITHFUL

Every `<!-- Formula verified -->` comment was opened against its source line:

| Article / formula | Source checked | Faithful? |
|---|---|---|
| Market model `R_it = a + b*R_mt`, AR = residual | R/models.R:248 (`firm_returns - (alpha + beta*index_returns)`) | YES |
| CAR = sum of AR over window | R/single_event_test_statistics.R:127 (`cumsum(abnormal_returns)`) | YES |
| BHAR = `prod(1+R_i) - prod(1+R_m)` | R/single_event_test_statistics.R:189–191 (`cumprod(1+firm) - cumprod(1+index)`) | YES |
| CSectT `t = sqrt(N)*AAR/s_AR` | R/multi_event_test_statistics.R:35 (`sqrt(n_valid)*aar/sd_ar`) | YES |
| Patell `Z = sum(SAR)/sqrt(sum(Q_i))`, `Q_i=(m-k)/(m-k-2)` | R/multi_event_test_statistics.R:113,150,166 (`sum_sar/sqrt(sum(Q_i))`, `Q_i=(m-k)/(m-k-2)`) | YES |
| BMP `t = sqrt(N)*mean(SAR)/sd(SAR)` | R/multi_event_test_statistics.R:459–461 (`sqrt(n_valid)*mean_sar/sd_sar`) | YES |
| KP `t_KP = t_BMP*sqrt((1-r)/(1+(N-1)r))`, r = avg off-diag corr | R/multi_event_test_statistics.R:660,671,677 (`kp_adj=sqrt(numer/denom)`, `r_bar=(sum-n)/(n*(n-1))`, `kp_t=bmp_t*kp_adj`) | YES |
| Static TWFE `y = a_i + l_t + d*D_it` | R/panel_event_study.R:209–214 (`Y ~ D + factor(unit) + factor(time)`) | YES |
| Dynamic TWFE event-time indicators, drop k=-1 | R/panel_event_study.R:242–254 (binned rel-time factor, base period dropped) | YES |
| SC objective: min pre-MSPE, w>=0, sum w=1 | R/synthetic_control.R:249 (`sum((y - X%*%w)^2)`, softmax simplex) | YES |
| Durbin-Watson `d = sum(diff(e)^2)/sum(e^2)` | R/diagnostics.R:56–61 (`sum(diff(resid)^2)/sum(resid^2)`) — exact | YES |
| Ljung-Box `Q = n(n+2) sum(rho_k^2/(n-k))` | R/diagnostics.R:64–67 (`Box.test type="Ljung-Box"`) | YES |
| Wild bootstrap `AR* = w_b*AR`, Rademacher | R/bootstrap.R:93 (`sample(c(-1,1))`, weighted AR) | YES |
| MC power = mean rejection indicator | R/simulation.R:115 (`colMeans(rejection_matrix)`) | YES |

No subtly-wrong subscript, missing correction term, or wrong null was found. The forecast-error
correction is present (Patell uses `forecast_error_corrected_sigma`, R/multi_event_test_statistics.R:121),
and the KP correction term matches Kolari-Pynnönen (2010) exactly.

## Citation Accuracy — ALL 15 CORRECT

Cross-checked against known bibliographic metadata: Fama-French 1993 (JFE 33(1):3–56),
Fama-French 2015 (JFE 116(1):1–22), Carhart 1997 (JF 52(1):57–82), Patell 1976 (JAR 14(2):246–276),
BMP 1991 (JFE 30(2):253–272), Corrado 1989 (JFE 23(2):385–395), Kolari-Pynnönen 2010 (RFS 23(11):3996–4025),
Callaway-Sant'Anna 2021 (JoE 225(2):200–230), Sun-Abraham 2021 (JoE 225(2):175–199),
deChaisemartin-D'Haultfoeuille 2020 (AER 110(9):2964–2996), Goodman-Bacon 2021 (JoE 225(2):254–277),
Abadie-Diamond-Hainmueller 2010 (JASA 105(490):493–505), Barclay-Warner 1993 (JFE 34(3):281–305),
Barber-Lyon 1997 (JFE 43(3):341–372). **BorusyakJaravelSpiess2024 (REStud 91(6):3253–3285) is
CORRECT** — the SUMMARY's low-confidence flag can be cleared; the entry needs no change.

## Warnings

### WR-01: AI-advisor article shows non-existent function `advise_llm()`

**File:** `vignettes/articles/methods-ai-advisor.Rmd:98`
**Issue:** The LLM-layer chunk calls `advise_llm(task, provider = "anthropic")`. There is no
exported (or internal) `advise_llm` in the package — the actual LLM entry point is `es_advise()`
(`R/advise.R:740`, `es_advise(diagnostics, task_type, provider, model, ...)`). The chunk is
`eval=FALSE` so the build does not break, but the article teaches users an API call that will
error with "could not find function advise_llm" if copied. The live chunk functions in the same
article (`es_diagnostics`, `recommend_stat`, `flag_robustness`, `plot_diagnostics`) are all real
and exported — only the LLM-layer name is wrong.
**Fix:** Replace `advise_llm(task, provider = "anthropic")` with the real signature, e.g.
`es_advise(es_diagnostics(task), task_type = "single", provider = "anthropic")` (confirm the
exact task_type/argument shape against `R/advise.R:740`), or drop the provider example if the
signature is materially different from the illustrative one shown.

### WR-02: Synthetic-control article calls `method = "optim"` the default; source default is `"quadprog"`

**File:** `vignettes/articles/methods-synthetic-control.Rmd:18,73,96`
**Issue:** §1 and §6 describe `method = "optim"` as the default solver ("We default to
`method = "optim"`"). The source signature is `estimate_synthetic_control(task, method = c("quadprog", "optim"), ...)`
(`R/synthetic_control.R:80`), so `match.arg` makes `"quadprog"` the actual default when `method`
is omitted. The live chunk itself is correct because it explicitly passes `method = "optim"`, so
the render is unaffected — but the prose misstates the package default, and quadprog silently
falls back to optim only when the package is present (R/synthetic_control.R:128).
**Fix:** Reword to "we explicitly choose `method = "optim"` (pure base-R `stats::optim`); the
package default is `"quadprog"` when that optional package is installed" — keep the explicit
`method = "optim"` call in the live chunk as-is.

## Info

### IN-01: BorusyakJaravelSpiess2024 uncertainty comment can be removed

**File:** `vignettes/articles/references.bib:127-128`
**Issue:** The `[ASSUMED, lower confidence]` comment on the Borusyak-Jaravel-Spiess entry is now
stale — the metadata (REStud vol 91, no 6, pp 3253–3285) is correct. Leaving the comment implies
a defect that does not exist.
**Fix:** Delete the low-confidence comment (lines 127–128); the entry is accurate as written.

### IN-02: Return-models `MacKinlay1997`/`Brown1985` cited but not spot-verified here

**File:** `vignettes/articles/methods-return-models.Rmd:79-80`, `references.bib`
**Issue:** The market-model null cites `[@MacKinlay1997; @Brown1985]`; these two entries were not
in the prompt's spot-check list and were not opened in this review. The 15 entries that were
checked are all correct, so confidence is high, but MacKinlay 1997 (J. Econ. Lit. 35(1):13–39)
and Brown-Warner 1985 (JFE 14(1):3–31) should be confirmed against the bib during any future
citation sweep.
**Fix:** Confirm MacKinlay1997 and Brown1985 volume/pages in a follow-up bib pass; no action
required for this phase's clearance.

---

_Reviewed: 2026-09-06_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: deep_
