# Milestones

## v0.63.0 v0.63.0 (Shipped: 2026-09-06)

**Phases completed:** 4 phases, 4 plans, 3 tasks

**Key accomplishments:**

- AAPL/MSFT/GOOGL Q1 2023 earnings-beat panel frozen via live Yahoo Finance fetch, documented with dieselgate-pattern roxygen, pipeline-proven to finite CAAR, and CRAN-clean.

---

## v0.61.0 v0.61.0 (Shipped: 2026-09-04)

**Phases completed:** 0 phases, 0 plans, 0 tasks

**Key accomplishments:**

- (none recorded)

---

## v0.60.0 Grounded AI Advisor (Shipped: 2026-09-04)

**Phases completed:** 4 phases, 10 plans, 10 tasks

**Key accomplishments:**

- Deterministic zero-dependency es_diagnostics() harvester: S3-classed named list extracting estimation-window fit signals, event-window AR/CAR p-values (via stats::pt — no dist objects), cross-sectional IQR/overlap, and per-event contract state from a fitted EventStudyTask, with anomaly-ranked max_events cap and aggregate remainder
- Offline KB-matching advice engine — `recommend_stat()`/`flag_robustness()` deliver severity-ranked `es_advice` objects from any fitted task or diagnostics, with zero dependencies and no-provider guarantee (ADV-08).
- Wire grounded `Advice` object into `generate_report()` via trailing `advice=NULL` param and guarded skeleton.Rmd chunk — NULL path is byte-identical, invalid advice degrades with one warning.
- Claude Code Agent Skill driving the full advisor loop via existing exports only, plus a CRAN-safe opt-in Advisor Pro waitlist footer with `?advisor_pro` doc topic and README section.
- Version bumped 0.50.0 → 0.60.0, NEWS v0.60.0 section written, dev dirs excluded from CRAN tarball via .Rbuildignore, advisor_pro.Rd generated, R CMD check --as-cran passes with 0 new NOTEs/WARNINGs.

---

## v0.50.0 Robustness Hardening (Shipped: 2026-09-02)

**Phases completed:** 4 phases, 10 plans, 18 tasks

**Key accomplishments:**

- Degenerate-input contract for MarketModel: R/contract.R with .resolve_degenerate_mode/.handle_degenerate, ParameterSet field, execute.R row-indexed key threading, byte-identical valid-input output confirmed
- testthat 3e regression net for the degenerate-input contract: 12 contract tests + 5 ParameterSet validation tests, degenerate data factories, man/degenerate-input-contract.Rd, and 973-test green suite
- Six models (MarketAdjusted, ComparisonPeriodMean, Custom, BHAR, Volume, Volatility) migrated onto the Phase 1 degenerate-input contract via .handle_degenerate(); .finite_residual_df() helper added; VolatilityModel guard-placement bug fixed; BHARModel df and unconditional-AR bugs fixed
- RollingWindowModel fully migrated onto .handle_degenerate() with n_valid upgrade; GARCHModel and DCCGARCHModel receive PRE-CALL contract guards before external package calls with FEC n_valid fix; Phase 3 failure wrapping untouched
- Surgical sigma==0 and n_events==1 denominator guards added to 5 test statistics; STATS-02/03/04 correctness locked with regression tests; full 409-test suite green.
- Mode-honoring missing-date degradation in prepare_event_study(), coalesce-guarded CAR cumsum in export, and tryCatch singular/collinear guard + message->warning in cross_sectional_regression()
- Wrapped all four external-package call sites (did, DIDmultiplegt, didimputation, sandwich) and both GARCH statistics paths (rugarch, rmgarch) with tryCatch+warning+NULL/NA degradation; added synthetic-control solve.QP and empty-pool guards.
- 25-component table-driven contract matrix (test_contract_matrix.R) and complete fix→test catalog in NEWS.md lock all Phases 1-3 degenerate-input hardening against regression
- Check gate passed — 0 errors/warnings vs baseline; fixed 3 new findings (non-ASCII WARNING, callr Suggests NOTE, n_car globalVariables NOTE) at source; full 1378-test suite green.

---
