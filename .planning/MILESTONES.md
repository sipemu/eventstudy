# Milestones

## v0.65.0 Polish (Shipped: 2026-09-09)

**Phases completed:** 5 phases (20–24), 10 plans, 32 tasks

**Key accomplishments:**

- Brand & identity: new CAR-curve logo + hex sticker, full favicon set + Open Graph card, pkgdown palette/typography/card-gallery aligned to the eventstudy.de ecosystem, lifecycle badge promoted `experimental` → `stable`.
- Plot aesthetics: new exported `theme_eventstudy()` + colorblind-safe Okabe-Ito `es_colours` palette (`R/theme.R`), applied across all ggplot2 helpers and plotly visuals, removing scattered hardcoded colours.
- Report aesthetics: `es_report()` tables via `tinytable` (byte-compatible `knitr::kable` fallback), captioned figures, per-format figure sizing via `ragg`, and a brand-tokened HTML-only `report.css` — the prose sanitiser, grounding guard, and `is_html_output()` switch left untouched.
- API & message polish: `print.*` returns `invisible(x)`, `format.*` methods added where missing, selected errors/warnings migrated to classed `eventstudy_*` rlang conditions naming the offending argument, and a `verbose=` quiet mode with a byte-identical default.
- Docs & site: `@family`/`@seealso` cross-links across the exported API, README Ecosystem section + honest 15+/12 counts, `pkgdown::check_pkgdown()` in CI, and a tightened getting-started vignette.

**Core invariant held:** Purely additive overlay — zero statistical-correctness or valid-input behavior change. Only optional Suggests (`tinytable`, `patchwork`, `ragg`, all `requireNamespace()`-guarded) added; no new Imports.

**Milestone audit:** PASSED (30/30 requirements, 5/5 phases, 5/5 integration seams, 5/5 flows). Test suite green (2359 pass / 0 fail); zero new `R CMD check` findings vs the documented 1-NOTE baseline.

**Closeout:** override_closeout. Known verification overrides: 2 newly acknowledged, 2 carried forward from a prior close (see STATE.md Deferred Items). Newly acknowledged: Phase 22 `22-VERIFICATION.md` felt-visual-quality eyeball (`status: human_needed` — full-TeX-Live/tabularray.sty environment limitation, not a code gap); Phase 24 deferred test-artifact-hygiene item (report-render tests emit transient `file*.log` into `skeleton/`, not gitignored).

**Release:** Annotated tag `v0.65.0` created locally (not pushed). CRAN submission not performed.

---

## v0.64.0 Automated AI Reporting (Shipped: 2026-09-07)

**Phases completed:** 4 phases, 9 plans, 11 tasks

**Key accomplishments:**

- Section-by-section grounded narrative assembler (one LLM call per section, never per format), static significance calibrator, KB citation extractor, joint-hypothesis caveat, and per-format prose sanitiser -- the complete NARR-01..05 / FORMAT-04 / OFFLINE-02 narrative-assembly layer for Phase 18
- Multi-format generate_report() loop (html/pdf/word/md, named-vector return), fixed 6-section skeleton.Rmd with knitr::is_html_output() plot switching, deterministic task/diagnostics tables, AI-vs-offline heading labels, and joint-hypothesis caveat
- Public `es_report()` wrapper that deep-clones the task, delegates narrative+render to `generate_report()`, and returns path(s) visibly.
- Additive `report=FALSE`/`report_args=list()` params on `run_event_study()` with byte-identical FALSE path guarded by `isTRUE(report)` and a `do.call(es_report, ...)` render path that attaches `attr(task, "report_path")` and emits one `message()`
- NAMESPACE exports es_report, DESCRIPTION bumped to 0.64.0, NEWS/README updated, CRAN check 0 errors 0 warnings 1 pre-existing note -- release gate HUMAN-APPROVED, v0.64.0 signed off

**Milestone audit:** PASSED (22/22 requirements). One critical gap found at audit — the free-text prose grounding guard (GROUND-01/02/03) was built in Phase 17 but never wired into the report narrative assembler — was closed by gap-closure Phase 19.1 (prose scanner wired into `assemble_report_narrative()`; report-path invariant locked by `test_prose_grounding_report_path.R`).

**Closeout:** override_closeout. Known verification overrides: 2 newly acknowledged, 0 carried forward from a prior close (see STATE.md Deferred Items) — both are stale Phase 12 carryforwards from the already-shipped v0.62.0 milestone (a `12-VERIFICATION.md` human-needed gap and a low-priority code-review todo), not v0.64.0 work.

**Release:** Tagged `v0.64.0` and published as a GitHub release (2026-09-07). CRAN submission not performed.

---

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
