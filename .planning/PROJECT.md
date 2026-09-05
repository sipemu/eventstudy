# EventStudy — Grounded AI Advisor

## What This Is

EventStudy is a comprehensive R package (CRAN) for financial event study analysis. It provides a composable R6 pipeline — `prepare_event_study()` → `fit_model()` → `calculate_statistics()` — with 13+ return models (Market, Fama-French 3/5, Carhart 4, GARCH, DCC-GARCH, Rolling-Window, BHAR, Volume, Volatility, Comparison-Period-Mean, Custom), 8+ test statistics (AR/CAR t-tests, Patell Z, BMP, Sign, Kolari-Pynnönen, Calendar-Time Portfolio, Cross-Sectional t), and specialized task types (panel DiD, intraday, synthetic control), plus bootstrap inference, cross-sectional regression, diagnostics, power simulation, and CSV/Excel/LaTeX export.

The package is mature and CRAN-published. The v0.50.0 milestone made it **never silently wrong** on degenerate input (documented contract + regression net). This milestone builds the next layer up: an **LLM-agnostic AI advisor** that guides a user through an entire event study and interprets *only* package-computed numbers — never fabricating a result.

## Core Value

The package must never produce a silently incorrect statistical result — and now, the AI layer must never present an ungrounded one. On degenerate input the pipeline errors clearly or returns NA with one warning; the advisor cites only diagnostics the package actually computed, and refuses to invent numbers. Trustworthy numbers, trustworthy interpretation.

## Current Milestone: v0.63.0 Documentation Depth — Methods & Worked Examples

**Goal:** Transform the EventStudy docs site from a grouped reference into a pyfda-caliber learning resource — every method family explained with real statistical exposition and academic references, every method demonstrated with rendered outputs (tables, printed results, plots), plus a cross-domain gallery of complete worked examples. Reference standard: `https://sipemu.github.io/pyfda/`.

**Target features:**
- **Learn/Methods article section** — one conceptual article per method family (return models · test statistics · panel DiD · intraday · synthetic control · AI advisor · diagnostics), each with formulas, assumptions, when-to-use guidance, and academic references
- **Rendered outputs inline** — each Methods article runs real code and renders tables, printed results, and plots (build-time executed, fully offline/CI-safe)
- **Cross-domain worked-examples gallery** — pyfda-style gallery of end-to-end workflows across domains (earnings surprises, M&A, regulatory shocks, …), each a complete rendered analysis
- **Curated real datasets per domain** — new bundled datasets (dieselgate-style, with `data-raw/` provenance) powering the gallery, sized so the CRAN tarball stays clean
- **pkgdown-only delivery** — the new Methods articles + gallery live in `vignettes/articles/` (`.Rbuildignore`d): they render on the site but stay out of the CRAN tarball; the existing 18 concise vignettes remain CRAN-shipped and untouched
- **CRAN-/CI-clean build** — the new articles render in the existing v0.62.0 CI pkgdown build fully offline; no new `R CMD check` NOTEs/WARNINGs; tarball not bloated; valid-input behavior unchanged

## Business Context

- **Customer**: Applied finance/econometrics researchers and quant practitioners running event studies in R
- **Revenue model**: Freemium — the bundled advisor (curated reference grounding) ships free/open-source; a future retrieval-grounded "Advisor Pro" (full literature corpus, RAG) is a paid tier
- **Success metric**: Advisor Pro waitlist signups — a demand signal gathered before the heavier RAG version is built
- **Strategy notes**: Advisor Pro does not exist yet; the waitlist validates commercial demand before investment. Nothing in this milestone commits to building it.

## Requirements

### Validated

<!-- Shipped and confirmed. Inferred capabilities + delivered milestones. -->

- ✓ Core pipeline: prepare → fit → calculate_statistics via `run_event_study()` — existing
- ✓ 13+ return models under a common `ModelBase` interface — existing
- ✓ Single-event (AR/CAR) and multi-event (AAR/CAAR, Patell, BMP, Sign, KP, CSect) test statistics — existing
- ✓ Panel DiD estimators (TWFE, Sun-Abraham, Callaway-Sant'Anna, BJS, de Chaisemartin-D'Haultfoeuille) — existing
- ✓ Intraday event studies (POSIXct, minute/second windows) — existing
- ✓ Synthetic control method — existing
- ✓ Bootstrap inference, cross-sectional regression, diagnostics, power simulation — existing
- ✓ CSV/Excel/LaTeX export, broom-compatible tidy(), RMarkdown `generate_report()` — existing
- ✓ testthat 3e suite (1378 tests) with mock-data helpers and `test_edge_cases.R` — existing
- ✓ **Degenerate-input contract** (`?degenerate-input-contract`, `R/contract.R`) with configurable strict/lenient handling across all 13 return models and 8+ test statistics — v0.50.0
- ✓ **Hardened prepare/window logic, export/tidy NA-safety, cross-sectional collinearity guards** — v0.50.0
- ✓ **Defensively wrapped external-package call sites** (did, DIDmultiplegt, didimputation, sandwich, rugarch, rmgarch, synthetic-control solve.QP) — v0.50.0
- ✓ **Durable regression net**: 25-component contract matrix, fix→test catalog, green `R CMD check` (0 new NOTEs/WARNINGs) — v0.50.0
- ✓ **Offline diagnostics layer** — deterministic zero-dependency `es_diagnostics()` harvester — v0.60.0
- ✓ **Grounded advise layer** — `es_advise()` `Advice` object with runtime grounding guard, all six advice modes, `generate_report()` integration — v0.60.0
- ✓ **LLM-agnostic provider abstraction** — hand-rolled thin `httr2` client (OpenAI-compatible + Anthropic + custom hook), 3-tier precedence, Suggests-guarded — v0.60.0
- ✓ **Grounding knowledge base** — pure-R assumption→test KB with academic citations, rule-based offline advice engine — v0.60.0
- ✓ **Claude Code Agent Skill + Advisor Pro waitlist** — `SKILL.md` loop over existing exports; CRAN-safe opt-in waitlist surface — v0.60.0
- ✓ **Advisor vignette + bundled multi-automaker dieselgate dataset** — offline-safe CRAN vignette, CI bands + group CAAR comparison, `data-raw/` provenance — v0.61.0
- ✓ **Curated pkgdown site + custom Bootstrap-5 theme** — thematically grouped Reference index, Articles nav over the 18 vignettes, README homepage, News/Changelog, custom theme (no logo) — v0.62.0
- ✓ **CI/CD docs deploy + repo linkage + release integrity** — r-lib `pkgdown.yaml` deploy to `gh-pages`/GitHub Pages, DESCRIPTION `URL` + README docs badge + `.Rbuildignore`, network-safe article build, CRAN-clean 0.62.0 release — v0.62.0

### Active

<!-- This milestone (v0.63.0). Detailed, testable REQ-IDs live in REQUIREMENTS.md. -->

- [ ] **Learn/Methods article section** — one conceptual article per method family (return models, test statistics, panel DiD, intraday, synthetic control, AI advisor, diagnostics) with formulas, assumptions, when-to-use, and academic references
- [ ] **Rendered outputs inline** — each Methods article executes at build time and renders tables, printed results, and plots, fully offline/CI-safe
- [ ] **Cross-domain worked-examples gallery** — pyfda-style gallery of complete rendered workflows across domains
- [ ] **Curated real datasets per domain** — new bundled datasets (dieselgate-style, `data-raw/` provenance) powering the gallery, sized to keep the CRAN tarball clean
- [ ] **pkgdown-only delivery** — new Methods articles + gallery in `vignettes/articles/` (`.Rbuildignore`d); render on site, stay out of the CRAN tarball; existing 18 vignettes untouched
- [ ] **CRAN-/CI-clean build** — new articles render in the existing CI pkgdown build offline; no new `R CMD check` NOTEs/WARNINGs; tarball not bloated

### Out of Scope

- **MCP server surface** — deferred; the Agent Skill delivers the full loop with less surface area. MCP can follow if agentic multi-tool demand appears.
- **Full retrieval-corpus (RAG) advisor** — this *is* the commercial "Advisor Pro"; not built this milestone, validated via waitlist first.
- **Native provider SDK dependencies** — HTTP-only via `httr2`; no heavy per-provider SDKs, to keep the dependency surface small and CRAN-clean.
- **Model fine-tuning / bespoke models** — the advisor orchestrates existing hosted/local LLMs; no training.
- **Native reimplementation of external estimators** (did, DIDmultiplegt, rugarch) — separate independence milestone.
- **Performance/scaling work** (streaming, data.table backend, sparse FE) — orthogonal; deferred.
- **Changing the statistical intent of any existing method** — behavior on valid input stays unchanged.
- **New statistical methods or estimators** — v0.63.0 is documentation depth only; no new models, tests, or task types.
- **Rewriting the existing 18 CRAN vignettes** — they stay as concise CRAN-shipped vignettes; the new rich content is additive and pkgdown-only.
- **Shipping the rich Methods articles/gallery inside the CRAN tarball** — deliberately pkgdown-only to keep the tarball small and `R CMD check` fast.

## Context

- **Mature brownfield CRAN package.** Codebase map complete in `.planning/codebase/`. R 4.1.0+, R6 OOP, tidyverse pipeline. `cran-comments.md`, `NEWS.md` present.
- **The advisor rides the pyfda/fdars pattern** (github.com/sipemu/pyfda): a deterministic offline `build_diagnostics` layer + a grounded `advise` layer over a uniform provider protocol, with the hard invariant that the LLM interprets only computed diagnostics. EventStudy adapts this: `es_diagnostics()` + `es_advise()`, provider precedence arg→env→default, and a grounding runtime guard.
- **v0.50.0 is the foundation.** The degenerate-input contract's `is_fitted` flags, NA discipline, and zero-variance/insufficient-obs signals become structured diagnostic inputs — the advisor can *flag robustness issues* directly from contract state.
- **Existing diagnostics to reuse:** `R/diagnostics.R` (Shapiro-Wilk, Durbin-Watson, Ljung-Box, pre-trend). Existing report path: `R/report.R` `generate_report()` — report-writing help drafts grounded narrative into it.
- **CRAN dependency discipline is non-negotiable.** The pattern from v0.50.0 (optional packages in Suggests, `requireNamespace()`-guarded) applies to every AI dependency; the offline layer must add zero hard deps.

## Constraints

- **Tech stack**: R 4.1.0+, R6, testthat 3e — no new language or framework; the advisor stays within the existing stack (+ `httr2`/`jsonlite` as Suggests)
- **Compatibility**: Behavior on existing valid inputs must not change — the advisor is additive; existing 1378 tests stay green
- **CRAN**: No new `R CMD check` NOTEs/WARNINGs; AI/HTTP dependencies stay in Suggests, guarded by `requireNamespace()`; offline diagnostics add no dependency
- **Grounding**: The LLM must never present a number absent from the diagnostics; enforced by schema + system prompt + a runtime guard, and covered by regression tests
- **Provider-agnostic**: No hard-coding to one vendor; provider selection is the single configuration seam (arg → env var → default)
- **No secrets in the package**: API keys come only from the user's environment; never bundled, logged, or committed

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Configurable strict vs lenient degenerate-input handling | Serves fail-fast interactive use and NA-tolerant batch runs | ✓ v0.50.0 |
| Zero-variance guard scoped to variance-dependent models only | Arithmetic models produce valid abnormal returns on constant input; degenerate handling for them lives at the test-statistic layer | ✓ v0.50.0 |
| Harden external-package areas defensively (wrap, don't reimplement) | Full coverage without taking on upstream maintenance | ✓ v0.50.0 |
| Acceptance bar = regression test per fix + contract matrix + green R CMD check | Converts recurring audit churn into a durable net | ✓ v0.50.0 |
| Two-layer advisor: deterministic offline diagnostics + grounded LLM advise (pyfda/fdars pattern) | Offline layer is always available and testable; LLM only interprets computed numbers — a direct extension of "never silently wrong" | — Pending |
| Grounding invariant enforced by a runtime guard | Schema + prompt alone can't guarantee no fabrication; the guard rejects any evidence citing values absent from the diagnostics | — Pending |
| LLM layer optional (Suggests: `httr2`/`jsonlite`, `requireNamespace()`-guarded); offline layer pure base R | Preserves CRAN cleanliness and no-API-key usability | — Pending |
| Provider abstraction = OpenAI-compatible + Anthropic + custom hook (not four native SDKs) | Fewest code paths to test; Ollama/gateways covered by the OpenAI-compatible endpoint | — Pending |
| Freemium: bundled advisor free, retrieval-corpus "Advisor Pro" as a future paid tier gated by a waitlist | Validate commercial demand before building the heavier RAG version | — Pending |
| Agent Skill surface now, MCP server deferred | The Skill delivers the full loop with less surface area; MCP can follow if agent demand appears | — Pending |
| Defer performance/scaling and native reimplementation | Real but orthogonal to this milestone | — Pending |
| Curated pkgdown site (grouped reference, custom homepage) over auto-generated default | Matches the fdars-r reference; a flat index over 30+ exports and 18 vignettes is not discoverable | ✓ v0.62.0 |
| Custom Bootstrap-5 theme, no logo | Professional look without needing artwork; logo can follow later | ✓ v0.62.0 |
| CI deploy to gh-pages on push-to-main + releases (r-lib `pkgdown.yaml`) | Always-current docs; standard, well-supported r-lib workflow; keeps site out of the CRAN tarball | ✓ v0.62.0 |
| New Methods articles + gallery as pkgdown-only (`vignettes/articles/`, `.Rbuildignore`d), not CRAN vignettes | Full-coverage rendered content would bloat the tarball and slow `R CMD check`; site-only keeps CRAN clean while web users get the rich content; existing concise vignettes stay CRAN-shipped | — Pending |
| Bundle curated real per-domain datasets (dieselgate-style, `data-raw/` provenance) for the gallery | Realistic worked examples beat synthetic-only; matches the pyfda examples standard | — Pending |
| Datasets used only by site-only articles must not bloat the CRAN tarball — compressed if shipped as documented `data()` datasets, or `.Rbuildignore`d if purely site-only | Full coverage across domains risks tarball growth; per-dataset placement decided in planning | — Pending |
| Reference standard for docs depth = pyfda site (conceptual method pages + worked-examples gallery + rendered outputs) | Same author/pattern family as the advisor's pyfda/fdars grounding; concrete, agreed bar for "thorough" | — Pending |
| All rendered articles execute fully offline (bundled data + `set.seed`), no network at build time | Must render in the existing v0.62.0 CI pkgdown build without flakiness; extends the v0.62.0 network-safe-article decision | — Pending |

## Evolution

This document evolves at phase transitions and milestone boundaries.

**After each phase transition** (via `/gsd-transition`):
1. Requirements invalidated? → Move to Out of Scope with reason
2. Requirements validated? → Move to Validated with phase reference
3. New requirements emerged? → Add to Active
4. Decisions to log? → Add to Key Decisions
5. "What This Is" still accurate? → Update if drifted

**After each milestone** (via `/gsd-complete-milestone`):
1. Full review of all sections
2. Core Value check — still the right priority?
3. Business Context check — customer, revenue model, success metric still accurate?
4. Audit Out of Scope — reasons still valid?
5. Update Context with current state

---
*Last updated: 2026-09-05 starting milestone v0.63.0 Documentation Depth — Methods & Worked Examples*
