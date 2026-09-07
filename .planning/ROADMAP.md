# Roadmap: EventStudy — Automated AI Reporting

## Milestones

- ✅ **v0.50.0 Robustness Hardening** — Phases 1-4 (shipped 2026-09-02)
- ✅ **v0.60.0 Grounded AI Advisor** — Phases 5-8 (shipped 2026-09-04)
- ✅ **v0.61.0 Advisor Vignette + Dieselgate Walkthrough** — Phases 9-10 (shipped 2026-09-04)
- ✅ **v0.62.0 Documentation Site (pkgdown + CI/CD)** — Phases 11-12 (shipped 2026-09-06)
- ✅ **v0.63.0 Documentation Depth — Methods & Worked Examples** — Phases 13-16 (shipped 2026-09-06)
- 🚧 **v0.64.0 Automated AI Reporting** — Phases 17-19 (in progress)

## Phases

**Phase Numbering:**

- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Numbering is continuous across milestones. Full per-phase detail for shipped
milestones lives in `.planning/milestones/v{X.Y}-ROADMAP.md`.

<details>
<summary>✅ v0.50.0 Robustness Hardening (Phases 1-4) — SHIPPED 2026-09-02</summary>

- [x] **Phase 1: Contract Foundation** — degenerate-input contract + strict/lenient mode (completed 2026-09-02)
- [x] **Phase 2: Model and Stats Sweep** — contract applied across all 13 models + all test statistics (completed 2026-09-02)
- [x] **Phase 3: Pipeline and External Hardening** — hardened prepare/export + wrapped external-package areas (completed 2026-09-02)
- [x] **Phase 4: Regression Net and Check Gate** — per-fix regression tests + contract matrix + green R CMD check (completed 2026-09-02)

Archive: `.planning/milestones/v0.50.0-ROADMAP.md`

</details>

<details>
<summary>✅ v0.60.0 Grounded AI Advisor (Phases 5-8) — SHIPPED 2026-09-04</summary>

**Milestone Goal:** Add an LLM-agnostic AI advisor that guides users through an entire event study — grounded so it interprets only package-computed numbers and never fabricates results.

- [x] **Phase 5: Offline Diagnostics + Grounding Knowledge Base** — deterministic zero-dependency `es_diagnostics()`, pure-R assumption→test KB, non-LLM rule-based advice fallback (completed 2026-09-03)
- [x] **Phase 6: Provider Abstraction + CRAN-Safe HTTP Harness** — LLM-agnostic `AdvisorProvider` R6 hierarchy (Anthropic, OpenAI-compatible, custom), 3-tier precedence, graceful degradation, offline-tested (completed 2026-09-04)
- [x] **Phase 7: Grounded Advise Layer + Grounding Guard** — `es_advise()` `Advice` object with runtime grounding guard, all six advice modes, `generate_report()` integration (completed 2026-09-04)
- [x] **Phase 8: Agent Skill + Waitlist + Green Check Gate** — Claude Code Agent Skill, "Advisor Pro" waitlist surface, green `R CMD check --as-cran` with suite green (completed 2026-09-04)

Archive: `.planning/milestones/v0.60.0-ROADMAP.md` · Audit: `.planning/milestones/v0.60.0-MILESTONE-AUDIT.md`

</details>

<details>
<summary>✅ v0.61.0 Advisor Vignette + Dieselgate Walkthrough (Phases 9-10) — SHIPPED 2026-09-04</summary>

**Milestone Goal:** Ship a CRAN vignette that explains the AI advisor's idea and mechanics, anchored by a real Volkswagen dieselgate worked example, and align the package's other doc entry points around that story.

- [x] **Phase 9: Bundled Dieselgate Dataset + Provenance** — reproducible `data-raw/` fetch of VW + benchmark returns around the Sept 2015 EPA disclosure, frozen into `data/` with documented provenance and `.Rd`, proven to drive a valid end-to-end event study (completed 2026-09-04)
- [x] **Phase 10: Advisor Vignette + Offline-Safe Build + Docs + Release** — the two-layer advisor vignette (deterministic layer live, LLM layer static/labelled) on the bundled dieselgate data, offline-safe build, aligned README/pkgdown/NEWS, and a CRAN-clean 0.61.0 release (completed 2026-09-04)

Archive: `.planning/milestones/v0.61.0-ROADMAP.md`

</details>

<details>
<summary>✅ v0.62.0 Documentation Site (pkgdown + CI/CD) (Phases 11-12) — SHIPPED 2026-09-06</summary>

**Milestone Goal:** Ship a curated, professionally-themed pkgdown documentation website for EventStudy — auto-built and deployed to GitHub Pages by CI — and link it prominently from the repo, matching the `fdars-r` documentation pattern.

- [x] **Phase 11: Curated pkgdown Site + Custom Theme (local build)** — `_pkgdown.yml` with grouped reference index, organized Articles nav over the 18 vignettes, README homepage, custom Bootstrap-5 theme, verified with a clean local `build_site()` (completed 2026-09-04)
- [x] **Phase 12: CI/CD Deploy + Repo Linkage + Release Integrity** — r-lib `pkgdown.yaml` workflow deploying to gh-pages, DESCRIPTION URL / README badge / `.Rbuildignore`, network-safe article build, and a CRAN-clean 0.62.0 release (completed 2026-09-06)

Archive: `.planning/milestones/v0.62.0-ROADMAP.md`

</details>

<details>
<summary>✅ v0.63.0 Documentation Depth — Methods & Worked Examples (Phases 13-16) — SHIPPED 2026-09-06</summary>

**Milestone Goal:** Transform the pkgdown site into a pyfda-caliber learning resource — a Learn/Methods track and a cross-domain worked-examples gallery powered by curated real datasets, all pkgdown-only and rendered fully offline in CI with no CRAN regressions.

- [x] **Phase 13: Article Infrastructure & Conventions Gate** — `vignettes/articles/`, shared `_setup.Rmd`, `references.bib`, `math-rendering: katex`, reusable skeleton, Methods/Gallery nav slots (completed 2026-09-05)
- [x] **Phase 14: Curated Per-Domain Datasets** — curated real datasets with reproducible `data-raw/` provenance + `DATA-SOURCES.md`, placed/sized to keep the CRAN tarball clean (completed 2026-09-05)
- [x] **Phase 15: Methods Articles + Rendered Outputs** — 7 formula-bearing Methods articles with build-time-rendered tables + plots, offline, formula-reviewed (completed 2026-09-06)
- [x] **Phase 16: Worked-Examples Gallery + Build & Release Integrity** — pyfda-style gallery + ≥3 cross-domain worked examples, green build/CI/`R CMD check` (completed 2026-09-06)

Archive: `.planning/milestones/v0.63.0-ROADMAP.md`

</details>

### 🚧 v0.64.0 Automated AI Reporting (Phases 17-19)

**Milestone Goal:** Add a single one-call entry point that takes an event study from data to a polished, publication-ready report — running the pipeline, harvesting diagnostics, generating grounded AI narrative, and rendering to the researcher's chosen format(s) — while never presenting an ungrounded number and always producing a full report even with no LLM configured.

- [x] **Phase 17: Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline** — extend the grounding guard to scan free-text narrative prose for ungrounded numeric literals, resolve the `report_writing`-is-LLM-only gap so a full report narrative renders with no provider, add the backward-compatible `narrative=` seam to `generate_report()`, and establish the `\dontrun{}`/`skip_on_cran()`/Suggests CRAN hygiene discipline (completed 2026-09-07)
- [x] **Phase 18: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly** — build the section-by-section grounded narrative assembler and the fixed report template (exec summary · data/methods · results · diagnostics · robustness/caveats · references) rendered to HTML/PDF/Word/Markdown with static-plot fallback, per-format prose sanitisation, graceful toolchain skips, and offline-vs-AI mode visibility (completed 2026-09-07)
- [ ] **Phase 19: One-Call `es_report()` Orchestrator, `run_event_study(report=)` & CRAN-Clean Release Gate** — the public `es_report()` entry point composing study → diagnostics → advise → render (returning output paths, deep-cloning the task), the additive `run_event_study(..., report=)` convenience, and the final CRAN-clean build/release gate verifying no new NOTEs/WARNINGs and a green suite

## Phase Details

### Phase 17: Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline

**Goal**: The milestone's core-value gate lands first — the grounding guard now catches a fabricated number sitting in free-text narrative prose (not just in structured `evidence[]` arrays), the report narrative renders complete with no LLM provider configured (the current `report_writing`-is-LLM-only `stop()` is resolved via the rule-based offline engine), `generate_report()` carries a backward-compatible `narrative=` seam, and the CRAN hygiene discipline for render examples/tests is established up front so no downstream phase can leak a check regression.
**Depends on**: Phase 16 (v0.63.x `es_diagnostics()` / `es_advise()` / grounding guard / `generate_report()` exist)
**Requirements**: GROUND-01, GROUND-02, GROUND-03, OFFLINE-01, REPORT-03
**Success Criteria** (what must be TRUE):

  1. A fabricated numeric literal placed in narrative prose (a number absent from `es_diagnostics()` beyond tolerance) is caught by the grounding guard — the offending section is dropped or flagged with exactly one warning and the unverified number is never emitted into rendered output; a regression test locks this report-path invariant.
  2. With no LLM provider configured, requesting a `report_writing` narrative no longer errors — the rule-based offline advice engine supplies complete narrative content, so a full report can be assembled offline.
  3. `generate_report()` accepts a new `narrative = NULL` parameter, and its existing `narrative = NULL` / `advice = NULL` output is byte-identical to the v0.63.x baseline (backward compatibility proven by a diff test).
  4. Every new `render()`-touching example is wrapped in `\dontrun{}` and every render/toolchain test uses `skip_on_cran()` / `skip_if_not_installed()`; `R CMD check --as-cran` triggers no network access or LaTeX toolchain and shows no new NOTEs/WARNINGs vs the current baseline with the suite green.

**Plans**: 3/3 plans executed

- [x] 17-01-PLAN.md — Tracer: offline `report_writing` narrative engine + `generate_report(narrative=)` seam, one complete offline report rendered end-to-end (OFFLINE-01, REPORT-03)
- [x] 17-02-PLAN.md — Prose grounding scanner: extract numeric literals, check against `es_diagnostics()` registry, drop-and-keep + single warning, rounding/year/constant exemptions (GROUND-01/02/03)
- [x] 17-03-PLAN.md — CRAN hygiene: `tinytex` Suggests, golden-file byte-identical backward-compat test, committed `R CMD check` baseline (REPORT-03)

### Phase 18: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly

**Goal**: A complete, well-structured report renders to any of HTML/PDF/Word/Markdown from the fixed template, its narrative assembled section-by-section as independent grounded requests (LLM contacted once per section, not per format), with data/methods and results content auto-filled from task metadata and diagnostics (never the LLM), format-correct plots and sanitised prose, graceful toolchain degradation, and a visible offline-vs-AI mode distinction.
**Depends on**: Phase 17 (grounding-prose guard, offline `report_writing` fallback, and the `narrative=` renderer seam must exist first)
**Requirements**: NARR-01, NARR-02, NARR-03, NARR-04, NARR-05, FORMAT-01, FORMAT-02, FORMAT-03, FORMAT-04, TMPL-01, TMPL-02, OFFLINE-02
**Success Criteria** (what must be TRUE):

  1. The narrative is assembled section-by-section (executive summary, data/methods, results interpretation, robustness/caveats), each an independent grounded request — the LLM is contacted once per section regardless of how many output formats are produced — with references pulled from the knowledge-base citation records (never LLM-generated), significance language set by a static p-value calibration function, and the fixed joint-hypothesis caveat present in every report.
  2. One fixed template (executive summary · data/methods · results · diagnostics · robustness/caveats · references) renders with section presence toggled by arguments and no custom templating, and the data/methods and results content (windows, model, AR/CAR/AAR/CAAR tables, significance) is auto-filled from task metadata and `es_diagnostics()` keys, not from the LLM.
  3. The same template renders to HTML (default), PDF, Word (.docx), and Markdown, selectable per call (one or several), with plots switching to static ggplot2 for non-HTML formats via `knitr::is_html_output()` (interactive plotly only in HTML) and prose sanitised per format (LaTeX/XML special characters, smart quotes, em-dashes) so PDF/Word render without corruption.
  4. A missing optional toolchain degrades gracefully — PDF/Word/Markdown each skip with exactly one informative message and continue; only total inability to render the HTML baseline errors — and the rendered report visibly distinguishes AI-grounded from offline rule-based narrative (section label/heading plus a console message on the mode used).

**Plans**: 2/2 plans executed
**UI hint**: yes

- [x] 18-01-PLAN.md — Grounded narrative assembler: section-by-section es_advise (once per section), static significance calibration, KB references, joint-hypothesis caveat, per-format prose sanitiser (NARR-01..05, FORMAT-04, OFFLINE-02)
- [x] 18-02-PLAN.md — Multi-format renderer + fixed 6-section template: format-vector loop with toolchain skips, deterministic task/diagnostics auto-fill, is_html_output plot switch, AI-vs-offline mode visibility (FORMAT-01/02/03, TMPL-01/02, NARR-05, OFFLINE-02)

### Phase 19: One-Call `es_report()` Orchestrator, `run_event_study(report=)` & CRAN-Clean Release Gate

**Goal**: The user gets the headline one-call value — `es_report()` takes a fitted task through diagnostics → grounded advise → multi-format render in a single call and returns the output file path(s) without mutating the caller's object, `run_event_study(..., report = TRUE)` offers the same as an additive convenience with no behavior change when omitted, and the whole milestone ships proven clean through a final CRAN/build gate.
**Depends on**: Phase 18 (the multi-format renderer, template, and narrative assembler it orchestrates must exist); Phase 17 (grounding + offline + CRAN discipline it inherits)
**Requirements**: REPORT-01, REPORT-02, REPORT-04, CRAN-01, CRAN-02
**Success Criteria** (what must be TRUE):

  1. A user calls `es_report()` on a fitted `EventStudyTask`; it harvests diagnostics, assembles the narrative, renders the report, and returns the output file path(s) — and the caller's `EventStudyTask` is unchanged afterward (deep-cloned before rendering).
  2. `run_event_study(..., report = TRUE)` produces a report as part of a standard run; the parameter defaults to `FALSE` and omitting it leaves `run_event_study()` behavior byte-identical to the prior release.
  3. New dependencies (`tinytex`) stay in `Suggests`, `requireNamespace()`-guarded; all `render()` calls are `\dontrun{}` in examples and `skip_on_cran()` / `skip_if_not_installed()` in tests, so `R CMD check` triggers no network or LaTeX toolchain.
  4. The final `R CMD check --as-cran` shows no new NOTEs/WARNINGs versus the current baseline, the full existing test suite stays green, and the package is bumped with a NEWS.md `v0.64.0` entry recording the automated reporting feature.

**Plans**: 3/3 plans executed

Plans:

- [x] 19-01-PLAN.md — es_report() orchestrator: one-call study -> diagnostics -> multi-format render, deep-clone non-mutation, visible return path(s) (REPORT-01, REPORT-04)
- [x] 19-02-PLAN.md — run_event_study(report=/report_args=) additive convenience with byte-identical FALSE path and report_path attribute (REPORT-02)
- [x] 19-03-PLAN.md — CRAN release gate: roxygen/NAMESPACE regen, DESCRIPTION 0.64.0, NEWS + README, R CMD check --as-cran clean vs baseline + suite green (CRAN-01, CRAN-02)

## Progress

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 1. Contract Foundation | v0.50.0 | 2/2 | Complete | 2026-09-02 |
| 2. Model and Stats Sweep | v0.50.0 | 4/4 | Complete | 2026-09-02 |
| 3. Pipeline and External Hardening | v0.50.0 | 2/2 | Complete | 2026-09-02 |
| 4. Regression Net and Check Gate | v0.50.0 | 2/2 | Complete | 2026-09-02 |
| 5. Offline Diagnostics + Grounding KB | v0.60.0 | 3/3 | Complete | 2026-09-03 |
| 6. Provider Abstraction + HTTP Harness | v0.60.0 | 3/3 | Complete | 2026-09-04 |
| 7. Grounded Advise Layer + Guard | v0.60.0 | 2/2 | Complete | 2026-09-04 |
| 8. Agent Skill + Waitlist + Check Gate | v0.60.0 | 2/2 | Complete | 2026-09-04 |
| 9. Bundled Dieselgate Dataset + Provenance | v0.61.0 | 1/1 | Complete | 2026-09-04 |
| 10. Advisor Vignette + Offline-Safe Build + Docs + Release | v0.61.0 | 1/1 | Complete | 2026-09-04 |
| 11. Curated pkgdown Site + Custom Theme (local build) | v0.62.0 | 1/1 | Complete | 2026-09-04 |
| 12. CI/CD Deploy + Repo Linkage + Release Integrity | v0.62.0 | 1/1 | Complete | 2026-09-06 |
| 13. Article Infrastructure & Conventions Gate | v0.63.0 | 1/1 | Complete | 2026-09-05 |
| 14. Curated Per-Domain Datasets | v0.63.0 | 1/1 | Complete | 2026-09-05 |
| 15. Methods Articles + Rendered Outputs | v0.63.0 | 1/1 | Complete | 2026-09-06 |
| 16. Worked-Examples Gallery + Build & Release Integrity | v0.63.0 | 1/1 | Complete | 2026-09-06 |
| 17. Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline | v0.64.0 | 3/3 | Complete    | 2026-09-07 |
| 18. Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly | v0.64.0 | 2/2 | Complete    | 2026-09-07 |
| 19. es_report() Orchestrator, run_event_study(report=) & CRAN-Clean Release Gate | v0.64.0 | 3/3 | In Progress|  |
