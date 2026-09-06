# Research Synthesis: v0.64.0 Multi-Format AI Report Wrapper

**Milestone:** EventStudy v0.64.0 — One-Call Automated AI Reporting  
**Researched:** 2026-09-06  
**Synthesis Confidence:** MEDIUM-HIGH  
**Last Updated:** 2026-09-06

---

## Executive Summary

EventStudy v0.64.0 adds a one-call entry point, `es_report()`, that orchestrates the existing diagnostics and advice pipelines into a complete, multi-format automated report with grounded AI narrative sections. The milestone is **additive and backward-compatible**: all existing functions remain unchanged; `es_report()` composes them without breaking existing callers.

The core innovation is a **multi-section narrative architecture** where each report section (executive summary, data/methods, results, robustness/caveats) is an independent LLM request guarded by the existing grounding guard, or an offline KB-based recommendation when no LLM provider is available. This ensures the report is never blocked by a missing API key and never produces a silently incorrect statistical claim.

The critical architectural challenge is **grounding coverage for prose sections**. The existing guard validates `evidence[]` arrays in structured recommendations but does not scan free-text `interpretation` fields for fabricated numbers. The multi-section narrative amplifies this surface — four independent prose fields, each a hallucination risk. Pitfalls research identifies this as the make-or-break invariant that must be solved in Phase 1 before any template work begins.

Implementation follows a proven pattern: four rendering phases, each independently testable. Phase 1 establishes the grounding architecture and CRAN hygiene. Phase 2 implements multi-format rendering with static-plot fallbacks. Phases 3–4 complete offline-first validation and advanced format support.

---

## Key Findings

### From STACK.md

**Recommended Stack (No New Package Dependencies Required)**

| Component | Decision | Rationale |
|-----------|----------|-----------|
| **Report rendering** | `rmarkdown` (2.32) + `knitr` (1.52) — already in Suggests | All four output formats (HTML, PDF, Word, Markdown) are built-in; adding new packages would violate CRAN discipline |
| **HTML output** | `rmarkdown::html_document(toc=TRUE, toc_float=TRUE, theme="flatly", code_folding="hide")` | Zero extra dependencies; self-contained HTML compatible with GitHub rendering |
| **PDF output** | `rmarkdown::pdf_document(toc=TRUE)` with system LaTeX (TinyTeX or TeX Live) | Minimal dependencies; system LaTeX is user-environment concern, not package-level |
| **Word output** | `rmarkdown::word_document(toc=TRUE, reference_docx="default")` | Pandoc handles DOCX natively; rich table support not required for statistical results |
| **Markdown output** | `rmarkdown::md_document(variant="gfm")` | GFM variant is readable on GitHub; useful for wiki/documentation embedding |
| **LaTeX toolchain detection** | `tinytex::is_tinytex()` (optional, add to Suggests only) | Optional supplement to `Sys.which("pdflatex")`; catches TinyTeX installs with PATH-invisible setups |
| **Plotting library (format-specific)** | Plotly (interactive) for HTML; ggplot2 (static) for PDF/Word/Markdown | Leverages existing plot infrastructure; avoids webshot2/chromote heavy chain for static |

**Key Version Constraints**

- **pandoc >= 2.8:** Required by rmarkdown; already implicit in existing `generate_report()`
- **R >= 4.1.0:** Existing package constraint; unchanged
- **rmarkdown >= 2.14:** `html_document(code_folding)` and `md_document(variant="gfm")` stable; `params` passthrough via `render()` mature
- **knitr >= 1.37:** `params` passing in `render()` stable; no newer features required

**Critical Cross-Research Disagreement (Recorded as Open Decision)**

STACK.md recommends `rmarkdown::word_document()` (plain, zero new deps) while PITFALLS.md suggests `officedown::rdocx_document()` for rich table/image support. **Recommendation: Use plain `word_document()`** for v0.64.0. EventStudy statistical reports use simple tables + plots; rich Word features are not required. Aligns with hard CRAN-discipline constraint (no new deps). Defer officedown upgrade to v0.65.0 if demand for advanced Word formatting emerges.

**What NOT to Add**

| Package | Why Excluded |
|---------|-------------|
| `officedown` | Word format works via `rmarkdown::word_document()`; officedown adds officer, rvg, xml2, uuid, memoise (~6 transitive deps) for features not needed |
| `tinytex` in Imports | PDF is optional; users with system LaTeX should not pay a tinytex dependency |
| `webshot2` / `chromote` | Plotly → PNG fallback for static formats requires system Chrome; use ggplot2 static plots instead |

---

### From FEATURES.md

**Table Stakes — Must Ship in v0.64.0**

| Feature | Why Critical |
|---------|-------------|
| **One-call `es_report()` entry point** | Researchers currently orchestrate `es_diagnostics()` → `es_advise()` → `generate_report()` manually |
| **Offline-first rendering** | Report must complete with no LLM provider; researchers without API keys deserve full reports |
| **Executive summary + AI interpretation** | Differentiator vs. competitor tools; every academic report needs an abstract |
| **Data/Methods auto-fill section** | Peer reviewers demand reproducibility; zero LLM needed |
| **CAAR results table + plot** | The headline deliverable of any event study |
| **Robustness/caveats section** | Distinguishes trustworthy reports from p-hacking exercises |
| **Grounding guard visible in footer** | Academic reproducibility demands knowing if AI narrative is grounded |
| **HTML format (default)** | Safe default; zero extra toolchain required |
| **Multi-format output (HTML, PDF, Word, Markdown)** | Different audiences, different needs |
| **Section toggle args** | Different contexts need different subsets |

**Differentiators**

- Grounded AI narrative with uncertainty hedging (names actual values, hedging language)
- Automatic test-statistic recommendation callout (KB-driven methodology guidance)
- Degenerate-event disclosure (prevents silent failures)
- Pre-trend diagnostic integration (flags most common reason for reviewer rejection)
- Significance-star table convention disclosure (prevents misreading across fields)

**Critical Dependency: Offline Fallback for `report_writing`**

Current status: `report_writing` task type is `LLM_ONLY_TYPES` — requires provider. **Recommendation for v0.64.0:** Accept empty Advice objects when provider is absent. Offline mode renders complete statistical report with robustness section from KB; AI prose sections show "Not Available" placeholder. This is pragmatic and unblocks v0.64.0. Rule-based narrative templates deferred to v0.64.x enhancement.

---

### From ARCHITECTURE.md

**System Structure: Pure Composition**

```
es_report(task, provider, formats, sections, ...)
    ├── Step 1: Ensure task is fitted [via run_event_study()]
    ├── Step 2: Harvest diagnostics [es_diagnostics(task)]
    ├── Step 3: Assemble narrative [.assemble_report_advice() x4]
    │   ├── es_advise(diag, "report_writing", provider) → Advice (exec summary)
    │   ├── es_advise(diag, "report_writing", provider) → Advice (data/methods)
    │   ├── es_advise(diag, "report_writing", provider) → Advice (results)
    │   └── es_advise(diag, "flag_robustness", provider) → Advice (robustness)
    └── Step 4: Render per format [generate_report() x N formats]
        ├── generate_report(format="html", narrative=narrative_list) → .html
        ├── generate_report(format="pdf", narrative=narrative_list) → .pdf
        ├── generate_report(format="word", narrative=narrative_list) → .docx
        └── generate_report(format="markdown", narrative=narrative_list) → .md
```

**Build Order (Dependency-Respecting)**

1. Extend `generate_report()` + `skeleton.Rmd` (add `narrative=` param, format dispatch)
2. Implement `.assemble_report_advice()` (internal orchestrator for multi-section advise calls)
3. Implement `es_report()` (public API; composes Steps 1-2)
4. Add optional `report=` param to `run_event_study()` (convenience; lowest priority)
5. Regression tests (grounding invariant, offline path, backward-compat)

**Key Architectural Patterns**

1. **Four-step orchestration:** Independent, sequentially executable; each step can fail gracefully
2. **Multi-section narrative via repeated `es_advise()` calls:** One call per section; section-level guard drops are isolated
3. **Offline-first fallback:** When `provider=NULL`, LLM-only sections return `.empty_advice()`; KB sections use deterministic rule engine
4. **Multi-format via parameterized single template:** One `skeleton.Rmd`, N formats; format dispatch via rmarkdown's `output_format=` argument
5. **Grounding guard preservation:** Every `Advice` object reaching the template is post-guard

---

### From PITFALLS.md

**Critical Pitfalls: Must Prevent Before Phase 2**

| Pitfall | Severity | Phase | Prevention |
|---------|----------|-------|-----------|
| **Grounding guard does not protect prose in `interpretation` field** | CRITICAL | Phase 1 | Extend `.validate_grounding()` to scan free-text fields for numeric literals; cross-check each against diagnostics |
| **CRAN R CMD check triggered by rendering in `@examples`** | CRITICAL | Phase 1 | Wrap all `es_report()` / `generate_report()` examples in `\dontrun{}`; add CI matrix job without optional packages |
| **PDF rendering via tinytex breaks in CI/CRAN environments** | HIGH | Phase 2 | Default to HTML only; guard PDF render; skip PDF tests with `skip_on_cran()` |
| **Plotly in static formats (PDF/Word) renders blank without webshot2** | HIGH | Phase 2 | Use `knitr::is_html_output()` to switch: plotly for HTML, ggplot2 static for PDF/Word/Markdown |
| **Non-ASCII characters in LLM-generated prose break LaTeX/XML** | MEDIUM | Phase 2 | Sanitise all LLM text: replace `&`, smart quotes, em-dashes; use `xelatex` instead of `pdflatex` |
| **Offline fallback silently degrades without visible distinction** | MEDIUM | Phase 1+3 | Inspect `advice$is_deterministic`; use distinct headings; emit console message stating which mode was used |
| **Multi-section prose grounding gaps** | HIGH | Phase 1 | Extend guard to cover all prose sections with numeric scanner + section_evidence arrays |
| **DESCRIPTION Suggests boundary violated** | MEDIUM | Phase 1+2 | Guard every new package call with `requireNamespace()`; add CI matrix running `--no-suggests` |
| **R6 task object mutated via render() reference semantics** | MEDIUM | Phase 2 | Deep-clone task before passing to render: `task$clone(deep=TRUE)` |
| **knitr figure directory deletion in multi-format sequential renders** | MEDIUM | Phase 2 | Use unique `output_dir` per format or coordinate via `render(output_format="all")` |

**Open Decisions (Flag for Planning Discussion)**

| Decision | Options | Recommendation |
|----------|---------|----------------|
| **Word output: use `rmarkdown::word_document()` or `officedown::rdocx_document()`?** | A: Plain word_document (0 new deps) vs B: officedown (adds ~6 transitive deps) | Choose A. EventStudy reports use simple tables; rich Word features not required. Aligns with CRAN discipline. |
| **Prose numeric grounding: scan free-text in Phase 1 or defer?** | A: Phase 1 (blocking) vs B: Phase 1 manual discipline + Phase 2 scanner | Choose A. "Never silently wrong" invariant is non-negotiable. Implement scanner in Phase 1 before any LLM calls. |
| **Multi-format render strategy: per-format loop or single `render("all")` call?** | A: Loop (programmatic) vs B: Single call (coordinates internally) | Choose A with testing. Loop is caller-controlled; MUST test multi-format combination to catch figure-directory deletion bug. Add regression test in Phase 2. |

---

## Implications for Roadmap

### Phase 1: Grounding Architecture & CRAN Hygiene (High Risk, Blocking)

**Deliverables:**

1. Extend `.validate_grounding()` to cover prose fields (numeric literal scanner + cross-check)
2. Define multi-section JSON schema for `report_writing` with per-section evidence arrays
3. Implement offline fallback for `report_writing` (Option 2: empty Advice when no provider)
4. CRAN check hygiene scaffolding (`\dontrun{}` on all examples; CI matrix without optional packages)
5. Add `narrative=NULL` parameter to `generate_report()` (backward-compatible)

**Testing Gates:**
- Grounding guard successfully drops/appends caveats for fabricated numbers in prose
- Offline fallback renders complete statistical report with empty AI sections + placeholder
- CRAN check passes on minimal CI runner without optional packages
- All existing `generate_report()` callers produce identical output

**Why First:** Grounding invariant is non-negotiable. If not solid before template work, hallucinated numbers leak into reports. CRAN gates must be established day one.

---

### Phase 2: Multi-Format Rendering & Report Orchestration (Moderate Risk, Core Delivery)

**Deliverables:**

1. Implement `.assemble_report_advice()` internal orchestrator (multi-section advise loop with offline fallback)
2. Implement public `es_report()` entry point (4-step orchestration; returns named vector of output paths)
3. Extend `skeleton.Rmd` for narrative injection and multi-format support (4 narrative chunks; 4 output formats)
4. Format-specific rendering logic (HTML plotly; PDF/Word/Markdown ggplot2 static)
5. Format-specific error handling and guards (soft guards for PDF/Word/Markdown; hard stop only for HTML)
6. Prose sanitisation for LaTeX/XML (replace special characters per format)
7. R6 task object protection (deep-clone before render; read-only template)
8. Multi-format combination testing (HTML+PDF+Word sequential; verify figures survive)

**Features Delivered (User-Facing):**
- One-call `es_report()` entry point
- Offline-first rendering (complete report with no LLM provider)
- Executive summary + AI interpretation (when available; placeholder otherwise)
- Data/Methods auto-filled section
- CAAR results section + format-specific plots
- Robustness/caveats section (KB + joint-hypothesis caveat)
- Grounding provenance footer
- HTML, PDF, Word, Markdown formats
- Section toggle args

**Testing Gates:**
- Multi-format render produces 4 valid files with correct plot types
- Large study renders without memory explosion
- Non-ASCII fixture renders correctly in all formats
- Task unchanged after render
- Format-specific guards work (missing LaTeX → message + skip)
- Figure directories survive multi-format sequential render

---

### Phase 3: Offline-First Validation & Advanced Language Calibration (Optional v0.64.x)

**Deliverables:**

1. KB-based rule narrative for `report_writing` (when `provider=NULL`)
2. Significance language calibration (p-value thresholds → explicit labels)
3. Pre-trend test integration (flag if significant pre-event ARs)
4. Comprehensive offline mode visibility testing

**Why Optional:** These are refinements. MVP is offline graceful degradation (empty AI sections + KB robustness). Can ship in v0.64.x point release.

---

### Phase 4: Bootstrap CI Integration & Panel Task Support (v0.65.0+)

Deferred to next version. Not blocking v0.64.0.

---

## Confidence Assessment

| Area | Confidence | Gaps |
|------|-----------|------|
| **Stack** | HIGH | None — all versions verified on CRAN 2026-09-06; no new hard deps |
| **Features** | MEDIUM | (1) KB-based offline narrative deferred to Phase 3; (2) Language calibration needs finalization; (3) Bootstrap integration deferred |
| **Architecture** | HIGH | None — based on direct codebase inspection; composition proven; backward-compat clear |
| **Pitfalls** | MEDIUM | (1) Prose scanner specifics (tolerance, regex) need validation on real LLM outputs; (2) Sanitisation fixtures need development; (3) Figure-deletion edge case needs combination test |

---

## Research Flags for Planning

**Phases Requiring Deep Research During Planning:**

- **Phase 1 (Grounding):** Numeric literal regex and tolerance thresholds for prose scanner need validation against real LLM output samples. Spike: collect 10 LLM outputs from mock `es_advise()` calls; measure hallucination rate by type (rounding, off-by-one, completely fabricated); define tolerance.

- **Phase 2 (Multi-Format):** Sanitisation helper edge cases need test fixtures. Spike: create fixtures with em-dashes, smart quotes, Unicode, XML entities; test each format to verify sanitisation works.

- **Phase 2 (Multi-Format):** Multi-format knitr figure-deletion edge case (Pitfall #10) needs verification. Spike: test matrix with HTML+PDF+Word sequential render; confirm figure paths survive.

**Phases with Proven Patterns (Shallow Research):**

- **Phase 1 (CRAN Hygiene):** CI matrix patterns are standard R-pkg practice; follow ropensci HTTP-testing guidance.
- **Phase 2 (Plotly fallback):** `knitr::is_html_output()` is documented in R Markdown Cookbook; standard pattern.
- **Phase 2 (LaTeX engine):** XeLaTeX for UTF-8 safety is documented best practice.

---

## Gaps to Address During Planning

1. **Offline `report_writing` narrative:** Current design assumes empty AI prose sections offline. Planning must confirm: (a) ship as-is for v0.64.0, or (b) design KB rule templates? Recommend (a); flag (b) as v0.64.x enhancement. **Cost difference is significant.**

2. **Prose grounding scanner specifics:** Regex patterns and tolerance thresholds need finalization on real LLM outputs. Spike work, not design work; plan accordingly.

3. **Sanitisation fixture coverage:** Each output format (PDF/Word/Markdown) may have different escape requirements. Planning phase includes fixture generation.

4. **Figure directory deletion regression:** Needs concrete test scenario in EventStudy context. Planning includes combination test validation.

5. **Offline mode visibility confirmation:** "AI Interpretation (Not Available)" placeholder acceptable or too subtle? Planning should validate user messaging. Consider: distinct heading style, console message from `es_report()` stating "Offline mode active."

---

## Sources

**STACK.md:** CRAN rmarkdown (2.32) + knitr (1.52) pages; rmarkdown render() reference; tinytex::is_tinytex(); officedown dep tree

**FEATURES.md:** EventStudyTools methodology; MacKinlay (1997), Kothari/Warner (2007); FinGround (arXiv 2604.23588) on LLM hallucinations; easystats/report R package; Lumivero / AI Analyst papers on overclaiming

**ARCHITECTURE.md:** Direct codebase inspection (R/report.R, R/advise.R, R/es_diagnostics.R, R/execute.R); existing advice= and .empty_advice() patterns

**PITFALLS.md:** ropensci HTTP-testing book; R Markdown Cookbook; R Packages (2e); tinytex GitHub issues; plotly GitHub #889; Peter Ralph / luke.geek.nz on silent fallback trust erosion

---

*Synthesized from: STACK.md, FEATURES.md, ARCHITECTURE.md, PITFALLS.md*  
*Synthesis completed: 2026-09-06*  
*Confidence: MEDIUM-HIGH (architecture and pitfalls HIGH; features and gaps require planning confirmation)*
