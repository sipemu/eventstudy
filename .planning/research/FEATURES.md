# Feature Research

**Domain:** One-call automated AI report for a financial event-study R package (v0.64.0)
**Researched:** 2026-09-06
**Confidence:** MEDIUM (domain knowledge HIGH from codebase; AI-report best-practices MEDIUM from web research)

---

## Context: What Already Exists (Do Not Re-research)

The following are shipped and must not be redesigned — they are the inputs to this milestone:

| Existing Piece | What It Provides | How the Report Uses It |
|---|---|---|
| `es_diagnostics(task)` | `es_diagnostics` S3 object — 6 sections: `meta`, `estimation_window`, `event_window`, `cross_sectional`, `contract_state`, `aggregate_summary` | Primary grounding source for all AI narrative |
| `es_advise(diag, task_type="report_writing", provider=p)` | `Advice` S3: `interpretation`, `recommendations[]`, `caveats[]`, `n_dropped`, `source` | Supplies prose for exec summary, methods narrative, results interpretation, and caveats sections |
| Offline KB + rule engine | `recommend_stat()`, `flag_robustness()` — deterministic offline advice without LLM | Fallback narrative when no provider configured |
| `generate_report(task, advice=advice, ...)` | Lower-level RMarkdown renderer; existing sections: summary, data, diagnostics, single_event, multi_event, cross_sectional, appendix, advice | The renderer `es_report()` must call internally |
| `model_diagnostics(task)` | Tibble: shapiro_p, dw_stat, ljung_box_p, acf1, sigma, r2 per event | Diagnostic table in methods section |
| `pretrend_test(task)` | Pre-event AAR joint significance | Robustness section |
| `plot_event_study(task)` | Plotly AAR/CAAR plot with CI bands | Results section figure |
| `export_results(task)` | LaTeX/CSV/Excel tables | Appendix or inline tables |

**The one-call wrapper `es_report()` composes these pieces. It must add zero novel statistical logic — only orchestration and a fixed template.**

---

## Feature Landscape

### Table Stakes (Users Expect These)

Features that must be present for an academic/quant researcher to trust and use the report. Missing any one of these means the report is not publication-ready and the researcher falls back to writing sections manually.

| Feature | Why Expected | Complexity | Depends On |
|---------|--------------|------------|-----------|
| **Executive summary section** — 1-3 sentence plain-English interpretation of the key result (sign, magnitude, significance of CAAR) | Every academic report has an abstract/summary; without it the reader has no orientation | LOW | `Advice$interpretation` from `es_advise(task_type="report_writing")`; fallback: offline KB `interpret` from `recommend_stat()` |
| **Data and Methods section** — event definition (date, window lengths, estimation window), return model name and parameters, test statistics used, sample size | A peer reviewer's first question is "what did you do exactly?"; missing this = not reproducible | LOW | `task$data_tbl` metadata, `task$params` or `ParameterSet` fields, `es_diagnostics()$meta` |
| **AR/CAR results table** — per-event CARs with t-statistics and p-values, formatted with significance stars | The primary deliverable of any event study; without it the report has no result | MEDIUM | `task$data_tbl` CART column; `export_results()` or direct unnesting |
| **AAR/CAAR multi-event table** — relative_index, AAR, CAAR, test statistic values (Patell Z, BMP, etc.) with significance indicators | For multi-firm studies (the dominant use case) CAAR is the headline number | MEDIUM | `task$aar_caar_tbl` nested column; existing multi_event section in `generate_report()` |
| **CAAR event-window plot** — AAR and CAAR over relative event time with confidence bands | The canonical visual for event studies; reviewers expect it | LOW | `plot_event_study(task)` — already exists |
| **Model diagnostics summary** — R-squared, residual sigma, Shapiro-Wilk p, Durbin-Watson statistic per event (or aggregate) | Reviewers check whether the return model fits; a low R² undermines the abnormal return calculation | LOW | `es_diagnostics()$estimation_window`; `model_diagnostics(task)` |
| **Robustness / caveats section** — AI-grounded flags from `flag_robustness()` or KB, plus the joint-hypothesis caveat and any grounding-guard drops | Distinguishes a trustworthy report from a p-hacking exercise | MEDIUM | `es_advise(diag, task_type="flag_robustness")`; `Advice$caveats`; `Advice$n_dropped` |
| **References section** — MacKinlay (1997), Kothari/Warner (2007), and any model-specific citations pulled from the KB | Every academic paper needs references; these are the standard event-study methodological cites | LOW | `es_kb()` citation records; static list for the model chosen |
| **Offline-first rendering** — report renders complete with no LLM configured; narrative sections use the offline KB rule-based engine | Researchers without API keys must get a full report, not an error | MEDIUM | Offline path already exists in `es_advise()` for KB types; `report_writing` task type currently requires a provider — this gap must be filled |
| **Multi-format output** — HTML (default), PDF, Word (.docx), Markdown from one call | Supervisors expect Word; paper submission systems expect PDF; web use expects HTML | MEDIUM | `rmarkdown::render()` with different `output_format` arguments; all are standard rmarkdown |
| **Single `es_report()` entry point** — one function call from raw task to rendered file | Without this, users still have to orchestrate `es_diagnostics()` + `es_advise()` + `generate_report()` themselves | MEDIUM | Wrapper that sequences existing functions |
| **Section toggle args** — researcher can suppress sections (e.g., `sections = c("summary", "results", "references")`) | Different contexts need different subsets; a supervisor briefing omits appendices | LOW | Mirrors existing `sections=` arg in `generate_report()` |
| **Grounding provenance disclosure** — report footer states whether narrative came from an LLM (provider name) or offline rule engine; states N recommendations dropped by guard if any | Academic reproducibility requires knowing the provenance of any interpretive text | LOW | `Advice$source`, `Advice$is_deterministic`, `Advice$n_dropped` already in the S3 object |

### Differentiators (Competitive Advantage)

Features that set this report apart from manually-assembled or competitor outputs. Not strictly required for "complete," but required for "great."

| Feature | Value Proposition | Complexity | Depends On |
|---------|-------------------|------------|-----------|
| **Grounded AI narrative with uncertainty hedging** — LLM-written prose that names the actual CAAR value, p-value, and R² and uses language like "the results suggest" rather than "the results show" | No competing R event-study package produces AI-interpreted narrative; this is the unique feature | MEDIUM | `Advice$interpretation` from `es_advise(task_type="report_writing")`; existing runtime grounding guard enforces citation of computed values only |
| **Automatic model-appropriate test-statistic recommendation callout** — a highlighted note in the methods section ("Given overlapping event windows, Kolari-Pynnonen is preferred over Patell") pulled from KB rules | Steers researchers away from common methodological mistakes without requiring them to know the literature | LOW | `recommend_stat()` KB output; `es_diagnostics()$cross_sectional.n_overlap_pairs` fires KB-OVERLAP-KP rule |
| **Degenerate-event disclosure** — if `contract_state$is_fitted` has FALSE entries, a clearly-styled callout box warns "N events could not be fitted and are excluded from aggregate statistics" | Prevents a researcher from submitting results where silent failures inflated or deflated the CAAR | LOW | `es_diagnostics()$contract_state.is_fitted`; `es_diagnostics()$meta.n_events_total` vs `cross_sectional.n_valid_events` |
| **Aggregate-vs-shown distinction note** — when n_events > max_events cap (default 20), the report notes that per-event detail is shown for top-20 by anomaly score; aggregate statistics cover all N events | Prevents a reader from thinking the aggregate CAAR covers fewer events than it does | LOW | `es_diagnostics()$meta.n_events_summarized` and `aggregate_summary` section |
| **Pre-trend diagnostic integration** — if `pretrend_test(task)` shows significant pre-event ARs (p < 0.05), the robustness section flags this prominently with the specific p-value | Pre-trend contamination is the most common reason reviewers reject event studies | LOW | `pretrend_test(task)` output; can be called inside `es_report()` before rendering |
| **Bootstrap CI alongside asymptotic CI** — when bootstrap results exist in the task, report both sets of confidence intervals in the results table; note which is preferred given sample size | Matches Kothari/Warner (2007) recommendation to report both; shows methodological rigor | MEDIUM | Requires `bootstrap_test()` to have been run; report checks for presence and conditionally includes |
| **Significance-star table convention disclosure** — legend "* p<0.10, ** p<0.05, *** p<0.01" appears at every table footer | Reviewers in different fields use different conventions; explicit disclosure prevents misreading | LOW | Static template text in every table-rendering chunk |
| **HTML interactive plots, static plots for PDF/Word** — `plot_event_study()` in Plotly mode for HTML, ggplot static for PDF/Word | HTML output should use the package's existing interactive Plotly; PDF/Word require static | MEDIUM | `interactive` arg already in `generate_report()`; dispatch on format |

### Anti-Features (Commonly Requested, Often Problematic)

These must be explicitly ruled out of scope now to prevent scope creep and to protect grounding integrity.

| Anti-Feature | Why It Seems Appealing | Why It Is Problematic | What to Do Instead |
|---|---|---|---|
| **Fabricating or interpolating numbers not in es_diagnostics()** | The LLM could fill gaps in the diagnostic object (e.g., estimate power from CAR magnitude) | Core invariant violation: the grounding guard exists precisely to prevent this; a fabricated power estimate is a fabricated statistical claim | Any number in the report must come from `es_diagnostics()`, `model_diagnostics()`, or `pretrend_test()`; if a number is not there, say "not computed" or omit |
| **Custom templating (user-provided Rmd)** | Researchers want their institute's header | Breaks the fixed-template guarantee; creates infinite maintenance surface; template variables change between versions | One polished fixed template; section toggling via `sections=` arg covers 90% of customization needs; deferred to a future major version if demand proven |
| **Claiming statistical significance without reporting the test statistic and p-value** | Cleaner prose | Unacceptable in academic reporting; a reader cannot verify the claim | Always emit: test statistic name, value, and p-value together. Never "significant abnormal returns" without numbers |
| **Overclaiming based on borderline results** | AI prose tends toward confident language | A CAAR with p = 0.049 is not "highly significant"; an R² of 0.04 does not "strongly support" the market model | Calibrate language to numeric thresholds: p < 0.01 → "strongly significant"; p in [0.01, 0.05] → "statistically significant"; p in [0.05, 0.10] → "marginally significant"; p > 0.10 → "not statistically significant at conventional levels" |
| **Ignoring the joint hypothesis problem** | Simplifies the narrative | The fundamental critique of event studies (MacKinlay 1997, Kothari/Warner 2007): rejecting H0 jointly tests the event AND the return model; the report must state this | A fixed one-sentence caveat in every report's caveats section: "Abnormal returns are measured relative to [model name], so significant results jointly test both the event's impact and model specification." This is not optional |
| **Verbose boilerplate that restates inputs** | More text looks more complete | "The estimation window is 250 days" is not interpretation — it is a restatement of a parameter. Boilerplate dilutes the signal of genuine AI interpretation | Distinguish two roles: (a) auto-filled parameter tables from task metadata (Data/Methods section — no LLM needed); (b) LLM-generated interpretation of *what the results mean* (Results/Summary — LLM or KB) |
| **Generating references the LLM invents** | Complete reference list is expected | LLMs fabricate DOIs, page numbers, journal names at high rates | Pull references only from `es_kb()` citation records (which are hard-coded in R) and from a static methodological references list embedded in the template. Never ask the LLM to generate citations |
| **One-size-fits-all language regardless of task type** | Simpler prompt engineering | A panel DiD report needs different language than a single-firm study; using event-study language for a PanelEventStudyTask is confusing | Dispatch on `inherits(task, "PanelEventStudyTask")` vs `"EventStudyTask"` inside `es_report()`; use task-type-specific prompt context for `es_advise()` |
| **Running the full study pipeline inside es_report()** | Convenience: pass raw data, get report | Violates separation of concerns; makes testing impossible; changes behavior of an existing fitted task; adds enormous failure modes | `es_report()` accepts only a **fitted** task (post `run_event_study()` / `calculate_statistics()`); it orchestrates diagnostics + advise + render only |

---

## Report Sections: Concrete Content Specification

This is the fixed template `es_report()` must produce, with exact data sources per section. Section presence is toggled by `sections=` arg.

### Section 1: Executive Summary (`"summary"`)

**Content:**
- Study identifier: event type / firm name(s) / date range (from `task$data_tbl` event_id and date range)
- Headline result: CAAR for the primary window with test statistic and p-value (from `es_diagnostics()$cross_sectional`)
- One-paragraph AI interpretation (from `Advice$interpretation`; offline fallback: template sentence using CAAR sign and significance)
- N events total / N valid events (from `es_diagnostics()$meta` and `cross_sectional`)
- Degenerate-event alert callout if `n_valid_events < n_events_total`

**AI contribution:** The interpretation paragraph. Must cite only `Advice$interpretation` — never a number the LLM generated independently.
**Offline fallback:** Template: "Across [N] events, the cumulative average abnormal return over the [window] window is [CAAR]% ([test statistic] = [value], p = [p]). [Significant/Not significant] at the [level] level."

### Section 2: Data and Methods (`"methods"`)

**Content:**
- Table: Estimation window length, event window [start, end], return type (simple/log), return model name
- Return model formula (pulled from a static model→formula lookup table in the template, indexed by model class name)
- Test statistics used (from `ParameterSet` or inferred from column names in `task$data_tbl`)
- Sample: N total events, N firms, date range
- Recommended test statistic note from `recommend_stat()` KB output if it differs from what was used

**AI contribution:** None. This section is auto-filled from task metadata — no LLM interpretation; no hallucination risk.

**Data sources:** `task$data_tbl`, `task$params` (or inferred from column names), `es_diagnostics()$meta`

### Section 3: Results (`"results"`)

**Content:**
- CAAR plot: `plot_event_study(task)` — Plotly for HTML, static ggplot for PDF/Word
- Multi-event statistics table: AAR, CAAR, test statistic values, significance stars, one row per relative_index (from `task$aar_caar_tbl`)
- Single-event CAR summary: per-event table of firm, CAR, CAR t-stat, p-value, significance (from CART column, top 20 by anomaly score)
- Cross-sectional CAR dispersion: IQR and SD from `es_diagnostics()$cross_sectional`
- AI interpretation paragraph for the results: from `Advice$recommendations` entries where `kind == "interpret"` (or `task_type == "report_writing"`)
- Significance threshold legend at every table footer

**AI contribution:** The results interpretation paragraph. Must be grounded — every number cited must appear in `es_diagnostics()`. The grounding guard is already enforced at `es_advise()` call time; the report relies on its output.

**Data sources:** `task$aar_caar_tbl`, `task$data_tbl` (CART column), `es_diagnostics()$event_window`, `es_diagnostics()$cross_sectional`

### Section 4: Model Diagnostics (`"diagnostics"`)

**Content:**
- Table: per-event (or aggregate for large studies) — R², sigma, Shapiro-Wilk p, Durbin-Watson statistic, Ljung-Box p, ACF(1)
- Flag rows where normality is rejected (shapiro_p < 0.05) or autocorrelation is detected (dw_stat < 1.5)
- Pre-trend test result: t-statistic and p-value from `pretrend_test(task)` if pre-event observations exist
- Aggregate summary for studies beyond the top-20 cap (from `es_diagnostics()$aggregate_summary`)

**AI contribution:** None in the table. A one-sentence note per fired KB rule (e.g., "Residuals are non-normal in [N]% of events; consider non-parametric tests") pulled from `recommend_stat()` or `flag_robustness()` KB output.

**Data sources:** `es_diagnostics()$estimation_window`, `model_diagnostics(task)`, `pretrend_test(task)`, `es_diagnostics()$aggregate_summary`

### Section 5: Robustness and Caveats (`"robustness"`)

**Content:**
- Bullet list of fired robustness KB rules from `flag_robustness()` (with evidence values cited inline)
- Degenerate events count and fraction if any are unfitted
- Overlap pairs count and KP recommendation if `n_overlap_pairs > 0`
- Fixed mandatory caveat: the joint hypothesis problem (model + event)
- Grounding guard disclosure: if `Advice$n_dropped > 0`, "N AI recommendations were removed by the grounding guard because they cited uncomputed values"
- `Advice$caveats` rendered as a bulleted list

**AI contribution:** Prose expansion of each robustness recommendation (rationale + expected effect from `Advice$recommendations` where `kind == "robustness"`). Evidence values cited must be the same values in `es_diagnostics()`.

**Data sources:** `flag_robustness(diag)`, `es_diagnostics()$cross_sectional`, `es_diagnostics()$contract_state`, `Advice$caveats`, `Advice$n_dropped`

### Section 6: References (`"references"`)

**Content:**
- MacKinlay (1997) — always included
- Kothari / Warner (2007) — always included
- Model-specific reference from `es_kb()` citation records, indexed by the model class used
- Test-statistic-specific references: Patell (1976) if PatellZTest used; BMP (1991) if BMPTest; KP (2010) if KolariPynnonenTest

**AI contribution:** None. References are drawn only from hard-coded R code in `es_kb()` citation records and a static lookup table in the template.

### Section 7: Appendix (`"appendix"`)

**Content:**
- Report generation timestamp
- R version and EventStudy package version
- Provider used for AI narrative (or "offline rule engine")
- N recommendations generated / N dropped by grounding guard
- Full diagnostic object summary (`print.es_diagnostics()` output)

---

## Feature Dependencies

```
es_report() entry point
    requires: fitted EventStudyTask (post calculate_statistics())
    calls: es_diagnostics(task)
        calls: pretrend_test(task) [for robustness section]
    calls: es_advise(diag, task_type="report_writing", provider=provider)
        requires: provider arg OR offline fallback for report_writing type
        [CURRENT GAP]: report_writing is LLM_ONLY_TYPE — no offline fallback exists
    calls: generate_report(task, advice=advice, format=format, ...)
        requires: rmarkdown (Suggests)
        requires: knitr (Suggests)

Offline-first guarantee
    requires: offline fallback for report_writing task type
    blocks: the entire es_report() offline path
    must be resolved before es_report() can be CRAN-releasable

Multi-format output
    requires: rmarkdown word_document (needs pandoc)
    requires: rmarkdown pdf_document (needs LaTeX or tinytex)
    both are user-environment deps, not package deps

Grounding guard (existing)
    enforces: every Advice$recommendations[].evidence[] value matches es_diagnostics()
    the report inherits this guarantee by using Advice as-is
    must NOT be re-implemented inside the report template

Significance threshold table
    requires: static lookup of p-value ranges to language labels
    must be determined at template-authoring time, not at LLM call time
```

### Dependency Notes

- **Offline fallback for `report_writing` is the critical missing piece.** Currently `report_writing` is in `LLM_ONLY_TYPES` (advise.R:26) — passing `provider=NULL` throws `stop()`. For `es_report()` to be offline-first, the `report_writing` task type needs a rule-based narrative path, or `es_report()` falls back to assembling narrative from the KB types (`recommend_stat` + `flag_robustness` + `interpret`) and omits the free-form interpretation prose entirely when no provider is given.
- **`pretrend_test(task)` is not in `es_diagnostics()`** — the harvester does not call it. `es_report()` must call it separately and pass results as a param to `generate_report()`.
- **`cross_sectional_regression()` results** — the report includes a cross-sectional section only if the user passes `cross_sectional=` explicitly, matching the existing `generate_report()` behavior. Do not auto-run it inside `es_report()`.
- **Panel tasks (`PanelEventStudyTask`)** — `es_diagnostics()` currently only accepts `EventStudyTask`. The report must detect task type and skip diagnostics/advise sections for panel tasks, or error with a clear message.

---

## MVP Definition

### Must Ship in v0.64.0 (the one-call value)

- [ ] `es_report()` entry point — sequences `es_diagnostics()` → `es_advise()` → `generate_report()`; single function call from fitted task to rendered file
- [ ] Executive summary section — headline CAAR + interpretation (AI or offline template)
- [ ] Data/Methods section — auto-filled from task metadata; no LLM
- [ ] Results section — CAAR table + CAAR plot + AI interpretation paragraph
- [ ] Model diagnostics section — diagnostic table from `es_diagnostics()$estimation_window`
- [ ] Robustness/caveats section — KB flags + mandatory joint-hypothesis caveat + grounding disclosure
- [ ] References section — hard-coded, never LLM-generated
- [ ] Offline-first guarantee — full report without API key (requires offline narrative fallback for `report_writing`)
- [ ] HTML output default — standard rmarkdown html_document
- [ ] Grounding provenance footer — provider name or "offline" + guard drop count
- [ ] Degenerate-event alert — callout when `n_valid < n_total`
- [ ] Joint-hypothesis caveat — fixed mandatory text in every report's caveats

### Add After Core Works (v0.64.x or same milestone if time allows)

- [ ] PDF output — requires user-installed LaTeX or tinytex; guarded by format detection
- [ ] Word (.docx) output — requires pandoc; guarded by format detection
- [ ] Markdown output — rmarkdown `md_document`; useful for embedding in GitHub wikis
- [ ] Pre-trend test result in diagnostics section — call `pretrend_test()` inside `es_report()` if pre-event window exists
- [ ] Significance language calibration by p-value threshold — p<0.01 → "strongly significant"; p in [0.01,0.05] → "significant"; etc.

### Defer to v0.65.0 or Later

- [ ] Panel task (`PanelEventStudyTask`) support — `es_diagnostics()` extension needed first
- [ ] Custom template support — surface area too large; validate demand before building
- [ ] Bootstrap CI table integration — requires user to have already called `bootstrap_test()`; niche use case

---

## Feature Prioritization Matrix

| Feature | User Value | Implementation Cost | Priority |
|---------|------------|---------------------|----------|
| `es_report()` one-call entry point | HIGH | LOW (orchestration only) | P1 |
| Offline-first guarantee (report_writing fallback) | HIGH | MEDIUM (must extend advise.R) | P1 |
| Executive summary + AI interpretation | HIGH | LOW (Advice$interpretation exists) | P1 |
| Data/Methods auto-fill section | HIGH | LOW (metadata extraction) | P1 |
| CAAR results table + plot | HIGH | LOW (existing generate_report section) | P1 |
| Robustness/caveats section | HIGH | LOW (KB outputs exist) | P1 |
| Mandatory joint-hypothesis caveat | HIGH | LOW (static text) | P1 |
| References section (KB-sourced) | HIGH | LOW (es_kb() citations exist) | P1 |
| Grounding provenance footer | MEDIUM | LOW (Advice$source, n_dropped) | P1 |
| Degenerate-event alert callout | HIGH | LOW (contract_state.is_fitted) | P1 |
| HTML format output | HIGH | LOW (existing rmarkdown path) | P1 |
| Model diagnostics table | MEDIUM | LOW (es_diagnostics() exists) | P2 |
| PDF format output | MEDIUM | MEDIUM (LaTeX toolchain) | P2 |
| Word (.docx) output | MEDIUM | MEDIUM (pandoc dependency) | P2 |
| Markdown output | LOW | LOW | P2 |
| Significance language calibration | MEDIUM | LOW (lookup table) | P2 |
| Pre-trend test in diagnostics section | MEDIUM | LOW (pretrend_test() exists) | P2 |
| Bootstrap CI table integration | LOW | MEDIUM | P3 |
| Panel task support | LOW | HIGH (es_diagnostics extension) | P3 |

---

## Grounding and Anti-Hallucination: Critical Rules for Implementation

These rules must be enforced at template-authoring time, not left to runtime luck:

1. **Numbers come from two sources only:** `es_diagnostics()` object fields (cited by key path) and task metadata (`task$data_tbl`, `task$params`). Any number in the report narrative that does not come from one of these two sources is a fabrication, even if it happens to be correct.

2. **The grounding guard is upstream, not in the template.** The template renders `Advice$interpretation` and `Advice$recommendations` as-is. The guard already ran at `es_advise()` call time. The template must not add an additional layer that could reintroduce ungrounded numbers (e.g., no free-form LLM calls inside an R chunk in the Rmd template).

3. **References are never LLM-generated.** The LLM prompt for `report_writing` must instruct: do not generate citations. All citations come from `es_kb()$citation` records and a static lookup in the template.

4. **Language calibration for significance:** The template, not the LLM, converts p-values to language. A static function in the template: `sig_label <- function(p) if (p < 0.01) "strongly significant" else if (p < 0.05) "statistically significant" else if (p < 0.10) "marginally significant" else "not statistically significant at conventional levels"`. The LLM is told what significance label to use via the diagnostics, not asked to infer it.

5. **The joint-hypothesis caveat is not optional.** It is a static string embedded in the caveats section regardless of what the LLM returns. It cannot be overridden by section toggling.

6. **The grounding guard drop count must be disclosed.** If `Advice$n_dropped > 0`, the appendix and the caveats section both say so, by name. This makes the guard's operation visible to the researcher.

---

## Sources

- EventStudyTools: [Introduction to Event Study Methodology](https://www.eventstudytools.com/introduction-event-study-methodology) — standard 5-step structure; window definitions; CAAR as primary result
- PMC / NIH: [Event studies in international finance research](https://pmc.ncbi.nlm.nih.gov/articles/PMC9264305/) — robustness check requirements: multiple models, bootstrap alongside asymptotic, subsample validation
- FinGround (arXiv 2604.23588): [Detecting and Grounding Financial Hallucinations](https://arxiv.org/html/2604.23588) — LLMs fabricate financial metrics at high rates without explicit grounding; targeted rewriting with paragraph/table-cell citations achieves 93.2% faithfulness
- AI Analyst (arXiv 2507.00718): Financial time series LLM report generation — hedging language calibration; uncertainty rises as LLM extrapolates beyond grounded data
- easystats/report: [Automated Reporting of Results](https://easystats.github.io/report/) — R pattern: deterministic parameter tables + language-template-based interpretation; always emit CI alongside p-values
- Lumivero: [The rise of AI in statistical analysis](https://lumivero.com/resources/blog/ai-in-statistical-analysis/) — overclaiming is the primary failure mode; uncertainty quantification is routinely neglected
- MacKinlay (1997), "Event Studies in Economics and Finance," *Journal of Economic Literature*, 35(1):13-39 — standard event study methodology; joint hypothesis problem; window conventions
- Kothari, Warner (2007), "Econometrics of Event Studies," in *Handbook of Corporate Finance* — robustness requirements; calendar clustering; bad model problem

---

*Feature research for: EventStudy v0.64.0 — One-Call Automated AI Reporting*
*Researched: 2026-09-06*
