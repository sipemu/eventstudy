# Pitfalls Research

**Domain:** Multi-format AI-narrated report generation added to a CRAN R package (EventStudy v0.64.0)
**Researched:** 2026-09-06
**Confidence:** MEDIUM — sources are CRAN official docs, ropensci HTTP-testing book, rmarkdown cookbook, and cross-validated community experience. LLM-grounding findings from 2025-2026 arxiv/production literature. Plotly/webshot2 behavior confirmed from GitHub issues.

---

## Critical Pitfalls

### Pitfall 1: Grounding Guard Does Not Protect Prose in Multi-Section Narrative

**What goes wrong:**
The existing `es_advise()` grounding guard validates `evidence[]` arrays inside structured `recommendations` objects — it drops any recommendation whose cited key or value does not match the computed diagnostics. However, `es_report()` needs to draft free-prose sections: executive summary, data/methods narrative, results interpretation, robustness/caveats. These sections are plain strings in the `interpretation` field or rendered directly as markdown prose. The guard does not scan plain text strings for fabricated numbers. An LLM asked to write "The Patell Z-statistic of 3.72 on day 0 is highly significant..." in a prose paragraph invents that specific value from context — the value never passes through an `evidence[]` array and therefore the guard is blind to it.

The risk compounds in a full report because each section is an independent prompt invocation (or a large combined prompt with multiple section headers). Each section's prose has its own hallucination surface. A single-block `report_writing` advice call already had this exposure; a multi-section call amplifies it proportionally.

**Why it happens:**
The schema enforces the structure of `recommendations[]` but free-text fields (`interpretation`, each section's prose block) are unconstrained strings. Schema validation catches structural problems, not semantic ones. LLMs produce "confident fabrication" — plausible-sounding invented numbers — especially when asked to write narrative that echoes statistical results they saw earlier in the prompt. The longer the prompt context, the more likely a specific number from diagnostics gets paraphrased incorrectly (off by a digit, wrong section) in the prose.

**How to avoid:**
1. Restrict prose to templated slots, not open-ended generation. Instead of "write an executive summary citing the results," instruct: "Fill these exact sentence templates using only the values in `{diagnostics_json}`, leaving `[PLACEHOLDER]` for any value not present."
2. Add a post-generation prose scanner to `.validate_grounding()`. After building the `Advice` object, extract all numeric literals from the `interpretation` string (regex `\b\d+\.?\d*\b`) and cross-check each against the diagnostics. Any number that appears in prose but not in diagnostics within tolerance — append a caveat, optionally redact.
3. Scope the `report_writing` task type to structured fields only. Each prose section should be a separate field in the JSON schema (e.g., `executive_summary`, `methods_prose`, `results_interpretation`, `caveats_prose`), each accompanied by a `section_evidence[]` array identical in shape to the existing `evidence[]` requirement. This forces the guard's coverage to extend to each section.
4. System prompt discipline: add an explicit instruction "Every numeric value you write must appear verbatim in the diagnostics JSON above. Do not compute, round, or derive values."

**Warning signs:**
- LLM returns numbers in prose that differ from `es_diagnostics()` output by any amount, including rounding.
- The `interpretation` field contains p-values, t-statistics, or sample sizes not present in the diagnostics object.
- The report renders successfully but a researcher notices a discrepancy between the prose narrative and the printed table output.

**Phase to address:**
Phase 1 (grounding architecture for the report wrapper). Define the per-section JSON schema and extend `.validate_grounding()` to cover prose fields before any report template work begins. This is the invariant the rest of the milestone depends on.

---

### Pitfall 2: CRAN R CMD Check Triggered by Rendering in Examples or Tests

**What goes wrong:**
Any call to `generate_report()` or `es_report()` in a `@examples` block or a test that runs unconditionally will cause `R CMD check` to invoke `rmarkdown::render()`, which in turn invokes pandoc (must be present at check time) and potentially LaTeX (for PDF output). CRAN check machines do not have pandoc or tinytex pre-installed reliably. The check fails with an error about a missing system dependency, and CRAN may reject the package with a NOTE or WARNING. Even if pandoc is present, a PDF example will attempt `tinytex::install_tinytex()` or fail with a missing `pdflatex` error.

The existing `generate_report()` already uses `requireNamespace("rmarkdown")` to guard the function body, but that guard does not suppress example execution. `\dontrun{}` in roxygen examples suppresses example execution during `R CMD check --run-donttest` but not during `devtools::run_examples()`.

**Why it happens:**
Developers write examples that demonstrate the full workflow including rendering, which is correct for documentation purposes but breaks the CRAN check constraint that examples must run to completion in a clean environment without optional system tools.

**How to avoid:**
1. Wrap any `generate_report()` or `es_report()` call in `@examples` blocks with `\dontrun{}`. This is the correct CRAN convention when the example requires external tools not guaranteed to be present.
2. In testthat tests, wrap render calls with `skip_on_cran()`, `skip_if_not_installed("rmarkdown")`, and `skip_if_not_installed("knitr")`. For PDF tests additionally: `skip_if(!tinytex::is_tinytex(), "TinyTeX not installed")`. For Word tests: `skip_if_not_installed("officedown")`.
3. Never test the actual render output (the file on disk) during `R CMD check` — test only the R-level return value and the function's behavior on bad inputs.
4. For vignettes that demonstrate `es_report()`: place them in `vignettes/articles/` (.Rbuildignore'd) rather than in `vignettes/` (CRAN-shipped), following the pattern already established in v0.63.0 for Methods articles.

**Warning signs:**
- `R CMD check` produces `WARNING: running examples for source file 'report.R' ... Error: pandoc not found`.
- CI check passes locally (because pandoc/RStudio is installed) but fails on GitHub Actions or CRAN check machines.
- `devtools::check(run_dont_test = FALSE)` passes but `devtools::check(run_dont_test = TRUE)` fails.

**Phase to address:**
Phase 1 (CRAN hygiene scaffolding). Establish the `\dontrun{}` and skip discipline before adding any new exported functions. Add a CI matrix job that runs `R CMD check` on a minimal Ubuntu runner without pandoc to catch this class of error continuously.

---

### Pitfall 3: PDF Rendering via tinytex Breaks in CI and CRAN Environments

**What goes wrong:**
PDF output from `rmarkdown::pdf_document()` requires a working LaTeX installation. tinytex is the standard solution, but it fails in CI/CRAN environments in several ways: (a) `tlmgr` is out of date relative to the remote mirror, causing installation failures; (b) the entire tinytex installation can be wiped if `tlmgr_install()` fails mid-operation on an outdated mirror; (c) CRAN check machines run with restricted write access — tinytex installs to `$HOME/.TinyTeX` which may not exist; (d) LaTeX package auto-installation (`tinytex.install_packages = TRUE`) triggers network calls during `R CMD check`, which CRAN prohibits.

**Why it happens:**
PDF rendering is the most fragile output format because it has the deepest system dependency chain: R to rmarkdown to pandoc to LaTeX to tinytex/TeX Live to per-document LaTeX packages. Each link can break independently in a clean environment.

**How to avoid:**
1. Never ship PDF rendering as a default or auto-triggered format. Make `format = "html"` the default in `es_report()`; require the user to explicitly request `format = "pdf"`.
2. In the function body, guard PDF rendering: check `requireNamespace("tinytex", quietly = TRUE)` and `tinytex::is_tinytex()` before attempting PDF render; stop with an install message if absent.
3. Tests for PDF output: wrap entirely in `skip_if(!tinytex::is_tinytex(), "TinyTeX not installed")` and `skip_on_cran()`. Never run PDF render tests in the standard suite — put them in a separate `test_report_pdf.R` that is always skipped on CRAN.
4. In examples and vignettes, show only HTML rendering. Mention PDF in a code comment with `\dontrun{}`.
5. Do not list `tinytex` in `Suggests` unless the package explicitly calls tinytex API functions. The user's own LaTeX installation is sufficient; tinytex is a user-level concern, not a package dependency.

**Warning signs:**
- `R CMD check` NOTE: "checking for detritus in the temp directory ... LaTeX errors".
- CI job fails with "pdflatex: command not found" or "tlmgr: command not found".
- tinytex auto-installs a LaTeX package during `R CMD check` and CRAN flags a network access violation.

**Phase to address:**
Phase 2 (multi-format rendering). Establish the PDF guard and skip discipline at the same time as the PDF rendering code is written, not retrofitted later.

---

### Pitfall 4: Plotly in Static Formats Renders Blank Without webshot2 and Headless Chrome

**What goes wrong:**
The existing `generate_report()` uses plotly plots (via `plot_event_study()`) with `interactive = TRUE` in HTML output. When `format = "pdf"` or `format = "docx"`, plotly requires a PNG screenshot fallback via `webshot2`, which requires `chromote`, which requires a system headless Chrome/Chromium installation. Without this chain: the plot chunk either errors, renders blank, or prints a low-resolution PNG screenshot. The user sees a report with empty figure placeholders.

The current `generate_report()` passes `interactive` as a param but does not implement the static-format fallback. This is a known debt item that becomes critical when PDF/Word support is added.

**Why it happens:**
Plotly is an HTML widget — it is self-rendering JavaScript. Static formats cannot embed JavaScript. The fallback path (webshot2 to chromote to Chrome screenshot) is a heavy external dependency chain that many users will not have. The `always_allow_html: yes` YAML header is sometimes tried as a workaround but does not reliably produce rendered plots in PDF output.

**How to avoid:**
1. In the report template, detect output format at chunk evaluation time and switch plot type using `knitr::is_html_output()`: use `plot_event_study(task, interactive = TRUE)` (plotly) for HTML, and `plot_event_study(task, interactive = FALSE)` (ggplot2) for all static formats. The `interactive = FALSE` path already exists in `plot_event_study()`.
2. Never rely on webshot2 as a fallback in a CRAN package. webshot2 is not suitable for CRAN Suggests because it requires chromote which requires system Chrome. Treat the ggplot2 path as the canonical static output.
3. Document clearly in the function signature: `interactive = TRUE` applies only to HTML format; for PDF/Word/Markdown it is silently overridden to `FALSE` with a `message()`.

**Warning signs:**
- PDF or Word report has blank figure panels.
- Chromote/webshot2 warnings appear during render ("PhantomJS not found" or similar).
- Test that renders to PDF passes on developer machine (has Chrome) but fails in CI.

**Phase to address:**
Phase 2 (multi-format rendering). The format-conditional plot switching must be in the initial template design, not added as a patch after user complaints.

---

### Pitfall 5: Non-ASCII Characters and LaTeX/XML Special Characters Break Static Formats

**What goes wrong:**
LLM-generated prose can contain: (a) Unicode smart quotes, em-dashes, degree symbols, or other non-ASCII characters that cause `pdflatex` to fail with `! Package inputenc Error: Unicode character` unless the document uses XeLaTeX or LuaLaTeX; (b) XML special characters (`&`, `<`, `>`) in parameter values passed to the Word template, which silently break `officedown`/`officer` docx generation — the file fails to open in Word with no R-visible error; (c) characters that are outside the default pdflatex Latin-1 encoding even with `\usepackage[utf8]{inputenc}`.

This is specifically dangerous with AI-generated text because LLMs routinely produce typographically correct prose with smart quotes and em-dashes that are fine in HTML but silently break PDF/Word.

**Why it happens:**
HTML output is tolerant — browsers render almost any Unicode. pdflatex is much stricter. officedown docx generation goes through XML serialisation where `&` is the entity separator — embedding it in a string value produces malformed XML. Developers test in HTML and miss the failure until PDF/Word is attempted.

**How to avoid:**
1. Sanitise all LLM-generated text before embedding in the report template. Apply transformations appropriate to the output format: replace `&` with "and" or the appropriate escape, replace smart quotes with straight equivalents for LaTeX, replace em-dash with `---` for LaTeX.
2. Add a `sanitise_for_format()` helper in the template that is aware of `knitr::is_latex_output()` and `knitr::is_html_output()`.
3. Use `xelatex` instead of `pdflatex` as the PDF engine in the template YAML (`latex_engine: xelatex`) — this resolves most Unicode encoding issues without needing character substitution.
4. Declare `\VignetteEncoding{UTF-8}` in any CRAN-shipped vignette.
5. Run `tools::showNonASCII()` on a sample LLM output fixture during development to discover problem characters early.

**Warning signs:**
- `pdflatex` error: "Package inputenc Error: Unicode character".
- Word document fails to open: "The file is corrupt and cannot be opened."
- CI PDF build succeeds on a XeLaTeX machine but fails on a pdflatex-only machine.
- LLM output contains `--` (em-dash) or curly quotes visible in `cat()` output.

**Phase to address:**
Phase 2 (multi-format rendering). Build the sanitisation helper during template design, test against a sample LLM response that intentionally includes em-dashes and special characters.

---

### Pitfall 6: Offline Fallback Silently Degrades Without User Visibility

**What goes wrong:**
`es_report()` with no provider configured falls back to the existing rule-based offline advice engine. The offline output is complete and correct, but it is qualitatively different from AI-narrated prose: it is a structured list of KB recommendations rather than fluent paragraph-form narrative. If the report renders with no visible distinction between "AI-narrated" and "rule-based" mode, a researcher who expected an AI narrative will not know they received the fallback — and may cite the offline output as AI interpretation in a paper.

The inverse problem also exists: a provider is configured but fails (network timeout, quota exceeded, rate limit). The `es_advise()` failure path returns `.empty_advice()` — an `Advice` object with empty `interpretation` and empty `recommendations`. If `es_report()` renders an empty `Advice` without detecting the empty state, the report's AI section is silently blank or omitted with no explanation.

**Why it happens:**
Graceful degradation is designed to never crash, but "looks fine" and "is delivering the intended value" are different conditions. Empty `Advice` objects and offline KB `Advice` objects both pass `inherits(advice, "Advice")` checks. Without explicit state inspection, the report wrapper cannot distinguish them.

**How to avoid:**
1. Inspect `advice$source` and `advice$is_deterministic` explicitly in `es_report()` before rendering. Use `!is.null(advice) && inherits(advice, "Advice") && !isTRUE(advice$is_deterministic) && nzchar(advice$interpretation)` to detect a live AI-grounded result.
2. Use distinct section headings: "AI Interpretation (grounded)" for a live LLM path, "Methodology Summary (offline rule-based)" for the KB path, and "AI Interpretation (unavailable — configure a provider with `provider()`)" for the empty-Advice failure case.
3. Always emit a `message()` from `es_report()` stating which mode was used, so the user sees it in the console even if the report looks complete.
4. Check `length(advice$recommendations) == 0L && !nzchar(advice$interpretation)` to detect the empty-Advice failure case and render a placeholder section rather than silently omitting it.
5. Surface `advice$n_dropped` in the report footer so users know if any recommendations were dropped by the grounding guard.

**Warning signs:**
- Report renders successfully for a user who has no provider configured, but the AI section looks identical to the one produced with a provider.
- `advice$is_deterministic` is `TRUE` but the section header says "AI Interpretation."
- `advice$interpretation` is `""` and no placeholder or explanation is rendered.

**Phase to address:**
Phase 1 (report wrapper architecture) and Phase 3 (offline-first validation). The mode-detection logic belongs in the wrapper design; the regression test that the offline report clearly communicates its mode belongs in Phase 3.

---

### Pitfall 7: Multi-Section Prose Grounding Gaps (Interpretation Field Not Validated)

**What goes wrong:**
The existing `.validate_grounding()` guard checks `recommendations[].evidence[]` arrays. The `interpretation` field is an unconstrained string — the guard reads it but does not scan it for numeric values. For a single `report_writing` advice call, this means the LLM can write "The event-day abnormal return of 4.2% (t = 3.91, p < 0.01) represents a significant market reaction." If the actual computed value is 3.8% at t = 2.14, the guard is silent.

For `es_report()` with multiple sections, if each section is a separate `es_advise()` call, each section's `interpretation` field is independently unvalidated. Fabrication risk is proportional to the number of sections and the length of each interpretation string.

**Why it happens:**
The guard architecture was designed for the `recommend_stat` / `flag_robustness` use case where the grounded claim lives in `evidence[]`. The `report_writing` task type uses `interpretation` as a free-form prose field that was not in scope for the original guard. Adding a full report increases the surface area of free-form prose dramatically.

**How to avoid:**
1. For the multi-section report, prefer structured fields over free prose. Define a schema where each section has a `prose` string field and a `section_evidence[]` array in the same shape as the existing `evidence[]` requirement. The guard then validates every section's evidence array, and the prose becomes a "fill in this template given these validated values" task.
2. Add a prose numeric scanner to `.validate_grounding()`: extract all decimal numbers from `interpretation` (regex `\b\d+\.?\d*\b`), attempt to match each to the nearest diagnostic value. Any number that cannot be matched within a configurable tolerance triggers an appended caveat.
3. Instruct the LLM to avoid numeric prose in the system prompt: "In prose sections, do not write numeric values. Instead write qualitative descriptions (e.g. 'highly significant' rather than 't = 3.91'). Numeric values appear only in the evidence[] arrays."
4. Add a cross-section consistency check: if `es_report()` calls `es_advise()` multiple times, the same diagnostic key cited in two sections must have the same value.

**Warning signs:**
- Prose interpretation contains specific decimal values (t-statistics, p-values, return percentages) that differ from `es_diagnostics()` output.
- The same diagnostic value appears in two sections but with different magnitudes.
- Guard reports `n_dropped = 0` but the rendered report contains a numerical claim that is not in the diagnostics.

**Phase to address:**
Phase 1 (grounding architecture). The prose scanner should be part of the grounding guard design from the start. Define the multi-section JSON schema before writing the prompt or template.

---

### Pitfall 8: DESCRIPTION Suggests Boundary Violated by New Rendering Dependencies

**What goes wrong:**
`officedown` (Word output), `tinytex` (PDF toolchain check), `webshot2` (plotly static fallback) might be added to `Imports` instead of `Suggests` by mistake, or might be called without a `requireNamespace()` guard. Either causes an `R CMD check` ERROR: a package in `Imports` that is not available on the check machine causes immediate load failure. A package called without `requireNamespace()` while in Suggests produces "Package required but not installed" which CRAN treats as a submission blocker.

Additionally: `officedown` is a heavy package (imports officer, rlang, knitr, etc.). Adding it to `Imports` would impose it on all users, even those who only want HTML output.

**Why it happens:**
During rapid development, it is tempting to add packages to `Imports` for autocomplete convenience and to avoid the `requireNamespace()` boilerplate. The Suggests boundary discipline from v0.60.0 is easily eroded across multiple phases if not enforced by CI.

**How to avoid:**
1. For each new rendering dependency — rmarkdown, knitr, officedown, officer, flextable — add to `Suggests` only; guard every call site with `requireNamespace("pkg", quietly = TRUE)`.
2. Run `R CMD check --as-cran` in CI with a matrix that excludes all Suggests packages. This catches unguarded calls immediately.
3. In the `es_report()` function, at the top of each format-specific branch, check the required Suggests packages explicitly and stop with an install message.
4. Add a CI job step: `R CMD INSTALL --no-suggests .` then run `devtools::test()`. Any test that fails because a Suggests package is absent reveals an unguarded call.

**Warning signs:**
- `R CMD check --as-cran` NOTE: "Package suggested but not available for checking: 'officedown'".
- A test that calls `es_report(format = "docx")` without `skip_if_not_installed("officedown")` fails on a clean CI runner.
- CRAN submission feedback: "Please add the package to the Suggests field."

**Phase to address:**
Phase 1 (CRAN hygiene scaffolding) and Phase 2 (multi-format rendering). Both phases must enforce the Suggests discipline; the CI matrix without Suggests is the automated gate.

---

### Pitfall 9: Report Template Mutates R6 Task Object via render() Reference Semantics

**What goes wrong:**
`rmarkdown::render()` executes the `.Rmd` template in an isolated environment (`envir = new.env(parent = globalenv())`). The current `generate_report()` passes `params = list(task = task, ...)` to inject data. R6 objects are reference types — `params$task` in the template is the same R6 object, not a copy. If the template calls any mutating method (even accidentally, via active bindings with side effects), the modification propagates back to the caller's task object silently and unexpectedly.

Additionally, large `EventStudyTask` objects serialised through the `params` list can cause memory doubling — the original object plus the copy in the render environment both live in RAM simultaneously during rendering.

**Why it happens:**
Developers expect `params` to create a copy as it does for simple R objects (vectors, data frames). R6 objects break this expectation because they are environments, not values. The current `generate_report()` passes the task directly without cloning.

**How to avoid:**
1. Deep-clone the task before passing to render: `params = list(task = task$clone(deep = TRUE), ...)`.
2. Make the template read-only with respect to the task: never call mutating methods inside the template. Use only accessor methods.
3. For very large tasks, extract only the data the template needs before calling render and pass the extracted tibbles instead of the full R6 object.

**Warning signs:**
- The task object in the calling environment has modified state after `generate_report()` returns.
- Rendering a large study (50+ firms) causes an out-of-memory error or significant memory spike.
- The same task rendered twice produces different output due to reference-side-effects from the first render.

**Phase to address:**
Phase 2 (multi-format rendering). Add the deep-clone and read-only convention to the template design specification; add a regression test that verifies the task is unmodified after render.

---

### Pitfall 10: knitr Figure Directory Deletion When Rendering Multiple Formats Sequentially

**What goes wrong:**
When `es_report()` renders to multiple formats in a single call (e.g. HTML then PDF then Word), calling `rmarkdown::render()` sequentially for each format can delete the figure directory needed by a subsequent format. This is a documented knitr behaviour: when one format finishes and cleans up its figure path, it may remove the parent figure directory, breaking the next format's figure references.

Specifically: HTML renders to `report_files/figure-html/`, PDF renders to `report_files/figure-latex/`. When the HTML render finishes and removes `report_files/`, the PDF render's figure path is gone.

**Why it happens:**
The knitr figure cleanup logic does not coordinate across sequential `render()` calls for the same source file. The bug only manifests in the multi-format combination case, not when each format is rendered in isolation.

**How to avoid:**
1. Use `rmarkdown::render(output_format = "all")` when rendering multiple formats simultaneously rather than calling render once per format. This lets rmarkdown coordinate figure directory lifecycle internally.
2. Alternatively, render to a separate temporary file per format with a unique `output_dir` per format, so figure directories do not overlap.
3. When building `es_report()` for multi-format output, test the `c("html", "pdf", "docx")` combination explicitly, not just each format in isolation.

**Warning signs:**
- Second-format render fails with "figure file not found" or produces blank figures.
- Figure directories disappear during sequential format rendering.
- HTML renders correctly but PDF has broken image references.

**Phase to address:**
Phase 2 (multi-format rendering). Include a multi-format combination test from day one; the bug only manifests in the combination case.

---

## Technical Debt Patterns

| Shortcut | Immediate Benefit | Long-term Cost | When Acceptable |
|----------|-------------------|----------------|-----------------|
| Skip prose numeric scanning in grounding guard | Faster Phase 1 | Ungrounded numbers appear in report prose; violates "never silently wrong" invariant | Never — the invariant is non-negotiable |
| Use `rmarkdown::word_document()` instead of `officedown::rdocx_document()` | Fewer dependencies | Tables lose image/hyperlink support in Word; flextable falls back to plain text | Only if Word tables never contain embedded images |
| Pass full R6 task object to render() without cloning | Simpler code | Mutating template side effects corrupt caller state silently | Only if template is proven read-only and tested |
| Render all formats in a single `render(output_format = "all")` | Avoids figure directory deletion bug | Higher peak memory; single point of failure for all formats | Acceptable specifically for the multi-format case |
| Default format renders all at once | Convenient API | PDF/Word toolchain errors break the whole call even when user only needed HTML | Never — make HTML the sole default, others opt-in |
| Add `tinytex` to Suggests | Documents PDF toolchain | Users without TinyTeX see misleading "package not installed" rather than "LaTeX not found" | Never — tinytex is a user-level concern |
| Put `es_report()` example outside `\dontrun{}` | Example appears to execute in docs | `R CMD check` fails on CRAN machines without pandoc | Never |

---

## Integration Gotchas

| Integration | Common Mistake | Correct Approach |
|-------------|----------------|------------------|
| rmarkdown + Suggests | Calling `rmarkdown::render()` without `requireNamespace()` guard | Guard every call; stop with install message if absent |
| officedown Word output | Using `rmarkdown::word_document()` output format | Use `officedown::rdocx_document()` for full flextable support |
| plotly in PDF/Word | Expecting plotly to render via always_allow_html | Use `knitr::is_html_output()` to switch to ggplot2 for static formats |
| tinytex + CI | Calling `tinytex::install_tinytex()` in tests or examples | Never call install functions in tests; skip PDF tests without tinytex |
| httptest2 mocking | Calling real LLM API in testthat suite | Use `with_mock_api()` with pre-recorded fixtures; `skip_on_cran()` for live tests |
| LLM prose + non-ASCII | Embedding LLM output directly in rmarkdown | Sanitise: strip/replace `&`, smart quotes, em-dashes before embedding |
| R6 task + render params | Passing task directly via params list | Deep-clone before passing: `task$clone(deep = TRUE)` |
| Multi-format sequential render | Calling `render()` once per format in a loop | Use `render(output_format = "all")` or unique output dirs per format |

---

## Performance Traps

| Trap | Symptoms | Prevention | When It Breaks |
|------|----------|------------|----------------|
| Full task in render params | Peak RAM doubles during render for large studies | Pre-extract result tibbles; pass only what the template needs | Studies with 50+ firms, long windows |
| LLM call per report section | Slow report generation; multiple round-trips | Batch all section prompts in one call with a multi-section schema | Reports with 4+ sections on a slow provider |
| knitr cache across formats | Stale cache from HTML render used for PDF | Set `cache = FALSE` in template for publication reports | Any time chart data changes but cache is not invalidated |
| Sequential multi-format render | Figure directory deletion mid-run | Use `render(output_format = "all")` | Any multi-format render |

---

## Security Mistakes

| Mistake | Risk | Prevention |
|---------|------|------------|
| Logging the full prompt (contains diagnostics JSON) | Diagnostic data written to log files | Never log the full prompt; log only provider name, task_type, token count |
| Embedding provider API key in report metadata | Key exposed in HTML source, PDF metadata, or Word document properties | Never embed API key; source only from environment; strip from render params |
| LLM-generated content injected without sanitisation into Word XML | XML injection breaks docx structure; malformed documents | Sanitise all LLM text through a format-aware escaper before embedding |
| Report output file world-readable on shared systems | Diagnostic data (model results, firm identifiers) exposed | Document that output_file defaults to working directory; warn user |

---

## "Looks Done But Isn't" Checklist

- [ ] **Grounding guard on prose:** Verify `interpretation` field numeric values are cross-checked against diagnostics, not just `evidence[]` arrays.
- [ ] **Offline section heading:** Confirm the rendered report visibly distinguishes AI-grounded vs. offline rule-based narrative — check `advice$is_deterministic` flag.
- [ ] **Empty Advice handling:** Confirm an empty `Advice` object (failed provider call) renders a placeholder section, not a blank report.
- [ ] **PDF plotly fallback:** Confirm PDF/Word output uses ggplot2 static plots, not plotly — check for blank figures in a test render.
- [ ] **Non-ASCII sanitisation:** Confirm LLM text with em-dashes and smart quotes does not break PDF/Word render — test with a fixture that includes these characters.
- [ ] **CRAN check clean:** Run `R CMD check --as-cran` without Suggests installed — confirm no ERRORs or WARNINGs.
- [ ] **R6 task immutability:** Confirm the task object is unmodified after `es_report()` returns — compare `task$results` before and after.
- [ ] **Multi-format combination:** Confirm rendering HTML + PDF + Word in sequence does not delete figure directories — inspect output directory after the run.
- [ ] **Word image support:** Confirm flextable tables with images use `officedown::rdocx_document()`, not `rmarkdown::word_document()`.
- [ ] **Skip discipline:** Confirm every test that calls `generate_report()` or `es_report()` has `skip_on_cran()` and `skip_if_not_installed("rmarkdown")`.

---

## Recovery Strategies

| Pitfall | Recovery Cost | Recovery Steps |
|---------|---------------|----------------|
| Ungrounded prose in shipped report | HIGH | Patch grounding guard with prose scanner; add regression test; issue patch release |
| CRAN rejection for missing `\dontrun{}` | LOW | Add `\dontrun{}` wrapper; resubmit; one-line fix |
| PDF toolchain breaks CI | MEDIUM | Add `skip_if(!tinytex::is_tinytex())` guards; exclude PDF tests from CRAN matrix |
| Plotly blank in Word/PDF | MEDIUM | Add `knitr::is_html_output()` branch to template; switch to ggplot2 for static |
| Non-ASCII breaks Word generation | LOW | Add character sanitisation helper; test with fixture containing problematic chars |
| R6 task mutation via render | MEDIUM | Add `$clone(deep = TRUE)` before params pass; add before/after state test |
| Suggests boundary violated | LOW | Move package back to Suggests; add `requireNamespace()` guard; add CI matrix without Suggests |
| Silent offline fallback | MEDIUM | Add mode-detection logic; add distinct section headings; add console message |

---

## Pitfall-to-Phase Mapping

| Pitfall | Prevention Phase | Verification |
|---------|------------------|--------------|
| Grounding guard does not cover prose | Phase 1 (wrapper architecture) | Test: assert `.validate_grounding()` drops/caveats a recommendation whose interpretation cites a fabricated number |
| CRAN check triggered by render in examples | Phase 1 (CRAN hygiene) | CI: R CMD check --as-cran on clean runner; all render examples in `\dontrun{}` |
| PDF tinytex breaks in CI | Phase 2 (multi-format rendering) | CI: PDF test matrix job with `skip_if(!tinytex::is_tinytex())` |
| Plotly blank in static formats | Phase 2 (multi-format rendering) | Integration test: render to PDF/Word, assert figure files are non-empty PNGs |
| Non-ASCII characters break PDF/Word | Phase 2 (multi-format rendering) | Test: render template with em-dash and ampersand fixture, assert no error |
| Silent offline fallback | Phase 1 + Phase 3 (offline-first validation) | Test: confirm `advice$is_deterministic = TRUE` produces distinct heading; confirm empty Advice renders placeholder |
| Multi-section prose grounding gaps | Phase 1 (grounding architecture) | Test: multi-section schema with fabricated number in interpretation; assert caveat is appended |
| Suggests boundary violation | Phase 1 + Phase 2 | CI: `R CMD INSTALL --no-suggests` + `devtools::test()` — zero failures |
| R6 task mutation via render params | Phase 2 (multi-format rendering) | Test: task state before and after `es_report()` is identical |
| Figure directory deletion in multi-format render | Phase 2 (multi-format rendering) | Integration test: HTML + PDF + Word sequential render, all figure files present after |

---

## Sources

- [R-hub blog: Optimal workflows for package vignettes](https://blog.r-hub.io/2020/06/03/vignettes/) — CRAN vignette build vs. check distinction, VignetteBuilder discipline
- [ropensci HTTP testing book: Graceful HTTP packages](https://books.ropensci.org/http-testing/graceful.html) — skip_on_cran, offline mock patterns for httr2 packages
- [httptest2 CRAN package](https://cran.r-project.org/web/packages/httptest2/index.html) — with_mock_api, capture_requests, without_internet for httr2
- [TDS: Your JSON Is Valid but Your Data Is Wrong](https://towardsdatascience.com/your-json-is-valid-but-your-data-is-wrong-five-failure-modes-llm-structured-outputs-wont-catch/) — five structured-output failure modes that bypass schema validation
- [Parasoft: Controlling LLM Hallucinations at the Application Level](https://www.parasoft.com/blog/controlling-llm-hallucinations-application-level-best-practices/) — multi-layer grounding defense strategy
- [R-bloggers: Word Up — officedown/officer/flextable notes](https://www.r-bloggers.com/2022/09/word-up-notes-on-working-with-officer-officedown-and-flextable-to-generate-word-documents-in-rmarkdown/) — XML special characters breaking docx, table rendering issues
- [officeverse: officedown for Word](https://ardata-fr.github.io/officeverse/officedown-for-word.html) — rdocx_document requirement for image/hyperlink support in flextable
- [plotly GitHub issue #889](https://github.com/ropensci/plotly/issues/889) — plotly not visible in PDF rmarkdown output
- [R Markdown Cookbook: LaTeX or HTML output](https://bookdown.org/yihui/rmarkdown-cookbook/latex-html.html) — knitr::is_latex_output() and is_html_output() for conditional generation
- [Peter Ralph: R+markdown gotchas](https://petrelharp.github.io/r-markdown-tutorial/gotchas.html) — multi-format rendering pitfalls
- [luke.geek.nz: Silent LLM Fallbacks](https://luke.geek.nz/azure/silent-llm-fallback/) — silent fallback trust erosion
- [zenml.io: LLM fallback mechanisms](https://www.zenml.io/llmops-database/implementing-llm-fallback-mechanisms-for-production-incident-response-system) — graceful degradation vs. silent failure distinction
- [GSAR: Typed Grounding for Multi-Agent LLMs](https://arxiv.org/abs/2604.23366) — grounding typology for multi-section report claims
- [testthat skipping docs](https://testthat.r-lib.org/articles/skipping.html) — skip_on_cran, skip_if_not_installed discipline
- [R Packages (2e): R CMD check appendix](https://r-pkgs.org/R-CMD-check.html) — non-ASCII character handling, encoding declarations
- [tinytex GitHub issue #436](https://github.com/rstudio/tinytex/issues/436) — tlmgr mirror failures in CI

---
*Pitfalls research for: multi-format AI-narrated reporting in a CRAN R package (EventStudy v0.64.0)*
*Researched: 2026-09-06*
