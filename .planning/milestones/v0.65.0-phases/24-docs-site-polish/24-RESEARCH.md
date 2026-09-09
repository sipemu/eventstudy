# Phase 24: Docs & Site Polish — Research

**Researched:** 2026-09-09
**Domain:** R package documentation — roxygen2 @family/@seealso, README markdown, pkgdown configuration, CI integration
**Confidence:** HIGH

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

- **@family grouping scheme:** 7 `eventstudy-`-prefixed families (`eventstudy-pipeline`, `eventstudy-models`, `eventstudy-statistics`, `eventstudy-tasks`, `eventstudy-plots`, `eventstudy-export`, `eventstudy-advisor`). Every function gets at most ONE `@family`. Each family must have >= 2 members (roxygen suppresses single-member family lists). Final membership verified against the live NAMESPACE.
- **@seealso policy:** Hub-and-spoke on the four pipeline entry points; cross-family bridges only (NOT intra-family — `@family` already generates those). Every `\link{}` target must resolve to an exported, documented object.
- **README Ecosystem section:** Plain Markdown, ASCII-only, placed near the top (after badges, before or right after Installation). Three tools: Google Sheets template · R package · WebAssembly app. Link to eventstudy.de. Anchor phrase: "Event Study Analysis Made Simple".
- **Stale-count reconciliation:** README line 32 "13 Return Models" → "**15+ Return Models**"; line 34 "11 Test Statistics" → "**12 Test Statistics**".
- **pkgdown home markers:** Syntax is at Claude's discretion within the constraint of keeping `home: sidebar: false` as already set.
- **Vignettes:** Do NOT add a new CRAN vignette. Tighten existing `introduction.Rmd` (task → run → inspect → export/plot). Rich content stays in `vignettes/articles/`.
- **CI:** Add `pkgdown::check_pkgdown()` to the EXISTING `.github/workflows/pkgdown.yaml` job — do NOT create a new workflow.
- **Hard boundary:** Docs-only. No behavior change, no new Imports, no new `R CMD check` NOTEs/WARNINGs. DESCRIPTION unchanged. `man/` regenerated only via `devtools::document()` — never hand-edited.
- **YOLO mode:** Grey-area decisions auto-accepted at the recommended answer.

### Claude's Discretion

- Exact `@family` tag spelling (confirmed: `eventstudy-`-prefixed).
- Per-function membership edge cases (resolved below — see family map).
- Precise README Ecosystem wording/placement.
- Exact home-marker syntax.
- Specific CI step placement.

### Deferred Ideas (OUT OF SCOPE)

- Any behavior/API changes, new models/statistics, new Imports.
- New CRAN vignettes.
- Live pkgdown CI deploy / GitHub Pages enablement.

</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| DOCS-01 | `@family` + `@seealso` roxygen tags added across pipeline/model/statistic/advisor functions so the Reference index cross-links | Family membership map (Section A), @seealso plan (Section B), source file map (Section A sub-tables) |
| DOCS-02 | README refreshed with an Ecosystem section linking the three tools and eventstudy.de; pkgdown home markers added | README edits plan (Section C), exact line edit specifications |
| DOCS-03 | `pkgdown::check_pkgdown()` added to CI (catches silent broken cross-references); navbar/news wiring verified; rough edges fixed | CI location (Section D2), pre-existing check_pkgdown failure documented and fixed (Section C2) |
| DOCS-04 | Existing vignettes/articles tightened — getting-started flow and cross-links improved (no new CRAN vignettes) | Vignette analysis (Section D1) |

</phase_requirements>

---

## Summary

Phase 24 is a docs-only pass over 11 source files plus 3 config/workflow files. The work divides into two independent streams that can run concurrently: (a) roxygen stream — add `@family` and `@seealso` to `R/*.R` files then call `devtools::document()` ONCE at the end; (b) markdown/config stream — edit README.md, `_pkgdown.yml`, `introduction.Rmd`, and `.github/workflows/pkgdown.yaml` independently. The streams touch disjoint files and may be planned in separate waves.

The most important pre-existing bug discovered during research: `pkgdown::check_pkgdown()` already FAILS with 6 missing topics (`format.Advice`, `format.EventStudySummary`, `format.es_advice`, `format.es_cross_sectional`, `format.es_diagnostics`, `format.es_simulation`). These were added in Phase 23 with `@export` but were never placed in `_pkgdown.yml`. The fix is one line in `_pkgdown.yml`: add `starts_with("format.")` to the `internal` section. This must happen BEFORE or simultaneously with wiring `check_pkgdown` into CI, or the CI step will immediately fail.

**Primary recommendation:** Serialize the roxygen work into a single wave (DOCS-01 edits → one `devtools::document()` call), then run the non-roxygen edits (README, _pkgdown.yml, vignette, CI) in a parallel wave, and gate the final CI step on the fixed `_pkgdown.yml`.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| @family cross-links | R source files (roxygen headers) | man/*.Rd (generated) | roxygen2 generates the Rd; never hand-edit |
| @seealso hub-and-spoke | R source files (roxygen headers) | man/*.Rd (generated) | Same — write in R/, regenerate man/ |
| README Ecosystem section | README.md | _pkgdown.yml (home page source) | README is the CRAN and pkgdown home page |
| pkgdown reference index fixes | _pkgdown.yml | — | Declarative YAML config |
| CI check_pkgdown | .github/workflows/pkgdown.yaml | — | Add one step to existing job |
| Vignette tightening | vignettes/introduction.Rmd | — | Existing file, minimal edits |

---

## Section A: @family Membership Map (DOCS-01)

### A1. Critical Name-Resolution Rules

The following CONTEXT.md names do NOT match live NAMESPACE exports — use the LIVE names only:

| CONTEXT.md (wrong) | Live NAMESPACE export (correct) |
|--------------------|---------------------------------|
| `advise` | `es_advise` [VERIFIED: NAMESPACE line 15] |
| `plot_diagnostic` | `plot_diagnostics` (plural) [VERIFIED: NAMESPACE line 53] |
| `knowledge_base` | `es_kb` [VERIFIED: NAMESPACE line 16] |

Using the wrong name in `@seealso \code{\link{...}}` or `@family` causes `R CMD check` Rd failures.

### A2. Complete Family Membership Table

**Source:** NAMESPACE `export()` entries verified by `grep "^export(" NAMESPACE` (77 entries total). [VERIFIED: NAMESPACE]

Every function gets exactly one `@family` tag. The 3 `download_*` functions are left un-familied — they are data utilities that do not fit the 7 analysis families, and leaving them un-familied causes no check failures.

#### Family: `eventstudy-pipeline` (6 members)

| Function | Source File | Notes |
|----------|-------------|-------|
| `run_event_study` | `R/execute.R` | Main entry point |
| `fit_model` | `R/execute.R` | Step 2 of pipeline |
| `calculate_statistics` | `R/execute.R` | Step 3 of pipeline |
| `prepare_event_study` | `R/prepare_event_study.R` | Step 1 of pipeline |
| `EventStudyTask` | `R/task.R` | Task container / setup |
| `ParameterSet` | `R/parameter_set.R` | Configuration / setup |

#### Family: `eventstudy-models` (15 members)

| Function | Source File |
|----------|-------------|
| `MarketModel` | `R/models.R` |
| `MarketAdjustedModel` | `R/models.R` |
| `ComparisonPeriodMeanAdjustedModel` | `R/models.R` |
| `LinearFactorModel` | `R/models.R` |
| `FamaFrench3FactorModel` | `R/models.R` |
| `FamaFrench5FactorModel` | `R/models.R` |
| `Carhart4FactorModel` | `R/models.R` |
| `GARCHModel` | `R/models.R` |
| `BHARModel` | `R/models.R` |
| `VolumeModel` | `R/models.R` |
| `VolatilityModel` | `R/models.R` |
| `RollingWindowModel` | `R/models_time_varying.R` |
| `DCCGARCHModel` | `R/models_time_varying.R` |
| `SimpleReturn` | `R/return_calculation.R` |
| `LogReturn` | `R/return_calculation.R` |

Note: `CustomModel` is NOT in NAMESPACE (no `@export`) — it is an internal class and does not receive `@family`. [VERIFIED: NAMESPACE — no `export(CustomModel)` entry]

#### Family: `eventstudy-statistics` (23 members)

| Function | Source File |
|----------|-------------|
| `ARTTest` | `R/single_event_test_statistics.R` |
| `CARTTest` | `R/single_event_test_statistics.R` |
| `BHARTTest` | `R/single_event_test_statistics.R` |
| `CSectTTest` | `R/multi_event_test_statistics.R` |
| `PatellZTest` | `R/multi_event_test_statistics.R` |
| `BMPTest` | `R/multi_event_test_statistics.R` |
| `SignTest` | `R/multi_event_test_statistics.R` |
| `GeneralizedSignTest` | `R/multi_event_test_statistics.R` |
| `RankTest` | `R/multi_event_test_statistics.R` |
| `KolariPynnonenTest` | `R/multi_event_test_statistics.R` |
| `CalendarTimePortfolioTest` | `R/multi_event_test_statistics.R` |
| `SingleEventStatisticsSet` | `R/test_statistics_set.R` |
| `MultiEventStatisticsSet` | `R/test_statistics_set.R` |
| `StatisticsSetBase` | `R/test_statistics_set.R` |
| `bootstrap_test` | `R/bootstrap.R` |
| `adjust_p_values` | `R/p_adjustment.R` |
| `cross_sectional_regression` | `R/cross_sectional.R` |
| `car_by_group` | `R/cross_sectional.R` |
| `car_quantiles` | `R/cross_sectional.R` |
| `simulate_event_study` | `R/simulation.R` |
| `validate_task` | `R/task_validation.R` |
| `model_diagnostics` | `R/diagnostics.R` |
| `pretrend_test` | `R/diagnostics.R` |

Rationale for inclusions: `validate_task`, `model_diagnostics`, `pretrend_test` are statistical validation tools; `car_by_group`, `car_quantiles`, `simulate_event_study` are CAR-level analysis/inference — all belong with the statistical analysis family. This keeps the family cohesive and the count well above the >=2 floor.

#### Family: `eventstudy-tasks` (8 members)

| Function | Source File |
|----------|-------------|
| `PanelEventStudyTask` | `R/panel_event_study.R` |
| `estimate_panel_event_study` | `R/panel_event_study.R` |
| `plot_panel_event_study` | `R/panel_event_study.R` |
| `IntradayEventStudyTask` | `R/task_intraday.R` |
| `prepare_intraday_event_study` | `R/task_intraday.R` |
| `nonparametric_intraday_test` | `R/task_intraday.R` |
| `SyntheticControlTask` | `R/synthetic_control.R` |
| `estimate_synthetic_control` | `R/synthetic_control.R` |
| `sc_placebo_test` | `R/synthetic_control.R` |

Note: `plot_panel_event_study` and `plot_synthetic_control` are specialized to their task type and therefore belong in `eventstudy-tasks` rather than `eventstudy-plots`. This avoids a forced cross-family membership conflict (each function gets only ONE family).

Wait — `plot_panel_event_study` and `plot_synthetic_control` COULD go in either `eventstudy-tasks` or `eventstudy-plots`. Decision: assign to `eventstudy-tasks` because they are conceptually task-specific visualizations; `eventstudy-plots` covers the general-purpose plot functions.

Revised family: `eventstudy-tasks` has 9 members (including `sc_placebo_test`).

#### Family: `eventstudy-plots` (6 members)

| Function | Source File |
|----------|-------------|
| `plot_stocks` | `R/plotting.R` |
| `plot_event_study` | `R/plotting.R` |
| `plot_diagnostics` | `R/plotting.R` |
| `plot_car_distribution` | `R/cross_sectional.R` |
| `theme_eventstudy` | `R/theme.R` |
| `es_colours` | `R/theme.R` |

#### Family: `eventstudy-export` (4 members)

| Function | Source File |
|----------|-------------|
| `export_results` | `R/export.R` |
| `tidy.EventStudyTask` | `R/export.R` |
| `generate_report` | `R/report.R` |
| `es_report` | `R/report.R` |

#### Family: `eventstudy-advisor` (10 members)

| Function | Source File |
|----------|-------------|
| `es_advise` | `R/advise.R` |
| `es_diagnostics` | `R/es_diagnostics.R` |
| `es_kb` | `R/knowledge_base.R` |
| `recommend_stat` | `R/advise_offline.R` |
| `flag_robustness` | `R/advise_offline.R` |
| `provider` | `R/provider.R` |
| `ProviderBase` | `R/provider.R` |
| `AnthropicProvider` | `R/provider.R` |
| `OpenAICompatProvider` | `R/provider.R` |
| `CustomProvider` | `R/provider.R` |

#### Un-familied (no `@family` tag added)

| Function | Source File | Why un-familied |
|----------|-------------|-----------------|
| `download_stock_data` | `R/data_download.R` | Data utility — not analysis |
| `download_factor_data` | `R/data_download.R` | Data utility — not analysis |
| `download_risk_free_rate` | `R/data_download.R` | Data utility — not analysis |

### A3. Family Count Summary

| Family | Members | Min Threshold |
|--------|---------|---------------|
| eventstudy-pipeline | 6 | >= 2 ✓ |
| eventstudy-models | 15 | >= 2 ✓ |
| eventstudy-statistics | 23 | >= 2 ✓ |
| eventstudy-tasks | 9 | >= 2 ✓ |
| eventstudy-plots | 6 | >= 2 ✓ |
| eventstudy-export | 4 | >= 2 ✓ |
| eventstudy-advisor | 10 | >= 2 ✓ |
| Un-familied | 3 | — |
| **Total** | **76** | 77 exports minus 1 (`tidy.EventStudyTask` is an S3 method but gets `@family` in `export.R`) |

### A4. roxygen @family Syntax

```r
#' @family eventstudy-pipeline
```

Add one `@family` tag per function, in the roxygen block immediately above `@export`. Tag goes BEFORE `@export`. Existing `@seealso` tags in the same block are kept as-is and supplemented (never replaced).

---

## Section B: @seealso Plan (DOCS-02)

### B1. Hub-and-Spoke: Pipeline Entry Points

Each of the four pipeline steps `@seealso` the other three. These are cross-family bridges (pipeline → pipeline, which is within the same family, BUT the hub-and-spoke is the explicit instruction from CONTEXT). Since `@family` already generates "See Also" clusters within a family, these `@seealso` entries add a more prominent cross-link section showing the ordered flow.

**Source files:** `R/execute.R` (run_event_study, fit_model, calculate_statistics), `R/prepare_event_study.R` (prepare_event_study)

```r
# In run_event_study roxygen block:
#' @seealso
#'   \code{\link{prepare_event_study}}, \code{\link{fit_model}},
#'   \code{\link{calculate_statistics}}

# In prepare_event_study roxygen block:
#' @seealso
#'   \code{\link{run_event_study}}, \code{\link{fit_model}},
#'   \code{\link{calculate_statistics}}

# In fit_model roxygen block:
#' @seealso
#'   \code{\link{run_event_study}}, \code{\link{prepare_event_study}},
#'   \code{\link{calculate_statistics}}

# In calculate_statistics roxygen block:
#' @seealso
#'   \code{\link{run_event_study}}, \code{\link{prepare_event_study}},
#'   \code{\link{fit_model}}
```

### B2. Cross-Family Bridges

Per CONTEXT policy: sparse cross-family bridges only. `@family` generates intra-family clusters automatically — `@seealso` is reserved for cross-family navigation.

| Function | File | Add @seealso pointing to |
|----------|------|--------------------------|
| `run_event_study` | `R/execute.R` | Pipeline @seealso already covers; also link `MarketModel`, `ARTTest` (examples of models/statistics family) |
| `es_advise` | `R/advise.R` | Already has `@seealso` pointing to `es_diagnostics`, `recommend_stat`, `flag_robustness`, `provider` — these are fine; ADD `run_event_study` as the pipeline bridge |
| `es_diagnostics` | `R/es_diagnostics.R` | Already has `@seealso`; ADD `run_event_study` bridge |
| `es_report` | `R/report.R` | Already has `@seealso \code{\link{generate_report}}, \code{\link{es_diagnostics}}`; ADD `run_event_study` bridge |
| `export_results` | `R/export.R` | ADD `@seealso \code{\link{run_event_study}}, \code{\link{tidy.EventStudyTask}}` |

**Rule: do NOT create dense N×N webs.** Pipeline entry points `@seealso` each other plus 1-2 representatives from models/statistics. Advisor/export functions `@seealso` the pipeline entry point. Everything else is handled by `@family`.

### B3. Existing @seealso Tags — Do Not Regress

The following already have `@seealso` and must be preserved as-is (only extend, never replace): [VERIFIED: grep of R/*.R files]

| File | Current @seealso |
|------|-----------------|
| `R/contract.R` L46 | `\code{\link{ParameterSet}}, \code{\link{MarketModel}}` |
| `R/advise_offline.R` L32 | `\code{\link{flag_robustness}}, \code{\link{es_diagnostics}}, \code{\link{es_kb}}` |
| `R/advise_offline.R` L92 | `\code{\link{recommend_stat}}, \code{\link{es_diagnostics}}, \code{\link{es_kb}}` |
| `R/report.R` L55 | `\code{\link{generate_report}}, \code{\link{es_diagnostics}}` |
| `R/advise.R` L972 | `\code{\link{es_diagnostics}}, \code{\link{recommend_stat}}, \code{\link{flag_robustness}}, \code{\link{provider}}` |
| `R/knowledge_base.R` L426 | `\code{\link{recommend_stat}}, \code{\link{flag_robustness}}, \code{\link{es_diagnostics}}` |
| `R/theme.R` L17 | `\code{\link{theme_eventstudy}}, \code{\link{plot_event_study}}` |
| `R/theme.R` L50 | `\code{\link{es_colours}}, \code{\link{plot_event_study}}` |
| `R/es_diagnostics.R` L47 | `\code{\link{model_diagnostics}}, \code{\link{recommend_stat}}, \code{\link{flag_robustness}}` |

All targets in this table resolve to live NAMESPACE exports — they are already correct. [VERIFIED: NAMESPACE]

---

## Section C: README and _pkgdown.yml Edits (DOCS-03)

### C1. Stale Count Line Edits

**Exact current lines** [VERIFIED: README.md lines 32 and 34]:

```
Line 32: - **13 Return Models**: Market Model, Market Adjusted, Mean Adjusted, Fama-French 3- and 5-factor, Carhart 4-factor, GARCH(1,1), Buy-and-Hold Abnormal Returns (BHAR), Volume, and Volatility models.
Line 34: - **11 Test Statistics**: Parametric (AR T, CAR T, BHAR T, Cross-Sectional T, Patell Z, BMP) and non-parametric (Sign, Generalized Sign, Rank, Calendar-Time Portfolio).
```

**Required replacements:**

Line 32 → `- **15+ Return Models**: Market Model, Market Adjusted, Mean Adjusted, Fama-French 3- and 5-factor, Carhart 4-factor, GARCH(1,1), DCC-GARCH, Rolling-Window, Buy-and-Hold Abnormal Returns (BHAR), Volume, and Volatility models.`

Line 34 → `- **12 Test Statistics**: Parametric (AR T, CAR T, BHAR T, Cross-Sectional T, Patell Z, BMP) and non-parametric (Sign, Generalized Sign, Rank, Kolari-Pynnönen adjusted BMP, Calendar-Time Portfolio).`

Note on line 34: Adding "Kolari-Pynnönen adjusted BMP" as the 12th test statistic. The character "ö" is non-ASCII — check against Phase 20's CI guard. Phase 20's guard is BASELINE-AWARE (fails only on NEW non-ASCII). The description already contains "Pynnönen" in DESCRIPTION (`Kolari`-`Pynnönen` adjusted BMP), so this is not NEW non-ASCII. Alternatively, use ASCII-safe phrasing "Kolari-Pynnonen adjusted BMP" to be safe. [ASSUMED: that the Phase 20 guard allows pre-existing non-ASCII chars — verify against the CI grep command.]

Safest ASCII-only phrasing for README (since README has a strict non-ASCII guard):
`- **12 Test Statistics**: Parametric (AR T, CAR T, BHAR T, Cross-Sectional T, Patell Z, BMP, Kolari-Pynnonen) and non-parametric (Sign, Generalized Sign, Rank, Calendar-Time Portfolio).`

### C2. Pre-Existing check_pkgdown Failure (Must Fix BEFORE CI Step)

Running `pkgdown::check_pkgdown()` currently produces: [VERIFIED: Rscript execution]

```
ERROR: In _pkgdown.yml, 6 topics missing from index: "format.Advice",
"format.EventStudySummary", "format.es_advice", "format.es_cross_sectional",
"format.es_diagnostics", and "format.es_simulation".
```

**Root cause:** Phase 23 added 6 exported `format.*` methods (with `@export`). They appear in NAMESPACE as `export(format.Advice)` etc. The `_pkgdown.yml` `internal` section catches `starts_with("print.")` but NOT `starts_with("format.")`.

**Fix:** Edit `_pkgdown.yml` internal section:

```yaml
# CURRENT:
  - title: internal
    contents:
      - ModelBase
      - ReturnCalculation
      - TestStatisticBase
      - degenerate-input-contract
      - starts_with("print.")

# FIXED:
  - title: internal
    contents:
      - ModelBase
      - ReturnCalculation
      - TestStatisticBase
      - degenerate-input-contract
      - starts_with("print.")
      - starts_with("format.")
```

**This fix must land BEFORE `pkgdown::check_pkgdown()` is added to CI.** If CI runs `check_pkgdown` before the fix, the pkgdown job will immediately fail on every push.

### C3. README Ecosystem Section

**Placement:** After the description paragraph (README line 14) and before `## Installation` (line 16). This is "near the top, after badges" per CONTEXT.

**Content (plain Markdown, ASCII-only):**

```markdown
## Ecosystem

EventStudy is part of the [eventstudy.de](https://eventstudy.de) toolkit — "Event Study Analysis Made Simple":

- **Google Sheets Template** — quick, no-code event study for small samples
- **R Package (this package)** — full programmatic pipeline with 15+ models, 12 test statistics, AI advisor, and publication-ready export
- **WebAssembly App** — browser-based analysis with no installation required
```

No SVGs, no HTML cards — those stay in `pkgdown/extra.css` (already delivered in Phase 20).

### C4. pkgdown Home Markers

**pkgdown 2.2.0 finding:** pkgdown 2.2.0 does NOT have a `<!-- pkgdown-home-start/end -->` marker mechanism. [VERIFIED: pkgdown 2.2.0 source and documentation searched — no such string pattern exists]

The documented approach for pkgdown-specific home content is `pkgdown/index.md` (takes priority over README.md for the pkgdown home page only). However, `pkgdown/index.md` does not currently exist, and creating it would decouple the README from the pkgdown home — acceptable but more complex.

**Recommended approach (within Claude's discretion):** Add passive HTML comment delimiters to README.md for human navigability. These comments are valid Markdown, harmless on CRAN, and invisible on the rendered page:

```markdown
<!-- pkgdown-home-start -->
## Ecosystem
...
<!-- pkgdown-home-end -->
```

These do not activate any pkgdown behavior (pkgdown 2.2.0 ignores them), but they document intent and are harmless. Keep `home: sidebar: false` unchanged in `_pkgdown.yml`.

---

## Section D: Vignette and CI (DOCS-04)

### D1. introduction.Rmd — Required Edits

**Current state** [VERIFIED: vignettes/introduction.Rmd, 205 lines]:

Problems to fix:
1. **Stale title:** "Introducing EventStudy: A Powerful Tool..." — change to "Getting Started with EventStudy"
2. **Outdated sections:** "## Why Event Study?" and "## Key Features" are generic marketing copy; condense or remove
3. **Stale Roadmap section** (L199-201): Lists features now shipped ("coming soon" intraday, volume, volatility); replace with "Next Steps" that links to deeper vignettes
4. **"More a coming soon." (L110):** Remove this stale string
5. **eval=FALSE:** All code chunks have `eval=FALSE` in knitr opts — this is fine for CRAN (prevents network calls), keep it
6. **Missing run_event_study():** The example uses the 3-step manual pipeline but doesn't show `run_event_study()` as the one-call shortcut
7. **No cross-links:** Zero links to other vignettes or articles

**Minimal tightening approach (docs-only, no new CRAN vignette):**

1. Update YAML title to: `"Getting Started with EventStudy"`
2. Remove or collapse "Why Event Study?" section to 2-3 sentences at top
3. Add a "Quick Start" sub-section showing `run_event_study()` BEFORE the step-by-step pipeline walkthrough
4. Remove the stale Roadmap section entirely; replace with:

```markdown
## Next Steps

- **Return models:** See `vignette("factor-models-bhar")` for multi-factor and long-horizon models
- **Test statistics:** See `vignette("inference-robustness")` for robust inference and bootstrap
- **Result extraction:** See `vignette("result-extraction")` for export, tidy(), and cross-sectional analysis
- **AI advisor:** See `vignette("ai-advisor")` for deterministic diagnostics and LLM interpretation
- **Full gallery:** See `vignette("gallery")` for all available vignettes by topic
```

5. Fix `"More a coming soon."` → remove that sentence from L110

### D2. CI Workflow — Add check_pkgdown Step

**Workflow file:** `.github/workflows/pkgdown.yaml` [VERIFIED: file exists, read in full]

**Current structure:**
- Job `pkgdown` with steps: checkout → setup-pandoc → setup-r → setup-r-dependencies → "Build site" → "Deploy to GitHub pages"

**Add new step BEFORE "Build site"** so broken references fail BEFORE the deploy:

```yaml
      - name: Check pkgdown configuration
        run: pkgdown::check_pkgdown()
        shell: Rscript {0}

      - name: Build site
        run: pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
        shell: Rscript {0}
```

**Why before Build site:** `check_pkgdown()` validates config and cross-references without building the full site. If it fails, the expensive build step is skipped. If it passes, the build proceeds. This is the lowest-overhead placement.

**Dependencies:** The `any::pkgdown` package is already in `extra-packages` at the `setup-r-dependencies` step — no new dependency needed.

### D3. Vignette Cross-Links

Cross-links between vignettes should use the `vignette()` call format in prose:

```markdown
See `vignette("factor-models-bhar")` for multi-factor models.
```

On pkgdown, these render as clickable links to the article. On CRAN, they are recognizable as vignette references.

For linking FROM introduction.Rmd TO articles (in `vignettes/articles/`), use:
```markdown
See the [AI Advisor methods article](../articles/methods-ai-advisor.html) for a deep dive.
```

Only add cross-links in `introduction.Rmd` — do not edit the 18 other CRAN vignettes (scope creep risk; CONTEXT says "tighten introduction.Rmd ... and add cross-links between it and the deeper vignettes").

---

## Section E: Verification Recipe

After all edits, verify with this sequence:

### E1. roxygen Regeneration
```r
# Run in project root (R session or Rscript)
devtools::document()
```
Expected: man/ files regenerated; no `@family` / `@seealso` parse errors printed; family sections appear in the generated Rd for sample functions (e.g., check `man/run_event_study.Rd` — should contain `\section{See Also}` with the pipeline family cluster).

### E2. check_pkgdown Passes
```r
setwd("/path/to/eventstudy")
pkgdown::check_pkgdown()
```
Expected: no output / no ERROR. (Currently fails with 6 topics — must fix `_pkgdown.yml` first.)

### E3. R CMD check — No New NOTEs/WARNINGs
```bash
R CMD check --as-cran .
```
Expected: same 1 pre-existing NOTE (`median`/`tail` globals) — no new NOTEs or WARNINGs from `@family`/`@seealso` tags. The CRAN check validates that all `\link{}` targets in generated Rd files exist as documented objects.

Baseline: 1 pre-existing NOTE (median/tail globals), 0 WARNINGs. [VERIFIED: Phase 23 verification report]

### E4. @seealso/\link{} Target Validation
```bash
# Quick spot-check that all @seealso targets exist in NAMESPACE
grep -o '\\link{[^}]*}' man/run_event_study.Rd man/es_advise.Rd man/export_results.Rd \
  | sed 's/\\link{//;s/}//' \
  | while read sym; do
      grep -q "^export($sym)" NAMESPACE && echo "OK: $sym" || echo "MISSING: $sym"
    done
```

### E5. ASCII-Only README Guard
```bash
grep -P '[^\x00-\x7F]' README.md
```
Expected: no output. If the "Kolari-Pynnönen" name is used in line 34, this will flag it — use ASCII-only "Kolari-Pynnonen" instead.

### E6. Test Suite Green
```r
testthat::test_package("EventStudy")
```
Expected: 2222+ pass, 0 fail (docs-only changes cannot break tests).

### E7. DESCRIPTION Unchanged
```bash
git diff DESCRIPTION
```
Expected: empty diff. No new Imports or Suggests added in this phase.

---

## Section F: Wave/Sequencing Recommendation

### F1. Risk Analysis

- DOCS-01 (@family) and DOCS-02 (@seealso) BOTH edit R/ roxygen headers and BOTH require exactly ONE `devtools::document()` run at the end.
- If serialized incorrectly (document after each), the man/ dir is regenerated multiple times unnecessarily. If run in parallel by two agents, there is a race condition on man/.
- DOCS-03 (README + _pkgdown.yml) and DOCS-04 (vignette + CI) touch disjoint files from R/*.R and from each other — they CAN run in parallel with the roxygen work.

### F2. Recommended Wave Structure

```
Wave 1 (Sequential — single agent): DOCS-01 + DOCS-02 together
  └─ Edit R/*.R roxygen blocks (add @family AND @seealso to all needed files)
  └─ One devtools::document() call at end of wave
  └─ Verify: check all man/ Rd files have expected @family sections

Wave 2 (Parallel — safe): DOCS-03 + DOCS-04
  ├─ Task A: Fix _pkgdown.yml internal section (add starts_with("format."))
  │           + Add Ecosystem section to README.md
  │           + Fix stale counts (lines 32, 34)
  │           + Add pkgdown comment delimiters
  ├─ Task B: Tighten introduction.Rmd
  │           + Fix stale Roadmap section
  │           + Add Next Steps cross-links
  └─ Task C: Add check_pkgdown step to pkgdown.yaml CI

Wave 3 (Final gate): Run pkgdown::check_pkgdown() to confirm all passes
```

**Critical sequencing rule:** `_pkgdown.yml` fix (Task A) MUST commit before `pkgdown.yaml` CI step (Task C) is pushed to main — otherwise the CI step will immediately fail on existing push. In practice, both edits land in the same wave and the CI only runs on push to main, so same-commit delivery is fine.

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead |
|---------|-------------|-------------|
| Generating man/ files | Hand-editing .Rd files | `devtools::document()` — roxygen2 owns all man/*.Rd |
| Checking cross-references | Custom grep script | `pkgdown::check_pkgdown()` |
| @family link rendering | Custom HTML | roxygen2 generates the "See Also" family section automatically |
| vignette cross-references | Absolute URLs | `vignette("name")` function calls in prose |

---

## Common Pitfalls

### Pitfall 1: @seealso pointing to non-exported function name
**What goes wrong:** `R CMD check` warns "missing Rd file 'foo.Rd'" or "unknown topic 'foo'"
**Why it happens:** Using the wrong name (e.g., `\link{advise}` instead of `\link{es_advise}`)
**How to avoid:** Every `\link{}` target must match an `export(...)` entry in NAMESPACE verbatim
**Warning signs:** `R CMD check` output contains "missing Rd" or `check_pkgdown` warns about unknown topic

### Pitfall 2: Forgetting devtools::document() after roxygen edits
**What goes wrong:** man/ files stay stale; @family changes invisible to check and pkgdown
**Why it happens:** Editing R/*.R directly doesn't auto-update man/
**How to avoid:** Run `devtools::document()` exactly ONCE at the end of all R/*.R edits (Wave 1)
**Warning signs:** `git diff man/` shows no changes after roxygen edits

### Pitfall 3: Adding check_pkgdown to CI before fixing the format.* gap
**What goes wrong:** CI immediately fails on next push with 6 missing topics error
**Why it happens:** The existing _pkgdown.yml `internal` section is missing `starts_with("format.")`
**How to avoid:** Fix _pkgdown.yml in the same commit or BEFORE adding the CI step
**Warning signs:** `pkgdown::check_pkgdown()` locally shows ERROR before editing _pkgdown.yml

### Pitfall 4: Non-ASCII character in README
**What goes wrong:** Phase 20's CI guard fires ("non-ASCII character in README.md")
**Why it happens:** "Pynnönen" contains ö
**How to avoid:** Use ASCII-only spelling "Pynnonen" in README.md (the guard baseline-aware check still flags new non-ASCII added to README)
**Warning signs:** CI grep guard step fails

### Pitfall 5: @family on format.* or print.* methods
**What goes wrong:** These S3 methods are NOT in the family (they are `@keywords internal`-appropriate); adding `@family` to them would put them in the visible Reference index family cluster
**Why it happens:** Accidentally adding `@family` to the `format.*` blocks
**How to avoid:** Only add `@family` to the user-facing constructor/function — NOT to `format.*` or `print.*` helpers
**Warning signs:** pkgdown reference index shows format.* methods in family clusters

### Pitfall 6: Two @family tags on one function
**What goes wrong:** roxygen2 >= 7.3 may use only the last one; R CMD check may warn
**Why it happens:** Forgetting a function already has a family from a prior edit
**How to avoid:** Search each R/ file for existing `@family` before adding (`grep "@family" R/*.R` returns 0 currently — this phase is net-new)
**Warning signs:** `grep "@family" R/*.R` shows duplicates

---

## Code Examples

### @family tag placement (roxygen style in use)

```r
# Source: existing pattern in R/theme.R (style to replicate)
#' @seealso \code{\link{es_colours}}, \code{\link{plot_event_study}}
#'
#' @export
theme_eventstudy <- function(base_size = 11, base_family = "") {
```

Add `@family` immediately before `@seealso` (or before `@export` if no `@seealso`):

```r
#' @family eventstudy-plots
#' @seealso \code{\link{es_colours}}, \code{\link{plot_event_study}}
#'
#' @export
theme_eventstudy <- function(base_size = 11, base_family = "") {
```

### Multi-line @seealso (existing style)

```r
# From R/advise_offline.R (established pattern):
#' @seealso \code{\link{flag_robustness}}, \code{\link{es_diagnostics}},
#'   \code{\link{es_kb}}
```

Multi-line form uses continuation indent of 2 spaces on the `#'` continuation line.

### _pkgdown.yml internal section fix

```yaml
  - title: internal
    contents:
      - ModelBase
      - ReturnCalculation
      - TestStatisticBase
      - degenerate-input-contract
      - starts_with("print.")
      - starts_with("format.")
```

### CI step addition (pkgdown.yaml)

```yaml
      - name: Check pkgdown configuration
        run: pkgdown::check_pkgdown()
        shell: Rscript {0}

      - name: Build site
        run: pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
        shell: Rscript {0}
```

---

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|------------|-----------|---------|----------|
| roxygen2 | @family/@seealso regeneration | ✓ | 8.0.0 | — |
| devtools | devtools::document() | ✓ | yes | — |
| pkgdown | check_pkgdown() | ✓ | 2.2.0 | — |
| R | All | ✓ | (R session active) | — |

[VERIFIED: Rscript -e calls in this session]

---

## Validation Architecture

Nyquist validation is explicitly disabled (`workflow.nyquist_validation: false` in `.planning/config.json`). [VERIFIED: config.json]

Section omitted per config.

---

## Security Domain

`security_enforcement: true` in config. ASVS categories for this phase:

| ASVS Category | Applies | Rationale |
|---------------|---------|-----------|
| V2 Authentication | No | Docs-only; no auth surface |
| V3 Session Management | No | No session changes |
| V4 Access Control | No | No ACL changes |
| V5 Input Validation | No | No new input paths |
| V6 Cryptography | No | No crypto changes |

This is a pure documentation phase. The only "security" consideration is the ASCII-only README guard (prevents encoding attacks in CI) and that no new Imports are added (supply chain hygiene) — both already enforced by existing guards.

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | "Kolari-Pynnonen" ASCII spelling acceptable on CRAN; using "ö" would trip the Phase 20 CI guard | C1 | Would need to remove the extended form or use a different description — low risk since ASCII-only is safe |
| A2 | `plot_panel_event_study` and `plot_synthetic_control` belong in `eventstudy-tasks` rather than `eventstudy-plots` (edge case; CONTEXT says "at Claude's discretion") | A2 | Wrong aesthetic choice only — no functional breakage; easy to reverse |
| A3 | `validate_task`, `model_diagnostics`, `pretrend_test`, `car_by_group`, `car_quantiles`, `simulate_event_study` placed in `eventstudy-statistics` (not explicitly listed in CONTEXT.md) | A2 | Wrong family — easy to move, no functional breakage |
| A4 | `download_*` left un-familied is acceptable (CONTEXT says 7 families; data utilities don't fit any) | A2 | No issue — un-familied functions simply don't appear in any family cluster |

---

## Open Questions

1. **Line 34 "Kolari-Pynnönen" vs ASCII**
   - What we know: Phase 20's CI guard uses a baseline-aware grep for NEW non-ASCII in README.md
   - What's unclear: whether "Kolari-Pynnönen" in the existing DESCRIPTION counts as "pre-existing" in README.md context, or whether adding it to README.md would trigger the guard
   - Recommendation: Use ASCII-only "Kolari-Pynnonen" in README.md — safe choice with no functional downside

2. **Should `tidy.EventStudyTask` get `@family eventstudy-export`?**
   - What we know: S3 methods CAN receive `@family` tags via roxygen2; the Rd file for `tidy.EventStudyTask` exists in man/
   - What's unclear: Whether roxygen2 8.0.0 renders `@family` correctly on S3 `tidy.*` methods
   - Recommendation: Yes, add `@family eventstudy-export` — this is how broom-method families work in other packages; if it causes an Rd warning, remove it

---

## Sources

### Primary (HIGH confidence)
- NAMESPACE file (verified by `grep "^export(" NAMESPACE`) — live export membership [VERIFIED: NAMESPACE]
- `R/*.R` source files (verified by Read and grep) — function-to-file mapping [VERIFIED: individual files]
- pkgdown 2.2.0 `build_home` help text (Rscript session) — home page behavior [VERIFIED: Rscript]
- `pkgdown::check_pkgdown()` live run result [VERIFIED: Rscript session]
- `_pkgdown.yml` (read in full) — current reference section structure [VERIFIED: _pkgdown.yml]
- `.github/workflows/pkgdown.yaml` (read in full) — CI job structure [VERIFIED: pkgdown.yaml]
- `vignettes/introduction.Rmd` (read in full) — current vignette state [VERIFIED: introduction.Rmd]
- `README.md` lines 32, 34 (verified line numbers) — stale count locations [VERIFIED: README.md:32, README.md:34]
- Phase 23 VERIFICATION.md — test suite baseline (2222 pass, 1 pre-existing NOTE) [VERIFIED: 23-VERIFICATION.md]
- `.planning/config.json` — nyquist_validation=false confirmed [VERIFIED: config.json]

### Secondary (MEDIUM confidence)
- roxygen2 8.0.0 (installed, matching RoxygenNote in DESCRIPTION) — @family rendering behavior [ASSUMED for pkgdown visual rendering; VERIFIED for R CMD check behavior via documentation]

---

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — tools verified in R session
- Architecture: HIGH — all function-to-file mappings read directly from source
- Pitfalls: HIGH — most derived from live tool execution (check_pkgdown actual error)
- pkgdown home markers: MEDIUM — confirmed pkgdown 2.2.0 has no marker mechanism; "add HTML comments" is our pragmatic approach

**Research date:** 2026-09-09
**Valid until:** Stable (docs conventions in R/roxygen2/pkgdown are long-lived)
