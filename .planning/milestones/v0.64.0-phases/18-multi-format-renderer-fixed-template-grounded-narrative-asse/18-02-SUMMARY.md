---
phase: 18-multi-format-renderer-fixed-template-grounded-narrative-asse
plan: "02"
subsystem: reporting
tags: [multi-format, rmarkdown, template, narrative-assembly, offline-fallback, r-package]

requires:
  - phase: 18-multi-format-renderer-fixed-template-grounded-narrative-asse
    plan: "01"
    provides: "assemble_report_narrative(), .sanitise_prose(), JOINT_HYPOTHESIS_CAVEAT, section_sources/report_mode metadata"

provides:
  - "generate_report() multi-format render loop: vector format= renders one file per format, returns named character vector keyed by format"
  - ".build_output_format(), .pdf_toolchain_available(), .word_toolchain_available() -- toolchain detection helpers"
  - "Toolchain-skip channel: message() per skipped optional format, only HTML failure raises stop()"
  - "Narrative assembled ONCE before format loop; provider never called inside the loop (NARR-01 end-to-end)"
  - "skeleton.Rmd fixed 6-section template (exec_summary, data_methods, results, diagnostics, robustness, references)"
  - "knitr::is_html_output() plot switching in template (FORMAT-03)"
  - "Per-section AI-vs-offline heading label from section_sources (OFFLINE-02)"
  - "Joint-hypothesis caveat in every robustness section (NARR-05)"
  - "Deterministic Data & Methods / Results tables from params$task + params$diag (TMPL-02)"
  - "KB references rendered via params$references in dedicated references chunk (NARR-03)"
  - "test_report_multiformat.R: 42 tests covering FORMAT-01/02, TMPL-01/02, NARR-05, OFFLINE-02"

affects: ["phase-19", "es_report", "generate_report", "skeleton.Rmd"]

actuals:
  tokens: 74000
  tasks: 3
  commits: 4

tech-stack:
  added: []
  patterns:
    - "Assemble-before-render: narrative assembled once, same named list passed into every format render -- LLM call budget enforced by architecture"
    - "Toolchain-skip via message(); only HTML baseline failure raises stop() -- FORMAT-02 contract"
    - "Per-format fig.path isolation via output_options to prevent figure-directory collisions on sequential renders"
    - "Narrative validation: metadata fields (section_sources, report_mode) exempt from character-scalar guard"
    - "Fixed 6-section template with eval= guards per section key -- TMPL-01 contract"

key-files:
  created:
    - tests/testthat/test_report_multiformat.R
  modified:
    - R/report.R
    - inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd

key-decisions:
  - "Narrative validation must allow non-prose metadata fields (section_sources as list, report_mode as scalar) -- overly strict validator coerced pre-built assemble_report_narrative() output to NULL"
  - "appendix-section chunk name preserved for backward compat with test_report_advice.R structural assertion"
  - "Sections default changed from old 7-key set to the 6 new fixed keys; old backward-compat narrative-section chunk retained but only fires when none of the six new keys is in sections"
  - "Per-format fig.path passed via output_options = list('fig.path' = ...) per Spike 1 -- figure isolation works"
  - "Assumption probe: all 6 ASSUMED idioms confirmed (tinytex::is_tinytex EXISTS/TRUE, rmarkdown::pandoc_available EXISTS/TRUE, output_options fig.path param EXISTS, knitr::is_html_output EXISTS, md_document(variant) EXISTS)"

patterns-established:
  - "Mock-based FORMAT-02 test: with_mocked_bindings(.pdf_toolchain_available = ...) to test skip without a real render"
  - "NOT_CRAN env var to enable skip_on_cran render tests in CI"
  - "Offline narrative validation: pre-built list from assemble_report_narrative() passes through without re-assembly"

requirements-completed: [FORMAT-01, FORMAT-02, FORMAT-03, TMPL-01, TMPL-02, NARR-05, OFFLINE-02]

coverage:
  - id: D1
    description: "generate_report() vector format= renders one file per requested format, returns named character vector keyed by format (FORMAT-01)"
    requirement: FORMAT-01
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#FORMAT-01 render: single format='html' returns named vector with 'html' element"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#FORMAT-01 render: vector format returns paths for each rendered format"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#FORMAT-01: format formal exists and default is 'html'"
        status: pass
    human_judgment: false

  - id: D2
    description: "Missing optional toolchain emits one message() and skips; only HTML failure raises stop() (FORMAT-02)"
    requirement: FORMAT-02
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#FORMAT-02: unavailable pdf toolchain emits message, not stop()"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#FORMAT-02: .build_output_format returns NULL for pdf when toolchain unavailable"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#FORMAT-01: invalid format raises stop()"
        status: pass
    human_judgment: false

  - id: D3
    description: "skeleton.Rmd uses knitr::is_html_output() for plot switching; plotly in HTML, print(p) for PDF/Word/MD (FORMAT-03)"
    requirement: FORMAT-03
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#TMPL-01: skeleton.Rmd uses knitr::is_html_output for plot switching (FORMAT-03)"
        status: pass
    human_judgment: false

  - id: D4
    description: "skeleton.Rmd fixed 6-section template: diag/references params declared, section keys present (TMPL-01)"
    requirement: TMPL-01
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#TMPL-01: skeleton.Rmd sections: default contains all six new fixed keys"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#TMPL-01: skeleton.Rmd declares diag: NULL in params block"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#TMPL-01: skeleton.Rmd declares references: NULL in params block"
        status: pass
    human_judgment: false

  - id: D5
    description: "Data & Methods and Results tables auto-filled from params$task + params$diag, never from LLM (TMPL-02)"
    requirement: TMPL-02
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#TMPL-02: rendered HTML contains event count from task metadata"
        status: pass
    human_judgment: false

  - id: D6
    description: "Joint-hypothesis caveat present in every rendered report's robustness section (NARR-05)"
    requirement: NARR-05
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#NARR-05: assemble_report_narrative robustness contains caveat (unit, no render)"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#NARR-05: rendered HTML contains joint-hypothesis caveat"
        status: pass
    human_judgment: false

  - id: D7
    description: "AI-vs-offline mode distinction: console message AND section heading label (OFFLINE-02)"
    requirement: OFFLINE-02
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#OFFLINE-02: offline path emits 'Offline rule-based narrative' message"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#OFFLINE-02: AI path emits 'AI-grounded narrative' message"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_multiformat.R#OFFLINE-02: rendered HTML contains AI-vs-offline section heading label"
        status: pass
    human_judgment: false

duration: 13min
completed: 2026-09-07
status: complete
---

# Phase 18 Plan 02: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly Summary

**Multi-format generate_report() loop (html/pdf/word/md, named-vector return), fixed 6-section skeleton.Rmd with knitr::is_html_output() plot switching, deterministic task/diagnostics tables, AI-vs-offline heading labels, and joint-hypothesis caveat**

## Performance

- **Duration:** 13 min
- **Started:** 2026-09-07T10:04:54Z
- **Completed:** 2026-09-07T10:18:00Z
- **Tasks:** 3 (Task 0 checkpoint pre-approved, skipped)
- **Files modified:** 3 modified, 1 created

## Accomplishments

- Replaced `match.arg(format)` with `intersect(format, valid_formats)` vector loop; `generate_report()` now renders one file per requested format and returns a named character vector keyed by format (FORMAT-01)
- Added `.build_output_format()`, `.pdf_toolchain_available()`, `.word_toolchain_available()` helpers; missing toolchains skip with one `message()`, only HTML failure raises `stop()` (FORMAT-02)
- Narrative assembled ONCE before the format loop via `assemble_report_narrative()` -- provider never called per format; `provider=` formal added to `generate_report()` (NARR-01 enforced by architecture)
- Evolved `skeleton.Rmd` into fixed 6-section template (exec_summary, data_methods, results, diagnostics, robustness, references) with `diag:` and `references:` params, `eval= "<key>" %in% sections` guards, `knitr::is_html_output()` plot switching, per-section AI/offline heading labels from `section_sources`, and joint-hypothesis caveat (TMPL-01/02, FORMAT-03, NARR-05, OFFLINE-02)
- All 42 tests in `test_report_multiformat.R` green; 2207 suite-wide tests pass, 0 fail

## Task Commits

1. **Task 1: Multi-format render loop + toolchain skip** - `b951064` (feat)
2. **Task 2: Fixed 6-section skeleton.Rmd** - `0b45db1` (feat)
3. **Task 3: Narrative validation fix + appendix chunk name** - `0acccd7` (fix)

## Files Created/Modified

- `R/report.R` -- generate_report() format loop, .build_output_format(), .pdf_toolchain_available(), .word_toolchain_available(), narrative validation fix
- `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` -- complete rewrite to fixed 6-section template with all required params and chunks
- `tests/testthat/test_report_multiformat.R` -- 42 tests covering FORMAT-01/02, TMPL-01/02, NARR-05, OFFLINE-02 (NEW)

## Decisions Made

- Narrative validation must allow non-prose metadata fields (`section_sources` as list, `report_mode` as character scalar) -- overly strict validator coerced pre-built `assemble_report_narrative()` output to NULL
- `appendix-section` chunk name preserved for backward compat with `test_report_advice.R` structural assertion
- Sections default changed from old 7-key set to the 6 new fixed keys; old backward-compat narrative-section chunk retained but only fires when none of the six new keys is in `sections`
- Per-format `fig.path` passed via `output_options = list("fig.path" = ...)` per Spike 1 -- figure isolation verified

## Assumption Probe Results

All 6 [ASSUMED] idioms confirmed before building:

| # | Assumption | Outcome |
|---|-----------|---------|
| A1 | `tinytex::is_tinytex()` exists and is callable | CONFIRMED: returns TRUE |
| A2 | `rmarkdown::pandoc_available()` exists | CONFIRMED: returns TRUE |
| A3 | `rmarkdown::render()` has `output_options` param | CONFIRMED |
| A4 | `knitr::is_html_output()` exists | CONFIRMED |
| A5 | `rmarkdown::md_document(variant=)` exists | CONFIRMED |
| A6 | `rmarkdown::html_document()`, `pdf_document()`, `word_document()` exist | CONFIRMED |

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Narrative validation rejected assemble_report_narrative() output due to section_sources (list) element**
- **Found during:** Task 3 (OFFLINE-02 render tests)
- **Issue:** The narrative validator checked that ALL list elements are character scalars. The output of `assemble_report_narrative()` includes `section_sources` (a named list) and `report_mode` (scalar string). The validator rejected the pre-built narrative with a warning and coerced it to NULL, then re-assembled without the provider (defeating the NARR-01 invariant).
- **Fix:** Introduced `metadata_keys <- c("section_sources", "report_mode")` and only validated the prose section keys against the character-scalar constraint.
- **Files modified:** `R/report.R`
- **Committed in:** `0acccd7` (Task 3 commit)

**2. [Rule 1 - Bug] appendix chunk renamed from appendix-section to appendix, breaking test_report_advice.R**
- **Found during:** Task 3 (full suite run)
- **Issue:** `test_report_advice.R:162` asserts that an `appendix-section` chunk exists in skeleton.Rmd. The template rewrite changed the chunk name to `appendix`.
- **Fix:** Restored the chunk name to `appendix-section`.
- **Files modified:** `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd`
- **Committed in:** `0acccd7` (Task 3 commit)

---

**Total deviations:** 2 auto-fixed (2 Rule 1 bugs)
**Impact on plan:** Both fixes necessary for correctness and backward compat. No scope creep.

## Threat Mitigations Delivered

| Threat ID | Mitigation | Status |
|-----------|-----------|--------|
| T-18-04 | `.sanitise_prose(narrative, fmt)` applied per-format before render params in the format loop | Delivered |
| T-18-05 | Per-format unique `fig.path` via `output_options` in each `rmarkdown::render()` call | Delivered |
| T-18-06 | Tables in template sourced from `params$task` / `params$diag` -- LLM never supplies numbers | Delivered + tested |

## Known Stubs

None -- all six template sections are wired to real data sources (task metadata, diag, narrative, KB references). No placeholder text.

## Issues Encountered

None beyond the two auto-fixed bugs above.

## Next Phase Readiness

- Phase 19 `es_report()` orchestrator can call `assemble_report_narrative(diag, provider=)` then `generate_report(task, format=c(...), narrative=narrative)` through the established seams
- `generate_report()` returns a named character vector; `es_report()` can pass it through or take `[[1L]]` for single-format convenience
- All format constructors (html/pdf/word/md), toolchain guards, and fig.path isolation are in place

## Self-Check

- [x] `R/report.R` exists and is ASCII-clean
- [x] `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` exists and is ASCII-clean
- [x] `tests/testthat/test_report_multiformat.R` exists
- [x] Commits b951064, 0b45db1, 0acccd7 exist (verified by git log)
- [x] Full suite: 2207 PASS, 0 FAIL

## Self-Check: PASSED

---
*Phase: 18-multi-format-renderer-fixed-template-grounded-narrative-asse*
*Completed: 2026-09-07*
