---
phase: 18-multi-format-renderer-fixed-template-grounded-narrative-asse
plan: "01"
subsystem: reporting
tags: [narrative-assembly, grounding, sanitiser, section-hint, offline-fallback, r-package]

requires:
  - phase: 17-grounding-prose-hardening-offline-report-fallback-cran-base
    provides: "narrative= seam in generate_report(), .build_offline_narrative() with four section keys, prose grounding guard extended to free text"

provides:
  - "assemble_report_narrative() -- format-agnostic section-by-section assembler, one LLM call per section"
  - ".calibrate_significance() -- static four-tier p-value label calibrator"
  - ".extract_kb_references() -- KB-sourced citation extraction, deduped by key, alpha by author"
  - "JOINT_HYPOTHESIS_CAVEAT -- fixed static caveat appended to every robustness section"
  - ".sanitise_prose() / .sanitise_universal() / .sanitise_for_pdf() / .sanitise_for_word() -- per-format sanitiser map"
  - "section_hint=NULL seam on es_advise() and .build_prompt() -- additive arg, NULL path byte-identical"

affects: ["phase-18-02", "phase-19", "generate_report", "es_report"]

actuals:
  tokens: 52000
  tasks: 3
  commits: 3

tech-stack:
  added: []
  patterns:
    - "Assemble-before-render: narrative assembled once (format-agnostic) then same list passed into format loop -- LLM call budget enforced by architecture"
    - "Section-scoped prompt injection via section_hint= additive arg on es_advise(); NULL path backward-compat"
    - "Per-section offline fallback: es_advise() error or empty result falls back to .build_offline_narrative() section; report never incomplete"
    - "Sanitise-then-render: .sanitise_prose(text, format) applied before narrative enters render params"
    - "CRAN non-ASCII hygiene: smart-quote/dash literals in gsub patterns expressed via \\u Unicode escapes"

key-files:
  created:
    - R/report_narrative.R
    - tests/testthat/test_report_narrative_asm.R
    - tests/testthat/test_prose_sanitiser.R
  modified:
    - R/advise.R

key-decisions:
  - "section_hint= is additive and default-NULL; the NULL path produces a byte-identical prompt to pre-Phase-18 report_writing (backward compat lock)"
  - "data_methods always sourced from offline baseline (never LLM); only exec_summary/results/robustness are LLM-narrated"
  - "Provider error or empty Advice interpretation -> fall back to offline for that section only; report_mode stays 'ai' if any other section used LLM"
  - "Smart-quote/dash literals in R source expressed via \\u escapes (not raw bytes) so grep -nP [^\\x00-\\x7F] is clean"
  - "Tilde/caret sanitised to \\textasciitilde{}/\\textasciicircum{} (LaTeX macro form); test fixture excludes these chars from the unescaped-specials assertion"

patterns-established:
  - "Mock provider for assembler tests returns list(text = '...JSON...') matching es_advise() provider interface"
  - "Dedup test uses diagnostics fixture designed to fire KB rules with shared citation keys (BrownWarner1985, MacKinlay1997)"

requirements-completed: [NARR-01, NARR-02, NARR-03, NARR-04, NARR-05, FORMAT-04, OFFLINE-02]

coverage:
  - id: D1
    description: "assemble_report_narrative() returns four locked keys + section_sources + report_mode"
    requirement: NARR-02
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#assemble_report_narrative(provider=NULL) returns list with four section keys"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#OFFLINE-02: report_mode and section_sources with mock provider"
        status: pass
    human_judgment: false

  - id: D2
    description: "Mock provider contacted exactly 3 times (once per LLM section); data_methods always offline"
    requirement: NARR-01
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#NARR-01: mock provider contacted exactly 3 times (one per LLM section)"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#NARR-01: data_methods is always offline regardless of provider"
        status: pass
    human_judgment: false

  - id: D3
    description: "section_hint= injects section-scoping sentence into prompt; NULL path unchanged"
    requirement: NARR-01
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#section_hint= injects section name and 'Write ONLY the' into prompt"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#NULL section_hint path: es_advise report_writing prompt has no 'Write ONLY the'"
        status: pass
    human_judgment: false

  - id: D4
    description: "JOINT_HYPOTHESIS_CAVEAT appended to robustness section in every assembled narrative"
    requirement: NARR-05
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#NARR-05: joint-hypothesis caveat present in offline robustness section"
        status: pass
    human_judgment: false

  - id: D5
    description: ".calibrate_significance() returns correct four-tier labels + NA/non-numeric guard"
    requirement: NARR-04
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#.calibrate_significance() returns 'strongly significant'"
        status: pass
    human_judgment: false

  - id: D6
    description: ".extract_kb_references() deduplicates by key, sorts by author, returns KB-sourced citations only"
    requirement: NARR-03
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#.extract_kb_references() deduplicates when multiple rules share citation key"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#.extract_kb_references() returns empty list for all-NA diagnostics"
        status: pass
    human_judgment: false

  - id: D7
    description: "Per-format prose sanitiser: LaTeX specials escaped for pdf, XML entities for word, smart-quotes/dashes for all"
    requirement: FORMAT-04
    verification:
      - kind: unit
        ref: "tests/testthat/test_prose_sanitiser.R#.sanitise_for_pdf() escapes LaTeX specials: % $ & # _ { }"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_prose_sanitiser.R#.sanitise_for_word() produces &amp;, &lt;, &gt; from & < >"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_prose_sanitiser.R#.sanitise_universal() converts combined smart-quotes and em-dash"
        status: pass
      - kind: unit
        ref: "tests/testthat/test_prose_sanitiser.R#.sanitise_for_pdf() neutralises LaTeX \\input injection"
        status: pass
    human_judgment: false

  - id: D8
    description: "Provider error -> graceful section fallback, report never incomplete"
    requirement: OFFLINE-02
    verification:
      - kind: unit
        ref: "tests/testthat/test_report_narrative_asm.R#provider error causes graceful fallback to offline for that section"
        status: pass
    human_judgment: false

duration: 11min
completed: 2026-09-07
status: complete
---

# Phase 18 Plan 01: Grounded Narrative Assembler, Significance Calibration, KB References, and Per-Format Sanitiser Summary

**Section-by-section grounded narrative assembler (one LLM call per section, never per format), static significance calibrator, KB citation extractor, joint-hypothesis caveat, and per-format prose sanitiser -- the complete NARR-01..05 / FORMAT-04 / OFFLINE-02 narrative-assembly layer for Phase 18**

## Performance

- **Duration:** 11 min
- **Started:** 2026-09-07T09:38:14Z
- **Completed:** 2026-09-07T09:49:00Z
- **Tasks:** 3 (Task 0 checkpoint pre-approved, skipped)
- **Files modified:** 4 (1 created, 1 modified, 2 tests created)

## Accomplishments

- Created `R/report_narrative.R` with `assemble_report_narrative()` returning the four locked section keys (`exec_summary`, `data_methods`, `results`, `robustness`) plus `section_sources` and `report_mode` metadata; data_methods always offline, three LLM-narrated sections fall back per-section on error
- Added additive `section_hint=NULL` argument to `es_advise()` and `.build_prompt()` with exact injection logic: `sprintf("%s\n\nWrite ONLY the '%s' section...", report_base, section_hint)`; NULL path byte-identical to pre-Phase-18 report_writing prompt
- Implemented static helpers: `.calibrate_significance()` (4 tiers), `.extract_kb_references()` (KB-sourced, deduped by key, alpha by author), `JOINT_HYPOTHESIS_CAVEAT` constant
- Implemented per-format prose sanitiser: `.sanitise_universal()` (smart-quotes/dashes via `\u` escapes), `.sanitise_for_pdf()` (10 LaTeX specials, backslash first), `.sanitise_for_word()` (5 XML entities, ampersand first), `.sanitise_prose()` dispatcher
- 121 tests green (73 narrative + 48 sanitiser); file ASCII-clean per CRAN discipline

## Task Commits

1. **Task 1: End-to-end grounded narrative assembler** - `e680158` (feat)
2. **Task 2: Dedup/alpha-sort tests for .extract_kb_references()** - `9e191d9` (test)
3. **Task 3: Per-format prose sanitiser tests** - `b2c01df` (test)

## Files Created/Modified

- `R/report_narrative.R` (386 lines) -- All assembler helpers; package-internal, @noRd
- `R/advise.R` -- Added `section_hint=NULL` to `es_advise()` and `.build_prompt()` (4 lines changed)
- `tests/testthat/test_report_narrative_asm.R` -- 73 tests for NARR-01..05, OFFLINE-02, section_hint= wiring
- `tests/testthat/test_prose_sanitiser.R` -- 48 tests for FORMAT-04 per-format sanitisation

## Decisions Made

- `section_hint=` is additive and `default=NULL`; the NULL path produces a byte-identical prompt to pre-Phase-18 `report_writing` (backward compat lock per plan acceptance criteria)
- `data_methods` is always sourced from the offline baseline (never LLM); the three LLM-narrated sections are `exec_summary`, `results`, `robustness`
- Provider error or empty `Advice$interpretation` -> fall back to offline for that section only; `report_mode` stays `"ai"` if any other section used the LLM
- Smart-quote/dash literals in R source expressed via `\u` Unicode escapes (not raw bytes) so `grep -nP [^\x00-\x7F]` is clean
- Tilde/caret sanitised to `\textasciitilde{}`/`\textasciicircum{}` (LaTeX macro form); test fixture excludes these from the "no unescaped specials" `grepl` assertion since the macro body itself contains `{}`

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Mock provider format mismatch: test mock returned plain string, es_advise() expects list(text=...)**
- **Found during:** Task 1 (GREEN phase - writing tests first revealed interface mismatch)
- **Issue:** Initial test mock returned `"Mock LLM narrative prose..."` (plain string) but `es_advise()` provider path calls `.parse_advice_json(resp, ...)` which reads `resp$text`; plain string has no `$text` field, causing parse failure and empty Advice
- **Fix:** Updated mock's `complete()` to return `list(text = '{"interpretation":"...", "recommendations":[], "caveats":[]}')` matching real provider interface
- **Files modified:** `tests/testthat/test_report_narrative_asm.R`
- **Verification:** All 73 tests pass; mock_provider call_count == 3

**2. [Rule 1 - Bug] .extract_llm_section_prose returned fallback text marked as "ai" instead of NULL -> section marked as offline**
- **Found during:** Task 1 (provider error fallback test failing)
- **Issue:** Original helper returned `fallback_text` on empty Advice, which then passed the `nzchar(trimws(llm_prose))` check and got marked as "ai" even though the LLM didn't produce usable prose
- **Fix:** Refactored helper to `.extract_llm_section_prose()` returning `NULL` on failure (no fallback); `NULL` signals assembler to use offline and mark "offline"
- **Files modified:** `R/report_narrative.R`
- **Verification:** Provider error test passes; section_sources all "offline" after error

**3. [Rule 1 - Bug] Python script for non-ASCII replacement corrupted R source file by replacing ASCII " (double-quote) used as string delimiters with “” sequences**
- **Found during:** Task 1 (R parse error after first replacement attempt)
- **Fix:** Rewrote `R/report_narrative.R` completely with correct `\u` R Unicode escape sequences for smart-quote patterns in gsub calls; used targeted Python replacement for non-ASCII byte sequences only
- **Files modified:** `R/report_narrative.R`
- **Verification:** `grep -nP "[^\x00-\x7F]" R/report_narrative.R` returns nothing; all tests pass

**4. [Rule 1 - Bug] LaTeX all-specials test fixture: tilde/caret replacements introduce {} in \textasciitilde{} which are then caught by unescaped-specials regex**
- **Found during:** Task 3 (test_prose_sanitiser.R first run)
- **Issue:** The regex `(?<!\\)[%$&#_{}~^]` catches `{` in `\textasciitilde{}` and `\textasciicircum{}` because `{` is preceded by `e`, not `\`. This is correct behavior (the `{}` in macro calls are NOT escaped specials), but the test was asserting no such matches
- **Fix:** Split the all-specials test into: (1) specials excluding `~` and `^` (assertion regex `[%$&#_{}]`), (2) separate tests verifying `~` and `^` become macro calls with no bare literals remaining
- **Files modified:** `tests/testthat/test_prose_sanitiser.R`
- **Verification:** All 48 sanitiser tests pass

---

**Total deviations:** 4 auto-fixed (4 Rule 1 bugs)
**Impact on plan:** All auto-fixes essential for correctness. No scope creep. No architectural changes.

## Threat Mitigations Delivered

| Threat ID | Mitigation | Status |
|-----------|-----------|--------|
| T-18-01 | `.sanitise_prose()` escapes LaTeX `\input`/`\write18` and XML entities before render | Delivered + tested |
| T-18-02 | Inherited Phase 17 grounding guard on es_advise path; assembler falls back per section | Delivered + tested |
| T-18-03 | References from `es_kb()` only; never LLM-authored | Delivered + tested |

## Issues Encountered

- Non-ASCII CRAN hygiene: R source file with raw smart-quote/dash characters in `gsub()` patterns. Resolved by using `\u` Unicode escapes (R natively expands `"“"` to the Unicode character at runtime; source stays ASCII). Test file helper constants also use `\u` escapes.

## Known Stubs

None -- all four section keys wired to real prose sources (offline baseline or LLM); assembler is functional end-to-end.

## Next Phase Readiness

- Plan 02 (multi-format renderer + fixed template) can consume `assemble_report_narrative()` via the existing `narrative=` seam in `generate_report()`
- The `section_sources` and `report_mode` metadata are ready for the template's AI-vs-offline heading labels (OFFLINE-02)
- Phase 19 `es_report()` orchestrator can call `assemble_report_narrative()` then pass the result to `generate_report()`

## Self-Check

Checking created files and commits...

---
*Phase: 18-multi-format-renderer-fixed-template-grounded-narrative-asse*
*Completed: 2026-09-07*
