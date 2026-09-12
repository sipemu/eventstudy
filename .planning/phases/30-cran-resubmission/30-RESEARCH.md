# Phase 30: CRAN Resubmission - Research

**Researched:** 2026-09-12
**Domain:** CRAN submission policy, devtools/rcmdcheck toolchain, R CMD check --as-cran compliance
**Confidence:** HIGH (policy from official CRAN docs + R Packages 2e; toolchain from rhub.github.io)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

- **Submission boundary:** prepare-to-ready only. Executor does NOT call `devtools::submit_cran()`.
  CRAN-05 is a human checkpoint — operator (Simon Mueller, sm@data-zoo.de) submits + confirms email.
- **Multi-platform:** Claude runs win-builder (`devtools::check_win_devel()` and
  `devtools::check_win_release()`). Phase 29 CI stands as Linux/multi-OS evidence.
  rhub is NOT installed locally and is NOT to be installed. Document rhub v2 commands for the
  operator only.
- **Archival context:** Package archived 2024-04-20 (last version 0.39.2), reason "issues were not
  corrected despite reminders." Underlying issues were all NOTEs on the OLD web-API-client design
  (undocumented Rd args, 7 MB size, unused imports, undeclared R6). v0.66.0 is a ground-up rewrite.
- **Cover-letter framing:** Acknowledge archival + reason; frame v0.66.0 as substantially different
  package (composable R6 pipeline vs old web-API client). Do NOT merely bump version numbers.

### Claude's Discretion

- DESCRIPTION version bump to 0.66.0 (and NEWS.md 0.66.0 stanza if not present).
- Exact structure/wording of the rewritten `cran-comments.md`.
- CRAN-policy compliance audit details (network guards, tempdir-only writes, `\dontrun{}` audit,
  example runtime trimming) and any minor fixes needed to reach a clean `--as-cran`.

### Deferred Ideas (OUT OF SCOPE)

- `devtools::submit_cran()` invocation and CRAN email confirmation (CRAN-05) — operator only.
- rhub v2 execution — documented for operator; not triggered autonomously.
</user_constraints>

---

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| CRAN-01 | Clean local `R CMD check --as-cran`: 0 ERRORs, 0 WARNINGs, only explainable NOTEs | §Check commands; §Known NOTEs inventory; §_R_CHECK_FORCE_SUGGESTS_ |
| CRAN-02 | Multi-platform: win-builder initiated; rhub v2 documented for operator | §win-builder mechanics; §rhub v2 commands |
| CRAN-03 | Rewrite `cran-comments.md` with archival acknowledgment, fixes, platform results | §cran-comments.md structure; §Archival cover-letter framing |
| CRAN-04 | CRAN-policy compliance: no default network, tempdir-only writes, `\dontrun` audit, example runtimes | §\dontrun audit; §Network/file policy; §Skeleton log hygiene |
| CRAN-05 | Operator handoff: exact submit steps, email confirmation reminder | §Submission handoff runbook |
</phase_requirements>

---

## Summary

Phase 30 is a release-engineering phase, not a code-logic phase. The executor's job is to bring
`v0.66.0` to a state where `R CMD check --as-cran` is clean, every CRAN policy box is ticked,
and the maintainer has a one-command submit path with a correctly-framed cover letter.

**The hardest problem is not the check itself** — Phases 25–29 already drove the codebase to a
clean baseline. The hard problems are: (a) writing a `cran-comments.md` that honestly
acknowledges a 2-year archival and frames the rewrite credibly for CRAN reviewers; and (b)
auditing the `\dontrun{}` uses (8 Rd files affected) to ensure none are "gratuitous" in CRAN's
eyes (i.e., wrapping short, non-network, non-writing examples that could run as `\donttest{}`).

**Primary recommendation:** Run `rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"),
error_on = "never")` with `_R_CHECK_FORCE_SUGGESTS_=false` set, fix any new findings, then
invoke win-builder. Write `cran-comments.md` last (after results are in hand). Hand off to operator.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| Local `--as-cran` check | Dev toolchain (rcmdcheck) | DESCRIPTION/R sources | Detects policy violations before submission |
| Version bump | DESCRIPTION + NEWS.md | — | Version is the submission identity |
| Cover letter | cran-comments.md | — | CRAN policy requires written acknowledgment of prior issues |
| Win-builder checks | devtools async API | Maintainer email | Results sent to sm@data-zoo.de; async |
| rhub v2 (documented only) | Operator GitHub Actions | rhub package | Not auto-triggered; GH Actions-backed |
| Submission | Operator manual step | CRAN webform | Hard boundary; cannot be autonomous |
| Policy compliance | R/ sources + man/*.Rd + vignettes/ | inst/rmarkdown/ | Network, file writes, example runtime |

---

## Standard Stack

### Core — already installed, no new installs needed

| Tool | Version | Purpose | Note |
|------|---------|---------|------|
| `devtools` | installed | `check_win_devel()`, `check_win_release()`, `submit_cran()` | Already present [VERIFIED: CONTEXT.md] |
| `rcmdcheck` | installed | `rcmdcheck(args = c("--as-cran", ...))` | Already present [VERIFIED: CONTEXT.md] |
| R | 4.6.1 | Local check runtime | [VERIFIED: cran-comments.md history] |

**No new packages are added to DESCRIPTION for this phase.** [VERIFIED: 30-CONTEXT.md]

---

## CRAN Resubmission Policy

### Archived Package Resubmission — What CRAN Expects

**This is a fresh submission, not an "unarchive" request.** CRAN does not have an "unarchive" API.
Resubmission of a previously-archived package goes through the normal webform at
https://cran.r-project.org/submit.html — same process as a new submission. [CITED: cran.r-project.org/web/packages/policies.html]

**The version number must be higher than the archived version.** The archived version is 0.39.2;
v0.66.0 satisfies this requirement trivially. [CITED: r-pkgs.org/release.html]

**The cover letter (Comments field / cran-comments.md) must include a "Resubmission" section**
at the top that: [CITED: r-pkgs.org/release.html]
1. States this is a resubmission of a previously-archived package.
2. Names the archival date and stated reason.
3. Lists what changed to address those issues (or, for a rewrite, states that the package is a
   substantially different implementation that resolves the class of issues).
4. Reports current `R CMD check --as-cran` results (ERRORS/WARNINGS/NOTES) for each platform.

**CRAN reviewers specifically look for acknowledgment that the prior issues were understood and
addressed.** Omitting this causes automatic re-review delay. [ASSUMED — based on CRAN reviewer
documented behavior; not stated in written policy]

### cran-comments.md Canonical Structure for v0.66.0

```markdown
## Resubmission (previously archived 2024-04-20)

This package was archived on 2024-04-20 at version 0.39.2. The stated reason was
"issues were not corrected despite reminders." The underlying issues were all NOTE-level
findings on the original EventStudyTools web-API-client design: undocumented Rd
arguments, installed size 7 MB, unused imported packages, and undeclared R6 in Rd
cross-references.

The current submission (v0.66.0) is a complete, ground-up rewrite. The old web-API
client design has been entirely replaced by a composable R6 pipeline:
`prepare_event_study()` → `fit_model()` → `calculate_statistics()`. The class of issues
that led to archival has been resolved:

* All exported functions have complete Rd documentation with `@param`, `@return`,
  and `@examples`.
* Installed size is well under 5 MB (source package ~1.8 MB).
* All previously-unused imports have been removed. Optional packages are in Suggests,
  each guarded by `requireNamespace()`.
* NAMESPACE is auto-generated by roxygen2 with no undeclared cross-references.

## R CMD check results

### Local — Linux (Manjaro), R 4.6.1

```
0 errors | 0 warnings | N notes
```

Notes:
* [list any remaining notes verbatim with explanation]

### Windows (win-builder R-devel)

```
[paste win-builder email result here]
```

### Windows (win-builder R-release)

```
[paste win-builder email result here]
```

## Test suite

[ FAIL 0 | WARN 0 | SKIP N | PASS ~2359 ]

All ~2359 tests pass on Linux R 4.6.1. Skips are optional-Suggests-not-installed
(expected; each guarded by `requireNamespace()`).
```

The placeholders `[paste win-builder email result here]` must be filled by the executor
after win-builder emails arrive (typically within 30 minutes).

---

## R CMD check --as-cran Mechanics

### Local check command (CRAN-01)

```r
# Set environment variable to suppress Suggests-not-available ERROR
# (produces NOTE instead, which is explainable)
Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")

rcmdcheck::rcmdcheck(
  args    = c("--as-cran", "--no-manual"),
  error_on = "never"
)
```

Or equivalently from the shell:
```bash
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual EventStudy_0.66.0.tar.gz
```

**Why `_R_CHECK_FORCE_SUGGESTS_=false`:** Without this flag, packages listed in Suggests but not
installed in the check environment produce an ERROR, blocking the gate. With it set to `false`,
they produce an explainable NOTE instead. CRAN itself uses this flag in their infrastructure for
packages with many optional Suggests. [ASSUMED — widely documented practice; not stated in written
CRAN policy verbatim]

### Build tarball first

```r
devtools::build(manual = FALSE, vignettes = FALSE)
# Produces EventStudy_0.66.0.tar.gz in parent directory
```

Then run `rcmdcheck` against the tarball, not `load_all`. This catches install-vs-load_all
divergence (the class of bug Phase 29 was designed to catch).

### Explainable NOTEs inventory (pre-existing, non-blocking)

Based on the v0.65.0 check history in `cran-comments.md`:

| NOTE | Origin | Explainable? | Action |
|------|--------|-------------|--------|
| Packages suggested but not available: rugarch, rmgarch, did, DIDmultiplegt, didimputation, quadprog, sandwich, tidyquant, quantmod, DT, zoo, openxlsx, callr, lifecycle, gridExtra, httr2, jsonlite, tinytex, tinytable, patchwork, ragg | Optional Suggests, each `requireNamespace()`-guarded | YES — state in cover letter | None; explain in cran-comments.md |
| CRAN incoming feasibility (archived package) | Package was archived | YES — acknowledge in cover letter | None; cover letter is the remedy |
| `median`/`tail` undefined globals in `R/es_diagnostics.R` | Missing `importFrom` (pre-existing since Phase 5) | NO — must fix before submission | Add `importFrom(stats, median)` + `importFrom(utils, tail)` or add to `globalVariables()` |

**The undefined-globals NOTE must be fixed** — it is a genuine defect and is not explainable
under CRAN policy. It has been carried as tech debt since Phase 5 (v0.60.0 baseline). This phase
must resolve it. [VERIFIED: cran-comments.md:136-154]

---

## Win-Builder Mechanics (CRAN-02)

```r
# Submit to win-builder R-devel (async; results emailed to sm@data-zoo.de)
devtools::check_win_devel()

# Submit to win-builder R-release (async; results emailed to sm@data-zoo.de)
devtools::check_win_release()
```

**Behavior:** These functions build a source tarball and upload it to the CRAN win-builder service.
Results arrive by email to the DESCRIPTION `Maintainer` address (`sm@data-zoo.de`) within
approximately 30 minutes. [CITED: r-pkgs.org/release.html]

**The executor should call both, then wait for email results to paste into `cran-comments.md`
before handing off to the operator.**

---

## rhub v2 — Documented for Operator (Do Not Run Autonomously)

rhub v2 is NOT installed locally and must NOT be installed as part of this phase. The operator
can set it up independently after submission if additional platform evidence is desired.
[VERIFIED: 30-CONTEXT.md]

**Setup (one-time, operator performs):**
```r
# Install rhub (not done in this phase):
install.packages("rhub")

# One-time setup: adds .github/workflows/rhub.yaml to the repo
rhub::rhub_setup()
# Then: git add .github/workflows/rhub.yaml && git commit -m "chore: add rhub v2 workflow" && git push

# Verify configuration:
rhub::rhub_doctor()
```

**Running checks (operator performs after setup):**
```r
# Interactive platform selection — runs via GitHub Actions
rhub::rhub_check()

# List all available platforms first:
rhub::rhub_platforms()
```

**Recommended platforms for CRAN evidence:**
- `linux` (Ubuntu, R-release)
- `windows` (Windows Server, R-release)
- `macos` (macOS, R-release)
- `macos-arm64` (Apple Silicon, R-release)

rhub v2 uses GitHub Actions on the repo's own runners; results appear in the GitHub Actions tab.
Free for public repositories. [CITED: r-hub.github.io/rhub/]

**Note for cover letter:** Phase 29 CI (multi-OS `rcmdcheck` via GitHub Actions, including a
Suggests-forced leg) serves as the primary multi-platform evidence. rhub is supplementary.

---

## CRAN Policy Compliance Audit (CRAN-04)

### `\dontrun{}` vs `\donttest{}` Policy

CRAN policy: [CITED: cran.r-project.org/web/packages/policies.html]
- `\dontrun{}` — example is NEVER run (not by `R CMD check`, not by users running `example()`).
  Use ONLY when the example cannot be run at all (requires credentials, modifies system state,
  irreversible external action).
- `\donttest{}` — example is skipped by `R CMD check` but CAN be run by users. Use for examples
  that are slow (> a few seconds) but otherwise safe.
- **Gratuitous `\dontrun{}`** — using `\dontrun{}` on an example that COULD run (no network, no
  credentials, no system writes) is a CRAN policy violation that generates a NOTE.

**Audit of all 8 `\dontrun{}` uses in this package:**

| Rd file | Content inside `\dontrun` | Legitimate? | Verdict |
|---------|--------------------------|-------------|---------|
| `man/es_report.Rd` | Calls `run_event_study(my_task, ...)` then `es_report(task)` — requires data object `my_task`; renders to tempdir | Semi — rendering is slow, not unsafe | Convert to `\donttest{}` |
| `man/generate_report.Rd` | Calls `generate_report(task, ...)` — requires fitted task; writes to tempdir | Semi — slow render; uses tempdir | Convert to `\donttest{}` |
| `man/run_event_study.Rd` | Calls `run_event_study(my_task, ParameterSet$new(), report = TRUE)` — requires `my_task` | Data dependency only; no network; slow if report=TRUE | Convert to `\donttest{}` or add self-contained `my_task` stub |
| `man/AnthropicProvider.Rd` | Creates provider, calls `p$complete(...)` — requires `ANTHROPIC_API_KEY` | YES — network + credentials | Keep `\dontrun{}` |
| `man/OpenAICompatProvider.Rd` | Creates provider, calls network | YES — network + credentials | Keep `\dontrun{}` |
| `man/es_advise.Rd` | Offline KB path shown first (no LLM); LLM path inside nested `\dontrun` | Offline path: convert to `\donttest{}` | Mixed: split offline vs LLM paths |
| `man/flag_robustness.Rd` | Calls `run_event_study(my_task, ...)` — requires `my_task` | Data dependency; no network | Convert to `\donttest{}` or add stub |
| `man/nonparametric_intraday_test.Rd` | Calls test with `est_data`, `event_data` — requires data objects | Data dependency; no network | Convert to `\donttest{}` or add stub |

**Summary:** 2 of 8 `\dontrun{}` uses are fully legitimate (network + credentials). 6 are
candidates for conversion to `\donttest{}` or for adding self-contained data stubs. The examples
using `my_task` / `est_data` / `event_data` without defining them would cause check errors if
run — but the correct fix is `\donttest{}` (not `\dontrun{}`), because the code is valid and
could run with data available. [ASSUMED — classification based on reading example content;
CRAN reviewer judgment may differ on borderline cases]

**Recommended approach:** Convert all 6 candidates from `\dontrun{}` to `\donttest{}`. This
satisfies CRAN's "no gratuitous `\dontrun`" preference while still skipping the slow/data-
dependent examples during `R CMD check`. Do NOT try to run them in check by removing the
wrapper entirely — the pipeline examples take >>5 seconds.

### Network Access Policy

**Status: CLEAN** (confirmed in v0.60.0 check, documented in `cran-comments.md:158-162`).
All network access (LLM via `es_advise()`, factor data via `download_factor_data()`, stock data
via `download_stock_data()`) requires explicit user action and is in examples wrapped in
`\dontrun{}`. No default network access in tests or vignettes. [VERIFIED: cran-comments.md:158-162]

**Verification command:**
```bash
grep -rn "httr2\|jsonlite\|url(\|download\.\|readLines.*http\|GET\|POST" \
  tests/ vignettes/ --include="*.R" --include="*.Rmd"
# Expect: zero hits without a \dontrun or skip_on_cran() wrapper
```

### Temporary Directory Policy

CRAN policy: code may only write to `tempdir()` (or during installation to `TMPDIR`). [CITED: cran.r-project.org/web/packages/policies.html]

Files to check:
- `R/report.R` — uses `tempdir()` for render output? Must verify.
- `R/export.R` — `export_results()` writes to user-specified path. In examples, must use
  `tempdir()` as the output path, not `getwd()` or a hardcoded path.

**Verification command:**
```bash
grep -n "getwd\|setwd\|~\/" R/*.R man/*.Rd vignettes/*.Rmd 2>/dev/null | grep -v "#"
# Any hit that is NOT inside \dontrun{} or \donttest{} is a policy violation
```

### Skeleton .log Files Hygiene (CRAN-04)

`inst/rmarkdown/templates/event_study_report/skeleton/` contains transient `file*.log` files
(e.g., `file2692c28316539.log`) from test renders. These are stale artifacts.

**Status: Already covered by `.Rbuildignore`.**
The line `^inst/rmarkdown/templates/event_study_report/skeleton/.*\.log$` is present in
`.Rbuildignore`. [VERIFIED: .Rbuildignore (read this session)]

These files will NOT appear in the CRAN tarball. No action needed beyond confirming the
`.Rbuildignore` rule is intact.

**However:** The files still exist in the source tree, which adds noise and may confuse future
contributors. The executor may optionally delete them (they are regenerated by tests) and add a
`tests/testthat/teardown-*.R` cleanup. This is cosmetic — it does not affect the CRAN submission.

### Example Runtime Limits

CRAN policy: "Examples should run for no more than a few seconds each." [CITED: cran.r-project.org/web/packages/policies.html]

The pipeline examples (`run_event_study`, `es_report`, `generate_report`) will take several
seconds even with minimal data. Converting these from `\dontrun{}` to `\donttest{}` (see above)
is the correct solution — `\donttest{}` blocks are not timed by `R CMD check`.

For any `\examples{}` blocks that DO run (not wrapped in `\dontrun` or `\donttest`), verify
they complete in < 5 seconds. The dataset examples (`dieselgate`, `earnings_surprises`) and
utility examples (`recommend_stat`) have no wrapper and run inline — these must be fast.

---

## DESCRIPTION Version Bump

```r
# In DESCRIPTION, change:
Version: 0.65.0
Date: 2026-09-09
# To:
Version: 0.66.0
Date: 2026-09-12  # (or the actual submission prep date)
```

**NEWS.md:** Already has an `# EventStudy 0.66.0` stanza (Phase 28 content confirmed in NEWS.md).
The executor should add Phase 29 and Phase 30 items to the 0.66.0 stanza:
- Phase 29: install-tested CI, `report_table` export, vignette/example audits.
- Phase 30: CRAN resubmission prep, policy compliance, win-builder clean.

---

## Submission Handoff Runbook (CRAN-05)

The executor produces this verbatim handoff block for the operator:

```
=== CRAN SUBMISSION HANDOFF — EventStudy v0.66.0 ===

Preconditions (executor confirms before handing off):
  [ ] R CMD check --as-cran: 0 errors, 0 warnings, explainable notes only
  [ ] cran-comments.md: rewritten with archival acknowledgment + platform results
  [ ] win-builder results: pasted into cran-comments.md
  [ ] DESCRIPTION: Version 0.66.0, Date updated
  [ ] NEWS.md: 0.66.0 stanza complete
  [ ] All ~2359 tests green locally

Operator steps:
  1. Open RStudio / R console in the EventStudy project root.
  2. Run:  devtools::submit_cran()
     This builds a source tarball and opens the CRAN webform pre-filled.
  3. In the webform Comments field, paste the FULL contents of cran-comments.md.
  4. Check the "Uploaded by maintainer" checkbox, submit.
  5. Watch for an email from CRAN at sm@data-zoo.de within minutes.
     The email has a confirmation link — CLICK IT within 24 hours or the
     submission is discarded.
  6. CRAN review typically takes 1–10 business days. Respond promptly to any
     automated or manual feedback.

Alternative (manual webform without devtools):
  1. Build tarball: devtools::build(manual = FALSE, vignettes = FALSE)
  2. Go to: https://cran.r-project.org/submit.html
  3. Upload the .tar.gz, fill in the comments, submit.
  4. Same email confirmation step applies.

rhub v2 (supplementary, optional — operator only):
  install.packages("rhub")  # one-time
  rhub::rhub_setup()        # adds .github/workflows/rhub.yaml; commit + push
  rhub::rhub_check()        # interactive platform selection; runs via GitHub Actions
=== END HANDOFF ===
```

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| --as-cran check | Manual `R CMD check` shell invocation | `rcmdcheck::rcmdcheck(args = c("--as-cran", ...))` | Handles R version, returns structured results, integrates with devtools |
| Win-builder submission | Manual FTP/web upload | `devtools::check_win_devel()` / `check_win_release()` | One call, handles build + upload + email |
| Tarball build | `tar czf ...` shell command | `devtools::build()` | Applies `.Rbuildignore`, correct structure |
| Submit | Direct HTTP POST to CRAN | `devtools::submit_cran()` | Opens webform pre-filled; operator still confirms |

---

## Common Pitfalls

### Pitfall 1: Running `--as-cran` without `_R_CHECK_FORCE_SUGGESTS_=false`
**What goes wrong:** The check produces an ERROR (not NOTE) for every Suggests package not installed.
This blocks the gate and looks much worse than the actual state.
**How to avoid:** Always set `Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")` before
`rcmdcheck::rcmdcheck(args = c("--as-cran", ...))`.

### Pitfall 2: Checking from `load_all` instead of installed package
**What goes wrong:** `devtools::check()` uses `load_all` by default in some configurations, which
can miss install-time NAMESPACE issues (the exact class Phase 29 was designed to catch).
**How to avoid:** Build the tarball first with `devtools::build()`, then check the tarball:
`rcmdcheck::rcmdcheck("EventStudy_0.66.0.tar.gz", args = c("--as-cran", "--no-manual"))`.

### Pitfall 3: Submitting before win-builder results arrive
**What goes wrong:** `cran-comments.md` has placeholder text; CRAN reviewer sees incomplete
submission and may reject or request resubmission.
**How to avoid:** `check_win_devel()` results arrive within ~30 minutes. Wait for email, paste
results, THEN hand off to operator.

### Pitfall 4: Cover letter omits archival acknowledgment
**What goes wrong:** CRAN reviewer sees a package they know was archived; without explicit
acknowledgment, this reads as the submitter ignoring the archival, which delays review.
**How to avoid:** The first section of cran-comments.md must be "Resubmission (previously
archived 2024-04-20)" with explicit: date, reason, what changed.

### Pitfall 5: `\dontrun{}` on the full es_advise() offline example
**What goes wrong:** The offline KB path (`es_advise(diag, task_type = "recommend_stat")`) makes
no network call and has no credentials requirement. Wrapping it in `\dontrun{}` is gratuitous
and may receive a CRAN NOTE.
**How to avoid:** Split the es_advise.Rd example into offline (use `\donttest{}`) and LLM
provider paths (use `\dontrun{}`).

### Pitfall 6: Undefined globals NOTE left unresolved
**What goes wrong:** The `median`/`tail` undefined-globals NOTE has been carried since v0.60.0.
CRAN reviewers notice when the same NOTE appears across multiple submissions.
**How to avoid:** Fix before submission — add `@importFrom stats median` and
`@importFrom utils tail` to the roxygen header of `R/es_diagnostics.R`, then `devtools::document()`.

---

## Code Examples

### Check sequence (executor runs in order)

```r
# 1. Bump version in DESCRIPTION (text edit)

# 2. Fix undefined globals in es_diagnostics.R (roxygen edit + document)
devtools::document()

# 3. Build tarball
pkg_tarball <- devtools::build(manual = FALSE, vignettes = FALSE)
# Returns path like "../EventStudy_0.66.0.tar.gz"

# 4. Clean --as-cran check
Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")
rcmdcheck::rcmdcheck(
  path     = pkg_tarball,
  args     = c("--as-cran", "--no-manual"),
  error_on = "never"
)

# 5. Run full test suite
devtools::test()

# 6. Win-builder (async — results emailed to sm@data-zoo.de)
devtools::check_win_devel()
devtools::check_win_release()

# 7. After email arrives: paste results into cran-comments.md

# 8. Operator: devtools::submit_cran()
```

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | CRAN treats archived-package resubmission as a fresh submission (no unarchive API) | CRAN Policy | Low — consistent with all documented R community experience; worst case: email cran@r-project.org to ask |
| A2 | `_R_CHECK_FORCE_SUGGESTS_=false` converts Suggests-absent ERROR to NOTE (not silently suppresses) | Check mechanics | Low — documented community practice; verify by running check and observing output category |
| A3 | Win-builder results arrive within ~30 minutes | Win-builder | Low — documented in R Packages 2e; delays possible if win-builder is under load |
| A4 | 6 of the 8 `\dontrun{}` uses are candidates for `\donttest{}` conversion | \dontrun audit | Medium — CRAN reviewer judgment on "gratuitous" varies; converting to `\donttest{}` is the safe direction |
| A5 | Phase 29 CI counts as multi-OS evidence in the cover letter | Multi-platform | Low — CRAN accepts any documented CI evidence; explicit statement in cover letter is sufficient |

---

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|------------|-----------|---------|----------|
| `devtools` | win-builder, tarball build, submit | ✓ | installed | — |
| `rcmdcheck` | local --as-cran check | ✓ | installed | `devtools::check()` with args |
| R | local check runtime | ✓ | 4.6.1 | — |
| `rhub` | platform checks | ✗ | not installed | Phase 29 CI + win-builder (sufficient) |
| Internet (win-builder) | CRAN-02 | ✓ (assumed) | — | Cannot substitute; flag if offline |

---

## Validation Architecture

### Phase Gate

The phase gate for CRAN-01 is:
```r
Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")
rcmdcheck::rcmdcheck(
  args     = c("--as-cran", "--no-manual"),
  error_on = "never"
)
# Required: ERRORS: 0, WARNINGS: 0
# Acceptable: NOTES that are explainable (listed in cran-comments.md)
```

And:
```r
devtools::test()
# Required: 0 failures, 0 errors; skips for Suggests-absent OK
```

There is no automated Nyquist test harness for this phase — the deliverables are files
(`DESCRIPTION`, `NEWS.md`, `cran-comments.md`) and a check report. The gate is the check result.

---

## Security Domain

Not applicable to this phase. Phase 30 is release-engineering (version files, check commands,
cover letter). No new R code is introduced; no security surface changes.

---

## Sources

### Primary (HIGH confidence)
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html) — network/file write policy, size limits, example runtime policy
- [R Packages (2e) Chapter 22 — Releasing to CRAN](https://r-pkgs.org/release.html) — cran-comments.md structure, resubmission cover letter, win-builder mechanics
- [rhub v2 documentation](https://r-hub.github.io/rhub/) — `rhub_setup()`, `rhub_check()`, `rhub_platforms()` exact commands

### Secondary (MEDIUM confidence)
- Existing `cran-comments.md` in repo — prior check baselines, confirmed NOTEs inventory
- `.Rbuildignore` (read this session) — confirms skeleton `.log` files are already excluded from tarball

### Tertiary (LOW confidence / ASSUMED)
- `_R_CHECK_FORCE_SUGGESTS_=false` community practice — widely documented but not in written CRAN policy
- CRAN reviewer expectations for archived-package cover letters — observed community practice, not formal policy

---

## Metadata

**Confidence breakdown:**
- CRAN policy compliance: HIGH — from official CRAN policy page
- win-builder / rhub commands: HIGH — from official docs
- `\dontrun` audit classification: MEDIUM — based on reading example content; reviewer may differ
- Archival cover-letter framing: HIGH on structure; MEDIUM on exact wording

**Research date:** 2026-09-12
**Valid until:** 2027-03-12 (CRAN policy is stable; rhub v2 API may evolve)
