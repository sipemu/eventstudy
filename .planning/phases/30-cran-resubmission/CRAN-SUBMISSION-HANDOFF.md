# CRAN Submission Handoff — EventStudy v0.66.0

**Maintainer / operator:** Simon Mueller <sm@data-zoo.de>
**Prepared:** 2026-09-12 (Phase 30, Plan 02)

This is the copy-pasteable runbook for firing the CRAN submission. **Writing or
reading this file submits nothing.** The agent has prepared the package to
submission-ready and stops before the irreversible step (CRAN-05); the actions
below are yours to run because only you can receive and click the CRAN
confirmation email at sm@data-zoo.de.

---

## 1. Preconditions checklist (confirm before submitting)

Tick each box before running `submit_cran()`:

- [ ] `R CMD check --as-cran` on the built tarball is clean: **0 errors, 0
      warnings, explainable NOTEs only** (the single expected NOTE is the
      archived-package "New submission" / incoming-feasibility NOTE).
      Command used in prep:
      `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual EventStudy_0.66.0.tar.gz`
- [ ] `cran-comments.md` is rewritten with the 2024-04-20 archival
      acknowledgment and the ground-up-rewrite framing.
- [ ] **The two win-builder result blocks in `cran-comments.md` are filled**
      with the emailed R-devel and R-release summaries — **no
      `[PENDING win-builder email …]` placeholder remains.** Do NOT submit while
      any placeholder text is present (see Pitfall note below).
- [ ] `DESCRIPTION`: `Version: 0.66.0`, `Date:` current.
- [ ] `NEWS.md`: the `# EventStudy 0.66.0` stanza is complete.
- [ ] Full test suite green locally (~2359 tests; last prep run:
      `FAIL 0 | WARN 6 | SKIP 37 | PASS 2561`).

> **Pitfall — never submit with placeholder text.** If the win-builder emails
> have not yet arrived at sm@data-zoo.de, WAIT. They land ~15–30 min after
> dispatch. Paste the verbatim `N errors | N warnings | N notes` summary (and
> any NOTE text) into the two Windows blocks in `cran-comments.md` first, then
> submit.

---

## 2. Primary submit path (`devtools::submit_cran()`)

1. Open RStudio / an R console **at the EventStudy project root**.
2. Run:

   ```r
   devtools::submit_cran()
   ```

   This builds a source tarball and opens the CRAN submission webform
   pre-filled.
3. In the webform **Comments** field, paste the **FULL contents of
   `cran-comments.md`** (the whole file, including the archival acknowledgment
   and both win-builder result blocks).
4. Check the **"Uploaded by maintainer"** checkbox.
5. Submit.

---

## 3. Webform fallback (manual, without `submit_cran()`)

If `devtools::submit_cran()` is unavailable or fails:

1. Build the tarball:

   ```r
   devtools::build(manual = FALSE, vignettes = FALSE)
   ```

2. Go to: **https://cran.r-project.org/submit.html**
3. Upload the resulting `EventStudy_0.66.0.tar.gz`.
4. Paste the full `cran-comments.md` into the comments field, confirm you are
   the maintainer, and submit.
5. The same email-confirmation step (below) applies.

---

## 4. Email confirmation — REQUIRED within 24 hours

After you submit (either path), **CRAN emails a confirmation link to
sm@data-zoo.de**.

- **You must CLICK that link within 24 hours** or the submission is silently
  **discarded** and nothing is queued for review.
- After confirmation, CRAN review typically takes **1–10 business days**.
  Respond promptly to any automated or manual reviewer feedback.

The agent will NOT run `submit_cran()` and will NOT click the confirmation
link — those are yours.

---

## 5. rhub v2 — supplementary, optional (operator only, NOT run by the agent)

Phase 29 CI (multi-OS `rcmdcheck` via GitHub Actions, including a
Suggests-forced leg) is the primary multi-platform evidence, and win-builder
provides the Windows evidence. rhub is **not required** to submit and is **not
installed or triggered by the agent**. If you want extra platform coverage:

```r
# One-time setup
install.packages("rhub")
rhub::rhub_setup()   # adds .github/workflows/rhub.yaml — then commit + push it
rhub::rhub_doctor()  # verify configuration

# Run checks (GitHub-Actions-backed; results in the repo's Actions tab)
rhub::rhub_platforms()   # list available platforms
rhub::rhub_check()       # interactive platform selection
```

Recommended platforms for CRAN evidence: `linux`, `windows`, `macos`,
`macos-arm64` (all R-release). rhub v2 is free for public repositories.

---

*Prepared by the Phase 30 executor. This handoff is prep-only; the submission
and its email confirmation remain operator actions (CRAN-05).*
