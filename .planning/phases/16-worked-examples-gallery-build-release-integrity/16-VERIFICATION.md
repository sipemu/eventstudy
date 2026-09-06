---
phase: 16-worked-examples-gallery-build-release-integrity
verified: 2026-09-06T00:00:00Z
status: passed
score: 5/6 must-haves verified locally; BUILD-05 live-deploy accepted as tracked operator follow-up (user decision 2026-09-06)
behavior_unverified: 0
overrides_applied: 1
override_note: "BUILD-05 (live GitHub Actions green deploy) accepted-deferred by user on 2026-09-06 as a known operator step (enable GitHub Pages + push main — the same action pending from v0.62.0 Phase 12). All locally-verifiable content/config is proven correct: local pkgdown build EXIT 0, R CMD check 0E/0W/1 pre-existing NOTE, workflow byte-unchanged, _pkgdown.yml lists all 3 example slugs. The green Actions run remains as the deferred_human_verification item below and must be confirmed once Pages is live."
deferred_human_verification:
  - test: "Push Phase 16 commits to main (after Phase 12 operator step enables GitHub Pages), then confirm the pkgdown GitHub Actions run is green and all three example articles are live."
    expected: "Green Actions run; https://sipemu.github.io/eventstudy/articles/example-{earnings,regulatory,ma}.html each return HTTP 200 with rendered content."
    why_human: "The CI deploy requires a live GitHub Pages setup (Phase 12 operator step: enable Pages, push to main). Cannot be observed locally. The workflow file and _pkgdown.yml slugs are locally verified; only the live green Actions run is outstanding."
---

# Phase 16: Worked-Examples Gallery + Build & Release Integrity — Verification Report

**Phase Goal:** A cross-domain gallery of complete, rendered worked examples (data → model fit → test statistics → plots → written interpretation) connecting example → concept → API, and the entire v0.63.0 documentation addition ships proven clean: local build, CI deploy, and CRAN check all green.
**Verified:** 2026-09-06
**Status:** passed (BUILD-05 live-deploy accepted-deferred by user 2026-09-06 as a tracked operator follow-up)
**Re-verification:** No — initial verification

---

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
|---|-------|--------|---------|
| 1 | GALLERY-01: gallery.Rmd has .es-examples Worked Examples section with 3 cards; CSS slot .es-tag-examples #20c997 present; _pkgdown.yml Worked Examples articles group lists all 3 slugs; 3 card SVGs exist; navbar includes gallery + examples entries | ✓ VERIFIED | gallery.Rmd lines 161–180: .es-examples heading + 3 `<a>` elements for earnings/regulatory/M&A; pkgdown/extra.css lines 108, 146: .es-section-heading.es-examples and .es-tag-examples with #20c997; _pkgdown.yml lines 295–302: "Worked Examples" group; SVGs present (2048/1324/1419 bytes); navbar left: [...gallery, examples] at line 18 |
| 2 | GALLERY-02: all 3 example-*.Rmd exist, follow 10-section skeleton, use child="_setup.Rmd", have >=1 eval=TRUE table chunk AND >=1 eval=TRUE plotly chunk; domains are distinct (earnings/regulatory/M&A); rendered HTML confirmed table+plot | ✓ VERIFIED | All 3 Rmds exist; each has 10 `##` sections; child="_setup.Rmd" at line 7 of each; kable tables and ggplotly/plot_ly calls confirmed in sources; docs/articles/example-{earnings,regulatory,ma}.html contain `<table>` (1/2/1 hits) and plotly/htmlwidget (9/8/11 hits); domains: earnings (earnings_surprises dataset, MarketModel/Patell/BMP), regulatory (dieselgate two-group SignTest), M&A (simulate_event_study power analysis) |
| 3 | GALLERY-03: each example cross-links to methods-*.html AND ../reference/<fn>.html | ✓ VERIFIED | Rendered HTML: earnings has 12 methods- links + 14 ../reference/ links; regulatory has 12 + 16; M&A has 9 + 9. Source Rmds confirm explicit links to methods-return-models.html, methods-test-statistics.html, methods-diagnostics.html, methods-panel-did.html, and ../reference/run_event_study.html, ../reference/EventStudyTask.html, ../reference/ParameterSet.html, etc. |
| 4 | BUILD-04: local pkgdown build recorded EXIT 0, 0 errors, 0 new warnings | ✓ VERIFIED | SUMMARY documents pkgdown::build_site_github_pages(new_process=FALSE, install=FALSE) EXIT 0, 0 errors; 21 benign VignetteIndexEntry messages (18 pre-existing + 3 for new site-only articles following established Phase 15 convention — no new warning class); cran-comments.md line 243 confirms "0 errors"; docs/articles/ HTML files present as build artifacts (untracked, not committed); Phase 16 verification script result "ALL CHECKS PASSED" recorded in SUMMARY |
| 5 | BUILD-05: workflow byte-unchanged + _pkgdown.yml lists all 3 slugs; live CI deploy HUMAN_NEEDED | ⚠️ PRESENT_BEHAVIOR_UNVERIFIED (intentional human_needed) | .github/workflows/pkgdown.yaml: zero diff vs Phase 16 start (git diff 73a90b9^..HEAD -- .github/workflows/pkgdown.yaml returns empty); _pkgdown.yml Worked Examples group lists articles/example-{earnings,regulatory,ma} (lines 300–302); navbar examples component wired (lines 49–57); live green Actions deploy requires Phase 12 operator step (enable GitHub Pages + push to main) — cannot be observed locally |
| 6 | BUILD-06: R CMD check 0 errors / 0 warnings / 1 pre-existing NOTE; .Rbuildignore ^vignettes/articles intact; no R/, NAMESPACE, DESCRIPTION, data/ changes; 18 non-gallery CRAN vignettes byte-unchanged | ✓ VERIFIED | cran-comments.md line 212: "ERRORS: 0  WARNINGS: 0  NOTES: 1" for v0.63.0; NOTE is pre-existing median/tail undefined globals in es_diagnostics.R (Phase 5 baseline); .Rbuildignore line 16: "^vignettes/articles"; git diff 73a90b9^..HEAD -- R/ NAMESPACE DESCRIPTION data/ returns empty; git diff 73a90b9^..HEAD -- vignettes/ shows only gallery.Rmd + 3 new example-*.Rmd (no pre-existing vignette modified); SVG sizes negligible (2.0/1.3/1.4 KB); no VignetteIndexEntry in any example-*.Rmd |

**Score:** 5/6 truths verified (1 locally-verifiable sub-truth confirmed; 1 truth split as PRESENT_BEHAVIOR_UNVERIFIED for the live CI deploy only)

---

### Deferred Items

None.

---

### Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `vignettes/articles/example-earnings.Rmd` | Earnings example, 10-section skeleton | ✓ VERIFIED | Exists; 10 sections; child="_setup.Rmd"; kable + ggplotly; no VignetteIndexEntry |
| `vignettes/articles/example-regulatory.Rmd` | Regulatory shock (dieselgate two-group) | ✓ VERIFIED | Exists; 10 sections; child="_setup.Rmd"; kable + ggplotly; no VignetteIndexEntry |
| `vignettes/articles/example-ma.Rmd` | M&A power analysis (synthetic) | ✓ VERIFIED | Exists; 10 sections; child="_setup.Rmd"; power table + plotly power curve; SYNTHETIC DATA NOTICE block; no VignetteIndexEntry |
| `man/figures/card-example-earnings.svg` | Teal card SVG for gallery | ✓ VERIFIED | 2048 bytes; present |
| `man/figures/card-example-regulatory.svg` | Teal card SVG for gallery | ✓ VERIFIED | 1324 bytes; present |
| `man/figures/card-example-ma.svg` | Teal card SVG for gallery | ✓ VERIFIED | 1419 bytes; present |

---

### Key Link Verification

| From | To | Via | Status | Details |
|------|----|-----|--------|---------|
| gallery.Rmd "Worked Examples" section | example-{earnings,regulatory,ma}.html | bare href="example-*.html" (sibling in docs/articles/) | ✓ WIRED | gallery.Rmd lines 165/171/177; bare href correct for gallery.html sibling context |
| _pkgdown.yml "Worked Examples" articles group | 3 example slugs | articles/example-{earnings,regulatory,ma} | ✓ WIRED | _pkgdown.yml lines 300–302; articles/ prefix required and present |
| pkgdown/extra.css | .es-tag-examples { background:#20c997 } | direct rule | ✓ WIRED | line 146 confirmed |
| example-*.Rmd | _setup.Rmd | child="_setup.Rmd" | ✓ WIRED | line 7 of each article |
| example-*.Rmd | methods-*.html + ../reference/<fn>.html | relative anchor hrefs | ✓ WIRED | confirmed in rendered HTML (9–16 cross-links per article) |
| navbar structure | gallery + examples entries | left:[..., gallery, examples] | ✓ WIRED | _pkgdown.yml line 18; components: gallery (line 46) and examples (line 49) both defined |

---

### Data-Flow Trace (Level 4)

| Artifact | Data Variable | Source | Produces Real Data | Status |
|----------|--------------|--------|-------------------|--------|
| example-earnings.Rmd results table | CAR tibble from `EventStudy::tidy.EventStudyTask(result, type="car")` | `data("earnings_surprises")` → `EventStudyTask$new()` → `run_event_study()` | Yes — live eval chunk with real dataset | ✓ FLOWING |
| example-regulatory.Rmd CAAR table | CAAR from `EventStudy::tidy.EventStudyTask(result, type="aar")` sliced + `car_by_group()` | `data("dieselgate")` → pipeline | Yes — live eval chunk with bundled dataset | ✓ FLOWING |
| example-ma.Rmd power table | `$power` extracted from `simulate_event_study()` return list, mapped over `abnormal_return` grid | `simulate_event_study(seed=42)` | Yes — real extracted values (gradient 0.035→0.96 confirmed in SUMMARY) | ✓ FLOWING |
| example-earnings.Rmd p-values | `p_patell`, `p_bmp` from `pnorm`/`pt` on extracted statistics | computed from `PatellZTest`/`BMPTest` result tibble | Yes — concrete values shown in REVIEW-FIX: Patell Z=1.664 (p=0.0961), BMP=2.248 (p=0.1536) | ✓ FLOWING |

---

### Behavioral Spot-Checks

| Behavior | Command | Result | Status |
|----------|---------|--------|--------|
| 3 example HTMLs have table + plotly content | grep -c in docs/articles/*.html | earnings: table=1 plot=9; regulatory: table=2 plot=8; M&A: table=1 plot=11 | ✓ PASS |
| Cross-links present in rendered HTML | grep "methods-" and "../reference/" | 9–16 hits each per article | ✓ PASS |
| gallery.html has es-examples Worked Examples section | grep "es-examples" in gallery.html | 10 hits; heading confirmed | ✓ PASS |
| SYNTHETIC notice in M&A rendered HTML | grep -c "SYNTHETIC" in example-ma.html | 1 | ✓ PASS |
| No R/, NAMESPACE, DESCRIPTION, data/ changes in Phase 16 | git diff 73a90b9^..HEAD -- R/ NAMESPACE DESCRIPTION data/ | empty | ✓ PASS |
| .Rbuildignore excludes vignettes/articles | grep "articles" .Rbuildignore | ^vignettes/articles at line 16 | ✓ PASS |
| Workflow file byte-unchanged | git diff 73a90b9^..HEAD -- .github/workflows/pkgdown.yaml | empty | ✓ PASS |
| 18 non-gallery CRAN vignettes unchanged | git diff 73a90b9^..HEAD -- vignettes/ showing only gallery.Rmd + 3 new articles | confirmed | ✓ PASS |
| cran-comments.md has v0.62.0 + v0.63.0 sections | grep "v0.62.0\|v0.63.0" cran-comments.md | lines 175–254 confirm both sections | ✓ PASS |
| WR-01 fix: [@Corrado1989] replaced with [@Brown1985] | grep "Brown1985\|Corrado1989" example-regulatory.Rmd | Brown1985 at line 139; Corrado1989 absent | ✓ PASS |
| WR-02 fix: Monte Carlo caveat present | grep "Monte Carlo" example-ma.Rmd | lines 125–128: blockquote caveat present | ✓ PASS |
| IN-01 fix: H_0 states AAR level | grep "H_0.*AAR" example-earnings.Rmd | line 28: E[AAR_t] = 0 | ✓ PASS |
| IN-02 fix: p-values computed in stat-values chunk | grep "p_patell\|p_bmp" example-earnings.Rmd | lines 142–143 confirmed | ✓ PASS |
| IN-03 fix: articles/ prefix comment in _pkgdown.yml | grep "articles/ prefix" _pkgdown.yml | lines 290–294 confirmed | ✓ PASS |

---

### Probe Execution

Step 7c: SKIPPED — no probe-*.sh scripts declared for Phase 16. The PLAN specifies inline Rscript verification commands; the SUMMARY records the Phase 16 verification script output "ALL CHECKS PASSED". Full site rebuild is not reproducible locally without R package environment; the spot-checks above verify the build artifacts directly.

---

### Requirements Coverage

| Requirement | Source Plan | Description | Status | Evidence |
|-------------|-------------|-------------|--------|---------|
| GALLERY-01 | 16-01-PLAN.md | Pyfda-style gallery card index reachable from navbar | ✓ SATISFIED | gallery.Rmd + CSS slot + _pkgdown.yml + navbar — all verified above |
| GALLERY-02 | 16-01-PLAN.md | 3 complete end-to-end worked examples (data→fit→stats→plots→interpretation) | ✓ SATISFIED | 3 Rmds + 3 rendered HTMLs with table + plotly content confirmed |
| GALLERY-03 | 16-01-PLAN.md | Each example cross-links to Methods articles and reference pages | ✓ SATISFIED | 9–16 cross-links per article in rendered HTML |
| BUILD-04 | 16-01-PLAN.md | build_site() local EXIT 0, 0 new warnings | ✓ SATISFIED | SUMMARY + cran-comments.md document EXIT 0; build artifacts (docs/) present |
| BUILD-05 | 16-01-PLAN.md | CI workflow builds and deploys green (live Actions run) | ⚠️ HUMAN_NEEDED | Locally-verifiable parts confirmed; live CI run pending Phase 12 operator step |
| BUILD-06 | 16-01-PLAN.md | R CMD check 0 new NOTEs/WARNINGs; tarball clean; suite green | ✓ SATISFIED | cran-comments.md v0.63.0 section: 0 errors / 0 warnings / 1 pre-existing note |

---

### Anti-Patterns Found

| File | Line | Pattern | Severity | Impact |
|------|------|---------|----------|--------|
| cran-comments.md | 142 | `\uXXXX` in text | ℹ️ Info | Not a debt marker — describes a Unicode escape sequence; CRAN submission advice. No action needed. |

No TBD, FIXME, or unreferenced XXX debt markers found in any Phase 16 source file. The `XXX` in cran-comments.md is embedded in `\uXXXX` describing an escape syntax.

---

### Human Verification Required

#### 1. BUILD-05 Live CI Deploy

**Test:** After Phase 12 operator step (enable GitHub Pages on the repo, push Phase 16 commits to main), observe the GitHub Actions pkgdown workflow run triggered by the push.
**Expected:** Actions run completes green. The three example articles are live and return HTTP 200:
- https://sipemu.github.io/eventstudy/articles/example-earnings.html
- https://sipemu.github.io/eventstudy/articles/example-regulatory.html
- https://sipemu.github.io/eventstudy/articles/example-ma.html
**Why human:** The CI deploy requires a live GitHub Pages setup. The `.github/workflows/pkgdown.yaml` is confirmed byte-unchanged and runs the identical `pkgdown::build_site_github_pages(new_process=FALSE, install=FALSE)` command that passed locally. The `_pkgdown.yml` Worked Examples group lists all 3 slugs. The workflow discovers new articles automatically. Only the live Actions run cannot be observed without pushing to the remote.

---

### Gaps Summary

No gaps. All locally-verifiable must-haves are VERIFIED. The single human_needed item (BUILD-05 live CI deploy) is the expected and explicitly pre-declared outstanding item per both the PLAN and the SUMMARY — it is gated on the Phase 12 operator step that was already documented as deferred. The phase goal is substantively achieved: the gallery, articles, CSS, YAML, build gate, and CRAN check are all confirmed in the codebase.

---

_Verified: 2026-09-06_
_Verifier: Claude (gsd-verifier)_
