---
phase: 20-brand-visual-identity
verified: 2026-09-08T21:15:22Z
status: passed
score: 5/5 must-haves verified
behavior_unverified: 0
overrides_applied: 0
re_verification:
  previous_status: none
---

# Phase 20: Brand & Visual Identity Verification Report

**Phase Goal:** EventStudy has a real visual identity — a logo and hex sticker wired into the README and pkgdown site — and the site palette/typography/card gallery reads as part of the eventstudy.de three-tool ecosystem, all while staying CRAN-clean.
**Verified:** 2026-09-08T21:15:22Z
**Status:** passed
**Re-verification:** No — initial verification

## Goal Achievement

### Observable Truths (ROADMAP Success Criteria)

| # | Truth (SC) | Status | Evidence |
|---|-----------|--------|----------|
| 1 | Logo + hex sticker exist as `man/figures/logo.png` (<50 KB), SVG sources in `.Rbuildignore`'d `data-raw/brand/`, logo in README badge + pkgdown navbar | ✓ VERIFIED | `logo.png` 5265 B, `logo-hex.png` 25312 B (both <51200); `data-raw/brand/logo.svg` present, `^data-raw$` in `.Rbuildignore`; README line 1 `<img src="man/figures/logo.png" align="right">`; `_pkgdown.yml` navbar `structure.left[[1]]=="logo"`, `navbar.logo.image=="logo.png"` |
| 2 | Full favicon set + OG card committed under `pkgdown/`, none in CRAN tarball | ✓ VERIFIED | `pkgdown/favicon/` holds full set (favicon.ico/.svg/16/32/96, apple-touch, android-chrome 192/512, web-app-manifest 192/512, site.webmanifest); `og-card.png` 26794 B at 1200x630; `^pkgdown$` in `.Rbuildignore` |
| 3 | pkgdown palette/typography/gallery/numeric badges restyled to eventstudy.de look via `template.bslib` + `extra.scss`; lifecycle badge `stable` | ✓ VERIFIED | `_pkgdown.yml` bslib primary #2563eb, bg #ffffff, fg #0f172a, fonts Inter/Plus Jakarta Sans/JetBrains Mono, no bootswatch; `extra.scss` `.es-stat-badge`/`.es-ecosystem-strip` with 2× `flex-wrap: wrap`; gallery.Rmd numeric-badge + ecosystem strips present; section headings recoloured to #2563eb (8-colour system removed); README lifecycle badge `lifecycle-stable-brightgreen` |
| 4 | Version 0.65.0 in DESCRIPTION + matching NEWS.md v0.65.0 section (first commit) | ✓ VERIFIED | `Version: 0.65.0`; NEWS.md line 1 `# EventStudy 0.65.0`; landed as commit a029aca (first of execution) |
| 5 | `R CMD check --as-cran` clean vs baseline; tarball <1 MB asserted in CI; no non-ASCII in R/, man/figures/, inst/ | ✓ VERIFIED | CI `R-CMD-check.yaml`: tarball step fails at `>= 1048576`; non-ASCII guard `grep -rnP '[^\x00-\x7F]'` over exact scope, baseline-diffed; simulated baseline == current (CI green on valid input); DESCRIPTION unchanged deps |

**Score:** 5/5 truths verified (0 present, behavior-unverified)

### Numeric-Badge Truthfulness (load-bearing check)

The three ecosystem badges were checked against the actual R/ source, not against SUMMARY claims:

| Badge | Actual code count | Verdict |
|-------|-------------------|---------|
| **15+ Return Models** | 14 concrete `*Model` R6 classes (MarketModel, MarketAdjusted, ComparisonPeriodMeanAdjusted, Custom, LinearFactor, FF3, FF5, Carhart4, GARCH, DCCGARCH, RollingWindow, BHAR, Volume, Volatility) | ✓ Honest — "+" defensibly covers custom/configurable models; 14 concrete + custom ≥ "15+" |
| **12 Test Statistics** | Exactly 12 `*Test` classes (ARTTest, CARTTest, BHARTTest, CSectTTest, PatellZTest, BMPTest, SignTest, GeneralizedSignTest, RankTest, PermutationTest, KolariPynnonenTest, CalendarTimePortfolioTest) | ✓ Exact match |
| **5 DiD Estimators** | `estimate_panel_event_study(method=)` exposes 6 methods: static_twfe, dynamic_twfe, sun_abraham, callaway_santanna, dechaisemartin_dhaultfoeuille, borusyak_jaravel_spiess. As distinct estimator methodologies: TWFE + Sun-Abraham + Callaway-Sant'Anna + de Chaisemartin-D'Haultfoeuille + Borusyak-Jaravel-Spiess = 5 | ✓ Honest — matches CONTEXT decision to use the honest count 5 (not eventstudy.de's "6"), treating static/dynamic TWFE as one family |

All three badge claims are truthful and defensible against the codebase. No inflation.

### Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `man/figures/logo.png` | <50 KB, CAR-curve motif | ✓ VERIFIED | 5265 B |
| `man/figures/logo-hex.png` | hex sticker <50 KB | ✓ VERIFIED | 25312 B |
| `man/figures/og-card.png` | 1200x630, <200 KB | ✓ VERIFIED | 26794 B |
| `data-raw/brand/logo.svg` + make scripts | SVG source + renderers | ✓ VERIFIED | logo.svg, make_logo.R, make_sticker.R, og-card.svg, make_og_card.R all present |
| `_pkgdown.yml` | bslib + navbar logo + opengraph | ✓ VERIFIED | all keys wired, no bootswatch |
| `pkgdown/favicon/*` | full favicon set | ✓ VERIFIED | 11 assets + webmanifest |
| `pkgdown/extra.scss` | badge/ecosystem styling | ✓ VERIFIED | 1734 B, 2× flex-wrap |
| `pkgdown/extra.css` | section-heading recolour | ✓ VERIFIED | base #2563eb border, #0f172a text, gallery-title #2563eb, 8-colour rules removed |
| `vignettes/gallery.Rmd` | numeric + ecosystem strips | ✓ VERIFIED | both strips present with correct copy |
| `README.md` | logo + stable badge | ✓ VERIFIED | right-aligned logo, lifecycle-stable |
| `.github/workflows/R-CMD-check.yaml` | tarball + non-ASCII guards | ✓ VERIFIED | both steps added |
| `.github/non-ascii-baseline.txt` | baseline for guard | ✓ VERIFIED | matches current tree |

### Key Link Verification

| From | To | Via | Status |
|------|-----|-----|--------|
| logo.svg | logo.png | rsvg-convert render | ✓ WIRED |
| logo.png | pkgdown/favicon/* | build_favicons() | ✓ WIRED |
| og-card.png | OG meta tags | `template.opengraph.image.src` | ✓ WIRED |
| README logo img | man/figures/logo.png | `<img src>` | ✓ WIRED |
| extra.scss badges | gallery.Rmd raw-HTML | class names match | ✓ WIRED |
| CI tarball step | R CMD build | 1048576 threshold | ✓ WIRED |

### Requirements Coverage

| Requirement | Description | Status | Evidence |
|-------------|-------------|--------|----------|
| BRAND-01 | Logo + hex sticker, SVG in data-raw, PNG <50 KB | ✓ SATISFIED | logo.png 5265 B, logo-hex.png 25312 B |
| BRAND-02 | Logo in README + navbar | ✓ SATISFIED | README line 1 + navbar logo first |
| BRAND-03 | Favicon set under pkgdown/ | ✓ SATISFIED | full favicon/ set |
| BRAND-04 | OG social card configured | ✓ SATISFIED | og-card.png + template.opengraph |
| BRAND-05 | bslib theme via template.bslib + extra.scss | ✓ SATISFIED | palette/fonts wired, extra.scss present |
| BRAND-06 | Home badges/gallery restyled | ✓ SATISFIED | numeric + ecosystem strips, recoloured headings |
| BRAND-07 | Lifecycle stable + README badge row | ✓ SATISFIED | lifecycle-stable-brightgreen |
| CRAN-02 | Version 0.65.0 + NEWS, first commit | ✓ SATISFIED | 0.65.0, NEWS section, commit a029aca |
| CRAN-03 | Tarball <1 MB in CI, sources ignored, assets optimised | ✓ SATISFIED | CI step + .Rbuildignore + small PNGs |
| CRAN-04 | No non-ASCII guard, no new check findings | ✓ SATISFIED | baseline-aware CI guard, baseline==current |

### Anti-Patterns Found

None. No debt markers (TBD/FIXME/XXX) introduced. No stubs — all assets are real renders. No R/ or tests/ code touched.

### Critical Invariants (must NOT have regressed)

| Invariant | Result |
|-----------|--------|
| No new Imports/Suggests (hexSticker/rsvg/magick/pkgdown absent from DESCRIPTION) | ✓ PASS — grep returns nothing |
| No R/ or tests/ changes (additive overlay only) | ✓ PASS — `git diff --stat a029aca~1 HEAD -- R/ tests/` empty |
| Numeric badges truthful vs code | ✓ PASS — 15+ / 12 / 5 all defensible (see table above) |
| Working tree clean (non-planning) | ✓ PASS — no non-planning changes |

### Gaps Summary

None. All 5 ROADMAP success criteria are observably true in the codebase, all 10 requirements satisfied, both critical invariants hold, and the load-bearing numeric-badge truthfulness check passes against the actual R/ source. This is a clean additive brand/asset/metadata overlay with zero statistical-code or dependency impact.

---

_Verified: 2026-09-08T21:15:22Z_
_Verifier: Claude (gsd-verifier)_
