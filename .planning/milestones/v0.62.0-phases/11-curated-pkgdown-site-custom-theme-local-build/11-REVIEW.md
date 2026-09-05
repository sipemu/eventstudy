---
phase: 11-curated-pkgdown-site-custom-theme-local-build
reviewed: 2026-09-04T00:00:00Z
depth: standard
files_reviewed: 2
files_reviewed_list:
  - _pkgdown.yml
  - pkgdown/extra.scss
findings:
  critical: 0
  warning: 1
  info: 1
  total: 2
status: issues_found
---

# Phase 11: Code Review Report

**Reviewed:** 2026-09-04
**Depth:** standard
**Files Reviewed:** 2
**Status:** issues_found

## Summary

Reviewed `_pkgdown.yml` (site config) and `pkgdown/extra.scss` (SCSS overrides) for Phase 11. The overall structure is sound: the pkgdown `url:` matches the GitHub Pages target, all 18 vignette slugs resolve to real `.Rmd` files, all 18 navbar article `href:` values match vignette slugs exactly, there are no duplicate reference topics, and all NAMESPACE-exported symbols appear in exactly one reference group. The four `title: internal` entries (`ModelBase`, `ReturnCalculation`, `TestStatisticBase`, `degenerate-input-contract`) have corresponding `.Rd` files with correct `\alias{}` entries; pkgdown will render them correctly without warnings despite their absence from `NAMESPACE`. bslib color and typography variables (`bg`, `fg`, `primary`, etc.) are valid first-class `bs_theme()` arguments; `font-size-base` and `headings-font-weight` are valid Bootstrap 5 SASS variable names that bslib accepts via the `...` passthrough.

Two issues were found: a dead CSS selector in `extra.scss` (INFO) and a bslib variable naming inconsistency that risks the override silently not applying (WARNING).

## Structural Findings (fallow)

No structural pre-pass provided.

## Narrative Findings (AI reviewer)

## Warnings

### WR-01: `font-size-base` and `headings-font-weight` bslib key names may silently fail

**File:** `_pkgdown.yml:27-28`
**Issue:** The bslib block uses `font-size-base` and `headings-font-weight` (hyphenated). bslib's `bs_theme()` passes unrecognized named arguments through as Bootstrap SASS variable overrides, but the mapping between YAML key names and SASS variable names depends on whether pkgdown performs a hyphen-to-underscore translation before calling `bs_theme()`. The formal `bs_theme()` parameters use underscores (`base_font`, `heading_font`) while Bootstrap SASS variables use hyphens (`$font-size-base`, `$headings-font-weight`). If pkgdown passes these keys to `bs_theme()` verbatim (hyphenated), R will coerce the hyphenated names into list element names correctly at the YAML parse level, but `bs_theme()`'s `...` handling sends them to `sass::sass_layer()` with the hyphenated names — which is actually the correct form for Bootstrap SASS vars. This path works in bslib >= 0.4.0. However, pkgdown versions prior to 2.0.7 did not forward unknown bslib keys to `bs_theme()` at all, silently dropping them with no error. Since there is no pinned pkgdown version requirement, a user running an older pkgdown will see the default font size and heading weight with no warning. The risk is silent visual regression, not a build error.

**Fix:** Either document the minimum pkgdown version required (>= 2.0.7) in `DESCRIPTION` under `Suggests`, or replace with bslib-idiomatic equivalents that are resilient across pkgdown versions. If pkgdown >= 2.0.7 is already enforced elsewhere, this can be downgraded to INFO:

```yaml
template:
  bootstrap: 5
  bslib:
    bg: "#FFFFFF"
    fg: "#1A1A1A"
    primary: "#1C4E80"
    secondary: "#F8F9FA"
    success: "#198754"
    info: "#0dcaf0"
    warning: "#ffc107"
    danger: "#C0392B"
    base_font: {google: "Inter"}
    heading_font: {google: "Inter"}
    code_font: {google: "JetBrains Mono"}
    font-size-base: "1rem"        # requires pkgdown >= 2.0.7
    headings-font-weight: "600"   # requires pkgdown >= 2.0.7
```

Or move `font-size-base` and `headings-font-weight` into `pkgdown/extra.scss` as SCSS variable overrides, which are version-independent:

```scss
// In extra.scss — works with any pkgdown version
$font-size-base: 1rem;
$headings-font-weight: 600;
```

## Info

### IN-01: `.r-function` selector in `extra.scss` is dead — downlit does not emit that class

**File:** `pkgdown/extra.scss:5`
**Issue:** The combined selector `.r-function, .fu` applies the brand accent color `#1C4E80` to function names in R code blocks. The `.fu` half is correct — downlit (pkgdown's R code highlighter) assigns `class="fu"` to function-call tokens. However, `.r-function` is not a class emitted by downlit, highlight.js, or any other highlighter used by pkgdown. It will never match any element on the rendered site. The rule is dead weight but harmless.

**Fix:** Remove the dead selector to keep the stylesheet minimal:

```scss
// Syntax highlighting: function/symbol names use the brand accent color
// downlit assigns class="fu" to R function names
.fu {
  color: #1C4E80;
}
```

---

_Reviewed: 2026-09-04_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: standard_
