---
phase: 13-article-infrastructure-conventions-gate
reviewed: 2026-09-05T23:15:00Z
depth: standard
files_reviewed: 6
files_reviewed_list:
  - _pkgdown.yml
  - .Rbuildignore
  - vignettes/articles/_setup.Rmd
  - vignettes/articles/references.bib
  - vignettes/articles/_article-skeleton.Rmd
  - vignettes/articles/smoke-test.Rmd
findings:
  critical: 2
  warning: 1
  info: 1
  total: 4
status: issues_found
---

# Phase 13: Code Review Report

**Reviewed:** 2026-09-05T23:15:00Z
**Depth:** standard
**Files Reviewed:** 6
**Status:** issues_found

## Summary

Phase 13 establishes article infrastructure for the pkgdown-delivered Methods articles layer: a shared `_setup.Rmd` child chunk, co-located `references.bib`, a 10-section skeleton template, and a working smoke-test article (KaTeX + citation + plotly). The `math-rendering: katex` placement under `template:` is correct for pkgdown 2.2.0. The core smoke-test renders and the KaTeX/MathJax collision is avoided.

Two critical defects exist. First, the `.Rbuildignore` regex `^vignettes/articles$` does not match the directory path as R CMD build evaluates it, meaning `vignettes/articles/` may not be excluded from the CRAN tarball — and both `smoke-test.Rmd` and `_article-skeleton.Rmd` carry live `VignetteIndexEntry` metadata, which would cause `R CMD check` to attempt to build them and fail. Second, `_article-skeleton.Rmd` contains a `VignetteIndexEntry` in its YAML front matter; if the exclusion ever fails, this placeholder article ("Article Title") will appear in the CRAN vignette index and fail to build reproducibly.

---

## Critical Issues

### CR-01: `.Rbuildignore` regex does not reliably exclude `vignettes/articles/`

**File:** `.Rbuildignore:17`
**Issue:** The pattern `^vignettes/articles$` uses a bare `$` end-anchor. R's `tools:::.make_file_exts` and the `R CMD build` exclusion engine call `grepl(pattern, path)` against paths that include a trailing `/` for directories (e.g. `"vignettes/articles/"`). The dollar-sign anchor means the pattern does NOT match `"vignettes/articles/"` — only the bare string `"vignettes/articles"`. In practice on some R versions and build environments this means the entire `vignettes/articles/` subtree is silently included in the tarball.

Both `smoke-test.Rmd` and `_article-skeleton.Rmd` carry full `%\VignetteIndexEntry` / `%\VignetteEngine` declarations. If they land in the CRAN tarball inside `vignettes/`, R CMD check will attempt to build all registered vignettes, encounter `plotly::ggplotly()` (Suggests-only) possibly missing, and fail. The summary states verification passed — this is likely because `R CMD build` was run in the project root where the articles subdirectory happened to be matched; the regex risk is real on stricter build environments.

**Fix:** Use a pattern that matches regardless of trailing slash. The conventional CRAN-safe form is:

```text
^vignettes/articles
```

(drop the `$`). This matches `vignettes/articles`, `vignettes/articles/`, and any file beneath it. Alternatively use `^vignettes/articles(/|$)` for precision. Update `.Rbuildignore` line 17 accordingly.

---

### CR-02: `_article-skeleton.Rmd` carries live `VignetteIndexEntry` metadata

**File:** `vignettes/articles/_article-skeleton.Rmd:5-8`
**Issue:** The skeleton template contains:

```
%\VignetteIndexEntry{Article Title}
%\VignetteEngine{knitr::rmarkdown}
%\VignetteEncoding{UTF-8}
```

If the `.Rbuildignore` exclusion fails (see CR-01), `R CMD check` will see this as a registered vignette titled "Article Title" and try to build it. The skeleton references `dieselgate$firm` etc. as placeholders that will evaluate correctly, but the article produces a generic/unlabelled output that is not a real deliverable. More critically, the `_` prefix convention (used to suppress pkgdown rendering) is not respected by `R CMD check` — only `_pkgdown.yml`'s `articles:` section controls pkgdown exclusion. CRAN's vignette index would receive a stub entry.

Even if CR-01 is fixed, having `VignetteIndexEntry` in a file that is intentionally a template (never to be shipped as a vignette) is a latent hazard for any future `.Rbuildignore` regression.

**Fix:** Remove the `vignette:` block entirely from `_article-skeleton.Rmd`'s YAML front matter, replacing it with a comment:

```yaml
---
title: "Article Title"
output: rmarkdown::html_vignette
bibliography: references.bib
# NOTE: No vignette: block — this file is a template, not a CRAN vignette.
# It is excluded from the tarball via .Rbuildignore (^vignettes/articles).
---
```

---

## Warnings

### WR-01: Navbar duplication — `articles` and `gallery` both link to `articles/gallery.html`

**File:** `_pkgdown.yml:27-37`
**Issue:** The `articles` component (line 28–30) and the new `gallery` component (line 35–37) resolve to the same URL `articles/gallery.html`. The navbar therefore shows two entries — "Articles" and "Gallery" — that navigate to identical content. This is a UX defect and will become a maintainability problem when the gallery page diverges. The `structure.left` list is `[get-started, reference, articles, methods, gallery]`, so both entries render side-by-side.

**Fix:** Either:
- Remove the `articles` component override and keep only `gallery` (letting pkgdown render its built-in articles dropdown for the `articles` slot), or
- Remove the `gallery` component entirely from `components` and `structure.left`, since `articles` already covers it.

The second option preserves the pkgdown convention where `articles` is the standard navbar slot for the article index.

---

## Info

### IN-01: `_setup.Rmd` chunk label begins with underscore

**File:** `vignettes/articles/_setup.Rmd:1`
**Issue:** The chunk label is `` `{r _setup, include=FALSE}` ``. Chunk labels starting with `_` are valid in knitr >= 1.35 (released 2021) and the package requires R 4.1.0+ which pairs with a recent knitr, so this will not break in practice. However, older knitr versions (pre-1.35) would silently mangle the label. The label also matches the filename, which is fine, but the convention in this codebase is lowercase-hyphen labels (e.g. `load-data`, `run-pipeline` in smoke-test.Rmd).

**Fix:** Rename the chunk label to `setup` or `article-setup` to match existing conventions and avoid any edge-case knitr compatibility questions:

```r
```{r setup, include=FALSE}
```

---

_Reviewed: 2026-09-05T23:15:00Z_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: standard_
