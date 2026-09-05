# Stack Research

**Domain:** pkgdown rich-article documentation for a CRAN R package (EventStudy v0.63.0)
**Researched:** 2026-09-05
**Confidence:** MEDIUM (pkgdown behavior verified against official docs + known GitHub issues; plotly/MathJax conflict verified against upstream issue tracker; bibliography quirk verified against community documentation)

## Context

This replaces the v0.60.0 AI-advisor stack research. This research answers: what tooling and configuration is needed to build rich rendered pkgdown articles (Methods pages + worked-examples gallery) on top of the existing v0.62.0 pkgdown site? The existing stack (R 4.1.0+, R6, tidyverse, ggplot2, plotly, knitr, rmarkdown, pkgdown, Bootstrap 5) is already in place. Research scope is limited to NEW capabilities: articles mechanism, math rendering, table rendering, plot rendering, and academic citations.

---

## 1. Articles Mechanism: vignettes/articles/ vs vignettes/

### How it works

pkgdown distinguishes two kinds of rendered content:

- **Vignettes** (`vignettes/*.Rmd`): CRAN-shipped. Must have `VignetteIndexEntry` in YAML. Included in the tarball. Slow `R CMD check` because they re-build. These are the existing 18 concise vignettes — do not touch them.
- **Articles** (`vignettes/articles/*.Rmd`): pkgdown-only. No `VignetteIndexEntry`. Excluded from the CRAN tarball via `.Rbuildignore`. Created with `usethis::use_article()` which automatically adds `^vignettes/articles$` to `.Rbuildignore`. Never touched by `R CMD check` — zero impact on check time or tarball size.

The YAML front matter for an article is minimal — no `vignette:` block:

```yaml
---
title: "Return Models — Methods and Assumptions"
output: rmarkdown::html_vignette
---
```

pkgdown renders `vignettes/articles/*.Rmd` to `docs/articles/*.html` using its own document format (a wrapper around `rmarkdown::html_document()` that accepts the pkgdown template, theme, and `self_contained` arguments).

### _pkgdown.yml articles: section

Articles are grouped and navbarred via the `articles:` key. Each group supports:

```yaml
articles:
  - title: "Learn / Methods"
    navbar: "Methods"           # label shown in the navbar dropdown; omit to hide from navbar
    desc: >
      Conceptual method pages with formulas, assumptions, and academic references.
    contents:
      - return-models-methods   # slug = filename without .Rmd
      - test-statistics-methods
      - panel-did-methods
      - intraday-methods
      - synthetic-control-methods
      - diagnostics-methods
      - ai-advisor-methods

  - title: "Worked Examples"
    navbar: "Examples"
    desc: >
      End-to-end rendered analyses across financial domains.
    contents:
      - example-earnings-surprise
      - example-merger
      - example-regulatory-shock
```

If some groups have no `navbar:` key, pkgdown automatically appends a "More..." dropdown item. Sections without `navbar:` are visible only on the articles index page.

### Current _pkgdown.yml integration point

The existing `_pkgdown.yml` has a one-entry `articles:` section covering the 18 existing vignettes in `vignettes/` (not `vignettes/articles/`). The new rich articles live in `vignettes/articles/` and should be added as NEW title groups in the `articles:` section alongside the existing groups. The existing navbar `articles:` component points to `articles/gallery.html` — the gallery article for the new worked-examples gallery should be placed at `vignettes/articles/gallery.html` so this link continues to work, or the navbar href should be updated to point to an index page.

### DESCRIPTION impact

None. No new packages. `vignettes/articles/` is pure `.Rbuildignore`d content.

### CI workflow impact

None structural. `pkgdown::build_site_github_pages()` in `.github/workflows/pkgdown.yaml` already renders all vignettes including `vignettes/articles/`. The `setup-r-dependencies` step with `needs: website` installs all Suggests — any new Suggests added for article rendering will be installed automatically.

---

## 2. Math Rendering

### Recommendation: `template.math-rendering: katex`

Add one line to `_pkgdown.yml`:

```yaml
template:
  bootstrap: 5
  math-rendering: katex
```

### Why KaTeX, not MathJax or MathML

pkgdown 2.1.0+ (current: 2.2.1) supports three options under `template.math-rendering`:

| Option | Dependencies | Fidelity | Plotly conflict | Verdict |
|--------|-------------|----------|-----------------|---------|
| `mathml` | zero (default) | Low — browser-native, poor on complex formulas | None | Good fallback, bad for real math exposition |
| `mathjax` | CDN-loaded | High | **YES** — documented conflict (pkgdown#1338): plotly.js loads its own MathJax instance, causing a JavaScript clash | Do not use with plotly articles |
| `katex` | CDN-loaded | High — fast, renders well | None | **Recommended** |

KaTeX does not conflict with plotly. MathJax does. Since plotly is already in DESCRIPTION Imports (always present), and the Methods articles will have both formulas and plots, KaTeX is the only viable high-fidelity option.

### CDN note

KaTeX is CDN-loaded at render time. The existing CI workflow (`pkgdown.yaml`) does not run with strict offline mode — it uses `ubuntu-latest` with network access. This is already the case for the current site build. No change needed.

For the article `.Rmd` files, write formulas in standard LaTeX notation:

```
Inline: $AR_{it} = R_{it} - E[R_{it}]$

Display: $$CAR_i(\tau_1, \tau_2) = \sum_{t=\tau_1}^{\tau_2} AR_{it}$$
```

Pandoc processes these before pkgdown applies KaTeX rendering. No special R package needed.

### DESCRIPTION impact

None. `template.math-rendering` is a pkgdown configuration setting — not an R package dependency.

---

## 3. Table Rendering

### Recommendation: `knitr::kable()` as primary, `DT::datatable()` for interactive result sets

#### knitr::kable() — zero new dependency

`knitr` is already in DESCRIPTION Suggests (required for `VignetteBuilder: knitr`). `knitr::kable()` produces clean HTML tables that render correctly in pkgdown articles. Use `format = "html"` and Bootstrap-class-friendly options:

```r
knitr::kable(
  result_tbl,
  format    = "html",
  digits    = 4,
  caption   = "Cumulative Abnormal Returns",
  col.names = c("Event", "CAR", "t-stat", "p-value")
)
```

For the Methods articles (comparison tables, assumption tables, parameter grids), `kable()` is sufficient and adds zero dependency.

#### DT::datatable() — already in Suggests

`DT` is already in DESCRIPTION Suggests. Use for worked-examples gallery tables where interactivity (sorting, filtering) adds value — e.g., a large cross-event results table. `DT::datatable()` produces an htmlwidget that renders correctly in pkgdown articles (not in `as_is: true` mode, but pkgdown articles use the standard format).

```r
DT::datatable(
  results_tbl,
  options = list(pageLength = 10, scrollX = TRUE),
  rownames = FALSE
)
```

#### What NOT to add

- **gt**: powerful but not in DESCRIPTION and pulls many sub-dependencies. Zero benefit over `kable()` for this use case. Do not add.
- **kableExtra**: not in DESCRIPTION Suggests. Adds Bootstrap-class styling, but pkgdown Bootstrap 5 already provides acceptable table styling via `kable()`. Do not add.
- **flextable**, **huxtable**, **reactable**: not present, not needed. Do not add.

### DESCRIPTION impact

None — both `knitr` (Suggests) and `DT` (Suggests) are already present.

---

## 4. Plot Rendering

### Recommendation: ggplot2 for static (Methods articles), plotly for interactive (worked examples)

Both are already present: `ggplot2` and `plotly` are in DESCRIPTION Imports (always available).

#### ggplot2 static — for Methods articles

Methods/conceptual articles (those explaining formulas and assumptions) should use `ggplot2` for illustrative plots (distribution shapes, timeline diagrams, model comparison plots). Static images embed cleanly, have no JS weight, and have no conflict with KaTeX math rendering.

```r
# In a Methods article chunk:
ggplot(sim_data, aes(x = t, y = ar)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
  theme_minimal()
```

#### plotly interactive — for worked-examples gallery

The worked-examples articles (complete rendered analyses) should use the existing `plot_event_study()`, `plot_car_distribution()`, etc. — which already return plotly widgets. These render correctly in standard pkgdown articles.

#### Plotly + KaTeX co-existence

With `math-rendering: katex` (not `mathjax`), plotly and math formulas coexist without conflict in the same article. This is the reason for the KaTeX recommendation above.

#### htmlwidgets in CI

pkgdown's bundled jQuery/Bootstrap take precedence over widget-bundled versions (since pkgdown 2.0.2). This means the plotly widget's bundled jQuery/Bootstrap are overridden — this has been the behavior since v0.62.0's CI setup, so no new concern. The existing `plot_event_study()` usage in `ai-advisor.Rmd` already demonstrates this works.

### DESCRIPTION impact

None — ggplot2 and plotly are already in Imports.

---

## 5. Citations and Academic References

### Recommendation: shared `refs.bib` + pandoc-citeproc + `resource_files:` trick

#### Mechanism

R Markdown articles use pandoc's built-in citation processing (pandoc-citeproc). No R package needed. The `r-lib/actions/setup-pandoc` step in `.github/workflows/pkgdown.yaml` already installs Pandoc including citeproc.

#### File layout

Place a single shared bibliography file at `vignettes/articles/refs.bib`. Each article references it with a relative path:

```yaml
---
title: "Return Models — Methods and Assumptions"
bibliography: refs.bib
csl: refs.csl
link-citations: true
resource_files:
  - refs.csl
output: rmarkdown::html_vignette
---
```

In the body, cite with `[@MacKinlay1997]` syntax. The references section renders automatically at the bottom of the article.

#### The resource_files: trick (mandatory for pkgdown)

pkgdown uses a different mechanism than standard `rmarkdown::render()` to determine which resource files to copy alongside an article. The `.csl` file is NOT copied by default, causing `pandoc-citeproc` to fail silently or error. **You must declare it explicitly in `resource_files:`** in the YAML front matter. This is a documented pkgdown quirk — not a bug, but easy to miss.

The `.bib` file does NOT need `resource_files:` — only the `.csl` file does.

#### Single .bib file (mandatory)

A known pkgdown issue causes pandoc-citeproc error 83 when the `bibliography:` YAML field lists multiple `.bib` files. Use a single concatenated `refs.bib` for all articles. If the bibliography grows large, concatenation is still safer than splitting.

#### CSL choice

Use an economics/finance-appropriate CSL file. Recommended: `apa.csl` (widely recognized) or `chicago-author-date.csl` (standard in finance). Download from the [Zotero CSL repository](https://www.zotero.org/styles) and place at `vignettes/articles/refs.csl`. Add `^vignettes/articles` to `.Rbuildignore` (already handled by `use_article()`) — the `.csl` file is excluded from the tarball automatically.

#### Alternatives NOT recommended

- **Rdpack**: designed for generating Rd documentation cross-references, not for Rmd article prose citations. Heavyweight for this use. Do not add.
- **citr** (RStudio addin): build-time irrelevant, only for interactive editing. Do not add.
- **Inline references** (hard-coded text): unmaintainable at scale across 7+ Methods articles. Do not use.

### DESCRIPTION impact

None — pandoc-citeproc is part of Pandoc (installed by `setup-pandoc@v2`), not an R package.

---

## 6. Build Tooling and Caching

### Chunk caching: use knitr cache sparingly

knitr supports `cache = TRUE` per chunk. For slow computations in worked-examples (e.g., simulation with many replications), cache to `vignettes/articles/cache/` to avoid re-running on every pkgdown build:

```r
knitr::opts_chunk$set(cache = TRUE, cache.path = "cache/")
```

Add `^vignettes/articles/cache` to `.Rbuildignore` (already covered by the `^vignettes/articles` pattern). Cache invalidates automatically when chunk code changes.

**Caution:** do not cache chunks that produce htmlwidgets (plotly) — widgets contain session-dependent JS that does not serialize well to the knitr cache. Cache only pure-R computation chunks, not plot output chunks.

### build_articles() lazy mode

`pkgdown::build_articles(lazy = TRUE)` only re-renders articles whose source `.Rmd` is newer than the output `.html`. This is already the behavior in local development. For CI, `build_site_github_pages()` does a full build — acceptable since CI runs on push-to-main only.

### set.seed() discipline

All stochastic chunks (simulations, bootstrap) must call `set.seed()` at the start of the article's setup chunk to ensure reproducible output across CI builds. This is the existing pattern in `ai-advisor.Rmd`.

### No new R packages for build tooling

Do not add `pkgdown.offline`, `targets`, `tarchetypes`, or any other build-orchestration package. The existing `pkgdown::build_site_github_pages()` call in CI is sufficient.

### DESCRIPTION impact

None.

---

## Complete _pkgdown.yml delta

The minimal changes to the existing `_pkgdown.yml` to support the new articles:

```yaml
# Add to the existing template: block:
template:
  bootstrap: 5
  math-rendering: katex    # ADD THIS LINE

# Existing navbar components stay unchanged.
# Add new groups to the articles: section alongside existing groups:

articles:
  # ... existing 8 groups remain unchanged ...

  - title: "Learn / Methods"
    navbar: "Methods"
    desc: >
      Conceptual method pages with formulas, assumptions, when-to-use guidance,
      and academic references.
    contents:
      - return-models-methods
      - test-statistics-methods
      - panel-did-methods
      - intraday-methods
      - synthetic-control-methods
      - diagnostics-methods
      - ai-advisor-methods

  - title: "Worked Examples"
    navbar: "Examples"
    desc: >
      End-to-end rendered event study analyses across financial domains.
    contents:
      - example-earnings-surprise
      - example-merger
      - example-regulatory-shock
      - example-intraday-hft
      - example-panel-policy
```

The existing navbar already has `articles:` pointing to `articles/gallery.html`. The gallery article should be updated or a new gallery index article created in `vignettes/articles/gallery.Rmd` to serve as the landing page for the examples section. The navbar component `href: articles/gallery.html` remains valid.

---

## Recommended Stack Summary

| Capability | Tool | Version | DESCRIPTION field | Notes |
|------------|------|---------|-------------------|-------|
| Articles mechanism | `vignettes/articles/` + pkgdown | 2.2.1 | none | `.Rbuildignore`d automatically |
| Math rendering | `template.math-rendering: katex` | pkgdown 2.2.1 | none | _pkgdown.yml config only; CDN |
| Simple tables | `knitr::kable()` | knitr ≥ 1.50 | already Suggests | Zero new dep |
| Interactive tables | `DT::datatable()` | DT current | already Suggests | For result exploration |
| Static plots | `ggplot2` | current | already Imports | For Methods articles |
| Interactive plots | `plotly` via existing fns | 4.11.0 | already Imports | For worked examples |
| Citations | pandoc-citeproc + `.bib` | bundled with Pandoc | none | No R package needed |
| Build caching | `knitr cache = TRUE` per chunk | knitr | none | For slow sim chunks only |

**New DESCRIPTION Imports additions:** none  
**New DESCRIPTION Suggests additions:** none  
**New _pkgdown.yml changes:** one line (`math-rendering: katex`) + new article groups  
**New .Rbuildignore additions:** `^vignettes/articles$` (added automatically by `usethis::use_article()`)

---

## What NOT to Add

| Avoid | Why | Use Instead |
|-------|-----|-------------|
| `template.math-rendering: mathjax` | Conflicts with plotly.js (pkgdown#1338 — documented JS clash) | `katex` |
| `template.math-rendering: mathml` (as sole option) | Acceptable for simple inline formulas but poor fidelity for multi-line display equations (summations, matrices) | `katex` |
| `gt` package | Not in DESCRIPTION; pulls many sub-deps; no benefit over `kable()` for this use case | `knitr::kable()` |
| `kableExtra` package | Not in DESCRIPTION Suggests; Bootstrap 5 styling sufficient via `kable()` | `knitr::kable()` |
| `Rdpack` | Designed for Rd cross-references, not Rmd prose citations | pandoc-citeproc + `.bib` |
| Multiple `.bib` files in `bibliography:` | Known pkgdown + pandoc-citeproc bug: error 83 when both present | Single concatenated `refs.bib` |
| CSL file without `resource_files:` | pkgdown does not copy `.csl` at the right time; pandoc-citeproc fails | Always declare in `resource_files:` |
| `pkgdown.offline` | Unnecessary — CI has network; KaTeX CDN is only external dep | Nothing (CDN is fine) |
| `quarto` for new articles | Rough edges documented in pkgdown 2.1.0 release notes; adds Quarto installation requirement to CI | Stick with `.Rmd` |
| `cache = TRUE` on plotly/htmlwidget chunks | Widgets don't serialize reliably to knitr cache | Cache computation chunks only; render plots fresh |
| Moving new rich articles into `vignettes/` (CRAN vignettes) | Bloats tarball; slows `R CMD check`; risks NOTE on long-running vignettes | Keep in `vignettes/articles/` |
| Adding any new package to DESCRIPTION Imports | Would force the dep on every user; incompatible with CRAN cleanliness goal | All new docs tooling stays in Suggests or is zero-dep |

---

## Alternatives Considered

| Category | Recommended | Alternative | Why Not |
|----------|-------------|-------------|---------|
| Math rendering | `katex` | `mathjax` | Conflicts with plotly; no advantage over KaTeX for this content |
| Math rendering | `katex` | `mathml` | Poor fidelity for display equations; acceptable only as a fallback |
| Simple tables | `knitr::kable()` | `gt` | New dependency; heavier; no rendering advantage in pkgdown |
| Interactive tables | `DT::datatable()` | `reactable` | Not in DESCRIPTION; `DT` already present |
| Citations | pandoc-citeproc + `.bib` | `Rdpack` | Wrong tool for Rmd articles; designed for Rd |
| Articles format | `.Rmd` | `.qmd` (Quarto) | Rough edges in pkgdown 2.1.0; adds Quarto install dep to CI |

---

## Version Compatibility

| Package | Version in DESCRIPTION | Compatible With | Notes |
|---------|----------------------|-----------------|-------|
| pkgdown | any::pkgdown in CI | 2.2.1 current | `math-rendering: katex` requires ≥ 2.1.0 |
| knitr | Suggests, ≥ 1.43 implied | 1.51+ (2025) | `kable()` stable across versions |
| DT | Suggests | current (0.33+) | htmlwidget; Bootstrap 5 compatible |
| plotly | Imports | 4.11.0 (2025) | KaTeX co-existence verified |
| rmarkdown | Suggests | current | pandoc-citeproc via bundled Pandoc |

---

## Sources

- [pkgdown: build_articles reference](https://pkgdown.r-lib.org/reference/build_articles.html) — articles mechanism, vignettes/articles/, navbar: field, htmlwidgets limitation with as_is:true (MEDIUM confidence)
- [pkgdown: Customise your site](https://pkgdown.r-lib.org/articles/customise.html) — `template.math-rendering` options (mathml/katex/mathjax), _pkgdown.yml syntax (MEDIUM confidence)
- [pkgdown 2.1.0 release blog](https://tidyverse.org/blog/2024/07/pkgdown-2-1-0/) — CDN elimination, math-rendering introduction, version confirmed (MEDIUM confidence)
- [pkgdown NEWS](https://cran.r-project.org/web/packages/pkgdown/news/news.html) — current version 2.2.1 confirmed (MEDIUM confidence)
- [pkgdown#1338: plotly and MathJax conflict](https://github.com/r-lib/pkgdown/issues/1338) — documented plotly+MathJax JS clash; KaTeX avoids it (MEDIUM confidence)
- [usethis: use_vignette/use_article](https://usethis.r-lib.org/reference/use_vignette.html) — use_article() adds `vignettes/articles` to .Rbuildignore automatically (MEDIUM confidence)
- [svPkgdown bibliography example](https://www.sciviews.org/svPkgdown/articles/test/bibliography.html) — `resource_files:` trick for CSL in pkgdown (MEDIUM confidence)
- [R Packages (2e): Vignettes](https://r-pkgs.org/vignettes.html) — articles vs vignettes distinction (MEDIUM confidence)

---
*Stack research for: pkgdown rich-article documentation (v0.63.0 Documentation Depth milestone)*
*Researched: 2026-09-05*
