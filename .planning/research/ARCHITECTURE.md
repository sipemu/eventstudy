# Architecture Research

**Domain:** pkgdown rich-docs integration for an existing CRAN R package
**Researched:** 2026-09-05
**Confidence:** HIGH (pkgdown mechanics confirmed against official source + usethis source code; CI patterns confirmed against existing workflow)

---

## 1. ARTICLE LAYOUT — vignettes/articles/ vs vignettes/

### The canonical site-only pattern

pkgdown treats `vignettes/articles/` as the standard subdirectory for site-only content. This is not a convention invented here — `usethis::use_article()` creates files there by default and automatically adds `vignettes/articles` to `.Rbuildignore`. It is the officially supported pattern.

**What `build_articles()` does with this directory:**

- Renders every `.Rmd` (and `.qmd`) under `vignettes/` recursively, including subdirectories.
- Files in `vignettes/articles/` are **automatically unnested** into `articles/` in the output. `vignettes/articles/methods-return-models.Rmd` becomes `articles/methods-return-models.html` — NOT `articles/articles/methods-return-models.html`. pkgdown generates automatic redirects.
- Files starting with `_` are skipped (child/shared document pattern — useful for a shared `_setup.Rmd` included via `child=` chunks).
- Files in `vignettes/tutorials/` are routed to `build_tutorials()` instead — avoid that name.

**What ships to the CRAN tarball:**

`R CMD build` includes the contents of `vignettes/` by default. To exclude `vignettes/articles/` from the tarball, that subdirectory must be listed in `.Rbuildignore`. The correct regex pattern is:

```
^vignettes/articles$
```

This is a regex anchored at the package root. It excludes the `vignettes/articles/` directory and everything inside it. The `data-raw` line already in `.Rbuildignore` uses `^data-raw$` without trailing slash — follow the same convention.

**YAML header for site-only articles:**

Site-only articles must NOT contain a `vignette:` YAML block. The `vignette:` block (with `%\VignetteIndexEntry{}` etc.) is what makes R's build system treat a file as a vignette. Articles in `vignettes/articles/` have no `VignetteBuilder` interaction. The correct header is:

```yaml
---
title: "Methods: Return Models"
output: rmarkdown::html_vignette
---
```

Or, with TOC:

```yaml
---
title: "Methods: Return Models"
output:
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 3
---
```

**Contrast with existing 18 CRAN vignettes** (in `vignettes/*.Rmd`): they contain the full `vignette:` YAML block and ship in the tarball. They are NOT touched by this milestone.

### Resulting directory layout

```
vignettes/
├── introduction.Rmd          <- CRAN, unchanged
├── ai-advisor.Rmd            <- CRAN, unchanged
├── ... (16 more, all CRAN)
└── articles/                 <- NEW, .Rbuildignore'd, site-only
    ├── _setup.Rmd            <- shared child doc (skipped by pkgdown as standalone)
    ├── methods-return-models.Rmd
    ├── methods-test-statistics.Rmd
    ├── methods-panel-did.Rmd
    ├── methods-intraday.Rmd
    ├── methods-synthetic-control.Rmd
    ├── methods-diagnostics.Rmd
    ├── methods-ai-advisor.Rmd
    ├── gallery-index.Rmd
    ├── gallery-earnings-surprises.Rmd
    ├── gallery-ma-announcements.Rmd
    └── gallery-regulatory-shocks.Rmd

data-raw/
├── dieselgate.R              <- existing provenance script
├── earnings-surprises.R      <- NEW provenance script
└── ...

data/
├── dieselgate.rda            <- existing, LazyData: true, documented in R/
├── earnings_surprises.rda    <- NEW (if shipped as data() dataset)
└── ...

R/
├── data-dieselgate.R         <- existing roxygen doc
├── data-earnings-surprises.R <- NEW roxygen doc (if dataset shipped)
└── ...
```

---

## 2. NAVBAR / MENU EXTENSION

### How pkgdown's navbar system works

The `_pkgdown.yml` navbar has two independent layers:

1. **`navbar: structure:`** — controls the ordering of named slots in the left/right bars.
2. **`navbar: components:`** — defines each named slot's content (text, href, or dropdown menu).

The `articles:` auto-component pkgdown generates from the `articles:` section can be fully overridden by redefining it in `navbar: components:`. New distinct menus require new component names added to both `structure:` and `components:`.

### Current navbar (existing, do not break)

```yaml
navbar:
  structure:
    left: [get-started, reference, articles]
    right: [search, github]
  components:
    get-started:
      text: "Get Started"
      href: articles/introduction.html
    reference:
      text: "Reference"
      href: reference/index.html
    articles:
      text: "Articles"
      href: articles/gallery.html
    github:
      icon: fab fa-github fa-lg
      href: https://github.com/sipemu/eventstudy
      aria-label: GitHub
```

The existing `articles` component is a flat link to the gallery page.

### Target navbar extension

Add `learn` and `gallery` as new slots alongside the existing ones.

```yaml
navbar:
  structure:
    left: [get-started, reference, articles, learn, gallery]
    right: [search, github]
  components:
    get-started:
      text: "Get Started"
      href: articles/introduction.html
    reference:
      text: "Reference"
      href: reference/index.html
    articles:
      text: "Articles"
      href: articles/gallery.html
    learn:
      text: "Learn"
      menu:
        - text: "Return Models"
          href: articles/methods-return-models.html
        - text: "Test Statistics"
          href: articles/methods-test-statistics.html
        - text: "Panel DiD"
          href: articles/methods-panel-did.html
        - text: "Intraday"
          href: articles/methods-intraday.html
        - text: "Synthetic Control"
          href: articles/methods-synthetic-control.html
        - text: "-------"
        - text: "Diagnostics"
          href: articles/methods-diagnostics.html
        - text: "AI Advisor"
          href: articles/methods-ai-advisor.html
    gallery:
      text: "Gallery"
      menu:
        - text: "Earnings Surprises"
          href: articles/gallery-earnings-surprises.html
        - text: "M&A Announcements"
          href: articles/gallery-ma-announcements.html
        - text: "Regulatory Shocks"
          href: articles/gallery-regulatory-shocks.html
        - text: "-------"
        - text: "All Examples"
          href: articles/gallery-index.html
    github:
      icon: fab fa-github fa-lg
      href: https://github.com/sipemu/eventstudy
      aria-label: GitHub
```

### articles: section additions for new content

New articles must be registered in the `articles:` section to appear on `articles/index.html`. Append two new sections after the existing ones:

```yaml
articles:
  # ... all existing sections unchanged ...

  - title: "Learn: Methods"
    desc: >
      Conceptual deep-dives with statistical exposition, assumptions,
      when-to-use guidance, and academic references for every method family.
    contents:
      - methods-return-models
      - methods-test-statistics
      - methods-panel-did
      - methods-intraday
      - methods-synthetic-control
      - methods-diagnostics
      - methods-ai-advisor

  - title: "Gallery"
    desc: >
      End-to-end worked examples across domains, each rendered with
      real curated data.
    contents:
      - gallery-index
      - gallery-earnings-surprises
      - gallery-ma-announcements
      - gallery-regulatory-shocks
```

Content names are the filenames without `.Rmd` extension and without the `articles/` prefix. Since pkgdown unnests `vignettes/articles/methods-return-models.Rmd` → `articles/methods-return-models.html`, the content name is simply `methods-return-models`.

---

## 3. DATASET PLACEMENT DECISION FRAMEWORK

### The dieselgate precedent (confirmed from source)

`dieselgate` is shipped as a documented `data()` dataset:

- **Location:** `data/dieselgate.rda` (bzip2-compressed, 9 265 bytes)
- **Provenance:** `data-raw/dieselgate.R` (network-fetching script, `.Rbuildignore`'d via `^data-raw$`)
- **Roxygen doc:** `R/data-dieselgate.R` with `@docType data`, `@keywords datasets`, `@name dieselgate`, `@usage data(dieselgate)`, full `@format` and `@source`
- **Access:** `data(dieselgate)` or lazy-loaded automatically (`LazyData: true` in DESCRIPTION)
- **CRAN tarball:** included; at 9 KB it is negligible
- **Reference index:** listed via `has_keyword("datasets")` in `_pkgdown.yml`

### Decision framework per dataset

| Criterion | Ship as data() dataset | Site-only in data-raw/ |
|-----------|------------------------|------------------------|
| Size | < ~500 KB compressed | > 500 KB |
| Usage scope | CRAN vignettes, examples, tests, or gallery | Gallery articles only |
| CRAN check | Must pass R CMD check | Not subject to CRAN checks |
| User access | data() works in installed package | Unavailable outside build |
| Required files | data/*.rda + R/data-*.R + data-raw/*.R | data-raw/*.rds only |
| .Rbuildignore | Not needed for data/ | Required entry |

**Site-only dataset loading pattern:**

pkgdown's `build_articles()` sets the working directory to the package root during article rendering. Articles can therefore load site-only data with:

```r
# In a vignettes/articles/ article
earnings <- readRDS("data-raw/earnings-surprises.rds")
```

`data-raw/` is already `.Rbuildignore`'d (`^data-raw$` is in `.Rbuildignore`). Any `.rds` files there are excluded from the CRAN tarball automatically. An alternative is to place them inside `vignettes/articles/data/` — that directory is already excluded because the whole `vignettes/articles/` subtree is in `.Rbuildignore`.

**Recommended decision per new dataset (all estimated < 200 KB compressed):**

Ship all three new gallery datasets as documented `data()` datasets, following the dieselgate pattern. Rationale: they are small, they benefit from `has_keyword("datasets")` Reference index exposure, and users can load them interactively from an installed package to reproduce gallery analyses.

---

## 4. OFFLINE EXECUTION AND CI INTEGRATION

### Deterministic rendering requirements

- Use only `data()` datasets or local file paths — no network calls in evaluated chunks.
- Call `set.seed()` in every article's knitr setup chunk.
- Any chunk requiring a network call or API key: set `eval = FALSE` and show output as a static verbatim block. This is already established practice in `vignettes/ai-advisor.Rmd`.
- For the AI Advisor methods article: use `Sys.setenv(EVENTSTUDY_NO_NETWORK = "1")` in the setup chunk, mirroring the existing vignette.

### Shared setup chunk

Create `vignettes/articles/_setup.Rmd` (underscore prefix — pkgdown skips it as a standalone article):

```r
# vignettes/articles/_setup.Rmd
```{r, include=FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 8,
  fig.height = 5,
  out.width  = "100%"
)
set.seed(42)
library(EventStudy)
```
```

Include it in every article:

```yaml
---
title: "Methods: Return Models"
output: rmarkdown::html_vignette
---
```{r setup, child="_setup.Rmd"}
```
```

### CI integration

The existing `pkgdown.yaml` workflow requires **no changes** to the workflow file itself. The build discovers `vignettes/articles/` automatically. The only DESCRIPTION change needed is adding `Config/Needs/website`:

```
Config/Needs/website: pkgdown
```

Extend this field as new article-only dependencies are identified (e.g., `gt`, `flextable`). Packages already in `Suggests` (`DT`, `zoo`, etc.) are installed automatically.

**Lazy rebuild behavior:** `build_site_github_pages()` uses lazy article building by default — only rebuilds articles whose source is newer than the cached HTML. On a fresh CI checkout there is no cache, so all articles rebuild on every push. This is correct and expected.

Do not add `docs/` to CI cache — stale HTML could be deployed incorrectly.

---

## 5. BUILD ORDER AND INTEGRATION POINTS

### New vs Modified files

| File | Status | Notes |
|------|--------|-------|
| `vignettes/articles/` | NEW directory | |
| `vignettes/articles/_setup.Rmd` | NEW | Shared knitr opts + set.seed |
| `vignettes/articles/methods-*.Rmd` (x7) | NEW | Methods articles |
| `vignettes/articles/gallery-*.Rmd` (x4) | NEW | Gallery articles including index |
| `data-raw/earnings-surprises.R` | NEW | Dataset provenance script |
| `data-raw/ma-announcements.R` | NEW | Dataset provenance script |
| `data-raw/regulatory-shocks.R` | NEW | Dataset provenance script |
| `data/earnings_surprises.rda` | NEW | Bundled dataset |
| `data/ma_announcements.rda` | NEW | Bundled dataset |
| `data/regulatory_shocks.rda` | NEW | Bundled dataset |
| `R/data-earnings-surprises.R` | NEW | Roxygen doc (@docType data) |
| `R/data-ma-announcements.R` | NEW | Roxygen doc |
| `R/data-regulatory-shocks.R` | NEW | Roxygen doc |
| `.Rbuildignore` | MODIFIED | Add `^vignettes/articles$` |
| `_pkgdown.yml` | MODIFIED | Add learn/gallery navbar; extend articles: sections |
| `DESCRIPTION` | MODIFIED | Add Config/Needs/website; bump version 0.63.0 |
| `NEWS.md` | MODIFIED | v0.63.0 entry |
| `cran-comments.md` | MODIFIED | CRAN submission notes |
| `.github/workflows/pkgdown.yaml` | UNCHANGED | No changes needed |
| `vignettes/*.Rmd` (existing 18) | UNCHANGED | CRAN vignettes untouched |

### Suggested build order

```
Phase A — Scaffold (gate)
  A1. Add ^vignettes/articles$ to .Rbuildignore
  A2. Create vignettes/articles/ + _setup.Rmd
  A3. Add Config/Needs/website to DESCRIPTION
  A4. Verify: R CMD check passes, no new NOTEs

Phase B — Datasets (articles depend on data)
  B1. Write data-raw/earnings-surprises.R + run -> data/earnings_surprises.rda
  B2. Write R/data-earnings-surprises.R (roxygen)
  B3. Repeat for M&A and regulatory shocks
  B4. devtools::document() -> NAMESPACE + man/
  B5. Verify data() loads each dataset

Phase C — Methods articles (any order, independent)
  C1-C7. Write vignettes/articles/methods-*.Rmd
  Build: pkgdown::build_article("methods-return-models") per article

Phase D — Gallery articles (depend on Phase B datasets)
  D1-D4. Write vignettes/articles/gallery-*.Rmd
  Build: pkgdown::build_article("gallery-earnings-surprises") per article

Phase E — _pkgdown.yml and navbar
  E1. Add learn: + gallery: navbar components + structure slots
  E2. Add Learn: Methods and Gallery sections to articles:
  E3. pkgdown::build_site() locally — verify menus, no broken links
  E4. Verify all 18 existing CRAN articles still accessible

Phase F — Final verification
  F1. R CMD check (0 new NOTEs or WARNINGs)
  F2. pkgdown::build_site_github_pages() locally
  F3. Push to main -> CI pkgdown build -> verify deploy
```

---

## Anti-Patterns to Avoid

### Anti-Pattern 1: Adding vignette: block to site-only articles

**What people do:** Copy a CRAN vignette header including `%\VignetteIndexEntry{}` into `vignettes/articles/` files.

**Why it's wrong:** If `.Rbuildignore` ever slips, R's build system tries to build them as CRAN vignettes — network-dependent or heavy articles then fail `R CMD check`.

**Do this instead:** Use a clean header with no `vignette:` section.

### Anti-Pattern 2: Forgetting both navbar: structure: and navbar: components: updates

**What people do:** Define a new `learn:` component but forget to add `learn` to `navbar: structure: left:`.

**Why it's wrong:** The component is defined but never rendered. It silently disappears from the navbar.

**Do this instead:** Always update both `navbar: structure: left:` and `navbar: components:` in the same commit.

### Anti-Pattern 3: Network calls in evaluated chunks

**What people do:** Call `download_stock_data()` with `eval = TRUE` in an article chunk.

**Why it's wrong:** CI has no guarantee of network access to external financial APIs; builds become flaky.

**Do this instead:** Use bundled `data()` datasets. Show download code with `eval = FALSE`.

### Anti-Pattern 4: Using vignettes/tutorials/ as the subdirectory name

**What people do:** Create `vignettes/tutorials/` thinking it equals `vignettes/articles/`.

**Why it's wrong:** pkgdown routes `vignettes/tutorials/` to `build_tutorials()` — a different rendering path (learnr-style). Articles land in `tutorials/` on the site, not `articles/`, breaking all nav hrefs.

**Do this instead:** Use only `vignettes/articles/`.

### Anti-Pattern 5: Shipping large datasets in data/

**What people do:** Bundle a 10 MB dataset via `usethis::use_data()` for convenience.

**Why it's wrong:** `data/` ships in the CRAN tarball. CRAN has a ~5 MB total size limit.

**Do this instead:** For datasets > ~500 KB compressed, store as `.rds` in `data-raw/` and load with `readRDS("data-raw/bigdata.rds")` in the article.

---

## Sources

- pkgdown `build_articles()` reference: [pkgdown.r-lib.org/reference/build_articles.html](https://pkgdown.r-lib.org/reference/build_articles.html) (MEDIUM — official)
- pkgdown customise article: [pkgdown.r-lib.org/articles/customise.html](https://pkgdown.r-lib.org/articles/customise.html) (MEDIUM — official)
- usethis `use_article()` source: [rdrr.io/cran/usethis/src/R/vignette.R](https://rdrr.io/cran/usethis/src/R/vignette.R) — confirms `vignettes/articles/` directory and exact `.Rbuildignore` entry (HIGH — source code)
- pkgdown articles/vignettes deep-wiki: [deepwiki.com/r-lib/pkgdown/4.2-articles-and-vignettes](https://deepwiki.com/r-lib/pkgdown/4.2-articles-and-vignettes) — confirms auto-unnesting of `vignettes/articles/` (MEDIUM)
- Config/Needs/website: [r-pkgs.org/dependencies-in-practice.html](https://r-pkgs.org/dependencies-in-practice.html) (HIGH — Hadley Wickham canonical)
- Data placement: [r-pkgs.org/data.html](https://r-pkgs.org/data.html) (HIGH — canonical)
- Dieselgate precedent: inspected directly in `R/data-dieselgate.R` and `data-raw/dieselgate.R` (HIGH — primary source)
- Existing CI workflow: inspected directly in `.github/workflows/pkgdown.yaml` (HIGH — primary source)

---

*Architecture research for: EventStudy v0.63.0 Documentation Depth — Methods & Worked Examples*
*Researched: 2026-09-05*
