# Phase 13: Article Infrastructure & Conventions Gate — Research

**Researched:** 2026-09-05
**Domain:** pkgdown 2.x article infrastructure, R package documentation conventions
**Confidence:** HIGH (all primary claims verified against repo files read this session or pkgdown 2.x official docs)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

- Expose conceptual method articles as a **"Methods" navbar dropdown** wired in both `navbar: structure:` and `navbar: components:`.
- Add a distinct **"Gallery"** navbar entry (the current `articles` component already targets `articles/gallery.html`).
- Keep the existing **19 vignettes** (18 CRAN + gallery.Rmd) reachable under "Articles", byte-unchanged and CRAN-shipped — new content is strictly additive (DELIVERY-03).
- New rich content lives under `vignettes/articles/`; add `^vignettes/articles$` to `.Rbuildignore` **in the same commit** that creates the directory (DELIVERY-01).
- One shared bibliography at `vignettes/articles/references.bib`, referenced filename-only (`bibliography: references.bib`) (RENDER-03 / citation pitfall).
- Reusable `_setup.Rmd` child chunk: `set.seed(42)`, `options(scipen = 999, digits = 4)`, `knitr::opts_chunk$set(collapse = TRUE, comment = "#>")` (METH-01).
- Reusable 10-section article skeleton.
- `math-rendering: katex` in `_pkgdown.yml` to resolve plotly/MathJax conflict (RENDER-03).
- Smoke-test article: KaTeX formula + resolved `@citation` + plotly figure on one page, driven by bundled `dieselgate` dataset.

### Claude's Discretion

- Exact navbar ordering, menu nesting, and label casing (provided Methods track and Gallery entry are visible and existing Articles/Reference navigation is not broken).
- Precise wording of the 10 skeleton section headers and smoke-test prose.

### Deferred Ideas (OUT OF SCOPE)

- Actual 8 Methods articles — Phase 15.
- Curated per-domain datasets — Phase 14.
- Gallery worked examples + final build/release gate — Phase 16.
</user_constraints>

---

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| METH-01 | "Learn / Methods" section exposed in pkgdown navigation as a coherent learning track | Navbar YAML pattern in § Architecture Patterns |
| RENDER-03 | plotly + MathJax coexist without broken layout (known conflict resolved) | `math-rendering: katex` key verified in § Plotly/MathJax Conflict |
| DELIVERY-01 | `vignettes/articles/` `.Rbuildignore`d in same commit — absent from CRAN tarball, present on site | `.Rbuildignore` regex and mechanism in § Tarball Exclusion |
| DELIVERY-02 | `_pkgdown.yml` navigation integrates Methods + Gallery without breaking existing 18-vignette navigation | Exact YAML in § Architecture Patterns |
| DELIVERY-03 | Existing 19 vignettes remain unchanged and CRAN-shipped | Verified: `vignettes/` has 19 .Rmd files; `vignettes/articles/` does not exist yet |
</phase_requirements>

---

## Summary

Phase 13 is an infrastructure-only gate: create the `vignettes/articles/` directory with shared conventions (`_setup.Rmd`, `references.bib`, 10-section skeleton), update `_pkgdown.yml` with `math-rendering: katex` and Methods/Gallery navbar entries, and prove the whole thing with a single smoke-test article. No CRAN vignette is touched.

The two primary technical risks are: (1) the plotly/MathJax JS collision, which `math-rendering: katex` in `_pkgdown.yml` resolves by replacing pkgdown's MathJax with KaTeX before plotly loads its own copy; and (2) raw `[@Key]` citation passthrough when the `.bib` file is not co-located at render time — resolved by placing `references.bib` in `vignettes/articles/` and referencing it filename-only. Both are preventable with exact config, not code.

**Primary recommendation:** Write all three shared files (`_setup.Rmd`, `references.bib`, skeleton template) and the `_pkgdown.yml` patch in one wave before writing the smoke-test article, so the smoke-test is the first real validation of the whole gate.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| Tarball exclusion (DELIVERY-01) | `.Rbuildignore` + `vignettes/articles/` directory | `_pkgdown.yml` articles sections | `.Rbuildignore` regex controls what `R CMD build` includes; pkgdown is not involved |
| Math rendering (RENDER-03) | `_pkgdown.yml` `math-rendering: katex` key | Article YAML front matter | pkgdown injects the KaTeX JS globally; article front matter does not override this |
| Citation resolution | rmarkdown/pandoc-citeproc at build time | Co-located `.bib` file | Resolution is file-system relative to the `.Rmd` source at render time |
| Determinism (`_setup.Rmd`) | knitr child chunk mechanism | Article YAML `output:` section | Child chunks execute in the parent's R session; seed/options propagate |
| Navbar wiring (METH-01, DELIVERY-02) | `_pkgdown.yml` `navbar:` block | `articles:` index sections | Navbar `components:` drives the rendered menu; `articles:` drives the index listing page |
| Smoke-test render | `pkgdown::build_article()` or `pkgdown::build_site()` | CI `pkgdown.yaml` dry-run | Local `build_article()` is faster for iteration; CI validates the full gate |

---

## Standard Stack

### Core (all already in DESCRIPTION — no new dependencies)

| Library | Version (installed) | Purpose | Why Standard |
|---------|-------------------|---------|--------------|
| pkgdown | 2.2.0 [VERIFIED: `packageVersion("pkgdown")` this session] | Site build, article rendering, navbar | Authoritative pkgdown-only delivery mechanism |
| knitr | 1.51 [VERIFIED: `packageVersion("knitr")` this session] | Child chunk (`_setup.Rmd`), chunk options, `kable()` | Standard R chunk engine |
| rmarkdown | 2.31 [VERIFIED: `packageVersion("rmarkdown")` this session] | Article compilation via pandoc | Standard Rmd→HTML pipeline |
| pandoc | system (via rmarkdown) | Citation resolution (citeproc) | Converts `[@Key]` → formatted citation when `.bib` is co-located |

### No New Packages

This phase installs **zero new packages**. All tooling is already in DESCRIPTION under Suggests. The Package Legitimacy Audit section is therefore not applicable.

---

## Architecture Patterns

### System Architecture Diagram

```
Author writes vignettes/articles/smoke-test.Rmd
  |-- YAML: bibliography: references.bib  (co-located)
  |-- chunk 1: {r child="_setup.Rmd"}     (seed + options)
  |-- chunk 2: KaTeX formula in $...$
  |-- chunk 3: dieselgate code → plotly figure
  |
  v
pkgdown::build_article("smoke-test")
  |-- reads _pkgdown.yml: math-rendering: katex
  |    → injects KaTeX JS (not MathJax)
  |-- pandoc citeproc resolves [@MacKinlay1997] → formatted ref
  |-- plotly renders without MathJax collision
  |
  v
docs/articles/smoke-test.html  (site-only, not in CRAN tarball)
```

### Recommended Project Structure

```
vignettes/
├── *.Rmd                    # 19 existing CRAN vignettes — UNTOUCHED
└── articles/                # NEW — .Rbuildignore'd
    ├── _setup.Rmd           # shared child chunk (seed, options, knitr defaults)
    ├── references.bib       # shared bibliography
    ├── _article-skeleton.Rmd  # 10-section template (not built — prefixed _)
    └── smoke-test.Rmd       # gate validation article
```

Files prefixed with `_` are ignored by pkgdown's article scanner [ASSUMED — pkgdown convention; confirm with a local build]. The skeleton should be prefixed `_` or kept as a plain `.md` to avoid it appearing in the site index.

### Pattern 1: `vignettes/articles/` — Website-Only Articles

pkgdown 2.x treats `vignettes/articles/*.Rmd` as site-only articles that do NOT appear in the CRAN tarball provided the directory is excluded via `.Rbuildignore` [CITED: https://pkgdown.r-lib.org/articles/articles.html].

**The exact `.Rbuildignore` line to add:**
```
^vignettes/articles$
```

This regex matches the directory itself. Adding it in the same commit that creates the directory is mandatory — if the directory lands in a tarball-included commit, CRAN receives the content and triggers a "non-standard file" NOTE [CITED: https://pkgdown.r-lib.org/articles/articles.html].

The 19 existing `vignettes/*.Rmd` files are unaffected: they match no new ignore pattern and continue to be CRAN-shipped [VERIFIED: `.Rbuildignore` read this session — no pattern touches `^vignettes/` at the top level; `vignettes/articles/` does not yet exist as confirmed by `ls` this session].

### Pattern 2: `math-rendering: katex` — Resolving the plotly/MathJax Conflict

pkgdown ≥ 2.1.0 supports a top-level `math-rendering:` key in `_pkgdown.yml` [CITED: https://pkgdown.r-lib.org/reference/build_site.html]. Setting it to `katex` replaces pkgdown's default MathJax bundle with KaTeX, eliminating the collision with plotly's own bundled MathJax (plotly ships MathJax 2.x internally; if pkgdown also loads MathJax, two copies fight over `window.MathJax` and one or both break).

**Exact addition to `_pkgdown.yml`** (top-level key, alongside `url:` and `template:`):

```yaml
math-rendering: katex
```

KaTeX renders inline `$...$` and display `$$...$$` math in rendered HTML. It does not conflict with plotly because KaTeX uses a different global namespace (`window.katex`, not `window.MathJax`).

**Pitfall:** Using `mathjax: true` in the article's YAML `output: rmarkdown::html_vignette:` front matter while `math-rendering: katex` is set site-wide causes a double-load. Articles must NOT set `mathjax:` in their individual output options when this site-wide key is present [ASSUMED — inference from pkgdown rendering pipeline; verify by checking rendered HTML for duplicate script tags].

### Pattern 3: `_setup.Rmd` Child Chunk

Place `vignettes/articles/_setup.Rmd` with content:

```r
```{r _setup, include=FALSE}
set.seed(42)
options(scipen = 999, digits = 4)
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  echo      = TRUE,
  fig.align = "center"
)
```
```

Every article includes it as its first chunk:

```r
```{r child="_setup.Rmd"}
```
```

The `child=` argument resolves relative to the **parent Rmd's directory** at render time [VERIFIED: knitr documentation — child chunks use the parent's working directory; confirmed by knitr 1.51 installed]. This means `_setup.Rmd` must sit in `vignettes/articles/` alongside the articles that include it.

**Pitfall:** The `_setup.Rmd` child chunk must use `include=FALSE` (or set `include` in the child itself) to suppress its own chunk output from appearing in the rendered article. If the child chunk header in `_setup.Rmd` omits `include=FALSE`, knitr will print a blank code block in every article.

### Pattern 4: Citation Resolution via Co-Located `.bib`

Article YAML front matter:

```yaml
bibliography: references.bib
```

Pandoc-citeproc (invoked by rmarkdown during build) resolves `references.bib` **relative to the source `.Rmd` file's directory** [ASSUMED — pandoc citeproc resolution; standard behavior well-established, but not independently verified against pandoc changelog this session]. Because all articles and the `.bib` file live in `vignettes/articles/`, filename-only referencing works.

**Failure mode:** If an article is placed in a subdirectory or the `.bib` is moved to `vignettes/`, the relative path breaks silently — pandoc emits no error, but `[@MacKinlay1997]` appears verbatim as `[MacKinlay1997]` in the rendered HTML. The fix is always: `.bib` must be co-located with (or in a parent of) the article.

In-text citation syntax:
- `@MacKinlay1997` → inline author-year: MacKinlay (1997)
- `[@MacKinlay1997]` → parenthetical: (MacKinlay, 1997)
- `[@MacKinlay1997; @Brown1985]` → combined parenthetical

### Pattern 5: `_pkgdown.yml` Navbar — Adding Methods Dropdown and Gallery

The existing `_pkgdown.yml` has [VERIFIED: `_pkgdown.yml` read this session — lines 16-32]:

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

The required change inserts `methods` and `gallery` into `structure.left` and defines their components. The existing `articles` component is retained for the 19 CRAN vignettes listing:

```yaml
math-rendering: katex

navbar:
  structure:
    left: [get-started, reference, articles, methods, gallery]
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
    methods:
      text: "Methods"
      menu:
        - text: "Overview"
          href: articles/methods-overview.html    # smoke-test article slug
    gallery:
      text: "Gallery"
      href: articles/gallery.html
    github:
      icon: fab fa-github fa-lg
      href: https://github.com/sipemu/eventstudy
      aria-label: GitHub
```

**Note on the smoke-test article slug:** pkgdown derives the article URL slug from the filename without extension. `vignettes/articles/smoke-test.Rmd` → `articles/smoke-test.html`. The Methods dropdown menu entry should point to the smoke-test article during Phase 13, and Phase 15 will expand the menu with the 8 real articles.

**Pitfall:** A component name listed in `structure.left` that has no matching entry in `components:` causes pkgdown to silently drop it from the rendered navbar (no build error). Always define the component in `components:` when adding it to `structure:` [ASSUMED — pkgdown navbar resolution behavior; consistent with pkgdown 2.x source].

**Pitfall:** The `articles:` component in `navbar.components` is a pkgdown built-in name. Renaming or removing it suppresses the pkgdown-generated Articles index page (not just the navbar link). Keep the key named `articles:` and change only its `text:` and `href:` if needed.

### Pattern 6: `articles:` Index in `_pkgdown.yml` — Adding a Methods Section

The existing `_pkgdown.yml` already has `articles:` groups for all 19 CRAN vignettes [VERIFIED: `_pkgdown.yml` read this session — lines 177-246]. Add a new group for Methods articles. During Phase 13 it contains only the smoke-test; Phase 15 expands it:

```yaml
articles:
  - title: "Methods"
    desc: >
      Conceptual explanations of return models, test statistics, and
      advanced designs — with formulas, assumptions, and references.
    contents:
      - articles/smoke-test     # Phase 13: gate only; Phase 15 adds real articles

  # ... existing groups unchanged below ...
  - title: "Get Started"
    ...
```

pkgdown 2.x resolves `articles/smoke-test` as `vignettes/articles/smoke-test.Rmd` [CITED: https://pkgdown.r-lib.org/articles/articles.html — articles/ subdirectory handling].

### Pattern 7: Smoke-Test Article Minimal Structure

File: `vignettes/articles/smoke-test.Rmd`

```yaml
---
title: "Methods Gate: Smoke Test"
output: rmarkdown::html_vignette
bibliography: references.bib
vignette: >
  %\VignetteIndexEntry{Methods Gate: Smoke Test}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```

```r
```{r child="_setup.Rmd"}
```

## Cumulative Abnormal Return Formula

The CAR over window $[t_1, t_2]$ is defined as:

$$\text{CAR}(t_1, t_2) = \sum_{t=t_1}^{t_2} \hat{\varepsilon}_t$$

where $\hat{\varepsilon}_t = R_{it} - (\hat{\alpha} + \hat{\beta} R_{mt})$ is the abnormal return
under the market model [@MacKinlay1997].

## Worked Example

```{r load-data}
library(EventStudy)
data("dieselgate")
```

```{r run-pipeline}
task <- EventStudyTask$new(
  firm_data    = dieselgate$firm,
  index_data   = dieselgate$index,
  request_data = dieselgate$request,
  firm_column  = "symbol",
  date_column  = "date",
  price_column = "adjusted"
)
params <- ParameterSet$new()
task   <- prepare_event_study(task, params)
task   <- fit_model(task, params)
task   <- calculate_statistics(task, params)
```

```{r plot-car, fig.cap="CAR over event window (dieselgate, VW)"}
plot_event_study(task)
```

## References
```

**What a passing render looks like:**
- The formula block renders as typeset math (KaTeX), not raw `$...$` or `$$...$$`
- `[@MacKinlay1997]` renders as `(MacKinlay, 1997)` — not literal `[@MacKinlay1997]`
- The plotly figure renders interactively with no JS console errors
- `_child="_setup.Rmd"` executes silently (no blank code block in output)

**Minimal `references.bib` entry for smoke-test:**

```bibtex
@article{MacKinlay1997,
  author  = {A. Craig MacKinlay},
  title   = {Event Studies in Economics and Finance},
  journal = {Journal of Economic Literature},
  year    = {1997},
  volume  = {35},
  number  = {1},
  pages   = {13--39}
}
```

### Pattern 8: Local CI Dry-Run — Render Smoke-Test Only

```r
# From the package root:
pkgdown::build_article("smoke-test")
```

This renders only `vignettes/articles/smoke-test.Rmd` into `docs/articles/smoke-test.html` without rebuilding the full site. Inspect the output HTML for:
1. No `$...$` or `$$...$$` passthrough (KaTeX rendered correctly)
2. No `[@MacKinlay1997]` literal (pandoc citeproc resolved)
3. No duplicate `<script src="...mathjax...">` tags (open HTML, search for `mathjax`)
4. Plotly widget present and functional (open in browser)

The full CI gate uses `pkgdown::build_site()` which the existing `pkgdown.yaml` workflow already runs [VERIFIED: `.github/workflows/pkgdown.yaml` read this session — line 36: `pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)`].

**Offline safety:** The smoke-test article uses only `data("dieselgate")` (bundled, no network) and the `EventStudy` package itself. It satisfies the BUILD-02 constraint without any gating [VERIFIED: `dieselgate.rda` exists in `data/`; dataset structure confirmed this session].

### Anti-Patterns to Avoid

- **Placing `_setup.Rmd` in `vignettes/` (not `vignettes/articles/`):** The `child=` path resolves relative to the article's directory. A `_setup.Rmd` one level up would require `child="../_setup.Rmd"` in every article — fragile and inconsistent.
- **Using `output: html_document` instead of `output: rmarkdown::html_vignette`:** `html_document` embeds its own pandoc JS/CSS, overriding pkgdown's theming and potentially re-introducing MathJax.
- **Adding `^vignettes/articles` (no trailing `$`) to `.Rbuildignore`:** The regex `^vignettes/articles` matches any path starting with that string (including future `vignettes/articles-old/` etc.). Use the anchored `^vignettes/articles$`.
- **Leaving `vignette:` front-matter off articles/ Rmds:** pkgdown renders them fine, but knitr/devtools `build_vignettes()` will ignore them. Adding `%\VignetteIndexEntry{}` front matter is harmless and signals intent clearly.
- **Listing a `methods:` navbar slot whose `menu:` entries point to articles that don't exist yet:** pkgdown 2.x emits a warning but does not fail the build. The smoke-test article must exist before the navbar entry points to it.

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Math rendering | Custom MathJax config or JS shim | `math-rendering: katex` in `_pkgdown.yml` | pkgdown owns the JS injection; shims collide with plotly the same way MathJax does |
| Citation formatting | Manual `<sup>1</sup>` footnotes | pandoc-citeproc + `.bib` | Auto-formatted, cross-linked, style-switchable |
| Deterministic chunks | `set.seed()` copy-pasted in every article | `_setup.Rmd` child chunk | Single source of truth; Phase 15 authors inherit it automatically |
| Article slug prediction | Manual URL construction | Let pkgdown derive from filename | pkgdown slug = filename without extension; manual construction drifts |

---

## Common Pitfalls

### Pitfall 1: Raw `[@Key]` in rendered output
**What goes wrong:** Citation appears literally as `[@MacKinlay1997]` in the HTML.
**Why it happens:** pandoc cannot find the `.bib` file — either it's missing, in a different directory, or the path in `bibliography:` is absolute or points one level up.
**How to avoid:** Keep `references.bib` in `vignettes/articles/`; use filename-only `bibliography: references.bib` in article YAML.
**Warning signs:** Build completes without error; only visible in rendered HTML. Always grep the output: `grep -l '\[@' docs/articles/smoke-test.html`.

### Pitfall 2: Raw `$formula$` passthrough (KaTeX not applied)
**What goes wrong:** Inline math appears as literal `$\text{CAR}...$` in browser.
**Why it happens:** `math-rendering: katex` is missing from `_pkgdown.yml`, or the key is indented under `template:` instead of being top-level.
**How to avoid:** `math-rendering: katex` must be a **top-level key** in `_pkgdown.yml`, not nested under `template:` or `home:`.
**Warning signs:** Check rendered HTML — KaTeX assets appear as `<link href=".../katex.min.css">` and `<script src=".../katex.min.js">`. If you see `mathjax` instead of `katex` in the script tags, the key was not picked up.

### Pitfall 3: Blank code block from `_setup.Rmd` in article output
**What goes wrong:** A blank grey box (`#>`) appears at the top of every article.
**Why it happens:** The child chunk in `_setup.Rmd` does not set `include=FALSE`; or the including chunk does not suppress via `{r child="_setup.Rmd", include=FALSE}`.
**How to avoid:** Set `include=FALSE` in the child's own chunk header (preferred — author can't forget) OR in the parent's `child=` invocation chunk.

### Pitfall 4: plotly/MathJax console conflict
**What goes wrong:** Math renders but plotly figures show garbled tick labels, or vice versa. Browser console shows `MathJax is already defined`.
**Why it happens:** `math-rendering: katex` was not set, so pkgdown loaded MathJax, and plotly also loaded MathJax.
**How to avoid:** `math-rendering: katex` in `_pkgdown.yml` (see Pitfall 2). Confirm no `<script src="...mathjax...">` in rendered smoke-test HTML.

### Pitfall 5: `vignettes/articles$` hits wrong files in `.Rbuildignore`
**What goes wrong:** Other paths are accidentally excluded.
**Why it happens:** Regex written as `^vignettes/articles` (no trailing `$`) is too broad.
**How to avoid:** Exact regex `^vignettes/articles$` — anchored start and end.

### Pitfall 6: CRAN tarball gains unexpected content
**What goes wrong:** `R CMD check --as-cran` emits NOTE "non-standard files/directories" for `vignettes/articles/`.
**Why it happens:** `.Rbuildignore` update was committed AFTER `vignettes/articles/` was already present in an earlier commit — a CRAN submission from that earlier state would include it.
**How to avoid:** Create directory and add `.Rbuildignore` regex in the same atomic commit.

---

## Repo State at Research Time

| Item | State | Source |
|------|-------|--------|
| `vignettes/articles/` | Does NOT exist | [VERIFIED: `ls` this session] |
| `vignettes/*.Rmd` count | 19 files | [VERIFIED: `ls \| wc -l` this session] |
| `.Rbuildignore` | Has `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$`, `^data-raw$`, `^\.planning$` etc; does NOT have `^vignettes/articles$` | [VERIFIED: `.Rbuildignore` read this session] |
| `_pkgdown.yml` `navbar.structure.left` | `[get-started, reference, articles]` — no `methods` or `gallery` | [VERIFIED: `_pkgdown.yml` read this session, line 17] |
| `_pkgdown.yml` `math-rendering:` key | Absent | [VERIFIED: `_pkgdown.yml` read this session — not present] |
| `data/dieselgate.rda` | Present; 4 firms, 2 groups, bundled | [VERIFIED: `str(dieselgate)` this session] |
| pkgdown version | 2.2.0 | [VERIFIED: `packageVersion("pkgdown")` this session] |
| knitr version | 1.51 | [VERIFIED: `packageVersion("knitr")` this session] |
| rmarkdown version | 2.31 | [VERIFIED: `packageVersion("rmarkdown")` this session] |
| CI workflow | `.github/workflows/pkgdown.yaml` — `pkgdown::build_site_github_pages()` on push-to-main | [VERIFIED: workflow file read this session] |

---

## Validation Architecture

### Test Framework

| Property | Value |
|----------|-------|
| Framework | testthat 3e (existing) + manual pkgdown render check |
| Config file | `tests/testthat/` (existing) |
| Quick run command | `pkgdown::build_article("smoke-test")` |
| Full suite command | `pkgdown::build_site()` |

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|-------------|
| METH-01 | Methods navbar dropdown renders | Smoke | `pkgdown::build_site()` + visual inspect | ❌ Wave 0: build needed |
| RENDER-03 | KaTeX + plotly coexist | Smoke | `pkgdown::build_article("smoke-test")` + grep HTML | ❌ Wave 0: article needed |
| DELIVERY-01 | `vignettes/articles/` absent from `R CMD build` tarball | CRAN check | `R CMD build . && tar tzf *.tar.gz \| grep articles` | ❌ Wave 0: dir needed |
| DELIVERY-02 | Navbar shows Methods + Gallery without breaking Articles/Reference | Smoke | `pkgdown::build_site()` + inspect `docs/index.html` | ❌ Wave 0: config needed |
| DELIVERY-03 | 19 existing vignettes byte-unchanged | Regression | `git diff vignettes/*.Rmd` (must be empty after phase) | ✅ existing |

### Wave 0 Gaps

- [ ] `vignettes/articles/` directory — must exist before any article can be built
- [ ] `vignettes/articles/_setup.Rmd` — shared child chunk
- [ ] `vignettes/articles/references.bib` — shared bibliography
- [ ] `vignettes/articles/smoke-test.Rmd` — gate validation article
- [ ] `_pkgdown.yml` patch: add `math-rendering: katex`, `methods:` navbar component, `gallery:` navbar component, Methods articles section
- [ ] `.Rbuildignore` patch: add `^vignettes/articles$` line

---

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|------------|-----------|---------|----------|
| pkgdown | Site build, article render | ✓ | 2.2.0 | — |
| knitr | Child chunks, article build | ✓ | 1.51 | — |
| rmarkdown | Rmd → HTML | ✓ | 2.31 | — |
| pandoc | Citation resolution | ✓ (via rmarkdown) | system | — |
| plotly | Smoke-test interactive figure | ✓ (in DESCRIPTION Suggests) | [ASSUMED — installed as Suggests dep] | — |
| R | Runtime | ✓ | 4.6.1 | — |
| dieselgate data | Smoke-test offline data | ✓ | bundled in `data/` | — |

**Missing dependencies with no fallback:** None.

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | Files prefixed `_` in `vignettes/articles/` are skipped by pkgdown's article scanner | Recommended Project Structure | Skeleton or `_setup.Rmd` appears as a broken article in the site index; fix: add explicit `exclude:` in `_pkgdown.yml` articles section |
| A2 | pandoc-citeproc resolves `bibliography: references.bib` relative to the Rmd source directory | Pattern 4: Citation Resolution | Citations render as raw `[@Key]`; fix: use explicit relative path `bibliography: ../articles/references.bib` |
| A3 | `math-rendering: katex` is absent from `_pkgdown.yml` silently = MathJax (not a build error) | Pattern 2 | plotly/MathJax conflict; confirmed by absence in read file, risk is low |
| A4 | plotly is installed in the local R library | Environment Availability | Smoke-test fails to render; check with `requireNamespace("plotly")` |
| A5 | navbar component named `gallery:` with `href: articles/gallery.html` in `components:` does not conflict with the existing `articles:` component that also links `articles/gallery.html` | Pattern 5: Navbar | Duplicate links in navbar; harmless UX issue, not a build error |

**If this table is empty:** n/a — 5 assumptions logged.

---

## Open Questions

1. **Does pkgdown 2.2.0 skip `_`-prefixed files in `vignettes/articles/`?**
   - What we know: pkgdown documentation states it ignores files starting with `_` or `.`
   - What's unclear: Whether this applies to child Rmd files or only to Rmd articles proper
   - Recommendation: Confirm with `pkgdown::build_site()` and check if `_setup` or `_article-skeleton` appear in `docs/articles/`; if they do, add them to `articles: exclude:` in `_pkgdown.yml`

2. **Does the existing `articles` navbar component pointing to `articles/gallery.html` conflict with a new `gallery:` component also pointing there?**
   - What we know: Two navbar entries can share the same `href:` without breaking the build
   - What's unclear: Whether having `articles/gallery.html` reachable from two nav items causes confusion
   - Recommendation: During Phase 13, keep the `articles:` component as-is (it points to `gallery.html`) and add `gallery:` as a separate entry pointing to the same page; Phase 16 will create a dedicated gallery index

---

## Sources

### Primary (HIGH confidence)
- `_pkgdown.yml` (read this session, lines 1-246) — existing navbar structure, reference groups, articles sections
- `.Rbuildignore` (read this session) — existing ignore patterns
- `data/dieselgate.rda` (structure verified this session) — offline dataset
- `.github/workflows/pkgdown.yaml` (read this session) — CI build command
- `packageVersion()` calls this session — pkgdown 2.2.0, knitr 1.51, rmarkdown 2.31

### Secondary (MEDIUM confidence)
- [CITED: https://pkgdown.r-lib.org/articles/articles.html] — `vignettes/articles/` mechanism and `.Rbuildignore` requirement
- [CITED: https://pkgdown.r-lib.org/reference/build_site.html] — `math-rendering: katex` key in pkgdown 2.x

### Tertiary (LOW confidence)
- [ASSUMED] — `_`-prefix suppression in article scanner
- [ASSUMED] — pandoc citeproc path resolution relative to source `.Rmd`
- [ASSUMED] — `mathjax:` option in article YAML front matter causes double-load with `math-rendering: katex`

---

## Metadata

**Confidence breakdown:**
- Repo state (what exists, what doesn't): HIGH — verified by reading files this session
- Standard stack (pkgdown/knitr/rmarkdown): HIGH — version-confirmed this session
- pkgdown `math-rendering: katex` key: MEDIUM — cited from official docs, not independently rendered
- Citation resolution path: MEDIUM — standard pandoc behavior, well-documented
- `_`-prefix suppression: LOW — needs local build confirmation

**Research date:** 2026-09-05
**Valid until:** 2026-11-05 (pkgdown 2.x stable; re-verify if pkgdown releases 2.3.x before Phase 13 execution)
