# Feature Research

**Domain:** Polish milestone on a mature CRAN R package (financial event study analysis)
**Researched:** 2026-09-08
**Confidence:** MEDIUM (cross-checked against existing codebase + official pkgdown/lifecycle/cli docs)

---

## Scope Note

This file covers the four polish surfaces for v0.65.0 only. All core pipeline, models, test statistics, AI advisor, reporting, and site infrastructure features are already shipped (see PROJECT.md Validated section). Nothing here re-proposes existing capabilities.

---

## Surface 1 — Brand & Visual Identity

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Existing Component | Notes |
|---------|--------------|------------|-------------------|-------|
| Logo PNG in `man/figures/logo.png` auto-detected by pkgdown | pkgdown auto-displays in navbar, README img, and as favicon source; any package with a pkgdown site "should" have one | LOW | `_pkgdown.yml` (no logo key needed — auto-detect) | Must be ~2400x2772px, transparent BG per hexb.in spec; `usethis::use_logo()` handles placement + `.Rbuildignore` |
| Hex sticker SVG/PNG asset stored in `man/figures/` | Community norm — tidyverse, rOpenSci, nearly every serious CRAN package has one; README badge and download link are expected | LOW–MEDIUM | None (no logo exists yet) | Produced with hexSticker R package; design must express the "event study / time-series + statistics" domain; color must align to eventstudy.de palette |
| Favicon set in `pkgdown/favicon/` | Any site without a favicon looks unfinished; browsers show generic icon | LOW | `_pkgdown.yml` + `pkgdown/extra.css` | Generated once with `pkgdown::build_favicons()`; stored and committed; auto-included on rebuild |
| Hex badge in README (`![hex](man/figures/logo.png)`) | Every R package README that has a logo shows it at the top-right, between title and badges | LOW | `README.md` | Standard placement: right-aligned img tag or `usethis::use_logo()` badge snippet |
| Lifecycle badge corrected to `stable` | Current badge says "experimental"; the package is at v0.64.0 with a full test suite and CRAN release — "experimental" signals untrustworthiness to researchers | LOW | `README.md` badges block | Change to `[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)]` |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Existing Component | Notes |
|---------|-------------------|------------|-------------------|-------|
| eventstudy.de-aligned pkgdown theme (palette + typography) | Researcher lands on pkgdown from eventstudy.de and sees the same visual language — establishes the three-tool ecosystem coherence ("Event Study Analysis Made Simple") | MEDIUM | `pkgdown/extra.css`, `_pkgdown.yml` template bslib block | Align bslib `primary`, `bg`, `fg` to the neutral card/badge palette of eventstudy.de; add `base_font`/`heading_font` via Google Fonts |
| Open Graph / social preview card configured | Sharing a link to the pkgdown site on Twitter/LinkedIn shows a branded card (logo + description) instead of a blank preview | LOW | `_pkgdown.yml` `template: opengraph:` block | Requires `url:` in `_pkgdown.yml` (already present); add `opengraph: image: src + alt + twitter: card: summary_large_image` |
| Logo mark reusable SVG source (for eventstudy.de propagation) | Three-tool ecosystem currently has no logo anywhere; adding one to the R package hex sticker creates the first shared visual identity that can propagate to the GSheets template and WebAssembly app over time | HIGH (coordination) | None; no logo exists on eventstudy.de | Design constraint: the hex sticker IS the logo; eventstudy.de adoption is out of scope for this R package milestone but the asset must be reusable (SVG source required) |

### Anti-Features (Explicitly Excluded)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Animated / video logo in README | Looks "modern"; GitHub supports GIF badges | Adds distracting noise; research-domain users expect academic seriousness; GitHub strips GIF in some contexts | Static PNG hex sticker — timeless and reusable |
| Dark-mode toggle on pkgdown site | Popular in web frameworks | pkgdown's light-switch requires careful bslib CSS variables that can break the eventstudy.de palette alignment; this is a rabbit hole for a polish minor | Revisit when bslib dark-mode support stabilizes; omit now |
| Custom pkgdown template package (e.g., à la `rotemplate`) | Full design control | Heavyweight maintenance burden; over-engineered for a single-package site; `bslib + extra.css` already used and working | Extend existing `extra.css` + `_pkgdown.yml` bslib variables only |

---

## Surface 2 — Report & Plot Aesthetics

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Existing Component | Notes |
|---------|--------------|------------|-------------------|-------|
| Consistent color palette across ALL plots (AR, CAR, AAR, CAAR, diagnostic) | Users copying figures into a paper or report expect all EventStudy plots to look like they came from the same package | MEDIUM | `R/plotting.R` (352 lines), `inst/rmarkdown/.../skeleton.Rmd` | Currently hardcoded `"steelblue"`, `"red"`, `"grey40"` per plot function; no shared palette. Fix: define an internal `.es_palette` vector and reference it everywhere |
| Colorblind-safe palette | Accessibility requirement for journal submission (many journals now require CVD-safe figures); researchers submitting papers expect this | LOW–MEDIUM | `R/plotting.R` | Replace `"steelblue"` series with Okabe-Ito (8-color, well-tested, built into ggplot2 via `scale_colour_manual(values=c("#E69F00","#56B4E9",...))`) or viridis; zero new dependencies |
| `theme_minimal()` made consistent — margin, axis text, legend placement | Multiple plot functions each call `theme_minimal()` then add inconsistent per-plot overrides; visual differences emerge between plot types | LOW | `R/plotting.R` lines 184, 267, 317, 330, 348 | Define one internal `es_theme()` helper (not exported, no new dep) and replace repeated `theme_minimal() + theme(...)` calls; any downstream ggplot2 plot produced by the package inherits this automatically |
| Figure captions on plot chunks in `es_report()` HTML/PDF output | Academic reports require numbered figure captions; papers need "Figure 1: Cumulative Abnormal Returns around event date" | MEDIUM | `skeleton.Rmd` (fig.cap not set on plot chunks; kable tables already have caption=) | Add `fig.cap` to each plot chunk in the skeleton; knitr numbers automatically in HTML/PDF. Currently captions are only on kable tables, not on plots |
| Styled kable tables in HTML report | Plain `knitr::kable()` in HTML output renders as unstyled HTML table — looks unpolished in a rendered HTML report | LOW | `skeleton.Rmd` (uses bare `knitr::kable()` throughout) | Use `knitr::kable(..., booktabs=TRUE)` for PDF (zero new dep); optionally add `kableExtra::kable_styling()` guarded by `requireNamespace("kableExtra")` for HTML — Suggests-only |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Existing Component | Notes |
|---------|-------------------|------------|-------------------|-------|
| Typographic hierarchy in `es_report()` HTML output | Distinguishes the report from a raw `.Rmd` knit; signals EventStudy is a polished tool, not a script; metric callout boxes make key numbers pop | MEDIUM | `skeleton.Rmd` (uses `## ` headers, no custom CSS) | Inject a `<style>` block or link a custom CSS via `html_document(css=...)` inside the template that sets font stack, header sizing, metric callout boxes. Must not require new hard dependencies |
| Confidence-band fills distinguishable in multi-group AAR/CAAR plots | Multi-group overlapping confidence bands need distinct, clearly labeled fills; current single-group "steelblue" is fine but multi-group coloring is undefined | MEDIUM | `R/plotting.R` `plot_event_study()` | Map group variable to color scale; CI fill = same hue at 20% alpha. Requires `.es_palette` already proposed above |
| `es_report()` opens browser for HTML when called interactively | Quality-of-life: `es_report(task)` already prints the path; opening the file in the default browser adds UX delight for a one-call function at the REPL | LOW | `R/report.R` `es_report()` | `utils::browseURL(path)` when `open = TRUE` (default: `interactive()` guard so batch scripts are unaffected). Opt-in argument |

### Anti-Features (Explicitly Excluded)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| kableExtra as a hard Import | Prettier HTML tables; popular package | kableExtra has been dropped from Imports by several packages due to LaTeX conflicts and dependency weight | Suggests-only with `requireNamespace()` guard; fall back to plain knitr::kable with `booktabs=TRUE` |
| gt package for report tables | gt produces beautiful HTML tables | gt adds a significant dependency chain; CRAN tarball weight increases | Stick with knitr::kable + optional kableExtra |
| ggplot2 → plotly conversion for ALL report plots | Interactive plots everywhere look modern | ggplotly() conversion loses ggplot2 theme details and breaks on multi-facet plots; PDF renders break; the existing `knitr::is_html_output()` guard is the correct design | Keep ggplot2 static for PDF/Word; plotly for HTML when `interactive=TRUE` — existing pattern in skeleton.Rmd |
| Custom R Markdown template file the user can edit | Power users want to tweak the report | Template drift with package updates is a support burden; any user edit is lost on package upgrade | Expose `sections=`, `title=`, `author=` args (already done); for deep customization, `generate_report()` is the lower-level API |

---

## Surface 3 — API & Message Polish

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Existing Component | Notes |
|---------|--------------|------------|-------------------|-------|
| All `print.*` S3 methods return `invisible(x)` | R convention: all `print.*` S3 methods must return their argument invisibly so objects can be used in pipelines without double-printing | LOW | `R/task.R` print.EventStudySummary, `R/advise.R` print.Advice, `R/advise_offline.R` print.es_advice, `R/simulation.R` print.es_simulation | Audit each: must end with `return(invisible(x))`, not `invisible(NULL)` or missing return |
| Error messages that name the offending argument and its value | `stop("task must be an EventStudyTask.")` gives no clue what was actually passed; idiomatic: `stop("'task' must be an EventStudyTask, got: ", class(task)[1])` | LOW | `R/prepare_event_study.R` lines 14, 18; `R/task.R` many stop() calls | Add the actual value/class to all stop() messages; no new dependency — plain base-R improvement |
| `format()` method defined for every class that has a `print()` | R convention: `print()` wraps `format()`; missing format() means piping object into `glue()` or `paste()` fails | LOW–MEDIUM | `R/advise.R` (print.Advice exists, format.Advice does not); same for es_diagnostics, es_simulation | Define `format.Cls` assembling the character representation; rewrite `print.Cls` to call `cat(format(x), sep="\n")` |
| Argument name consistency across all public functions | `run_event_study()`, `fit_model()`, `calculate_statistics()` consistently use `task` then `parameter_set`; newer functions (`es_report`, `es_advise`) must match this contract | LOW | `R/execute.R`, `R/report.R`, `R/advise.R` | Audit: `task` first arg, `parameter_set` second where applicable, `...` at end; document the rule |
| `run_event_study()` gets `verbose = TRUE/FALSE` | Batch users do not want console output; interactive users want progress signals; currently `message("Report written to: ...")` always fires | LOW | `R/execute.R` line 42 | Add `verbose = TRUE` default; wrap message() calls in `if (verbose)`. Backward-compatible (default TRUE preserves existing behavior) |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Existing Component | Notes |
|---------|-------------------|------------|-------------------|-------|
| Classed conditions for the three most common user errors | Users can programmatically catch "not fitted" vs "bad input" vs "missing package" in tryCatch; production pipelines that wrap EventStudy need this | MEDIUM | `R/task.R`, `R/models.R`, `R/prepare_event_study.R` — all use bare `stop()` | Three target classes: `eventstudy_error_not_fitted`, `eventstudy_error_bad_input`, `eventstudy_error_missing_package`. Implement via `rlang::abort(message, class="eventstudy_error_X")`. rlang already in Imports |
| cli-styled `print.EventStudyTask` with header, key stats in aligned columns | Current `cat()` output is plain; tidyverse packages (tibble, dplyr) all use cli formatting; researchers opening a task at the REPL should get a dashboard-style summary | MEDIUM | `R/task.R` lines 93–115: bare `cat()` calls | Rewrite using `cli_h1()`, `cli_dl()` for definition list (key: value), `cli_rule()` for separator. Add cli to Suggests; if not available, fall back to existing cat() pattern |
| `summary()` method for `EventStudyTask` returning a proper S3 object | `summary(task)` currently falls through to the R6 default; users expect a proper summary on a model-like object | MEDIUM | `R/task.R` — `print.EventStudySummary` S3 exists (line 313); `summary.EventStudyTask` not found | Define `summary.EventStudyTask` returning an `EventStudySummary` S3 object; `print.EventStudySummary` already exists — just needs the `summary()` entrypoint wired |

### Anti-Features (Explicitly Excluded)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Renaming public functions (e.g. `run_event_study` → `event_study()`) | Shorter, friendlier names | Any function rename in a CRAN package with downstream users is a breaking change requiring a full deprecation cycle; this is a polish minor, not a 1.0 rename | Use `lifecycle::deprecate_warn()` only if a specific argument name is genuinely confusing; no function renames this milestone |
| cli as a hard Import | cli formatting is polished and widely used | cli is heavier than it looks; several packages have resisted importing it; Suggests means users without cli still work fine | Add to Suggests with `requireNamespace()` guard in print methods |
| Progress bars for long model fits | UX improvement during batch multi-event runs | progress package dependency; sequential purrr::map() loops would need restructuring; scope creep | `verbose` argument with messages covers the need |

---

## Surface 4 — Docs & Site Polish

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Existing Component | Notes |
|---------|--------------|------------|-------------------|-------|
| `@family` roxygen tags on related function groups | Generates "See Also" sections; drives pkgdown reference page cross-linking; rOpenSci requires it for peer review packages | LOW | `R/` roxygen docs — no `@family` tags found in search | Tag groups: pipeline (`run_event_study`, `prepare_event_study`, `fit_model`, `calculate_statistics`), advisor (`es_advise`, `es_diagnostics`, `es_report`), export (`export_results`, `tidy.EventStudyTask`), plots (`plot_event_study`, `plot_stocks`, `plot_diagnostic`) |
| Lifecycle badge corrected to `stable` in README | Currently says "experimental"; misleading for a CRAN-released package at v0.64.0 with 2287 passing tests | LOW | `README.md` line 7 | Change badge URL and label; update `lifecycle` field in DESCRIPTION if present |
| `<!-- pkgdown: home: start/end -->` markers in README | Controls which README content appears on the pkgdown homepage; without markers, the entire README (including dev-install instructions) appears verbatim | LOW | `README.md` — no markers present | Add markers so the pkgdown homepage shows Features + Quick Start but hides the dev-only GitHub install block |
| `@seealso` cross-links on the three pipeline functions | Users reading `fit_model()` docs should see a link to `calculate_statistics()`; without this, function discovery requires knowing names in advance | LOW | `R/execute.R`, `R/report.R` roxygen blocks — sparse `@seealso` | Add `@seealso \code{\link{calculate_statistics}}` etc. to `run_event_study`, `fit_model`, `calculate_statistics`; add advisor cross-links to `es_advise`, `es_report` |
| Getting-started article listed first in Articles nav | "Get Started" nav item exists but `introduction` vignette order in Articles section of `_pkgdown.yml` must be verified | LOW | `_pkgdown.yml` articles section | Confirm `introduction` is the first entry in the "Get Started" group; if not, re-order |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Existing Component | Notes |
|---------|-------------------|------------|-------------------|-------|
| README "Ecosystem" section linking eventstudy.de, GSheets template, and WebAssembly app | Researchers finding the R package via CRAN or GitHub should immediately understand the three-tool ecosystem | LOW | `README.md` — no ecosystem section | Add a concise "Ecosystem" or "Part of the EventStudy Suite" section after Features; 3–5 bullet points with links |
| pkgdown homepage card strip mirroring eventstudy.de layout | The eventstudy.de brand hub uses a card + numeric-badge layout; the pkgdown homepage echoing this establishes ecosystem coherence | MEDIUM | `pkgdown/extra.css` (gallery grid CSS exists), `README.md` (homepage source) | Adapt existing `.es-gallery` CSS to a "three-tool ecosystem" card strip inside the `pkgdown: home:` block; low-content change |
| NEWS.md linked from pkgdown navbar | Researchers tracking package changes should reach the changelog in one click; common in mature packages | LOW | `_pkgdown.yml` — no News component in navbar currently | Add `news: { one_page: true }` in `_pkgdown.yml`; pkgdown auto-renders NEWS.md at news/index.html |
| `CONTRIBUTING.md` pointing to dev workflow | Reduces maintainer questions from new contributors; common in active CRAN packages | LOW | None found | Minimal file: PR welcome, run `devtools::check()`, reference `cran-comments.md`; `.Rbuildignore`d |

### Anti-Features (Explicitly Excluded)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Rewriting the 18 CRAN vignettes | They could be restructured or updated | Breaking change to vignette slugs breaks external URLs; any rewrite risks introducing errors that fail `R CMD check --as-cran` | Add cross-links and a better intro; restructuring is a future milestone |
| Moving pkgdown-only articles into CRAN vignettes | Makes rich content available offline in the tarball | Bloats the tarball; slows `R CMD check`; the v0.63.0 decision was deliberate | Status quo: pkgdown-only, clearly labeled in the navbar |
| PDF vignette for CRAN | Some journals request PDF | PDF vignettes require LaTeX toolchain in CI; adds a NOTE in `R CMD check` if tinytex fails | HTML vignettes on CRAN; PDF available via `es_report(format="pdf")` |

---

## Feature Dependencies

```
Surface 1: Brand
  hex sticker design (SVG source)
      └── enables ──> man/figures/logo.png placement
      └── enables ──> README hex badge
  man/figures/logo.png
      └── enables ──> pkgdown::build_favicons() → pkgdown/favicon/
      └── enables ──> Open Graph social card (image src)
  bslib palette alignment
      └── requires ──> eventstudy.de color codes (design input)
      └── enhances ──> Open Graph social card image consistency

Surface 2: Report / Plot Aesthetics
  .es_palette constant
      └── requires ──> colorblind-safe color selection (design decision, no code dep)
      └── enables  ──> es_theme() internal helper
      └── enables  ──> consistent multi-group CI band coloring in plot_event_study()
  es_theme() internal helper
      └── requires ──> .es_palette
      └── enables  ──> all ggplot2 plot functions (R/plotting.R)
  knitr fig.cap on plot chunks
      └── requires ──> skeleton.Rmd chunk refactor
      └── independent of palette/theme changes

Surface 3: API & Message Polish
  format.Cls methods
      └── enables  ──> print.Cls rewrite (print calls format)
  classed conditions (rlang::abort)
      └── requires ──> rlang (already in Imports — zero new dep)
      └── independent of cli
  cli print.EventStudyTask
      └── requires ──> cli in Suggests
      └── independent of classed conditions
  verbose= argument
      └── independent of all other polish items

Surface 4: Docs & Site
  @family + @seealso tags (roxygen)
      └── requires ──> devtools::document() rebuild
      └── enables  ──> pkgdown reference grouping improvements
  pkgdown: home: markers in README
      └── enables  ──> homepage card strip (markers must exist first)
  logo.png (Surface 1)
      └── enables  ──> Open Graph image (Surface 1 must complete first)
```

---

## MVP Definition

### v0.65.0 Polish minimum

All four surfaces are in scope. "MVP" here means the irreducible minimum within each surface to satisfy the "Polish" release label.

- [ ] **Brand**: Hex sticker PNG + `man/figures/logo.png` placement + favicon generation + lifecycle badge corrected to `stable`
- [ ] **Report/Plot**: Shared `.es_palette` constant + `es_theme()` internal helper wired into all ggplot2 plots + `fig.cap` on plot chunks in skeleton.Rmd
- [ ] **API**: Audit all `print.*` methods return `invisible(x)` + error messages name offending argument values
- [ ] **Docs**: `@family` + `@seealso` tags on pipeline + advisor functions; lifecycle badge; `<!-- pkgdown: home: -->` markers

### Add after initial surface pass (v0.65.x)

- [ ] Open Graph social preview card — 15-minute config once logo exists
- [ ] bslib palette alignment to eventstudy.de — needs color code input from brand
- [ ] cli-styled `print.EventStudyTask` — higher DX value; needs cli in Suggests
- [ ] `format()` methods for all S3 classes — complete the print/format contract
- [ ] `summary.EventStudyTask` S3 wiring — needs design of what to include
- [ ] README Ecosystem section — content decision (links to eventstudy.de tools)
- [ ] `run_event_study(verbose=)` — low risk, good for batch users
- [ ] NEWS.md in pkgdown navbar
- [ ] Typographic CSS in `es_report()` HTML

### Future consideration (v0.66.0+)

- [ ] Full classed condition hierarchy (all `stop()` → `rlang::abort()`) — needs naming convention decision
- [ ] CONTRIBUTING.md — helpful but not user-visible behavior
- [ ] kableExtra-styled report tables — Suggests dep, design decision needed

---

## Feature Prioritization Matrix

| Feature | User Value | Implementation Cost | Priority |
|---------|------------|---------------------|----------|
| Hex sticker / logo asset created | HIGH | MEDIUM | P1 |
| `man/figures/logo.png` placement + favicon | HIGH | LOW | P1 |
| Lifecycle badge → `stable` | HIGH | LOW | P1 |
| `.es_palette` constant + `es_theme()` helper | HIGH | LOW | P1 |
| `fig.cap` on plot chunks in skeleton.Rmd | HIGH | LOW | P1 |
| `@family` + `@seealso` roxygen tags | MEDIUM | LOW | P1 |
| `<!-- pkgdown: home: -->` markers in README | MEDIUM | LOW | P1 |
| `print.*` → `invisible(x)` audit | MEDIUM | LOW | P1 |
| Error messages name offending values | MEDIUM | LOW | P1 |
| bslib palette alignment to eventstudy.de | HIGH | MEDIUM | P2 |
| Open Graph social preview card | MEDIUM | LOW | P2 |
| `format()` methods for S3 classes | MEDIUM | LOW | P2 |
| Classed conditions (`rlang::abort`) | MEDIUM | MEDIUM | P2 |
| cli `print.EventStudyTask` | MEDIUM | MEDIUM | P2 |
| `summary.EventStudyTask` S3 wiring | MEDIUM | LOW | P2 |
| `run_event_study(verbose=)` argument | LOW | LOW | P2 |
| README Ecosystem section | MEDIUM | LOW | P2 |
| NEWS.md in pkgdown navbar | LOW | LOW | P2 |
| Typographic CSS in `es_report()` HTML | LOW | MEDIUM | P3 |
| `es_report()` browser-open on HTML | LOW | LOW | P3 |
| CONTRIBUTING.md | LOW | LOW | P3 |

**Priority key:**
- P1: Must have for v0.65.0 to read as a "Polish" release
- P2: High bang-for-buck; include if time allows
- P3: Nice to have; defer to v0.65.x or v0.66.0

---

## Comparator Analysis

| Feature | estudy2 (CRAN) | eventstudyr (CRAN) | EventStudy (this package) |
|---------|----------------|---------------------|--------------------------|
| Hex sticker / logo | No | No | Planned v0.65.0 |
| pkgdown site | No | No | Shipped v0.62.0 |
| Colorblind-safe plots | No | No | Planned v0.65.0 |
| Consistent internal theme | No | No | Planned v0.65.0 |
| cli-style print methods | No | No | Planned v0.65.0 |
| Classed error conditions | No | No | Planned v0.65.0 |
| lifecycle badges | No | No | Planned v0.65.0 |
| @family cross-linking | No | No | Planned v0.65.0 |

Neither comparator has a pkgdown site or any of these polish features. This milestone makes EventStudy the reference implementation in the space for package quality, not just statistical coverage.

---

## Sources

- [pkgdown Customise](https://pkgdown.r-lib.org/articles/customise.html) — bslib, fonts, navbar (webfetch, LOW confidence)
- [pkgdown build_favicons](https://pkgdown.r-lib.org/reference/build_favicons.html) — logo placement, favicon generation (webfetch, LOW confidence)
- [pkgdown Metadata / OG](https://pkgdown.r-lib.org/articles/metadata.html) — Open Graph social card config (webfetch, LOW confidence)
- [hexSticker GitHub](https://github.com/GuangchuangYu/hexSticker) — sticker() function, PNG dimensions (webfetch, LOW confidence)
- [R Packages (2e) Lifecycle chapter](https://r-pkgs.org/lifecycle.html) — deprecation workflow, CRAN backward-compat expectations (webfetch, LOW confidence)
- [cli cli_format_method](https://cli.r-lib.org/reference/cli_format_method.html) — format/print S3 idiom (webfetch, LOW confidence)
- [rOpenSci Dev Guide](https://devguide.ropensci.org/pkg_building.html) — README structure, badges, @family, cross-linking (webfetch, LOW confidence)
- Codebase inspection: `R/plotting.R`, `R/task.R`, `R/report.R`, `inst/rmarkdown/templates/.../skeleton.Rmd`, `_pkgdown.yml`, `pkgdown/extra.css`, `README.md`, `DESCRIPTION` (direct Read, HIGH confidence)

---
*Feature research for: EventStudy v0.65.0 Polish milestone*
*Researched: 2026-09-08*
