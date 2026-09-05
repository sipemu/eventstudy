# Project Research Summary

**Project:** EventStudy — v0.63.0 Documentation Depth: Methods & Worked Examples
**Domain:** pkgdown rich-article documentation for a mature CRAN R statistics package
**Researched:** 2026-09-05
**Confidence:** HIGH

## Executive Summary

This milestone is a **documentation-depth** effort, not a code change: transform the existing v0.62.0 pkgdown site from a grouped reference into a pyfda-caliber learning resource (reference standard: `https://sipemu.github.io/pyfda/`). The delivery mechanism is well-established and low-risk — pkgdown treats `vignettes/articles/*.Rmd` as site-only content that is auto-`.Rbuildignore`d by `usethis::use_article()`, rendered by the existing CI workflow, and excluded from the CRAN tarball. Every capability needed (math rendering, tables, static/interactive plots, citations, build caching) is achievable with **zero new DESCRIPTION dependencies** — the entire toolchain (`knitr`, `DT`, `ggplot2`, `plotly`, pandoc-citeproc) is already present. The one hard config decision is settled: use `template.math-rendering: katex`, not `mathjax`, because MathJax has a documented JS conflict with plotly (pkgdown#1338) and both formulas and plots must coexist on the same page.

The recommended approach is a **template-first, foundation-gated build**: bake determinism (`set.seed`, `options(scipen, digits)`), the math-delimiter convention (`$...$`/`$$...$$` only), the shared `references.bib` co-located in `vignettes/articles/`, and the ggplot2-vs-plotly policy into a single reusable article skeleton *before* any content is written. Then produce the two highest-value Methods articles (Return Models, Test Statistics) and one proof-of-concept gallery example (Dieselgate, using already-bundled data), then fan out to the remaining method families and the cross-domain gallery. Content must be genuinely additive — the 18 existing CRAN vignettes cover the *API*; these articles add *formulas, assumptions, when-to-use guidance, academic references, and rendered outputs* that the vignettes deliberately lack (all existing vignettes are `eval=FALSE`).

The dominant risks are all documentation-integrity, not build-mechanics: **subtly wrong statistical formulas** in a statistics package's own docs (mitigate with a per-article formula-correctness review against primary literature + package source + a `test-formula-consistency.R` net), **non-deterministic rendered output** polluting CI diffs and undermining gallery credibility (mitigate with disciplined seeding, verified by building twice and diffing), **silent citation/math-escaping failures** that build clean but render raw `[@Key]`/`$LaTeX$` in the browser (mitigate with a `grep '\[@'` CI gate and filename-only bib paths), and **CRAN cleanliness regressions** from datasets (`data/` is NOT excluded by `.Rbuildignore`; enforce a <=600 KB total budget, mandatory `man/` docs, and legally-redistributable-only sources). Data licensing is the sharpest strategic call: prefer `simulate_event_study()` + `set.seed()` over Yahoo Finance scraping — six new scraped datasets turn one defensible illustrative sample (dieselgate) into a systematic redistribution posture.

## Key Findings

### Recommended Stack

The stack is essentially "use what's already there, plus one YAML line." No new Imports, no new Suggests. All rich-docs capabilities are delivered by pkgdown's articles mechanism and packages already in DESCRIPTION. See STACK.md for the full `_pkgdown.yml` delta.

**Core technologies:**
- **`vignettes/articles/` + pkgdown 2.2.1**: site-only rendered articles — auto-`.Rbuildignore`d by `usethis::use_article()`, excluded from tarball, zero `R CMD check` impact
- **`template.math-rendering: katex`** (one YAML line): high-fidelity formula rendering — KaTeX, NOT MathJax, because MathJax conflicts with plotly.js (pkgdown#1338); KaTeX has no such conflict
- **`knitr::kable()` (primary) + `DT::datatable()` (interactive)**: tables — both already in Suggests; no `gt`/`kableExtra` (unnecessary new deps)
- **`ggplot2` (Methods articles) + `plotly` (gallery)**: plots — both already in Imports; static for exposition, interactive only where hover is the pedagogical point
- **pandoc-citeproc + single shared `references.bib`**: academic citations — bundled with Pandoc (installed by CI's `setup-pandoc@v2`); no R citation package needed

**Explicitly rejected:** MathJax (plotly conflict), `gt`/`kableExtra`/`flextable` (needless deps), `Rdpack` (wrong tool for Rmd prose), multiple `.bib` files (pandoc-citeproc error 83), Quarto/`.qmd` (rough pkgdown edges + CI install burden), `pkgdown.offline`/`targets` (unnecessary), caching plotly widget chunks (widgets don't serialize).

### Expected Features

The bar is set by pyfda's method pages (conceptual intro -> key formula in LaTeX -> assumption checklist -> when-to-use decision table -> rendered worked example -> comparative output -> references -> see-also). FEATURES.md defines a reusable 10-section article template that every Methods article must follow (1,500–2,500 words; the assumption checklist, when-to-use table, and comparative output are non-negotiable).

**Must have (table stakes):**
- Rendered code output on every article (existing vignettes are all `eval=FALSE` — this is the core differentiator vs. status quo)
- LaTeX formula rendering for every method family (an econometrics package without math reads as incomplete)
- When-to-use decision table per method family + academic references per method
- A distinct "Learn/Methods" navbar section (separate from the existing "Articles" vignette listing)
- Worked examples with real/realistic data + a gallery landing page with domain cards

**Should have (differentiators):**
- pyfda-style method-page template with assumption-checklist callout boxes
- Cross-method comparison tables within a family (Patell vs BMP vs KP vs Sign in one view)
- AI-Advisor tie-in callout in every method page (connects learning content to the package's unique feature)
- Cross-domain gallery (8 proposed examples across scandal, earnings, monetary policy, FDA, GDPR, dividends, intraday, synthetic control)
- Offline-safe build with bundled/simulated datasets

**Defer (v0.64.0):** printed-PDF export of articles, Shiny interactivity (requires a server; incompatible with static pkgdown), cross-article search index (pkgdown already provides site search).

### Architecture Approach

The integration is purely additive and file-driven: create `vignettes/articles/` (with a shared `_setup.Rmd` child doc for seed + knitr opts), register new articles in `_pkgdown.yml`'s `articles:` section, and extend the navbar in BOTH `navbar: structure:` AND `navbar: components:` (a new `learn:` + `gallery:` slot). The existing `.github/workflows/pkgdown.yaml` needs **no changes** — it discovers `vignettes/articles/` automatically; only `Config/Needs/website: pkgdown` is added to DESCRIPTION. pkgdown auto-unnests `vignettes/articles/foo.Rmd` -> `articles/foo.html` (content name is the bare filename). See ARCHITECTURE.md for the exact navbar YAML, the full new/modified-file table, and the 6-phase build order (Scaffold -> Datasets -> Methods articles -> Gallery -> navbar wiring -> verification).

**Major components:**
1. **Article infrastructure** — `vignettes/articles/` dir + `_setup.Rmd` shared child + `references.bib` + `.Rbuildignore` entry (`^vignettes/articles$`); the reusable article template lives here
2. **Bundled/simulated datasets** — dieselgate precedent: `data-raw/*.R` provenance + `data/*.rda` (if shipped) + `R/data-*.R` roxygen doc; or `.rds` in `vignettes/articles/data/` for purely site-only data
3. **Methods articles (x7)** — return models, test statistics, panel DiD, intraday, synthetic control, diagnostics, AI advisor — each conceptual, formula-bearing, rendered
4. **Gallery articles (x3–8)** — end-to-end worked examples + a card-index landing page
5. **`_pkgdown.yml` navbar/articles wiring** — learn + gallery slots, math-rendering config

### Critical Pitfalls

1. **Subtly wrong statistical formulas** — a statistics package's own docs are held to a higher bar; a wrong DoF correction or omitted covariance term actively misleads researchers who cite it. *Avoid:* per-article formula review against the PRIMARY paper (Patell 1976, not MacKinlay 1997) AND the package source (`R/*_test_statistics.R`); add `tests/testthat/test-formula-consistency.R` asserting package output == hand-computed formula to 4 decimals.
2. **Non-deterministic rendered output** — `bootstrap_test()`/`simulate_event_study()`/GARCH without seeding produce different HTML every build, polluting `gh-pages` diffs and destroying gallery credibility. *Avoid:* `set.seed()` in every stochastic chunk + `options(scipen=999, digits=4)` in setup; verify by building twice and diffing `docs/articles/` (zero numeric diffs is the bar).
3. **Silent citation/math-escaping failures** — clean build, but the browser shows raw `[@MacKinlay1997]` or raw `$LaTeX$`. *Avoid:* co-locate `references.bib` in `vignettes/articles/` with a filename-only path; use only `$...$`/`$$...$$` delimiters; add a `grep -r '\[@' docs/articles/` CI gate that must return empty.
4. **CRAN cleanliness regressions from datasets** — `.Rbuildignore` does NOT exclude `data/`; undocumented `.rda` = CRAN-blocking WARNING; oversized `data/` = installed-size NOTE. *Avoid:* <=120 KB/dataset, <=600 KB `data/` total; mandatory `man/` doc (`@format`/`@source`) created in the same commit as the `.rda`; add `^vignettes/articles$` to `.Rbuildignore` in the commit that CREATES the directory.
5. **Data-redistribution legal risk** — Yahoo Finance ToS prohibits redistribution; six scraped datasets escalate one illustrative sample into a systematic collection. *Avoid:* prefer `simulate_event_study()` + `set.seed()` (zero licensing risk, offline-safe, reproducible); reuse bundled `dieselgate` where possible; if scraping is unavoidable, cap scope and add a `meta$license_note` + `data-raw/DATA-SOURCES.md`.
6. **plotly page-weight / silent blank-figure failures + long CI build time** — 13+ articles x multiple plotly widgets = tens of MB JS; GARCH fits + big bootstraps blow past a reasonable CI budget. *Avoid:* ggplot2 static for Methods articles, one plotly max per gallery article (never `ggplotly()` as a drop-in, never `saveWidget(selfcontained=TRUE)`); cap `n_boot=99`/`n_sim=100` in articles, cache GARCH fits as `.rds`; 60 s/article render budget, <15 min total CI.

## Implications for Roadmap

Based on research, the suggested phase structure closely follows the ARCHITECTURE.md build order and the FEATURES.md MVP tiers. The critical dependency is: **infrastructure (template + determinism + citation + math conventions) must be locked before any content**, because 8 of 12 pitfalls are prevented once-and-for-all in the template, and re-discovering them per-article is the primary failure mode.

### Phase 1: Article Infrastructure & Conventions (gate)
**Rationale:** Every downstream pitfall (determinism, math escaping, citation resolution, tarball exclusion, plotly policy, content-duplication) is cheapest to prevent in the shared template before content exists. This is the highest-leverage phase.
**Delivers:** `vignettes/articles/` + `^vignettes/articles$` in `.Rbuildignore` (same commit) - `_setup.Rmd` (seed + `options` + knitr opts) - `references.bib` (co-located, filename-only path) - `math-rendering: katex` in `_pkgdown.yml` - reusable 10-section article skeleton - `Config/Needs/website: pkgdown` in DESCRIPTION - a smoke-test formula + citation verified to render in a CI dry-run - one-paragraph content brief per planned article.
**Addresses:** LaTeX rendering, reusable template, Learn navbar section (table stakes).
**Avoids:** Pitfalls 1–2 (determinism), 3 (escaping/citations), 5 (tarball exclusion #8), and 11 (content briefs prevent duplication).

### Phase 2: Curated Datasets
**Rationale:** Gallery articles depend on data; get the size budget, licensing posture, and `man/` documentation discipline right on the FIRST dataset, not as a pre-CRAN cleanup.
**Delivers:** Per-dataset `data-raw/*.R` provenance (with `meta$license_note`) + `data-raw/DATA-SOURCES.md` - bundled `data/*.rda` (or `.rds` in `vignettes/articles/data/` for site-only) + `R/data-*.R` roxygen - `simulate_event_study()`-based datasets preferred over scraped data.
**Uses:** dieselgate precedent (STACK/ARCHITECTURE); `simulate_event_study()` (FEATURES data-strategy Tier 2).
**Avoids:** Pitfalls 3 (tarball bloat/installed-size NOTE), 4 (redistribution), 9 (undocumented dataset WARNING).

### Phase 3: Core Methods Articles + Proof-of-Concept Gallery
**Rationale:** Return Models and Test Statistics are the highest-traffic concepts with the most unique formula content; G-1 Dieselgate uses already-bundled data and proves the gallery format end to end.
**Delivers:** Return Models Methods article - Test Statistics Methods article (both rendered, formula-reviewed) - G-1 Dieselgate gallery example.
**Implements:** Methods articles + gallery components; `test-formula-consistency.R` begun.
**Avoids:** Pitfall 7 (formula correctness — review gate per article), 6 (plotly page-weight — static-first), 10 (`stopifnot()` output assertions per chunk).

### Phase 4: Remaining Method Families + Core Gallery
**Rationale:** Diagnostics (advisor tie-in is the unique value-add), Panel DiD (rendered event-time plot is the visual centerpiece), and Synthetic Control (gap plot + placebo) round out the high-value method pages; the differentiation-heavy gallery examples (FOMC/KP, GDPR/Callaway-Sant'Anna) showcase capabilities no competitor documents.
**Delivers:** Diagnostics, Panel DiD, Synthetic Control Methods articles - G-8 (synthetic control, reuses dieselgate), G-3 (FOMC/KP), G-5 (GDPR staggered panel) gallery examples.
**Avoids:** Pitfalls 7 (formula review continues), 12 (render-budget + caching for GARCH/heavy models).

### Phase 5: Complete Gallery, Minor Articles & Integration
**Rationale:** Intraday + AI Advisor articles are shorter/lower-traffic; the remaining gallery examples exercise the last untested surfaces; navbar wiring and the card-index landing page make the section discoverable; the canary test locks the whole thing.
**Delivers:** Intraday + AI Advisor Methods articles - G-2/G-4/G-6/G-7 gallery examples - gallery landing card index - full `_pkgdown.yml` navbar wiring (learn + gallery slots in BOTH structure and components) - `tests/testthat/test-article-outputs.R` canary - final `R CMD check --as-cran` (0 new NOTEs/WARNINGs) + `pkgdown::build_site_github_pages()` + CI deploy verification.
**Avoids:** Pitfall 2 (navbar structure+components both updated), 8 (final tarball check), 10 (canary test).

### Phase Ordering Rationale

- **Infrastructure-first is non-negotiable:** the research is emphatic that 8 of 12 pitfalls are template-level and re-discovering them per-article is the dominant failure mode — so conventions gate content.
- **Datasets before gallery:** gallery articles have a hard data dependency; the size/licensing/documentation discipline must be established on dataset #1 (ARCHITECTURE build order Phase B; PITFALLS 3/4/9 all say "at creation time, not cleanup").
- **Highest-value content first:** FEATURES MVP tiers put Return Models + Test Statistics + G-1 in P1 because they carry the most unique formula content and G-1 needs no new data.
- **Method families are independent:** Methods articles can be written in any order (ARCHITECTURE Phase C), so phases 3–5 group by value, not dependency.
- **Navbar + canary last:** navbar wiring needs the content to exist to link to; the canary test locks all rendered outputs against future API drift (PITFALLS 10).

### Research Flags

Phases likely needing deeper research during planning:
- **Phase 2 (Datasets):** dataset-source licensing decisions per gallery domain are consequential and case-specific — the `simulate` vs. scrape call, and the exact size budget per dataset, warrant a focused planning pass (FEATURES data-strategy table + PITFALLS 4 are the inputs).
- **Phase 3–4 (Methods articles, formula content):** each formula must be verified against the PRIMARY paper AND the package source implementation — this is per-article research, not skippable; the formula-review gate is a merge blocker.

Phases with standard patterns (skip research-phase):
- **Phase 1 (Infrastructure):** pkgdown articles mechanism, math-rendering config, `.Rbuildignore`, citation setup are all well-documented and confirmed against official sources + the existing dieselgate/CI precedent.
- **Phase 5 (navbar/integration):** pure `_pkgdown.yml` wiring and CI verification against a known-good v0.62.0 workflow.

## Confidence Assessment

| Area | Confidence | Notes |
|------|------------|-------|
| Stack | MEDIUM | pkgdown behavior verified against official docs; plotly/MathJax conflict verified against pkgdown#1338; CSL `resource_files:` and single-`.bib` quirks verified against community docs but not first-hand in this repo |
| Features | HIGH | pyfda reference standard analyzed in detail; existing-vignette complement map built from direct inspection; competitor analysis grounded |
| Architecture | HIGH | pkgdown mechanics confirmed against official source + usethis source code; dieselgate precedent + CI workflow inspected directly in-repo |
| Pitfalls | HIGH | Every pitfall grounded in direct inspection of `.github/workflows/pkgdown.yaml`, DESCRIPTION, `.Rbuildignore`, `data-raw/dieselgate.R`, and the 19 existing vignettes |

**Overall confidence:** HIGH

### Gaps to Address

- **CSL `resource_files:` and single-`.bib` behavior (MEDIUM):** the pkgdown quirk requiring `.csl` in `resource_files:` and forbidding multiple `.bib` files is community-documented, not verified in this repo — validate in the Phase 1 CI dry-run (build the smoke-test article and confirm citations render).
- **Math delimiter portability across pandoc versions (MEDIUM):** local RStudio pandoc vs. CI `setup-pandoc@v2` may differ; PITFALLS 5 recommends verifying `pandoc --version` parity — confirm in the Phase 1 dry-run rather than after content exists.
- **Dataset placement decision per gallery example (MEDIUM):** `data()` dataset vs. site-only `.rds` in `vignettes/articles/data/` is decided per dataset against the size budget — resolve during Phase 2 planning using the ARCHITECTURE decision framework.
- **CI build-time headroom (LOW):** the 5–8 min -> 25–40 min estimate is a projection; the concrete GARCH/bootstrap caching strategy (PITFALLS 12) should be validated empirically in Phase 4 when the heavy articles land.

## Sources

### Primary (HIGH confidence)
- Direct in-repo inspection — `.github/workflows/pkgdown.yaml`, `DESCRIPTION`, `.Rbuildignore`, `_pkgdown.yml`, `data-raw/dieselgate.R`, `data/dieselgate.rda`, 19 vignette files, `R/single_event_test_statistics.R`, `R/multi_event_test_statistics.R` (ARCHITECTURE, PITFALLS)
- usethis `use_article()` source — confirms `vignettes/articles/` + exact `.Rbuildignore` entry (ARCHITECTURE)
- r-pkgs.org (Wickham) — data placement, `Config/Needs/website`, articles-vs-vignettes (ARCHITECTURE)
- pyfda reference site (`sipemu.github.io/pyfda`) — method-page template standard, analyzed in detail (FEATURES)

### Secondary (MEDIUM confidence)
- pkgdown official docs — `build_articles` reference, customise article, 2.1.0 release blog, NEWS (v2.2.1) (STACK, ARCHITECTURE)
- pkgdown#1338 — documented plotly + MathJax JS conflict (STACK)
- svPkgdown bibliography example — `resource_files:` CSL trick (STACK)
- Primary econometrics literature for formula content — Patell (1976), BMP (1991), Fama-French (1993), Kolari-Pynnonen (2010), Abadie-Diamond-Hainmueller (2010), Miller (2023), Callaway-Sant'Anna (2021) (FEATURES)
- `eventstudies` CRAN package — accepted pattern for bundled simulated event-study data (FEATURES)

### Tertiary (LOW confidence)
- CI build-time growth projection (5–8 -> 25–40 min) — estimate pending empirical validation in Phase 4 (PITFALLS)
- Data-source licensing terms (Yahoo Finance ToS, Ken French library, FRBSF USMPD) — as stated by sources; verify per-jurisdiction before bundling any scraped data (FEATURES, PITFALLS)

---
*Research completed: 2026-09-05*
*Ready for roadmap: yes*
