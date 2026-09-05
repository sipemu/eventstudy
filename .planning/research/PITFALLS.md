# Pitfalls Research

**Domain:** Adding rich rendered pkgdown documentation (conceptual method articles + worked-examples gallery + bundled datasets) to a CRAN R package with CI-deployed pkgdown (v0.63.0 — Documentation Depth)
**Researched:** 2026-09-05
**Confidence:** HIGH

---

## Critical Pitfalls

### Pitfall 1: Non-Deterministic Rendered Output Causes Noisy CI Diffs and Untrustworthy Gallery Numbers

**What goes wrong:**
Articles use stochastic operations — `bootstrap_test()`, `simulate_event_study()`, GARCH fitting, synthetic data generation — without a `set.seed()` call immediately before the stochastic expression, or they set seed once at document top then consume it during an earlier calculation. Output tables, p-values, and power curves differ on every build. The CI job (`pkgdown.yaml`) runs `pkgdown::build_site_github_pages(new_process = FALSE)` in a fresh R session; if rendered HTML differs from the previous commit, the `gh-pages` branch accumulates large spurious diffs, diff-based review is useless, and the gallery loses credibility when printed numbers don't match the surrounding narrative.

This package already has three stochastic components that will appear in the new articles: `bootstrap_test()` (wild bootstrap), `simulate_event_study()` (Monte Carlo), and `GARCHModel`/`DCCGARCHModel` fitting (numerical optimiser path depends on initial values). Existing CRAN vignettes handle this correctly — `diagnostics-validation.Rmd`, `factor-models-bhar.Rmd`, `panel-event-study.Rmd`, and others each call `set.seed(42)` before stochastic chunks. The new articles must replicate this discipline, not re-discover it after the first noisy build.

Additionally, locale-sensitive number formatting (comma vs period decimal separator), `Sys.Date()` embedded in output cells, and OS-specific floating-point differences across the ubuntu/macos/windows R-CMD-check matrix produce non-determinism across runners. pkgdown renders on ubuntu-latest only, but the principle applies to any article output a human reads.

**Why it happens:**
Authors write articles interactively in RStudio, where the global session already has a seed state that happens to produce stable output locally. In CI, each build starts from a cold R session with no seed, so results vary. The problem is invisible until after multiple builds accumulate on `gh-pages`.

**How to avoid:**
- Place `set.seed(<fixed integer>)` in the `setup` chunk of every article that calls any stochastic function. Also place it immediately before each stochastic expression if the seed may have been consumed by an earlier call in the same document.
- Set `options(scipen = 999, digits = 4)` in each article's setup chunk to pin numeric formatting across locales and OS.
- Never embed `Sys.Date()` or `Sys.time()` in rendered output cells. Date-stamp only in YAML `date:` fields (pkgdown handles those separately) or as a fixed string frozen to the dataset access date.
- For simulation articles, capture stochastic output in a fixed-seed chunk and then display it in a follow-on read-only chunk (`eval = FALSE` + hard-coded display) if the simulation is too slow for CI but the numbers are the pedagogical point.
- After `pkgdown::build_site()` locally, run it a second time and diff `docs/articles/` — zero numeric changes is the bar, not "approximately the same."

**Warning signs:**
- Two consecutive local `pkgdown::build_site()` runs produce different HTML for any article.
- The `gh-pages` branch has commits where the only change is numeric values in table cells.
- Any article chunk contains `rnorm(`, `sample(`, `bootstrap_test(`, or `simulate_event_study(` without an immediately preceding `set.seed()`.

**Phase to address:** Phase 1 (article infrastructure) — bake `set.seed()` and `options()` into the canonical article template before any individual article is written. One template used by all articles eliminates the per-article rediscovery risk.

---

### Pitfall 2: plotly Widgets Cause JS Bloat, Slow Pages, and Silent CI Render Failures

**What goes wrong:**
`plot_event_study()` and `plot_stocks()` return plotly objects. When rendered inside `rmarkdown::html_vignette` output (what all 18 existing vignettes use), each plotly widget inlines the full plotly.js library (~3.5 MB minified) into the HTML page when `self_contained = TRUE`. pkgdown renders `vignettes/articles/` as full HTML documents via its own pipeline and can deduplicate some JS across pages, but if widgets are embedded using `htmlwidgets::saveWidget()` with `selfcontained = TRUE` anywhere in an article, the deduplication is bypassed and the full library lands inline per page.

With 7 method articles + 6 gallery articles = 13 articles each potentially containing 2–4 plotly figures, the site accumulates tens of MB of inlined JS. Page load slows visibly. GitHub Pages has no content-delivery optimization by default. More critically, plotly widgets can silently fail to render in environments where the DOM is not fully initialized before the widget's JS fires — this surfaces as a blank figure area with no error message, which is worse than a missing figure.

The package imports `plotly` unconditionally (it is in `Imports:` in DESCRIPTION), so there is no cost to using it — which makes it the path of least resistance for article authors.

**Why it happens:**
`plot_event_study()` is the package's native visualization API; using it in articles feels natural and demonstrates the full product. The page weight and JS deduplication implications are not visible during local authoring.

**How to avoid:**
- For method articles (statistical exposition, formula illustration, diagnostic output): use ggplot2 static output, not plotly. Add `fig.width = 7, fig.height = 4` chunk options so figures rasterize at a predictable size. The pedagogical point is the pattern of the AR/CAR trajectory, not the hover interactivity.
- For gallery articles where hover-on-event-day interactivity is genuinely part of the worked example value: allow one plotly widget per article, but do not use `htmlwidgets::saveWidget(..., selfcontained = TRUE)` — let pkgdown's template asset pipeline handle the JS dependency.
- Never use `plotly::ggplotly()` as a drop-in replacement for a static ggplot in an article chunk. It increases output size, changes figure dimensions unpredictably, and adds the full plotly.js dependency even for a simple line chart.
- After each article build, check the HTML file size: `ls -lh docs/articles/myarticle.html`. Target under 2 MB per article.
- Offline rendering test: open the built HTML in a browser with network connectivity disabled. Every figure must be visible. Any blank area reveals a missing or broken dependency.

**Warning signs:**
- Any `docs/articles/*.html` exceeds 5 MB.
- More than one `<script src="...plotly">` or inline plotly JS block in a single article HTML.
- A figure area is blank when the article is opened offline after a local `pkgdown::build_site()`.
- An article chunk contains `ggplotly(` on a plot that is not interactive by design.

**Phase to address:** Phase 1 (article infrastructure) — decide the ggplot2-vs-plotly policy per article type before writing any article. Encode the decision in the article template's setup comment. Phase 2 (method articles) — enforce static figures throughout. Phase 3 (gallery) — allow plotly selectively, one widget per article maximum.

---

### Pitfall 3: New Bundled Datasets Bloat the CRAN Tarball and Trigger an Installed-Size NOTE

**What goes wrong:**
`vignettes/articles/` is correctly `.Rbuildignore`d so the article source stays out of the CRAN tarball. But `data/` objects (`.rda` files documented via `man/`) ship unconditionally — `.Rbuildignore` does not exclude `data/`. CRAN's automated check triggers a NOTE when the installed package size exceeds 5 MB, with particular scrutiny when `data/` alone exceeds ~1 MB. A six-domain gallery (earnings surprises, M&A, regulatory shocks, pharmaceutical, macro, ESG) with 3–5 firms each over 300+ trading days produces 50–200 KB per dataset before compression. At bzip2 compression (60–75% typical), a gallery with six new datasets adds 90–600 KB to the tarball — potentially pushing `data/` over the CRAN comfort zone when added to the existing `dieselgate.rda` (9.1 KB).

The current DESCRIPTION has `LazyData: true`, which is correct behavior (objects load on access), but all `data/` objects appear in the package's namespace from the moment the package is loaded — the tarball cost is paid at CRAN submission, not at user runtime.

**Why it happens:**
Each dataset is created individually for its article; the cumulative tarball impact is not tracked until CRAN submission. `usethis::use_data()` puts everything in `data/` by default with no size warning.

**How to avoid:**
- Establish a per-dataset compressed size budget before writing any `data-raw/` script: target ≤120 KB compressed per dataset; `data/` ceiling of 600 KB total across all v0.63.0 additions (including `dieselgate.rda`).
- Thin datasets aggressively: cap at 3 firms + 1 index, date range trimmed to estimation window + (3 × event window length) + 10 buffer days. Store only the columns the article consumes: `date`, `symbol`, `adjusted` (or `close`). No factor data columns, no volume data, no intermediate computed columns.
- For datasets used only by pkgdown-only articles (not referenced from any CRAN-shipped vignette), place the raw `.rda` in `vignettes/articles/data/` (not `data/`), add `^vignettes/articles/data` to `.Rbuildignore`, and document them only in `data-raw/`. This keeps them entirely out of the tarball and the CRAN data-documentation burden.
- After every new `data-raw/` run, enforce: `R CMD build . --no-build-vignettes && tar tzf EventStudy_*.tar.gz | grep "^EventStudy/data/" | awk '{sum += $5} END {print sum/1024, "KB"}'`.
- Run `R CMD check --as-cran` locally; a NOTE about installed size is a build-blocking signal before CRAN submission.

**Warning signs:**
- `R CMD check --as-cran` emits `NOTE: installed size is X.XMb; sub-directories of 1Mb or more: data`.
- `tar tzf *.tar.gz | grep "^EventStudy/data/"` shows total data directory size exceeding 600 KB.
- A `data-raw/` script fetches more than 12 months of daily data per firm (far more than any single event study needs).
- `data/` contains `.rda` files that are only referenced in `vignettes/articles/*.Rmd` (not in any CRAN-shipped vignette or exported function).

**Phase to address:** Phase 2 (dataset curation) — apply the size budget discipline to the first dataset created; do not defer to a "tidy up before CRAN submission" pass.

---

### Pitfall 4: Bundled Financial Data That Is Not Legally Redistributable

**What goes wrong:**
Yahoo Finance's Terms of Service prohibit redistribution of its data in compiled/redistributable forms. The existing `dieselgate.rda` uses Yahoo Finance adjusted prices (via `tidyquant::tq_get()`) and acknowledges this in `data-raw/dieselgate.R` with the note "Small illustrative sample bundled for academic / demonstration use only." The provenance script records `source = "Yahoo Finance (daily adjusted prices)"` but does not record a `license_note` field addressing redistribution rights.

CRAN has not challenged this for one small dataset. If the gallery adds six new datasets, all from Yahoo Finance, the aggregate redistribution surface grows from one illustrative sample to a systematic data collection — a materially different posture that is more likely to attract legal challenge or CRAN policy review. Other common sources have similar restrictions: Bloomberg data is contractually prohibited from redistribution; Refinitiv/LSEG requires a commercial license; CRSP is institution-licensed.

**Why it happens:**
`tq_get()` works with no API key, is already a Suggests package in DESCRIPTION, and the dieselgate precedent appears to have worked. Authors extend the pattern to new datasets without re-evaluating the cumulative legal posture.

**How to avoid:**
- Before writing any new `data-raw/` script, answer: "Can we redistribute this data source in a CRAN package?" Use only sources with clear open-redistribution terms: Kenneth French's Data Library (public domain, factor data), FRED (public domain, macro data), ECB SDW (CC BY 4.0), or academic replication datasets published under CC0.
- For stock price data where no open alternative exists for the specific event: consider a synthetic-hybrid approach — fetch real event parameters (event date, firm identity, event description) from public records, then generate a small realistic synthetic panel calibrated to the historical volatility/return profile. This eliminates redistribution risk while preserving realism for the worked example.
- Where Yahoo Finance prices must be used (extending the dieselgate pattern), strictly limit scope: ≤5 firms, ≤18 months, event window only. Add a `meta$license_note` field explicitly addressing redistribution status: `"Yahoo Finance daily adjusted prices; bundled as a small illustrative academic sample. Users must verify compliance with Yahoo Finance Terms of Service for their jurisdiction."`.
- Create a `data-raw/DATA-SOURCES.md` file documenting the license status, source URL, access date, and redistribution rationale for every dataset. CRAN reviewers and users can then evaluate the terms themselves.

**Warning signs:**
- A new `data-raw/` script calls `tidyquant::tq_get()` and passes the result to `usethis::use_data()` with no `meta$license_note` field addressing redistribution.
- Any dataset's provenance includes `source = "Bloomberg"`, `source = "Refinitiv"`, or `source = "CRSP"` without an institutional license covering redistribution.
- A dataset includes more than 500 trading days per firm (harder to defend as an "illustrative sample").
- `data-raw/DATA-SOURCES.md` does not exist.

**Phase to address:** Phase 2 (dataset curation) — evaluate each dataset source before writing the `data-raw/` script. The redistribution question must be answered before the dataset is generated, not at CRAN submission time.

---

### Pitfall 5: MathJax Escaping Failures Make Statistical Formulas Render as Raw LaTeX Strings

**What goes wrong:**
pkgdown injects a MathJax CDN `<script>` tag into article HTML via its page template. At build time (rendering HTML from Rmd), MathJax is not needed — it runs in the browser. So CI builds succeed even when MathJax is misconfigured. The failure is invisible until a human opens the page.

The more acute problem is escaping. pkgdown renders articles via pandoc after knitr processes the `.Rmd`. The sequence knitr → pandoc → pkgdown template introduces escaping subtleties: backslash sequences in `.Rmd` source may be consumed by knitr before pandoc sees them, causing `\(` inline math delimiters to become `(` in pandoc input. With the statistics package's formula density — Patell Z, BMP variance, Kolari-Pynnönen eigenvalue correction, Sun-Abraham estimator — any escaping error converts a rendered formula to raw `$\hat{\sigma}^2_{i,AR}$` in the published page. There is no build warning; the article appears to build successfully.

Different pandoc versions (the author's local version vs. the `r-lib/actions/setup-pandoc@v2` version pinned in CI) may interpret the same `.Rmd` math escaping differently, producing output that looks correct locally but breaks in CI, or vice versa.

**Why it happens:**
Authors develop articles locally in RStudio, which uses its own bundled pandoc. RStudio Preview renders math correctly. pkgdown CI uses `r-lib/actions/setup-pandoc@v2` which installs a potentially different pandoc version. The difference only surfaces when comparing local and CI outputs.

**How to avoid:**
- Use only `$...$` for inline math and `$$...$$` for display math throughout all articles. Do not mix with `\(...\)` or `\[...\]` — the dollar-sign delimiters are more robustly handled across pandoc versions.
- For multi-line equations, use `$$\begin{aligned}...\end{aligned}$$` — the most reliably rendered form.
- After every `pkgdown::build_site()`, visually inspect the first formula in each article by opening the HTML in a browser. Run `grep -r '\\\$\|\\\\(' docs/articles/` to detect raw backslash-dollar or `\(` sequences surviving into HTML output (a sign of escaped-but-unrendered math).
- Add a CI smoke test: after the pkgdown build, run `grep -c 'MathJax' docs/articles/return-models.html` to verify the MathJax script tag is present.
- Check that `pandoc --version` in the CI environment matches the version used locally during article development. If they diverge by a major version, test locally using the CI pandoc version via Docker.

**Warning signs:**
- Any article HTML contains literal `$` characters in paragraph text where formulas should be rendered.
- `grep -r '\[@' docs/articles/` (for citations) or `grep -r '\$\b' docs/articles/*.html` (for raw math delimiters) returns hits.
- RStudio Preview shows rendered formulas but the pkgdown-built HTML shows raw LaTeX strings.
- `pandoc --version` locally differs from the version installed by `r-lib/actions/setup-pandoc@v2`.

**Phase to address:** Phase 1 (article infrastructure) — establish the math delimiter convention before article writing begins. Add a smoke-test formula to the article template and verify it renders correctly in a CI dry-run before writing any real content.

---

### Pitfall 6: Citation Pipeline Silently Drops References — Published Articles Have Raw [@Key] Markers

**What goes wrong:**
Method articles for a statistics package must cite primary literature: MacKinlay (1997), Brown & Warner (1985), Patell (1976), Boehmer, Musumeci & Poulsen (1991), Kolari & Pynnönen (2010), Callaway & Sant'Anna (2021), etc. The standard approach is `bibliography: references.bib` in the YAML header and `[@MacKinlay1997]`-style citation keys.

The silent failure: if the `.bib` file path cannot be resolved relative to pkgdown's article render working directory, pandoc drops all citations and renders `[@MacKinlay1997]` as literal text in the HTML — no build error, no warning, just raw citation markers on the published page. pkgdown renders articles from the package root (not from `vignettes/articles/`), so a relative path like `bibliography: references.bib` that resolves correctly when `knitr::render()` is called from `vignettes/articles/` fails when called from the package root via pkgdown.

There are currently no citations in any of the 18 existing vignettes (none uses a `bibliography:` YAML field). This milestone will be the first time the citation pipeline is exercised in this project — making path failures likely on the first attempt.

**Why it happens:**
rmarkdown `bibliography:` path resolution varies between rendering contexts. Authors test locally by knitting from the `vignettes/articles/` directory, where the relative path resolves. pkgdown renders from the package root, where the same path fails silently.

**How to avoid:**
- Place `references.bib` in `vignettes/articles/` and use `bibliography: references.bib` (filename only, no path component). pkgdown renders articles with the vignette directory as the working directory for file resolution — co-location avoids the path problem entirely.
- Use a single shared `references.bib` for all articles. Do not create per-article bib files. Since symlinks are unreliable on Windows, commit the single file once and reference it consistently.
- Add a CI citation check: after `pkgdown::build_site()`, run `grep -r '\[@' docs/articles/` — any hit means a citation failed to render. Gate the CI build on this grep returning empty (add as a step in `pkgdown.yaml` after the build step).
- Use `knitr::write_bib()` in a `data-raw/` script to auto-generate BibTeX entries for all R packages cited in articles — keeps package version citations accurate and reproducible.
- Validate by running `grep -l 'References' docs/articles/*.html | wc -l` — should equal the number of articles that have `bibliography:` in their YAML.

**Warning signs:**
- Any article HTML contains `[@` as literal text (run `grep -r '\[@' docs/articles/`).
- A method article on, e.g., test statistics has prose claiming "...as shown in the literature..." with no parenthetical citation and no References section.
- `grep -r 'bibliography:' vignettes/articles/*.Rmd` shows paths with `../` or `../../` prefixes.

**Phase to address:** Phase 1 (article infrastructure) — set up `references.bib` and verify citation rendering before any article is written. The citation pipeline is one-time infrastructure; getting it right first eliminates the failure mode entirely.

---

### Pitfall 7: Documented Statistical Formulas That Are Subtly Wrong

**What goes wrong:**
A financial statistics package publishing its own conceptual documentation is held to a higher standard than a utility package. If the Patell Z formula is presented with the wrong degrees-of-freedom correction, if the BMP variance expression omits the cross-sectional covariance term, or if the Kolari-Pynnönen adjustment description loses the eigenvalue correction, the article is not just incomplete — it is actively misleading to researchers who cite EventStudy in a paper. Researchers may run the package with an incorrect understanding of what it is computing, then cite the wrong formula from the docs in their methodology section.

This is a realistic risk for this specific package: the KP test, BMP test, and Callaway-Sant'Anna estimator all have nuances that are easy to get wrong in LaTeX (sign conventions, normalisation constants, unbalanced panel treatment). The package implementation is the ground truth; the documentation must agree with it exactly — not approximately.

Secondary risk: misattributed citations. Citing "MacKinlay 1997" for the Patell Z formula (MacKinlay reviews the methodology; Patell 1976 is the primary source) is a quality signal that the author read the textbook, not the paper. Citation errors undermine the scholarly credibility of the documentation and are particularly visible to academics who are the primary users.

**Why it happens:**
Article authors write formulas from memory or from secondary sources (textbooks, Wikipedia, other package documentation). Minor transcription errors in LaTeX — a missing subscript, a wrong normalisation constant, a `n-2` vs `n-1` degrees-of-freedom difference — are invisible during review because the rendered output looks mathematically plausible. The R implementation and the displayed formula are never mechanically cross-checked.

**How to avoid:**
- For every formula displayed in a method article: identify the primary literature source (the original paper, not a textbook review). The formula in the article must match the primary source exactly — including normalisation constants, subscript notation, and edge-case handling.
- Cross-check each displayed formula against the package source implementation in `R/single_event_test_statistics.R`, `R/multi_event_test_statistics.R`, and `R/models.R`. The formula in the article and the arithmetic in the R code must be compatible. Add a source comment to the R file: `# Formula: see vignettes/articles/test-statistics.Rmd, equation 3.1` so future maintainers know to update both.
- Where the implementation deviates from the textbook formula (e.g., a degrees-of-freedom adjustment, a finite-sample correction specific to MacKinlay's appendix), document the deviation explicitly: "Note: the implementation uses [X] rather than [Y] from Patell (1976) because [reason]. This matches Brown & Warner (1985) section 3.2."
- Require a formula correctness review — a second pass specifically comparing article LaTeX against the primary paper — as a mandatory gate before each method article is merged. Not a general content review; a focused formula-check.
- Add `tests/testthat/test-formula-consistency.R`: for at least one synthetic example per test statistic, compute the statistic using the package function and hand-compute it using the formula displayed in the article. Assert equality to four decimal places. Formula errors become test failures.

**Warning signs:**
- An article formula references a variable (e.g., `$M_i$`, `$S^2_{\epsilon_i}$`) that does not appear in the primary paper being cited.
- The normalisation constant in the displayed formula differs from what is in the R source by more than a sign or a scalar factor.
- The article cites "MacKinlay 1997" for a test statistic whose primary source is a different paper (Patell 1976, Boehmer et al. 1991, Kolari & Pynnönen 2010).
- `test-formula-consistency.R` does not exist after the method articles phase is complete.

**Phase to address:** Phase 2 (method articles) — formula correctness review is a required merge gate per article, not a milestone-end cleanup. One article → one formula review before the next article begins.

---

### Pitfall 8: Articles Accidentally Shipped in the CRAN Tarball via Missing .Rbuildignore Entry

**What goes wrong:**
The current `.Rbuildignore` correctly excludes `^data-raw$`, `^docs$`, `^pkgdown$`, and `^_pkgdown\.yml$`. But it does not yet contain an entry for `^vignettes/articles` — because that directory does not yet exist. When `vignettes/articles/` is created for the new articles, `R CMD build` will include it in the CRAN tarball unless `.Rbuildignore` is updated in the same commit.

The consequence: the CRAN tarball contains `.Rmd` files in `vignettes/articles/` that do not have a `VignetteEngine` declaration in their YAML (they are pkgdown-only articles, not CRAN vignettes). `R CMD check --as-cran` then emits `WARNING: vignette source file 'vignettes/articles/return-models.Rmd' without corresponding vignette builder`. A WARNING is CRAN-blocking.

If `vignettes/articles/data/` is used for purely article-local datasets, the same issue applies to that directory.

**Why it happens:**
`.Rbuildignore` entries are added reactively — when `R CMD check` complains — rather than proactively when the directory is created. The gap between "create the directory" and "run R CMD check" is typically days or weeks into the article-writing process.

**How to avoid:**
- The `.Rbuildignore` entry `^vignettes/articles` must be added in the same commit that creates the `vignettes/articles/` directory. Never as a follow-up commit.
- After adding the entry, immediately verify: `R CMD build . --no-build-vignettes && tar tzf EventStudy_*.tar.gz | grep articles` should return zero lines.
- If `vignettes/articles/data/` is used for article-local datasets, add `^vignettes/articles/data` as a separate explicit entry (or rely on `^vignettes/articles` to cover the whole subtree — verify with the tar check above).
- Add the tar check as a CI step in `R-CMD-check.yaml` or as a local pre-release checklist item.

**Warning signs:**
- `tar tzf EventStudy_*.tar.gz | grep articles` returns any hits.
- `R CMD check --as-cran` emits `W  vignette without corresponding vignette builder` after any new article is added.

**Phase to address:** Phase 1 (article infrastructure) — `.Rbuildignore` entry is a prerequisite to creating `vignettes/articles/`, not a cleanup task.

---

### Pitfall 9: Dataset Without man/ Documentation Triggers R CMD check WARNING

**What goes wrong:**
Any object saved to `data/` via `usethis::use_data()` must have a corresponding `man/*.Rd` documentation page, or `R CMD check --as-cran` emits `WARNING: "dataset 'xyz' is not documented"`. This is a WARNING (not a NOTE), which is CRAN-blocking. The existing `dieselgate` dataset presumably has its `man/dieselgate.Rd`; any new dataset added for gallery domains must have one too.

Additionally, if the man page exists but lacks `@format` (with field-level descriptions for every column), `@source` (with full URL and access date), or `@examples` (even a one-liner `data(newdataset)` suffices), `R CMD check` emits style-level NOTEs that CRAN reviewers flag.

**Why it happens:**
`usethis::use_data()` creates the `.rda` file but does not create the documentation stub. Authors write the man page last, after the article is done, and sometimes forget it entirely until `R CMD check` fails.

**How to avoid:**
- Create the roxygen documentation stub for a new dataset in the same commit as the `data-raw/` script and the `.rda` file. Required fields: `@name`, `@title`, `@description`, `@format` (one `\item` per column), `@source` (URL + access date), `@examples` (one line), `@docType data`.
- Use `devtools::document()` immediately after creating the stub to verify the `man/*.Rd` is generated correctly.
- Run `R CMD check --as-cran` locally after adding any new dataset before committing.
- The existing R-CMD-check.yaml CI matrix (ubuntu/macos/windows with `--as-cran`) provides the safety net, but local verification before pushing avoids wasted CI cycles.

**Warning signs:**
- `R CMD check` output shows `W  checking for unstated dependencies...` or `W  No documentation for...` after a `use_data()` call.
- `data/` contains an `.rda` file with no matching `R/<datasetname>.R` file containing a `#' @name` roxygen block.
- `devtools::document()` does not produce a `man/<datasetname>.Rd` file.

**Phase to address:** Phase 2 (dataset curation) — dataset documentation is a required artifact of dataset creation, not a separate step.

---

### Pitfall 10: Rendered Outputs Go Stale When the Package API Changes

**What goes wrong:**
Method articles execute real package code at build time (`eval = TRUE`). If a function's output format changes in a future version — a renamed tibble column, a changed `print()` method output, a new diagnostic warning — the article's rendered output becomes inconsistent with the surrounding prose. The prose says "the `ar` column contains..." but the rendered table shows the column is now called `abnormal_return`. The code still runs; the mismatch is only visible to a human reader.

This failure mode is not caught by `R CMD check`: the new articles are in `vignettes/articles/` (`.Rbuildignore`d) and are not rendered during CRAN's vignette check. The only feedback loop is the pkgdown CI job on push-to-main — which only catches broken code (errors), not semantically stale output (columns renamed but code still executes).

Compounding this: the gallery articles will reference real bundled datasets and real computed statistics. Any future API drift (e.g., `calculate_statistics()` renames a column) that the article's surrounding prose describes explicitly will create a live published inconsistency that users encounter.

**Why it happens:**
Documentation is written once and implicitly trusted to stay current. The only process that would catch drift is "update docs whenever API changes," which requires explicit policy enforcement — and it is easy to miss when the focus is on the code change.

**How to avoid:**
- For every article chunk that produces a named output (tibble columns, print output structure), add a `stopifnot()` assertion immediately after: `stopifnot("ar" %in% names(result), "car" %in% names(result))`. This converts API-drift bugs from invisible mismatches into build-breaking errors — the pkgdown CI job will fail loudly.
- Add a `tests/testthat/test-article-outputs.R` canary that re-runs the key computations from each article and asserts structural outputs (column names, object classes, statistic names). This runs during `R CMD check` and catches drift before the articles go stale on the deployed site.
- When the package API changes in a way that affects article outputs, treat "update affected articles" as a required subtask of the API change PR — not a follow-up.
- Never embed hardcoded numeric output values in article prose ("the t-statistic is -4.23"). Reference values programmatically via inline R (`r round(result$t_stat, 2)`) or annotate that the value depends on the seed and dataset.

**Warning signs:**
- `pkgdown::build_site()` succeeds but a rendered table's column names do not match what the prose describes.
- `NEWS.md` has an entry noting a renamed output column without a corresponding commit touching `vignettes/articles/`.
- No `tests/testthat/test-article-outputs.R` exists after the method articles phase is complete.
- An article chunk does not contain any `stopifnot()` assertions on the output it discusses.

**Phase to address:** Phase 2 (method articles) and Phase 3 (gallery) — add `stopifnot()` assertions per article as each is written. Phase 4 (integration) — add the canary test file.

---

### Pitfall 11: Content Duplication Between New Method Articles and Existing 18 CRAN Vignettes

**What goes wrong:**
The 18 existing CRAN vignettes already cover every method: introduction, result-extraction, diagnostics-validation, inference-robustness, factor-models-bhar, time-varying-models, modern-did-estimators, panel-event-study, intraday, synthetic-control, etc. The new method articles are meant to add conceptual depth with formulas, assumptions, and academic context — not to repeat the existing walkthrough. If method articles duplicate existing vignettes (same pipeline code, same data, same narrative structure), the site has redundant content that confuses users ("which should I read?") and doubles the maintenance burden: every API change must be updated in two places.

**Why it happens:**
Article authors naturally start from the existing vignette as a reference for what the function does, then re-explain it from scratch because that is faster than reading the primary literature. The result is a vignette wearing a formula costume.

**How to avoid:**
- Before writing any article, define a one-paragraph content brief that states: what statistical concept this article covers; what it explicitly defers to the corresponding CRAN vignette; what the reader should know after this article that they could not learn from the vignette alone.
- Cross-link rather than duplicate: method articles link to the corresponding vignette for usage details and vice versa.
- A method article should not contain the full `prepare_event_study() → fit_model() → calculate_statistics()` pipeline except as a minimal reproducible setup before the statistical point. If an article needs more than 20 lines of setup code, it has absorbed vignette content.
- Establish in `_pkgdown.yml` a separate "Learn" navbar section (distinct from the current "Articles" section that lists the vignettes) to visually reinforce the distinction between "how to use" and "why it works."

**Warning signs:**
- A method article draft contains a code block that is identical or near-identical to a block in the corresponding vignette.
- `wc -l vignettes/articles/return-models.Rmd` exceeds 500 lines (a sign it has absorbed vignette content).
- The method article uses the same `dieselgate` dataset in the same event window configuration that the existing `introduction.Rmd` vignette uses, producing identical output tables.

**Phase to address:** Phase 1 (planning) — write content briefs for all articles before writing any article. Use the briefs as acceptance criteria during Phase 2 review.

---

### Pitfall 12: Long pkgdown Build Time Blocks CI as Article Count Grows

**What goes wrong:**
The current pkgdown CI job builds the site synchronously. With 18 existing vignettes and 13 new articles (7 method + 6 gallery), each executing real code, the build can grow from an estimated 5–8 minutes to 25–40 minutes. A 40-minute CI feedback loop on every push-to-main makes iterative article development impractical. The GARCH and DCC-GARCH articles are the highest risk: `GARCHModel$new()$fit()` on a 300-day time series takes 10–30 seconds per firm. A gallery article with 4 firms could add 2–4 minutes per render.

**Why it happens:**
Each article is developed in isolation; the cumulative build time impact is not considered until all articles are written and CI visibly slows.

**How to avoid:**
- Benchmark each article's render time during development: `system.time(rmarkdown::render("vignettes/articles/myarticle.Rmd"))` must complete in under 60 seconds. Gate on this before the article is merged.
- For GARCH and DCC-GARCH articles: pre-fit the model object using a fixed seed and cache it as an `.rds` file in `vignettes/articles/data/`. Load the cached object in the article: `task <- readRDS("cached_garch_fit.rds")`. Update the cache only when the model implementation changes, not on every render.
- For bootstrap articles: cap `n_boot` at 99 (vs. 999 in production) — 10× faster. Add a comment: `# n_boot = 99 for article render speed; use n_boot = 999 in production`.
- For simulation articles: cap `n_sim` at 100 (vs. 1000 in production). Add the same explanatory comment.
- Target total pkgdown CI time under 15 minutes. If it exceeds this after Phase 3, audit article render times and add caching for the slowest articles.

**Warning signs:**
- The pkgdown CI job takes longer than 15 minutes.
- Any single article's local render time exceeds 90 seconds.
- A GARCH or DCC-GARCH article calls `GARCHModel$new()$fit()` inside a loop over multiple firms without a cached-result check.

**Phase to address:** Phase 2 (method articles) and Phase 3 (gallery) — enforce the 60-second render budget per article during writing; add caching for heavy computations in the same PR as the article, not as a follow-up optimization.

---

## Technical Debt Patterns

| Shortcut | Immediate Benefit | Long-term Cost | When Acceptable |
|----------|-------------------|----------------|-----------------|
| Writing formulas from memory without verifying against primary source | Faster article drafting | Incorrect formulas published in a statistics package's official documentation | Never — always verify against the primary paper |
| Using Yahoo Finance prices for all gallery datasets | Easy `tq_get()` calls; realistic data | Redistribution risk grows with each new dataset; potential CRAN challenge | One dataset (dieselgate) may be defensible as illustrative academic use; six is a systematic collection |
| Using plotly for all article figures | Demonstrates native package API; interactive charts | ~3.5 MB JS per page; blank-figure CI silent failures; slow site | Only for gallery pages where hover interactivity is the explicit pedagogical point |
| Deferring `.Rbuildignore` entry for `vignettes/articles/` | Less upfront setup | CRAN tarball includes article Rmd files; R CMD check WARNING about missing VignetteEngine | Never — add the entry in the same commit that creates the directory |
| Hardcoding numeric results in article prose | Easier to write | Values go stale when `data-raw/` is re-run; invisible mismatch with rendered output | Never — reference via inline R or annotate that the value is seed-fixed |
| Omitting `set.seed()` in stochastic article chunks | Less boilerplate | Non-deterministic rendered output; noisy CI diffs; gallery numbers change on every build | Never in any chunk with a random component |
| Placing gallery datasets in `data/` rather than `vignettes/articles/data/` | Standard `data()` access for article code | Tarball bloat; data documentation burden; CRAN installed-size NOTE | Only for datasets also used in CRAN-shipped vignettes or exported functions |
| Using `eval = FALSE` + pasted static output for all code chunks | Fast build; no CI failures | Docs diverge immediately from the package; stale output is undetectable | Acceptable only for LLM/API-key-dependent chunks — follow the ai-advisor.Rmd pattern |

---

## Integration Gotchas

| Integration | Common Mistake | Correct Approach |
|-------------|----------------|------------------|
| pkgdown + `bibliography:` | `bibliography: ../../references.bib` resolves from vignette directory locally but fails in pkgdown render from package root | Place `references.bib` in `vignettes/articles/` and use `bibliography: references.bib` (filename only, no path) |
| pkgdown + plotly | Using `plotly::ggplotly()` in every article chunk; pkgdown embeds full plotly.js per page | Use ggplot2 for method articles; allow plotly selectively in gallery, relying on pkgdown's asset deduplication |
| `data/` vs `vignettes/articles/data/` | Storing article-only datasets in `data/` because `usethis::use_data()` makes it easy | Purely article-only datasets go in `vignettes/articles/data/` with a `.Rbuildignore` entry; only datasets used in CRAN vignettes or exported functions go in `data/` |
| GitHub Actions pkgdown + Suggests packages | A package in `Suggests` that is used in an article is not installed by the default `setup-r-dependencies` step with `needs: website` | Add the package to `Config/Needs/website:` in DESCRIPTION, or add it to `extra-packages:` in the pkgdown workflow step |
| knitr + MathJax delimiters | Mixing `\(...\)` and `$...$` math delimiters in the same article — some pandoc versions reject one form silently | Use only `$...$` / `$$...$$` throughout all articles |
| `R CMD build` + `vignettes/articles/` | Creating the directory without a `.Rbuildignore` entry — article Rmds land in the tarball | Add `^vignettes/articles` to `.Rbuildignore` in the same commit that creates the directory; verify with `tar tzf *.tar.gz | grep articles` |
| testthat + article output assertions | `stopifnot()` assertions in article chunks do not run during `R CMD check` | Mirror key article computations in `tests/testthat/test-article-outputs.R` which does run in `R CMD check` |

---

## "Looks Done But Isn't" Checklist

- [ ] **`.Rbuildignore` entry:** `vignettes/articles/` created AND `^vignettes/articles` in `.Rbuildignore` in the same commit — verify with `R CMD build . --no-build-vignettes && tar tzf EventStudy_*.tar.gz | grep articles` returning empty.
- [ ] **Dataset documentation:** Every new `.rda` in `data/` has a `man/*.Rd` with `@format` field descriptions — verify with `R CMD check --as-cran` showing zero WARNING/NOTE about undocumented data.
- [ ] **Formula correctness:** Every formula in every method article verified against the primary literature source AND the package source implementation — not just "it looks right" during authoring.
- [ ] **Citation rendering:** Every `[@Key]` citation renders as an author-year citation — verify with `grep -r '\[@' docs/articles/` returning empty after every build.
- [ ] **Determinism:** Every article with a stochastic function has `set.seed()` immediately before it — verify by building the site twice and diffing the HTML.
- [ ] **Tarball data budget:** `tar tzf EventStudy_*.tar.gz | grep "^EventStudy/data/"` shows total `data/` ≤ 600 KB — verify after every new dataset is added.
- [ ] **Dataset licensing:** Every `data-raw/` script has a `meta$license_note` or equivalent field addressing redistribution — verify by reading the provenance comment block in each script.
- [ ] **Build time:** Every article renders in under 60 seconds locally — verify with `system.time(rmarkdown::render(...))` per article.
- [ ] **Math rendering:** Every article in `docs/articles/` renders formulas visually (not as raw `$...$`) — verify by opening each article in a browser and visually inspecting the first formula block.
- [ ] **Content non-duplication:** Every method article links to the corresponding CRAN vignette for API usage details rather than repeating the pipeline walkthrough — verify that no method article contains the full three-step pipeline without a "See vignette X" cross-reference.
- [ ] **Article output assertions:** Every article chunk producing a named output has at least one `stopifnot()` asserting column names or object class — verify by grep on the article source.
- [ ] **pkgdown build time:** Total CI pkgdown job completes in under 15 minutes — verify after Phase 3 is complete.

---

## Pitfall-to-Phase Mapping

| Pitfall | Prevention Phase | Verification |
|---------|------------------|--------------|
| Non-deterministic rendered output | Phase 1: canonical article template with seed + options block | Build site twice locally; diff `docs/articles/` — zero numeric diffs |
| plotly page weight and silent render failures | Phase 1: ggplot2-vs-plotly policy; Phase 2: enforce static in method articles; Phase 3: selective plotly in gallery | Article HTML size check; offline browser test |
| Dataset tarball bloat + installed-size NOTE | Phase 2: per-dataset size budget at creation time | `R CMD build` → `tar tzf` → data directory total ≤ 600 KB |
| Dataset redistribution licensing | Phase 2: source evaluation before writing any `data-raw/` script | Every `data-raw/` script has `meta$license_note`; `data-raw/DATA-SOURCES.md` exists |
| MathJax / LaTeX escaping failures | Phase 1: delimiter convention; smoke-test formula in template | `grep -r '\[@' docs/articles/` returns empty; visual inspection of first formula per article |
| Citation pipeline silent failure | Phase 1: `references.bib` setup + CI grep gate | `grep -r '\[@' docs/articles/` returns empty after every build |
| Subtly wrong statistical formulas | Phase 2: formula review gate before each article merges | Primary source + implementation cross-check sign-off; `test-formula-consistency.R` exists |
| Articles shipped in CRAN tarball | Phase 1: `.Rbuildignore` entry in first commit | `tar tzf *.tar.gz \| grep articles` returns empty |
| Dataset without man/ documentation | Phase 2: documentation as dataset creation prerequisite | `R CMD check --as-cran` zero WARNING on data documentation |
| Stale rendered outputs after API changes | Phase 2–3: `stopifnot()` assertions per article; Phase 4: canary test file | `R CMD check` fails if canary detects output column drift |
| Content duplication vs existing vignettes | Phase 1: content briefs per article before writing begins | Each article brief is an acceptance criterion; method articles contain no full pipeline without a vignette cross-reference |
| Long CI build time | Phase 2–3: 60-second render budget; caching for heavy models | pkgdown CI job under 15 minutes; no single article exceeds 90 seconds locally |

---

## Sources

- Direct inspection: `.github/workflows/pkgdown.yaml` — `new_process = FALSE`; no network guard in pkgdown step; `extra-packages: any::pkgdown, local::.`; missing `Config/Needs/website:` pattern
- Direct inspection: `DESCRIPTION` — `LazyData: true`; `plotly` in `Imports:`; Suggests list; current version 0.62.0
- Direct inspection: `.Rbuildignore` — current exclusions (`^data-raw$`, `^docs$`, `^pkgdown$`, `^_pkgdown\.yml$`); notably absent: `^vignettes/articles`
- Direct inspection: `data-raw/dieselgate.R` — Yahoo Finance provenance pattern; `meta$source` present; `meta$license_note` absent; the pattern to replicate and improve for new datasets
- Direct inspection: `data/dieselgate.rda` — 9.1 KB baseline dataset; the comparison point for new dataset size budgeting
- Direct inspection: 19 vignette files in `vignettes/` — existing `set.seed(42)` convention; `eval = FALSE` pattern for LLM/network chunks; `rmarkdown::html_vignette` output format; zero existing `bibliography:` YAML fields (citation pipeline untested in this project)
- Direct inspection: `_pkgdown.yml` — Bootstrap 5 template; no MathJax override; current articles nav structure; `articles:` sections covering all 18 existing vignettes
- Direct inspection: `R/single_event_test_statistics.R`, `R/multi_event_test_statistics.R` — ground-truth implementations that article formulas must match
- Package knowledge: CRAN installed-size NOTE threshold (~5 MB package / ~1 MB data subdir); pkgdown working directory for article rendering; pandoc `$...$` vs `\(...\)` delimiter compatibility; plotly.js self-contained embed size (~3.5 MB); Yahoo Finance ToS section 5 redistribution restriction; pkgdown asset deduplication behavior for htmlwidgets

---
*Pitfalls research for: v0.63.0 Documentation Depth — CRAN R package with CI-deployed pkgdown, rich method articles, worked-examples gallery, bundled datasets*
*Researched: 2026-09-05*
