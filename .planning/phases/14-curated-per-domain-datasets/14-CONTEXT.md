# Phase 14: Curated Per-Domain Datasets - Context

**Gathered:** 2026-09-05
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous)

<domain>
## Phase Boundary

Produce one curated, provenance-documented, placement-recorded dataset per
gallery domain that Phase 16 requires, so that every gallery worked example
is unblocked and proven end-to-end before any article prose is written.
Establish size, documentation, and licensing discipline on the first dataset
and replicate it for each subsequent one.

In scope:
- Selecting gallery domains (minimum 3, aligned to Phase 16's "earnings
  surprises, M&A announcements, regulatory/enforcement shocks" examples).
- Deciding per domain: real fetch vs. simulate_event_study() + set.seed().
- Deciding per dataset: data/ (CRAN-shipped .rda) vs. site-only
  vignettes/articles/data/ .rds vs. inline simulation (no file).
- Creating data-raw/DATA-SOURCES.md as a provenance registry (new file,
  excluded from CRAN tarball by existing ^data-raw$ .Rbuildignore rule).
- Writing reproducible data-raw/<dataset>.R scripts following dieselgate.R
  exactly (header provenance block, fetch, freeze, usethis::use_data or
  bzip2 save).
- Writing roxygen data docs in R/data-<dataset>.R with full @format /
  @source / @name / @usage / @docType data — 88-line dieselgate pattern.
- Verifying each dataset drives a valid prepare_event_study() -> fit_model()
  -> calculate_statistics() pipeline run before closing the phase.

Out of scope:
- Writing gallery article prose or worked-example Rmd files (Phase 16).
- Writing Methods article content (Phase 15).
- Any change to the 18 CRAN vignettes.
- Any change to _pkgdown.yml, _setup.Rmd, references.bib, or other infra
  delivered by Phase 13.

</domain>

<decisions>
## Implementation Decisions

**Both grey-area batches ACCEPTED by user (Accept all), 2026-09-05.**

### Gallery Domains and Dataset Strategy
- Three domains required by Phase 16 (ROADMAP): earnings surprises,
  M&A announcements, regulatory/enforcement shock.
- Regulatory shock: reuse the existing dieselgate dataset (already `data/`,
  already proven end-to-end). No new dataset needed for this domain.
- Earnings surprises: fetch a small real panel (3-5 firms, ~300 trading
  days) from Yahoo Finance under the same posture as dieselgate — bundled
  in data/ as a .rda with full roxygen doc.
- M&A announcements: generate inline via simulate_event_study(seed = N)
  inside the gallery article — no frozen file needed. The M&A domain's
  statistical story (clustered event dates, cross-sectional variation) is
  well-served by synthetic data, and simulation eliminates licensing risk.
- This yields one new real dataset (earnings), one inline simulation (M&A),
  and one dieselgate reuse (regulatory). Total new data/ additions: 1.
- Methods articles (Phase 15) reuse dieselgate + simulate — no new datasets.

### Placement Decision per Dataset
- Earnings dataset: data/ (CRAN-shipped .rda, usethis::use_data, bzip2
  compression, LazyData: true already set). Target <= 27 KB compressed
  (<= 3x dieselgate's 9.1 KB). Accessed via data() in gallery articles.
- M&A dataset: no file — simulate_event_study() called inline with a fixed
  seed inside the article chunk. No data-raw/ script needed beyond a comment
  in the article's setup chunk.
- Dieselgate: unchanged. The gallery article for the regulatory-shock domain
  calls data("dieselgate") — already available.

### Provenance Mechanism
- Create data-raw/DATA-SOURCES.md as a new lightweight registry table
  (one row per bundled dataset: name, source, access date, license note,
  script path, compressed size). Excluded from CRAN tarball by existing
  ^data-raw$ .Rbuildignore rule.
- Retrofit a dieselgate entry into DATA-SOURCES.md.
- Each new data-raw/<dataset>.R script carries a full header provenance
  block (same structure as data-raw/dieselgate.R lines 5-26): source,
  tickers, event, date range, access date, license note, reproduction
  command.
- Roxygen doc in R/data-<dataset>.R is mandatory for every data/-shipped
  dataset: @format (describe-list), @source, @name, @usage data(<name>),
  @docType data, @keywords datasets. Pattern: R/data-dieselgate.R.

### Size and Documentation Discipline
- Compressed size cap: <= 27 KB per new data/ dataset (soft target, 3x
  dieselgate). If a candidate dataset exceeds this, switch to site-only
  .rds or simulation.
- Compression: bzip2 (usethis::use_data default; explicit in fallback path
  data-raw/dieselgate.R line 102).
- LazyData: true is already set in DESCRIPTION (line 71) — no change needed.
- Documentation completeness check: every data/ dataset must pass
  R CMD check --as-cran with no NOTE about missing documentation before
  the phase is closed.
- End-to-end proof required: each dataset must run the full pipeline and
  produce finite statistics in a scratch script before phase close.

### Licensing Posture
- Yahoo Finance daily adjusted prices, small illustrative sample, academic /
  demonstration use only — same posture as dieselgate, already accepted by
  CRAN. Acceptable for new price datasets.
- Factor data (if needed for a multi-factor model example): Kenneth French
  Data Library (freely redistributable), not Yahoo Finance.
- Simulated data: no licensing concern.
- Each dataset's @source roxygen tag and data-raw/ header must state:
  source name, access date, and "small illustrative sample bundled for
  academic / demonstration use only."

### Claude's Discretion
- Exact firm tickers and earnings quarter chosen for the earnings dataset,
  provided the panel is small (3-5 firms), compresses <= 27 KB, and tells a
  clear abnormal-return story.
- Exact simulate_event_study() parameters for the M&A inline example.
- Column layout of the DATA-SOURCES.md registry table.

</decisions>

<code_context>
## Existing Code Insights

### The Dieselgate Pattern (canonical reference)
- data-raw/dieselgate.R — 111-line provenance script: header block (lines
  5-26), fetch via download_stock_data() (lines 36-56), assemble frozen
  list (lines 62-95), usethis::use_data() with bzip2 fallback (lines
  98-103).
- R/data-dieselgate.R — 88-line roxygen data doc: @format describe-list
  (lines 22-46), @details (lines 48-58), @source (lines 60-63), @examples
  \donttest{} (lines 65-82), @docType data / @keywords datasets / @name /
  @usage (lines 84-87), bare string "dieselgate" sentinel (line 88).
- data/dieselgate.rda — 9.1 KB bzip2-compressed frozen snapshot.
- DATA-SOURCES.md does not yet exist in the repo; Phase 14 creates it.

### Simulation Capability
- R/simulation.R simulate_event_study() (lines 32-80): fully self-contained,
  zero-network, accepts seed parameter, produces a valid EventStudyTask
  internally. All DGP parameters are configurable (alpha, beta, sigma_firm,
  sigma_market). Suitable for any domain where the "story" does not require
  authentic provenance.

### Article Infrastructure (Phase 13 outputs)
- vignettes/articles/_setup.Rmd: set.seed(42), options(scipen, digits),
  knitr opts — all articles inherit this.
- vignettes/articles/_article-skeleton.Rmd line 51: data("dieselgate") as
  placeholder — implies data() is the expected article access pattern for
  gallery-quality examples.
- ^vignettes/articles already in .Rbuildignore — any data/ subdirectory
  created under vignettes/articles/ is automatically excluded from tarball.
  vignettes/articles/data/ does not exist yet; no new .Rbuildignore rule
  needed if created there.

### .Rbuildignore State
- ^data-raw$ already excluded — DATA-SOURCES.md placed in data-raw/ is
  automatically excluded.
- ^vignettes/articles already excluded (Phase 13, CR-01 fix dropped the $) —
  site-only .rds in vignettes/articles/data/ is automatically excluded
  without new rules.
- CRAN tarball currently contains only data/dieselgate.rda (9.1 KB) as data.

### Network / Reproducibility Risk (carry to planner)
- The earnings dataset requires a ONE-TIME real Yahoo Finance fetch at
  dataset-build time via download_stock_data() (same as dieselgate.R). The
  fetch happens once; the frozen .rda ships. If the execution environment
  has no network, the executor must fall back gracefully (bundle a
  pre-frozen snapshot or flag a blocker) — never ship an empty/placeholder
  dataset. Mirrors the BUILD-02 offline-reproducibility discipline.

</code_context>

<integration>
## Integration Points

- data/ — one new .rda added (earnings surprises dataset); dieselgate.rda
  unchanged.
- data-raw/ — one new <dataset>.R provenance script + new DATA-SOURCES.md
  registry (includes dieselgate retrospective entry).
- R/ — one new data-<dataset>.R roxygen doc file.
- vignettes/articles/ — no new files in Phase 14 (article Rmds are Phase
  16); datasets are made available here indirectly via data() and
  simulate_event_study().
- NAMESPACE / DESCRIPTION — no changes needed (LazyData: true already set;
  data objects are not exported via NAMESPACE).
- _pkgdown.yml — Data & datasets Reference group already exists (from Phase
  11); new dataset will appear there automatically once its roxygen @name
  is present.

</integration>

<deferred>
## Deferred

- Gallery article prose (Rmd files for the worked examples) — Phase 16.
- Methods article content — Phase 15.
- Any _pkgdown.yml structural changes beyond what Phase 13 delivered.
- A fourth or fifth gallery domain (beyond the three named in ROADMAP
  Phase 16) — not in scope for Phase 14.

</deferred>
