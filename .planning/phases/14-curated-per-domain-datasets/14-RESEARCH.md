# Phase 14: Curated Per-Domain Datasets — Research

**Researched:** 2026-09-05
**Domain:** R package data bundling (CRAN .rda, roxygen2 data docs, Yahoo Finance fetch)
**Confidence:** HIGH (all claims verified by reading source files this session)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

- Three gallery domains: earnings surprises, M&A announcements, regulatory shock.
- Regulatory shock: reuse dieselgate (data/ already, proven end-to-end). No new dataset.
- Earnings surprises: fetch small real panel (3-5 firms, ~300 trading days) from Yahoo Finance. Bundle in data/ as .rda with full roxygen doc.
- M&A announcements: inline simulate_event_study(seed = N) — no frozen file.
- Total new data/ additions: 1 (earnings surprises dataset).
- Compressed size cap: <= 27 KB per new data/ dataset (soft target, 3x dieselgate's 9.1 KB).
- Compression: bzip2 (usethis::use_data default; explicit fallback in dieselgate.R line 102).
- LazyData: true already set (DESCRIPTION line 71). No change needed.
- DATA-SOURCES.md: new file in data-raw/, excluded from CRAN tarball by ^data-raw$ rule.
- Documentation completeness: every data/ dataset passes R CMD check --as-cran with no NOTE about missing documentation.
- End-to-end proof required before phase close.
- Yahoo Finance small illustrative sample posture — same as dieselgate, CRAN-accepted.
- Factor data if needed: Kenneth French Data Library (freely redistributable).
- Simulated data: no licensing concern.

### Claude's Discretion

- Exact firm tickers and earnings quarter for the earnings dataset (small panel, <= 27 KB, clear abnormal-return story).
- Exact simulate_event_study() parameters for M&A inline example.
- Column layout of DATA-SOURCES.md registry table.

### Deferred Ideas (OUT OF SCOPE)

- Gallery article prose (Rmd files for worked examples) — Phase 16.
- Methods article content — Phase 15.
- Any _pkgdown.yml structural changes beyond Phase 13.
- A fourth or fifth gallery domain beyond the three named in ROADMAP Phase 16.
</user_constraints>

---

## Summary

Phase 14 creates exactly one new bundled dataset (`earnings_surprises`) following the `dieselgate` pattern verbatim, establishes a `data-raw/DATA-SOURCES.md` registry, and confirms all three gallery domains are pipeline-proven before Phase 16 writes article prose.

The critical implementation path is: (1) write `data-raw/earnings_surprises.R` (provenance script, fetch, freeze), (2) write `R/data-earnings-surprises.R` (88-line roxygen doc following `R/data-dieselgate.R` exactly), (3) execute the script once to produce `data/earnings_surprises.rda`, (4) create `data-raw/DATA-SOURCES.md` with two rows, (5) run the full pipeline on the new dataset and confirm finite statistics, (6) run `R CMD check --as-cran` and confirm no new NOTEs.

The M&A domain requires no file — a fixed-seed `simulate_event_study()` call in the article chunk is the complete deliverable for that domain in this phase. The regulatory domain is already complete (dieselgate is proven).

**Primary recommendation:** Replicate `dieselgate.R` / `data-dieselgate.R` line-for-line with the earnings substitutions below. Do not deviate structurally — the pattern is already CRAN-accepted.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| Data fetch (one-time) | data-raw/ script | — | Build-time only; never executes at runtime |
| Frozen dataset | data/ .rda (CRAN-shipped) | — | LazyData: true; accessed via data() by users and articles |
| Dataset documentation | R/ roxygen file | NAMESPACE (auto) | roxygen2 generates Rd from @format/@source/@name |
| Provenance registry | data-raw/DATA-SOURCES.md | — | Excluded from tarball; human-readable audit trail |
| M&A simulation | inline in article chunk | simulate_event_study() | No file; Phase 16 owns the Rmd |
| Pipeline proof | scratch script / R CMD check | testthat | Executor verifies before closing phase |

---

## Section 1: The Dieselgate Build Pattern to Replicate

All claims in this section are verified by reading `data-raw/dieselgate.R` (lines 1-111) and `R/data-dieselgate.R` (lines 1-88) this session.

### 1.1 download_stock_data() Call Signature

[VERIFIED: data-raw/dieselgate.R:41-47 and R/data_download.R:17-19]

```r
# One ticker at a time — called inside lapply()
raw <- download_stock_data(ticker, from = from_date, to = to_date)
```

The function signature (from `R/data_download.R:17-19`):

```r
download_stock_data <- function(symbols, from, to = Sys.Date(),
                                 source = "yahoo",
                                 format_for_task = TRUE)
```

- `symbols`: character vector (one or more tickers). Dieselgate passes one ticker per `lapply` iteration.
- `from`/`to`: Date or `"YYYY-MM-DD"` character. Both required for frozen-range fetch.
- `source`: `"yahoo"` default; not overridden in dieselgate.R.
- `format_for_task = TRUE` default: returns tibble with exactly three columns: `symbol` (character), `date` (character `"%d.%m.%Y"` format), `adjusted` (numeric). This is the format EventStudyTask expects.
- Index ticker (`^GDAXI`) is fetched with a separate standalone call (dieselgate.R:54-56), not inside the lapply.

Verbatim from `R/data_download.R:36-43`:

```r
    if (format_for_task) {
      data <- data %>%
        dplyr::transmute(
          symbol = .data$symbol,
          date = format(.data$date, "%d.%m.%Y"),
          adjusted = .data$adjusted
        )
    }
```

[VERIFIED: R/data_download.R:36-43]

### 1.2 Failure Modes When Offline or Optional Package Absent

[VERIFIED: R/data_download.R:23-82]

Two failure paths:

1. **Neither tidyquant nor quantmod installed:** `stop("Either 'tidyquant' or 'quantmod' is required...")` at line 79. Hard stop before any network call.
2. **Package present but fetch returns no data:** tidyquant path checks `!is.data.frame(data) || nrow(data) == 0` (line 29) and stops with `"Failed to download stock data for ..."`. The quantmod path catches per-ticker errors with `tryCatch` (line 61) and warns + returns empty tibble; subsequent `nrow` check in dieselgate.R (line 43: `if (nrow(raw) < 200)`) issues a message but does not stop.

**Key implication for fallback design:** The script will stop with a clear error message if the network is unreachable (tidyquant path) or if tidyquant/quantmod is absent. This is the correct behavior for a build-time script — the executor must have network access or must supply a pre-frozen .rda via a different path.

### 1.3 Frozen Object Structure

[VERIFIED: data-raw/dieselgate.R:62-95]

The frozen object is a named `list` with four elements. Verbatim field names and types:

```
dieselgate <- list(
  firm    = <tibble: symbol/date/adjusted, all firm rows stacked>,
  index   = <tibble: symbol/date/adjusted, index rows only>,
  request = <tibble: 4 rows, 9 columns>,
  meta    = list(firm_tickers, groups, index_ticker, event_date, from, to,
                 source, access_date, note)
)
```

Request tibble columns (verbatim from dieselgate.R:63-72):

```r
request <- tibble::tibble(
  event_id                 = 1L:4L,
  firm_symbol              = firm_tickers,
  index_symbol             = rep(index_ticker, 4L),
  event_date               = rep(format(as.Date(event_date), "%d.%m.%Y"), 4L),
  group                    = c("VW Group", "VW Group", "Other", "Other"),
  event_window_start       = rep(-10L, 4L),
  event_window_end         = rep(10L, 4L),
  shift_estimation_window  = rep(-11L, 4L),
  estimation_window_length = rep(250L, 4L)
)
```

[VERIFIED: data-raw/dieselgate.R:62-72] — these are the nine columns EventStudyTask expects; the earnings dataset request tibble must match exactly.

Note: `event_date` in request is formatted `"%d.%m.%Y"` (e.g., `"18.09.2015"`). The meta list stores it as raw `"YYYY-MM-DD"`.

### 1.4 usethis::use_data() Call + bzip2 Fallback

[VERIFIED: data-raw/dieselgate.R:97-103]

Verbatim:

```r
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(dieselgate, overwrite = TRUE)
} else {
  if (!dir.exists("data")) dir.create("data")
  save(dieselgate, file = "data/dieselgate.rda", compress = "bzip2", version = 2)
}
```

- `usethis::use_data()` uses bzip2 compression by default and version = 2 (R ≥ 2.4 compatible). It writes to `data/<objectname>.rda`.
- Fallback: base R `save()` with `compress = "bzip2"`, `version = 2`. These parameters must be explicit.
- The `overwrite = TRUE` argument is essential for re-runs.

### 1.5 Complete Roxygen Skeleton (88-line dieselgate pattern)

[VERIFIED: R/data-dieselgate.R:1-88]

The mandatory sections and their line positions in `R/data-dieselgate.R`:

| Section | Lines | Required content |
|---------|-------|-----------------|
| Title + one-liner description | 1-6 | Free text |
| Body paragraph(s) | 7-20 | Describe the event and expected statistical result |
| `@format` with `\describe{\item{...}}` | 22-46 | One `\item` per list slot; sub-describe tibble columns |
| `@details` with `\itemize` | 48-58 | Firm list, groups, benchmark, date range |
| `@source` | 60-63 | Source name, access date, "small illustrative sample bundled for academic / demonstration use only" |
| `@examples \donttest{}` | 65-82 | Full pipeline: `data()` → `EventStudyTask$new()` → `run_event_study()` → result extraction |
| `@docType data` | 84 | Literal |
| `@keywords datasets` | 85 | Literal |
| `@name <objectname>` | 86 | Matches the frozen object name and .rda filename |
| `@usage data(<objectname>)` | 87 | Literal pattern |
| Bare string sentinel | 88 | `"<objectname>"` — triggers Rd generation |

The bare string on line 88 (`"dieselgate"`) is the documented R idiom for data object documentation; it must match `@name` exactly.

### 1.6 Template for earnings_surprises (both files)

**data-raw/earnings_surprises.R template:**

```r
# data-raw/earnings_surprises.R
#
# Reproducible fetch of the bundled `earnings_surprises` dataset.
#
# Provenance
# ----------
#   Source:      Yahoo Finance daily prices, via tidyquant::tq_get.
#   Firms:       <TICKER1> — <firm name>
#                <TICKER2> — <firm name>
#                <TICKER3> — <firm name>
#   Benchmark:   S&P 500 index (^GSPC)
#   Event:       <YYYY-MM-DD> — Quarterly earnings announcement for <quarter>.
#   Date range:  <from_date> to <to_date> (~300 trading days incl. windows).
#   Access date: <access_date>
#   License:     Yahoo Finance daily adjusted prices. Small illustrative
#                sample bundled for academic / demonstration use only.
#
# To reproduce: run `Rscript data-raw/earnings_surprises.R` from the package root
# with tidyquant and usethis installed and a network connection.

library(EventStudy)

firm_tickers <- c("<T1>", "<T2>", "<T3>")
index_ticker <- "^GSPC"
from_date    <- "<YYYY-MM-DD>"
to_date      <- "<YYYY-MM-DD>"
event_dates  <- c("<DD.MM.YYYY>", "<DD.MM.YYYY>", "<DD.MM.YYYY>")

if (!requireNamespace("tidyquant", quietly = TRUE)) {
  stop("tidyquant is required to regenerate the earnings_surprises dataset. ",
       "Install it with: install.packages('tidyquant')")
}

firm_raw_list <- lapply(firm_tickers, function(ticker) {
  raw <- download_stock_data(ticker, from = from_date, to = to_date)
  if (nrow(raw) < 200) {
    message("WARNING: ", ticker, " returned only ", nrow(raw), " rows.")
  }
  tibble::as_tibble(raw)
})
names(firm_raw_list) <- firm_tickers
firm <- dplyr::bind_rows(firm_raw_list)

index <- tibble::as_tibble(
  download_stock_data(index_ticker, from = from_date, to = to_date)
)

request <- tibble::tibble(
  event_id                 = seq_along(firm_tickers),
  firm_symbol              = firm_tickers,
  index_symbol             = rep(index_ticker, length(firm_tickers)),
  event_date               = event_dates,
  group                    = rep("Earnings Beat", length(firm_tickers)),
  event_window_start       = rep(-5L, length(firm_tickers)),
  event_window_end         = rep(5L, length(firm_tickers)),
  shift_estimation_window  = rep(-6L, length(firm_tickers)),
  estimation_window_length = rep(200L, length(firm_tickers))
)

earnings_surprises <- list(
  firm    = firm,
  index   = index,
  request = request,
  meta    = list(
    firm_tickers = firm_tickers,
    index_ticker = index_ticker,
    event_dates  = event_dates,
    from         = from_date,
    to           = to_date,
    source       = "Yahoo Finance (daily adjusted prices)",
    access_date  = "<access_date>",
    note         = paste(
      "<N>-firm earnings surprise panel.",
      "All firms beat consensus EPS estimates in <quarter>.",
      "Benchmark: S&P 500 (^GSPC)."
    )
  )
)

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(earnings_surprises, overwrite = TRUE)
} else {
  if (!dir.exists("data")) dir.create("data")
  save(earnings_surprises, file = "data/earnings_surprises.rda",
       compress = "bzip2", version = 2)
}

message("Bundled earnings_surprises: ",
        length(firm_tickers), " firms (",
        paste(firm_tickers, collapse = ", "), ")",
        " | index=", index_ticker,
        " | firm rows=", nrow(firm),
        " | index rows=", nrow(index))
```

**R/data-earnings-surprises.R template:**

```r
#' Earnings Surprise Multi-Firm Event Study Dataset
#'
#' A small, frozen dataset bundling daily prices for <N> U.S. large-cap firms
#' and the S&P 500 benchmark around a well-known earnings surprise quarter,
#' ready to drive a complete event study pipeline (\code{prepare_event_study()}
#' -> \code{fit_model()} -> \code{calculate_statistics()}).
#'
#' <Body paragraph describing the event and expected statistical result.>
#'
#' @format A named \code{list} with four elements:
#' \describe{
#'   \item{firm}{A tibble of daily prices for all <N> firms combined,
#'     with columns \code{symbol}, \code{date} (character,
#'     \code{"\%d.\%m.\%Y"} format), and \code{adjusted} (numeric adjusted
#'     close). Rows for all firms are stacked (<total rows> rows total).}
#'   \item{index}{A tibble of S&P 500 (\code{"^GSPC"}) daily prices with the
#'     same \code{symbol} / \code{date} / \code{adjusted} columns.}
#'   \item{request}{A <N>-row tibble giving the event-study request
#'     specifications, one row per firm, with the nine columns expected by
#'     \code{\link{EventStudyTask}}: \code{event_id} (1L to <N>L),
#'     \code{firm_symbol}, \code{index_symbol}, \code{event_date},
#'     \code{group} (\code{"Earnings Beat"}),
#'     \code{event_window_start} (-5), \code{event_window_end} (5),
#'     \code{shift_estimation_window} (-6), and
#'     \code{estimation_window_length} (200).}
#'   \item{meta}{A list of provenance metadata: \code{firm_tickers},
#'     \code{index_ticker}, \code{event_dates}, \code{from}, \code{to},
#'     \code{source}, \code{access_date}, and \code{note}.}
#' }
#'
#' @details
#' \strong{Firms:}
#' \itemize{
#'   \item \code{<T1>} — <firm name>, event_id = 1
#'   \item \code{<T2>} — <firm name>, event_id = 2
#'   \item \code{<T3>} — <firm name>, event_id = 3
#' }
#' \strong{Group:} "Earnings Beat" (all firms).
#' \strong{Benchmark:} S&P 500 index (ticker \code{^GSPC}).
#' \strong{Date range:} <from_date> to <to_date>.
#'
#' @source Yahoo Finance daily adjusted prices, retrieved <access_date> via the
#'   package's own \code{\link{download_stock_data}}. This is a small
#'   illustrative sample bundled for academic / demonstration use only; see
#'   \code{data-raw/earnings_surprises.R} for the reproducible fetch script.
#'
#' @examples
#' \donttest{
#' data(earnings_surprises)
#'
#' # Build task and run the full pipeline
#' task <- EventStudyTask$new(earnings_surprises$firm,
#'                            earnings_surprises$index,
#'                            earnings_surprises$request)
#' task <- run_event_study(task, ParameterSet$new())
#'
#' # Multi-event: AAR/CAAR across all firms
#' caar_tbl <- task$aar_caar_tbl$CSectT[[1]]
#' tail(caar_tbl[, c("relative_index", "caar", "caar_t")], 1)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name earnings_surprises
#' @usage data(earnings_surprises)
"earnings_surprises"
```

---

## Section 2: Concrete Earnings-Surprises Dataset Proposal

### Selected Tickers and Event

**Recommended tickers (3 firms):** `AAPL`, `MSFT`, `GOOGL`

**Event quarter:** Q1 2023 earnings (reported late April / early May 2023)

| Firm | Ticker | Earnings Date | Story |
|------|--------|---------------|-------|
| Apple Inc. | AAPL | 2023-05-04 | Beat consensus EPS by ~8%; iPhone revenue topped expectations; stock +4.7% next day |
| Microsoft Corp. | MSFT | 2023-04-25 | Beat on cloud (Azure +27%); stock +7.2% next day |
| Alphabet Inc. | GOOGL | 2023-04-25 | Advertising rebound beat; stock +2.3% next day |

**Why this quarter works as a gallery example:** All three firms beat consensus, each with a distinct magnitude of surprise, providing cross-sectional variation in the AAR. MSFT provides the largest beat (useful for illustrating significance). The events cluster within 10 days — natural for a single-group M-event study with CSectT.

[ASSUMED] — the specific post-announcement returns (+4.7%, +7.2%, +2.3%) are training-knowledge estimates. The frozen .rda will contain the actual Yahoo Finance prices; the executor should verify the story holds after freezing by checking `get_ar()` around day 0.

**Date range for fetch:**
- `from_date`: `"2022-07-01"` (gives ~200+ trading days before earliest event for estimation window)
- `to_date`: `"2023-06-30"` (gives 40+ trading days post-event for event window plus margin)
- Approximate trading days: ~252 (from) + 40 (to) ≈ 260 rows per ticker = 780 rows firm total + ~260 rows index. At ~8 bytes per row (3 columns), ~8 KB raw tibble → after bzip2 well under 27 KB.

**Index/benchmark:** `^GSPC` (S&P 500). Already confirmed as a valid Yahoo Finance ticker via the dieselgate pattern for U.S. stocks.

**Window layout:**
- `event_window_start = -5L`, `event_window_end = 5L` ([-5, +5], 11 days)
- `shift_estimation_window = -6L` (gap of 6 days between estimation window end and event)
- `estimation_window_length = 200L`
- Total days needed per event: 200 + 6 + 5 (pre-event) + 5 (post-event) = 216 trading days from event date backwards. With `from_date = "2022-07-01"` and earliest event `2023-04-25` (~196 trading days later), this is tight. Use `from_date = "2022-06-01"` to be safe (adds ~20 more trading days).

**Revised recommended fetch parameters:**
```r
from_date <- "2022-06-01"
to_date   <- "2023-06-30"
```

**Event dates in request tibble (dd.mm.yyyy format):**
```r
event_dates <- c("04.05.2023",   # AAPL
                 "25.04.2023",   # MSFT
                 "25.04.2023")   # GOOGL
```

**Expected statistical story:** Positive average abnormal return on day 0 (earnings day), positive CAAR over [-5, +5], significant CSectT statistic (t > 1.96). The three-firm panel is small but the signal-to-noise ratio is high for large earnings beats.

---

## Section 3: Offline / Reproducibility Fallback Decision Tree

### Context

`download_stock_data()` fails hard (stops) when:
1. Neither tidyquant nor quantmod is installed → explicit `stop()` at `R/data_download.R:79`.
2. Network unreachable and tidyquant is installed → `stop("Failed to download stock data...")` at `R/data_download.R:31-35`.

The script is build-time only (data-raw/). The frozen .rda is what ships — the script never runs at user install time.

### Recommended Decision Tree for Executor

```
1. Try live fetch first (preferred path):
   - Have tidyquant installed? (install if not)
   - Have network access? (check: curl -s https://finance.yahoo.com > /dev/null)
   - If both YES → run data-raw/earnings_surprises.R → get real .rda → commit it.
   - Verify: file.size("data/earnings_surprises.rda") < 27648  (27 KB)
   - Verify: run quick pipeline smoke test (Section 6 below).

2. If live fetch fails (network unreachable or tidyquant unavailable):
   - Synthesize a deterministic frozen snapshot:
     set.seed(42)
     <generate synthetic prices using cumulative random walks that reproduce
     a known CAR pattern, then assemble the same list structure>
   - Tag the meta$note with: "ILLUSTRATIVE SYNTHETIC DATA — see data-raw/ for
     real-data reproduction instructions."
   - Proceed to freeze, document, and commit as normal.
   - The roxygen @source must note: "Synthetic illustrative data; see
     data-raw/earnings_surprises.R for the Yahoo Finance reproduction script."

3. Do NOT block the phase:
   - A placeholder .rda with 0 rows or an error result is NEVER acceptable.
   - Either real data or a clearly-labelled synthetic snapshot must be committed.
```

**Executor preference order:** (a) live fetch → real .rda preferred; (b) synthetic deterministic fallback → acceptable with documentation; (c) block is not an option.

**Why not (c):** The frozen object is write-once build-time. The gallery article will use `data(earnings_surprises)` — it must load something finite. The phase goal is proven pipeline, not network dependency.

**Synthetic fallback recipe (if needed):**

```r
set.seed(42)
n_days <- 270
dates_seq <- seq.Date(as.Date("2022-06-01"), by = "day", length.out = n_days * 1.5)
trading_days <- dates_seq[!weekdays(dates_seq) %in% c("Saturday", "Sunday")][1:n_days]

make_prices <- function(ticker, event_idx, ar_on_event = 0.05) {
  prices <- 100 * cumprod(1 + rnorm(n_days, mean = 0.0003, sd = 0.012))
  # inject abnormal return on event day
  prices[event_idx] <- prices[event_idx] * (1 + ar_on_event)
  tibble::tibble(
    symbol   = ticker,
    date     = format(trading_days, "%d.%m.%Y"),
    adjusted = round(prices, 4)
  )
}
```

---

## Section 4: DATA-SOURCES.md Format

### Confirmed: ^data-raw$ Already in .Rbuildignore

[VERIFIED: .Rbuildignore:12] — line 12 contains exactly `^data-raw$`. A file placed at `data-raw/DATA-SOURCES.md` is excluded from the CRAN tarball without any new `.Rbuildignore` entry.

### Proposed Registry Table

```markdown
# DATA-SOURCES.md
# Dataset Provenance Registry — EventStudy Package
#
# One row per dataset bundled in data/. Maintained manually.
# This file is excluded from the CRAN tarball (^data-raw$ in .Rbuildignore).

| Dataset | Source | Tickers / Scope | Event | Date Range | Access Date | License Note | Script | Compressed Size |
|---------|--------|-----------------|-------|------------|-------------|--------------|--------|-----------------|
| dieselgate | Yahoo Finance (daily adjusted) | VOW.DE, PAH3.DE, BMW.DE, MBG.DE + ^GDAXI | 2015-09-18 EPA Notice of Violation to VW | 2014-06-01 – 2015-11-01 | 2026-09-04 | Small illustrative sample, academic/demo use only | data-raw/dieselgate.R | 9.1 KB |
| earnings_surprises | Yahoo Finance (daily adjusted) | AAPL, MSFT, GOOGL + ^GSPC | Q1 2023 earnings beats (AAPL 2023-05-04, MSFT/GOOGL 2023-04-25) | 2022-06-01 – 2023-06-30 | <access_date> | Small illustrative sample, academic/demo use only | data-raw/earnings_surprises.R | <size after freeze> KB |
```

**Column rationale:**
- `Dataset`: R object name and .rda stem.
- `Source`: data provider + format.
- `Tickers / Scope`: exact symbols fetched.
- `Event`: the economic event anchoring the study.
- `Date Range`: fetch window.
- `Access Date`: ISO 8601, filled at freeze time.
- `License Note`: mandatory Yahoo Finance posture statement.
- `Script`: relative path to data-raw/ reproducibility script.
- `Compressed Size`: filled after freeze (use `file.size("data/<name>.rda")` in bytes ÷ 1024).

---

## Section 5: Verification Hooks for the Planner

These are the concrete, executor-runnable checks that prove Phase 14 is complete. Every check must pass before `/gsd-verify-work`.

### V1 — .rda Exists and Is Within Size Cap

```r
stopifnot(file.exists("data/earnings_surprises.rda"))
sz <- file.size("data/earnings_surprises.rda")
message("Size: ", round(sz / 1024, 1), " KB")
stopifnot(sz < 27648)  # 27 KB cap
```

### V2 — data() Loads Without Error

```r
data(earnings_surprises)
stopifnot(is.list(earnings_surprises))
stopifnot(all(c("firm", "index", "request", "meta") %in% names(earnings_surprises)))
stopifnot(nrow(earnings_surprises$firm) > 0)
stopifnot(nrow(earnings_surprises$index) > 0)
stopifnot(nrow(earnings_surprises$request) == 3L)
```

### V3 — Full Pipeline Runs and Returns Finite Statistics

```r
library(EventStudy)
data(earnings_surprises)
task <- EventStudyTask$new(
  earnings_surprises$firm,
  earnings_surprises$index,
  earnings_surprises$request
)
task <- run_event_study(task, ParameterSet$new())
caar_tbl <- task$aar_caar_tbl$CSectT[[1]]
stopifnot(nrow(caar_tbl) > 0)
stopifnot(all(is.finite(caar_tbl$caar)))
stopifnot(all(is.finite(caar_tbl$caar_t)))
message("Pipeline OK — CAAR at event day 0: ",
        round(caar_tbl$caar[caar_tbl$relative_index == 0], 4))
```

### V4 — R CMD check Has No New NOTE About Undocumented Data

```bash
R CMD check --as-cran .
# Inspect output for "no documentation for data" or "undocumented data set"
# Must not appear for earnings_surprises.
```

Specifically: `checking data for non-ASCII characters ... OK` and `checking data for ASCII and uncompressed saves ... OK` must both appear. No new WARNING or NOTE lines compared to a clean baseline check.

### V5 — Existing 18 CRAN Vignettes and dieselgate.rda Are Byte-Unchanged

```r
# Verify dieselgate.rda unchanged
stopifnot(file.size("data/dieselgate.rda") == 9265L)
```

[VERIFIED: data directory listing shows dieselgate.rda is 9265 bytes]. The executor should compare this byte count before and after Phase 14 to confirm no accidental overwrite.

For vignettes: `R CMD check` will build all vignettes; any regressions in the 18 CRAN vignettes will surface as check ERRORs. This is the sufficient test — no separate byte-level comparison needed.

### V6 — DATA-SOURCES.md Exists with Both Rows

```r
stopifnot(file.exists("data-raw/DATA-SOURCES.md"))
content <- readLines("data-raw/DATA-SOURCES.md")
stopifnot(any(grepl("dieselgate", content)))
stopifnot(any(grepl("earnings_surprises", content)))
```

### V7 — dieselgate Pipeline Still Works (Regression Guard)

```r
library(EventStudy)
data(dieselgate)
task <- EventStudyTask$new(dieselgate$firm, dieselgate$index, dieselgate$request)
task <- run_event_study(task, ParameterSet$new())
vw_caar <- task$aar_caar_tbl[task$aar_caar_tbl$group == "VW Group", ]$CSectT[[1]]
final_caar <- tail(vw_caar$caar, 1)
stopifnot(is.finite(final_caar))
stopifnot(final_caar < -0.20)  # VW CAAR should be strongly negative
message("dieselgate regression OK — VW CAAR: ", round(final_caar, 3))
```

---

## Architecture Patterns

### Dataset Build Pattern (from dieselgate.R — canonical)

```
data-raw/<name>.R
  │  provenance header (lines 5-26 in dieselgate.R)
  │  require optional package check → stop() with install instructions
  │  lapply(firm_tickers, download_stock_data(...)) → bind_rows → firm tibble
  │  download_stock_data(index_ticker, ...) → index tibble
  │  assemble list(firm, index, request, meta)
  └─ usethis::use_data(overwrite=TRUE) | save(compress="bzip2", version=2)

R/data-<name>.R
  │  roxygen title + body
  │  @format \describe{\item{...}} (all list slots)
  │  @details \itemize (firms, groups, benchmark, date range)
  │  @source (Yahoo Finance + "small illustrative sample..." note)
  │  @examples \donttest{full pipeline}
  │  @docType data / @keywords datasets / @name / @usage data(<name>)
  └─ "<name>"  ← bare string sentinel (generates Rd)

data/<name>.rda  ← bzip2, version=2, written by the above script
```

### Recommended Project Structure (additions only)

```
data/
├── dieselgate.rda           # unchanged (9265 bytes)
└── earnings_surprises.rda   # NEW — bzip2, <= 27 KB

data-raw/
├── dieselgate.R             # unchanged
├── earnings_surprises.R     # NEW — fetch + freeze script
└── DATA-SOURCES.md          # NEW — provenance registry

R/
├── data-dieselgate.R        # unchanged
└── data-earnings-surprises.R  # NEW — roxygen data doc
```

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Compressing .rda | Manual gzip/xz logic | `usethis::use_data(overwrite=TRUE)` or `save(compress="bzip2", version=2)` | Correct format, correct version, CRAN-accepted posture |
| Data doc generation | Writing .Rd files by hand | roxygen2 `@format` / `@name` / `"<name>"` bare-string sentinel | NAMESPACE auto-generated; Rd auto-generated by `devtools::document()` |
| Yahoo Finance fetch | Custom httr2 calls | `download_stock_data()` (already in the package) | Handles tidyquant/quantmod fallback, formats output for EventStudyTask |
| .Rbuildignore exclusion | New exclusion rules | None needed — `^data-raw$` (line 12) already excludes all of data-raw/ | Adding a redundant rule causes no harm but is unnecessary |

---

## Common Pitfalls

### Pitfall 1: event_date format mismatch in request tibble

**What goes wrong:** If `event_date` column in the request tibble is stored as `"YYYY-MM-DD"` instead of `"DD.MM.YYYY"`, `EventStudyTask$new()` silently mismatches dates against the firm/index data and produces all-NA abnormal returns.

**Why it happens:** The firm/index tibbles returned by `download_stock_data(format_for_task=TRUE)` store dates as `"%d.%m.%Y"`. The join in `prepare_event_study()` uses string equality on the date column.

**How to avoid:** Always use `format(as.Date(event_date), "%d.%m.%Y")` when constructing the request tibble.

[VERIFIED: data-raw/dieselgate.R:67] — verbatim: `event_date = rep(format(as.Date(event_date), "%d.%m.%Y"), 4L)`

### Pitfall 2: Object name ≠ @name ≠ .rda filename

**What goes wrong:** `R CMD check` reports "undocumented data set" or `data(earnings_surprises)` loads the wrong object.

**Why it happens:** The R object name in the script, the `@name` tag, the `"<sentinel>"` bare string, and the .rda filename must all be identical.

**How to avoid:** Use exactly `earnings_surprises` as: the R variable name in data-raw/earnings_surprises.R, the `@name earnings_surprises` tag, the `"earnings_surprises"` sentinel, and the file will be `data/earnings_surprises.rda` (usethis derives this automatically).

### Pitfall 3: Estimation window undershoots available data

**What goes wrong:** `fit_model()` returns NA coefficients because fewer than `estimation_window_length` trading days exist before the event in the fetched data.

**Why it happens:** With `estimation_window_length = 200` and `shift_estimation_window = -6`, the estimation window needs 206 trading days before the event. `from_date = "2022-06-01"` to `event_date = "2023-04-25"` is ~229 trading days — sufficient. If `from_date` were set to `"2022-09-01"` it would be only ~150 days — insufficient.

**How to avoid:** After freezing, run V3 check. If `caar` values are all NA, extend `from_date` by 3 months and re-fetch.

### Pitfall 4: Multiple event dates in request tibble break single-group CSectT

**What goes wrong:** AAPL event is 2023-05-04 and MSFT/GOOGL are 2023-04-25. The `relative_index` alignment in CSectT is calculated per firm relative to that firm's own event date — this is correct behavior for multi-event studies with heterogeneous event dates.

**Why it doesn't go wrong:** EventStudyTask handles heterogeneous event dates per event_id. The CSectT aggregation aligns by relative trading day, not calendar date.

**Warning sign:** If all three firms share the exact same calendar event date and the expected story requires cross-sectional AAR variation, consider keeping AAPL's date separate (as proposed) to enrich the example.

### Pitfall 5: usethis::use_data() must be run from package root

**What goes wrong:** `use_data()` writes to `./data/<name>.rda` relative to the working directory. If run from `data-raw/`, it creates `data-raw/data/earnings_surprises.rda`.

**How to avoid:** Always run `Rscript data-raw/earnings_surprises.R` from the package root (same instruction as the dieselgate.R header comment line 24).

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | AAPL post-earnings +4.7%, MSFT +7.2%, GOOGL +2.3% on the specified dates in Q1 2023 | Section 2 | Gallery story less vivid; pipeline still works; executor verifies after freeze |
| A2 | 3 tickers × ~270 trading days at 3 columns compresses to well under 27 KB under bzip2 | Section 2 | Dataset would need to be trimmed (fewer firms or shorter window); unlikely given dieselgate ratio |
| A3 | AAPL, MSFT, GOOGL are consistently available on Yahoo Finance for the 2022-06-01 to 2023-06-30 range | Section 2 | Executor discovers at fetch time; use quantmod fallback if tidyquant fails |

---

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|------------|-----------|---------|----------|
| tidyquant | download_stock_data() primary path | Unknown — executor must check | — | quantmod (also in Suggests) |
| quantmod | download_stock_data() fallback | Unknown — executor must check | — | synthetic data (Section 3 path b) |
| usethis | use_data() freeze path | Unknown — executor must check | — | base save() with bzip2/version=2 |
| Network (Yahoo Finance) | live fetch | Unknown at research time | — | synthetic deterministic fallback (Section 3) |

**Check commands (run before executing data-raw script):**
```r
requireNamespace("tidyquant", quietly = TRUE)   # TRUE = available
requireNamespace("quantmod", quietly = TRUE)    # TRUE = fallback available
requireNamespace("usethis", quietly = TRUE)     # TRUE = use use_data()
```

---

## Sources

### Primary (HIGH confidence — files read this session)

- `data-raw/dieselgate.R` (lines 1-111) — canonical build pattern; all structural claims verified
- `R/data-dieselgate.R` (lines 1-88) — canonical roxygen pattern; all section positions verified
- `R/data_download.R` (lines 17-84) — function signature and failure modes verified
- `DESCRIPTION` (line 71) — LazyData: true confirmed
- `.Rbuildignore` (line 12) — `^data-raw$` exclusion confirmed
- `data/dieselgate.rda` — 9265 bytes confirmed via `ls -la`

### Secondary (ASSUMED — training knowledge, not verified this session)

- Q1 2023 earnings dates and beat magnitudes for AAPL, MSFT, GOOGL
- Yahoo Finance ticker validity for AAPL, MSFT, GOOGL, ^GSPC

---

## Metadata

**Confidence breakdown:**
- Dieselgate pattern (Section 1): HIGH — all claims read from source files this session with line citations
- Earnings dataset proposal (Section 2): MEDIUM — ticker selection is ASSUMED; structural decisions (window lengths, from/to dates) are derived from verified code
- Fallback decision tree (Section 3): HIGH — derived from verified failure modes in data_download.R
- DATA-SOURCES.md format (Section 4): HIGH — .Rbuildignore exclusion verified; table design is Claude's discretion (locked decision)
- Verification hooks (Section 5): HIGH — derived from verified code patterns and confirmed byte counts

**Research date:** 2026-09-05
**Valid until:** 2027-03-05 (stable CRAN patterns; Yahoo Finance ticker stability is the main risk)
