# data-raw/earnings_surprises.R
#
# Reproducible fetch of the bundled `earnings_surprises` dataset.
#
# Provenance
# ----------
#   Source:      Yahoo Finance daily prices, via tidyquant::tq_get.
#   Firms:       Apple Inc.                   (AAPL, NASDAQ)
#                Microsoft Corporation        (MSFT, NASDAQ)
#                Alphabet Inc. Class A        (GOOGL, NASDAQ)
#   Benchmark:   S&P 500 index               (^GSPC)
#   Event:       First-calendar-quarter 2023 (Jan-Mar) earnings announcements,
#                reported late April / early May 2023:
#                  2023-05-04 -- AAPL Q2 FY2023 earnings (Jan-Mar quarter; beat consensus ~8%)
#                  2023-04-25 -- MSFT Q3 FY2023 earnings (Jan-Mar quarter; Azure beat; +7% next day)
#                  2023-04-25 -- GOOGL Q1 CY2023 earnings (Jan-Mar quarter; ad revenue rebound)
#   Date range:  2022-06-01 to 2023-06-30 (~270 trading days, covers the full
#                200-day estimation window plus [-5, +5] event window with margin).
#   Access date: 2026-09-05
#   License:     Yahoo Finance daily adjusted prices. Small illustrative
#                sample bundled for academic / demonstration use only.
#
# To reproduce: run `Rscript data-raw/earnings_surprises.R` from the package root
# with tidyquant and usethis installed and a network connection.

library(EventStudy)

firm_tickers <- c("AAPL", "MSFT", "GOOGL")
index_ticker <- "^GSPC"
from_date    <- "2022-06-01"
to_date      <- "2023-06-30"
event_dates  <- c("04.05.2023",   # AAPL -- Q2 FY2023 earnings (Jan-Mar quarter), 2023-05-04 in dd.mm.yyyy
                  "25.04.2023",   # MSFT -- Q3 FY2023 earnings (Jan-Mar quarter), 2023-04-25
                  "25.04.2023")   # GOOGL -- Q1 CY2023 earnings (Jan-Mar quarter), 2023-04-25

# --- Fetch firm prices --------------------------------------------------------
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

# Combine all firms into a single tibble (union of rows by symbol/date/adjusted)
firm <- dplyr::bind_rows(firm_raw_list)

# --- Fetch benchmark index prices --------------------------------------------
index <- tibble::as_tibble(
  download_stock_data(index_ticker, from = from_date, to = to_date)
)

# --- Assemble the frozen object ----------------------------------------------
# Three-row request tibble: one row per firm, all in the "Earnings Beat" group.
# event_date values are already in "%d.%m.%Y" format -- do NOT reformat.
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
    access_date  = format(Sys.Date(), "%Y-%m-%d"),
    note         = paste(
      "3-firm earnings surprise panel.",
      "All firms beat consensus EPS estimates in the first calendar quarter of 2023 (Jan-Mar),",
      "reported late April / early May 2023.",
      "AAPL event: 2023-05-04 (Q2 FY2023); MSFT event: 2023-04-25 (Q3 FY2023);",
      "GOOGL event: 2023-04-25 (Q1 CY2023). Benchmark: S&P 500 (^GSPC)."
    )
  )
)

# --- Freeze -------------------------------------------------------------------
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
