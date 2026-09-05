# DATA-SOURCES.md
# Dataset Provenance Registry — EventStudy Package
#
# One row per dataset bundled in data/. Maintained manually when a new
# dataset is added or an existing one is refreshed.
# This file is excluded from the CRAN tarball (^data-raw$ in .Rbuildignore).

| Dataset | Source | Tickers / Scope | Event | Date Range | Access Date | License Note | Script | Compressed Size |
|---------|--------|-----------------|-------|------------|-------------|--------------|--------|-----------------|
| dieselgate | Yahoo Finance (daily adjusted) | VOW.DE, PAH3.DE, BMW.DE, MBG.DE + ^GDAXI | 2015-09-18 EPA Notice of Violation to VW | 2014-06-01 to 2015-11-01 | 2026-09-04 | Small illustrative sample, academic/demo use only | data-raw/dieselgate.R | 9.1 KB |
| earnings_surprises | Yahoo Finance (daily adjusted) | AAPL, MSFT, GOOGL + ^GSPC | Q1 2023 earnings beats (AAPL 2023-05-04, MSFT/GOOGL 2023-04-25) | 2022-06-01 to 2023-06-30 | 2026-09-05 | Small illustrative sample, academic/demo use only | data-raw/earnings_surprises.R | 6.0 KB |
