#' Earnings Surprise Multi-Firm Event Study Dataset
#'
#' A small, frozen dataset bundling daily prices for three U.S. large-cap firms
#' and the S&P 500 benchmark around the Q1 2023 earnings surprise quarter,
#' ready to drive a complete event study pipeline (\code{prepare_event_study()}
#' -> \code{fit_model()} -> \code{calculate_statistics()}).
#'
#' All three firms beat consensus EPS estimates in Q1 2023: Apple Inc. reported
#' on \strong{2023-05-04} and beat by approximately 8\%, driving a strong
#' next-day return; Microsoft Corporation reported on \strong{2023-04-25},
#' beating cloud (Azure) estimates with a roughly +7\% next-day response;
#' Alphabet Inc. reported on \strong{2023-04-25}, with advertising revenue
#' exceeding expectations. The bundled window layout uses a 200-trading-day
#' estimation window ending 6 days before each event and an event window of
#' \code{[-5, +5]} trading days. Running a market model on this single-group
#' panel produces a positive cumulative average abnormal return (CAAR) over the
#' event window, reflecting the shared earnings-beat signal across all three firms.
#'
#' @format A named \code{list} with four elements:
#' \describe{
#'   \item{firm}{A tibble of daily prices for all three firms combined,
#'     with columns \code{symbol} (one of \code{"AAPL"}, \code{"MSFT"},
#'     \code{"GOOGL"}), \code{date} (character,
#'     \code{"\%d.\%m.\%Y"} format), and \code{adjusted} (numeric adjusted
#'     close). Rows for all firms are stacked (813 rows total).}
#'   \item{index}{A tibble of S&P 500 (\code{"^GSPC"}) daily prices with the
#'     same \code{symbol} / \code{date} / \code{adjusted} columns, used as the
#'     benchmark for all three events.}
#'   \item{request}{A three-row tibble giving the event-study request
#'     specifications, one row per firm, with the nine columns expected by
#'     \code{\link{EventStudyTask}}: \code{event_id} (1L to 3L),
#'     \code{firm_symbol}, \code{index_symbol}, \code{event_date}
#'     (\code{"04.05.2023"} for AAPL, \code{"25.04.2023"} for MSFT and GOOGL),
#'     \code{group} (\code{"Earnings Beat"} for all firms),
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
#'   \item \code{AAPL} — Apple Inc. (NASDAQ), event_id = 1
#'   \item \code{MSFT} — Microsoft Corporation (NASDAQ), event_id = 2
#'   \item \code{GOOGL} — Alphabet Inc. Class A (NASDAQ), event_id = 3
#' }
#' \strong{Group:} "Earnings Beat" (all firms).
#' \strong{Benchmark:} S&P 500 index (ticker \code{^GSPC}).
#' \strong{Date range:} 2022-06-01 to 2023-06-30.
#'
#' @source Yahoo Finance daily adjusted prices, retrieved 2026-09-05 via the
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
