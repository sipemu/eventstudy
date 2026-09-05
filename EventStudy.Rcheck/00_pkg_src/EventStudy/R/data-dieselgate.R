#' Volkswagen "Dieselgate" Multi-Automaker Event Study Dataset
#'
#' A small, frozen dataset bundling daily prices for four German automakers and
#' the DAX benchmark index around the 2015 "dieselgate" emissions scandal, ready
#' to drive a complete multi-group event study (\code{prepare_event_study()} ->
#' \code{fit_model()} -> \code{calculate_statistics()}).
#'
#' The event is the U.S. Environmental Protection Agency's Notice of Violation
#' issued to Volkswagen on \strong{2015-09-18} (a Friday); the share-price crash
#' lands on the following trading days. The dataset covers two groups: the
#' "VW Group" (VOW.DE, PAH3.DE — directly implicated firms) and "Other"
#' (BMW.DE, MBG.DE — peer automakers). The bundled window layout uses a
#' 250-trading-day estimation window ending 11 days before the event and an
#' event window of \code{[-10, +10]} trading days.
#'
#' Fitting a market model on the "VW Group" events produces a strongly negative
#' cumulative average abnormal return (CAAR approximately -39%), driven by
#' roughly -17\% and -13\% average abnormal returns on the first two trading days
#' after the disclosure, while the "Other" peer automakers show near-zero CAAR
#' (approximately +1\%), illustrating the idiosyncratic nature of the shock.
#'
#' @format A named \code{list} with four elements:
#' \describe{
#'   \item{firm}{A tibble of daily prices for all four automakers combined,
#'     with columns \code{symbol} (one of \code{"VOW.DE"}, \code{"PAH3.DE"},
#'     \code{"BMW.DE"}, \code{"MBG.DE"}), \code{date} (character,
#'     \code{"\%d.\%m.\%Y"} format), and \code{adjusted} (numeric adjusted
#'     close). Rows for all firms are stacked (1 440 rows total).}
#'   \item{index}{A tibble of DAX (\code{"^GDAXI"}) daily prices with the same
#'     \code{symbol} / \code{date} / \code{adjusted} columns, used as the
#'     benchmark / reference market for all four events.}
#'   \item{request}{A four-row tibble giving the event-study request
#'     specifications, one row per firm, with the nine columns expected by
#'     \code{\link{EventStudyTask}}: \code{event_id} (1L to 4L),
#'     \code{firm_symbol}, \code{index_symbol}, \code{event_date}
#'     (\code{"18.09.2015"}), \code{group} (\code{"VW Group"} for
#'     VOW.DE/PAH3.DE, \code{"Other"} for BMW.DE/MBG.DE),
#'     \code{event_window_start} (-10), \code{event_window_end} (10),
#'     \code{shift_estimation_window} (-11), and
#'     \code{estimation_window_length} (250). \code{event_id = 1} is
#'     VOW.DE for backward compatibility.}
#'   \item{meta}{A list of provenance metadata: \code{firm_tickers} (character
#'     vector of all four tickers), \code{groups} (named list mapping group
#'     labels to tickers), \code{index_ticker}, \code{event_date}, \code{from},
#'     \code{to}, \code{source}, \code{access_date}, and \code{note}.}
#' }
#'
#' @details
#' \strong{Firms:}
#' \itemize{
#'   \item \code{VOW.DE} — Volkswagen AG ordinary shares (Xetra), event_id = 1
#'   \item \code{PAH3.DE} — Porsche Automobil Holding SE (Xetra), event_id = 2
#'   \item \code{BMW.DE} — BMW AG (Xetra), event_id = 3
#'   \item \code{MBG.DE} — Mercedes-Benz Group AG (Xetra), event_id = 4
#' }
#' \strong{Groups:} "VW Group" (VOW.DE, PAH3.DE) vs "Other" (BMW.DE, MBG.DE).
#' \strong{Benchmark:} DAX performance index (ticker \code{^GDAXI}).
#' \strong{Date range:} 2014-06-01 to 2015-11-01.
#'
#' @source Yahoo Finance daily adjusted prices, retrieved 2026-09-04 via the
#'   package's own \code{\link{download_stock_data}}. This is a small
#'   illustrative sample bundled for academic / demonstration use only; see
#'   \code{data-raw/dieselgate.R} for the reproducible fetch script.
#'
#' @examples
#' \donttest{
#' data(dieselgate)
#'
#' # Build a multi-group task and run the full pipeline
#' task <- EventStudyTask$new(dieselgate$firm, dieselgate$index,
#'                            dieselgate$request)
#' task <- run_event_study(task, ParameterSet$new())
#'
#' # Single-firm: VW crash (event_id = 1)
#' task$get_car(1L)
#'
#' # Multi-group: CAAR comparison
#' vw_caar    <- task$aar_caar_tbl[task$aar_caar_tbl$group == "VW Group", ]$CSectT[[1]]
#' other_caar <- task$aar_caar_tbl[task$aar_caar_tbl$group == "Other", ]$CSectT[[1]]
#' tail(vw_caar[, c("relative_index", "caar", "caar_t")], 1)
#' tail(other_caar[, c("relative_index", "caar", "caar_t")], 1)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dieselgate
#' @usage data(dieselgate)
"dieselgate"
