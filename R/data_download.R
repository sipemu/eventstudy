#' Download Stock Data
#'
#' Downloads historical stock price data for specified symbols. Uses
#' \pkg{tidyquant} if available, otherwise falls back to \pkg{quantmod}.
#'
#' @param symbols Character vector of stock ticker symbols.
#' @param from Start date (Date or character in YYYY-MM-DD format).
#' @param to End date. Default \code{Sys.Date()}.
#' @param source Data source: \code{"yahoo"} (default).
#' @param format_for_task Logical. If TRUE, formats output for
#'   \code{EventStudyTask}: columns \code{symbol}, \code{date} (dd.mm.yyyy),
#'   \code{adjusted}.
#'
#' @return A tibble with stock data.
#'
#' @export
download_stock_data <- function(symbols, from, to = Sys.Date(),
                                 source = "yahoo",
                                 format_for_task = TRUE) {
  from <- as.Date(from)
  to <- as.Date(to)

  if (requireNamespace("tidyquant", quietly = TRUE)) {
    data <- tidyquant::tq_get(symbols, get = "stock.prices",
                                from = from, to = to)
    if (format_for_task) {
      data <- data %>%
        dplyr::transmute(
          symbol = .data$symbol,
          date = format(.data$date, "%d.%m.%Y"),
          adjusted = .data$adjusted
        )
    }
  } else if (requireNamespace("quantmod", quietly = TRUE)) {
    data_list <- lapply(symbols, function(sym) {
      tryCatch({
        xts_data <- quantmod::getSymbols(sym, src = source,
                                          from = from, to = to,
                                          auto.assign = FALSE)
        adj_col <- paste0(sym, ".Adjusted")
        if (adj_col %in% colnames(xts_data)) {
          adj <- as.numeric(xts_data[, adj_col])
        } else {
          adj <- as.numeric(xts_data[, ncol(xts_data)])
        }
        tibble::tibble(
          symbol = sym,
          date = zoo::index(xts_data),
          adjusted = adj
        )
      }, error = function(e) {
        warning("Failed to download data for ", sym, ": ",
                conditionMessage(e))
        tibble::tibble(symbol = character(0), date = as.Date(character(0)),
                        adjusted = numeric(0))
      })
    })
    data <- dplyr::bind_rows(data_list)

    if (format_for_task) {
      data <- data %>%
        dplyr::transmute(
          symbol = .data$symbol,
          date = format(.data$date, "%d.%m.%Y"),
          adjusted = .data$adjusted
        )
    }
  } else {
    stop("Either 'tidyquant' or 'quantmod' is required to download stock data. ",
         "Install with: install.packages('tidyquant')")
  }

  data
}


#' Download Factor Data
#'
#' Downloads Fama-French factor data from the Kenneth French Data Library.
#' No external package dependencies required (uses base R download + CSV parsing).
#'
#' @param model Factor model: \code{"ff3"} (default), \code{"ff5"}, \code{"mom"}
#'   (momentum), or \code{"ff3_monthly"}, \code{"ff5_monthly"}.
#' @param frequency Data frequency: \code{"daily"} (default) or \code{"monthly"}.
#' @param format_for_task Logical. If TRUE, formats for EventStudyTask factor_tbl.
#'
#' @return A tibble with factor data.
#'
#' @export
download_factor_data <- function(model = c("ff3", "ff5", "mom"),
                                   frequency = c("daily", "monthly"),
                                   format_for_task = TRUE) {
  model <- match.arg(model)
  frequency <- match.arg(frequency)

  # Kenneth French Data Library URLs
  base_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

  file_map <- list(
    ff3 = list(
      daily = "F-F_Research_Data_Factors_daily_CSV.zip",
      monthly = "F-F_Research_Data_Factors_CSV.zip"
    ),
    ff5 = list(
      daily = "F-F_Research_Data_5_Factors_2x3_daily_CSV.zip",
      monthly = "F-F_Research_Data_5_Factors_2x3_CSV.zip"
    ),
    mom = list(
      daily = "F-F_Momentum_Factor_daily_CSV.zip",
      monthly = "F-F_Momentum_Factor_CSV.zip"
    )
  )

  zip_file <- file_map[[model]][[frequency]]
  url <- paste0(base_url, zip_file)

  # Download to temp file
 tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempdir()

  tryCatch({
    utils::download.file(url, tmp_zip, mode = "wb", quiet = TRUE)
    csv_files <- utils::unzip(tmp_zip, exdir = tmp_dir)
    csv_file <- csv_files[grepl("\\.CSV$|\\.csv$", csv_files)][1]

    # Read CSV, skipping header lines
    lines <- readLines(csv_file, warn = FALSE)

    # Find the start of data (first line with a number in position 1)
    data_start <- which(grepl("^\\s*\\d", lines))[1]
    if (is.na(data_start)) {
      stop("Could not parse factor data file.")
    }

    # Find where data ends (blank line or non-numeric start)
    data_lines <- lines[data_start:length(lines)]
    end_idx <- which(!grepl("^\\s*-?\\d", data_lines) |
                       nchar(trimws(data_lines)) == 0)[1]
    if (!is.na(end_idx)) {
      data_lines <- data_lines[seq_len(end_idx - 1)]
    }

    # Header is one line before data
    header_line <- lines[data_start - 1]
    headers <- trimws(strsplit(header_line, ",")[[1]])
    # Remove empty headers
    headers <- headers[nchar(headers) > 0]

    # Parse data
    data_text <- paste(c(paste(c("date_raw", headers), collapse = ","),
                          data_lines), collapse = "\n")
    factor_data <- utils::read.csv(text = data_text, header = TRUE,
                                     stringsAsFactors = FALSE)

    # Clean up date column
    factor_data$date_raw <- trimws(as.character(factor_data$date_raw))

    # Convert dates
    if (frequency == "daily") {
      factor_data$date <- as.Date(factor_data$date_raw, format = "%Y%m%d")
    } else {
      factor_data$date <- as.Date(paste0(factor_data$date_raw, "01"),
                                    format = "%Y%m%d")
    }

    # Remove rows where date parsing failed
    factor_data <- factor_data[!is.na(factor_data$date), ]

    # Convert factors from percentages to decimals
    numeric_cols <- setdiff(names(factor_data), c("date_raw", "date"))
    for (col in numeric_cols) {
      factor_data[[col]] <- as.numeric(factor_data[[col]]) / 100
    }

    # Remove raw date column
    factor_data$date_raw <- NULL

    # Standardize column names for EventStudyTask
    name_map <- c(
      "Mkt.RF" = "market_excess", "Mkt-RF" = "market_excess",
      "SMB" = "smb", "HML" = "hml", "RF" = "risk_free_rate",
      "RMW" = "rmw", "CMA" = "cma", "Mom" = "mom",
      "MOM" = "mom"
    )

    for (old_name in names(name_map)) {
      if (old_name %in% names(factor_data)) {
        names(factor_data)[names(factor_data) == old_name] <- name_map[old_name]
      }
    }

    factor_data <- tibble::as_tibble(factor_data)

    if (format_for_task) {
      factor_data <- factor_data %>%
        dplyr::mutate(date = format(date, "%d.%m.%Y"))
    }

    factor_data

  }, error = function(e) {
    stop("Failed to download factor data: ", conditionMessage(e))
  }, finally = {
    unlink(tmp_zip)
  })
}


#' Download Risk-Free Rate
#'
#' Downloads risk-free rate data from the Kenneth French Data Library
#' (extracted from the Fama-French factor files).
#'
#' @param frequency Data frequency: \code{"daily"} (default) or \code{"monthly"}.
#' @param source Data source: \code{"french"} (default).
#' @param format_for_task Logical. If TRUE, formats for EventStudyTask.
#'
#' @return A tibble with \code{date} and \code{risk_free_rate} columns.
#'
#' @export
download_risk_free_rate <- function(frequency = c("daily", "monthly"),
                                      source = "french",
                                      format_for_task = TRUE) {
  frequency <- match.arg(frequency)

  ff3_data <- download_factor_data(
    model = "ff3", frequency = frequency,
    format_for_task = format_for_task
  )

  if ("risk_free_rate" %in% names(ff3_data)) {
    ff3_data %>%
      dplyr::select(date, risk_free_rate)
  } else {
    stop("Could not extract risk-free rate from factor data.")
  }
}
