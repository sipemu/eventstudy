#' Harvest Diagnostics from a Fitted EventStudyTask
#'
#' Extracts already-computed statistical signals from a fitted
#' \code{EventStudyTask} into a flat, JSON-ready S3 list. This is the
#' always-available grounding foundation for the knowledge base and offline
#' advice layer — it recomputes nothing; all signals already exist in the
#' fitted task.
#'
#' @param task A fitted \code{EventStudyTask} (after \code{fit_model()} and
#'   optionally \code{calculate_statistics()} have been called).
#' @param max_events Integer. Maximum number of events to include in the
#'   per-event sections (\code{estimation_window}, \code{event_window},
#'   \code{contract_state}). Events are ranked by anomaly score (degenerate
#'   events first, then by absolute final CAR). The remainder is summarised
#'   in \code{aggregate_summary}. Default: 20.
#'
#' @return A named list of class \code{"es_diagnostics"} with six sections:
#' \describe{
#'   \item{\code{meta}}{List: \code{n_events_total} (integer), \code{n_events_shown}
#'     (integer), \code{n_events_summarized} (integer), \code{event_ids_shown}
#'     (integer vector).}
#'   \item{\code{estimation_window}}{List of plain numeric vectors (length
#'     \code{n_events_shown}): \code{r2}, \code{sigma}, \code{degree_of_freedom},
#'     \code{shapiro_p}, \code{dw_stat}, \code{ljung_box_p}, \code{acf1}.
#'     Each entry is \code{NA_real_} when the model was not fitted or residuals
#'     are insufficient for the test.}
#'   \item{\code{event_window}}{List of plain numeric vectors (length
#'     \code{n_events_shown}): \code{ar_t} (last event-window day AR t-stat),
#'     \code{ar_p} (two-sided p-value), \code{car_t} (full-window CAR t-stat),
#'     \code{car_p} (two-sided p-value), \code{final_car}. All \code{NA_real_}
#'     when the \code{ART}/\code{CART} columns are absent.}
#'   \item{\code{cross_sectional}}{List of scalars aggregated across ALL events:
#'     \code{n_events} (total), \code{n_valid_events} (fitted count),
#'     \code{car_iqr}, \code{car_sd}, \code{n_overlap_pairs}, \code{any_overlap}.
#'     Multi-event fields degrade to \code{NA} when \code{task$aar_caar_tbl}
#'     is \code{NULL}.}
#'   \item{\code{contract_state}}{List of logical/numeric vectors (length
#'     \code{n_events_shown}): \code{is_fitted}, \code{na_ar_count},
#'     \code{na_est_count}, \code{insufficient_obs}, \code{zero_var_index}.}
#'   \item{\code{aggregate_summary}}{Named list summarising remainder events
#'     (those beyond \code{max_events}): \code{n_summarized},
#'     \code{mean_r2}, \code{median_r2}, \code{mean_final_car},
#'     \code{n_fitted}, \code{n_degenerate}. \code{NULL} when no events
#'     are summarised (all shown).}
#' }
#'
#' @seealso \code{\link{model_diagnostics}}, \code{\link{recommend_stat}},
#'   \code{\link{flag_robustness}}
#'
#' @export
es_diagnostics <- function(task, max_events = 20L) {
  if (!inherits(task, "EventStudyTask")) {
    stop("task must be an EventStudyTask.", call. = FALSE)
  }
  if (!"model" %in% names(task$data_tbl)) {
    stop("Models have not been fitted. Run fit_model() first.", call. = FALSE)
  }

  max_events <- as.integer(max_events)
  n_total    <- nrow(task$data_tbl)

  # Step 1: rank all events by anomaly score (degenerate = Inf)
  ranking  <- .rank_events_for_cap(task)
  n_shown  <- min(max_events, n_total)
  top_idx  <- ranking$row_idx[seq_len(n_shown)]
  rest_idx <- if (n_total > n_shown) ranking$row_idx[seq(n_shown + 1L, n_total)] else integer(0L)

  # Step 2: extract per-event signals for the top-N events
  est_signals <- .extract_estimation_signals(task, top_idx)
  ew_signals  <- .extract_event_window_signals(task, top_idx)
  contract    <- .extract_contract_state(task, top_idx)

  # Step 3: cross-sectional signals (all events)
  cs_signals  <- .extract_cross_sectional_signals(task)

  # Step 4: aggregate summary for remainder (NULL when nothing truncated)
  agg_summary <- if (length(rest_idx) > 0L) {
    .aggregate_remainder(task, rest_idx)
  } else {
    NULL
  }

  result <- list(
    meta = list(
      n_events_total      = n_total,
      n_events_shown      = n_shown,
      n_events_summarized = length(rest_idx),
      event_ids_shown     = task$data_tbl$event_id[top_idx]
    ),
    estimation_window   = est_signals,
    event_window        = ew_signals,
    cross_sectional     = cs_signals,
    contract_state      = contract,
    aggregate_summary   = agg_summary
  )
  class(result) <- "es_diagnostics"
  result
}


#' Print method for es_diagnostics objects
#'
#' Prints a concise summary of the diagnostics, following the package
#' convention of \code{print.es_simulation} and \code{print.es_cross_sectional}.
#'
#' @param x An object of class \code{"es_diagnostics"}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.es_diagnostics <- function(x, ...) {
  cat("Event Study Diagnostics\n")
  cat("=======================\n")
  cat("Events total:   ", x$meta$n_events_total, "\n")
  cat("Events shown:   ", x$meta$n_events_shown, "(full detail)\n")

  n_valid <- x$cross_sectional$n_valid_events
  if (!is.null(n_valid) && !is.na(n_valid)) {
    cat("Events valid:   ", n_valid, "\n")
  }

  # Report if any events were summarised (truncated)
  if (!is.null(x$meta$n_events_summarized) && x$meta$n_events_summarized > 0L) {
    cat("Events summarized:", x$meta$n_events_summarized,
        "(aggregate summary only)\n")
  }

  # Warn if degenerate events existed in the shown set
  if (!is.null(x$contract_state$is_fitted)) {
    n_degen_shown <- sum(!x$contract_state$is_fitted, na.rm = TRUE)
    if (n_degen_shown > 0L) {
      cat("[WARN]", n_degen_shown,
          "degenerate event(s) in shown set (is_fitted=FALSE)\n")
    }
  }

  cat("\nEstimation window (medians across shown events):\n")
  cat("  R-squared:    ",
      round(median(x$estimation_window$r2, na.rm = TRUE), 4), "\n")
  cat("  Shapiro-Wilk p:",
      round(median(x$estimation_window$shapiro_p, na.rm = TRUE), 4), "\n")
  cat("  DW statistic: ",
      round(median(x$estimation_window$dw_stat, na.rm = TRUE), 4), "\n")

  cs <- x$cross_sectional
  cat("\nEvent window (cross-sectional):\n")
  car_iqr_val <- if (!is.null(cs$car_iqr) && !is.na(cs$car_iqr)) {
    round(cs$car_iqr, 6)
  } else {
    "NA"
  }
  cat("  CAR IQR:       ", car_iqr_val, "\n")

  n_op <- if (!is.null(cs$n_overlap_pairs) && !is.na(cs$n_overlap_pairs)) {
    cs$n_overlap_pairs
  } else {
    "NA"
  }
  cat("  Overlap pairs: ", n_op, "\n")

  invisible(x)
}


# ---- Internal helpers (all @noRd) ----------------------------------------

#' Rank all events by anomaly score for top-N cap
#'
#' Degenerate events (is_fitted == FALSE) receive Inf so they always surface
#' in the top-N. Fitted events are ranked by abs(final_car) from the last row
#' of the CART tibble. When CART is absent, abs(mean(AR)) is used as fallback.
#' Ties broken by original event order (stable sort).
#'
#' @param task A fitted EventStudyTask.
#' @return A tibble with columns \code{row_idx} (integer) and
#'   \code{anomaly_score} (numeric, possibly Inf).
#' @noRd
.rank_events_for_cap <- function(task) {
  n <- nrow(task$data_tbl)
  has_cart <- "CART" %in% names(task$data_tbl)

  scores <- vapply(seq_len(n), function(i) {
    model <- task$data_tbl$model[[i]]
    if (!model$is_fitted) {
      return(Inf)
    }
    # Try to get abs(final_car) from CART
    score <- tryCatch({
      if (has_cart) {
        cart <- task$data_tbl$CART[[i]]
        if (!is.null(cart) && "car" %in% names(cart) && nrow(cart) > 0L) {
          abs(tail(cart$car, 1L))
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)

    # Fallback: abs(mean(abnormal_returns)) in event window
    if (is.na(score)) {
      score <- tryCatch({
        d <- task$data_tbl$data[[i]]
        ev_ar <- d$abnormal_returns[d$event_window == 1]
        abs(mean(ev_ar, na.rm = TRUE))
      }, error = function(e) 0)
    }
    score %||% 0
  }, numeric(1L))

  # Order descending (highest anomaly first); ties preserved by original idx
  ord <- order(-scores, seq_len(n))
  tibble::tibble(row_idx = ord, anomaly_score = scores[ord])
}


#' Extract estimation-window signals for the given event indices
#'
#' @param task A fitted EventStudyTask.
#' @param idx Integer vector of row indices into task$data_tbl.
#' @return A named list of plain numeric vectors.
#' @noRd
.extract_estimation_signals <- function(task, idx) {
  n <- length(idx)
  r2             <- numeric(n)
  sigma          <- numeric(n)
  degree_of_freedom <- numeric(n)
  acf1           <- numeric(n)
  shapiro_p      <- numeric(n)
  dw_stat        <- numeric(n)
  ljung_box_p    <- numeric(n)

  for (k in seq_along(idx)) {
    i     <- idx[k]
    model <- task$data_tbl$model[[i]]

    if (!model$is_fitted) {
      r2[k]             <- NA_real_
      sigma[k]          <- NA_real_
      degree_of_freedom[k] <- NA_real_
      acf1[k]           <- NA_real_
      shapiro_p[k]      <- NA_real_
      dw_stat[k]        <- NA_real_
      ljung_box_p[k]    <- NA_real_
      next
    }

    stats   <- model$statistics
    resids  <- stats$residuals

    r2[k]             <- stats$r2 %||% NA_real_
    sigma[k]          <- stats$sigma %||% NA_real_
    degree_of_freedom[k] <- stats$degree_of_freedom %||% NA_real_
    acf1[k]           <- stats$first_order_auto_correlation %||% NA_real_

    # Shapiro-Wilk normality test (requires 3–5000 observations)
    shapiro_p[k] <- tryCatch({
      clean_r <- stats::na.omit(resids)
      if (length(clean_r) >= 3L && length(clean_r) <= 5000L) {
        stats::shapiro.test(clean_r)$p.value
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)

    # Durbin-Watson statistic (inline formula from R/diagnostics.R)
    dw_stat[k] <- tryCatch({
      clean_r <- stats::na.omit(resids)
      denom   <- sum(clean_r^2)
      if (length(clean_r) < 2L || denom < .Machine$double.eps) {
        NA_real_
      } else {
        sum(diff(clean_r)^2) / denom
      }
    }, error = function(e) NA_real_)

    # Ljung-Box test (from R/diagnostics.R)
    ljung_box_p[k] <- tryCatch({
      clean_r <- stats::na.omit(resids)
      if (length(clean_r) > 10L) {
        stats::Box.test(
          clean_r,
          lag  = min(10L, floor(length(clean_r) / 5L)),
          type = "Ljung-Box"
        )$p.value
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
  }

  list(
    r2                = r2,
    sigma             = sigma,
    degree_of_freedom = degree_of_freedom,
    acf1              = acf1,
    shapiro_p         = shapiro_p,
    dw_stat           = dw_stat,
    ljung_box_p       = ljung_box_p
  )
}


#' Extract event-window AR/CAR signals for the given event indices
#'
#' Converts distributional dist objects to plain scalar p-values via
#' stats::pt() — never stores dist objects in the output list.
#'
#' @param task A fitted EventStudyTask.
#' @param idx Integer vector of row indices into task$data_tbl.
#' @return A named list of plain numeric vectors.
#' @noRd
.extract_event_window_signals <- function(task, idx) {
  n <- length(idx)
  has_art  <- "ART"  %in% names(task$data_tbl)
  has_cart <- "CART" %in% names(task$data_tbl)

  ar_t      <- rep(NA_real_, n)
  ar_p      <- rep(NA_real_, n)
  car_t     <- rep(NA_real_, n)
  car_p     <- rep(NA_real_, n)
  final_car <- rep(NA_real_, n)

  for (k in seq_along(idx)) {
    i     <- idx[k]
    model <- task$data_tbl$model[[i]]
    df    <- model$statistics$degree_of_freedom %||% 1L
    df    <- max(df, 1L)

    # AR t-stat from ART column
    if (has_art) {
      tryCatch({
        art <- task$data_tbl$ART[[i]]
        if (!is.null(art) && "ar_t" %in% names(art) && nrow(art) > 0L) {
          # Take last row's ar_t (most common: event day = last index)
          ar_t_val <- tail(art$ar_t, 1L)
          if (is.numeric(ar_t_val) && length(ar_t_val) == 1L) {
            ar_t[k] <- ar_t_val
            if (!is.na(ar_t_val)) {
              ar_p[k] <- stats::pt(abs(ar_t_val), df = df, lower.tail = FALSE) * 2
            }
          }
        }
      }, error = function(e) NULL)
    }

    # CAR t-stat + final CAR from CART column
    if (has_cart) {
      tryCatch({
        cart <- task$data_tbl$CART[[i]]
        if (!is.null(cart) && nrow(cart) > 0L) {
          # Final row = full-window summary
          last_row <- tail(cart, 1L)
          if ("car" %in% names(last_row)) {
            final_car[k] <- last_row$car
          }
          if ("car_t" %in% names(last_row)) {
            car_t_val <- last_row$car_t
            if (is.numeric(car_t_val) && length(car_t_val) == 1L) {
              car_t[k] <- car_t_val
              if (!is.na(car_t_val)) {
                car_p[k] <- stats::pt(abs(car_t_val), df = df, lower.tail = FALSE) * 2
              }
            }
          }
        }
      }, error = function(e) NULL)
    }
  }

  list(
    ar_t      = ar_t,
    ar_p      = ar_p,
    car_t     = car_t,
    car_p     = car_p,
    final_car = final_car
  )
}


#' Extract cross-sectional signals aggregated across ALL events
#'
#' Computes n_events, n_valid_events, CAR dispersion (IQR, SD), and
#' calendar-date event-window overlap pairs. Guards against NULL aar_caar_tbl.
#'
#' @param task A fitted EventStudyTask.
#' @return A named list of scalars.
#' @noRd
.extract_cross_sectional_signals <- function(task) {
  n_total <- nrow(task$data_tbl)

  # Count fitted events
  n_valid <- sum(vapply(task$data_tbl$model, function(m) isTRUE(m$is_fitted), logical(1L)))

  # CAR dispersion across all events (from CART if available)
  car_iqr <- NA_real_
  car_sd  <- NA_real_
  has_cart <- "CART" %in% names(task$data_tbl)
  if (has_cart) {
    cars <- vapply(seq_len(n_total), function(i) {
      tryCatch({
        cart <- task$data_tbl$CART[[i]]
        if (!is.null(cart) && "car" %in% names(cart) && nrow(cart) > 0L) {
          tail(cart$car, 1L)
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_)
    }, numeric(1L))
    car_iqr <- stats::IQR(cars, na.rm = TRUE)
    car_sd  <- stats::sd(cars, na.rm = TRUE)
  }

  # Event-window overlap: count pairs with overlapping calendar-date windows
  n_overlap_pairs <- NA_integer_
  any_overlap     <- NA
  tryCatch({
    n <- n_total
    # Collect event-window date ranges from the nested data tibbles
    min_dates <- character(n)
    max_dates <- character(n)
    for (i in seq_len(n)) {
      d  <- task$data_tbl$data[[i]]
      ew <- d[d$event_window == 1, ]
      if (nrow(ew) == 0L) {
        min_dates[i] <- NA_character_
        max_dates[i] <- NA_character_
      } else {
        # Convert dates to comparable Date objects. Try ISO 8601 first (also
        # handles Date-class input), then fall back to the package's legacy
        # "%d.%m.%Y" character convention. Behavior is identical on valid
        # package data; this only adds robustness to alternate date encodings.
        dates_i <- tryCatch({
          parsed <- suppressWarnings(as.Date(ew$date))
          if (all(is.na(parsed))) {
            suppressWarnings(as.Date(ew$date, "%d.%m.%Y"))
          } else {
            parsed
          }
        }, error = function(e) as.Date(NA))
        valid_dates <- dates_i[!is.na(dates_i)]
        if (length(valid_dates) == 0L) {
          min_dates[i] <- NA_character_
          max_dates[i] <- NA_character_
        } else {
          min_dates[i] <- as.character(min(valid_dates))
          max_dates[i] <- as.character(max(valid_dates))
        }
      }
    }
    # Count overlapping pairs with a simple nested loop
    count <- 0L
    for (a in seq_len(n - 1L)) {
      if (is.na(min_dates[a])) next
      d_min_a <- as.Date(min_dates[a])
      d_max_a <- as.Date(max_dates[a])
      for (b in seq(a + 1L, n)) {
        if (is.na(min_dates[b])) next
        d_min_b <- as.Date(min_dates[b])
        d_max_b <- as.Date(max_dates[b])
        if (d_min_a <= d_max_b && d_min_b <= d_max_a) {
          count <- count + 1L
        }
      }
    }
    n_overlap_pairs <- count
    any_overlap     <- count > 0L
  }, error = function(e) NULL)

  list(
    n_events        = n_total,
    n_valid_events  = n_valid,
    car_iqr         = car_iqr,
    car_sd          = car_sd,
    n_overlap_pairs = n_overlap_pairs,
    any_overlap     = any_overlap
  )
}


#' Extract per-event contract state for the given event indices
#'
#' Derives degenerate flags (insufficient_obs, zero_var_index) by re-examining
#' the estimation-window data — the exact two guards from MarketModel$fit().
#'
#' @param task A fitted EventStudyTask.
#' @param idx Integer vector of row indices into task$data_tbl.
#' @return A named list of logical/numeric vectors.
#' @noRd
.extract_contract_state <- function(task, idx) {
  n <- length(idx)
  is_fitted_vec    <- logical(n)
  na_ar_count_vec  <- rep(NA_integer_, n)
  na_est_count_vec <- rep(NA_integer_, n)
  insuff_obs_vec   <- rep(NA, n)
  zero_var_vec     <- rep(NA, n)

  for (k in seq_along(idx)) {
    i     <- idx[k]
    model <- task$data_tbl$model[[i]]
    d     <- task$data_tbl$data[[i]]

    is_fitted_vec[k] <- isTRUE(model$is_fitted)

    # NA counts in event window (abnormal_returns column)
    tryCatch({
      ev_ar <- d$abnormal_returns[d$event_window == 1]
      na_ar_count_vec[k] <- sum(is.na(ev_ar))
    }, error = function(e) { na_ar_count_vec[k] <<- NA_integer_ })

    # NA counts in estimation window (both firm_returns and index_returns)
    tryCatch({
      est_d <- d[d$estimation_window == 1, ]
      na_est_count_vec[k] <- sum(
        is.na(est_d$firm_returns) | is.na(est_d$index_returns)
      )
    }, error = function(e) { na_est_count_vec[k] <<- NA_integer_ })

    # Replicate the two MarketModel$fit() guards to derive degenerate flags
    tryCatch({
      est_d    <- d[d$estimation_window == 1, ]
      n_valid  <- sum(!is.na(est_d$firm_returns) & !is.na(est_d$index_returns))
      insuff_obs_vec[k] <- n_valid < 2L

      if (!insuff_obs_vec[k] && "index_returns" %in% names(est_d)) {
        zero_var_vec[k] <- stats::sd(est_d$index_returns, na.rm = TRUE) < .Machine$double.eps
      } else {
        zero_var_vec[k] <- FALSE
      }
    }, error = function(e) {
      insuff_obs_vec[k] <<- FALSE
      zero_var_vec[k]   <<- FALSE
    })
  }

  list(
    is_fitted       = is_fitted_vec,
    na_ar_count     = na_ar_count_vec,
    na_est_count    = na_est_count_vec,
    insufficient_obs = insuff_obs_vec,
    zero_var_index  = zero_var_vec
  )
}


#' Aggregate remainder events beyond the top-N cap
#'
#' Produces compact summary statistics for events that were not included in
#' the full per-event detail sections.
#'
#' @param task A fitted EventStudyTask.
#' @param rest_idx Integer vector of row indices for the remainder events.
#' @return A named list with summary statistics, or NULL when rest_idx is empty.
#' @noRd
.aggregate_remainder <- function(task, rest_idx) {
  if (length(rest_idx) == 0L) return(NULL)

  n_rem <- length(rest_idx)

  # R2 vector for remainder
  r2_vals <- vapply(rest_idx, function(i) {
    tryCatch({
      task$data_tbl$model[[i]]$statistics$r2 %||% NA_real_
    }, error = function(e) NA_real_)
  }, numeric(1L))

  # final_car for remainder
  has_cart <- "CART" %in% names(task$data_tbl)
  final_car_vals <- vapply(rest_idx, function(i) {
    if (!has_cart) return(NA_real_)
    tryCatch({
      cart <- task$data_tbl$CART[[i]]
      if (!is.null(cart) && "car" %in% names(cart) && nrow(cart) > 0L) {
        tail(cart$car, 1L)
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
  }, numeric(1L))

  # fitted count in remainder
  n_fitted_rem <- sum(vapply(rest_idx, function(i) {
    isTRUE(task$data_tbl$model[[i]]$is_fitted)
  }, logical(1L)))

  list(
    n_summarized   = n_rem,
    mean_r2        = mean(r2_vals, na.rm = TRUE),
    median_r2      = median(r2_vals, na.rm = TRUE),
    mean_final_car = mean(final_car_vals, na.rm = TRUE),
    n_fitted       = n_fitted_rem,
    n_degenerate   = n_rem - n_fitted_rem
  )
}
