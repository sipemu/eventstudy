#' Adjust P-Values for Multiple Testing
#'
#' Computes adjusted p-values for AAR and CAAR test statistics across the
#' event window, correcting for the multiple comparisons problem. Supports
#' all methods available in \code{\link[stats]{p.adjust}}.
#'
#' @param task A fitted EventStudyTask with \code{aar_caar_tbl} populated.
#' @param method Adjustment method passed to \code{\link[stats]{p.adjust}}.
#'   Common choices: \code{"BH"} (Benjamini-Hochberg, default), \code{"bonferroni"},
#'   \code{"holm"}, \code{"hochberg"}, \code{"BY"}, \code{"none"}.
#' @param stat_name Name of the multi-event test statistic to adjust.
#'   Must match a column name in \code{task$aar_caar_tbl}. Default \code{"CSectT"}.
#' @param group Optional group name to filter. If NULL, adjusts all groups.
#'
#' @return A tibble with columns from the original test statistic result plus
#'   \code{p_raw_aar}, \code{p_adj_aar}, \code{p_raw_caar}, \code{p_adj_caar}.
#'
#' @export
adjust_p_values <- function(task, method = "BH", stat_name = "CSectT",
                             group = NULL) {
  if (!inherits(task, "EventStudyTask")) {
    stop("task must be an EventStudyTask.")
  }
  if (is.null(task$aar_caar_tbl)) {
    stop("No AAR/CAAR results found. Run calculate_statistics() first.")
  }

  # Validate method
  valid_methods <- c("holm", "hochberg", "hommel", "bonferroni", "BH",
                     "BY", "fdr", "none")
  method <- match.arg(method, valid_methods)

  # Extract the test statistic results
  aar_caar <- task$aar_caar_tbl

  if (!is.null(group)) {
    aar_caar <- aar_caar[aar_caar$group == group, , drop = FALSE]
  }

  if (!stat_name %in% names(aar_caar)) {
    stop("Test statistic '", stat_name, "' not found in aar_caar_tbl. ",
         "Available: ", paste(setdiff(names(aar_caar),
                                       c("group", "data", "model")),
                              collapse = ", "))
  }

  results <- lapply(seq_len(nrow(aar_caar)), function(i) {
    stat_tbl <- aar_caar[[stat_name]][[i]]

    # Detect the test statistic column
    has_t <- "aar_t" %in% names(stat_tbl)
    has_z <- "aar_z" %in% names(stat_tbl)

    if (has_t) {
      # t-distribution based
      df <- stat_tbl$n_valid_events - 1
      stat_col <- stat_tbl$aar_t
      p_raw_aar <- 2 * stats::pt(abs(stat_col), df = df, lower.tail = FALSE)

      caar_col <- stat_tbl$caar_t
      p_raw_caar <- 2 * stats::pt(abs(caar_col), df = df, lower.tail = FALSE)
    } else if (has_z) {
      # Normal distribution based
      stat_col <- stat_tbl$aar_z
      p_raw_aar <- 2 * stats::pnorm(abs(stat_col), lower.tail = FALSE)

      caar_col <- stat_tbl$caar_z
      p_raw_caar <- 2 * stats::pnorm(abs(caar_col), lower.tail = FALSE)
    } else if ("bmp_t" %in% names(stat_tbl)) {
      df <- stat_tbl$n_valid_events - 1
      stat_col <- stat_tbl$bmp_t
      p_raw_aar <- 2 * stats::pt(abs(stat_col), df = df, lower.tail = FALSE)
      caar_col <- stat_tbl$cbmp_t
      p_raw_caar <- 2 * stats::pt(abs(caar_col), df = df, lower.tail = FALSE)
    } else if ("kp_t" %in% names(stat_tbl)) {
      df <- stat_tbl$n_valid_events - 1
      stat_col <- stat_tbl$kp_t
      p_raw_aar <- 2 * stats::pt(abs(stat_col), df = df, lower.tail = FALSE)
      caar_col <- stat_tbl$ckp_t
      p_raw_caar <- 2 * stats::pt(abs(caar_col), df = df, lower.tail = FALSE)
    } else if ("sign_z" %in% names(stat_tbl)) {
      stat_col <- stat_tbl$sign_z
      p_raw_aar <- 2 * stats::pnorm(abs(stat_col), lower.tail = FALSE)
      caar_col <- stat_tbl$csign_z
      p_raw_caar <- 2 * stats::pnorm(abs(caar_col), lower.tail = FALSE)
    } else if ("gsign_z" %in% names(stat_tbl)) {
      stat_col <- stat_tbl$gsign_z
      p_raw_aar <- 2 * stats::pnorm(abs(stat_col), lower.tail = FALSE)
      caar_col <- stat_tbl$cgsign_z
      p_raw_caar <- 2 * stats::pnorm(abs(caar_col), lower.tail = FALSE)
    } else if ("rank_z" %in% names(stat_tbl)) {
      stat_col <- stat_tbl$rank_z
      p_raw_aar <- 2 * stats::pnorm(abs(stat_col), lower.tail = FALSE)
      p_raw_caar <- rep(NA_real_, length(stat_col))
    } else {
      stop("Cannot detect test statistic type in '", stat_name, "'.")
    }

    p_adj_aar <- stats::p.adjust(p_raw_aar, method = method)
    p_adj_caar <- stats::p.adjust(p_raw_caar, method = method)

    stat_tbl$p_raw_aar <- p_raw_aar
    stat_tbl$p_adj_aar <- p_adj_aar
    stat_tbl$p_raw_caar <- p_raw_caar
    stat_tbl$p_adj_caar <- p_adj_caar

    if (!is.null(group)) {
      stat_tbl$group <- aar_caar$group[i]
    } else {
      stat_tbl$group <- aar_caar$group[i]
    }

    stat_tbl
  })

  dplyr::bind_rows(results)
}
