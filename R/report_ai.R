#' Extract Structured Context for AI Report
#'
#' Serializes event study results into a structured list that can be passed
#' to an LLM for interpretation. Extracts key results, diagnostics, model
#' fit statistics, and (optionally) cross-sectional regression results.
#'
#' @param task A fitted \code{EventStudyTask} (after \code{run_event_study}).
#' @param parameter_set The \code{ParameterSet} used for the event study.
#' @param cross_sectional Optional cross-sectional regression result
#'   (from \code{cross_sectional_regression}).
#'
#' @return A list with structured context for each AI report section.
#'
#' @export
extract_report_context <- function(task,
                                   parameter_set = NULL,
                                   cross_sectional = NULL) {
  if (!inherits(task, "EventStudyTask")) {
    stop("task must be an EventStudyTask object.")
  }
  if (!"model" %in% names(task$data_tbl)) {
    stop("Models have not been fitted. Run run_event_study() first.")
  }

  context <- list()

  # Study configuration
  context$study_type <- "Classical Event Study"
  context$n_events <- nrow(task$data_tbl)
  context$groups <- unique(task$data_tbl$group)
  context$n_groups <- length(context$groups)
  context$return_model <- if (!is.null(parameter_set)) {
    class(parameter_set$return_model)[1]
  } else {
    "Unknown"
  }

  # Event window from first event's request data
  first_request <- task$data_tbl$request[[1]]
  context$event_window <- c(
    first_request$event_window_start,
    first_request$event_window_end
  )
  context$estimation_window_length <- first_request$estimation_window_length

  # Key results: AAR/CAAR tables
  context$key_results_text <- .format_key_results(task)
  context$aar_caar_text <- .format_aar_caar(task)
  context$single_event_text <- .format_single_event_summary(task)

  # Diagnostics
  diag <- tryCatch(model_diagnostics(task), error = function(e) NULL)
  context$diagnostics_text <- .format_diagnostics(diag)

  # Pre-trend test
  pretrend <- tryCatch(pretrend_test(task), error = function(e) NULL)
  context$pretrend_text <- .format_pretrend(pretrend)

  # Model fit statistics
  context$model_fit_text <- .format_model_fit(task)

  # Test statistics available
  context$test_statistics_text <- .format_test_statistics(task)

  # Cross-sectional (optional)
  context$has_cross_sectional <- !is.null(cross_sectional)
  if (!is.null(cross_sectional)) {
    context$cross_sectional_text <- .format_cross_sectional(cross_sectional)
  }

  context
}


#' Generate AI-Powered Event Study Report
#'
#' Uses an LLM (via the \pkg{ellmer} package) to generate interpretive text
#' for each section of an event study report. Supports multiple providers
#' (Claude, OpenAI, Gemini, Ollama) and produces deterministic output
#' through temperature=0 and disk caching.
#'
#' @param task A fitted \code{EventStudyTask} (after \code{run_event_study}).
#' @param parameter_set The \code{ParameterSet} used for the event study.
#' @param cross_sectional Optional cross-sectional regression result.
#' @param provider LLM provider: \code{"anthropic"}, \code{"openai"},
#'   \code{"google"}, or \code{"ollama"}. Default \code{"anthropic"}.
#' @param model Model identifier. Defaults vary by provider:
#'   Claude Sonnet for Anthropic, GPT-4.1 for OpenAI.
#' @param sections Character vector of AI sections to generate. Any subset of:
#'   \code{"executive_summary"}, \code{"result_interpretation"},
#'   \code{"test_guidance"}, \code{"model_validation"},
#'   \code{"cross_sectional"}.
#' @param cache_dir Directory for caching AI responses. Default
#'   \code{tempdir()}. Set to a persistent path for reproducible reports.
#' @param seed Random seed for LLM reproducibility. Default 42.
#' @param output_file Output file path for the rendered report. If NULL,
#'   returns the AI text without rendering.
#' @param format Report output format: \code{"html"} or \code{"pdf"}.
#' @param title Report title.
#' @param author Author name.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return If \code{output_file} is specified, the path to the rendered report
#'   (invisibly). Otherwise, a named list of AI-generated text sections.
#'
#' @export
generate_ai_report <- function(task,
                               parameter_set = NULL,
                               cross_sectional = NULL,
                               provider = c("anthropic", "openai", "google", "ollama"),
                               model = NULL,
                               sections = c("executive_summary",
                                            "result_interpretation",
                                            "test_guidance",
                                            "model_validation",
                                            "cross_sectional"),
                               cache_dir = tempdir(),
                               seed = 42,
                               output_file = NULL,
                               format = c("html", "pdf"),
                               title = "AI-Powered Event Study Report",
                               author = NULL,
                               verbose = TRUE) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for AI report generation. ",
         "Install it with: install.packages('ellmer')")
  }

  provider <- match.arg(provider)
  format <- match.arg(format)

  # Remove cross_sectional section if no data
  if (is.null(cross_sectional)) {
    sections <- setdiff(sections, "cross_sectional")
  }

  # Extract structured context
  if (verbose) message("Extracting report context...")
  context <- extract_report_context(task, parameter_set, cross_sectional)

  # Check cache first
  cache_file <- file.path(cache_dir, "ai_report_cache.json")
  cached <- .load_cache(cache_file, context)
  if (!is.null(cached)) {
    if (verbose) message("Using cached AI responses from: ", cache_file)
    ai_sections <- cached
    if (is.null(output_file)) return(ai_sections)
    return(.render_ai_report(
      task = task, ai_sections = ai_sections,
      cross_sectional = cross_sectional,
      output_file = output_file, format = format,
      title = title, author = author
    ))
  }

  # Create chat object
  chat <- .create_chat(provider, model, seed)

  # Generate each section
  ai_sections <- list()
  total_cost <- 0

  for (section in sections) {
    if (verbose) message("Generating: ", section, "...")

    prompt <- switch(section,
      executive_summary = .ai_prompt_executive_summary(context),
      result_interpretation = .ai_prompt_result_interpretation(context),
      test_guidance = .ai_prompt_test_guidance(context),
      model_validation = .ai_prompt_model_validation(context),
      cross_sectional = .ai_prompt_cross_sectional(context),
      stop("Unknown section: ", section)
    )

    # Use a fresh chat clone per section to keep context isolated
    section_chat <- chat$clone()
    ai_sections[[section]] <- section_chat$chat(prompt)
    cost <- tryCatch(section_chat$get_cost(), error = function(e) NA_real_)
    if (!is.na(cost)) total_cost <- total_cost + cost
  }

  if (verbose) {
    message("AI generation complete.")
    if (total_cost > 0) {
      message(sprintf("Estimated cost: $%.4f", total_cost))
    }
  }

  # Cache the results
  .save_cache(ai_sections, context, cache_file)
  if (verbose) message("Cache saved: ", cache_file)

  # If no output file requested, return the sections

  if (is.null(output_file)) {
    return(ai_sections)
  }

  # Render the report
  .render_ai_report(
    task = task,
    ai_sections = ai_sections,
    cross_sectional = cross_sectional,
    output_file = output_file,
    format = format,
    title = title,
    author = author
  )
}


#' @noRd
.create_chat <- function(provider, model, seed) {
  params <- ellmer::params(temperature = 0, seed = seed)
  system_prompt <- .ai_system_prompt()

  switch(provider,
    anthropic = ellmer::chat_anthropic(
      system_prompt = system_prompt,
      params = params,
      model = model
    ),
    openai = ellmer::chat_openai(
      system_prompt = system_prompt,
      params = params,
      model = model
    ),
    google = ellmer::chat_google_gemini(
      system_prompt = system_prompt,
      params = params,
      model = model
    ),
    ollama = ellmer::chat_ollama(
      system_prompt = system_prompt,
      params = params,
      model = model %||% "llama3.1"
    ),
    stop("Unsupported provider: ", provider)
  )
}


#' @noRd
.save_cache <- function(ai_sections, context, cache_file) {
  cache_data <- list(
    timestamp = Sys.time(),
    context_hash = digest::digest(context, algo = "sha256"),
    sections = ai_sections
  )
  tryCatch({
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(cache_data, cache_file, auto_unbox = TRUE,
                         pretty = TRUE)
  }, error = function(e) {
    warning("Failed to save cache: ", e$message)
  })
}


#' @noRd
.load_cache <- function(cache_file, context) {
  if (!file.exists(cache_file)) return(NULL)

  tryCatch({
    cache_data <- jsonlite::read_json(cache_file, simplifyVector = TRUE)
    context_hash <- digest::digest(context, algo = "sha256")
    if (identical(cache_data$context_hash, context_hash)) {
      return(cache_data$sections)
    }
    NULL
  }, error = function(e) NULL)
}


#' @noRd
.render_ai_report <- function(task, ai_sections, cross_sectional,
                              output_file, format, title, author) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for report rendering. ",
         "Install it with: install.packages('rmarkdown')")
  }

  template_path <- system.file(
    "rmarkdown/templates/ai_event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  if (template_path == "") {
    stop("AI report template not found. Ensure the package is properly installed.")
  }

  if (format == "html") {
    output_format <- rmarkdown::html_document(
      toc = TRUE, toc_float = TRUE,
      theme = "flatly", code_folding = "hide",
      css = system.file(
        "rmarkdown/templates/ai_event_study_report/skeleton/styles.css",
        package = "EventStudy"
      )
    )
  } else {
    output_format <- rmarkdown::pdf_document(toc = TRUE)
  }

  ext <- if (format == "html") ".html" else ".pdf"
  if (!grepl(paste0("\\", ext, "$"), output_file)) {
    output_file <- paste0(tools::file_path_sans_ext(output_file), ext)
  }

  output_dir <- dirname(output_file)
  output_name <- basename(output_file)
  if (output_dir == ".") output_dir <- getwd()

  rmarkdown::render(
    input = template_path,
    output_format = output_format,
    output_file = output_name,
    output_dir = output_dir,
    params = list(
      task = task,
      ai_sections = ai_sections,
      cross_sectional = cross_sectional,
      title = title,
      author = author %||% ""
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  output_path <- file.path(output_dir, output_name)
  message("AI report generated: ", output_path)
  invisible(output_path)
}


# --- Context formatting helpers ---

#' @noRd
.format_key_results <- function(task) {
  if (is.null(task$aar_caar_tbl)) {
    return("AAR/CAAR statistics have not been calculated.")
  }

  stat_names <- setdiff(
    names(task$aar_caar_tbl),
    c("group", "data", "model")
  )

  if (length(stat_names) == 0) return("No test statistics available.")

  # Use first available statistic
  stat_name <- stat_names[1]
  lines <- character()

  for (i in seq_len(nrow(task$aar_caar_tbl))) {
    grp <- task$aar_caar_tbl$group[i]
    tbl <- task$aar_caar_tbl[[stat_name]][[i]]

    # Event day (t=0)
    event_day <- tbl[tbl$relative_index == 0, ]
    if (nrow(event_day) > 0) {
      lines <- c(lines, sprintf(
        "Group '%s' (test: %s): AAR at t=0 = %.4f (t=%.2f), CAAR at t=0 = %.4f (t=%.2f), N=%d events",
        grp, stat_name,
        event_day$aar[1], event_day$aar_t[1],
        event_day$caar[1], event_day$caar_t[1],
        event_day$n_events[1]
      ))
    }

    # Final day
    final_day <- tbl[nrow(tbl), ]
    lines <- c(lines, sprintf(
      "Group '%s': Final CAAR %s = %.4f (t=%.2f)",
      grp, final_day$car_window[1],
      final_day$caar[1], final_day$caar_t[1]
    ))
  }

  paste(lines, collapse = "\n")
}


#' @noRd
.format_aar_caar <- function(task) {
  if (is.null(task$aar_caar_tbl)) {
    return("No AAR/CAAR results available.")
  }

  stat_names <- setdiff(
    names(task$aar_caar_tbl),
    c("group", "data", "model")
  )

  lines <- character()
  for (stat_name in stat_names) {
    lines <- c(lines, sprintf("### Test: %s", stat_name))
    for (i in seq_len(nrow(task$aar_caar_tbl))) {
      grp <- task$aar_caar_tbl$group[i]
      tbl <- task$aar_caar_tbl[[stat_name]][[i]]
      lines <- c(lines, sprintf("\nGroup: %s", grp))

      # Format as compact table
      for (j in seq_len(nrow(tbl))) {
        row <- tbl[j, ]
        lines <- c(lines, sprintf(
          "  t=%+d: AAR=%.4f (t=%.2f) CAAR=%.4f (t=%.2f) N=%d [%d+/%d-] %s",
          row$relative_index, row$aar, row$aar_t,
          row$caar, row$caar_t,
          row$n_events, row$n_pos, row$n_neg,
          row$car_window
        ))
      }
    }
  }

  paste(lines, collapse = "\n")
}


#' @noRd
.format_single_event_summary <- function(task) {
  lines <- character()

  for (i in seq_len(min(nrow(task$data_tbl), 10))) {
    row <- task$data_tbl[i, ]
    eid <- row$event_id
    sym <- row$firm_symbol

    ar_data <- tryCatch({
      d <- row$data[[1]]
      d <- d[d$event_window == 1, ]
      if (!"abnormal_returns" %in% names(d)) return(NULL)
      d
    }, error = function(e) NULL)

    if (is.null(ar_data) || nrow(ar_data) == 0) next

    car <- sum(ar_data$abnormal_returns, na.rm = TRUE)
    ar_event <- ar_data$abnormal_returns[ar_data$relative_index == 0]
    if (length(ar_event) == 0) ar_event <- NA

    lines <- c(lines, sprintf(
      "Event '%s' (%s): AR at t=0 = %.4f, CAR = %.4f",
      eid, sym, ar_event, car
    ))
  }

  if (nrow(task$data_tbl) > 10) {
    lines <- c(lines, sprintf("... and %d more events", nrow(task$data_tbl) - 10))
  }

  paste(lines, collapse = "\n")
}


#' @noRd
.format_diagnostics <- function(diag) {
  if (is.null(diag)) return("Diagnostics not available.")

  lines <- character()
  lines <- c(lines, sprintf("Number of events analyzed: %d", nrow(diag)))
  lines <- c(lines, sprintf("Events with fitted models: %d",
                             sum(diag$is_fitted, na.rm = TRUE)))

  fitted <- diag[diag$is_fitted == TRUE, ]
  if (nrow(fitted) == 0) return(paste(lines, collapse = "\n"))

  # Shapiro-Wilk summary
  shapiro_vals <- na.omit(fitted$shapiro_p)
  if (length(shapiro_vals) > 0) {
    n_normal <- sum(shapiro_vals > 0.05)
    lines <- c(lines, sprintf(
      "Shapiro-Wilk normality: %d/%d events have normal residuals (p > 0.05)",
      n_normal, length(shapiro_vals)
    ))
    lines <- c(lines, sprintf(
      "  Range of p-values: [%.4f, %.4f], Median: %.4f",
      min(shapiro_vals), max(shapiro_vals), stats::median(shapiro_vals)
    ))
  }

  # Durbin-Watson summary
  dw_vals <- na.omit(fitted$dw_stat)
  if (length(dw_vals) > 0) {
    lines <- c(lines, sprintf(
      "Durbin-Watson statistic: Range [%.3f, %.3f], Mean: %.3f",
      min(dw_vals), max(dw_vals), mean(dw_vals)
    ))
    n_auto <- sum(dw_vals < 1.5 | dw_vals > 2.5)
    lines <- c(lines, sprintf(
      "  Events with potential autocorrelation (DW < 1.5 or > 2.5): %d/%d",
      n_auto, length(dw_vals)
    ))
  }

  # Ljung-Box summary
  lb_vals <- na.omit(fitted$ljung_box_p)
  if (length(lb_vals) > 0) {
    n_serial <- sum(lb_vals < 0.05)
    lines <- c(lines, sprintf(
      "Ljung-Box serial correlation: %d/%d events show significant autocorrelation (p < 0.05)",
      n_serial, length(lb_vals)
    ))
  }

  paste(lines, collapse = "\n")
}


#' @noRd
.format_pretrend <- function(pretrend) {
  if (is.null(pretrend)) return("Pre-trend test not available.")

  lines <- character()
  for (i in seq_len(nrow(pretrend))) {
    row <- pretrend[i, ]
    sig <- if (row$p_value < 0.01) "***" else if (row$p_value < 0.05) "**" else if (row$p_value < 0.10) "*" else ""
    lines <- c(lines, sprintf(
      "Group '%s': Mean pre-event AR = %.4f, t-stat = %.2f, p-value = %.4f%s (N=%d events, %d pre-periods)",
      row$group, row$mean_pre_ar, row$t_stat, row$p_value, sig,
      row$n_events, row$n_pre_periods
    ))
  }

  paste(lines, collapse = "\n")
}


#' @noRd
.format_model_fit <- function(task) {
  lines <- character()

  sigmas <- purrr::map_dbl(task$data_tbl$model, ~ .x$statistics$sigma)
  r2s <- purrr::map_dbl(task$data_tbl$model, ~ {
    r2 <- .x$statistics$r2
    if (is.null(r2)) NA_real_ else r2
  })
  acf1s <- purrr::map_dbl(task$data_tbl$model, ~ {
    ac <- .x$statistics$first_order_auto_correlation
    if (is.null(ac)) NA_real_ else ac
  })

  lines <- c(lines, sprintf(
    "Residual sigma: Range [%.6f, %.6f], Mean: %.6f",
    min(sigmas, na.rm = TRUE), max(sigmas, na.rm = TRUE),
    mean(sigmas, na.rm = TRUE)
  ))

  r2_valid <- na.omit(r2s)
  if (length(r2_valid) > 0) {
    lines <- c(lines, sprintf(
      "R-squared: Range [%.4f, %.4f], Mean: %.4f",
      min(r2_valid), max(r2_valid), mean(r2_valid)
    ))
  }

  acf_valid <- na.omit(acf1s)
  if (length(acf_valid) > 0) {
    lines <- c(lines, sprintf(
      "First-order autocorrelation: Range [%.4f, %.4f], Mean: %.4f",
      min(acf_valid), max(acf_valid), mean(acf_valid)
    ))
  }

  paste(lines, collapse = "\n")
}


#' @noRd
.format_test_statistics <- function(task) {
  if (is.null(task$aar_caar_tbl)) {
    return("No multi-event test statistics computed.")
  }

  stat_names <- setdiff(
    names(task$aar_caar_tbl),
    c("group", "data", "model")
  )

  if (length(stat_names) == 0) return("No test statistics available.")

  descriptions <- c(
    CSectT = "Cross-sectional t-test (parametric, assumes independence)",
    PatellZ = "Patell Z-test (standardized, accounts for forecast error variance)",
    BMP = "Boehmer-Musumeci-Poulsen test (robust to event-induced variance)",
    Sign = "Sign test (non-parametric, robust to non-normality)",
    GeneralizedSign = "Generalized sign test (adjusts for fraction of positive ARs)",
    Rank = "Rank test (non-parametric, robust to non-normality and outliers)",
    KolariPynnonen = "Kolari-Pynnonen adjusted BMP (robust to cross-correlation)"
  )

  lines <- character()
  for (sn in stat_names) {
    desc <- descriptions[sn]
    if (is.na(desc)) desc <- sn
    lines <- c(lines, sprintf("- %s: %s", sn, desc))
  }

  paste(lines, collapse = "\n")
}


#' @noRd
.format_cross_sectional <- function(cs) {
  if (!inherits(cs, "es_cross_sectional")) {
    return("Cross-sectional results not in expected format.")
  }

  lines <- character()
  lines <- c(lines, sprintf("N observations: %d", cs$n_obs))
  lines <- c(lines, sprintf("R-squared: %.4f", cs$r_squared))
  lines <- c(lines, sprintf("Adj. R-squared: %.4f", cs$adj_r_squared))
  lines <- c(lines, "\nCoefficients:")

  for (var_name in rownames(cs$coefficients)) {
    coef <- cs$coefficients[var_name, ]
    sig <- if (coef$p.value < 0.01) "***" else if (coef$p.value < 0.05) "**" else if (coef$p.value < 0.10) "*" else ""
    lines <- c(lines, sprintf(
      "  %s: estimate=%.6f, SE=%.6f, t=%.2f, p=%.4f%s",
      var_name, coef$estimate, coef$std.error, coef$statistic, coef$p.value, sig
    ))
  }

  paste(lines, collapse = "\n")
}
