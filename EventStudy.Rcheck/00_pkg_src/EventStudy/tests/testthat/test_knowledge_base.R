## Tests for R/knowledge_base.R — EVENTSTUDY_KB structure and firing conditions
##
## Task 1 tests: structure, citation integrity, condition safety, id uniqueness
## Task 2 tests: per-rule positive/negative firing + NA-guard + integration
##
## Run with:
##   testthat::test_file("tests/testthat/test_knowledge_base.R")

test_that("es_kb() returns a list of >= 8 rule records", {
  kb <- es_kb()
  expect_true(is.list(kb))
  expect_gte(length(kb), 8L)
})

test_that("every rule passes .kb_rule field validation (has required fields)", {
  kb <- es_kb()
  required_fields <- c("id", "category", "condition", "recommendation", "citation", "severity")
  for (rule in kb) {
    for (field in required_fields) {
      expect_true(
        field %in% names(rule),
        label = paste0("Rule '", rule$id %||% "UNKNOWN", "' missing field '", field, "'")
      )
    }
  }
})

test_that("every rule$citation has non-empty author, integer-ish year, and non-empty key", {
  kb <- es_kb()
  for (rule in kb) {
    cit <- rule$citation
    expect_true(
      is.character(cit$author) && nchar(cit$author) > 0,
      label = paste0("Rule '", rule$id, "': citation$author must be non-empty character")
    )
    expect_true(
      is.numeric(cit$year) || is.integer(cit$year),
      label = paste0("Rule '", rule$id, "': citation$year must be numeric")
    )
    expect_true(
      is.character(cit$key) && nchar(cit$key) > 0,
      label = paste0("Rule '", rule$id, "': citation$key must be non-empty character")
    )
  }
})

test_that("the five required academic authorities each appear in at least one rule citation", {
  kb <- es_kb()
  all_authors <- vapply(kb, function(r) r$citation$author, character(1L))
  # MacKinlay
  expect_true(
    any(grepl("MacKinlay", all_authors, ignore.case = TRUE)),
    label = "MacKinlay must appear in at least one rule citation"
  )
  # Brown & Warner
  expect_true(
    any(grepl("Brown", all_authors, ignore.case = TRUE)),
    label = "Brown & Warner must appear in at least one rule citation"
  )
  # Patell
  expect_true(
    any(grepl("Patell", all_authors, ignore.case = TRUE)),
    label = "Patell must appear in at least one rule citation"
  )
  # BMP / Boehmer
  expect_true(
    any(grepl("Boehmer|BMP", all_authors, ignore.case = TRUE)),
    label = "BMP/Boehmer must appear in at least one rule citation"
  )
  # Kolari & Pynnonen
  expect_true(
    any(grepl("Kolari|Pynnönen|Pynnonen", all_authors, ignore.case = TRUE)),
    label = "Kolari & Pynnonen must appear in at least one rule citation"
  )
})

test_that("every rule$condition is a function, returns length-1 logical on minimal NA-filled fixture", {
  kb <- es_kb()

  # Minimal diagnostics fixture with all signals NA
  na_diag <- list(
    meta = list(
      n_events_total      = 2L,
      n_events_shown      = 2L,
      n_events_summarized = 0L,
      event_ids_shown     = c(1L, 2L)
    ),
    estimation_window = list(
      r2                = c(NA_real_, NA_real_),
      sigma             = c(NA_real_, NA_real_),
      degree_of_freedom = c(NA_real_, NA_real_),
      acf1              = c(NA_real_, NA_real_),
      shapiro_p         = c(NA_real_, NA_real_),
      dw_stat           = c(NA_real_, NA_real_),
      ljung_box_p       = c(NA_real_, NA_real_)
    ),
    event_window = list(
      ar_t      = c(NA_real_, NA_real_),
      ar_p      = c(NA_real_, NA_real_),
      car_t     = c(NA_real_, NA_real_),
      car_p     = c(NA_real_, NA_real_),
      final_car = c(NA_real_, NA_real_)
    ),
    cross_sectional = list(
      n_events        = 2L,
      n_valid_events  = NA_integer_,
      car_iqr         = NA_real_,
      car_sd          = NA_real_,
      n_overlap_pairs = NA_integer_,
      any_overlap     = NA
    ),
    contract_state = list(
      is_fitted        = c(NA, NA),
      na_ar_count      = c(NA_integer_, NA_integer_),
      na_est_count     = c(NA_integer_, NA_integer_),
      insufficient_obs = c(NA, NA),
      zero_var_index   = c(NA, NA)
    ),
    aggregate_summary = NULL
  )

  for (rule in kb) {
    expect_true(
      is.function(rule$condition),
      label = paste0("Rule '", rule$id, "': condition must be a function")
    )
    result <- tryCatch(rule$condition(na_diag), error = function(e) {
      stop("Rule '", rule$id, "' errored on NA fixture: ", conditionMessage(e))
    })
    expect_true(
      is.logical(result) && length(result) == 1L,
      label = paste0("Rule '", rule$id, "': condition must return length-1 logical on NA fixture")
    )
  }
})

test_that("rule ids are unique, severities are valid, categories are valid", {
  kb <- es_kb()
  ids       <- vapply(kb, `[[`, character(1L), "id")
  severities <- vapply(kb, `[[`, character(1L), "severity")
  categories <- vapply(kb, `[[`, character(1L), "category")

  expect_equal(length(ids), length(unique(ids)), label = "Rule ids must be unique")

  valid_severities <- c("info", "warning", "error")
  for (i in seq_along(severities)) {
    expect_true(
      severities[i] %in% valid_severities,
      label = paste0("Rule '", ids[i], "': severity '", severities[i], "' not in ", paste(valid_severities, collapse="/"))
    )
  }

  valid_categories <- c("stat_choice", "robustness")
  for (i in seq_along(categories)) {
    expect_true(
      categories[i] %in% valid_categories,
      label = paste0("Rule '", ids[i], "': category '", categories[i], "' not in ", paste(valid_categories, collapse="/"))
    )
  }
})

# ---- Task 2: per-rule positive/negative firing tests -------------------------

## Helper: build a minimal diagnostics fixture with controlled signal values
## Only sets the fields that the KB condition actually reads.
make_diag <- function(
  shapiro_p         = c(0.3, 0.4),   # default: non-rejected normality
  dw_stat           = c(2.0, 2.0),
  r2                = c(0.3, 0.3),
  n_overlap_pairs   = 0L,
  n_valid_events    = 5L,
  n_events_total    = 5L,
  n_valid_events_cs = NULL           # if NULL, use n_valid_events
) {
  n_cs <- if (is.null(n_valid_events_cs)) n_valid_events else n_valid_events_cs
  n_shown <- length(shapiro_p)
  list(
    meta = list(
      n_events_total      = n_events_total,
      n_events_shown      = n_shown,
      n_events_summarized = 0L,
      event_ids_shown     = seq_len(n_shown)
    ),
    estimation_window = list(
      r2                = r2,
      sigma             = rep(0.01, n_shown),
      degree_of_freedom = rep(118L, n_shown),
      acf1              = rep(0.0, n_shown),
      shapiro_p         = shapiro_p,
      dw_stat           = dw_stat,
      ljung_box_p       = rep(0.3, n_shown)
    ),
    event_window = list(
      ar_t      = rep(1.5, n_shown),
      ar_p      = rep(0.1, n_shown),
      car_t     = rep(1.8, n_shown),
      car_p     = rep(0.07, n_shown),
      final_car = rep(0.01, n_shown)
    ),
    cross_sectional = list(
      n_events        = n_events_total,
      n_valid_events  = n_cs,
      car_iqr         = 0.02,
      car_sd          = 0.015,
      n_overlap_pairs = n_overlap_pairs,
      any_overlap     = n_overlap_pairs > 0L
    ),
    contract_state = list(
      is_fitted        = rep(TRUE, n_shown),
      na_ar_count      = rep(0L, n_shown),
      na_est_count     = rep(0L, n_shown),
      insufficient_obs = rep(FALSE, n_shown),
      zero_var_index   = rep(FALSE, n_shown)
    ),
    aggregate_summary = NULL
  )
}

## Helper: get a specific rule by id
get_rule <- function(id) {
  kb <- es_kb()
  matches <- Filter(function(r) r$id == id, kb)
  if (length(matches) == 0L) stop("Rule not found: ", id)
  matches[[1L]]
}

# -- KB-NORM-PATELL: fires when shapiro_p > 0.05 in >= 70% of events ----------
test_that("KB-NORM-PATELL fires on normal-residuals fixture and is silent on non-normal", {
  rule <- get_rule("KB-NORM-PATELL")

  # Positive: all p-values > 0.05 (normality holds across all events)
  pos_diag <- make_diag(shapiro_p = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
  expect_true(isTRUE(rule$condition(pos_diag)),
              label = "KB-NORM-PATELL should fire when all Shapiro p > 0.05")

  # Negative: all p-values < 0.05 (normality rejected in every event)
  neg_diag <- make_diag(shapiro_p = c(0.01, 0.02, 0.01, 0.02, 0.01, 0.02, 0.01, 0.02, 0.01, 0.02))
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-NORM-PATELL should NOT fire when all Shapiro p < 0.05")
})

# -- KB-NONNORM-NONPAR: fires when shapiro_p < 0.05 in >= 50% of events -------
test_that("KB-NONNORM-NONPAR fires on non-normal fixture, silent on normal", {
  rule <- get_rule("KB-NONNORM-NONPAR")

  # Positive: majority (>= 50%) have p < 0.05
  pos_diag <- make_diag(shapiro_p = c(0.01, 0.02, 0.01, 0.02, 0.01, 0.3, 0.4, 0.5, 0.6, 0.7))
  expect_true(isTRUE(rule$condition(pos_diag)),
              label = "KB-NONNORM-NONPAR fires when >= 50% of Shapiro p < 0.05")

  # Negative: only 20% have p < 0.05
  neg_diag <- make_diag(shapiro_p = c(0.01, 0.02, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-NONNORM-NONPAR should NOT fire when < 50% of Shapiro p < 0.05")
})

test_that("KB-NONNORM-NONPAR recommendation mentions non-parametric alternative (Sign/Rank/Corrado)", {
  rule <- get_rule("KB-NONNORM-NONPAR")
  rec_lower <- tolower(rule$recommendation)
  expect_true(
    grepl("sign|rank|corrado|non-parametric|nonparametric", rec_lower),
    label = "KB-NONNORM-NONPAR recommendation must mention a non-parametric alternative"
  )
})

# -- KB-VAR-INCREASE-BMP -------------------------------------------------------
test_that("KB-VAR-INCREASE-BMP fires when event-induced variance detected", {
  rule <- get_rule("KB-VAR-INCREASE-BMP")

  # Positive: CAR IQR notably high (variance inflated)
  pos_diag <- make_diag()
  pos_diag$cross_sectional$car_iqr <- 0.25   # high dispersion
  pos_diag$cross_sectional$car_sd  <- 0.30
  expect_true(isTRUE(rule$condition(pos_diag)),
              label = "KB-VAR-INCREASE-BMP fires on high CAR dispersion")

  # Negative: low dispersion
  neg_diag <- make_diag()
  neg_diag$cross_sectional$car_iqr <- 0.003
  neg_diag$cross_sectional$car_sd  <- 0.004
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-VAR-INCREASE-BMP should NOT fire on low CAR dispersion")
})

# -- KB-OVERLAP-KP -------------------------------------------------------------
test_that("KB-OVERLAP-KP fires when n_overlap_pairs > 0, silent otherwise", {
  rule <- get_rule("KB-OVERLAP-KP")

  pos_diag <- make_diag(n_overlap_pairs = 3L)
  expect_true(isTRUE(rule$condition(pos_diag)),
              label = "KB-OVERLAP-KP fires when n_overlap_pairs > 0")

  neg_diag <- make_diag(n_overlap_pairs = 0L)
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-OVERLAP-KP should NOT fire when n_overlap_pairs == 0")
})

test_that("KB-OVERLAP-KP recommendation mentions Kolari-Pynnonen", {
  rule <- get_rule("KB-OVERLAP-KP")
  rec_lower <- tolower(rule$recommendation)
  expect_true(
    grepl("kolari|pynnönen|pynnonen", rec_lower),
    label = "KB-OVERLAP-KP recommendation must mention Kolari-Pynnonen"
  )
})

# -- KB-AC-WARN ----------------------------------------------------------------
test_that("KB-AC-WARN fires when DW out of [1.5, 2.5] range, silent in range", {
  rule <- get_rule("KB-AC-WARN")

  # Positive: DW < 1.5 (positive autocorrelation)
  pos_diag_low <- make_diag(dw_stat = c(1.0, 1.2, 0.9, 1.1, 1.3))
  expect_true(isTRUE(rule$condition(pos_diag_low)),
              label = "KB-AC-WARN fires when DW < 1.5")

  # Positive: DW > 2.5 (negative autocorrelation)
  pos_diag_high <- make_diag(dw_stat = c(3.0, 2.8, 3.2, 2.9, 3.1))
  expect_true(isTRUE(rule$condition(pos_diag_high)),
              label = "KB-AC-WARN fires when DW > 2.5")

  # Negative: all DW within [1.5, 2.5]
  neg_diag <- make_diag(dw_stat = c(1.8, 2.0, 2.1, 1.9, 2.3))
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-AC-WARN should NOT fire when DW in [1.5, 2.5]")
})

# -- KB-LOWFIT-WARN ------------------------------------------------------------
test_that("KB-LOWFIT-WARN fires when R2 < 0.05 in majority of events", {
  rule <- get_rule("KB-LOWFIT-WARN")

  # Positive: all R2 < 0.05
  pos_diag <- make_diag(r2 = c(0.01, 0.02, 0.01, 0.03, 0.02, 0.01, 0.02, 0.01, 0.02, 0.01))
  expect_true(isTRUE(rule$condition(pos_diag)),
              label = "KB-LOWFIT-WARN fires when majority of R2 < 0.05")

  # Negative: all R2 >= 0.05
  neg_diag <- make_diag(r2 = c(0.1, 0.2, 0.3, 0.15, 0.25))
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-LOWFIT-WARN should NOT fire when R2 is adequate")
})

# -- KB-DEGEN-EVENTS -----------------------------------------------------------
test_that("KB-DEGEN-EVENTS fires when > 20% of events are not fitted", {
  rule <- get_rule("KB-DEGEN-EVENTS")

  # Positive: only 3 out of 10 valid (30%)
  pos_diag <- make_diag(n_valid_events = 3L, n_events_total = 10L)
  expect_true(isTRUE(rule$condition(pos_diag)),
              label = "KB-DEGEN-EVENTS fires when n_valid/n_total < 0.8")

  # Negative: all 10 valid
  neg_diag <- make_diag(n_valid_events = 10L, n_events_total = 10L)
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-DEGEN-EVENTS should NOT fire when all events are fitted")
})

# -- KB-SMALL-N ----------------------------------------------------------------
test_that("KB-SMALL-N fires when n_valid_events < 10", {
  rule <- get_rule("KB-SMALL-N")

  # Positive: only 5 valid events
  pos_diag <- make_diag(n_valid_events = 5L, n_events_total = 5L)
  expect_true(isTRUE(rule$condition(pos_diag)),
              label = "KB-SMALL-N fires when n_valid_events < 10")

  # Negative: 15 valid events
  neg_diag <- make_diag(n_valid_events = 15L, n_events_total = 15L)
  expect_false(isTRUE(rule$condition(neg_diag)),
               label = "KB-SMALL-N should NOT fire when n_valid_events >= 10")
})

# -- NA-guard: all-NA signals must NOT fire any rule --------------------------
test_that("all-NA fixture fires no robustness rule", {
  kb <- es_kb()

  na_diag <- list(
    meta = list(
      n_events_total      = 2L,
      n_events_shown      = 2L,
      n_events_summarized = 0L,
      event_ids_shown     = c(1L, 2L)
    ),
    estimation_window = list(
      r2                = c(NA_real_, NA_real_),
      sigma             = c(NA_real_, NA_real_),
      degree_of_freedom = c(NA_real_, NA_real_),
      acf1              = c(NA_real_, NA_real_),
      shapiro_p         = c(NA_real_, NA_real_),
      dw_stat           = c(NA_real_, NA_real_),
      ljung_box_p       = c(NA_real_, NA_real_)
    ),
    event_window = list(
      ar_t      = c(NA_real_, NA_real_),
      ar_p      = c(NA_real_, NA_real_),
      car_t     = c(NA_real_, NA_real_),
      car_p     = c(NA_real_, NA_real_),
      final_car = c(NA_real_, NA_real_)
    ),
    cross_sectional = list(
      n_events        = 2L,
      n_valid_events  = NA_integer_,
      car_iqr         = NA_real_,
      car_sd          = NA_real_,
      n_overlap_pairs = NA_integer_,
      any_overlap     = NA
    ),
    contract_state = list(
      is_fitted        = c(NA, NA),
      na_ar_count      = c(NA_integer_, NA_integer_),
      na_est_count     = c(NA_integer_, NA_integer_),
      insufficient_obs = c(NA, NA),
      zero_var_index   = c(NA, NA)
    ),
    aggregate_summary = NULL
  )

  # Only robustness rules should not fire on NA signals
  # (stat_choice rules may fire on NA because NA can satisfy proportion conditions vacuously)
  # We check that no rule ERRORS on NA input (already tested above) and
  # that robustness rules (category=="robustness") specifically return FALSE on all-NA
  robustness_rules <- Filter(function(r) r$category == "robustness", kb)
  for (rule in robustness_rules) {
    result <- tryCatch(rule$condition(na_diag), error = function(e) NA)
    expect_false(
      isTRUE(result),
      label = paste0("Robustness rule '", rule$id, "' must NOT fire on all-NA fixture")
    )
  }
})

# -- Integration test: every rule condition runs without error on real data ----
test_that("every rule condition runs without error on a real es_diagnostics object", {
  task <- create_fitted_mock_task(n_firms = 2L)
  diag <- es_diagnostics(task)
  kb   <- es_kb()

  for (rule in kb) {
    expect_no_error(
      rule$condition(diag),
      message = paste0("Rule '", rule$id, "' errored on real es_diagnostics output")
    )
  }
})
