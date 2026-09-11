# Regression net for the return-shape contract system (Phase 28, Plan 02).
# Requirement: APIS-02 — R/shape_contracts.R, option-gated default-off,
# warn-once-never-stop, covering single-event + multi-event tibbles and both
# is_fitted = FALSE degenerate shapes.
#
# All option state is contained via withr::local_options() so tests do not
# leak to the global option environment.

suppressPackageStartupMessages({
  library(EventStudy, warn.conflicts = FALSE)
})

# ---------------------------------------------------------------------------
# Test helpers
# ---------------------------------------------------------------------------

# Build a fully-fitted mock task with ART (single-event) and CSectT (multi-event)
.make_fitted_task <- function() {
  ps <- ParameterSet$new(return_model = MarketModel$new())
  ps$single_event_statistics <- SingleEventStatisticsSet$new()
  ps$single_event_statistics$add_test(ARTTest$new())
  ps$multi_event_statistics <- MultiEventStatisticsSet$new()
  ps$multi_event_statistics$add_test(CSectTTest$new())

  task <- create_fitted_mock_task()  # from helper-mock-data.R
  # The fixture already has ART + CART + CSectT; return it directly
  task
}

# Build a degenerate (is_fitted = FALSE) task by feeding insufficient data
.make_degenerate_task <- function() {
  # Use the degenerate model fixture that produces all-NA abnormal returns
  degen_data <- create_degenerate_model_data_insufficient()
  model <- MarketModel$new()
  model$degenerate_mode <- "lenient"
  model$event_id    <- "DEGEN_E1"
  model$firm_symbol <- "DEGEN"
  suppressWarnings(model$fit(degen_data))
  # Build a degenerate ART result tibble (same cols, NA values)
  abs_ret <- suppressWarnings(model$abnormal_returns(degen_data)) %>%
    dplyr::mutate(event_window = 1)
  art_result   <- ARTTest$new()$compute(abs_ret, model)
  cart_result  <- CARTTest$new()$compute(abs_ret, model)
  list(art = art_result, cart = cart_result, model = model, data = abs_ret)
}


# ===========================================================================
# SHAPE-01: Off by default — no warnings emitted by existing pipeline
# ===========================================================================

test_that("SHAPE-01: option off by default — pipeline emits no shape warnings", {
  withr::local_options(list(EventStudy.shape_contracts = NULL))
  expect_false(.resolve_shape_contract_mode())
  # The option must default to FALSE; no shape check runs
  task <- .make_fitted_task()
  expect_no_warning(.check_single_event_shape(task$data_tbl$ART[[1]], "ART"))
  expect_no_warning(.check_aar_caar_shape(task$aar_caar_tbl$CSectT[[1]], "CSectT"))
})

test_that("SHAPE-01b: option explicitly FALSE — check functions are no-ops", {
  withr::local_options(list(EventStudy.shape_contracts = FALSE))
  expect_false(.resolve_shape_contract_mode())
})

test_that("SHAPE-01c: option TRUE is detected", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  expect_true(.resolve_shape_contract_mode())
})


# ===========================================================================
# SHAPE-02: Option ON + valid fitted tibbles — no warning
# ===========================================================================

test_that("SHAPE-02a: option on + valid ART tibble — no warning", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  task <- .make_fitted_task()
  expect_no_warning(
    .check_single_event_shape(task$data_tbl$ART[[1]], "ART")
  )
})

test_that("SHAPE-02b: option on + valid CART tibble — no warning", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  task <- .make_fitted_task()
  expect_no_warning(
    .check_single_event_shape(task$data_tbl$CART[[1]], "CART")
  )
})

test_that("SHAPE-02c: option on + valid CSectT (AAR/CAAR) tibble — no warning", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  task <- .make_fitted_task()
  expect_no_warning(
    .check_aar_caar_shape(task$aar_caar_tbl$CSectT[[1]], "CSectT")
  )
})


# ===========================================================================
# SHAPE-03: Option ON + drifted column — exactly one warning, no error
# ===========================================================================

test_that("SHAPE-03a: option on + missing column in ART tibble — exactly one warning, no error/stop", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  task <- .make_fitted_task()
  # Drop a required column to simulate drift
  drifted <- task$data_tbl$ART[[1]][, setdiff(names(task$data_tbl$ART[[1]]), "ar_t")]
  expect_warning(
    .check_single_event_shape(drifted, "ART"),
    regexp = "shape contract violated.*missing column.*ar_t",
    fixed = FALSE
  )
  # Must not stop
  expect_no_error(
    suppressWarnings(.check_single_event_shape(drifted, "ART"))
  )
})

test_that("SHAPE-03b: option on + missing column in CSectT tibble — exactly one warning, no error", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  task <- .make_fitted_task()
  drifted <- task$aar_caar_tbl$CSectT[[1]][, setdiff(names(task$aar_caar_tbl$CSectT[[1]]), "caar_t")]
  expect_warning(
    .check_aar_caar_shape(drifted, "CSectT"),
    regexp = "shape contract violated.*missing column.*caar_t",
    fixed = FALSE
  )
  expect_no_error(
    suppressWarnings(.check_aar_caar_shape(drifted, "CSectT"))
  )
})

test_that("SHAPE-03c: exactly ONE warning emitted (not multiple) for a single drifted column", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  task <- .make_fitted_task()
  drifted <- task$data_tbl$ART[[1]][, setdiff(names(task$data_tbl$ART[[1]]), "ar_t")]
  warns <- character(0)
  withCallingHandlers(
    .check_single_event_shape(drifted, "ART"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 1L)
})


# ===========================================================================
# SHAPE-04: Degenerate (is_fitted = FALSE) shapes are VALID — no warning
# ===========================================================================

test_that("SHAPE-04a: single-event ART is_fitted=FALSE degenerate (all-NA) — no warning", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  degen <- .make_degenerate_task()
  expect_no_warning(
    .check_single_event_shape(degen$art, "ART")
  )
})

test_that("SHAPE-04b: single-event CART is_fitted=FALSE degenerate (all-NA) — no warning", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  degen <- .make_degenerate_task()
  expect_no_warning(
    .check_single_event_shape(degen$cart, "CART")
  )
})

test_that("SHAPE-04c: degenerate CSectT (multi-event, all-NA aar/caar) — no warning", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  # Build a degenerate CSectT-shaped tibble manually (same cols, NA values)
  degen_csect <- tibble::tibble(
    relative_index  = -2L:2L,
    aar             = NA_real_,
    n_events        = 1L,
    n_valid_events  = 0L,
    n_pos           = 0L,
    n_neg           = 0L,
    aar_t           = NA_real_,
    caar            = NA_real_,
    caar_t          = NA_real_,
    car_window      = paste0("[-2, ", -2L:2L, "]")
  )
  expect_no_warning(
    .check_aar_caar_shape(degen_csect, "CSectT")
  )
})

test_that("SHAPE-04d: drifted degenerate (missing column) — exactly one warning, no error", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  # Degenerate shape but a column renamed (true structural drift)
  degen <- .make_degenerate_task()
  drifted_degen <- degen$art[, setdiff(names(degen$art), "abnormal_returns")]
  # Rename to simulate a column-rename drift
  names(drifted_degen)[names(drifted_degen) == "ar_t"] <- "art_statistic"

  warns <- character(0)
  withCallingHandlers(
    .check_single_event_shape(drifted_degen, "ART"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 1L)
  expect_match(warns[1], "shape contract violated")
  expect_no_error(suppressWarnings(.check_single_event_shape(drifted_degen, "ART")))
})


# ===========================================================================
# SHAPE-05: Unknown stat names are silently skipped (additive-only contract)
# ===========================================================================

test_that("SHAPE-05: unknown stat name is silently skipped — no warning, no error", {
  withr::local_options(list(EventStudy.shape_contracts = TRUE))
  fake_tbl <- tibble::tibble(x = 1:3, y = letters[1:3])
  expect_no_warning(.check_single_event_shape(fake_tbl, "UnknownStat"))
  expect_no_warning(.check_aar_caar_shape(fake_tbl, "UnknownMultiStat"))
})


# ===========================================================================
# SHAPE-06: .check_shape() internals — direct unit tests
# ===========================================================================

test_that("SHAPE-06a: .check_shape() returns invisible TRUE on valid input", {
  tbl  <- tibble::tibble(a = 1L, b = "x")
  spec <- c(a = "numeric", b = "character")
  result <- .check_shape(tbl, names(spec), spec, "test-context")
  expect_true(result)
})

test_that("SHAPE-06b: .check_shape() warns on missing column", {
  tbl  <- tibble::tibble(a = 1L)
  spec <- c(a = "numeric", b = "character")
  expect_warning(
    .check_shape(tbl, names(spec), spec, "test-context"),
    regexp = "missing column"
  )
})

test_that("SHAPE-06c: .check_shape() never stops on mismatch", {
  tbl  <- tibble::tibble(a = "wrong_type_here")
  spec <- c(a = "numeric")
  expect_no_error(suppressWarnings(.check_shape(tbl, names(spec), spec, "test-ctx")))
})
