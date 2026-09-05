# Tests for prepare_event_study() degenerate-input handling
#
# Covers: missing event date in lenient mode (warn + zero windows) and strict
# mode (stop naming the event), plus valid-date windowing unchanged.

test_that("missing event date lenient: exactly one warning naming event_id and firm_symbol", {
  # Non-existent event date in lenient (default) mode must emit exactly one
  # warning whose text contains the offending event_id AND firm_symbol.
  task <- create_mock_task(n_firms = 1)
  ps   <- ParameterSet$new()

  # Inject a date that does not exist in the trading data
  task$data_tbl$request[[1]]$event_date <- "99.99.9999"

  warnings_seen <- character(0)
  withCallingHandlers(
    {
      result <- prepare_event_study(task, ps)
    },
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings_seen, 1)
  expect_true(grepl("1", warnings_seen[1]))            # event_id
  expect_true(grepl("FIRM_A", warnings_seen[1]))       # firm_symbol
})


test_that("missing event date lenient: returned data_tbl has all-zero windows", {
  # After lenient warning, the offending row's data must have event_window=0L
  # and estimation_window=0L on every row so the model layer degrades cleanly.
  task <- create_mock_task(n_firms = 1)
  ps   <- ParameterSet$new()

  task$data_tbl$request[[1]]$event_date <- "99.99.9999"

  result <- suppressWarnings(prepare_event_study(task, ps))

  d <- result$data_tbl$data[[1]]
  expect_true(all(d$event_window      == 0L))
  expect_true(all(d$estimation_window == 0L))
  expect_true(all(is.na(d$relative_index)))
})


test_that("missing event date strict: stop() names component, event_id, firm_symbol", {
  # Strict mode must raise stop() whose message contains the three keys.
  task <- create_mock_task(n_firms = 1)
  ps   <- ParameterSet$new(degenerate_handling = "strict")

  task$data_tbl$request[[1]]$event_date <- "99.99.9999"

  err <- tryCatch(
    prepare_event_study(task, ps),
    error = function(e) conditionMessage(e)
  )

  expect_type(err, "character")
  expect_true(grepl("prepare_event_study", err))
  expect_true(grepl("1",      err))   # event_id
  expect_true(grepl("FIRM_A", err))   # firm_symbol
})


test_that("valid event date: window columns byte-identical to pre-change output", {
  # A non-degenerate event must produce exactly the same window columns as
  # before — the mode-threading must not disturb the happy path.
  task <- create_mock_task(n_firms = 2)
  ps   <- ParameterSet$new()

  result <- prepare_event_study(task, ps)

  for (i in seq_len(nrow(result$data_tbl))) {
    d <- result$data_tbl$data[[i]]
    # Windows must be integer 0/1, relative_index must be present
    expect_true("event_window"      %in% names(d))
    expect_true("estimation_window" %in% names(d))
    expect_true("relative_index"    %in% names(d))
    # At least some event-window and estimation-window rows must exist
    expect_true(sum(d$event_window)      > 0)
    expect_true(sum(d$estimation_window) > 0)
  }
})


test_that("lenient mode: one valid + one bad event — only bad event warns, good event windows intact", {
  # When a two-firm task has one valid date and one missing date, only the
  # missing one should warn, and the valid firm's windows must be unaffected.
  task <- create_mock_task(n_firms = 2)
  ps   <- ParameterSet$new()

  # Corrupt only the second firm's event date
  task$data_tbl$request[[2]]$event_date <- "99.99.9999"

  warnings_seen <- character(0)
  result <- withCallingHandlers(
    prepare_event_study(task, ps),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Exactly one warning
  expect_length(warnings_seen, 1)

  # First firm (valid date) still has real windows
  d_good <- result$data_tbl$data[[1]]
  expect_true(sum(d_good$event_window)      > 0)
  expect_true(sum(d_good$estimation_window) > 0)

  # Second firm (missing date) has all-zero windows
  d_bad <- result$data_tbl$data[[2]]
  expect_true(all(d_bad$event_window      == 0L))
  expect_true(all(d_bad$estimation_window == 0L))
})
