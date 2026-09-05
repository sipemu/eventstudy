# Regression test for the dplyr::lag import bug (quick-260904-kxy).
#
# Bare lag() in SimpleReturn/LogReturn used to resolve to stats::lag inside the
# EventStudy namespace (dplyr::lag was not imported), which no-ops on a plain
# numeric vector. That made every return compute as 0 -- including row 1, which
# should be NA -- and the resulting zero-variance series tripped the market-model
# variance guard, so all abnormal returns / CARs came back NA. The bug was silent
# whenever dplyr was NOT attached (tests/audit run with dplyr attached, masking
# stats::lag). These tests lock the namespace resolution and the numeric behavior
# so the regression cannot silently return.

test_that("EventStudy namespace resolves lag to dplyr::lag, never stats::lag", {
  ns <- asNamespace("EventStudy")
  # Directly locks the import: if the @importFrom dplyr lag is dropped, `lag`
  # inside the package namespace falls back to stats::lag and this fails.
  expect_true(identical(get("lag", envir = ns), dplyr::lag))
  expect_false(identical(get("lag", envir = ns), stats::lag))
})

test_that("SimpleReturn: row 1 is NA and subsequent returns are correct non-zero values", {
  tbl <- tibble::tibble(adjusted = c(100, 110, 99))
  out <- SimpleReturn$new()$calculate_return(tbl)

  # Row 1 has no prior price -> NA (not a silent 0).
  expect_true(is.na(out$adjusted_return[1]))
  # (110 - 100) / 100 = 0.1
  expect_equal(out$adjusted_return[2], 0.1)
  # (99 - 110) / 110 = -0.1
  expect_equal(out$adjusted_return[3], -0.1)
  # And crucially: returns are NOT all zero (the bug's signature).
  expect_false(isTRUE(all(out$adjusted_return[-1] == 0)))
})

test_that("LogReturn: row 1 is NA and subsequent returns are correct non-zero values", {
  tbl <- tibble::tibble(adjusted = c(100, 110, 99))
  out <- LogReturn$new()$calculate_return(tbl)

  expect_true(is.na(out$adjusted_return[1]))
  expect_equal(out$adjusted_return[2], log(110 / 100))
  expect_equal(out$adjusted_return[3], log(99 / 110))
  expect_false(isTRUE(all(out$adjusted_return[-1] == 0)))
})
