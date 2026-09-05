# Tests for the CustomProvider seam and the ProviderBase abstract contract.
#
# CustomProvider is the in-process, offline end-to-end vehicle for the provider
# seam: a user function(prompt, schema) -> list|character wrapped so that a
# throwing function degrades to exactly one warning + NA (mirroring the
# .handle_degenerate() discipline in R/contract.R). No HTTP is involved.

test_that("CustomProvider happy path returns an es_provider_response", {
  p <- CustomProvider$new(function(prompt, schema) "canned advice")
  res <- p$complete("hi")

  expect_true(inherits(res, "es_provider_response"))
  expect_identical(res$source, "custom")
  expect_false(res$is_deterministic)
  expect_identical(res$text, "canned advice")
  expect_null(res$error)
})

test_that("a throwing custom fn degrades to exactly one warning + NA, never errors", {
  p <- CustomProvider$new(function(prompt, schema) stop("boom"))

  expect_warning(res <- p$complete("hi"), "failed")
  expect_true(inherits(res, "es_provider_response"))
  expect_true(is.na(res$text))
  expect_type(res$text, "character")
  expect_false(is.null(res$error))
  expect_type(res$error, "character")
})

test_that("only one warning is emitted on the degrade path", {
  p <- CustomProvider$new(function(prompt, schema) stop("boom"))
  warns <- character(0)
  withCallingHandlers(
    p$complete("hi"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 1L)
})

test_that("a custom fn returning NULL degrades to one warning + NA, never errors", {
  p <- CustomProvider$new(function(prompt, schema) NULL)

  expect_warning(res <- p$complete("hi"), "failed")
  expect_true(inherits(res, "es_provider_response"))
  expect_true(is.na(res$text))
  expect_type(res$text, "character")
  expect_false(is.null(res$error))
})

test_that("a custom fn returning character(0) degrades to one warning + NA, never errors", {
  p <- CustomProvider$new(function(prompt, schema) character(0))

  expect_warning(res <- p$complete("hi"), "failed")
  expect_true(inherits(res, "es_provider_response"))
  expect_true(is.na(res$text))
  expect_type(res$text, "character")
  expect_false(is.null(res$error))
})

test_that("empty-return degrade path (NULL / character(0)) emits EXACTLY ONE warning", {
  count_warnings <- function(expr) {
    n <- 0L
    withCallingHandlers(
      force(expr),
      warning = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    n
  }
  for (fn in list(function(prompt, schema) NULL,
                  function(prompt, schema) character(0))) {
    p <- CustomProvider$new(fn)
    expect_equal(count_warnings(p$complete("hi")), 1L)
  }
})

test_that("ProviderBase is abstract: complete() stops with a clear message", {
  base <- ProviderBase$new()
  expect_error(base$complete("hi"), "abstract")
})

test_that("the (prompt, schema) seam reaches the user function", {
  fn <- function(prompt, schema) if (is.null(schema)) "noschema" else "hasschema"
  p <- CustomProvider$new(fn)

  expect_identical(p$complete("hi")$text, "noschema")
  expect_identical(p$complete("hi", schema = list(x = 1))$text, "hasschema")
})
