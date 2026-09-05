test_that("ParameterSet creates with all defaults", {
  ps = ParameterSet$new()
  expect_true(inherits(ps, "ParameterSet"))
  expect_true(inherits(ps$return_calculation, "ReturnCalculation"))
  expect_true(inherits(ps$return_model, "ModelBase"))
  expect_true(inherits(ps$single_event_statistics, "StatisticsSetBase"))
  expect_true(inherits(ps$multi_event_statistics, "StatisticsSetBase"))
})


test_that("ParameterSet print method works", {
  ps = ParameterSet$new()
  expect_output(print(ps), "EventStudy ParameterSet")
  expect_output(print(ps), "Return calculation")
  expect_output(print(ps), "Return model")
  expect_output(print(ps), "Single event tests")
  expect_output(print(ps), "Multi event tests")
})


test_that("ParameterSet accepts custom parameters", {
  ps = ParameterSet$new(
    return_calculation = LogReturn$new(),
    return_model = MarketAdjustedModel$new(),
    single_event_statistics = NULL,
    multi_event_statistics = NULL
  )
  expect_equal(ps$return_calculation$name, "log return")
  expect_equal(ps$return_model$model_name, "MarketAdjustedModel")
  expect_null(ps$single_event_statistics)
  expect_null(ps$multi_event_statistics)
})


test_that("ParameterSet rejects invalid objects", {
  expect_error(
    ParameterSet$new(return_calculation = "not a class"),
    "not of type"
  )
  expect_error(
    ParameterSet$new(return_model = "not a class"),
    "not of type"
  )
})


# ---------------------------------------------------------------------------
# degenerate_handling field validation (CONTRACT-02 / STRIDE T-01-02)
# ---------------------------------------------------------------------------

test_that("ParameterSet degenerate_handling defaults to NULL", {
  ps <- ParameterSet$new()
  expect_null(ps$degenerate_handling)
})

test_that("ParameterSet degenerate_handling accepts 'strict'", {
  ps <- ParameterSet$new(degenerate_handling = "strict")
  expect_equal(ps$degenerate_handling, "strict")
})

test_that("ParameterSet degenerate_handling accepts 'lenient'", {
  ps <- ParameterSet$new(degenerate_handling = "lenient")
  expect_equal(ps$degenerate_handling, "lenient")
})

test_that("ParameterSet degenerate_handling rejects wrong-case 'Lenient' via match.arg", {
  expect_error(
    ParameterSet$new(degenerate_handling = "Lenient"),
    "arg"
  )
})

test_that("ParameterSet degenerate_handling rejects unknown value 'bogus' via match.arg", {
  expect_error(
    ParameterSet$new(degenerate_handling = "bogus"),
    "arg"
  )
})
