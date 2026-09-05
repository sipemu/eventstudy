test_that("SingleEventStatisticsSet has default tests", {
  ses = SingleEventStatisticsSet$new()
  expect_equal(length(ses$tests), 2)
  expect_true(inherits(ses$tests[[1]], "ARTTest"))
  expect_true(inherits(ses$tests[[2]], "CARTTest"))
})


test_that("MultiEventStatisticsSet has default tests", {
  mes = MultiEventStatisticsSet$new()
  expect_equal(length(mes$tests), 1)
  expect_true(inherits(mes$tests[[1]], "CSectTTest"))
})


test_that("SingleEventStatisticsSet validates against TestStatisticBase (bug fix)", {
  ses = SingleEventStatisticsSet$new()
  # Should accept any TestStatisticBase descendant
  expect_no_error(ses$add_test(ARTTest$new()))
})


test_that("MultiEventStatisticsSet validates against TestStatisticBase (bug fix)", {
  mes = MultiEventStatisticsSet$new()
  # Should accept any TestStatisticBase descendant
  expect_no_error(mes$add_test(SignTest$new()))
  expect_no_error(mes$add_test(RankTest$new()))
  expect_no_error(mes$add_test(BMPTest$new()))
})


test_that("StatisticsSetBase rejects non-test objects", {
  ses = SingleEventStatisticsSet$new()
  expect_error(ses$add_test("not a test"), "must be of class")
  expect_error(ses$add_test(42), "must be of class")
})


test_that("StatisticsSetBase initialize with custom tests", {
  tests = list(ARTTest$new())
  ses = StatisticsSetBase$new(tests = tests)
  expect_equal(length(ses$tests), 1)
})


test_that("StatisticsSetBase initialize rejects non-list", {
  expect_error(StatisticsSetBase$new(tests = "not a list"), "must be a list")
})
