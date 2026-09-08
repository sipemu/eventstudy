test_that("theme_eventstudy() returns a theme/gg object", {
  th <- theme_eventstudy()
  expect_true(inherits(th, "theme"))
  expect_true(inherits(th, "gg"))
})

test_that("theme_eventstudy() accepts base_size and base_family", {
  th <- theme_eventstudy(base_size = 14, base_family = "")
  expect_true(inherits(th, "theme"))
})

test_that("es_colours is a non-empty named character vector of valid hex codes", {
  expect_true(is.character(es_colours))
  expect_true(!is.null(names(es_colours)))
  expect_true(length(es_colours) > 0)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", es_colours)))
  expect_identical(unname(es_colours[["primary"]]), "#2563eb")
})
