# check_type() tests

test_that("types within the limit pass silently", {
  expect_no_condition(check_type("default"))
  expect_no_condition(check_type(c("season", "weekend")))
  expect_no_condition(check_type("season", max = 1))
  expect_identical(check_type(c("season", "weekend")), c("season", "weekend"))
})

test_that("too many types raise an error", {
  expect_error(
    check_type(c("season", "weekend", "daylight")),
    regexp = "at most 2 values"
  )
  expect_error(
    check_type(c("season", "weekend"), max = 1),
    regexp = "at most 1 value,"
  )
})

test_that("errors are reported from the calling function", {
  f <- function(type) check_type(type, max = 1)
  err <- tryCatch(f(c("a", "b")), error = function(e) e)
  expect_identical(deparse(conditionCall(err)), "f(c(\"a\", \"b\"))")
})

test_that("plotting functions reject more than two types", {
  dat <- head(mydata, 500)
  types <- c("season", "weekend", "daylight")

  expect_error(windRose(dat, type = types, plot = FALSE), "at most 2")
  expect_error(
    pollutionRose(dat, pollutant = "nox", type = types, plot = FALSE),
    "at most 2"
  )
  expect_error(
    scatterPlot(dat, x = "nox", y = "no2", type = types, plot = FALSE),
    "at most 2"
  )
  expect_error(
    trendLevel(dat, pollutant = "nox", type = types, plot = FALSE),
    "at most 2"
  )
})

test_that("single-type functions reject two types", {
  dat <- head(mydata, 500)
  expect_error(
    corPlot(dat, type = c("season", "weekend"), plot = FALSE),
    "at most 1 value"
  )
})
