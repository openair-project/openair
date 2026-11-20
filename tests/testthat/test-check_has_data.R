# check_has_data() and checkPrep(require_data = TRUE) tests
dat <- data.frame(
  date = as.POSIXct("2020-01-01", tz = "GMT") + 3600 * 0:2,
  num = c(1.5, 2, NA),
  all_na = NA_real_
)

test_that("check_has_data() errors for empty data or all-missing variables", {
  expect_error(check_has_data(dat[0, ], "num"), regexp = "has no rows")
  expect_error(
    check_has_data(dat, c("num", "all_na")),
    regexp = "only missing values"
  )
  expect_no_error(check_has_data(dat, "num"))
})

test_that("checkPrep() only checks for data when require_data = TRUE", {
  # off by default, so non-plotting functions can accept these inputs
  expect_no_error(checkPrep(dat[0, ], c("date", "num"), "default"))
  expect_no_error(checkPrep(dat, c("date", "all_na"), "default"))

  expect_error(
    checkPrep(dat[0, ], c("date", "num"), "default", require_data = TRUE),
    regexp = "has no rows"
  )
  expect_error(
    checkPrep(dat, c("date", "all_na"), "default", require_data = TRUE),
    regexp = "only missing values"
  )
  expect_no_error(
    checkPrep(dat, c("date", "num"), "default", require_data = TRUE)
  )
})

test_that("checkPrep() checks for data after removing invalid values", {
  # all wind speeds are negative, so are removed, leaving nothing to plot
  neg_ws <- data.frame(ws = c(-1, -2), wd = c(90, 180))
  expect_error(
    suppressWarnings(
      checkPrep(neg_ws, c("ws", "wd"), "default", require_data = TRUE)
    ),
    regexp = "only missing values"
  )
})

test_that("plotting functions give a clear error when there is no data", {
  dat_plot <- openair::mydata[1:200, ]
  dat_plot$nox <- NA_real_

  # errors are reported from the user-facing function
  err <- tryCatch(
    polarPlot(dat_plot, pollutant = "nox", plot = FALSE),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "only missing values")
  expect_identical(as.character(conditionCall(err)[[1]]), "polarPlot")

  expect_error(
    timePlot(dat_plot[0, ], pollutant = "no2", plot = FALSE),
    regexp = "has no rows"
  )
  expect_error(
    timeVariation(dat_plot, pollutant = "nox", plot = FALSE),
    regexp = "only missing values"
  )
})
