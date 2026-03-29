skip_on_cran()

# scatterPlot tests — plot = FALSE throughout to skip rendering
# Small slice; daily averages reduce point count further for level/hexbin
dat <- selectByDate(mydata, year = 2003, month = 1)
dat_day <- timeAverage(dat, avg.time = "day", progress = FALSE)

# Shared results — method = "scatter" is the workhorse
sp_default <- scatterPlot(dat, x = "nox", y = "no2", plot = FALSE)
sp_z <- scatterPlot(dat, x = "nox", y = "no2", z = "o3", plot = FALSE)
sp_group <- scatterPlot(
  dat,
  x = "nox",
  y = "no2",
  group = "weekend",
  plot = FALSE
)

# --- Return value structure --------------------------------------------------

test_that("scatterPlot returns an openair object with expected components", {
  expect_s3_class(sp_default, "openair")
  expect_named(sp_default, c("plot", "data", "call"))
  expect_s3_class(sp_default$plot, "ggplot")
})

test_that("scatterPlot$data contains at least the x and y columns", {
  expect_true(all(c("nox", "no2") %in% names(sp_default$data)))
})

test_that("scatterPlot$data row count matches input after no averaging", {
  expect_equal(nrow(sp_default$data), nrow(dat))
})

# --- z variable --------------------------------------------------------------

test_that("z column is present in data when z is specified", {
  expect_true("o3" %in% names(sp_z$data))
})

# --- group variable ----------------------------------------------------------

test_that("group column is present in data when group is specified", {
  expect_true("weekend" %in% names(sp_group$data))
})

test_that("group and type cannot overlap", {
  expect_error(
    scatterPlot(
      dat,
      x = "nox",
      y = "no2",
      group = "weekend",
      type = "weekend",
      plot = FALSE
    ),
    regexp = "group"
  )
})

# --- type / faceting ---------------------------------------------------------

test_that("type = 'season' adds a season column to the data", {
  result <- scatterPlot(
    dat,
    x = "nox",
    y = "no2",
    type = "season",
    plot = FALSE
  )
  expect_true("season" %in% names(result$data))
})

# --- avg.time ----------------------------------------------------------------

test_that("avg.time = 'day' reduces row count to number of days", {
  result <- scatterPlot(
    dat,
    x = "nox",
    y = "no2",
    avg.time = "day",
    plot = FALSE
  )
  expect_lte(nrow(result$data), 31L)
  expect_gt(nrow(result$data), 1L)
})

# --- method variants ---------------------------------------------------------

test_that("method = 'hexbin' returns an openair object with a ggplot", {
  result <- scatterPlot(
    dat,
    x = "nox",
    y = "no2",
    method = "hexbin",
    plot = FALSE
  )
  expect_s3_class(result, "openair")
  expect_s3_class(result$plot, "ggplot")
})

test_that("method = 'density' returns an openair object with a ggplot", {
  result <- scatterPlot(
    dat,
    x = "nox",
    y = "no2",
    method = "density",
    plot = FALSE
  )
  expect_s3_class(result, "openair")
  expect_s3_class(result$plot, "ggplot")
})

test_that("method = 'level' requires z; errors without it", {
  expect_error(
    scatterPlot(dat_day, x = "nox", y = "no2", method = "level", plot = FALSE),
    regexp = "z"
  )
})

test_that("method = 'level' returns an openair object when z is supplied", {
  result <- scatterPlot(
    dat_day,
    x = "nox",
    y = "no2",
    z = "o3",
    method = "level",
    plot = FALSE
  )
  expect_s3_class(result, "openair")
  expect_s3_class(result$plot, "ggplot")
})

# --- log scales --------------------------------------------------------------

test_that("log.x = TRUE drops non-positive x values", {
  result <- scatterPlot(dat, x = "nox", y = "no2", log.x = TRUE, plot = FALSE)
  expect_true(all(result$data$nox > 0, na.rm = TRUE))
})

test_that("log.y = TRUE drops non-positive y values", {
  result <- scatterPlot(dat, x = "nox", y = "no2", log.y = TRUE, plot = FALSE)
  expect_true(all(result$data$no2 > 0, na.rm = TRUE))
})

# --- Input validation --------------------------------------------------------

test_that("invalid method raises an error", {
  expect_error(
    scatterPlot(dat, x = "nox", y = "no2", method = "violin", plot = FALSE),
    regexp = "method"
  )
})
