# timePlot tests — plot = FALSE throughout
# No GAM needed for most tests; only smooth = TRUE path uses one (skip on CRAN)
dat <- selectByDate(mydata, year = 2003, month = 1)
dat3 <- selectByDate(mydata, year = 2001:2003)

# Shared result — single pollutant, all defaults
tp <- timePlot(dat, pollutant = "nox", plot = FALSE)

# =============================================================================
# Return value structure
# =============================================================================

test_that("timePlot returns an openair object with expected components", {
  expect_s3_class(tp, "openair")
  expect_named(tp, c("plot", "data", "call"))
  expect_s3_class(tp$plot, "ggplot")
  expect_s3_class(tp$data, "data.frame")
})

test_that("$data has date, variable and value columns", {
  expect_true(all(c("date", "variable", "value") %in% names(tp$data)))
})

test_that("$data row count matches input row count for single pollutant", {
  expect_equal(nrow(tp$data), nrow(dat))
})

# =============================================================================
# Multiple pollutants
# =============================================================================

test_that("multiple pollutants produce one row per pollutant per timestamp", {
  tp_multi <- timePlot(dat, pollutant = c("nox", "no2"), plot = FALSE)
  expect_equal(nrow(tp_multi$data), 2L * nrow(dat))
  expect_setequal(levels(tp_multi$data$variable), c("nox", "no2"))
})

# =============================================================================
# group = TRUE
# =============================================================================

test_that("group = TRUE still returns one row per pollutant per timestamp", {
  tp_grp <- timePlot(
    dat,
    pollutant = c("nox", "no2"),
    group = TRUE,
    plot = FALSE
  )
  expect_equal(nrow(tp_grp$data), 2L * nrow(dat))
})

# =============================================================================
# group as a column name
# =============================================================================

test_that("group as a column name adds that column to $data", {
  dat_long <- rbind(
    cbind(dat[, c("date", "nox")], site = "A"),
    cbind(transform(dat[, c("date", "nox")], nox = nox * 1.2), site = "B")
  )
  tp_site <- timePlot(dat_long, pollutant = "nox", group = "site", plot = FALSE)
  expect_true("site" %in% names(tp_site$data))
  expect_setequal(unique(tp_site$data$site), c("A", "B"))
})

# =============================================================================
# avg.time
# =============================================================================

test_that("avg.time = 'day' reduces rows to one per day", {
  tp_day <- timePlot(dat, pollutant = "nox", avg.time = "day", plot = FALSE)
  expect_lte(nrow(tp_day$data), 31L)
  expect_gt(nrow(tp_day$data), 1L)
})

test_that("avg.time = 'month' applied to 3 years gives ~36 rows per pollutant", {
  tp_mon <- timePlot(dat3, pollutant = "nox", avg.time = "month", plot = FALSE)
  expect_equal(nrow(tp_mon$data), 36L)
})

# =============================================================================
# normalise
# =============================================================================

test_that("normalise = 'mean' centres values around 1 for each pollutant", {
  tp_norm <- timePlot(dat, pollutant = "nox", normalise = "mean", plot = FALSE)
  expect_equal(mean(tp_norm$data$value, na.rm = TRUE), 1, tolerance = 1e-9)
})

test_that("normalise by date sets values at that date to 100", {
  tp_date <- timePlot(
    dat3,
    pollutant = "nox",
    normalise = "1/1/2003",
    plot = FALSE
  )
  # find the row closest to 2003-01-01
  target <- as.POSIXct("2003-01-01", tz = "GMT")
  closest <- tp_date$data$date[which.min(abs(tp_date$data$date - target))]
  val_at_date <- tp_date$data$value[tp_date$data$date == closest]
  expect_equal(val_at_date, 100, tolerance = 1e-6)
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'weekend' adds a weekend column to $data", {
  tp_type <- timePlot(dat, pollutant = "nox", type = "weekend", plot = FALSE)
  expect_true("weekend" %in% names(tp_type$data))
})

# =============================================================================
# smooth (skip on CRAN — requires GAM)
# =============================================================================

test_that("smooth = TRUE produces a valid ggplot without error", {
  skip_on_cran()
  expect_no_error(
    timePlot(dat, pollutant = "nox", smooth = TRUE, plot = FALSE)
  )
})

# =============================================================================
# Input validation
# =============================================================================

test_that("multiple pollutants and multiple percentiles errors", {
  expect_error(
    timePlot(
      dat,
      pollutant = c("nox", "no2"),
      avg.time = "day",
      percentile = c(5, 95),
      plot = FALSE
    ),
    regexp = "one.*pollutant|percentile"
  )
})

test_that("group column not found in data errors", {
  expect_error(
    timePlot(dat, pollutant = "nox", group = "not_a_column", plot = FALSE),
    regexp = "not found"
  )
})

test_that("more than two types errors", {
  expect_error(
    timePlot(
      dat,
      pollutant = "nox",
      type = c("season", "weekend", "year"),
      plot = FALSE
    ),
    regexp = "2.*type"
  )
})
