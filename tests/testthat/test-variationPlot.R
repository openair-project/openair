# Small, fixed slices — subsample heavily to keep bootstrap fast
dat <- selectByDate(mydata, year = 2003, month = 1:3)
dat1 <- selectByDate(mydata, year = 2003, month = 1)

# Run the most-used configurations once and reuse
vp_mean <- variationPlot(
  dat,
  pollutant = "nox",
  x = "hour",
  B = 20,
  plot = FALSE
)
vp_median <- variationPlot(
  dat,
  pollutant = "nox",
  x = "hour",
  statistic = "median",
  plot = FALSE
)
tv <- timeVariation(dat, pollutant = "nox", B = 20, plot = FALSE)

# =============================================================================
# variationPlot
# =============================================================================

# --- Return value structure --------------------------------------------------

test_that("variationPlot returns an openair object with expected components", {
  expect_s3_class(vp_mean, "openair")
  expect_named(vp_mean, c("plot", "data", "call"))
  expect_s3_class(vp_mean$plot, "ggplot")
  expect_s3_class(vp_mean$data, "tbl_df")
})

test_that("variationPlot$data has required columns", {
  expect_true(all(
    c("x", "group", "mid", "min", "max", "ci", "statistic") %in%
      names(vp_mean$data)
  ))
})

# --- Data values -------------------------------------------------------------

test_that("variationPlot mean: mid is between min and max", {
  d <- vp_mean$data
  ok <- !is.na(d$mid) & !is.na(d$min) & !is.na(d$max)
  expect_true(all(d$mid[ok] >= d$min[ok]))
  expect_true(all(d$mid[ok] <= d$max[ok]))
})

test_that("variationPlot median: mid is between min and max", {
  d <- vp_median$data
  ok <- !is.na(d$mid) & !is.na(d$min) & !is.na(d$max)
  expect_true(all(d$mid[ok] >= d$min[ok]))
  expect_true(all(d$mid[ok] <= d$max[ok]))
})

test_that("variationPlot statistic column records the statistic used", {
  expect_true(all(vp_mean$data$statistic == "mean"))
  expect_true(all(vp_median$data$statistic == "median"))
})

test_that("variationPlot x = 'hour' produces 24 x levels", {
  expect_equal(length(unique(stats::na.omit(vp_mean$data$x))), 24L)
})

# --- normalise ---------------------------------------------------------------

test_that("normalise = TRUE scales mid values to be centred near 1", {
  vp_norm <- variationPlot(
    dat,
    pollutant = "nox",
    x = "hour",
    normalise = TRUE,
    B = 20,
    plot = FALSE
  )
  expect_equal(mean(vp_norm$data$mid, na.rm = TRUE), 1, tolerance = 0.1)
})

# --- multiple pollutants -----------------------------------------------------

test_that("multiple pollutants appear as separate group levels", {
  vp_multi <- variationPlot(
    dat,
    pollutant = c("nox", "no2"),
    x = "hour",
    B = 20,
    plot = FALSE
  )
  expect_setequal(levels(vp_multi$data$group), c("nox", "no2"))
})

# --- conf.int ----------------------------------------------------------------

test_that("multiple conf.int values produce multiple ci rows per x", {
  vp_ci <- variationPlot(
    dat,
    pollutant = "nox",
    x = "hour",
    conf.int = c(0.5, 0.95),
    B = 20,
    plot = FALSE
  )
  expect_setequal(unique(vp_ci$data$ci), c(0.5, 0.95))
})

# --- difference --------------------------------------------------------------

test_that("difference = TRUE adds a difference group level", {
  vp_diff <- variationPlot(
    dat,
    pollutant = c("nox", "no2"),
    x = "hour",
    difference = TRUE,
    B = 20,
    plot = FALSE
  )
  groups <- as.character(levels(vp_diff$data$group))
  expect_true(any(grepl("-", groups, fixed = TRUE)))
})

# --- input validation --------------------------------------------------------

test_that("variationPlot errors with more than two types", {
  expect_error(
    variationPlot(
      dat,
      pollutant = "nox",
      x = "hour",
      type = c("season", "year", "weekday"),
      plot = FALSE
    ),
    regexp = "type"
  )
})

test_that("variationPlot errors when group and multiple pollutants are both set", {
  expect_error(
    variationPlot(
      dat,
      pollutant = c("nox", "no2"),
      x = "hour",
      group = "season",
      plot = FALSE
    ),
    regexp = "pollutant"
  )
})

test_that("variationPlot errors when type duplicates pollutant", {
  expect_error(
    variationPlot(
      dat,
      pollutant = "nox",
      x = "hour",
      type = "nox",
      plot = FALSE
    ),
    regexp = "type"
  )
})

test_that("variationPlot errors on invalid statistic", {
  expect_error(
    variationPlot(
      dat,
      pollutant = "nox",
      x = "hour",
      statistic = "mode",
      plot = FALSE
    ),
    regexp = "statistic"
  )
})

# =============================================================================
# timeVariation
# =============================================================================

# --- Return value structure --------------------------------------------------

test_that("timeVariation returns an openair object with expected components", {
  expect_s3_class(tv, "openair")
  expect_true(all(c("plot", "data", "call") %in% names(tv)))
})

test_that("timeVariation$plot is a list named after panels", {
  default_panels <- c("hour.weekday", "hour", "month", "weekday")
  expect_true(all(default_panels %in% names(tv$plot)))
})

test_that("timeVariation$data is a list of tibbles named after panels", {
  default_panels <- c("hour.weekday", "hour", "month", "weekday")
  expect_true(all(default_panels %in% names(tv$data)))
  expect_true(all(purrr::map_lgl(tv$data[default_panels], tibble::is_tibble)))
})

test_that("timeVariation$main.plot is a patchwork object", {
  expect_true(inherits(tv$main.plot, "patchwork"))
})

# --- Panel content -----------------------------------------------------------

test_that("timeVariation hour panel has 24 x levels", {
  expect_equal(length(unique(stats::na.omit(tv$data$hour$x))), 24L)
})

test_that("timeVariation month panel has 12 x levels", {
  expect_equal(dplyr::n_distinct(stats::na.omit(tv$data$month$x)), 12L)
})

test_that("timeVariation mid is between min and max in all panels", {
  for (panel in c("hour", "month", "weekday")) {
    d <- tv$data[[panel]]
    ok <- !is.na(d$mid) & !is.na(d$min) & !is.na(d$max)
    expect_true(all(d$mid[ok] >= d$min[ok]), label = paste(panel, "mid >= min"))
    expect_true(all(d$mid[ok] <= d$max[ok]), label = paste(panel, "mid <= max"))
  }
})

# --- Custom panels -----------------------------------------------------------

test_that("custom panels argument produces output keyed by those panel names", {
  tv_custom <- timeVariation(
    dat1,
    pollutant = "nox",
    panels = c("hour", "month"),
    B = 20,
    plot = FALSE
  )
  expect_true(all(c("hour", "month") %in% names(tv_custom$data)))
  expect_false("weekday" %in% names(tv_custom$data))
})

# --- input validation --------------------------------------------------------

test_that("timeVariation errors with more than one type", {
  expect_error(
    timeVariation(
      dat,
      pollutant = "nox",
      type = c("season", "year"),
      B = 20,
      plot = FALSE
    ),
    regexp = "type"
  )
})
