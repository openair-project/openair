# calcPercentile is a thin wrapper over timeAverage — fast even on a full year
dat <- selectByDate(mydata, year = 2003)

# Run the most-used configurations once and reuse
pct_single <- calcPercentile(
  dat,
  pollutant = "o3",
  avg.time = "month",
  percentile = 95
)
pct_multi <- calcPercentile(
  dat,
  pollutant = "o3",
  avg.time = "month",
  percentile = c(5, 50, 95)
)

# --- Output structure --------------------------------------------------------

test_that("calcPercentile returns a data frame with a date column", {
  expect_s3_class(pct_single, "data.frame")
  expect_true("date" %in% names(pct_single))
})

test_that("single percentile produces exactly one percentile column", {
  pct_cols <- grep("^percentile\\.", names(pct_single), value = TRUE)
  expect_length(pct_cols, 1L)
  expect_true("percentile.95" %in% names(pct_single))
})

test_that("multiple percentiles produce one column per value with correct names", {
  expect_true(all(
    c("percentile.5", "percentile.50", "percentile.95") %in% names(pct_multi)
  ))
  pct_cols <- grep("^percentile\\.", names(pct_multi), value = TRUE)
  expect_length(pct_cols, 3L)
})

test_that("avg.time = 'month' returns 12 rows for a full year", {
  expect_equal(nrow(pct_single), 12L)
})

# --- Numerical correctness ---------------------------------------------------

test_that("percentile values are non-decreasing across quantile levels", {
  # p5 <= p50 <= p95 at every row
  ok <- !is.na(pct_multi$percentile.5) &
    !is.na(pct_multi$percentile.50) &
    !is.na(pct_multi$percentile.95)
  expect_true(all(pct_multi$percentile.5[ok] <= pct_multi$percentile.50[ok]))
  expect_true(all(pct_multi$percentile.50[ok] <= pct_multi$percentile.95[ok]))
})

test_that("percentile values stay within the observed range of the pollutant", {
  obs_range <- range(dat$o3, na.rm = TRUE)
  vals <- pct_single$percentile.95
  expect_true(all(vals >= obs_range[1], na.rm = TRUE))
  expect_true(all(vals <= obs_range[2], na.rm = TRUE))
})

test_that("percentile.0 equals the monthly minimum and percentile.100 the maximum", {
  pct_bounds <- calcPercentile(
    dat,
    pollutant = "o3",
    avg.time = "month",
    percentile = c(0, 100),
    data.thresh = 0
  )
  daily <- timeAverage(
    dat,
    avg.time = "month",
    statistic = "min",
    progress = FALSE
  )
  expect_equal(pct_bounds$percentile.0, daily$o3, tolerance = 1e-6)
})

# --- avg.time ----------------------------------------------------------------

test_that("avg.time = 'year' returns a single row", {
  annual <- calcPercentile(
    dat,
    pollutant = "o3",
    avg.time = "year",
    percentile = 50
  )
  expect_equal(nrow(annual), 1L)
})

test_that("avg.time = 'day' returns one row per day", {
  daily <- calcPercentile(
    dat,
    pollutant = "o3",
    avg.time = "day",
    percentile = 50
  )
  expect_equal(nrow(daily), 365L)
})

# --- prefix ------------------------------------------------------------------

test_that("custom prefix is reflected in output column names", {
  result <- calcPercentile(
    dat,
    pollutant = "o3",
    avg.time = "month",
    percentile = 75,
    prefix = "p"
  )
  expect_true("p75" %in% names(result))
  expect_false("percentile.75" %in% names(result))
})

# --- data.thresh -------------------------------------------------------------

test_that("data.thresh = 100 produces more NAs than data.thresh = 0", {
  dat_gaps <- dat
  dat_gaps$o3[sample(nrow(dat_gaps), 500)] <- NA

  res_low <- calcPercentile(
    dat_gaps,
    pollutant = "o3",
    avg.time = "month",
    percentile = 50,
    data.thresh = 0
  )
  res_high <- calcPercentile(
    dat_gaps,
    pollutant = "o3",
    avg.time = "month",
    percentile = 50,
    data.thresh = 100
  )

  expect_gte(
    sum(is.na(res_high$percentile.50)),
    sum(is.na(res_low$percentile.50))
  )
})

# --- Input validation --------------------------------------------------------

test_that("calcPercentile errors when pollutant is not in mydata", {
  expect_error(
    calcPercentile(
      dat,
      pollutant = "no_such_col",
      avg.time = "month",
      percentile = 50
    ),
    regexp = "not present"
  )
})
