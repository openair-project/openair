# timeAverage tests
# Use a small slice for speed; one full year gives clean monthly/annual rows
dat <- selectByDate(mydata, year = 2003)
dat1 <- selectByDate(mydata, year = 2003, month = 1)

# Shared results — most tests reuse these
ta_day <- timeAverage(dat, avg.time = "day", progress = FALSE)
ta_month <- timeAverage(dat, avg.time = "month", progress = FALSE)
ta_year <- timeAverage(dat, avg.time = "year", progress = FALSE)

# --- Output structure --------------------------------------------------------

test_that("timeAverage returns a data frame (tibble) with a date column", {
  expect_s3_class(ta_day, "data.frame")
  expect_true("date" %in% names(ta_day))
  expect_s3_class(ta_day$date, "POSIXct")
})

test_that("timeAverage preserves numeric pollutant columns", {
  expect_true(all(c("no2", "o3", "nox", "ws", "wd") %in% names(ta_day)))
})

# --- Row counts --------------------------------------------------------------

test_that("avg.time = 'day' returns one row per day", {
  expect_equal(nrow(ta_day), 365L)
})

test_that("avg.time = 'month' returns one row per month", {
  expect_equal(nrow(ta_month), 12L)
})

test_that("avg.time = 'year' returns one row", {
  expect_equal(nrow(ta_year), 1L)
})

test_that("avg.time = '2 month' returns 6 rows for a full year", {
  result <- timeAverage(dat, avg.time = "2 month", progress = FALSE)
  expect_equal(nrow(result), 6L)
})

# --- Aggregation correctness -------------------------------------------------

test_that("daily mean is between hourly min and max", {
  safe_stat <- function(fn, x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      fn(x, na.rm = TRUE)
    }
  }
  daily_min <- tapply(
    dat$no2,
    lubridate::date(dat$date),
    \(x) safe_stat(min, x)
  )
  daily_max <- tapply(
    dat$no2,
    lubridate::date(dat$date),
    \(x) safe_stat(max, x)
  )
  non_na <- !is.na(ta_day$no2)

  expect_true(all(ta_day$no2[non_na] >= daily_min[non_na] - 1e-9))
  expect_true(all(ta_day$no2[non_na] <= daily_max[non_na] + 1e-9))
})

test_that("annual mean matches a direct calculation", {
  expected <- mean(dat$no2, na.rm = TRUE)
  expect_equal(ta_year$no2, expected, tolerance = 1e-6)
})

# --- statistic argument ------------------------------------------------------

test_that("statistic = 'max' returns values >= statistic = 'mean'", {
  ta_max <- timeAverage(
    dat,
    avg.time = "month",
    statistic = "max",
    progress = FALSE
  )
  non_na <- !is.na(ta_month$no2) & !is.na(ta_max$no2)
  expect_true(all(ta_max$no2[non_na] >= ta_month$no2[non_na]))
})

test_that("statistic = 'min' returns values <= statistic = 'mean'", {
  ta_min <- timeAverage(
    dat,
    avg.time = "month",
    statistic = "min",
    progress = FALSE
  )
  non_na <- !is.na(ta_month$no2) & !is.na(ta_min$no2)
  expect_true(all(ta_min$no2[non_na] <= ta_month$no2[non_na]))
})

test_that("statistic = 'sum' exceeds statistic = 'mean' for positive values", {
  ta_sum <- timeAverage(
    dat,
    avg.time = "month",
    statistic = "sum",
    progress = FALSE
  )
  non_na <- !is.na(ta_month$no2) & !is.na(ta_sum$no2)
  expect_true(all(ta_sum$no2[non_na] >= ta_month$no2[non_na]))
})

test_that("statistic = 'frequency' returns integer counts <= hours in period", {
  ta_freq <- timeAverage(
    dat1,
    avg.time = "day",
    statistic = "frequency",
    progress = FALSE
  )
  expect_true(all(ta_freq$no2 <= 24, na.rm = TRUE))
  expect_true(all(ta_freq$no2 >= 0, na.rm = TRUE))
})

test_that("statistic = 'percentile' with percentile = 95 is >= mean", {
  ta_p95 <- timeAverage(
    dat,
    avg.time = "month",
    statistic = "percentile",
    percentile = 95,
    progress = FALSE
  )
  non_na <- !is.na(ta_month$no2) & !is.na(ta_p95$no2)
  expect_true(all(ta_p95$no2[non_na] >= ta_month$no2[non_na]))
})

# --- Wind direction ----------------------------------------------------------

test_that("wind direction stays in [0, 360]", {
  non_na <- !is.na(ta_day$wd)
  expect_true(all(ta_day$wd[non_na] >= 0))
  expect_true(all(ta_day$wd[non_na] <= 360))
})

test_that("vector-averaged wd handles the 350/10 degree case correctly", {
  # Exact 350 + 10 average should be 0 (or 360), not 180
  two_hours <- data.frame(
    date = as.POSIXct(c("2023-01-01 00:00", "2023-01-01 01:00"), tz = "GMT"),
    wd = c(350, 10),
    ws = c(2, 2)
  )
  result <- timeAverage(two_hours, avg.time = "day", progress = FALSE)
  # The result should be close to 0 or 360, not near 180
  avg_wd <- result$wd
  expect_true(avg_wd <= 10 || avg_wd >= 350)
})

# --- data.thresh -------------------------------------------------------------

test_that("data.thresh = 100 produces more NAs than data.thresh = 0", {
  dat_gaps <- dat1
  dat_gaps$no2[sample(nrow(dat_gaps), 300)] <- NA

  res_low <- timeAverage(
    dat_gaps,
    avg.time = "day",
    data.thresh = 0,
    progress = FALSE
  )
  res_high <- timeAverage(
    dat_gaps,
    avg.time = "day",
    data.thresh = 100,
    progress = FALSE
  )

  expect_gte(sum(is.na(res_high$no2)), sum(is.na(res_low$no2)))
})

# --- Expansion (avg.time < data interval) ------------------------------------

test_that("expanding hourly to 15-min produces 4x as many rows", {
  result <- timeAverage(
    dat1,
    avg.time = "15 min",
    fill = TRUE,
    progress = FALSE
  )
  expect_equal(nrow(result), (nrow(dat1) * 4L) - 3)
})

# --- avg.time = 'season' -----------------------------------------------------

test_that("avg.time = 'season' returns 5 rows for a full year", {
  result <- timeAverage(dat, avg.time = "season", progress = FALSE)
  expect_equal(nrow(result), 5L)
})

# --- Input validation --------------------------------------------------------

test_that("invalid statistic raises an error", {
  expect_error(
    timeAverage(
      dat,
      avg.time = "day",
      statistic = "geometric_mean",
      progress = FALSE
    ),
    regexp = "statistic"
  )
})

test_that("data.thresh outside [0, 100] raises an error", {
  expect_error(
    timeAverage(dat, avg.time = "day", data.thresh = 101, progress = FALSE),
    regexp = "capture"
  )
})

test_that("percentile outside [0, 100] raises an error", {
  expect_error(
    timeAverage(
      dat,
      avg.time = "day",
      statistic = "percentile",
      percentile = 110,
      progress = FALSE
    ),
    regexp = "Percentile"
  )
})

test_that("invalid avg.time unit raises an error", {
  expect_error(
    timeAverage(dat, avg.time = "fortnight", progress = FALSE),
    regexp = "not recognised"
  )
})
