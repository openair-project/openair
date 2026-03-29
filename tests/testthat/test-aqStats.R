# One year is enough for all tests; two years only where multi-year is the point
dat <- selectByDate(mydata, year = 2003)
dat2 <- selectByDate(mydata, year = 2002:2003)

# Run each configuration once and reuse across tests
result_no2 <- aqStats(dat, pollutant = "no2", progress = FALSE)
result_pm10 <- aqStats(dat, pollutant = "pm10", progress = FALSE)
result_o3 <- aqStats(dat, pollutant = "o3", progress = FALSE)
result_multi_long <- aqStats(dat2, pollutant = c("no2", "o3"), progress = FALSE)
result_multi_wide <- aqStats(
  dat2,
  pollutant = c("no2", "o3"),
  transpose = TRUE,
  progress = FALSE
)

# --- Schema ------------------------------------------------------------------

test_that("no2 result has expected columns and lacks pm10/o3-specific ones", {
  expect_s3_class(result_no2, "data.frame")
  expect_true(all(
    c(
      "year",
      "date",
      "dat.cap",
      "mean",
      "min",
      "max",
      "median",
      "percentile.95",
      "percentile.99",
      "max_daily",
      "roll_8_max",
      "roll_24_max",
      "hours"
    ) %in%
      names(result_no2)
  ))
  expect_false("days" %in% names(result_no2))
  expect_false("AOT40" %in% names(result_no2))
})

test_that("pm10 result has 'days' and lacks 'hours'", {
  expect_true("days" %in% names(result_pm10))
  expect_false("hours" %in% names(result_pm10))
})

test_that("o3 result has exceedance and AOT40 columns and lacks 'hours'/'days'", {
  expect_true(all(
    c("roll.8.O3.gt.100", "roll.8.O3.gt.120", "AOT40") %in%
      names(result_o3)
  ))
  expect_false("hours" %in% names(result_o3))
  expect_false("days" %in% names(result_o3))
})

# --- Row counts and years ----------------------------------------------------

test_that("single year returns one row; two years returns two rows with correct year values", {
  expect_equal(nrow(result_no2), 1L)
  two_year <- aqStats(dat2, pollutant = "no2", progress = FALSE)
  expect_equal(nrow(two_year), 2L)
  expect_equal(sort(two_year$year), c(2002L, 2003L))
})

test_that("multiple pollutants return one row per pollutant per year", {
  expect_equal(nrow(result_multi_long), 4L) # 2 years x 2 pollutants
  expect_true("pollutant" %in% names(result_multi_long))
  expect_setequal(unique(result_multi_long$pollutant), c("no2", "o3"))
})

test_that("transpose = FALSE produces fewer rows and more columns than transpose = TRUE", {
  expect_gt(nrow(result_multi_wide), nrow(result_multi_long))
  expect_lt(ncol(result_multi_wide), ncol(result_multi_long))
})

# --- Statistical invariants --------------------------------------------------

test_that("dat.cap is in [0, 100]; mean is between min and max; p99 >= p95", {
  expect_true(all(result_no2$dat.cap >= 0 & result_no2$dat.cap <= 100))
  expect_true(all(result_no2$mean >= result_no2$min, na.rm = TRUE))
  expect_true(all(result_no2$mean <= result_no2$max, na.rm = TRUE))
  expect_true(all(
    result_no2$percentile.99 >= result_no2$percentile.95,
    na.rm = TRUE
  ))
})

test_that("max_daily <= max and roll maxima <= max", {
  expect_true(all(result_no2$max_daily <= result_no2$max, na.rm = TRUE))
  expect_true(all(result_no2$roll_8_max <= result_no2$max, na.rm = TRUE))
  expect_true(all(result_no2$roll_24_max <= result_no2$max, na.rm = TRUE))
})

test_that("o3 exceedance counts are non-negative and gt.120 <= gt.100", {
  expect_true(all(result_o3$roll.8.O3.gt.100 >= 0, na.rm = TRUE))
  expect_true(all(result_o3$roll.8.O3.gt.120 >= 0, na.rm = TRUE))
  expect_true(all(
    result_o3$roll.8.O3.gt.120 <= result_o3$roll.8.O3.gt.100,
    na.rm = TRUE
  ))
})

# --- Custom percentiles ------------------------------------------------------

test_that("custom percentiles produce correct column names", {
  result <- aqStats(
    dat,
    pollutant = "no2",
    percentile = c(50, 75, 90),
    progress = FALSE
  )
  expect_true(all(
    c("percentile.50", "percentile.75", "percentile.90") %in% names(result)
  ))
  expect_false("percentile.95" %in% names(result))
})

# --- data.thresh and missing data --------------------------------------------

test_that("data.thresh masks mean below threshold; dat.cap is never masked", {
  dat_gap <- dat
  set.seed(42)
  dat_gap$no2[sample(nrow(dat_gap), floor(nrow(dat_gap) * 0.80))] <- NA

  res_thresh <- aqStats(
    dat_gap,
    pollutant = "no2",
    data.thresh = 75,
    progress = FALSE
  )
  res_none <- aqStats(
    dat_gap,
    pollutant = "no2",
    data.thresh = 0,
    progress = FALSE
  )

  expect_true(is.na(res_thresh$mean))
  expect_false(is.na(res_none$mean))

  # dat.cap itself is never masked
  dat_allna <- dat
  dat_allna$no2 <- NA_real_
  res_allna <- aqStats(
    dat_allna,
    pollutant = "no2",
    data.thresh = 75,
    progress = FALSE
  )
  expect_false(is.na(res_allna$dat.cap))
  expect_equal(res_allna$dat.cap, 0)
})
