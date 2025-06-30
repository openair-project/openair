test_that("aqstats works", {
  testdat <- head(mydata, n = 100)

  stats <- aqStats(testdat, c("no2", "o3", "pm10", "so2"))

  expect_equal(
    names(stats),
    c(
      "default",
      "pollutant",
      "year",
      "date",
      "dat.cap",
      "mean",
      "min",
      "max",
      "median",
      "max_daily",
      "roll_8_max",
      "roll_24_max",
      "percentile.95",
      "percentile.99",
      "hours",
      "roll.8.O3.gt.100",
      "roll.8.O3.gt.120",
      "AOT40",
      "days"
    )
  )

  # check stats only calculated for specific pollutants
  expect_equal(stats$hours, c(0, NA, NA, NA))
  expect_equal(stats$roll.8.O3.gt.120, c(NA, 0, NA, NA))
  expect_equal(stats$days, c(NA, NA, 0, NA))

  # transpose
  stats_t <- aqStats(testdat, c("no2", "o3", "pm10", "so2"), transpose = TRUE)

  expect_equal(
    names(stats_t),
    c("default", "year", "date", "name", "no2", "o3", "pm10", "so2")
  )

  # error if duplicate dates
  expect_error(aqStats(rbind(testdat, testdat)))
})
