test_that("multiplication works", {
  testdat <- dplyr::filter(mydata, lubridate::year(date) == 2005)
  
  single <- calcPercentile(testdat, pollutant = "o3")
  expect_s3_class(single, "data.frame")
  expect_equal(names(single), c("date", "percentile.50"))
  expect_type(single$percentile.50, "double")
  
  typed <- calcPercentile(testdat, pollutant = "o3", type = "season")
  expect_equal(names(typed), c("date", "season", "percentile.50"))
  
  multiple <- calcPercentile(
    testdat,
    pollutant = "o3",
    percentile = c(10, 50, 90),
    prefix = "p"
  )
  expect_equal(nrow(multiple), nrow(single))
  expect_equal(names(multiple), c("date", "p10", "p50", "p90"))
  
  expect_error(calcPercentile(testdat, pollutant = "co2"))
  
})
