test_that("splitByDate works", {
  # check it works w/ Dates
  date <- as.Date("2025/01/01")
  dummy <- data.frame(date = date + (-5:5), nox = 100 + seq(-50, 50, 10))
  splitDate <- splitByDate(dummy, dates = c("2025/01/01"))
  
  expect_equal(nrow(splitDate), nrow(dummy))
  expect_s3_class(splitDate$split.by, "factor")
  expect_equal(names(splitDate), c("date", "nox", "split.by"))
  
  # check it works w/ Datetimes
  date2 <- as.POSIXct("2025/01/01 00:00:00")
  dummy2 <- data.frame(date = date2 + (-5:5), nox = 100 + seq(-50, 50, 10))
  splitTime <- splitByDate(dummy2, dates = c("2025/01/01 00:00:01"))
  expect_equal(nrow(splitDate), nrow(dummy2))
  expect_equal(splitDate$split.by, ordered(rep(c("before", "after"), c(6L, 5L)), levels = c("before", "after")))
  
  # Date dataframe, datetime chr input
  expect_no_error(splitByDate(dummy, "2025/01/01 01:00:00"))
  
  # Datetime dataframe, date chr input
  expect_no_error(splitByDate(dummy2, "2025/01/01"))
  
  # Date dataframe, Date input
  expect_no_error(splitByDate(dummy, mean(dummy$date)))
  
  # Datetime dataframe, Datetime input
  expect_no_error(splitByDate(dummy2, mean(dummy2$date)))
  
  # Error w/ mismatch
  expect_error(splitByDate(dummy, dates = mean(dummy$date), labels = "onelab"))
})
