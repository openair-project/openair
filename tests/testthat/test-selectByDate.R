# selectByDate tests
# mydata spans 2003-2005 at hourly resolution - subsample for speed where possible

# --- start / end -------------------------------------------------------------

test_that("start filters to on-or-after the given date", {
  result <- selectByDate(mydata, start = "1/1/2004")

  expect_true(all(result$date >= as.POSIXct("2004-01-01", tz = "GMT")))
})

test_that("end filters to on-or-before end of the given date (full day included)", {
  result <- selectByDate(mydata, end = "31/12/2003")

  expect_true(all(result$date <= as.POSIXct("2003-12-31 23:59:59", tz = "GMT")))
})

test_that("start and end together select the correct closed interval", {
  result <- selectByDate(mydata, start = "1/6/2003", end = "30/6/2003")

  expect_true(all(lubridate::year(result$date) == 2003))
  expect_true(all(lubridate::month(result$date) == 6))
})

test_that("ISO yyyy-mm-dd start/end format is accepted", {
  result <- selectByDate(mydata, start = "2004-01-01", end = "2004-12-31")

  expect_true(all(lubridate::year(result$date) == 2004))
})

# --- year --------------------------------------------------------------------

test_that("year selects a single year", {
  result <- selectByDate(mydata, year = 2004)

  expect_true(all(lubridate::year(result$date) == 2004))
  expect_gt(nrow(result), 0L)
})

test_that("year selects multiple years with a vector", {
  result <- selectByDate(mydata, year = c(2003, 2005))

  expect_setequal(unique(lubridate::year(result$date)), c(2003, 2005))
})

test_that("year range with : selects all included years", {
  result <- selectByDate(mydata, year = 2003:2005)

  expect_setequal(unique(lubridate::year(result$date)), 2003:2005)
})

# --- month -------------------------------------------------------------------

test_that("month numeric selects correct months", {
  result <- selectByDate(mydata, month = c(1, 7))

  expect_true(all(lubridate::month(result$date) %in% c(1, 7)))
})

test_that("month by full name is accepted", {
  result <- selectByDate(mydata, month = c("January", "July"))

  expect_true(all(lubridate::month(result$date) %in% c(1, 7)))
})

test_that("month by 3-letter abbreviation is accepted", {
  result <- selectByDate(mydata, month = c("jan", "jul"))

  expect_true(all(lubridate::month(result$date) %in% c(1, 7)))
})

# --- hour --------------------------------------------------------------------

test_that("hour selects only specified hours", {
  result <- selectByDate(mydata, hour = 9:17)

  expect_true(all(lubridate::hour(result$date) %in% 9:17))
})

test_that("hour = 0 selects midnight rows only", {
  result <- selectByDate(mydata, hour = 0)

  expect_true(all(lubridate::hour(result$date) == 0))
})

# --- day (numeric - day of month) --------------------------------------------

test_that("numeric day selects correct days of the month", {
  result <- selectByDate(mydata, day = 1:5)

  expect_true(all(lubridate::mday(result$date) %in% 1:5))
})

# --- day (character - day of week) -------------------------------------------

test_that("day = 'weekday' returns only Mon-Fri", {
  result <- selectByDate(mydata, day = "weekday")

  expect_true(all(lubridate::wday(result$date) %in% 2:6))
})

test_that("day = 'weekend' returns only Sat-Sun", {
  result <- selectByDate(mydata, day = "weekend")

  expect_true(all(lubridate::wday(result$date) %in% c(1, 7)))
})

test_that("weekday and weekend together return all days", {
  full <- nrow(mydata)
  result <- selectByDate(mydata, day = c("weekday", "weekend"))

  expect_equal(nrow(result), full)
})

test_that("day by full name is accepted", {
  result <- selectByDate(mydata, day = c("Monday", "Friday"))

  expect_true(all(lubridate::wday(result$date) %in% c(2, 6)))
})

test_that("day by 3-letter abbreviation is accepted", {
  result <- selectByDate(mydata, day = c("Mon", "Fri"))

  expect_true(all(lubridate::wday(result$date) %in% c(2, 6)))
})

test_that("invalid day name raises an error", {
  expect_error(selectByDate(mydata, day = "Funday"), regexp = "day")
})

# --- combinations ------------------------------------------------------------

test_that("year + month combination filters correctly", {
  result <- selectByDate(mydata, year = 2003, month = 6)

  expect_true(all(lubridate::year(result$date) == 2003))
  expect_true(all(lubridate::month(result$date) == 6))
})

test_that("day + hour combination filters correctly", {
  result <- selectByDate(mydata, day = "weekday", hour = 7:19)

  expect_true(all(lubridate::wday(result$date) %in% 2:6))
  expect_true(all(lubridate::hour(result$date) %in% 7:19))
})

# --- edge cases / validation -------------------------------------------------

test_that("selectByDate returns 0 rows (not an error) for an empty date range", {
  result <- selectByDate(mydata, start = "2020-01-01", end = "2020-12-31")

  expect_equal(nrow(result), 0L)
})

test_that("selectByDate errors when 'date' column is absent", {
  no_date <- data.frame(x = 1:5)

  expect_error(selectByDate(no_date), regexp = "date")
})

test_that("selectByDate errors when 'date' column is not Date/POSIXt", {
  bad <- mydata
  bad$date <- as.character(bad$date)

  expect_error(selectByDate(bad), regexp = "date")
})

test_that("selectByDate with no arguments returns the full data frame", {
  result <- selectByDate(mydata)

  expect_equal(nrow(result), nrow(mydata))
})
