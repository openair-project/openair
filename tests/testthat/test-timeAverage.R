test_that("timeaverage averages & pads", {
  # standard timeaverage
  testdat <- selectByDate(mydata, year = 1999)
  monthly <- timeAverage(testdat, avg.time = "month")
  expect_length(monthly$date, 12)
  expect_equal(lubridate::month(monthly$date), 1:12)
  expect_equal(lubridate::day(monthly$date), rep(1, 12))
  expect_equal(lubridate::hour(monthly$date), rep(0, 12))

  # type arg
  testdat <- dplyr::bind_rows(
    "a" = selectByDate(mydata, year = 2000),
    "b" = selectByDate(mydata, year = 2000),
    .id = "site"
  )
  typed <- timeAverage(tidyr::drop_na(testdat, nox), type = c("site", "nox"))
  expect_s3_class(typed$nox, "factor")
  expect_s3_class(typed$site, "factor")
  expect_in(unique(testdat$site), typed$site)

  # intervals
  testdat <- selectByDate(mydata, year = 1999:2000)
  padded <-
    timeAverage(testdat, "year") |>
    timeAverage("month")
  expect_length(padded$date, 24)
  expect_equal(
    round(padded$ws, 3),
    rep(c(4.587, NA, 4.796, NA), rep(c(1L, 11L), 2))
  )

  # ensure filling works as expected
  padded2 <-
    selectByDate(mydata, year = 1999, month = 1, day = 1, hour = 1:10) |>
    dplyr::mutate(co = c(1:5, rep(NA, 2), 8:10)) |>
    timeAverage("30 min", fill = TRUE)
  expect_length(padded2$date, 21)
  expect_equal(
    padded2$co,
    rep(
      c(1L, 2L, 3L, 4L, 5L, NA, 8L, 9L, 10L),
      rep(c(2L, 4L, 2L, 3L), c(5L, 1L, 2L, 1L))
    )
  )
})

test_that("timeaverage start/end dates work", {
  ## start.date
  testdat <- selectByDate(mydata, year = 2000, month = 2)
  testdat2 <- dplyr::slice_tail(testdat, n = -3)

  # irregular time period
  x1 <- timeAverage(testdat2, "5 hour")
  expect_equal(lubridate::hour(x1$date[1]), lubridate::hour(testdat2$date[1]))

  # regular time period
  x2 <- timeAverage(testdat2, "5 hour", start.date = min(testdat$date)[1])
  expect_equal(lubridate::hour(x2$date[1]), lubridate::hour(testdat$date[1]))

  ## end.date
  testdat3 <- selectByDate(mydata, year = 2000, month = 2:10)

  x3 <- timeAverage(testdat3, "month")
  expect_length(x3$date, 9)

  x4 <- timeAverage(
    testdat3,
    "month",
    start.date = "2000/01/01",
    end.date = "2000/12/01"
  )
  expect_length(x4$date, 12)
})

test_that("timeaverage catches errors", {
  expect_error(timeAverage(mydata, statistic = "percentile", percentile = 110))
  expect_error(timeAverage(mydata, statistic = "percentile", percentile = -10))
  expect_error(timeAverage(mydata, data.thresh = -10))
  expect_error(timeAverage(mydata, data.thresh = 120))
  expect_error(timeAverage(mydata, avg.time = "50 foobars"))
})

test_that("different timeaverage stats", {
  testdat <- selectByDate(mydata, year = 2000)
  expect_error(timeAverage(testdat, "month", statistic = "foobar"))
  valid_stats <-
    c(
      "mean",
      "median",
      "frequency",
      "max",
      "min",
      "sum",
      "sd",
      "percentile",
      "data.cap"
    )
  for (i in valid_stats) {
    if (i == "percentile") {
      expect_error(timeAverage(testdat, "month", statistic = i))
      expect_no_error(timeAverage(
        testdat,
        "month",
        statistic = i,
        percentile = 90
      ))
    } else {
      expect_no_error(timeAverage(
        testdat,
        "month",
        statistic = i
      ))
    }
  }
})

test_that("intervals work", {
  testdat <- selectByDate(mydata, year = 2000) |>
    timeAverage("month")

  x1 <- timeAverage(testdat, "month", interval = "hour", data.thresh = 90)
  expect_equal(x1$ws[1:11], rep(NA_real_, 11))
})

test_that("windspeed working", {
  testdat <- selectByDate(mydata, year = 2001, month = 6)

  # has ws/wd
  x1 <- timeAverage(testdat, "year")
  expect_equal(round(x1$ws, 2), 4.22)
  expect_equal(round(x1$wd, 2), 251.97)

  # vector ws
  x2 <- timeAverage(testdat, "year", vector.ws = TRUE)
  expect_equal(round(x2$ws, 2), 1.90)
  expect_equal(round(x2$wd, 2), 238.53)

  # does something different w/ no ws
  x3 <- testdat |> dplyr::select(-"ws") |> timeAverage("year")
  expect_equal(round(x3$wd, 2), 251.97)
})

test_that("seasons working", {
  testdat <- selectByDate(mydata, year = 2000:2002)
  seasonal <- timeAverage(testdat, "season")
  seasonal <- seasonal |> cutData("season", names = c("testseason"))
  expect_equal(seasonal$season, seasonal$testseason)
})

test_that("timeaverage works with Date", {
  testdat <-
    selectByDate(mydata, year = 2003) |>
    timeAverage("day") |>
    dplyr::mutate(date = as.Date(date))

  expect_no_error(timeAverage(testdat, "month"))
})
