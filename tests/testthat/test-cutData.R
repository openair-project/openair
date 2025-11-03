test_that("cutData works", {
  testdat <- dplyr::filter(mydata, lubridate::year(date) == 2005)
  testdat <- dplyr::select(testdat, date, ws, wd, nox, no2)

  default <- cutData(testdat)
  expect_equal(names(default), c("date", "ws", "wd", "nox", "no2", "default"))
  expect_equal(nrow(default), nrow(testdat))

  default2 <- cutData(testdat, names = c("dateranges"))
  expect_equal(
    names(default2),
    c("date", "ws", "wd", "nox", "no2", "dateranges")
  )

  multiple <- cutData(testdat, type = c("default", "month"))
  expect_equal(
    names(multiple),
    c("date", "ws", "wd", "nox", "no2", "default", "month")
  )

  expect_error(cutData(
    testdat,
    type = c("default", "month"),
    names = c("onename")
  ))

  conds <- c(
    "default",
    "year",
    "hour",
    "month",
    "season",
    "week",
    "weekday",
    "wd",
    "weekend",
    "monthyear",
    "yearmonth",
    "bstgmt",
    "gmtbst",
    "dst",
    "daylight",
    "seasonyear",
    "yearseason"
  )

  # check all options work
  allopts <- cutData(
    testdat[!is.na(testdat$wd), ],
    type = conds,
    local.tz = "Europe/London"
  )
  expect_equal(
    names(allopts),
    c(
      "date",
      "ws",
      "wd",
      "nox",
      "no2",
      "default",
      "year",
      "hour",
      "month",
      "season",
      "week",
      "weekday",
      "weekend",
      "monthyear",
      "yearmonth",
      "bstgmt",
      "gmtbst",
      "dst",
      "daylight",
      "seasonyear",
      "yearseason"
    )
  )
  expect_s3_class(allopts$wd, "ordered")
  expect_s3_class(allopts$year, "ordered")
  expect_s3_class(allopts$hour, "ordered")
  expect_s3_class(allopts$month, "ordered")
  expect_s3_class(allopts$season, "ordered")
  expect_s3_class(allopts$week, "ordered")
  expect_s3_class(allopts$weekday, "ordered")
  expect_s3_class(allopts$weekend, "ordered")
  expect_s3_class(allopts$monthyear, "ordered")
  expect_s3_class(allopts$yearmonth, "ordered")
  expect_s3_class(allopts$bstgmt, "factor")
  expect_s3_class(allopts$gmtbst, "factor")
  expect_s3_class(allopts$dst, "factor")
  expect_s3_class(allopts$daylight, "factor")
  expect_s3_class(allopts$seasonyear, "ordered")
  expect_s3_class(allopts$yearseason, "ordered")

  # don't overwrite if suffix
  suffix <- cutData(
    testdat[
      !is.na(testdat$no2) &
        !is.na(testdat$nox),
    ],
    c("no2", "nox"),
    suffix = "_cuts"
  )
  expect_type(suffix$no2, "integer")
  expect_type(suffix$nox, "integer")
  expect_s3_class(suffix$no2_cuts, "factor")
  expect_s3_class(suffix$nox_cuts, "factor")

  # error with a random string
  expect_error(cutData(testdat, "some_silly_type"))

  # hemisphere working?
  southern <- cutData(testdat, "season", hemisphere = "southern")
  expect_equal(
    levels(southern$season),
    c("summer (DJF)", "autumn (MAM)", "winter (JJA)")
  )

  # all "drop" options working?
  for (drop in c("none", "empty", "outside")) {
    dropopts <- cutData(
      testdat[!is.na(testdat$wd), ],
      type = conds,
      drop = drop
    )
    expect_s3_class(dropopts$wd, "ordered")
    expect_s3_class(dropopts$year, "ordered")
    expect_s3_class(dropopts$hour, "ordered")
    expect_s3_class(dropopts$month, "ordered")
    expect_s3_class(dropopts$season, "ordered")
    expect_s3_class(dropopts$week, "ordered")
    expect_s3_class(dropopts$weekday, "ordered")
    expect_s3_class(dropopts$weekend, "ordered")
    expect_s3_class(dropopts$monthyear, "ordered")
    expect_s3_class(dropopts$yearmonth, "ordered")
    expect_s3_class(dropopts$bstgmt, "factor")
    expect_s3_class(dropopts$gmtbst, "factor")
    expect_s3_class(dropopts$dst, "factor")
    expect_s3_class(dropopts$daylight, "factor")
    expect_s3_class(dropopts$seasonyear, "ordered")
    expect_s3_class(dropopts$yearseason, "ordered")
  }
})
