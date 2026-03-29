# Helper: one week of hourly data with some rows removed to create gaps
make_gappy <- function(remove_idx = c(2, 5, 10)) {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1:7)
  dat[-remove_idx, ]
}

# --- Basic output ------------------------------------------------------------

test_that("datePad returns same data when no gaps exist", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1)

  result <- datePad(dat)

  expect_equal(nrow(result), nrow(dat))
})

test_that("datePad fills gaps to produce a contiguous hourly sequence", {
  gappy <- make_gappy()
  full <- selectByDate(mydata, year = 2003, month = 1, day = 1:7)

  result <- datePad(gappy)

  expect_equal(nrow(result), nrow(full))
})

test_that("datePad result covers exactly min to max date of input", {
  gappy <- make_gappy()
  result <- datePad(gappy)

  expect_equal(min(result$date), min(gappy$date))
  expect_equal(max(result$date), max(gappy$date))
})

test_that("datePad produces no duplicate dates", {
  gappy <- make_gappy()
  result <- datePad(gappy)

  expect_equal(anyDuplicated(result$date), 0L)
})

test_that("datePad introduced rows have NA for pollutant values", {
  gappy <- make_gappy()
  result <- datePad(gappy)

  added_dates <- setdiff(result$date, gappy$date)
  added_rows <- result[result$date %in% added_dates, ]

  expect_true(all(is.na(added_rows$no2)))
})

test_that("datePad preserves original non-gap rows unchanged", {
  gappy <- make_gappy()
  result <- datePad(gappy)

  kept <- result[result$date %in% gappy$date, ]
  expect_equal(kept$no2, gappy$no2)
})

# --- start.date / end.date ---------------------------------------------------

test_that("datePad respects explicit start.date earlier than data minimum", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 3:7)
  early_start <- as.POSIXct("2003-01-01 00:00:00", tz = "GMT")

  result <- datePad(dat, start.date = early_start)

  expect_equal(min(result$date), early_start)
})

test_that("datePad respects explicit end.date later than data maximum", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1:5)
  late_end <- as.POSIXct("2003-01-10 23:00:00", tz = "GMT")

  result <- datePad(dat, end.date = late_end)

  expect_equal(max(result$date), late_end)
})

# --- interval ----------------------------------------------------------------

test_that("datePad with interval = '30 min' doubles the row count on gap-free data", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1)

  result <- datePad(dat, interval = "30 min")

  # 24-hour day at 30-min intervals = 48 rows vs 24 at hourly
  expect_equal(nrow(result), 2L * nrow(dat) - 1L)
})

test_that("datePad timestamps are evenly spaced at requested interval", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1)
  result <- datePad(dat, interval = "30 min")

  diffs <- as.numeric(diff(result$date), units = "mins")
  expect_true(all(diffs == 30))
})

# --- fill = TRUE -------------------------------------------------------------

test_that("datePad fill = TRUE carries forward block values into sub-intervals", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1)
  result <- datePad(dat, interval = "30 min", fill = TRUE)

  # Each pair of rows (on-the-hour and half-past) should share the same no2 value
  on_hour <- result[as.numeric(format(result$date, "%M")) == 0, ]
  half_past <- result[as.numeric(format(result$date, "%M")) == 30, ]

  # Align by removing the last half-past (no following block)
  n <- min(nrow(on_hour), nrow(half_past))
  expect_equal(on_hour$no2[seq_len(n)], half_past$no2[seq_len(n)])
})

test_that("datePad fill = TRUE and fill = FALSE produce different results when interval is finer", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1)
  filled <- datePad(dat, interval = "30 min", fill = TRUE)
  unfilled <- datePad(dat, interval = "30 min", fill = FALSE)

  # unfilled half-past rows should be NA; filled ones should not (mostly)
  half_past_filled <- filled[as.numeric(format(filled$date, "%M")) == 30, ]
  half_past_unfilled <- unfilled[
    as.numeric(format(unfilled$date, "%M")) == 30,
  ]

  expect_gt(
    sum(!is.na(half_past_filled$no2)),
    sum(!is.na(half_past_unfilled$no2))
  )
})

# --- Edge cases --------------------------------------------------------------

test_that("datePad returns input unchanged for single-row data frame", {
  single <- mydata[1, ]

  result <- datePad(single)

  expect_equal(nrow(result), 1L)
  expect_equal(result$date, single$date)
})

test_that("datePad errors when 'date' column is absent", {
  no_date <- data.frame(no2 = 1:5)

  expect_error(datePad(no_date), regexp = "date")
})

test_that("datePad preserves POSIXct timezone", {
  dat <- selectByDate(mydata, year = 2003, month = 1, day = 1:3)
  result <- datePad(make_gappy())

  expect_equal(attr(result$date, "tzone"), attr(mydata$date, "tzone"))
})
