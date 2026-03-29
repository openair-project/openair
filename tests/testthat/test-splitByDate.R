# splitByDate tests
# Use a small slice of mydata plus a tiny synthetic frame for exact checks
dat <- selectByDate(mydata, year = 2003)

# Shared results
sbd_single <- splitByDate(
  dat,
  dates = "1/7/2003",
  labels = c("before", "after")
)
sbd_multi <- splitByDate(
  dat,
  dates = c("1/4/2003", "1/10/2003"),
  labels = c("early", "mid", "late")
)

# --- Output structure --------------------------------------------------------

test_that("splitByDate returns a data frame with one more column than input", {
  expect_equal(ncol(sbd_single), ncol(dat) + 1L)
  expect_true("split.by" %in% names(sbd_single))
})

test_that("row count is unchanged", {
  expect_equal(nrow(sbd_single), nrow(dat))
})

test_that("custom name argument is used for the new column", {
  result <- splitByDate(
    dat,
    dates = "1/7/2003",
    labels = c("before", "after"),
    name = "period"
  )
  expect_true("period" %in% names(result))
  expect_false("split.by" %in% names(result))
})

test_that("new column is an ordered factor with the supplied labels", {
  expect_true(is.factor(sbd_single$split.by))
  expect_true(is.ordered(sbd_single$split.by))
  expect_equal(levels(sbd_single$split.by), c("before", "after"))
})

# --- Correctness with known split --------------------------------------------

test_that("single split: all rows are labelled 'before' or 'after'", {
  expect_true(all(sbd_single$split.by %in% c("before", "after")))
})

test_that("single split: rows before split date are 'before', on/after are 'after'", {
  split_dt <- as.POSIXct("2003-07-01", tz = "GMT")
  expect_true(all(sbd_single$split.by[dat$date < split_dt] == "before"))
  expect_true(all(sbd_single$split.by[dat$date > split_dt] == "after"))
})

test_that("multiple splits produce correct label for each partition", {
  early_dt <- as.POSIXct("2003-04-01", tz = "GMT")
  late_dt <- as.POSIXct("2003-10-01", tz = "GMT")

  expect_true(all(sbd_multi$split.by[dat$date < early_dt] == "early"))
  expect_true(all(sbd_multi$split.by[dat$date > late_dt] == "late"))
  mid_rows <- dat$date > early_dt & dat$date < late_dt
  expect_true(all(sbd_multi$split.by[mid_rows] == "mid"))
})

test_that("all three labels are present in a three-partition split", {
  expect_setequal(levels(sbd_multi$split.by), c("early", "mid", "late"))
  expect_true(all(c("early", "mid", "late") %in% sbd_multi$split.by))
})

# --- Date input formats ------------------------------------------------------

test_that("YYYY/MM/DD string format is accepted", {
  result <- splitByDate(
    dat,
    dates = "2003/07/01",
    labels = c("before", "after")
  )
  expect_setequal(as.character(unique(result$split.by)), c("before", "after"))
})

test_that("R Date object is accepted as dates argument", {
  result <- splitByDate(
    dat,
    dates = as.POSIXct("2003-07-01", tz = "GMT"),
    labels = c("before", "after")
  )
  expect_true(all(result$split.by %in% c("before", "after")))
})

# --- Synthetic data (Date class, not POSIXct) --------------------------------

test_that("works correctly with a Date-class date column", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
    x = rnorm(366)
  )
  result <- splitByDate(df, dates = "2020/07/01", labels = c("h1", "h2"))
  expect_true(is.ordered(result$split.by))
  expect_true(all(result$split.by[df$date < as.Date("2020-07-01")] == "h1"))
  expect_true(all(result$split.by[df$date > as.Date("2020-07-01")] == "h2"))
})

# --- Input validation --------------------------------------------------------

test_that("mismatched dates and labels raises an informative error", {
  # 1 date needs exactly 2 labels
  expect_error(
    splitByDate(dat, dates = "1/7/2003", labels = c("only_one")),
    regexp = "mis-match"
  )
  expect_error(
    splitByDate(dat, dates = "1/7/2003", labels = c("a", "b", "c")),
    regexp = "mis-match"
  )
})
