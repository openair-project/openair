# cutData tests
# Pure transformation — fast even on a full year; only subsample where needed
dat <- selectByDate(mydata, year = 2003)
dat1 <- selectByDate(mydata, year = 2003, month = 6) # single month for edge cases

# Shared results — reuse across related tests
cd_year <- cutData(dat, type = "year")
cd_month <- cutData(dat, type = "month")
cd_weekday <- cutData(dat, type = "weekday")
cd_season <- cutData(dat, type = "season")

# =============================================================================
# Output structure
# =============================================================================

test_that("cutData adds exactly one column per type", {
  expect_equal(ncol(cd_year), ncol(dat) + 1L)
  expect_equal(ncol(cd_month), ncol(dat) + 1L)
  expect_equal(ncol(cd_weekday), ncol(dat) + 1L)
})

test_that("cutData preserves row count", {
  expect_equal(nrow(cd_year), nrow(dat))
  expect_equal(nrow(cd_season), nrow(dat))
})

test_that("cutData adds multiple columns for multiple types", {
  result <- cutData(dat, type = c("year", "month"))
  expect_equal(ncol(result), ncol(dat) + 2L)
  expect_true(all(c("year", "month") %in% names(result)))
})

test_that("custom names argument renames the new column", {
  result <- cutData(dat, type = "month", names = "mon")
  expect_true("mon" %in% names(result))
  expect_false("month" %in% names(result))
})

test_that("names length mismatch with type raises an error", {
  expect_error(
    cutData(dat, type = c("year", "month"), names = "just_one"),
    regexp = "Length"
  )
})

# =============================================================================
# Built-in types — factor class and level correctness
# =============================================================================

test_that("type = 'year' produces an ordered factor with correct levels", {
  expect_true(is.ordered(cd_year$year))
  expect_equal(levels(cd_year$year), "2003")
})

test_that("type = 'month' produces 12 ordered levels for a full year", {
  expect_true(is.ordered(cd_month$month))
  expect_equal(nlevels(cd_month$month), 12L)
})

test_that("type = 'month' with drop = 'none' retains all 12 months even for partial data", {
  result <- cutData(dat1, type = "month", drop = "none") # June only
  expect_equal(nlevels(result$month), 12L)
})

test_that("type = 'month' with drop = 'empty' (default) retains only present months", {
  result <- cutData(dat1, type = "month") # June only
  expect_equal(nlevels(result$month), 1L)
  expect_equal(as.character(result$month[1]), "June")
})

test_that("type = 'hour' produces an ordered factor with values 0-23", {
  result <- cutData(dat1, type = "hour")
  expect_true(is.ordered(result$hour))
  expect_true(all(as.integer(as.character(result$hour)) %in% 0:23))
})

test_that("type = 'weekday' produces 7 ordered levels starting Monday", {
  expect_true(is.ordered(cd_weekday$weekday))
  expect_equal(nlevels(cd_weekday$weekday), 7L)
  expect_equal(levels(cd_weekday$weekday)[1], "Monday")
})

test_that("type = 'weekday' start.day = 0 starts on Sunday", {
  result <- cutData(dat, type = "weekday", start.day = 0)
  expect_equal(levels(result$weekday)[1], "Sunday")
})

test_that("type = 'weekend' produces exactly two levels: weekday and weekend", {
  result <- cutData(dat, type = "weekend")
  expect_setequal(levels(result$weekend), c("weekday", "weekend"))
})

test_that("weekday and weekend together account for all rows", {
  result <- cutData(dat, type = "weekend")
  expect_equal(nrow(result), nrow(dat))
  expect_true(all(result$weekend %in% c("weekday", "weekend")))
})

test_that("type = 'season' produces 4 ordered levels for a full year", {
  expect_true(is.ordered(cd_season$season))
  expect_equal(nlevels(cd_season$season), 4L)
})

test_that("type = 'season' with hemisphere = 'southern' produces different levels", {
  south <- cutData(dat, type = "season", hemisphere = "southern")
  expect_false(identical(levels(cd_season$season), levels(south$season)))
})

test_that("type = 'season' start.season = 'winter' places winter first", {
  result <- cutData(dat, type = "season", start.season = "winter")
  expect_true(grepl("winter", levels(result$season)[1], ignore.case = TRUE))
})

test_that("type = 'monthyear' produces one level per unique year-month combination", {
  result <- cutData(dat, type = "monthyear")
  expect_equal(nlevels(result$monthyear), 12L) # one year -> 12 combos
})

test_that("type = 'wd' produces 8 ordered compass-direction levels", {
  result <- cutData(dat[!is.na(dat$wd), ], type = "wd")
  expect_equal(nlevels(result$wd), 8L)
  expect_setequal(
    levels(result$wd),
    c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  )
})

test_that("type = 'default' adds a single factor level describing the date range", {
  result <- cutData(dat, type = "default")
  expect_true("default" %in% names(result))
  expect_equal(nlevels(result$default), 1L)
  expect_true(grepl("2003", levels(result$default)[1]))
})

# =============================================================================
# User-defined column types
# =============================================================================

test_that("a numeric column is split into n.levels quantile bins", {
  result <- cutData(dat[!is.na(dat$nox), ], type = "nox", n.levels = 4)
  expect_true("nox" %in% names(result))
  expect_equal(nlevels(result$nox), 4L)
})

test_that("a character/factor column is used as-is", {
  dat_fac <- dat
  dat_fac$site <- factor(c(
    rep("A", nrow(dat) %/% 2),
    rep("B", nrow(dat) - nrow(dat) %/% 2)
  ))
  result <- cutData(dat_fac, type = "site")
  expect_setequal(levels(result$site), c("A", "B"))
})

test_that("unrecognised type not in data raises an error", {
  expect_error(cutData(dat, type = "not_a_column"), regexp = "neither")
})

# =============================================================================
# suffix argument
# =============================================================================

test_that("suffix prevents overwriting an existing column of the same name", {
  result <- cutData(dat[!is.na(dat$nox), ], type = "nox", suffix = "_cut")
  expect_true("nox_cut" %in% names(result))
  # original nox column should still be numeric
  expect_true(is.numeric(result$nox))
})
