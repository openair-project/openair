# binData / bootMeanDF / bootMean tests
# bootMeanDF and bootMean are pure functions — no mydata needed
# binData uses a tiny slice; keep B low for speed
dat <- selectByDate(mydata, year = 2003, month = 1)

set.seed(42)
bd <- binData(dat, bin = "ws", uncer = "nox", n = 10, B = 50)

# =============================================================================
# bootMeanDF
# =============================================================================

test_that("bootMeanDF returns a data frame with mean, min, max, n columns", {
  result <- bootMeanDF(rnorm(50), B = 100)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("mean", "min", "max", "n"))
})

test_that("bootMeanDF: min <= mean <= max", {
  result <- bootMeanDF(rnorm(100, mean = 5), B = 200)
  expect_lte(result$min, result$mean)
  expect_lte(result$mean, result$max)
})

test_that("bootMeanDF: mean is close to the true mean for large n", {
  x <- rnorm(2000, mean = 10, sd = 1)
  result <- bootMeanDF(x, B = 100)
  expect_equal(result$mean, mean(x), tolerance = 1e-9)
})

test_that("bootMeanDF: n equals the number of non-NA values", {
  x <- c(rnorm(40), rep(NA, 10))
  result <- bootMeanDF(x, B = 100)
  expect_equal(result$n, 40L)
})

test_that("bootMeanDF: wider conf.int produces wider interval", {
  set.seed(1)
  x <- rnorm(100)
  narrow <- bootMeanDF(x, conf.int = 0.50, B = 500)
  wide <- bootMeanDF(x, conf.int = 0.99, B = 500)
  expect_gt(wide$max - wide$min, narrow$max - narrow$min)
})

test_that("bootMeanDF errors when x is not a vector", {
  expect_error(bootMeanDF(data.frame(x = 1:5)), regexp = "vector")
})

test_that("bootMeanDF handles a single-element vector without error", {
  expect_no_error(bootMeanDF(42, B = 10))
})

test_that("bootMeanDF handles all-NA vector returning NA intervals", {
  result <- bootMeanDF(rep(NA_real_, 10), B = 50)
  expect_true(is.na(result$min))
  expect_true(is.na(result$max))
})

# =============================================================================
# binData
# =============================================================================

test_that("binData returns a data frame with required columns", {
  expect_s3_class(bd, "data.frame")
  expect_true(all(c("interval", "mean", "min", "max") %in% names(bd)))
})

test_that("binData: number of rows equals number of non-empty intervals", {
  expect_lte(nrow(bd), 10L) # at most n intervals
  expect_gte(nrow(bd), 1L)
})

test_that("binData: min <= mean <= max in every row", {
  ok <- !is.na(bd$mean) & !is.na(bd$min) & !is.na(bd$max)
  expect_true(all(bd$min[ok] <= bd$mean[ok]))
  expect_true(all(bd$mean[ok] <= bd$max[ok]))
})

test_that("binData: mean of uncer column is within the observed range", {
  obs_range <- range(dat$nox, na.rm = TRUE)
  non_na <- bd$mean[!is.na(bd$mean)]
  expect_true(all(non_na >= obs_range[1]))
  expect_true(all(non_na <= obs_range[2]))
})

test_that("binData: interval argument overrides n", {
  result <- binData(dat, bin = "ws", uncer = "nox", interval = 2, B = 50)
  expect_true("interval" %in% names(result))
  expect_gte(nrow(result), 1L)
})

test_that("binData: breaks argument produces the correct number of intervals", {
  result <- binData(
    dat,
    bin = "ws",
    uncer = "nox",
    breaks = c(0, 2, 4, 6, 8, 15),
    B = 50
  )
  expect_lte(nrow(result), 5L)
})

test_that("binData: type = 'weekday' adds a weekday column and increases row groups", {
  result <- binData(
    dat,
    bin = "ws",
    uncer = "nox",
    type = "weekday",
    n = 5,
    B = 50
  )
  expect_true("weekday" %in% names(result))
  # two groups (weekday / weekend) so more rows than without type
  expect_gt(nrow(result), nrow(bd))
})

test_that("binData: more intervals (larger n) produces more rows", {
  coarse <- binData(dat, bin = "ws", uncer = "nox", n = 5, B = 50)
  fine <- binData(dat, bin = "ws", uncer = "nox", n = 20, B = 50)
  expect_gte(nrow(fine), nrow(coarse))
})
