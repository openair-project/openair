# trendLevel tests — plot = FALSE throughout to skip rendering
# Single month gives enough data while keeping aggregation fast
dat <- selectByDate(mydata, year = 2003, month = 1)

# Shared results
tl_default <- trendLevel(dat, pollutant = "nox", plot = FALSE)
tl_median <- trendLevel(
  dat,
  pollutant = "nox",
  statistic = "median",
  plot = FALSE
)

# --- Return value structure --------------------------------------------------

test_that("trendLevel returns an openair object with expected components", {
  expect_s3_class(tl_default, "openair")
  expect_named(tl_default, c("plot", "data", "call"))
  expect_s3_class(tl_default$plot, "ggplot")
  expect_s3_class(tl_default$data, "tbl_df")
})

test_that("trendLevel$data has axis columns plus pollutant and n", {
  # defaults: x = "month", y = "hour"
  expect_true(all(c("month", "hour", "nox", "n") %in% names(tl_default$data)))
})

# --- Data values -------------------------------------------------------------

test_that("n is non-negative in every bin", {
  expect_true(all(tl_default$data$n >= 0, na.rm = TRUE))
})

test_that("mean values stay within the observed range of the pollutant", {
  obs_range <- range(dat$nox, na.rm = TRUE)
  non_na <- tl_default$data$nox[!is.na(tl_default$data$nox)]
  expect_true(all(non_na >= obs_range[1]))
  expect_true(all(non_na <= obs_range[2]))
})

test_that("median statistic produces different values from mean", {
  expect_false(isTRUE(all.equal(tl_default$data$nox, tl_median$data$nox)))
})

# --- statistic argument ------------------------------------------------------

test_that("statistic = 'max' produces values >= statistic = 'mean'", {
  tl_max <- trendLevel(dat, pollutant = "nox", statistic = "max", plot = FALSE)
  non_na <- !is.na(tl_default$data$nox) & !is.na(tl_max$data$nox)
  expect_true(all(tl_max$data$nox[non_na] >= tl_default$data$nox[non_na]))
})

test_that("statistic = 'frequency' returns integer counts", {
  tl_freq <- trendLevel(
    dat,
    pollutant = "nox",
    statistic = "frequency",
    plot = FALSE
  )
  non_na <- tl_freq$data$nox[!is.na(tl_freq$data$nox)]
  expect_true(all(non_na == floor(non_na)))
  expect_true(all(non_na >= 0))
})

test_that("user-supplied function is accepted as statistic", {
  my_mean <- function(x) mean(x, na.rm = TRUE)
  tl_fun <- trendLevel(
    dat,
    pollutant = "nox",
    statistic = my_mean,
    plot = FALSE
  )
  # Should match the built-in mean
  expect_equal(tl_fun$data$nox, tl_default$data$nox, tolerance = 1e-6)
})

# --- x / y / type axes -------------------------------------------------------

test_that("custom x and y produce corresponding columns in output data", {
  tl_custom <- trendLevel(
    dat,
    pollutant = "nox",
    x = "hour",
    y = "weekday",
    plot = FALSE
  )
  expect_true(all(c("hour", "weekday") %in% names(tl_custom$data)))
})

test_that("type produces a faceting column in output data", {
  tl_type <- trendLevel(dat, pollutant = "nox", type = "weekend", plot = FALSE)
  expect_true("weekend" %in% names(tl_type$data))
})

# --- min.bin -----------------------------------------------------------------

test_that("min.bin = 5 produces at least as many NAs as min.bin = 1", {
  tl_strict <- trendLevel(dat, pollutant = "nox", min.bin = 5, plot = FALSE)
  tl_loose <- trendLevel(dat, pollutant = "nox", min.bin = 1, plot = FALSE)
  expect_gte(sum(is.na(tl_strict$data$nox)), sum(is.na(tl_loose$data$nox)))
})

# --- breaks / categorical scale ----------------------------------------------

test_that("supplying breaks discretises the nox column", {
  tl_cat <- trendLevel(
    dat,
    pollutant = "nox",
    breaks = c(0, 50, 200, 1000),
    plot = FALSE
  )
  expect_true(is.factor(tl_cat$data$nox))
})

# --- Input validation --------------------------------------------------------

test_that("duplicate terms across pollutant/x/y/type raises an error", {
  expect_error(
    trendLevel(dat, pollutant = "nox", x = "nox", plot = FALSE),
    regexp = "Duplicate"
  )
})

test_that("invalid statistic raises an error", {
  expect_error(
    trendLevel(
      dat,
      pollutant = "nox",
      statistic = "geometric_mean",
      plot = FALSE
    ),
    regexp = "statistic"
  )
})

test_that("multiple x values warns and uses only the first", {
  expect_warning(
    trendLevel(dat, pollutant = "nox", x = c("month", "hour"), plot = FALSE),
    regexp = "multiple"
  )
})

test_that("more than two type values warns and truncates to two", {
  expect_warning(
    trendLevel(
      dat,
      pollutant = "nox",
      type = c("weekend", "season", "year"),
      plot = FALSE
    ),
    regexp = "more than two"
  )
})
