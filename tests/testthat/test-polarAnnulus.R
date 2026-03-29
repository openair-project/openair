# polarAnnulus tests — plot = FALSE throughout
# GAM fitting is slow; skip on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}
# The GAM fitting inside polarAnnulus is the slow step; limit data heavily.
# Three months gives enough temporal range for all four periods without
# being painfully slow.
dat <- selectByDate(mydata, year = 2003, month = 1:3)
dat2 <- selectByDate(mydata, year = 2003)

# Shared result — default period = "hour"
pa <- polarAnnulus(dat, pollutant = "nox", plot = FALSE)

# --- Return value structure --------------------------------------------------

test_that("polarAnnulus returns an openair object with expected components", {
  expect_s3_class(pa, "openair")
  expect_named(pa, c("plot", "data", "call"))
  expect_s3_class(pa$plot, "ggplot")
  expect_s3_class(pa$data, "tbl_df")
})

test_that("$data has wd, pred and the period column", {
  expect_true(all(c("wd", "pred", "hour") %in% names(pa$data)))
})

# --- Data values -------------------------------------------------------------

test_that("wd is in [0, 360]", {
  non_na <- pa$data$wd[!is.na(pa$data$wd)]
  expect_true(all(non_na >= 0 & non_na <= 360))
})

test_that("period = 'hour': hour values are in [0, 23]", {
  non_na <- pa$data$hour[!is.na(pa$data$hour)]
  expect_true(all(non_na >= 0 & non_na <= 23))
})

test_that("pred values are finite or NA (no NaN or Inf)", {
  non_na <- pa$data$pred[!is.na(pa$data$pred)]
  expect_true(all(is.finite(non_na)))
})

# --- period variants ---------------------------------------------------------

test_that("period = 'season' adds a 'season' column and pred is in [0, 53]", {
  pa_s <- polarAnnulus(dat2, pollutant = "nox", period = "season", plot = FALSE)
  expect_true("season" %in% names(pa_s$data))
  non_na <- pa_s$data$season[!is.na(pa_s$data$season)]
  expect_true(all(non_na >= 0 & non_na <= 53))
})

test_that("period = 'weekday' adds a 'weekday' column", {
  pa_w <- polarAnnulus(dat, pollutant = "nox", period = "weekday", plot = FALSE)
  expect_true("weekday" %in% names(pa_w$data))
})

test_that("period = 'trend' adds a Date 'trend' column", {
  pa_t <- polarAnnulus(dat, pollutant = "nox", period = "trend", plot = FALSE)
  expect_true("trend" %in% names(pa_t$data))
  expect_true(inherits(pa_t$data$trend, "Date"))
})

# --- statistic variants ------------------------------------------------------

test_that("statistic = 'median' returns a valid result", {
  pa_med <- polarAnnulus(
    dat,
    pollutant = "nox",
    statistic = "median",
    plot = FALSE
  )
  expect_s3_class(pa_med, "openair")
  expect_true(all(is.finite(pa_med$data$pred) | is.na(pa_med$data$pred)))
})

test_that("statistic = 'frequency' produces non-negative pred values", {
  pa_freq <- polarAnnulus(
    dat,
    pollutant = "nox",
    statistic = "frequency",
    plot = FALSE
  )
  non_na <- pa_freq$data$pred[!is.na(pa_freq$data$pred)]
  expect_true(all(non_na >= 0))
})

# --- normalise ---------------------------------------------------------------

test_that("normalise = TRUE centres pred values around 1", {
  pa_norm <- polarAnnulus(
    dat,
    pollutant = "nox",
    normalise = TRUE,
    plot = FALSE
  )
  expect_equal(mean(pa_norm$data$pred, na.rm = TRUE), 1, tolerance = 0.05)
})

# --- multiple pollutants -----------------------------------------------------

test_that("multiple pollutants add a 'variable' column to the data", {
  pa_multi <- polarAnnulus(dat, pollutant = c("nox", "no2"), plot = FALSE)
  expect_true("variable" %in% names(pa_multi$data))
})

# --- type conditioning -------------------------------------------------------

test_that("type = 'weekend' produces data with a 'weekend' column", {
  pa_type <- polarAnnulus(
    dat,
    pollutant = "nox",
    type = "weekend",
    plot = FALSE
  )
  expect_true("weekend" %in% names(pa_type$data))
})

# --- Input validation --------------------------------------------------------

test_that("type = 'season' and period = 'trend' raises an error", {
  expect_error(
    polarAnnulus(
      dat,
      pollutant = "nox",
      type = "season",
      period = "trend",
      plot = FALSE
    ),
    regexp = "trend"
  )
})

test_that("more than two types raises an error", {
  expect_error(
    polarAnnulus(
      dat,
      pollutant = "nox",
      type = c("season", "weekend", "year"),
      plot = FALSE
    ),
    regexp = "two"
  )
})

test_that("invalid statistic raises an error", {
  expect_error(
    polarAnnulus(
      dat,
      pollutant = "nox",
      statistic = "geometric_mean",
      plot = FALSE
    ),
    regexp = "statistic"
  )
})

test_that("invalid period raises an error", {
  expect_error(
    polarAnnulus(dat, pollutant = "nox", period = "decade", plot = FALSE),
    regexp = "period"
  )
})
