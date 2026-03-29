# smoothTrend tests — plot = FALSE throughout
# GAM fitting is involved; skip on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Three years gives enough monthly data for a stable GAM trend
dat <- selectByDate(mydata, year = 2001:2003)

# Shared result
st <- smoothTrend(dat, pollutant = "nox", plot = FALSE, progress = FALSE)

# =============================================================================
# Return value structure
# =============================================================================

test_that("smoothTrend returns an openair object with expected components", {
  expect_s3_class(st, "openair")
  expect_named(st, c("plot", "data", "call"))
  expect_s3_class(st$plot, "ggplot")
})

test_that("$data is a list with 'data' and 'fit' elements", {
  expect_type(st$data, "list")
  expect_true(all(c("data", "fit") %in% names(st$data)))
})

test_that("$data$data has date, conc and variable columns", {
  expect_true(all(c("date", "conc", "variable") %in% names(st$data$data)))
})

test_that("$data$fit has date, pred, lower and upper columns", {
  expect_true(all(c("date", "pred", "lower", "upper") %in% names(st$data$fit)))
})

# =============================================================================
# Data values
# =============================================================================

test_that("fit$date is POSIXct", {
  expect_s3_class(st$data$fit$date, "POSIXct")
})

test_that("fit: lower <= pred <= upper", {
  fit <- st$data$fit
  ok <- !is.na(fit$pred) & !is.na(fit$lower) & !is.na(fit$upper)
  expect_true(all(fit$lower[ok] <= fit$pred[ok]))
  expect_true(all(fit$pred[ok] <= fit$upper[ok]))
})

test_that("fit has 100 rows by default (n = 100 prediction points)", {
  expect_equal(nrow(st$data$fit), 100L)
})

test_that("data$data rows equal number of non-NA monthly means", {
  monthly <- timeAverage(dat, avg.time = "month", progress = FALSE)
  n_months <- sum(!is.na(monthly$nox))
  expect_equal(nrow(st$data$data), n_months)
})

# =============================================================================
# Multiple pollutants
# =============================================================================

test_that("multiple pollutants produce one variable level per pollutant", {
  st_multi <- smoothTrend(
    dat,
    pollutant = c("nox", "no2"),
    plot = FALSE,
    progress = FALSE
  )
  expect_setequal(levels(st_multi$data$data$variable), c("nox", "no2"))
})

# =============================================================================
# statistic and percentile
# =============================================================================

test_that("statistic = 'percentile' with percentile = 95 runs without error", {
  expect_no_error(
    smoothTrend(
      dat,
      pollutant = "o3",
      statistic = "percentile",
      percentile = 95,
      plot = FALSE,
      progress = FALSE
    )
  )
})

test_that("multiple percentiles produce one variable level per percentile", {
  st_pct <- smoothTrend(
    dat,
    pollutant = "o3",
    percentile = c(5, 50, 95),
    statistic = "percentile",
    plot = FALSE,
    progress = FALSE
  )
  expect_equal(nlevels(st_pct$data$data$variable), 3L)
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'season' produces four variable groups in $data$data", {
  st_s <- smoothTrend(
    dat,
    pollutant = "nox",
    type = "season",
    plot = FALSE,
    progress = FALSE
  )
  expect_true("season" %in% names(st_s$data$data))
  expect_equal(dplyr::n_distinct(st_s$data$data$season), 4L)
})

# =============================================================================
# deseason
# =============================================================================

test_that("deseason = TRUE produces a different trend from deseason = FALSE", {
  st_ds <- smoothTrend(
    dat,
    pollutant = "nox",
    deseason = TRUE,
    plot = FALSE,
    progress = FALSE
  )
  st_raw <- st # deseason = FALSE by default

  # deseasoned concentrations differ from raw monthly means
  expect_false(isTRUE(all.equal(st_ds$data$data$conc, st_raw$data$data$conc)))
})

# =============================================================================
# ci = FALSE
# =============================================================================

test_that("ci = FALSE returns the same fit but the plot still builds", {
  st_noci <- smoothTrend(
    dat,
    pollutant = "nox",
    ci = FALSE,
    plot = FALSE,
    progress = FALSE
  )
  expect_s3_class(st_noci$plot, "ggplot")
  # Fit data should be the same; CI ribbon is suppressed in the plot, not the data
  expect_equal(st_noci$data$fit$pred, st$data$fit$pred, tolerance = 1e-6)
})
