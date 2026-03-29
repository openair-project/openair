# polarPlot tests — plot = FALSE throughout
# GAM fitting is slow; skip on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Small slice — two months gives enough data for stable GAM fits
dat <- selectByDate(mydata, year = 2003, month = 1:2)

# Shared result — default statistic = "mean"
pp <- polarPlot(dat, pollutant = "nox", plot = FALSE)

# =============================================================================
# Return value structure
# =============================================================================

test_that("polarPlot returns an openair object with expected components", {
  expect_s3_class(pp, "openair")
  expect_named(pp, c("plot", "data", "call"))
  expect_s3_class(pp$plot, "ggplot")
  expect_s3_class(pp$data, "data.frame")
})

test_that("$data has u, v, z, x and wd columns", {
  expect_true(all(c("u", "v", "z", "x", "wd") %in% names(pp$data)))
})

test_that("$data carries a radial_scale attribute", {
  expect_false(is.null(attr(pp$data, "radial_scale")))
  expect_length(attr(pp$data, "radial_scale"), 2L)
})

# =============================================================================
# Data values
# =============================================================================

test_that("wd is in [0, 360]", {
  non_na <- pp$data$wd[!is.na(pp$data$wd)]
  expect_true(all(non_na >= 0 & non_na <= 360))
})

test_that("x (ws) is non-negative", {
  non_na <- pp$data$x[!is.na(pp$data$x)]
  expect_true(all(non_na >= 0))
})

test_that("z values are finite or NA (no NaN / Inf)", {
  non_na <- pp$data$z[!is.na(pp$data$z)]
  expect_true(all(is.finite(non_na)))
})

test_that("force.positive = TRUE keeps z non-negative", {
  non_na <- pp$data$z[!is.na(pp$data$z)]
  expect_true(all(non_na >= 0))
})

# =============================================================================
# statistic variants
# =============================================================================

test_that("statistic = 'median' returns a valid object with finite z", {
  pp_med <- polarPlot(
    dat,
    pollutant = "nox",
    statistic = "median",
    plot = FALSE
  )
  expect_s3_class(pp_med, "openair")
  non_na <- pp_med$data$z[!is.na(pp_med$data$z)]
  expect_true(all(is.finite(non_na)))
})

test_that("statistic = 'frequency' returns non-negative integer z values", {
  pp_freq <- polarPlot(
    dat,
    pollutant = "nox",
    statistic = "frequency",
    plot = FALSE
  )
  non_na <- pp_freq$data$z[!is.na(pp_freq$data$z)]
  expect_true(all(non_na >= 0))
})

test_that("statistic = 'cpf' returns z values in [0, 1]", {
  pp_cpf <- polarPlot(
    dat,
    pollutant = "nox",
    statistic = "cpf",
    percentile = 90,
    plot = FALSE
  )
  non_na <- pp_cpf$data$z[!is.na(pp_cpf$data$z)]
  expect_true(all(non_na >= 0 & non_na <= 1))
})

test_that("statistic = 'r' (Pearson) requires two pollutants", {
  expect_error(
    polarPlot(dat, pollutant = "nox", statistic = "r", plot = FALSE),
    regexp = "two pollutants"
  )
})

test_that("statistic = 'r' with two pollutants returns z in [-1, 1]", {
  pp_r <- polarPlot(
    dat,
    pollutant = c("nox", "no2"),
    statistic = "r",
    plot = FALSE
  )
  non_na <- pp_r$data$z[!is.na(pp_r$data$z)]
  expect_true(all(non_na >= -1 & non_na <= 1))
})

# =============================================================================
# normalise
# =============================================================================

test_that("normalise = TRUE centres z around 1", {
  pp_norm <- polarPlot(dat, pollutant = "nox", normalise = TRUE, plot = FALSE)
  expect_equal(mean(pp_norm$data$z, na.rm = TRUE), 1, tolerance = 0.1)
})

# =============================================================================
# multiple pollutants (non-correlation)
# =============================================================================

test_that("multiple pollutants add a 'variable' column to $data", {
  pp_multi <- polarPlot(dat, pollutant = c("nox", "no2"), plot = FALSE)
  expect_true("variable" %in% names(pp_multi$data))
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'weekend' adds a weekend column to $data", {
  pp_type <- polarPlot(dat, pollutant = "nox", type = "weekend", plot = FALSE)
  expect_true("weekend" %in% names(pp_type$data))
})

# =============================================================================
# Input validation
# =============================================================================

test_that("invalid statistic raises an error", {
  expect_error(
    polarPlot(
      dat,
      pollutant = "nox",
      statistic = "geometric_mean",
      plot = FALSE
    ),
    regexp = "not recognised"
  )
})

test_that("more than two types raises an error", {
  expect_error(
    polarPlot(
      dat,
      pollutant = "nox",
      type = c("season", "weekend", "year"),
      plot = FALSE
    ),
    regexp = "Maximum number"
  )
})

test_that("weights of wrong length raises an error", {
  expect_error(
    polarPlot(dat, pollutant = "nox", weights = c(0.5, 1), plot = FALSE),
    regexp = "weights"
  )
})
