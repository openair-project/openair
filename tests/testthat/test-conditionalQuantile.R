# conditionalQuantile tests — plot = FALSE throughout
#
# Build a small synthetic obs/mod data frame with known properties.
# Using a near-perfect model (mod ≈ obs + noise) gives well-behaved quantiles.

set.seed(42)
n <- 500L
obs <- runif(n, 5, 100)
syn <- data.frame(
  date = seq(as.POSIXct("2023-01-01", tz = "GMT"), by = "hour", length.out = n),
  obs = obs,
  mod = obs + rnorm(n, 0, 5) # near-perfect predictor
)

# Shared result — covers most tests
cq <- conditionalQuantile(
  syn,
  obs = "obs",
  mod = "mod",
  bins = 15,
  plot = FALSE
)

# --- Return value structure --------------------------------------------------

test_that("conditionalQuantile returns an openair object with expected components", {
  expect_s3_class(cq, "openair")
  expect_named(cq, c("plot", "data", "call"))
  expect_s3_class(cq$plot, "ggplot")
})

test_that("$data is a data frame with a cq list-column", {
  expect_s3_class(cq$data, "data.frame")
  expect_true("cq" %in% names(cq$data))
})

test_that("each cq element contains quant, hist, and obs_range sub-frames", {
  inner <- cq$data$cq[[1]]
  expect_named(inner, c("quant", "hist", "obs_range"))
  expect_s3_class(inner$quant, "data.frame")
  expect_s3_class(inner$hist, "data.frame")
})

# --- Quantile data values ----------------------------------------------------

test_that("quant$x values are within the data range", {
  q <- cq$data$cq[[1]]$quant
  range <- range(c(syn$obs, syn$mod))
  expect_true(all(q$x >= range[1]))
  expect_true(all(q$x <= range[2]))
})

test_that("q1 (25th pct) <= med <= q2 (75th pct) where all non-NA", {
  q <- cq$data$cq[[1]]$quant
  ok <- !is.na(q$q1) & !is.na(q$med) & !is.na(q$q2)
  expect_true(all(q$q1[ok] <= q$med[ok]))
  expect_true(all(q$med[ok] <= q$q2[ok]))
})

test_that("q3 (10th pct) <= q1 (25th pct) and q2 (75th pct) <= q4 (90th pct) where non-NA", {
  q <- cq$data$cq[[1]]$quant
  ok_lo <- !is.na(q$q3) & !is.na(q$q1)
  ok_hi <- !is.na(q$q2) & !is.na(q$q4)
  expect_true(all(q$q3[ok_lo] <= q$q1[ok_lo]))
  expect_true(all(q$q2[ok_hi] <= q$q4[ok_hi]))
})

test_that("a near-perfect model has median close to the 1:1 line", {
  q <- cq$data$cq[[1]]$quant
  non_na <- !is.na(q$med)
  resid <- abs(q$med[non_na] - q$x[non_na])
  # For a near-perfect predictor the median should be within ~15 units of x
  expect_true(all(resid < 15))
})

# --- Histogram data ----------------------------------------------------------

test_that("hist data has x, count, and hist_type columns", {
  h <- cq$data$cq[[1]]$hist
  expect_true(all(c("x", "count", "hist_type") %in% names(h)))
})

test_that("hist_type contains only 'predicted' and 'observed'", {
  h <- cq$data$cq[[1]]$hist
  expect_setequal(unique(h$hist_type), c("predicted", "observed"))
})

test_that("histogram counts are non-negative integers", {
  h <- cq$data$cq[[1]]$hist
  expect_true(all(h$count >= 0))
  expect_true(all(h$count == floor(h$count)))
})

# --- bins and min.bin --------------------------------------------------------

test_that("more bins produces more rows in quant", {
  cq_coarse <- conditionalQuantile(
    syn,
    obs = "obs",
    mod = "mod",
    bins = 10,
    plot = FALSE
  )
  cq_fine <- conditionalQuantile(
    syn,
    obs = "obs",
    mod = "mod",
    bins = 20,
    plot = FALSE
  )
  expect_lt(
    nrow(cq_coarse$data$cq[[1]]$quant),
    nrow(cq_fine$data$cq[[1]]$quant)
  )
})

test_that("higher min.bin produces more NAs in the outer quantile columns", {
  cq_strict <- conditionalQuantile(
    syn,
    obs = "obs",
    mod = "mod",
    bins = 15,
    min.bin = c(200, 400),
    plot = FALSE
  )
  q_strict <- cq_strict$data$cq[[1]]$quant
  q_loose <- cq$data$cq[[1]]$quant
  expect_gte(sum(is.na(q_strict$q1)), sum(is.na(q_loose$q1)))
})

# --- type conditioning -------------------------------------------------------

test_that("type = 'daylight' produces two rows in $data", {
  cq_season <- conditionalQuantile(
    syn,
    obs = "obs",
    mod = "mod",
    bins = 10,
    type = "daylight",
    plot = FALSE
  )
  expect_true("daylight" %in% names(cq_season$data))
  expect_equal(length(cq_season$data$daylight), 2L)
})

# --- Input validation --------------------------------------------------------

test_that("more than two types raises an error", {
  expect_error(
    conditionalQuantile(
      syn,
      obs = "obs",
      mod = "mod",
      type = c("season", "weekend", "year"),
      plot = FALSE
    ),
    regexp = "two types"
  )
})
