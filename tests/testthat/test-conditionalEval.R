skip_on_cran()

# conditionalEval tests — plot = FALSE throughout
#
# mydata lacks obs/mod pairs, so we build a minimal synthetic frame.
# Keep it small (one year, hourly) so bootstrap loops are fast.

set.seed(42)
n <- 500L
syn <- data.frame(
  date = seq(as.POSIXct("2023-01-01", tz = "GMT"), by = "hour", length.out = n),
  obs = runif(n, 10, 100),
  mod = runif(n, 10, 100),
  nox.obs = runif(n, 5, 80),
  nox.mod = runif(n, 5, 80),
  ws.obs = runif(n, 1, 10),
  ws.mod = runif(n, 1, 10)
)
# Add a categorical column for "other" mode tests
syn$season <- cutData(syn, "season")$season

# Shared results (B=50 in bootstrap via bins to keep it fast)
ce_mb <- conditionalEval(
  syn,
  obs = "obs",
  mod = "mod",
  var.obs = "nox.obs",
  var.mod = "nox.mod",
  statistic = "MB",
  bins = 15,
  plot = FALSE
)
ce_other <- conditionalEval(
  syn,
  obs = "obs",
  mod = "mod",
  statistic = "season",
  bins = 15,
  plot = FALSE
)

# --- Return value structure --------------------------------------------------

test_that("conditionalEval returns an openair object with expected components", {
  expect_s3_class(ce_mb, "openair")
  expect_named(ce_mb, c("plot", "data", "call"))
  expect_s3_class(ce_mb$plot, "patchwork")
})

test_that("stat mode: $data has pred.cut, mean, lower, upper, statistic, group columns", {
  expect_true(all(
    c("pred.cut", "mean", "lower", "upper", "statistic", "group") %in%
      names(ce_mb$data)
  ))
})

test_that("other mode: $data has pred.cut, Freq, and the statistic column", {
  expect_true(all(c("pred.cut", "Freq", "season") %in% names(ce_other$data)))
})

# --- Data values — stat mode -------------------------------------------------

test_that("pred.cut values are numeric and within the data range", {
  obs_range <- range(c(syn$obs, syn$mod))
  non_na <- ce_mb$data$pred.cut[!is.na(ce_mb$data$pred.cut)]
  expect_true(all(non_na >= obs_range[1]))
  expect_true(all(non_na <= obs_range[2]))
})

test_that("lower <= mean <= upper in stat mode (where all are non-NA)", {
  d <- ce_mb$data
  ok <- !is.na(d$mean) & !is.na(d$lower) & !is.na(d$upper)
  expect_true(all(d$lower[ok] <= d$mean[ok]))
  expect_true(all(d$mean[ok] <= d$upper[ok]))
})

test_that("statistic column contains only the requested statistic", {
  expect_true(all(as.character(ce_mb$data$statistic) == "MB"))
})

test_that("group column contains only the requested var.obs", {
  expect_true(all(as.character(ce_mb$data$group) == "nox.obs"))
})

# --- Multiple statistics and variables ---------------------------------------

test_that("multiple statistics produce one level per stat in the statistic column", {
  ce_multi <- conditionalEval(
    syn,
    obs = "obs",
    mod = "mod",
    var.obs = c("nox.obs", "ws.obs"),
    var.mod = c("nox.mod", "ws.mod"),
    statistic = c("MB", "NMB"),
    bins = 10,
    plot = FALSE
  )
  expect_setequal(levels(ce_multi$data$statistic), c("MB", "NMB"))
  expect_setequal(levels(ce_multi$data$group), c("nox.obs", "ws.obs"))
})

# --- Data values — other mode ------------------------------------------------

test_that("Freq values sum to approximately 1 within each pred.cut bin", {
  totals <- tapply(ce_other$data$Freq, ce_other$data$pred.cut, sum)
  expect_true(all(abs(totals - 1) < 1e-9))
})

test_that("Freq values are in [0, 1]", {
  expect_true(all(ce_other$data$Freq >= 0 & ce_other$data$Freq <= 1))
})

# --- Input validation --------------------------------------------------------

test_that("more than one type raises an error", {
  expect_error(
    conditionalEval(
      syn,
      obs = "obs",
      mod = "mod",
      var.obs = "nox.obs",
      var.mod = "nox.mod",
      type = c("season", "weekend"),
      plot = FALSE
    ),
    regexp = "one type"
  )
})

test_that("mismatched var.obs and var.mod lengths raise an error", {
  expect_error(
    conditionalEval(
      syn,
      obs = "obs",
      mod = "mod",
      var.obs = c("nox.obs", "ws.obs"),
      var.mod = "nox.mod",
      statistic = "MB",
      bins = 10,
      plot = FALSE
    ),
    regexp = "var.obs"
  )
})
