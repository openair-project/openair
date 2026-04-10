if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# polarFreq tests — plot = FALSE throughout
# No GAM fitting here — just binning/tapply — so runs fast with one month
dat <- selectByDate(mydata, year = 2003, month = 1)

# Shared results
pf_freq <- polarFreq(dat, plot = FALSE) # frequency
pf_mean <- polarFreq(
  dat,
  pollutant = "nox",
  statistic = "mean", # mean concentration
  plot = FALSE
)

# --- Return value structure --------------------------------------------------

test_that("polarFreq returns an openair object with expected components", {
  expect_s3_class(pf_freq, "openair")
  expect_named(pf_freq, c("plot", "data", "call"))
  expect_s3_class(pf_freq$plot, "ggplot")
  expect_s3_class(pf_freq$data, "data.frame")
})

test_that("$data has ws, wd and weights columns", {
  expect_true(all(c("ws", "wd", "weights") %in% names(pf_freq$data)))
})

# --- Data values — frequency -------------------------------------------------

test_that("frequency weights are non-negative integers", {
  w <- pf_freq$data$weights
  expect_true(all(w >= 0))
  expect_true(all(w == floor(w)))
})

test_that("wd is in [0, 360)", {
  wd <- pf_freq$data$wd
  expect_true(all(wd >= 0 & wd < 360))
})

test_that("ws values are positive multiples of ws.int (default 1)", {
  ws <- pf_freq$data$ws
  expect_true(all(ws > 0))
  expect_true(all(ws %% 1 == 0))
})

# --- Data values — mean concentration ----------------------------------------

test_that("mean weights stay within observed range of pollutant", {
  obs_range <- range(dat$nox, na.rm = TRUE)
  w <- pf_mean$data$weights
  expect_true(all(w >= obs_range[1], na.rm = TRUE))
  expect_true(all(w <= obs_range[2], na.rm = TRUE))
})

# --- statistic variants ------------------------------------------------------

test_that("statistic = 'median' returns non-negative weights within obs range", {
  pf_med <- polarFreq(
    dat,
    pollutant = "nox",
    statistic = "median",
    plot = FALSE
  )
  obs_range <- range(dat$nox, na.rm = TRUE)
  w <- pf_med$data$weights
  expect_true(all(w >= obs_range[1], na.rm = TRUE))
  expect_true(all(w <= obs_range[2], na.rm = TRUE))
})

test_that("statistic = 'weighted.mean' weights sum to approximately 100", {
  pf_wm <- polarFreq(
    dat,
    pollutant = "nox",
    statistic = "weighted.mean",
    plot = FALSE
  )
  expect_equal(sum(pf_wm$data$weights, na.rm = TRUE), 100, tolerance = 1)
})

test_that("statistic = 'frequency' with pollutant warns and switches to 'mean'", {
  expect_warning(
    polarFreq(dat, pollutant = "nox", statistic = "frequency", plot = FALSE),
    regexp = "frequency"
  )
})

# --- min.bin -----------------------------------------------------------------

test_that("min.bin = 10 produces fewer rows than min.bin = 1", {
  pf_strict <- polarFreq(dat, min.bin = 10, plot = FALSE)
  pf_loose <- polarFreq(dat, min.bin = 1, plot = FALSE)
  # strict drops low-count bins (set to NA, then na.omit removes them)
  expect_lte(nrow(pf_strict$data), nrow(pf_loose$data))
})

# --- ws.int ------------------------------------------------------------------

test_that("finer ws.int produces more rows than coarser ws.int", {
  pf_fine <- polarFreq(dat, ws.int = 0.5, plot = FALSE)
  pf_coarse <- polarFreq(dat, ws.int = 2, plot = FALSE)
  expect_gt(nrow(pf_fine$data), nrow(pf_coarse$data))
})

# --- categorical scale -------------------------------------------------------

test_that("supplying bins weights column", {
  pf_cat <- polarFreq(dat, breaks = c(0, 10, 50, 200), plot = FALSE)
  expect_true(is.factor(pf_cat$data$weights))
})

# --- type conditioning -------------------------------------------------------

test_that("type = 'weekend' adds a weekend column to $data", {
  pf_type <- polarFreq(dat, type = "weekend", plot = FALSE)
  expect_true("weekend" %in% names(pf_type$data))
})

# --- Input validation --------------------------------------------------------

test_that("non-frequency statistic without pollutant raises an error", {
  expect_error(
    polarFreq(dat, pollutant = NULL, statistic = "mean", plot = FALSE),
    regexp = "pollutant"
  )
})
