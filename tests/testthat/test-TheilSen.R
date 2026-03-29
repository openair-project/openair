# TheilSen tests — plot = FALSE throughout
# Bootstrap simulations make this moderately slow; skip on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Three years gives enough monthly data for stable Theil-Sen estimates
dat <- selectByDate(mydata, year = 2001:2003)

# Shared result — default settings
ts_res <- TheilSen(dat, pollutant = "nox", plot = FALSE, silent = TRUE)

# =============================================================================
# Return value structure
# =============================================================================

test_that("TheilSen returns an openair object with expected components", {
  expect_s3_class(ts_res, "openair")
  expect_named(ts_res, c("plot", "data", "call"))
  expect_s3_class(ts_res$plot, "ggplot")
})

test_that("$data is a list with main.data and res2 elements", {
  expect_type(ts_res$data, "list")
  expect_true(all(c("main.data", "res2") %in% names(ts_res$data)))
})

test_that("main.data has date, conc, slope, lower, upper columns", {
  expect_true(all(
    c("date", "conc", "slope", "lower", "upper") %in%
      names(ts_res$data$main.data)
  ))
})

test_that("res2 has slope, lower, upper, p.stars columns", {
  expect_true(all(
    c("slope", "lower", "upper", "p.stars") %in%
      names(ts_res$data$res2)
  ))
})

# =============================================================================
# Statistical values
# =============================================================================

test_that("res2 has exactly one row for type = 'default'", {
  expect_equal(nrow(ts_res$data$res2), 1L)
})

test_that("lower <= slope <= upper (confidence interval wraps the estimate)", {
  r <- ts_res$data$res2
  expect_lte(r$lower, r$slope)
  expect_lte(r$slope, r$upper)
})

test_that("p.stars is one of the expected significance codes", {
  valid <- c("***", "**", "*", "+", "")
  expect_true(all(ts_res$data$res2$p.stars %in% valid))
})

test_that("main.data has one row per monthly average", {
  monthly <- timeAverage(dat, avg.time = "month", progress = FALSE)
  expect_equal(nrow(ts_res$data$main.data), nrow(monthly))
})

# =============================================================================
# avg.time
# =============================================================================

test_that("avg.time = 'year' produces fewer rows in main.data than 'month'", {
  ts_yr <- TheilSen(
    dat,
    pollutant = "nox",
    avg.time = "year",
    plot = FALSE,
    silent = TRUE
  )
  expect_lt(nrow(ts_yr$data$main.data), nrow(ts_res$data$main.data))
})

# =============================================================================
# slope.percent
# =============================================================================

test_that("slope.percent = TRUE adds slope.percent to res2", {
  ts_pct <- TheilSen(
    dat,
    pollutant = "nox",
    slope.percent = TRUE,
    plot = FALSE,
    silent = TRUE
  )
  expect_true("slope.percent" %in% names(ts_pct$data$res2))
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'season' produces four rows in res2", {
  ts_s <- TheilSen(
    dat,
    pollutant = "nox",
    type = "season",
    plot = FALSE,
    silent = TRUE
  )
  expect_equal(nrow(ts_s$data$res2), 4L)
  expect_true("season" %in% names(ts_s$data$res2))
})

# =============================================================================
# Input validation
# =============================================================================

test_that("invalid avg.time raises an error", {
  expect_error(
    TheilSen(
      dat,
      pollutant = "nox",
      avg.time = "week",
      plot = FALSE,
      silent = TRUE
    ),
    regexp = "avg.time"
  )
})
