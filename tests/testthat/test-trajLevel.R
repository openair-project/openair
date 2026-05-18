# trajLevel tests — plot = FALSE throughout
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}
skip_if_not_installed("sf")
skip_if_not_installed("rnaturalearthdata")

traj <- importTraj() |>
  selectByDate(month = 1)

traj$nox <- randu$x[seq_along(traj$pressure)]

tl <- trajLevel(traj, map = FALSE, plot = FALSE)

# --- Return value structure --------------------------------------------------

test_that("trajLevel returns an openair object with expected components", {
  expect_s3_class(tl, "openair")
  expect_named(tl, c("plot", "data", "call"))
  expect_s3_class(tl$plot, "ggplot")
  expect_s3_class(tl$data, "data.frame")
})

test_that("$data has xgrid, ygrid, count and statistic columns for 'frequency'", {
  expect_true(all(
    c("xgrid", "ygrid", "count", "statistic") %in% names(tl$data)
  ))
})

# --- Grid values — frequency -------------------------------------------------

test_that("frequency: count is a non-negative integer", {
  expect_true(all(tl$data$count >= 0))
  expect_true(all(tl$data$count == floor(tl$data$count)))
})

test_that("frequency: xgrid and ygrid are rounded to nearest degree (default lon.inc = 1)", {
  expect_true(all(tl$data$xgrid == round(tl$data$xgrid)))
  expect_true(all(tl$data$ygrid == round(tl$data$ygrid)))
})

# --- statistic = "pscf" ------------------------------------------------------

test_that("statistic = 'pscf' returns values in [0, 1]", {
  tl_pscf <- trajLevel(
    traj,
    pollutant = "nox",
    statistic = "pscf",
    map = FALSE,
    plot = FALSE
  )
  non_na <- tl_pscf$data$nox[!is.na(tl_pscf$data$nox)]
  expect_true(all(non_na >= 0 & non_na <= 1))
})

test_that("statistic = 'pscf' $data has a percentile column", {
  tl_pscf <- trajLevel(
    traj,
    pollutant = "nox",
    statistic = "pscf",
    map = FALSE,
    plot = FALSE
  )
  expect_true("percentile" %in% names(tl_pscf$data))
})

# --- statistic = "cwt" -------------------------------------------------------

test_that("statistic = 'cwt' returns non-negative values", {
  tl_cwt <- trajLevel(
    traj,
    pollutant = "nox",
    statistic = "cwt",
    map = FALSE,
    plot = FALSE
  )
  non_na <- tl_cwt$data$nox[!is.na(tl_cwt$data$nox)]
  expect_true(all(non_na >= 0))
})

# --- lon.inc / lat.inc -------------------------------------------------------

test_that("finer grid produces more cells than coarser grid", {
  tl_fine <- trajLevel(traj, lon.inc = 0.5, map = FALSE, plot = FALSE)
  tl_coarse <- trajLevel(traj, lon.inc = 2.0, map = FALSE, plot = FALSE)
  expect_gt(nrow(tl_fine$data), nrow(tl_coarse$data))
})

# --- Input validation --------------------------------------------------------

test_that("invalid statistic raises an error", {
  expect_error(
    trajLevel(traj, statistic = "mean", map = FALSE, plot = FALSE),
    regexp = "statistic"
  )
})

test_that("sqtba with non-default type raises an error", {
  expect_error(
    trajLevel(
      traj,
      statistic = "sqtba",
      type = "month",
      map = FALSE,
      plot = FALSE
    ),
    regexp = "type"
  )
})
