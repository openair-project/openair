# trajPlot tests — plot = FALSE throughout
# Requires sf and maps packages; skip on CRAN (rendering + map data download)
skip_on_cran()
skip_if_not_installed("sf")
skip_if_not_installed("maps")

traj <- importTraj() |>
  selectByDate(month = 1)

traj$nox <- randu$x[seq_along(traj$pressure)]

# Shared result — no pollutant, default settings
tp <- trajPlot(traj, map = FALSE, plot = FALSE)

# =============================================================================
# Return value structure
# =============================================================================

test_that("trajPlot returns an openair object with expected components", {
  expect_s3_class(tp, "openair")
  expect_named(tp, c("plot", "data", "call"))
  expect_s3_class(tp$plot, "ggplot")
  expect_s3_class(tp$data, "tbl_df")
})

test_that("$data retains date, lat, lon and hour.inc columns", {
  expect_true(all(
    c("date", "date2", "lat", "lon", "hour.inc") %in%
      names(tp$data)
  ))
})

# =============================================================================
# Full-length trajectory filtering
# =============================================================================

test_that("only full-length trajectories are retained", {
  # All trajectories in $data should have the same length as the most common
  traj_lens <- table(abs(tp$data$len))
  modal_len <- as.integer(names(which.max(traj_lens)))
  expect_true(all(abs(tp$data$len) == modal_len))
})

test_that("no rows have NA lat or lon", {
  expect_false(any(is.na(tp$data$lat)))
  expect_false(any(is.na(tp$data$lon)))
})

# =============================================================================
# Pollutant colouring
# =============================================================================

test_that("supplying a pollutant retains it in $data", {
  tp_pol <- trajPlot(traj, pollutant = "nox", map = FALSE, plot = FALSE)
  expect_true("nox" %in% names(tp_pol$data))
  non_na <- tp_pol$data$nox[!is.na(tp_pol$data$nox)]
  expect_true(all(is.finite(non_na)))
})

# =============================================================================
# group
# =============================================================================

test_that("group adds a column derived from the grouping variable", {
  traj_grp <- traj
  traj_grp$month <- lubridate::month(traj_grp$date)
  tp_grp <- trajPlot(traj_grp, group = "month", map = FALSE, plot = FALSE)
  expect_true("month" %in% names(tp_grp$data))
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'daylight' adds a daylight column to $data", {
  # Give enough data to populate multiple months
  tp_type <- trajPlot(traj, type = "daylight", map = FALSE, plot = FALSE)
  expect_true("daylight" %in% names(tp_type$data))
})

# =============================================================================
# Duplicate detection
# =============================================================================

test_that("duplicate trajectories raise an informative error", {
  traj_dup <- dplyr::bind_rows(traj, traj) # exact duplicates
  expect_error(
    trajPlot(traj_dup, map = FALSE, plot = FALSE),
    regexp = "Duplicates"
  )
})
