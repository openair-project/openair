# trajCluster tests — plot = FALSE throughout
# PAM clustering on trajectory distance matrices is slow; skip on CRAN
skip_on_cran()
skip_if_not_installed("sf")
skip_if_not_installed("maps")

traj <- importTraj() |>
  selectByDate(month = 1)

traj$nox <- randu$x[seq_along(traj$pressure)]

# Shared result — 3 clusters, Euclidean method
tc <- trajCluster(traj, n.cluster = 3, plot = FALSE)

# --- Return value structure --------------------------------------------------

test_that("trajCluster returns an openair object with data and call", {
  expect_s3_class(tc, "openair")
  expect_true(all(c("data", "call") %in% names(tc)))
})

test_that("$data is a list with traj and results elements", {
  expect_type(tc$data, "list")
  expect_true(all(c("traj", "results") %in% names(tc$data)))
})

# --- traj component ----------------------------------------------------------

test_that("traj has a 'cluster' column with C1/C2/C3 labels", {
  expect_true("cluster" %in% names(tc$data$traj))
  expect_true(all(grepl("^C[0-9]+$", tc$data$traj$cluster)))
})

test_that("traj has exactly n.cluster unique cluster labels", {
  n_clusters <- dplyr::n_distinct(tc$data$traj$cluster)
  expect_equal(n_clusters, 3L)
})

# --- results component -------------------------------------------------------

test_that("results has cluster, freq, lat, lon and hour.inc columns", {
  expect_true(all(
    c("cluster", "freq", "lat", "lon", "hour.inc") %in%
      names(tc$data$results)
  ))
})

test_that("results freq values sum to 100 (percentage of all trajectories)", {
  freqs <- dplyr::distinct(tc$data$results, cluster, freq)$freq
  expect_equal(sum(freqs), 100, tolerance = 0.1)
})

test_that("results has one row per cluster per hour.inc step", {
  n_hours <- dplyr::n_distinct(traj$hour.inc)
  expect_equal(nrow(tc$data$results), 3L * n_hours)
})

# --- n.cluster ---------------------------------------------------------------

test_that("increasing n.cluster produces more clusters", {
  tc5 <- trajCluster(traj, n.cluster = 5, plot = FALSE)
  expect_equal(dplyr::n_distinct(tc5$data$traj$cluster), 5L)
})

# --- method = "Angle" --------------------------------------------------------

test_that("method = 'Angle' runs and returns the same structure", {
  tc_ang <- trajCluster(traj, n.cluster = 3, method = "Angle", plot = FALSE)
  expect_true("cluster" %in% names(tc_ang$data$traj))
  expect_equal(dplyr::n_distinct(tc_ang$data$traj$cluster), 3L)
})
