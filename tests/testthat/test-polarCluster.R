# polarCluster tests — plot = FALSE throughout
# PAM clustering + underlying polarPlot GAM is slow; skip on CRAN
skip_on_cran()

# Small slice — one month is enough; 2 clusters keeps PAM fast
dat <- selectByDate(mydata, year = 2003, month = 1)

# Shared result — single cluster count so $data is the labelled data frame
pc <- polarCluster(dat, pollutant = "nox", n.clusters = 2, plot = FALSE)

# --- Return value structure --------------------------------------------------

test_that("polarCluster returns an openair object with expected components", {
  expect_s3_class(pc, "openair")
  expect_true(all(c("plot", "data", "call", "clust_stats") %in% names(pc)))
  expect_s3_class(pc$plot, "ggplot")
})

test_that("$data is a data frame with a 'cluster' column", {
  expect_s3_class(pc$data, "data.frame")
  expect_true("cluster" %in% names(pc$data))
})

test_that("$clust_stats is a data frame with a row per cluster", {
  expect_s3_class(pc$clust_stats, "data.frame")
  expect_equal(nrow(pc$clust_stats), 2L)
})

# --- Cluster labels and counts -----------------------------------------------

test_that("cluster labels are C1 and C2 for n.clusters = 2", {
  non_na <- pc$data$cluster[!is.na(pc$data$cluster)]
  expect_true(all(non_na %in% c("C1", "C2")))
})

test_that("both clusters are non-empty", {
  tab <- table(pc$data$cluster)
  expect_true(all(tab > 0))
})

test_that("cluster assignments cover most of the non-NA input rows", {
  n_input <- sum(!is.na(dat$nox))
  n_assigned <- sum(!is.na(pc$data$cluster))
  expect_gt(n_assigned, n_input * 0.5)
})

# --- clust_stats content -----------------------------------------------------

test_that("clust_stats n values sum to the number of assigned rows", {
  expect_equal(sum(pc$clust_stats$n), sum(!is.na(pc$data$cluster)))
})

test_that("clust_stats n_percent sums to 100", {
  expect_equal(sum(pc$clust_stats$n_percent), 100)
})

test_that("clust_stats mean_nox is positive (nox is always positive)", {
  expect_true(all(pc$clust_stats$mean_nox > 0))
})

# --- n.clusters > 1 (faceted diagnostic plot) --------------------------------

test_that("n.clusters of length > 1 returns a valid openair object", {
  pc_multi <- polarCluster(
    dat,
    pollutant = "nox",
    n.clusters = 2:3,
    plot = FALSE
  )
  expect_s3_class(pc_multi, "openair")
  expect_s3_class(pc_multi$plot, "ggplot")
})

# --- plot.data = TRUE --------------------------------------------------------

test_that("plot.data = TRUE returns the polar grid data, not the labelled input", {
  pc_pd <- polarCluster(
    dat,
    pollutant = "nox",
    n.clusters = 2,
    plot = FALSE,
    plot.data = TRUE
  )
  # grid data has u, v, z columns; labelled data has date, cluster
  expect_true("u" %in% names(pc_pd$data))
  expect_false("date" %in% names(pc_pd$data))
})
