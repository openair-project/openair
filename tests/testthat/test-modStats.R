# modStats tests
# The individual statistic functions (MB, FAC2, etc.) are pure, so we test
# them directly with small synthetic data for exact numerical checks.
# modStats() itself is tested with mydata (no2 ~ nox).

dat <- selectByDate(mydata, year = 2003)

# Shared modStats result (all default statistics)
ms <- modStats(dat, mod = "no2", obs = "nox")

# Synthetic pair with known properties for exact checks
perfect <- data.frame(obs = 1:10 * 10, mod = 1:10 * 10) # perfect model
biased <- data.frame(obs = 1:10 * 10, mod = 1:10 * 10 * 2) # mod = 2 * obs

# =============================================================================
# modStats — output structure
# =============================================================================

test_that("modStats returns a tibble with all default statistic columns", {
  expect_s3_class(ms, "tbl_df")
  expect_true(all(
    c(
      "n",
      "FAC2",
      "MB",
      "MGE",
      "NMB",
      "NMGE",
      "RMSE",
      "r",
      "P",
      "COE",
      "IOA"
    ) %in%
      names(ms)
  ))
})

test_that("modStats returns one row for type = 'default'", {
  expect_equal(nrow(ms), 1L)
})

test_that("modStats type = 'season' returns 4 rows", {
  ms_season <- modStats(dat, mod = "no2", obs = "nox", type = "season")
  expect_equal(nrow(ms_season), 4L)
  expect_true("season" %in% names(ms_season))
})

test_that("selecting a subset of statistics returns only those columns", {
  ms_sub <- modStats(
    dat,
    mod = "no2",
    obs = "nox",
    statistic = c("n", "MB", "r")
  )
  expect_true(all(c("n", "MB", "r", "P") %in% names(ms_sub)))
  expect_false("FAC2" %in% names(ms_sub))
})

test_that("n equals number of complete pairs", {
  n_complete <- sum(complete.cases(dat[, c("no2", "nox")]))
  expect_equal(ms$n, n_complete)
})

# =============================================================================
# Individual statistic functions — exact numerical checks
# =============================================================================

test_that("MB is 0 for a perfect model and positive when mod > obs", {
  expect_equal(MB(perfect)$MB, 0)
  expect_gt(MB(biased)$MB, 0)
})

test_that("MB equals mean(mod - obs)", {
  d <- data.frame(obs = c(10, 20, 30), mod = c(12, 18, 33))
  expect_equal(MB(d)$MB, mean(d$mod - d$obs))
})

test_that("MGE is 0 for a perfect model and equals MB when all errors are positive", {
  expect_equal(MGE(perfect)$MGE, 0)
  # biased: mod = 2*obs, so mod - obs = obs, all positive; MGE == MB
  expect_equal(MGE(biased)$MGE, MB(biased)$MB)
})

test_that("NMB is 0 for a perfect model", {
  expect_equal(NMB(perfect)$NMB, 0)
})

test_that("NMGE is 0 for a perfect model and >= |NMB|", {
  d <- data.frame(obs = c(10, 20, 30), mod = c(5, 25, 35))
  expect_equal(NMGE(perfect)$NMGE, 0)
  expect_gte(NMGE(d)$NMGE, abs(NMB(d)$NMB))
})

test_that("RMSE is 0 for a perfect model and > MB for asymmetric errors", {
  expect_equal(RMSE(perfect)$RMSE, 0)
  d <- data.frame(obs = c(10, 10, 10), mod = c(0, 10, 30))
  expect_gt(RMSE(d)$RMSE, abs(MB(d)$MB))
})

test_that("FAC2 is 1 for a perfect model and < 1 when mod = 3 * obs", {
  expect_equal(FAC2(perfect)$FAC2, 1)
  extreme <- data.frame(obs = 1:10, mod = 1:10 * 3)
  expect_lt(FAC2(extreme)$FAC2, 1)
})

test_that("r is 1 for a perfect model and returns both r and P", {
  result <- r(perfect)
  expect_equal(result$r, 1, tolerance = 1e-9)
  expect_true("P" %in% names(result))
  expect_gte(result$P, 0)
})

test_that("COE is 1 for a perfect model and <= 1 otherwise", {
  expect_equal(COE(perfect)$COE, 1)
  d <- data.frame(obs = c(10, 20, 30), mod = c(15, 15, 25))
  expect_lte(COE(d)$COE, 1)
})

test_that("IOA is 1 for a perfect model and in [-1, 1]", {
  expect_equal(IOA(perfect)$IOA, 1)
  d <- data.frame(obs = c(10, 20, 30), mod = c(5, 25, 35))
  expect_gte(IOA(d)$IOA, -1)
  expect_lte(IOA(d)$IOA, 1)
})

test_that("MGE >= MB.abs: gross error is always >= absolute mean bias", {
  set.seed(1)
  d <- data.frame(obs = runif(50, 5, 100), mod = runif(50, 5, 100))
  expect_gte(MGE(d)$MGE, abs(MB(d)$MB))
})

test_that("RMSE >= MGE: squared errors penalise outliers more", {
  set.seed(2)
  d <- data.frame(obs = runif(50, 5, 100), mod = runif(50, 5, 100))
  expect_gte(RMSE(d)$RMSE, MGE(d)$MGE)
})
