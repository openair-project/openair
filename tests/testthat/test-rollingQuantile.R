# Small slice for speed
dat <- selectByDate(mydata, year = 2003, month = 1:3)

# --- Output structure --------------------------------------------------------

test_that("rollingQuantile returns a data frame with the same number of rows", {
  result <- rollingQuantile(dat, pollutant = "o3", width = 8, probs = 0.5)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(dat))
})

test_that("rollingQuantile adds one column per quantile with correct naming", {
  result <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 8,
    probs = c(0.05, 0.95)
  )

  expect_true("q_o3_0.05" %in% names(result))
  expect_true("q_o3_0.95" %in% names(result))
})

test_that("rollingQuantile single prob produces exactly one new quantile column", {
  before <- names(dat)
  result <- rollingQuantile(dat, pollutant = "o3", width = 8, probs = 0.5)
  new_cols <- setdiff(names(result), before)

  expect_true("q_o3_0.5" %in% new_cols)
})

# --- Numerical sanity --------------------------------------------------------

test_that("rolling median (p=0.5) is numeric with no NaN", {
  result <- rollingQuantile(dat, pollutant = "o3", width = 8, probs = 0.5)

  expect_true(is.numeric(result$q_o3_0.5))
  expect_false(any(is.nan(result$q_o3_0.5)))
})

test_that("rolling quantile values stay within the observed range", {
  result <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 8,
    probs = c(0.05, 0.95),
    data.thresh = 0
  )

  obs_range <- range(dat$o3, na.rm = TRUE)
  non_na_lo <- result$q_o3_0.05[!is.na(result$q_o3_0.05)]
  non_na_hi <- result$q_o3_0.95[!is.na(result$q_o3_0.95)]

  expect_true(all(non_na_lo >= obs_range[1]))
  expect_true(all(non_na_hi <= obs_range[2]))
})

test_that("upper quantile >= lower quantile at every row", {
  result <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 8,
    probs = c(0.1, 0.9),
    data.thresh = 0
  )

  both_non_na <- !is.na(result$q_o3_0.1) & !is.na(result$q_o3_0.9)
  expect_true(all(result$q_o3_0.9[both_non_na] >= result$q_o3_0.1[both_non_na]))
})

test_that("wider window produces a smoother series (lower variance)", {
  narrow <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 4,
    probs = 0.5,
    data.thresh = 0
  )$q_o3_0.5
  wide <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 48,
    probs = 0.5,
    data.thresh = 0
  )$q_o3_0.5

  expect_lt(var(wide, na.rm = TRUE), var(narrow, na.rm = TRUE))
})

test_that("data.thresh = 100 produces more NAs than data.thresh = 0", {
  res_low <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 8,
    probs = 0.5,
    data.thresh = 0
  )
  res_high <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 8,
    probs = 0.5,
    data.thresh = 100
  )

  expect_gte(sum(is.na(res_high$q_o3_0.5)), sum(is.na(res_low$q_o3_0.5)))
})

# --- Alignment ---------------------------------------------------------------

test_that("align = 'right' and align = 'left' produce different results", {
  right <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 24,
    probs = 0.5,
    align = "right"
  )$q_o3_0.5
  left <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 24,
    probs = 0.5,
    align = "left"
  )$q_o3_0.5

  expect_false(isTRUE(all.equal(right, left)))
})

test_that("align = 'right' has NAs at the start, not the end", {
  result <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 24,
    probs = 0.5,
    align = "right",
    data.thresh = 100
  )

  expect_true(all(is.na(result$q_o3_0.5[1:23])))
  expect_false(is.na(result$q_o3_0.5[nrow(result)]))
})

test_that("align = 'left' has NAs at the end, not the start", {
  result <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 24,
    probs = 0.5,
    align = "left",
    data.thresh = 100
  )

  n <- nrow(result)
  expect_true(all(is.na(result$q_o3_0.5[(n - 22):n])))
  expect_false(is.na(result$q_o3_0.5[1]))
})

test_that("align = 'center' is silently treated as 'centre'", {
  res_center <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 8,
    probs = 0.5,
    align = "center"
  )
  res_centre <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 8,
    probs = 0.5,
    align = "centre"
  )

  expect_equal(res_center$q_o3_0.5, res_centre$q_o3_0.5)
})

# --- Input validation --------------------------------------------------------

test_that("rollingQuantile errors when data.thresh is out of range", {
  expect_error(
    rollingQuantile(dat, pollutant = "o3", width = 8, data.thresh = -1),
    regexp = "data.thresh"
  )
  expect_error(
    rollingQuantile(dat, pollutant = "o3", width = 8, data.thresh = 101),
    regexp = "data.thresh"
  )
})

test_that("rollingQuantile errors when probs is out of range", {
  expect_error(
    rollingQuantile(dat, pollutant = "o3", width = 8, probs = -0.1),
    regexp = "probs"
  )
  expect_error(
    rollingQuantile(dat, pollutant = "o3", width = 8, probs = 1.1),
    regexp = "probs"
  )
})

test_that("rollingQuantile errors on non-numeric pollutant column", {
  bad <- dat
  bad$o3 <- as.character(bad$o3)

  expect_error(
    rollingQuantile(bad, pollutant = "o3", width = 8),
    regexp = "not numeric"
  )
})

test_that("rollingQuantile errors on invalid align argument", {
  expect_error(
    rollingQuantile(dat, pollutant = "o3", width = 8, align = "diagonal"),
    regexp = "align"
  )
})

# --- Edge cases --------------------------------------------------------------

test_that("width = 1 returns the original values (window is a single point)", {
  result <- rollingQuantile(
    dat,
    pollutant = "o3",
    width = 1,
    probs = 0.5,
    data.thresh = 0
  )

  non_na <- !is.na(dat$o3)
  expect_equal(result$q_o3_0.5[non_na], dat$o3[non_na], tolerance = 1e-6)
})

test_that("rollingQuantile handles all-NA input without erroring", {
  dat_na <- dat
  dat_na$o3 <- NA_real_

  expect_no_error(
    rollingQuantile(dat_na, pollutant = "o3", width = 8, probs = 0.5)
  )
})

test_that("rollingQuantile with date.pad = TRUE still returns original row count", {
  dat_gap <- dat[!lubridate::month(dat$date) %in% 2, ]

  result <- rollingQuantile(
    dat_gap,
    pollutant = "o3",
    width = 8,
    probs = 0.5,
    date.pad = TRUE
  )

  expect_equal(nrow(result), nrow(dat_gap))
})
