# Small slice for speed
dat <- selectByDate(mydata, year = 2003, month = 1:3)

# --- Output structure --------------------------------------------------------

test_that("GaussianSmooth returns a data frame with the same number of rows", {
  result <- GaussianSmooth(dat, pollutant = "o3", sigma = 24)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(dat))
})

test_that("GaussianSmooth adds smooth_ and n_ columns for a single pollutant", {
  result <- GaussianSmooth(dat, pollutant = "o3", sigma = 24)

  expect_true("smooth_o3" %in% names(result))
  expect_true("n_o3" %in% names(result))
})

test_that("GaussianSmooth respects new.name for a single pollutant", {
  result <- GaussianSmooth(
    dat,
    pollutant = "o3",
    sigma = 24,
    new.name = "o3_gauss"
  )

  expect_true("o3_gauss" %in% names(result))
  expect_false("smooth_o3" %in% names(result))
  # n_ column name is always derived from the pollutant name, not new.name
  expect_true("n_o3" %in% names(result))
})

test_that("GaussianSmooth handles multiple pollutants and adds a column pair for each", {
  result <- GaussianSmooth(dat, pollutant = c("o3", "no2"), sigma = 24)

  expect_true(all(
    c("smooth_o3", "smooth_no2", "n_o3", "n_no2") %in% names(result)
  ))
})

test_that("GaussianSmooth warns when new.name length mismatches pollutant length", {
  expect_warning(
    GaussianSmooth(
      dat,
      pollutant = c("o3", "no2"),
      sigma = 24,
      new.name = "just_one"
    ),
    regexp = "does not match"
  )
})

# --- Numerical sanity --------------------------------------------------------

test_that("smooth_o3 is numeric with no NaN values", {
  result <- GaussianSmooth(dat, pollutant = "o3", sigma = 24)

  expect_true(is.numeric(result$smooth_o3))
  expect_false(any(is.nan(result$smooth_o3)))
})

test_that("n_o3 is non-negative everywhere", {
  result <- GaussianSmooth(dat, pollutant = "o3", sigma = 24)

  expect_true(all(result$n_o3 >= 0, na.rm = TRUE))
})

test_that("smoothed values stay within the observed range", {
  result <- GaussianSmooth(dat, pollutant = "o3", sigma = 24, data.thresh = 0)

  obs_range <- range(dat$o3, na.rm = TRUE)
  non_na <- result$smooth_o3[!is.na(result$smooth_o3)]

  expect_true(all(non_na >= obs_range[1]))
  expect_true(all(non_na <= obs_range[2]))
})

test_that("larger sigma produces a smoother (lower variance) series", {
  narrow <- GaussianSmooth(
    dat,
    pollutant = "o3",
    sigma = 2,
    data.thresh = 0
  )$smooth_o3
  wide <- GaussianSmooth(
    dat,
    pollutant = "o3",
    sigma = 120,
    data.thresh = 0
  )$smooth_o3

  expect_lt(var(wide, na.rm = TRUE), var(narrow, na.rm = TRUE))
})

test_that("data.thresh = 100 produces at least as many NAs as data.thresh = 0", {
  res_low <- GaussianSmooth(dat, pollutant = "o3", sigma = 24, data.thresh = 0)
  res_high <- GaussianSmooth(
    dat,
    pollutant = "o3",
    sigma = 24,
    data.thresh = 100
  )

  expect_gte(sum(is.na(res_high$smooth_o3)), sum(is.na(res_low$smooth_o3)))
})

# --- Input validation --------------------------------------------------------

test_that("GaussianSmooth errors when data.thresh is out of range", {
  expect_error(
    GaussianSmooth(dat, pollutant = "o3", sigma = 24, data.thresh = -1),
    regexp = "data.thresh"
  )
  expect_error(
    GaussianSmooth(dat, pollutant = "o3", sigma = 24, data.thresh = 101),
    regexp = "data.thresh"
  )
})

test_that("GaussianSmooth errors on non-numeric pollutant column", {
  bad <- dat
  bad$o3 <- as.character(bad$o3)

  expect_error(
    GaussianSmooth(bad, pollutant = "o3", sigma = 24),
    regexp = "not numeric"
  )
})

# --- Edge cases --------------------------------------------------------------

test_that("GaussianSmooth handles all-NA input without erroring", {
  dat_na <- dat
  dat_na$o3 <- NA_real_

  expect_no_error(GaussianSmooth(dat_na, pollutant = "o3", sigma = 24))
})

test_that("GaussianSmooth with date.pad = TRUE still returns original row count", {
  dat_gap <- dat[!lubridate::month(dat$date) %in% 2, ]

  result <- GaussianSmooth(
    dat_gap,
    pollutant = "o3",
    sigma = 24,
    date.pad = TRUE
  )

  expect_equal(nrow(result), nrow(dat_gap))
})

test_that("GaussianSmooth output is unchanged when input has no gaps (date.pad has no effect)", {
  res_nopad <- GaussianSmooth(
    dat,
    pollutant = "o3",
    sigma = 24,
    date.pad = FALSE
  )
  res_pad <- GaussianSmooth(dat, pollutant = "o3", sigma = 24, date.pad = TRUE)

  expect_equal(res_nopad$smooth_o3, res_pad$smooth_o3)
})
