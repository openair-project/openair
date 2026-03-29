# Use a small, single-year slice throughout for speed
dat <- selectByDate(mydata, year = 2003)

# --- Output structure --------------------------------------------------------

test_that("WhittakerSmooth returns a data frame with the same number of rows", {
  result <- WhittakerSmooth(dat, pollutant = "o3", lambda = 100)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(dat))
})

test_that("WhittakerSmooth adds a smooth_ column by default", {
  result <- WhittakerSmooth(dat, pollutant = "o3", lambda = 100)

  expect_true("smooth_o3" %in% names(result))
})

test_that("WhittakerSmooth respects new.name for single pollutant", {
  result <- WhittakerSmooth(
    dat,
    pollutant = "o3",
    lambda = 100,
    new.name = "o3_smooth"
  )

  expect_true("o3_smooth" %in% names(result))
  expect_false("smooth_o3" %in% names(result))
})

test_that("WhittakerSmooth handles multiple pollutants and returns a column for each", {
  result <- WhittakerSmooth(dat, pollutant = c("o3", "no2"), lambda = 100)

  expect_true(all(c("smooth_o3", "smooth_no2") %in% names(result)))
})

test_that("WhittakerSmooth with p adds _baseline and _increment columns", {
  result <- WhittakerSmooth(dat, pollutant = "o3", lambda = 100, p = 0.05)

  expect_true(all(c("o3_baseline", "o3_increment") %in% names(result)))
  expect_false("smooth_o3" %in% names(result))
})

test_that("WhittakerSmooth with p and multiple pollutants adds columns for each", {
  result <- WhittakerSmooth(
    dat,
    pollutant = c("o3", "no2"),
    lambda = 100,
    p = 0.05
  )

  expect_true(all(
    c("o3_baseline", "o3_increment", "no2_baseline", "no2_increment") %in%
      names(result)
  ))
})

# --- Smoothed values are numerically sensible --------------------------------

test_that("smooth_o3 is numeric with no NaN values", {
  result <- WhittakerSmooth(dat, pollutant = "o3", lambda = 100)

  expect_true(is.numeric(result$smooth_o3))
  expect_false(any(is.nan(result$smooth_o3)))
})

test_that("smoothed values stay within a reasonable range of the input", {
  result <- WhittakerSmooth(dat, pollutant = "o3", lambda = 100)

  obs_range <- range(dat$o3, na.rm = TRUE)
  # Allow modest overshoot from the smoother but not orders of magnitude
  expect_true(min(result$smooth_o3, na.rm = TRUE) >= obs_range[1] - 5)
  expect_true(max(result$smooth_o3, na.rm = TRUE) <= obs_range[2] + 5)
})

test_that("higher lambda produces a smoother (lower variance) series", {
  smooth_low <- WhittakerSmooth(dat, pollutant = "o3", lambda = 1)$smooth_o3
  smooth_high <- WhittakerSmooth(dat, pollutant = "o3", lambda = 1e6)$smooth_o3

  expect_lt(var(smooth_high, na.rm = TRUE), var(smooth_low, na.rm = TRUE))
})

test_that("baseline is always <= observed values (ALS property)", {
  result <- WhittakerSmooth(dat, pollutant = "o3", lambda = 1000, p = 0.01)

  obs <- dat$o3
  base <- result$o3_baseline
  # For small p the baseline should hug the bottom: allow only trivial excess
  non_na <- !is.na(obs) & !is.na(base)
  excess <- mean(base[non_na] > obs[non_na] + 1) # fraction exceeding obs by >1
  expect_lt(excess, 0.05)
})

test_that("increment equals observed minus baseline (within floating-point tolerance)", {
  result <- WhittakerSmooth(dat, pollutant = "o3", lambda = 100, p = 0.05)

  non_na <- !is.na(dat$o3) & !is.na(result$o3_baseline)
  expect_equal(
    result$o3_increment[non_na],
    (dat$o3 - result$o3_baseline)[non_na],
    tolerance = 1e-6
  )
})

# --- Input validation --------------------------------------------------------

test_that("WhittakerSmooth errors on non-numeric pollutant column", {
  bad <- dat
  bad$o3 <- as.character(bad$o3)

  expect_error(
    WhittakerSmooth(bad, pollutant = "o3", lambda = 100),
    regexp = "not numeric"
  )
})

test_that("WhittakerSmooth errors when p is outside [0, 1]", {
  expect_error(
    WhittakerSmooth(dat, pollutant = "o3", lambda = 100, p = 1.5),
    regexp = "between 0 and 1"
  )
  expect_error(
    WhittakerSmooth(dat, pollutant = "o3", lambda = 100, p = -0.1),
    regexp = "between 0 and 1"
  )
})

test_that("WhittakerSmooth errors when p length mismatches pollutant length", {
  expect_error(
    WhittakerSmooth(
      dat,
      pollutant = c("o3", "no2"),
      lambda = 100,
      p = c(0.01, 0.05, 0.1)
    ),
    regexp = "length 1 or the same length"
  )
})

test_that("WhittakerSmooth warns when new.name length mismatches pollutant length", {
  expect_warning(
    WhittakerSmooth(
      dat,
      pollutant = c("o3", "no2"),
      lambda = 100,
      new.name = "just_one"
    ),
    regexp = "does not match"
  )
})

test_that("WhittakerSmooth warns when new.name is supplied alongside p", {
  expect_warning(
    WhittakerSmooth(
      dat,
      pollutant = "o3",
      lambda = 100,
      p = 0.05,
      new.name = "ignored"
    ),
    regexp = "ignored"
  )
})

# --- Missing data handling ---------------------------------------------------

test_that("WhittakerSmooth handles data with missing values without erroring", {
  dat_gaps <- dat
  dat_gaps$o3[sample(nrow(dat_gaps), 200)] <- NA

  expect_no_error(WhittakerSmooth(dat_gaps, pollutant = "o3", lambda = 100))
})

test_that("WhittakerSmooth with date.pad = TRUE still returns original row count", {
  # Introduce a gap then pad back
  dat_gap <- dat[!lubridate::month(dat$date) %in% 6, ]

  result <- WhittakerSmooth(
    dat_gap,
    pollutant = "o3",
    lambda = 100,
    date.pad = TRUE
  )

  expect_equal(nrow(result), nrow(dat_gap))
})
