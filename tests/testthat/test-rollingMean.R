# rollingMean tests
# Small slice for speed; one month is plenty for rolling window tests
dat <- selectByDate(mydata, year = 2003, month = 1)

# Shared results — reuse across most tests
rm_default <- rollingMean(dat, pollutant = "o3", width = 8, data.thresh = 0)
rm_right <- rollingMean(
  dat,
  pollutant = "o3",
  width = 8,
  data.thresh = 0,
  align = "right"
)
rm_left <- rollingMean(
  dat,
  pollutant = "o3",
  width = 8,
  data.thresh = 0,
  align = "left"
)

# --- Output structure --------------------------------------------------------

test_that("rollingMean returns a data frame with the same number of rows", {
  expect_s3_class(rm_default, "data.frame")
  expect_equal(nrow(rm_default), nrow(dat))
})

test_that("rollingMean adds a rolling column and an n_ count column", {
  expect_true("rolling8o3" %in% names(rm_default))
  expect_true("n_o3" %in% names(rm_default))
})

test_that("new.name overrides the default column name", {
  result <- rollingMean(
    dat,
    pollutant = "o3",
    width = 8,
    new.name = "o3_8h",
    data.thresh = 0
  )
  expect_true("o3_8h" %in% names(result))
  expect_false("rolling8o3" %in% names(result))
})

# --- Numerical sanity --------------------------------------------------------

test_that("rolling mean is numeric with no NaN values", {
  expect_true(is.numeric(rm_default$rolling8o3))
  expect_false(any(is.nan(rm_default$rolling8o3)))
})

test_that("n_ count column is non-negative and <= window width", {
  expect_true(all(rm_default$n_o3 >= 0, na.rm = TRUE))
  expect_true(all(rm_default$n_o3 <= 8, na.rm = TRUE))
})

test_that("rolling mean values stay within the observed range", {
  obs_range <- range(dat$o3, na.rm = TRUE)
  non_na <- rm_default$rolling8o3[!is.na(rm_default$rolling8o3)]
  expect_true(all(non_na >= obs_range[1]))
  expect_true(all(non_na <= obs_range[2]))
})

test_that("wider window produces smoother (lower variance) series", {
  narrow <- rollingMean(
    dat,
    pollutant = "o3",
    width = 2,
    data.thresh = 0
  )$rolling2o3
  wide <- rollingMean(
    dat,
    pollutant = "o3",
    width = 24,
    data.thresh = 0
  )$rolling24o3
  expect_lt(var(wide, na.rm = TRUE), var(narrow, na.rm = TRUE))
})

test_that("width = 1 returns values equal to the original series", {
  result <- rollingMean(dat, pollutant = "o3", width = 1, data.thresh = 0)
  non_na <- !is.na(dat$o3)
  expect_equal(result$rolling1o3[non_na], dat$o3[non_na], tolerance = 1e-6)
})

# --- Alignment ---------------------------------------------------------------

test_that("align = 'right' and align = 'left' produce different results", {
  expect_false(isTRUE(all.equal(rm_right$rolling8o3, rm_left$rolling8o3)))
})

test_that("align = 'right' has NAs at the start, not the end", {
  result <- rollingMean(
    dat,
    pollutant = "o3",
    width = 8,
    data.thresh = 100,
    align = "right"
  )
  expect_true(all(is.na(result$rolling8o3[1:7])))
  expect_false(is.na(result$rolling8o3[nrow(result)]))
})

test_that("align = 'left' has NAs at the end, not the start", {
  result <- rollingMean(
    dat,
    pollutant = "o3",
    width = 8,
    data.thresh = 100,
    align = "left"
  )
  n <- nrow(result)
  expect_true(all(is.na(result$rolling8o3[(n - 6):n])))
  expect_false(is.na(result$rolling8o3[1]))
})

test_that("align = 'center' is silently treated as 'centre'", {
  res_center <- rollingMean(
    dat,
    pollutant = "o3",
    width = 8,
    data.thresh = 0,
    align = "center"
  )
  res_centre <- rollingMean(
    dat,
    pollutant = "o3",
    width = 8,
    data.thresh = 0,
    align = "centre"
  )
  expect_equal(res_center$rolling8o3, res_centre$rolling8o3)
})

# --- data.thresh -------------------------------------------------------------

test_that("data.thresh = 100 produces at least as many NAs as data.thresh = 0", {
  dat_gaps <- dat
  dat_gaps$o3[sample(nrow(dat_gaps), 100)] <- NA

  res_low <- rollingMean(dat_gaps, pollutant = "o3", width = 8, data.thresh = 0)
  res_high <- rollingMean(
    dat_gaps,
    pollutant = "o3",
    width = 8,
    data.thresh = 100
  )

  expect_gte(sum(is.na(res_high$rolling8o3)), sum(is.na(res_low$rolling8o3)))
})

# --- Edge cases --------------------------------------------------------------

test_that("rollingMean handles all-NA input without erroring", {
  dat_na <- dat
  dat_na$o3 <- NA_real_
  expect_no_error(rollingMean(
    dat_na,
    pollutant = "o3",
    width = 8,
    data.thresh = 0
  ))
})

test_that("rollingMean with date.pad = TRUE still returns original row count", {
  dat_gap <- dat[!lubridate::hour(dat$date) %in% 12:14, ]
  result <- rollingMean(
    dat_gap,
    pollutant = "o3",
    width = 8,
    data.thresh = 0,
    date.pad = TRUE
  )
  expect_equal(nrow(result), nrow(dat_gap))
})

# --- Input validation --------------------------------------------------------

test_that("rollingMean errors on non-numeric pollutant column", {
  bad <- dat
  bad$o3 <- as.character(bad$o3)
  expect_error(
    rollingMean(bad, pollutant = "o3", width = 8),
    regexp = "not numeric"
  )
})

test_that("rollingMean errors when data.thresh is out of range", {
  expect_error(
    rollingMean(dat, pollutant = "o3", width = 8, data.thresh = -1),
    regexp = "data.thresh"
  )
  expect_error(
    rollingMean(dat, pollutant = "o3", width = 8, data.thresh = 101),
    regexp = "data.thresh"
  )
})

test_that("rollingMean errors on invalid align argument", {
  expect_error(
    rollingMean(dat, pollutant = "o3", width = 8, align = "diagonal"),
    regexp = "align"
  )
})
