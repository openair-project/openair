# windRose / pollutionRose tests — plot = FALSE throughout
# Pure binning — no GAM, no skip_on_cran needed
dat <- selectByDate(mydata, year = 2003, month = 1)

# Shared results
wr <- windRose(dat, plot = FALSE)
pr <- pollutionRose(dat, pollutant = "nox", plot = FALSE)

# =============================================================================
# windRose — return value structure
# =============================================================================

test_that("windRose returns an openair object with expected components", {
  expect_s3_class(wr, "openair")
  expect_named(wr, c("plot", "data", "call"))
  expect_s3_class(wr$plot, "ggplot")
  expect_s3_class(wr$data, "tbl_df")
})

test_that("$data has wd, calm and at least one Interval column", {
  expect_true("wd" %in% names(wr$data))
  expect_true("calm" %in% names(wr$data))
  expect_true(any(grepl("^Interval", names(wr$data))))
})

test_that("wd values are multiples of angle (default 30) or the calm flag (-999)", {
  wd_vals <- wr$data$wd
  non_calm <- wd_vals[wd_vals != -999]
  expect_true(all(non_calm %% 30 == 0))
})

# =============================================================================
# windRose — proportion invariants
# =============================================================================

test_that("prop.count frequencies sum to ~100 within each panel (including calm)", {
  # The maximum Interval column gives cumulative proportions up to max ws
  int_cols <- grep("^Interval", names(wr$data), value = TRUE)
  max_col <- int_cols[length(int_cols)]

  # Sum of directional proportions + calm ≈ 100
  dir_rows <- wr$data[wr$data$wd != -999, ]
  total <- sum(dir_rows[[max_col]], na.rm = TRUE) + unique(dir_rows$calm)
  expect_equal(total, 100, tolerance = 1)
})

test_that("Interval proportions are non-negative", {
  int_cols <- grep("^Interval", names(wr$data), value = TRUE)
  vals <- unlist(wr$data[wr$data$wd != -999, int_cols])
  expect_true(all(vals >= 0, na.rm = TRUE))
})

test_that("calm proportion is in [0, 100]", {
  expect_true(all(wr$data$calm >= 0 & wr$data$calm <= 100, na.rm = TRUE))
})

# =============================================================================
# windRose — statistic variants
# =============================================================================

test_that("statistic = 'abs.count' returns integer-valued frequencies", {
  wr_abs <- windRose(dat, statistic = "abs.count", plot = FALSE)
  int_cols <- grep("^Interval", names(wr_abs$data), value = TRUE)
  vals <- unlist(wr_abs$data[wr_abs$data$wd != -999, int_cols])
  expect_true(all(vals == floor(vals), na.rm = TRUE))
})

# =============================================================================
# windRose — type conditioning
# =============================================================================

test_that("type = 'weekend' adds a weekend column to $data", {
  wr_type <- windRose(dat, type = "weekend", plot = FALSE)
  expect_true("weekend" %in% names(wr_type$data))
})

# =============================================================================
# windRose — angle
# =============================================================================

test_that("angle = 10 produces more direction bins than angle = 30", {
  wr_10 <- windRose(dat, angle = 10, plot = FALSE)
  n_dirs_30 <- sum(wr$data$wd != -999)
  n_dirs_10 <- sum(wr_10$data$wd != -999)
  expect_gt(n_dirs_10, n_dirs_30)
})

# =============================================================================
# pollutionRose — return value structure
# =============================================================================

test_that("pollutionRose returns an openair object with expected components", {
  expect_s3_class(pr, "openair")
  expect_named(pr, c("plot", "data", "call"))
  expect_s3_class(pr$plot, "ggplot")
})

test_that("pollutionRose $data has wd and Interval columns", {
  expect_true("wd" %in% names(pr$data))
  expect_true(any(grepl("^Interval", names(pr$data))))
})

# =============================================================================
# pollutionRose — breaks attribute
# =============================================================================

test_that("$data has an 'intervals' attribute listing the break labels", {
  expect_false(is.null(attr(pr$data, "intervals")))
  expect_type(attr(pr$data, "intervals"), "character")
})

# =============================================================================
# windRose — normalise
# =============================================================================

test_that("normalise = TRUE produces a 'freq' column in $data", {
  wr_norm <- windRose(dat, normalise = TRUE, plot = FALSE)
  expect_true("freq" %in% names(wr_norm$data))
})
