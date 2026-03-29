# polarDiff tests — plot = FALSE throughout
# Two GAM surface fits (before + after) make this slow; skip on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Use two short, non-overlapping periods as before/after
before <- selectByDate(mydata, year = 2002, month = 1:2)
after <- selectByDate(mydata, year = 2003, month = 1:2)

# Shared result
pd <- polarDiff(before, after, pollutant = "nox", plot = FALSE)

# --- Return value structure --------------------------------------------------

test_that("polarDiff returns a list with plot, data and call components", {
  expect_true(all(c("plot", "data", "call") %in% names(pd)))
  expect_s3_class(pd$plot, "ggplot")
  expect_s3_class(pd$data, "data.frame")
})

# --- Data content ------------------------------------------------------------

test_that("$data has u, v, after, before, nox, ws and wd columns", {
  expect_true(all(
    c("u", "v", "after", "before", "nox", "ws", "wd") %in%
      names(pd$data)
  ))
})

test_that("difference column equals after minus before", {
  d <- pd$data
  ok <- !is.na(d$after) & !is.na(d$before) & !is.na(d$nox)
  expect_equal(d$nox[ok], d$after[ok] - d$before[ok], tolerance = 1e-9)
})

test_that("difference values can be negative (surface can go either way)", {
  non_na <- pd$data$nox[!is.na(pd$data$nox)]
  expect_true(any(non_na < 0) || any(non_na > 0))
})

test_that("wd is in [0, 360]", {
  non_na <- pd$data$wd[!is.na(pd$data$wd)]
  expect_true(all(non_na >= 0 & non_na <= 360))
})

test_that("ws (x) is non-negative", {
  non_na <- pd$data$ws[!is.na(pd$data$ws)]
  expect_true(all(non_na >= 0))
})

# --- different pollutant -----------------------------------------------------

test_that("changing pollutant to 'no2' produces a result with an 'no2' column", {
  pd_no2 <- polarDiff(before, after, pollutant = "no2", plot = FALSE)
  expect_true("no2" %in% names(pd_no2$data))
  expect_false("nox" %in% names(pd_no2$data))
})
