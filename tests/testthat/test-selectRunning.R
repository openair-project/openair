# selectRunning tests
# Use a small slice and also construct minimal synthetic data for precise checks
dat <- selectByDate(mydata, year = 2003, month = 1)

# Shared results — one flag run, one filter run
sr_flag <- selectRunning(
  dat,
  pollutant = "nox",
  criterion = ">",
  run.len = 5,
  threshold = 100,
  mode = "flag"
)
sr_filter <- selectRunning(
  dat,
  pollutant = "nox",
  criterion = ">",
  run.len = 5,
  threshold = 100,
  mode = "filter"
)

# --- Output structure --------------------------------------------------------

test_that("mode = 'flag' returns same number of rows as input with a new column", {
  expect_equal(nrow(sr_flag), nrow(dat))
  expect_true("criterion" %in% names(sr_flag))
})

test_that("mode = 'filter' returns a subset of rows, all meeting the criterion", {
  expect_lte(nrow(sr_filter), nrow(dat))
  expect_true(all(sr_filter$nox > 100, na.rm = TRUE))
})

test_that("flag column contains only the two result labels", {
  expect_true(all(sr_flag$criterion %in% c("yes", "no")))
})

test_that("custom name and result labels are used", {
  result <- selectRunning(
    dat,
    pollutant = "nox",
    criterion = ">",
    run.len = 5,
    threshold = 100,
    name = "episode",
    result = c("in", "out")
  )
  expect_true("episode" %in% names(result))
  expect_true(all(result$episode %in% c("in", "out")))
})

# --- Correctness with synthetic data -----------------------------------------

# Build a known sequence: 6 consecutive hours above threshold then a gap
make_synthetic <- function() {
  dates <- seq(
    as.POSIXct("2023-01-01 00:00", tz = "GMT"),
    by = "1 hour",
    length.out = 20
  )
  nox <- c(rep(600, 6), rep(50, 4), rep(600, 3), rep(50, 7))
  data.frame(date = dates, nox = nox)
}
syn <- make_synthetic()

test_that("run of exactly run.len hours is flagged 'yes'", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = ">",
    run.len = 6,
    threshold = 500,
    mode = "flag"
  )
  # First 6 rows: run of 6 -> should be flagged yes
  expect_true(all(result$criterion[1:6] == "yes"))
})

test_that("run shorter than run.len is not flagged", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = ">",
    run.len = 6,
    threshold = 500,
    mode = "flag"
  )
  # Rows 11-13: run of only 3 -> should be no
  expect_true(all(result$criterion[11:13] == "no"))
})

test_that("rows below threshold are never flagged 'yes'", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = ">",
    run.len = 2,
    threshold = 500,
    mode = "flag"
  )
  below <- result[result$nox <= 500, ]
  expect_true(all(below$criterion == "no"))
})

test_that("mode = 'filter' returns only rows in qualifying runs", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = ">",
    run.len = 6,
    threshold = 500,
    mode = "filter"
  )
  expect_equal(nrow(result), 6L)
  expect_true(all(result$nox > 500))
})

test_that("run.len = 1 flags every row meeting the criterion", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = ">",
    run.len = 1,
    threshold = 500,
    mode = "flag"
  )
  expected_yes <- sum(syn$nox > 500)
  expect_equal(sum(result$criterion == "yes"), expected_yes)
})

# --- Criteria ----------------------------------------------------------------

test_that("criterion '<' flags runs below threshold", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = "<",
    run.len = 4,
    threshold = 100,
    mode = "flag"
  )
  # rows 7-10 are 50 (4 in a row below 100) -> all yes
  expect_true(all(result$criterion[7:10] == "yes"))
  expect_true(all(result$criterion[1:6] == "no"))
})

test_that("criterion '>=' flags values equal to threshold", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = ">=",
    run.len = 6,
    threshold = 600,
    mode = "flag"
  )
  expect_true(all(result$criterion[1:6] == "yes"))
})

# --- run.len edge case -------------------------------------------------------

test_that("run.len longer than any run returns no 'yes' flags", {
  result <- selectRunning(
    syn,
    pollutant = "nox",
    criterion = ">",
    run.len = 99,
    threshold = 500,
    mode = "flag"
  )
  expect_true(all(result$criterion == "no"))
})

# --- Input validation --------------------------------------------------------

test_that("invalid criterion raises an error", {
  expect_error(
    selectRunning(
      dat,
      pollutant = "nox",
      criterion = "~>",
      run.len = 5,
      threshold = 100
    ),
    regexp = "criterion"
  )
})

test_that("invalid mode raises an error", {
  expect_error(
    selectRunning(
      dat,
      pollutant = "nox",
      mode = "summarise",
      run.len = 5,
      threshold = 100
    ),
    regexp = "mode"
  )
})
