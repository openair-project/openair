# check_numeric() tests
dat <- data.frame(
  num = c(1.5, 2, NA),
  chr_num = c("1.5", "2", NA),
  fct_num = factor(c("1.5", "2", NA)),
  chr = c("a", "b", NA),
  date = as.Date(c("2020-01-01", "2020-01-02", NA))
)

test_that("numeric columns are returned unchanged without warning", {
  expect_no_warning(out <- check_numeric(dat, "num"))
  expect_identical(out, dat)
})

test_that("numbers stored as text are converted with a warning", {
  expect_warning(out <- check_numeric(dat, "chr_num"), regexp = "Converted")
  expect_identical(out$chr_num, c(1.5, 2, NA))
})

test_that("factors are converted using their labels, not their codes", {
  expect_warning(out <- check_numeric(dat, "fct_num"), regexp = "Converted")
  expect_identical(out$fct_num, c(1.5, 2, NA))
})

test_that("values that can't be converted raise an error", {
  expect_error(check_numeric(dat, "chr"), regexp = "must be numeric")
  expect_error(check_numeric(dat, "date"), regexp = "must be numeric")
})

test_that("missing columns raise an error", {
  expect_error(check_numeric(dat, c("num", "nope")), regexp = "Can't find")
})

test_that("errors are reported from the calling function", {
  f <- function(x) check_numeric(x, "chr")
  expect_error(f(dat), class = "rlang_error")
  err <- tryCatch(f(dat), error = function(e) e)
  expect_identical(deparse(conditionCall(err)), "f(dat)")
})
