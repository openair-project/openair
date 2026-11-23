dat <- selectByDate(mydata, year = 2003, month = 1)
pp <- polarPlot(dat, "nox", plot = FALSE)
tv <- timeVariation(dat, "nox", B = 5, plot = FALSE)

test_that("print() returns the object invisibly", {
  capture.output(out <- withVisible(print(pp, plot = FALSE)))
  expect_false(out$visible)
  expect_identical(out$value, pp)
})

test_that("print() writes a description to stdout", {
  out <- capture.output(print(tv, plot = FALSE), type = "output")
  out <- cli::ansi_strip(out)
  expect_true(any(grepl("timeVariation(", out, fixed = TRUE)))
  expect_true(any(grepl("Data: 4 data frames", out, fixed = TRUE)))
  expect_true(any(grepl("$plot$hour", out, fixed = TRUE)))

  out <- cli::ansi_strip(capture.output(print(pp, plot = FALSE)))
  expect_true(any(grepl("Data: $data", out, fixed = TRUE)))

  # a long call is shown as a single call
  long <- pp
  long$call <- str2lang(paste0("f(", strrep("a + ", 60), "a)"))
  out <- cli::ansi_strip(capture.output(print(long, plot = FALSE)))
  expect_false(any(grepl("` and `", out, fixed = TRUE)))
})

test_that("head(), tail() and summary() return results without printing", {
  expect_silent(h <- head(pp))
  expect_silent(t <- tail(tv, subset = "hour"))
  expect_silent(s <- summary(pp))

  expect_equal(h, utils::head(pp$data))
  expect_named(t, "hour")
  expect_equal(t$hour, utils::tail(tv$data$hour))
  expect_s3_class(s, "table")
  expect_true(withVisible(head(pp))$visible)
})

test_that("missing subsets error, naming the function", {
  expect_error(summary(tv, subset = "nope"), "summary()", fixed = TRUE)
})

test_that("partly missing subsets warn and are ignored", {
  expect_warning(
    out <- summary(tv, subset = c("hour", "nope")),
    "nope"
  )
  expect_named(out, "hour")
})

test_that("grid.draw() allows ggsave() to save openair objects", {
  for (obj in list(pp, tv)) {
    path <- tempfile(fileext = ".png")
    ggplot2::ggsave(path, obj, width = 4, height = 4, dpi = 30)
    expect_gt(file.size(path), 0)
    unlink(path)
  }
})

test_that("as.data.frame() and as_tibble() return the data", {
  expect_s3_class(as.data.frame(pp), "data.frame")
  expect_false(inherits(as.data.frame(pp), "tbl_df"))
  expect_equal(as.data.frame(pp), as.data.frame(pp$data))
  expect_s3_class(tibble::as_tibble(pp), "tbl_df")

  # objects with several data frames need a subset
  expect_error(as.data.frame(tv), "subset")
  expect_error(tibble::as_tibble(tv, subset = "nope"), "subset")
  expect_equal(
    tibble::as_tibble(tv, subset = "hour"),
    tibble::as_tibble(tv$data$hour)
  )

  expect_warning(as.data.frame(pp, subset = "hour"), "ignored")
})
