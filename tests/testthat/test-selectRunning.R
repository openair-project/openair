test_that("selectRunning works", {
  startdate <- as.Date(ISOdate(2020, 01, 01, 01))

  testdat <- data.frame(
    date = startdate + 1:10,
    nox = seq(400, 1000, length.out = 10),
    cat = c(rep("A", 5), rep("B", 5))
  )

  x1 <- selectRunning(testdat, pollutant = "nox", run.len = 5)
  expect_equal(nrow(x1), nrow(testdat))
  expect_type(x1$criterion, "character")
  expect_equal(x1$criterion, c(rep("no", 2), rep("yes", 8)))

  x2 <- selectRunning(testdat, pollutant = "nox", run.len = 5, type = "cat")
  expect_equal(x2$criterion, c(rep("no", 5), rep("yes", 5)))

  testdat2 <- data.frame(
    date = startdate + 1:10,
    nox = c(1, 1, 1000, 1, 1, 1000, 1000, 1000, 1, 1)
  )

  x3 <- selectRunning(
    testdat2,
    pollutant = "nox",
    threshold = 500,
    run.len = 3,
    mode = "filter"
  )

  date_out <- x3$date
  date_orig <- testdat2[6:8, ]$date

  expect_equal(nrow(x3), 3L)
  expect_equal(date_out, date_orig)
  expect_equal(names(x3), names(testdat2))
})
