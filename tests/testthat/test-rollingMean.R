test_that("rollingMean works", {
  testdat <- head(mydata, n = 20)

  # check C works
  expect_no_error(rollingMean(testdat))

  # different alignments
  left <- rollingMean(testdat, align = "left")
  expect_equal(left$rolling8o3[15:20], rep(NA_real_, 6))

  right <- rollingMean(testdat, align = "right")
  expect_equal(right$rolling8o3[1:6], rep(NA_real_, 6))

  middle <- rollingMean(testdat, align = "center")
  expect_equal(middle$rolling8o3[c(1:4, 19:20)], rep(NA_real_, 6))

  # edge cases
  expect_error(rollingMean(testdat, data.thresh = 200))
  expect_no_error(rollingMean(testdat, width = 0L))

  # check function errors
  testdat2 <- rbind(
    transform(testdat, site = "A"),
    transform(testdat, site = "B")
  )
  expect_error(rollingMean(testdat2))
})
