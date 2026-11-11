dat <- selectByDate(mydata, year = 2003, month = 1:3)

test_that("type = 'wd' uses the compass facet layout", {
  out <- suppressWarnings(
    distPlot(dat, pollutant = "nox", type = "wd", plot = FALSE)
  )
  expect_s3_class(out$plot$facet, "FacetWinddir")

  out <- distPlot(dat, pollutant = "nox", type = "season", plot = FALSE)
  expect_false(inherits(out$plot$facet, "FacetWinddir"))
})
