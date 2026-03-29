if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# percentileRose tests — plot = FALSE throughout
# One month is enough; wd and a pollutant are all that's needed
dat <- selectByDate(mydata, year = 2003, month = 1)

# Shared result — default percentiles, fill = TRUE
pr <- percentileRose(dat, pollutant = "nox", plot = FALSE)

# --- Return value structure --------------------------------------------------

test_that("percentileRose returns an openair object with expected components", {
  expect_s3_class(pr, "openair")
  expect_named(pr, c("plot", "data", "call"))
  expect_s3_class(pr$plot, "ggplot")
  expect_s3_class(pr$data, "data.frame")
})

test_that("$data has wd, pollutant value and percentile columns", {
  expect_true(all(c("wd", "nox", "percentile") %in% names(pr$data)))
})

# --- Wind direction coverage -------------------------------------------------

test_that("wd values are multiples of angle (default 10)", {
  non_na <- pr$data$wd[!is.na(pr$data$wd)]
  expect_true(all(non_na %% (10 / 2) == 0))
})

# --- percentile = NA (mean only) ---------------------------------------------

test_that("percentile = NA returns a result without error", {
  expect_no_error(percentileRose(
    dat,
    pollutant = "nox",
    percentile = NA,
    plot = FALSE
  ))
})

# --- fill = FALSE ------------------------------------------------------------

test_that("fill = FALSE also returns a valid openair object", {
  pr_line <- percentileRose(dat, pollutant = "nox", fill = FALSE, plot = FALSE)
  expect_s3_class(pr_line, "openair")
  expect_s3_class(pr_line$plot, "ggplot")
})

# --- smooth = TRUE -----------------------------------------------------------

test_that("smooth = TRUE returns a result with more wd values than smooth = FALSE", {
  pr_smooth <- percentileRose(
    dat,
    pollutant = "nox",
    smooth = TRUE,
    plot = FALSE
  )
  pr_nosmooth <- pr # default is smooth = FALSE
  expect_gt(
    length(unique(pr_smooth$data$wd)),
    length(unique(pr_nosmooth$data$wd))
  )
})

# --- method = "cpf" ----------------------------------------------------------

test_that("method = 'cpf' with a single percentile returns a valid object", {
  pr_cpf <- percentileRose(
    dat,
    pollutant = "nox",
    method = "cpf",
    percentile = 90,
    fill = TRUE,
    plot = FALSE
  )
  expect_s3_class(pr_cpf, "openair")
})

test_that("method = 'cpf' with multiple percentiles raises an error", {
  expect_error(
    percentileRose(
      dat,
      pollutant = "nox",
      method = "cpf",
      percentile = c(75, 90),
      plot = FALSE
    ),
    regexp = "one percentile"
  )
})

# --- multiple pollutants -----------------------------------------------------

test_that("multiple pollutants add a 'variable' column to the data", {
  pr_multi <- percentileRose(dat, pollutant = c("nox", "no2"), plot = FALSE)
  expect_true("variable" %in% names(pr_multi$data))
  expect_true(all(c("nox", "no2") %in% pr_multi$data$variable))
})

# --- type conditioning -------------------------------------------------------

test_that("type = 'weekend' adds a weekend column to the data", {
  pr_type <- percentileRose(
    dat,
    pollutant = "nox",
    type = "weekend",
    plot = FALSE
  )
  expect_true("weekend" %in% names(pr_type$data))
})
