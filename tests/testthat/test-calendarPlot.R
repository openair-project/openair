if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# calendarPlot tests — plot = FALSE throughout to skip rendering
# One year, one month subsample where possible for speed
dat <- selectByDate(mydata, year = 2003)
dat1 <- selectByDate(mydata, year = 2003, month = 1)

# Shared result — covers most structural tests
cp <- calendarPlot(dat, pollutant = "o3", year = 2003, plot = FALSE)

# --- Return value structure --------------------------------------------------

test_that("calendarPlot returns an openair object with expected components", {
  expect_s3_class(cp, "openair")
  expect_named(cp, c("plot", "data", "call"))
  expect_s3_class(cp$plot, "ggplot")
  expect_s3_class(cp$data, "data.frame")
})

test_that("calendarPlot$data has calendar grid columns", {
  expect_true(all(
    c("x", "y", "conc.mat", "date.mat", "date") %in% names(cp$data)
  ))
})

test_that("x is in 1:7 (day of week columns) and y is in 1:6 (week rows)", {
  expect_true(all(cp$data$x %in% 1:7))
  expect_true(all(cp$data$y %in% 1:6))
})

# --- Year / month filtering --------------------------------------------------

test_that("year argument filters data to the requested year", {
  non_na <- cp$data$date[!is.na(cp$data$date)]
  expect_true(all(lubridate::year(non_na) == 2003))
})

test_that("month argument retains only the requested months", {
  cp_months <- calendarPlot(
    dat,
    pollutant = "o3",
    year = 2003,
    month = c(1, 6),
    plot = FALSE
  )
  non_na <- cp_months$data$date[!is.na(cp_months$data$date)]
  expect_true(all(lubridate::month(non_na) %in% c(1, 6)))
})

test_that("12 months of data produces 12 unique cuts labels", {
  n_months <- dplyr::n_distinct(cp$data$cuts, na.rm = TRUE)
  expect_equal(n_months, 12L)
})

# --- Data values -------------------------------------------------------------

test_that("conc.mat values (non-NA) stay within the observed daily range", {
  daily <- timeAverage(dat, avg.time = "day", progress = FALSE)
  obs_range <- range(daily$o3, na.rm = TRUE)
  non_na <- cp$data$conc.mat[!is.na(cp$data$conc.mat)]
  expect_true(all(non_na >= obs_range[1] - 1e-9))
  expect_true(all(non_na <= obs_range[2] + 1e-9))
})

# --- statistic ---------------------------------------------------------------

test_that("statistic = 'max' produces values >= statistic = 'mean'", {
  cp_max <- calendarPlot(
    dat1,
    pollutant = "o3",
    year = 2003,
    month = 1,
    statistic = "max",
    plot = FALSE
  )
  cp_mean <- calendarPlot(
    dat1,
    pollutant = "o3",
    year = 2003,
    month = 1,
    statistic = "mean",
    plot = FALSE
  )

  non_na <- !is.na(cp_max$data$conc.mat) & !is.na(cp_mean$data$conc.mat)
  expect_true(all(
    cp_max$data$conc.mat[non_na] >= cp_mean$data$conc.mat[non_na]
  ))
})

# --- annotate ----------------------------------------------------------------

test_that("annotate options are accepted without error", {
  for (ann in c("date", "value", "none")) {
    expect_no_error(
      calendarPlot(
        dat1,
        pollutant = "o3",
        year = 2003,
        month = 1,
        annotate = ann,
        plot = FALSE
      )
    )
  }
})

# --- categorical scale -------------------------------------------------------

test_that("supplying breaks produces a factor conc.mat column", {
  cp_cat <- calendarPlot(
    dat1,
    pollutant = "o3",
    year = 2003,
    month = 1,
    breaks = c(0, 40, 80, 200),
    labels = c("low", "mid", "high"),
    plot = FALSE
  )
  expect_true(is.factor(cp_cat$data$conc.mat))
  expect_true(all(levels(cp_cat$data$conc.mat) %in% c("low", "mid", "high")))
})

# --- remove.empty ------------------------------------------------------------

test_that("remove.empty = TRUE produces fewer rows than remove.empty = FALSE for sparse data", {
  sparse <- selectByDate(mydata, year = 2003, month = c(1, 6))
  cp_rm <- calendarPlot(
    sparse,
    pollutant = "o3",
    year = 2003,
    remove.empty = TRUE,
    plot = FALSE
  )
  cp_keep <- calendarPlot(
    sparse,
    pollutant = "o3",
    year = 2003,
    remove.empty = FALSE,
    plot = FALSE
  )
  expect_lte(nrow(cp_rm$data), nrow(cp_keep$data))
})

# --- Input validation --------------------------------------------------------

test_that("more than two type values raises an error", {
  expect_error(
    calendarPlot(
      dat,
      pollutant = "o3",
      year = 2003,
      type = c("month", "season", "year"),
      plot = FALSE
    ),
    regexp = "length 1 or 2"
  )
})

test_that("two types without 'month' raises an error", {
  expect_error(
    calendarPlot(
      dat,
      pollutant = "o3",
      year = 2003,
      type = c("season", "year"),
      plot = FALSE
    ),
    regexp = "month"
  )
})

test_that("w.shift outside [0, 6] raises an error", {
  expect_error(
    calendarPlot(
      dat1,
      pollutant = "o3",
      year = 2003,
      month = 1,
      w.shift = 7,
      plot = FALSE
    ),
    regexp = "w.shift"
  )
})

test_that("no data for the requested year raises an error", {
  expect_error(
    calendarPlot(dat, pollutant = "o3", year = 1900, plot = FALSE),
    regexp = "No data"
  )
})
