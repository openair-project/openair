# timeProp tests — plot = FALSE throughout
# Pure aggregation/reshaping — no GAM, no skip_on_cran needed
dat <- selectByDate(mydata, year = 2003, month = 1:3)
dat <- dat[!is.na(dat$wd), ]

# Shared result — monthly bars split by wind direction
tp <- timeProp(
  dat,
  pollutant = "so2",
  proportion = "wd",
  avg.time = "month",
  plot = FALSE
)

# =============================================================================
# Return value structure
# =============================================================================

test_that("timeProp returns an openair object with expected components", {
  expect_s3_class(tp, "openair")
  expect_named(tp, c("plot", "data", "call"))
  expect_s3_class(tp$plot, "ggplot")
  expect_s3_class(tp$data, "data.frame")
})

test_that("$data has xleft, xright, date and proportion columns", {
  expect_true(all(c("xleft", "xright", "date", "wd") %in% names(tp$data)))
})

test_that("$data has bar-geometry columns: var2, var2lag, Var1", {
  expect_true(all(c("var2", "var2lag", "Var1") %in% names(tp$data)))
})

# =============================================================================
# Bar geometry invariants
# =============================================================================

test_that("var2lag < var2 for all non-empty bars (bar has positive height)", {
  non_zero <- tp$data[tp$data$Var1 > 0, ]
  expect_true(all(non_zero$var2lag < non_zero$var2))
})

test_that("xleft < xright for all bars (bar has positive width)", {
  expect_true(all(tp$data$xleft < tp$data$xright))
})

test_that("Var1 (weighted contribution) is non-negative", {
  expect_true(all(tp$data$Var1 >= 0, na.rm = TRUE))
})

# =============================================================================
# Proportion column
# =============================================================================

test_that("all 8 wind direction sectors are represented", {
  expect_equal(nlevels(tp$data$wd), 8L)
})

test_that("proportion column is a factor", {
  expect_true(is.factor(tp$data$wd))
})

# =============================================================================
# normalise
# =============================================================================

test_that("normalise = TRUE makes bar totals sum to 100 within each date", {
  tp_norm <- timeProp(
    dat,
    pollutant = "so2",
    proportion = "wd",
    avg.time = "month",
    normalise = TRUE,
    plot = FALSE
  )

  totals <- tapply(tp_norm$data$Var1, tp_norm$data$date, sum, na.rm = TRUE)
  expect_true(all(abs(totals - 100) < 0.01))
})

# =============================================================================
# avg.time
# =============================================================================

test_that("avg.time = 'day' produces more date levels than avg.time = 'month'", {
  tp_day <- timeProp(
    dat,
    pollutant = "so2",
    proportion = "wd",
    avg.time = "day",
    plot = FALSE
  )
  expect_gt(
    dplyr::n_distinct(tp_day$data$date),
    dplyr::n_distinct(tp$data$date)
  )
})

# =============================================================================
# numeric proportion (split into quantiles)
# =============================================================================

test_that("numeric proportion column is split into quantile bins", {
  tp_ws <- timeProp(
    dat,
    pollutant = "so2",
    proportion = "ws",
    avg.time = "month",
    plot = FALSE
  )
  expect_true(is.factor(tp_ws$data$ws))
  expect_lte(nlevels(tp_ws$data$ws), 4L)
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'weekend' adds a weekend column to $data", {
  tp_type <- timeProp(
    dat,
    pollutant = "so2",
    proportion = "wd",
    avg.time = "month",
    type = "weekend",
    plot = FALSE
  )
  expect_true("weekend" %in% names(tp_type$data))
})
