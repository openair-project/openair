# Small, fixed slices — subsample heavily to keep bootstrap fast
dat <- selectByDate(mydata, year = 2003, month = 1:3)
dat1 <- selectByDate(mydata, year = 2003, month = 1)

# Run the most-used configurations once and reuse
vp_mean <- variationPlot(
  dat,
  pollutant = "nox",
  x = "hour",
  B = 20,
  plot = FALSE
)
vp_median <- variationPlot(
  dat,
  pollutant = "nox",
  x = "hour",
  statistic = "median",
  plot = FALSE
)
tv <- timeVariation(dat, pollutant = "nox", B = 20, plot = FALSE)

# =============================================================================
# variationPlot
# =============================================================================

# --- Return value structure --------------------------------------------------

test_that("variationPlot returns an openair object with expected components", {
  expect_s3_class(vp_mean, "openair")
  expect_named(vp_mean, c("plot", "data", "call"))
  expect_s3_class(vp_mean$plot, "ggplot")
  expect_s3_class(vp_mean$data, "tbl_df")
})

test_that("variationPlot$data has required columns", {
  expect_true(all(
    c("x", "group", "mid", "min", "max", "ci", "statistic") %in%
      names(vp_mean$data)
  ))
})

# --- Data values -------------------------------------------------------------

test_that("variationPlot mean: mid is between min and max", {
  d <- vp_mean$data
  ok <- !is.na(d$mid) & !is.na(d$min) & !is.na(d$max)
  expect_true(all(d$mid[ok] >= d$min[ok]))
  expect_true(all(d$mid[ok] <= d$max[ok]))
})

test_that("variationPlot median: mid is between min and max", {
  d <- vp_median$data
  ok <- !is.na(d$mid) & !is.na(d$min) & !is.na(d$max)
  expect_true(all(d$mid[ok] >= d$min[ok]))
  expect_true(all(d$mid[ok] <= d$max[ok]))
})

test_that("variationPlot statistic column records the statistic used", {
  expect_true(all(vp_mean$data$statistic == "mean"))
  expect_true(all(vp_median$data$statistic == "median"))
})

test_that("variationPlot x = 'hour' produces 24 x levels", {
  expect_equal(length(unique(stats::na.omit(vp_mean$data$x))), 24L)
})

# --- normalise ---------------------------------------------------------------

test_that("normalise = TRUE scales mid values to be centred near 1", {
  vp_norm <- variationPlot(
    dat,
    pollutant = "nox",
    x = "hour",
    normalise = TRUE,
    B = 20,
    plot = FALSE
  )
  expect_equal(mean(vp_norm$data$mid, na.rm = TRUE), 1, tolerance = 0.1)
})

# --- multiple pollutants -----------------------------------------------------

test_that("multiple pollutants appear as separate group levels", {
  vp_multi <- variationPlot(
    dat,
    pollutant = c("nox", "no2"),
    x = "hour",
    B = 20,
    plot = FALSE
  )
  expect_setequal(levels(vp_multi$data$group), c("nox", "no2"))
})

# --- conf.int ----------------------------------------------------------------

test_that("multiple conf.int values produce multiple ci rows per x", {
  vp_ci <- variationPlot(
    dat,
    pollutant = "nox",
    x = "hour",
    conf.int = c(0.5, 0.95),
    B = 20,
    plot = FALSE
  )
  expect_setequal(unique(vp_ci$data$ci), c(0.5, 0.95))
})

# --- difference --------------------------------------------------------------

test_that("difference = TRUE adds a difference group level", {
  vp_diff <- variationPlot(
    dat,
    pollutant = c("nox", "no2"),
    x = "hour",
    difference = TRUE,
    B = 20,
    plot = FALSE
  )
  groups <- as.character(levels(vp_diff$data$group))
  expect_true(any(grepl("-", groups, fixed = TRUE)))
})

# --- input validation --------------------------------------------------------

test_that("variationPlot errors with more than two types", {
  expect_error(
    variationPlot(
      dat,
      pollutant = "nox",
      x = "hour",
      type = c("season", "year", "weekday"),
      plot = FALSE
    ),
    regexp = "type"
  )
})

test_that("variationPlot errors when group and multiple pollutants are both set", {
  expect_error(
    variationPlot(
      dat,
      pollutant = c("nox", "no2"),
      x = "hour",
      group = "season",
      plot = FALSE
    ),
    regexp = "pollutant"
  )
})

test_that("variationPlot errors when type duplicates pollutant", {
  expect_error(
    variationPlot(
      dat,
      pollutant = "nox",
      x = "hour",
      type = "nox",
      plot = FALSE
    ),
    regexp = "type"
  )
})

test_that("variationPlot errors on invalid statistic", {
  expect_error(
    variationPlot(
      dat,
      pollutant = "nox",
      x = "hour",
      statistic = "mode",
      plot = FALSE
    ),
    regexp = "statistic"
  )
})

# =============================================================================
# timeVariation
# =============================================================================

# --- Return value structure --------------------------------------------------

test_that("timeVariation returns an openair object with expected components", {
  expect_s3_class(tv, "openair")
  expect_true(all(c("plot", "data", "call") %in% names(tv)))
})

test_that("timeVariation$plot is a list named after panels", {
  default_panels <- c("hour.weekday", "hour", "month", "weekday")
  expect_true(all(default_panels %in% names(tv$plot)))
})

test_that("timeVariation$data is a list of tibbles named after panels", {
  default_panels <- c("hour.weekday", "hour", "month", "weekday")
  expect_true(all(default_panels %in% names(tv$data)))
  expect_true(all(purrr::map_lgl(tv$data[default_panels], is.data.frame)))
})

test_that("timeVariation$main.plot is a patchwork object", {
  expect_true(inherits(tv$main.plot, "patchwork"))
})

# --- Panel content -----------------------------------------------------------

test_that("timeVariation hour panel has 24 x levels", {
  expect_equal(length(unique(stats::na.omit(tv$data$hour$x))), 24L)
})

test_that("timeVariation month panel has 12 x levels", {
  expect_equal(dplyr::n_distinct(stats::na.omit(tv$data$month$x)), 12L)
})

test_that("timeVariation mid is between min and max in all panels", {
  for (panel in c("hour", "month", "weekday")) {
    d <- tv$data[[panel]]
    ok <- !is.na(d$mid) & !is.na(d$min) & !is.na(d$max)
    expect_true(all(d$mid[ok] >= d$min[ok]), label = paste(panel, "mid >= min"))
    expect_true(all(d$mid[ok] <= d$max[ok]), label = paste(panel, "mid <= max"))
  }
})

# --- Custom panels -----------------------------------------------------------

test_that("custom panels argument produces output keyed by those panel names", {
  tv_custom <- timeVariation(
    dat1,
    pollutant = "nox",
    panels = c("hour", "month"),
    B = 20,
    plot = FALSE
  )
  expect_true(all(c("hour", "month") %in% names(tv_custom$data)))
  expect_false("weekday" %in% names(tv_custom$data))
})

# --- input validation --------------------------------------------------------

test_that("timeVariation errors with more than one type", {
  expect_error(
    timeVariation(
      dat,
      pollutant = "nox",
      type = c("season", "year"),
      B = 20,
      plot = FALSE
    ),
    regexp = "type"
  )
})

test_that("type = 'wd' uses the compass facet layout", {
  out <- suppressWarnings(
    variationPlot(dat, pollutant = "nox", type = "wd", plot = FALSE)
  )
  expect_s3_class(out$plot$facet, "FacetWinddir")

  out <- variationPlot(dat, pollutant = "nox", type = "season", plot = FALSE)
  expect_false(inherits(out$plot$facet, "FacetWinddir"))
})

# --- windflow ----------------------------------------------------------------

test_that("windflow adds vector-averaged ws/wd and an arrow layer", {
  out <- variationPlot(
    dat1,
    pollutant = "nox",
    x = "hour",
    B = 20,
    windflow = TRUE,
    plot = FALSE
  )
  expect_true(all(c("ws", "wd") %in% names(out$data)))
  expect_true(any(vapply(
    out$plot$layers,
    \(l) inherits(l$geom, "GeomWindflow"),
    logical(1)
  )))

  # check the vector average against a manual calculation for one hour
  h0 <- dplyr::filter(dat1, as.numeric(format(date, "%H")) == 0)
  u <- mean(-h0$ws * sin(h0$wd * pi / 180), na.rm = TRUE)
  v <- mean(-h0$ws * cos(h0$wd * pi / 180), na.rm = TRUE)
  res <- dplyr::filter(out$data, x == 0)
  expect_equal(res$ws, sqrt(u^2 + v^2))
  expect_equal(res$wd, (atan2(-u, -v) * 180 / pi) %% 360)
})

test_that("windflow is off by default and calculated per group", {
  out <- variationPlot(dat1, pollutant = "nox", B = 20, plot = FALSE)
  expect_false(any(c("ws", "wd") %in% names(out$data)))

  split <- splitByDate(dat, dates = "1/2/2003", labels = c("a", "b"))
  out <- variationPlot(
    split,
    pollutant = "nox",
    x = "hour",
    group = "split.by",
    B = 20,
    windflow = TRUE,
    plot = FALSE
  )
  wind <- dplyr::distinct(out$data, x, group, ws)
  expect_false(isTRUE(all.equal(
    wind$ws[wind$group == "a"],
    wind$ws[wind$group == "b"]
  )))
})

test_that("windflow works when ws is also a pollutant, and in timeVariation", {
  out <- variationPlot(
    dat1,
    pollutant = c("ws", "nox"),
    B = 20,
    windflow = TRUE,
    plot = FALSE
  )
  expect_setequal(levels(out$data$group), c("ws", "nox"))

  tv_wf <- timeVariation(
    dat1,
    pollutant = "nox",
    B = 20,
    windflow = TRUE,
    plot = FALSE
  )
  panels <- setdiff(names(tv_wf$data), "subsets")
  expect_true(all(vapply(
    tv_wf$data[panels],
    \(d) all(c("ws", "wd") %in% names(d)),
    logical(1)
  )))
})

# --- wd as a pollutant -------------------------------------------------------

test_that("wd can be a pollutant and is vector-averaged", {
  for (p in list("wd", c("nox", "wd"), c("wd", "ws", "nox"))) {
    out <- variationPlot(dat1, pollutant = p, x = "hour", B = 20, plot = FALSE)
    expect_equal(levels(out$data$group), p)
  }

  out <- variationPlot(dat1, pollutant = "wd", x = "hour", B = 20, plot = FALSE)
  h0 <- dplyr::filter(dat1, format(date, "%H") == "00")
  u <- mean(sin(h0$wd * pi / 180), na.rm = TRUE)
  v <- mean(cos(h0$wd * pi / 180), na.rm = TRUE)
  expect_equal(
    dplyr::filter(out$data, x == 0)$mid,
    (atan2(u, v) * 180 / pi) %% 360
  )
})

test_that("wd is vector-averaged when used with group", {
  wd_dat <- data.frame(
    date = as.POSIXct("2024-01-01", tz = "GMT") + 3600 * (0:47),
    wd = rep(c(350, 10), 24),
    g = rep(c("p", "q"), each = 24)
  )
  for (st in c("mean", "median")) {
    out <- variationPlot(
      wd_dat,
      pollutant = "wd",
      x = "weekday",
      group = "g",
      statistic = st,
      ci = FALSE,
      B = 5,
      plot = FALSE
    )
    # one value per weekday/group, all north (an arithmetic mean gives 180)
    res <- dplyr::filter(out$data, !is.na(mid))
    expect_equal(dplyr::n_distinct(res$group), 2)
    expect_true(all(abs((res$mid + 180) %% 360 - 180) < 1e-6))
    expect_false(any(grepl("_u$|_v$|wd_component", names(out$data))))
  }
})

test_that("difference with wd gives a clear error", {
  split <- splitByDate(dat1, dates = "15/1/2003", labels = c("a", "b"))
  expect_error(
    variationPlot(dat1, pollutant = c("wd", "nox"), difference = TRUE, plot = FALSE),
    "difference"
  )
  expect_error(
    variationPlot(
      split,
      pollutant = "wd",
      group = "split.by",
      difference = TRUE,
      plot = FALSE
    ),
    "difference"
  )
  expect_error(
    timeVariation(dat1, pollutant = c("wd", "nox"), difference = TRUE, plot = FALSE),
    "difference"
  )
})
