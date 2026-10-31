# TheilSen tests — plot = FALSE throughout
# Bootstrap simulations make this moderately slow; skip on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Three years gives enough monthly data for stable Theil-Sen estimates
dat <- selectByDate(mydata, year = 2001:2003)

# Shared result — default settings
ts_res <- TheilSen(dat, pollutant = "nox", plot = FALSE, silent = TRUE)

# =============================================================================
# Return value structure
# =============================================================================

test_that("TheilSen returns an openair object with expected components", {
  expect_s3_class(ts_res, "openair")
  expect_named(ts_res, c("plot", "data", "call"))
  expect_s3_class(ts_res$plot, "ggplot")
})

test_that("$data is a list with main.data and res2 elements", {
  expect_type(ts_res$data, "list")
  expect_true(all(c("main.data", "res2") %in% names(ts_res$data)))
})

test_that("main.data has date, conc, slope, lower, upper columns", {
  expect_true(all(
    c("date", "conc", "slope", "lower", "upper") %in%
      names(ts_res$data$main.data)
  ))
})

test_that("res2 has slope, lower, upper, p.stars columns", {
  expect_true(all(
    c("slope", "lower", "upper", "p.stars") %in%
      names(ts_res$data$res2)
  ))
})

# =============================================================================
# Statistical values
# =============================================================================

test_that("res2 has exactly one row for type = 'default'", {
  expect_equal(nrow(ts_res$data$res2), 1L)
})

test_that("lower <= slope <= upper (confidence interval wraps the estimate)", {
  r <- ts_res$data$res2
  expect_lte(r$lower, r$slope)
  expect_lte(r$slope, r$upper)
})

test_that("p.stars is one of the expected significance codes", {
  valid <- c("***", "**", "*", "+", "")
  expect_true(all(ts_res$data$res2$p.stars %in% valid))
})

test_that("main.data has one row per monthly average", {
  monthly <- timeAverage(dat, avg.time = "month", progress = FALSE)
  expect_equal(nrow(ts_res$data$main.data), nrow(monthly))
})

# =============================================================================
# avg.time
# =============================================================================

test_that("avg.time = 'year' produces fewer rows in main.data than 'month'", {
  ts_yr <- TheilSen(
    dat,
    pollutant = "nox",
    avg.time = "year",
    plot = FALSE,
    silent = TRUE
  )
  expect_lt(nrow(ts_yr$data$main.data), nrow(ts_res$data$main.data))
})

# =============================================================================
# slope.percent
# =============================================================================

test_that("slope.percent = TRUE adds slope.percent to res2", {
  ts_pct <- TheilSen(
    dat,
    pollutant = "nox",
    slope.percent = TRUE,
    plot = FALSE,
    silent = TRUE
  )
  expect_true("slope.percent" %in% names(ts_pct$data$res2))
})

# =============================================================================
# type conditioning
# =============================================================================

test_that("type = 'season' produces four rows in res2", {
  ts_s <- TheilSen(
    dat,
    pollutant = "nox",
    type = "season",
    plot = FALSE,
    silent = TRUE
  )
  expect_equal(nrow(ts_s$data$res2), 4L)
  expect_true("season" %in% names(ts_s$data$res2))
})

# =============================================================================
# Input validation
# =============================================================================

test_that("invalid avg.time raises an error", {
  expect_error(
    TheilSen(
      dat,
      pollutant = "nox",
      avg.time = "week",
      plot = FALSE,
      silent = TRUE
    ),
    regexp = "avg.time"
  )
})

# =============================================================================
# group
# =============================================================================

ts_grp <- TheilSen(
  dat,
  pollutant = "nox",
  group = "weekend",
  plot = FALSE,
  silent = TRUE
)

test_that("group fits one trend per group", {
  expect_equal(nrow(ts_grp$data$res2), 2L)
  expect_true("weekend" %in% names(ts_grp$data$res2))
  expect_true("weekend" %in% names(ts_grp$data$main.data))
})

test_that("group trends are coloured by group and labels are stacked", {
  ld <- ggplot2::layer_data(ts_grp$plot, 2)
  expect_equal(dplyr::n_distinct(ld$colour), 2L)

  txt <- ggplot2::get_layer_data(ts_grp$plot, length(ts_grp$plot$layers))
  expect_equal(sort(txt$vjust), c(1, 2.5))
})

test_that("group works alongside type", {
  ts_tg <- TheilSen(
    dat,
    pollutant = "nox",
    type = "season",
    group = "weekend",
    plot = FALSE,
    silent = TRUE
  )
  expect_equal(nrow(ts_tg$data$res2), 8L)
})

test_that("group cannot also be used in type", {
  expect_error(
    TheilSen(
      dat,
      pollutant = "nox",
      type = "weekend",
      group = "weekend",
      plot = FALSE,
      silent = TRUE
    ),
    regexp = "group"
  )
})

# =============================================================================
# Colours
# =============================================================================

test_that("cols sets the colour of the data and trend", {
  ts_col <- TheilSen(
    dat,
    pollutant = "nox",
    cols = "blue",
    plot = FALSE,
    silent = TRUE
  )
  expect_equal(ggplot2::layer_data(ts_col$plot, 2)$colour[1], "#0000FFFF")
})

test_that("deprecated colour arguments warn, and data.col maps to cols", {
  expect_warning(
    ts_dep <- TheilSen(
      dat,
      pollutant = "nox",
      data.col = "blue",
      plot = FALSE,
      silent = TRUE
    ),
    regexp = "data.col"
  )
  expect_equal(ggplot2::layer_data(ts_dep$plot, 2)$colour[1], "#0000FFFF")

  expect_warning(
    TheilSen(
      dat,
      pollutant = "nox",
      text.col = "black",
      plot = FALSE,
      silent = TRUE
    ),
    regexp = "deprecated"
  )
})

test_that("trend lines span the data period of each group", {
  fit <- build_theilsen_fit(
    ts_grp$data$main.data,
    ts_grp$data$res2,
    c("default", "weekend")
  )
  main <- dplyr::filter(ts_grp$data$main.data, !is.na(.data$conc))
  expect_equal(nrow(fit), 4L)
  expect_equal(min(fit$date), min(main$date))
  expect_equal(max(fit$date), max(main$date))
})

test_that("trend labels are prefixed with the group name only when grouped", {
  n <- length(ts_grp$plot$layers)
  labs <- ggplot2::layer_data(ts_grp$plot, n)$label
  expect_true(all(grepl("^bold\\(paste\\(.*(weekday|weekend).*': '", labs)))
  # labels must be valid plotmath
  expect_no_error(lapply(labs, \(x) parse(text = x)))

  n <- length(ts_res$plot$layers)
  lab <- ggplot2::layer_data(ts_res$plot, n)$label
  expect_false(grepl("': '", lab))
  expect_no_error(parse(text = lab))
})

# =============================================================================
# trend and ci
# =============================================================================

test_that("trend options are merged with defaults and recycled", {
  expect_equal(
    resolve_theilsen_trend(list(linewidth = 2)),
    list(linetype = c(1, 5), linewidth = c(2, 2), alpha = c(1, 1))
  )
  expect_equal(
    resolve_theilsen_trend(list())$linewidth,
    c(1.25, 0.5)
  )
})

test_that("legacy lattice trend names are mapped, and col warns", {
  expect_warning(
    out <- resolve_theilsen_trend(
      list(lty = c(2, 3), lwd = c(4, 2), col = c("red", "red"))
    ),
    regexp = "col"
  )
  expect_equal(out$linetype, c(2, 3))
  expect_equal(out$linewidth, c(2, 1))
})

test_that("trend must be a named list", {
  expect_error(resolve_theilsen_trend(c(1, 2)), regexp = "named list")
})

test_that("trend controls the trend and confidence lines", {
  ts_tr <- TheilSen(
    dat,
    pollutant = "nox",
    trend = list(linetype = c(2, 3), linewidth = c(3, 0.25)),
    plot = FALSE,
    silent = TRUE
  )
  # layers: data line, data points, imputed points, upper, lower, trend
  upper <- ggplot2::layer_data(ts_tr$plot, 4)
  main <- ggplot2::layer_data(ts_tr$plot, 6)
  expect_equal(unique(main$linewidth), 3)
  expect_equal(unique(main$linetype), 2)
  expect_equal(unique(upper$linewidth), 0.25)
  expect_equal(unique(upper$linetype), 3)
})

test_that("ci = FALSE removes the confidence lines", {
  ts_noci <- TheilSen(
    dat,
    pollutant = "nox",
    ci = FALSE,
    plot = FALSE,
    silent = TRUE
  )
  expect_equal(
    length(ts_noci$plot$layers),
    length(ts_res$plot$layers) - 2L
  )
})
