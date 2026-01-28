#' Plot an air quality timeseries chart with Theil-Sen slope estimates
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   The [plot_trend_theilsen()] function provides a collection of functions to
#'   analyse trends in air pollution data. It is flexible in the sense that it
#'   can be applied to data in many ways, e.g., by day of the week, hour of day
#'   and wind direction. This flexibility makes it much easier to draw
#'   inferences from data e.g. why is there a strong downward trend in
#'   concentration from one wind sector and not another, or why trends on one
#'   day of the week or a certain time of day are unexpected.
#'
#'   For data that are strongly seasonal, perhaps from a background site, or a
#'   pollutant such as ozone, it will be important to deseasonalise the data
#'   (using the option `deseason = TRUE`. Similarly, for data that increase,
#'   then decrease, or show sharp changes it may be better to use
#'   [plot_trend_smooth()].
#'
#'   A minimum of 6 points are required for trend estimates to be made.
#'
#'   Note that the symbols shown next to each trend estimate relate to how
#'   statistically significant the trend estimate is: p $<$ 0.001 =
#' ***, p $<$ 0.01 = **, p $<$ 0.05 = * and p $<$ 0.1 = $+$.
#'
#'   Some of the code used in [TheilSen()] is based on that from Rand Wilcox.
#'   This mostly relates to the Theil-Sen slope estimates and uncertainties.
#'   Further modifications have been made to take account of correlated data
#'   based on Kunsch (1989). The basic function has been adapted to take account
#'   of auto-correlated data using block bootstrap simulations if `autocor =
#'   TRUE` (Kunsch, 1989). We follow the suggestion of Kunsch (1989) of setting
#'   the block length to n(1/3) where n is the length of the time series.
#'
#' @inheritParams shared_ggplot_params
#' @inheritParams timeAverage
#'
#' @param deseason Should the data be de-deasonalized first? If `TRUE` the
#'   function `stl` is used (seasonal trend decomposition using loess). Note
#'   that if `TRUE` missing data are first imputed using a Kalman filter and
#'   Kalman smooth.
#'
#' @param alpha For the confidence interval calculations of the slope. The
#'   default is 0.05. To show 99\% confidence intervals for the value of the
#'   trend, choose alpha = 0.01 etc.
#'
#' @param slope_unit The text shown for the slope (default is
#'   \sQuote{units/year}).
#'
#' @param cols The colour scheme to use in the plot. Passed to [openColours()].
#'   Likely more usefully, three colours can be defined in a single vector. For
#'   example, when `cols = c("blue", "red", "green")`, the data will be coloured
#'   blue, the trend lines red, and the annotations green.
#'
#' @param autocor Should autocorrelation be considered in the trend uncertainty
#'   estimates? The default is `FALSE`. Generally, accounting for
#'   autocorrelation increases the uncertainty of the trend estimate ---
#'   sometimes by a large amount.
#'
#' @inheritSection shared_ggplot_params Controlling scales
#' @inheritSection shared_ggplot_params Conditioning with `type`
#'
#' @author David Carslaw
#' @author Jack Davison
#' @author Rand Wilcox (trend code)
#'
#' @family ggplot2 time series and trend functions
#'
#' @export
#'
#' @references
#'
#' Helsel, D., Hirsch, R., 2002. Statistical methods in water resources. US
#' Geological Survey. Note that this is a very good resource for statistics as
#' applied to environmental data.
#'
#' Hirsch, R. M., Slack, J. R., Smith, R. A., 1982. Techniques of trend analysis
#' for monthly water-quality data. Water Resources Research 18 (1), 107-121.
#'
#' Kunsch, H. R., 1989. The jackknife and the bootstrap for general stationary
#' observations. Annals of Statistics 17 (3), 1217-1241.
#'
#' Sen, P. K., 1968. Estimates of regression coefficient based on Kendall's tau.
#' Journal of the American Statistical Association 63(324).
#'
#' Theil, H., 1950. A rank invariant method of linear and polynomial regression
#' analysis, i, ii, iii. Proceedings of the Koninklijke Nederlandse Akademie
#' Wetenschappen, Series A - Mathematical Sciences 53, 386-392, 521-525,
#' 1397-1412.
#'
#' See also several of the Air Quality Expert Group (AQEG) reports for the use
#' of similar tests applied to UK/European air quality data.
#'
#' @examples
#' # trend plot for nox
#' plot_trend_theilsen(mydata, pollutant = "nox")
#'
#' \dontrun{
#' # trend plot for ozone with p=0.01 i.e. uncertainty in slope shown at
#' # 99 % confidence interval
#' plot_trend_theilsen(mydata, pollutant = "o3", alpha = 0.01)
#'
#' # trend plot by each of 8 wind sectors
#' plot_trend_theilsen(mydata, pollutant = "o3", type = "wd")
#'
#' # and for a subset of data (from year 2000 onwards)
#' plot_trend_theilsen(selectByDate(mydata, year = 2000:2005), pollutant = "o3")
#' }
plot_trend_theilsen <- function(
  data,
  pollutant = "nox",
  type = NULL,
  avg.time = "month",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  alpha = 0.05,
  deseason = FALSE,
  autocor = FALSE,
  scale_x = openair::scale_opts(),
  scale_y = openair::scale_opts(),
  cols = "tol",
  slope_unit = NULL,
  auto_text = TRUE,
  facet_opts = openair::facet_opts(),
  plot = TRUE,
  ...
) {
  type <- type %||% "default"
  scale_x <- resolve_scale_opts(scale_x)
  scale_y <- resolve_scale_opts(scale_y)

  # find time interval
  # need this because if user has a data capture threshold, need to know
  # original time interval
  # Working this out for unique dates for all data is what is done here.
  # More reliable than trying to work it out after conditioning where there
  # may be too few data for the calculation to be reliable
  interval <- find.time.interval(data$date)

  # equivalent number of days, used to refine interval for month/year
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
    24 /
    3600

  # better interval, most common interval in a year
  if (days == 31) {
    interval <- "month"
  }
  if (days %in% c(365, 366)) {
    interval <- "year"
  }

  # data checks
  data <- checkPrep(data, c("date", pollutant), type, remove.calm = FALSE)

  # cutData depending on type
  data <- cutData(data, type, ...)

  # for overall data and graph plotting
  start.year <- startYear(data$date)
  end.year <- endYear(data$date)

  # time average to requested interval
  data <- suppressWarnings(
    timeAverage(
      data,
      type = type,
      avg.time = avg.time,
      statistic = statistic,
      percentile = percentile,
      data.thresh = data.thresh,
      interval = interval,
      progress = FALSE
    )
  )

  # timeAverage drops type if default
  if ("default" %in% type) {
    data$default <- "default"
  }

  # map process condition over data
  split.data <-
    mapType(
      data,
      type = type,
      fun = \(df) {
        process_theilsen_cond(
          df,
          pollutant = pollutant,
          avg.time = avg.time,
          deseason = deseason,
          alpha = alpha,
          autocor = autocor
        )
      },
      .include_default = TRUE
    )

  if (nrow(split.data) < 2) {
    cli::cli_abort(
      "Insufficient data for plotting; please review {.arg data}, {.arg avg.time} and {.arg type}."
    )
  }

  # calculate slopes etc.
  split.data <- dplyr::mutate(
    split.data,
    slope = 365 * .data$b,
    intercept = .data$a,
    intercept.lower = .data$lower.a,
    intercept.upper = .data$upper.a,
    lower = 365 * .data$lower.b,
    upper = 365 * .data$upper.b
  )

  # aggregated results
  res2 <- dplyr::summarise(
    split.data,
    dplyr::across(
      dplyr::everything(),
      ~ mean(.x, na.rm = TRUE)
    ),
    .by = dplyr::all_of(c(type, "p.stars"))
  ) |>
    dplyr::filter(!is.na(.data$conc))

  # calculate percentage changes in slope and uncertainties need
  # start and end dates (in days) to work out concentrations at those
  # points percentage change defined as 100.(C.end/C.start -1) /
  # duration
  start <- dplyr::slice_head(split.data, n = 1, by = dplyr::all_of(type))
  end <- dplyr::slice_tail(split.data, n = 1, by = dplyr::all_of(type))

  # get table of percentage changes
  percent.change <- dplyr::left_join(
    start,
    end,
    by = type,
    suffix = c(".start", ".end")
  ) |>
    dplyr::mutate(
      slope.percent = 100 *
        365 *
        ((.data$slope.start *
          as.numeric(.data$date.end) /
          365 +
          .data$intercept.start) /
          (.data$slope.start *
            as.numeric(.data$date.start) /
            365 +
            .data$intercept.start) -
          1) /
        (as.numeric(.data$date.end) - as.numeric(.data$date.start)),
      lower.percent = 100 *
        365 *
        ((.data$lower.start *
          as.numeric(.data$date.end) /
          365 +
          .data$intercept.lower.start) /
          (.data$lower.start *
            as.numeric(.data$date.start) /
            365 +
            .data$intercept.lower.start) -
          1) /
        (as.numeric(.data$date.end) - as.numeric(.data$date.start)),
      upper.percent = 100 *
        365 *
        ((.data$upper.start *
          as.numeric(.data$date.end) /
          365 +
          .data$intercept.upper.start) /
          (.data$upper.start *
            as.numeric(.data$date.start) /
            365 +
            .data$intercept.upper.start) -
          1) /
        (as.numeric(.data$date.end) - as.numeric(.data$date.start))
    ) |>
    dplyr::select(
      dplyr::all_of(c(type, "slope.percent", "lower.percent", "upper.percent"))
    )

  # bind to original data
  split.data <- dplyr::left_join(split.data, percent.change, by = type)
  res2 <- dplyr::left_join(res2, percent.change, by = type)

  # label for plot
  res2$label <- paste0(
    signif(res2$slope, 3),
    " [",
    signif(res2$lower, 3),
    ", ",
    signif(res2$upper, 3),
    "] ",
    slope_unit %||% "units/year",
    " ",
    res2$p.stars
  )

  # determine facet
  facet_fun <- get_facet_fun(
    type,
    facet_opts = facet_opts,
    auto_text = auto_text
  )

  # construct plot
  plt <-
    dplyr::tibble(split.data) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$conc, x = .data$date, color = "Data"),
      linewidth = 0.25
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$conc, x = .data$date, color = "Data"),
      shape = 1
    ) +
    theme_oa_classic() +
    ggplot2::geom_abline(
      data = res2,
      ggplot2::aes(slope = .data$b, intercept = .data$a, color = "TheilSen"),
      linewidth = 0.5
    ) +
    ggplot2::geom_abline(
      data = res2,
      ggplot2::aes(
        slope = .data$upper.b,
        intercept = .data$upper.a,
        color = "TheilSen"
      ),
      linewidth = 0.5,
      lty = 5
    ) +
    ggplot2::geom_abline(
      data = res2,
      ggplot2::aes(
        slope = .data$lower.b,
        intercept = .data$lower.a,
        color = "TheilSen"
      ),
      linewidth = 0.5,
      lty = 5
    ) +
    ggplot2::geom_text(
      data = res2,
      x = I(0.5),
      y = I(0.98),
      ggplot2::aes(label = .data$label, color = "Label"),
      vjust = 1,
      hjust = 0.5,
      fontface = "bold",
      size = 3
    ) +
    ggplot2::guides(color = ggplot2::guide_none()) +
    ggplot2::labs(
      x = NULL,
      y = label_openair(pollutant, auto_text = auto_text)
    ) +
    ggplot2::scale_y_continuous(
      breaks = scale_y$breaks,
      labels = scale_y$labels,
      limits = scale_y$limits,
      transform = scale_y$transform,
      position = scale_y$position %||% "left",
      sec.axis = scale_y$sec.axis
    ) +
    ggplot2::scale_x_date(
      breaks = scale_x$breaks,
      labels = scale_x$labels,
      limits = scale_x$limits,
      date_breaks = scale_x$date_breaks,
      date_labels = scale_x$date_labels,
      position = scale_x$position %||% "bottom",
      sec.axis = scale_x$sec.axis
    ) +
    ggplot2::scale_color_manual(
      breaks = c("Data", "TheilSen", "Label"),
      values = openair::openColours(
        scheme = cols,
        n = 3
      ),
      label = \(x) label_openair(x, auto_text = auto_text),
      drop = FALSE
    ) +
    facet_fun

  # output #
  if (plot) {
    return(plt)
  } else {
    return(
      list(
        data = dplyr::tibble(split.data),
        trend = dplyr::tibble(res2)
      )
    )
  }
}


#' Theil-Sen slope estimates and tests for trend.
#'
#' The [TheilSen()] function provides a collection of functions to analyse
#' trends in air pollution data. The [TheilSen()] function is flexible in the
#' sense that it can be applied to data in many ways e.g. by day of the week,
#' hour of day and wind direction. This flexibility makes it much easier to draw
#' inferences from data e.g. why is there a strong downward trend in
#' concentration from one wind sector and not another, or why trends on one day
#' of the week or a certain time of day are unexpected.
#'
#' For data that are strongly seasonal, perhaps from a background site, or a
#' pollutant such as ozone, it will be important to deseasonalise the data
#' (using the option `deseason = TRUE`.Similarly, for data that increase, then
#' decrease, or show sharp changes it may be better to use [smoothTrend()].
#'
#' A minimum of 6 points are required for trend estimates to be made.
#'
#' Note! that since version 0.5-11 openair uses Theil-Sen to derive the p values
#' also for the slope. This is to ensure there is consistency between the
#' calculated p value and other trend parameters i.e. slope estimates and
#' uncertainties. The p value and all uncertainties are calculated through
#' bootstrap simulations.
#'
#' Note that the symbols shown next to each trend estimate relate to how
#' statistically significant the trend estimate is: p $<$ 0.001 =
#' ***, p $<$ 0.01 = **, p $<$ 0.05 = * and p $<$ 0.1 = $+$.
#'
#' Some of the code used in [TheilSen()] is based on that from Rand Wilcox. This
#' mostly relates to the Theil-Sen slope estimates and uncertainties. Further
#' modifications have been made to take account of correlated data based on
#' Kunsch (1989). The basic function has been adapted to take account of
#' auto-correlated data using block bootstrap simulations if `autocor = TRUE`
#' (Kunsch, 1989). We follow the suggestion of Kunsch (1989) of setting the
#' block length to n(1/3) where n is the length of the time series.
#'
#' The slope estimate and confidence intervals in the slope are plotted and
#' numerical information presented.
#'
#' @inheritParams timePlot
#'
#' @aliases TheilSen
#' @param pollutant The parameter for which a trend test is required. Mandatory.
#' @param deseason Should the data be de-deasonalized first? If `TRUE` the
#'   function `stl` is used (seasonal trend decomposition using loess). Note
#'   that if `TRUE` missing data are first imputed using a Kalman filter and
#'   Kalman smooth.
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in [cutData()],
#'   e.g., `"season"`, `"year"`, `"weekday"` and so on. For example, `type =
#'   "season"` will produce four plots --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data frame.
#'   If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Type can be up length two e.g. `type = c("season", "weekday")` will produce
#'   a 2x2 plot split by season and day of the week. Note, when two types are
#'   provided the first forms the columns and the second the rows.
#' @param avg.time Can be `"month"` (the default), `"season"` or `"year"`.
#'   Determines the time over which data should be averaged. Note that for
#'   `"year"`, six or more years are required. For `"season"` the data are split
#'   up into spring: March, April, May etc. Note that December is considered as
#'   belonging to winter of the following year.
#' @param alpha For the confidence interval calculations of the slope. The
#'   default is 0.05. To show 99\% confidence intervals for the value of the
#'   trend, choose alpha = 0.01 etc.
#' @param dec.place The number of decimal places to display the trend estimate
#'   at. The default is 2.
#' @param lab.frac Fraction along the y-axis that the trend information should
#'   be printed at, default 0.99.
#' @param lab.cex Size of text for trend information.
#' @param data.col Colour name for the data
#' @param trend list containing information on the line width, line type and
#'   line colour for the main trend line and confidence intervals respectively.
#' @param text.col Colour name for the slope/uncertainty numeric estimates
#' @param slope.text The text shown for the slope (default is
#'   \sQuote{units/year}).
#' @param cols Predefined colour scheme, currently only enabled for
#'   `"greyscale"`.
#' @param shade The colour used for marking alternate years. Use `"white"` or
#'   `"transparent"` to remove shading.
#' @param autocor Should autocorrelation be considered in the trend uncertainty
#'   estimates? The default is `FALSE`. Generally, accounting for
#'   autocorrelation increases the uncertainty of the trend estimate ---
#'   sometimes by a large amount.
#' @param slope.percent Should the slope and the slope uncertainties be
#'   expressed as a percentage change per year? The default is `FALSE` and the
#'   slope is expressed as an average units/year change e.g. ppb. Percentage
#'   changes can often be confusing and should be clearly defined. Here the
#'   percentage change is expressed as 100 * (C.end/C.start - 1) / (end.year -
#'   start.year). Where C.start is the concentration at the start date and C.end
#'   is the concentration at the end date.
#'
#'   For `avg.time = "year"` (end.year - start.year) will be the total number of
#'   years - 1. For example, given a concentration in year 1 of 100 units and a
#'   percentage reduction of 5%/yr, after 5 years there will be 75 units but the
#'   actual time span will be 6 years i.e. year 1 is used as a reference year.
#'   Things are slightly different for monthly values e.g. `avg.time = "month"`,
#'   which will use the total number of months as a basis of the time span and
#'   is therefore able to deal with partial years. There can be slight
#'   differences in the %/yr trend estimate therefore, depending on whether
#'   monthly or annual values are considered.
#' @param silent When `FALSE` the function will give updates on trend-fitting
#'   progress.
#' @param ... Other graphical parameters passed onto [cutData()] and
#'   [lattice::xyplot()]. For example, [TheilSen()] passes the option
#'   `hemisphere = "southern"` on to [cutData()] to provide southern (rather
#'   than default northern) hemisphere handling of `type = "season"`. Similarly,
#'   common axis and title labelling options (such as `xlab`, `ylab`, `main`)
#'   are passed to [lattice::xyplot()] via [quickText()] to handle routine
#'   formatting.
#' @export TheilSen
#' @return an [openair][openair-package] object. The `data` component of the
#'   `TheilSen` output includes two subsets: `main.data`, the monthly data
#'   `res2` the trend statistics. For `output <- TheilSen(mydata, "nox")`, these
#'   can be extracted as `object$data$main.data` and `object$data$res2`,
#'   respectively. Note: In the case of the intercept, it is assumed the y-axis
#'   crosses the x-axis on 1/1/1970.
#' @author David Carslaw with some trend code from Rand Wilcox
#' @family time series and trend functions
#' @references
#'
#' Helsel, D., Hirsch, R., 2002. Statistical methods in water resources. US
#' Geological Survey. Note that this is a very good resource for statistics as
#' applied to environmental data.
#'
#' Hirsch, R. M., Slack, J. R., Smith, R. A., 1982. Techniques of trend analysis
#' for monthly water-quality data. Water Resources Research 18 (1), 107-121.
#'
#' Kunsch, H. R., 1989. The jackknife and the bootstrap for general stationary
#' observations. Annals of Statistics 17 (3), 1217-1241.
#'
#' Sen, P. K., 1968. Estimates of regression coefficient based on Kendall's tau.
#' Journal of the American Statistical Association 63(324).
#'
#' Theil, H., 1950. A rank invariant method of linear and polynomial regression
#' analysis, i, ii, iii. Proceedings of the Koninklijke Nederlandse Akademie
#' Wetenschappen, Series A - Mathematical Sciences 53, 386-392, 521-525,
#' 1397-1412.
#'
#' See also several of the Air Quality Expert Group (AQEG) reports for
#' the use of similar tests applied to UK/European air quality data.
#'
#' @examples
#' # trend plot for nox
#' TheilSen(mydata, pollutant = "nox")
#'
#' \dontrun{
#' # trend plot for ozone with p=0.01 i.e. uncertainty in slope shown at
#' # 99 % confidence interval
#' TheilSen(mydata, pollutant = "o3", ylab = "o3 (ppb)", alpha = 0.01)
#'
#' # trend plot by each of 8 wind sectors
#' TheilSen(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")
#'
#' # and for a subset of data (from year 2000 onwards)
#' TheilSen(selectByDate(mydata, year = 2000:2005), pollutant = "o3", ylab = "o3 (ppb)")
#' }
TheilSen <- function(
  mydata,
  pollutant = "nox",
  deseason = FALSE,
  type = "default",
  avg.time = "month",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  alpha = 0.05,
  dec.place = 2,
  lab.frac = 0.99,
  lab.cex = 0.8,
  data.col = "cornflowerblue",
  trend = list(lty = c(1, 5), lwd = c(2, 1), col = c("red", "red")),
  text.col = "darkgreen",
  slope.text = NULL,
  cols = NULL,
  shade = "grey95",
  autocor = FALSE,
  slope.percent = FALSE,
  x.relation = "same",
  y.relation = "same",
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  silent = FALSE,
  ...
) {
  # avg.time limited to yera/month/season
  rlang::arg_match(avg.time, c("year", "month", "season"))

  # set graphics
  current.font <- trellis.par.get("fontsize")

  # reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    # other local colours
    trend$col <- c("black", "black")
    data.col <- "darkgrey"
    text.col <- "black"
  } else {
    data.col <- data.col
    text.col <- text.col
  }

  # extra.args setup
  extra.args <- list(...)

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab %||% "date", auto.text)
  extra.args$ylab <- quickText(extra.args$ylab %||% pollutant, auto.text)
  extra.args$main <- quickText(extra.args$main %||% "", auto.text)

  extra.args$layout <- extra.args$layout %||% NULL

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  xlim <- extra.args$xlim %||% NULL

  # find time interval
  # need this because if user has a data capture threshold, need to know
  # original time interval
  # Working this out for unique dates for all data is what is done here.
  # More reliable than trying to work it out after conditioning where there
  # may be too few data for the calculation to be reliable
  interval <- find.time.interval(mydata$date)

  # equivalent number of days, used to refine interval for month/year
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
    24 /
    3600

  # better interval, most common interval in a year
  if (days == 31) {
    interval <- "month"
  }
  if (days %in% c(365, 366)) {
    interval <- "year"
  }

  # data checks
  vars <- c("date", pollutant)
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # date formatting for plot
  breaks <- dateBreaks(mydata$date, date.breaks)
  date.at <- as_date(breaks$major)
  formats <- date.format %||% breaks$format

  # cutData depending on type
  mydata <- cutData(mydata, type, ...)

  # for overall data and graph plotting
  start.year <- startYear(mydata$date)
  end.year <- endYear(mydata$date)

  # time average to requested interval
  mydata <- suppressWarnings(
    timeAverage(
      mydata,
      type = type,
      avg.time = avg.time,
      statistic = statistic,
      percentile = percentile,
      data.thresh = data.thresh,
      interval = interval,
      progress = !silent
    )
  )

  # timeAverage drops type if default
  if ("default" %in% type) {
    mydata$default <- "default"
  }

  # need to work out how to use dplyr if it does not return a data frame due to too few data
  if (!silent) {
    cli::cli_inform(c("i" = "Taking bootstrap samples. Please wait."))
  }

  # map process condition over data
  split.data <-
    mapType(
      mydata,
      type = type,
      fun = \(df) {
        process_theilsen_cond(
          df,
          pollutant = pollutant,
          avg.time = avg.time,
          deseason = deseason,
          alpha = alpha,
          autocor = autocor,
          silent = silent
        )
      },
      .progress = !silent,
      .include_default = TRUE
    )

  if (nrow(split.data) < 2) {
    cli::cli_abort(
      "Insufficient data for plotting; please review {.arg mydata}, {.arg avg.time} and {.arg type}."
    )
  }

  # special wd layout
  # (type field in results.grid called type not wd)
  if (
    length(type) == 1 &&
      type[1] == "wd" &&
      is.null(extra.args$layout)
  ) {
    # re-order to make sensible layout
    # starting point code as of ManKendall
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    split.data$wd <- ordered(split.data$wd, levels = wds)
    wd.ok <-
      sapply(wds, function(x) {
        if (x %in% unique(split.data$wd)) {
          FALSE
        } else {
          TRUE
        }
      })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    split.data$wd <- factor(split.data$wd)
    extra.args$layout <- c(3, 3)
    if (!"skip" %in% names(extra.args)) {
      extra.args$skip <- skip
    }
  }

  if (!"skip" %in% names(extra.args)) {
    extra.args$skip <- FALSE
  }

  # proper names of labelling #
  strip.dat <- strip.fun(split.data, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]

  # calculate slopes etc.
  split.data <- dplyr::mutate(
    split.data,
    slope = 365 * .data$b,
    intercept = .data$a,
    intercept.lower = .data$lower.a,
    intercept.upper = .data$upper.a,
    lower = 365 * .data$lower.b,
    upper = 365 * .data$upper.b
  )

  # aggregated results
  vars <- c(type, "p.stars")
  res2 <- dplyr::summarise(
    split.data,
    dplyr::across(
      dplyr::everything(),
      ~ mean(.x, na.rm = TRUE)
    ),
    .by = dplyr::all_of(vars)
  )

  # calculate percentage changes in slope and uncertainties need
  # start and end dates (in days) to work out concentrations at those
  # points percentage change defined as 100.(C.end/C.start -1) /
  # duration

  start <- dplyr::slice_head(split.data, n = 1, by = dplyr::all_of(type))
  end <- dplyr::slice_tail(split.data, n = 1, by = dplyr::all_of(type))

  # get table of percentage changes
  percent.change <- dplyr::left_join(
    start,
    end,
    by = type,
    suffix = c(".start", ".end")
  ) |>
    dplyr::mutate(
      slope.percent = 100 *
        365 *
        ((.data$slope.start *
          as.numeric(.data$date.end) /
          365 +
          .data$intercept.start) /
          (.data$slope.start *
            as.numeric(.data$date.start) /
            365 +
            .data$intercept.start) -
          1) /
        (as.numeric(.data$date.end) - as.numeric(.data$date.start)),
      lower.percent = 100 *
        365 *
        ((.data$lower.start *
          as.numeric(.data$date.end) /
          365 +
          .data$intercept.lower.start) /
          (.data$lower.start *
            as.numeric(.data$date.start) /
            365 +
            .data$intercept.lower.start) -
          1) /
        (as.numeric(.data$date.end) - as.numeric(.data$date.start)),
      upper.percent = 100 *
        365 *
        ((.data$upper.start *
          as.numeric(.data$date.end) /
          365 +
          .data$intercept.upper.start) /
          (.data$upper.start *
            as.numeric(.data$date.start) /
            365 +
            .data$intercept.upper.start) -
          1) /
        (as.numeric(.data$date.end) - as.numeric(.data$date.start))
    ) |>
    dplyr::select(
      dplyr::all_of(c(type, "slope.percent", "lower.percent", "upper.percent"))
    )

  # bind to original data
  split.data <- dplyr::left_join(split.data, percent.change, by = type)
  res2 <- dplyr::left_join(res2, percent.change, by = type)

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("conc ~ date| ", temp, sep = ""))

  gap <- (max(split.data$date) - min(split.data$date)) / 80
  if (is.null(xlim)) {
    xlim <- range(split.data$date) + c(-1 * gap, gap)
  }

  xyplot.args <- list(
    x = myform,
    data = split.data,
    par.strip.text = list(cex = 0.8),
    as.table = TRUE,
    xlim = xlim,
    strip = strip,
    strip.left = strip.left,
    scales = list(
      x = list(
        at = date.at,
        format = formats,
        relation = x.relation
      ),
      y = list(relation = y.relation, rot = 0)
    ),
    panel = function(x, y, subscripts, ...) {
      # year shading
      panel_shade(
        split.data,
        start.year,
        end.year,
        ylim = current.panel.limits()$ylim,
        shade
      )
      panel.grid(-1, 0)

      panel.xyplot(x, y, type = "b", col = data.col, ...)

      sub.dat <- split.data[subscripts, ]

      # need some data to plot, check if enough information to show trend
      if (nrow(sub.dat) > 0 && !all(is.na(sub.dat$slope))) {
        panel.abline(
          a = sub.dat[1, "intercept"],
          b = sub.dat[1, "slope"] / 365,
          col = trend$col[1],
          lwd = trend$lwd[1],
          lty = trend$lty[1]
        )
        panel.abline(
          a = sub.dat[1, "intercept.lower"],
          b = sub.dat[1, "lower"] / 365,
          col = trend$col[2],
          lwd = trend$lwd[2],
          lty = trend$lty[2]
        )
        panel.abline(
          a = sub.dat[1, "intercept.upper"],
          b = sub.dat[1, "upper"] / 365,
          col = trend$col[2],
          lwd = trend$lwd[2],
          lty = trend$lty[2]
        )

        # for text on plot - % trend or not?
        slope <- "slope"
        lower <- "lower"
        upper <- "upper"
        units <- "units"

        if (slope.percent) {
          slope <- "slope.percent"
          lower <- "lower.percent"
          upper <- "upper.percent"
          units <- "%"
        }

        # allow user defined slope text
        if (!is.null(slope.text)) {
          slope.text <- slope.text
        } else {
          slope.text <- paste0(units, "/year")
        }

        # plot top, middle
        panel.text(
          mean(c(
            current.panel.limits()$xlim[2],
            current.panel.limits()$xlim[1]
          )),
          current.panel.limits()$ylim[1] +
            lab.frac *
              (current.panel.limits()$ylim[2] -
                current.panel.limits()$ylim[1]),
          paste(
            round(sub.dat[1, slope], dec.place),
            " ",
            "[",
            round(sub.dat[1, lower], dec.place),
            ", ",
            round(sub.dat[1, upper], dec.place),
            "] ",
            slope.text,
            " ",
            sub.dat[1, "p.stars"],
            sep = ""
          ),
          cex = lab.cex,
          adj = c(0.5, 1),
          col = text.col,
          font = 2
        )
      }
    }
  )

  # reset for extra.args
  xyplot.args <- listUpdate(xyplot.args, extra.args)

  # plot
  plt <- do.call(xyplot, xyplot.args)

  # output #

  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    } else {
      plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    }
  }

  newdata <- list(
    main.data = dplyr::tibble(split.data),
    res2 = dplyr::tibble(res2),
    subsets = c("main.data", "res2")
  )
  output <- list(
    plot = plt,
    data = newdata,
    call = match.call()
  )
  class(output) <- "openair"

  invisible(output)
}

process_theilsen_cond <- function(
  mydata,
  pollutant,
  avg.time,
  deseason,
  alpha,
  autocor,
  silent = TRUE
) {
  if (all(is.na(mydata[[pollutant]]))) {
    return(data.frame(
      b = NA,
      a = NA,
      lower.a = NA,
      upper.a = NA,
      lower.b = NA,
      upper.b = NA,
      p.stars = NA
    ))
  }

  # sometimes data have long trailing NAs, so start and end at
  # first and last data
  min.idx <- min(which(!is.na(mydata[, pollutant])))
  max.idx <- max(which(!is.na(mydata[, pollutant])))
  mydata <- mydata[min.idx:max.idx, ]

  # these subsets may have different dates to overall
  start.year <- startYear(mydata$date)
  end.year <- endYear(mydata$date)
  start.month <- startMonth(mydata$date)
  end.month <- endMonth(mydata$date)

  if (avg.time == "month") {
    mydata$date <- as_date(mydata$date)

    deseas <- mydata[[pollutant]]

    # can't deseason less than 2 years of data
    if (nrow(mydata) <= 24) {
      deseason <- FALSE
    }

    if (deseason) {
      myts <- ts(
        mydata[[pollutant]],
        start = c(start.year, start.month),
        end = c(end.year, end.month),
        frequency = 12
      )

      # fill any missing data using a Kalman filter

      if (any(is.na(myts))) {
        fit <- ts(rowSums(tsSmooth(StructTS(myts))[, -2]))
        id <- which(is.na(myts))

        myts[id] <- fit[id]
      }

      # key thing is to allow the seanonal cycle to vary, hence
      # s.window should not be "periodic"; set quite high to avoid
      # overly fitted seasonal cycle
      # robustness also makes sense for sometimes noisy data
      ssd <- stl(myts, s.window = 11, robust = TRUE, s.degree = 1)

      deseas <- ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]

      deseas <- as.vector(deseas)
    }

    all.results <- data.frame(
      date = mydata$date,
      conc = deseas,
      stringsAsFactors = FALSE
    )
    results <- na.omit(all.results)
  } else {
    # assume annual
    all.results <- data.frame(
      date = as_date(mydata$date),
      conc = mydata[[pollutant]],
      stringsAsFactors = FALSE
    )
    results <- na.omit(all.results)
  }

  # now calculate trend, uncertainties etc #
  if (nrow(results) < 6) {
    # need enough data to calculate trend, set missing if not

    results <- mutate(
      results,
      b = NA,
      a = NA,
      lower.a = NA,
      upper.a = NA,
      lower.b = NA,
      upper.b = NA,
      p.stars = NA
    )

    return(results)
  }

  MKresults <- mk_stats(
    results$date,
    results$conc,
    alpha,
    autocor,
    silent = silent
  )

  # make sure missing data are put back in for plotting
  results <- merge(all.results, MKresults, by = "date", all = TRUE)

  results
}

panel_shade <- function(
  split.data,
  start.year,
  end.year,
  ylim,
  shade = "grey95"
) {
  x1 <- as.POSIXct(
    seq(
      ISOdate(start.year - 6, 1, 1),
      ISOdate(end.year + 5, 1, 1),
      by = "2 years"
    ),
    "GMT"
  )
  x2 <- as.POSIXct(
    seq(
      ISOdate(start.year + 1 - 6, 1, 1),
      ISOdate(end.year + 5, 1, 1),
      by = "2 years"
    ),
    "GMT"
  )

  if (class(split.data$date)[1] == "Date") {
    x1 <- as_date(x1)
    x2 <- as_date(x2)
  }

  rng <- range(split.data$conc, na.rm = TRUE) # range of data
  y1 <- min(split.data$conc, na.rm = TRUE) - 0.1 * abs(rng[2] - rng[1])
  y2 <- max(split.data$conc, na.rm = TRUE) + 0.1 * abs(rng[2] - rng[1])

  # if user selects specific limits

  if (!missing(ylim)) {
    y1 <- ylim[1] - 0.1 * abs(ylim[2] - ylim[1])
    y2 <- ylim[2] + 0.1 * abs(ylim[2] - ylim[1])
  }

  sapply(seq_along(x1), function(x) {
    lpolygon(
      c(x1[x], x1[x], x2[x], x2[x]),
      c(y1, y2, y2, y1),
      col = shade,
      border = "grey95"
    )
  })
}

mk_stats <- function(x, y, alpha, autocor, silent) {
  # compute regression CIs
  estimates <- regci(
    as.numeric(x),
    y,
    alpha = alpha,
    autocor = autocor,
    pr = silent
  )$regci

  # extract p-values, assign signif stars
  p <- estimates[2, "p-value"]
  stars <- cut(
    p,
    breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    labels = c("***", "**", "*", "+", ""),
    right = FALSE,
    include.lowest = TRUE
  )

  results <-
    data.frame(
      date = x,
      a = estimates[1, 3],
      b = estimates[2, 3],
      upper.a = estimates[1, 1],
      upper.b = estimates[2, 2],
      lower.a = estimates[1, 2],
      lower.b = estimates[2, 1],
      p = p,
      p.stars = stars,
      stringsAsFactors = FALSE
    )

  results
}
