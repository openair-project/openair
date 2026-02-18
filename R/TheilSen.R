#' Tests for trends using Theil-Sen estimates
#'
#' Theil-Sen slope estimates and tests for trend. The `TheilSen` function is
#' flexible in the sense that it can be applied to data in many ways e.g. by day
#' of the week, hour of day and wind direction. This flexibility makes it much
#' easier to draw inferences from data e.g. why is there a strong downward trend
#' in concentration from one wind sector and not another, or why trends on one
#' day of the week or a certain time of day are unexpected.
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
#' Some of the code used in `TheilSen` is based on that from Rand Wilcox. This
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
#' @aliases TheilSen
#' @param mydata A data frame containing the field `date` and at least one other
#'   parameter for which a trend test is required; typically (but not
#'   necessarily) a pollutant.
#' @param pollutant The parameter for which a trend test is required. Mandatory.
#' @param deseason Should the data be de-deasonalized first? If `TRUE` the
#'   function `stl` is used (seasonal trend decomposition using loess). Note
#'   that if `TRUE` missing data are first imputed using a Kalman filter and
#'   Kalman smooth.
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in `cutData` e.g.
#'   \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so on. For example,
#'   `type = "season"` will produce four plots --- one for each season.
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
#' @param avg.time Can be \dQuote{month} (the default), \dQuote{season} or
#'   \dQuote{year}. Determines the time over which data should be averaged. Note
#'   that for \dQuote{year}, six or more years are required. For \dQuote{season}
#'   the data are split up into spring: March, April, May etc. Note that
#'   December is considered as belonging to winter of the following year.
#' @param statistic Statistic used for calculating monthly values. Default is
#'   \dQuote{mean}, but can also be \dQuote{percentile}. See `timeAverage` for
#'   more details.
#' @param percentile Single percentile value to use if `statistic =
#'   "percentile"` is chosen.
#' @param data.thresh The data capture threshold to use (%) when aggregating the
#'   data using `avg.time`. A value of zero means that all available data will
#'   be used in a particular period regardless if of the number of values
#'   available. Conversely, a value of 100 will mean that all data will need to
#'   be present for the average to be calculated, else it is recorded as `NA`.
#' @param alpha For the confidence interval calculations of the slope. The
#'   default is 0.05. To show 99\% confidence intervals for the value of the
#'   trend, choose alpha = 0.01 etc.
#' @param dec.place The number of decimal places to display the trend estimate
#'   at. The default is 2.
#' @param lab.frac Fraction along the y-axis that the trend information should
#'   be printed at, default 0.99.
#' @param lab.cex Size of text for trend information.
#' @param x.relation This determines how the x-axis scale is plotted.
#'   \dQuote{same} ensures all panels use the same scale and \dQuote{free} will
#'   use panel-specific scales. The latter is a useful setting when plotting
#'   data with very different values.
#' @param y.relation This determines how the y-axis scale is plotted.
#'   \dQuote{same} ensures all panels use the same scale and \dQuote{free} will
#'   use panel-specific scales. The latter is a useful setting when plotting
#'   data with very different values.
#' @param data.col Colour name for the data
#' @param trend list containing information on the line width, line type and
#'   line colour for the main trend line and confidence intervals respectively.
#' @param text.col Colour name for the slope/uncertainty numeric estimates
#' @param slope.text The text shown for the slope (default is
#'   \sQuote{units/year}).
#' @param cols Predefined colour scheme, currently only enabled for
#'   `"greyscale"`.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly e.g.  by subscripting the \sQuote{2} in NO2.
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
#' @param date.breaks Number of major x-axis intervals to use. The function will
#'   try and choose a sensible number of dates/times as well as formatting the
#'   date/time appropriately to the range being considered. This does not always
#'   work as desired automatically. The user can therefore increase or decrease
#'   the number of intervals by adjusting the value of `date.breaks` up or down.
#' @param date.format This option controls the date format on the x-axis. While
#'   `TheilSen` generally sets the date format sensibly there can be some
#'   situations where the user wishes to have more control. For format types see
#'   `strptime`. For example, to format the date like \dQuote{Jan-2012} set
#'   `date.format = "\%b-\%Y"`.
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract trend components and plotting them in other ways.
#' @param silent When `FALSE` the function will give updates on trend-fitting
#'   progress.
#' @param ... Other graphical parameters passed onto `cutData` and
#'   `lattice:xyplot`. For example, `TheilSen` passes the option `hemisphere =
#'   "southern"` on to `cutData` to provide southern (rather than default
#'   northern) hemisphere handling of `type = "season"`. Similarly, common axis
#'   and title labelling options (such as `xlab`, `ylab`, `main`) are passed to
#'   `xyplot` via `quickText` to handle routine formatting.
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
#' \dots{} see also several of the Air Quality Expert Group (AQEG) reports for
#' the use of similar tests applied to UK/European air quality data.
#' @examples
#' # trend plot for nox
#' TheilSen(mydata, pollutant = "nox")
#'
#' # trend plot for ozone with p=0.01 i.e. uncertainty in slope shown at
#' # 99 % confidence interval
#'
#' \dontrun{
#' TheilSen(mydata, pollutant = "o3", ylab = "o3 (ppb)", alpha = 0.01)
#' }
#'
#' # trend plot by each of 8 wind sectors
#' \dontrun{
#' TheilSen(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")
#' }
#'
#' # and for a subset of data (from year 2000 onwards)
#' \dontrun{
#' TheilSen(selectByDate(mydata, year = 2000:2005), pollutant = "o3", ylab = "o3 (ppb)")
#' }
TheilSen <- function(
  mydata,
  pollutant = "nox",
  deseason = FALSE,
  type = "default",
  avg.time = "month",
  statistic = "mean",
  percentile = NA,
  data.thresh = 0,
  alpha = 0.05,
  dec.place = 2,
  lab.frac = 0.99,
  lab.cex = 0.8,
  x.relation = "same",
  y.relation = "same",
  data.col = "cornflowerblue",
  trend = list(lty = c(1, 5), lwd = c(2, 1), col = c("red", "red")),
  text.col = "darkgreen",
  slope.text = NULL,
  cols = NULL,
  auto.text = TRUE,
  autocor = FALSE,
  slope.percent = FALSE,
  date.breaks = 7,
  date.format = NULL,
  plot = TRUE,
  silent = FALSE,
  ...
) {
  # input checking
  rlang::arg_match(avg.time, c("year", "month", "season"))

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    # other local colours
    trend$col <- c("black", "black")
    data.col <- "darkgrey"
    text.col <- "black"
  }

  # extra.args setup
  extra.args <- list(...)

  # label controls
  extra.args$ylab <- quickText(extra.args$ylab %||% pollutant, auto.text)
  extra.args$xlab <- quickText(extra.args$xlab %||% "year", auto.text)
  extra.args$main <- quickText(extra.args$main, auto.text)

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
  if (floor(days) == 31) {
    interval <- "month"
  }
  if (floor(days) %in% c(365, 366)) {
    interval <- "year"
  }

  # data checks
  mydata <- checkPrep(mydata, c("date", pollutant), type, remove.calm = FALSE)

  # cutData depending on type
  mydata <- cutData(mydata, type, ...)

  # for overall data and graph plotting
  start.year <- startYear(mydata$date)
  end.year <- endYear(mydata$date)
  start.month <- startMonth(mydata$date)
  end.month <- endMonth(mydata$date)

  mydata <- timeAverage(
    mydata,
    type = type,
    avg.time = avg.time,
    statistic = statistic,
    percentile = percentile,
    data.thresh = data.thresh,
    interval = interval,
    progress = !silent
  )

  # timeAverage drops type if default
  if ("default" %in% type) {
    mydata$default <- "default"
  }

  process.cond <- function(mydata) {
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
      mydata$date <- lubridate::as_date(mydata$date)

      deseas <- mydata[[pollutant]]

      # can't deseason less than 2 years of data
      if (deseason && nrow(mydata) > 24) {
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
      results <- stats::na.omit(all.results)
    } else {
      # assume annual
      all.results <- data.frame(
        date = lubridate::as_date(mydata$date),
        conc = mydata[[pollutant]],
        stringsAsFactors = FALSE
      )
      results <- stats::na.omit(all.results)
    }

    # now calculate trend, uncertainties etc
    if (nrow(results) < 6) {
      # need enough data to calculate trend, set missing if not

      results <- dplyr::mutate(
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
    results <- dplyr::full_join(all.results, MKresults, by = "date")

    results
  }

  # split data by type and apply process.cond to each subset, then recombine
  split.data <- mydata |>
    dplyr::group_by(dplyr::across(dplyr::all_of(type))) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(
        data,
        process.cond,
        .progress = "Taking Bootstrap Samples"
      )
    ) |>
    tidyr::unnest(cols = c(data)) |>
    dplyr::ungroup()

  if (nrow(split.data) < 2) {
    return(NULL)
  }

  # calculate slopes etc
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
    dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)),
    .by = dplyr::all_of(c(type, "p.stars"))
  ) |>
    tidyr::drop_na()

  # calculate percentage changes in slope and uncertainties need
  # start and end dates (in days) to work out concentrations at those
  # points percentage change defined as 100.(C.end/C.start -1) /
  # duration
  start <- dplyr::slice_head(split.data, n = 1, by = dplyr::all_of(type))
  end <- dplyr::slice_tail(split.data, n = 1, by = dplyr::all_of(type))

  percent.change <- dplyr::full_join(
    start,
    end,
    by = type,
    suffix = c(".start", ".end")
  )

  pct_per_year <- function(slope, intercept, date_start, date_end) {
    d_start <- as.numeric(date_start)
    d_end <- as.numeric(date_end)
    c_start <- slope * d_start / 365 + intercept
    c_end <- slope * d_end / 365 + intercept
    100 * 365 * (c_end / c_start - 1) / (d_end - d_start)
  }

  percent.change <- percent.change |>
    dplyr::mutate(
      slope.percent = pct_per_year(
        .data$slope.start,
        .data$intercept.start,
        .data$date.start,
        .data$date.end
      ),
      lower.percent = pct_per_year(
        .data$lower.start,
        .data$intercept.lower.start,
        .data$date.start,
        .data$date.end
      ),
      upper.percent = pct_per_year(
        .data$upper.start,
        .data$intercept.upper.start,
        .data$date.start,
        .data$date.end
      )
    ) |>
    dplyr::select(
      dplyr::all_of(c(type, "slope.percent", "lower.percent", "upper.percent"))
    )

  split.data <- dplyr::left_join(split.data, percent.change, by = type)

  res2 <- dplyr::left_join(res2, percent.change, by = type)

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

  # x-axis scale function
  if (lubridate::is.Date(split.data$date)) {
    x_scale_fun <- ggplot2::scale_x_date
  } else {
    x_scale_fun <- ggplot2::scale_x_datetime
  }

  thePlot <-
    ggplot2::ggplot() +
    ggplot2::geom_line(
      data = split.data,
      mapping = ggplot2::aes(x = .data$date, y = .data$conc),
      colour = data.col
    ) +
    ggplot2::geom_point(
      data = split.data,
      mapping = ggplot2::aes(x = .data$date, y = .data$conc),
      size = extra.args$cex %||% 3,
      shape = 21,
      colour = data.col
    ) +
    ggplot2::geom_abline(
      data = res2,
      mapping = ggplot2::aes(intercept = .data$a, slope = .data$b),
      color = trend$col[1],
      linewidth = trend$lwd[1] / 2,
      linetype = trend$lty[1]
    ) +
    ggplot2::geom_abline(
      data = res2,
      mapping = ggplot2::aes(intercept = .data$upper.a, slope = .data$upper.b),
      color = trend$col[2],
      linewidth = trend$lwd[2] / 2,
      linetype = trend$lty[2]
    ) +
    ggplot2::geom_abline(
      data = res2,
      mapping = ggplot2::aes(intercept = .data$lower.a, slope = .data$lower.b),
      color = trend$col[2],
      linewidth = trend$lwd[2] / 2,
      linetype = trend$lty[2]
    ) +
    ggplot2::geom_text(
      data = res2,
      mapping = ggplot2::aes(
        label = paste(
          round(.data[[slope]], dec.place),
          " ",
          "[",
          round(.data[[lower]], dec.place),
          ", ",
          round(.data[[upper]], dec.place),
          "] ",
          slope.text %||% paste0(units, "/year"),
          " ",
          .data$p.stars,
          sep = ""
        )
      ),
      size = lab.cex * 5,
      color = text.col,
      y = I(lab.frac),
      x = I(0.5),
      vjust = 1,
      fontface = "bold"
    ) +
    theme_openair("none") +
    set_extra_fontsize(extra.args) +
    get_facet(
      type,
      extra.args,
      scales = relation_to_facet_scales(x.relation, y.relation),
      auto.text = auto.text,
      drop = FALSE
    ) +
    x_scale_fun(
      breaks = scales::breaks_pretty(date.breaks),
      date_labels = date.format %||% ggplot2::waiver(),
      limits = extra.args$xlim
    ) +
    ggplot2::scale_y_continuous(
      limits = extra.args$ylim
    ) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      title = extra.args$main
    )

  if (plot) {
    plot(thePlot)
  }

  output <- list(
    plot = thePlot,
    data = list(
      main.data = dplyr::tibble(split.data),
      res2 = dplyr::tibble(res2),
      subsets = c("main.data", "res2")
    ),
    call = match.call()
  )
  class(output) <- "openair"

  invisible(output)
}

# function to calculate theil-sen slope estimates, confidence intervals and p
# values
mk_stats <- function(x, y, alpha, autocor, silent) {
  estimates <- regci(
    as.numeric(x),
    y,
    alpha = alpha,
    autocor = autocor,
    pr = silent
  )$regci

  p <- estimates[2, 5]

  stars <- dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ "+",
    .default = ""
  )

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
}
