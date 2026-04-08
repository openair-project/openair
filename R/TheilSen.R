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
#' @inheritParams shared_openair_params
#'
#' @param mydata A data frame containing the field `date` and at least one other
#'   parameter for which a trend test is required; typically (but not
#'   necessarily) a pollutant.
#'
#' @param pollutant The parameter for which a trend test is required. Mandatory.
#'
#' @param deseason Should the data be de-deasonalized first? If `TRUE` the
#'   function `stl` is used (seasonal trend decomposition using loess). Note
#'   that if `TRUE` missing data are first imputed using a linear regression by month because `stl` cannot handle missing data. In this case the plot shows where the missing data have been imputed as a grey filled circle.
#'
#' @param avg.time Can be \dQuote{month} (the default), \dQuote{season} or
#'   \dQuote{year}. Determines the time over which data should be averaged. Note
#'   that for \dQuote{year}, six or more years are required. For \dQuote{season}
#'   the data are split up into spring: March, April, May etc. Note that
#'   December is considered as belonging to winter of the following year.
#'
#' @param statistic Statistic used for calculating monthly values. Default is
#'   \dQuote{mean}, but can also be \dQuote{percentile}. See [timeAverage()] for
#'   more details.
#'
#' @param percentile Single percentile value to use if `statistic =
#'   "percentile"` is chosen.
#'
#' @param data.thresh The data capture threshold to use (%) when aggregating the
#'   data using `avg.time`. A value of zero means that all available data will
#'   be used in a particular period regardless if of the number of values
#'   available. Conversely, a value of 100 will mean that all data will need to
#'   be present for the average to be calculated, else it is recorded as `NA`.
#'
#' @param alpha For the confidence interval calculations of the slope. The
#'   default is 0.05. To show 99\% confidence intervals for the value of the
#'   trend, choose alpha = 0.01 etc.
#'
#' @param dec.place The number of decimal places to display the trend estimate
#'   at. The default is 2.
#'
#' @param lab.frac Fraction along the y-axis that the trend information should
#'   be printed at, default 0.99.
#'
#' @param lab.cex Size of text for trend information.
#'
#' @param data.col Colour name for the data
#'
#' @param trend list containing information on the line width, line type and
#'   line colour for the main trend line and confidence intervals respectively.
#'
#' @param text.col Colour name for the slope/uncertainty numeric estimates
#'
#' @param slope.text The text shown for the slope (default is
#'   \sQuote{units/year}).
#'
#' @param cols Predefined colour scheme, currently only enabled for
#'   `"greyscale"`.
#'
#' @param autocor Should autocorrelation be considered in the trend uncertainty
#'   estimates? The default is `FALSE`. Generally, accounting for
#'   autocorrelation increases the uncertainty of the trend estimate ---
#'   sometimes by a large amount.
#'
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
#'
#' @param silent When `FALSE` the function will give updates on trend-fitting
#'   progress.
#'
#' @export TheilSen
#'
#' @return an [openair][openair-package] object. The `data` component of the
#'   `TheilSen` output includes two subsets: `main.data`, the monthly data
#'   `res2` the trend statistics. For `output <- TheilSen(mydata, "nox")`, these
#'   can be extracted as `object$data$main.data` and `object$data$res2`,
#'   respectively. Note: In the case of the intercept, it is assumed the y-axis
#'   crosses the x-axis on 1/1/1970.
#'
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
  strip.position = "top",
  plot = TRUE,
  silent = FALSE,
  ...
) {
  # 1. Input Checking & Argument Setup
  rlang::arg_match(avg.time, c("year", "month", "season"))

  if (length(cols) == 1 && cols == "greyscale") {
    trend$col <- c("black", "black")
    data.col <- "darkgrey"
    text.col <- "black"
  }

  # extra.args setup
  extra.args <- capture_dots(...)

  # label controls
  extra.args$ylab <- quickText(extra.args$ylab %||% pollutant, auto.text)
  extra.args$xlab <- quickText(extra.args$xlab %||% "year", auto.text)
  extra.args$title <- quickText(extra.args$title, auto.text)
  extra.args$subtitle <- quickText(extra.args$subtitle, auto.text)
  extra.args$caption <- quickText(extra.args$caption, auto.text)

  # 2. Time Interval & Data Preparation
  interval <- find_time_interval(mydata$date)
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) / 24 / 3600

  if (floor(days) == 31) {
    interval <- "month"
  }
  if (floor(days) %in% c(365, 366)) {
    interval <- "year"
  }

  mydata <- checkPrep(mydata, c("date", pollutant), type, remove.calm = FALSE)
  mydata <- cutData(mydata, type, ...)

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

  if ("default" %in% type) {
    mydata$default <- "default"
  }

  # 3. Calculate Theil-Sen statistics (Bootstrap)
  split.data <- mydata |>
    dplyr::group_by(dplyr::across(dplyr::all_of(type))) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(
        .data$data,
        process_theilsen_subset,
        pollutant = pollutant,
        avg.time = avg.time,
        deseason = deseason,
        alpha = alpha,
        autocor = autocor,
        silent = silent,
        .progress = "Taking Bootstrap Samples"
      )
    ) |>
    tidyr::unnest(cols = c("data")) |>
    dplyr::ungroup()

  if (nrow(split.data) < 2) {
    return(NULL)
  }

  split.data <- dplyr::mutate(
    split.data,
    slope = 365 * .data$b,
    intercept = .data$a,
    intercept.lower = .data$lower.a,
    intercept.upper = .data$upper.a,
    lower = 365 * .data$lower.b,
    upper = 365 * .data$upper.b
  )

  # 4. Aggregate Results & Calculate Percentage Trends
  res2 <- dplyr::summarise(
    split.data,
    dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)),
    .by = dplyr::all_of(c(type, "p.stars"))
  ) |>
    tidyr::drop_na()

  percent_data <- add_percentage_change(split.data, type)
  split.data <- dplyr::left_join(split.data, percent_data, by = type)
  res2 <- dplyr::left_join(res2, percent_data, by = type)

  # 5. Build and output plot
  thePlot <- build_theilsen_plot(
    split.data = split.data,
    res2 = res2,
    type = type,
    slope.percent = slope.percent,
    dec.place = dec.place,
    slope.text = slope.text,
    lab.cex = lab.cex,
    lab.frac = lab.frac,
    text.col = text.col,
    data.col = data.col,
    trend = trend,
    extra.args = extra.args,
    x.relation = x.relation,
    y.relation = y.relation,
    auto.text = auto.text,
    strip.position = strip.position,
    date.breaks = date.breaks,
    date.format = date.format
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

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

# Process individual subsets for Theil-Sen calculations and deseasonalization
process_theilsen_subset <- function(
  mydata,
  pollutant,
  avg.time,
  deseason,
  alpha,
  autocor,
  silent
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

  min.idx <- min(which(!is.na(mydata[, pollutant])))
  max.idx <- max(which(!is.na(mydata[, pollutant])))
  mydata <- mydata[min.idx:max.idx, ]

  start.year <- get_first_year(mydata$date)
  end.year <- get_last_year(mydata$date)
  start.month <- get_first_month(mydata$date)
  end.month <- get_last_month(mydata$date)

  if (avg.time == "month") {
    mydata$date <- lubridate::as_date(mydata$date)
    deseas <- mydata[[pollutant]]
    was_na <- rep(FALSE, nrow(mydata))

    if (deseason && nrow(mydata) > 24) {
      myts <- stats::ts(
        mydata[[pollutant]],
        start = c(start.year, start.month),
        end = c(end.year, end.month),
        frequency = 12
      )

      was_na <- is.na(myts)
      if (anyNA(myts)) {
        myts <- fill_ts_gaps(myts, pollutant)
      }

      ssd <- stats::stl(myts, s.window = 11, robust = TRUE, s.degree = 1)
      deseas <- as.vector(
        ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]
      )
    }

    all.results <- data.frame(
      date = mydata$date,
      conc = deseas,
      imputed = was_na,
      stringsAsFactors = FALSE
    )
  } else {
    all.results <- data.frame(
      date = lubridate::as_date(mydata$date),
      conc = mydata[[pollutant]],
      imputed = FALSE,
      stringsAsFactors = FALSE
    )
  }

  results <- stats::na.omit(all.results)

  if (nrow(results) < 6) {
    return(dplyr::mutate(
      results,
      b = NA,
      a = NA,
      lower.a = NA,
      upper.a = NA,
      lower.b = NA,
      upper.b = NA,
      p.stars = NA
    ))
  }

  MKresults <- mk_stats(
    results$date,
    results$conc,
    alpha,
    autocor,
    silent = silent
  )
  dplyr::full_join(all.results, MKresults, by = "date")
}

# Add percentage change per year
add_percentage_change <- function(split.data, type) {
  start <- dplyr::slice_head(split.data, n = 1, by = dplyr::all_of(type))
  end <- dplyr::slice_tail(split.data, n = 1, by = dplyr::all_of(type))

  dplyr::full_join(start, end, by = type, suffix = c(".start", ".end")) |>
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
    dplyr::select(dplyr::all_of(c(
      type,
      "slope.percent",
      "lower.percent",
      "upper.percent"
    )))
}

# Formula for % change per year
pct_per_year <- function(slope, intercept, date_start, date_end) {
  d_start <- as.numeric(date_start)
  d_end <- as.numeric(date_end)
  c_start <- slope * d_start / 365 + intercept
  c_end <- slope * d_end / 365 + intercept
  100 * 365 * (c_end / c_start - 1) / (d_end - d_start)
}

# Function to calculate Theil-Sen slope estimates, confidence intervals, and p values
mk_stats <- function(x, y, alpha, autocor, silent) {
  # Run the bootstrap regression on the RAW uncentered data
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
    upper.a = estimates[1, 1], # Lower intercept paired with...
    upper.b = estimates[2, 2], # Upper slope
    lower.a = estimates[1, 2], # Upper intercept paired with...
    lower.b = estimates[2, 1], # Lower slope
    p = p,
    p.stars = stars,
    stringsAsFactors = FALSE
  )
}

# Centralised ggplot construction
build_theilsen_plot <- function(
  split.data,
  res2,
  type,
  slope.percent,
  dec.place,
  slope.text,
  lab.cex,
  lab.frac,
  text.col,
  data.col,
  trend,
  extra.args,
  x.relation,
  y.relation,
  auto.text,
  strip.position,
  date.breaks,
  date.format
) {
  slope_var <- if (slope.percent) "slope.percent" else "slope"
  lower_var <- if (slope.percent) "lower.percent" else "lower"
  upper_var <- if (slope.percent) "upper.percent" else "upper"
  units_str <- if (slope.percent) "%" else "units"

  x_scale_fun <- if (lubridate::is.Date(split.data$date)) {
    ggplot2::scale_x_date
  } else {
    ggplot2::scale_x_datetime
  }

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = split.data,
      mapping = ggplot2::aes(x = .data$date, y = .data$conc),
      colour = data.col,
      linewidth = extra.args$linewidth[1] %||% 0.75,
      linetype = extra.args$linetype[1] %||% 1,
      alpha = extra.args$alpha %||% 1,
      lineend = extra.args$lineend %||% "butt",
      linejoin = extra.args$linejoin %||% "round",
      linemitre = extra.args$linemitre %||% 10
    ) +
    ggplot2::geom_point(
      data = split.data,
      mapping = ggplot2::aes(x = .data$date, y = .data$conc),
      size = extra.args$size[1] %||% 3,
      shape = extra.args$shape[1] %||% 1,
      alpha = extra.args$alpha[1] %||% 1,
      colour = data.col
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(split.data, .data$imputed),
      mapping = ggplot2::aes(x = .data$date, y = .data$conc),
      size = extra.args$cex %||% 3,
      shape = 21,
      fill = "grey50",
      colour = "grey20"
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
          round(.data[[slope_var]], dec.place),
          " [",
          round(.data[[lower_var]], dec.place),
          ", ",
          round(.data[[upper_var]], dec.place),
          "] ",
          slope.text %||% paste0(units_str, "/year"),
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
      drop = FALSE,
      strip.position = strip.position,
      wd.res = extra.args$wd.res %||% 8
    ) +
    x_scale_fun(
      breaks = scales::breaks_pretty(date.breaks),
      date_labels = date.format %||% ggplot2::waiver(),
      limits = extra.args$xlim
    ) +
    ggplot2::scale_y_continuous(limits = extra.args$ylim) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      title = extra.args$title,
      subtitle = extra.args$subtitle,
      caption = extra.args$caption
    )
}
