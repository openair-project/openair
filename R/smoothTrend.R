#' Calculate nonparametric smooth trends
#'
#' Use non-parametric methods to calculate time series trends
#'
#' The `smoothTrend` function provides a flexible way of estimating the
#' trend in the concentration of a pollutant or other variable. Monthly mean
#' values are calculated from an hourly (or higher resolution) or daily time
#' series. There is the option to deseasonalise the data if there is evidence
#' of a seasonal cycle.
#'
#' `smoothTrend` uses a Generalized Additive Model (GAM) from the
#' [gam()] package to find the most appropriate level of smoothing.
#' The function is particularly suited to situations where trends are not
#' monotonic (see discussion with [TheilSen()] for more details on
#' this). The `smoothTrend` function is particularly useful as an
#' exploratory technique e.g. to check how linear or non-linear trends are.
#'
#' 95% confidence intervals are shown by shading. Bootstrap estimates of the
#' confidence intervals are also available through the `simulate` option.
#' Residual resampling is used.
#'
#' Trends can be considered in a very wide range of ways, controlled by setting
#' `type` - see examples below.
#'
#' @param mydata A data frame containing the field `date` and at least one
#'   other parameter for which a trend test is required; typically (but not
#'   necessarily) a pollutant.
#' @param pollutant The parameter for which a trend test is required.
#'   Mandatory.
#' @param deseason Should the data be de-deasonalized first? If `TRUE` the
#'   function `stl` is used (seasonal trend decomposition using loess).
#'   Note that if `TRUE` missing data are first imputed using a
#'   Kalman filter and Kalman smooth.
#' @param type `type` determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce a single plot using the
#'   entire data. Type can be one of the built-in types as detailed in
#'   `cutData` e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and
#'   so on. For example, `type = "season"` will produce four plots --- one
#'   for each season.
#'
#'   It is also possible to choose `type` as another variable in the data
#'   frame. If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Type can be up length two e.g. `type = c("season", "weekday")` will
#'   produce a 2x2 plot split by season and day of the week. Note, when two
#'   types are provided the first forms the columns and the second the rows.
#' @param statistic Statistic used for calculating monthly values. Default is
#'   \dQuote{mean}, but can also be \dQuote{percentile}. See `timeAverage`
#'   for more details.
#' @param avg.time Can be \dQuote{month} (the default), \dQuote{season} or
#'   \dQuote{year}. Determines the time over which data should be averaged.
#'   Note that for \dQuote{year}, six or more years are required. For
#'   \dQuote{season} the data are plit up into spring: March, April, May etc.
#'   Note that December is considered as belonging to winter of the following
#'   year.
#' @param percentile Percentile value(s) to use if `statistic =
#'   "percentile"` is chosen. Can be a vector of numbers e.g. `percentile
#'   = c(5, 50, 95)` will plot the 5th, 50th and 95th percentile values
#'   together on the same plot.
#' @param data.thresh The data capture threshold to use (%) when aggregating
#'   the data using `avg.time`. A value of zero means that all available
#'   data will be used in a particular period regardless if of the number of
#'   values available. Conversely, a value of 100 will mean that all data will
#'   need to be present for the average to be calculated, else it is recorded
#'   as `NA`. Not used if `avg.time = "default"`.
#' @param simulate Should simulations be carried out to determine the
#'   Mann-Kendall tau and p-value. The default is `FALSE`. If `TRUE`,
#'   bootstrap simulations are undertaken, which also account for
#'   autocorrelation.
#' @param n Number of bootstrap simulations if `simulate = TRUE`.
#' @param autocor Should autocorrelation be considered in the trend uncertainty
#'   estimates? The default is `FALSE`. Generally, accounting for
#'   autocorrelation increases the uncertainty of the trend estimate sometimes
#'   by a large amount.
#' @param cols Colours to use. Can be a vector of colours e.g. `cols =
#'   c("black", "green")` or pre-defined openair colours --- see
#'   `openColours` for more details.
#' @param shade The colour used for marking alternate years. Use \dQuote{white}
#'   or \dQuote{transparent} to remove shading.
#' @param xlab x-axis label, by default \dQuote{year}.
#' @param y.relation This determines how the y-axis scale is plotted. "same"
#'   ensures all panels use the same scale and "free" will use panel-specific
#'   scales. The latter is a useful setting when plotting data with very
#'   different values.  ref.x See `ref.y` for details. In this case the
#'   correct date format should be used for a vertical line e.g.  `ref.x =
#'   list(v = as.POSIXct("2000-06-15"), lty = 5)`.
#' @param ref.x See `ref.y`.
#' @param ref.y A list with details of the horizontal lines to be added
#'   representing reference line(s). For example, `ref.y = list(h = 50,
#'   lty = 5)` will add a dashed horizontal line at 50. Several lines can be
#'   plotted e.g. `ref.y = list(h = c(50, 100), lty = c(1, 5), col =
#'   c("green", "blue"))`. See `panel.abline` in the `lattice`
#'   package for more details on adding/controlling lines.
#' @param key.columns Number of columns used if a key is drawn when using the
#'   option `statistic = "percentile"`.
#' @param name.pol Names to be given to the pollutant(s). This is useful if you
#'   want to give a fuller description of the variables, maybe also including
#'   subscripts etc.
#' @param ci Should confidence intervals be plotted? The default is
#'   `FALSE`.
#' @param alpha The alpha transparency of shaded confidence intervals - if
#'   plotted. A value of 0 is fully transparent and 1 is fully opaque.
#' @param date.breaks Number of major x-axis intervals to use. The function
#'   will try and choose a sensible number of dates/times as well as formatting
#'   the date/time appropriately to the range being considered. This does not
#'   always work as desired automatically. The user can therefore increase or
#'   decrease the number of intervals by adjusting the value of
#'   `date.breaks` up or down.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If
#'   `TRUE` titles and axis labels will automatically try and format
#'   pollutant names and units properly e.g.  by subscripting the \sQuote{2} in
#'   NO2.
#' @param k This is the smoothing parameter used by the `gam` function in
#'   package `mgcv`. By default it is not used and the amount of smoothing
#'   is optimised automatically. However, sometimes it is useful to set the
#'   smoothing amount manually using `k`.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract plot components and plotting them in other
#'   ways.
#' @param ... Other graphical parameters are passed onto `cutData` and
#'   `lattice:xyplot`. For example, `smoothTrend` passes the option
#'   `hemisphere = "southern"` on to `cutData` to provide southern
#'   (rather than default northern) hemisphere handling of `type =
#'   "season"`. Similarly, common graphical arguments, such as `xlim` and
#'   `ylim` for plotting ranges and `pch` and `cex` for plot
#'   symbol type and size, are passed on `xyplot`, although some local
#'   modifications may be applied by openair. For example, axis and title
#'   labelling options (such as `xlab`, `ylab` and `main`) are
#'   passed to `xyplot` via `quickText` to handle routine formatting.
#'   One special case here is that many graphical parameters can be vectors
#'   when used with `statistic = "percentile"` and a vector of
#'   `percentile` values, see examples below.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family time series and trend functions
#' @examples
#' # trend plot for nox
#' smoothTrend(mydata, pollutant = "nox")
#'
#' # trend plot by each of 8 wind sectors
#' \dontrun{smoothTrend(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")}
#'
#' # several pollutants, no plotting symbol
#' \dontrun{smoothTrend(mydata, pollutant = c("no2", "o3", "pm10", "pm25"), pch = NA)}
#'
#' # percentiles
#' \dontrun{smoothTrend(mydata, pollutant = "o3", statistic = "percentile",
#' percentile = 95)}
#'
#' # several percentiles with control over lines used
#' \dontrun{smoothTrend(mydata, pollutant = "o3", statistic = "percentile",
#' percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5))}
smoothTrend <- function(mydata, pollutant = "nox", deseason = FALSE,
                        type = "default", statistic = "mean", avg.time = "month",
                        percentile = NA, data.thresh = 0, simulate = FALSE,
                        n = 200, autocor = FALSE, cols = "brewer1",
                        shade = "grey95", xlab = "year",
                        y.relation = "same", ref.x = NULL, ref.y = NULL,
                        key.columns = length(percentile), name.pol = pollutant,
                        ci = TRUE, alpha = 0.2, date.breaks = 7,
                        auto.text = TRUE, k = NULL, plot = TRUE, ...) {

  ## get rid of R check annoyances
  variable <- NULL

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
 #
    fontsize = current.font
  ))

  ## Args setup
  Args <- list(...)

  # label controls
  ## xlab in args because local unique
  ## ylab in code before plot because of local unique
  Args$main <- if ("main" %in% names(Args)) {
    quickText(Args$main, auto.text)
  } else {
    quickText("", auto.text)
  }

  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  xlim <- if ("xlim" %in% names(Args)) {
    Args$xlim
  } else {
    NULL
  }

  # lty, lwd, pch, cex handling
  if (!"lty" %in% names(Args)) {
    Args$lty <- 1
  }
  if (!"lwd" %in% names(Args)) {
    Args$lwd <- 1
  }
  if (!"pch" %in% names(Args)) {
    Args$pch <- 1
  }
  if (!"cex" %in% names(Args)) {
    Args$cex <- 1
  }

  # layout default
  if (!"layout" %in% names(Args)) {
    Args$layout <- NULL
  }

  vars <- c("date", pollutant)

  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  if (!missing(percentile)) statistic <- "percentile"

  if (length(pollutant) > 1 & length(percentile) > 1) {
    warning(paste(
      "You cannot choose multiple percentiles and pollutants, using percentile =",
      percentile[1]
    ))
    percentile <- percentile[1]
  }

  if (!avg.time %in% c("year", "season", "month")) {
    stop("Averaging period must be 'month' or 'year'.")
  }

  ## find time interval
  # need this because if user has a data capture threshold, need to know
  # original time interval
  # Working this out for unique dates for all data is what is done here.
  # More reliable than trying to work it out after conditioning where there
  # may be too few data for the calculation to be reliable
  interval <- find.time.interval(mydata$date)

  ## equivalent number of days, used to refine interval for month/year
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
    24 / 3600

  ## better interval, most common interval in a year
  if (days == 31) interval <- "month"
  if (days %in% c(365, 366)) interval <- "year"

  ## for overall data and graph plotting
  start.year <- startYear(mydata$date)
  end.year <- endYear(mydata$date)
  start.month <- startMonth(mydata$date)
  end.month <- endMonth(mydata$date)

  ## date formatting for plot
  date.at <- dateBreaks(mydata$date, date.breaks)$major
  date.format <- dateBreaks(mydata$date)$format

  ## cutData depending on type
  mydata <- cutData(mydata, type, ...)

  ## reshape data, make sure there is not a variable called 'variable'
  if ("variable" %in% names(mydata)) {
    mydata <- rename(mydata, c(variable = "theVar"))
    type <- "theVar"
  }

  # in the case of mutiple percentiles, these are assinged and treated
  # like multiple pollutants

  mydata <- gather(mydata, key = variable, value = value, pollutant,
                   factor_key = TRUE)

  if (length(percentile) > 1) {
    vars <- c(type, "variable")

    mydata <- mydata %>%
      group_by(across(vars)) %>%
      do(calcPercentile(
        .,
        pollutant = "value",
        avg.time = avg.time, percentile = percentile,
        data.thresh = data.thresh
      ))

    vars <- paste0("percentile.", percentile)

    mydata <- gather(mydata, key = variable, value = value, vars)


  } else {

    mydata <- suppressWarnings(timeAverage(
      mydata,
      type = c(type, "variable"),
      avg.time = avg.time,
      percentile = percentile,
      statistic = statistic,
      data.thresh = data.thresh,
      interval = interval
    ))
  }

  # timeAverage drops type if default
  if (length(type) == 1 && type[1] == "default") {
    mydata$default <- "default"
  }

  process.cond <- function(mydata) {

    ## return if nothing to analyse
    if (all(is.na(mydata$value))) return(data.frame(date = NA, conc = NA))

    ## sometimes data have long trailing NAs, so start and end at
    ## first and last data
    min.idx <- min(which(!is.na(mydata[, "value"])))
    max.idx <- max(which(!is.na(mydata[, "value"])))
    mydata <- mydata[min.idx:max.idx, ]

    ## these subsets may have different dates to overall
    start.year <- startYear(mydata$date)
    end.year <- endYear(mydata$date)
    start.month <- startMonth(mydata$date)
    end.month <- endMonth(mydata$date)

    ## can't deseason less than 2 years of data
    if (nrow(mydata) <= 24) deseason <- FALSE

    if (deseason) {

      myts <- ts(
        mydata[["value"]],
        start = c(start.year, start.month),
        end = c(end.year, end.month), frequency = 12
      )

      # fill any missing data using a Kalman filter

      if (any(is.na(myts))) {

        # use forecast package to get best arima
        fit <- ts(rowSums(tsSmooth(StructTS(myts))[,-2]))
        id <- which(is.na(myts))

        myts[id] <- fit[id]

      }

      ssd <- stl(myts, s.window = 11, robust = TRUE, s.degree = 1)

      deseas <- ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]
      deseas <- as.vector(deseas)

      results <- data.frame(
        date = mydata$date, conc = as.vector(deseas),
        stringsAsFactors = FALSE
      )
    } else {
      results <- data.frame(
        date = mydata$date, conc = mydata[["value"]],
        stringsAsFactors = FALSE
      )
    }

    results
  }

  vars <- c(type, "variable")

  res <- mydata %>%
    group_by(across(vars)) %>%
    do(process.cond(.))

  ## smooth fits so that they can be returned to the user
  vars <- c(type, "variable")

  fit <- res %>%
    group_by(across(vars)) %>%
    group_modify(~ fitGam(x = "date", y = "conc", k = k, ...))

  class(fit$date) <- c("POSIXct", "POSIXt")

  ## special wd layout
  # (type field in results.grid called type not wd)
  if (length(type) == 1 & type[1] == "wd" & is.null(Args$layout)) {
    ## re-order to make sensible layout
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    res$wd <- ordered(res$wd, levels = wds)
    ## see if wd is actually there or not
    wd.ok <-
      sapply(wds, function(x) {
        if (x %in% unique(res$wd)) {
          FALSE
        } else {
          TRUE
        }
      })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    res$wd <- factor(res$wd) ## remove empty factor levels
    Args$layout <- if (type == "wd") {
      c(3, 3)
    } else {
      NULL
    }
    if (!"skip" %in% names(Args)) {
      Args$skip <- skip
    }
  }
  if (!"skip" %in% names(Args)) {
    Args$skip <- FALSE
  }

  ## proper names of labelling ###################################################
  strip.dat <- strip.fun(res, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]

  ## colours according to number of percentiles
  npol <- max(length(percentile), length(pollutant)) ## number of pollutants

  ## set up colours
  myColors <- if (length(cols) == 1 && cols == "greyscale") {
    openColours(cols, npol + 1)[-1]
  } else {
    openColours(cols, npol)
  }

  ## information for key
  npol <- na.omit(unique(res$variable))

  ## use pollutant names or user-supplied names
  if (!missing(name.pol)) {
    key.lab <- sapply(seq_along(name.pol), function(x)
      quickText(name.pol[x], auto.text))
  } else {
    key.lab <- sapply(seq_along(npol), function(x) quickText(npol[x], auto.text))
  }

  if (length(npol) > 1) {
    if (missing(key.columns)) {
      key.columns <- length(npol)
    }

    key <- list(
      lines = list(
        col = myColors[1:length(npol)],
        lty = Args$lty, lwd = Args$lwd,
        pch = Args$pch, type = "b", cex = Args$cex
      ),
      text = list(lab = key.lab), space = "bottom",
      columns = key.columns
    )

    ## use pollutant names or user-supplied names
    if (!"ylab" %in% names(Args)) {
      if (!missing(name.pol)) {
        Args$ylab <- quickText(paste(name.pol, collapse = ", "), auto.text)
      } else {
        Args$ylab <- quickText(paste(pollutant, collapse = ", "), auto.text)
      }
    }
  } else {
    key <- NULL ## either there is a key or there is not
  }

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("conc ~ date| ", temp, sep = ""))

  # ylab
  Args$ylab <- if ("ylab" %in% names(Args)) {
    quickText(Args$ylab, auto.text)
  } else {
    quickText(pollutant, auto.text)
  }

  gap <- difftime(max(mydata$date), min(mydata$date), units = "secs") / 80
  if (is.null(xlim)) xlim <- range(mydata$date) + c(-1 * gap, gap)

  xyplot.args <- list(
    x = myform, data = res, groups = res$variable,
    as.table = TRUE,
    strip = strip,
    strip.left = strip.left,
    key = key,
    xlim = xlim,
    par.strip.text = list(cex = 0.8),
    xlab = quickText(xlab, auto.text),
    scales = list(
      x = list(at = date.at, format = date.format),
      y = list(relation = y.relation, rot = 0)
    ),
    panel = panel.superpose,
    panel.groups = function(x,
                            y,
                            group.number,
                            lwd,
                            lty,
                            pch,
                            col,
                            col.line,
                            col.symbol,
                            subscripts,
                            type = "b",
                            ...) {
      if (group.number == 1) {
        ## otherwise this is called every time

        panel.shade(
          res, start.year, end.year,
          ylim = current.panel.limits()$ylim,
          shade
        )
        panel.grid(-1, 0)
      }

      panel.xyplot(
        x, y,
        type = "b", lwd = lwd,
        lty = lty, pch = pch,
        col.line = myColors[group.number],
        col.symbol = myColors[group.number], ...
      )

      panel.gam(
        x, y,
        col = myColors[group.number],
        k = k, myColors[group.number],
        simulate = simulate, n.sim = n,
        autocor = autocor, lty = 1, lwd = 1,
        se = ci, ...
      )

      ## add reference lines
      if (!is.null(ref.x)) do.call(panel.abline, ref.x)
      if (!is.null(ref.y)) do.call(panel.abline, ref.y)
    }
  )

  ## reset for Args
  xyplot.args <- listUpdate(xyplot.args, Args)

  ## plot
  plt <- do.call(xyplot, xyplot.args)

  newdata <- res
  output <- list(
    plot = plt, data = list(data = newdata, fit = fit),
    call = match.call()
  )
  class(output) <- "openair"

  ## output ########################################################################
  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    } else {
      plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    }
  }

  invisible(output)
}
