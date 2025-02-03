#' Function to rapidly provide an overview of air quality data
#'
#' This function provides a quick graphical and numerical summary of data. The
#' location presence/absence of data are shown, with summary statistics and
#' plots of variable distributions. [summaryPlot()] can also provide summaries
#' of a single pollutant across many sites.
#'
#' [summaryPlot()] produces two panels of plots: one showing the
#' presence/absence of data and the other the distributions. The left panel
#' shows time series and codes the presence or absence of data in different
#' colours. By stacking the plots one on top of another it is easy to compare
#' different pollutants/variables. Overall statistics are given for each
#' variable: mean, maximum, minimum, missing hours (also expressed as a
#' percentage), median and the 95th percentile. For each year the data capture
#' rate (expressed as a percentage of hours in that year) is also given.
#'
#' The right panel shows either a histogram or a density plot depending on the
#' choice of `type`. Density plots avoid the issue of arbitrary bin sizes that
#' can sometimes provide a misleading view of the data distribution. Density
#' plots are often more appropriate, but their effectiveness will depend on the
#' data in question.
#'
#' [summaryPlot()] will only show data that are numeric or integer type. This is
#' useful for checking that data have been imported properly. For example, if
#' for some reason a column representing wind speed erroneously had one or more
#' fields with characters in, the whole column would be either character or
#' factor type. The absence of a wind speed variable in the [summaryPlot()] plot
#' would therefore indicate a problem with the input data. In this particular
#' case, the user should go back to the source data and remove the characters or
#' remove them using R functions.
#'
#' If there is a field `site`, which would generally mean there is more than one
#' site, [summaryPlot()] will provide information on a
#' *single* pollutant across all sites, rather than provide details on all
#' pollutants at a *single* site. In this case the user should also provide a
#' name of a pollutant e.g. `pollutant = "nox"`. If a pollutant is not provided
#' the first numeric field will automatically be chosen.
#'
#' **It is strongly recommended that the [summaryPlot()] function is
#' applied to all new imported data sets to ensure the data are imported as
#' expected.**
#'
#' @param mydata A data frame to be summarised. Must contain a `date` field and
#'   at least one other parameter.
#' @param na.len Missing data are only shown with at least `na.len`
#'   *contiguous* missing vales. The purpose of setting `na.len` is
#'   for clarity: with long time series it is difficult to see where individual
#'   missing hours are. Furthermore, setting `na.len = 96`, for example would
#'   show where there are at least 4 days of continuous missing data.
#' @param clip When data contain outliers, the histogram or density plot can
#'   fail to show the distribution of the main body of data. Setting `clip =
#'   TRUE`, will remove the top 1 % of data to yield what is often a better
#'   display of the overall distribution of the data. The amount of clipping can
#'   be set with `percentile`.
#' @param percentile This is used to clip the data. For example, `percentile =
#'   0.99` (the default) will remove the top 1 percentile of values i.e. values
#'   greater than the 99th percentile will not be used.
#' @param type `type` is used to determine whether a histogram (the default) or
#'   a density plot is used to show the distribution of the data.
#' @param pollutant `pollutant` is used when there is a field `site` and there
#'   is more than one site in the data frame.
#' @param period `period` is either `years` (the default) or `months`.
#'   Statistics are calculated depending on the `period` chosen.
#' @param avg.time This defines the time period to average the time series
#'   plots. Can be "sec", "min", "hour", "day" (the default), "week", "month",
#'   "quarter" or "year". For much increased flexibility a number can precede
#'   these options followed by a space. For example, a [timeAverage()] of 2
#'   months would be `avg.time = "2 month"`.
#' @param print.datacap Should the data capture % be shown for each period?
#' @param breaks Number of histogram bins. Sometime useful but not easy to set a
#'   single value for a range of very different variables.
#' @param plot.type The `lattice` plot type, which is a line (`plot.type = "l"`)
#'   by default. Another useful option is `plot.type = "h"`, which draws
#'   vertical lines.
#' @param col.trend Colour to be used to show the monthly trend of the data,
#'   shown as a shaded region. Type `colors()` into R to see the full range of
#'   colour names.
#' @param col.data Colour to be used to show the *presence* of data. Type
#'   `colors()` into R to see the full range of colour names.
#' @param col.mis Colour to be used to show missing data.
#' @param col.hist Colour for the histogram or density plot.
#' @param cols Predefined colour scheme, currently only enabled for
#'   `"greyscale"`.
#' @param date.breaks Number of major x-axis intervals to use. The function will
#'   try and choose a sensible number of dates/times as well as formatting the
#'   date/time appropriately to the range being considered.  This does not
#'   always work as desired automatically. The user can therefore increase or
#'   decrease the number of intervals by adjusting the value of `date.breaks` up
#'   or down.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly, e.g., by subscripting the '2' in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#' @param debug Should data types be printed to the console? `TRUE` can be
#'   useful for debugging.
#' @param ... Other graphical parameters. Commonly used examples include the
#'   axis and title labelling options (such as `xlab`, `ylab` and `main`), which
#'   are all passed to the plot via [quickText()] to handle routine formatting.
#'   As [summaryPlot()] has two components, the axis labels may be a vector. For
#'   example, the default case (`type = "histogram"`) sets y labels equivalent
#'   to `ylab = c("", "Percent of Total")`.
#' @export
#' @author David Carslaw
#' @examples
#' # do not clip density plot data
#' \dontrun{
#' summaryPlot(mydata, clip = FALSE)
#' }
#'
#' # exclude highest 5 % of data etc.
#' \dontrun{
#' summaryPlot(mydata, percentile = 0.95)
#' }
#'
#' # show missing data where there are at least 96 contiguous missing
#' # values (4 days)
#' \dontrun{
#' summaryPlot(mydata, na.len = 96)
#' }
#'
#' # show data in green
#' \dontrun{
#' summaryPlot(mydata, col.data = "green")
#' }
#'
#' # show missing data in yellow
#' \dontrun{
#' summaryPlot(mydata, col.mis = "yellow")
#' }
#'
#' # show density plot line in black
#' \dontrun{
#' summaryPlot(mydata, col.dens = "black")
#' }
#'
summaryPlot <- function(mydata,
                        na.len = 24,
                        clip = TRUE,
                        percentile = 0.99,
                        type = "histogram",
                        pollutant = "nox",
                        period = "years",
                        avg.time = "day",
                        print.datacap = TRUE,
                        breaks = NULL,
                        plot.type = "l",
                        col.trend = "darkgoldenrod2",
                        col.data = "lightblue",
                        col.mis = "#A60A12",
                        col.hist = "forestgreen",
                        cols = NULL,
                        date.breaks = 7,
                        auto.text = TRUE,
                        plot = TRUE,
                        debug = FALSE,
                        ...) {
  ## get rid of R check annoyances
  value <- NULL

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    # strip
    current.strip <- trellis.par.get("strip.background")
    # other local colours
    col.trend <- "lightgrey"
    col.data <- "lightgrey"
    col.mis <- grDevices::grey(0.15)
    col.hist <- "grey"
    col.stat <- "black"
  } else {
    col.stat <- "darkgreen"
  }

  if (!period %in% c("years", "months", "days")) {
    stop("period should either be 'years' or 'months'.")
  }

  ## if site is in the data set, check none are missing
  ## seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) { ## split by site

    ## remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id, ]
    }
  }


  ## extra.args setup
  extra.args <- list(...)

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # label controls
  # all handled further in code body
  xlab <- if ("xlab" %in% names(extra.args)) {
    extra.args$xlab
  } else {
    NULL
  }
  ylab <- if ("ylab" %in% names(extra.args)) {
    extra.args$ylab
  } else {
    NULL
  }
  main <- if ("main" %in% names(extra.args)) {
    extra.args$main
  } else {
    NULL
  }

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }
  # drop main, xlab, ylab from extra.args
  extra.args <- extra.args[!names(extra.args) %in% c("xlab", "ylab", "main")]

  ## set panel strip to white
  suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))

  # the above might be a better retro fix for more complex functions?

  ## if date in format dd/mm/yyyy hh:mm (basic check)
  if (length(grep("/", as.character(mydata$date[1]))) > 0) {
    mydata$date <- as.POSIXct(strptime(mydata$date, "%d/%m/%Y %H:%M"))
  }

  ## check to see if there are any missing dates, stop if there are
  if (any(is.na(mydata$date))) {
    stop(cat("There are some missing dates on line(s)", which(is.na(mydata$date))), "\n")
  }


  ## for plot
  dateBreaks <- dateBreaks(mydata$date, date.breaks)$major

  ## print data types - helps with debugging
  if (debug) print(unlist(sapply(mydata, class)))

  ## check to see if there is a field site and >1 site
  ## if several sites and no pollutant supplied, use first numeric
  ## but also check to see if dates are duplicated, if not, OK to proceed
  len.all <- length(mydata$date)
  len.unique <- length(unique(mydata$date))

  if ("site" %in% names(mydata) & len.all != len.unique) {
    ## the data here ar ein "long" format, so make it "wide" and treat it like
    ## the usual data frame
    if (length(unique(mydata$site)) > 1) {
      ## get rid of unused factor levels if subset previously used
      mydata$site <- factor(mydata$site)
      ## id of first numeric column (pollutant)
      id <- which(sapply(mydata, class) %in% c("numeric", "integer"))[1]
      if (missing(pollutant)) pollutant <- names(mydata)[id]

      if (pollutant %in% names(mydata) == FALSE) {
        stop(cat("Can't find the variable", pollutant, "\n"))
      }

      mydata <- subset(mydata, select = c("date", "site", pollutant))
      names(mydata) <- c("date", "variable", "value")


      mydata <- tidyr::spread(mydata,
        key = variable, value = value
      )

      warning(paste("More than one site detected, using", pollutant))
    }
  }

  ## make sure only numeric data are selected
  dates <- mydata$date
  mydata <- mydata[, sapply(mydata, class) %in% c("numeric", "integer"), drop = FALSE]
  mydata <- data.frame(date = dates, mydata)

  ## remove variables where all are NA
  mydata <- mydata[, sapply(mydata, function(x) !all(is.na(x)))]

  ## force to be date/time class, even if originally Date class
  mydata$date <- as.POSIXct(mydata$date, lubridate::tz(mydata$date))

  ## proper names of labelling
  pol.name <- sapply(
    names(subset(mydata, select = -date)),
    function(x) quickText(x, auto.text)
  )

  ## round the dates depending on period
  min.year <- as.numeric(min(format(mydata$date, "%Y")))
  max.year <- as.numeric(max(format(mydata$date, "%Y")))
  start.date <- as.POSIXct(dateTrunc(min(mydata$date), period))
  end.date <- as.POSIXct(dateCeil(max(mydata$date), period) - 3600)

  ## find time interval of data and pad any missing times
  interval <- find.time.interval(mydata$date)
  all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
  mydata <- full_join(mydata, all.dates, by = "date") %>%
    arrange(date)



  ## means for trend line

  meanLine <- timeAverage(mydata, avg.time)
  meanLine <- gather(meanLine, key = variable, value = value, -date)

  # ensure order of pollutants is correct
  meanLine <- mutate(meanLine,
    variable = factor(variable, levels = unique(variable))
  )

  meanLine <- split(meanLine, meanLine$variable)

  mydata <- gather(mydata, key = variable, value = value, -date)

  # ensure order of pollutants is correct
  mydata <- mutate(mydata,
    variable = factor(variable, levels = unique(variable))
  )

  plot.missing <- function(mydata, na.len, col = "red") {
    dat <- ifelse(is.na(mydata[["value"]]), 1, 0)
    rle.seq <- rle(dat)
    cumsum.seq <- cumsum(rle.seq$lengths)
    myruns <- which(rle.seq$values == 1 & rle.seq$lengths >= na.len)

    ends <- cumsum.seq[myruns] # + 1 # to get whole hour
    newindex <- ifelse(myruns > 1, myruns - 1, 0)
    starts <- cumsum.seq[newindex] + 1
    if (0 %in% newindex) starts <- c(1, starts)
    data.frame(starts = mydata$date[starts], ends = mydata$date[ends])
  }

  summmary.stats <- function(mydata, period) {
    value <- mydata$value
    mis.dat <- sum(is.na(value))
    mis.per <- round(100 * mis.dat / nrow(mydata), 1)
    min.dat <- round(min(value, na.rm = TRUE), 1)
    max.dat <- round(max(value, na.rm = TRUE), 1)
    mean.dat <- round(mean(value, na.rm = TRUE), 1)
    median.dat <- round(median(value, na.rm = TRUE), 1)
    percentile <- round(quantile(value, probs = 0.95, na.rm = TRUE), 1)

    if (period == "years") format.t <- "%Y"
    if (period == "months") format.t <- "%Y-%m"
    if (period == "days") format.t <- "%Y-%m-%d"

    data.cap <- round(tapply(
      value, list(year = format(mydata$date, format.t)),
      function(x) 100 * length(na.omit(x)) / length(x)
    ), 1)
    res <- list(results = c(
      mis.dat, mis.per, min.dat, max.dat, mean.dat, median.dat,
      percentile
    ), data.cap = data.cap)
    return(res)
  }

  ## range in data
  range01 <- function(x) {
    y <- c(x, 0) ## to get sensible range
    rng <- range(y, na.rm = TRUE)
    (x - rng[1]) / diff(rng)
  }

  ## split data and calculate things needed for plot
  split.dat <- split(mydata, mydata$variable)

  sum.stats <- lapply(split.dat, summmary.stats, period)

  missing.dat <- lapply(split.dat, plot.missing, na.len)

  dummy.dat <- lapply(split.dat, head)
  dummy.dat <- do.call(rbind, dummy.dat)

  min.x <- as.numeric(min(mydata$date))
  max.x <- as.numeric(max(mydata$date))
  seq.year <- seq(start.date, end.date, by = period)


  # xlab, ylab handling for plt1
  # (so user inputs go through quicktext)
  my.ylab <- if (is.null(ylab[1]) || is.na(ylab[1])) {
    ""
  } else {
    quickText(ylab[1], auto.text)
  }
  my.xlab <- if (is.null(xlab[1]) || is.na(xlab[1])) {
    "date"
  } else {
    quickText(xlab[1], auto.text)
  }


  xyplot.args <- list(
    x = value ~ date | variable, data = dummy.dat, type = "n",
    ylim = c(0, 5.5),
    ylab = my.ylab,
    xlab = my.xlab,
    xlim = c(start.date - 60, end.date + 60),

    ## override scaling for more sensible date/time breaks
    scales = list(
      y = list(draw = FALSE),
      x = list(
        at = dateBreaks(mydata$date, date.breaks)$major,
        format = dateBreaks(mydata$date, date.breaks)$format
      )
    ),
    layout = c(1, length(unique(mydata$variable))),
    strip = FALSE,
    strip.left = strip.custom(horizontal = FALSE, factor.levels = pol.name),
    par.strip.text = list(cex = 0.7),
    panel = function(x, y, subscripts) {
      panelNo <- panel.number()

      panel.abline(v = dateBreaks, col = "grey85")

      ## plot the monthly mean data as a line
      meanLine[[panelNo]]$value <- 1 + range01(meanLine[[panelNo]]$value) * 4

      panel.xyplot(
        meanLine[[panelNo]]$date, meanLine[[panelNo]]$value,
        type = plot.type,
        col = col.trend, ...
      )

      ## plot all data region
      with(mydata, lrect(
        as.numeric(min(date)), 0,
        as.numeric(max(date)), 1,
        col = col.data, border = NA
      ))

      ## over-plot missing data - if there are any
      if (nrow(missing.dat[[panelNo]]) > 0) {
        lrect(
          as.numeric(missing.dat[[panelNo]]$starts), 0,
          as.numeric(missing.dat[[panelNo]]$ends), 1,
          col = col.mis, border = NA
        )
      }
      stats <- sum.stats[[panelNo]]$results
      data.cap <- sum.stats[[panelNo]]$data.cap

      ltext(min.x, 4, paste(
        "missing = ", stats[1], " (", stats[2], "%)",
        sep = ""
      ), cex = 0.6, pos = 4)

      ltext(min.x, 3, paste("min =", stats[3]), cex = 0.6, pos = 4)

      ltext(min.x, 2, paste("max =", stats[4]), cex = 0.6, pos = 4)

      ltext(max.x, 4, paste("mean =", stats[5]), cex = 0.6, pos = 2)

      ltext(max.x, 3, paste("median =", stats[6]), cex = 0.6, pos = 2)

      ltext(max.x, 2, paste("95th percentile =", stats[7]), cex = 0.6, pos = 2)


      if (print.datacap) {
        ltext(seq.year, 5, paste(data.cap, "%"), cex = 0.6, col = col.stat, pos = 4)
      }
    }
  )

  # reset for extra.args
  xyplot.args <- listUpdate(xyplot.args, extra.args)

  # plot
  plt1 <- do.call(xyplot, xyplot.args)

  ## this adjusts the space for the title to 2 lines (approx) if \n in title
  if (!is.null(main)) main <- quickText(main, auto.text)
  if (length(grep("atop", main) == 1)) y.upp <- 0.95 else y.upp <- 0.975
  if (is.null(main)) y.upp <- 1

  ## clip data to help show interesting part of distribution
  if (clip) {
    result <- lapply(split.dat, function(.df) {
      subset(.df, value < quantile(value, probs = percentile, na.rm = TRUE))
    })

    mydata <- do.call(rbind, result)
    row.names(mydata) <- NULL
  }

  # xlab, ylab handling for plt2
  # (so user inputs go through quicktext)
  # (and unique histogram/density naming is handled)
  my.ylab <- if (is.null(ylab[2]) || is.na(ylab[2])) {
    ""
  } else {
    ylab[2]
  }
  if (my.ylab == "") {
    if (type == "histogram") my.ylab <- "Percent of Total"
    if (type == "density") my.ylab <- "Density"
  } else {
    my.ylab <- quickText(my.ylab, auto.text)
  }

  my.xlab <- if (is.null(xlab[2]) || is.na(xlab[2])) {
    "value"
  } else {
    quickText(xlab[2], auto.text)
  }


  if (type == "histogram") {
    histogram.args <- list(
      x = ~ value | variable, data = mydata,
      xlab = my.xlab, ylab = my.ylab,
      par.strip.text = list(cex = 0.7),
      breaks = breaks,
      layout = c(1, length(unique(mydata$variable))),
      scales = list(relation = "free", y = list(rot = 0), xcex = 0.7),
      strip = FALSE,
      panel = function(x, ...) {
        panel.grid(-1, -1)
        panel.histogram(x, col = col.hist, border = NA, ...)
      }
    )


    plt2 <- do.call(histogram, histogram.args)
  } else {
    densityplot.args <- list(
      x = ~ value | variable, data = mydata,
      par.strip.text = list(cex = 0.7),
      xlab = my.xlab, ylab = my.ylab,
      layout = c(1, length(unique(mydata$variable))),
      scales = list(relation = "free", y = list(rot = 0), cex = 0.7),
      strip = FALSE,
      panel = function(x, ...) {
        panel.grid(-1, -1)
        panel.densityplot(
          x,
          lwd = 2, plot.points = FALSE,
          col = col.hist, ...
        )
      }
    )


    # plot
    plt2 <- do.call(densityplot, densityplot.args)

    #   plt2 <- densityplot()
  }


  # reset if greyscale
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set("strip.background", current.strip)
  }

  if (plot) {
    print(plt1, position = c(0, 0, 0.7, y.upp), more = TRUE)
    print(plt2, position = c(0.7, 0, 1, 0.975 * y.upp))

    ## use grid to add an overall title
    grid.text(main, 0.5, y.upp, gp = gpar(fontsize = 14))
  }

  ## create output with plot
  output <-
    list(
      plot = list(plt1, plt2),
      data = dplyr::tibble(dummy.dat),
      call = match.call()
    )
  class(output) <- "openair"
  invisible(output)
}
