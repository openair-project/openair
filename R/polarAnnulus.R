#' Bivariate polarAnnulus plot
#'
#' Typically plots the concentration of a pollutant by wind direction and as a
#' function of time as an annulus. The function is good for visualising how
#' concentrations of pollutants vary by wind direction and a time period e.g. by
#' month, day of week.
#'
#' The `polarAnnulus` function shares many of the properties of the
#' `polarPlot`. However, `polarAnnulus` is focussed on displaying
#' information on how concentrations of a pollutant (values of another variable)
#' vary with wind direction and time. Plotting as an annulus helps to reduce
#' compression of information towards the centre of the plot. The circular plot
#' is easy to interpret because wind direction is most easily understood in
#' polar rather than Cartesian coordinates.
#'
#' The inner part of the annulus represents the earliest time and the outer part
#' of the annulus the latest time. The time dimension can be shown in many ways
#' including "trend", "hour" (hour or day), "season" (month of the year) and
#' "weekday" (day of the week). Taking hour as an example, the plot will show
#' how concentrations vary by hour of the day and wind direction. Such plots can
#' be very useful for understanding how different source influences affect a
#' location.
#'
#' For `type = "trend"` the amount of smoothing does not vary linearly with
#' the length of the time series i.e. a certain amount of smoothing per unit
#' interval in time. This is a deliberate choice because should one be
#' interested in a subset (in time) of data, more detail will be provided for
#' the subset compared with the full data set. This allows users to investigate
#' specific periods in more detail. Full flexibility is given through the
#' smoothing parameter `k`.
#'
#' @inheritParams polarPlot
#' @param mydata A data frame minimally containing `date`, `wd` and a
#'   pollutant.
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`. There can also
#'   be more than one pollutant specified e.g. `pollutant = c("nox",
#'   "no2")`. The main use of using two or more pollutants is for model
#'   evaluation where two species would be expected to have similar
#'   concentrations. This saves the user stacking the data and it is possible to
#'   work with columns of data directly. A typical use would be `pollutant
#'   = c("obs", "mod")` to compare two columns \dQuote{obs} (the observations)
#'   and \dQuote{mod} (modelled values).
#' @param resolution Two plot resolutions can be set: \dQuote{normal} and
#'   \dQuote{fine} (the default).
#' @param local.tz Should the results be calculated in local time that includes
#'   a treatment of daylight savings time (DST)? The default is not to consider
#'   DST issues, provided the data were imported without a DST offset. Emissions
#'   activity tends to occur at local time e.g. rush hour is at 8 am every day.
#'   When the clocks go forward in spring, the emissions are effectively
#'   released into the atmosphere typically 1 hour earlier during the summertime
#'   i.e. when DST applies. When plotting diurnal profiles, this has the effect
#'   of \dQuote{smearing-out} the concentrations. Sometimes, a useful approach
#'   is to express time as local time. This correction tends to produce
#'   better-defined diurnal profiles of concentration (or other variables) and
#'   allows a better comparison to be made with emissions/activity data. If set
#'   to `FALSE` then GMT is used. Examples of usage include `local.tz
#'   = "Europe/London"`, `local.tz = "America/New_York"`. See
#'   `cutData` and `import` for more details.
#' @param period This determines the temporal period to consider. Options are
#'   \dQuote{hour} (the default, to plot diurnal variations), \dQuote{season} to
#'   plot variation throughout the year, \dQuote{weekday} to plot day of the
#'   week variation and \dQuote{trend} to plot the trend by wind direction.
#' @param type `type` determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce a single plot using the
#'   entire data. Type can be one of the built-in types as detailed in
#'   `cutData` e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so
#'   on. For example, `type = "season"` will produce four plots --- one for
#'   each season.
#'
#'   It is also possible to choose `type` as another variable in the data
#'   frame. If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Type can be up length two e.g. `type = c("season", "site")` will
#'   produce a 2x2 plot split by season and site. The use of two types is mostly
#'   meant for situations where there are several sites. Note, when two types
#'   are provided the first forms the columns and the second the rows.
#'
#'   Also note that for the `polarAnnulus` function some type/period
#'   combinations are forbidden or make little sense. For example, `type =
#'   "season"` and `period = "trend"` (which would result in a plot with
#'   too many gaps in it for sensible smoothing), or `type = "weekday"` and
#'   `period = "weekday"`.
#'
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be \dQuote{mean} (default), \dQuote{median},
#'   \dQuote{max} (maximum), \dQuote{frequency}. \dQuote{stdev} (standard
#'   deviation), \dQuote{weighted.mean} or \dQuote{cpf} (Conditional Probability
#'   Function). Because of the smoothing involved, the colour scale for some of
#'   these statistics is only to provide an indication of overall pattern and
#'   should not be interpreted in concentration units e.g. for `statistic =
#'   "weighted.mean"` where the bin mean is multiplied by the bin frequency and
#'   divided by the total frequency. In many cases using `polarFreq` will
#'   be better. Setting `statistic = "weighted.mean"` can be useful because
#'   it provides an indication of the concentration * frequency of occurrence
#'   and will highlight the wind speed/direction conditions that dominate the
#'   overall mean.
#' @param percentile If `statistic = "percentile"` or `statistic =
#'   "cpf"` then `percentile` is used, expressed from 0 to 100. Note that
#'   the percentile value is calculated in the wind speed, wind direction
#'   \sQuote{bins}. For this reason it can also be useful to set `min.bin`
#'   to ensure there are a sufficient number of points available to estimate a
#'   percentile. See `quantile` for more details of how percentiles are
#'   calculated.
#' @param width The width of the annulus; can be \dQuote{normal} (the default),
#'   \dQuote{thin} or \dQuote{fat}.
#' @param date.pad For `type = "trend"` (default), `date.pad = TRUE`
#'   will pad-out missing data to the beginning of the first year and the end of
#'   the last year. The purpose is to ensure that the trend plot begins and ends
#'   at the beginning or end of year.
#' @param k The smoothing value supplied to `gam` for the temporal and wind
#'   direction components, respectively. In some cases e.g. a trend plot with
#'   less than 1-year of data the smoothing with the default values may become
#'   too noisy and affected more by outliers. Choosing a lower value of `k`
#'   (say 10) may help produce a better plot.
#' @param ... Other graphical parameters passed onto `lattice:levelplot`
#'   and `cutData`. For example, `polarAnnulus` passes the option
#'   `hemisphere = "southern"` on to `cutData` to provide southern
#'   (rather than default northern) hemisphere handling of `type =
#'   "season"`. Similarly, common axis and title labelling options (such as
#'   `xlab`, `ylab`, `main`) are passed to `levelplot` via
#'   `quickText` to handle routine formatting.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family polar directional analysis functions
#' @examples
#' # diurnal plot for PM10 at Marylebone Rd
#' \dontrun{
#' polarAnnulus(mydata,
#'   pollutant = "pm10",
#'   main = "diurnal variation in pm10 at Marylebone Road"
#' )
#' }
#'
#' # seasonal plot for PM10 at Marylebone Rd
#' \dontrun{
#' polarAnnulus(mydata, poll = "pm10", period = "season")
#' }
#'
#' # trend in coarse particles (PMc = PM10 - PM2.5), calculate PMc first
#'
#' mydata$pmc <- mydata$pm10 - mydata$pm25
#' \dontrun{
#' polarAnnulus(mydata,
#'   poll = "pmc", period = "trend",
#'   main = "trend in pmc at Marylebone Road"
#' )
#' }
polarAnnulus <-
  function(
    mydata,
    pollutant = "nox",
    resolution = "fine",
    local.tz = NULL,
    period = "hour",
    type = "default",
    statistic = "mean",
    percentile = NA,
    limits = NULL,
    cols = "default",
    width = "normal",
    min.bin = 1,
    exclude.missing = TRUE,
    date.pad = FALSE,
    force.positive = TRUE,
    k = c(20, 10),
    normalise = FALSE,
    key.header = statistic,
    key.footer = pollutant,
    key.position = "right",
    key = TRUE,
    auto.text = TRUE,
    alpha = 1,
    plot = TRUE,
    ...
  ) {
    ## get rid of R check annoyances
    wd <- u <- v <- z <- all.dates <- NULL

    if (
      !statistic %in%
        c(
          "mean",
          "median",
          "frequency",
          "max",
          "stdev",
          "weighted.mean",
          "percentile",
          "cpf"
        )
    ) {
      stop(paste("statistic '", statistic, "' not recognised", sep = ""))
    }

    if (statistic == "percentile" & is.na(percentile & statistic != "cpf")) {
      warning("percentile value missing,  using 50")
      percentile <- 50
    }

    if (key.header[1] == "weighted.mean") {
      key.header <- c("weighted", "mean")
    }
    if (key.header[1] == "percentile") {
      key.header <- c(paste(percentile, "th", sep = ""), "percentile")
    }
    if (key.header[1] == "cpf") {
      key.header <- c("CPF", "probability")
    }

    ## extract variables of interest
    vars <- c("wd", "date", pollutant)

    if (period == "trend" & "season" %in% type) {
      stop("Cannot have same type as 'season' and period as 'trend'.")
    }
    if (length(type) > 2) {
      stop("Cannot have more than two types.")
    }

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
      trellis.par.set(list(strip.background = list(col = "white")))
    }

    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")

    ## reset graphic parameters
    on.exit(trellis.par.set(
      fontsize = current.font
    ))

    ## extra.args setup
    extra.args <- list(...)

    # label controls
    extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
      quickText(extra.args$xlab, auto.text)
    } else {
      quickText("", auto.text)
    }
    extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
      quickText(extra.args$ylab, auto.text)
    } else {
      quickText("", auto.text)
    }
    extra.args$main <- if ("main" %in% names(extra.args)) {
      quickText(extra.args$main, auto.text)
    } else {
      quickText("", auto.text)
    }

    if ("fontsize" %in% names(extra.args)) {
      trellis.par.set(fontsize = list(text = extra.args$fontsize))
    }

    ## check data
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    ## if more than one pollutant, need to stack the data and set type = "variable"
    ## this case is most relevent for model-measurement compasrions where data are in columns
    if (length(pollutant) > 1) {
      mydata <- gather(
        mydata,
        key = variable,
        value = value,
        pollutant,
        factor_key = TRUE
      )
      ## now set pollutant to "value"
      pollutant <- "value"
      type <- "variable"
    }

    d <- 10 ## d + upper = 1/2 width of annulus; do not need to adjust

    if (width == "normal") {
      upper <- 10
    }
    if (width == "thin") {
      upper <- 15
    }
    if (width == "fat") {
      upper <- 5
    }

    ## add extra wds - reduces discontinuity at 0/360
    zero.wd <- subset(mydata, wd == 360)

    if (nrow(zero.wd) > 0) {
      zero.wd$wd <- 0
      mydata <- rbind(mydata, zero.wd)
    }

    ## remove NAs
    mydata <- na.omit(mydata)
    mydata <- cutData(mydata, type, ...)

    ## convert to local time
    if (!is.null(local.tz)) {
      attr(mydata$date, "tzone") <- local.tz
    }

    ## for resolution of grid plotting (default = 0.2; fine = 0.1)
    if (resolution == "normal") {
      int <- 0.2
    }
    if (resolution == "fine") {
      int <- 0.1
    }
    if (resolution == "ultra.fine") {
      int <- 0.05
    } # very large files!

    len.int <- 20 / int + 1 ## number of x and y points to make up surfacexb

    ## for CPF
    Pval <- quantile(
      mydata[[pollutant]],
      probs = percentile / 100,
      na.rm = TRUE
    )

    if (statistic == "cpf") {
      sub <- paste(
        "CPF probability at the ",
        percentile,
        "th percentile (=",
        round(Pval, 1),
        ")",
        sep = ""
      )
    } else {
      sub <- NULL
    }

    prepare.grid <- function(mydata) {
      ## for padding to beginning of first year, end of last year
      if (date.pad) {
        min.year <- as.numeric(format(min(mydata$date, na.rm = TRUE), "%Y"))
        max.year <- as.numeric(format(max(mydata$date, na.rm = TRUE), "%Y"))

        all.dates <- data.frame(
          date = seq(
            ISOdate(min.year, 1, 1, 0, 0, 0, tz = "GMT"),
            ISOdate(max.year, 12, 31, 23, 0, 0, tz = "GMT"),
            by = "hour"
          )
        )

        all.dates <- data.frame(date = all.dates)
      }

      ## different date components, others available
      if (period == "trend") {
        if (date.pad) {
          ## for new limits with padding
          day <- as.numeric(format(all.dates$date, "%j"))
          year <- as.numeric(format(all.dates$date, "%Y"))
          trend2 <- year + day / 366
          min.trend <- min(trend2, na.rm = TRUE)
          max.trend <- max(trend2, na.rm = TRUE)

          ## actual data
          day <- as.numeric(format(mydata$date, "%j"))
          year <- as.numeric(format(mydata$date, "%Y"))
          trend <- year + day / 366
        } else {
          year <- as.numeric(format(mydata$date, "%Y"))
          day <- as.numeric(format(mydata$date, "%j"))
          trend <- year + day / 366
          min.trend <- min(trend, na.rm = TRUE)
          max.trend <- max(trend, na.rm = TRUE)
        }
      }

      if (period == "weekday") {
        hour <- as.numeric(format(mydata$date, "%H"))
        weekday <- as.numeric(format(mydata$date, "%w"))
        trend <- weekday + hour / 23
        min.trend <- 0
        max.trend <- 7
      }

      if (period == "season") {
        week <- as.numeric(format(mydata$date, "%W"))
        trend <- week
        min.trend <- 0
        max.trend <- 53
      }

      if (period == "hour") {
        hour <- as.numeric(format(mydata$date, "%H"))
        trend <- hour
        min.trend <- 0
        max.trend <- 23
      }

      trend <- 10 * (trend - min.trend) / (max.trend - min.trend)

      mydata <- cbind(mydata, trend)

      time.seq <- seq(0, 10, length = 24)

      wd <- seq(from = 0, to = 360, 10) # wind directions from 10 to 360
      ws.wd <- expand.grid(time.seq = time.seq, wd = wd)

      ## identify which ws and wd bins the data belong
      ## wd.cut <- cut(mydata$wd, seq(0, 360, 10))
      wd.cut <- cut(mydata$wd, seq(0, 360, length = 38), include.lowest = TRUE)

      ## divide-up the data for the annulus
      time.cut <- cut(
        mydata$trend,
        seq(0, 10, length = 25),
        include.lowest = TRUE
      )

      #   binned <- tapply(mydata[, pollutant], list(time.cut, wd.cut), mean, na.rm = TRUE)

      binned <- switch(
        statistic,
        frequency = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            length(na.omit(x))
          }
        ),
        mean = tapply(mydata[, pollutant], list(time.cut, wd.cut), function(x) {
          mean(x, na.rm = TRUE)
        }),
        median = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            median(x, na.rm = TRUE)
          }
        ),
        max = tapply(mydata[, pollutant], list(time.cut, wd.cut), function(x) {
          max(x, na.rm = TRUE)
        }),
        stdev = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            sd(x, na.rm = TRUE)
          }
        ),
        cpf = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) (length(which(x > Pval)) / length(x))
        ),
        weighted.mean = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) (mean(x) * length(x) / nrow(mydata))
        ),
        percentile = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            quantile(x, probs = percentile / 100, na.rm = TRUE)
          }
        )
      )

      binned <- as.vector(binned)

      ## frequency - remove points with freq < min.bin
      bin.len <- tapply(mydata[, pollutant], list(time.cut, wd.cut), length)
      binned.len <- as.vector(bin.len)

      ids <- which(binned.len < min.bin)
      binned[ids] <- NA

      ## data to predict over
      time.seq <- ws.wd$time.seq
      wd <- ws.wd$wd

      input.data <- expand.grid(
        time.seq = seq(0, 10, length = len.int),
        wd = seq(0, 360, length = len.int)
      )
      ###################### Smoothing#################################################

      ## run GAM to make a smooth surface
      if (force.positive) {
        n <- 0.5
      } else {
        n <- 1
      }

      input <- data.frame(binned, time.seq, wd)

      ## note use of cyclic smooth for the wind direction component
      Mgam <- gam(
        binned^n ~ te(time.seq, wd, k = k, bs = c("tp", "cc")),
        data = input
      )

      pred <- predict.gam(Mgam, input.data)
      pred <- pred^(1 / n)

      input.data <- cbind(input.data, pred)

      if (exclude.missing) {
        ## exclude predictions too far from data (from mgcv)
        x <- seq(0, 10, length = len.int)
        y <- seq(0, 360, length = len.int)
        res <- len.int
        wsp <- rep(x, res)
        wdp <- rep(y, rep(res, res))

        #  ind <- exclude.too.far(wsp, wdp, mydata$trend, mydata$wd, dist = 0.03)
        ## data with gaps caused by min.bin
        all.data <- na.omit(data.frame(
          time.seq = ws.wd$time.seq,
          wd = ws.wd$wd,
          binned
        ))
        ind <- with(
          all.data,
          exclude.too.far(wsp, wdp, time.seq, wd, dist = 0.03)
        )

        input.data$pred[ind] <- NA
        pred <- input.data$pred
      }

      #############################################################################
      ## transform to new coords - probably more efficient way of doing this
      ## need to transform to/from annulus to a cartesian coords

      ## new grid for plotting (the annulus)
      new.data <- expand.grid(
        u = seq(-upper - d, upper + d, by = int),
        v = seq(-upper - d, upper + d, by = int),
        z = NA,
        wd = NA,
        time.val = NA
      )

      new.data$id <- seq(nrow(new.data))

      ## calculate wd and time.val in on annulus in original units (helps with debugging)
      ## essentially start with final grid (annulus) and work backwards to find original data point in
      ## original coordinates
      new.data <- within(new.data, time.val <- (u^2 + v^2)^0.5 - upper)
      new.data <- within(new.data, wd <- 180 * atan2(u, v) / pi)
      new.data <- within(new.data, wd[wd < 0] <- wd[wd < 0] + 360) ## correct negative wds

      ## remove ids outside range
      ids <- with(
        new.data,
        which((u^2 + v^2)^0.5 > d + upper | (u^2 + v^2)^0.5 < upper)
      )
      new.data$wd[ids] <- NA
      new.data$time.val[ids] <- NA

      ## ids where there are data
      ids <- new.data$id[!is.na(new.data$wd)]

      ## indexes in original (cartesian) coordinates
      id.time <- round((2 * d / int) * new.data$time.val[ids] / 10) + 1
      id.wd <- round((2 * d / int) * new.data$wd[ids] / 360) + 1

      ## predicted values as a matrix
      pred <- matrix(pred, nrow = len.int, ncol = len.int)

      ## match the ids with predictions
      new.data$z[ids] <- sapply(
        seq_along(ids),
        function(x) pred[id.time[x], id.wd[x]]
      )

      new.data
    }

    ## more compact way?  Need to test
    results.grid <- mydata %>%
      group_by(across(type)) %>%
      nest() %>%
      mutate(data = purrr::map(data, prepare.grid)) %>%
      unnest(cols = c(data))

    ## normalise by divining by mean conditioning value if needed
    if (normalise) {
      results.grid <- results.grid %>%
        group_by(across(type)) %>%
        mutate(z = z / mean(z, na.rm = TRUE))

      if (missing(key.footer)) key.footer <- "normalised \nlevel"
    }

    ## proper names of labelling ###################################################
    strip.dat <- strip.fun(results.grid, type, auto.text)
    strip <- strip.dat[[1]]
    strip.left <- strip.dat[[2]]
    pol.name <- strip.dat[[3]]

    ## auto-scaling
    nlev <- 200 # preferred number of intervals
    ## handle missing breaks arguments
    if (any(is.null(limits)) | any(is.na(limits))) {
      # breaks <- pretty(results.grid$z, n = nlev)
      breaks <- seq(
        min(results.grid$z, na.rm = TRUE),
        max(results.grid$z, na.rm = TRUE),
        length.out = nlev
      )
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs
    } else {
      ## handle user limits and clipping
      breaks <- seq(min(limits), max(limits), length.out = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs

      ## case where user max is < data max
      if (max(limits) < max(results.grid[["z"]], na.rm = TRUE)) {
        id <- which(results.grid[["z"]] > max(limits))
        results.grid[["z"]][id] <- max(limits)
        labs[length(labs)] <- paste(">", labs[length(labs)])
      }

      ## case where user min is > data min
      if (min(limits) > min(results.grid[["z"]], na.rm = TRUE)) {
        id <- which(results.grid[["z"]] < min(limits))
        results.grid[["z"]][id] <- min(limits)
        labs[1] <- paste("<", labs[1])
      }
    }

    nlev2 <- length(breaks)

    col <- openColours(cols, (nlev2 - 1))
    col.scale <- breaks

    #################
    ## scale key setup
    #################
    legend <- list(
      col = col,
      at = col.scale,
      labels = list(labels = labs, at = at),
      space = key.position,
      auto.text = auto.text,
      footer = key.footer,
      header = key.header,
      height = 1,
      width = 1.5,
      fit = "all"
    )

    legend <- makeOpenKeyLegend(key, legend, "polarAnnulus")

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("z ~ u * v | ", temp, sep = ""))

    levelplot.args <- list(
      x = myform,
      results.grid,
      axes = FALSE,
      as.table = TRUE,
      aspect = 1,
      colorkey = FALSE,
      legend = legend,
      at = col.scale,
      col.regions = col,
      par.strip.text = list(cex = 0.8),
      scales = list(draw = FALSE),
      strip = strip,
      sub = sub,
      len <- upper + d + 3,
      xlim = c(-len, len),
      ylim = c(-len, len),
      panel = function(x, y, z, subscripts, ...) {
        panel.levelplot(
          x,
          y,
          z,
          subscripts,
          at = col.scale,
          lwd = 1,
          col.regions = col,
          labels = FALSE,
          alpha.regions = alpha
        )

        ## add axis line to central polarPlot
        llines(c(upper, upper + d), c(0, 0), col = "grey20")
        llines(c(0, 0), c(-upper, -upper - d), col = "grey20")

        ## add axis line to central polarPlot
        llines(c(0, 0), c(upper, upper + d), col = "grey20")
        llines(c(-upper, -upper - d), c(0, 0), col = "grey20")

        add.tick <- function(n, start = 0, end = 0) {
          ## start is an offset if time series does not begin in January

          ## left
          lsegments(
            seq(-upper - start, -upper - d + end, length = n),
            rep(-.5, n),
            seq(-upper - start, -upper - d + end, length = n),
            rep(.5, n),
            col = "grey20"
          )
          ## top
          lsegments(
            rep(-.5, n),
            seq(upper + start, upper + d - end, length = n),
            rep(.5, n),
            seq(upper + start, upper + d - end, length = n)
          )
          ## right
          lsegments(
            seq(upper + start, upper + d - end, length = n),
            rep(-.5, n),
            seq(upper + start, upper + d - end, length = n),
            rep(.5, n),
            col = "grey20"
          )
          ## bottom
          lsegments(
            rep(-.5, n),
            seq(-upper - start, -upper - d + end, length = n),
            rep(.5, n),
            seq(-upper - start, -upper - d + end, length = n)
          )
        }

        label.axis <- function(x, lab1, lab2, ticks) {
          ltext(x, upper, lab1, cex = 0.7, pos = 4)
          ltext(x, upper + d, lab2, cex = 0.7, pos = 4)
          ## at bottom
          ltext(-x, -upper, lab1, cex = 0.7, pos = 2)
          ltext(-x, -upper - d, lab2, cex = 0.7, pos = 2)
          add.tick(ticks)
        }

        if (period == "trend") {
          if (date.pad) {
            date.start <- min(all.dates$date)
            date.end <- max(all.dates$date)
          } else {
            date.start <- min(mydata$date)
            date.end <- max(mydata$date)
          }

          label.axis(
            0,
            format(date.start, "%d-%b-%Y"),
            format(date.end, "%d-%b-%Y"),
            2
          )

          ## add ticks at 1-Jan each year (could be missing dates)
          days <- seq(as.Date(date.start), as.Date(date.end), by = "day")

          if (length(days) > 365) {
            ## find number of Januarys
            num.jans <- which(format(days, "%j") == "001")
            ticks <- length(num.jans)
            start <- 10 * (num.jans[1] - 1) / length(days)
            end <- 10 - 10 * (num.jans[ticks] - 1) / length(days)
            if (length(num.jans) > 0) add.tick(ticks, start, end)

            ## mark montly intervals (approx)
          } else {
            ## find number of 01s
            num.jans <- which(format(days, "%d") == "01")
            ticks <- length(num.jans)
            start <- 10 * (num.jans[1] - 1) / length(days)
            end <- 10 - 10 * (num.jans[ticks] - 1) / length(days)
            if (length(num.jans) > 0) add.tick(ticks, start, end)
          }
        }

        if (period == "season") {
          label.axis(
            0,
            format(ISOdate(2000, 1, 1), "%B"),
            format(ISOdate(2000, 12, 1), "%B"),
            13
          )
        }

        if (period == "hour") {
          label.axis(0, "0", "23", 7)
        }

        if (period == "weekday") {
          local.weekdays <- format(ISOdate(2000, 1, 1:14), "%A")[order(format(
            ISOdate(2000, 1, 1:14),
            "%w"
          ))]
          loc.sunday <- local.weekdays[1]
          loc.saturday <- local.weekdays[length(local.weekdays)]
          label.axis(0, loc.sunday, loc.saturday, 8)
        }

        ## text for directions
        ltext(-upper - d - 1.5, 0, "W", cex = 0.7)
        ltext(0, -upper - d - 1.5, "S", cex = 0.7)
        ltext(0, upper + d + 1.5, "N", cex = 0.7)
        ltext(upper + d + 1.5, 0, "E", cex = 0.7)
      }
    )

    # reset for extra.args
    levelplot.args <- listUpdate(levelplot.args, extra.args)

    # plot
    plt <- do.call(levelplot, levelplot.args)

    #################
    # output
    #################
    if (plot) {
      if (length(type) == 1) {
        plot(plt)
      } else {
        plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
      }
    }
    newdata <- results.grid
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    invisible(output)
  }
