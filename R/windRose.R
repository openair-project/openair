#' Pollution rose variation of the traditional wind rose plot
#'
#' The traditional wind rose plot that plots wind speed and wind direction by
#' different intervals. The pollution rose applies the same plot structure but
#' substitutes other measurements, most commonly a pollutant time series, for
#' wind speed.
#'
#' [pollutionRose()] is a [windRose()] wrapper which brings `pollutant`
#' forward in the argument list, and attempts to sensibly rescale break points
#' based on the `pollutant` data range by by-passing `ws.int`.
#'
#' By default, [pollutionRose()] will plot a pollution rose of `nox` using
#' "wedge" style segments and placing the scale key to the right of the plot.
#'
#' It is possible to compare two wind speed-direction data sets using
#' [pollutionRose()]. There are many reasons for doing so e.g. to see how one
#' site compares with another or for meteorological model evaluation. In this
#' case, `ws` and `wd` are considered to the the reference data sets
#' with which a second set of wind speed and wind directions are to be compared
#' (`ws2` and `wd2`). The first set of values is subtracted from the
#' second and the differences compared. If for example, `wd2` was biased
#' positive compared with `wd` then `pollutionRose` will show the bias
#' in polar coordinates. In its default use, wind direction bias is colour-coded
#' to show negative bias in one colour and positive bias in another.
#'
#' @inheritParams windRose
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`.
#' @param breaks Most commonly, the number of break points for pollutant
#'   concentrations. The default, 6, attempts to breaks the supplied data at
#'   approximately 6 sensible break points. However, `breaks` can also be
#'   used to set specific break points. For example, the argument `breaks =
#'   c(0, 1, 10, 100)` breaks the data into segments <1, 1-10, 10-100, >100.
#' @inheritDotParams windRose -pollutant -key.footer -key.position -key -breaks
#'   -paddle -seg -normalise -plot
#' @export
#' @return an [openair][openair-package] object. Summarised proportions can be
#'   extracted directly using the `$data` operator, e.g.
#'   `object$data` for `output <- windRose(mydata)`. This returns a
#'   data frame with three set columns: `cond`, conditioning based on
#'   `type`; `wd`, the wind direction; and `calm`, the
#'   `statistic` for the proportion of data unattributed to any specific
#'   wind direction because it was collected under calm conditions; and then
#'   several (one for each range binned for the plot) columns giving proportions
#'   of measurements associated with each `ws` or `pollutant` range
#'   plotted as a discrete panel.
#' @family polar directional analysis functions
#' @examples
#' # pollutionRose of nox
#' pollutionRose(mydata, pollutant = "nox")
#'
#' ## source apportionment plot - contribution to mean
#' \dontrun{
#' pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")
#' }
#'
#' ## example of comparing 2 met sites
#' ## first we will make some new ws/wd data with a postive bias
#' mydata$ws2 <- mydata$ws + 2 * rnorm(nrow(mydata)) + 1
#' mydata$wd2 <- mydata$wd + 30 * rnorm(nrow(mydata)) + 30
#'
#' ## need to correct negative wd
#' id <- which(mydata$wd2 < 0)
#' mydata$wd2[id] <- mydata$wd2[id] + 360
#'
#' ## results show postive bias in wd and ws
#' pollutionRose(mydata, ws = "ws", wd = "wd", ws2 = "ws2", wd2 = "wd2")
pollutionRose <- function(
  mydata,
  pollutant = "nox",
  key.footer = pollutant,
  key.position = "right",
  key = TRUE,
  breaks = 6,
  paddle = FALSE,
  seg = 0.9,
  normalise = FALSE,
  alpha = 1,
  plot = TRUE,
  ...
) {
  ## extra args setup
  extra <- list(...)

  ## check to see if two met data sets are being compared.
  ## if so, set pollutant to one of the names
  if ("ws2" %in% names(extra)) {
    pollutant <- extra$ws
    if (missing(breaks)) breaks <- NA
  }

  if (is.null(breaks)) {
    breaks <- 6
  }

  if (is.numeric(breaks) & length(breaks) == 1) {
    ## breaks from the minimum to 90th percentile, which generally gives sensible
    ## spacing for skewed data. Maximum is added later.
    breaks <- unique(pretty(
      c(
        min(mydata[[pollutant]], na.rm = TRUE),
        quantile(mydata[[pollutant]], probs = 0.9, na.rm = TRUE)
      ),
      breaks
    ))
  }

  windRose(
    mydata,
    pollutant = pollutant,
    paddle = paddle,
    seg = seg,
    key.position = key.position,
    key.footer = key.footer,
    key = key,
    breaks = breaks,
    normalise = normalise,
    alpha = alpha,
    plot = plot,
    ...
  )
}

#' Traditional wind rose plot
#'
#' The traditional wind rose plot that plots wind speed and wind direction by
#' different intervals. The pollution rose applies the same plot structure but
#' substitutes other measurements, most commonly a pollutant time series, for
#' wind speed.
#'
#' For `windRose` data are summarised by direction, typically by 45 or 30
#' (or 10) degrees and by different wind speed categories. Typically, wind
#' speeds are represented by different width "paddles". The plots show the
#' proportion (here represented as a percentage) of time that the wind is from a
#' certain angle and wind speed range.
#'
#' By default `windRose` will plot a windRose in using "paddle" style
#' segments and placing the scale key below the plot.
#'
#' The argument `pollutant` uses the same plotting structure but
#' substitutes another data series, defined by `pollutant`, for wind speed.
#' It is recommended to use [pollutionRose()] for plotting pollutant
#' concentrations.
#'
#' The option `statistic = "prop.mean"` provides a measure of the relative
#' contribution of each bin to the panel mean, and is intended for use with
#' `pollutionRose`.
#'
#' @param mydata A data frame containing fields `ws` and `wd`
#' @param ws Name of the column representing wind speed.
#' @param wd Name of the column representing wind direction.
#' @param ws2,wd2 The user can supply a second set of wind speed and wind
#'   direction values with which the first can be compared. See
#'   [pollutionRose()] for more details.
#' @param ws.int The Wind speed interval. Default is 2 m/s but for low met masts
#'   with low mean wind speeds a value of 1 or 0.5 m/s may be better.
#' @param angle Default angle of \dQuote{spokes} is 30. Other potentially useful
#'   angles are 45 and 10. Note that the width of the wind speed interval may
#'   need adjusting using `width`.
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
#'   Type can be up length two e.g. `type = c("season", "weekday")` will
#'   produce a 2x2 plot split by season and day of the week. Note, when two
#'   types are provided the first forms the columns and the second the rows.
#' @param calm.thresh By default, conditions are considered to be calm when the
#'   wind speed is zero. The user can set a different threshold for calms be
#'   setting `calm.thresh` to a higher value. For example,
#'   `calm.thresh = 0.5` will identify wind speeds **below** 0.5 as calm.
#' @param bias.corr When `angle` does not divide exactly into 360 a bias is
#'   introduced in the frequencies when the wind direction is already supplied
#'   rounded to the nearest 10 degrees, as is often the case. For example, if
#'   `angle = 22.5`, N, E, S, W will include 3 wind sectors and all other
#'   angles will be two. A bias correction can made to correct for this problem.
#'   A simple method according to Applequist (2012) is used to adjust the
#'   frequencies.
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet},
#'   \dQuote{hue} and user defined. For user defined the user can supply a list
#'   of colour names recognised by R (type `colours()` to see the full
#'   list). An example would be `cols = c("yellow", "green", "blue",
#'   "black")`.
#' @param grid.line Grid line interval to use. If `NULL`, as in default,
#'   this is assigned based on the available data range. However, it can also be
#'   forced to a specific value, e.g. `grid.line = 10`. `grid.line`
#'   can also be a list to control the interval, line type and colour. For
#'   example `grid.line = list(value = 10, lty = 5, col = "purple")`.
#' @param width For `paddle = TRUE`, the adjustment factor for width of
#'   wind speed intervals. For example, `width = 1.5` will make the paddle
#'   width 1.5 times wider.
#' @param seg When `paddle = TRUE`, `seg` determines with width of the
#'   segments. For example, `seg = 0.5` will produce segments 0.5 *
#'   `angle`.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels will automatically try and format pollutant names
#'   and units properly, e.g., by subscripting the \sQuote{2} in NO2.
#' @param breaks Most commonly, the number of break points for wind speed. With
#'   the `ws.int` default of 2 m/s, the `breaks` default, 4, generates
#'   the break points 2, 4, 6, 8 m/s. However, `breaks` can also be used to
#'   set specific break points. For example, the argument `breaks = c(0, 1,
#'   10, 100)` breaks the data into segments <1, 1-10, 10-100, >100.
#' @param offset The size of the 'hole' in the middle of the plot, expressed as
#'   a percentage of the polar axis scale, default 10.
#' @param normalise If `TRUE` each wind direction segment is normalised to
#'   equal one. This is useful for showing how the concentrations (or other
#'   parameters) contribute to each wind sector when the proportion of time the
#'   wind is from that direction is low. A line showing the probability that the
#'   wind directions is from a particular wind sector is also shown.
#' @param max.freq Controls the scaling used by setting the maximum value for
#'   the radial limits. This is useful to ensure several plots use the same
#'   radial limits.
#' @param paddle Either `TRUE` or `FALSE`. If `TRUE` plots rose
#'   using 'paddle' style spokes. If `FALSE` plots rose using 'wedge' style
#'   spokes.
#' @param key.header Adds additional text/labels above the scale key. For
#'   example, passing `windRose(mydata, key.header = "ws")` adds the
#'   addition text as a scale header. Note: This argument is passed to
#'   [drawOpenKey()] via [quickText()], applying the auto.text argument, to
#'   handle formatting.
#' @param key.footer Adds additional text/labels below the scale key. See
#'   `key.header` for further information.
#' @param key.position Location where the scale key is to plotted. Allowed
#'   arguments currently include \dQuote{top}, \dQuote{right}, \dQuote{bottom}
#'   and \dQuote{left}.
#' @param key Fine control of the scale key via [drawOpenKey()].
#' @param dig.lab The number of significant figures at which scientific number
#'   formatting is used in break point and key labelling. Default 5.
#' @param include.lowest Logical. If `FALSE` (the default), the first
#'   interval will be left exclusive and right inclusive. If `TRUE`, the
#'   first interval will be left and right inclusive. Passed to the
#'   `include.lowest` argument of [cut()].
#' @param statistic The `statistic` to be applied to each data bin in the
#'   plot. Options currently include \dQuote{prop.count}, \dQuote{prop.mean} and
#'   \dQuote{abs.count}. The default \dQuote{prop.count} sizes bins according to
#'   the proportion of the frequency of measurements.  Similarly,
#'   \dQuote{prop.mean} sizes bins according to their relative contribution to
#'   the mean. \dQuote{abs.count} provides the absolute count of measurements in
#'   each bin.
#' @param pollutant Alternative data series to be sampled instead of wind speed.
#'   The [windRose()] default NULL is equivalent to `pollutant = "ws"`. Use
#'   in [pollutionRose()].
#' @param annotate If `TRUE` then the percentage calm and mean values are
#'   printed in each panel together with a description of the statistic below
#'   the plot. If `" "` then only the statistic is below the plot. Custom
#'   annotations may be added by setting value to `c("annotation 1",
#'   "annotation 2")`.
#' @param angle.scale The scale is by default shown at a 315 degree angle.
#'   Sometimes the placement of the scale may interfere with an interesting
#'   feature. The user can therefore set `angle.scale` to another value
#'   (between 0 and 360 degrees) to mitigate such problems. For example
#'   `angle.scale = 45` will draw the scale heading in a NE direction.
#' @param border Border colour for shaded areas. Default is no border.
#' @param alpha The alpha transparency to use for the plotting surface (a value
#'   between 0 and 1 with zero being fully transparent and 1 fully opaque).
#'   Setting a value below 1 can be useful when plotting surfaces on a map using
#'   the package `openairmaps`.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract plot components and plotting them in other ways.
#' @param ... Other parameters that are passed on to `drawOpenKey`,
#'   `lattice:xyplot` and `cutData`. Axis and title labelling options
#'   (`xlab`, `ylab`, `main`) are passed to `xyplot` via
#'   `quickText` to handle routine formatting.
#'
#' @export
#' @import dplyr
#' @return an [openair][openair-package] object. Summarised proportions can be
#'   extracted directly using the `$data` operator, e.g. `object$data`
#'   for `output <- windRose(mydata)`. This returns a data frame with three
#'   set columns: `cond`, conditioning based on `type`; `wd`, the
#'   wind direction; and `calm`, the `statistic` for the proportion of
#'   data unattributed to any specific wind direction because it was collected
#'   under calm conditions; and then several (one for each range binned for the
#'   plot) columns giving proportions of measurements associated with each
#'   `ws` or `pollutant` range plotted as a discrete panel.
#' @note `windRose` and `pollutionRose` both use [drawOpenKey()] to
#'   produce scale keys.
#' @author David Carslaw (with some additional contributions by Karl Ropkins)
#' @family polar directional analysis functions
#' @references
#'
#' Applequist, S, 2012: Wind Rose Bias Correction. J. Appl. Meteor. Climatol.,
#' 51, 1305-1309.
#'
#' Droppo,  J.G. and B.A. Napier (2008) Wind Direction Bias in Generating Wind
#' Roses and Conducting Sector-Based Air Dispersion Modeling, Journal of the Air
#' & Waste Management Association, 58:7, 913-918.
#'
#' @examples
#' # basic plot
#' windRose(mydata)
#'
#' # one windRose for each year
#' windRose(mydata, type = "year")
#'
#' # windRose in 10 degree intervals with gridlines and width adjusted
#' \dontrun{
#' windRose(mydata, angle = 10, width = 0.2, grid.line = 1)
#' }
windRose <- function(
  mydata,
  ws = "ws",
  wd = "wd",
  ws2 = NA,
  wd2 = NA,
  ws.int = 2,
  angle = 30,
  type = "default",
  calm.thresh = 0,
  bias.corr = TRUE,
  cols = "default",
  grid.line = NULL,
  width = 1,
  seg = NULL,
  auto.text = TRUE,
  breaks = 4,
  offset = 10,
  normalise = FALSE,
  max.freq = NULL,
  paddle = TRUE,
  key.header = NULL,
  key.footer = "(m/s)",
  key.position = "bottom",
  key = TRUE,
  dig.lab = 5,
  include.lowest = FALSE,
  statistic = "prop.count",
  pollutant = NULL,
  annotate = TRUE,
  angle.scale = 315,
  border = NA,
  alpha = 1,
  plot = TRUE,
  ...
) {
  if (is.null(seg)) {
    seg <- 0.9
  }

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    ## other local colours
    calm.col <- "black"
  } else {
    calm.col <- "forestgreen"
  }

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # make sure ws and wd and numeric
  mydata <- checkNum(mydata, vars = c(ws, wd))

  if (360 / angle != round(360 / angle)) {
    warning(
      "In windRose(...):\n  angle will produce some spoke overlap",
      "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.",
      call. = FALSE
    )
  }
  if (angle < 3) {
    warning(
      "In windRose(...):\n  angle too small",
      "\n  enforcing 'angle = 3'",
      call. = FALSE
    )
    angle <- 3
  }

  ## extra args setup
  extra <- list(...)

  ## label controls
  extra$xlab <- if ("xlab" %in% names(extra)) {
    quickText(extra$xlab, auto.text)
  } else {
    quickText("", auto.text)
  }
  extra$ylab <- if ("ylab" %in% names(extra)) {
    quickText(extra$ylab, auto.text)
  } else {
    quickText("", auto.text)
  }
  extra$main <- if ("main" %in% names(extra)) {
    quickText(extra$main, auto.text)
  } else {
    quickText("", auto.text)
  }

  if ("fontsize" %in% names(extra)) {
    trellis.par.set(fontsize = list(text = extra$fontsize))
  }

  ## preset statitistics

  if (is.character(statistic)) {
    ## allowed cases
    ok.stat <- c("prop.count", "prop.mean", "abs.count", "frequency")

    if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
      warning(
        "In windRose(...):\n  statistic unrecognised",
        "\n  enforcing statistic = 'prop.count'",
        call. = FALSE
      )
      statistic <- "prop.count"
    }

    if (statistic == "prop.count") {
      stat.fun <- length
      stat.unit <- "%"
      stat.scale <- "all"
      stat.lab <- "Frequency of counts by wind direction (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), digits = dig.lab)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }

    if (statistic == "prop.mean") {
      stat.fun <- function(x) sum(x, na.rm = TRUE)
      stat.unit <- "%"
      stat.scale <- "panel"
      stat.lab <- "Proportion contribution to the mean (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), digits = 5)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }

    if (statistic == "abs.count" | statistic == "frequency") {
      stat.fun <- length
      stat.unit <- ""
      stat.scale <- "none"
      stat.lab <- "Count by wind direction"
      stat.fun2 <- function(x) round(length(x), 0)
      stat.lab2 <- "count"
      stat.labcalm <- function(x) round(x, 0)
    }
  }

  if (is.list(statistic)) {
    ## IN DEVELOPMENT

    ## this section has no testing/protection
    ## but allows users to supply a function
    ## scale it by total data or panel
    ## convert proportions to percentage
    ## label it

    stat.fun <- statistic$fun
    stat.unit <- statistic$unit
    stat.scale <- statistic$scale
    stat.lab <- statistic$lab
    stat.fun2 <- statistic$fun2
    stat.lab2 <- statistic$lab2
    stat.labcalm <- statistic$labcalm
  }

  ## variables we need
  vars <- c(wd, ws)

  diff <- FALSE ## i.e. not two sets of ws/wd
  rm.neg <- TRUE ## will remove negative ws in check.prep

  ## case where two met data sets are to be compared
  if (!is.na(ws2) & !is.na(wd2)) {
    vars <- c(vars, ws2, wd2)
    diff <- TRUE
    rm.neg <- FALSE
    mydata$ws <- mydata[[ws2]] - mydata[[ws]]
    mydata$wd <- mydata[[wd2]] - mydata[[wd]]

    ## fix negative wd
    id <- which(mydata$wd < 0)
    if (length(id) > 0) {
      mydata$wd[id] <- mydata$wd[id] + 360
    }

    pollutant <- "ws"
    key.footer <- "ws"
    wd <- "wd"
    ws <- "ws"
    vars <- c("ws", "wd")
    if (missing(angle)) {
      angle <- 10
    }
    if (missing(offset)) {
      offset <- 20
    }
    ## set the breaks to cover all the data
    if (is.na(breaks[1])) {
      max.br <- max(ceiling(abs(c(
        min(mydata$ws, na.rm = TRUE),
        max(mydata$ws, na.rm = TRUE)
      ))))
      breaks <- c(-1 * max.br, 0, max.br)
    }

    if (missing(cols)) {
      cols <- c("lightskyblue", "tomato")
    }
    seg <- 1
  }

  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }

  if (!is.null(pollutant)) {
    vars <- c(vars, pollutant)
  }

  mydata <- cutData(mydata, type, ...)

  mydata <- checkPrep(
    mydata,
    vars,
    type,
    remove.calm = FALSE,
    remove.neg = rm.neg
  )

  # original data to use later
  mydata_orig <- mydata

  # remove lines where ws is missing
  # wd can be NA and ws 0 (calm)
  id <- which(is.na(mydata[[ws]]))

  if (length(id) > 0) {
    mydata <- mydata[-id, ]
  }

  if (is.null(pollutant)) {
    pollutant <- ws
  }

  mydata$x <- mydata[[pollutant]]

  mydata[[wd]] <- angle * ceiling(mydata[[wd]] / angle - 0.5)
  mydata[[wd]][mydata[[wd]] == 0] <- 360

  ## flag calms as negatives
  if (calm.thresh == 0) {
    mydata[[wd]][mydata[, ws] == 0] <- -999 ## set wd to flag where there are calms
  } else {
    mydata[[wd]][mydata[, ws] < calm.thresh] <- -999 ## Note < not <=
  }

  mydata[[wd]][mydata[, ws] < calm.thresh] <- -999 ## set wd to flag where there are calms
  ## do after rounding or -999 changes

  if (length(breaks) == 1) {
    breaks <- 0:(breaks - 1) * ws.int
  }

  if (max(breaks) < max(mydata$x, na.rm = TRUE)) {
    breaks <- c(breaks, max(mydata$x, na.rm = TRUE))
  }

  if (min(breaks) > min(mydata$x, na.rm = TRUE)) {
    warning("Some values are below minimum break.")
  }

  breaks <- unique(breaks)
  mydata$x <- cut(
    mydata$x,
    breaks = breaks,
    include.lowest = include.lowest,
    dig.lab = dig.lab
  )

  ## clean up cut intervals
  labs <- gsub("[(]|[)]|[[]|[]]", "", levels(mydata$x))
  labs <- gsub("[,]", " to ", labs)

  ## statistic handling

  prepare.grid <- function(mydata) {
    ## these are all calms...
    if (all(is.na(mydata$x))) {
      weights <- tibble(
        Interval1 = NA,
        wd = NA,
        calm = 100,
        panel.fun = NA,
        mean.wd = NA,
        freqs = NA
      )
    } else {
      levels(mydata$x) <- c(paste("Interval", 1:length(labs), sep = ""))

      all <- stat.fun(mydata[[wd]])
      calm <- mydata[mydata[[wd]] == -999, ][[pollutant]]

      calm <- stat.fun(calm)

      weights <- tapply(
        mydata[[pollutant]],
        list(mydata[[wd]], mydata$x),
        stat.fun
      )

      freqs <- tapply(mydata[[pollutant]], mydata[[wd]], length)

      ## scaling
      if (stat.scale == "all") {
        calm <- calm / all
        weights <- weights / all
      }

      if (stat.scale == "panel") {
        temp <- stat.fun(stat.fun(weights)) + calm
        calm <- calm / temp
        weights <- weights / temp
      }

      weights[is.na(weights)] <- 0
      weights <- t(apply(weights, 1, cumsum))

      if (stat.scale == "all" | stat.scale == "panel") {
        weights <- weights * 100
        calm <- calm * 100
      }

      panel.fun <- stat.fun2(mydata[[pollutant]])

      ## calculate mean wd - useful for cases comparing two met data sets
      u <- mean(sin(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
      v <- mean(cos(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
      mean.wd <- atan2(u, v) * 360 / 2 / pi

      if (all(is.na(mean.wd))) {
        mean.wd <- NA
      } else {
        if (mean.wd < 0) {
          mean.wd <- mean.wd + 360
        }
        ## show as a negative (bias)
        if (mean.wd > 180) mean.wd <- mean.wd - 360
      }

      weights <- bind_cols(
        as_tibble(weights),
        tibble(
          wd = as.numeric(row.names(weights)),
          calm = calm,
          panel.fun = panel.fun,
          mean.wd = mean.wd,
          freqs = freqs
        )
      )
    }

    weights
  }

  if (paddle) {
    poly <- function(wd, len1, len2, width, colour, x.off = 0, y.off = 0) {
      theta <- wd * pi / 180
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      x1 <- len1 * sin(theta) - width * cos(theta) + x.off
      x2 <- len1 * sin(theta) + width * cos(theta) + x.off
      x3 <- len2 * sin(theta) - width * cos(theta) + x.off
      x4 <- len2 * sin(theta) + width * cos(theta) + x.off
      y1 <- len1 * cos(theta) + width * sin(theta) + y.off
      y2 <- len1 * cos(theta) - width * sin(theta) + y.off
      y3 <- len2 * cos(theta) + width * sin(theta) + y.off
      y4 <- len2 * cos(theta) - width * sin(theta) + y.off
      lpolygon(
        c(x1, x2, x4, x3),
        c(y1, y2, y4, y3),
        col = colour,
        border = border
      )
    }
  } else {
    poly <- function(wd, len1, len2, width, colour, x.off = 0, y.off = 0) {
      len1 <- len1 + off.set
      len2 <- len2 + off.set

      theta <- seq(
        (wd - seg * angle / 2),
        (wd + seg * angle / 2),
        length.out = (angle - 2) * 10
      )
      theta <- ifelse(theta < 1, 360 - theta, theta)
      theta <- theta * pi / 180
      x1 <- len1 * sin(theta) + x.off
      x2 <- rev(len2 * sin(theta) + x.off)
      y1 <- len1 * cos(theta) + x.off
      y2 <- rev(len2 * cos(theta) + x.off)
      lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border)
    }
  }

  results <- mydata %>%
    group_by(across(type)) %>%
    do(prepare.grid(.))

  ## format
  results$calm <- stat.labcalm(results$calm)
  results$mean.wd <- stat.labcalm(results$mean.wd)

  # function to correct bias
  corr_bias <- function(results) {
    # check to see if data for this type combination are rounded to 10 degrees
    # round wd so that tiny differences between integer a numeric do not arise
    wd_select <- inner_join(mydata_orig, results[1, type], by = type)
    if (!all(round(wd_select[[wd]]) %% 10 == 0, na.rm = TRUE)) {
      return(results)
    }

    wds <- seq(10, 360, 10)
    tmp <- angle * ceiling(wds / angle - 0.5)
    id <- which(tmp == 0)
    if (length(id > 0)) {
      tmp[id] <- 360
    }
    tmp <- table(tmp) ## number of sectors spanned
    vars <- grep("Interval[1-9]", names(results)) ## the frequencies, without any calms

    # number of rows in data where wd != -999
    n_data <- nrow(filter(results, wd != -999))

    if (n_data > 0) {
      results[results[["wd"]] != -999, vars] <-
        results[results[["wd"]] != -999, vars] * mean(tmp) / tmp
    }

    return(results)
  }

  ## correction for bias when angle does not divide exactly into 360
  if (bias.corr) {
    results <- results %>%
      group_by(across(type)) %>%
      do(corr_bias(.))
  }

  ## proper names of labelling###########################################
  strip.dat <- strip.fun(results, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]

  if (length(labs) < length(cols)) {
    col <- cols[1:length(labs)]
  } else {
    col <- openColours(cols, length(labs))
  }

  legend_col <- col
  col <- grDevices::adjustcolor(col, alpha.f = alpha)

  ## normalise by sector

  if (normalise) {
    vars <- grep("Interval[1-9]", names(results))

    ## original frequencies, so we can plot the wind frequency line
    results$freq <- results[[max(vars)]]

    results$freq <- ave(
      results$freq,
      results[type],
      FUN = function(x) x / sum(x)
    )

    ## scale by maximum frequency
    results$norm <- results$freq / max(results$freq)

    ## normalise
    results[, vars] <- results[, vars] / results[[max(vars)]]

    stat.lab <- "Normalised by wind sector"
    stat.unit <- ""
  }

  if (is.null(max.freq)) {
    max.freq <- max(
      results[results$wd != -999, grep("Interval", names(results))],
      na.rm = TRUE
    )
  } else {
    max.freq <- max.freq
  }

  off.set <- max.freq * (offset / 100)
  box.widths <- seq(
    0.002^0.25,
    0.016^0.25,
    length.out = length(labs)
  )^4
  box.widths <- box.widths * max.freq * angle / 5

  ## key, colorkey, legend
  legend <- list(
    col = legend_col,
    space = key.position,
    auto.text = auto.text,
    labels = labs,
    footer = key.footer,
    header = key.header,
    height = 0.60,
    width = 1.5,
    fit = "scale",
    plot.style = if (paddle) "paddle" else "other"
  )

  legend <- makeOpenKeyLegend(key, legend, "windRose")

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Interval1 ~ wd | ", temp, sep = ""))

  # maximum annotation that covers the data
  mymax <- max(pretty(c(0, max.freq), 4))

  # check to see if grid.line is a list or not and set grid line properties
  grid.value <- NULL

  if (is.list(grid.line)) {
    if (is.null(grid.line[["value"]])) {
      grid.value <- NULL
    } else {
      grid.value <- grid.line[["value"]]
    }

    if (is.null(grid.line[["lty"]])) {
      grid.lty <- 1
    } else {
      grid.lty <- grid.line[["lty"]]
    }

    if (is.null(grid.line[["col"]])) {
      grid.col <- "grey85"
    } else {
      grid.col <- grid.line[["col"]]
    }
  } else {
    grid.value <- grid.line
    grid.lty <- 1
    grid.col <- "grey85"
  }

  myby <- if (is.null(grid.value)) pretty(c(0, mymax), 4)[2] else grid.value

  if (myby / mymax > 0.9) {
    myby <- mymax * 0.9
  }

  is_annotated <- any(annotate == TRUE) | any(is.character(annotate))

  if (is_annotated) {
    sub <- stat.lab
  } else {
    sub <- NULL
  }

  xy.args <- list(
    x = myform,
    xlim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
    ylim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
    data = results,
    type = "n",
    sub = sub,
    strip = strip,
    strip.left = strip.left,
    as.table = TRUE,
    aspect = 1,
    par.strip.text = list(cex = 0.8),
    scales = list(draw = FALSE),
    panel = function(x, y, subscripts, ...) {
      panel.xyplot(x, y, ...)
      angles <- seq(0, 2 * pi, length = 360)
      sapply(
        seq(off.set, mymax + off.set, by = myby),
        function(x) {
          llines(
            x * sin(angles),
            x * cos(angles),
            col = grid.col,
            lwd = 1,
            lty = grid.lty
          )
        }
      )

      dat <- results[subscripts, ] ## subset of data
      dat <- filter(dat, wd <= 360, wd >= 0)

      upper <- max.freq + off.set

      if (nrow(dat) > 0) {
        dat$Interval0 <- 0 ## make a lower bound to refer to

        for (i in 1:nrow(dat)) {
          ## go through wind angles 30, 60, ...

          for (j in seq_along(labs)) {
            ## go through paddles x1, x2, ...

            tmp <- paste(
              "poly(dat$wd[i], dat$Interval",
              j - 1,
              "[i], dat$Interval",
              j,
              "[i], width * box.widths[",
              j,
              "], col[",
              j,
              "])",
              sep = ""
            )

            eval(parse(text = tmp))
          }
        }
      }

      if (normalise) {
        panel.wdprob(dat, seg, angle, off.set)
      }

      ltext(
        seq((myby + off.set), (mymax + off.set), myby) *
          sin(pi * angle.scale / 180),
        seq((myby + off.set), (mymax + off.set), myby) *
          cos(pi * angle.scale / 180),
        paste(seq(myby, mymax, by = myby), stat.unit, sep = ""),
        cex = 0.7
      )

      # annotations
      if (annotate[1] == TRUE || length(annotate) == 2L || annotate[1] == " ") {
        if (annotate[1] == TRUE) {
          annotations_to_place <-
            paste0(
              stat.lab2,
              " = ",
              dat$panel.fun[1],
              "\n",
              "calm = ",
              dat$calm[1],
              stat.unit
            )
        }

        if (annotate[1] == " ") {
          annotations_to_place <-
            paste0(stat.lab2, " = ", dat$panel.fun[1])
        }

        if (length(annotate) == 2L) {
          annotations_to_place <-
            paste0(
              annotate[1],
              " = ",
              dat$panel.fun[1],
              "\n",
              annotate[2],
              " = ",
              dat$calm[1],
              stat.unit
            )
        }

        # comparing two wind roses
        if (diff) {
          annotate <- c("mean_ws", "mean_wd")
          annotations_to_place <- paste0(
            mean_ws = paste(
              "mean ws = ",
              round(as.numeric(dat$panel.fun[1]), 1)
            ),
            "\n",
            mean_wd = paste("mean wd = ", round(dat$mean.wd[1], 1))
          )
        }

        ltext(
          max.freq + off.set,
          -max.freq - off.set,
          label = annotations_to_place,
          adj = c(1, 0),
          cex = 0.7,
          col = calm.col
        )
      }

      ## add axis lines
      lsegments(-upper, 0, upper, 0)
      lsegments(0, -upper, 0, upper)

      if (!is.na(ws2) & !is.na(wd2)) {
        axislabs <- c("0", "+90", paste0("+/-", 180), "-90")
        s_adj <- 0.1
      } else {
        axislabs <- c("N", "E", "S", "W")
        s_adj <- 0.07
      }

      ltext(upper * -1 * 0.95, 0.07 * upper, axislabs[4], cex = 0.7)
      ltext(s_adj * upper, upper * -1 * 0.95, axislabs[3], cex = 0.7)
      ltext(0.07 * upper, upper * 0.95, axislabs[1], cex = 0.7)
      ltext(upper * 0.95, 0.07 * upper, axislabs[2], cex = 0.7)
    },
    legend = legend
  )

  ## reset for extra
  xy.args <- listUpdate(xy.args, extra)

  ## plot
  plt <- do.call(xyplot, xy.args)

  ## output ################################################################################

  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    } else {
      plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)
      plot(plt)
    }
  }

  newdata <- as_tibble(results)

  # give informative labels
  attr(newdata, "intervals") <- labs

  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}

## adds a line showing probability wind direction is from a particular sector
## used when normalise = TRUE

panel.wdprob <- function(dat, seg, angle, off.set) {
  len1 <- off.set

  x.off <- 0
  y.off <- 0

  makeline <- function(i, dat) {
    theta <- seq(
      (dat$wd[i] - seg * angle / 2),
      (dat$wd[i] + seg * angle / 2),
      length.out = (angle - 2) * 10
    )
    theta <- ifelse(theta < 1, 360 - theta, theta)
    theta <- theta * pi / 180
    x1 <- len1 * sin(theta) + x.off
    x2 <- rev((dat$norm[i] + off.set) * sin(theta) + x.off)
    y1 <- len1 * cos(theta) + x.off
    y2 <- rev((dat$norm[i] + off.set) * cos(theta) + x.off)
    lpolygon(
      c(x1, x2),
      c(y1, y2),
      col = "transparent",
      border = "black",
      lwd = 2
    )
  }

  lapply(1:nrow(dat), makeline, dat)
}
