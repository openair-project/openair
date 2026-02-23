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
#'   `quickText` to handle routing formatting.
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
  if (is.null(seg)) seg <- 0.9

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    calm.col <- "black"
  } else {
    calm.col <- "forestgreen"
  }

  ## extra args setup
  extra <- list(...)
  extra$main <- if ("main" %in% names(extra)) quickText(extra$main, auto.text) else NULL

  if ("fontsize" %in% names(extra)) {
    extra$fontsize <- extra$fontsize
  }

  ## statistic settings
  stat <- wr_statistic_settings(statistic, dig.lab)

  ## variables we need
  vars <- c(wd, ws)
  diff <- FALSE ## i.e. not two sets of ws/wd
  rm.neg <- TRUE ## will remove negative ws in check.prep

  ## make sure ws and wd are numeric
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

  ## case where two met data sets are to be compared
  if (!is.na(ws2) & !is.na(wd2)) {
    vars <- c(vars, ws2, wd2)
    diff <- TRUE
    rm.neg <- FALSE
    mydata$ws <- mydata[[ws2]] - mydata[[ws]]
    mydata$wd <- mydata[[wd2]] - mydata[[wd]]

    ## fix negative wd
    id <- which(mydata$wd < 0)
    if (length(id) > 0) mydata$wd[id] <- mydata$wd[id] + 360

    pollutant <- "ws"
    key.footer <- "ws"
    wd <- "wd"
    ws <- "ws"
    vars <- c("ws", "wd")
    if (missing(angle)) angle <- 10
    if (missing(offset)) offset <- 20

    ## set the breaks to cover all the data
    if (is.na(breaks[1])) {
      max.br <- max(ceiling(abs(c(
        min(mydata$ws, na.rm = TRUE),
        max(mydata$ws, na.rm = TRUE)
      ))))
      breaks <- c(-1 * max.br, 0, max.br)
    }

    if (missing(cols)) cols <- c("lightskyblue", "tomato")
    seg <- 1
  }

  if (any(type %in% dateTypes)) vars <- c(vars, "date")
  if (!is.null(pollutant)) vars <- c(vars, pollutant)

  mydata <- cutData(mydata, type, ...)
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE, remove.neg = rm.neg)

  ## original data for bias correction later
  mydata_orig <- mydata

  ## remove rows where ws is missing (wd can be NA when ws == 0, i.e. calm)
  id <- which(is.na(mydata[[ws]]))
  if (length(id) > 0) mydata <- mydata[-id, ]

  if (is.null(pollutant)) pollutant <- ws

  ## wind direction processing
  mydata$x <- mydata[[pollutant]]
  mydata[[wd]] <- angle * ceiling(mydata[[wd]] / angle - 0.5)
  mydata[[wd]][mydata[[wd]] == 0] <- 360

  ## flag calms as -999 (must be done after rounding wd)
  if (calm.thresh == 0) {
    mydata[[wd]][mydata[, ws] == 0] <- -999
  } else {
    mydata[[wd]][mydata[, ws] < calm.thresh] <- -999 ## Note < not <=
  }
  mydata[[wd]][mydata[, ws] < calm.thresh] <- -999

  ## breaks setup
  if (length(breaks) == 1) breaks <- 0:(breaks - 1) * ws.int
  if (max(breaks) < max(mydata$x, na.rm = TRUE)) breaks <- c(breaks, max(mydata$x, na.rm = TRUE))
  if (min(breaks) > min(mydata$x, na.rm = TRUE)) warning("Some values are below minimum break.")
  breaks <- unique(breaks)

  mydata$x <- cut(mydata$x, breaks = breaks, include.lowest = include.lowest, dig.lab = dig.lab)

  ## clean up cut intervals for display
  labs <- gsub("[(]|[)]|[[]|[]]", "", levels(mydata$x))
  labs <- gsub("[,]", " to ", labs)

  ## compute statistics per wind direction bin
  prepare.grid <- wr_make_prepare_grid(wd, pollutant, length(labs), stat$fun, stat$fun2, stat$scale)
  results <- mapType(mydata, fun = prepare.grid, type = type, .include_default = TRUE)

  results$calm <- stat$labcalm(results$calm)
  results$mean.wd <- stat$labcalm(results$mean.wd)

  ## correction for bias when angle does not divide exactly into 360
  if (bias.corr) {
    results <- mapType(
      results,
      type = type,
      fun = function(r) wr_correct_bias(r, mydata_orig, wd, type, angle),
      .include_default = TRUE
    )
  }

  ## colours
  col <- if (length(labs) < length(cols)) cols[seq_along(labs)] else openColours(cols, length(labs))
  legend_col <- col

  ## normalise by sector
  if (normalise) {
    vars <- grep("Interval[1-9]", names(results))

    ## original frequencies for the wind frequency line
    results$freq <- results[[max(vars)]]
    results$freq <- ave(results$freq, results[type], FUN = function(x) x / sum(x))
    results$norm <- results$freq / max(results$freq)

    results[, vars] <- results[, vars] / results[[max(vars)]]
    stat$lab <- "Normalised by wind sector"
    stat$unit <- ""
  }

  ## scale limits
  if (is.null(max.freq)) {
    max.freq <- max(
      results[results$wd != -999, grep("Interval", names(results))],
      na.rm = TRUE
    )
  }

  off.set <- max.freq * (offset / 100)
  box.widths <- seq(0.002^0.25, 0.016^0.25, length.out = length(labs))^4 * max.freq * angle / 5

  ## grid line settings
  grid <- wr_parse_grid_line(grid.line)
  mymax <- max(pretty(c(0, max.freq), 4))
  myby <- if (is.null(grid$value)) pretty(c(0, mymax), 4)[2] else grid$value
  if (myby / mymax > 0.9) myby <- mymax * 0.9

  upper <- max.freq + off.set
  lim <- 1.03 * upper

  ## type columns for faceting (exclude "default")
  facet_cols <- setdiff(type, "default")

  ## --- Build ggplot data layers ---

  ## polygon data for the rose segments
  poly_data <- wr_rose_polygon_data(
    results, labs, box.widths, width, off.set, paddle, seg, angle, facet_cols
  )

  ## concentric grid circles (no facet cols → appears in all panels)
  circle_data <- wr_grid_circle_data(off.set, mymax, myby)

  ## compass axis cross-hairs
  axis_data <- data.frame(
    x = c(-upper, 0), xend = c(upper, 0),
    y = c(0, -upper), yend = c(0, upper)
  )

  ## compass direction labels
  compass_data <- wr_compass_label_data(upper, !is.na(ws2) & !is.na(wd2))

  ## scale percentage labels along angle.scale spoke
  scale_label_data <- wr_scale_label_data(myby, mymax, off.set, angle.scale, stat$unit)

  ## per-panel annotations (calm%, mean)
  is_annotated <- any(annotate == TRUE) | any(is.character(annotate))
  if (is_annotated) {
    annot_data <- wr_annotation_data(
      results, annotate, diff, stat$lab2, stat$unit, max.freq, off.set, type, facet_cols
    )
  }

  ## normalise frequency outline
  if (normalise) {
    norm_data <- wr_normalise_line_data(results, seg, angle, off.set, facet_cols)
  }

  ## legend title: header (if any) above footer
  key_label <- if (!is.null(key.header)) {
    paste(quickText(key.header, auto.text), quickText(key.footer, auto.text), sep = "\n")
  } else {
    quickText(key.footer, auto.text)
  }
  legend_position <- if (isFALSE(key) || is.null(key)) "none" else key.position

  ## --- Assemble ggplot ---
  thePlot <-
    ggplot2::ggplot() +
    ## concentric grid circles
    ggplot2::geom_path(
      data = circle_data,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$circle),
      colour = grid$col, linewidth = 0.3, linetype = grid$lty,
      inherit.aes = FALSE
    ) +
    ## compass axis lines
    ggplot2::geom_segment(
      data = axis_data,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend
      ),
      colour = "black", linewidth = 0.4, inherit.aes = FALSE
    ) +
    ## rose polygons
    ggplot2::geom_polygon(
      data = poly_data,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        group = .data$poly_id, fill = .data$interval
      ),
      colour = border, alpha = alpha, inherit.aes = FALSE
    )

  ## normalise frequency line (outline only, no fill)
  if (normalise) {
    thePlot <- thePlot +
      ggplot2::geom_polygon(
        data = norm_data,
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$sector_id),
        fill = NA, colour = "black", linewidth = 0.8, inherit.aes = FALSE
      )
  }

  thePlot <- thePlot +
    ## scale labels along angle.scale spoke
    ggplot2::geom_text(
      data = scale_label_data,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 2.7, inherit.aes = FALSE
    ) +
    ## compass direction labels
    ggplot2::geom_text(
      data = compass_data,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 3, fontface = "bold", inherit.aes = FALSE
    )

  ## panel annotations (calm%, mean value)
  if (is_annotated) {
    thePlot <- thePlot +
      ggplot2::geom_label(
        data = annot_data,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        hjust = 1, vjust = 0, size = 2.5,
        colour = calm.col, fill = "white", label.size = NA,
        inherit.aes = FALSE
      )
  }

  thePlot <- thePlot +
    ## fill colour scale
    ggplot2::scale_fill_manual(
      values = setNames(legend_col, labs),
      name = key_label,
      guide = ggplot2::guide_legend(
        reverse = key.position %in% c("left", "right"),
        theme = ggplot2::theme(
          legend.title.position = if (key.position %in% c("left", "right")) "top" else key.position
        )
      )
    ) +
    ## faceting
    get_facet(
      type,
      extra.args = extra,
      scales = "fixed",
      auto.text = auto.text
    ) +
    ## square aspect ratio with fixed limits
    ggplot2::coord_fixed(
      ratio = 1,
      xlim = c(-lim, lim),
      ylim = c(-lim, lim),
      expand = FALSE
    ) +
    ## base theme
    theme_openair(legend_position) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = extra$main,
      caption = if (is_annotated) quickText(stat$lab, auto.text) else NULL
    ) +
    set_extra_fontsize(extra)

  ## output
  if (plot) plot(thePlot)

  newdata <- as_tibble(results)
  attr(newdata, "intervals") <- labs

  output <- list(plot = thePlot, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}

## ---------------------------------------------------------------------------
## Statistic settings
## ---------------------------------------------------------------------------

## Parse statistic argument into component functions and labels
wr_statistic_settings <- function(statistic, dig.lab) {
  if (is.list(statistic)) {
    return(list(
      fun = statistic$fun,
      unit = statistic$unit,
      scale = statistic$scale,
      lab = statistic$lab,
      fun2 = statistic$fun2,
      lab2 = statistic$lab2,
      labcalm = statistic$labcalm
    ))
  }

  ok.stat <- c("prop.count", "prop.mean", "abs.count", "frequency")
  if (!statistic[1] %in% ok.stat) {
    warning(
      "In windRose(...):\n  statistic unrecognised",
      "\n  enforcing statistic = 'prop.count'",
      call. = FALSE
    )
    statistic <- "prop.count"
  }

  switch(
    statistic,
    prop.count = list(
      fun = length,
      unit = "%",
      scale = "all",
      lab = "Frequency of counts by wind direction (%)",
      fun2 = function(x) format(mean(x, na.rm = TRUE), digits = dig.lab),
      lab2 = "mean",
      labcalm = function(x) round(x, 1)
    ),
    prop.mean = list(
      fun = function(x) sum(x, na.rm = TRUE),
      unit = "%",
      scale = "panel",
      lab = "Proportion contribution to the mean (%)",
      fun2 = function(x) format(mean(x, na.rm = TRUE), digits = 5),
      lab2 = "mean",
      labcalm = function(x) round(x, 1)
    ),
    abs.count = ,
    frequency = list(
      fun = length,
      unit = "",
      scale = "none",
      lab = "Count by wind direction",
      fun2 = function(x) round(length(x), 0),
      lab2 = "count",
      labcalm = function(x) round(x, 0)
    )
  )
}

## ---------------------------------------------------------------------------
## Statistics grid computation
## ---------------------------------------------------------------------------

## Factory returning the prepare.grid function with the given statistic settings
wr_make_prepare_grid <- function(wd, pollutant, n_labs, stat.fun, stat.fun2, stat.scale) {
  function(mydata) {
    ## all calms case
    if (all(is.na(mydata$x))) {
      return(tibble(
        Interval1 = NA,
        wd = NA,
        calm = 100,
        panel.fun = NA,
        mean.wd = NA,
        freqs = NA
      ))
    }

    levels(mydata$x) <- paste0("Interval", seq_len(n_labs))

    n_all <- stat.fun(mydata[[wd]])
    calm <- stat.fun(mydata[mydata[[wd]] == -999, ][[pollutant]])

    weights <- tapply(mydata[[pollutant]], list(mydata[[wd]], mydata$x), stat.fun)
    freqs <- tapply(mydata[[pollutant]], mydata[[wd]], length)

    ## scaling
    if (stat.scale == "all") {
      calm <- calm / n_all
      weights <- weights / n_all
    }

    if (stat.scale == "panel") {
      temp <- stat.fun(stat.fun(weights)) + calm
      calm <- calm / temp
      weights <- weights / temp
    }

    weights[is.na(weights)] <- 0
    weights <- t(apply(weights, 1, cumsum))

    if (stat.scale %in% c("all", "panel")) {
      weights <- weights * 100
      calm <- calm * 100
    }

    panel.fun <- stat.fun2(mydata[[pollutant]])

    ## calculate mean wd - useful for comparing two met data sets
    u <- mean(sin(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
    v <- mean(cos(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
    mean.wd <- atan2(u, v) * 360 / 2 / pi

    if (!all(is.na(mean.wd))) {
      if (mean.wd < 0) mean.wd <- mean.wd + 360
      ## show as a negative (bias)
      if (mean.wd > 180) mean.wd <- mean.wd - 360
    }

    bind_cols(
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
}

## ---------------------------------------------------------------------------
## Bias correction
## ---------------------------------------------------------------------------

## Correct frequency bias introduced when angle does not divide evenly into 360
wr_correct_bias <- function(results, mydata_orig, wd, type, angle) {
  ## check to see if data for this type combination are rounded to 10 degrees
  wd_select <- inner_join(mydata_orig, results[1, type], by = type)
  if (!all(round(wd_select[[wd]]) %% 10 == 0, na.rm = TRUE)) {
    return(results)
  }

  wds <- seq(10, 360, 10)
  tmp <- angle * ceiling(wds / angle - 0.5)
  id <- which(tmp == 0)
  if (length(id) > 0) tmp[id] <- 360
  tmp <- table(tmp) ## number of sectors spanned

  vars <- grep("Interval[1-9]", names(results))
  n_data <- nrow(filter(results, wd != -999))

  if (n_data > 0) {
    results[results[["wd"]] != -999, vars] <-
      results[results[["wd"]] != -999, vars] * mean(tmp) / tmp
  }

  results
}

## ---------------------------------------------------------------------------
## Grid line argument parsing
## ---------------------------------------------------------------------------

## Parse the grid.line argument (scalar or list) into consistent properties
wr_parse_grid_line <- function(grid.line) {
  if (is.list(grid.line)) {
    list(
      value = grid.line[["value"]],
      lty = if (is.null(grid.line[["lty"]])) 1 else grid.line[["lty"]],
      col = if (is.null(grid.line[["col"]])) "grey85" else grid.line[["col"]]
    )
  } else {
    list(value = grid.line, lty = 1, col = "grey85")
  }
}

## ---------------------------------------------------------------------------
## ggplot2 data-building helpers
## ---------------------------------------------------------------------------

## Compute paddle polygon vertices for one wind direction / interval pair
wr_paddle_verts <- function(wd, len1, len2, width, off.set) {
  theta <- wd * pi / 180
  r1 <- len1 + off.set
  r2 <- len2 + off.set
  data.frame(
    x = c(
      r1 * sin(theta) - width * cos(theta),
      r1 * sin(theta) + width * cos(theta),
      r2 * sin(theta) + width * cos(theta),
      r2 * sin(theta) - width * cos(theta)
    ),
    y = c(
      r1 * cos(theta) + width * sin(theta),
      r1 * cos(theta) - width * sin(theta),
      r2 * cos(theta) - width * sin(theta),
      r2 * cos(theta) + width * sin(theta)
    )
  )
}

## Compute wedge polygon vertices for one wind direction / interval pair
wr_wedge_verts <- function(wd, len1, len2, seg, angle, off.set) {
  r1 <- len1 + off.set
  r2 <- len2 + off.set
  theta <- seq(
    wd - seg * angle / 2,
    wd + seg * angle / 2,
    length.out = (angle - 2) * 10
  )
  theta <- ifelse(theta < 1, 360 - theta, theta)
  theta <- theta * pi / 180
  data.frame(
    x = c(r1 * sin(theta), rev(r2 * sin(theta))),
    y = c(r1 * cos(theta), rev(r2 * cos(theta)))
  )
}

## Build polygon vertex data frame for all rose segments (all panels combined)
wr_rose_polygon_data <- function(
  results, labs, box.widths, width, off.set,
  paddle, seg, angle, facet_cols
) {
  rose_dat <- dplyr::filter(results, .data$wd >= 0, .data$wd <= 360)
  rose_dat$Interval0 <- 0

  n_rows <- nrow(rose_dat)
  n_labs <- length(labs)
  poly_list <- vector("list", n_rows * n_labs)
  group_id <- 0L

  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_labs)) {
      group_id <- group_id + 1L
      len1 <- rose_dat[[paste0("Interval", j - 1L)]][i]
      len2 <- rose_dat[[paste0("Interval", j)]][i]

      verts <- if (paddle) {
        wr_paddle_verts(rose_dat$wd[i], len1, len2, width * box.widths[j], off.set)
      } else {
        wr_wedge_verts(rose_dat$wd[i], len1, len2, seg, angle, off.set)
      }

      verts$poly_id <- group_id
      verts$interval <- factor(labs[j], levels = labs)

      ## include faceting columns so geom_polygon stays in the correct panel
      for (col in facet_cols) {
        verts[[col]] <- rose_dat[[col]][i]
      }

      poly_list[[group_id]] <- verts
    }
  }

  dplyr::bind_rows(poly_list)
}

## Build concentric grid circle path data (no faceting columns → all panels)
wr_grid_circle_data <- function(off.set, mymax, myby) {
  radii <- seq(off.set, mymax + off.set, by = myby)
  theta <- seq(0, 2 * pi, length.out = 361)

  dplyr::bind_rows(lapply(seq_along(radii), function(i) {
    data.frame(
      x = radii[i] * sin(theta),
      y = radii[i] * cos(theta),
      circle = i
    )
  }))
}

## Build compass direction label positions
wr_compass_label_data <- function(upper, is_diff) {
  if (is_diff) {
    labels <- c("0", "+90", paste0("+/-", 180), "-90")
    s_adj <- 0.1
  } else {
    labels <- c("N", "E", "S", "W")
    s_adj <- 0.07
  }
  data.frame(
    x = c(0.07 * upper, upper * 0.95, s_adj * upper, upper * -0.95),
    y = c(upper * 0.95, 0.07 * upper, upper * -0.95, 0.07 * upper),
    label = labels
  )
}

## Build scale percentage label positions along the angle.scale spoke
wr_scale_label_data <- function(myby, mymax, off.set, angle.scale, stat_unit) {
  r_vals <- seq(myby + off.set, mymax + off.set, by = myby)
  angle_rad <- pi * angle.scale / 180
  data.frame(
    x = r_vals * sin(angle_rad),
    y = r_vals * cos(angle_rad),
    label = paste0(seq(myby, mymax, by = myby), stat_unit)
  )
}

## Build per-panel annotation data (calm%, mean value)
wr_annotation_data <- function(
  results, annotate, diff, stat_lab2, stat_unit,
  max.freq, off.set, type, facet_cols
) {
  ## one representative row per panel (all rows in a panel share the same
  ## panel-level statistics: panel.fun, calm, mean.wd)
  panel_dat <- results %>%
    dplyr::filter(.data$wd >= 0, .data$wd <= 360) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(type))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  panel_dat$label <- mapply(
    function(pf, cl, mwd) {
      wr_build_annotation(annotate, pf, cl, mwd, diff, stat_lab2, stat_unit)
    },
    panel_dat$panel.fun,
    panel_dat$calm,
    panel_dat$mean.wd,
    SIMPLIFY = TRUE
  )

  ## position: lower-right corner of the polar plot
  panel_dat$x <- max.freq + off.set
  panel_dat$y <- -(max.freq + off.set)

  ## keep only facet columns and the derived label/position columns
  keep <- unique(c(facet_cols, "x", "y", "label"))
  panel_dat[, keep, drop = FALSE]
}

## Build polygon outlines for the normalise frequency line (one per wd sector)
wr_normalise_line_data <- function(results, seg, angle, off.set, facet_cols) {
  dat <- dplyr::filter(results, .data$wd >= 0, .data$wd <= 360)

  sector_list <- lapply(seq_len(nrow(dat)), function(i) {
    theta <- seq(
      dat$wd[i] - seg * angle / 2,
      dat$wd[i] + seg * angle / 2,
      length.out = (angle - 2) * 10
    )
    theta <- ifelse(theta < 1, 360 - theta, theta)
    theta <- theta * pi / 180

    x1 <- off.set * sin(theta)
    x2 <- rev((dat$norm[i] + off.set) * sin(theta))
    y1 <- off.set * cos(theta)
    y2 <- rev((dat$norm[i] + off.set) * cos(theta))

    df <- data.frame(x = c(x1, x2), y = c(y1, y2), sector_id = i)

    for (col in facet_cols) {
      df[[col]] <- dat[[col]][i]
    }

    df
  })

  dplyr::bind_rows(sector_list)
}

## Build the annotation string displayed in each wind rose panel
wr_build_annotation <- function(annotate, panel.fun, calm, mean.wd, diff, stat.lab2, stat.unit) {
  ## comparing two wind roses always shows mean ws and mean wd bias
  if (diff) {
    return(paste0(
      "mean ws = ", round(as.numeric(panel.fun), 1),
      "\n",
      "mean wd = ", round(mean.wd, 1)
    ))
  }

  if (annotate[1] == TRUE) {
    return(paste0(
      stat.lab2, " = ", panel.fun,
      "\n",
      "calm = ", calm, stat.unit
    ))
  }

  if (annotate[1] == " ") {
    return(paste0(stat.lab2, " = ", panel.fun))
  }

  ## length(annotate) == 2: custom label pair supplied
  paste0(
    annotate[1], " = ", panel.fun,
    "\n",
    annotate[2], " = ", calm, stat.unit
  )
}
