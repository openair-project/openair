#' Function to plot wind speed/direction frequencies and other statistics
#'
#' `polarFreq` primarily plots wind speed-direction frequencies in
#' \sQuote{bins}. Each bin is colour-coded depending on the frequency of
#' measurements. Bins can also be used to show the concentration of pollutants
#' using a range of commonly used statistics.
#'
#' `polarFreq` is its default use provides details of wind speed and direction
#' frequencies. In this respect it is similar to [windRose()], but considers
#' wind direction intervals of 10 degrees and a user-specified wind speed
#' interval. The frequency of wind speeds/directions formed by these
#' \sQuote{bins} is represented on a colour scale.
#'
#' The `polarFreq` function is more flexible than either [windRose()] or
#' [polarPlot()]. It can, for example, also consider pollutant concentrations
#' (see examples below). Instead of the number of data points in each bin, the
#' concentration can be shown. Further, a range of statistics can be used to
#' describe each bin - see `statistic` above. Plotting mean concentrations is
#' useful for source identification and is the same as [polarPlot()] but without
#' smoothing, which may be preferable for some data. Plotting with `statistic =
#' "weighted.mean"` is particularly useful for understanding the relative
#' importance of different source contributions. For example, high mean
#' concentrations may be observed for high wind speed conditions, but the
#' weighted mean concentration may well show that the contribution to overall
#' concentrations is very low.
#'
#' `polarFreq` also offers great flexibility with the scale used and the user
#' has fine control over both the range, interval and colour.
#'
#' @inheritParams polarPlot
#'
#' @param mydata A data frame minimally containing `ws`, `wd` and `date`.
#'
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`
#'
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be one of:
#'
#'   - `"frequency"`: the simplest and plots the frequency of wind speed/direction in
#'   different bins. The scale therefore shows the counts in each bin.
#'
#'   - `"mean"`, `"median"`, `"max"` (maximum), `"stdev"` (standard deviation):
#'   Plots the relevant summary statistic of a pollutant in wind speed/direction
#'   bins.
#'
#'   - `"weighted.mean"` will plot the concentration of a pollutant
#'   weighted by wind speed/direction. Each segment therefore provides the
#'   percentage overall contribution to the total concentration.
#'
#'   Note that for options other than `"frequency"`, it is necessary to also
#'   provide the name of a `pollutant`.
#'
#' @param ws.int Wind speed interval assumed. In some cases e.g. a low met mast,
#'   an interval of 0.5 may be more appropriate.
#'
#' @param wd.nint Number of intervals of wind direction.
#'
#' @param grid.line Radial spacing of grid lines.
#'
#' @param breaks,labels If a categorical colour scale is required then `breaks`
#'   should be specified. These should be provided as a numeric vector, e.g.,
#'   `breaks = c(0, 50, 100, 1000)`. Users should set the maximum value of
#'   `breaks` to exceed the maximum data value to ensure it is within the
#'   maximum final range, e.g., 100--1000 in this case. Labels will
#'   automatically be generated, but can be customised by passing a character
#'   vector to `labels`, e.g., `labels = c("good", "bad", "very bad")`. In this
#'   example, `0 - 50` will be `"good"` and so on. Note there is one less label
#'   than break.
#'
#' @param trans Should a transformation be applied? Sometimes when producing
#'   plots of this kind they can be dominated by a few high points. The default
#'   therefore is `TRUE` and a square-root transform is applied. This results in
#'   a non-linear scale and (usually) a better representation of the
#'   distribution. If set to `FALSE` a linear scale is used.
#'
#' @param ws.upper A user-defined upper wind speed to use. This is useful for
#'   ensuring a consistent scale between different plots. For example, to always
#'   ensure that wind speeds are displayed between 1-10, set `ws.int = 10`.
#'
#' @param offset `offset` controls the size of the 'hole' in the middle and is
#'   expressed on a scale of `0` to `100`, where `0` is no hole and `100` is a
#'   hole that takes up the entire plotting area.
#'
#' @param border.col The colour of the boundary of each wind speed/direction
#'   bin. The default is transparent. Another useful choice sometimes is
#'   "white".
#'
#' @param ... Passed to [cutData()]. The following additional arguments are also
#'   available:
#'   - `xlab`, `ylab` and `main` override the x-axis label, y-axis label, and plot title.
#'   - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'   - `fontsize` overrides the overall font size of the plot.
#'   - `limits` sets the colour bar limits, if `breaks` is not being used.
#'
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family polar directional analysis functions
#' @examples
#' # basic wind frequency plot
#' polarFreq(mydata)
#'
#' # wind frequencies by year
#' \dontrun{
#' polarFreq(mydata, type = "year")
#' }
#'
#'
#' # mean SO2 by year, showing only bins with at least 2 points
#' \dontrun{
#' polarFreq(mydata, pollutant = "so2", type = "year", statistic = "mean", min.bin = 2)
#' }
#'
#' # weighted mean SO2 by year, showing only bins with at least 2 points
#' \dontrun{
#' polarFreq(mydata,
#'   pollutant = "so2", type = "year", statistic = "weighted.mean",
#'   min.bin = 2
#' )
#' }
#'
#' # windRose for just 2000 and 2003 with different colours
#' \dontrun{
#' polarFreq(subset(mydata, format(date, "%Y") %in% c(2000, 2003)),
#'   type = "year", cols = "turbo"
#' )
#' }
#'
#' # user defined breaks from 0-700 in intervals of 100 (note linear scale)
#' \dontrun{
#' polarFreq(mydata, breaks = seq(0, 700, 100))
#' }
#'
#' # more complicated user-defined breaks - useful for highlighting bins
#' # with a certain number of data points
#' \dontrun{
#' polarFreq(mydata, breaks = c(0, 10, 50, 100, 250, 500, 700))
#' }
#'
#' # source contribution plot and use of offset option
#' \dontrun{
#' polarFreq(mydata,
#'   pollutant = "pm25",
#'   statistic = "weighted.mean", offset = 50, ws.int = 25, trans = FALSE
#' )
#' }
polarFreq <- function(
  mydata,
  pollutant = NULL,
  statistic = "frequency",
  ws.int = 1,
  wd.nint = 36,
  grid.line = 5,
  breaks = NULL,
  labels = NULL,
  cols = "default",
  trans = TRUE,
  type = "default",
  min.bin = 1,
  ws.upper = NA,
  offset = 10,
  border.col = "transparent",
  key.header = statistic,
  key.footer = pollutant,
  key.position = "right",
  key = TRUE,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  if (rlang::is_logical(key) && !key) {
    key.position <- "none"
  }

  # extract necessary data
  vars <- c("wd", "ws")
  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }

  # intervals in wind direction
  wd.int <- 360 / round(wd.nint)

  # extra.args setup
  extra.args <- list(...)

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab, auto.text)
  extra.args$ylab <- quickText(extra.args$ylab, auto.text)
  extra.args$main <- quickText(extra.args$main, auto.text)

  if (!is.null(pollutant)) {
    vars <- c(vars, pollutant)
  }

  # data checks
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # to make first interval easier to work with, set ws = 0 + e
  ids <- which(mydata$ws == 0)
  mydata$ws[ids] <- mydata$ws[ids] + 0.0001

  # remove all NAs
  mydata <- na.omit(mydata)

  mydata <- cutData(mydata, type, ...)

  # if pollutant chosen but no statistic - use mean, issue warning
  if (statistic == "frequency" && !is.null(pollutant)) {
    cli::cli_warn(c(
      "x" = "{.code statistic == 'frequency'} incompatible with a defined {.field pollutant}.",
      "i" = "Setting {.field statistic} to {.code 'mean'}."
    ))
    statistic <- "mean"
  }

  # if statistic chosen but no pollutant stop
  if (statistic != "frequency" && is.null(pollutant)) {
    cli::cli_abort(c(
      "x" = "No {.field pollutant} chosen",
      "i" = "Please choose a {.field pollutant}, e.g., {.code pollutant = 'nox'}"
    ))
  }

  if (!(any(is.null(breaks)) || any(is.na(breaks)))) {
    trans <- FALSE
  } # over-ride transform if breaks supplied

  if (key.header == "weighted.mean") {
    key.header <- c("contribution (%)")
  }

  # make sure wd data are rounded to nearest 10
  mydata$wd <- wd.int * ceiling(mydata$wd / wd.int - 0.5)

  prepare.grid <- function(mydata) {
    mydata$wd[mydata$wd == 360] <- 0
    wd <- factor(mydata$wd)
    ws <- factor(ws.int * ceiling(mydata$ws / ws.int))

    if (statistic == "frequency") {
      # case with only ws and wd
      weights <- tapply(mydata$ws, list(wd, ws), function(x) length(na.omit(x)))
    }

    if (statistic == "mean") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd, ws),
        function(x) mean(x, na.rm = TRUE)
      )
    }

    if (statistic == "median") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd, ws),
        function(x) median(x, na.rm = TRUE)
      )
    }

    if (statistic == "max") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd, ws),
        function(x) max(x, na.rm = TRUE)
      )
    }

    if (statistic == "stdev") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd, ws),
        function(x) sd(x, na.rm = TRUE)
      )
    }

    if (statistic == "weighted.mean") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd, ws),
        function(x) (mean(x) * length(x) / nrow(mydata))
      )

      # note sum for matrix
      weights <- 100 * weights / sum(sum(weights, na.rm = TRUE))
    }

    weights <- as.vector(t(weights))

    # frequency - remove points with freq < min.bin
    bin.len <- tapply(mydata$ws, list(wd, ws), function(x) length(na.omit(x)))
    binned.len <- as.vector(t(bin.len))
    ids <- which(binned.len < min.bin)
    weights[ids] <- NA

    ws.wd <- expand.grid(
      ws = as.numeric(levels(ws)),
      wd = as.numeric(levels(wd))
    )

    weights <- cbind(ws.wd, weights)
    weights
  }

  results.grid <-
    mapType(
      mydata,
      type = type,
      fun = prepare.grid,
      .include_default = TRUE
    )

  results.grid <- na.omit(results.grid)

  # for pollution data
  results.grid$weights[results.grid$weights == "NaN"] <- 0
  results.grid$weights[which(is.na(results.grid$weights))] <- 0

  # handle breaks
  categorical <- FALSE
  if (!is.null(breaks)) {
    # assign labels if no labels are given
    labels <- breaksToLabels(breaks, labels)
    categorical <- TRUE
    results.grid <- dplyr::mutate(
      results.grid,
      cuts = cut(.data$weights, breaks = breaks, labels = labels)
    )
  }

  # set the upper wind speed
  if (is.na(ws.upper)) {
    max.ws <- max(results.grid$ws, na.rm = TRUE)
  } else {
    max.ws <- ws.upper
  }

  thePlot <-
    ggplot2::ggplot(
      results.grid,
      ggplot2::aes(x = .data$wd, y = .data$ws)
    ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data[[ifelse(categorical, "cuts", "weights")]]),
      colour = border.col,
      show.legend = TRUE
    ) +
    ggplot2::coord_radial(
      clip = "on",
      r.axis.inside = 45,
      rlim = c(NA, max.ws),
      inner.radius = offset / 100
    ) +
    scale_x_compass() +
    ggplot2::scale_y_continuous(
      oob = scales::oob_keep,
      breaks = seq(0, max.ws, by = grid.line),
      expand = ggplot2::expansion(c(0, .1))
    ) +
    theme_openair_radial(key.position) +
    set_extra_fontsize(extra.args) +
    ggplot2::labs(
      y = extra.args$ylab,
      x = extra.args$xlab,
      title = extra.args$main,
      fill = quickText(
        paste(
          key.header,
          key.footer,
          sep = ifelse(key.position %in% c("top", "bottom"), " ", "\n")
        ),
        auto.text = auto.text
      )
    ) +
    get_facet(
      type,
      extra.args,
      scales = "fixed",
      auto.text = auto.text
    )

  if (categorical) {
    thePlot <-
      thePlot +
      ggplot2::scale_fill_manual(
        values = openColours(
          scheme = cols,
          n = dplyr::n_distinct(levels(results.grid$cuts))
        ),
        breaks = levels(results.grid$cuts),
        drop = FALSE
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          reverse = key.position %in% c("left", "right"),
          theme = ggplot2::theme(
            legend.title.position = ifelse(
              key.position %in% c("left", "right"),
              "top",
              key.position
            ),
            legend.text.position = key.position
          ),
          nrow = if (key.position %in% c("left", "right")) NULL else 1
        )
      )
  } else {
    thePlot <-
      thePlot +
      ggplot2::scale_fill_gradientn(
        colours = openColours(cols),
        transform = ifelse(trans, "sqrt", "identity"),
        oob = scales::oob_squish,
        breaks = scales::pretty_breaks(6),
        limits = extra.args$limit
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_colorbar(
          theme = ggplot2::theme(
            legend.title.position = ifelse(
              key.position %in% c("left", "right"),
              "top",
              key.position
            ),
            legend.text.position = key.position
          )
        )
      )
  }

  # make key full width/height
  if (key.position %in% c("left", "right")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.height = ggplot2::unit(1, "null"),
        legend.key.spacing.y = ggplot2::unit(0, "cm")
      )
  }
  if (key.position %in% c("top", "bottom")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.width = ggplot2::unit(1, "null"),
        legend.key.spacing.x = ggplot2::unit(0, "cm")
      )
  }

  if (plot) {
    plot(thePlot)
  }

  output <- list(
    plot = thePlot,
    data = results.grid,
    call = match.call()
  )
  class(output) <- "openair"

  invisible(output)
}
