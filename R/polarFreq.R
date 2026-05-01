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
#' @inheritParams shared_openair_params
#' @inheritParams polarPlot
#'
#' @param mydata A data frame minimally containing a wind speed, a decimal wind
#'   direction, and `date`.
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
#' @param limits The limits of the colour bar (e.g., `c(0, 100)`).
#'
#' @param ws.int Wind speed interval assumed. In some cases e.g. a low met mast,
#'   an interval of 0.5 may be more appropriate.
#'
#' @param wd.nint Number of intervals of wind direction.
#'
#' @param grid.line Radial spacing of grid lines.
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
#' @param border.col The colour of the boundary of each wind speed/direction
#'   bin. The default is transparent. Another useful choice sometimes is
#'   "white".
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
  ws = "ws",
  wd = "wd",
  statistic = "frequency",
  ws.int = 1,
  wd.nint = 36,
  grid.line = 5,
  limits = NULL,
  breaks = NULL,
  cols = "default",
  trans = TRUE,
  type = "default",
  min.bin = 1,
  ws.upper = NA,
  angle.scale = 45,
  offset = 10,
  border.col = "transparent",
  key.title = paste(statistic, pollutant, sep = " "),
  key.position = "right",
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
  ...
) {
  # check key.position
  key.position <- check_key_position(key.position, key)

  # extract necessary data
  vars <- c(wd, ws)
  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }

  # intervals in wind direction
  wd.int <- 360 / round(wd.nint)

  # extra.args setup
  extra.args <- capture_dots(...)

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab, auto.text)
  extra.args$ylab <- quickText(extra.args$ylab, auto.text)
  extra.args$title <- quickText(extra.args$title, auto.text)
  extra.args$subtitle <- quickText(extra.args$subtitle, auto.text)
  extra.args$caption <- quickText(extra.args$caption, auto.text)
  extra.args$tag <- quickText(extra.args$tag, auto.text)

  # deal with breaks
  break_opts <- resolve_break_opts(breaks, extra.args)

  if (!is.null(pollutant)) {
    vars <- c(vars, pollutant)
  }

  # data checks
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # to make first interval easier to work with, set ws = 0 + e
  ids <- which(mydata[[ws]] == 0)
  mydata[[ws]][ids] <- mydata[[ws]][ids] + 0.0001

  # remove all NAs
  mydata <- stats::na.omit(mydata)

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

  # over-ride transform if breaks supplied
  if (!(any(is.null(break_opts$breaks)) || anyNA(break_opts$breaks))) {
    trans <- FALSE
  }

  # replace weighted.mean with a nicer label
  key.title <- check_key_header(key.title, extra.args)
  key.title <- gsub("weighted.mean", "contribution (%)", key.title)

  # make sure wd data are rounded to nearest 10
  mydata$wd <- wd.int * ceiling(mydata[[wd]] / wd.int - 0.5)

  prepare.grid <- function(mydata) {
    mydata[[wd]][mydata$wd == 360] <- 0
    wd_vec <- factor(mydata[[wd]])
    ws_vec <- factor(ws.int * ceiling(mydata[[ws]] / ws.int))

    if (statistic == "frequency") {
      # case with only ws and wd
      weights <- tapply(mydata[[ws]], list(wd_vec, ws_vec), function(x) {
        length(stats::na.omit(x))
      })
    }

    if (statistic == "mean") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd_vec, ws_vec),
        function(x) mean(x, na.rm = TRUE)
      )
    }

    if (statistic == "median") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd_vec, ws_vec),
        function(x) stats::median(x, na.rm = TRUE)
      )
    }

    if (statistic == "max") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd_vec, ws_vec),
        function(x) max(x, na.rm = TRUE)
      )
    }

    if (statistic == "stdev") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd_vec, ws_vec),
        function(x) stats::sd(x, na.rm = TRUE)
      )
    }

    if (statistic == "weighted.mean") {
      weights <- tapply(
        mydata[[pollutant]],
        list(wd_vec, ws_vec),
        function(x) (mean(x) * length(x) / nrow(mydata))
      )

      # note sum for matrix
      weights <- 100 * weights / sum(sum(weights, na.rm = TRUE))
    }

    weights <- as.vector(t(weights))

    # frequency - remove points with freq < min.bin
    bin.len <- tapply(mydata[[ws]], list(wd_vec, ws_vec), function(x) {
      length(stats::na.omit(x))
    })
    binned.len <- as.vector(t(bin.len))
    ids <- which(binned.len < min.bin)
    weights[ids] <- NA

    ws.wd <- expand.grid(
      ws = as.numeric(levels(ws_vec)),
      wd = as.numeric(levels(wd_vec))
    )

    weights <- cbind(ws.wd, weights)
    weights
  }

  results.grid <-
    map_type(
      mydata,
      type = type,
      fun = prepare.grid,
      .include_default = TRUE
    )

  results.grid <- stats::na.omit(results.grid)

  # for pollution data
  results.grid$weights[results.grid$weights == "NaN"] <- 0
  results.grid$weights[which(is.na(results.grid$weights))] <- 0

  # handle breaks
  categorical <- !is.null(break_opts$breaks)
  results.grid$weights <- cut_plot_breaks(
    results.grid$weights,
    break_opts
  )

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
      ggplot2::aes(fill = .data[["weights"]]),
      colour = border.col,
      show.legend = TRUE
    ) +
    ggplot2::ggproto(
      NULL,
      ggplot2::coord_radial(
        clip = "on",
        r.axis.inside = angle.scale,
        rlim = c(NA, max.ws),
        inner.radius = offset / 100
      ),
      inner_radius = c(offset / 100, 1) * 0.475
    ) +
    scale_x_compass() +
    ggplot2::scale_y_continuous(
      oob = scales::oob_keep,
      breaks = seq(0, max.ws, by = grid.line),
      expand = ggplot2::expansion(c(0, 0.1))
    ) +
    theme_openair_radial(
      key.position = key.position,
      extra.args = extra.args,
      panel.ontop = TRUE
    ) +
    ggplot2::labs(
      y = extra.args$ylab,
      x = extra.args$xlab,
      title = extra.args$title,
      subtitle = extra.args$subtitle,
      caption = extra.args$caption,
      tag = extra.args$tag,
      fill = quickText(key.title, auto.text = auto.text)
    ) +
    get_facet(
      type,
      extra.args,
      auto.text = auto.text,
      wd.res = extra.args$wd.res %||% 8
    )

  if (categorical) {
    thePlot <-
      thePlot +
      ggplot2::scale_fill_manual(
        values = resolve_colour_opts(
          cols,
          n = dplyr::n_distinct(levels(results.grid$weights))
        ),
        breaks = levels(results.grid$weights),
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
        colours = resolve_colour_opts(cols, 100),
        transform = ifelse(trans, "sqrt", "identity"),
        oob = scales::oob_squish,
        breaks = scales::pretty_breaks(6),
        limits = limits
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

  # add compass points
  thePlot <- thePlot +
    annotate_compass_points(
      size = ifelse(
        extra.args$annotate %||% TRUE,
        if (is.null(extra.args$fontsize)) 3 else extra.args$fontsize / 3,
        0
      )
    )

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
