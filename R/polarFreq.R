#' Function to plot wind speed/direction frequencies and other statistics
#'
#' [polarFreq()] primarily plots wind speed-direction frequencies in 'bins'.
#' Each bin is colour-coded depending on the frequency of measurements. Bins can
#' also be used to show the concentration of pollutants using a range of
#' commonly used statistics.
#'
#' [polarFreq()] is its default use provides details of wind speed and direction
#' frequencies. In this respect it is similar to [windRose()], but considers
#' wind direction intervals of 10 degrees and a user-specified wind speed
#' interval. The frequency of wind speeds/directions formed by these 'bins' is
#' represented on a colour scale.
#'
#' The [polarFreq()] function is more flexible than either [windRose()] or
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
#' [polarFreq()] also offers great flexibility with the scale used and the user
#' has fine control over both the range, interval and colour.
#'
#' @inheritParams polarPlot
#' @param mydata A data frame minimally containing `ws`, `wd` and `date`.
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be:
#'   - `"frequency"` (the default) is the simplest and plots the frequency of
#'   wind speed/direction in different bins. The scale therefore shows the
#'   counts in each bin.
#'   - `"mean"`, the average/mean `pollutant` per bin
#'   - `"median"`, the median `pollutant` per bin
#'   - `"max"`, the maximum `pollutant` per bin
#'   - `"min"`, the minimum `pollutant` per bin
#'   - `"stdev"`, the `pollutant` standard deviation in each bin
#'   - `"weighted.mean"`, will plot the concentration of the `pollutant` weighted
#'   by wind speed/direction. Each segment therefore provides the percentage
#'   overall contribution to the total concentration.
#'
#'   Note that for options other than `"frequency"`, it is necessary to also
#'   provide the name of a pollutant.
#' @param ws.int Wind speed interval assumed. In some cases, e.g., a low met
#'   mast, an interval of `0.5` may be more appropriate.
#' @param wd.nint Number of intervals of wind direction.
#' @param grid.line Radial spacing of grid lines.
#' @param breaks The user can provide their own scale. `breaks` expects a
#'   sequence of numbers that define the range of the scale. The sequence could
#'   represent one with equal spacing e.g. `breaks = seq(0, 100, 10)` - a scale
#'   from 0-10 in intervals of 10, or a more flexible sequence e.g. `breaks =
#'   c(0, 1, 5, 7, 10)`, which may be useful for some situations.
#' @param trans Should a transformation be applied? Sometimes when producing
#'   plots of this kind they can be dominated by a few high points. The default
#'   therefore is `TRUE` and a square-root transform is applied. This results in
#'   a non-linear scale and (usually) a better representation of the
#'   distribution. If set to `FALSE` a linear scale is used.
#' @param ws.upper A user-defined upper wind speed to use. This is useful for
#'   ensuring a consistent scale between different plots. For example, to always
#'   ensure that wind speeds are displayed between 1-10, set `ws.int = 10`.
#' @param offset `offset` controls the size of the 'hole' in the middle and is
#'   expressed as a percentage of the maximum wind speed. Setting a higher
#'   `offset` e.g. 50 is useful for `statistic = "weighted.mean"` when `ws.int`
#'   is greater than the maximum wind speed. See example below.
#' @param border.col The colour of the boundary of each wind speed/direction
#'   bin. The default is transparent. Another useful choice sometimes is
#'   "white".
#' @param ... Other graphical parameters passed onto [lattice::xyplot()] and
#'   [cutData()]. For example, [polarFreq()] passes the option `hemisphere =
#'   "southern"` on to [cutData()] to provide southern (rather than default
#'   northern) hemisphere handling of `type = "season"`. Similarly, common axis
#'   and title labelling options (such as `xlab`, `ylab`, `main`) are passed to
#'   [lattice::xyplot()] via [quickText()] to handle routine formatting.
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
#'
#' # mean SO2 by year, showing only bins with at least 2 points
#' polarFreq(mydata, pollutant = "so2", type = "year", statistic = "mean", min.bin = 2)
#'
#' # weighted mean SO2 by year, showing only bins with at least 2 points
#' polarFreq(mydata,
#'   pollutant = "so2", type = "year", statistic = "weighted.mean",
#'   min.bin = 2
#' )
#'
#' # windRose for just 2000 and 2003 with different colours
#' polarFreq(subset(mydata, format(date, "%Y") %in% c(2000, 2003)),
#'   type = "year", cols = "turbo"
#' )
#'
#' # user defined breaks from 0-700 in intervals of 100 (note linear scale)
#' polarFreq(mydata, breaks = seq(0, 700, 100))
#'
#' # more complicated user-defined breaks - useful for highlighting bins
#' # with a certain number of data points
#' polarFreq(mydata, breaks = c(0, 10, 50, 100, 250, 500, 700))
#'
#' # source contribution plot and use of offset option
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
  cols = "default",
  trans = TRUE,
  type = "default",
  min.bin = 1,
  ws.upper = NULL,
  offset = 10,
  border.col = "transparent",
  key.header = statistic,
  key.footer = pollutant,
  key.position = "right",
  key = TRUE,
  auto.text = TRUE,
  alpha = 1,
  plot = TRUE,
  ...
) {
  # validate various inputs for polarFreq
  inputs <- validate_polarfreq_args(
    statistic = statistic,
    pollutant = pollutant,
    trans = trans,
    breaks = breaks
  )
  statistic <- inputs$statistic
  pollutant <- inputs$pollutant
  coef <- inputs$coef

  # extract necessary data
  vars <- c("wd", "ws")
  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }
  if (!is.null(pollutant)) {
    vars <- c(vars, pollutant)
  }

  # prepare data
  mydata <-
    mydata %>%
    # openair data checks
    checkPrep(vars, type, remove.calm = FALSE) %>%
    # set ws = 0 to be a small value; makes first bin easier to deal with
    dplyr::mutate(
      ws = dplyr::if_else(
        .data$ws == 0,
        .data$ws + .Machine$double.eps,
        .data$ws
      )
    ) %>%
    # drop missing values
    tidyr::drop_na() %>%
    # add type columns
    cutData(type = type, ...)

  # Wind speed parameters
  max.ws <- ws.upper %||% max(mydata$ws, na.rm = TRUE)
  offset <- (max.ws * offset) / 5 / 10

  # Wind direction parameters
  wd.int <- 360 / round(wd.nint)

  # Round wind direction to intervals
  mydata <- dplyr::mutate(
    mydata,
    wd = wd.int * ceiling(.data$wd / wd.int - 0.5)
  )

  # function to draw polar polygons
  draw_polarfreq_poly <- function(dir, speed, colour) {
    # offset by 3 * ws.int so that centre is not compressed
    angle <- seq(dir - wd.int / 2, dir + wd.int / 2, length = round(wd.int))
    x1 <- (speed + offset - ws.int) * sin(pi * angle / 180)
    y1 <- (speed + offset - ws.int) * cos(pi * angle / 180)
    x2 <- rev((speed + offset) * sin(pi * angle / 180))
    y2 <- rev((speed + offset) * cos(pi * angle / 180))
    lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border.col, lwd = 0.5)
  }

  # prepare plotting data for lattice
  results_grid <-
    purrr::map(
      .x = split(
        mydata,
        mydata[type],
        drop = TRUE
      ),
      .f = function(x) {
        out <- prepare_polarfreq_grid(
          mydata = x,
          statistic = statistic,
          pollutant = pollutant,
          ws.int = ws.int,
          min.bin = min.bin
        )
        out[type] <- x[1, type]
        return(out)
      }
    ) %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na()

  # Setup strip labels
  strip.dat <- strip.fun(results_grid, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]

  # Square root transformation
  results_grid <-
    dplyr::mutate(results_grid, weights = .data$weights^(1 / coef))

  # Handle breaks and colours
  if (any(is.null(breaks)) || any(is.na(breaks))) {
    breaks <- unique(c(0, pretty(results_grid$weights, 200)))
    br <- pretty((c(0, results_grid$weights)^coef), n = 10) # breaks for scale
  } else {
    br <- breaks
  }
  col <- openColours(cols, (length(breaks) - 1))

  # Categorise weights and clean data
  results_grid <-
    dplyr::mutate(
      results_grid,
      div = cut(.data$weights, breaks, include.lowest = TRUE),
      # deal with missing weights data
      weights = dplyr::case_when(
        is.nan(.data$weights) ~ 0,
        is.na(.data$weights) ~ 0,
        .default = weights
      )
    )

  #  scale key setup ################################################################################################
  if (key.header == "weighted.mean") {
    key.header <- c("contribution", "(%)")
  }

  legend <- list(
    col = col[1:(length(breaks) - 1)],
    at = breaks,
    labels = list(at = br^(1 / coef), labels = br),
    space = key.position,
    auto.text = auto.text,
    footer = key.footer,
    header = key.header,
    height = 1,
    width = 1.5,
    fit = "all"
  )

  legend <- makeOpenKeyLegend(key, legend, "polarFreq")

  type_formula <- paste(type, collapse = "+")
  myform <- formula(paste0("ws ~ wd | ", type_formula))
  span <- ws.int * floor(max.ws / ws.int) + ws.int + offset

  xyplot.args <- list(
    x = myform,
    xlim = 1.03 * c(-span, span),
    ylim = 1.03 * c(-span, span),
    data = results_grid,
    par.strip.text = list(cex = 0.8),
    type = "n",
    strip = strip,
    strip.left = strip.left,
    as.table = TRUE,
    aspect = 1,
    scales = list(draw = FALSE),
    panel = function(x, y, subscripts, ...) {
      panel.xyplot(x, y, ...)

      subdata <- results_grid[subscripts, ]

      for (i in seq_len(nrow(subdata))) {
        colour <- col[as.numeric(subdata$div[i])]
        colour <- grDevices::adjustcolor(colour, alpha.f = alpha)
        draw_polarfreq_poly(subdata$wd[i], subdata$ws[i], colour)
      }

      # annotate
      if (ws.int < max.ws) {
        # don't annotate if only 1 interval
        angles <- seq(0, 2 * pi, length = 360)
        sapply(seq(0, 20 * grid.line, by = grid.line), function(x) {
          llines(
            (offset + x) * sin(angles),
            (offset + x) * cos(angles),
            col = "grey",
            lty = 5
          )
        })

        # radial labels
        sapply(seq(0, 20 * grid.line, by = grid.line), function(x) {
          ltext(
            (offset + x) * sin(pi / 4),
            (offset + x) * cos(pi / 4),
            x,
            cex = 0.7
          )
        })
      }

      larrows(-span, 0, -offset, 0, code = 1, length = 0.1)
      larrows(span, 0, offset, 0, code = 1, length = 0.1)
      larrows(0, -span, 0, -offset, code = 1, length = 0.1)
      larrows(0, span, 0, offset, code = 1, length = 0.1)

      ltext(-span * 0.95, 0.07 * span, "W", cex = 0.7)
      ltext(0.07 * span, -span * 0.95, "S", cex = 0.7)
      ltext(0.07 * span, span * 0.95, "N", cex = 0.7)
      ltext(span * 0.95, 0.07 * span, "E", cex = 0.7)
    },
    legend = legend
  )

  # handle extra arguments

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  # set graphics parameters
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  extra.args <- list(...)
  extra.args$xlab <- quickText(extra.args$xlab %||% "", auto.text)
  extra.args$ylab <- quickText(extra.args$ylab %||% "", auto.text)
  extra.args$main <- quickText(extra.args$main %||% "", auto.text)

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  xyplot.args <- listUpdate(xyplot.args, extra.args)

  # plot
  plt <- do.call(xyplot, xyplot.args)

  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    } else {
      plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    }
  }
  output <- list(
    plot = plt,
    data = dplyr::tibble(results_grid),
    call = match.call()
  )
  class(output) <- "openair"

  invisible(output)
}

#' Validate many inputs for the polarFreq function
#' @noRd
validate_polarfreq_args <- function(statistic, pollutant, trans, breaks) {
  rlang::arg_match(
    statistic,
    values = c(
      "frequency",
      "mean",
      "median",
      "max",
      "min",
      "stdev",
      "weighted.mean"
    )
  )

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

  # deal with 'trans' and 'breaks' conflict
  if (!(any(is.null(breaks)) || any(is.na(breaks)))) {
    if (trans) {
      cli::cli_warn(c(
        "x" = "Providing {.arg breaks} conflicts with {.code trans = TRUE}.",
        "i" = "Setting {.code trans = FALSE}."
      ))
      trans <- FALSE
    }
  }

  # apply square root transform?
  coef <- ifelse(trans, 2, 1)

  return(list(
    statistic = statistic,
    pollutant = pollutant,
    coef = coef
  ))
}

#' Prepare data for polarFreq
#' @noRd
prepare_polarfreq_grid <- function(
  mydata,
  statistic,
  pollutant,
  ws.int,
  min.bin
) {
  # bin the data into all wds and defined ws
  grid_data <-
    mydata %>%
    dplyr::mutate(
      wd_factor = factor(.data$wd),
      ws_factor = factor(ws.int * ceiling(.data$ws / ws.int))
    ) %>%
    dplyr::filter(
      !is.na(.data$wd),
      !is.na(.data$ws)
    )

  # count values in each bin - needed regardless of statistic
  bin_counts <-
    dplyr::count(
      grid_data,
      .data$wd_factor,
      .data$ws_factor,
      .drop = FALSE,
      name = "bin_count"
    )

  if (statistic == "frequency") {
    # simple situation - just use weights from counts
    weights_df <- dplyr::rename(bin_counts, weights = "bin_count")
  } else {
    # summarise per bin for different stats types
    weights_df <-
      grid_data %>%
      dplyr::filter(!is.na(.data[[pollutant]])) %>%
      dplyr::summarise(
        weights = switch(
          statistic,
          "mean" = mean(.data[[pollutant]], na.rm = TRUE),
          "median" = median(.data[[pollutant]], na.rm = TRUE),
          "min" = min(.data[[pollutant]], na.rm = TRUE),
          "max" = max(.data[[pollutant]], na.rm = TRUE),
          "stdev" = sd(.data[[pollutant]], na.rm = TRUE),
          "weighted.mean" = {
            poll_mean <- mean(.data[[pollutant]], na.rm = TRUE)
            poll_count <- dplyr::n()
            total_count <- nrow(grid_data)
            poll_mean * poll_count / total_count
          }
        ),
        .by = c("wd_factor", "ws_factor")
      )

    # normalise weighted means
    if (statistic == "weighted.mean") {
      weights_df <- dplyr::mutate(
        weights_df,
        weights = 100 * .data$weights / sum(.data$weights)
      )
    }
  }

  weights_df |>
    tidyr::complete(.data$wd_factor, .data$ws_factor) |>
    dplyr::left_join(
      bin_counts,
      by = dplyr::join_by("wd_factor", "ws_factor")
    ) |>
    dplyr::mutate(
      weights = dplyr::if_else(
        .data$bin_count < min.bin,
        NA_real_,
        .data$weights
      ),
      ws = as.numeric(as.character(.data$ws_factor)),
      wd = as.numeric(as.character(.data$wd_factor))
    ) |>
    dplyr::select("ws", "wd", "weights") |>
    dplyr::arrange(.data$wd, .data$ws)
}
