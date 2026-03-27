#' Plot heat maps of atmospheric composition data
#'
#' The [trendLevel()] function provides a way of rapidly showing a large amount
#' of data in a condensed form. In one plot, the variation in the concentration
#' of one pollutant can to shown as a function of between two and four
#' categorical properties. The default arguments plot hour of day on the x-axis
#' and month of year on the y-axis. However, `x`, `y` and `type` and summarising
#' statistics can all be modified to provide a range of other similar plots, all
#' being passed to [cutData()] for discretisation. The average wind speed and
#' direction in each bin can also be plotted using the `windflow` argument.
#'
#' [trendLevel()] allows the use of third party summarising functions via the
#' `statistic` option. Any additional function arguments not included within a
#' function called using `statistic` should be supplied as a list of named
#' parameters and sent using `stat.args`. For example, the encoded option
#' `statistic = "mean"` is equivalent to `statistic = mean, stat.args =
#' list(na.rm = TRUE)` or the R command `mean(x, na.rm = TRUE)`. Many R
#' functions and user's own code could be applied in a similar fashion, subject
#' to the following restrictions: the first argument sent to the function must
#' be the data series to be analysed; the name 'x' cannot be used for any of the
#' extra options supplied in `stat.args`; and the function should return the
#' required answer as a numeric or `NA`. Note: If the supplied function returns
#' more than one answer, currently only the first of these is retained and used
#' by [trendLevel()]. All other returned information will be ignored without
#' warning. If the function terminates with an error when it is sent an empty
#' data series, the option `stat.safe.mode` should not be set to `FALSE` or
#' [trendLevel()] may fail. Note: The `stat.safe.mode = TRUE` option returns an
#' NA without warning for empty data series.
#'
#' @param mydata The openair data frame to use to generate the [trendLevel()]
#'   plot.
#' @param pollutant The name of the data series in `mydata` to sample to produce
#'   the [trendLevel()] plot.
#' @param x,y,type The name of the data series to use as the [trendLevel()]
#'   x-axis, y-axis or conditioning variable, passed to [cutData()]. These are
#'   used before applying `statistic`. [trendLevel()] does not allow duplication
#'   in `x`, `y` and `type` options.
#' @param rotate.axis The rotation to be applied to `trendLevel` `x` and `y`
#'   axes. The default, `c(90, 0)`, rotates the x axis by 90 degrees but does
#'   not rotate the y axis. If only one value is supplied, this is applied to
#'   both axes; if more than two values are supplied, only the first two are
#'   used.
#' @param n.levels The number of levels to split `x`, `y` and `type` data into
#'   if numeric. The default, `c(10, 10, 4)`, cuts numeric `x` and `y` data into
#'   ten levels and numeric `type` data into four levels. This option is ignored
#'   for date conditioning and factors. If less than three values are supplied,
#'   three values are determined by recursion; if more than three values are
#'   supplied, only the first three are used.
#' @param limits The colour scale range to use when generating the
#'   [trendLevel()] plot.
#' @param min.bin The minimum number of records required in a bin to show a
#'   value. Bins with fewer than `min.bin` records are set to `NA`. The default
#'   is 1, i.e., all bins with no records are set to `NA`. Setting `min.bin` to
#'   a value greater than 1 can be useful to exclude bins with very few records
#'   that might produce unreliable statistic values.
#' @param windflow This option calculates the vector average of wind speed and
#'   direction and shows the result as an arrow. The option is a list, e.g.,
#'   `windflow = list(col = "grey", lwd = 2, scale = 0.1)`. This option requires
#'   wind speed (`ws`) and wind direction (`wd`) to be available.
#' @param cols The colour set to use to colour the [trendLevel()] surface.
#'   `cols` is passed to [openColours()] for evaluation.
#' @param auto.text Automatic routine text formatting. `auto.text = TRUE` passes
#'   axis labels, legend titles, and facet labels through [quickText()] to
#'   provide common text formatting.  The alternative `auto.text = FALSE` turns
#'   this option off and passes any supplied labels to the plot without
#'   modification.
#' @param key.header,key.footer `key.header` adds a title to the legend, passed
#'   through [quickText()] if `quick.text = TRUE`. `key.footer` is no longer
#'   directly supported, and is appended to the bottom of `key.header` to form
#'   part of the legend title.
#' @param key.position Location where the scale key should be plotted. Allowed
#'   arguments currently include `"top"`, `"right"`, `"bottom"`, and `"left"`.
#' @param key Show a key?
#' @param strip.position Location where the facet 'strips' are located when
#'   using `type`. When one `type` is provided, can be one of `"left"`,
#'   `"right"`, `"bottom"` or `"top"`. When two `type`s are provided, this
#'   argument defines whether the strips are "switched" and can take either
#'   `"x"`, `"y"`, or `"both"`. For example, `"x"` will switch the 'top' strip
#'   locations to the bottom of the plot.
#' @param breaks,labels If a categorical colour scale is required then `breaks`
#'   should be specified. These should be provided as a numeric vector, e.g.,
#'   `breaks = c(0, 50, 100, 1000)`. Users should set the maximum value of
#'   `breaks` to exceed the maximum data value to ensure it is within the
#'   maximum final range, e.g., 100--1000 in this case. Labels will
#'   automatically be generated, but can be customised by passing a character
#'   vector to `labels`, e.g., `labels = c("good", "bad", "very bad")`. In this
#'   example, `0 - 50` will be `"good"` and so on. Note there is one less label
#'   than break.
#' @param statistic The statistic to apply when aggregating the data; default is
#'   the mean. Can be one of `"mean"`, `"max"`, `"min"`, `"median"`,
#'   `"frequency"`, `"sum"`, `"sd"`, `"percentile"`. Note that `"sd"` is the
#'   standard deviation, `"frequency"` is the number (frequency) of valid
#'   records in the period and `"data.cap"` is the percentage data capture.
#'   `"percentile"` is the percentile level (%) between 0-100, which can be set
#'   using the `"percentile"` option. Functions can also be sent directly via
#'   `statistic`; see 'Details' for more information.
#' @param percentile The percentile level used when `statistic = "percentile"`.
#'   The default is 95%.
#' @param stat.args Additional options to be used with `statistic` if this is a
#'   function. The extra options should be supplied as a list of named
#'   parameters; see 'Details' for more information.
#' @param stat.safe.mode An addition protection applied when using functions
#'   directly with `statistic` that most users can ignore. This option returns
#'   `NA` instead of running `statistic` on binned sub samples that are empty.
#'   Many common functions terminate with an error message when applied to an
#'   empty dataset. So, this option provides a mechanism to work with such
#'   functions. For a very few cases, e.g., for a function that counted missing
#'   entries, it might need to be set to `FALSE`; see 'Details' for more
#'   information.
#' @param drop.unused.types Hide unused/empty `type` conditioning cases. Some
#'   conditioning options may generate empty cases for some data sets, e.g. a
#'   hour of the day when no measurements were taken. Empty `x` and `y` cases
#'   generate 'holes' in individual plots. However, empty `type` cases would
#'   produce blank panels if plotted. Therefore, the default, `TRUE`, excludes
#'   these empty panels from the plot. The alternative `FALSE` plots all `type`
#'   panels.
#' @param col.na Colour to be used to show missing data.
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available:
#'   - `xlab`, `ylab` and `main` override the x-axis label, y-axis label, and plot title.
#'   - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'   - `fontsize` overrides the overall font size of the plot.
#'   - `border` sets the border colour of each tile.
#' @export
#' @return an [openair][openair-package] object.
#' @author Karl Ropkins
#' @author David Carslaw
#' @author Jack Davison
#' @examples
#' # basic use
#' # default statistic = "mean"
#' trendLevel(mydata, pollutant = "nox")
#'
#' # applying same as 'own' statistic
#' my.mean <- function(x) mean(x, na.rm = TRUE)
#' trendLevel(mydata, pollutant = "nox", statistic = my.mean)
#'
#' # alternative for 'third party' statistic
#' # trendLevel(mydata, pollutant = "nox", statistic = mean,
#' #           stat.args = list(na.rm = TRUE))
#'
#' \dontrun{
#' # example with categorical scale
#' trendLevel(mydata,
#'   pollutant = "no2",
#'   border = "white", statistic = "max",
#'   breaks = c(0, 50, 100, 500),
#'   labels = c("low", "medium", "high"),
#'   cols = c("forestgreen", "yellow", "red")
#' )
#' }
trendLevel <- function(
  mydata,
  pollutant = "nox",
  x = "month",
  y = "hour",
  type = "default",
  rotate.axis = c(90, 0),
  n.levels = c(10, 10, 4),
  windflow = NULL,
  limits = NULL,
  min.bin = 1,
  cols = "default",
  auto.text = TRUE,
  key = TRUE,
  key.header = "use.stat.name",
  key.footer = pollutant,
  key.position = "right",
  strip.position = "top",
  labels = NULL,
  breaks = NULL,
  statistic = c(
    "mean",
    "max",
    "min",
    "median",
    "frequency",
    "sum",
    "sd",
    "percentile"
  ),
  percentile = 95,
  stat.args = NULL,
  stat.safe.mode = TRUE,
  drop.unused.types = TRUE,
  col.na = "white",
  plot = TRUE,
  ...
) {
  if (rlang::is_logical(key) && !key) {
    key.position <- "none"
  }

  extra.args <- rlang::list2(...)

  # check length of x
  if (length(x) > 1) {
    x <- x[1]
    cli::cli_warn(
      c(
        "!" = "{.fun openair::trendLevel} does not allow multiple {.field x} values.",
        "i" = "Setting {.field x} to '{x}'."
      )
    )
  }

  # check length of y
  if (length(y) > 1) {
    y <- y[1]
    cli::cli_warn(
      c(
        "!" = "{.fun openair::trendLevel} does not allow multiple {.field y} values.",
        "i" = "Setting {.field y} to '{y}'."
      )
    )
  }

  # check length of type
  if (length(type) > 2) {
    type <- type[1:2]
    cli::cli_warn(
      c(
        "!" = "{.fun openair::trendLevel} does not allow more than two {.field type} values.",
        "i" = "Setting {.field type} to '{type}'."
      )
    )
  }

  # ensure x, y and type are unique
  vars <- c(pollutant, x, y, type)
  if (length(vars) != length(unique(vars))) {
    cli::cli_abort(
      c(
        "x" = "{.fun openair::trendLevel} could not rationalise plot structure.",
        "i" = "Duplicate term(s) in {.field pollutant} ('{pollutant}'), {.field x} ('{x}'), {.field y} ('{y}'), and {.field type} ('{type}')."
      )
    )
  }

  # make sure windflow is a list
  # lower the default range - good for heatmap plots
  windflow <- resolve_windflow_opts(windflow, range = c(0.01, 0.5))

  if (
    windflow$windflow &&
      (x %in%
        c("ws", "wd") ||
        y %in% c("ws", "wd") ||
        any(type %in% c("ws", "wd")))
  ) {
    cli::cli_abort(
      "In {.fun openair::trendLevel}, cannot use {.arg windflow} and have {.arg x}, {.arg y} and/or {.arg type} be 'ws' or 'wd'."
    )
  }

  # number vector handling
  ls.check.fun <- function(vector, vector.name, len) {
    if (!is.numeric(vector)) {
      cli::cli_warn(
        c(
          "!" = "{.fun openair::trendLevel} ignored unrecognised {.field vector.name} option.",
          "i" = "See {.fun openair::trendLevel} for more information."
        ),
        call. = FALSE
      )
      # use current default
      vector <- eval(formals(trendLevel)[[vector.name]])
    }

    if (length(vector) < len) {
      vector <- rep(vector, len)[1:len]
    }
    # insert default if not given
    ifelse(is.na(vector), eval(formals(trendLevel)[[vector.name]]), vector)
  }
  rotate.axis <- ls.check.fun(rotate.axis, "rotate.axis", 2)
  n.levels <- ls.check.fun(n.levels, "n.levels", 3)

  # statistic handling
  if (is.character(statistic) || is.function(statistic)) {
    if (is.character(statistic)) {
      # hardcoded statistic options
      statistic <- rlang::arg_match(statistic)
      stat.name <- statistic

      if (statistic == "mean") {
        stat.fun <- mean
        stat.args <- list(na.rm = TRUE)
      }

      if (statistic == "median") {
        stat.fun <- stats::median
        stat.args <- list(na.rm = TRUE)
      }

      if (statistic == "sd") {
        stat.fun <- stats::sd
        stat.args <- list(na.rm = TRUE)
      }

      if (statistic == "max") {
        stat.fun <- function(x, ...) {
          if (all(is.na(x))) {
            NA
          } else {
            max(x, ...)
          }
        }
        stat.args <- list(na.rm = TRUE)
      }

      if (statistic == "min") {
        stat.fun <- function(x, ...) {
          if (all(is.na(x))) {
            NA
          } else {
            min(x, ...)
          }
        }
        stat.args <- list(na.rm = TRUE)
      }

      if (statistic == "sum") {
        stat.fun <- function(x, ...) {
          if (all(is.na(x))) {
            NA
          } else {
            sum(x, ...)
          }
        }
        stat.args <- list(na.rm = TRUE)
      }

      if (statistic == "frequency") {
        stat.fun <- function(x, ...) {
          if (all(is.na(x))) {
            NA
          } else {
            length(stats::na.omit(x))
          }
        }
        stat.args <- NULL
      }

      if (statistic == "percentile") {
        if (percentile < 0 || percentile > 100) {
          cli::cli_abort("{.field percentile} outside {0}-{100}.")
        }
        probs <- percentile / 100
        stat.fun <- function(x, ...) {
          stats::quantile(x, probs = probs, names = FALSE, ...)
        }
        stat.args <- list(na.rm = TRUE)

        lastnum <- substr(percentile, nchar(percentile), nchar(percentile))
        numend <- dplyr::recode_values(
          lastnum,
          "1" ~ "st",
          "2" ~ "nd",
          "3" ~ "rd",
          default = "th"
        )

        stat.name <- paste0(percentile, numend, " Perc.")
      }
    } else {
      # user defined function handling
      # default unnamed stats to 'level'

      stat.name <- substitute(statistic)
      if (length(stat.name) != 1) {
        stat.name <- "level"
      }

      if (stat.safe.mode) {
        stat.fun <- function(x, ...) {
          if (all(is.na(x))) NA else statistic(x, ...)[1]
        }
      } else {
        stat.fun <- function(x, ...) statistic(x, ...)[1]
      }
    }
  } else {
    # early end for bad stats
    cli::cli_abort(
      c(
        "x" = "{.fun openair::trendLevel} could not apply given {.field statistic} option.",
        "i" = "Please provide a valid function or character vector.",
        "i" = "Valid character vectors: {.emph {eval(formals(trendLevel)$statistic)}}"
      )
    )
  }

  # checkPrep
  temp <- c(pollutant)
  if (x %in% dateTypes || y %in% dateTypes || any(type %in% dateTypes)) {
    temp <- c(temp, "date")
  }
  if (windflow$windflow) {
    temp <- c(temp, "ws", "wd")
  }

  # all of x, y, temp need to be handled as type here
  mydata <- checkPrep(mydata, temp, type = c(x, y, type), remove.calm = FALSE)

  # if using windflow, need the u & v components of wind
  if (windflow$windflow) {
    mydata <- mydata |>
      dplyr::mutate(
        wind_u = -1 * .data$ws * sin(.data$wd * pi / 180),
        wind_v = -1 * .data$ws * cos(.data$wd * pi / 180)
      ) |>
      dplyr::select(
        -c("ws", "wd")
      )
  }

  # cutData
  # different n.levels for axis and type, axes get `is.axis = TRUE`
  newdata <-
    mydata |>
    cutData(x, n.levels = n.levels[1], is.axis = TRUE, ...) |>
    cutData(y, n.levels = n.levels[2], is.axis = TRUE, ...) |>
    cutData(type, n.levels = n.levels[3], drop = "outside", ...) |>
    tidyr::complete(!!!rlang::syms(c(x, y, type)))

  # select only pollutant and axis/facet columns
  newdata <- dplyr::select(
    newdata,
    dplyr::any_of(c(pollutant, x, y, type, "wind_u", "wind_v"))
  )

  # apply statistic
  calc_stat <- function(x) {
    args <- append(stat.args, list(x = x))
    rlang::exec(stat.fun, !!!args)
  }

  # NB: don't overwrite `pollutant` initially so that `n` is still valid
  newdata_poll <-
    newdata |>
    dplyr::summarise(
      poll_avg = calc_stat(.data[[pollutant]]),
      n = length(stats::na.omit(.data[[pollutant]])),
      .by = dplyr::all_of(c(x, y, type))
    )
  names(newdata_poll)[names(newdata_poll) == "poll_avg"] <- pollutant

  # if using windflow, calculate mean wind_u and wind_v for each bin
  if (windflow$windflow) {
    newdata_met <- newdata |>
      dplyr::summarise(
        wind_u = mean(.data[["wind_u"]], na.rm = TRUE),
        wind_v = mean(.data[["wind_v"]], na.rm = TRUE),
        .by = dplyr::all_of(c(x, y, type))
      )

    newdata <-
      dplyr::left_join(
        newdata_poll,
        newdata_met,
        by = c(x, y, type)
      )
  } else {
    newdata <- newdata_poll
  }

  # if using windflow, convert wind_u and wind_v back into ws & wd
  if (windflow$windflow) {
    newdata <- newdata |>
      dplyr::mutate(
        ws = sqrt(.data$wind_u^2 + .data$wind_v^2),
        wd = (atan2(-.data$wind_u, -.data$wind_v) * 180 / pi) %% 360
      ) |>
      dplyr::select(-c("wind_u", "wind_v"))
  }

  # remove bins with fewer than min.bin records
  vars <- c(pollutant)
  if (windflow$windflow) {
    vars <- c(vars, "ws", "wd")
  }
  newdata <- dplyr::mutate(
    newdata,
    dplyr::across(
      dplyr::any_of(vars),
      \(x) ifelse(.data$n < min.bin, NA, x)
    )
  )

  # key.header, footer stat.name recovery
  if (!is.null(key.header)) {
    if (is.character(key.header)) {
      key.header <- gsub("use.stat.name", stat.name, key.header)
    }
  }
  if (!is.null(key.footer)) {
    if (is.character(key.footer)) {
      key.footer <- gsub("use.stat.name", stat.name, key.footer)
    }
  }

  # categorical colour scale or not?
  categorical <- FALSE
  if (!is.null(breaks)) {
    # get breaks
    labels <- get_labels_from_breaks(breaks, labels)

    # cut data into categories
    newdata$cuts <- cut(
      newdata[[pollutant]],
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE
    )

    categorical <- TRUE
  }

  # construct plot
  thePlot <-
    ggplot2::ggplot(
      newdata,
      ggplot2::aes(
        x = num_convert(.data[[x]]),
        y = num_convert(.data[[y]]),
        fill = .data[[ifelse(categorical, "cuts", pollutant)]]
      )
    ) +
    ggplot2::geom_tile(
      colour = extra.args$border %||% "transparent",
      show.legend = TRUE
    ) +
    ggplot2::coord_cartesian(clip = "off", expand = FALSE) +
    theme_openair(key.position) +
    set_extra_fontsize(extra.args) +
    ggplot2::labs(
      x = quickText(extra.args$xlab %||% x, auto.text = auto.text),
      y = quickText(extra.args$ylab %||% y, auto.text = auto.text),
      title = quickText(extra.args$main %||% "", auto.text = auto.text),
      fill = quickText(
        paste(
          key.header,
          key.footer,
          sep = ifelse(key.position %in% c("top", "bottom"), " ", "\n")
        ),
        auto.text = auto.text
      )
    ) +
    ggplot2::guides(
      x = ggplot2::guide_axis(check.overlap = TRUE, angle = rotate.axis[1]),
      y = ggplot2::guide_axis(check.overlap = TRUE, angle = rotate.axis[2])
    ) +
    get_facet(
      type,
      extra.args = extra.args,
      scales = "fixed",
      auto.text = auto.text,
      drop = drop.unused.types,
      strip.position = strip.position
    )

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

  # colours
  if (categorical) {
    thePlot <-
      thePlot +
      ggplot2::scale_fill_manual(
        values = openColours(
          scheme = cols,
          n = dplyr::n_distinct(levels(newdata$cuts))
        ),
        na.value = col.na,
        breaks = levels(newdata$cuts),
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
        na.value = col.na,
        oob = scales::oob_squish,
        limit = limits
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

  if (windflow$windflow) {
    thePlot <- thePlot +
      layer_windflow_opts(newdata, windflow)
  }

  if (plot) {
    plot(thePlot)
  }

  output <- list(
    plot = thePlot,
    data = dplyr::tibble(newdata),
    call = match.call()
  )
  class(output) <- "openair"
  invisible(output)
}

num_convert <- function(x) {
  y <- utils::type.convert(x, as.is = TRUE)
  if (is.numeric(y) || is.integer(y)) {
    return(y)
  } else {
    return(x)
  }
}
