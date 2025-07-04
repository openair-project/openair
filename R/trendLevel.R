#' Plot heat map trends
#'
#' The trendLevel function provides a way of rapidly showing a large amount of
#' data in a condensed form. In one plot, the variation in the concentration of
#' one pollutant can to shown as a function of three other categorical
#' properties. The default version of the plot uses y = hour of day, x = month
#' of year and type = year to provide information on trends, seasonal effects
#' and diurnal variations. However, x, y and type and summarising statistics can
#' all be modified to provide a range of other similar plots.
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
#' @param cols The colour set to use to colour the [trendLevel()] surface.
#'   `cols` is passed to [openColours()] for evaluation.
#' @param auto.text Automatic routine text formatting. `auto.text = TRUE` passes
#'   common `lattice` labelling terms (e.g. `xlab` for the x-axis, `ylab` for
#'   the y-axis and `main` for the title) to the plot via [quickText()] to
#'   provide common text formatting.  The alternative `auto.text = FALSE` turns
#'   this option off and passes any supplied labels to the plot without
#'   modification.
#' @param key.header,key.footer Adds additional text labels above and/or below
#'   the scale key, respectively. For example, passing the options `key.header =
#'   "", key.footer = c("mean","nox")` adds the addition text as a scale footer.
#'   If enabled (`auto.text = TRUE`), these arguments are passed to the scale
#'   key ([drawOpenKey()]) via [quickText()] to handle formatting. The term
#'   `"get.stat.name"`, used as the default `key.header` setting, is reserved
#'   and automatically adds statistic function names or defaults to `"level"`
#'   when unnamed functions are requested via `statistic`.
#' @param key.position Location where the scale key should be plotted. Allowed
#'   arguments currently include `"top"`, `"right"`, `"bottom"`, and `"left"`.
#' @param key Fine control of the scale key via [drawOpenKey()].
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
#' @param ... Addition options are passed on to [cutData()] for `type` handling
#'   and [lattice::levelplot()] for finer control of the plot itself.
#' @export
#' @return an [openair][openair-package] object.
#' @author Karl Ropkins
#' @author David Carslaw
#' @author Jack Davison
#' @family time series and trend functions
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
  type = "year",
  rotate.axis = c(90, 0),
  n.levels = c(10, 10, 4),
  limits = c(0, 100),
  cols = "default",
  auto.text = TRUE,
  key.header = "use.stat.name",
  key.footer = pollutant,
  key.position = "right",
  key = TRUE,
  labels = NA,
  breaks = NA,
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
  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  # set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  # reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # check length of x
  if (length(x) > 1) {
    x <- x[1]
    xlab <- xlab[1]
    cli::cli_warn(
      c(
        "!" = "{.fun trendLevel} does not allow multiple {.field x} values.",
        "i" = "Setting {.field x} to '{x}'."
      )
    )
  }

  # check length of y
  if (length(y) > 1) {
    y <- y[1]
    ylab <- ylab[1]
    cli::cli_warn(
      c(
        "!" = "{.fun trendLevel} does not allow multiple {.field y} values.",
        "i" = "Setting {.field y} to '{y}'."
      )
    )
  }

  # check length of type
  if (length(type) > 1) {
    type <- type[1]
    cli::cli_warn(
      c(
        "!" = "{.fun trendLevel} does not allow multiple {.field type} values.",
        "i" = "Setting {.field type} to '{type}'."
      )
    )
  }

  # ensure x, y and type are unique
  vars <- c(pollutant, x, y, type)
  if (length(vars) != length(unique(vars))) {
    cli::cli_abort(
      c(
        "x" = "{.fun trendLevel} could not rationalise plot structure.",
        "i" = "Duplicate term(s) in {.field pollutant} ('{pollutant}'), {.field x} ('{x}'), {.field y} ('{y}'), and {.field type} ('{type}')."
      )
    )
  }

  # assume pollutant scale is not a categorical value
  category <- FALSE
  if (!anyNA(breaks)) {
    # assign labels if no labels are given
    if (anyNA(labels)) {
      labels <- c()
      for (i in seq_along(breaks)) {
        lhs <- breaks[i]
        rhs <- breaks[i + 1]
        str <- paste(lhs, rhs, sep = " - ")
        labels <- append(labels, str)
      }
      labels <- labels[-i]
    }
    category <- TRUE
  }

  # extra.args
  extra.args <- list(...)

  # font size
  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab %||% x, auto.text = auto.text)
  extra.args$ylab <- quickText(extra.args$ylab %||% y, auto.text = auto.text)
  extra.args$main <- quickText(extra.args$main %||% "", auto.text = auto.text)

  # number vector handling
  ls.check.fun <- function(vector, vector.name, len) {
    if (!is.numeric(vector)) {
      cli::cli_warn(
        c(
          "!" = "{.fun trendLevel} ignored unrecognised {.field vector.name} option.",
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
            length(na.omit(x))
          }
        }
        stat.args <- NULL
      }

      if (statistic == "percentile") {
        if (percentile < 0 | percentile > 100) {
          cli::cli_abort("{.field percentile} outside {0}-{100}.")
        }
        probs <- percentile / 100
        stat.fun <- function(x, ...) {
          stats::quantile(x, probs = probs, names = FALSE, ...)
        }
        stat.args <- list(na.rm = TRUE)

        lastnum <- substr(percentile, nchar(percentile), nchar(percentile))
        numend <- dplyr::case_match(
          lastnum,
          "1" ~ "st",
          "2" ~ "nd",
          "3" ~ "rd",
          .default = "th"
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
        "x" = "{.fun trendLevel} could not apply given {.field statistic} option.",
        "i" = "Please provide a valid function or character vector.",
        "i" = "Valid character vectors: {.emph {eval(formals(trendLevel)$statistic)}}"
      )
    )
  }

  # checkPrep
  temp <- c(pollutant)
  if ("date" %in% names(mydata)) {
    temp <- c("date", pollutant)
  }

  # all of x, y, temp need to be handled as type here
  mydata <- checkPrep(mydata, temp, type = c(x, y, type), remove.calm = FALSE)

  # cutData
  # different n.levels for axis and type, axes get `is.axis = TRUE`
  newdata <-
    mydata |>
    cutData(x, n.levels = n.levels[1], is.axis = TRUE, ...) |>
    cutData(y, n.levels = n.levels[2], is.axis = TRUE, ...) |>
    cutData(type, n.levels = n.levels[3], ...)

  # select only pollutant and axis/facet columns
  newdata <- newdata[c(pollutant, x, y, type)]

  calc_stat <- function(x) {
    args <- append(stat.args, list(x = x))
    rlang::exec(stat.fun, !!!args)
  }
  newdata <-
    newdata |>
    dplyr::summarise(
      {{ pollutant }} := calc_stat(.data[[pollutant]]),
      .by = dplyr::all_of(c(x, y, type))
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c(x, y, type)),
      function(x) factor(x, ordered = FALSE)
    )) |>
    as.data.frame()

  # plot setup
  temp <- paste(type, collapse = "+")
  myform <- formula(paste0(pollutant, " ~ ", x, " * ", y, " | ", temp))

  if (type == "default") {
    myform <- formula(paste0(pollutant, " ~ ", x, " * ", y))
  }

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

  # special case handling
  # layout for wd
  if (
    length(type) == 1 && type[1] == "wd" && !"layout" %in% names(extra.args)
  ) {
    # re-order to make sensible layout

    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    newdata$wd <- ordered(newdata$wd, levels = wds)
    wd.ok <- sapply(
      wds,
      function(x) if (x %in% unique(newdata$wd)) FALSE else TRUE
    )
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    newdata$wd <- factor(newdata$wd)
    extra.args$layout <- c(3, 3)
    if (!"skip" %in% names(extra.args)) {
      extra.args$skip <- skip
    }
  }

  temp <- if (is.factor(newdata[, type[1]])) {
    levels(newdata[, type[1]])
  } else {
    unique(newdata[, type[1]])
  }
  temp <- sapply(temp, function(x) quickText(x, auto.text))
  if (is.factor(temp)) {
    temp <- as.character(temp)
  }
  strip <- strip.custom(
    factor.levels = temp,
    strip.levels = c(TRUE, FALSE),
    strip.names = FALSE
  )

  strip.left <- if (length(type) == 1) {
    FALSE
  } else {
    temp <- sapply(
      unique(newdata[, type[2]]),
      function(x) quickText(x, auto.text)
    )
    if (is.factor(temp)) {
      temp <- as.character(temp)
    }
    strip.custom(factor.levels = temp)
  }

  suppressWarnings(trellis.par.set(list(
    strip.background = list(col = "white")
  )))

  scales <- list(
    x = list(rot = rotate.axis[1]),
    y = list(rot = rotate.axis[2])
  )

  # categorical colour scale or not?

  if (category) {
    # check the breaks and labels are consistent
    if (length(labels) + 1 != length(breaks)) {
      stop("Need one more break than labels")
    }

    # cut data into categories
    newdata$cuts <- cut(
      newdata[, pollutant],
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE
    )
    n <- length(levels(newdata$cuts))

    col.regions <- openColours(cols, n)
    col.scale <- breaks
    legend <- list(
      col = col.regions,
      space = key.position,
      auto.text = auto.text,
      labels = levels(newdata$cuts),
      footer = key.footer,
      header = key.header,
      height = 0.8,
      width = 1.5,
      fit = "scale",
      plot.style = "other"
    )

    col.scale <- breaks
    legend <- makeOpenKeyLegend(key, legend, "windRose")
  } else {
    # continuous colour scale

    # auto-scaling
    nlev <- 200 # preferred number of intervals

    # handle missing breaks arguments

    if (missing(limits)) {
      breaks <- seq(
        min(newdata[, pollutant], na.rm = TRUE),
        max(newdata[, pollutant], na.rm = TRUE),
        length.out = nlev
      )

      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs
    } else {
      # handle user limits and clipping
      breaks <- seq(min(limits), max(limits), length.out = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs

      # case where user max is < data max
      if (max(limits) < max(newdata[, pollutant], na.rm = TRUE)) {
        id <- which(newdata[, pollutant] > max(limits))
        newdata[id, pollutant] <- max(limits)
        labs[length(labs)] <- paste(">", labs[length(labs)])
      }

      # case where user min is > data min
      if (min(limits) > min(newdata[, pollutant], na.rm = TRUE)) {
        id <- which(newdata[, pollutant] < min(limits))
        newdata[id, pollutant] <- min(limits)
        labs[1] <- paste("<", labs[1])
      }
    }

    nlev2 <- length(breaks)

    col.regions <- openColours(cols, (nlev2 - 1))

    col.scale <- breaks

    legend <- list(
      col = col.regions,
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
    legend <- makeOpenKeyLegend(key, legend, "polarPlot")
  }

  # turn off colorkey
  colorkey <- FALSE

  # stop overlapping labels
  yscale.lp <- function(...) {
    ans <- yscale.components.default(...)
    ans$left$labels$check.overlap <- TRUE
    ans$left$labels$labels <- levels(newdata[, y])
    ans$left$labels$at <- seq_along(levels(newdata[, y]))

    ans
  }
  xscale.lp <- function(...) {
    ans <- xscale.components.default(...)
    ans$bottom$labels$check.overlap <- TRUE
    ans$bottom$labels$labels <- levels(newdata[, x])
    ans$bottom$labels$at <- seq_along(levels(newdata[, x]))
    ans
  }

  # plot

  # The following listUpdate steps are used to stop this falling
  # over with a lattice error if the user passes any of
  # the locally defined options below as part of call.
  # If they do reset it is obviously Caveat emptor...

  # the axes are discrete factors - therefore can define exactly
  xlim <- range(as.numeric(newdata[, x])) + c(-0.5, 0.5)
  ylim <- range(as.numeric(newdata[, y])) + c(-0.5, 0.5)

  # lattice does not display all labels - even if requested when there are many
  # make a bit more space when there are >25
  if (length(levels(newdata[[y]])) > 25) {
    ylim <- ylim + c(-0.3, 0.3)
  }
  if (length(levels(newdata[[x]])) > 25) {
    xlim <- xlim + c(-0.3, 0.3)
  }

  # openair defaults for plot
  levelplot.args <- list(
    x = myform,
    data = newdata,
    as.table = TRUE,
    legend = legend,
    colorkey = colorkey,
    at = breaks,
    col.regions = col.regions,
    scales = scales,
    yscale.components = yscale.lp,
    xscale.components = xscale.lp,
    par.strip.text = list(cex = 0.8),
    strip = strip,
    strip.left = strip.left,
    xlim = xlim,
    ylim = ylim,
    panel = function(x, y, ...) {
      panel.fill(col = col.na)
      panel.levelplot(x, y, ...)
    }
  )
  # reset for extra.args
  levelplot.args <- listUpdate(levelplot.args, extra.args)
  plt <- do.call(levelplot, levelplot.args)

  # update for two levels
  if (length(type) > 1) {
    plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)
  }

  # openair output
  if (plot) {
    plot(plt)
  }

  output <- list(plot = plt, data = dplyr::tibble(newdata), call = match.call())
  class(output) <- "openair"
  invisible(output)
}
