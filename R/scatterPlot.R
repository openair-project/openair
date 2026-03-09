#' Flexible scatter plots
#'
#' Scatter plots with conditioning and three main approaches: conventional
#' scatterPlot, hexagonal binning and kernel density estimates. The former also
#' has options for fitting smooth fits and linear models with uncertainties
#' shown.
#'
#' [scatterPlot()] is the basic function for plotting scatter plots in flexible
#' ways in `openair`. It is flexible enough to consider lots of conditioning
#' variables and takes care of fitting smooth or linear relationships to the
#' data.
#'
#' There are four main ways of plotting the relationship between two variables,
#' which are set using the `method` option. The default `"scatter"` will plot a
#' conventional scatterPlot. In cases where there are lots of data and
#' over-plotting becomes a problem, then `method = "hexbin"` or `method =
#' "density"` can be useful. The former requires the `hexbin` package to be
#' installed.
#'
#' There is also a `method = "level"` which will bin the `x` and `y` data
#' according to the intervals set for `x.inc` and `y.inc` and colour the bins
#' according to levels of a third variable, `z`. Sometimes however, a far better
#' understanding of the relationship between three variables (`x`, `y` and `z`)
#' is gained by fitting a smooth surface through the data. See examples below.
#'
#' A smooth fit is shown if `smooth = TRUE` which can help show the overall form
#' of the data e.g. whether the relationship appears to be linear or not. Also,
#' a linear fit can be shown using `linear = TRUE` as an option.
#'
#' The user has fine control over the choice of colours and symbol type used.
#'
#' Another way of reducing the number of points used in the plots which can
#' sometimes be useful is to aggregate the data. For example, hourly data can be
#' aggregated to daily data. See [timePlot()] for examples here.
#'
#' By default plots are shown with a colour key at the bottom and in the case of
#' conditioning, strips on the top of each plot. Sometimes this may be overkill
#' and the user can opt to remove the key and/or the strip by setting `key`
#' and/or `strip` to `FALSE`. One reason to do this is to maximise the plotting
#' area and therefore the information shown.
#' @param mydata A data frame containing at least two numeric variables to plot.
#' @param x Name of the x-variable to plot. Note that x can be a date field or a
#'   factor. For example, `x` can be one of the `openair` built in types such as
#'   `"year"` or `"season"`.
#' @param y Name of the numeric y-variable to plot.
#' @param z Name of the numeric z-variable to plot for `method = "scatter"` or
#'   `method = "level"`. Note that for `method = "scatter"` points will be
#'   coloured according to a continuous colour scale, whereas for `method =
#'   "level"` the surface is coloured.
#' @param method Methods include \dQuote{scatter} (conventional scatter plot),
#'   \dQuote{hexbin} (hexagonal binning using the `hexbin` package).
#'   \dQuote{level} for a binned or smooth surface plot and \dQuote{density} (2D
#'   kernel density estimates).
#' @param group The grouping variable to use, if any. Setting this to a variable
#'   in the data frame has the effect of plotting several series in the same
#'   panel using different symbols/colours etc. If set to a variable that is a
#'   character or factor, those categories or factor levels will be used
#'   directly. If set to a numeric variable, it will split that variable in to
#'   quantiles.
#' @param avg.time This defines the time period to average to. Can be
#'   \dQuote{sec}, \dQuote{min}, \dQuote{hour}, \dQuote{day}, \dQuote{DSTday},
#'   \dQuote{week}, \dQuote{month}, \dQuote{quarter} or \dQuote{year}. For much
#'   increased flexibility a number can precede these options followed by a
#'   space. For example, a timeAverage of 2 months would be `period = "2
#'   month"`. See function `timeAverage` for further details on this. This
#'   option se useful as one method by which the number of points plotted is
#'   reduced i.e. by choosing a longer averaging time.
#' @param data.thresh The data capture threshold to use (\%) when aggregating
#'   the data using `avg.time`. A value of zero means that all available data
#'   will be used in a particular period regardless if of the number of values
#'   available. Conversely, a value of 100 will mean that all data will need to
#'   be present for the average to be calculated, else it is recorded as `NA`.
#'   Not used if `avg.time = "default"`.
#' @param statistic The statistic to apply when aggregating the data; default is
#'   the mean. Can be one of "mean", "max", "min", "median", "frequency", "sd",
#'   "percentile". Note that "sd" is the standard deviation and "frequency" is
#'   the number (frequency) of valid records in the period. "percentile" is the
#'   percentile level (\%) between 0-100, which can be set using the
#'   "percentile" option - see below. Not used if `avg.time = "default"`.
#' @param percentile The percentile level in percent used when `statistic =
#'   "percentile"` and when aggregating the data with `avg.time`. The default is
#'   95. Not used if `avg.time = "default"`.
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in `cutData` e.g.
#'   \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so on. For example,
#'   `type = "season"` will produce four plots --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data frame.
#'   If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Type can be up length two e.g. `type = c("season", "weekday")` will produce
#'   a 2x2 plot split by season and day of the week. Note, when two types are
#'   provided the first forms the columns and the second the rows.
#' @param smooth A smooth line is fitted to the data if `TRUE`; optionally with
#'   95 percent confidence intervals shown. For `method = "level"` a smooth
#'   surface will be fitted to binned data.
#' @param spline A smooth spline is fitted to the data if `TRUE`. This is
#'   particularly useful when there are fewer data points or when a connection
#'   line between a sequence of points is required.
#' @param linear A linear model is fitted to the data if `TRUE`; optionally with
#'   95 percent confidence intervals shown. The equation of the line and R2
#'   value is also shown.
#' @param ci Should the confidence intervals for the smooth/linear fit be shown?
#' @param mod.line If `TRUE` three lines are added to the scatter plot to help
#'   inform model evaluation. The 1:1 line is solid and the 1:0.5 and 1:2 lines
#'   are dashed. Together these lines help show how close a group of points are
#'   to a 1:1 relationship and also show the points that are within a factor of
#'   two (FAC2).
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   `RColorBrewer` colours --- see the `openair` `openColours` function for
#'   more details. For user defined the user can supply a list of colour names
#'   recognised by R (type `colours()` to see the full list). An example would
#'   be `cols = c("yellow", "green", "blue")`
#' @param plot.type Type of plot: \dQuote{p} (points, default), \dQuote{l}
#'   (lines) or \dQuote{b} (both points and lines).
#' @param key Should a key be drawn? The default is `TRUE`.
#' @param key.title The title of the key (if used).
#' @param key.columns Number of columns to be used in the key. With many
#'   pollutants a single column can make to key too wide. The user can thus
#'   choose to use several columns by setting `columns` to be less than the
#'   number of pollutants.
#' @param key.position Location where the scale key is to plotted.  Allowed
#'   arguments currently include \dQuote{top}, \dQuote{right}, \dQuote{bottom}
#'   and \dQuote{left}.
#' @param strip Should a strip be drawn? The default is `TRUE`.
#' @param log.x Should the x-axis appear on a log scale? The default is `FALSE`.
#'   If `TRUE` a well-formatted log10 scale is used. This can be useful for
#'   checking linearity once logged.
#' @param log.y Should the y-axis appear on a log scale? The default is `FALSE`.
#'   If `TRUE` a well-formatted log10 scale is used. This can be useful for
#'   checking linearity once logged.
#' @param x.inc The x-interval to be used for binning data when `method =
#'   "level"`.
#' @param y.inc The y-interval to be used for binning data when `method =
#'   "level"`.
#' @param limits For `method = "level"` the function does its best to choose
#'   sensible limits automatically. However, there are circumstances when the
#'   user will wish to set different ones. The limits are set in the form
#'   `c(lower, upper)`, so `limits = c(0, 100)` would force the plot limits to
#'   span 0-100.
#' @param windflow This option allows a scatter plot to show the wind
#'   speed/direction shows as an arrow. The option is a list e.g. `windflow =
#'   list(col = "grey", lwd = 2, scale = 0.1)`. This option requires wind speed
#'   (`ws`) and wind direction (`wd`) to be available.
#' @param y.relation This determines how the y-axis scale is plotted.
#'   \dQuote{same} ensures all panels use the same scale and \dQuote{free} will
#'   use panel-specific scales. The latter is a useful setting when plotting
#'   data with very different values.
#' @param x.relation This determines how the x-axis scale is plotted.
#'   \dQuote{same} ensures all panels use the same scale and \dQuote{free} will
#'   use panel-specific scales. The latter is a useful setting when plotting
#'   data with very different values.
#' @param ref.x See `ref.y` for details.
#' @param ref.y A list with details of the horizontal lines to be added
#'   representing reference line(s). For example, `ref.y = list(h = 50, lty =
#'   5)` will add a dashed horizontal line at 50. Several lines can be plotted
#'   e.g. `ref.y = list(h = c(50, 100), lty = c(1, 5), col = c("green",
#'   "blue"))`. See `gg_ref_y` for more details.
#' @param k Smoothing parameter supplied to `gam` for fitting a smooth
#'   surface when `method = "level"`.
#' @param dist When plotting smooth surfaces (`method = "level"` and
#'   `smooth = TRUE`), `dist` controls how far from the original data
#'   the predictions should be made. See `exclude.too.far` from the
#'   `mgcv` package. Data are first transformed to a unit square. Values
#'   should be between 0 and 1.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels will automatically try and format pollutant names
#'   and units properly e.g.  by subscripting the \sQuote{2} in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract plot components and plotting them in other ways.
#' @param ... Other graphical parameters passed onto `cutData` and ggplot2
#'   layers. For example, `scatterPlot` passes the option `hemisphere =
#'   "southern"` on to `cutData` to provide southern (rather than default
#'   northern) hemisphere handling of `type = "season"`. Common graphical
#'   parameters include `xlab`, `ylab`, `main`, `alpha`, `cex` (point size),
#'   `lwd` (line width) and `lty` (line type). For `method = "hexbin"` a
#'   log-scale fill is applied by default; pass `trans = NULL` to disable or
#'   provide custom `trans` and `inv` transform functions.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @seealso [timePlot()] and [timeAverage()] for details on selecting averaging
#'   times and other statistics in a flexible way
#' @examples
#' # load openair data if not loaded already
#' dat2004 <- selectByDate(mydata, year = 2004)
#'
#' # basic use, single pollutant
#'
#' scatterPlot(dat2004, x = "nox", y = "no2")
#' \dontrun{
#' # scatterPlot by year
#' scatterPlot(mydata, x = "nox", y = "no2", type = "year")
#' }
#'
#' # scatterPlot by day of the week, removing key at bottom
#' scatterPlot(dat2004,
#'   x = "nox", y = "no2", type = "weekday", key =
#'     FALSE
#' )
#'
#' # example of the use of continuous where colour is used to show
#' # different levels of a third (numeric) variable
#' # plot daily averages and choose a filled plot symbol (pch = 16)
#' # select only 2004
#' \dontrun{
#'
#' scatterPlot(dat2004, x = "nox", y = "no2", z = "co", avg.time = "day", pch = 16)
#'
#' # show linear fit, by year
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", smooth =
#'     FALSE, linear = TRUE
#' )
#'
#' # do the same, but for daily means...
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", smooth =
#'     FALSE, linear = TRUE, avg.time = "day"
#' )
#'
#' # log scales
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", smooth =
#'     FALSE, linear = TRUE, avg.time = "day", log.x = TRUE, log.y = TRUE
#' )
#'
#' # also works with the x-axis in date format (alternative to timePlot)
#' scatterPlot(mydata,
#'   x = "date", y = "no2", avg.time = "month",
#'   key = FALSE
#' )
#'
#' ## multiple types and grouping variable and continuous colour scale
#' scatterPlot(mydata, x = "nox", y = "no2", z = "o3", type = c("season", "weekend"))
#'
#' # use hexagonal binning
#' scatterPlot(mydata, x = "nox", y = "no2", method = "hexbin")
#'
#' # scatterPlot by year
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", method =
#'     "hexbin"
#' )
#'
#' ## bin data and plot it - can see how for high NO2, O3 is also high
#' scatterPlot(mydata, x = "nox", y = "no2", z = "o3", method = "level", dist = 0.02)
#'
#' ## fit surface for clearer view of relationship
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", z = "o3", method = "level",
#'   x.inc = 10, y.inc = 2, smooth = TRUE
#' )
#' }
scatterPlot <- function(
  mydata,
  x = "nox",
  y = "no2",
  z = NA,
  method = "scatter",
  group = NA,
  avg.time = "default",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  type = "default",
  smooth = FALSE,
  spline = FALSE,
  linear = FALSE,
  ci = TRUE,
  mod.line = FALSE,
  cols = "hue",
  plot.type = "p",
  key = TRUE,
  key.title = group,
  key.columns = 1,
  key.position = "right",
  strip = TRUE,
  log.x = FALSE,
  log.y = FALSE,
  x.inc = NULL,
  y.inc = NULL,
  limits = NULL,
  windflow = NULL,
  y.relation = "same",
  x.relation = "same",
  ref.x = NULL,
  ref.y = NULL,
  k = NA,
  dist = 0.02,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  ## handle key
  if (isFALSE(key)) {
    key.position <- "none"
  }

  ## extra args
  extra.args <- rlang::list2(...)
  extra.args$layout <- extra.args$layout %||% NULL
  extra.args$cex <- extra.args$cex %||% 1
  extra.args$lwd <- extra.args$lwd %||% 1
  extra.args$lty <- extra.args$lty %||% 1
  extra.args$alpha <- extra.args$alpha %||% NA

  ## validate
  if (is.na(z) && method == "level") {
    cli::cli_abort(
      "Need to specify {.arg z} when using {.code method = 'level'}."
    )
  }
  if (!is.na(group) && any(group %in% type)) {
    cli::cli_abort("Can't have {.arg group} also in {.arg type}.")
  }
  rlang::arg_match(method, c("scatter", "hexbin", "level", "density"))
  rlang::arg_match(plot.type, c("p", "l", "b", "s", "S", "spline"))

  ## axis labels / title
  xlab <- quickText(extra.args$xlab %||% x, auto.text)
  ylab <- quickText(extra.args$ylab %||% y, auto.text)
  main <- quickText(
    extra.args$main,
    auto.text
  )

  ## prepare data
  mydata <- prepare_scatter_data(
    mydata = mydata,
    x = x,
    y = y,
    z = z,
    group = group,
    type = type,
    avg.time = avg.time,
    data.thresh = data.thresh,
    statistic = statistic,
    percentile = percentile,
    windflow = windflow,
    log.x = log.x,
    log.y = log.y,
    ...
  )

  ## dispatch to method
  plt <- switch(
    method,
    scatter = scatter_scatter(
      mydata = mydata,
      x = x,
      y = y,
      z = z,
      group = group,
      type = type,
      cols = cols,
      plot.type = plot.type,
      smooth = smooth,
      spline = spline,
      linear = linear,
      ci = ci,
      mod.line = mod.line,
      windflow = windflow,
      log.x = log.x,
      log.y = log.y,
      x.relation = x.relation,
      y.relation = y.relation,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      key.title = key.title,
      key.columns = key.columns,
      strip = strip,
      limits = limits,
      k = k,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      main = main,
      extra.args = extra.args
    ),
    hexbin = scatter_hexbin(
      mydata = mydata,
      x = x,
      y = y,
      group = group,
      type = type,
      cols = cols,
      linear = linear,
      ci = ci,
      mod.line = mod.line,
      log.x = log.x,
      log.y = log.y,
      x.relation = x.relation,
      y.relation = y.relation,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      strip = strip,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      main = main,
      extra.args = extra.args
    ),
    level = scatter_level(
      mydata = mydata,
      x = x,
      y = y,
      z = z,
      type = type,
      cols = cols,
      statistic = statistic,
      smooth = smooth,
      mod.line = mod.line,
      x.inc = x.inc,
      y.inc = y.inc,
      limits = limits,
      log.x = log.x,
      log.y = log.y,
      x.relation = x.relation,
      y.relation = y.relation,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      strip = strip,
      k = k,
      dist = dist,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      main = main,
      extra.args = extra.args
    ),
    density = scatter_density(
      mydata = mydata,
      x = x,
      y = y,
      type = type,
      cols = cols,
      mod.line = mod.line,
      log.x = log.x,
      log.y = log.y,
      x.relation = x.relation,
      y.relation = y.relation,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      strip = strip,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      main = main,
      extra.args = extra.args
    )
  )

  plt <- plt + set_extra_fontsize(extra.args)

  if (plot) {
    print(plt)
  }

  output <- list(plot = plt, data = mydata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}


# Data preparation --------------------------------------------------------

#' Prepare data for scatterPlot
#' @noRd
prepare_scatter_data <- function(
  mydata,
  x,
  y,
  z,
  group,
  type,
  avg.time,
  data.thresh,
  statistic,
  percentile,
  windflow,
  log.x,
  log.y,
  ...
) {
  ## types needed for time averaging
  types <- if (!is.na(group)) c(type, group) else type

  ## time average if needed
  if (avg.time != "default") {
    mydata <- cutData(mydata, types, ...)
    if ("default" %in% types) {
      mydata$default <- 0L
    }
    mydata <- timeAverage(
      mydata,
      type = types,
      avg.time = avg.time,
      statistic = statistic,
      percentile = percentile,
      data.thresh = data.thresh
    )
  }

  ## build required variable list
  vars <- if (any(type %in% dateTypes) || avg.time != "default") {
    c("date", x, y)
  } else {
    c(x, y)
  }

  if (!is.na(group)) {
    if (
      group %in% dateTypes || avg.time != "default" || any(type %in% dateTypes)
    ) {
      vars <- if (group %in% dateTypes) {
        unique(c(vars, "date"))
      } else {
        unique(c(vars, "date", group))
      }
    } else {
      vars <- unique(c(vars, group))
    }
  }

  if (!is.null(windflow)) {
    vars <- unique(c(vars, "wd", "ws"))
  }
  if (!is.na(z)) {
    vars <- c(vars, z)
  }

  mydata <- checkPrep(mydata, unique(vars), type)

  ## filter for log scales (require strictly positive)
  if (log.x) {
    mydata <- mydata[!is.na(mydata[[x]]) & mydata[[x]] > 0, ]
  }
  if (log.y) {
    mydata <- mydata[!is.na(mydata[[y]]) & mydata[[y]] > 0, ]
  }

  mydata
}


# Scatter method ----------------------------------------------------------

#' Build ggplot for method = "scatter"
#' @noRd
scatter_scatter <- function(
  mydata,
  x,
  y,
  z,
  group,
  type,
  cols,
  plot.type,
  smooth,
  spline,
  linear,
  ci,
  mod.line,
  windflow,
  log.x,
  log.y,
  x.relation,
  y.relation,
  ref.x,
  ref.y,
  key.position,
  key.title,
  key.columns,
  strip,
  limits,
  k,
  auto.text,
  xlab,
  ylab,
  main,
  extra.args
) {
  has_z <- !is.na(z)
  has_group <- !is.na(group)

  ## cut data for conditioning and group
  mydata <- cutData(mydata, type)
  if (has_group) {
    mydata <- cutData(mydata, group)
  }

  ## colour palette info
  if (has_z) {
    ## continuous z overrides group
    n_groups <- 1L
    group_var <- NULL
    group_levels <- "all"
    if (missing(cols) || identical(cols, "hue")) cols <- "default"
  } else if (has_group) {
    group_levels <- levels(as.factor(mydata[[group]]))
    n_groups <- length(group_levels)
    group_var <- group
  } else {
    n_groups <- 1L
    group_var <- NULL
    group_levels <- "all"
  }

  myColors <- openColours(cols, n_groups)

  ## panel column names for LM labels and facet
  panel_cols <- setdiff(type, "default")

  ## facet scales
  facet_scales <- relation_to_facet_scales(x.relation, y.relation)

  ## point size / alpha
  pt_size <- extra.args$cex %||% 1
  pt_alpha <- extra.args$alpha %||% NA
  pt_shape <- extra.args$pch %||%
    (if (has_z) {
      16L
    } else if (has_group) {
      NULL
    } else {
      16L
    })
  ln_width <- (extra.args$lwd %||% 1) / 2
  ln_type <- extra.args$lty %||% 1

  ## base plot
  plt <- ggplot2::ggplot(mydata, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  ## add points / lines based on plot.type and colour mode
  if (has_z) {
    z_aes <- ggplot2::aes(color = .data[[z]])
    plt <- plt +
      .scatter_geoms(
        plot.type,
        z_aes,
        pt_size,
        pt_alpha,
        pt_shape,
        ln_width,
        ln_type
      )

    ## continuous colour scale for z
    if (is.null(limits)) {
      z_limits <- NULL
      z_labs <- ggplot2::waiver()
    } else {
      z_limits <- limits
      z_labs <- .scatter_limit_labels(mydata[[z]], limits)
    }
    plt <- plt +
      ggplot2::scale_color_gradientn(
        colors = openColours(cols, 200),
        limits = z_limits,
        oob = scales::oob_squish,
        labels = z_labs,
        guide = ggplot2::guide_colorbar(
          title = quickText(z, auto.text),
          position = key.position
        )
      )
  } else if (has_group) {
    grp_aes <- ggplot2::aes(
      color = .data[[group]],
      shape = .data[[group]]
    )
    plt <- plt +
      .scatter_geoms(
        plot.type,
        grp_aes,
        pt_size,
        pt_alpha,
        NULL,
        ln_width,
        ln_type
      )

    pch_vals <- if (!is.null(pt_shape)) {
      rep_len(pt_shape, n_groups)
    } else {
      seq_len(n_groups) %% 25 + 1L
    }

    pol_name <- sapply(group_levels, function(x) quickText(x, auto.text))

    theGuide <- ggplot2::guide_legend(
      title = quickText(key.title, auto.text),
      ncol = key.columns,
      theme = ggplot2::theme(
        legend.title.position = if (key.position %in% c("left", "right")) {
          "top"
        } else {
          key.position
        }
      )
    )

    plt <- plt +
      ggplot2::scale_color_manual(
        values = myColors,
        labels = pol_name,
        guide = theGuide
      ) +
      ggplot2::scale_shape_manual(
        values = pch_vals,
        labels = pol_name,
        guide = theGuide
      )
  } else {
    ## single series — fixed colour, no legend
    plt <- plt +
      .scatter_geoms(
        plot.type,
        ggplot2::aes(),
        pt_size,
        pt_alpha,
        pt_shape %||% 16L,
        ln_width,
        ln_type,
        fixed_color = myColors[1L]
      )
  }

  ## smooth fit
  if (smooth) {
    gam_formula <- if (is.na(k)) {
      y ~ s(x, bs = "cs")
    } else {
      stats::as.formula(paste0("y ~ s(x, k=", k, ", bs='cs')"))
    }
    plt <- plt +
      ggplot2::geom_smooth(
        method = "gam",
        formula = gam_formula,
        se = ci,
        color = "grey20"
      )
  }

  ## spline fit
  if (spline || plot.type %in% c("s", "S", "spline")) {
    plt <- plt +
      ggplot2::geom_smooth(
        method = "loess",
        se = FALSE,
        color = "grey20",
        linewidth = ln_width
      )
  }

  ## linear fit
  if (linear) {
    if (has_group) {
      ## per-group lines: explicitly map colour/fill so geom_smooth produces one
      ## line per group (the base ggplot() call has no group aesthetic)
      plt <- plt +
        ggplot2::geom_smooth(
          mapping = ggplot2::aes(
            color = .data[[group_var]],
            fill = .data[[group_var]]
          ),
          method = "lm",
          formula = y ~ x,
          se = ci,
          linewidth = 1,
          linetype = 5,
          show.legend = FALSE
        )
    } else {
      plt <- plt +
        ggplot2::geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = ci,
          color = "black",
          fill = "grey80",
          linewidth = 1,
          linetype = 5
        )
    }
    lm_labs <- compute_lm_labels(
      mydata = mydata,
      x = x,
      y = y,
      group_var = group_var,
      panel_cols = panel_cols,
      x_nam = x,
      y_nam = y,
      auto.text = auto.text
    )
    if (!is.null(lm_labs) && nrow(lm_labs) > 0L) {
      if (has_group) {
        plt <- plt +
          ggplot2::geom_text(
            data = lm_labs,
            mapping = ggplot2::aes(
              x = .data$x_pos,
              y = .data$y_pos,
              label = .data$label,
              vjust = .data$g_idx * 1.5,
              color = .data[[group_var]]
            ),
            hjust = 0,
            inherit.aes = FALSE,
            parse = TRUE,
            size = 2.5,
            show.legend = FALSE
          )
      } else {
        plt <- plt +
          ggplot2::geom_text(
            data = lm_labs,
            mapping = ggplot2::aes(
              x = .data$x_pos,
              y = .data$y_pos,
              label = .data$label,
              vjust = .data$g_idx * 1.5
            ),
            hjust = 0,
            inherit.aes = FALSE,
            parse = TRUE,
            size = 2.5,
            color = "black"
          )
      }
    }
  }

  ## model evaluation lines (FAC2)
  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  ## windflow arrows
  if (!is.null(windflow)) {
    plt <- plt +
      layer_windflow(
        ggplot2::aes(ws = .data$ws, wd = .data$wd),
        arrow = grid::arrow(
          angle = windflow$angle %||% 15,
          length = windflow$length %||% grid::unit(0.5, "lines"),
          ends = windflow$ends %||% "last",
          type = windflow$type %||% "closed"
        ),
        colour = windflow$col %||% "grey",
        linewidth = (windflow$lwd %||% 1) / 2
      )
  }

  ## reference lines
  plt <- plt + gg_ref_x(ref.x) + gg_ref_y(ref.y)

  ## axis scales
  plt <- plt +
    .scatter_x_scale(log.x, x, mydata, extra.args) +
    .scatter_y_scale(log.y, y, mydata, extra.args) +
    gg_coord_limits(extra.args)

  ## faceting
  plt <- plt +
    get_facet(
      type,
      extra.args,
      scales = facet_scales,
      auto.text = auto.text,
      drop = FALSE
    )

  ## theme + labels + strip control
  plt <- plt +
    theme_openair(
      key.position = if (n_groups == 1L && !has_z) "none" else key.position
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    .strip_theme(strip)

  plt
}


# Hexbin method -----------------------------------------------------------

#' Build ggplot for method = "hexbin"
#' @noRd
scatter_hexbin <- function(
  mydata,
  x,
  y,
  group,
  type,
  cols,
  linear,
  ci,
  mod.line,
  log.x,
  log.y,
  x.relation,
  y.relation,
  ref.x,
  ref.y,
  key.position,
  strip,
  auto.text,
  xlab,
  ylab,
  main,
  extra.args
) {
  mydata <- cutData(mydata, type)

  myColors <- openColours(cols, 200)

  ## hexbin fill transform (default: log; trans = NULL → none)
  hex_transform <- if (
    "trans" %in% names(extra.args) && is.null(extra.args$trans)
  ) {
    "identity"
  } else if ("trans" %in% names(extra.args) && is.function(extra.args$trans)) {
    inv_fn <- extra.args$inv %||% function(x) x
    scales::new_transform("hex_custom", extra.args$trans, inv_fn)
  } else {
    "log"
  }

  plt <- ggplot2::ggplot(mydata, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_hex(bins = extra.args$bins %||% 30) +
    ggplot2::scale_fill_gradientn(
      colors = myColors,
      transform = hex_transform,
      labels = scales::label_number(accuracy = 1),
      guide = ggplot2::guide_colorbar(
        title = "count",
        position = key.position
      )
    )

  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  if (linear) {
    plt <- plt +
      ggplot2::geom_smooth(
        method = "lm",
        se = ci,
        color = "black",
        fill = "grey80",
        linewidth = 1,
        linetype = 5
      )
    lm_labs <- compute_lm_labels(
      mydata = mydata,
      x = x,
      y = y,
      group_var = NA,
      panel_cols = setdiff(type, "default"),
      x_nam = x,
      y_nam = y,
      auto.text = auto.text
    )
    if (!is.null(lm_labs) && nrow(lm_labs) > 0L) {
      plt <- plt +
        ggplot2::geom_text(
          data = lm_labs,
          mapping = ggplot2::aes(
            x = .data$x_pos,
            y = .data$y_pos,
            label = .data$label,
            vjust = .data$g_idx * 1.5
          ),
          hjust = 0,
          inherit.aes = FALSE,
          parse = TRUE,
          size = 2.5
        )
    }
  }

  plt <- plt +
    gg_ref_x(ref.x) +
    gg_ref_y(ref.y) +
    .scatter_x_scale(log.x, x, mydata, extra.args) +
    .scatter_y_scale(log.y, y, mydata, extra.args) +
    gg_coord_limits(extra.args) +
    get_facet(
      type,
      extra.args,
      scales = relation_to_facet_scales(x.relation, y.relation),
      auto.text = auto.text,
      drop = FALSE
    ) +
    theme_openair(key.position = key.position) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    .strip_theme(strip)

  plt
}


# Level method ------------------------------------------------------------

#' Build ggplot for method = "level"
#' @noRd
scatter_level <- function(
  mydata,
  x,
  y,
  z,
  type,
  cols,
  statistic,
  smooth,
  mod.line,
  x.inc,
  y.inc,
  limits,
  log.x,
  log.y,
  x.relation,
  y.relation,
  ref.x,
  ref.y,
  key.position,
  strip,
  k,
  dist,
  auto.text,
  xlab,
  ylab,
  main,
  extra.args
) {
  mydata <- cutData(mydata, type)

  ## bin intervals
  if (is.null(x.inc)) {
    x.inc <- prettyGap(mydata[[x]])
  }
  if (is.null(y.inc)) {
    y.inc <- prettyGap(mydata[[y]])
  }

  mydata$xgrid <- round_any(mydata[[x]], x.inc)
  mydata$ygrid <- round_any(mydata[[y]], y.inc)

  ## type columns (cutData creates a "default" column for type = "default")
  type_cols <- type # keep all, including "default"
  vars <- c("xgrid", "ygrid", type_cols)

  ## aggregate only if needed (data not already gridded)
  if (nrow(unique(mydata[c("xgrid", "ygrid")])) != nrow(mydata)) {
    vars_select <- c(vars, z)
    ## retain only needed columns but preserve all type cols
    avail_cols <- intersect(vars_select, names(mydata))
    agg_by_cols <- intersect(vars, names(mydata))
    mydata <- mydata |>
      dplyr::select(dplyr::all_of(avail_cols)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(agg_by_cols))) |>
      dplyr::summarise(
        !!z := switch(
          statistic,
          frequency = length(.data[[z]]),
          median = stats::median(.data[[z]], na.rm = TRUE),
          mean(.data[[z]], na.rm = TRUE) # default: mean
        ),
        .groups = "drop"
      )
  }

  ## smooth surface via GAM
  if (smooth) {
    k_val <- if (is.na(k)) 10L else k
    mydata <- mapType(
      mydata,
      type = type,
      fun = function(df) smooth_level_grid(df, z, k_val, dist),
      .include_default = TRUE
    )
  }

  ## colour scale limits
  z_vals <- mydata[[z]]
  if (is.null(limits)) {
    z_limits <- range(z_vals, na.rm = TRUE)
    scale_labs <- ggplot2::waiver()
  } else {
    z_limits <- limits
    scale_labs <- .scatter_limit_labels(z_vals, limits)
    ## clip values to limits
    mydata[[z]] <- pmin(pmax(mydata[[z]], limits[1L]), limits[2L])
  }

  if (missing(cols) || identical(cols, "hue")) {
    cols <- "default"
  }
  myColors <- openColours(cols, 200)

  geom_fn <- if (smooth) ggplot2::geom_raster else ggplot2::geom_tile

  plt <- ggplot2::ggplot(
    mydata,
    ggplot2::aes(x = .data$xgrid, y = .data$ygrid, fill = .data[[z]])
  ) +
    geom_fn() +
    ggplot2::scale_fill_gradientn(
      colors = myColors,
      limits = z_limits,
      oob = scales::oob_squish,
      labels = scale_labs,
      na.value = "transparent",
      guide = ggplot2::guide_colorbar(
        title = quickText(z, auto.text),
        position = key.position
      )
    )

  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  plt <- plt +
    gg_ref_x(ref.x) +
    gg_ref_y(ref.y) +
    .scatter_x_scale(log.x, "xgrid", mydata, extra.args) +
    .scatter_y_scale(log.y, "ygrid", mydata, extra.args) +
    gg_coord_limits(extra.args) +
    get_facet(
      type,
      extra.args,
      scales = relation_to_facet_scales(x.relation, y.relation),
      auto.text = auto.text,
      drop = FALSE
    ) +
    theme_openair(key.position = key.position) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    .strip_theme(strip)

  plt
}

#' Fit smooth GAM surface for level plot
#' @noRd
smooth_level_grid <- function(mydata, z, k, dist) {
  res <- 201L
  mydata <- stats::na.omit(mydata)

  myform <- stats::as.formula(paste0(
    z,
    " ~ ti(xgrid, k=",
    k,
    ") + ti(ygrid, k=",
    k,
    ") + ti(xgrid, ygrid, k=",
    k,
    ")"
  ))

  Mgam <- mgcv::gam(myform, data = mydata)
  xseq <- seq(min(mydata$xgrid), max(mydata$xgrid), length.out = res)
  yseq <- seq(min(mydata$ygrid), max(mydata$ygrid), length.out = res)
  new.dat <- expand.grid(xgrid = xseq, ygrid = yseq)

  new.dat[[z]] <- as.vector(mgcv::predict.gam(Mgam, newdata = new.dat))

  ## exclude predictions too far from original data
  too_far <- mgcv::exclude.too.far(
    rep(xseq, res),
    rep(yseq, each = res),
    mydata$xgrid,
    mydata$ygrid,
    dist = dist
  )
  new.dat[[z]][too_far] <- NA_real_

  new.dat
}


# Density method ----------------------------------------------------------

#' Build ggplot for method = "density"
#' @noRd
scatter_density <- function(
  mydata,
  x,
  y,
  type,
  cols,
  mod.line,
  log.x,
  log.y,
  x.relation,
  y.relation,
  ref.x,
  ref.y,
  key.position,
  strip,
  auto.text,
  xlab,
  ylab,
  main,
  extra.args
) {
  mydata <- cutData(mydata, type)

  if (missing(cols) || identical(cols, "hue")) {
    cols <- "default"
  }

  ## number of fill levels for density — use 9 discrete bands
  n_bands <- 9L
  fill_vals <- c("transparent", openColours(cols, n_bands - 1L))

  plt <- ggplot2::ggplot(mydata, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::stat_density_2d_filled(
      contour_var = "ndensity",
      bins = n_bands
    ) +
    ggplot2::scale_fill_manual(
      values = fill_vals,
      guide = ggplot2::guide_colorsteps(
        title = "intensity",
        position = key.position
      )
    )

  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  plt <- plt +
    gg_ref_x(ref.x) +
    gg_ref_y(ref.y) +
    .scatter_x_scale(log.x, x, mydata, extra.args) +
    .scatter_y_scale(log.y, y, mydata, extra.args) +
    gg_coord_limits(extra.args) +
    get_facet(
      type,
      extra.args,
      scales = relation_to_facet_scales(x.relation, y.relation),
      auto.text = auto.text,
      drop = FALSE
    ) +
    theme_openair(key.position = key.position) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    .strip_theme(strip)

  plt
}


# Shared helpers ----------------------------------------------------------

#' Add geom layers based on plot.type
#' @noRd
.scatter_geoms <- function(
  plot.type,
  mapping,
  size,
  alpha,
  shape,
  lwd,
  lty,
  fixed_color = NULL
) {
  alpha_val <- if (is.na(alpha)) 1 else alpha
  pt_args <- list(mapping = mapping, size = size, alpha = alpha_val)
  if (!is.null(shape)) {
    pt_args$shape <- shape
  }
  if (!is.null(fixed_color)) {
    pt_args$color <- fixed_color
  }

  ln_args <- list(
    mapping = mapping,
    linewidth = lwd,
    linetype = lty,
    alpha = alpha_val
  )
  if (!is.null(fixed_color)) {
    ln_args$color <- fixed_color
  }

  switch(
    plot.type,
    "p" = list(do.call(ggplot2::geom_point, pt_args)),
    "l" = list(do.call(ggplot2::geom_line, ln_args)),
    "b" = list(
      do.call(ggplot2::geom_line, ln_args),
      do.call(ggplot2::geom_point, pt_args)
    ),
    ## spline/S/s handled via smooth
    list(do.call(ggplot2::geom_point, pt_args))
  )
}

#' Build x-axis scale (log or linear)
#' @noRd
.scatter_x_scale <- function(log.x, x_col, mydata, extra.args) {
  if (log.x) {
    ggplot2::scale_x_continuous(
      transform = "log10",
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    )
  } else {
    x_vals <- mydata[[x_col]]
    is_date <- inherits(x_vals, c("Date", "POSIXct", "POSIXlt"))
    if (is_date) {
      if (inherits(x_vals, "Date")) {
        ggplot2::scale_x_date(
          labels = extra.args$date.format %||% ggplot2::waiver(),
          expand = ggplot2::expansion(mult = c(0.025, 0.025))
        )
      } else {
        ggplot2::scale_x_datetime(
          date_labels = extra.args$date.format %||% ggplot2::waiver(),
          expand = ggplot2::expansion(mult = c(0.025, 0.025))
        )
      }
    } else {
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.05))
      )
    }
  }
}

#' Build y-axis scale (log or linear)
#' @noRd
.scatter_y_scale <- function(log.y, y_col, mydata, extra.args) {
  if (log.y) {
    ggplot2::scale_y_continuous(
      transform = "log10",
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    )
  } else {
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    )
  }
}

#' Apply xlim/ylim via coord_cartesian so data outside limits is not dropped
#' @noRd
gg_coord_limits <- function(extra.args) {
  xlim <- extra.args$xlim
  ylim <- extra.args$ylim
  if (!is.null(xlim) || !is.null(ylim)) {
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  } else {
    list()
  }
}

#' Build custom labels for clipped colour limits
#' @noRd
.scatter_limit_labels <- function(z_vals, limits) {
  function(breaks) {
    labs <- as.character(breaks)
    if (max(limits) < max(z_vals, na.rm = TRUE)) {
      labs[which.max(breaks)] <- paste(">", labs[which.max(breaks)])
    }
    if (min(limits) > min(z_vals, na.rm = TRUE)) {
      labs[which.min(breaks)] <- paste("<", labs[which.min(breaks)])
    }
    labs
  }
}

#' Add strip theme override
#' @noRd
.strip_theme <- function(strip) {
  if (!strip) {
    ggplot2::theme(
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
  } else {
    list()
  }
}

#' FAC2 model evaluation lines
#'
#' Adds three reference lines for model evaluation (1:1, 1:2, 1:0.5). Correct
#' on any axis transformation as `geom_function` operates in data space.
#' @noRd
gg_mod_lines <- function() {
  list(
    ggplot2::geom_function(fun = identity, linetype = 1L, color = "black"),
    ggplot2::geom_function(
      fun = function(x) 2 * x,
      linetype = 5L,
      color = "black"
    ),
    ggplot2::geom_function(
      fun = function(x) x / 2,
      linetype = 5L,
      color = "black"
    )
  )
}

## Convert a variable name to a plotmath-safe string fragment using quickText.
## quickText() returns an expression object; deparse(expr[[1]]) converts the
## inner call back to a string that can be embedded in a paste(...) plotmath
## expression rendered by geom_text(parse = TRUE).
.fmt_varname <- function(name, auto.text) {
  if (!auto.text) {
    return(paste0("'", name, "'"))
  }
  qt <- quickText(name, auto.text = TRUE)
  if (is.expression(qt)) {
    deparse(qt[[1]])
  } else {
    paste0("'", as.character(qt), "'")
  }
}

## Returns a data frame with one row per (panel × group) combination
## containing the fitted equation, R² and positioning info for geom_text.
## @noRd
compute_lm_labels <- function(
  mydata,
  x,
  y,
  group_var,
  panel_cols,
  x_nam,
  y_nam,
  auto.text
) {
  by_cols <- c(
    panel_cols,
    if (!is.null(group_var) && !is.na(group_var)) group_var else character(0L)
  )
  by_cols <- by_cols[by_cols != "default"]

  ## fit lm for a single subset; returns NULL on failure or < 3 obs
  fit_one <- function(df, g_idx = 1L) {
    lm_dat <- stats::na.omit(data.frame(xv = df[[x]], yv = df[[y]]))
    if (nrow(lm_dat) < 3L) {
      return(NULL)
    }
    tryCatch(
      {
        mod <- stats::lm(yv ~ xv, data = lm_dat)
        b <- stats::coef(mod)
        r2 <- summary(mod)$r.squared
        symb <- if (b[1L] >= 0) "+" else ""
        ## Build a plotmath string: variable names are deparsed quickText
        ## expressions embedded in paste(...); geom_text uses parse = TRUE.
        x_pm <- .fmt_varname(x_nam, auto.text)
        y_pm <- .fmt_varname(y_nam, auto.text)
        lbl <- paste0(
          "paste(",
          y_pm,
          ", '=",
          sprintf("%.2f", b[2L]),
          "[', ",
          x_pm,
          ", ']",
          symb,
          sprintf("%.2f", b[1L]),
          " ', R^2, '=",
          sprintf("%.2f", r2),
          "')"
        )
        data.frame(
          x_pos = -Inf,
          y_pos = Inf,
          g_idx = g_idx,
          label = lbl,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) NULL
    )
  }

  ## add group index within each panel for y-offset
  has_group <- !is.null(group_var) && !is.na(group_var)
  if (has_group) {
    panel_only <- setdiff(by_cols, group_var)
    if (length(panel_only) > 0L) {
      mydata <- mydata |>
        dplyr::group_by(dplyr::across(dplyr::all_of(panel_only))) |>
        dplyr::mutate(
          .g_idx = match(.data[[group_var]], unique(.data[[group_var]]))
        ) |>
        dplyr::ungroup()
    } else {
      mydata <- dplyr::mutate(
        mydata,
        .g_idx = match(.data[[group_var]], unique(.data[[group_var]]))
      )
    }
  } else {
    mydata <- dplyr::mutate(mydata, .g_idx = 1L)
  }

  if (length(by_cols) == 0L) {
    return(fit_one(mydata))
  }

  ## split by panel + group, fit lm for each subset
  split_cols <- c(by_cols, ".g_idx")
  grouped <- dplyr::group_by(mydata, dplyr::across(dplyr::all_of(split_cols)))
  keys <- dplyr::group_keys(grouped)
  groups <- dplyr::group_split(grouped)

  results <- lapply(seq_along(groups), function(i) {
    r <- fit_one(groups[[i]], g_idx = keys$.g_idx[[i]])
    if (is.null(r)) {
      return(NULL)
    }
    key_row <- keys[i, setdiff(names(keys), ".g_idx"), drop = FALSE]
    rownames(key_row) <- NULL
    rownames(r) <- NULL
    cbind(key_row, r)
  })

  dplyr::bind_rows(Filter(Negate(is.null), results))
}
