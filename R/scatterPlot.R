#' Flexible scatter plots
#'
#' Scatter plots with conditioning and three main approaches: conventional
#' scatterPlot, hexagonal binning and kernel density estimates. The former also
#' has options for fitting smooth fits and linear models with uncertainties
#' shown.
#'
#' [scatterPlot()] is the basic function for plotting scatter plots in flexible
#' ways in `openair`. It is flexible enough to consider lots of
#' conditioning variables and takes care of fitting smooth or linear
#' relationships to the data.
#'
#' There are four main ways of plotting the relationship between two variables,
#' which are set using the `method` option. The default `"scatter"`
#' will plot a conventional scatterPlot. In cases where there are lots of data
#' and over-plotting becomes a problem, then `method = "hexbin"` or
#' `method = "density"` can be useful. The former requires the
#' `hexbin` package to be installed.
#'
#' There is also a `method = "level"` which will bin the `x` and
#' `y` data according to the intervals set for `x.inc` and
#' `y.inc` and colour the bins according to levels of a third variable,
#' `z`. Sometimes however, a far better understanding of the relationship
#' between three variables (`x`, `y` and `z`) is gained by
#' fitting a smooth surface through the data. See examples below.
#'
#' A smooth fit is shown if `smooth = TRUE` which can help show the overall
#' form of the data e.g. whether the relationship appears to be linear or not.
#' Also, a linear fit can be shown using `linear = TRUE` as an option.
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
#' and/or `strip` to `FALSE`. One reason to do this is to maximise the
#' plotting area and therefore the information shown.
#' @param mydata A data frame containing at least two numeric variables to plot.
#' @param x Name of the x-variable to plot. Note that x can be a date field or a
#'   factor. For example, `x` can be one of the `openair` built in
#'   types such as `"year"` or `"season"`.
#' @param y Name of the numeric y-variable to plot.
#' @param z Name of the numeric z-variable to plot for `method = "scatter"`
#'   or `method = "level"`. Note that for `method = "scatter"` points
#'   will be coloured according to a continuous colour scale, whereas for
#'   `method = "level"` the surface is coloured.
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
#'   the data using `avg.time`. A value of zero means that all available
#'   data will be used in a particular period regardless if of the number of
#'   values available. Conversely, a value of 100 will mean that all data will
#'   need to be present for the average to be calculated, else it is recorded as
#'   `NA`. Not used if `avg.time = "default"`.
#' @param statistic The statistic to apply when aggregating the data; default is
#'   the mean. Can be one of "mean", "max", "min", "median", "frequency", "sd",
#'   "percentile". Note that "sd" is the standard deviation and "frequency" is
#'   the number (frequency) of valid records in the period. "percentile" is the
#'   percentile level (\%) between 0-100, which can be set using the
#'   "percentile" option - see below. Not used if `avg.time = "default"`.
#' @param percentile The percentile level in percent used when `statistic =
#'   "percentile"` and when aggregating the data with `avg.time`. The
#'   default is 95. Not used if `avg.time = "default"`.
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
#' @param smooth A smooth line is fitted to the data if `TRUE`; optionally
#'   with 95 percent confidence intervals shown. For `method = "level"` a
#'   smooth surface will be fitted to binned data.
#' @param spline A smooth spline is fitted to the data if `TRUE`. This is
#'   particularly useful when there are fewer data points or when a connection
#'   line between a sequence of points is required.
#' @param linear A linear model is fitted to the data if `TRUE`; optionally
#'   with 95 percent confidence intervals shown. The equation of the line and R2
#'   value is also shown.
#' @param ci Should the confidence intervals for the smooth/linear fit be shown?
#' @param mod.line If `TRUE` three lines are added to the scatter plot to
#'   help inform model evaluation. The 1:1 line is solid and the 1:0.5 and 1:2
#'   lines are dashed. Together these lines help show how close a group of
#'   points are to a 1:1 relationship and also show the points that are within a
#'   factor of two (FAC2). `mod.line` is appropriately transformed when x
#'   or y axes are on a log scale.
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   `RColorBrewer` colours --- see the `openair` `openColours`
#'   function for more details. For user defined the user can supply a list of
#'   colour names recognised by R (type `colours()` to see the full list).
#'   An example would be `cols = c("yellow", "green", "blue")`
#' @param plot.type `lattice` plot type. Can be \dQuote{p} (points ---
#'   default), \dQuote{l} (lines) or \dQuote{b} (lines and points).
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
#' @param log.x Should the x-axis appear on a log scale? The default is
#'   `FALSE`. If `TRUE` a well-formatted log10 scale is used. This can
#'   be useful for checking linearity once logged.
#' @param log.y Should the y-axis appear on a log scale? The default is
#'   `FALSE`. If `TRUE` a well-formatted log10 scale is used. This can
#'   be useful for checking linearity once logged.
#' @param x.inc The x-interval to be used for binning data when `method =
#'   "level"`.
#' @param y.inc The y-interval to be used for binning data when `method =
#'   "level"`.
#' @param limits For `method = "level"` the function does its best to
#'   choose sensible limits automatically. However, there are circumstances when
#'   the user will wish to set different ones. The limits are set in the form
#'   `c(lower, upper)`, so `limits = c(0, 100)` would force the plot
#'   limits to span 0-100.
#' @param windflow This option allows a scatter plot to show the wind
#'   speed/direction shows as an arrow. The option is a list e.g. `windflow
#'   = list(col = "grey", lwd = 2, scale = 0.1)`. This option requires wind
#'   speed (`ws`) and wind direction (`wd`) to be available.
#'
#'   The maximum length of the arrow plotted is a fraction of the plot dimension
#'   with the longest arrow being `scale` of the plot x-y dimension. Note,
#'   if the plot size is adjusted manually by the user it should be re-plotted
#'   to ensure the correct wind angle. The list may contain other options to
#'   `panel.arrows` in the `lattice` package. Other useful options
#'   include `length`, which controls the length of the arrow head and
#'   `angle`, which controls the angle of the arrow head.
#'
#'   This option works best where there are not too many data to ensure
#'   over-plotting does not become a problem.
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
#'   representing reference line(s). For example, `ref.y = list(h = 50, lty
#'   = 5)` will add a dashed horizontal line at 50. Several lines can be plotted
#'   e.g. `ref.y = list(h = c(50, 100), lty = c(1, 5), col = c("green",
#'   "blue"))`. See `panel.abline` in the `lattice` package for more
#'   details on adding/controlling lines.
#' @param k Smoothing parameter supplied to `gam` for fitting a smooth
#'   surface when `method = "level"`.
#' @param dist When plotting smooth surfaces (`method = "level"` and
#'   `smooth = TRUE`, `dist` controls how far from the original data
#'   the predictions should be made. See `exclude.too.far` from the
#'   `mgcv` package. Data are first transformed to a unit square. Values
#'   should be between 0 and 1.
#' @param map Should a base map be drawn? This option is under development.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels will automatically try and format pollutant names
#'   and units properly e.g.  by subscripting the \sQuote{2} in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract plot components and plotting them in other ways.
#' @param ... Other graphical parameters are passed onto `cutData` and an
#'   appropriate `lattice` plot function (`xyplot`, `levelplot`
#'   or `hexbinplot` depending on `method`). For example,
#'   `scatterPlot` passes the option `hemisphere = "southern"` on to
#'   `cutData` to provide southern (rather than default northern)
#'   hemisphere handling of `type = "season"`. Similarly, for the default
#'   case `method = "scatter"` common axis and title labelling options
#'   (such as `xlab`, `ylab`, `main`) are passed to `xyplot`
#'   via `quickText` to handle routine formatting. Other common graphical
#'   parameters, e.g. `layout` for panel arrangement, `pch` for plot
#'   symbol and `lwd` and `lty` for line width and type, as also
#'   available (see examples below).
#'
#'   For `method = "hexbin"` it can be useful to transform the scale if it
#'   is dominated by a few very high values. This is possible by supplying two
#'   functions: one that that applies the transformation and the other that
#'   inverses it. For log scaling (the default) for example, `trans =
#'   function(x) log(x)` and `inv = function(x) exp(x)`. For a square root
#'   transform use `trans = sqrt` and `inv = function(x) x^2`. To not
#'   carry out any transformation the options `trans = NULL` and `inv
#'   = NULL` should be used.
#' @export
#' @import mapproj hexbin
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @seealso [linearRelation()], [timePlot()] and
#'   [timeAverage()] for details on selecting averaging times and other
#'   statistics in a flexible way
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
#'
#' library(hexbin)
#' # basic use, single pollutant
#' scatterPlot(mydata, x = "nox", y = "no2", method = "hexbin")
#'
#' # scatterPlot by year
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", method =
#'     "hexbin"
#' )
#'
#'
#' ## bin data and plot it - can see how for high NO2, O3 is also high
#'
#' scatterPlot(mydata, x = "nox", y = "no2", z = "o3", method = "level", dist = 0.02)
#'
#'
#' ## fit surface for clearer view of relationship - clear effect of
#' ## increased O3
#'
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
  map = FALSE,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  ## basic function to plot single/multiple time series in flexible waysproduce scatterPlot
  ## Author: David Carslaw 27 Jan. 10
  ## method = scatter/hexbin/kernel

  x.nam <- x ## names of pollutants for linear model equation
  y.nam <- y
  thekey <- key

  xgrid <- NULL
  ygrid <- NULL
  group.number <- NULL

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    ## other local colours
    method.col <- "greyscale"
  } else {
    method.col <- cols
  }

  ## Args setup
  Args <- list(...)

  Args$xlab <- if ("xlab" %in% names(Args)) {
    quickText(Args$xlab, auto.text)
  } else {
    quickText(x, auto.text)
  }

  Args$ylab <- if ("ylab" %in% names(Args)) {
    quickText(Args$ylab, auto.text)
  } else {
    quickText(y, auto.text)
  }

  Args$key.footer <- if ("key.footer" %in% names(Args)) {
    Args$key.footer
  } else {
    NULL
  }

  if (!"aspect" %in% names(Args) && method == "hexbin") {
    Args$aspect <- 1
  }

  if (!"lwd" %in% names(Args)) {
    Args$lwd <- 1
  }

  if (!"fill" %in% names(Args)) {
    Args$fill <- "transparent"
  }

  if (!"lty" %in% names(Args)) {
    Args$lty <- 1
  }

  if (!"cex" %in% names(Args)) {
    Args$cex <- 1
  }

  if (!"layout" %in% names(Args)) {
    Args$layout <- NULL
  }

  if ("trajStat" %in% names(Args)) {
    trajStat <- Args$trajStat
  } else {
    trajStat <- "mean"
  }

  if ("date.format" %in% names(Args)) {
    Args$date.format <- Args$date.format
  } else {
    Args$date.format <- NULL
  }

  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  Args$map.cols <- Args$map.cols %||% "grey20"

  Args$map.border <- Args$map.border %||% "black"

  Args$map.alpha <- Args$map.alpha %||% 0.2

  Args$map.fill <- Args$map.fill %||% TRUE

  Args$map.res <- Args$map.res %||% "default"

  Args$map.lty <- Args$map.lty %||% 1

  Args$map.lwd <- Args$map.lwd %||% 1

  Args$traj <- Args$traj %||% FALSE

  Args$projection <- Args$projection %||% "lambert"

  Args$parameters <- Args$parameters %||% c(51, 51)

  Args$orientation <- Args$orientation %||% c(90, 0, 0)

  Args$grid.col <- Args$grid.col %||% "deepskyblue"

  Args$npoints <- Args$npoints %||% 12

  Args$origin <- Args$origin %||% TRUE

  Args$clusters <- Args$clusters %||% NA

  ## transform hexbin by default
  Args$trans <- if ("trans" %in% names(Args)) Args$trans else function(x) log(x)
  Args$inv <- if ("inv" %in% names(Args)) Args$inv else function(x) exp(x)

  # For Log scaling (adapted from lattice book
  if (log.x) {
    nlog.x <- 10
  } else {
    nlog.x <- FALSE
  }
  if (log.y) {
    nlog.y <- 10
  } else {
    nlog.y <- FALSE
  }

  ## average the data if necessary (default does nothing)
  ## note - need to average before cutting data up etc

  if (!is.na(group)) {
    types <- c(type, group)
  } else {
    types <- type
  }
  if (avg.time != "default") {
    ## can't have a type or group that is date-based
    #   if (group %in% dateTypes | type  %in% dateTypes)
    #     stop ("Can't have an averging period set and a time-based 'type' or 'group'.")
    mydata <- cutData(mydata, types)
    if ("default" %in% types) {
      mydata$default <- 0
    } ## FIX ME

    mydata <- timeAverage(
      mydata,
      type = types,
      avg.time = avg.time,
      statistic = statistic,
      percentile = percentile,
      data.thresh = data.thresh
    )
  }

  ## the following makes sure all variables are present, which depends on 'group'
  ## and 'type'
  if (is.na(z) & method == "level") {
    stop("Need to specify 'z' when using method = 'level'")
  }

  if (any(type %in% dateTypes) | !missing(avg.time)) {
    vars <- c("date", x, y)
  } else {
    vars <- c(x, y)
  }

  ## if group is present, need to add that list of variables unless it is a
  ## pre-defined date-based one
  if (!is.na(group)) {
    if (group %in% dateTypes | !missing(avg.time) | any(type %in% dateTypes)) {
      if (group %in% dateTypes) {
        vars <- unique(c(vars, "date")) ## don't need group because it is
        ## defined by date
      } else {
        vars <- unique(c(vars, "date", group))
      }
    } else {
      vars <- unique(c(vars, group))
    }
  }

  if (!is.na(group)) {
    if (group %in% type) {
      stop("Can't have 'group' also in 'type'.")
    }
  }

  ## will need date so that trajectory groups can be coloured

  if (Args$traj && method %in% c("scatter", "density", "hexbin")) {
    vars <- c(vars, "date")
  }

  ## data checks
  if (!is.null(windflow)) {
    vars <- unique(c(vars, "wd", "ws"))
  }

  if (!is.na(z)) {
    vars <- c(vars, z)
  }
  mydata <- checkPrep(mydata, unique(vars), type)

  ## remove missing data except for time series where we want to show gaps
  ## this also removes missing factors
  if (class(mydata[[x]])[1] != "Date" & !"POSIXt" %in% class(mydata[, x])) {
    ##   mydata <- na.omit(mydata)
  }

  ## if x is a factor/character, then rotate axis labels for clearer labels
  x.rot <- 0
  if ("factor" %in% class(mydata[[x]]) | "character" %in% class(mydata[, x])) {
    x.rot <- 90
    mydata[, x] <- factor(mydata[[x]])
  }

  ## continuous colors

  if (!is.na(z) & method == "scatter") {
    if (z %in% dateTypes) {
      stop(
        "You tried to use a date type for the 'z' variable. \nColour coding requires 'z' to be continuous numeric variable'"
      )
    }

    ## check to see if type is numeric/integer
    if (class(mydata[[z]]) %in% c("integer", "numeric") == FALSE) {
      stop(paste(
        "Continuous colour coding requires ",
        z,
        " to be numeric",
        sep = ""
      ))
    }

    ## don't need a key with this
    key <- NULL

    mydata <- cutData(mydata, type, ...)

    if (missing(cols)) {
      cols <- "default"
    } ## better default colours for this
    thecol <- openColours(cols, 100)[cut(mydata[[z]], 100, label = FALSE)]
    mydata$col <- thecol

    ## don't need to group by all levels - want one smooth etc
    group <- "NewGroupVar"
    mydata$NewGroupVar <- "NewGroupVar"

    if (!"pch" %in% names(Args)) {
      Args$pch <- 16
    }

    nlev <- 200

    ## handling of colour scale limits
    if (missing(limits)) {
      breaks <- seq(
        min(mydata[[z]], na.rm = TRUE),
        max(mydata[[z]], na.rm = TRUE),
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
      if (max(limits) < max(mydata[[z]], na.rm = TRUE)) {
        id <- which(mydata[[z]] > max(limits))
        mydata[[z]][id] <- max(limits)
        labs[length(labs)] <- paste(">", labs[length(labs)])
      }

      ## case where user min is > data min
      if (min(limits) > min(mydata[[z]], na.rm = TRUE)) {
        id <- which(mydata[[z]] < min(limits))
        mydata[[z]][id] <- min(limits)
        labs[1] <- paste("<", labs[1])
      }

      thecol <- openColours(
        cols,
        100
      )[cut(
        mydata[[z]],
        breaks = seq(
          limits[1],
          limits[2],
          length.out = 100
        ),
        label = FALSE
      )]

      mydata$col <- thecol
    }

    if (thekey) {
      nlev2 <- length(breaks)
      col <- openColours(cols, (nlev2 - 1))

      col.scale <- breaks
      legend <- list(
        col = col,
        at = col.scale,
        labels = list(labels = labs, at = at),
        space = key.position,
        auto.text = auto.text,
        footer = Args$key.footer,
        header = Args$key.header,
        height = 1,
        width = 1.5,
        fit = "all"
      )
      legend <- makeOpenKeyLegend(TRUE, legend, "other")
    } else {
      legend <- NULL
    }
  } else {
    ## not continuous z

    mydata <- cutData(mydata, type, ...)

    if (!is.na(group)) {
      mydata <- cutData(mydata, group, ...)
    }

    legend <- NULL
  }

  ## if no group to plot, then add a dummy one to make xyplot work
  if (is.na(group)) {
    mydata$MyGroupVar <- factor("MyGroupVar")
    group <- "MyGroupVar"
  }

  ## number of groups
  npol <- length(levels(as.factor(mydata[[group]])))

  if (!"pch" %in% names(Args)) {
    Args$pch <- seq(npol)
  }

  ## set up colours
  myColors <- openColours(cols, npol)

  ## basic function for lattice call + defaults
  temp <- paste(type, collapse = "+")
  myform <- formula(paste(y, "~", x, "|", temp, sep = ""))

  scales <- list(
    x = list(
      log = nlog.x,
      rot = x.rot,
      relation = x.relation,
      format = Args$date.format
    ),
    y = list(log = nlog.y, relation = y.relation, rot = 0)
  )

  ## don't need scales for trajectories
  if (Args$traj | method %in% c("traj", "map")) {
    scales <- list(x = list(draw = FALSE), y = list(draw = FALSE))
  }

  ## if logs are chosen, ensure data >0 for line fitting etc
  if (log.x) {
    mydata <- mydata[mydata[, x] > 0, ]
  }
  if (log.y) {
    mydata <- mydata[mydata[, y] > 0, ]
  }

  pol.name <- sapply(
    levels(mydata[[group]]),
    function(x) quickText(x, auto.text)
  )

  if (is.na(z)) {
    ## non-continuous key

    if (key & npol > 1) {
      if (plot.type == "p") {
        key <- list(
          points = list(
            col = myColors[1:npol],
            fill = Args$fill,
            cex = Args$cex
          ),
          pch = if ("pch" %in% names(Args)) Args$pch else 1,
          text = list(lab = pol.name, cex = 0.8),
          space = key.position,
          columns = key.columns,
          title = quickText(key.title, auto.text),
          cex.title = 1,
          border = "grey"
        )
      }

      if (plot.type %in% c("l", "s", "S", "spline")) {
        key <- list(
          lines = list(
            col = myColors[1:npol],
            lty = Args$lty,
            lwd = Args$lwd
          ),
          text = list(lab = pol.name, cex = 0.8),
          space = key.position,
          columns = key.columns,
          title = quickText(key.title, auto.text),
          cex.title = 1,
          border = "grey"
        )
      }

      if (plot.type == "b") {
        key <- list(
          points = list(col = myColors[1:npol]),
          pch = if ("pch" %in% names(Args)) Args$pch else 1,
          lines = list(
            col = myColors[1:npol],
            lty = Args$lty,
            lwd = Args$lwd
          ),
          text = list(lab = pol.name, cex = 0.8),
          space = key.position,
          columns = key.columns,
          title = quickText(key.title, auto.text),
          cex.title = 1,
          border = "grey"
        )
      }
    } else {
      key <- NULL
    }
  }

  ## special wd layout
  if (length(type) == 1 & type[1] == "wd" & is.null(Args$layout)) {
    ## re-order to make sensible layout
    ## starting point code as of ManKendall
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    mydata$wd <- ordered(mydata$wd, levels = wds)

    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(mydata$wd)) FALSE else TRUE
    })

    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    mydata$wd <- factor(mydata$wd)
    Args$layout <- c(3, 3)
    if (!"skip" %in% names(Args)) {
      Args$skip <- skip
    }
  }
  if (!"skip" %in% names(Args)) {
    Args$skip <- FALSE
  }

  ## proper names of stripName

  strip.dat <- strip.fun(mydata, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]

  ## no strip needed for single panel
  if (length(type) == 1 & type[1] == "default") {
    strip <- FALSE
  }

  ## not sure how to evaluate "group" in xyplot, so change to a fixed name
  id <- which(names(mydata) == group)
  names(mydata)[id] <- "MyGroupVar"

  plotType <- if (!Args$traj) c("p", "g") else "n"

  # scatter plot ------------------------------------------------------------

  if (method == "scatter") {
    if (missing(k)) {
      k <- NULL
    } ## auto-smoothing by default

    ## record openair type, distinct from plot type below
    Type <- type

    xy.args <- list(
      x = myform,
      data = mydata,
      groups = mydata$MyGroupVar,
      type = plotType,
      as.table = TRUE,
      scales = scales,
      key = key,
      par.strip.text = list(cex = 0.8),
      strip = strip,
      strip.left = strip.left,
      yscale.components = yscale.components.log10ticks,
      xscale.components = xscale.components.log10ticks,
      legend = legend,
      windflow = windflow,
      panel = panel.superpose,
      ...,
      panel.groups = function(
        x,
        y,
        col.symbol,
        col,
        type,
        col.line,
        lty,
        lwd,
        group.number,
        subscripts,
        windflow,
        ...
      ) {
        ## maximum number of actual groups in this panel
        groupMax <-
          length(unique(factor(mydata$MyGroupVar[subscripts])))

        ## specific treatment of trajectory lines
        ## in order to avoid a line back to the origin, need to process
        ## in batches

        if (Args$traj) {
          addTraj(
            mydata,
            subscripts,
            Args,
            z,
            lty,
            myColors,
            group.number,
            lwd,
            groupMax,
            Type
          )
        }

        ## add base map
        if (map && group.number == groupMax) {
          add.map(Args, ...)
        }

        ## add a windflow plot
        if (!is.null(windflow)) {
          list1 <- list(x, y, dat = mydata, subscripts)
          list2 <- windflow
          flow.args <- listUpdate(list1, list2)
          do.call(panel.windflow, flow.args)
        }

        if (!is.na(z) & !Args$traj) {
          panel.xyplot(
            x,
            y,
            col.symbol = thecol[subscripts],
            as.table = TRUE,
            ...
          )
        }

        if (is.na(z) & !Args$traj) {
          panel.xyplot(
            x,
            y,
            type = plot.type,
            col.symbol = myColors[group.number],
            col.line = myColors[group.number],
            lty = lty,
            lwd = lwd,
            as.table = TRUE,
            ...
          )
        }

        # check if grouping variable to use correct name in equation
        y.nam <-
          if (group == "MyGroupVar") {
            y.nam
          } else {
            mydata[["MyGroupVar"]][subscripts[1]]
          }

        if (linear) {
          panel.linear(
            x,
            y,
            col = "black",
            myColors = Args$fill,
            lwd = 1,
            lty = 5,
            x.nam = x.nam,
            y.nam = y.nam,
            se = ci,
            group.number = group.number
          )
        }

        if (smooth) {
          panel.gam(
            x,
            y,
            col = "grey20",
            col.se = "black",
            lty = 1,
            lwd = 1,
            se = ci,
            k = k,
            ...
          )
        }

        if (spline) {
          panel.smooth.spline(x, y, col = "grey20", lwd = lwd, ...)
        }

        if (mod.line && group.number == 1) {
          panel.modline(log.x, log.y)
        }

        ## add reference lines
        if (!is.null(ref.x)) {
          do.call(panel.abline, ref.x)
        }
        if (!is.null(ref.y)) do.call(panel.abline, ref.y)
      }
    )

    ## by default title if z set
    ## else none
    default.main <- if (is.na(z)) "" else paste(x, "vs.", y, "by levels of", z)

    Args$main <- if ("main" %in% names(Args)) {
      quickText(Args$main, auto.text)
    } else {
      quickText(default.main, auto.text)
    }

    if (!"pch" %in% names(Args)) {
      Args$pch <- 1
    }

    ## reset for Args
    xy.args <- listUpdate(xy.args, Args)

    ## plot
    plt <- do.call(xyplot, xy.args)
  }

  # hexbin plot -------------------------------------------------------------

  if (method == "hexbin") {
    hex.args <- list(
      x = myform,
      data = mydata,
      strip = strip,
      scales = scales,
      strip.left = strip.left,
      as.table = TRUE,
      yscale.components = yscale.components.log10ticks,
      xscale.components = xscale.components.log10ticks,
      par.strip.text = list(cex = 0.8),
      colorkey = TRUE,
      cex.labels = 0.8,
      cex.title = 1,
      colramp = function(n) {
        openColours(method.col, n)
      },
      ...,
      panel = function(x, subscripts, ...) {
        if (!Args$traj) {
          panel.grid(-1, -1)
        }
        panel.hexbinplot(x, ...)

        if (mod.line) {
          panel.modline(log.x, log.y)
        }

        if (linear & npol == 1) {
          panel.linear(
            x,
            mydata[[y]][subscripts],
            x.nam = x.nam,
            y.nam = y.nam,
            TRUE,
            col = "black",
            myColors = Args$fill,
            lwd = 1,
            lty = 5,
            se = ci,
            group.number = 1
          )
        }

        ## base map
        if (map) {
          add.map(Args, ...)
        }

        ## add mark for receptor location
        if (Args$origin) {
          lpoints(
            Args$receptor[1],
            Args$receptor[2],
            pch = 16,
            cex = 1.5,
            col = "black"
          )
        }

        ## add reference lines
        if (!is.null(ref.x)) {
          do.call(panel.abline, ref.x)
        }
        if (!is.null(ref.y)) do.call(panel.abline, ref.y)
      }
    )

    ## by default no title ever
    Args$main <- if ("main" %in% names(Args)) {
      quickText(Args$main, auto.text)
    } else {
      quickText("", auto.text)
    }

    if (!"pch" %in% names(Args)) {
      Args$pch <- 1
    }

    ## reset for Args
    hex.args <- listUpdate(hex.args, Args)

    ## plot
    plt <- do.call(hexbinplot, hex.args)
  }

  if (method == "level") {
    # level plot --------------------------------------------------------------

    if (missing(x.inc)) {
      x.inc <- prettyGap(mydata[[x]])
    }
    if (missing(y.inc)) {
      y.inc <- prettyGap(mydata[[y]])
    }

    ## bin data
    mydata$ygrid <- round_any(mydata[[y]], y.inc)
    mydata$xgrid <- round_any(mydata[[x]], x.inc)

    vars <- c("xgrid", "ygrid", type) # for selecting / grouping

    ## only aggregate if we have to (for data pre-gridded)
    if (
      nrow(unique(subset(mydata, select = c(xgrid, ygrid)))) != nrow(mydata)
    ) {
      if (statistic == "frequency") {
        vars_select <- c(vars, z)
        mydata <- select(mydata, vars_select) %>%
          group_by(across(vars)) %>%
          summarise(MN = length(.data[[z]]))
      }

      if (statistic == "mean") {
        vars_select <- c(vars, z)
        mydata <- select(mydata, vars_select) %>%
          group_by(across(vars)) %>%
          summarise(MN = mean(.data[[z]], na.rm = TRUE))
      }

      if (statistic == "median") {
        vars_select <- c(vars, z)
        mydata <- select(mydata, vars_select) %>%
          group_by(across(vars)) %>%
          summarise(MN = median(.data[[z]], na.rm = TRUE))
      }

      names(mydata)[which(names(mydata) == "MN")] <- z
    }

    smooth.grid <- function(mydata, z) {
      myform <- formula(paste(
        z,
        "~ ti(xgrid, k = ",
        k,
        ") + ti(ygrid, k = ",
        k,
        ") + ti(xgrid, ygrid, k = ",
        k,
        ")",
        sep = ""
      ))
      res <- 201

      mydata <- na.omit(mydata)

      Mgam <- mgcv::gam(myform, data = mydata)
      new.data <- expand.grid(
        xgrid = seq(
          min(mydata$xgrid, na.rm = TRUE),
          max(mydata$xgrid, na.rm = TRUE),
          length = res
        ),
        ygrid = seq(
          min(mydata$ygrid, na.rm = TRUE),
          max(mydata$ygrid, na.rm = TRUE),
          length = res
        )
      )

      pred <- predict.gam(Mgam, newdata = new.data)
      pred <- as.vector(pred)

      new.data[, z] <- pred

      ## exlcude too far
      ## exclude predictions too far from data (from mgcv)
      x <- seq(
        min(mydata$xgrid, na.rm = TRUE),
        max(mydata$xgrid, na.rm = TRUE),
        length = res
      )
      y <- seq(
        min(mydata$ygrid, na.rm = TRUE),
        max(mydata$ygrid, na.rm = TRUE),
        length = res
      )

      wsp <- rep(x, res)
      wdp <- rep(y, rep(res, res))

      ## data with gaps caused by min.bin
      all.data <- na.omit(data.frame(
        xgrid = mydata$xgrid,
        ygrid = mydata$ygrid,
        z
      ))

      ind <- with(
        all.data,
        exclude.too.far(
          wsp,
          wdp,
          mydata$xgrid,
          mydata$ygrid,
          dist = dist
        )
      )

      new.data[ind, z] <- NA

      new.data
    }

    if (smooth) {
      mydata <- mydata %>%
        group_by(across(type)) %>%
        do(smooth.grid(., z))
    }

    ## basic function for lattice call + defaults
    temp <- paste(type, collapse = "+")
    myform <- formula(paste(z, "~ xgrid * ygrid |", temp, sep = ""))

    nlev <- 200

    ## handling of colour scale limits
    if (missing(limits)) {
      breaks <- pretty(mydata[[z]], n = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs
    } else {
      ## handle user limits and clipping
      breaks <- pretty(limits, n = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      at <- labs

      ## case where user max is < data max
      if (max(limits) < max(mydata[[z]], na.rm = TRUE)) {
        id <- which(mydata[[z]] > max(limits))
        mydata[[z]][id] <- max(limits)
        labs[length(labs)] <- paste(">", labs[length(labs)])
      }

      ## case where user min is > data min
      if (min(limits) > min(mydata[[z]], na.rm = TRUE)) {
        id <- which(mydata[[z]] < min(limits))
        mydata[[z]][id] <- min(limits)
        labs[1] <- paste("<", labs[1])
      }
    }

    nlev2 <- length(breaks)

    if (missing(cols)) {
      cols <- "default"
    }
    col <- openColours(cols, (nlev2 - 1))
    breaks <- c(breaks[1:(length(breaks) - 1)], max(mydata[[z]], na.rm = TRUE))

    col.scale <- breaks

    legend <- list(
      col = col,
      at = col.scale,
      labels = list(labels = labs, at = at),
      space = key.position,
      auto.text = auto.text,
      footer = Args$key.footer,
      header = Args$key.header,
      height = 0.8,
      width = 1.5,
      fit = "scale",
      plot.style = c("ticks", "border")
    )

    legend <- makeOpenKeyLegend(key, legend, "windRose")

    levelplot.args <- list(
      x = myform,
      data = mydata,
      type = plotType,
      strip = strip,
      as.table = TRUE,
      region = TRUE,
      scales = scales,
      yscale.components = yscale.components.log10ticks,
      xscale.components = xscale.components.log10ticks,
      col.regions = col,
      at = col.scale,
      par.strip.text = list(cex = 0.8),
      colorkey = FALSE,
      legend = legend,
      panel = function(x, y, z, subscripts, ...) {
        panel.grid(h = -1, v = -1)
        panel.levelplot(x, y, z, subscripts, ...)

        if (mod.line) {
          panel.modline(log.x, log.y)
        }

        ## add base map
        if (map) {
          add.map(Args, ...)
        }

        ## add reference lines
        if (!is.null(ref.x)) {
          do.call(panel.abline, ref.x)
        }
        if (!is.null(ref.y)) do.call(panel.abline, ref.y)
      }
    )

    ## z must exist to get here
    Args$main <- if ("main" %in% names(Args)) {
      quickText(Args$main, auto.text)
    } else {
      quickText(paste(x, "vs.", y, "by levels of", z), auto.text)
    }

    if (!"pch" %in% names(Args)) {
      Args$pch <- 1
    }

    ## reset for Args
    levelplot.args <- listUpdate(levelplot.args, Args)

    ## plot
    plt <- do.call(levelplot, levelplot.args)
  }

  # trajectory plot ---------------------------------------------------------

  if (method %in% c("traj", "map")) {
    if (missing(x.inc)) {
      x.inc <- prettyGap(mydata[[x]])
    }
    if (missing(y.inc)) {
      y.inc <- prettyGap(mydata[[y]])
    }

    ## bin data
    mydata$ygrid <- round_any(mydata[[y]], y.inc)
    mydata$xgrid <- round_any(mydata[[x]], x.inc)

    rhs <- paste0("xgrid * ygrid |", type)
    myform <- formula(paste(z, "~", rhs))

    ## add vertices of each grid so that polygons can be drawn
    mydata <- transform(
      mydata,
      x1 = xgrid - x.inc / 2,
      x2 = xgrid - x.inc / 2,
      x3 = xgrid + x.inc / 2,
      x4 = xgrid + x.inc / 2,
      y1 = ygrid - y.inc / 2,
      y2 = ygrid + y.inc / 2,
      y3 = ygrid + y.inc / 2,
      y4 = ygrid - y.inc / 2
    )

    ## find coordinates in appropriate map projection
    coord1 <- mapproject(
      x = mydata$x1,
      y = mydata$y1,
      projection = Args$projection,
      parameters = Args$parameters,
      orientation = Args$orientation
    )

    coord2 <- mapproject(
      x = mydata$x2,
      y = mydata$y2,
      projection = Args$projection,
      parameters = Args$parameters,
      orientation = Args$orientation
    )

    coord3 <- mapproject(
      x = mydata$x3,
      y = mydata$y3,
      projection = Args$projection,
      parameters = Args$parameters,
      orientation = Args$orientation
    )

    coord4 <- mapproject(
      x = mydata$x4,
      y = mydata$y4,
      projection = Args$projection,
      parameters = Args$parameters,
      orientation = Args$orientation
    )

    coordGrid <- mapproject(
      x = mydata$xgrid,
      y = mydata$ygrid,
      projection = Args$projection,
      parameters = Args$parameters,
      orientation = Args$orientation
    )

    mydata <- transform(
      mydata,
      x1 = coord1$x,
      x2 = coord2$x,
      x3 = coord3$x,
      x4 = coord4$x,
      y1 = coord1$y,
      y2 = coord2$y,
      y3 = coord3$y,
      y4 = coord4$y,
      xgrid = coordGrid$x,
      ygrid = coordGrid$y
    )

    smooth.grid <- function(mydata, z) {
      myform <-
        formula(paste0(z, "^0.5 ~ s(xgrid, ygrid, k = ", k, ")", sep = ""))

      res <- 101
      Mgam <- mgcv::gam(myform, data = mydata)

      new.data <- expand.grid(
        xgrid = seq(
          min(mydata$xgrid),
          max(mydata$xgrid),
          length = res
        ),
        ygrid = seq(
          min(mydata$ygrid),
          max(mydata$ygrid),
          length = res
        )
      )

      pred <- predict.gam(Mgam, newdata = new.data)
      pred <- as.vector(pred)^2

      new.data[, z] <- pred

      ## exlcude too far
      ## exclude predictions too far from data (from mgcv)
      x <- seq(min(mydata$xgrid), max(mydata$xgrid), length = res)
      y <- seq(min(mydata$ygrid), max(mydata$ygrid), length = res)

      wsp <- rep(x, res)
      wdp <- rep(y, rep(res, res))

      ## data with gaps caused by min.bin
      all.data <- na.omit(data.frame(
        xgrid = mydata$xgrid,
        ygrid = mydata$ygrid,
        z
      ))

      ind <- with(
        all.data,
        exclude.too.far(
          wsp,
          wdp,
          mydata$xgrid,
          mydata$ygrid,
          dist = dist
        )
      )

      new.data[ind, z] <- NA

      new.data
    }

    if (smooth) {
      mydata <- mydata %>%
        group_by(across(type)) %>%
        do(smooth.grid(., z))
    }

    ## basic function for lattice call + defaults
    temp <- paste(type, collapse = "+")
    if (!smooth) {
      myform <- formula(paste(z, "~ x1 * y1 |", temp, sep = ""))
    }

    nlev <- 200

    ## handling of colour scale limits
    if (missing(limits)) {
      breaks <- pretty(mydata[[z]], n = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
    } else {
      ## handle user limits and clipping
      breaks <- pretty(limits, n = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]

      ## case where user max is < data max
      if (max(limits) < max(mydata[[z]], na.rm = TRUE)) {
        id <- which(mydata[[z]] > max(limits))
        mydata[[z]][id] <- max(limits)
        labs[length(labs)] <- paste(">", labs[length(labs)])
      }

      ## case where user min is > data min
      if (min(limits) > min(mydata[[z]], na.rm = TRUE)) {
        id <- which(mydata[[z]] < min(limits))
        mydata[[z]][id] <- min(limits)
        labs[1] <- paste("<", labs[1])
      }
    }

    nlev2 <- length(breaks)

    if (missing(cols)) {
      cols <- "default"
    }

    thecol <- openColours(cols, length(breaks) - 1)[cut(
      mydata[[z]],
      breaks,
      label = FALSE
    )]
    mydata$col <- thecol
    col <- thecol

    n <- length(breaks)
    col <- openColours(cols, n)

    col.scale <- breaks

    ## this is the default
    if (trajStat %in% c("cwt", "pscf", "mean", "frequency", "sqtba")) {
      legend <- list(
        col = col,
        at = breaks,
        labels = list(labels = labs),
        space = key.position,
        auto.text = auto.text,
        footer = Args$key.footer,
        header = Args$key.header,
        height = 1,
        width = 1.5,
        fit = "all"
      )
      legend <- makeOpenKeyLegend(key, legend, "other")
    }

    if (trajStat %in% c("frequency", "difference") && !smooth) {
      if (trajStat == "frequency") {
        breaks <- c(0, 1, 5, 10, 25, 100)
        labels <- c("0 to 1", "1 to 5", "5 to 10", "10 to 25", "25 to 100")
      }

      if (trajStat == "difference") {
        breaks <- c(-15000, -10, -5, -1, 1, 5, 10, 15000)
        labels <- c(
          "<-10",
          "-10 to -5",
          "-5 to -1",
          "-1 to 1",
          "1 to 5",
          "5 to 10",
          ">10"
        )
      }

      ## frequency plot
      n <- 7
      col <- openColours(cols, n)
      legend <- list(
        col = col,
        space = key.position,
        auto.text = auto.text,
        labels = labels,
        footer = Args$key.footer,
        header = Args$key.header,
        height = 0.8,
        width = 1.5,
        fit = "scale",
        plot.style = "other"
      )

      col.scale <- breaks

      thecol <- openColours(cols, length(breaks) - 1)[cut(
        mydata[[z]],
        breaks,
        label = FALSE
      )]
      mydata$col <- thecol
      col <- thecol

      legend <- makeOpenKeyLegend(key, legend, "windRose")
    }

    lv.args <- list(
      x = myform,
      data = mydata,
      type = plotType,
      strip = strip,
      as.table = TRUE,
      region = TRUE,
      scales = scales,
      col.regions = col,
      at = col.scale,
      yscale.components = yscale.components.log10ticks,
      xscale.components = xscale.components.log10ticks,
      par.strip.text = list(cex = 0.8),
      colorkey = FALSE,
      legend = legend,
      panel = function(x, y, z, subscripts, ...) {
        ## plot individual polygons
        if (!smooth) {
          sub <- mydata[subscripts, ] ## deal with one (type) at a time

          for (i in 1:nrow(sub)) {
            lpolygon(
              x = c(sub$x1[i], sub$x2[i], sub$x3[i], sub$x4[i]),
              y = c(sub$y1[i], sub$y2[i], sub$y3[i], sub$y4[i]),
              col = sub$col[i],
              border = Args$border
            )
          }
        } else {
          panel.levelplot(x, y, z, subscripts, labels = FALSE, ...)
        }

        ## add base map
        if (map) {
          add.map(Args, ...)
        }

        ## add mark for receptor location
        if (Args$origin) {
          lpoints(
            Args$receptor[["x"]],
            Args$receptor[["y"]],
            pch = 16,
            cex = 1.5,
            col = "black"
          )
        }

        ## add reference lines
        if (!is.null(ref.x)) {
          do.call(panel.abline, ref.x)
        }
        if (!is.null(ref.y)) do.call(panel.abline, ref.y)
      }
    )

    ## z must exist to get here
    Args$main <- if ("main" %in% names(Args)) {
      quickText(Args$main, auto.text)
    } else {
      quickText(paste(x, "vs.", y, "by levels of", z), auto.text)
    }

    if (!"pch" %in% names(Args)) {
      Args$pch <- 1
    }

    ## reset for Args
    lv.args <- listUpdate(lv.args, Args)

    ## plot
    plt <- do.call(levelplot, lv.args)
  }

  # kernel density ----------------------------------------------------------

  if (method == "density") {
    prepare.grid <- function(subdata) {
      n <- nrow(subdata) ## for intensity estimate
      x <- subdata[[x]]
      y <- subdata[[y]]
      xy <- grDevices::xy.coords(x, y, "xlab", "ylab")
      xlab <- xy$xlab
      ylab <- xy$ylab
      x <- cbind(xy$x, xy$y)[
        is.finite(xy$x) & is.finite(xy$y),
        ,
        drop = FALSE
      ]
      xlim <- range(x[, 1])
      ylim <- range(x[, 2])

      Map <- .smoothScatterCalcDensity(x, 256)
      xm <- Map$x1
      ym <- Map$x2

      dens <- Map$fhat

      grid <- expand.grid(x = xm, y = ym)

      results <- data.frame(x = grid$x, y = grid$y, z = as.vector(dens) * n)
      results
    }

    ## ###########################################################################

    results.grid <- mydata %>%
      group_by(across(type)) %>%
      do(prepare.grid(.))

    ## auto-scaling
    nlev <- nrow(mydata) ## preferred number of intervals
    breaks <- pretty(results.grid$z, n = nlev)

    nlev2 <- length(breaks)

    col <- openColours(method.col, (nlev2 - 1)) # was "default"??
    col <- c("transparent", col) ## add white at bottom
    col.scale <- breaks

    legend <- list(
      col = col,
      at = col.scale,
      space = key.position,
      auto.text = auto.text,
      footer = "intensity",
      header = Args$key.header,
      height = 1,
      width = 1.5,
      fit = "all"
    )

    legend <- makeOpenKeyLegend(TRUE, legend, "other")

    ## basic function for lattice call + defaults
    temp <- paste(type, collapse = "+")
    myform <- formula(paste("z ~ x * y", "|", temp, sep = ""))

    levelplot.args <- list(
      x = myform,
      data = results.grid,
      as.table = TRUE,
      scales = scales,
      strip = strip,
      yscale.components = yscale.components.log10ticks,
      xscale.components = xscale.components.log10ticks,
      strip.left = strip.left,
      par.strip.text = list(cex = 0.8),
      col.regions = col,
      legend = legend,
      region = TRUE,
      at = col.scale,
      colorkey = FALSE,
      ...,
      panel = function(x, y, z, subscripts, ...) {
        if (!Args$traj) {
          panel.grid(-1, -1)
        }
        panel.levelplot(
          x,
          y,
          z,
          subscripts,
          pretty = TRUE,
          labels = FALSE,
          ...
        )

        if (mod.line) {
          panel.modline(log.x, log.y)
        }

        ## base map
        if (map) {
          add.map(Args, ...)
        }

        ## add reference lines
        if (!is.null(ref.x)) {
          do.call(panel.abline, ref.x)
        }
        if (!is.null(ref.y)) do.call(panel.abline, ref.y)
      }
    )

    ## by default no title ever
    Args$main <- if ("main" %in% names(Args)) {
      quickText(Args$main, auto.text)
    } else {
      quickText("", auto.text)
    }

    if (!"pch" %in% names(Args)) {
      Args$pch <- 1
    }

    ## reset for Args
    levelplot.args <- listUpdate(levelplot.args, Args)

    ## plot
    plt <- do.call(levelplot, levelplot.args)
  }

  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    } else {
      plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    }
  }

  newdata <- mydata
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"

  invisible(output)
}


# add map function --------------------------------------------------------

add.map <- function(Args, ...) {
  if (Args$map.res == "default") {
    res <- "world"
  } else {
    res <- "worldHires"
  }

  ## USA specific maps
  if (Args$map.res == "state") {
    res <- "state"
  }

  if (Args$map.fill) {
    mp <-
      maps::map(
        database = res,
        plot = FALSE,
        fill = TRUE,
        projection = Args$projection,
        parameters = Args$parameters,
        orientation = Args$orientation
      )

    mp <- maps::map.wrap(mp)

    panel.polygon(
      mp$x,
      mp$y,
      col = Args$map.cols,
      border = Args$map.border,
      alpha = Args$map.alpha,
      lty = Args$map.lty,
      lwd = Args$map.lwd
    )
  } else {
    mp <- maps::map(
      database = res,
      plot = FALSE,
      projection = Args$projection,
      parameters = Args$parameters,
      orientation = Args$orientation
    )
    mp <- maps::map.wrap(mp)
    llines(
      mp$x,
      mp$y,
      col = Args$map.border,
      lty = Args$map.lty,
      lwd = Args$map.lwd
    )
  }

  map.grid2(
    lim = Args$trajLims,
    projection = Args$projection,
    parameters = Args$parameters,
    orientation = Args$orientation,
    col = Args$grid.col
  )
}


# Add FAC2 lines ----------------------------------------------------------

## takes account of log-scaling for x/y, x and y
panel.modline <- function(log.x = FALSE, log.y = FALSE) {
  x <- NULL ## silence R check

  if (!log.x && !log.y) {
    panel.curve(x * 1, lty = 1)
    panel.curve(x * 2, lty = 5)
    panel.curve(x / 2, lty = 5)
  }

  if (log.x && log.y) {
    panel.curve(x * 1, lty = 1)
    panel.curve(x + log10(2), lty = 5)
    panel.curve(x - log10(2), lty = 5)
  }

  if (!log.x && log.y) {
    panel.curve(log10(x), lty = 1)
    panel.curve(log10(x) + log10(2), lty = 5)
    panel.curve(log10(x) - log10(2), lty = 5)
  }

  if (log.x && !log.y) {
    panel.curve(10^(x), lty = 1)
    panel.curve(10^(x) / 2, lty = 5)
    panel.curve(10^(x) * 2, lty = 5)
  }
}


# add linear fit ----------------------------------------------------------

panel.linear <- function(
  x,
  y,
  x.nam = "x",
  y.nam = "y",
  se = TRUE,
  col = plot.line$col,
  col.se = "black",
  lty = 1,
  lwd = 1,
  alpha.se = 0.25,
  border = NA,
  group.number,
  myColors = myColors,
  group.value,
  type,
  col.line,
  col.symbol,
  fill,
  pch,
  cex,
  font,
  fontface,
  fontfamily
) {
  ## get rid of R check annoyances
  plot.line <- NULL

  n <- 100

  thedata <- data.frame(x = x, y = y)
  thedata <- na.omit(thedata)

  # make sure equation is shown
  if (length(myColors) == 1) {
    myColors <- "black"
  }

  tryCatch(
    {
      mod <- lm(y ~ x, data = thedata)

      lims <- current.panel.limits()

      xrange <- c(
        max(min(lims$x, na.rm = TRUE), min(x, na.rm = TRUE)),
        min(max(lims$x, na.rm = TRUE), max(x, na.rm = TRUE))
      )

      xseq <- seq(xrange[1], xrange[2], length = n)

      pred <- predict(mod, data.frame(x = xseq), interval = "confidence")

      if (se) {
        ## predicts 95% CI by default
        panel.polygon(
          x = c(xseq, rev(xseq)),
          y = c(pred[, 2], rev(pred[, 3])),
          col = col.se,
          alpha = alpha.se,
          border = border
        )
      }

      pred <- pred[, 1]

      panel.lines(xseq, pred, lty = 1, lwd = 1)

      x <- current.panel.limits()$xlim[1]

      y <- (1 - group.number / 20) * current.panel.limits()$ylim[2]

      r.sq <- summary(mod)$r.squared
      slope <- coef(mod)[2]
      intercept <- coef(mod)[1]

      if (intercept >= 0) {
        symb <- "+"
      } else {
        symb <- ""
      }
      panel.text(
        x,
        y,
        quickText(paste(
          y.nam,
          "=",
          sprintf(slope, fmt = "%#.2f"),
          "[",
          x.nam,
          "]",
          symb,
          sprintf(intercept, fmt = "%#.2f"),
          " R2=",
          sprintf(r.sq, fmt = "%#.2f"),
          sep = ""
        )),
        cex = 0.7,
        pos = 4,
        col = myColors[group.number]
      )
    },
    error = function(x) {
      return()
    }
  )
}


# trajectory handling -------------------------------------------

addTraj <- function(
  mydata,
  subscripts,
  Args,
  z,
  lty,
  myColors,
  group.number,
  lwd,
  groupMax,
  type
) {
  ## data of interest
  tmp <- split(
    mydata[subscripts, ],
    mydata[subscripts, "date"]
  )

  if (!is.na(z)) {
    ## colour by
    lapply(tmp, function(dat) {
      llines(
        dat$lon,
        dat$lat,
        col.line = dat$col,
        lwd = lwd,
        lty = lty
      )
    })

    ## major npoints from trajPlot, don't plot if NA
    if (!is.na(Args$npoints)) {
      id <- seq(
        min(subscripts),
        max(subscripts),
        by = Args$npoints
      )

      lapply(tmp, function(dat) {
        lpoints(
          dat[id, ][["lon"]],
          dat[id, ][["lat"]],
          col = dat$col,
          pch = 16
        )
      })
    }
  } else {
    ## colour by a z

    lapply(tmp, function(dat) {
      llines(
        dat$lon,
        dat$lat,
        col.line = myColors[group.number],
        lwd = lwd,
        lty = lty
      )
    })

    ## major npoints from trajPlot
    if (!is.na(Args$npoints)) {
      lapply(tmp, function(dat) {
        lpoints(
          dat[seq(1, nrow(dat), Args$npoints), ][["lon"]],
          dat[seq(1, nrow(dat), Args$npoints), ][["lat"]],
          col = myColors[group.number],
          pch = 16
        )
      })
    }

    ## add mark for receptor location
    if (Args$origin) {
      lpoints(
        Args$receptor[1],
        Args$receptor[2],
        pch = 16,
        cex = 1.5,
        col = "black"
      )
    }

    ## add cluster proportions if available

    if (
      all(!is.na(Args$clusters)) &&
        group.number == length(levels(mydata$MyGroupVar))
    ) {
      ## make sure we match clusters in case order mixed
      vars <- c(type, "MyGroupVar")

      pnts <- mydata %>%
        group_by(across(vars)) %>%
        dplyr::slice_head(n = 1)

      if (length(unique(pnts$lon)) == 1 & length(unique(pnts$lat)) == 1) {
        pnts <- mydata %>%
          group_by(across(vars)) %>%
          dplyr::slice_tail(n = 1)
      }

      pnts <- merge(
        pnts,
        Args$clusters,
        by.x = c(type, "MyGroupVar"),
        by.y = c(type, "cluster")
      )

      ## select tye correct type
      pnts <- pnts[pnts[[type]] == mydata[[type]][subscripts[1]], ]

      ## clusterProp is from trajCluster
      ltext(
        pnts$lon,
        pnts$lat,
        label = paste0(pnts$freq, "%"),
        pos = 3
      )
    }
  }
}
