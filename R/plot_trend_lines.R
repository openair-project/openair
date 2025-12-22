#' Plot air quality trends using a tradition line chart
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   The [plot_trend_lines()] function simply plots a traditional line chart of
#'   air pollution data. Some additional functionality is also included, such as
#'   self-contained time-averaging and the addition of 'windflow' wind arrows.
#'
#' @inheritParams plot_trend_smooth
#' @inheritParams timeAverage
#'
#' @param date_pad Should missing data be padded-out? This is useful where a
#'   data frame consists of two or more "chunks" of data with time gaps between
#'   them. By setting `date.pad = TRUE` the time gaps between the chunks are
#'   shown properly, rather than with a line connecting each chunk. For
#'   irregular data, set to `FALSE`. Note, this should not be set for `type`
#'   other than `default`.
#'
#' @inheritSection shared_ggplot_params Conditioning with `type`
#'
#' @export
#' @return an [openair][openair-package] object
#'
#' @author Jack Davison
#' @author David Carslaw
#'
#' @family ggplot2 time series and trend functions
#' @seealso the legacy [smoothTrend()] function
#'
#' @examples
#' # trend plot for nox
#' plot_trend_smooth(mydata, pollutant = "nox")
#'
#' # trend plot by each of 8 wind sectors
#' \dontrun{
#' plot_trend_smooth(mydata, pollutant = "o3", type = "wd")
#'
#' # several pollutants
#' plot_trend_smooth(mydata, pollutant = c("no2", "o3", "pm10", "pm25"))
#'
#' # percentiles
#' plot_trend_smooth(
#'   mydata,
#'   pollutant = "o3",
#'   statistic = "percentile",
#'   percentile = 95
#' )
#' }
plot_trend_lines <- function(
  data,
  pollutant,
  group = NULL,
  type = NULL,
  avg.time = "default",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  date_pad = FALSE,
  windflow = FALSE,
  cols = "tol",
  auto_text = TRUE,
  facet_opts = openair::facet_opts(),
  plot = TRUE,
  ...
) {
  type <- type %||% "default"

  if (rlang::is_logical(windflow)) {
    windflow <- windflow_opts(windflow = windflow)
  }

  # warning messages and other checks
  if (length(percentile) > 1 && length(pollutant) > 1) {
    cli::cli_abort(
      "Only one {.field pollutant} allowed when considering more than one {.field percentile}."
    )
  }

  # check & cut data
  data <- prepare_timeplot_data(
    mydata = data,
    pollutant = pollutant,
    type = c(type, group),
    avg.time = avg.time,
    date.pad = date_pad,
    windflow = windflow$windflow,
    ...
  )

  # time average & reshape data
  data <- time_average_timeplot_data(
    mydata = data,
    pollutant = pollutant,
    type = c(type, group),
    statistic = statistic,
    avg.time = avg.time,
    data.thresh = data.thresh,
    percentile = percentile,
    windflow = if (windflow$windflow) {
      TRUE
    } else {
      NULL
    }
  )
  pollutant <- data$pollutant
  plotdata <- data$data

  # get facet approach
  facet_fun <- get_facet_fun(type, facet_opts)

  # if group is null, we use the 'variable' column
  if (is.null(group)) {
    group <- "variable"
  }

  plotdata <-
    dplyr::mutate(
      dplyr::ungroup(plotdata),
      dplyr::across(
        dplyr::where(is.factor),
        \(x) factor(x, ordered = FALSE)
      )
    )

  # construct plot
  plt <-
    ggplot2::ggplot(
      plotdata,
      ggplot2::aes(x = .data$date, y = .data$value)
    ) +
    ggplot2::geom_line(ggplot2::aes(
      color = .data[[group]]
    )) +
    theme_oa_classic() +
    ggplot2::theme(
      palette.fill.discrete = c(
        openair::openColours(
          scheme = cols,
          n = dplyr::n_distinct(plotdata[[group]])
        ),
        "black"
      ),
      palette.colour.discrete = c(
        openair::openColours(
          scheme = cols,
          n = dplyr::n_distinct(plotdata[[group]])
        ),
        "black"
      )
    ) +
    ggplot2::labs(
      y = label_openair(
        paste(pollutant, collapse = ", "),
        auto_text = auto_text
      ),
      x = NULL,
      color = NULL
    ) +
    facet_fun +
    ggplot2::scale_color_discrete(
      label = label_openair
    )

  # remove legend if only one item
  if (dplyr::n_distinct(plotdata[[group]]) == 1) {
    plt <-
      plt +
      ggplot2::guides(
        fill = ggplot2::guide_none(),
        color = ggplot2::guide_none()
      )
  }

  # add windflow if requested
  if (windflow$windflow) {
    plt <- plt +
      layer_windflow(
        ggplot2::aes(
          ws = .data$ws,
          wd = .data$wd,
          color = .data[[group]]
        ),
        arrow = windflow$arrow,
        limits = windflow$limits,
        range = windflow$range
      )
  }

  # return
  if (plot) {
    return(plt)
  } else {
    return(plotdata)
  }
}


#' Plot time series, perhaps for multiple pollutants, grouped or in separate
#' panels.
#'
#' The [timePlot()] is the basic time series plotting function in `openair`. Its
#' purpose is to make it quick and easy to plot time series for pollutants and
#' other variables. The other purpose is to plot potentially many variables
#' together in as compact a way as possible.
#'
#' The function is flexible enough to plot more than one variable at once. If
#' more than one variable is chosen plots it can either show all variables on
#' the same plot (with different line types) *on the same scale*, or (if `group
#' = FALSE`) each variable in its own panels with its own scale.
#'
#' The general preference is not to plot two variables on the same graph with
#' two different y-scales. It can be misleading to do so and difficult with more
#' than two variables. If there is in interest in plotting several variables
#' together that have very different scales, then it can be useful to normalise
#' the data first, which can be down be setting the `normalise` option.
#'
#' The user has fine control over the choice of colours, line width and line
#' types used. This is useful for example, to emphasise a particular variable
#' with a specific line type/colour/width.
#'
#' [timePlot()] works very well with [selectByDate()], which is used for
#' selecting particular date ranges quickly and easily. See examples below.
#'
#' By default plots are shown with a colour key at the bottom and in the case of
#' multiple pollutants or sites, strips on the left of each plot. Sometimes this
#' may be overkill and the user can opt to remove the key and/or the strip by
#' setting `key` and/or `strip` to `FALSE`. One reason to do this is to maximise
#' the plotting area and therefore the information shown.
#'
#' @inheritParams timeAverage
#'
#' @param mydata A data frame of time series. Must include a `date` field and at
#'   least one variable to plot.
#' @param pollutant Name of variable to plot. Two or more pollutants can be
#'   plotted, in which case a form like `pollutant = c("nox", "co")` should be
#'   used.
#' @param group If more than one pollutant is chosen, should they all be plotted
#'   on the same graph together? The default is `FALSE`, which means they are
#'   plotted in separate panels with their own scaled. If `TRUE` then they are
#'   plotted on the same plot with the same scale.
#' @param stack If `TRUE` the time series will be stacked by year. This option
#'   can be useful if there are several years worth of data making it difficult
#'   to see much detail when plotted on a single plot.
#' @param normalise Should variables be normalised? The default is is not to
#'   normalise the data. `normalise` can take two values, either `"mean"` or a
#'   string representing a date in UK format e.g. "1/1/1998" (in the format
#'   dd/mm/YYYY). If `normalise = "mean"` then each time series is divided by
#'   its mean value.  If a date is chosen, then values at that date are set to
#'   100 and the rest of the data scaled accordingly. Choosing a date (say at
#'   the beginning of a time series) is very useful for showing how trends
#'   diverge over time. Setting `group = TRUE` is often useful too to show all
#'   time series together in one panel.
#' @param percentile The percentile level in percent used when `statistic =
#'   "percentile"` and when aggregating the data with `avg.time`. More than one
#'   percentile level is allowed for `type = "default"` e.g. `percentile = c(50,
#'   95)`. Not used if `avg.time = "default"`.
#' @param date.pad Should missing data be padded-out? This is useful where a
#'   data frame consists of two or more "chunks" of data with time gaps between
#'   them. By setting `date.pad = TRUE` the time gaps between the chunks are
#'   shown properly, rather than with a line connecting each chunk. For
#'   irregular data, set to `FALSE`. Note, this should not be set for `type`
#'   other than `default`.
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in [cutData()],
#'   e.g., `"season"`, `"year"`, `"weekday"` and so on. For example, `type =
#'   "season"` will produce four plots --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data frame.
#'   If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   `type` must be of length one.
#' @param cols Colours to be used for plotting; see [openColours()] for details.
#' @param plot.type The `lattice` plot type, which is a line (`plot.type = "l"`)
#'   by default. Another useful option is `plot.type = "h"`, which draws
#'   vertical lines.
#' @param key Should a key be drawn? The default is `TRUE`.
#' @param log Should the y-axis appear on a log scale? The default is `FALSE`.
#'   If `TRUE` a well-formatted log10 scale is used. This can be useful for
#'   plotting data for several different pollutants that exist on very different
#'   scales. It is therefore useful to use `log = TRUE` together with `group =
#'   TRUE`.
#' @param windflow This option allows a scatter plot to show the wind
#'   speed/direction as an arrow. The option is a list e.g. `windflow = list(col
#'   = "grey", lwd = 2, scale = 0.1)`. This option requires wind speed (`ws`)
#'   and wind direction (`wd`) to be available.
#'
#'   The maximum length of the arrow plotted is a fraction of the plot dimension
#'   with the longest arrow being `scale` of the plot x-y dimension. Note, if
#'   the plot size is adjusted manually by the user it should be re-plotted to
#'   ensure the correct wind angle. The list may contain other options to
#'   `panel.arrows` in the `lattice` package. Other useful options include
#'   `length`, which controls the length of the arrow head and `angle`, which
#'   controls the angle of the arrow head.
#'
#'   This option works best where there are not too many data to ensure
#'   over-plotting does not become a problem.
#' @param smooth Should a smooth line be applied to the data? The default is
#'   `FALSE`.
#' @param ci If a smooth fit line is applied, then `ci` determines whether the
#'   95 percent confidence intervals are shown.
#' @param x.relation,y.relation This determines how the x- or y-axis scale is
#'   plotted. `"same"` ensures all panels use the same scale and `"free"` will
#'   use panel-specific scales. The latter is a useful setting when plotting
#'   data with very different values.
#' @param ref.x See `ref.y` for details. In this case the correct date format
#'   should be used for a vertical line e.g. `ref.x = list(v =
#'   as.POSIXct("2000-06-15"), lty = 5)`.
#' @param ref.y A list with details of the horizontal lines to be added
#'   representing reference line(s). For example, `ref.y = list(h = 50, lty =
#'   5)` will add a dashed horizontal line at 50. Several lines can be plotted
#'   e.g. `ref.y = list(h = c(50, 100), lty = c(1, 5), col = c("green",
#'   "blue"))`. See `panel.abline` in the `lattice` package for more details on
#'   adding/controlling lines.
#' @param key.columns Number of columns to be used in the key. With many
#'   pollutants a single column can make to key too wide. The user can thus
#'   choose to use several columns by setting `columns` to be less than the
#'   number of pollutants.
#' @param key.position Location where the scale key is to plotted. Can include
#'   `"top"`, `"bottom"`, `"right"` and `"left"`.
#' @param name.pol This option can be used to give alternative names for the
#'   variables plotted. Instead of taking the column headings as names, the user
#'   can supply replacements. For example, if a column had the name "nox" and
#'   the user wanted a different description, then setting `name.pol = "nox
#'   before change"` can be used. If more than one pollutant is plotted then use
#'   `c` e.g. `name.pol = c("nox here", "o3 there")`.
#' @param date.breaks Number of major x-axis intervals to use. The function will
#'   try and choose a sensible number of dates/times as well as formatting the
#'   date/time appropriately to the range being considered. This does not always
#'   work as desired automatically. The user can therefore increase or decrease
#'   the number of intervals by adjusting the value of `date.breaks` up or down.
#' @param date.format This option controls the date format on the x-axis. While
#'   [timePlot()] generally sets the date format sensibly there can be some
#'   situations where the user wishes to have more control. For format types see
#'   [strptime()]. For example, to format the date like "Jan-2012" set
#'   `date.format = "\%b-\%Y"`.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly, e.g., by subscripting the '2' in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#' @param ... Other graphical parameters are passed onto [cutData()] and
#'   [lattice::xyplot()]. For example, [timePlot()] passes the option `hemisphere
#'   = "southern"` on to [cutData()] to provide southern (rather than default
#'   northern) hemisphere handling of `type = "season"`. Similarly, most common
#'   plotting parameters, such as `layout` for panel arrangement and `pch` and
#'   `cex` for plot symbol type and size and `lty` and `lwd` for line type and
#'   width, as passed to [lattice::xyplot()], although some maybe locally
#'   managed by `openair` on route, e.g., axis and title labelling options (such
#'   as `xlab`, `ylab`, `main`) are passed via [quickText()] to handle routine
#'   formatting. See examples below.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family time series and trend functions
#' @seealso the newer [plot_trend_lines()] function
#' @examples
#'
#' # basic use, single pollutant
#' timePlot(mydata, pollutant = "nox")
#'
#' # two pollutants in separate panels
#' \dontrun{
#' timePlot(mydata, pollutant = c("nox", "no2"))
#'
#' # two pollutants in the same panel with the same scale
#' timePlot(mydata, pollutant = c("nox", "no2"), group = TRUE)
#'
#' # alternative by normalising concentrations and plotting on the same scale
#' timePlot(mydata,
#'   pollutant = c("nox", "co", "pm10", "so2"), group = TRUE, avg.time =
#'     "year", normalise = "1/1/1998", lwd = 3, lty = 1
#' )
#'
#' # examples of selecting by date
#'
#' # plot for nox in 1999
#' timePlot(selectByDate(mydata, year = 1999), pollutant = "nox")
#'
#' # select specific date range for two pollutants
#' timePlot(selectByDate(mydata, start = "6/8/2003", end = "13/8/2003"),
#'   pollutant = c("no2", "o3")
#' )
#'
#' # choose different line styles etc
#' timePlot(mydata, pollutant = c("nox", "no2"), lty = 1)
#'
#' # choose different line styles etc
#' timePlot(selectByDate(mydata, year = 2004, month = 6),
#'   pollutant =
#'     c("nox", "no2"), lwd = c(1, 2), col = "black"
#' )
#'
#' # different averaging times
#'
#' # daily mean O3
#' timePlot(mydata, pollutant = "o3", avg.time = "day")
#'
#' # daily mean O3 ensuring each day has data capture of at least 75%
#' timePlot(mydata, pollutant = "o3", avg.time = "day", data.thresh = 75)
#'
#' # 2-week average of O3 concentrations
#' #' timePlot(mydata, pollutant = "o3", avg.time = "2 week")
#' }
#'
timePlot <- function(
  mydata,
  pollutant = "nox",
  group = FALSE,
  stack = FALSE,
  normalise = NULL,
  avg.time = "default",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  date.pad = FALSE,
  type = "default",
  cols = "brewer1",
  plot.type = "l",
  log = FALSE,
  windflow = NULL,
  smooth = FALSE,
  ci = TRUE,
  x.relation = "same",
  y.relation = "same",
  ref.x = NULL,
  ref.y = NULL,
  key = TRUE,
  key.columns = 1,
  key.position = "bottom",
  name.pol = pollutant,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  ## ---- Setup & Validation ----

  # Args setup
  Args <- list(...)

  # warning messages and other checks
  if (length(percentile) > 1 && length(pollutant) > 1) {
    cli::cli_abort(
      "Only one {.field pollutant} allowed when considering more than one {.field percentile}."
    )
  }

  if (!missing(statistic) && missing(avg.time)) {
    cli::cli_inform("No {.field avg.time} specified; using 'month'.")
    avg.time <- "month"
  }

  rlang::arg_match(x.relation, c("same", "free"))
  rlang::arg_match(y.relation, c("same", "free"))

  ## ---- Graphics & Styling ----

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  # set & reset graphics
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(
    fontsize = current.font
  ))
  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  # style controls
  Args$pch <- Args$pch %||% NA
  Args$lwd <- Args$lwd %||% 1
  Args$lty <- Args$lty %||% NULL
  Args$layout <- Args$layout %||% NULL

  # global variables
  xlim <- Args$xlim %||% NULL
  strip <- Args$strip %||% TRUE

  ## ---- Data processing ----

  # check & cut data
  mydata <- prepare_timeplot_data(
    mydata = mydata,
    pollutant = pollutant,
    type = type,
    avg.time = avg.time,
    date.pad = date.pad,
    windflow = windflow,
    ...
  )

  # time average & reshape data
  mydata <- time_average_timeplot_data(
    mydata = mydata,
    pollutant = pollutant,
    type = type,
    statistic = statistic,
    avg.time = avg.time,
    data.thresh = data.thresh,
    percentile = percentile,
    windflow = windflow
  )
  pollutant <- mydata$pollutant
  mydata <- mydata$data

  # normalise data (if required)
  mydata <- normalise_timeplot_data(mydata, normalise = normalise)

  ## ---- Groups & Layout Logic ----

  # need to group pollutants if conditioning
  if (avg.time != "default" && length(percentile) > 1L && missing(group)) {
    group <- TRUE
  }
  if (type != "default") {
    group <- TRUE
  }

  # number of pollutants (or sites for type = "site")
  npol <- length(unique(mydata$variable)) # number of pollutants

  # layout - stack vertically
  if (is.null(Args$layout) && !group && !stack) {
    Args$layout <- c(1, npol)
  }

  if (missing(key.columns)) {
    key.columns <- npol
  }

  ## ---- Layout & Colours ----

  # label controls
  Args$xlab <- quickText(Args$xlab %||% "", auto.text)
  Args$main <- quickText(Args$main %||% NULL, auto.text)
  Args$ylab <- quickText(
    Args$ylab %||%
      dplyr::case_when(
        !is.null(normalise) ~
          paste("normalised", paste(pollutant, collapse = ", ")),
        .default = paste(pollutant, collapse = ", ")
      ),
    auto.text
  )

  # get pollutant labels
  if (!missing(name.pol)) {
    mylab <- sapply(
      seq_along(name.pol),
      function(x) quickText(name.pol[x], auto.text)
    )
  } else {
    mylab <- sapply(
      seq_along(pollutant),
      function(x) quickText(pollutant[x], auto.text)
    )
  }

  # set up colours
  myColors <- if (length(cols) == 1 && cols == "greyscale") {
    openColours(cols, npol + 1)[-1]
  } else {
    openColours(cols, npol)
  }

  ## ---- Date & Scale Config ----

  # For date scale
  if (x.relation == "free") {
    dates <-
      purrr::map(
        split(mydata, mydata[type]),
        function(x) dateBreaks(x$date, date.breaks)$major
      ) |>
      purrr::list_c() |>
      unique()
  } else {
    dates <- dateBreaks(mydata$date, date.breaks)$major
  }

  # Date Axis Formatting
  if (is.null(date.format)) {
    if (x.relation == "free") {
      formats <- dateBreaks(
        split(mydata, mydata[type])[[1]]$date,
        date.breaks
      )$format
    } else {
      formats <- dateBreaks(mydata$date, date.breaks)$format
    }
  } else {
    formats <- date.format
  }

  # Handle log scaling
  nlog <- FALSE
  if (log) {
    nlog <- 10
  }

  # Default scales
  scales <- list(
    x = list(relation = x.relation, at = dates, format = formats),
    y = list(relation = y.relation, log = nlog, rot = 0)
  )

  ## ---- Plot Structure & Formula ----

  # basic function for lattice call + defaults
  myform <- formula(paste("value ~ date |", type))
  theStrip <- strip

  if (is.null(Args$strip)) {
    strip <- TRUE
  }

  strip.left <- FALSE

  # layout changes depending on plot type
  if (!group) {
    # separate panels per pollutant
    if (is.null(Args$strip)) {
      strip <- FALSE
    }

    myform <- formula("value ~ date | variable")

    if (npol == 1) {
      strip.left <- FALSE
    } else {
      strip.left <- strip.custom(
        par.strip.text = list(cex = 0.9),
        horizontal = FALSE,
        factor.levels = mylab
      )
    }

    scales <- list(
      x = list(
        relation = x.relation,
        at = dates,
        format = formats
      ),
      y = list(
        relation = y.relation,
        rot = 0,
        log = nlog
      )
    )

    if (is.null(Args$lty)) Args$lty <- 1 # don't need different line types here
  }

  # set lty if not set by this point
  if (is.null(Args$lty)) {
    Args$lty <- seq_along(pollutant)
  }

  if (type == "default") {
    strip <- FALSE
  }

  ## ---- Special Layouts ----

  # if stacking of plots by year is needed
  if (stack) {
    mydata$year <- as.character(year(mydata$date))
    if (is.null(Args$layout)) {
      Args$layout <- c(1, length(unique(mydata$year)))
    }
    strip <- FALSE
    myform <- formula("value ~ date | year")
    strip.left <- strip.custom(
      par.strip.text = list(cex = 0.9),
      horizontal = FALSE
    )
    #  dates <- unique(dateTrunc(mydata$date, "months")) - this does not work?
    dates <- as.POSIXct(
      unique(paste(format(mydata$date, "%Y-%m"), "-01", sep = "")),
      "GMT"
    )

    scales <- list(
      x = list(
        format = "%d-%b",
        relation = "sliced"
      ),
      y = list(log = nlog)
    )

    xlim <- lapply(split(mydata, mydata["year"]), function(x) range(x$date))
  }

  # special layout if type = "wd"
  if (length(type) == 1 && type[1] == "wd" && is.null(Args$layout)) {
    # re-order to make sensible layout
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    mydata$wd <- ordered(mydata$wd, levels = wds)

    # see if wd is actually there or not
    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(mydata$wd)) FALSE else TRUE
    })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])

    mydata$wd <- factor(mydata$wd) # remove empty factor levels

    Args$layout <- c(3, 3)
    if (!"skip" %in% names(Args)) {
      Args$skip <- skip
    }
  }
  if (!"skip" %in% names(Args)) {
    Args$skip <- FALSE
  }

  ## ---- Key & Strip Config ----

  if (key) {
    # type of key depends on whether points are plotted or not
    if (any(!is.na(Args$pch))) {
      key <- list(
        lines = list(
          col = myColors[1:npol],
          lty = Args$lty,
          lwd = Args$lwd
        ),
        points = list(
          pch = Args$pch,
          col = myColors[1:npol]
        ),
        text = list(lab = mylab),
        space = key.position,
        columns = key.columns
      )
    } else {
      key <- list(
        lines = list(
          col = myColors[1:npol],
          lty = Args$lty,
          lwd = Args$lwd
        ),
        text = list(lab = mylab),
        space = key.position,
        columns = key.columns
      )
    }
  } else {
    key <- NULL # either there is a key or there is not
  }

  if (theStrip) {
    strip <- strip
    strip.left <- strip.left
  } else {
    strip <- FALSE
    strip.left <- FALSE
  }

  ## ---- Final Data Prep ----

  # allow reasonable gaps at ends, default has too much padding
  if (is.null(xlim)) {
    get_xrange <- function(x) {
      gap <- difftime(max(x$date), min(x$date), units = "secs") / 80
      range(x$date[!is.na(x$value)]) + c(-1 * gap, gap)
    }

    if (x.relation == "same") {
      xlim <- get_xrange(mydata)
    } else {
      splitvar <- type
      if (type == "default" && length(pollutant) > 1L && !group) {
        splitvar <- "variable"
      }
      xlim <- lapply(split(mydata, mydata[splitvar]), get_xrange)
      if (type == "default") {
        xlim <- xlim[[1]]
      }
    }
  }

  # make sure order is correct
  mydata$variable <- factor(mydata$variable, levels = pollutant)

  ## ---- Create Plot ----

  # the plot
  xyplot.args <- list(
    x = myform,
    data = mydata,
    groups = mydata$variable,
    as.table = TRUE,
    par.strip.text = list(cex = 0.8),
    scales = scales,
    key = key,
    xlim = xlim,
    strip = strip,
    strip.left = strip.left,
    windflow = windflow,
    yscale.components = yscale.components.log10ticks,
    panel = panel.superpose,
    panel.groups = function(
      x,
      y,
      col.line,
      col.symbol,
      col,
      col.se,
      type,
      group.number,
      lty,
      lwd,
      pch,
      subscripts,
      windflow,
      ...
    ) {
      if (group.number == 1) {
        panel.grid(-1, 0)
        panel.abline(v = dates, col = "grey90")
      }
      if (!group & !stack) {
        panel.abline(v = dates, col = "grey90")
        panel.grid(-1, 0)
      }

      panel.xyplot(
        x,
        y,
        type = plot.type,
        lty = lty,
        lwd = lwd,
        pch = pch,
        col.line = myColors[group.number],
        col.symbol = myColors[group.number],
        ...
      )
      # deal with points separately - useful if missing data where line
      # does not join consequtive points
      if (any(!is.na(Args$pch))) {
        lpoints(
          x,
          y,
          type = "p",
          pch = Args$pch[group.number],
          col.symbol = myColors[group.number],
          ...
        )
      }

      if (!is.null(windflow)) {
        list1 <- list(x, y, dat = mydata, subscripts)
        list2 <- windflow
        flow.args <- listUpdate(list1, list2)
        do.call(panel.windflow, flow.args)
      }

      if (smooth) {
        panel.gam(
          x,
          y,
          col = myColors[group.number],
          col.se = myColors[group.number],
          lty = 1,
          lwd = 1,
          se = ci,
          k = NULL,
          ...
        )
      }

      # add reference lines

      if (!is.null(ref.x)) {
        do.call(panel.abline, ref.x)
      }
      if (!is.null(ref.y)) do.call(panel.abline, ref.y)
    }
  )

  # reset for Args
  xyplot.args <- listUpdate(xyplot.args, Args)

  # plot
  plt <- do.call(xyplot, xyplot.args)

  # output
  if (plot) {
    plot(plt)
  }
  newdata <- mydata
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"

  invisible(output)
}

#' Prepare data for the timeplot function
#' @noRd
prepare_timeplot_data <- function(
  mydata,
  pollutant,
  type,
  avg.time,
  date.pad,
  windflow,
  ...
) {
  # determine necessary variables
  vars <- c("date", pollutant)
  if (!is.null(windflow)) {
    vars <- unique(c(vars, "wd", "ws"))
  }

  # standard data checks
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # pad out any missing date/times so that line don't extend between areas of missing data
  if (date.pad) {
    mydata <- date.pad(mydata, type = type)
  }

  # cut data
  mydata <- cutData(mydata, type, ...)

  # check for duplicates - can't really have duplicate data in a timeplot
  if (avg.time == "default") {
    checkDuplicateRows(mydata, type, fn = cli::cli_abort)
  }

  # return data
  return(mydata)
}

#' timeAverage and reshape timeplot data
#' @noRd
time_average_timeplot_data <- function(
  mydata,
  pollutant,
  type,
  statistic,
  avg.time,
  data.thresh,
  percentile,
  windflow
) {
  # average the data if necessary (default does nothing)
  if (avg.time != "default") {
    # deal with multiple percentile values

    if (length(percentile) > 1) {
      prefix <- paste(pollutant, "percentile ")

      mydata <-
        calcPercentile(
          mydata,
          type = type,
          pollutant = pollutant,
          avg.time = avg.time,
          data.thresh = data.thresh,
          percentile = percentile,
          prefix = prefix
        )

      pollutant <- paste0(prefix, percentile)
    } else {
      mydata <- timeAverage(
        mydata,
        pollutant = pollutant,
        type = type,
        statistic = statistic,
        avg.time = avg.time,
        data.thresh = data.thresh,
        percentile = percentile
      )
    }
  }

  # timeAverage drops type if default
  if (any(type == "default")) {
    mydata$default <- "default"
  }

  # need to flag if ws/wd are being plotted *and* used for windflow
  flag_wind_pollutant <- !is.null(windflow) &&
    ("ws" %in% pollutant || "wd" %in% pollutant)

  # retain ws/wd if needed later
  if (flag_wind_pollutant) {
    # only select what is being pivoted, as it will need joining back on
    met_vars <- c("ws", "wd")
    met_vars <- met_vars[met_vars %in% pollutant]
    met_data <- dplyr::select(mydata, dplyr::any_of(c("date", met_vars)))
  }

  # reshape
  mydata <-
    tidyr::pivot_longer(
      mydata,
      cols = dplyr::all_of(pollutant),
      names_to = "variable",
      values_to = "value"
    )

  # bind on ws/wd if needed for windflow
  if (flag_wind_pollutant) {
    mydata <- dplyr::left_join(
      mydata,
      met_data,
      by = dplyr::join_by("date")
    ) |>
      dplyr::relocate(dplyr::any_of(c("ws", "wd")), .after = "date")
  }

  # need to return pollutant as it can change
  return(list(
    data = mydata,
    pollutant = pollutant
  ))
}

#' Apply normalisation to timeplot data
#' @noRd
normalise_timeplot_data <-
  function(mydata, normalise) {
    # preserve order of pollutants
    mydata <- dplyr::mutate(
      mydata,
      variable = factor(.data$variable, levels = unique(.data$variable))
    )

    # if normalise isn't given, just return the data
    if (is.null(normalise)) {
      return(mydata)
    }

    # handle normalisation
    if (normalise == "mean") {
      mydata <-
        dplyr::mutate(
          mydata,
          value = .data$value / mean(.data$value, na.rm = TRUE),
          .by = "variable"
        )
    } else {
      # convert string to date
      target_date <- as.POSIXct(strptime(
        normalise,
        format = "%d/%m/%Y",
        tz = "GMT"
      ))

      if (is.na(target_date)) {
        cli::cli_abort(c(
          "x" = "Provided {.field normalise} option not recognised.",
          "i" = "{.field normalise} must be either 'mean' or a string in the format 'DD/MM/YYYY'."
        ))
      }

      # find nearest values to each date
      target_date_values <-
        tidyr::drop_na(mydata) |>
        slice_min(
          abs(.data$date - target_date),
          n = 1L,
          with_ties = FALSE,
          by = "variable"
        ) |>
        dplyr::select("variable", "target_value" = "value")

      # scale value to 100 at specific date
      mydata <-
        mydata |>
        dplyr::left_join(
          target_date_values,
          by = dplyr::join_by(variable)
        ) |>
        dplyr::mutate(
          value = 100 * (.data$value / .data$target_value)
        ) |>
        dplyr::select(-"target_value")
    }

    # return input data
    return(mydata)
  }

# function to plot wind flow arrows
panel.windflow <- function(
  x,
  y,
  dat,
  subscripts,
  scale = 0.2,
  ws = "ws",
  wd = "wd",
  col = "black",
  lwd = 1,
  length = 0.1,
  angle = 20,
  ...
) {
  max.ws <- max(dat[[ws]], na.rm = TRUE)

  delta.x <- scale * diff(current.panel.limits()$xlim)
  delta.y <- scale * diff(current.panel.limits()$ylim)

  # actual shape of the plot window
  delta.x.cm <- diff(current.panel.limits(unit = "cm")$xlim)
  delta.y.cm <- diff(current.panel.limits(unit = "cm")$ylim)

  # physical size of plot windows, correct so wd is right when plotted.
  # need to replot if window re-scaled by user
  if (delta.x.cm > delta.y.cm) {
    delta.y <- delta.y * delta.x.cm / delta.y.cm
  } else {
    delta.x <- delta.x * delta.y.cm / delta.x.cm
  }

  x0 <- delta.x *
    dat[[ws]][subscripts] *
    sin(2 * pi * dat[[wd]][subscripts] / 360) /
    max.ws

  y0 <- delta.y *
    dat[[ws]][subscripts] *
    cos(2 * pi * dat[[wd]][subscripts] / 360) /
    max.ws

  panel.arrows(
    x0 = x - x0 / 2,
    y0 = y - y0 / 2,
    x1 = x + x0 / 2,
    y1 = y + y0 / 2,
    length = length,
    angle = angle,
    code = 1,
    col = col,
    lwd = lwd
  )
}
