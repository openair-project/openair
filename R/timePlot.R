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
#' @inheritParams timeAverage
#'
#' @param mydata A data frame of time series. Must include a `date` field and at
#'   least one variable to plot.
#'
#' @param pollutant Name of variable to plot. Two or more pollutants can be
#'   plotted, in which case a form like `pollutant = c("nox", "co")` should be
#'   used.
#'
#' @param group Controls how multiple lines/series are grouped. Three options
#'   are available:
#'
#'   - `FALSE` (default): each pollutant is plotted in its own panel with its
#'     own scale.
#'   - `TRUE`: all pollutants are plotted together on the same panel and scale,
#'     coloured by pollutant name.
#'   - A character string giving the name of a column in `mydata` (e.g.
#'     `group = "site"` or `group = "pollutant"`): lines are coloured by the
#'     values in that column. With a single `pollutant` all groups appear in
#'     one panel; with multiple `pollutant`s each pollutant gets its own panel
#'     and lines within each panel are coloured by the group column. This is
#'     particularly useful for long-format data where multiple species are
#'     stored in one column.
#'
#' @param stack If `TRUE` the time series will be stacked by year. This option
#'   can be useful if there are several years worth of data making it difficult
#'   to see much detail when plotted on a single plot.
#'
#' @param normalise Should variables be normalised? The default is is not to
#'   normalise the data. `normalise` can take two values, either `"mean"` or a
#'   string representing a date in UK format e.g. "1/1/1998" (in the format
#'   dd/mm/YYYY). If `normalise = "mean"` then each time series is divided by
#'   its mean value.  If a date is chosen, then values at that date are set to
#'   100 and the rest of the data scaled accordingly. Choosing a date (say at
#'   the beginning of a time series) is very useful for showing how trends
#'   diverge over time. Setting `group = TRUE` is often useful too to show all
#'   time series together in one panel.
#'
#' @param percentile The percentile level in percent used when `statistic =
#'   "percentile"` and when aggregating the data with `avg.time`. More than one
#'   percentile level is allowed for `type = "default"` e.g. `percentile = c(50,
#'   95)`. Not used if `avg.time = "default"`.
#'
#' @param date.pad Should missing data be padded-out? This is useful where a
#'   data frame consists of two or more "chunks" of data with time gaps between
#'   them. By setting `date.pad = TRUE` the time gaps between the chunks are
#'   shown properly, rather than with a line connecting each chunk. For
#'   irregular data, set to `FALSE`. Note, this should not be set for `type`
#'   other than `default`.
#'
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
#'
#' @param cols Colours to be used for plotting; see [openColours()] for details.
#'
#' @param key Should a key be drawn? The default is `TRUE`.
#'
#' @param log Should the y-axis appear on a log scale? The default is `FALSE`.
#'   If `TRUE` a well-formatted log10 scale is used. This can be useful for
#'   plotting data for several different pollutants that exist on very different
#'   scales. It is therefore useful to use `log = TRUE` together with `group =
#'   TRUE`.
#'
#' @param windflow If `TRUE`, the vector-averaged wind speed and direction will
#'   be plotted using arrows. Alternatively, can be a list of arguments to
#'   control the appearance of the arrows (colour, linewidth, alpha value,
#'   etc.). See [windflowOpts()] for details.
#'
#' @param smooth Should a smooth line be applied to the data? The default is
#'   `FALSE`.
#'
#' @param smooth_k An integer controlling the number of basis functions used in
#'   the GAM smooth. In a GAM, `k` sets the maximum degrees of freedom for the
#'   smooth term: larger values allow more flexibility and can capture finer
#'   structure in the data, while smaller values produce smoother, less wiggly
#'   fits. The default (`NULL`) lets `ggplot2` choose automatically (typically
#'   `k = 10`). Increase `k` if the smooth appears too rigid; decrease it to
#'   avoid over-fitting.
#'
#' @param ci If a smooth fit line is applied, then `ci` determines whether the
#'   95 percent confidence intervals are shown.
#'
#' @param x.relation,y.relation This determines how the x- or y-axis scale is
#'   plotted. `"same"` ensures all panels use the same scale and `"free"` will
#'   use panel-specific scales. The latter is a useful setting when plotting
#'   data with very different values.
#'
#' @param ref.x See `ref.y` for details. In this case the correct date format
#'   should be used for a vertical line e.g. `ref.x = list(v =
#'   as.POSIXct("2000-06-15"), lty = 5)`.
#'
#' @param ref.y A list with details of the horizontal lines to be added
#'   representing reference line(s). For example, `ref.y = list(h = 50, lty =
#'   5)` will add a dashed horizontal line at 50. Several lines can be plotted
#'   e.g. `ref.y = list(h = c(50, 100), lty = c(1, 5), col = c("green",
#'   "blue"))`.
#'
#' @param key.columns Number of columns to be used in the key. With many
#'   pollutants a single column can make to key too wide. The user can thus
#'   choose to use several columns by setting `columns` to be less than the
#'   number of pollutants.
#'
#' @param key.position Location where the scale key is to plotted. Can include
#'   `"top"`, `"bottom"`, `"right"` and `"left"`.
#'
#' @param strip.position Location where the facet 'strips' are located when
#'   using `type`. When one `type` is provided, can be one of `"left"`,
#'   `"right"`, `"bottom"` or `"top"`. When two `type`s are provided, this
#'   argument defines whether the strips are "switched" and can take either
#'   `"x"`, `"y"`, or `"both"`. For example, `"x"` will switch the 'top' strip
#'   locations to the bottom of the plot.
#'
#' @param name.pol This option can be used to give alternative names for the
#'   variables plotted. Instead of taking the column headings as names, the user
#'   can supply replacements. For example, if a column had the name "nox" and
#'   the user wanted a different description, then setting `name.pol = "nox
#'   before change"` can be used. If more than one pollutant is plotted then use
#'   `c` e.g. `name.pol = c("nox here", "o3 there")`.
#'
#' @param date.breaks Number of major x-axis intervals to use. The function will
#'   try and choose a sensible number of dates/times as well as formatting the
#'   date/time appropriately to the range being considered. This does not always
#'   work as desired automatically. The user can therefore increase or decrease
#'   the number of intervals by adjusting the value of `date.breaks` up or down.
#'
#' @param date.format This option controls the date format on the x-axis. While
#'   [timePlot()] generally sets the date format sensibly there can be some
#'   situations where the user wishes to have more control. For format types see
#'   [strptime()]. For example, to format the date like "Jan-2012" set
#'   `date.format = "\%b-\%Y"`.
#'
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly, e.g., by subscripting the '2' in NO2.
#'
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#'
#' @param ... Other graphical parameters are passed onto [cutData()] and other
#'   functions. For example, [timePlot()] passes the option `hemisphere =
#'   "southern"` on to [cutData()] to provide southern (rather than default
#'   northern) hemisphere handling of `type = "season"`. Similarly, most common
#'   plotting parameters, such as `layout` for panel arrangement and `pch` and
#'   `cex` for plot symbol type and size and `lty` and `lwd` for line type and
#'   width, although some maybe locally managed by `openair` on route, e.g.,
#'   axis and title labelling options (such as `xlab`, `ylab`, `main`) are
#'   passed via [quickText()] to handle routine formatting. See examples below.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @author Jack Davison
#' @family time series and trend functions
#' @examples
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
#' # group by a column (e.g. long-format data with a 'site' column)
#' d <- rbind(
#'   cbind(mydata[, c("date", "nox")], site = "London"),
#'   cbind(transform(mydata[, c("date", "nox")], nox = nox * 1.5), site = "Manchester")
#' )
#' timePlot(d, pollutant = "nox", group = "site")
#'
#' # alternative by normalising concentrations and plotting on the same scale
#' timePlot(
#'   mydata,
#'   pollutant = c("nox", "co", "pm10", "so2"),
#'   group = TRUE,
#'   avg.time = "year",
#'   normalise = "1/1/1998",
#'   lwd = 3,
#'   lty = 1
#' )
#'
#' # examples of selecting by date
#'
#' # plot for nox in 1999
#' timePlot(selectByDate(mydata, year = 1999), pollutant = "nox")
#'
#' # select specific date range for two pollutants
#' timePlot(
#'   selectByDate(mydata, start = "6/8/2003", end = "13/8/2003"),
#'   pollutant = c("no2", "o3")
#' )
#'
#' # choose different line styles etc
#' timePlot(mydata, pollutant = c("nox", "no2"), lty = 1)
#'
#' # choose different line styles etc
#' timePlot(
#'   selectByDate(mydata, year = 2004, month = 6),
#'   pollutant = c("nox", "no2"),
#'   lwd = c(1, 2),
#'   col = "black"
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
#' timePlot(mydata, pollutant = "o3", avg.time = "2 week")
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
  log = FALSE,
  windflow = NULL,
  smooth = FALSE,
  smooth_k = NULL,
  ci = TRUE,
  x.relation = "same",
  y.relation = "same",
  ref.x = NULL,
  ref.y = NULL,
  key = TRUE,
  key.columns = NULL,
  key.position = "bottom",
  strip.position = "top",
  name.pol = pollutant,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  if (rlang::is_logical(key) && !key) {
    key.position <- "none"
  }

  # Args setup
  extra.args <- rlang::list2(...)

  # detect if group is a column name rather than a boolean
  group_is_col <- is.character(group) && length(group) == 1

  # warning messages and other checks
  if (length(percentile) > 1 && length(pollutant) > 1) {
    cli::cli_abort(
      "Only one {.arg pollutant} allowed when considering more than one {.arg percentile}."
    )
  }

  if (stack && length(type) > 1) {
    cli::cli_abort(
      "Cannot {.arg stack} and have more than one {.arg type}."
    )
  }

  if (
    isFALSE(group) &&
      length(type) > 1 &&
      (length(pollutant) > 1 || length(percentile) > 1)
  ) {
    cli::cli_abort(
      "{.arg group} cannot be {FALSE} and have more than one {.arg type}."
    )
  }

  if (length(type) > 2) {
    cli::cli_abort(
      "Cannot have more than 2 {.arg type}s."
    )
  }

  if (!missing(statistic) && missing(avg.time)) {
    cli::cli_inform("No {.field avg.time} specified; using 'month'.")
    avg.time <- "month"
  }

  rlang::arg_match(x.relation, c("same", "free"))
  rlang::arg_match(y.relation, c("same", "free"))

  if (group_is_col) {
    if (!group %in% names(mydata) && !group %in% dateTypes) {
      cli::cli_abort("Column {.val {group}} not found in {.arg mydata}.")
    }
    if (group %in% type) {
      cli::cli_abort(
        "{.arg group} column {.val {group}} is also used in {.arg type}. Choose one or the other."
      )
    }
    if (length(type) > 1 && length(pollutant) > 1) {
      cli::cli_abort(
        "Cannot combine multiple {.arg pollutant}s, multiple {.arg type}s, and a {.arg group} column simultaneously."
      )
    }
  }

  # ensure windflow is a list
  windflow <- resolve_windflow_opts(windflow)

  # style controls
  extra.args$pch <- extra.args$pch %||% NA
  extra.args$layout <- extra.args$layout %||% NULL

  # check & cut data
  mydata <- prepare_timeplot_data(
    mydata = mydata,
    pollutant = pollutant,
    type = type,
    avg.time = avg.time,
    date.pad = date.pad,
    windflow = windflow,
    group = group,
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
    windflow = windflow,
    group = group,
    ...
  )
  pollutant <- mydata$pollutant
  mydata <- mydata$data

  # normalise data (if required)
  mydata <- normalise_timeplot_data(mydata, normalise = normalise)

  # need to group pollutants if conditioning
  if (avg.time != "default" && length(percentile) > 1L && missing(group)) {
    group <- TRUE
  }

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab %||% "", auto.text)
  extra.args$main <- quickText(extra.args$main, auto.text)
  extra.args$ylab <- quickText(
    extra.args$ylab %||%
      dplyr::case_when(
        !is.null(normalise) ~
          paste("normalised", paste(pollutant, collapse = ", ")),
        .default = paste(pollutant, collapse = ", ")
      ),
    auto.text
  )

  # if stacking of plots by year is needed
  if (stack || all(type == "year")) {
    mydata$year <- as.character(lubridate::year(mydata$date))
    if (is.null(extra.args$layout)) {
      extra.args$layout <- c(1, length(unique(mydata$year)))
    }
    lubridate::year(mydata$date) <- lubridate::year(mydata$date)[1]
    date.format <- date.format %||% "%b"
  }

  # make sure order is correct
  mydata$variable <- factor(
    mydata$variable,
    levels = pollutant,
    labels = name.pol
  )

  # x-axis scale function
  if (lubridate::is.Date(mydata$date)) {
    x_scale_fun <- ggplot2::scale_x_date
  } else {
    x_scale_fun <- ggplot2::scale_x_datetime
  }

  # number of distinct pollutants (for panel layout / faceting)
  npol <- length(unique(mydata$variable))

  # number of groups used for colour/linetype/linewidth aesthetics
  if (group_is_col) {
    group_levels <- unique(as.character(mydata[[group]]))
    mydata[[group]] <- factor(mydata[[group]], levels = group_levels)
    n_groups <- length(group_levels)
  } else {
    n_groups <- npol
  }

  extra.args$lty <- extra.args$lty %||% 1
  while (length(extra.args$lty) < n_groups) {
    extra.args$lty <- c(extra.args$lty, extra.args$lty)
  }
  extra.args$lty <- extra.args$lty[1:n_groups]

  extra.args$lwd <- extra.args$lwd %||% 1
  while (length(extra.args$lwd) < n_groups) {
    extra.args$lwd <- c(extra.args$lwd, extra.args$lwd)
  }
  extra.args$lwd <- extra.args$lwd[1:n_groups]

  # layout - stack vertically
  if (
    is.null(extra.args$layout) &&
      !isTRUE(group) &&
      !stack &&
      all(type == "default")
  ) {
    extra.args$layout <- c(1, npol)
  }

  # deal with type
  # when group is a string, !isTRUE(group) is TRUE, so multiple pollutants
  # still get their own panels (coloured by the group column within each)
  if (!isTRUE(group) && npol > 1) {
    type <- c(type, "variable")
  }

  if (stack) {
    type <- c(type, "year")
  }

  if (length(type) > 1) {
    type <- type[type != "default"]
  }

  theGuide <-
    ggplot2::guide_legend(
      reverse = key.position %in% c("left", "right"),
      theme = ggplot2::theme(
        legend.title.position = ifelse(
          key.position %in% c("left", "right"),
          "top",
          key.position
        )
      ),
      ncol = if (missing(key.columns)) {
        if (key.position %in% c("left", "right")) {
          1
        } else {
          n_groups
        }
      } else {
        key.columns
      }
    )

  # aesthetic column: group column name when group is a string, else "variable"
  aes_col <- if (group_is_col) group else "variable"
  # legend title: show group column name when group is a string, else no title
  legend_title <- if (group_is_col) quickText(group, auto.text) else NULL

  # built plot
  thePlot <-
    ggplot2::ggplot(
      mydata,
      ggplot2::aes(
        x = .data$date,
        y = .data$value,
        color = .data[[aes_col]],
        fill = .data[[aes_col]],
        linetype = .data[[aes_col]]
      )
    ) +
    ggplot2::geom_line(
      ggplot2::aes(linewidth = .data[[aes_col]]),
      show.legend = TRUE
    ) +
    gg_ref_x(ref.x) +
    gg_ref_y(ref.y) +
    theme_openair(key.position = ifelse(n_groups == 1, "none", key.position)) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      title = extra.args$main,
      fill = legend_title,
      colour = legend_title,
      linetype = legend_title,
      linewidth = legend_title
    ) +
    get_facet(
      type,
      extra.args,
      scales = relation_to_facet_scales(x.relation, y.relation),
      auto.text = auto.text,
      drop = FALSE,
      strip.position = strip.position
    ) +
    ggplot2::coord_cartesian(
      xlim = extra.args$xlim,
      ylim = extra.args$ylim
    ) +
    x_scale_fun(
      breaks = scales::breaks_pretty(date.breaks),
      date_labels = date.format %||% ggplot2::waiver(),
      expand = ggplot2::expansion(c(0.025, 0.025))
    ) +
    ggplot2::scale_y_continuous(
      expand = if (is.null(extra.args$ylim)) {
        ggplot2::expansion(mult = c(0.1, 0.1))
      } else {
        ggplot2::expansion()
      },
      transform = ifelse(log, "log10", "identity")
    ) +
    ggplot2::scale_color_manual(
      values = openColours(scheme = cols, n = n_groups),
      drop = FALSE,
      label = \(x) label_openair(x, auto_text = auto.text),
      aesthetics = c("fill", "colour")
    ) +
    ggplot2::scale_linetype_manual(
      values = extra.args$lty,
      drop = FALSE,
      label = \(x) label_openair(x, auto_text = auto.text)
    ) +
    ggplot2::scale_linewidth_manual(
      values = extra.args$lwd / 2,
      drop = FALSE,
      label = \(x) label_openair(x, auto_text = auto.text)
    ) +
    ggplot2::guides(
      fill = theGuide,
      color = theGuide,
      linetype = theGuide,
      linewidth = theGuide
    )

  if (stack) {
    thePlot <-
      thePlot +
      ggplot2::theme(
        panel.spacing.y = ggplot2::unit(0, "cm")
      )
  }

  if (smooth) {
    smooth_formula <- if (!is.null(smooth_k)) {
      stats::as.formula(paste0("y ~ s(x, k = ", smooth_k, ")"))
    } else {
      y ~ s(x)
    }
    thePlot <- thePlot +
      ggplot2::stat_smooth(method = "gam", formula = smooth_formula, se = ci)
  }

  if (windflow$windflow) {
    thePlot <-
      thePlot +
      layer_windflow_opts(data = NULL, windflow_opts = windflow)
  }

  # output
  if (plot) {
    plot(thePlot)
  }

  output <- list(plot = thePlot, data = mydata, call = match.call())
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
  group = FALSE,
  ...
) {
  # determine necessary variables
  vars <- c("date", pollutant)
  if (windflow$windflow) {
    vars <- unique(c(vars, "wd", "ws"))
  }
  if (is.character(group)) {
    if (!group %in% dateTypes) {
      vars <- unique(c(vars, group))
    }
  }

  # standard data checks
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # pad out any missing date/times so that line don't extend between areas of missing data
  if (date.pad) {
    mydata <- datePad(mydata, type = type)
  }

  # cut data
  mydata <- cutData(mydata, type, ...)

  # check for duplicates - can't really have duplicate data in a timeplot
  # when group is a column, duplicate check must also split by that column
  if (avg.time == "default") {
    check_type <- if (is.character(group)) c(type, group) else type
    check_duplicate_rows(mydata, check_type, fn = cli::cli_abort)
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
  windflow,
  group = FALSE,
  ...
) {
  # when group is a column name, average within each group separately
  avg_type <- if (is.character(group)) c(type, group) else type

  # average the data if necessary (default does nothing)
  if (avg.time != "default") {
    # deal with multiple percentile values

    if (length(percentile) > 1) {
      prefix <- paste(pollutant, "percentile ")

      mydata <-
        calcPercentile(
          mydata,
          type = avg_type,
          pollutant = pollutant,
          avg.time = avg.time,
          data.thresh = data.thresh,
          percentile = percentile,
          prefix = prefix,
          ...
        )

      pollutant <- paste0(prefix, percentile)
    } else {
      mydata <- timeAverage(
        mydata,
        pollutant = pollutant,
        type = avg_type,
        statistic = statistic,
        avg.time = avg.time,
        data.thresh = data.thresh,
        percentile = percentile,
        ...
      )
    }
  } else if (is.character(group)) {
    mydata <- cutData(mydata, type = group, ...)
  }

  # timeAverage drops type if default
  if (any(type == "default")) {
    mydata$default <- "default"
  }

  # need to flag if ws/wd are being plotted *and* used for windflow
  flag_wind_pollutant <- windflow$windflow &&
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
        dplyr::slice_min(
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
          by = dplyr::join_by("variable")
        ) |>
        dplyr::mutate(
          value = 100 * (.data$value / .data$target_value)
        ) |>
        dplyr::select(-"target_value")
    }

    # return input data
    return(mydata)
  }
