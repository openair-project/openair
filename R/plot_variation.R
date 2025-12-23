#' Plot the variation in a variable with flexible conditioning
#'
#' This function plots the variation of a pollutant or other variable (e.g.,
#' bootstrapped confidence interval around the mean, or a quantile range around
#' the median). These variations are grouped by up to three different
#' conditioning variables - `x` (x-axis), `group` (colour) and `type` (panels).
#' `x` may usefully be a temporal variable for time series data (e.g.,
#' `"month"`, `"year"`, etc.), but could be any factor (e.g., `"site"`) or
#' numeric (e.g., `"no2"`, which will be binned) variable.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @inheritParams shared_ggplot_params
#'
#' @param x,type The name of the data series to use as the x-axis or
#'   conditioning variable, passed to [cutData()]. These are used before
#'   applying `statistic`. `type` may be `NULL` or a vector with maximum length
#'   2, which creates a 2D grid of plots.
#'
#' @param group The name of the data series to use to colour traces within a
#'   single panel, passed to [cutData()].
#'
#' @param statistic One of:
#'
#' - `"mean"`, which displays the bootstrapped mean confidence interval in the data.
#'
#' - `"median"`, which displays the median value along with a quantile range.
#'
#'   The `stat_interval` argument helps control how the intervals are
#'   calculated.
#'
#' @param stat_interval A single numeric value between `0` and `1`. When
#'   `statistic = "mean"`, this is the confidence interval around the
#'   bootstrapped mean. When `statistic = "median"`, this is the upper quantile
#'   (the lower quantile is calculated using `1 - stat_interval`). `TRUE` will
#'   use sensible defaults (`0.95` for `"mean"`, `0.75` for `"median"`) and
#'   `FALSE` will remove intervals entirely.
#'
#' @inheritSection shared_ggplot_params Controlling scales
#' @inheritSection shared_ggplot_params Conditioning with `type`
#'
#' @export
#'
#' @return a [ggplot2][ggplot2::ggplot2-package] plot, or `data.frame` if `plot
#'   = FALSE`.
#'
#' @author Jack Davison
#'
#' @seealso The legacy [timeVariation()] function
#'
#' @export
plot_variation <- function(
  data,
  pollutant,
  x = "month",
  group = NULL,
  type = NULL,
  statistic = c("mean", "median"),
  stat_interval = TRUE,
  windflow = FALSE,
  scale_y = openair::scale_opts(),
  cols = "tol",
  auto_text = TRUE,
  facet_opts = openair::facet_opts(),
  plot = TRUE,
  ...
) {
  scale_y <- resolve_scale_opts(scale_y)

  # disallow multiple pollutants & groups
  if (length(pollutant) > 1L && !is.null(group)) {
    cli::cli_abort(
      "Please provide {.emph either} {1} {.field pollutant} and a {.field group}, {.emph or} multiple {.field pollutants} but leave {.field group} as {.code NULL}."
    )
  }

  # match statistic input
  statistic <- rlang::arg_match(statistic)

  # get x/group/type variables
  data <- cutData(data, x, is.axis = TRUE, ...)
  data <- cutData(data, c(type, group), is.axis = FALSE, ...)

  # always put in long format
  data <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(pollutant),
    names_to = "variable",
    values_to = "value",
    names_transform = factor
  )

  # if no group, the group is the pollutant (even if only one)
  if (is.null(group)) {
    group <- "variable"
  }

  # calculate summaries
  if (statistic == "mean") {
    if (rlang::is_logical(stat_interval)) {
      stat_interval <- ifelse(stat_interval, 0.95, 0)
    }
    probs <- sort(c(1 - stat_interval, stat_interval))
    plotdata <-
      dplyr::reframe(
        data,
        bootMeanDF(.data$value, conf.int = stat_interval, B = 100),
        .by = dplyr::all_of(c(group, x, type))
      )
  } else if (statistic == "median") {
    if (rlang::is_logical(stat_interval)) {
      stat_interval <- ifelse(stat_interval, 0.75, 0.5)
    }
    probs <- sort(c(1 - stat_interval, stat_interval))
    plotdata <-
      dplyr::reframe(
        data,
        mean = stats::median(.data$value, na.rm = TRUE),
        min = stats::quantile(.data$value, na.rm = TRUE, probs = probs[1]),
        max = stats::quantile(.data$value, na.rm = TRUE, probs = probs[2]),
        .by = dplyr::all_of(c(group, x, type))
      )
  }

  # if windflow, need average ws/wd
  if (windflow) {
    winddata <-
      data |>
      dplyr::mutate(
        u = -.data$ws * sin(.data$wd * pi / 180),
        v = -.data$ws * cos(.data$wd * pi / 180)
      ) |>
      dplyr::summarise(
        u = mean(u, na.rm = TRUE),
        v = mean(v, na.rm = TRUE),
        .by = dplyr::all_of(c(group, x, type))
      ) |>
      dplyr::mutate(
        ws = sqrt(u^2 + v^2),
        wd = atan2(u, v) * 180 / pi,
        wd = (wd + 360) %% 360,
        .keep = "unused"
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(c(group, x, type))
      ))

    plotdata <- dplyr::left_join(plotdata, winddata, by = c(group, x, type))
  }

  # construct plot
  plt <-
    plotdata |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[x]],
        y = .data$mean,
        ymax = .data$max,
        ymin = .data$min
      )
    ) +
    get_facet_fun(
      type,
      facet_opts = facet_opts,
      auto_text = auto_text
    ) +
    ggplot2::labs(
      color = NULL,
      fill = NULL,
      x = label_openair(x, auto_text = auto_text),
      y = label_openair(
        paste(pollutant, collapse = ", "),
        auto_text = auto_text
      )
    ) +
    theme_oa_classic() +
    ggplot2::scale_color_manual(
      values = openair::openColours(
        scheme = cols,
        n = dplyr::n_distinct(levels(plotdata[[group]]))
      ),
      label = \(x) label_openair(x, auto_text = auto_text),
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      breaks = scale_y$breaks,
      labels = scale_y$labels,
      limits = scale_y$limits,
      transform = scale_y$transform,
      position = scale_y$position %||% "left",
      sec.axis = scale_y$sec.axis
    ) +
    ggplot2::scale_color_discrete(
      label = label_openair,
      aesthetics = c("color", "fill")
    )

  # add geoms depending on the data type
  if (is.ordered(plotdata[[x]]) || is.numeric(plotdata[[x]])) {
    plt <-
      plt +
      ggplot2::geom_line(
        ggplot2::aes(
          group = factor(.data[[group]], ordered = FALSE),
          color = factor(.data[[group]], ordered = FALSE)
        )
      ) +
      ggplot2::geom_crossbar(
        ggplot2::aes(
          fill = factor(.data[[group]], ordered = FALSE)
        ),
        alpha = 1 / 3,
        color = NA
      )
  } else {
    plt <-
      plt +
      ggplot2::geom_crossbar(
        ggplot2::aes(
          fill = factor(.data[[group]], ordered = FALSE)
        ),
        alpha = 1 / 3,
        color = NA
      ) +
      ggplot2::geom_crossbar(
        ggplot2::aes(
          color = factor(.data[[group]], ordered = FALSE),
          ymin = .data$mean,
          ymax = .data$mean
        )
      )
  }

  # remove legend if only one item
  if (dplyr::n_distinct(levels(plotdata[[group]])) == 1) {
    plt <- plt +
      ggplot2::guides(
        color = ggplot2::guide_none(),
        fill = ggplot2::guide_none()
      )
  }

  # if windflow, need to add it
  if (windflow) {
    plt <- plt +
      layer_windflow(ggplot2::aes(
        ws = .data$ws,
        wd = .data$wd,
        group = .data[[group]]
      ))
  }

  if (plot) {
    return(plt)
  } else {
    return(plotdata)
  }
}

#' Temporal variation plots with flexible panel control
#'
#' Plots temporal variation for different variables, typically pollutant
#' concentrations, across user-defined time scales. Multiple panels can be
#' shown, such as hour of the day, day of the week, week of the year, month of
#' the year, annual mean, or any other time-based grouping the user specifies.
#' By default, this function plots the diurnal, day of the week and monthly
#' variation for different variables, typically pollutant concentrations. Four
#' separate plots are produced.
#'
#' The variation of pollutant concentrations by time can reveal many interesting
#' features that relate to source types and meteorology. For traffic sources,
#' there are often important differences in the way vehicles vary by type -
#' e.g., fewer heavy vehicles at weekends.
#'
#' The [timeVariation()] function makes it easy to see how concentrations (and
#' many other variable types) vary across different temporal resolutions. Users
#' have full control over which based panels are shown, allowing for more
#' tailored and insightful analysis.
#'
#' The plots also show the 95% confidence intervals in the mean. The 95%
#' confidence intervals are calculated through bootstrap simulations, which will
#' provide more robust estimates of the confidence intervals (particularly when
#' there are relatively few data).
#'
#' The function can handle multiple pollutants and uses the flexible `type`
#' option to provide separate panels for each 'type' — see [cutData()] for more
#' details. [timeVariation()] also accepts a `group` option, useful for stacked
#' data. This works similarly to having multiple pollutants in separate columns.
#'
#' Users can supply their own `ylim`, e.g. `ylim = c(0, 200)`, which will be
#' used for all plots. Alternatively, `ylim` can be a list equal to the length
#' of `panels` to control y-limits for each individual panel, e.g. `ylim =
#' list(c(-100,500), c(200, 300), c(-400,400), c(50,70))`.
#'
#' The `difference` option calculates the difference in means between two
#' pollutants, along with bootstrap estimates of the 95\% confidence intervals
#' in the difference. This works in two ways: either two pollutants are supplied
#' in separate columns (e.g. `pollutant = c("no2", "o3")`), or there are two
#' unique values of `group`. The difference is calculated as the second
#' pollutant minus the first and is labelled accordingly. This feature is
#' particularly useful for model evaluation and identifying where models diverge
#' from observations across time scales.
#'
#' Note also that the [timeVariation()] function works well on a subset of data
#' and in conjunction with other plots. For example, a [polarPlot()] may
#' highlight an interesting feature for a particular wind speed/direction range.
#' By filtering for those conditions [timeVariation()] can help determine
#' whether the temporal variation of that feature differs from other features
#' --- and help with source identification.
#'
#' The function also supports non-pollutant variables, such as meteorological or
#' traffic flow data.
#'
#' Depending on the choice of statistic, a subheading is added. Users can
#' control the text in the subheading through the use of `sub` e.g. `sub = ""`
#' will remove any subheading.
#'
#' @inheritParams timePlot
#'
#' @param panels A vector of character values which can be passed to
#'   [cutData()]; used to define each panel in the plot. The first panel will
#'   take up the entire first row, and any remaining panels will make up the
#'   bottom row. If a single panel is given, it will take up the entire plotting
#'   area. Combining two `type` strings delimited with a full stop (e.g.,
#'   `"hour.weekday"`) will use the first as the x-axis variable the second as a
#'   facet.
#'
#' @param local.tz Should the results be calculated in local time that includes
#'   a treatment of daylight savings time (DST)? The default is not to consider
#'   DST issues, provided the data were imported without a DST offset. Emissions
#'   activity tends to occur at local time e.g. rush hour is at 8 am every day.
#'   When the clocks go forward in spring, the emissions are effectively
#'   released into the atmosphere typically 1 hour earlier during the summertime
#'   i.e. when DST applies. When plotting diurnal profiles, this has the effect
#'   of \dQuote{smearing-out} the concentrations. Sometimes, a useful approach
#'   is to express time as local time. This correction tends to produce
#'   better-defined diurnal profiles of concentration (or other variables) and
#'   allows a better comparison to be made with emissions/activity data. If set
#'   to `FALSE` then GMT is used. Examples of usage include `local.tz =
#'   "Europe/London"`, `local.tz = "America/New_York"`. See `cutData` and
#'   `import` for more details.
#'
#' @param normalise Should variables be normalised? The default is `FALSE`. If
#'   `TRUE` then the variable(s) are divided by their mean values. This helps to
#'   compare the shape of the diurnal trends for variables on very different
#'   scales.
#'
#' @param xlab x-axis label; one for each `panel`. Defaults to the x-axis
#'   variable defined in `panels`. Must be the same length as `panels`.
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
#'   Only one `type` is allowed in [timeVariation()], and it is applied to each
#'   `panel`. For additional splits, use the `"x.type"` syntax in the `panels`
#'   argument (e.g, `panels = c("hour.weekday")`).
#'
#' @param group This sets the grouping variable to be used. For example, if a
#'   data frame had a column `site` setting `group = "site"` will plot all sites
#'   together in each panel.
#'
#' @param difference If two pollutants are chosen then setting `difference =
#'   TRUE` will also plot the difference in means between the two variables as
#'   `pollutant[2] - pollutant[1]`. Bootstrap 95\% confidence intervals of the
#'   difference in means are also calculated. A horizontal dashed line is shown
#'   at y = 0. The difference can also be calculated if there is a column that
#'   identifies two groups, e.g., having used [splitByDate()]. In this case it
#'   is possible to call [timeVariation()] with the option `group = "split.by"`
#'   and `difference = TRUE`.
#'
#' @param statistic Can be `"mean"` (default) or `"median"`. If the statistic is
#'   `"mean"` then the mean line and the 95% confidence interval in the mean are
#'   plotted by default. If the statistic is `"median"` then the median line is
#'   plotted together with the 5/95 and 25/75th quantiles are plotted. Users can
#'   control the confidence intervals with `conf.int`.
#'
#' @param conf.int The confidence intervals to be plotted. If `statistic =
#'   "mean"` then the confidence intervals in the mean are plotted. If
#'   `statistic = "median"` then the `conf.int` and `1 - conf.int` *quantiles*
#'   are plotted. `conf.int` can be of length 2, which is most useful for
#'   showing quantiles. For example `conf.int = c(0.75, 0.99)` will yield a plot
#'   showing the median, 25/75 and 5/95th quantiles.
#'
#' @param B Number of bootstrap replicates to use. Can be useful to reduce this
#'   value when there are a large number of observations available to increase
#'   the speed of the calculations without affecting the 95% confidence interval
#'   calculations by much.
#'
#' @param ci Should confidence intervals be shown? The default is `TRUE`.
#'   Setting this to `FALSE` can be useful if multiple pollutants are chosen
#'   where over-lapping confidence intervals can over complicate plots.
#'
#' @param key By default [timeVariation()] produces four plots on one page.
#'   While it is useful to see these plots together, it is sometimes necessary
#'   just to use one for a report. If `key` is `TRUE`, a key is added to all
#'   plots allowing the extraction of a single plot *with* key. See below for an
#'   example. If `key` is `FALSE`, no key is shown for any plot.
#'
#' @param start.day What day of the week should the plots start on? The user can
#'   change the start day by supplying an integer between `0` and `6`. `Sunday =
#'   0`, `Monday = 1`, and so on. For example to start the weekday plots on a
#'   Saturday, choose `start.day = 6`.
#'
#' @param panel.gap The gap between panels in any split panel (e.g., the default
#'   `"hour.weekday"` panel).
#'
#' @param alpha The alpha transparency used for plotting confidence intervals.
#'   `0` is fully transparent and 1 is opaque. The default is `0.4`.
#'
#' @param ... Other graphical parameters passed onto [lattice::xyplot()] and
#'   [cutData()]. For example, in the case of [cutData()] the option `hemisphere
#'   = "southern"`. Note that [cutData()] is used in `type`, `group` and
#'   `panels`, and `...` will be passed to all three.
#'
#' @import lattice
#' @export
#' @return an [openair][openair-package] object. The components of
#'   [timeVariation()] are named after `panels`. Associated data.frames can be
#'   extracted directly using the `subset` option, e.g. as in `plot(object,
#'   subset = "day.hour")`, `summary(output, subset = "hour")`, etc., for
#'   `output <- timeVariation(mydata, "nox")`
#' @author David Carslaw
#' @family time series and trend functions
#' @seealso The newer [plot_variation()] function
#' @examples
#'
#' # basic use
#' timeVariation(mydata, pollutant = "nox")
#'
#' # for a subset of conditions
#' \dontrun{
#' timeVariation(subset(mydata, ws > 3 & wd > 100 & wd < 270),
#'   pollutant = "pm10", ylab = "pm10 (ug/m3)"
#' )
#'
#' # multiple pollutants with concentrations normalised
#' timeVariation(mydata, pollutant = c("nox", "co"), normalise = TRUE)
#'
#' # show BST/GMT variation (see ?cutData for more details)
#' # the NOx plot shows the profiles are very similar when expressed in
#' # local time, showing that the profile is dominated by a local source
#' # that varies by local time and not by GMT i.e. road vehicle emissions
#'
#' timeVariation(mydata, pollutant = "nox", type = "dst", local.tz = "Europe/London")
#'
#' # In this case it is better to group the results for clarity:
#' timeVariation(mydata, pollutant = "nox", group = "dst", local.tz = "Europe/London")
#'
#' # By contrast, a variable such as wind speed shows a clear shift when
#' #  expressed in local time. These two plots can help show whether the
#' #  variation is dominated by man-made influences or natural processes
#'
#' timeVariation(mydata, pollutant = "ws", group = "dst", local.tz = "Europe/London")
#'
#' # It is also possible to plot several variables and set type. For
#' # example, consider the NOx and NO2 split by levels of O3:
#'
#' timeVariation(mydata, pollutant = c("nox", "no2"), type = "o3", normalise = TRUE)
#'
#' # difference in concentrations
#' timeVariation(mydata, poll = c("pm25", "pm10"), difference = TRUE)
#'
#' # It is also useful to consider how concentrations vary by
#' # considering two different periods e.g. in intervention
#' # analysis. In the following plot NO2 has clearly increased but much
#' # less so at weekends - perhaps suggesting vehicles other than cars
#' # are important because flows of cars are approximately invariant by
#' # day of the week
#'
#' mydata <- splitByDate(mydata, dates = "1/1/2003", labels = c("before Jan. 2003", "After Jan. 2003"))
#' timeVariation(mydata, pollutant = "no2", group = "split.by", difference = TRUE)
#'
#' # sub plots can be extracted from the openair object
#' myplot <- timeVariation(mydata, pollutant = "no2")
#' plot(myplot, subset = "day.hour") # top weekday and plot
#'
#' # individual plots
#' # plot(myplot, subset="day.hour") for the weekday and hours subplot (top)
#' # plot(myplot, subset="hour") for the diurnal plot
#' # plot(myplot, subset="day") for the weekday plot
#' # plot(myplot, subset="month") for the monthly plot
#'
#' # numerical results (mean, lower/upper uncertainties)
#' # myplot$data$day.hour # the weekday and hour data set
#' # summary(myplot, subset = "hour") #summary of hour data set
#' # head(myplot, subset = "day") #head/top of day data set
#' # tail(myplot, subset = "month") #tail/top of month data set
#'
#' # plot quantiles and median
#' timeVariation(mydata, stati = "median", poll = "pm10", col = "firebrick")
#'
#' # with different intervals
#' timeVariation(mydata,
#'   stati = "median", poll = "pm10", conf.int = c(0.75, 0.99),
#'   col = "firebrick"
#' )
#'
#' # with different (arbitrary) panels
#' # note 'hemisphere' is passed to cutData() for season
#' timeVariation(
#'   mydata,
#'   pollutant = "no2",
#'   panels = c("weekday.season", "year", "wd"),
#'   hemisphere = "southern"
#' )
#' }
timeVariation <- function(
  mydata,
  pollutant = "nox",
  panels = c(
    "hour.weekday",
    "hour",
    "month",
    "weekday"
  ),
  local.tz = NULL,
  normalise = FALSE,
  xlab = NULL,
  name.pol = pollutant,
  type = "default",
  group = NULL,
  difference = FALSE,
  statistic = "mean",
  conf.int = 0.95,
  B = 100,
  ci = TRUE,
  cols = "hue",
  ref.y = NULL,
  key = NULL,
  key.columns = NULL,
  start.day = 1,
  panel.gap = 0.2,
  auto.text = TRUE,
  alpha = 0.4,
  plot = TRUE,
  ...
) {
  # if median, use alternative default
  if (statistic == "median" && missing(conf.int)) {
    conf.int <- c(0.75, 0.95)
  }

  # validate inputs
  validate_tv_inputs(
    mydata = mydata,
    group = group,
    pollutant = pollutant,
    type = type,
    difference = difference,
    statistic = statistic,
    conf.int = conf.int,
    panels = panels,
    xlab = xlab
  )

  # graphical parameter handling

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  # extra.args setup
  extra.args <- list(...)

  # month.last deprecation
  if ("month.last" %in% names(extra.args)) {
    if (isTRUE(extra.args$month.last)) {
      cli::cli_warn(c(
        "!" = "{.arg month.last} has been deprecated. Please use the {.arg panels} argument for flexible control over panels.",
        "i" = "Setting {.arg panels} to {.code c('hour.weekday', 'hour', 'weekday', 'month')}."
      ))
      panels <- c("hour.weekday", "hour", "weekday", "month")
    }
    extra.args$month.last <- NULL
  }

  # set graphics
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(
    fontsize = current.font
  ))
  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  # label controls
  # xlab handled in formals and code because unique
  extra.args$ylab <- quickText(
    extra.args$ylab %||%
      ifelse(normalise, "normalised level", paste(pollutant, collapse = ", ")),
    auto.text
  )
  extra.args$main <- quickText(extra.args$main %||% "", auto.text)

  extra.args$sub <- quickText(
    extra.args$sub %||% create_tv_sub_text(statistic, conf.int),
    auto.text
  )

  extra.args$lwd <- extra.args$lwd %||% 2

  # if user supplies separate ylims for each plot
  ylimList <- FALSE
  if ("ylim" %in% names(extra.args)) {
    if (is.list(extra.args$ylim)) {
      if (length(extra.args$ylim) != length(panels)) {
        cli::cli_abort(
          "{.arg ylim} should be equal in length to {.arg panels} ({length(panels)})."
        )
      }
      ylim.list <- extra.args$ylim
      ylimList <- TRUE
    }
  }

  # check & cut data
  vars <- c("date", pollutant)

  # if group is present and not a date-type (e.g., year), add to vars
  if (!is.null(group)) {
    if (!group %in% dateTypes) {
      vars <- unique(c(vars, group))
    }
  }

  # if any panels aren't datetypes (e.g., sites), add to vars
  panel_vars <- unique(purrr::list_c(strsplit(panels, "\\.")))
  if (any(!panel_vars %in% dateTypes)) {
    vars <- unique(c(vars, panel_vars[!panel_vars %in% dateTypes]))
  }

  # data checks
  mydata <- mydata |>
    checkPrep(vars, type, remove.calm = FALSE) |>
    cutData(type = c(type, group), local.tz = local.tz, ...)

  # need to isolate "type" as timeVar will try to turn it numeric, which will
  # break the strip labels
  if (type != "default") {
    orig_type <- type
    mydata$openair_type <- mydata[[type]]
    type <- "openair_type"
  }

  # need to isolate "group" as if the grouping var is also a panel it'll be
  # stripped away
  if (!is.null(group)) {
    orig_group <- group
    mydata$openair_group <- mydata[[group]]
    group <- "openair_group"
  }

  # put in local time if needed
  if (!is.null(local.tz)) {
    attr(mydata$date, "tzone") <- local.tz
  }

  # title for overall and individual plots
  overall.main <- extra.args$main
  extra.args$main <- ""
  overall.sub <- extra.args$sub
  extra.args$sub <- ""

  # labels for pollutants, can be user-defined, special handling when difference = TRUE
  poll.orig <- pollutant
  if (difference && is.null(group)) {
    pollutant <- c(pollutant, paste(pollutant[2], "-", pollutant[1]))
  }
  mylab <- sapply(seq_along(name.pol), function(x) {
    quickText(name.pol[x], auto.text)
  })

  if (is.null(group)) {
    mydata <- tidyr::pivot_longer(
      mydata,
      cols = dplyr::all_of(poll.orig),
      names_to = "variable",
      values_to = "value"
    )
    mydata$variable <- factor(mydata$variable, levels = pollutant)
  } else {
    # group needs to be 'variable' and pollutant 'value'
    id <- which(names(mydata) == poll.orig)
    names(mydata)[id] <- "value"
    id <- which(names(mydata) == group)
    names(mydata)[id] <- "variable"

    mydata$variable <- factor(mydata$variable) # drop unused factor levels
    the.names <- levels(mydata[["variable"]])
    if (difference) {
      the.names <- c(
        the.names,
        paste(
          levels(mydata$variable)[2],
          "-",
          levels(mydata$variable)[1]
        )
      )
    }
    mylab <- sapply(the.names, function(x) quickText(x, auto.text))
  }

  npol <- length(levels(mydata$variable)) # number of pollutants

  if (difference) {
    npol <- 3 # 3 pollutants if difference considered
    if (is.null(group)) {
      poll1 <- pollutant[1]
    }
    poll2 <- pollutant[2]
    if (!is.null(group)) {
      poll1 <- levels(mydata$variable)[1]
    }
    poll2 <- levels(mydata$variable)[2]
  }

  # number of columns for key
  key.columns <- key.columns %||% npol

  myColors <- openColours(cols, npol)

  # for individual plot keys - useful if only one of the plots is extracted after printing
  key_input <- key
  if (isTRUE(key_input)) {
    key <- list(
      rectangles = list(col = myColors[1:npol], border = NA),
      title = "",
      text = list(lab = mylab),
      space = "bottom",
      columns = key.columns,
      lines.title = 1
    )

    extra.args$main <- overall.main
  } else if (isFALSE(key_input)) {
    key <- NULL
  }

  # get the xvars and facets for each panel
  panels_x <- list()
  panels_facet <- list()
  for (i in panels) {
    if (grepl("\\.", i)) {
      x <- strsplit(i, "\\.")[[1]]
      panels_x <- append(panels_x, x[1])
      panels_facet <- append(panels_facet, x[2])
    } else {
      panels_x <- append(panels_x, i)
      panels_facet <- append(panels_facet, list(NULL))
    }
  }

  # if xlab not given, use xvar
  xlab <- xlab %||% panels_x

  # need to retain a list of data, a list of plots, and a list of strips
  data_out <- list()
  plot_out <- list()
  strips_out <- list()

  # create panels iteratively
  for (i in seq_along(panels_x)) {
    # prepare data
    panel.data <-
      prep_panel_data(
        mydata,
        vars = panels_x[[i]],
        facet_vars = panels_facet[[i]],
        conf.int,
        difference,
        normalise,
        type,
        pollutant,
        poll1,
        poll2,
        B,
        statistic,
        start.day = start.day,
        ...
      )
    data_out <- append(data_out, list(panel.data))

    # get ylim for plot
    extra.args <- update_extra_args_ylim(
      data = panel.data$data,
      extra.args,
      ci,
      ylim.list,
      index = i,
      ylimList
    )

    # strip for plot - needed if type used
    strip <- create_tv_strip(
      panel.data$data,
      type = type,
      auto.text = auto.text,
      facet_var = panels_facet[[i]]
    )
    strips_out <- append(strips_out, list(strip))

    # create xyplot
    # (for whatever reason, errors occur when this isn't a function)
    quick_create_tv_xyplot <- function(
      data,
      xvar,
      xlab,
      strip
    ) {
      create_tv_xyplot(
        data = data$data,
        xvar = xvar,
        type = type,
        v_gridlines = data$x_breaks,
        v_labels = data$x_labels,
        xlab = quickText(xlab, auto.text = auto.text),
        key = key,
        strip = strip,
        myColors = myColors,
        panel.gap = panel.gap,
        fun_panel_groups = create_tv_panel_groups(
          data$data,
          xvar[1],
          difference,
          myColors,
          alpha,
          ci,
          ref.y,
          group = group,
          plot_type = ifelse(data$ordered, "l", "p")
        ),
        extra.args = extra.args
      )
    }

    # create plot
    thePlot <- quick_create_tv_xyplot(
      data = panel.data,
      xvar = c(panels_x[[i]], panels_facet[[i]]),
      xlab = xlab[i],
      strip
    )
    plot_out <- append(plot_out, list(thePlot))
  }

  # name the outputs
  names(data_out) <- panels
  names(plot_out) <- panels

  # format output data for return
  format_tv_data_for_output <- function(data) {
    out_data <- data$data

    # give the "type" column a nicer name
    if (type != "default") {
      names(out_data)[names(out_data) == "openair_type"] <- paste(
        orig_type,
        "type",
        sep = "_"
      )
    }

    # reformat the variable column in a nicer way - as long as there's x_labels
    # (i.e., ignore hour/week). Use full labels as sometimes you end up with
    # repeated factor levels (e.g., for month)
    if (!is.null(data$x_labels)) {
      out_data[data$var] <- factor(
        out_data[[data$var]],
        levels = data$x_breaks,
        labels = data$x_labels_full
      )
    }

    out_data
  }

  # if only one panel, just let it fill the whole area
  if (length(plot_out) == 1L) {
    if (is.null(key_input) || isTRUE(key_input)) {
      plot_out[[1]] <-
        update(
          plot_out[[1]],
          key = list(
            rectangles = list(col = myColors[1:npol], border = NA),
            text = list(lab = mylab),
            space = "bottom",
            columns = key.columns,
            title = "",
            lines.title = 1
          )
        )
    }

    if (plot) {
      if (!is.null(panels_facet[[1]]) && type != "default") {
        plot(
          useOuterStrips(
            plot_out[[1]],
            strip = strips_out[[1]]$strip,
            strip.left = strips_out[[1]]$strip.left
          )
        )
      } else {
        plot(plot_out[[1]])
      }
    }

    output <- list(
      plot = append(plot_out, list(subsets = panels)),
      data = append(
        purrr::map(data_out, format_tv_data_for_output),
        list(subsets = panels)
      ),
      call = match.call(),
      main.plot = function(...) {
        plot(plot_out[[1]], ...)
      },
      ind.plot = function(x, ...) {
        plot(x, ...)
      }
    )
    class(output) <- "openair"

    invisible(output)
  } else {
    # this adjusts the space for the title to 2 lines (approx) if \n in title
    if (length(grep("atop", overall.main) == 1)) {
      y.upp <- 0.95
      y.dwn <- 0.05
    } else {
      y.upp <- 0.975
      y.dwn <- 0.025
    }

    main.plot <- function(...) {
      if (is.null(key_input) || isTRUE(key_input)) {
        if (type == "default") {
          print(
            update(
              plot_out[[1]],
              key = list(
                rectangles = list(col = myColors[1:npol], border = NA),
                text = list(lab = mylab),
                space = "bottom",
                columns = key.columns,
                title = "",
                lines.title = 1
              )
            ),
            position = c(0, 0.5, 1, y.upp),
            more = TRUE
          )
        } else {
          print(
            update(
              useOuterStrips(
                plot_out[[1]],
                strip = strips_out[[1]]$strip,
                strip.left = strips_out[[1]]$strip.left
              ),
              key = list(
                rectangles = list(col = myColors[1:npol], border = NA),
                text = list(lab = mylab),
                space = "bottom",
                columns = key.columns,
                title = "",
                lines.title = 1
              )
            ),
            position = c(0, 0.5, 1, y.upp),
            more = TRUE
          )
        }
      } else {
        if (type == "default") {
          print(
            plot_out[[1]],
            position = c(0, 0.5, 1, y.upp),
            more = TRUE
          )
        } else {
          print(
            useOuterStrips(
              plot_out[[1]],
              strip = strips_out[[1]]$strip,
              strip.left = strips_out[[1]]$strip.left
            ),
            position = c(0, 0.5, 1, y.upp),
            more = TRUE
          )
        }
      }

      # iteratively plot lower panels
      bounds <- seq(0, 1, length.out = length(panels))
      for (i in seq_along(plot_out[-1])) {
        if (!is.null(panels_facet[-1][[i]]) && type != "default") {
          print(
            useOuterStrips(
              plot_out[-1][[i]],
              strip = strips_out[-1][[i]]$strip,
              strip.left = strips_out[-1][[i]]$strip.left
            ),
            position = c(bounds[i], y.dwn, bounds[i + 1], 0.53),
            more = i != max(seq_along(plot_out[-1]))
          )
        } else {
          print(
            plot_out[-1][[i]],
            position = c(bounds[i], y.dwn, bounds[i + 1], 0.53),
            more = i != max(seq_along(plot_out[-1]))
          )
        }
      }

      # use grid to add an overall title
      grid.text(overall.main, 0.5, y.upp, gp = gpar(fontsize = 14))
      grid.text(overall.sub, 0.5, y.dwn, gp = gpar(fontsize = 12))
    }

    ind.plot <- function(x, ...) {
      plot(
        update(
          x,
          key = list(
            rectangles = list(col = myColors[1:npol], border = NA),
            text = list(lab = mylab),
            space = "top",
            columns = key.columns
          )
        ),
        ...
      )
    }

    if (plot) {
      main.plot()
    }
    output <- list(
      plot = append(plot_out, list(subsets = panels)),
      data = append(
        purrr::map(data_out, format_tv_data_for_output),
        list(subsets = panels)
      ),
      call = match.call(),
      main.plot = main.plot,
      ind.plot = ind.plot
    )
    class(output) <- "openair"

    invisible(output)
  }
}

# validate timevar inputs
validate_tv_inputs <- function(
  mydata,
  group,
  pollutant,
  type,
  difference,
  statistic,
  conf.int,
  panels,
  xlab
) {
  if (length(type) > 1) {
    cli::cli_abort(
      c(
        "x" = "Can only have one {.arg type} for {.fun openair::timeVariation}.",
        "i" = "In {.fun openair::timeVariation}, {.arg type} is global to all panels. To add additional types to individual panels, use {.code x.type} syntax - e.g., {.code panel = 'hour.weekday'}."
      )
    )
  }

  # validate inputs
  if (!is.null(group) && length(pollutant) > 1) {
    cli::cli_abort(
      "Can only have one {.arg pollutant} and one {.arg group}, or several {.arg pollutant}s and no {.arg group}."
    )
  }

  if (type %in% pollutant) {
    cli::cli_abort(
      "{.arg type} cannot be in {.arg pollutant}. Problem variable: {type[type %in% pollutant]}."
    )
  }

  if (!is.null(group)) {
    if (group %in% pollutant) {
      cli::cli_abort(
        "{.arg group} cannot be in {.arg pollutant}. Problem variable: {group[group %in% pollutant]}."
      )
    }
  }

  # if differences between two pollutants are calculated
  if (difference) {
    if (is.null(group)) {
      if (length(pollutant) != 2) {
        cli::cli_abort(
          "Need to specify two {.arg pollutant}s to calculate their difference."
        )
      }
    }

    if (!is.null(group)) {
      test <- cutData(mydata, type = group)
      if (length(unique(na.omit(test[[group]]))) != 2) {
        cli::cli_abort(
          "Need to specify two {.arg group}s to calculate their difference."
        )
      }
    }
  }

  # statistic check
  rlang::arg_match(statistic, c("mean", "median"))

  if (!length(unique(conf.int)) %in% c(1L, 2L)) {
    cli::cli_abort("{.arg conf.int} can only be of length 1 or 2.")
  }

  # check length of xlab
  if (!is.null(xlab)) {
    if (length(xlab) != length(panels)) {
      cli::cli_abort(
        "Length of {.arg xlab} must be equal to length of {.arg panels}."
      )
    }
  }
}


# calculate difference and normalise
prep_panel_data <- function(
  mydata,
  vars,
  facet_vars = NULL,
  conf.int,
  difference,
  normalise,
  type,
  pollutant,
  poll1,
  poll2,
  B,
  statistic,
  start.day,
  ...
) {
  # mainly use "outside", but for vars treated as numeric (e.g., hour) we need
  # "none" to retain correct factor labels
  drop <- "outside"
  if (vars %in% c("hour", "week")) {
    drop <- "none"
  }

  # cut data for the variables
  mydata <-
    cutData(
      mydata,
      type = c(vars, facet_vars),
      start.day = start.day,
      is.axis = TRUE,
      drop = drop,
      ...
    ) |>
    dplyr::arrange(.data[[vars]])

  # retain the labels for the plot; some need a bit of customisation
  label.len <- 100L
  if (vars == "weekday") {
    label.len <- 3L
  }
  if (vars == "month") {
    label.len <- 1L
  }
  x_labels_full <- levels(mydata[[vars]])
  x_labels <- substr(x_labels_full, 1, label.len)

  # breaks - mostly just an ID for the labels, but can be overwritten
  x_breaks <- seq_along(x_labels)

  # retain whether variable is ordered - used for line vs point
  ordered <- is.ordered(mydata[[vars]])

  # set the x variable to be numeric for plotting
  mydata <-
    dplyr::mutate(
      mydata,
      # set all as numeric
      dplyr::across(dplyr::all_of(vars), \(x) {
        as.numeric(as.factor(x))
      })
    )

  # special case for hour - starts at 00 so needs to bump down one
  if (vars == "hour") {
    mydata$hour <- mydata$hour - 1L
    x_labels <- NULL
    if (dplyr::n_distinct(mydata$hour) == 24) {
      x_breaks <- c(0, 6, 12, 18, 23)
    } else {
      x_breaks <- unique(as.integer(pretty(as.numeric(mydata$hour))))
      x_breaks <- x_breaks[
        x_breaks > min(mydata$hour) & x_breaks < max(mydata$hour)
      ]
      x_breaks <- sort(unique(c(range(mydata$hour, na.rm = TRUE), x_breaks)))
    }
  }

  # same situation for week
  if (vars == "week") {
    mydata$week <- mydata$week - 1L
    x_labels <- NULL
    x_breaks <- unique(as.integer(pretty(as.numeric(mydata$week))))
    x_breaks <- x_breaks[
      x_breaks > min(mydata$week) & x_breaks < max(mydata$week)
    ]
    x_breaks <- sort(unique(c(range(mydata$week, na.rm = TRUE), x_breaks)))
  }

  # combine plotting & facet variables now
  vars <- c(vars, facet_vars)

  # calculate diffs
  if (difference) {
    data <- errorDiff(
      mydata,
      vars = vars,
      type = type,
      poll1 = poll1,
      poll2 = poll2,
      B = B,
      conf.int = conf.int
    )
  } else {
    data <- purrr::map(
      .x = conf.int,
      .f = function(x) {
        calculate_tv_summary_values(
          x,
          mydata,
          vars = vars,
          pollutant,
          type,
          B = B,
          statistic = statistic
        )
      }
    ) |>
      dplyr::bind_rows() |>
      dplyr::tibble()
  }

  # if normalise selected, normalise the data
  if (normalise) {
    data <- mapType(data, type = "variable", fun = function(x) {
      Mean <- mean(x$Mean, na.rm = TRUE)
      x$Mean <- x$Mean / Mean
      x$Lower <- x$Lower / Mean
      x$Upper <- x$Upper / Mean
      x
    })
  }

  # missing Lower ci, set to mean
  ids <- which(is.na(data$Lower))
  data$Lower[ids] <- data$Mean[ids]

  # missing Upper ci, set to mean
  ids <- which(is.na(data$Upper))
  data$Upper[ids] <- data$Mean[ids]

  # return data
  list(
    data = data,
    x_labels = x_labels,
    x_labels_full = x_labels_full,
    x_breaks = x_breaks,
    ordered = ordered,
    var = vars[1]
  )
}

# sub heading stat info
create_tv_sub_text <- function(statistic, conf.int) {
  if (statistic == "mean") {
    sub.text <- paste(
      "mean and ",
      100 * conf.int[1],
      "% confidence interval in mean",
      sep = ""
    )
  }

  if (statistic == "median") {
    if (length(conf.int) == 1L) {
      sub.text <- paste(
        "median and ",
        100 * (1 - conf.int[1]),
        "/",
        100 * conf.int[1],
        "th quantiles",
        sep = ""
      )
    } else {
      sub.text <- paste(
        "median, ",
        100 * (1 - conf.int[1]),
        "/",
        100 * conf.int[1],
        " and ",
        100 * (1 - conf.int[2]),
        "/",
        100 * conf.int[2],
        "th quantiles",
        sep = ""
      )
    }
  }

  sub.text
}

# create strip for a tv panel
create_tv_strip <- function(data, type, auto.text, facet_var = NULL) {
  # Base strip for faceted plots
  strip <- if (!is.null(facet_var)) {
    strip.custom(par.strip.text = list(cex = 0.8))
  } else {
    FALSE
  }

  # Handle default type
  if (type == "default") {
    return(list(
      strip = strip,
      strip.left = FALSE,
      layout = if (!is.null(facet_var)) {
        c(dplyr::n_distinct(data[[facet_var]]), 1)
      } else {
        NULL
      }
    ))
  }

  # Create custom strip with quickText labels
  type_strip <- strip.custom(
    factor.levels = sapply(
      levels(factor(data[[type]])),
      function(x) quickText(x, auto.text)
    )
  )

  # Position strip based on faceting
  if (!is.null(facet_var)) {
    list(
      strip = strip,
      strip.left = type_strip,
      layout = NULL
    )
  } else {
    list(
      strip = type_strip,
      strip.left = FALSE,
      layout = NULL
    )
  }
}

# create formula for a tv panel
create_tv_formula <- function(xvar, type) {
  if (length(xvar) == 1L) {
    temp <- paste(type, collapse = "+")
    myform <- formula(paste("Mean ~", xvar, "|", temp))
  }

  if (length(xvar) == 2L) {
    xvar <- paste(xvar, collapse = "|")
    temp <- paste(type, collapse = "+")
    if (type == "default") {
      myform <- formula(paste("Mean ~", xvar))
    } else {
      myform <- formula(paste("Mean ~", xvar, "*", temp, sep = ""))
    }
    myform
  }

  myform
}

# create the panel.groups function for panel.superpose
create_tv_panel_groups <- function(
  data,
  xvar,
  difference,
  myColors,
  alpha,
  ci,
  ref.y,
  group = NULL,
  plot_type = "l"
) {
  function(
    x,
    y,
    col.line,
    type,
    group.number,
    subscripts,
    ...
  ) {
    if (difference) {
      panel.abline(h = 0, lty = 5)
    }

    # if lots of data, use polygons
    if (length(unique(x)) < 15) {
      ci_fun <- make_rectangles
    } else {
      ci_fun <- make_polygons
    }

    pltType <- plot_type

    # a line won't work for a single point
    if (length(subscripts) == 1) {
      pltType <- "p"
      ci_fun <- make_rectangles
    }

    # special cases - don't want to split a line within a plot
    if (!is.null(group)) {
      if (xvar == "month" && group == "season") {
        pltType <- "p"
      }
      if (xvar == "weekday" && group == "weekend") {
        pltType <- "p"
      }
      if (xvar == "hour" && group == "daylight") {
        pltType <- "p"
        ci_fun <- make_rectangles
      }
    }

    # plot once
    id <- which(data$ci[subscripts] == data$ci[1])
    panel.xyplot(
      x[id],
      y[id],
      type = pltType,
      col.line = myColors[group.number],
      ...
    )

    if (ci) {
      ci_fun(
        data[subscripts, ],
        x = xvar,
        y = "Mean",
        group.number,
        myColors,
        alpha
      )
    }

    # reference line(s)
    if (!is.null(ref.y)) {
      do.call(panel.abline, ref.y)
    }
  }
}

# xy.args
create_tv_xyplot <- function(
  data,
  xvar,
  type,
  v_gridlines,
  v_labels = NULL,
  xlab,
  key,
  strip,
  myColors,
  panel.gap,
  fun_panel_groups,
  extra.args
) {
  # don't want to add space for hourly plots
  xlim_adj <- 0.5
  if (xvar[1] %in% c("week", "hour")) {
    xlim_adj <- 0
  }

  # xy args
  xy.args <- list(
    x = create_tv_formula(xvar, type),
    data = data,
    groups = data$variable,
    as.table = TRUE,
    xlab = xlab,
    xlim = c(
      min(v_gridlines, na.rm = TRUE) - xlim_adj,
      max(v_gridlines, na.rm = TRUE) + xlim_adj
    ),
    strip = strip$strip,
    strip.left = strip$strip.left,
    between = list(x = panel.gap),
    layout = strip$layout,
    par.strip.text = list(cex = 0.8),
    key = key,
    scales = list(
      x = list(at = v_gridlines, labels = v_labels %||% v_gridlines)
    ),
    par.settings = simpleTheme(col = myColors, pch = 16),
    panel = function(x, y, ...) {
      panel.grid(-1, 0)
      panel.abline(v = v_gridlines, col = "grey85")
      panel.superpose(
        x,
        y,
        ...,
        panel.groups = fun_panel_groups
      )
    }
  )

  # reset for extra.args
  xy.args <- listUpdate(xy.args, extra.args)

  # plot
  do.call(xyplot, xy.args)
}

# set the ylim
update_extra_args_ylim <- function(
  data,
  extra.args,
  ci,
  ylim.list,
  index,
  ylimList
) {
  # y range taking account of expanded uncertainties
  get_tv_ylim <- function(x, ci) {
    # if no CI information, just return
    if (all(is.na(x[, c("Lower", "Upper")]))) {
      return(NULL)
    }

    if (ci) {
      lims <- range(c(x$Lower, x$Upper), na.rm = TRUE)
    } else {
      lims <- range(c(x$Mean, x$Mean), na.rm = TRUE)
      if (diff(lims) == 0) {
        return(NULL)
      }
    }

    inc <- 0.04 * abs(lims[2] - lims[1])
    lims <- c(lims[1] - inc, lims[2] + inc)
  }

  # user supplied separate ylim
  if (ylimList) {
    extra.args$ylim <- ylim.list[[index]]
  } else {
    extra.args$ylim <- get_tv_ylim(data, ci)
  }

  extra.args
}

# process
calculate_tv_summary_values <- function(
  conf.int = conf.int,
  mydata,
  vars = "day.hour",
  pollutant,
  type,
  B = B,
  statistic = statistic
) {
  stat <- if (statistic == "mean") {
    bootMean
  } else {
    calculate_median_quants
  }

  calc_summary_values <- function(
    conf.int = conf.int,
    mydata,
    vars = vars,
    FUN,
    type = type,
    B = B,
    statistic = statistic
  ) {
    mydata |>
      dplyr::reframe(
        value = list(FUN(
          value,
          B = B,
          statistic = statistic,
          conf.int = conf.int
        )),
        .by = dplyr::all_of(c("variable", vars, type))
      ) |>
      tidyr::unnest_wider(value)
  }

  results <- list()

  # process non-wind direction components
  if (any(pollutant != "wd")) {
    results$data1 <- mydata |>
      dplyr::filter(.data$variable != "wd") |>
      calc_summary_values(
        vars,
        stat,
        type,
        B = B,
        statistic = statistic,
        conf.int = conf.int
      )
  }

  if ("wd" %in% pollutant) {
    if (length(pollutant) > 1) {
      mydata <- subset(mydata, variable == "wd")
    }
    results$data2 <- mydata |>
      dplyr::filter(.data$variable == "wd") |>
      calc_summary_values(
        vars,
        wd_smean_normal,
        type,
        B = B,
        statistic = statistic,
        conf.int = conf.int
      )
  }

  dplyr::bind_rows(results) |>
    dplyr::mutate(ci = conf.int)
}

wd_smean_normal <- function(wd, B = B, statistic, conf.int) {
  # function to calculate mean and 95% CI of the mean for wd
  u <- mean(sin(pi * wd / 180), na.rm = TRUE)
  v <- mean(cos(pi * wd / 180), na.rm = TRUE)
  Mean <- as.vector(atan2(u, v) * 360 / 2 / pi)
  ids <- which(Mean < 0) # ids where wd < 0
  Mean[ids] <- Mean[ids] + 360

  # to calculate SD and conf int, need to know how much the angle changes from one point
  # to the next. Also cannot be more than 180 degrees. Example change from 350 to 10 is not
  # 340 but 20.
  wd.diff <- diff(wd)
  ids <- which(wd.diff < 0)
  wd.diff[ids] <- wd.diff[ids] + 360
  ids <- which(wd.diff > 180)
  wd.diff[ids] <- abs(wd.diff[ids] - 360)

  if (statistic == "mean") {
    intervals <- bootMean(wd.diff, B = B, conf.int)
  } else {
    intervals <- calculate_median_quants(wd.diff, conf.int)
  }
  Lower <- intervals[2]
  names(Lower) <- NULL

  Upper <- intervals[3]
  names(Upper) <- NULL
  diff.wd <- (Upper - Lower) / 2

  c(Mean = Mean, Lower = Mean - diff.wd, Upper = Mean + diff.wd)
}

#' bootstrap mean difference confidence intervals
#' @noRd
errorDiff <- function(
  mydata,
  vars,
  poll1,
  poll2,
  type,
  B = B,
  conf.int = conf.int
) {
  # it could be dates duplicate e.g. run function over several sites
  if (anyDuplicated(mydata$date) > 0) {
    mydata$rowid <- seq_len(nrow(mydata))
  }

  mydata <- tidyr::pivot_wider(
    mydata,
    names_from = "variable",
    values_from = "value"
  )

  splits <- c(vars, type)

  # warnings from dplyr seem harmless FIXME
  res <-
    mapType(
      mydata,
      type = splits,
      fun = \(df) bootMeanDiff(df, x = poll1, y = poll2, B = B, na.rm = TRUE)
    )

  # make sure we keep the order correct
  res$variable <- ordered(res$variable, levels = res$variable[1:3])
  res$ci <- conf.int[1]
  res
}


# function to calculate median and lower/upper quantiles
calculate_median_quants <- function(x, conf.int = 0.95, na.rm = TRUE, ...) {
  quant <- quantile(x, probs = c(0.5, (1 - conf.int), conf.int), na.rm = na.rm)
  names(quant) <- c("Mean", "Lower", "Upper")
  quant
}

# Helper function for shared CI band logic
make_ci_bands <- function(dat, x, y, group.number, myColors, alpha, draw_func) {
  ci <- sort(unique(dat$ci))
  len <- length(ci)

  if (len == 1L) {
    id1 <- which(dat$ci == ci[1])
    fac <- 2
  } else if (len == 2L) {
    id1 <- which(dat$ci == ci[2])
    id2 <- which(dat$ci == ci[1])
    fac <- 1
  }

  # Draw first band
  draw_func(
    dat = dat,
    x = x,
    ids = id1,
    group.number = group.number,
    myColors = myColors,
    alpha = fac * alpha / 2,
    is_outer = (len == 2L)
  )

  # Draw second band if needed
  if (len == 2L) {
    draw_func(
      dat = dat,
      x = x,
      ids = id2,
      group.number = group.number,
      myColors = myColors,
      alpha = alpha,
      is_outer = FALSE
    )
  }
}

# Make poly function
make_polygons <- function(
  dat,
  x = "hour",
  y = "Mean",
  group.number,
  myColors,
  alpha
) {
  make_ci_bands(
    dat,
    x,
    y,
    group.number,
    myColors,
    alpha,
    function(dat, x, ids, group.number, myColors, alpha, is_outer) {
      poly.na(
        dat[[x]][ids],
        dat$Lower[ids],
        dat[[x]][ids],
        dat$Upper[ids],
        group.number,
        myColors,
        alpha = alpha
      )
    }
  )
}

# Make rect function
make_rectangles <- function(
  dat,
  x = "weekday",
  y = "Mean",
  group.number,
  myColors,
  alpha
) {
  make_ci_bands(
    dat,
    x,
    y,
    group.number,
    myColors,
    alpha,
    function(dat, x, ids, group.number, myColors, alpha, is_outer) {
      width <- ifelse(is_outer, 0.15, 0.3)
      panel.rect(
        dat[[x]][ids] - width,
        dat$Lower[ids],
        dat[[x]][ids] + width,
        dat$Upper[ids],
        fill = myColors[group.number],
        border = NA,
        alpha = alpha
      )
    }
  )
}
