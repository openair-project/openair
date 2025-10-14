#' Time series plot with categories shown as a stacked bar chart
#'
#' This function shows time series plots as stacked bar charts. The different
#' categories in the bar chart are made up from a character or factor variable
#' in a data frame. The function is primarily developed to support the plotting
#' of cluster analysis output from [polarCluster()] and [trajCluster()] that
#' consider local and regional (back trajectory) cluster analysis respectively.
#' However, the function has more general use for understanding time series
#' data.
#'
#' In order to plot time series in this way, some sort of time aggregation is
#' needed, which is controlled by the option `avg.time`.
#'
#' The plot shows the value of `pollutant` on the y-axis (averaged according to
#' `avg.time`). The time intervals are made up of bars split according to
#' `proportion`. The bars therefore show how the total value of `pollutant` is
#' made up for any time interval.
#'
#' @inheritParams timePlot
#'
#' @param mydata A data frame containing the fields `date`, `pollutant` and a
#'   splitting variable `proportion`
#' @param pollutant Name of the pollutant to plot contained in `mydata`.
#' @param proportion The splitting variable that makes up the bars in the bar
#'   chart e.g. `proportion = "cluster"` if the output from `polarCluster` is
#'   being analysed. If `proportion` is a numeric variable it is split into 4
#'   quantiles (by default) by `cutData`. If `proportion` is a factor or
#'   character variable then the categories are used directly.
#' @param avg.time This defines the time period to average to. Can be `"sec"`,
#'   `"min"`, `"hour"`, `"day"`, `"DSTday"`, `"week"`, `"month"`, `"quarter"` or
#'   `"year"`. For much increased flexibility a number can precede these options
#'   followed by a space. For example, a timeAverage of 2 months would be
#'   `period = "2 month"`.
#'
#'   Note that `avg.time` when used in `timeProp` should be greater than the
#'   time gap in the original data. For example, `avg.time = "day"` for hourly
#'   data is OK, but `avg.time = "hour"` for daily data is not.
#' @param normalise If `normalise = TRUE` then each time interval is scaled to
#'   100. This is helpful to show the relative (percentage) contribution of the
#'   proportions.
#' @param key.title The title of the key.
#' @param ... Other graphical parameters passed onto `timeProp` and `cutData`.
#'   For example, `timeProp` passes the option `hemisphere = "southern"` on to
#'   `cutData` to provide southern (rather than default northern) hemisphere
#'   handling of `type = "season"`. Similarly, common axis and title labelling
#'   options (such as `xlab`, `ylab`, `main`) are passed to `xyplot` via
#'   `quickText` to handle routine formatting.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family time series and trend functions
#' @family cluster analysis functions
#' @examples
#' # monthly plot of SO2 showing the contribution by wind sector
#' timeProp(mydata, pollutant = "so2", avg.time = "month", proportion = "wd")
timeProp <- function(
  mydata,
  pollutant = "nox",
  proportion = "cluster",
  avg.time = "day",
  type = "default",
  cols = "Set1",
  normalise = FALSE,
  key = TRUE,
  key.columns = 1,
  key.position = "right",
  key.title = proportion,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  # can only have one type
  if (length(type) > 1) {
    cli::cli_abort("{.arg type} can only be of length {1L}.")
  }

  # extra.args setup
  extra.args <- list(...)

  # reset graphic parameters
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # label controls
  main <- quickText(extra.args$main %||% "", auto.text)
  xlab <- quickText(extra.args$xlab %||% "date", auto.text)
  ylab <- quickText(extra.args$ylab %||% pollutant, auto.text)
  sub <- extra.args$sub %||% "contribution weighted by mean"

  # fontsize handling
  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  # variables needed
  vars <- c("date", pollutant, proportion)

  # check the data
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # time zone of input data
  tzone <- attr(mydata$date, "tzone")

  # cut data
  mydata <- cutData(mydata, c(type, proportion))

  # groups for dplyr
  group_1 <- c("xleft", "xright", type)
  group_2 <- c(type, "xleft", "xright", proportion)

  # summarise by proportion, type etc
  results <-
    mydata |>
    # calculate left and right extremes of each bar, add the most common
    # non-zero time interval to left to get right
    dplyr::mutate(
      xleft = as.POSIXct(cut(.data$date, avg.time), tz = tzone),
      xright = .data$xleft + median(diff(.data$xleft)[diff(.data$xleft) != 0])
    ) |>
    # calculate mean & count per type & pollutant, retain type mean
    dplyr::summarise(
      {{ pollutant }} := mean(.data[[pollutant]], na.rm = TRUE),
      n = dplyr::n(),
      .by = dplyr::all_of(group_2)
    ) |>
    # needs specific arrangement for lattice
    dplyr::arrange(
      .data[[type]],
      .data$xleft,
      .data$xright,
      .data[[proportion]]
    ) |>
    # weighted mean, with cumulative sum for bar heights
    dplyr::mutate(
      weighted_mean = .data[[pollutant]] * n / sum(n),
      Var1 = tidyr::replace_na(.data$weighted_mean, 0),
      var2 = cumsum(.data$Var1),
      date = .data$xleft,
      .by = dplyr::all_of(group_1)
    )

  # normalise to 100 if needed
  if (normalise) {
    results <-
      dplyr::mutate(
        results,
        Var1 = .data$Var1 * (100 / sum(.data$Var1, na.rm = TRUE)),
        var2 = cumsum(.data$Var1),
        .by = dplyr::all_of(c(type, "date"))
      )
  }

  # make sure we know order of data frame for adding other dates
  results <- dplyr::arrange(results, .data[[type]], "date")

  # proper names of labelling #
  strip.dat <- strip.fun(results, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]

  # labelling on plot
  labs <- sapply(
    rev(levels(results[[proportion]])),
    function(x) quickText(x, auto.text)
  )

  # the few colours used for scaling
  nProp <- length(levels(results[[proportion]]))
  scaleCol <- openColours(cols, nProp)

  # add colours to the dataframe
  results <-
    dplyr::mutate(
      results,
      cols = scaleCol[as.integer(.data[[proportion]])]
    )

  # formula for lattice
  myform <- formula(paste("Var1 ~ date | ", type, sep = ""))

  # date axis formating
  breaks <- dateBreaks(results$date, date.breaks)
  dates <- breaks$major
  formats <- date.format %||% breaks$format
  scales <- list(x = list(at = dates, format = formats))

  # change in style if normalising data
  if (normalise) {
    ylab <- quickText(paste("% contribution to", pollutant), auto.text)
    pad <- 1
  } else {
    pad <- 1.04
  }

  # set limits, if not set by user
  xlim <- extra.args$xlim %||% range(c(results$xleft, results$xright))
  ylim <- extra.args$ylim %||% c(0, pad * max(results$var2, na.rm = TRUE))

  # set up key, if required
  if (key) {
    key <- list(
      rectangles = list(col = rev(scaleCol), border = NA),
      text = list(labs),
      space = key.position,
      title = quickText(key.title, auto.text),
      cex.title = 1,
      columns = key.columns
    )
  } else {
    key <- NULL
  }

  # construct plot
  plt <- xyplot(
    myform,
    data = results,
    as.table = TRUE,
    strip = strip,
    strip.left = strip.left,
    groups = get(proportion),
    stack = TRUE,
    sub = sub,
    scales = scales,
    col = scaleCol,
    border = NA,
    key = key,
    par.strip.text = list(cex = 0.8),
    ...,
    panel = function(..., col, subscripts) {
      panel.grid(-1, 0)
      panel.abline(v = dates, col = "grey95", ...)
      lapply(split(results[subscripts, ], results$date[subscripts]), panelBar)
    }
  )

  # update extra args; usual method does not seem to work
  plt <- modifyList(
    plt,
    list(
      ylab = ylab,
      xlab = xlab,
      x.limits = xlim,
      y.limits = ylim,
      main = main
    )
  )

  if (plot) {
    print(plt)
  }

  output <- list(
    plot = plt,
    data = results,
    call = match.call()
  )
  class(output) <- "openair"
  invisible(output)
}

#' plot individual rectangles as lattice panel.barchar is *very* slow
#' @noRd
panelBar <- function(dat) {
  xleft <- unclass(dat$xleft)
  ybottom <- lag(dat$var2, default = 0)
  xright <- unclass(dat$xright)
  ytop <- dat$var2

  lrect(
    xleft = xleft,
    ybottom = ybottom,
    xright = xright,
    ytop = ytop,
    fill = dat$cols,
    border = NA
  )
}
