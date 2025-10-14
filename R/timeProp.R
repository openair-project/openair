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

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  # extra.args setup
  extra.args <- list(...)

  # set graphaics
  current.font <- trellis.par.get("fontsize")

  # reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # label controls
  main <- quickText(extra.args$main %||% "", auto.text)
  xlab <- quickText(extra.args$xlab %||% "date", auto.text)
  ylab <- quickText(extra.args$ylab %||% pollutant, auto.text)

  xlim <- extra.args$xlim %||% NULL
  ylim <- extra.args$ylim %||% NULL

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
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
  # add the most common non-zero time interval

  results <- mydata |>
    dplyr::mutate(
      xleft = as.POSIXct(cut(.data$date, avg.time), tz = tzone),
      xright = .data$xleft + median(diff(.data$xleft)[diff(.data$xleft) != 0])
    ) |>
    dplyr::group_by(dplyr::across(group_1)) |> # group by type and date interval to get overall average
    dplyr::mutate(mean_value = mean(.data[[pollutant]], na.rm = TRUE)) |>
    dplyr::group_by(dplyr::across(group_2)) |>
    dplyr::summarise(
      {{ pollutant }} := mean(.data[[pollutant]], na.rm = TRUE),
      mean_value = mean(.data$mean_value, na.rm = TRUE),
      n = length(.data$date)
    ) |>
    dplyr::group_by(dplyr::across(group_1)) |>
    dplyr::mutate(
      weighted_mean = .data[[pollutant]] * n / sum(n),
      Var1 = tidyr::replace_na(.data$weighted_mean, 0),
      var2 = cumsum(.data$Var1),
      date = .data$xleft
    )

  # normlaise to 100 if needed
  vars <- c(type, "date")
  if (normalise) {
    results <- results |>
      dplyr::group_by(dplyr::across(vars)) |>
      dplyr::mutate(
        Var1 = .data$Var1 * (100 / sum(.data$Var1, na.rm = TRUE)),
        var2 = cumsum(.data$Var1)
      )
  }

  # proper names of labelling #
  strip.dat <- strip.fun(results, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]

  # work out width of each bar
  nProp <- length(levels(results[[proportion]]))

  # labelling on plot
  labs <- sapply(
    rev(levels(results[[proportion]])),
    function(x) quickText(x, auto.text)
  )

  # make sure we know order of data frame for adding other dates
  results <- dplyr::arrange(results, type, "date")

  # the few colours used for scaling
  scaleCol <- openColours(cols, nProp)

  # levels of proportion
  thelevels <- levels(results[[proportion]])

  # add colour directly to data frame for easy reference
  cols <- data.frame(cols = scaleCol, stringsAsFactors = FALSE)
  cols[[proportion]] <- as.character(levels(results[[proportion]]))

  # need to merge based on character, not factor
  results[[proportion]] <- as.character(results[[proportion]])

  results <- dplyr::full_join(results, cols, by = proportion)

  results[[proportion]] <- factor(results[[proportion]], levels = thelevels)

  myform <- formula(paste("Var1 ~ date | ", type, sep = ""))

  dates <- dateBreaks(results$date, date.breaks)$major # for date scale

  # date axis formating
  if (is.null(date.format)) {
    formats <- dateBreaks(results$date, date.breaks)$format
  } else {
    formats <- date.format
  }

  scales <- list(x = list(at = dates, format = formats))

  y.max <- max(results$var2, na.rm = TRUE)

  if (is.null(xlim)) {
    xlim <- range(c(results$xleft, results$xright))
  }

  if (normalise) {
    pad <- 1
  } else {
    pad <- 1.04
  }
  if (is.null(ylim)) {
    ylim <- c(0, pad * y.max)
  }

  if (normalise) {
    ylab <- quickText(paste("% contribution to", pollutant), auto.text)
  }

  # sub heading
  sub <- "contribution weighted by mean"

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
