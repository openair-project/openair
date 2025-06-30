#' Time series plot with categories shown as a stacked bar chart
#'
#' This function shows time series plots as stacked bar charts. The different
#' categories in the bar chart are made up from a character or factor variable
#' in a data frame. The function is primarily developed to support the plotting
#' of cluster analysis output from [polarCluster()] and
#' [trajCluster()] that consider local and regional (back trajectory)
#' cluster analysis respectively. However, the function has more general use for
#' understanding time series data.
#'
#' In order to plot time series in this way, some sort of time aggregation is
#' needed, which is controlled by the option `avg.time`.
#'
#' The plot shows the value of `pollutant` on the y-axis (averaged
#' according to `avg.time`). The time intervals are made up of bars split
#' according to `proportion`. The bars therefore show how the total value
#' of `pollutant` is made up for any time interval.
#'
#' @param mydata A data frame containing the fields `date`,
#'   `pollutant` and a splitting variable `proportion`
#' @param pollutant Name of the pollutant to plot contained in `mydata`.
#' @param proportion The splitting variable that makes up the bars in the bar
#'   chart e.g. `proportion = "cluster"` if the output from
#'   `polarCluster` is being analysed. If `proportion` is a numeric
#'   variable it is split into 4 quantiles (by default) by `cutData`. If
#'   `proportion` is a factor or character variable then the categories are
#'   used directly.
#' @param avg.time This defines the time period to average to. Can be
#'   \dQuote{sec}, \dQuote{min}, \dQuote{hour}, \dQuote{day}, \dQuote{DSTday},
#'   \dQuote{week}, \dQuote{month}, \dQuote{quarter} or \dQuote{year}. For much
#'   increased flexibility a number can precede these options followed by a
#'   space. For example, a timeAverage of 2 months would be `period = "2
#'   month"`.
#'
#'   Note that `avg.time` when used in `timeProp` should be greater
#'   than the time gap in the original data. For example, `avg.time =
#'   "day"` for hourly data is OK, but `avg.time = "hour"` for daily data
#'   is not.
#' @param type `type` determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce a single plot using the
#'   entire data. Type can be one of the built-in types as detailed in
#'   `cutData` e.g. "season", "year", "weekday" and so on. For example,
#'   `type = "season"` will produce four plots --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data
#'   frame. If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   `type` must be of length one.
#' @param normalise If `normalise = TRUE` then each time interval is scaled
#'   to 100. This is helpful to show the relative (percentage) contribution of
#'   the proportions.
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   `RColorBrewer` colours --- see the `openair` `openColours`
#'   function for more details. For user defined the user can supply a list of
#'   colour names recognised by R (type `colours()` to see the full list).
#'   An example would be `cols = c("yellow", "green", "blue")`
#' @param date.breaks Number of major x-axis intervals to use. The function will
#'   try and choose a sensible number of dates/times as well as formatting the
#'   date/time appropriately to the range being considered.  This does not
#'   always work as desired automatically. The user can therefore increase or
#'   decrease the number of intervals by adjusting the value of
#'   `date.breaks` up or down.
#' @param date.format This option controls the date format on the x-axis. While
#'   `timePlot` generally sets the date format sensibly there can be some
#'   situations where the user wishes to have more control. For format types see
#'   `strptime`. For example, to format the date like \dQuote{Jan-2012} set
#'   `date.format = "\%b-\%Y"`.
#' @param key.columns Number of columns to be used in the key. With many
#'   pollutants a single column can make to key too wide. The user can thus
#'   choose to use several columns by setting `columns` to be less than the
#'   number of pollutants.
#' @param key.position Location where the scale key is to plotted. Allowed
#'   arguments currently include \dQuote{top}, \dQuote{right}, \dQuote{bottom}
#'   and \dQuote{left}.
#' @param key.title The title of the key.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels etc. will automatically try and format pollutant
#'   names and units properly e.g.  by subscripting the `2' in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract plot components and plotting them in other ways.
#' @param ... Other graphical parameters passed onto `timeProp` and
#'   `cutData`. For example, `timeProp` passes the option
#'   `hemisphere = "southern"` on to `cutData` to provide southern
#'   (rather than default northern) hemisphere handling of `type =
#'   "season"`. Similarly, common axis and title labelling options (such as
#'   `xlab`, `ylab`, `main`) are passed to `xyplot` via
#'   `quickText` to handle routine formatting.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family time series and trend functions
#' @family cluster analysis functions
#' @examples
#' ## monthly plot of SO2 showing the contribution by wind sector
#' timeProp(mydata, pollutant = "so2", avg.time = "month", proportion = "wd")
timeProp <- function(
  mydata,
  pollutant = "nox",
  proportion = "cluster",
  avg.time = "day",
  type = "default",
  normalise = FALSE,
  cols = "Set1",
  date.breaks = 7,
  date.format = NULL,
  key.columns = 1,
  key.position = "right",
  key.title = proportion,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  ## keep check happy
  sums <- NULL
  freq <- NULL
  Var1 <- NULL
  means <- NULL
  date2 <- NULL
  mean_value <- weighted_mean <- xleft <- NULL

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  if (length(type) > 1) stop("'type' can only be of length 1.")

  ## if proportion is not categorical then make it so
  if (!class(mydata[[proportion]]) %in% c("factor")) {
    mydata <- cutData(mydata, proportion, ...)
  }

  ## extra.args setup
  extra.args <- list(...)

  ## set graphaics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  ## label controls

  main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  } else {
    quickText("", auto.text)
  }

  xlab <- if ("xlab" %in% names(extra.args)) {
    quickText(extra.args$xlab, auto.text)
  } else {
    "date"
  }

  ylab <- if ("ylab" %in% names(extra.args)) {
    quickText(extra.args$ylab, auto.text)
  } else {
    quickText(pollutant, auto.text)
  }

  xlim <- if ("xlim" %in% names(extra.args)) {
    extra.args$xlim
  } else {
    NULL
  }

  ylim <- if ("ylim" %in% names(extra.args)) {
    extra.args$ylim
  } else {
    NULL
  }

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  ## variables needed
  vars <- c("date", pollutant, proportion)

  if (any(type %in% dateTypes)) vars <- unique(c("date", vars))

  ## check the data
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # time zone of input data
  tzone <- attr(mydata$date, "tzone")

  # cut data
  mydata <- cutData(mydata, c(type, proportion))

  group_1 <- c("xleft", "xright", type)
  group_2 <- c(type, "xleft", "xright", proportion)

  # summarise by proportion, type etc
  # add the most common non-zero time interval

  results <- mydata %>%
    mutate(
      xleft = as.POSIXct(cut(date, avg.time), tz = tzone),
      xright = xleft + median(diff(xleft)[diff(xleft) != 0])
    ) %>%
    group_by(across(group_1)) %>% # group by type and date interval to get overall average
    mutate(mean_value = mean(.data[[pollutant]], na.rm = TRUE)) %>%
    group_by(across(group_2)) %>%
    summarise(
      {{ pollutant }} := mean(.data[[pollutant]], na.rm = TRUE),
      mean_value = mean(mean_value, na.rm = TRUE),
      n = length(date)
    ) %>%
    group_by(across(group_1)) %>%
    mutate(
      weighted_mean = .data[[pollutant]] * n / sum(n),
      Var1 = replace_na(weighted_mean, 0),
      var2 = cumsum(Var1),
      date = xleft
    )

  ## normlaise to 100 if needed
  vars <- c(type, "date")
  if (normalise) {
    results <- results %>%
      group_by(across(vars)) %>%
      mutate(
        Var1 = Var1 * (100 / sum(Var1, na.rm = TRUE)),
        var2 = cumsum(Var1)
      )
  }

  ## proper names of labelling ###################################################
  strip.dat <- strip.fun(results, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]

  ## work out width of each bar
  nProp <- length(levels(results[[proportion]]))

  # labelling on plot
  labs <- sapply(
    rev(levels(results[[proportion]])),
    function(x) quickText(x, auto.text)
  )

  # make sure we know order of data frame for adding other dates
  results <- arrange(results, type, "date")

  # xleft, xright used by plot function
  # results$xleft <- results$date
  # results$xright <- results$date2
  # ## don't need date2
  # results <- select(results, -date2)

  # the few colours used for scaling
  scaleCol <- openColours(cols, nProp)

  # levels of proportion
  thelevels <- levels(results[[proportion]])

  # add colour directly to data frame for easy reference
  cols <- data.frame(cols = scaleCol, stringsAsFactors = FALSE)
  cols[[proportion]] <- as.character(levels(results[[proportion]]))

  # need to merge based on character, not factor
  results[[proportion]] <- as.character(results[[proportion]])

  results <- full_join(results, cols, by = proportion)

  results[[proportion]] <- factor(results[[proportion]], levels = thelevels)

  # remove missing so we can do a cumsum
  #  results <- na.omit(results)

  # y values for plotting rectangles
  # results <- results %>%
  #   group_by(across(vars)) %>%
  #   mutate(var2 = cumsum(Var1))

  myform <- formula(paste("Var1 ~ date | ", type, sep = ""))

  dates <- dateBreaks(results$date, date.breaks)$major ## for date scale

  ## date axis formating
  if (is.null(date.format)) {
    formats <- dateBreaks(results$date, date.breaks)$format
  } else {
    formats <- date.format
  }

  scales <- list(x = list(at = dates, format = formats))

  y.max <- max(results$var2, na.rm = TRUE)

  if (is.null(xlim)) xlim <- range(c(results$xleft, results$xright))

  if (normalise) pad <- 1 else pad <- 1.04
  if (is.null(ylim)) ylim <- c(0, pad * y.max)

  if (normalise) {
    ylab <- quickText(paste("% contribution to", pollutant), auto.text)
  }

  ## sub heading

  sub <- "contribution weighted by mean"

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
    key = list(
      rectangles = list(col = rev(scaleCol), border = NA),
      text = list(labs),
      space = key.position,
      title = quickText(key.title, auto.text),
      cex.title = 1,
      columns = key.columns
    ),
    par.strip.text = list(cex = 0.8),
    ...,
    panel = function(..., col, subscripts) {
      panel.grid(-1, 0)
      panel.abline(v = dates, col = "grey95", ...)
      split(results[subscripts, ], results$date[subscripts]) %>%
        lapply(., panelBar)
    }
  )

  ## update extra args; usual method does not seem to work...
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

  if (plot) print(plt)

  output <- list(
    plot = plt,
    data = results,
    call = match.call()
  )
  class(output) <- "openair"
  invisible(output)
}

# plot individual rectangles as lattice panel.barchar is *very* slow

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
