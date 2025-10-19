#' Diurnal, day of the week and monthly variation
#'
#' Plots the diurnal, day of the week and monthly variation for different
#' variables, typically pollutant concentrations. Four separate plots are
#' produced.
#'
#' The variation of pollutant concentrations by hour of the day and day of the
#' week etc. can reveal many interesting features that relate to source types
#' and meteorology. For traffic sources, there are often important differences
#' in the way vehicles vary by vehicles type e.g. less heavy vehicles at
#' weekends.
#'
#' The [timeVariation()] function makes it easy to see how concentrations (and
#' many other variable types) vary by hour of the day and day of the week.
#'
#' The plots also show the 95\% confidence intervals in the mean. The 95\%
#' confidence intervals in the mean are calculated through bootstrap
#' simulations, which will provide more robust estimates of the confidence
#' intervals (particularly when there are relatively few data).
#'
#' The function can handle multiple pollutants and uses the flexible `type`
#' option to provide separate panels for each 'type' --- see [cutData()] for
#' more details. [timeVariation()] can also accept a `group` option which is
#' useful if data are stacked. This will work in a similar way to having
#' multiple pollutants in separate columns.
#'
#' The user can supply their own `ylim` e.g. `ylim = c(0, 200)` that will be
#' used for all plots. `ylim` can also be a list of length four to control the
#' y-limits on each individual plot e.g. `ylim = list(c(-100,500), c(200, 300),
#' c(-400,400), c(50,70))`. These pairs correspond to the hour, weekday, month
#' and day-hour plots respectively.
#'
#' The option `difference` will calculate the difference in means of two
#' pollutants together with bootstrap estimates of the 95\% confidence intervals
#' in the difference in the mean. This works in two ways: either two pollutants
#' are supplied in separate columns e.g. `pollutant = c("no2", "o3")`, or there
#' are two unique values of `group`. The difference is calculated as the second
#' pollutant minus the first and is labelled as such. Considering differences in
#' this way can provide many useful insights and is particularly useful for
#' model evaluation when information is needed about where a model differs from
#' observations by many different time scales. The manual contains various
#' examples of using `difference = TRUE`.
#'
#' Note also that the [timeVariation()] function works well on a subset of data
#' and in conjunction with other plots. For example, a [polarPlot()] may
#' highlight an interesting feature for a particular wind speed/direction range.
#' By filtering for those conditions [timeVariation()] can help determine
#' whether the temporal variation of that feature differs from other features
#' --- and help with source identification.
#'
#' In addition, [timeVariation()] will work well with other variables if
#' available. Examples include meteorological and traffic flow data.
#'
#' Depending on the choice of statistic, a subheading is added. Users can
#' control the text in the subheading through the use of `sub` e.g. `sub = ""`
#' will remove any subheading.
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
#' @param normalise Should variables be normalised? The default is `FALSE`. If
#'   `TRUE` then the variable(s) are divided by their mean values. This helps to
#'   compare the shape of the diurnal trends for variables on very different
#'   scales.
#' @param xlab x-axis label; one for each sub-plot.
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
#'   Only one `type` is allowed in`timeVariation`.
#' @param group This sets the grouping variable to be used. For example, if a
#'   data frame had a column `site` setting `group = "site"` will plot all sites
#'   together in each panel. See examples below.
#' @param difference If two pollutants are chosen then setting `difference =
#'   TRUE` will also plot the difference in means between the two variables as
#'   `pollutant[2] - pollutant[1]`. Bootstrap 95\% confidence intervals of the
#'   difference in means are also calculated. A horizontal dashed line is shown
#'   at y = 0. The difference can also be calculated if there is a column that
#'   identifies two groups e.g. having used `splitByDate`. In this case it is
#'   possible to call `timeVariation` with the option `group = "split.by"` and
#'   `difference = TRUE`.
#' @param statistic Can be `"mean"` (default) or `"median"`. If the statistic is
#'   `"mean"` then the mean line and the 95% confidence interval in the mean are
#'   plotted by default. If the statistic is `"median"` then the median line is
#'   plotted together with the 5/95 and 25/75th quantiles are plotted. Users can
#'   control the confidence intervals with `conf.int`.
#' @param conf.int The confidence intervals to be plotted. If `statistic =
#'   "mean"` then the confidence intervals in the mean are plotted. If
#'   `statistic = "median"` then the `conf.int` and `1 - conf.int` *quantiles*
#'   are plotted. `conf.int` can be of length 2, which is most useful for
#'   showing quantiles. For example `conf.int = c(0.75, 0.99)` will yield a plot
#'   showing the median, 25/75 and 5/95th quantiles.
#' @param B Number of bootstrap replicates to use. Can be useful to reduce this
#'   value when there are a large number of observations available to increase
#'   the speed of the calculations without affecting the 95\% confidence
#'   interval calculations by much.
#' @param ci Should confidence intervals be shown? The default is `TRUE`.
#'   Setting this to `FALSE` can be useful if multiple pollutants are chosen
#'   where over-lapping confidence intervals can over complicate plots.
#' @param key By default `timeVariation` produces four plots on one page. While
#'   it is useful to see these plots together, it is sometimes necessary just to
#'   use one for a report. If `key` is `TRUE`, a key is added to all plots
#'   allowing the extraction of a single plot *with* key. See below for an
#'   example.
#' @param start.day What day of the week should the plots start on? The user can
#'   change the start day by supplying an integer between 0 and 6. Sunday = 0,
#'   Monday = 1, \ldots For example to start the weekday plots on a Saturday,
#'   choose `start.day = 6`.
#' @param panel.gap The gap between panels in the hour-day plot.
#' @param alpha The alpha transparency used for plotting confidence intervals. 0
#'   is fully transparent and 1 is opaque. The default is 0.4
#' @param month.last Should the order of the plots be changed so the plot
#'   showing monthly means be the last plot for a logical hierarchy of averaging
#'   periods?
#' @param ... Other graphical parameters passed onto [lattice::xyplot()] and
#'   [cutData()]. For example, in the case of [cutData()] the option `hemisphere
#'   = "southern"`.
#'
#' @import lattice
#' @export
#' @return an [openair][openair-package] object. The four components of
#'   timeVariation are: `day.hour`, `hour`, `day` and `month`. Associated
#'   data.frames can be extracted directly using the `subset` option, e.g. as in
#'   `plot(object, subset = "day.hour")`, `summary(output, subset = "hour")`,
#'   etc., for `output <- timeVariation(mydata, "nox")`
#' @author David Carslaw
#' @family time series and trend functions
#' @examples
#'
#'
#' # basic use
#' timeVariation(mydata, pollutant = "nox")
#'
#' # for a subset of conditions
#' \dontrun{
#' timeVariation(subset(mydata, ws > 3 & wd > 100 & wd < 270),
#'   pollutant = "pm10", ylab = "pm10 (ug/m3)"
#' )
#' }
#'
#' # multiple pollutants with concentrations normalised
#' \dontrun{
#' timeVariation(mydata, pollutant = c("nox", "co"), normalise = TRUE)
#' }
#'
#' # show BST/GMT variation (see ?cutData for more details)
#' # the NOx plot shows the profiles are very similar when expressed in
#' # local time, showing that the profile is dominated by a local source
#' # that varies by local time and not by GMT i.e. road vehicle emissions
#'
#' \dontrun{
#' timeVariation(mydata, pollutant = "nox", type = "dst", local.tz = "Europe/London")
#' }
#'
#' # In this case it is better to group the results for clarity:
#' \dontrun{
#' timeVariation(mydata, pollutant = "nox", group = "dst", local.tz = "Europe/London")
#' }
#'
#' # By contrast, a variable such as wind speed shows a clear shift when
#' #  expressed in local time. These two plots can help show whether the
#' #  variation is dominated by man-made influences or natural processes
#'
#' \dontrun{
#' timeVariation(mydata, pollutant = "ws", group = "dst", local.tz = "Europe/London")
#' }
#'
#' # It is also possible to plot several variables and set type. For
#' # example, consider the NOx and NO2 split by levels of O3:
#'
#' \dontrun{
#' timeVariation(mydata, pollutant = c("nox", "no2"), type = "o3", normalise = TRUE)
#' }
#'
#' # difference in concentrations
#' \dontrun{
#' timeVariation(mydata, poll = c("pm25", "pm10"), difference = TRUE)
#' }
#'
#' # It is also useful to consider how concentrations vary by
#' # considering two different periods e.g. in intervention
#' # analysis. In the following plot NO2 has clearly increased but much
#' # less so at weekends - perhaps suggesting vehicles other than cars
#' # are important because flows of cars are approximately invariant by
#' # day of the week
#'
#' \dontrun{
#' mydata <- splitByDate(mydata, dates = "1/1/2003", labels = c("before Jan. 2003", "After Jan. 2003"))
#' timeVariation(mydata, pollutant = "no2", group = "split.by", difference = TRUE)
#' }
#'
#' # sub plots can be extracted from the openair object
#' \dontrun{
#' myplot <- timeVariation(mydata, pollutant = "no2")
#' plot(myplot, subset = "day.hour") # top weekday and plot
#' }
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
#' \dontrun{
#' timeVariation(mydata, stati = "median", poll = "pm10", col = "firebrick")
#'
#' # with different intervals
#' timeVariation(mydata,
#'   stati = "median", poll = "pm10", conf.int = c(0.75, 0.99),
#'   col = "firebrick"
#' )
#' }
#'
timeVariation <- function(
  mydata,
  pollutant = "nox",
  local.tz = NULL,
  normalise = FALSE,
  xlab = c(
    "hour",
    "hour",
    "month",
    "weekday"
  ),
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
  key.columns = 1,
  start.day = 1,
  panel.gap = 0.2,
  auto.text = TRUE,
  alpha = 0.4,
  month.last = FALSE,
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

  # extra.args setup
  extra.args <- list(...)

  # label controls
  # xlab handled in formals and code because unique
  extra.args$ylab <- quickText(
    extra.args$ylab %||%
      ifelse(normalise, "normalised level", paste(pollutant, collapse = ", ")),
    auto.text
  )
  extra.args$main <- quickText(extra.args$main %||% "", auto.text)

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  if (statistic == "median" && missing(conf.int)) {
    conf.int <- c(0.75, 0.95)
  }

  # sub heading stat info
  if (statistic == "mean") {
    sub.text <- paste(
      "mean and ",
      100 * conf.int[1],
      "% confidence interval in mean",
      sep = ""
    )
  } else {
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

  extra.args$sub <- quickText(extra.args$sub %||% sub.text, auto.text)

  extra.args$lwd <- extra.args$lwd %||% 2

  ylim.handler <- !("ylim" %in% names(extra.args))

  # if user supplies separate ylims for each plot
  ylimList <- FALSE

  if ("ylim" %in% names(extra.args)) {
    if (is.list(extra.args$ylim)) {
      if (length(extra.args$ylim) != 4) {
        stop("ylim should be a list of 4")
      }
      ylim.list <- extra.args$ylim
      ylimList <- TRUE
    }
  }

  vars <- c("date", pollutant)

  #  various check to make sure all the data are available #

  if (!missing(group) & length(pollutant) > 1) {
    stop(
      "Can only have one pollutant with a grouping variable, or several pollutants and no grouping variable."
    )
  }

  # only one type for now
  if (length(type) > 1) {
    stop("Can only have one type for timeVariation.")
  }

  if (type %in% pollutant) {
    stop("Cannot have type the same as a pollutant name.")
  }

  if (!missing(group)) {
    if (group %in% pollutant) {
      stop("Cannot have group the same as a pollutant name.")
    }
  }

  # if differences between two pollutants are calculated
  if (difference) {
    if (missing(group)) {
      if (length(pollutant) != 2) {
        stop("Need to specify two pollutants to calculate their difference.")
      }
    }

    if (!missing(group)) {
      if (length(unique(na.omit(mydata[[group]]))) != 2) {
        stop("Need to specify two pollutants to calculate their difference.")
      }
    }
  }

  # statistic check
  if (!statistic %in% c("mean", "median")) {
    statistic <- "mean"
    warning("statistic should be 'mean' or 'median',  setting it to 'mean'.")
  }
  # #

  # check to see if type = "variable" (word used in code, so change)
  if (type == "variable") {
    mydata <- rename(mydata, c(variable = "tempVar"))
    type <- "tempVar"
  }

  # if group is present, need to add that list of variables
  if (!missing(group)) {
    if (group %in% dateTypes) {
      vars <- unique(c(vars, "date"))
    } else {
      vars <- unique(c(vars, group))
    }
  }

  # data checks
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  if (!missing(group)) {
    mydata <- cutData(mydata, group, local.tz = local.tz, ...)
  }
  mydata <- cutData(mydata, type, local.tz = local.tz, ...)

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
  if (difference && missing(group)) {
    pollutant <- c(pollutant, paste(pollutant[2], "-", pollutant[1]))
  }

  if (missing(name.pol)) {
    mylab <- sapply(seq_along(pollutant), function(x) {
      quickText(pollutant[x], auto.text)
    })
  }

  if (!missing(name.pol)) {
    mylab <- sapply(seq_along(name.pol), function(x) {
      quickText(name.pol[x], auto.text)
    })
  }

  if (missing(group)) {
    mydata <- gather(mydata, key = variable, value = value, poll.orig)
    mydata$variable <- factor(mydata$variable, levels = pollutant) # drop unused factor levels
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

  # function to normalise
  divide.by.mean <- function(x) {
    Mean <- mean(x$Mean, na.rm = TRUE)
    x$Mean <- x$Mean / Mean
    x$Lower <- x$Lower / Mean
    x$Upper <- x$Upper / Mean
    x
  }

  # get days in right order
  days <- format(ISOdate(2000, 1, 2:8), "%A")
  days.abb <- format(ISOdate(2000, 1, 2:8), "%a")
  if (start.day < 0 || start.day > 6) {
    stop("start.day must be between 0 and 6.")
  }

  if (start.day > 0) {
    day.ord <- c(days[(1 + start.day):7], days[1:(1 + start.day - 1)])
    day.ord.abb <- c(
      days.abb[(1 + start.day):7],
      days.abb[1:(1 + start.day - 1)]
    )
  } else {
    day.ord <- days
    day.ord.abb <- days.abb
  }

  # calculate temporal components
  mydata <- mutate(
    mydata,
    wkday = wday(date, label = TRUE, abbr = FALSE),
    wkday = ordered(wkday, levels = day.ord),
    hour = hour(date),
    mnth = month(date)
  )

  # y range taking account of expanded uncertainties
  rng <- function(x) {
    # if no CI information, just return
    if (all(is.na(x[, c("Lower", "Upper")]))) {
      lims <- NULL
      return(lims)
    }

    if (ci) {
      lims <- range(c(x$Lower, x$Upper), na.rm = TRUE)
      inc <- 0.04 * abs(lims[2] - lims[1])
      lims <- c(lims[1] - inc, lims[2] + inc)
    } else {
      lims <- range(c(x$Mean, x$Mean), na.rm = TRUE)
      inc <- 0.04 * abs(lims[2] - lims[1])
      lims <- c(lims[1] - inc, lims[2] + inc)
      if (diff(lims) == 0) lims <- NULL
    }
    lims
  }

  npol <- length(levels(mydata$variable)) # number of pollutants

  if (difference) {
    npol <- 3 # 3 pollutants if difference considered
    if (missing(group)) {
      poll1 <- pollutant[1]
    }
    poll2 <- pollutant[2]
    if (!missing(group)) {
      poll1 <- levels(mydata$variable)[1]
    }
    poll2 <- levels(mydata$variable)[2]
  }

  # number of columns for key
  if (missing(key.columns)) {
    key.columns <- npol
  }

  myColors <- openColours(cols, npol)

  # for individual plot keys - useful if only one of the plots is extracted after printing
  if (!is.null(key)) {
    key <- list(
      rectangles = list(col = myColors[1:npol], border = NA),
      title = "",
      text = list(lab = mylab),
      space = "bottom",
      columns = key.columns,
      lines.title = 1
    )

    extra.args$main <- overall.main
  }

  # data frame of confidence intervals
  conf_int <- data.frame(ci = conf.int)

  # hour #

  if (difference) {
    data.hour <- errorDiff(
      mydata,
      vars = "hour",
      type = type,
      poll1 = poll1,
      poll2 = poll2,
      B = B,
      conf.int = conf.int
    )
  } else {
    data.hour <-
      purrr::map(
        .x = conf_int$ci,
        .f = function(x) {
          proc(
            x,
            mydata,
            vars = "hour",
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

  if (normalise) {
    data.hour <- mapType(
      data.hour,
      type = "variable",
      fun = divide.by.mean
    )
  }

  if (is.null(xlab[2]) | is.na(xlab[2])) {
    xlab[2] <- "hour"
  }

  # proper names of labelling #
  if (type != "default") {
    stripName <- sapply(
      levels(factor(data.hour[[type]])),
      function(x) quickText(x, auto.text)
    )
    strip <- strip.custom(factor.levels = stripName)
  } else {
    strip <- FALSE
  }
  # #

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Mean ~ hour | ", temp, sep = ""))

  # ylim hander
  if (ylim.handler) {
    extra.args$ylim <- rng(data.hour)
  }

  # user supplied separate ylim
  if (ylimList) {
    extra.args$ylim <- ylim.list[[1]]
  }

  # plot
  xy.args <- list(
    x = myform,
    data = data.hour,
    groups = data.hour$variable,
    as.table = TRUE,
    xlab = xlab[2],
    xlim = c(0, 23),
    strip = strip,
    par.strip.text = list(cex = 0.8),
    key = key,
    scales = list(x = list(at = c(0, 6, 12, 18, 23))),
    par.settings = simpleTheme(col = myColors),
    panel = function(x, y, ...) {
      panel.grid(-1, 0)
      panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
      panel.superpose(
        x,
        y,
        ...,
        panel.groups = function(
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
          # plot once
          id <- which(data.hour$ci[subscripts] == data.hour$ci[1])
          panel.xyplot(
            x[id],
            y[id],
            type = "l",
            col.line = myColors[group.number],
            ...
          )

          if (ci) {
            mkpoly(
              data.hour[subscripts, ],
              x = "hour",
              y = "Mean",
              group.number,
              myColors,
              alpha
            )
          }
          # reference line(s)
          if (!is.null(ref.y)) do.call(panel.abline, ref.y)
        }
      )
    }
  )

  # reset for extra.args
  xy.args <- listUpdate(xy.args, extra.args)

  # plot
  hour <- do.call(xyplot, xy.args)

  # weekday #

  if (difference) {
    data.weekday <- errorDiff(
      mydata,
      vars = "wkday",
      type = type,
      poll1 = poll1,
      poll2 = poll2,
      B = B,
      conf.int = conf.int
    )
  } else {
    data.weekday <-
      purrr::map(
        .x = conf_int$ci,
        .f = function(x) {
          proc(
            x,
            mydata,
            vars = "wkday",
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

  if (normalise) {
    data.weekday <- mapType(
      data.weekday,
      type = "variable",
      fun = divide.by.mean
    )
  }

  data.weekday$wkday <- ordered(data.weekday$wkday, levels = day.ord)

  data.weekday$wkday <- as.numeric(as.factor(data.weekday$wkday))

  if (is.null(xlab[4]) | is.na(xlab[4])) {
    xlab[4] <- "weekday"
  }

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Mean ~ wkday | ", temp, sep = ""))

  # ylim hander
  if (ylim.handler) {
    extra.args$ylim <- rng(data.weekday)
  }

  # user supplied separate ylim
  if (ylimList) {
    extra.args$ylim <- ylim.list[[2]]
  }

  # plot
  xy.args <- list(
    x = myform,
    data = data.weekday,
    groups = data.weekday$variable,
    as.table = TRUE,
    par.settings = simpleTheme(col = myColors, pch = 16),
    scales = list(x = list(at = 1:7, labels = day.ord.abb)),
    xlab = xlab[4],
    strip = strip,
    par.strip.text = list(cex = 0.8),
    key = key,
    panel = function(x, y, ...) {
      panel.grid(-1, 0)
      panel.abline(v = 1:7, col = "grey85")
      panel.superpose(
        x,
        y,
        ...,
        panel.groups = function(
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

          # only plot median once if 2 conf.int
          id <- which(data.weekday$ci[subscripts] == data.weekday$ci[1])
          panel.xyplot(
            x[id],
            y[id],
            type = "l",
            col.line = myColors[group.number],
            ...
          )

          if (ci) {
            mkrect(
              data.weekday[subscripts, ],
              x = "wkday",
              y = "Mean",
              group.number,
              myColors,
              alpha
            )
          }
          # refrence line(s)
          if (!is.null(ref.y)) do.call(panel.abline, ref.y)
        }
      )
    }
  )
  # reset for extra.args
  xy.args <- listUpdate(xy.args, extra.args)

  # plot
  day <- do.call(xyplot, xy.args)

  # month #

  if (difference) {
    data.month <- errorDiff(
      mydata,
      vars = "mnth",
      type = type,
      poll1 = poll1,
      poll2 = poll2,
      B = B,
      conf.int = conf.int
    )
  } else {
    data.month <-
      purrr::map(
        .x = conf_int$ci,
        .f = function(x) {
          proc(
            x,
            mydata,
            vars = "mnth",
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

  if (normalise) {
    data.month <- mapType(
      data.month,
      type = "variable",
      fun = divide.by.mean
    )
  }

  if (is.null(xlab[3]) | is.na(xlab[3])) {
    xlab[3] <- "month"
  }

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Mean ~ mnth | ", temp, sep = ""))

  # ylim hander
  if (ylim.handler) {
    extra.args$ylim <- rng(data.month)
  }

  # user supplied separate ylim
  if (ylimList) {
    extra.args$ylim <- ylim.list[[3]]
  }

  # plot
  xy.args <-
    list(
      x = myform,
      data = data.month,
      groups = data.month$variable,
      as.table = TRUE,
      xlab = xlab[3],
      xlim = c(0.5, 12.5),
      key = key,
      strip = strip,
      par.strip.text = list(cex = 0.8),
      par.settings = simpleTheme(col = myColors, pch = 16),
      scales = list(
        x = list(
          at = 1:12,
          labels = substr(
            format(
              seq(
                as_date("2000-01-01"),
                as_date("2000-12-31"),
                "month"
              ),
              "%B"
            ),
            1,
            1
          )
        )
      ),
      panel = function(x, y, ...) {
        panel.grid(-1, 0)
        panel.abline(v = 1:12, col = "grey85")
        panel.superpose(
          x,
          y,
          ...,
          panel.groups = function(
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

            # a line won't work for a single point
            pltType <- "l"
            if (length(subscripts) == 1) {
              pltType <- "p"
            }

            # only plot median once if 2 conf.int
            id <- which(data.month$ci[subscripts] == data.month$ci[1])

            # lines not needed if split by season
            if (group != "season" || is.null(group)) {
              panel.xyplot(
                x[id],
                y[id],
                type = pltType,
                col.line = myColors[group.number],
                ...
              )
            }

            if (ci) {
              mkrect(
                data.month[subscripts, ],
                x = "mnth",
                y = "Mean",
                group.number,
                myColors,
                alpha
              )
            }
            # refrence line(s)
            if (!is.null(ref.y)) do.call(panel.abline, ref.y)
          }
        )
      }
    )

  # reset for extra.args
  xy.args <- listUpdate(xy.args, extra.args)

  # plot
  month <- do.call(xyplot, xy.args)

  # day and hour #

  if (difference) {
    data.day.hour <- errorDiff(
      mydata,
      vars = "day.hour",
      type = type,
      poll1 = poll1,
      poll2 = poll2,
      B = B,
      conf.int = conf.int
    )
  } else {
    data.day.hour <-
      purrr::map(
        .x = conf_int$ci,
        .f = function(x) {
          proc(
            x,
            mydata,
            vars = "day.hour",
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

  if (normalise) {
    data.day.hour <- mapType(
      data.day.hour,
      type = "variable",
      fun = divide.by.mean
    )
  }

  ids <- which(is.na(data.day.hour$Lower)) # missing Lower ci, set to mean

  data.day.hour$Lower[ids] <- data.day.hour$Mean[ids]
  ids <- which(is.na(data.day.hour$Upper)) # missing Upper ci, set to mean
  data.day.hour$Upper[ids] <- data.day.hour$Mean[ids]

  if (is.null(xlab[1]) | is.na(xlab[1])) {
    xlab[1] <- "hour"
  }

  # proper names of labelling #

  strip <- strip.custom(par.strip.text = list(cex = 0.8))

  if (type == "default") {
    strip.left <- FALSE
    layout <- c(length(unique(mydata$wkday)), 1)
  } else {
    # two conditioning variables

    stripName <- sapply(
      levels(factor(data.day.hour[[type]])),
      function(x) quickText(x, auto.text)
    )
    strip.left <- strip.custom(factor.levels = stripName)
    layout <- NULL
  }
  # #

  temp <- paste(type, collapse = "+")
  if (type == "default") {
    myform <- formula("Mean ~ hour | wkday")
  } else {
    myform <- formula(paste("Mean ~ hour | wkday *", temp, sep = ""))
  }

  # ylim hander
  if (ylim.handler) {
    extra.args$ylim <- rng(data.day.hour)
  }

  # user supplied separate ylim
  if (ylimList) {
    extra.args$ylim <- ylim.list[[4]]
  }

  # plot
  xy.args <- list(
    x = myform,
    data = data.day.hour,
    groups = data.day.hour$variable,
    as.table = TRUE,
    xlim = c(0, 23),
    xlab = xlab[1],
    layout = layout,
    par.settings = simpleTheme(col = myColors),
    between = list(x = panel.gap),
    scales = list(x = list(at = c(0, 6, 12, 18, 23))),
    key = key,
    strip = strip,
    strip.left = strip.left,
    par.strip.text = list(cex = 0.8),
    panel = function(x, y, ...) {
      panel.grid(-1, 0)
      panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
      panel.superpose(
        x,
        y,
        ...,
        panel.groups = function(
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

          # only plot median once if 2 conf.int
          id <- which(data.day.hour$ci[subscripts] == data.day.hour$ci[1])

          panel.xyplot(
            x[id],
            y[id],
            type = "l",
            col.line = myColors[group.number],
            ...
          )

          if (ci) {
            mkpoly(
              data.day.hour[subscripts, ],
              x = "hour",
              y = "Mean",
              group.number,
              myColors,
              alpha
            )
          }
          # refrence line(s)
          if (!is.null(ref.y)) do.call(panel.abline, ref.y)
        }
      )
    }
  )

  # reset for extra.args
  xy.args <- listUpdate(xy.args, extra.args)

  # plot
  day.hour <- do.call(xyplot, xy.args)

  subsets <- c("day.hour", "hour", "day", "month")

  # this adjusts the space for the title to 2 lines (approx) if \n in title
  if (length(grep("atop", overall.main) == 1)) {
    y.upp <- 0.95
    y.dwn <- 0.05
  } else {
    y.upp <- 0.975
    y.dwn <- 0.025
  }

  main.plot <- function(...) {
    if (type == "default") {
      print(
        update(
          day.hour,
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
          useOuterStrips(day.hour, strip = strip, strip.left = strip.left),
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

    # Build the plot panels in different orders
    if (!month.last) {
      # The original plot orders
      print(hour, position = c(0, y.dwn, 0.33, 0.53), more = TRUE)
      print(month, position = c(0.33, y.dwn, 0.66, 0.53), more = TRUE)
      print(day, position = c(0.66, y.dwn, 1, 0.53))
    } else {
      # Move around the plot order so they follow a logical hierarchy of averaging
      # periods hour-day-month
      print(hour, position = c(0, y.dwn, 0.33, 0.53), more = TRUE)
      print(day, position = c(0.33, y.dwn, 0.66, 0.53), more = TRUE)
      print(month, position = c(0.66, y.dwn, 1, 0.53))
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
    plot = list(day.hour, hour, day, month, subsets = subsets),
    data = list(
      data.day.hour,
      data.hour,
      data.weekday,
      data.month,
      subsets = subsets
    ),
    call = match.call(),
    main.plot = main.plot,
    ind.plot = ind.plot
  )
  names(output$data)[1:4] <- subsets
  names(output$plot)[1:4] <- subsets
  class(output) <- "openair"

  invisible(output)
}

proc <- function(
  conf.int = conf.int,
  mydata,
  vars = "day.hour",
  pollutant,
  type,
  B = B,
  statistic = statistic
) {
  # get rid of R check annoyances
  variable <- value <- NULL

  if (statistic == "mean") {
    stat <- bootMean
  } else {
    stat <- median_hilow
  }

  summary.values <- function(
    conf.int = conf.int,
    mydata,
    vars = vars,
    FUN,
    type = type,
    B = B,
    statistic = statistic
  ) {
    if (vars == "hour") {
      myform <- formula(paste("value ~ variable + hour +", type))
    }

    if (vars == "day.hour") {
      myform <- formula(paste("value ~ variable + wkday + hour +", type))
    }

    if (vars == "wkday") {
      myform <- formula(paste("value ~ variable + wkday +", type))
    }

    if (vars == "mnth") {
      myform <- formula(paste("value ~ variable + mnth +", type))
    }

    mydata <- aggregate(
      myform,
      data = mydata,
      FUN,
      B = B,
      statistic = statistic,
      conf.int = conf.int
    )
    mydata
  }

  # function to calculate statistics dealing with wd properly
  if (any(!pollutant %in% "wd")) {
    data1 <- subset(mydata, variable != "wd")
    data1 <- summary.values(
      data1,
      vars,
      stat,
      type,
      B = B,
      statistic = statistic,
      conf.int = conf.int
    )
    data1 <- data.frame(subset(data1, select = -value), data1$value)
  }

  if ("wd" %in% pollutant) {
    if (length(pollutant) > 1) {
      mydata <- subset(mydata, variable == "wd")
    }
    data2 <- summary.values(
      conf.int,
      mydata,
      vars,
      wd.smean.normal,
      type,
      B = B,
      statistic = statistic
    )
    data2 <- data.frame(subset(data2, select = -value), data2$value)
  }

  if (length(pollutant) > 1 & "wd" %in% pollutant) {
    data2 <- bind_rows(data1, data2)
  }

  if (!"wd" %in% pollutant) {
    data2 <- data1
  }

  if (length(pollutant) == 1 & "wd" %in% pollutant) {
    data2 <- data2
  }

  data2$ci <- conf.int
  data2
}

wd.smean.normal <- function(wd, B = B, statistic, conf.int) {
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
    intervals <- median_hilow(wd.diff, conf.int)
  }
  Lower <- intervals[2]
  names(Lower) <- NULL

  Upper <- intervals[3]
  names(Upper) <- NULL
  diff.wd <- (Upper - Lower) / 2

  c(Mean = Mean, Lower = Mean - diff.wd, Upper = Mean + diff.wd)
}

errorDiff <- function(
  mydata,
  vars = "day.hour",
  poll1,
  poll2,
  type,
  B = B,
  conf.int = conf.int
) {
  # bootstrap mean difference confidence intervals
  # rearrange data

  # it could be dates duplicate e.g. run function over several sites

  if (anyDuplicated(mydata$date) > 0) {
    mydata$rowid <- 1:nrow(mydata)
  }

  mydata <- pivot_wider(mydata, names_from = "variable", values_from = "value")

  if (vars == "hour") {
    splits <- c("hour", type)
  }
  if (vars == "day.hour") {
    splits <- c("hour", "wkday", type)
  }
  if (vars == "wkday") {
    splits <- c("wkday", type)
  }
  if (vars == "mnth") {
    splits <- c("mnth", type)
  }

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
median_hilow <- function(x, conf.int = 0.95, na.rm = TRUE, ...) {
  quant <- quantile(x, probs = c(0.5, (1 - conf.int), conf.int), na.rm = na.rm)
  names(quant) <- c("Mean", "Lower", "Upper")
  quant
}

# make poly function #
mkpoly <- function(dat, x = "hour", y = "Mean", group.number, myColors, alpha) {
  len <- length(unique(dat$ci)) # number of confidence intervals to consider
  ci <- sort(unique(dat$ci))

  fac <- 2

  if (len == 1L) {
    id1 <- which(dat$ci == ci[1])
  } # ids of higher band

  if (len == 2L) {
    id1 <- which(dat$ci == ci[2])
    id2 <- which(dat$ci == ci[1])
    fac <- 1
  }

  poly.na(
    dat[[x]][id1],
    dat$Lower[id1],
    dat[[x]][id1],
    dat$Upper[id1],
    group.number,
    myColors,
    alpha = fac * alpha / 2
  )

  if (len == 2L) {
    poly.na(
      dat[[x]][id2],
      dat$Lower[id2],
      dat[[x]][id2],
      dat$Upper[id2],
      group.number,
      myColors,
      alpha = alpha
    )
  }
}

mkrect <- function(
  dat,
  x = "wkday",
  y = "Mean",
  group.number,
  myColors,
  alpha
) {
  len <- length(unique(dat$ci)) # number of confidence intervals to consider
  ci <- sort(unique(dat$ci))

  fac <- 2

  if (len == 1L) {
    id1 <- which(dat$ci == ci[1])
  } # ids of higher band

  if (len == 2L) {
    id1 <- which(dat$ci == ci[2])
    id2 <- which(dat$ci == ci[1])
    fac <- 1
  }

  panel.rect(
    dat[[x]][id1] - 0.15 * fac,
    dat$Lower[id1],
    dat[[x]][id1] + 0.15 * fac,
    dat$Upper[id1],
    fill = myColors[group.number],
    border = NA,
    alpha = fac * alpha / 2
  )

  if (len == 2L) {
    panel.rect(
      dat[[x]][id2] - 0.3,
      dat$Lower[id2],
      dat[[x]][id2] + 0.3,
      dat$Upper[id2],
      fill = myColors[group.number],
      border = NA,
      alpha = alpha
    )
  }
}
