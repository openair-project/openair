#' Plot time series
#'
#' Plot time series quickly, perhaps for multiple pollutants, grouped or in
#' separate panels.
#'
#' The `timePlot` is the basic time series plotting function in
#' `openair`. Its purpose is to make it quick and easy to plot time series
#' for pollutants and other variables. The other purpose is to plot potentially
#' many variables together in as compact a way as possible.
#'
#' The function is flexible enough to plot more than one variable at once. If
#' more than one variable is chosen plots it can either show all variables on
#' the same plot (with different line types) *on the same scale*, or (if
#' `group = FALSE`) each variable in its own panels with its own scale.
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
#' `timePlot` works very well with [selectByDate()], which is used for
#' selecting particular date ranges quickly and easily. See examples below.
#'
#' By default plots are shown with a colour key at the bottom and in the case of
#' multiple pollutants or sites, strips on the left of each plot. Sometimes this
#' may be overkill and the user can opt to remove the key and/or the strip by
#' setting `key` and/or `strip` to `FALSE`. One reason to do this
#' is to maximise the plotting area and therefore the information shown.
#'
#' @param mydata A data frame of time series. Must include a `date` field
#'   and at least one variable to plot.
#' @param pollutant Name of variable to plot. Two or more pollutants can be
#'   plotted, in which case a form like `pollutant = c("nox", "co")` should
#'   be used.
#' @param group If more than one pollutant is chosen, should they all be plotted
#'   on the same graph together? The default is `FALSE`, which means they
#'   are plotted in separate panels with their own scaled. If `TRUE` then
#'   they are plotted on the same plot with the same scale.
#' @param stack If `TRUE` the time series will be stacked by year. This
#'   option can be useful if there are several years worth of data making it
#'   difficult to see much detail when plotted on a single plot.
#' @param normalise Should variables be normalised? The default is is not to
#'   normalise the data. `normalise` can take two values, either
#'   \dQuote{mean} or a string representing a date in UK format e.g. "1/1/1998"
#'   (in the format dd/mm/YYYY). If `normalise = "mean"` then each time
#'   series is divided by its mean value.  If a date is chosen, then values at
#'   that date are set to 100 and the rest of the data scaled accordingly.
#'   Choosing a date (say at the beginning of a time series) is very useful for
#'   showing how trends diverge over time. Setting `group = TRUE` is often
#'   useful too to show all time series together in one panel.
#' @param avg.time This defines the time period to average to. Can be
#'   \dQuote{sec}, \dQuote{min}, \dQuote{hour}, \dQuote{day}, \dQuote{DSTday},
#'   \dQuote{week}, \dQuote{month}, \dQuote{quarter} or \dQuote{year}. For much
#'   increased flexibility a number can precede these options followed by a
#'   space. For example, a timeAverage of 2 months would be `period = "2
#'   month"`. See function `timeAverage` for further details on this.
#' @param data.thresh The data capture threshold to use when aggregating the
#'   data using `avg.time`. A value of zero means that all available data
#'   will be used in a particular period regardless if of the number of values
#'   available. Conversely, a value of 100 will mean that all data will need to
#'   be present for the average to be calculated, else it is recorded as
#'   `NA`. Not used if `avg.time = "default"`.
#' @param statistic The statistic to apply when aggregating the data; default is
#'   the mean. Can be one of \dQuote{mean}, \dQuote{max}, \dQuote{min},
#'   \dQuote{median}, \dQuote{frequency}, \dQuote{sd}, \dQuote{percentile}. Note
#'   that \dQuote{sd} is the standard deviation and \dQuote{frequency} is the
#'   number (frequency) of valid records in the period.  \dQuote{percentile} is
#'   the percentile level between 0-100, which can be set using the
#'   \dQuote{percentile} option - see below. Not used if `avg.time =
#'   "default"`.
#' @param percentile The percentile level in percent used when `statistic =
#'   "percentile"` and when aggregating the data with `avg.time`. More than
#'   one percentile level is allowed for `type = "default"` e.g.
#'   `percentile = c(50, 95)`. Not used if `avg.time = "default"`.
#' @param date.pad Should missing data be padded-out? This is useful where a
#'   data frame consists of two or more "chunks" of data with time gaps between
#'   them. By setting `date.pad = TRUE` the time gaps between the chunks
#'   are shown properly, rather than with a line connecting each chunk. For
#'   irregular data, set to `FALSE`. Note, this should not be set for
#'   `type` other than `default`.
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
#'   Only one `type` is currently allowed in `timePlot`.
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   `RColorBrewer` colours --- see the `openair` `openColours`
#'   function for more details. For user defined the user can supply a list of
#'   colour names recognised by R (type `colours()` to see the full list).
#'   An example would be `cols = c("yellow", "green", "blue")`
#' @param plot.type The `lattice` plot type, which is a line
#'   (`plot.type = "l"`) by default. Another useful option is
#'   `plot.type = "h"`, which draws vertical lines.
#' @param key Should a key be drawn? The default is `TRUE`.
#' @param log Should the y-axis appear on a log scale? The default is
#'   `FALSE`. If `TRUE` a well-formatted log10 scale is used. This can
#'   be useful for plotting data for several different pollutants that exist on
#'   very different scales. It is therefore useful to use `log = TRUE`
#'   together with `group = TRUE`.
#' @param windflow This option allows a scatter plot to show the wind
#'   speed/direction as an arrow. The option is a list e.g. `windflow =
#'   list(col = "grey", lwd = 2, scale = 0.1)`. This option requires wind speed
#'   (`ws`) and wind direction (`wd`) to be available.
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
#' @param smooth Should a smooth line be applied to the data? The default is
#'   `FALSE`.
#' @param ci If a smooth fit line is applied, then `ci` determines whether
#'   the 95 percent confidence intervals are shown.
#' @param y.relation This determines how the y-axis scale is plotted. "same"
#'   ensures all panels use the same scale and "free" will use panel-specific
#'   scales. The latter is a useful setting when plotting data with very
#'   different values.
#' @param ref.x See `ref.y` for details. In this case the correct date
#'   format should be used for a vertical line e.g. `ref.x = list(v =
#'   as.POSIXct("2000-06-15"), lty = 5)`.
#' @param ref.y A list with details of the horizontal lines to be added
#'   representing reference line(s). For example, `ref.y = list(h = 50, lty
#'   = 5)` will add a dashed horizontal line at 50. Several lines can be plotted
#'   e.g. `ref.y = list(h = c(50, 100), lty = c(1, 5), col = c("green",
#'   "blue"))`. See `panel.abline` in the `lattice` package for more
#'   details on adding/controlling lines.
#' @param key.columns Number of columns to be used in the key. With many
#'   pollutants a single column can make to key too wide. The user can thus
#'   choose to use several columns by setting `columns` to be less than the
#'   number of pollutants.
#' @param key.position Location where the scale key is to plotted. Can include
#'   \dQuote{top}, \dQuote{bottom}, \dQuote{right} and \dQuote{left}.
#' @param name.pol This option can be used to give alternative names for the
#'   variables plotted. Instead of taking the column headings as names, the user
#'   can supply replacements. For example, if a column had the name \dQuote{nox}
#'   and the user wanted a different description, then setting `name.pol =
#'   "nox before change"` can be used. If more than one pollutant is plotted
#'   then use `c` e.g. `name.pol = c("nox here", "o3 there")`.
#' @param date.breaks Number of major x-axis intervals to use. The function will
#'   try and choose a sensible number of dates/times as well as formatting the
#'   date/time appropriately to the range being considered. This does not always
#'   work as desired automatically. The user can therefore increase or decrease
#'   the number of intervals by adjusting the value of `date.breaks` up or
#'   down.
#' @param date.format This option controls the date format on the x-axis. While
#'   `timePlot` generally sets the date format sensibly there can be some
#'   situations where the user wishes to have more control. For format types see
#'   `strptime`. For example, to format the date like \dQuote{Jan-2012} set
#'   `date.format = "\%b-\%Y"`.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels will automatically try and format pollutant names
#'   and units properly e.g.  by subscripting the \sQuote{2} in NO2.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract plot components and plotting them in other ways.
#' @param ... Other graphical parameters are passed onto `cutData` and
#'   `lattice:xyplot`. For example, `timePlot` passes the option
#'   `hemisphere = "southern"` on to `cutData` to provide southern
#'   (rather than default northern) hemisphere handling of `type =
#'   "season"`. Similarly, most common plotting parameters, such as
#'   `layout` for panel arrangement and `pch` and `cex` for plot
#'   symbol type and size and `lty` and `lwd` for line type and width,
#'   as passed to `xyplot`, although some maybe locally managed by
#'   `openair` on route, e.g. axis and title labelling options (such as
#'   `xlab`, `ylab`, `main`) are passed via `quickText` to
#'   handle routine formatting. See examples below.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family time series and trend functions
#' @examples
#'
#'
#' # basic use, single pollutant
#' timePlot(mydata, pollutant = "nox")
#'
#' # two pollutants in separate panels
#' \dontrun{
#' timePlot(mydata, pollutant = c("nox", "no2"))
#' }
#'
#' # two pollutants in the same panel with the same scale
#' \dontrun{
#' timePlot(mydata, pollutant = c("nox", "no2"), group = TRUE)
#' }
#'
#' # alternative by normalising concentrations and plotting on the same
#' scale
#' \dontrun{
#' timePlot(mydata,
#'   pollutant = c("nox", "co", "pm10", "so2"), group = TRUE, avg.time =
#'     "year", normalise = "1/1/1998", lwd = 3, lty = 1
#' )
#' }
#'
#' # examples of selecting by date
#'
#' # plot for nox in 1999
#' \dontrun{
#' timePlot(selectByDate(mydata, year = 1999), pollutant = "nox")
#' }
#'
#' # select specific date range for two pollutants
#' \dontrun{
#' timePlot(selectByDate(mydata, start = "6/8/2003", end = "13/8/2003"),
#'   pollutant = c("no2", "o3")
#' )
#' }
#'
#' # choose different line styles etc
#' \dontrun{
#' timePlot(mydata, pollutant = c("nox", "no2"), lty = 1)
#' }
#'
#' # choose different line styles etc
#' \dontrun{
#' timePlot(selectByDate(mydata, year = 2004, month = 6),
#'   pollutant =
#'     c("nox", "no2"), lwd = c(1, 2), col = "black"
#' )
#' }
#'
#' # different averaging times
#'
#' # daily mean O3
#' \dontrun{
#' timePlot(mydata, pollutant = "o3", avg.time = "day")
#' }
#'
#' # daily mean O3 ensuring each day has data capture of at least 75%
#' \dontrun{
#' timePlot(mydata, pollutant = "o3", avg.time = "day", data.thresh = 75)
#' }
#'
#' # 2-week average of O3 concentrations
#' \dontrun{
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
  plot.type = "l",
  key = TRUE,
  log = FALSE,
  windflow = NULL,
  smooth = FALSE,
  ci = TRUE,
  y.relation = "same",
  ref.x = NULL,
  ref.y = NULL,
  key.columns = 1,
  key.position = "bottom",
  name.pol = pollutant,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  ## basic function to plot single/multiple time series in flexible ways
  ## optionally includes several pre-deifined averaging periods
  ## can deal with wide range of date/time formats e.g. minute, 15-min, hourly, daily

  ## note that in the case of type "site", each site is thought of as a "pollutant"

  ## Author: David Carslaw 11 Sep. 09
  ## CHANGES:

  ## get rid of R check annoyances
  variable <- year <- NULL

  ## # EXPERIMENTAL LOG SCALING###############################################
  if (log) nlog <- 10 else nlog <- FALSE

  ## #################################################################################

  vars <- c("date", pollutant)

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  ## ################################################################################

  ## Args setup
  Args <- list(...)

  # label controls
  # (further xlab handling in code body)
  Args$xlab <- if ("xlab" %in% names(Args)) {
    quickText(Args$xlab, auto.text)
  } else {
    quickText("", auto.text)
  }
  Args$ylab <- if ("ylab" %in% names(Args)) {
    quickText(Args$ylab, auto.text)
  } else {
    NULL
  }

  if ("main" %in% names(Args)) {
    if (!is.list(Args$main)) Args$main <- quickText(Args$main, auto.text)
  }

  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  xlim <- if ("xlim" %in% names(Args)) {
    Args$xlim
  } else {
    NULL
  }

  if (!"pch" %in% names(Args)) {
    Args$pch <- NA
  }
  if (!"lwd" %in% names(Args)) {
    Args$lwd <- 1
  }
  if (!"lty" %in% names(Args)) {
    Args$lty <- NULL
  }

  ## layout
  ## (type and group handling in code body)
  if (!"layout" %in% names(Args)) {
    Args$layout <- NULL
  }

  ## strip
  ## extensive handling in main code body)
  strip <- if ("strip" %in% names(Args)) {
    Args$strip
  } else {
    TRUE
  }

  ## ### warning messages and other checks ################################################

  ## also ckeck that column "site" is present when type set to "default"
  ## but also check to see if dates are duplicated, if not, OK to proceed
  len.all <- length(mydata$date)
  len.unique <- length(unique(mydata$date))

  if (type == "default" & "site" %in% names(mydata) & len.all != len.unique) {
    if (length(unique(factor(mydata$site))) > 1)
      stop(
        "More than one site has been detected: choose type = 'site' and pollutant(s)"
      )
  }

  if (length(percentile) > 1 & length(pollutant) > 1) {
    stop("Only one pollutant allowed when considering more than one percentile")
  }

  if (!missing(statistic) & missing(avg.time)) {
    message("No averaging time applied, using avg.time ='month'")
    avg.time <- "month"
  }

  ## #######################################################################################

  if (!is.null(windflow)) {
    vars <- unique(c(vars, "wd", "ws"))
  }

  ## data checks
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  ## pad out any missing date/times so that line don't extend between areas of missing data

  theStrip <- strip

  if (date.pad) mydata <- date.pad(mydata, type = type)

  mydata <- cutData(mydata, type, ...)

  ## average the data if necessary (default does nothing)
  if (avg.time != "default") {
    ## deal with mutiple percentile values

    if (length(percentile) > 1) {
      mydata <- mydata %>%
        group_by(across(type)) %>%
        do(calcPercentile(
          .,
          pollutant = pollutant,
          avg.time = avg.time,
          data.thresh = data.thresh,
          percentile = percentile
        ))

      pollutant <- paste("percentile.", percentile, sep = "")

      if (missing(group)) group <- TRUE
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
  if (type == "default") mydata$default <- "default"

  if (!is.null(windflow)) {
    mydata <- gather(mydata, key = variable, value = value, pollutant)
  } else {
    #   mydata <- melt(mydata, id.var = c("date", type))
    mydata <- gather(mydata, key = variable, value = value, pollutant)
  }

  if (type != "default") group <- TRUE ## need to group pollutants if conditioning

  ## number of pollutants (or sites for type = "site")
  npol <- length(unique(mydata$variable)) ## number of pollutants

  ## layout - stack vertically
  if (is.null(Args$layout) & !group & !stack) Args$layout <- c(1, npol)

  ## function to normalise data ##################################
  divide.by.mean <- function(x) {
    Mean <- mean(x$value, na.rm = TRUE)
    x$value <- x$value / Mean
    x
  }

  ## function to normalise data by a specific date ##################################
  norm.by.date <- function(x, thedate) {
    ## nearest date in time series
    ## need to find first non-missing value
    temp <- na.omit(x)
    id <- which(abs(temp$date - thedate) == min(abs(temp$date - thedate)))
    id <- temp$date[id] ## the nearest date for non-missing data
    x$value <- 100 * x$value / x$value[x$date == id]
    x
  }

  ## need to check the ylab handling below
  ## not sure what was meant

  if (!missing(normalise)) {
    # preserve order of pollutants after group_by (if not factor, is alphabetic)
    mydata <- mutate(
      mydata,
      variable = factor(variable, levels = unique(variable))
    )

    if (is.null(Args$ylab)) {
      Args$ylab <- "normalised level"
    }

    if (normalise == "mean") {
      mydata <- group_by(mydata, variable) %>%
        do(divide.by.mean(.))
    } else {
      ## scale value to 100 at specific date

      thedate <- as.POSIXct(strptime(
        normalise,
        format = "%d/%m/%Y",
        tz = "GMT"
      ))

      mydata <- group_by(mydata, variable) %>%
        do(norm.by.date(., thedate = thedate))
    }
  }

  # set ylab as pollutant(s) if not already set
  if (is.null(Args$ylab)) {
    Args$ylab <- quickText(paste(pollutant, collapse = ", "), auto.text)
  }

  mylab <- sapply(
    seq_along(pollutant),
    function(x) quickText(pollutant[x], auto.text)
  )

  ## user-supplied names
  if (!missing(name.pol)) {
    mylab <- sapply(seq_along(name.pol), function(x) {
      quickText(name.pol[x], auto.text)
    })
  }

  ## set up colours
  myColors <- if (length(cols) == 1 && cols == "greyscale") {
    openColours(cols, npol + 1)[-1]
  } else {
    openColours(cols, npol)
  }

  ## basic function for lattice call + defaults
  myform <- formula(paste("value ~ date |", type))

  if (is.null(Args$strip)) {
    strip <- TRUE
  }

  strip.left <- FALSE

  dates <- dateBreaks(mydata$date, date.breaks)$major ## for date scale

  ## date axis formating
  if (is.null(date.format)) {
    formats <- dateBreaks(mydata$date, date.breaks)$format
  } else {
    formats <- date.format
  }

  scales <- list(
    x = list(at = dates, format = formats),
    y = list(log = nlog, relation = y.relation, rot = 0)
  )

  ## layout changes depening on plot type

  if (!group) {
    ## sepate panels per pollutant
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
      x = list(at = dates, format = formats),
      y = list(
        relation = y.relation,
        rot = 0,
        log = nlog
      )
    )

    if (is.null(Args$lty)) Args$lty <- 1 ## don't need different line types here
  }

  ## set lty if not set by this point
  if (is.null(Args$lty)) {
    Args$lty <- 1:length(pollutant)
  }

  if (type == "default") strip <- FALSE

  ## if stacking of plots by year is needed
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
    ##  dates <- unique(dateTrunc(mydata$date, "months")) - this does not work?
    dates <- as.POSIXct(
      unique(paste(format(mydata$date, "%Y-%m"), "-01", sep = "")),
      "GMT"
    )

    scales <- list(
      x = list(format = "%d-%b", relation = "sliced"),
      y = list(log = nlog)
    )

    xlim <- lapply(split(mydata, mydata["year"]), function(x) range(x$date))
  }

  if (missing(key.columns)) key.columns <- npol

  ## keys and strips - to show or not

  if (key) {
    ## type of key depends on whether points are plotted or not
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
    key <- NULL ## either there is a key or there is not
  }

  if (theStrip) {
    strip <- strip
    strip.left <- strip.left
  } else {
    strip <- FALSE
    strip.left <- FALSE
  }

  ## special layout if type = "wd"
  if (length(type) == 1 & type[1] == "wd" & is.null(Args$layout)) {
    ## re-order to make sensible layout
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    mydata$wd <- ordered(mydata$wd, levels = wds)

    ## see if wd is actually there or not
    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(mydata$wd)) FALSE else TRUE
    })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])

    mydata$wd <- factor(mydata$wd) ## remove empty factor levels

    Args$layout <- c(3, 3)
    if (!"skip" %in% names(Args)) {
      Args$skip <- skip
    }
  }
  if (!"skip" %in% names(Args)) {
    Args$skip <- FALSE
  }

  ## allow reasonable gaps at ends, default has too much padding
  gap <- difftime(max(mydata$date), min(mydata$date), units = "secs") / 80
  if (is.null(xlim)) xlim <- range(mydata$date) + c(-1 * gap, gap)

  # make sure order is correct
  mydata$variable <- factor(mydata$variable, levels = pollutant)

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
      ## deal with points separately - useful if missing data where line
      ## does not join consequtive points
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

      ## add reference lines

      if (!is.null(ref.x)) do.call(panel.abline, ref.x)
      if (!is.null(ref.y)) do.call(panel.abline, ref.y)
    }
  )

  ## reset for Args
  xyplot.args <- listUpdate(xyplot.args, Args)

  # plot
  plt <- do.call(xyplot, xyplot.args)

  ## output

  if (plot) plot(plt)
  newdata <- mydata
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"

  invisible(output)
}


## function to plot wind flow arrows
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

  ## actual shape of the plot window
  delta.x.cm <- diff(current.panel.limits(unit = "cm")$xlim)
  delta.y.cm <- diff(current.panel.limits(unit = "cm")$ylim)

  ## physical size of plot windows, correct so wd is right when plotted.
  ## need to replot if window re-scaled by user
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
