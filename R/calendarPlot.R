#' Plot time series values in a conventional calendar format
#'
#' This function will plot data by month laid out in a conventional calendar
#' format. The main purpose is to help rapidly visualise potentially complex
#' data in a familiar way. Users can also choose to show daily mean wind vectors
#' if wind speed and direction are available.
#'
#' [calendarPlot()] will plot data in a conventional calendar format, i.e., by
#' month and day of the week. Daily statistics are calculated using
#' [timeAverage()], which by default will calculate the daily mean
#' concentration.
#'
#' If wind direction is available it is then possible to plot the wind direction
#' vector on each day. This is very useful for getting a feel for the
#' meteorological conditions that affect pollutant concentrations. Note that if
#' hourly or higher time resolution are supplied, then [calendarPlot()] will
#' calculate daily averages using [timeAverage()], which ensures that wind
#' directions are vector-averaged.
#'
#' If wind speed is also available, then setting the option `annotate = "ws"`
#' will plot the wind vectors whose length is scaled to the wind speed. Thus
#' information on the daily mean wind speed and direction are available.
#'
#' It is also possible to plot categorical scales. This is useful where, for
#' example, an air quality index defines concentrations as bands, e.g., "good",
#' "poor". In these cases users must supply `labels` and corresponding `breaks`.
#'
#' Note that is is possible to pre-calculate concentrations in some way before
#' passing the data to [calendarPlot()]. For example [rollingMean()] could be
#' used to calculate rolling 8-hour mean concentrations. The data can then be
#' passed to [calendarPlot()] and `statistic = "max"` chosen, which will plot
#' maximum daily 8-hour mean concentrations.
#'
#' @inheritParams timePlot
#'
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox". `
#' @param year Year to plot e.g. `year = 2003`. If not supplied all data
#'   potentially spanning several years will be plotted.
#' @param month If only certain month are required. By default the function will
#'   plot an entire year even if months are missing. To only plot certain months
#'   use the `month` option where month is a numeric 1:12 e.g. `month = c(1,
#'   12)` to only plot January and December.
#' @param annotate This option controls what appears on each day of the
#'   calendar. Can be:
#'   - `"date"` --- shows day of the month
#'   - `"wd"` --- shows vector-averaged wind direction
#'   - `"ws"` --- shows vector-averaged wind direction scaled by wind speed
#'   - `"value"` --- shows the daily mean value
#' @param statistic Statistic passed to [timeAverage()]. Note that if `statistic
#'   %in% c("max", "min")` and `annotate` is "ws" or "wd", the hour
#'   corresponding to the maximum/minimum concentration of `polluant` is used to
#'   provide the associated `ws` or `wd` and not the maximum/minimum daily `ws`
#'   or `wd`.
#' @param limits Use this option to manually set the colour scale limits. This
#'   is useful in the case when there is a need for two or more plots and a
#'   consistent scale is needed on each. Set the limits to cover the maximum
#'   range of the data for all plots of interest. For example, if one plot had
#'   data covering 0--60 and another 0--100, then set `limits = c(0, 100)`. Note
#'   that data will be ignored if outside the limits range.
#' @param lim A threshold value to help differentiate values above and below
#'   `lim`. It is used when `annotate = "value"`. See next few options for
#'   control over the labels used.
#' @param col.lim For the annotation of concentration labels on each day. The
#'   first sets the colour of the text below `lim` and the second sets the
#'   colour of the text above `lim`.
#' @param col.arrow The colour of the annotated wind direction / wind speed
#'   arrows.
#' @param font.lim For the annotation of concentration labels on each day. The
#'   first sets the font of the text below `lim` and the second sets the font of
#'   the text above `lim`. Note that font = 1 is normal text and font = 2 is
#'   bold text.
#' @param cex.lim For the annotation of concentration labels on each day. The
#'   first sets the size of the text below `lim` and the second sets the size of
#'   the text above `lim`.
#' @param cex.date The base size of the annotation text for the date.
#' @param digits The number of digits used to display concentration values when
#'   `annotate = "value"`.
#' @param breaks If a categorical scale is required then these breaks will be
#'   used. For example, `breaks = c(0, 50, 100, 1000)`. In this case "good"
#'   corresponds to values between 0 and 50 and so on. Users should set the
#'   maximum value of `breaks` to exceed the maximum data value to ensure it is
#'   within the maximum final range e.g. 100--1000 in this case.
#' @param labels If a categorical scale is defined using `breaks`, then `labels`
#'   can be used to override the default category labels, e.g., `labels =
#'   c("good", "bad", "very bad")`. Note there is one less label than break.
#' @param w.shift Controls the order of the days of the week. By default the
#'   plot shows Saturday first (`w.shift = 0`). To change this so that it starts
#'   on a Monday for example, set `w.shift = 2`, and so on.
#' @param w.abbr.len The default (`1`) abbreviates the days of the week to a
#'   single letter (e.g., in English, S/S/M/T/W/T/F). `w.abbr.len` defines the
#'   number of letters to abbreviate until. For example, `w.abbr.len = 3` will
#'   abbreviate "Monday" to "Mon".
#' @param remove.empty Should months with no data present be removed? Default is
#'   `TRUE`.
#' @param show.year If only a single year is being plotted, should the calendar
#'   labels include the year label? `TRUE` creates labels like "January-2000",
#'   `FALSE` labels just as "January". If multiple years of data are detected,
#'   this option is forced to be `TRUE`.
#' @param key.header Adds additional text/labels to the scale key. For example,
#'   passing `calendarPlot(mydata, key.header = "header", key.footer =
#'   "footer")` adds addition text above and below the scale key. These
#'   arguments are passed to [drawOpenKey()] via [quickText()], applying the
#'   `auto.text` argument, to handle formatting.
#' @param key.footer see `key.header`.
#' @param key.position Location where the scale key is to plotted. Allowed
#'   arguments currently include `"top"`, `"right"`, `"bottom"` and `"left"`.
#' @param key Fine control of the scale key via [drawOpenKey()]. See
#'   [drawOpenKey()] for further details.
#' @param ... Other graphical parameters are passed onto the `lattice` function
#'   [lattice::levelplot()], with common axis and title labelling options (such
#'   as `xlab`, `ylab`, `main`) being passed to via [quickText()] to handle
#'   routine formatting.
#' @export
#' @import grid
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family time series and trend functions
#' @examples
#' # basic plot
#' calendarPlot(mydata, pollutant = "o3", year = 2003)
#'
#' # show wind vectors
#' calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "wd")
#' \dontrun{
#' # show wind vectors scaled by wind speed and different colours
#' calendarPlot(mydata,
#'   pollutant = "o3", year = 2003, annotate = "ws",
#'   cols = "heat"
#' )
#'
#' # show only specific months with selectByDate
#' calendarPlot(selectByDate(mydata, month = c(3, 6, 10), year = 2003),
#'   pollutant = "o3", year = 2003, annotate = "ws", cols = "heat"
#' )
#'
#' # categorical scale example
#' calendarPlot(mydata,
#'   pollutant = "no2", breaks = c(0, 50, 100, 150, 1000),
#'   labels = c("Very low", "Low", "High", "Very High"),
#'   cols = c("lightblue", "green", "yellow", "red"), statistic = "max"
#' )
#'
#' # UK daily air quality index
#' pm10.breaks <- c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000)
#' calendarPlot(mydata, "pm10",
#'   year = 1999, breaks = pm10.breaks,
#'   labels = c(1:10), cols = "daqi", statistic = "mean", key.header = "DAQI"
#' )
#' }
calendarPlot <-
  function(
    mydata,
    pollutant = "nox",
    year = NULL,
    month = NULL,
    annotate = "date",
    statistic = "mean",
    data.thresh = 0,
    percentile = NA,
    cols = "heat",
    limits = c(0, 100),
    lim = NULL,
    col.lim = c("grey30", "black"),
    col.arrow = "black",
    font.lim = c(1, 2),
    cex.lim = c(0.6, 1),
    cex.date = 0.6,
    digits = 0,
    labels = NA,
    breaks = NA,
    w.shift = 0,
    w.abbr.len = 1,
    remove.empty = TRUE,
    show.year = TRUE,
    key.header = "",
    key.footer = "",
    key.position = "right",
    key = TRUE,
    auto.text = TRUE,
    plot = TRUE,
    ...
  ) {
    ## ---- Setup & Validation ----

    # check w.shift
    if (w.shift < 0 || w.shift > 6) {
      cli::cli_abort("{.field w.shift} should be between {0} and {6}.")
    }

    # extra args
    extra.args <- list(...)

    ## ---- Graphics ----

    # set graphics
    current.font <- trellis.par.get("fontsize")
    on.exit(trellis.par.set(fontsize = current.font))

    if ("fontsize" %in% names(extra.args)) {
      trellis.par.set(fontsize = list(text = extra.args$fontsize))
    }

    # themes for calendarPlot
    def.theme <- list(
      strip.background = list(col = "#ffe5cc"),
      strip.border = list(col = "black"),
      axis.line = list(col = "black"),
      par.strip.text = list(cex = 1)
    )

    cal.theme <- list(
      strip.background = list(col = "grey90"),
      strip.border = list(col = "transparent"),
      axis.line = list(col = "transparent"),
      par.strip.text = list(cex = 0.8)
    )

    lattice.options(default.theme = cal.theme)
    on.exit(
      # reset theme
      lattice.options(default.theme = def.theme)
    )

    ## ---- Labels ----

    # label controls
    extra.args$xlab <- quickText(extra.args$xlab %||% "", auto.text)
    extra.args$ylab <- quickText(extra.args$ylab %||% "", auto.text)
    main <- quickText(extra.args$main %||% NULL, auto.text)

    ## ---- Data Preparation ----

    # filter and check data
    mydata <- prepare_calendar_data(
      mydata,
      year = year,
      month = month,
      pollutant = pollutant,
      annotate = annotate
    )

    # all the days in the period - to be bound later
    all_dates <- seq(
      lubridate::as_date(lubridate::floor_date(min(mydata$date), "month")),
      lubridate::as_date(lubridate::ceiling_date(max(mydata$date), "month")) -
        1,
      by = "day"
    )

    ## ---- Data Aggregation ----

    # if statistic is max/min we want the corresponding ws/wd for the pollutant,
    # not simply the max ws/wd
    if (statistic %in% c("max", "min")) {
      if (statistic == "max") {
        which.fun <- which.max
      } else if (statistic == "min") {
        which.fun <- which.min
      }

      # max ws/wd for hour with max pollutant value
      maxes <- mydata |>
        dplyr::mutate(date = lubridate::as_date(.data$date)) |>
        dplyr::group_by(date) |>
        dplyr::slice(which.fun(.data[[pollutant]]))

      # averaged data, make sure Date format (max returns POSIXct)
      mydata <- timeAverage(
        mydata,
        "day",
        statistic = statistic,
        data.thresh = data.thresh
      ) |>
        dplyr::mutate(date = lubridate::as_date(.data$date))

      # replace with parallel max
      mydata <- dplyr::left_join(
        dplyr::select(mydata, !dplyr::any_of(c("ws", "wd"))),
        dplyr::select(maxes, !.data[[pollutant]]),
        by = dplyr::join_by(date)
      )
    } else {
      # calculate daily means
      mydata <- timeAverage(
        mydata,
        "day",
        statistic = statistic,
        data.thresh = data.thresh,
        percentile = percentile
      )

      mydata$date <- lubridate::as_date(mydata$date)
    }

    ## ---- Data Structuring ----

    # make sure all days are available
    mydata <-
      dplyr::left_join(data.frame(date = all_dates), mydata, by = "date")

    # split by year-month, and set 'type' to this
    if (show.year) {
      mydata <- dplyr::mutate(
        mydata,
        cuts = format(.data$date, "%B-%Y"),
        cuts = ordered(.data$cuts, levels = unique(.data$cuts))
      )
    } else {
      # cut just by month - although check duplicate years
      mydata <- dplyr::mutate(
        mydata,
        cuts = format(.data$date, "%B"),
        years = format(.data$date, "%Y"),
        cuts = ordered(.data$cuts, levels = unique(.data$cuts))
      )

      # check duplicates
      verify_duplicates <-
        mydata |>
        dplyr::count(.data$cuts, .data$years) |>
        dplyr::add_count(.data$cuts, name = "month_counts")

      # if any duplicates, set show.year = TRUE
      if (
        any(verify_duplicates$month_counts > 1L) ||
          dplyr::n_distinct(verify_duplicates$years) > 1L
      ) {
        mydata <- dplyr::mutate(
          mydata,
          cuts = format(.data$date, "%B-%Y"),
          cuts = ordered(.data$cuts, levels = unique(.data$cuts))
        )
      }

      mydata <- dplyr::select(mydata, -"years")
    }

    type <- "cuts"

    # drop empty months?
    if (remove.empty) {
      mydata <- mydata |>
        dplyr::group_by(.data$cuts) |>
        dplyr::mutate(
          empty = all(is.na(dplyr::across(dplyr::all_of(pollutant))))
        ) |>
        dplyr::filter(!.data$empty) |>
        dplyr::ungroup()
    }

    # snapshot data for later
    original_data <- mydata

    # timeAverage will pad-out missing months
    if (!is.null(month)) {
      mydata <- selectByDate(mydata, month = month)
    }

    # transform data into a calendar grid
    mydata <- mapType(
      data,
      type = type,
      fun = \(df) prepare_calendar_grid(df, pollutant, w.shift),
      .include_default = TRUE
    ) |>
      # retain actual numerical value (retain for categorical scales)
      dplyr::mutate(value = .data$conc.mat)

    ## ---- Wind Speed / Direction Annotations ----

    # need wd dataframe if ws/wd annotation
    if (annotate %in% c("ws", "wd")) {
      wd <- original_data |>
        dplyr::mutate(wd = .data$wd * 2 * pi / 360) |>
        mapType(
          data,
          type = type,
          fun = \(df) prepare_calendar_grid(df, "wd", w.shift),
          .include_default = TRUE
        ) |>
        # actual numerical value (retain for categorical scales)
        dplyr::mutate(value = .data$conc.mat)
    }

    # if ws annotation, also need ws
    if (annotate == "ws") {
      ws <- original_data |>
        dplyr::mutate(wd = .data$wd * 2 * pi / 360) |>
        mapType(
          data,
          type = type,
          fun = \(df) prepare_calendar_grid(df, "ws", w.shift),
          .include_default = TRUE
        ) |>
        dplyr::mutate(
          # normalise wind speeds to highest daily mean
          conc.mat = .data$conc.mat / max(.data$conc.mat, na.rm = TRUE),
          # actual numerical value (retain for categorical scales)
          value = .data$conc.mat
        )
    }

    ## ---- Scales & Colours Setup ----

    # get name for lattice strips
    strip <- strip.fun(mydata, type, auto.text)[[1]]

    # handle breaks
    if (!anyNA(breaks)) {
      # assign labels if no labels are given
      if (anyNA(labels)) {
        labels <- c()
        for (i in seq_along(breaks)) {
          lhs <- breaks[i]
          rhs <- breaks[i + 1]
          str <- paste(lhs, rhs, sep = " - ")
          labels <- append(labels, str)
        }
        labels <- labels[-i]
      }

      category <- TRUE
      mydata <- dplyr::mutate(
        mydata,
        conc.mat = cut(.data$conc.mat, breaks = breaks, labels = labels)
      )
    } else {
      category <- FALSE
    }

    ## ---- Categorical Scales ----

    # categorical scales required
    if (category) {
      # check the breaks and labels are consistent
      if (length(labels) + 1 != length(breaks)) {
        stop("Need one more break than labels")
      }
      n <- length(levels(mydata$conc.mat))

      col <- openColours(cols, n)
      legend <- list(
        col = col,
        space = key.position,
        auto.text = auto.text,
        labels = levels(mydata$conc.mat),
        footer = key.footer,
        header = key.header,
        height = 0.8,
        width = 1.5,
        fit = "scale",
        plot.style = "other"
      )

      col.scale <- breaks
      legend <- makeOpenKeyLegend(key, legend, "windRose")
    } else {
      # continuous colour scale
      nlev <- 200

      # handle missing breaks arguments

      if (missing(limits)) {
        breaks <- pretty(mydata$value, n = nlev)
        labs <- pretty(breaks, 7)
        labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      } else {
        breaks <- pretty(limits, n = nlev)
        labs <- pretty(breaks, 7)
        labs <- labs[labs >= min(breaks) & labs <= max(breaks)]

        if (max(limits) < max(mydata$value, na.rm = TRUE)) {
          # if clipping highest, then annotate differently
          id <- which(mydata$value > max(limits))
          mydata$value[id] <- max(limits)
          labs <- pretty(breaks, 7)
          labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
          labs[length(labs)] <- paste(">", labs[length(labs)])
        }
      }

      nlev2 <- length(breaks)
      col <- openColours(cols, (nlev2 - 1))
      col.scale <- breaks

      legend <- list(
        col = col,
        at = col.scale,
        labels = list(labels = labs),
        space = key.position,
        auto.text = auto.text,
        footer = key.footer,
        header = key.header,
        height = 1,
        width = 1.5,
        fit = "all"
      )

      legend <- makeOpenKeyLegend(key, legend, "calendarPlot")
    }

    ## ---- Construct Plot ----

    # get weekday abbreviation for axis
    weekday.abb <-
      substr(format(ISOdate(2000, 1, 2:8), "%A"), 1, w.abbr.len)[
        ((6:12) +
          w.shift) %%
          7 +
          1
      ]

    lv.args <- list(
      x = value ~ x * y | cuts,
      data = mydata,
      par.settings = cal.theme,
      main = main,
      strip = strip,
      par.strip.text = list(cex = 0.9),
      at = col.scale,
      col.regions = col,
      as.table = TRUE,
      scales = list(
        y = list(draw = FALSE),
        x = list(
          at = 1:7,
          labels = weekday.abb,
          tck = 0
        ),
        par.strip.text = list(cex = 0.8),
        alternating = 1,
        relation = "free"
      ),
      aspect = 6 / 7,
      between = list(x = 1),
      colorkey = FALSE,
      legend = legend,
      panel = function(x, y, subscripts, ...) {
        panel.levelplot(x, y, subscripts, ...)
        panel.abline(v = c(0.5:7.5), col = "grey90")
        panel.abline(h = c(0.5:7.5), col = "grey90")

        if (annotate == "date") {
          ltext(
            x[subscripts],
            y[subscripts],
            labels = mydata$date.mat[subscripts],
            cex = cex.date,
            col = as.character(mydata$dateColour[subscripts])
          )
        }

        if (annotate == "value") {
          # add some dates for navigation
          date.col <- as.character(mydata$dateColour[subscripts])
          ids <- which(date.col == "black")
          date.col[ids] <- "transparent"
          ltext(
            x[subscripts],
            y[subscripts],
            labels = mydata$date.mat[subscripts],
            cex = cex.date,
            col = date.col
          )

          concs <- mydata$value[subscripts]

          # deal with values above/below threshold
          ids <- seq_along(concs)
          the.cols <- rep(col.lim[1], length(ids))
          the.font <- rep(font.lim[1], length(ids))
          the.cex <- rep(cex.lim[1], length(ids))
          if (!is.null(lim)) {
            # ids where conc is >= lim
            ids <- which(concs >= lim)
            the.cols[ids] <- col.lim[2]
            the.font[ids] <- font.lim[2]
            the.cex[ids] <- cex.lim[2]
          }

          the.labs <- round(concs, digits = digits)
          id <- which(is.na(the.labs))
          if (length(id) > 0) {
            the.labs <- as.character(the.labs)
            the.labs[id] <- ""
          }
          ltext(
            x[subscripts],
            y[subscripts],
            labels = the.labs,
            cex = the.cex,
            font = the.font,
            col = the.cols
          )
        }

        if (annotate == "wd") {
          larrows(
            x + 0.5 * sin(wd$value[subscripts]),
            y + 0.5 * cos(wd$value[subscripts]),
            x + -0.5 * sin(wd$value[subscripts]),
            y + -0.5 * cos(wd$value[subscripts]),
            angle = 20,
            length = 0.07,
            lwd = 0.5,
            col = col.arrow
          )
        }

        if (annotate == "ws") {
          larrows(
            x + (0.5 * sin(wd$value[subscripts]) * ws$value[subscripts]),
            y + (0.5 * cos(wd$value[subscripts]) * ws$value[subscripts]),
            x + (-0.5 * sin(wd$value[subscripts]) * ws$value[subscripts]),
            y + (-0.5 * cos(wd$value[subscripts]) * ws$value[subscripts]),
            angle = 20,
            length = 0.07,
            lwd = 0.5,
            col = col.arrow
          )
        }
      }
    )

    # reset for extra.args
    lv.args <- listUpdate(lv.args, extra.args)

    # plot
    if (plot) {
      print(do.call(levelplot, lv.args))
    }

    # output
    plt <- do.call(levelplot, lv.args)

    # add in ws and wd if there
    newdata <-
      dplyr::left_join(
        mydata,
        dplyr::select(original_data, dplyr::any_of(c("date", "ws", "wd"))),
        by = "date"
      )

    output <- list(
      plot = plt,
      data = newdata,
      call = match.call()
    )
    class(output) <- "openair"
    invisible(output)
  }

#' @noRd
prepare_calendar_data <- function(mydata, year, month, pollutant, annotate) {
  # filter by year
  if (!is.null(year)) {
    mydata <- selectByDate(mydata, year = year)
  }

  # filter by month
  if (!is.null(month)) {
    mydata <- selectByDate(mydata, month = month)
  }

  # if no data left, error
  if (nrow(mydata) == 0) {
    cli::cli_abort(c(
      "x" = "No data to plot.",
      "i" = "Check {.field year} and {.field month}."
    ))
  }

  # extract variables of interest
  if (annotate %in% c("date", "value")) {
    vars <- c("date", pollutant)
  }
  if (annotate == "wd") {
    vars <- c("wd", "ws", "date", pollutant)
  }
  if (annotate == "ws") {
    vars <- c("wd", "ws", "date", pollutant)
  }

  # check input data
  mydata <-
    checkPrep(mydata, vars, "default", remove.calm = FALSE)

  return(mydata)
}

#' @noRd
prepare_calendar_grid <- function(mydata, pollutant, w.shift) {
  # number of blank cells at beginning to get calendar format
  pad.start <-
    (as.numeric(format(mydata$date[1], "%w")) - w.shift) %% 7 + 1

  # need to do in reverse to plot easily
  conc <- rev(mydata[[pollutant]])
  actual_date <- rev(mydata$date)

  # day of the month
  theDates <- rev(as.numeric(format(mydata$date, "%d")))

  # get number of days left over at end of 7x6 regular grid
  daysAtEnd <- (7 * 6) - pad.start - nrow(mydata)

  # get relevant days in previous and next month, like a real calendar
  if (daysAtEnd > 0) {
    endDates <- mydata$date[nrow(mydata)] + (1:daysAtEnd)
    endDates <- rev(as.numeric(format(endDates, "%d")))
    conc <- c(rep(NA, daysAtEnd), conc)
    actual_date <- c(rep(NA, daysAtEnd), actual_date)
    theDates <- c(endDates, theDates)
  }

  if (pad.start > 0) {
    beginDates <- -1 * (1:pad.start) + mydata$date[1]
    beginDates <- as.numeric(format(beginDates, "%d"))
    conc <- c(conc, rep(NA, pad.start))
    actual_date <- c(actual_date, rep(NA, pad.start))
    theDates <- c(theDates, beginDates)
  }

  # colurs for dates
  dateColour <- c(
    rep("grey70", daysAtEnd),
    rep("black", nrow(mydata)),
    rep("grey70", pad.start)
  )

  # create and reverse matrix for data
  reversed_matrix <- function(data) {
    mat <- matrix(data, ncol = 7, byrow = TRUE)
    as.vector(apply(mat, 1, rev))
  }

  # Create all matrices
  conc.mat <- reversed_matrix(conc)
  date.mat <- reversed_matrix(theDates)
  actual_date.mat <- reversed_matrix(actual_date)
  colour.mat <- reversed_matrix(dateColour)

  # Create grid and results
  grid <- data.frame(expand.grid(x = 1:7, y = 1:6))

  results <- dplyr::tibble(
    x = grid$x,
    y = grid$y,
    conc.mat,
    date.mat = date.mat,
    dateColour = colour.mat,
    date = lubridate::as_date(actual_date.mat)
  )
  results
}
