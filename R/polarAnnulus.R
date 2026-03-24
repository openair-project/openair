#' Bivariate polarAnnulus plot
#'
#' Typically plots the concentration of a pollutant by wind direction and as a
#' function of time as an annulus. The function is good for visualising how
#' concentrations of pollutants vary by wind direction and a time period e.g. by
#' month, day of week.
#'
#' The `polarAnnulus` function shares many of the properties of the
#' `polarPlot`. However, `polarAnnulus` is focussed on displaying
#' information on how concentrations of a pollutant (values of another variable)
#' vary with wind direction and time. Plotting as an annulus helps to reduce
#' compression of information towards the centre of the plot. The circular plot
#' is easy to interpret because wind direction is most easily understood in
#' polar rather than Cartesian coordinates.
#'
#' The inner part of the annulus represents the earliest time and the outer part
#' of the annulus the latest time. The time dimension can be shown in many ways
#' including "trend", "hour" (hour or day), "season" (month of the year) and
#' "weekday" (day of the week). Taking hour as an example, the plot will show
#' how concentrations vary by hour of the day and wind direction. Such plots can
#' be very useful for understanding how different source influences affect a
#' location.
#'
#' For `type = "trend"` the amount of smoothing does not vary linearly with
#' the length of the time series i.e. a certain amount of smoothing per unit
#' interval in time. This is a deliberate choice because should one be
#' interested in a subset (in time) of data, more detail will be provided for
#' the subset compared with the full data set. This allows users to investigate
#' specific periods in more detail. Full flexibility is given through the
#' smoothing parameter `k`.
#'
#' @inheritParams polarPlot

#' @param mydata A data frame minimally containing `date`, `wd` and a
#'   pollutant.

#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`. There can also be
#'   more than one pollutant specified e.g. `pollutant = c("nox", "no2")`. The
#'   main use of using two or more pollutants is for model evaluation where two
#'   species would be expected to have similar concentrations. This saves the
#'   user stacking the data and it is possible to work with columns of data
#'   directly. A typical use would be `pollutant = c("obs", "mod")` to compare
#'   two columns \dQuote{obs} (the observations) and \dQuote{mod} (modelled
#'   values).
#'
#' @param offset `offset` controls the size of the 'hole' in the middle and is
#'   expressed on a scale of `0` to `100`, where `0` is no hole and `100` is a
#'   hole that takes up the entire plotting area.
#'
#' @param strip.position Location where the facet 'strips' are located when
#'   using `type`. When one `type` is provided, can be one of `"left"`,
#'   `"right"`, `"bottom"` or `"top"`. When two `type`s are provided, this
#'   argument defines whether the strips are "switched" and can take either
#'   `"x"`, `"y"`, or `"both"`. For example, `"x"` will switch the 'top' strip
#'   locations to the bottom of the plot.
#'
#' @param resolution Two plot resolutions can be set: \dQuote{normal} and
#'   \dQuote{fine} (the default).
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
#' @param period This determines the temporal period to consider. Options are
#'   \dQuote{hour} (the default, to plot diurnal variations), \dQuote{season} to
#'   plot variation throughout the year, \dQuote{weekday} to plot day of the
#'   week variation and \dQuote{trend} to plot the trend by wind direction.
#'
#' @param col.na Colour to be used to show missing data.
#'
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
#'   Type can be up length two e.g. `type = c("season", "site")` will produce a
#'   2x2 plot split by season and site. The use of two types is mostly meant for
#'   situations where there are several sites. Note, when two types are provided
#'   the first forms the columns and the second the rows.
#'
#'   Also note that for the `polarAnnulus` function some type/period
#'   combinations are forbidden or make little sense. For example, `type =
#'   "season"` and `period = "trend"` (which would result in a plot with too
#'   many gaps in it for sensible smoothing), or `type = "weekday"` and `period
#'   = "weekday"`.
#'
#' @param statistic The statistic that should be applied to each wind
#'   speed/direction bin. Can be \dQuote{mean} (default), \dQuote{median},
#'   \dQuote{max} (maximum), \dQuote{frequency}. \dQuote{stdev} (standard
#'   deviation), \dQuote{weighted.mean} or \dQuote{cpf} (Conditional Probability
#'   Function). Because of the smoothing involved, the colour scale for some of
#'   these statistics is only to provide an indication of overall pattern and
#'   should not be interpreted in concentration units e.g. for `statistic =
#'   "weighted.mean"` where the bin mean is multiplied by the bin frequency and
#'   divided by the total frequency. In many cases using `polarFreq` will be
#'   better. Setting `statistic = "weighted.mean"` can be useful because it
#'   provides an indication of the concentration * frequency of occurrence and
#'   will highlight the wind speed/direction conditions that dominate the
#'   overall mean.
#'
#' @param percentile If `statistic = "percentile"` or `statistic = "cpf"` then
#'   `percentile` is used, expressed from 0 to 100. Note that the percentile
#'   value is calculated in the wind speed, wind direction \sQuote{bins}. For
#'   this reason it can also be useful to set `min.bin` to ensure there are a
#'   sufficient number of points available to estimate a percentile. See
#'   `quantile` for more details of how percentiles are calculated.
#'
#' @param date.pad For `type = "trend"` (default), `date.pad = TRUE` will
#'   pad-out missing data to the beginning of the first year and the end of the
#'   last year. The purpose is to ensure that the trend plot begins and ends at
#'   the beginning or end of year.
#'
#' @param k The smoothing value supplied to `gam` for the temporal and wind
#'   direction components, respectively. In some cases e.g. a trend plot with
#'   less than 1-year of data the smoothing with the default values may become
#'   too noisy and affected more by outliers. Choosing a lower value of `k` (say
#'   10) may help produce a better plot.
#'
#' @param angle.scale The scale is by default shown at a 315 degree angle.
#'   Sometimes the placement of the scale may interfere with an interesting
#'   feature. The user can therefore set `angle.scale` to another value (between
#'   0 and 360 degrees) to mitigate such problems. For example `angle.scale =
#'   45` will draw the scale heading in a NE direction.
#'
#' @param ... Other graphical parameters passed onto `cutData` and other
#'   functions. For example, `polarAnnulus` passes the option `hemisphere =
#'   "southern"` on to `cutData` to provide southern (rather than default
#'   northern) hemisphere handling of `type = "season"`. Similarly, common axis
#'   and title labelling options (such as `xlab`, `ylab`, `main`) are passed to
#'   `levelplot` via `quickText` to handle routine formatting.
#'
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @author Jack Davison
#' @family polar directional analysis functions
#' @examples
#' # diurnal plot for PM10 at Marylebone Rd
#' \dontrun{
#' polarAnnulus(mydata,
#'   pollutant = "pm10",
#'   main = "diurnal variation in pm10 at Marylebone Road"
#' )
#' }
#'
#' # seasonal plot for PM10 at Marylebone Rd
#' \dontrun{
#' polarAnnulus(mydata, poll = "pm10", period = "season")
#' }
#'
#' # trend in coarse particles (PMc = PM10 - PM2.5), calculate PMc first
#'
#' mydata$pmc <- mydata$pm10 - mydata$pm25
#' \dontrun{
#' polarAnnulus(mydata,
#'   poll = "pmc", period = "trend",
#'   main = "trend in pmc at Marylebone Road"
#' )
#' }
polarAnnulus <-
  function(
    mydata,
    pollutant = "nox",
    resolution = "fine",
    local.tz = NULL,
    period = "hour",
    type = "default",
    statistic = "mean",
    percentile = NA,
    limits = NULL,
    cols = "default",
    col.na = "white",
    offset = 50,
    angle.scale = 0,
    min.bin = 1,
    exclude.missing = TRUE,
    date.pad = FALSE,
    force.positive = TRUE,
    k = c(20, 10),
    normalise = FALSE,
    strip.position = "top",
    key.header = statistic,
    key.footer = pollutant,
    key.position = "right",
    key = TRUE,
    auto.text = TRUE,
    plot = TRUE,
    ...
  ) {
    # check statistic value is valid
    statistic <- tolower(statistic)
    statistic <- rlang::arg_match(
      statistic,
      c(
        "mean",
        "median",
        "frequency",
        "max",
        "stdev",
        "weighted.mean",
        "percentile",
        "cpf"
      )
    )

    # check period value is valid
    period <- tolower(period)
    period <- rlang::arg_match(period, c("hour", "weekday", "season", "trend"))

    # check other inputs
    if (statistic == "percentile" && is.na(percentile & statistic != "cpf")) {
      cli::cli_warn("{.arg percentile} value missing,  using 50")
      percentile <- 50
    }

    if (key.header[1] == "weighted.mean") {
      key.header <- c("weighted mean")
    }
    if (key.header[1] == "percentile") {
      lastnum <- substr(percentile, nchar(percentile), nchar(percentile))
      numend <- dplyr::recode_values(
        lastnum,
        "1" ~ "st",
        "2" ~ "nd",
        "3" ~ "rd",
        default = "th"
      )

      key.header <- paste(paste0(percentile, numend), "percentile")
    }
    if (key.header[1] == "cpf") {
      key.header <- c("CPF probability")
    }

    # extract variables of interest
    vars <- c("wd", "date", pollutant)

    if (period == "trend" && "season" %in% type) {
      cli::cli_abort(
        "Cannot have same {.arg type} as {'season'} and {.arg period} as {'trend'}."
      )
    }
    if (length(type) > 2) {
      cli::cli_abort("Cannot have more than two {.arg types}.")
    }

    # extra.args setup
    extra.args <- list(...)

    # label controls
    extra.args$xlab <- quickText(extra.args$xlab, auto.text)
    extra.args$ylab <- quickText(extra.args$ylab, auto.text)
    extra.args$main <- quickText(extra.args$main, auto.text)

    # check data
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    # if more than one pollutant, need to stack the data and set type = "variable"
    # this case is most relevent for model-measurement compasrions where data are in columns
    if (length(pollutant) > 1) {
      mydata <- tidyr::gather(
        mydata,
        key = variable,
        value = value,
        pollutant,
        factor_key = TRUE
      )
      # now set pollutant to "value"
      pollutant <- "value"
      type <- "variable"
    }

    # add extra wds - reduces discontinuity at 0/360
    zero.wd <- subset(mydata, wd == 360)

    if (nrow(zero.wd) > 0) {
      zero.wd$wd <- 0
      mydata <- rbind(mydata, zero.wd)
    }

    # remove NAs
    mydata <- stats::na.omit(mydata)
    mydata <- cutData(mydata, type, ...)

    # convert to local time
    if (!is.null(local.tz)) {
      attr(mydata$date, "tzone") <- local.tz
    }

    # for resolution of grid plotting (default = 0.2; fine = 0.1)
    if (resolution == "normal") {
      int <- 0.2
    }
    if (resolution == "fine") {
      int <- 0.1
    }
    if (resolution == "ultra.fine") {
      int <- 0.05
    } # very large files!

    len.int <- 20 / int + 1 # number of x and y points to make up surfacexb

    # for CPF
    Pval <- stats::quantile(
      mydata[[pollutant]],
      probs = percentile / 100,
      na.rm = TRUE
    )

    if (statistic == "cpf") {
      sub <- paste0(
        "CPF probability at the ",
        percentile,
        "th percentile (=",
        round(Pval, 1),
        ")"
      )
    } else {
      sub <- NULL
    }

    prepare.grid <- function(mydata) {
      # for padding to beginning of first year, end of last year
      if (date.pad) {
        min.year <- as.numeric(format(min(mydata$date, na.rm = TRUE), "%Y"))
        max.year <- as.numeric(format(max(mydata$date, na.rm = TRUE), "%Y"))

        all.dates <- data.frame(
          date = seq(
            ISOdate(min.year, 1, 1, 0, 0, 0, tz = "GMT"),
            ISOdate(max.year, 12, 31, 23, 0, 0, tz = "GMT"),
            by = "hour"
          )
        )

        all.dates <- data.frame(date = all.dates)
      }

      # different date components, others available
      if (period == "trend") {
        if (date.pad) {
          # for new limits with padding
          day <- as.numeric(format(all.dates$date, "%j"))
          year <- as.numeric(format(all.dates$date, "%Y"))
          trend2 <- year + day / 366
          min.trend <- min(trend2, na.rm = TRUE)
          max.trend <- max(trend2, na.rm = TRUE)

          # actual data
          day <- as.numeric(format(mydata$date, "%j"))
          year <- as.numeric(format(mydata$date, "%Y"))
          trend <- year + day / 366
        } else {
          year <- as.numeric(format(mydata$date, "%Y"))
          day <- as.numeric(format(mydata$date, "%j"))
          trend <- year + day / 366
          min.trend <- min(trend, na.rm = TRUE)
          max.trend <- max(trend, na.rm = TRUE)
        }
      }

      if (period == "weekday") {
        hour <- as.numeric(format(mydata$date, "%H"))
        weekday <- as.numeric(format(mydata$date, "%w"))
        trend <- weekday + hour / 23
        min.trend <- 0
        max.trend <- 7
      }

      if (period == "season") {
        week <- as.numeric(format(mydata$date, "%W"))
        trend <- week
        min.trend <- 0
        max.trend <- 53
      }

      if (period == "hour") {
        hour <- as.numeric(format(mydata$date, "%H"))
        trend <- hour
        min.trend <- 0
        max.trend <- 23
      }

      trend <- 10 * (trend - min.trend) / (max.trend - min.trend)

      mydata <- cbind(mydata, trend)

      time.seq <- seq(0, 10, length = 24)

      wd <- seq(from = 0, to = 360, 10) # wind directions from 10 to 360
      ws.wd <- expand.grid(time.seq = time.seq, wd = wd)

      # identify which ws and wd bins the data belong
      wd.cut <- cut(mydata$wd, seq(0, 360, length = 38), include.lowest = TRUE)

      # divide-up the data for the annulus
      time.cut <- cut(
        mydata$trend,
        seq(0, 10, length = 25),
        include.lowest = TRUE
      )

      binned <- switch(
        statistic,
        frequency = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            length(stats::na.omit(x))
          }
        ),
        mean = tapply(mydata[, pollutant], list(time.cut, wd.cut), function(x) {
          mean(x, na.rm = TRUE)
        }),
        median = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            stats::median(x, na.rm = TRUE)
          }
        ),
        max = tapply(mydata[, pollutant], list(time.cut, wd.cut), function(x) {
          max(x, na.rm = TRUE)
        }),
        stdev = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            stats::sd(x, na.rm = TRUE)
          }
        ),
        cpf = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) (length(which(x > Pval)) / length(x))
        ),
        weighted.mean = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) (mean(x) * length(x) / nrow(mydata))
        ),
        percentile = tapply(
          mydata[, pollutant],
          list(time.cut, wd.cut),
          function(x) {
            stats::quantile(x, probs = percentile / 100, na.rm = TRUE)
          }
        )
      )

      binned <- as.vector(binned)

      # frequency - remove points with freq < min.bin
      bin.len <- tapply(mydata[, pollutant], list(time.cut, wd.cut), length)
      binned.len <- as.vector(bin.len)

      ids <- which(binned.len < min.bin)
      binned[ids] <- NA

      # data to predict over
      time.seq <- ws.wd$time.seq
      wd <- ws.wd$wd

      input.data <- expand.grid(
        time.seq = seq(0, 10, length = len.int),
        wd = seq(0, 360, length = len.int)
      )
      # Smoothing

      # run GAM to make a smooth surface
      if (force.positive) {
        n <- 0.5
      } else {
        n <- 1
      }

      input <- data.frame(binned, time.seq, wd)

      # note use of cyclic smooth for the wind direction component
      Mgam <- mgcv::gam(
        binned^n ~ te(time.seq, wd, k = k, bs = c("tp", "cc")),
        data = input
      )

      pred <- mgcv::predict.gam(Mgam, input.data)
      pred <- pred^(1 / n)

      input.data <- cbind(input.data, pred)

      if (exclude.missing) {
        # exclude predictions too far from data (from mgcv)
        x <- seq(0, 10, length = len.int)
        y <- seq(0, 360, length = len.int)
        res <- len.int
        wsp <- rep(x, res)
        wdp <- rep(y, rep(res, res))

        # data with gaps caused by min.bin
        all.data <- stats::na.omit(data.frame(
          time.seq = ws.wd$time.seq,
          wd = ws.wd$wd,
          binned
        ))
        ind <- with(
          all.data,
          mgcv::exclude.too.far(wsp, wdp, time.seq, wd, dist = 0.03)
        )

        input.data$pred[ind] <- NA
      }

      # back-transform time axis to original scale
      input.data$time.seq <- min.trend +
        (input.data$time.seq / 10) * (max.trend - min.trend)

      if (period == "trend") {
        year_part <- floor(input.data$time.seq)
        day_part <- round((input.data$time.seq - year_part) * 366)
        input.data$time.seq <- as.Date(
          paste(year_part, day_part, sep = "-"),
          format = "%Y-%j"
        )
      }

      names(input.data)[names(input.data) == "time.seq"] <- period

      input.data
    }

    # create grid for each type and bind together
    results.grid <- map_type(
      mydata,
      type,
      prepare.grid,
      .include_default = TRUE
    ) |>
      dplyr::tibble()

    # normalise by divining by mean conditioning value if needed
    if (normalise) {
      results.grid <-
        dplyr::mutate(
          results.grid,
          pred = .data$pred / mean(.data$pred, na.rm = TRUE),
          .by = dplyr::all_of(type)
        )

      if (missing(key.footer)) key.footer <- "normalised level"
    }

    # plotting
    thePlot <-
      ggplot2::ggplot(
        results.grid,
        ggplot2::aes(x = .data$wd, y = .data[[period]])
      ) +
      ggplot2::geom_tile(
        ggplot2::aes(fill = .data$pred, colour = .data$pred),
        na.rm = TRUE,
        show.legend = TRUE
      ) +
      ggplot2::ggproto(
        NULL,
        ggplot2::coord_radial(
          inner.radius = offset / 100,
          r.axis.inside = angle.scale,
          clip = "off"
        ),
        inner_radius = c(offset / 100, 1) * 0.475
      ) +
      scale_x_compass() +
      theme_openair_radial(key.position, panel.ontop = TRUE) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(
          colour = "grey50",
          linetype = 2,
          linewidth = 0.25
        )
      ) +
      ggplot2::scale_fill_gradientn(
        colours = openColours(cols),
        aesthetics = c("colour", "fill"),
        limits = limits,
        breaks = scales::breaks_pretty(6),
        na.value = col.na
      ) +
      ggplot2::labs(
        x = extra.args$xlab,
        y = extra.args$ylab,
        title = extra.args$main,
        caption = sub,
        colour = quickText(
          paste(
            key.header,
            key.footer,
            sep = ifelse(key.position %in% c("top", "bottom"), " ", "\n")
          ),
          auto.text = auto.text
        ),
        fill = quickText(
          paste(
            key.header,
            key.footer,
            sep = ifelse(key.position %in% c("top", "bottom"), " ", "\n")
          ),
          auto.text = auto.text
        )
      ) +
      annotate_compass_points(
        size = if (is.null(extra.args$fontsize)) 3 else extra.args$fontsize / 3
      ) +
      get_facet(
        type = type,
        extra.args = extra.args,
        scales = "fixed",
        auto.text = auto.text,
        drop = FALSE,
        strip.position = strip.position
      ) +
      ggplot2::guides(
        r = ggplot2::guide_axis(check.overlap = TRUE)
      )

    # set y axis breaks and labels
    if (period == "trend") {
      # let ggplot2 decide - creates nice breaks regardless of width
      thePlot <-
        thePlot +
        ggplot2::scale_y_date(
          oob = scales::oob_keep,
          expand = ggplot2::expansion(c(0, 0))
        )
    } else {
      if (period == "hour") {
        y_breaks <- seq(0, 23, length.out = 7)
        y_labels <- seq(0, 24, length.out = 7)
      }

      if (period == "weekday") {
        # a Sunday-Saturday sequence
        weekdays <-
          format(
            seq(as.Date("2000-01-02"), by = "day", length.out = 7),
            "%A"
          )

        middle_days <- !weekdays %in% c(weekdays[1], weekdays[length(weekdays)])
        weekdays[middle_days] <- substr(weekdays[middle_days], 1, 1)

        y_breaks <- seq(0, 7, length.out = 8)
        y_breaks <- y_breaks[-length(y_breaks)]
        y_labels <- weekdays
      }

      if (period == "season") {
        months <- format(ISOdate(2000, 1:12, 1), "%B")
        middle_months <- !months %in% c(months[1], months[length(months)])
        months[middle_months] <- substr(months[middle_months], 1, 1)

        y_breaks <- seq(0, 53, length.out = 13)
        y_breaks <- y_breaks[-length(y_breaks)]
        y_labels <- months
      }

      thePlot <-
        thePlot +
        ggplot2::scale_y_continuous(
          oob = scales::oob_keep,
          expand = ggplot2::expansion(c(0, 0)),
          breaks = y_breaks,
          labels = y_labels
        )
    }

    # make key full width/height
    if (key.position %in% c("left", "right")) {
      thePlot <- thePlot +
        ggplot2::theme(
          legend.key.height = ggplot2::unit(1, "null"),
          legend.key.spacing.y = ggplot2::unit(0, "cm")
        )
    }
    if (key.position %in% c("top", "bottom")) {
      thePlot <- thePlot +
        ggplot2::theme(
          legend.key.width = ggplot2::unit(1, "null"),
          legend.key.spacing.x = ggplot2::unit(0, "cm")
        )
    }

    # outputs
    if (plot) {
      plot(thePlot)
    }
    output <- list(plot = thePlot, data = results.grid, call = match.call())
    class(output) <- "openair"

    invisible(output)
  }
