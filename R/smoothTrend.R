#' Calculate nonparametric smooth trends
#'
#' Use non-parametric methods to calculate time series trends
#'
#' The [smoothTrend()] function provides a flexible way of estimating the trend
#' in the concentration of a pollutant or other variable. Monthly mean values
#' are calculated from an hourly (or higher resolution) or daily time series.
#' There is the option to deseasonalise the data if there is evidence of a
#' seasonal cycle.
#'
#' [smoothTrend()] uses a Generalized Additive Model (GAM) from the
#' [mgcv::gam()] package to find the most appropriate level of smoothing. The
#' function is particularly suited to situations where trends are not monotonic
#' (see discussion with [TheilSen()] for more details on this). The
#' [smoothTrend()] function is particularly useful as an exploratory technique
#' e.g. to check how linear or non-linear trends are.
#'
#' 95% confidence intervals are shown by shading. Bootstrap estimates of the
#' confidence intervals are also available through the `simulate` option.
#' Residual resampling is used.
#'
#' Trends can be considered in a very wide range of ways, controlled by setting
#' `type` - see examples below.
#'
#' @inheritParams timePlot
#' @inheritParams timeAverage
#'
#' @param deseason Should the data be de-deasonalized first? If `TRUE` the
#'   function `stl` is used (seasonal trend decomposition using loess). Note
#'   that if `TRUE` missing data are first imputed using a Kalman filter and
#'   Kalman smooth.
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
#'   Type can be up length two e.g. `type = c("season", "weekday")` will produce
#'   a 2x2 plot split by season and day of the week. Note, when two types are
#'   provided the first forms the columns and the second the rows.
#' @param statistic Statistic used for calculating monthly values. Default is
#'   `"mean"`, but can also be `"percentile"`. See [timeAverage()] for more
#'   details.
#' @param percentile Percentile value(s) to use if `statistic = "percentile"` is
#'   chosen. Can be a vector of numbers e.g. `percentile = c(5, 50, 95)` will
#'   plot the 5th, 50th and 95th percentile values together on the same plot.
#' @param simulate Should simulations be carried out to determine the
#'   Mann-Kendall tau and p-value. The default is `FALSE`. If `TRUE`, bootstrap
#'   simulations are undertaken, which also account for autocorrelation.
#' @param n Number of bootstrap simulations if `simulate = TRUE`.
#' @param autocor Should autocorrelation be considered in the trend uncertainty
#'   estimates? The default is `FALSE`. Generally, accounting for
#'   autocorrelation increases the uncertainty of the trend estimate sometimes
#'   by a large amount.
#' @param shade The colour used for marking alternate years. Use `"white"` or
#'   `"transparent"` to remove shading.
#' @param x.relation,y.relation This determines how the x- and y-axis scales are
#'   plotted. `"same"` ensures all panels use the same scale and `"free"` will
#'   use panel-specific scales. The latter is a useful setting when plotting
#'   data with very different values.
#' @param ci Should confidence intervals be plotted? The default is `TRUE`.
#' @param alpha The alpha transparency of shaded confidence intervals - if
#'   plotted. A value of 0 is fully transparent and 1 is fully opaque.
#' @param k This is the smoothing parameter used by the [mgcv::gam()] function
#'   in package `mgcv`. By default it is not used and the amount of smoothing is
#'   optimised automatically. However, sometimes it is useful to set the
#'   smoothing amount manually using `k`.
#' @param ... Other graphical parameters are passed onto [cutData()] and
#'   [lattice::xyplot()]. For example, [smoothTrend()] passes the option
#'   `hemisphere = "southern"` on to [cutData()] to provide southern (rather
#'   than default northern) hemisphere handling of `type = "season"`. Similarly,
#'   common graphical arguments, such as `xlim` and `ylim` for plotting ranges
#'   and `pch` and `cex` for plot symbol type and size, are passed on
#'   [lattice::xyplot()], although some local modifications may be applied by
#'   openair. For example, axis and title labelling options (such as `xlab`,
#'   `ylab` and `main`) are passed to [lattice::xyplot()] via [quickText()] to
#'   handle routine formatting. One special case here is that many graphical
#'   parameters can be vectors when used with `statistic = "percentile"` and a
#'   vector of `percentile` values, see examples below.
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @family time series and trend functions
#' @examples
#' # trend plot for nox
#' smoothTrend(mydata, pollutant = "nox")
#'
#' # trend plot by each of 8 wind sectors
#' \dontrun{
#' smoothTrend(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")
#'
#' # several pollutants, no plotting symbol
#' smoothTrend(mydata, pollutant = c("no2", "o3", "pm10", "pm25"), pch = NA)
#'
#' # percentiles
#' smoothTrend(mydata,
#'   pollutant = "o3", statistic = "percentile",
#'   percentile = 95
#' )
#'
#' # several percentiles with control over lines used
#' smoothTrend(mydata,
#'   pollutant = "o3", statistic = "percentile",
#'   percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5)
#' )
#' }
smoothTrend <- function(
  mydata,
  pollutant = "nox",
  avg.time = "month",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  k = NULL,
  deseason = FALSE,
  simulate = FALSE,
  n = 200,
  autocor = FALSE,
  type = "default",
  cols = "brewer1",
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
  ci = TRUE,
  alpha = 0.2,
  shade = "grey95",
  plot = TRUE,
  progress = TRUE,
  ...
) {
  ## ---- Setup -----

  # Args setup
  Args <- list(...)

  # validation checks
  if (length(pollutant) > 1 && length(percentile) > 1) {
    cli::cli_warn(
      "You cannot choose multiple {.field percentiles} and {.field pollutants}; using {.field percentile} = {percentile[1]}."
    )
    percentile <- percentile[1]
  }

  # check avg.time
  rlang::arg_match(x.relation, c("same", "free"))
  rlang::arg_match(y.relation, c("same", "free"))

  # set stat to percentile if user provides some
  if (!missing(percentile)) {
    cli::cli_inform(
      "{.field percentiles} provided ({percentile}); setting {.field statistic} to 'percentile'."
    )
    statistic <- "percentile"
  }

  ## ---- Graphics and Styling ----

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  # set & preserve graphics
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(
    fontsize = current.font
  ))
  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  # Style controls w/ defaults
  Args$lty <- Args$lty %||% 1L
  Args$lwd <- Args$lwd %||% 1L
  Args$pch <- Args$pch %||% 1L
  Args$cex <- Args$cex %||% 1L
  Args$layout <- Args$layout %||% NULL

  # Global var
  xlim <- Args$xlim %||% NULL

  ## ---- Label Config ----

  # label controls
  Args$main <- quickText(Args$main %||% "", auto.text)
  Args$ylab <- quickText(
    Args$ylab %||% paste(pollutant, collapse = ", "),
    auto.text
  )
  Args$xlab <- quickText(Args$xlab %||% NULL, auto.text)

  ## ---- Time intervals & date set-up ----

  # find time interval
  # need this because if user has a data capture threshold, need to know
  # original time interval. better to do this before conditioning
  interval <- find.time.interval(mydata$date)

  # equivalent number of days, used to refine interval for month/year
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) / 24 / 3600

  # better interval, most common interval in a year
  interval <-
    dplyr::case_match(
      days,
      31 ~ "month",
      c(365, 366) ~ "year",
      .default = interval
    )

  # for overall data and graph plotting
  start.year <- startYear(mydata$date)
  end.year <- endYear(mydata$date)

  ## ---- data processing ----

  mydata <-
    prepare_smoothtrend_data(
      mydata,
      pollutant = pollutant,
      type = type,
      avg.time = avg.time,
      statistic = statistic,
      percentile = percentile,
      data.thresh = data.thresh,
      interval = interval,
      progress = progress,
      ...
    )

  # set new variables
  vars <- c(type, "variable")

  # prep data for modelling
  res <-
    mapType(
      mydata,
      type = vars,
      fun = \(df) deseason_smoothtrend_data(df, deseason = deseason),
      .include_default = TRUE
    )

  fit <-
    mapType(
      res,
      type = vars,
      fun = \(df) fit_smoothtrend_gam(df, x = "date", y = "conc", k = k, ...),
      .include_default = TRUE
    )

  class(fit$date) <- c("POSIXct", "POSIXt")

  ## ---- special layout config ----

  # special wd layout
  # (type field in results.grid called type not wd)
  if (length(type) == 1 && type[1] == "wd" && is.null(Args$layout)) {
    # re-order to make sensible layout
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    res$wd <- ordered(res$wd, levels = wds)
    # see if wd is actually there or not
    wd.ok <-
      sapply(wds, function(x) {
        if (x %in% unique(res$wd)) {
          FALSE
        } else {
          TRUE
        }
      })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    res$wd <- factor(res$wd) # remove empty factor levels
    Args$layout <- if (type == "wd") {
      c(3, 3)
    } else {
      NULL
    }
    if (!"skip" %in% names(Args)) {
      Args$skip <- skip
    }
  }
  if (!"skip" %in% names(Args)) {
    Args$skip <- FALSE
  }

  ## ---- strip & labels ----

  # proper names of labelling #
  strip.dat <- strip.fun(res, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]

  ## ---- colours & keys ----

  # colours according to number of percentiles
  npol <- max(length(percentile), length(pollutant)) # number of pollutants

  # set up colours
  myColors <- if (length(cols) == 1 && cols == "greyscale") {
    openColours(cols, npol + 1)[-1]
  } else {
    openColours(cols, npol)
  }

  # information for key
  npol <- na.omit(unique(res$variable))

  # use pollutant names or user-supplied names
  if (!missing(name.pol)) {
    key.lab <- sapply(seq_along(name.pol), function(x) {
      quickText(name.pol[x], auto.text)
    })
  } else {
    key.lab <- sapply(
      seq_along(npol),
      function(x) quickText(npol[x], auto.text)
    )
  }

  if (length(npol) > 1L) {
    if (key) {
      if (missing(key.columns) && key.position %in% c("top", "bottom")) {
        key.columns <- length(npol)
      }

      key <- list(
        lines = list(
          col = myColors[seq_along(npol)],
          lty = Args$lty,
          lwd = Args$lwd,
          pch = Args$pch,
          type = "b",
          cex = Args$cex
        ),
        text = list(lab = key.lab),
        space = key.position,
        columns = key.columns
      )
    } else {
      key <- NULL
    }

    # use pollutant names or user-supplied names
    if (!"ylab" %in% names(Args)) {
      if (!missing(name.pol)) {
        Args$ylab <- quickText(paste(name.pol, collapse = ", "), auto.text)
      } else {
        Args$ylab <- quickText(paste(pollutant, collapse = ", "), auto.text)
      }
    }
  } else {
    key <- NULL # either there is a key or there is not
  }

  ## ---- plot structure & formula ----

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("conc ~ date| ", temp, sep = ""))

  if (is.null(xlim)) {
    if (x.relation == "free") {
      xlim <-
        purrr::map(
          split(mydata, mydata[type], drop = TRUE),
          function(x) {
            gap <- difftime(max(x$date), min(x$date), units = "secs") / 80
            xlim <- range(x$date) + c(-1 * gap, gap)
            xlim
          }
        )
    } else {
      gap <- difftime(max(mydata$date), min(mydata$date), units = "secs") / 80
      xlim <- range(mydata$date) + c(-1 * gap, gap)
    }
  }

  # date formatting for plot
  if (x.relation == "free") {
    date.at <-
      purrr::map(
        split(mydata, mydata[type], drop = TRUE),
        function(x) dateBreaks(x$date, date.breaks)$major
      )
    if (type == "default") {
      date.at <- date.at[[1]]
    }
  } else {
    date.at <- dateBreaks(mydata$date, date.breaks)$major
  }

  if (is.null(date.format)) {
    if (x.relation == "free") {
      date.format <-
        dateBreaks(
          split(mydata, mydata[type], drop = TRUE)[[1]]$date,
          date.breaks
        )$format
    } else {
      date.format <- dateBreaks(mydata$date, date.breaks)$format
    }
  }

  ## ---- create plot ----

  xyplot.args <- list(
    x = myform,
    data = res,
    groups = res$variable,
    as.table = TRUE,
    strip = strip,
    strip.left = strip.left,
    key = key,
    xlim = xlim,
    par.strip.text = list(cex = 0.8),
    scales = list(
      x = list(relation = x.relation, at = date.at, format = date.format),
      y = list(relation = y.relation, rot = 0)
    ),
    panel = panel.superpose,
    panel.groups = function(
      x,
      y,
      group.number,
      lwd,
      lty,
      pch,
      col,
      col.line,
      col.symbol,
      subscripts,
      type = "b",
      ...
    ) {
      if (group.number == 1) {
        panel.grid(-1, 0)
      }

      panel.xyplot(
        x,
        y,
        type = "b",
        lwd = lwd,
        lty = lty,
        pch = pch,
        col.line = myColors[group.number],
        col.symbol = myColors[group.number],
        ...
      )

      panel.gam(
        x,
        y,
        col = myColors[group.number],
        k = k,
        myColors[group.number],
        simulate = simulate,
        n.sim = n,
        autocor = autocor,
        lty = 1,
        lwd = 1,
        se = ci,
        ...
      )

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

  newdata <- res
  output <- list(
    plot = plt,
    data = list(data = newdata, fit = fit),
    call = match.call()
  )
  class(output) <- "openair"

  # output #
  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    } else {
      plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    }
  }

  invisible(output)
}

#' Prepares smoothtrend data - check, cut, timeavg, reshape
#' @noRd
prepare_smoothtrend_data <- function(
  mydata,
  pollutant,
  type,
  avg.time,
  statistic,
  percentile,
  data.thresh,
  interval,
  progress,
  ...
) {
  # default checks
  vars <- c("date", pollutant)
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  # cutData depending on type
  mydata <- cutData(mydata, type, ...)

  # reshape data, make sure there is not a variable called 'variable'
  if ("variable" %in% names(mydata)) {
    mydata <- dplyr::rename(mydata, c(variable = "theVar"))
    type <- "theVar"
  }

  # in the case of multiple percentiles, these are assinged and treated
  # like multiple pollutants
  mydata <- tidyr::pivot_longer(
    mydata,
    cols = pollutant,
    names_to = "variable",
    values_to = "value",
    names_transform = list(
      variable = factor
    )
  )

  if (length(percentile) > 1) {
    vars <- c(type, "variable")

    prefix <- paste0(pollutant, " percentile ")

    mydata <- calcPercentile(
      mydata,
      type = vars,
      pollutant = "value",
      avg.time = avg.time,
      percentile = percentile,
      data.thresh = data.thresh,
      prefix = prefix
    )

    vars <- paste0(prefix, percentile)

    mydata <-
      tidyr::pivot_longer(
        dplyr::select(mydata, -"variable"),
        cols = dplyr::all_of(vars),
        names_to = "variable",
        values_to = "value"
      ) |>
      dplyr::arrange(.data$variable)
  } else {
    mydata <- suppressWarnings(timeAverage(
      mydata,
      type = c(type, "variable"),
      avg.time = avg.time,
      percentile = percentile,
      statistic = statistic,
      data.thresh = data.thresh,
      interval = interval,
      progress = progress
    ))
  }

  # timeAverage drops type if default
  if (length(type) == 1 && type[1] == "default") {
    mydata$default <- "default"
  }

  # return data
  mydata
}

#' Apply deaseason, if requested
#' @noRd
deseason_smoothtrend_data <- function(mydata, deseason) {
  # return if nothing to analyse
  if (all(is.na(mydata$value))) {
    return(data.frame(date = NA, conc = NA))
  }

  # sometimes data have long trailing NAs, so start and end at
  # first and last data
  min.idx <- min(which(!is.na(mydata[, "value"])))
  max.idx <- max(which(!is.na(mydata[, "value"])))
  mydata <- mydata[min.idx:max.idx, ]

  # these subsets may have different dates to overall
  start.year <- startYear(mydata$date)
  end.year <- endYear(mydata$date)
  start.month <- startMonth(mydata$date)
  end.month <- endMonth(mydata$date)

  # can't deseason less than 2 years of data
  if (nrow(mydata) <= 24) {
    deseason <- FALSE
  }

  if (deseason) {
    myts <- ts(
      mydata[["value"]],
      start = c(start.year, start.month),
      end = c(end.year, end.month),
      frequency = 12
    )

    # fill any missing data using a Kalman filter

    if (any(is.na(myts))) {
      # use forecast package to get best arima
      fit <- ts(rowSums(tsSmooth(StructTS(myts))[, -2]))
      id <- which(is.na(myts))

      myts[id] <- fit[id]
    }

    ssd <- stl(myts, s.window = 11, robust = TRUE, s.degree = 1)

    deseas <- ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]
    deseas <- as.vector(deseas)

    results <- data.frame(
      date = mydata$date,
      conc = as.vector(deseas),
      stringsAsFactors = FALSE
    )
  } else {
    results <- data.frame(
      date = mydata$date,
      conc = mydata[["value"]],
      stringsAsFactors = FALSE
    )
  }

  results
}

#' Fit a GAM for smoothTrend
#' @noRd
fit_smoothtrend_gam <- function(
  thedata,
  x = "date",
  y = "conc",
  form = y ~ x,
  k = k,
  Args,
  ...,
  simulate = FALSE,
  n.sim = 200,
  autocor = FALSE,
  se = TRUE,
  level = 0.95,
  n = 100
) {
  ## panel function to add a smooth line to a plot
  ## Uses a GAM (mgcv) to fit smooth
  ## Optionally can plot 95% confidence intervals and run bootstrap simulations
  ## to estimate uncertainties. Simple block bootstrap is also available for correlated data

  data.orig <- thedata ## return this if all else fails

  id <- which(names(thedata) == x)
  names(thedata)[id] <- "x"
  id <- which(names(thedata) == y)
  names(thedata)[id] <- "y"

  # can only fit numeric, so convert back after fitting
  class_x <- class(thedata$x)

  thedata$x <- as.numeric(thedata$x)

  tryCatch(
    {
      if (!simulate) {
        if (is.null(k)) {
          mod <- suppressWarnings(mgcv::gam(
            y ~ s(x),
            select = TRUE,
            data = thedata,
            ...
          ))
        } else {
          mod <- suppressWarnings(mgcv::gam(
            y ~ s(x, k = k),
            select = TRUE,
            data = thedata,
            ...
          ))
        }

        xseq <- seq(
          min(thedata$x, na.rm = TRUE),
          max(thedata$x, na.rm = TRUE),
          length = n
        )

        ## for uncertainties
        std <- qnorm(level / 2 + 0.5)

        pred <- predict(mod, data.frame(x = xseq), se = se)

        results <- data.frame(
          date = xseq,
          pred = pred$fit,
          lower = pred$fit - std * pred$se,
          upper = pred$fit + std * pred$se
        )
      } else {
        ## simulations required

        sam.size <- nrow(thedata)

        xseq <- seq(
          min(thedata$x, na.rm = TRUE),
          max(thedata$x, na.rm = TRUE),
          length = n
        )

        boot.pred <- matrix(nrow = sam.size, ncol = n.sim)

        message("Taking bootstrap samples. Please wait...")

        ## set up bootstrap
        block.length <- 1

        if (autocor) {
          block.length <- round(sam.size^(1 / 3))
        }
        index <- samp.boot.block(sam.size, n.sim, block.length)

        ## predict first
        if (is.null(k)) {
          mod <- mgcv::gam(y ~ s(x), data = thedata, ...)
        } else {
          mod <- mgcv::gam(y ~ s(x, k = k), data = thedata, ...)
        }

        residuals <- residuals(mod) ## residuals of the model

        pred.input <- predict(mod, thedata)

        for (i in 1:n.sim) {
          ## make new data
          new.data <- data.frame(
            x = xseq,
            y = pred.input + residuals[index[, i]]
          )

          mod <- mgcv::gam(y ~ s(x), data = new.data, ...)

          pred <- predict(mod, new.data)

          boot.pred[, i] <- as.vector(pred)
        }

        ## calculate percentiles
        percentiles <- apply(
          boot.pred,
          1,
          function(x) quantile(x, probs = c(0.025, 0.975))
        )

        results <- as.data.frame(cbind(
          pred = rowMeans(boot.pred),
          lower = percentiles[1, ],
          upper = percentiles[2, ]
        ))
      }

      # convert class back to original
      class(results[[x]]) <- class_x
      return(results)
    },
    error = function(x) {
      data.orig
    }
  )
}
