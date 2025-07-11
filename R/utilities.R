## TODO: Add comment
#
## Author: David Carslaw
## useful utility functions
## with some updates and modification by Karl Ropkins
###############################################################################

startYear <- function(dat) as.numeric(format(min(dat[order(dat)]), "%Y"))
endYear <- function(dat) as.numeric(format(max(dat[order(dat)]), "%Y"))
startMonth <- function(dat) as.numeric(format(min(dat[order(dat)]), "%m"))
endMonth <- function(dat) as.numeric(format(max(dat[order(dat)]), "%m"))

## these are pre-defined type that need a field "date"; used by cutData
dateTypes <- c(
  "year",
  "hour",
  "month",
  "season",
  "weekday",
  "weekend",
  "monthyear",
  "gmtbst",
  "bstgmt",
  "dst",
  "daylight",
  "week",
  "seasonyear",
  "yearseason"
)

## sets up how openair graphics look by default and resets on exit

setGraphics <- function(fontsize = 5) {
  current.strip <- trellis.par.get("strip.background")
  trellis.par.set(fontsize = list(text = fontsize))

  ## reset graphic parameters
  font.orig <- trellis.par.get("fontsize")$text
  on.exit(trellis.par.set(
    fontsize = list(text = font.orig)
  ))
}


# function to test of a suggested package is available and warn if not
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }

  stop(
    "Package `",
    package,
    "` required for `",
    fun,
    "`.\n",
    "Please install and try again.",
    call. = FALSE
  )
}

###############################################################################

## function to find averaging period of data, returns "xx sec"
## for use in filling in gaps in time series data
## it finds the table of values of time gaps and picks the biggest
## can't think of better way unless user specifies what the time interval is meant to be

find.time.interval <- function(dates) {
  ## could have several sites, dates may be unordered
  ## find the most common time gap in all the data
  dates <- unique(dates) ## make sure they are unique

  # work out the most common time gap of unique, ordered dates
  id <- which.max(table(diff(as.numeric(unique(dates[order(dates)])))))
  seconds <- as.numeric(names(id))

  if ("POSIXt" %in% class(dates)) {
    seconds <- paste(seconds, "sec")
  }

  if (class(dates)[1] == "Date") {
    seconds <- seconds * 3600 * 24
    seconds <- paste(seconds, "sec")
  }

  seconds
}

## #################################################################
# Function to pad out missing time data
# assumes data have already been split by type, so just take first
# tries to work out time interval of input based on most common gap
# can print assumed gap to screen

date.pad <- function(mydata, type = NULL, print.int = FALSE) {
  # if one line, just return
  if (nrow(mydata) < 2) {
    return(mydata)
  }

  ## time zone of data
  TZ <- attr(mydata$date, "tzone")
  if (is.null(TZ)) {
    TZ <- "GMT"
  } ## as it is on Windows for BST

  ## function to fill missing data gaps
  ## assume no missing data to begin with

  ## pad out missing data
  start.date <- min(mydata$date, na.rm = TRUE)
  end.date <- max(mydata$date, na.rm = TRUE)

  ## interval in seconds
  interval <- find.time.interval(mydata$date)

  ## equivalent number of days, used to refine interval for month/year
  days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
    24 /
    3600

  ## find time interval of data
  if (class(mydata$date)[1] == "Date") {
    interval <- paste(days, "day")
  } else {
    ## this will be in seconds
    interval <- find.time.interval(mydata$date)
  }

  ## better interval, most common interval in a year
  if (days == 31) {
    interval <- "month"
  }
  if (days %in% c(365, 366)) {
    interval <- "year"
  }

  ## only pad if there are missing data
  if (length(unique(diff(mydata$date))) != 1L) {
    all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
    mydata <- mydata %>% full_join(all.dates, by = "date")

    # add missing types - if type is present
    if (!is.null(type)) {
      mydata[type] <- mydata[1, type]
    }
  }

  ## return the same TZ that we started with
  attr(mydata$date, "tzone") <- TZ

  if (print.int) {
    message("Input data time interval assumed is ", interval)
  }

  # make sure date-sorted
  mydata <- arrange(mydata, date)

  mydata
}
#############################################################################################

## unitility function to convert decimal date to POSIXct
decimalDate <- function(x, date = "date") {
  thedata <- x
  x <- x[, date]
  x.year <- floor(x)
  ## fraction of the year
  x.frac <- x - x.year
  ## number of seconds in each year
  x.sec.yr <- unclass(ISOdate(x.year + 1, 1, 1, 0, 0, 0)) -
    unclass(ISOdate(x.year, 1, 1, 0, 0, 0))
  ## now get the actual time
  x.actual <- ISOdate(x.year, 1, 1, 0, 0, 0) + x.frac * x.sec.yr
  x.actual <- as.POSIXct(trunc(x.actual, "hours"), "GMT")
  thedata$date <- x.actual
  thedata
}

convert.date <- function(mydata, format = "%d/%m/%Y %H:%M") {
  mydata$date <- as.POSIXct(strptime(mydata$date, format = format), "GMT")
  mydata
}


#############################################################################################

## from Deepayan Sarkar
panel.smooth.spline <-
  function(
    x,
    y,
    w = NULL,
    df,
    spar = NULL,
    cv = FALSE,
    lwd = lwd,
    lty = plot.line$lty,
    col,
    col.line = plot.line$col,
    type,
    horizontal = FALSE,
    all.knots = TRUE,
    ...
  ) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) {
      return()
    }
    if (!missing(col)) {
      if (missing(col.line)) {
        col.line <- col
      }
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal) {
      spline <-
        smooth.spline(
          y[ok],
          x[ok],
          w = w,
          df = df,
          spar = spar,
          cv = cv
        )
      panel.lines(
        x = spline$y,
        y = spline$x,
        col = col.line,
        lty = lty,
        lwd = lwd,
        ...
      )
    } else {
      spline <-
        smooth.spline(
          x[ok],
          y[ok],
          w = w,
          df = df,
          spar = spar,
          cv = cv
        )
      panel.lines(
        x = spline$x,
        y = spline$y,
        col = col.line,
        lty = lty,
        lwd = lwd,
        ...
      )
    }
  }

### panel functions for plots based on lattice ####################################################

panel.gam <- function(
  x,
  y,
  form = y ~ x,
  method = "loess",
  k = k,
  Args,
  ...,
  simulate = FALSE,
  n.sim = 200,
  autocor = FALSE,
  se = TRUE,
  level = 0.95,
  n = 100,
  col = plot.line$col,
  col.se = col,
  lty = plot.line$lty,
  lwd = plot.line$lwd,
  alpha = plot.line$alpha,
  alpha.se = 0.20,
  border = NA,
  subscripts,
  group.number,
  group.value,
  type,
  col.line,
  col.symbol,
  fill,
  pch,
  cex,
  font,
  fontface,
  fontfamily
) {
  ## panel function to add a smooth line to a plot
  ## Uses a GAM (mgcv) to fit smooth
  ## Optionally can plot 95% confidence intervals and run bootstrap simulations
  ## to estimate uncertainties. Simple block bootstrap is also available for correlated data

  ## get rid of R check annoyances#
  plot.line <- NULL

  thedata <- data.frame(x = x, y = y)
  thedata <- na.omit(thedata)

  tryCatch(
    {
      if (!simulate) {
        if (is.null(k)) {
          mod <- suppressWarnings(gam(
            y ~ s(x),
            select = TRUE,
            data = thedata,
            ...
          ))
        } else {
          mod <- suppressWarnings(gam(
            y ~ s(x, k = k),
            select = TRUE,
            data = thedata,
            ...
          ))
        }

        lims <- current.panel.limits()
        xrange <- c(
          max(min(lims$x), min(x, na.rm = TRUE)),
          min(max(lims$x), max(x, na.rm = TRUE))
        )
        xseq <- seq(xrange[1], xrange[2], length = n)

        ## for uncertainties
        std <- qnorm(level / 2 + 0.5)

        pred <- predict(mod, data.frame(x = xseq), se = TRUE)

        panel.lines(
          xseq,
          pred$fit,
          col = col,
          alpha = alpha,
          lty = lty,
          lwd = 2
        )

        results <- data.frame(
          date = xseq,
          pred = pred$fit,
          lower = pred$fit - std * pred$se,
          upper = pred$fit + std * pred$se
        )

        if (se) {
          panel.polygon(
            x = c(xseq, rev(xseq)),
            y = c(
              pred$fit -
                std * pred$se,
              rev(pred$fit + std * pred$se)
            ),
            col = col.se,
            alpha = alpha.se,
            border = border
          )
          pred <- pred$fit
        }
      } else {
        ## simulations required

        x <- thedata$x
        y <- thedata$y

        sam.size <- length(x)

        lims <- current.panel.limits()
        xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
        xseq <- seq(xrange[1], xrange[2], length = sam.size)

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
          mod <- gam(y ~ s(x), data = thedata, ...)
        } else {
          mod <- gam(y ~ s(x, k = k), data = thedata, ...)
        }

        residuals <- residuals(mod) ## residuals of the model

        pred.input <- predict(mod, thedata)

        for (i in 1:n.sim) {
          ## make new data
          new.data <- data.frame(
            x = xseq,
            y = pred.input + residuals[index[, i]]
          )

          mod <- gam(y ~ s(x), data = new.data, ...)

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

        if (se) {
          panel.polygon(
            x = c(xseq, rev(xseq)),
            y = c(results$lower, rev(results$upper)),
            col = col.se,
            alpha = alpha.se,
            border = border
          )
        }

        panel.lines(
          xseq,
          pred.input,
          col = col,
          alpha = alpha,
          lty = lty,
          lwd = 2
        )
      }
      results
    },
    error = function(x) {
      return()
    }
  )
}


## version of GAM fitting not for plotting - need to rationalise both...
fitGam <- function(
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
          mod <- suppressWarnings(gam(
            y ~ s(x),
            select = TRUE,
            data = thedata,
            ...
          ))
        } else {
          mod <- suppressWarnings(gam(
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
          mod <- gam(y ~ s(x), data = thedata, ...)
        } else {
          mod <- gam(y ~ s(x, k = k), data = thedata, ...)
        }

        residuals <- residuals(mod) ## residuals of the model

        pred.input <- predict(mod, thedata)

        for (i in 1:n.sim) {
          ## make new data
          new.data <- data.frame(
            x = xseq,
            y = pred.input + residuals[index[, i]]
          )

          mod <- gam(y ~ s(x), data = new.data, ...)

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


#########################################################################################################

## error in mean from Hmisc

errorInMean <- function(
  x,
  mult = qt((1 + conf.int) / 2, n - 1),
  conf.int = 0.95,
  na.rm = TRUE
) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  if (n < 2) {
    return(c(Mean = mean(x), Lower = NA, Upper = NA))
  }
  xbar <- sum(x) / n
  se <- sqrt(sum((x - xbar)^2) / n / (n - 1))
  c(
    Mean = xbar,
    Lower = xbar - mult * se,
    Upper = xbar +
      mult *
        se
  )
}

###########################################################################################################

## list update function
## for lattice type object structure and ... handling

## (currently used by)
## (all openair plots that include colorkey controlled by drawOpenKey)

## listUpdate function
# [in development]
listUpdate <- function(
  a,
  b,
  drop.dots = TRUE,
  subset.a = NULL,
  subset.b = NULL
) {
  if (drop.dots) {
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if (!is.null(subset.a)) {
    a <- a[names(a) %in% subset.a]
  }
  if (!is.null(subset.b)) {
    b <- b[names(b) %in% subset.b]
  }
  if (length(names(b) > 0)) {
    a <- modifyList(a, b)
  }
  a
}

#############################################################################################################

## makeOpenKeyLegend v0.1

## common code for making legend list
## objects for use with drawOpenkey outputs

## uses listUpdate in utilities

makeOpenKeyLegend <- function(key, default.key, fun.name = "function") {
  # handle logicals and lists
  if (is.logical(key)) {
    legend <- if (key) default.key else NULL
  } else if (is.list(key)) {
    legend <- listUpdate(default.key, key)
  } else {
    if (!is.null(key)) {
      warning(
        paste(
          "In ",
          fun.name,
          "(...):\n unrecognised key not exported/applied\n",
          " [see ?drawOpenKey for key structure/options]",
          sep = ""
        ),
        call. = FALSE
      )
    }
    legend <- NULL
  }

  # structure like legend for drawOpenKey
  if (!is.null(legend)) {
    legend <- list(
      right = list(
        fun = drawOpenKey,
        args = list(key = legend),
        draw = FALSE
      )
    )
    if ("space" %in% names(legend$right$args$key)) {
      names(legend)[[1]] <- legend$right$args$key$space
    }
  }
  legend
}

## polygon that can deal with missing data for use in lattice plots with groups
poly.na <- function(
  x1,
  y1,
  x2,
  y2,
  group.number,
  myColors,
  alpha = 0.4,
  border = NA
) {
  for (i in seq(2, length(x1))) {
    if (!any(is.na(y2[c(i - 1, i)]))) {
      lpolygon(
        c(x1[i - 1], x1[i], x2[i], x2[i - 1]),
        c(y1[i - 1], y1[i], y2[i], y2[i - 1]),
        col = myColors[group.number],
        border = border,
        alpha = alpha
      )
    }
  }
}


## gives names of lattice strips
strip.fun <- function(results.grid, type, auto.text) {
  ## proper names of labelling ###################################################
  pol.name <- sapply(
    levels(factor(results.grid[[type[1]]])),
    function(x) quickText(x, auto.text)
  )
  strip <- strip.custom(factor.levels = pol.name)

  if (length(type) == 1) {
    strip.left <- FALSE
  } else {
    ## two conditioning variables

    pol.name <- sapply(
      levels(factor(results.grid[[type[2]]])),
      function(x) quickText(x, auto.text)
    )
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  if (length(type) == 1 & type[1] == "default") {
    strip <- FALSE
  } ## remove strip
  list(strip, strip.left, pol.name)
}


## from lattice
chooseFace <- function(fontface = NULL, font = 1) {
  if (is.null(fontface)) {
    font
  } else {
    fontface
  }
}


## .smoothScatterCalcDensity() is also in graphics, but not exported.
.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x) {
  if (!("KernSmooth" %in% loadedNamespaces())) {
    ns <- try(loadNamespace("KernSmooth"))
    if (isNamespace(ns)) {
      message("(loaded the KernSmooth namespace)")
    } else {
      stop(
        "panel.smoothScatter() requires the KernSmooth package, but unable to load KernSmooth namespace"
      )
    }
  }
  if (length(nbin) == 1) {
    nbin <- c(nbin, nbin)
  }
  if (!is.numeric(nbin) || (length(nbin) != 2)) {
    stop("'nbin' must be numeric of length 1 or 2")
  }
  if (missing(bandwidth)) {
    bandwidth <- diff(apply(
      x,
      2,
      quantile,
      probs = c(0.05, 0.95),
      na.rm = TRUE
    )) /
      25
  } else {
    if (!is.numeric(bandwidth)) stop("'bandwidth' must be numeric")
  }
  bandwidth[bandwidth == 0] <- 1
  ## create density map
  if (missing(range.x)) {
    rv <- KernSmooth::bkde2D(x, gridsize = nbin, bandwidth = bandwidth)
  } else {
    rv <- KernSmooth::bkde2D(
      x,
      gridsize = nbin,
      bandwidth = bandwidth,
      range.x = range.x
    )
  }
  rv$bandwidth <- bandwidth
  return(rv)
}


## simple rounding function from plyr
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

## pretty gap calculator
prettyGap <- function(x, n = 100) {
  return(diff(pretty(x, n))[1])
}

# function to check variables are numeric, if not force with warning
checkNum <- function(mydata, vars) {
  for (i in seq_along(vars)) {
    if (!is.numeric(mydata[[vars[i]]])) {
      mydata[[vars[i]]] <- as.numeric(as.character(mydata[[vars[i]]]))

      warning(
        paste(vars[i], "is not numeric, forcing to numeric..."),
        call. = FALSE
      )
    }
  }

  return(mydata)
}

#' Function to check if duplicate dates are present in mydata by type
#' @param mydata Data input
#' @param type `type` from parent function
#' @param fn One of `cli::cli_warn` or `cli::cli_abort`
#' @noRd
checkDuplicateRows <- function(mydata, type = NULL, fn = cli::cli_warn) {
  if (is.null(type)) {
    flag <- length(mydata$date) != length(unique(mydata$date))
  } else {
    flag <-
      split(mydata, mydata[type], drop = TRUE) %>%
      purrr::map_vec(function(x) {
        dates <- x$date
        unique_dates <- unique(x$date)
        length(dates) != length(unique_dates)
      }) %>%
      any()
  }

  if (flag) {
    fn(
      c(
        "!" = "Duplicate dates detected in mydata{.field $date}.",
        "i" = 'Are there multiple sites in {.code mydata}? Use the {.field type} argument to condition them separately.'
      ),
      call = NULL
    )
  }
}
