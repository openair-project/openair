## The code below is modified from that supplied by Felix Andrews
## (http://n4.nabble.com/proposal-for-new-axis-Date-axis-POSIXct-td976219.html#a1462140)
## Used in openair functions that plot date/times

## Copyright (C) Felix Andrews <felix@nfrac.org>

dateBreaks <- function(x, n = 5, min.n = n %/% 2, ...) {
  ## get rid of R check annoyances

  isDate <- inherits(x, "Date")
  x <- as.POSIXct(x)
  if (isDate) {
    # the timezone *does* matter
    attr(x, "tzone") <- "GMT"
  }
  zz <- range(x, na.rm = TRUE)
  if (diff(as.numeric(zz)) == 0) {
    # one value only
    zz <- zz + c(0, 60)
  }

  ## specify the set of pretty timesteps
  MIN <- 60
  HOUR <- MIN * 60
  DAY <- HOUR * 24
  YEAR <- DAY * 365.25
  MONTH <- YEAR / 12
  steps <-
    list(
      "1 sec" = list(1, format = "%S", start = "mins"),
      "2 secs" = list(2),
      "5 secs" = list(5),
      "10 secs" = list(10),
      "15 secs" = list(15),
      "30 secs" = list(30, format = "%H:%M:%S"),
      "1 min" = list(1 * MIN, format = "%H:%M"),
      "2 mins" = list(2 * MIN, start = "hours"),
      "5 mins" = list(5 * MIN),
      "10 mins" = list(10 * MIN),
      "15 mins" = list(15 * MIN),
      "30 mins" = list(30 * MIN),
      "1 hour" = list(1 * HOUR),
      "3 hours" = list(3 * HOUR, start = "days"),
      "6 hours" = list(6 * HOUR, format = "%b %d %H:%M"),
      "12 hours" = list(12 * HOUR),
      "1 DSTday" = list(1 * DAY, format = "%b %d"),
      "2 DSTdays" = list(2 * DAY),
      "1 week" = list(7 * DAY, start = "weeks"),
      "halfmonth" = list(MONTH / 2, start = "months"),
      "1 month" = list(1 * MONTH, format = "%b"),
      "3 months" = list(3 * MONTH, start = "years"),
      "6 months" = list(6 * MONTH, format = "%Y-%m"),
      "1 year" = list(1 * YEAR, format = "%Y"),
      "2 years" = list(2 * YEAR, start = "decades"),
      "5 years" = list(5 * YEAR),
      "10 years" = list(10 * YEAR),
      "20 years" = list(20 * YEAR, start = "centuries"),
      "50 years" = list(50 * YEAR),
      "100 years" = list(100 * YEAR),
      "200 years" = list(200 * YEAR),
      "500 years" = list(500 * YEAR),
      "1000 years" = list(1000 * YEAR)
    )
  ## carry forward 'format' and 'start' to following steps
  for (i in seq_along(steps)) {
    if (is.null(steps[[i]]$format)) {
      steps[[i]]$format <- steps[[i - 1]]$format
    }
    if (is.null(steps[[i]]$start)) {
      steps[[i]]$start <- steps[[i - 1]]$start
    }
    steps[[i]]$spec <- names(steps)[i]
  }
  ## crudely work out number of steps in the given interval
  xspan <- diff(as.numeric(zz))
  nsteps <- sapply(steps, function(s) {
    xspan / s[[1]]
  })
  init.i <- which.min(abs(nsteps - n))
  ## calculate actual number of ticks in the given interval
  calcSteps <- function(s) {
    startTime <- dateTrunc(min(zz), units = s$start) ## FIXME: should be trunc() eventually
    if (identical(s$spec, "halfmonth")) {
      at <- seq(startTime, max(zz), by = "months")
      at2 <- as.POSIXlt(at)
      at2$mday <- 15L
      at <- structure(
        sort(c(as.POSIXct(at), as.POSIXct(at2))),
        tzone = attr(at, "tzone")
      )
    } else {
      at <- seq(startTime, max(zz), by = s$spec)
    }
    at <- at[(min(zz) <= at) & (at <= max(zz))]
    at
  }
  init.at <- calcSteps(steps[[init.i]])
  init.n <- length(init.at) - 1L
  ## bump it up if below acceptable threshold
  while (init.n < min.n) {
    init.i <- init.i - 1L
    if (init.i == 0) {
      stop("range too small for min.n")
    }
    init.at <- calcSteps(steps[[init.i]])
    init.n <- length(init.at) - 1L
  }
  makeOutput <- function(at, s) {
    flabels <- format(at, s$format)
    ans <-
      #   if (isDate) as.Date(round(at, units = "days"))
      if (isDate) {
        as_date(round(at))
      } else {
        as.POSIXct(at)
      }
    attr(ans, "labels") <- flabels

    list(major = ans, format = s$format)
    #  ans
  }
  if (init.n == n) {
    ## perfect
    return(makeOutput(init.at, steps[[init.i]]))
  }
  if (init.n > n) {
    ## too many ticks
    new.i <- init.i + 1L
    new.i <- min(new.i, length(steps))
  } else {
    ## too few ticks
    new.i <- init.i - 1L
    new.i <- max(new.i, 1L)
  }
  new.at <- calcSteps(steps[[new.i]])
  new.n <- length(new.at) - 1L
  ## work out whether new.at or init.at is better
  if (new.n < min.n) {
    new.n <- -Inf
  }
  if (abs(new.n - n) < abs(init.n - n)) {
    makeOutput(new.at, steps[[new.i]])
  } else {
    makeOutput(init.at, steps[[init.i]])
  }
}
## utility function, extending the base function of same name
dateTrunc <-
  function(
    x,
    units = c(
      "secs",
      "mins",
      "hours",
      "days",
      "weeks",
      "months",
      "years",
      "decades",
      "centuries"
    ),
    start.on.monday = TRUE
  ) {
    x <- as.POSIXlt(x)
    if (units %in% c("secs", "mins", "hours", "days")) {
      return(base::trunc.POSIXt(x, units))
    }
    x <- base::trunc.POSIXt(x, "days")
    if (length(x$sec)) {
      switch(
        units,
        weeks = {
          x$mday <- x$mday - x$wday
          if (start.on.monday) {
            x$mday <- x$mday + ifelse(x$wday > 0L, 1L, -6L)
          }
        },
        months = {
          x$mday <- 1
        },
        years = {
          x$mday <- 1
          x$mon <- 0
        },
        decades = {
          x$mday <- 1
          x$mon <- 0
          x$year <- (x$year %/% 10) * 10
        },
        centuries = {
          x$mday <- 1
          x$mon <- 0
          x$year <- (x$year %/% 100) * 100
        }
      )
    }
    x
  }

dateCeil <- function(
  x,
  units = c(
    "secs",
    "mins",
    "hours",
    "days",
    "months",
    "years"
  ),
  ...
) {
  units <- match.arg(units)
  x <- as.POSIXlt(x)
  isdst <- x$isdst
  if (length(x$sec) > 0 && x != dateTrunc(x, units = units)) {
    switch(
      units,
      secs = {
        x$sec <- ceiling(x$sec)
      },
      mins = {
        x$sec <- 0
        x$min <- x$min + 1
      },
      hours = {
        x$sec <- 0
        x$min <- 0
        x$hour <- x$hour + 1
      },
      days = {
        x$sec <- 0
        x$min <- 0
        x$hour <- 0
        x$mday <- x$mday + 1
        isdst <- x$isdst <- -1
      },
      months = {
        x$sec <- 0
        x$min <- 0
        x$hour <- 0
        x$mday <- 1
        x$mon <- x$mon + 1
        isdst <- x$isdst <- -1
      },
      years = {
        x$sec <- 0
        x$min <- 0
        x$hour <- 0
        x$mday <- 1
        x$mon <- 0
        x$year <- x$year + 1
        isdst <- x$isdst <- -1
      }
    )
    x <- as.POSIXlt(as.POSIXct(x))
    if (isdst == -1) {
      x$isdst <- -1
    }
  }
  return(x)
}

## not overlu useful yet!
roundDate <- function(dates, avg.time = "hour", start.date = NULL) {
  TZ <- attr(dates, "tzone")
  if (is.null(TZ)) {
    TZ <- "GMT"
  } ## as it is on Windows for BST

  if (!missing(start.date)) {
    dates <- c(as.POSIXct(start.date), dates)
  }

  dates <- cut(dates, avg.time)
  dates <- as.POSIXct(dates, TZ)
  attr(dates, "tzone") <- TZ
  ## start.date only to get nice rounding

  if (!is.null(start.date)) {
    dates[-1]
  } else {
    dates
  }
}
