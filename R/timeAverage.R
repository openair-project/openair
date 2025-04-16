#' Function to calculate time averages for data frames
#'
#' Function to flexibly aggregate or expand data frames by different time
#' periods, calculating vector-averaged wind direction where appropriate. The
#' averaged periods can also take account of data capture rates.
#'
#' This function calculates time averages for a data frame. It also treats wind
#' direction correctly through vector-averaging. For example, the average of 350
#' degrees and 10 degrees is either 0 or 360 - not 180. The calculations
#' therefore average the wind components.
#'
#' When a data capture threshold is set through `data.thresh` it is necessary
#' for [timeAverage()] to know what the original time interval of the input time
#' series is. The function will try and calculate this interval based on the
#' most common time gap (and will print the assumed time gap to the screen).
#' This works fine most of the time but there are occasions where it may not
#' e.g. when very few data exist in a data frame or the data are monthly (i.e.
#' non-regular time interval between months). In this case the user can
#' explicitly specify the interval through `interval` in the same format as
#' `avg.time` e.g. `interval = "month"`. It may also be useful to set
#' `start.date` and `end.date` if the time series do not span the entire period
#' of interest. For example, if a time series ended in October and annual means
#' are required, setting `end.date` to the end of the year will ensure that the
#' whole period is covered and that `data.thresh` is correctly calculated. The
#' same also goes for a time series that starts later in the year where
#' `start.date` should be set to the beginning of the year.
#'
#' [timeAverage()] should be useful in many circumstances where it is necessary
#' to work with different time average data. For example, hourly air pollution
#' data and 15-minute meteorological data. To merge the two data sets
#' [timeAverage()] can be used to make the meteorological data 1-hour means
#' first. Alternatively, [timeAverage()] can be used to expand the hourly data
#' to 15 minute data - see example below.
#'
#' For the research community [timeAverage()] should be useful for dealing with
#' outputs from instruments where there are a range of time periods used.
#'
#' It is also very useful for plotting data using [timePlot()]. Often the data
#' are too dense to see patterns and setting different averaging periods easily
#' helps with interpretation.
#'
#' @param mydata A data frame containing a `date` field . Can be class `POSIXct`
#'   or `Date`.
#' @param avg.time This defines the time period to average to. Can be `"sec"`,
#'   `"min"`, `"hour"`, `"day"`, `"DSTday"`, `"week"`, `"month"`, `"quarter"` or
#'   `"year"`. For much increased flexibility a number can precede these options
#'   followed by a space. For example, a timeAverage of 2 months would be
#'   `period = "2 month"`. In addition, `avg.time` can equal `"season"`, in
#'   which case 3-month seasonal values are calculated with spring defined as
#'   March, April, May and so on.
#'
#'   Note that `avg.time` can be *less* than the time interval of the original
#'   series, in which case the series is expanded to the new time interval. This
#'   is useful, for example, for calculating a 15-minute time series from an
#'   hourly one where an hourly value is repeated for each new 15-minute period.
#'   Note that when expanding data in this way it is necessary to ensure that
#'   the time interval of the original series is an exact multiple of `avg.time`
#'   e.g. hour to 10 minutes, day to hour. Also, the input time series must have
#'   consistent time gaps between successive intervals so that [timeAverage()]
#'   can work out how much 'padding' to apply. To pad-out data in this way
#'   choose `fill = TRUE`.
#' @param data.thresh The data capture threshold to use (%). A value of zero
#'   means that all available data will be used in a particular period
#'   regardless if of the number of values available. Conversely, a value of 100
#'   will mean that all data will need to be present for the average to be
#'   calculated, else it is recorded as `NA`. See also `interval`, `start.date`
#'   and `end.date` to see whether it is advisable to set these other options.
#'
#' @param statistic The statistic to apply when aggregating the data; default is
#'   the mean. Can be one of `"mean"`, `"max"`, `"min"`, `"median"`,
#'   `"frequency"`, `"sum"`, `"sd"`, `"percentile"`. Note that `"sd"` is the
#'   standard deviation, `"frequency"` is the number (frequency) of valid
#'   records in the period and `"data.cap"` is the percentage data capture.
#'   `"percentile"` is the percentile level (%) between 0-100, which can be set
#'   using the `"percentile"` option --- see below. Not used if `avg.time =
#'   "default"`.
#'
#' @param type `type` allows [timeAverage()] to be applied to cases where there
#'   are groups of data that need to be split and the function applied to each
#'   group. The most common example is data with multiple sites identified with
#'   a column representing site name e.g. `type = "site"`. More generally,
#'   `type` should be used where the date repeats for a particular grouping
#'   variable. However, if type is not supplied the data will still be averaged
#'   but the grouping variables (character or factor) will be dropped.
#' @param percentile The percentile level used when `statistic = "percentile"`.
#'   The default is 95%.
#' @param start.date A string giving a start date to use. This is sometimes
#'   useful if a time series starts between obvious intervals. For example, for
#'   a 1-minute time series that starts `2009-11-29 12:07:00` that needs to be
#'   averaged up to 15-minute means, the intervals would be `2009-11-29
#'   12:07:00`, `2009-11-29 12:22:00`, etc. Often, however, it is better to
#'   round down to a more obvious start point, e.g., `2009-11-29 12:00:00` such
#'   that the sequence is then `2009-11-29 12:00:00`, `2009-11-29 12:15:00`, and
#'   so on. `start.date` is therefore used to force this type of sequence.
#' @param end.date A string giving an end date to use. This is sometimes useful
#'   to make sure a time series extends to a known end point and is useful when
#'   `data.thresh > 0` but the input time series does not extend up to the final
#'   full interval. For example, if a time series ends sometime in October but
#'   annual means are required with a data capture of >75 % then it is necessary
#'   to extend the time series up until the end of the year. Input in the format
#'   yyyy-mm-dd HH:MM.
#' @param interval The [timeAverage()] function tries to determine the interval
#'   of the original time series (e.g. hourly) by calculating the most common
#'   interval between time steps. The interval is needed for calculations where
#'   the `data.thresh` >0. For the vast majority of regular time series this
#'   works fine. However, for data with very poor data capture or irregular time
#'   series the automatic detection may not work. Also, for time series such as
#'   monthly time series where there is a variable difference in time between
#'   months users should specify the time interval explicitly e.g. `interval =
#'   "month"`. Users can also supply a time interval to
#'   *force* on the time series. See `avg.time` for the format.
#'
#'   This option can sometimes be useful with `start.date` and `end.date` to
#'   ensure full periods are considered e.g. a full year when `avg.time =
#'   "year"`.
#' @param vector.ws Should vector averaging be carried out on wind speed if
#'   available? The default is `FALSE` and scalar averages are calculated.
#'   Vector averaging of the wind speed is carried out on the u and v wind
#'   components. For example, consider the average of two hours where the wind
#'   direction and speed of the first hour is 0 degrees and 2m/s and 180 degrees
#'   and 2m/s for the second hour. The scalar average of the wind speed is
#'   simply the arithmetic average = 2m/s and the vector average is 0m/s.
#'   Vector-averaged wind speeds will always be lower than scalar-averaged
#'   values.
#' @param fill When time series are expanded, i.e., when a time interval is less
#'   than the original time series, data are 'padded out' with `NA`. To
#'   'pad-out' the additional data with the first row in each original time
#'   interval, choose `fill = TRUE`.
#' @param progress Show a progress bar when many groups make up `type`? Defaults
#'   to `TRUE`.
#' @param ... Additional arguments for other functions calling [timeAverage()].
#' @import dplyr
#' @export
#' @return Returns a data frame with date in class `POSIXct`.
#' @author David Carslaw
#' @seealso [timePlot()] that plots time series data and uses [timeAverage()] to
#'   aggregate data where necessary.
#' @seealso [calcPercentile()] that wraps [timeAverage()] to allow multiple
#'   percentiles to be calculated at once.
#' @examples
#' # daily average values
#' daily <- timeAverage(mydata, avg.time = "day")
#'
#' # daily average values ensuring at least 75 % data capture
#' # i.e., at least 18 valid hours
#' \dontrun{
#' daily <- timeAverage(mydata, avg.time = "day", data.thresh = 75)
#' }
#'
#' # 2-weekly averages
#' \dontrun{
#' fortnight <- timeAverage(mydata, avg.time = "2 week")
#' }
#'
#' # make a 15-minute time series from an hourly one
#' \dontrun{
#' min15 <- timeAverage(mydata, avg.time = "15 min", fill = TRUE)
#' }
#'
#' # average by grouping variable
#' \dontrun{
#' dat <- importAURN(c("kc1", "my1"), year = 2011:2013)
#' timeAverage(dat, avg.time = "year", type = "site")
#'
#' # can also retain site code
#' timeAverage(dat, avg.time = "year", type = c("site", "code"))
#'
#' # or just average all the data, dropping site/code
#' timeAverage(dat, avg.time = "year")
#' }
timeAverage <- function(
  mydata,
  avg.time = "day",
  data.thresh = 0,
  statistic = "mean",
  type = "default",
  percentile = NA,
  start.date = NA,
  end.date = NA,
  interval = NA,
  vector.ws = FALSE,
  fill = FALSE,
  progress = TRUE,
  ...
) {
  # validate function inputs
  inputs <- validate_timeaverage_inputs(
    data.thresh = data.thresh,
    percentile = percentile,
    statistic = statistic
  )
  data.thresh <- inputs$data.thresh
  percentile <- inputs$percentile

  # flag whether a time series has already been padded to fill time gaps
  padded <- FALSE

  # extract variables of interest
  vars <- unique(c("date", names(mydata)))
  mydata <- checkPrep(
    mydata,
    vars,
    type = "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )

  # time zone of data (replace missing w/ GMT)
  TZ <- attr(mydata$date, "tzone") %||% "GMT"

  # get an appropriate function for the statistic
  FUN <- get_statistic_function(statistic = statistic, percentile = percentile)

  # function to calculate means
  #
  # need to check whether avg.time is > or < actual time gap of data
  # then data will be expanded or aggregated accordingly
  calc.mean <- function(mydata, start.date, end.date) {
    # deal with start/end date arguments
    mydata <- bind_start_and_end_dates(
      mydata = mydata,
      type = type,
      start.date = start.date,
      end.date = end.date,
      TZ = TZ
    )

    # if interval specified, then use it to pad the data
    if (!is.na(interval)) {
      mydata <-
        mydata %>%
        split(mydata[type], drop = TRUE) %>%
        purrr::map(function(x) {
          pad_dates_timeavg(
            mydata = x,
            type = type,
            interval = interval
          )
        }) %>%
        purrr::list_rbind()

      # make sure missing types are inserted
      mydata[type] <- mydata[type] <- mydata[1, type]

      padded <- TRUE
    }

    # obtain time parameters; seconds in the avg.time interval and seconds in
    # the data interval
    time_params <- get_time_parameters(mydata = mydata, avg.time = avg.time)
    seconds_data_interval <- time_params$seconds_data_interval
    seconds_avgtime_interval <- time_params$seconds_avgtime_interval

    ## check to see if we need to expand data rather than aggregate it
    ## i.e. chosen time interval less than that of data
    if (seconds_avgtime_interval < seconds_data_interval) {
      ## original dates
      theDates <- mydata$date

      ## need to add a date to the end when expanding times
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
      if (days %in% c(30, 31)) {
        interval <- "month"
      }
      if (days %in% c(365, 366)) {
        interval <- "year"
      }

      allDates <- seq(min(mydata$date), max(mydata$date), by = interval)
      allDates <- c(allDates, max(allDates) + seconds_data_interval)

      ## all data with new time interval
      allData <- data.frame(date = seq(min(allDates), max(allDates), avg.time))

      ## merge with original data, which leaves gaps to fill
      mydata <- full_join(mydata, allData, by = "date") %>%
        arrange(date)

      ## make sure missing types are inserted
      mydata[type] <- mydata[type] <- mydata[1, type]

      if (fill) {
        ## this will copy-down data to next original row of data
        ## number of additional lines to fill
        inflateFac <- seconds_data_interval / seconds_avgtime_interval
        if (seconds_data_interval %% seconds_avgtime_interval != 0) {
          stop(
            "Non-regular time expansion selected, or non-regular input time series."
          )
        }

        ## ids of original dates in new dates
        ids <- which(mydata$date %in% theDates)

        date <- mydata$date
        mydata <- subset(mydata, select = -date)

        for (i in 1:(inflateFac - 1)) {
          mydata[ids + i, ] <- mydata[ids, ]
        }

        mydata <- cbind(date, mydata)
        mydata <- mydata[seq_len(nrow(mydata)) - 1, ] ## don't need last row
      }

      return(mydata)
    }

    # calculate Uu & Vv if "wd" (& "ws") are in mydata
    mydata <- calculate_wind_components(mydata = mydata)

    # handle 'season' as special case
    if (avg.time == "season") {
      mydata <- handle_season_avgtime(mydata, type = type, ...)
    }

    ## Aggregate data

    ## variables to split by
    vars <- c(type, "date")
    if (avg.time == "season") {
      vars <- unique(c(vars, "season"))
    }

    # if a data threshold has been set and data hasn't already been padded,
    # pad the data to ensure that DC% calculation is accurate
    if (data.thresh != 0 && !padded) {
      mydata <- date.pad(mydata, type = type)
    }

    # cut date column into bins unless season (dealt w/ earlier)
    if (avg.time != "season") {
      mydata$date <-
        lubridate::as_datetime(
          as.character(cut(mydata$date, avg.time)),
          tz = TZ
        )
    }

    if (data.thresh != 0) {
      avmet <- mydata %>%
        group_by(across(vars)) %>%
        summarise(across(
          where(function(x) {
            !is.factor(x) && !is.character(x)
          }),
          function(x) {
            if (sum(is.na(x)) / length(x) <= 1 - data.thresh) {
              FUN(x)
            } else {
              NA
            }
          }
        ))
    } else {
      avmet <-
        mydata %>%
        group_by(across(vars)) %>%
        summarise(across(
          where(function(x) {
            !is.factor(x) && !is.character(x)
          }),
          function(x) {
            FUN(x)
          }
        ))
    }

    if ("wd" %in% names(mydata) && statistic != "data.cap") {
      if (is.numeric(mydata$wd)) {
        ## mean wd
        avmet <-
          avmet %>%
          dplyr::mutate(
            wd = as.vector(atan2(.data$Uu, .data$Vv) * 360 / 2 / pi),
            # correct negative wind directions
            wd = dplyr::if_else(.data$wd < 0, .data$wd + 360, .data$wd)
          )
        
        ## vector average ws
        if ("ws" %in% names(mydata) && vector.ws) {
          avmet <- dplyr::mutate(avmet, ws = (.data$Uu^2 + .data$Vv^2)^0.5)
        }
        
        avmet <- dplyr::select(avmet, -"Uu", -"Vv")
      }
    }

    ## fill missing gaps - but only for non-dst data
    if (avg.time != "season" && !any(dst(avmet$date))) {
      avmet <- pad_dates_timeavg(avmet, type = type, interval = avg.time)
    }

    avmet
  }

  # cut data into intervals
  mydata <- cutData(mydata, type)

  # select date, type, and all non-factor/character columns
  mydata <-
    dplyr::select(
      mydata,
      dplyr::all_of(c("date", type)),
      dplyr::where(function(x) {
        !is.character(x) && !is.factor(x)
      })
    )

  # calculate stats split by type
  if (progress) {
    progress <- "Calculating Time Averages"
  }

  # calculate averages
  mydata <-
    mydata %>%
    split(mydata[type], drop = TRUE) %>%
    purrr::map(
      calc.mean,
      start.date = start.date,
      end.date = end.date,
      .progress = progress
    ) %>%
    purrr::list_rbind() %>%
    dplyr::as_tibble()

  # drop default column if it exists
  if ("default" %in% names(mydata)) {
    mydata$default <- NULL
  }

  # return
  return(mydata)
}

#' Checks relevant timeAverage inputs
#' @noRd
validate_timeaverage_inputs <- function(data.thresh, percentile, statistic) {
  # Percentile
  if (!is.na(percentile)) {
    if (percentile < 0 || percentile > 100) {
      cli::cli_abort("Percentile range, {percentile}, outside {0L}-{100L}")
    }
    percentile <- percentile / 100
  }

  # Data Threshold
  if (data.thresh < 0 || data.thresh > 100) {
    cli::cli_abort("Data capture range, {data.thresh}, outside {0L}-{100L}")
  }
  data.thresh <- data.thresh / 100

  # Statistic
  valid_stats <-
    c(
      "mean",
      "median",
      "frequency",
      "max",
      "min",
      "sum",
      "sd",
      "percentile",
      "data.cap"
    )
  statistic <- rlang::arg_match(statistic, valid_stats)

  return(list(data.thresh = data.thresh, percentile = percentile))
}

#' Get an appropriate function for the statistic
#' @noRd
get_statistic_function <- function(statistic, percentile) {
  if (statistic == "mean") {
    return(function(x) {
      mean(x, na.rm = TRUE)
    })
  }
  if (statistic == "median") {
    return(function(x) {
      median(x, na.rm = TRUE)
    })
  }
  if (statistic == "frequency") {
    return(function(x) {
      length(na.omit(x))
    })
  }
  if (statistic == "max") {
    return(function(x) {
      if (all(is.na(x))) {
        NA
      } else {
        max(x, na.rm = TRUE)
      }
    })
  }
  if (statistic == "min") {
    return(function(x) {
      min(x, na.rm = TRUE)
    })
  }
  if (statistic == "sum") {
    return(function(x) {
      if (all(is.na(x))) {
        NA
      } else {
        sum(x, na.rm = TRUE)
      }
    })
  }
  if (statistic == "sd") {
    return(function(x) {
      sd(x, na.rm = TRUE)
    })
  }
  if (statistic == "data.cap") {
    return(function(x) {
      if (all(is.na(x))) {
        res <- 0
      } else {
        res <- 100 * (1 - sum(is.na(x)) / length(x))
      }
      res
    })
  }
  if (statistic == "percentile") {
    return(function(x) {
      quantile(x, probs = percentile, na.rm = TRUE)
    })
  }
}

#' Add start.date and end.date, if they exist
#' @noRd
bind_start_and_end_dates <- function(mydata, type, start.date, end.date, TZ) {
  if (!is.na(start.date)) {
    firstLine <- data.frame(date = as.POSIXct(start.date, tz = TZ))
    firstLine[type] <- mydata[1, type]
    mydata <- bind_rows(firstLine, mydata)
  }

  if (!is.na(end.date)) {
    lastLine <- data.frame(date = as.POSIXct(end.date, tz = TZ))
    lastLine[type] <- mydata[1, type]
    mydata <- bind_rows(mydata, lastLine)
  }

  mydata$date <- as.POSIXct(format(mydata$date), tz = TZ)

  return(mydata)
}

#' Pad the data
#' @noRd
pad_dates_timeavg <- function(mydata, type = NULL, interval = "month") {
  # assume by the time we get here the data have been split into types
  # This means we just need to pad out the missing types based on first
  # line.

  start.date <- min(mydata$date, na.rm = TRUE)
  end.date <- max(mydata$date, na.rm = TRUE)

  # interval is in seconds, so convert to days if Date class and not POSIXct
  if (class(mydata$date)[1] == "Date") {
    interval <- paste(
      as.numeric(strsplit(interval, " ")[[1]][1]) / 3600 / 24,
      "days"
    )
  }

  all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
  mydata <- mydata %>% full_join(all.dates, by = "date")

  # add in missing types if gaps are made
  if (!is.null(type)) {
    mydata[type] <- mydata[1, type]
  }

  # make sure order is correct
  mydata <- arrange(mydata, date)

  return(mydata)
}

#' Get the intervals in the original data and in the avg.time period
#' @noRd
get_time_parameters <- function(mydata, avg.time) {
  # Time diff in seconds of original data
  seconds_data_interval <- as.numeric(strsplit(
    find.time.interval(mydata$date),
    " "
  )[[1]][1])

  # Parse time diff of new interval
  by2 <- strsplit(avg.time, " ", fixed = TRUE)[[1]]

  seconds_avgtime_interval <- 1
  if (length(by2) > 1) {
    seconds_avgtime_interval <- as.numeric(by2[1])
  }
  units <- by2[length(by2)]

  # Convert units to seconds
  int <- switch(
    units,
    "sec" = 1,
    "min" = 60,
    "hour" = 3600,
    "day" = 3600 * 24,
    "week" = 3600 * 24 * 7,
    "month" = 3600 * 24 * 31,
    "quarter" = 3600 * 24 * 31 * 3,
    "season" = 3600 * 24 * 31 * 3,
    "year" = 3600 * 8784
  )

  seconds_avgtime_interval <- seconds_avgtime_interval * int # interval in seconds
  if (is.na(seconds_data_interval)) {
    seconds_data_interval <- seconds_avgtime_interval # when only one row
  }

  return(
    list(
      seconds_data_interval = seconds_data_interval,
      seconds_avgtime_interval = seconds_avgtime_interval
    )
  )
}

#' Calculate Uu and Vv if wd & ws are in the data
#' @noRd
calculate_wind_components <- function(mydata) {
  if ("wd" %in% names(mydata)) {
    if (is.numeric(mydata$wd)) {
      if ("ws" %in% names(mydata)) {
        mydata <- dplyr::mutate(
          mydata,
          Uu = .data$ws * sin(2 * pi * .data$wd / 360),
          Vv = .data$ws * cos(2 * pi * .data$wd / 360)
        )
      } else {
        mydata <- dplyr::mutate(
          mydata,
          Uu = sin(2 * pi * .data$wd / 360),
          Vv = cos(2 * pi * .data$wd / 360)
        )
      }
    }
  }

  return(mydata)
}

#' Function to handle getting a mean date in year-season by shifting
#' December into next year
#' @noRd
handle_season_avgtime <- function(mydata, type, ...) {
  # don't cut again if type = "season"
  if (!"season" %in% type) {
    mydata <- cutData(mydata, type = "season", ...)
  }

  mydata %>%
    # drop missing seasons
    dplyr::filter(!is.na(.data$season)) %>%
    # extract year/month
    dplyr::mutate(
      year = lubridate::year(.data$date),
      month = lubridate::month(.data$date)
    ) %>%
    # nudge December into next month
    dplyr::mutate(
      year = dplyr::if_else(.data$month == 12, .data$year + 1, .data$year)
    ) %>%
    # get average date per year-season
    dplyr::mutate(
      date = mean(date, na.rm = TRUE),
      .by = c("year", "season")
    ) %>%
    # drop year/month cols
    dplyr::select(-"year", -"month")
}
