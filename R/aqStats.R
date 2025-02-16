#' Calculate summary statistics for air pollution data by year
#'
#' Calculate a range of air pollution-relevant statistics by year.
#'
#' This function calculates a range of common and air pollution-specific
#' statistics from a data frame. The statistics are calculated on an annual
#' basis and the input is assumed to be hourly data. The function can cope with
#' several sites and years, e.g., using `type = "site"`. The user can control
#' the output by setting `transpose` appropriately.
#'
#' Note that the input data is assumed to be in mass units e.g. ug/m3 for all
#' species except CO (mg/m3).
#'
#' The following statistics are calculated:
#'
#' - **data.capture** --- percentage data capture over a full year.
#'
#' - **mean** --- annual mean.
#'
#' - **minimum** --- minimum hourly value.
#'
#' - **maximum** --- maximum hourly value.
#'
#' - **median** --- median value.
#'
#' - **max.daily** --- maximum daily mean.
#'
#' - **max.rolling.8** --- maximum 8-hour rolling mean.
#'
#' - **max.rolling.24** --- maximum 24-hour rolling mean.
#'
#' - **percentile.95** --- 95th percentile. Note that several percentiles
#' can be calculated.
#'
#' - **roll.8.O3.gt.100** --- number of days when the daily maximum
#' rolling 8-hour mean ozone concentration is >100 ug/m3. This is the target
#' value.
#'
#' - **roll.8.O3.gt.120** --- number of days when the daily maximum
#' rolling 8-hour mean ozone concentration is >120 ug/m3. This is the Limit
#' Value not to be exceeded > 10 days a year.
#'
#' - **AOT40** --- is the accumulated amount of ozone over the threshold
#' value of 40 ppb for daylight hours in the growing season (April to
#' September). Note that `latitude` and `longitude` can also be passed to this
#' calculation.
#'
#' - **hours.gt.200** --- number of hours NO2 is more than 200 ug/m3.
#'
#' - **days.gt.50** --- number of days PM10 is more than 50 ug/m3.
#'
#' For the rolling means, the user can supply the option `align`, which can be
#' "centre" (default), "left" or "right". See [rollingMean()] for more details.
#'
#' There can be small discrepancies with the AURN due to the treatment of
#' rounding data. The [aqStats()] function does not round, whereas AURN data can
#' be rounded at several stages during the calculations.
#'
#' @inheritParams timeAverage
#' @param mydata A data frame containing a `date` field of hourly data.
#' @param pollutant The name of a pollutant e.g. `pollutant = c("o3", "pm10")`.
#' @param percentile Percentile values to calculate for each pollutant.
#' @param transpose The default is to return a data frame with columns
#'   representing the statistics. If `transpose = TRUE` then the results have
#'   columns for each pollutant-site combination.
#' @param ... Other arguments, currently unused.
#' @export
#' @author David Carslaw
#' @examples
#'
#' # Statistics for 2004. NOTE! these data are in ppb/ppm so the
#' # example is for illustrative purposes only
#' aqStats(selectByDate(mydata, year = 2004), pollutant = "no2")
aqStats <- function(mydata,
                    pollutant = "no2",
                    type = "default",
                    data.thresh = 0,
                    percentile = c(95, 99),
                    transpose = FALSE,
                    ...) {
  ## get rid of R check annoyances
  year <- rolling8value <- NULL
  daylight <- NULL
  . <- NULL

  # variables we need
  vars <- c("date", pollutant, type)

  # cut data by type
  mydata <- cutData(mydata, type)

  # check we have the variables
  mydata <- checkPrep(mydata,
    vars,
    "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )

  # reorganise data
  mydata <-
    gather(mydata, key = pollutant, value = value, pollutant) %>%
    mutate(year = lubridate::year(date))

  vars <- c(type, "pollutant", "year")

  # calculate the statistics
  results <- mydata %>%
    group_by(across(vars)) %>%
    do(calcStats(.,
      data.thresh = data.thresh,
      percentile = percentile, ...
    ))

  ## transpose if requested
  if (transpose) {
    results <- gather(results,
      key = variable,
      value = value, -c(type, pollutant, year, date)
    )

    if (type != "default") {
      results <- unite(results, site_pol, type, pollutant)
    } else {
      results <- unite(results, site_pol, pollutant)
    }

    results <- spread(results, site_pol, value)

    ## sort out names
    names(results) <- gsub("\\_", " ", names(results))
  }

  return(results)
}

# function to calculate statistics
calcStats <- function(mydata, data.thresh, percentile, ...) {
  rolling8value <- NULL # keep R check happy

  # check to see if dates duplicate
  if (length(unique(mydata$date)) != length(mydata$date)) {
    warning("Duplicate dates detected, more than one site? Use type = 'site'",
      call. = FALSE
    )
  }

  ## pre-defined list of pollutants that need special treatment
  thePolls <- c("no2", "o3", "pm10", "co")

  ## fill any missing hours
  start.date <- floor_date(min(mydata$date), "year")
  end.date <- ceiling_date(max(mydata$date), "year") - 3600

  ## find time interval of data and pad any missing times
  interval <- find.time.interval(mydata$date)
  all.dates <-
    data.frame(date = seq(start.date, end.date, by = interval))

  # pad out names where needed
  if (nrow(mydata) != nrow(all.dates)) {
    mydata <- full_join(mydata, all.dates, by = "date")
    mydata[setdiff(names(mydata), c("date", "value"))] <-
      mydata[1, setdiff(names(mydata), c("date", "value"))]
  }


  # statistics

  Mean <- timeAverage(
    mydata,
    avg.time = "year",
    statistic = "mean",
    data.thresh,
    print.int = FALSE
  ) %>%
    rename(mean = value)

  Min <- timeAverage(
    mydata,
    avg.time = "year",
    statistic = "min",
    data.thresh,
    print.int = FALSE
  ) %>%
    rename(min = value)

  Max <- timeAverage(
    mydata,
    avg.time = "year",
    statistic = "max",
    data.thresh,
    print.int = FALSE
  ) %>%
    rename(max = value)

  maxDaily <- timeAverage(
    mydata,
    avg.time = "day",
    statistic = "mean",
    data.thresh,
    print.int = FALSE
  ) %>%
    timeAverage(
      avg.time = "year",
      statistic = "max",
      data.thresh,
      print.int = FALSE
    ) %>%
    rename(max_daily = value)

  Median <- timeAverage(mydata,
    avg.time = "year",
    statistic = "median",
    data.thresh
  ) %>%
    rename(median = value)

  dataCapture <- group_by(mydata, year) %>%
    summarise(
      date = min(date),
      dat.cap = 100 * length(na.omit(value)) /
        length(value)
    )


  rollMax8 <- group_by(mydata, year) %>%
    do(
      rollingMean(
        .,
        pollutant = "value",
        data.thresh = data.thresh,
        width = 8,
        new.name = "value",
        ...
      )
    ) %>%
    do(timeAverage(.,
      avg.time = "year", statistic = "max",
      data.thresh
    )) %>%
    rename(roll_8_max = value)

  rollMax24 <- group_by(mydata, year) %>%
    do(
      rollingMean(
        .,
        pollutant = "value",
        data.thresh = data.thresh,
        width = 24,
        new.name = "value",
        ...
      )
    ) %>%
    do(timeAverage(.,
      avg.time = "year", statistic = "max",
      data.thresh
    )) %>%
    rename(roll_24_max = value)

  Percentile <- group_by(mydata, year) %>%
    do(
      calcPercentile(
        .,
        avg.time = "year",
        pollutant = "value",
        data.thresh = data.thresh,
        percentile = percentile
      )
    )


  vars <- c("year", "date")

  # specific treatment of pollutants

  if (length(grep("o3", mydata$pollutant[1], ignore.case = TRUE)) == 1) {
    rollingO3 <- group_by(mydata, year) %>%
      do(rollingMean(., "value", data.thresh = data.thresh, ...)) %>%
      do(timeAverage(
        .,
        avg.time = "day",
        statistic = "max",
        data.thresh = data.thresh
      )) %>%
      summarise(roll.8.O3.gt.100 = length(which(rolling8value > 100)))


    rollingO3$date <- Mean$date

    rollingO3b <- group_by(mydata, year) %>%
      do(rollingMean(., "value", data.thresh = data.thresh, ...)) %>%
      do(timeAverage(
        .,
        avg.time = "day",
        statistic = "max",
        data.thresh = data.thresh
      )) %>%
      summarise(roll.8.O3.gt.120 = length(which(rolling8value > 120)))

    rollingO3b$date <- Mean$date

    AOT40 <- group_by(mydata, year) %>%
      do(AOT40(., "value"))

    AOT40$date <- Mean$date

    o3.results <- list(
      dataCapture,
      Mean,
      Min,
      Max,
      Median,
      maxDaily,
      rollMax8,
      rollMax24,
      Percentile,
      rollingO3,
      rollingO3b,
      AOT40
    )
    o3.results <- Reduce(function(x, y) {
      merge(x, y,
        by = vars,
        all = TRUE
      )
    }, o3.results)

    results <- o3.results
    results
  }

  if (length(grep("no2", mydata$pollutant[1], ignore.case = TRUE)) == 1) {
    hours <- group_by(mydata, year) %>%
      summarise(hours = length(which(value > 200)))

    hours$date <- Mean$date

    no2.results <- list(
      dataCapture,
      Mean,
      Min,
      Max,
      Median,
      maxDaily,
      rollMax8,
      rollMax24,
      Percentile,
      hours
    )

    no2.results <- Reduce(function(x, y) {
      merge(x, y,
        by = vars,
        all = TRUE
      )
    }, no2.results)

    results <- no2.results
    results
  }

  if (length(grep("pm10", mydata$pollutant[1], ignore.case = TRUE)) == 1) {
    days <- group_by(mydata, year) %>%
      do(timeAverage(.,
        avg.time = "day", statistic = "mean", data.thresh
      )) %>%
      summarise(days = length(which(value > 50)))


    days$date <- Mean$date

    pm10.results <- list(
      dataCapture,
      Mean,
      Min,
      Max,
      Median,
      maxDaily,
      rollMax8,
      rollMax24,
      Percentile,
      days
    )

    pm10.results <- Reduce(function(x, y) {
      merge(x, y,
        by = vars,
        all = TRUE
      )
    }, pm10.results)

    results <- pm10.results
    results
  }

  if (length(grep("co", mydata$pollutant[1], ignore.case = TRUE)) == 1) {
    co.results <- list(
      dataCapture,
      Mean,
      Min,
      Max,
      Median,
      maxDaily,
      rollMax8,
      rollMax24,
      Percentile
    )
    co.results <- Reduce(function(x, y) {
      merge(x, y,
        by = vars,
        all = TRUE
      )
    }, co.results)
    results <- co.results
    results
  }


  ## see if pollutant string in any pre-defined ones
  ## if not calculate basic stats
  if (all(is.na(sapply(thePolls, function(x) {
    grep(x, mydata$pollutant[1], ignore.case = TRUE)
  }) > 0))) {
    results <- list(
      dataCapture,
      Mean,
      Min,
      Max,
      Median,
      maxDaily,
      rollMax8,
      rollMax24,
      Percentile
    )

    results <- Reduce(function(x, y) {
      merge(x, y,
        by = vars,
        all = TRUE
      )
    }, results)


    results <- results
  }


  results
}

AOT40 <- function(mydata, pollutant, ...) {
  ## note the assumption is the O3 is in ug/m3
  daylight <- NULL

  ## need daylight hours in growing season (April to September)
  mydata <- selectByDate(mydata, month = 4:9)
  mydata <- cutData(mydata, "daylight", ...)
  mydata <- subset(mydata, daylight == "daylight")
  AOT40 <-
    ifelse(mydata[[pollutant]] - 80 < 0, 0, mydata[[pollutant]] - 80)
  AOT40 <- sum(AOT40, na.rm = TRUE) * 0.50 ## for ppb
  AOT40 <- tibble(AOT40)
  return(AOT40)
}
