#' Calculate summary statistics for air pollution data by year
#'
#' This function calculates a range of common and air pollution-specific
#' statistics from a data frame. The statistics are calculated on an annual
#' basis and the input is assumed to be hourly data. The function can cope with
#' several sites and years, e.g., using `type = "site"`. The user can control
#' the output by setting `transpose` appropriately. Note that the input data is
#' assumed to be in mass units, e.g., ug/m3 for all species except CO (mg/m3).
#'
#' The following statistics are calculated:
#'
#' For all pollutants:
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
#' When `pollutant == "o3"`:
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
#' When `pollutant == "no2"`:
#'
#' - **hours.gt.200** --- number of hours NO2 is more than 200 ug/m3.
#'
#' When `pollutant == "pm10"`:
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
#'   Additional statistics will be calculated if `pollutant %in% c("no2",
#'   "pm10", "o3")`.
#' @param percentile Percentile values to calculate for each pollutant.
#' @param transpose The default is to return a data frame with columns
#'   representing the statistics. If `transpose = TRUE` then the results have
#'   columns for each pollutant-type combination.
#' @param ... Other arguments, currently unused.
#' @export
#' @author David Carslaw
#' @examples
#'
#' # Statistics for 2004. NOTE! these data are in ppb/ppm so the
#' # example is for illustrative purposes only
#' aqStats(selectByDate(mydata, year = 2004), pollutant = "no2")
aqStats <- function(
  mydata,
  pollutant = "no2",
  type = "default",
  data.thresh = 0,
  percentile = c(95, 99),
  transpose = FALSE,
  progress = TRUE,
  ...
) {
  # variables we need
  vars <- c("date", pollutant, type)

  # cut data by type
  mydata <- cutData(mydata, type)

  # check we have the variables
  mydata <- checkPrep(
    mydata,
    vars,
    "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )

  # reorganise data
  mydata <-
    mydata %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(pollutant),
      names_to = "pollutant"
    ) %>%
    mutate(year = lubridate::year(date))

  vars <- c(type, "pollutant", "year")

  # calculate the statistics
  results <-
    purrr::map(
      .x = split(mydata, mydata[vars], sep = "__"),
      .f = function(x) {
        out <- rlang::exec(
          calcStats,
          !!!rlang::list2(
            mydata = x,
            data.thresh = data.thresh,
            percentile = percentile,
            ...
          )
        )
        out[vars] <- x[vars][1, , drop = FALSE]
        out
      },
      .progress = progress
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::relocate(dplyr::all_of(vars))

  # transpose if requested
  if (transpose) {
    unite_vars <- c(type, "pollutant")
    if (type == "default") {
      unite_vars <- c("pollutant")
    }

    results <-
      results %>%
      tidyr::pivot_longer(-dplyr::all_of(c(vars, "date"))) %>%
      tidyr::unite(site_pol, dplyr::all_of(unite_vars)) %>%
      tidyr::pivot_wider(names_from = "site_pol") %>%
      dplyr::rename_with(function(x) {
        gsub("_", " ", x)
      })
  }

  # return
  return(results)
}

# function to calculate statistics
calcStats <- function(mydata, data.thresh, percentile, ...) {
  # check to see if dates duplicate
  if (length(unique(mydata$date)) != length(mydata$date)) {
    cli::cli_warn(
      c(
        "!" = 'Duplicate dates detected per {.field type}.',
        "i" = 'Is there more than one site? Use {.code {.field type} = "site"}.'
      ),
      call = NULL
    )
  }

  # fill any missing hours
  start.date <- lubridate::floor_date(min(mydata$date), "year")
  end.date <- lubridate::ceiling_date(max(mydata$date), "year") - 3600

  # find time interval of data and pad any missing times
  interval <- find.time.interval(mydata$date)
  all.dates <-
    data.frame(date = seq(start.date, end.date, by = interval))

  # pad out names where needed
  if (nrow(mydata) != nrow(all.dates)) {
    mydata <- dplyr::full_join(mydata, all.dates, by = "date")
    mydata[setdiff(names(mydata), c("date", "value"))] <-
      mydata[1, setdiff(names(mydata), c("date", "value"))]
  }

  # convenience function for basic statistics
  timeAverageYear <- function(mydata, stat, newname = stat) {
    df <-
      timeAverage(
        mydata,
        avg.time = "year",
        statistic = stat,
        data.thresh = data.thresh,
        print.int = FALSE
      )

    names(df)[names(df) == "value"] <- newname

    return(df)
  }

  # convenience function for rolled maxes
  maxRollTimeAverage <- function(mydata, width, newname, ...) {
    purrr::map(
      .x = split(mydata, ~year),
      .f = function(x) {
        rlang::exec(
          rollingMean,
          !!!rlang::list2(
            mydata = x,
            pollutant = "value",
            data.thresh = data.thresh,
            width = width,
            new.name = "value",
            ...
          )
        ) %>%
          timeAverageYear("max", newname)
      }
    ) %>%
      dplyr::bind_rows()
  }

  # simple stats
  Mean <- timeAverageYear(mydata, "mean")
  Min <- timeAverageYear(mydata, "min")
  Max <- timeAverageYear(mydata, "max")
  Median <- timeAverageYear(mydata, "median")
  rollMax8 <-
    maxRollTimeAverage(mydata, width = 8L, newname = "roll_8_max", ...)
  rollMax24 <-
    maxRollTimeAverage(mydata, width = 24L, newname = "roll_24_max", ...)

  # maximum daily mean
  maxDaily <- timeAverage(
    mydata,
    avg.time = "day",
    statistic = "mean",
    data.thresh,
    print.int = FALSE
  ) %>%
    timeAverageYear("max", "max_daily")

  # data capture
  dataCapture <-
    dplyr::summarise(
      mydata,
      date = min(.data$date, na.rm = TRUE),
      dat.cap = 100 * mean(!is.na(.data$value)),
      .by = "year"
    )

  # percentiles
  Percentile <-
    purrr::imap(
      .x = split(mydata, ~year),
      .f = function(x, i) {
        calcPercentile(
          x,
          avg.time = "year",
          pollutant = "value",
          data.thresh = data.thresh,
          percentile = percentile
        ) %>%
          dplyr::mutate(year = as.numeric(i))
      }
    ) %>%
    dplyr::bind_rows()

  # Tables list to merge
  tables <- list(
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

  # specific treatment of pollutants
  # Ozone
  if (grepl("o3", mydata$pollutant[1], ignore.case = TRUE)) {
    # ozone greater than 100
    rollingO3 <-
      purrr::imap(
        .x = split(mydata, ~year),
        .f = function(x, i) {
          rlang::exec(
            rollingMean,
            !!!rlang::list2(
              mydata = x,
              pollutant = "value",
              data.thresh = data.thresh,
              ...
            )
          ) %>%
            timeAverage(
              avg.time = "day",
              statistic = "max",
              data.thresh = data.thresh
            ) %>%
            dplyr::summarise(
              roll.8.O3.gt.100 = sum(.data$rolling8value > 100, na.rm = TRUE),
              roll.8.O3.gt.120 = sum(.data$rolling8value > 120, na.rm = TRUE)
            ) %>%
            dplyr::mutate(year = as.numeric(i))
        }
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(date = Mean$date)

    aot40 <-
      purrr::imap(
        .x = split(mydata, ~year),
        .f = function(x, i) {
          rlang::exec(
            AOT40,
            !!!rlang::list2(
              mydata = x,
              pollutant = "value",
              ...
            )
          ) %>%
            dplyr::mutate(year = as.numeric(i))
        }
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(date = Mean$date)

    tables <- append(tables, list(rollingO3, aot40))
  }

  # Nitrogen Dioxide
  if (grepl("no2", mydata$pollutant[1], ignore.case = TRUE)) {
    hours <-
      dplyr::summarise(
        mydata,
        hours = sum(.data$value > 200, na.rm = TRUE),
        .by = "year"
      ) %>%
      dplyr::mutate(date = Mean$date)

    tables <- append(tables, list(hours))
  }

  # Particulate Matter
  if (length(grep("pm10", mydata$pollutant[1], ignore.case = TRUE)) == 1) {
    days <-
      timeAverage(
        mydata,
        avg.time = "day",
        statistic = "mean",
        data.thresh = data.thresh,
        type = "year"
      ) %>%
      dplyr::summarise(days = sum(.data$value > 50, na.rm = TRUE)) %>%
      dplyr::mutate(date = Mean$date, year = mydata$year[1])

    tables <- append(tables, list(days))
  }

  # Combine
  purrr::reduce(
    .x = tables,
    .f = function(x, y) {
      dplyr::full_join(x, y, by = c("date", "year"))
    }
  )
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
