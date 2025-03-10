#' Calculate user-defined air quality statistics
#'
#' [calcAQStats()] calculates user-defined annual air-quality statistics
#' constructed using [aqStat()]. This pair of functions allows users to flexibly
#' define air quality statistics and limits relevant to their local air quality
#' legislation and quickly track compliance.
#'
#' @rdname calc-aq-stats
#' @order 1
#'
#' @param mydata A data frame of time series. Must include a `date` field and
#'   any variables required by the provided `aqstats`.
#' @param aqstats A list of [aqStat()] objects to calculate.
#' @param period A time period to calculate statistics for. Currently, only
#'   `"year"` is supported, which will calculate annual statistics.
#' @param progress Show a progress bar when large numbers of statistics are
#'   being calculated? Defaults to `TRUE`.
#'
#' @author Jack Davison
#' @export
#'
#' @seealso [aqStats()], for a simpler but more prescriptive way of calculating
#'   air quality statistics
#'
#' @section Data Transformation Pipeline:
#'
#'   [calcAQStats()] does *a lot* in one go, so it is worth outlining the order
#'   of proceedings:
#'
#'   - First, the data is time-averaged using `avg.time`. This is passed
#'   straight to [timeAverage()]. For hourly data and the default `avg.time` of
#'   `"hour"`, effectively nothing happens at this stage.
#'
#'   - Second, if `roll.width` is specified, a rolling mean is calculated
#'   using [rollingMean()]. Typically this should be combined with `avg.time =
#'   "hour"` to ensure *hourly* data is rolled. Most likely, `roll.width` will
#'   be `8L` (for ozone & carbon monoxide) or `24L` (for particulates).
#'
#'   - If `roll.avg.time` is set, the average rolled values will then
#'   themselves be averaged. `roll.avg.stat` defaults to `"max"`, which is
#'   expected to be the most useful for almost all applications. These options
#'   are likely only useful when `stat = "limit"` to compare complex statistics
#'   like "daily max rolling 8-hour mean ozone" with a limit value.
#'
#'   - Next, the `stat` is used to calculate a `period` (by default, annual)
#'   statistic. If `stat != "limit"` this, again, is passed straight to
#'   [timeAverage()]. If `stat == "limit"`, each value is checked against the
#'   `limit` and the number of values exceeding the limit are returned.
#'
#'  
#'
#' @examples
#' # calculate some UK AQ limits
#' calcAQStats(
#'   mydata = openair::mydata,
#'   aqstats = list(
#'     # PM10 limits
#'     aqStat("pm10", "limit", "day", limit = 50),
#'     aqStat("pm10", "limit", "year", limit = 40),
#'     # NO2 limits
#'     aqStat("no2", "limit", "hour", limit = 200),
#'     aqStat("no2", "limit", "year", limit = 40),
#'     # O3 limit
#'     aqStat("o3", "limit", "hour", roll = 8L, limit = 100),
#'     # SO2 limits
#'     aqStat("so2", "limit", "15 min", limit = 266),
#'     aqStat("so2", "limit", "hour", limit = 350),
#'     aqStat("so2", "limit", "day", limit = 125),
#'     # CO limits
#'     aqStat("co", "limit", "hour", roll = 8L, roll.avg.time = "day", limit = 10)
#'   ),
#'   period = "year"
#' )
calcAQStats <- function(mydata,
                        aqstats,
                        period = c("year"),
                        progress = TRUE) {
  period <- match.arg(period)
  
  # catch passing a single aqstat
  if (inherits(aqstats, "aqStat")) {
    aqstats <- list(aqstats)
  }
  
  # catch passing things that aren't aqstats
  if (!all(purrr::map_vec(test, function(x)
    inherits(x, "aqStat")))) {
    cli::cli_abort(
      c("x" = "Unknown object passed to {.field aqstats}.",
        "i" = "Please pass a {.fun list} of objects created by {.fun aqStat}.")
    )
  }
  
  # calculate air quality stats
  purrr::map(
    aqstats,
    ~ calculate_aqstat(mydata, .x, period = period),
    .progress = ifelse(progress, "Calculating Statistics", FALSE)
  ) %>%
    purrr::reduce(function(x, y)
      dplyr::left_join(x, y, by = "date"))
}

#' @rdname calc-aq-stats
#' @order 2
#'
#' @param pollutant The pollutant (or other named variable) of interest, e.g.,
#'   `"no2"`. This should align with a column name in the `data.frame` passed to
#'   [calcAQStats()].
#' @param stat The grand statistic to calculate. Simple statistics include
#'   `"mean"` (the default), `"min"`, `"median"` or `"max"`. Can also be
#'   `"percentile"`, the percentile being defined using the `percentile` option.
#'   Finally, can be `"limit"`, which compares values against the `limit` value
#'   and returns the number of instances where the value exceeds the limit.
#' @param avg.time An initial averaging time to calculate *before* the `stat` is
#'   calculated; passed directly to [timeAverage()].
#' @param roll.width If an integer value is provided, a rolling mean will be
#'   calculated *after* time averaging and before *stat*. For example, to
#'   calculate an 8-hour rolling mean set `avg.time = "1 hour", roll = 8L`.
#' @param roll.avg.time,roll.avg.stat If `roll.width` is specified, users can
#'   additionally set `roll.avg.time` and `roll.avg.stat` to time average the
#'   rolling mean values using [timeAverage()]. This could be useful to
#'   calculate a *max* daily running 8 hour mean to compare with an ozone limit,
#'   for example.
#' @param limit The limit value, for `stat = "limit"`.
#' @param percentile The percentile value, for `stat = "percentile"`.
#' @param name Optionally change the output column name for this air quality
#'   statistic.
#'
#' @author Jack Davison
#' @export
#' 
aqStat <- function(pollutant = "no2",
                   stat = c("mean", "min", "median", "max", "percentile", "limit"),
                   avg.time = "hour",
                   limit = NA,
                   percentile = NA,
                   roll.width = NA,
                   roll.avg.time = NA,
                   roll.avg.stat = "max",
                   name = NULL) {
  stat <- match.arg(stat)
  
  out <-
    list(
      pollutant = pollutant,
      stat = stat,
      avg.time = avg.time,
      limit = limit,
      percentile = percentile,
      roll = roll.width,
      roll.avg.time = roll.avg.time,
      roll.avg.stat = roll.avg.stat,
      name = name
    )
  
  class(out) <- "aqStat"
  
  return(out)
}

#' Helper for calculating air quality statistics
#' @noRd
calculate_aqstat <- function(data, aqstat, period) {
  thedata <- dplyr::select(data, dplyr::all_of(c("date", aqstat$pollutant)))
  
  thedata <- openair::timeAverage(thedata, avg.time = aqstat$avg.time)
  
  if (!is.na(aqstat$roll)) {
    thedata <-
      openair::rollingMean(
        thedata,
        pollutant = aqstat$pollutant,
        width = aqstat$roll,
        new.name = aqstat$pollutant
      )
    
    if (!is.na(aqstat$roll.avg.time)) {
      thedata <-
        openair::timeAverage(
          thedata,
          pollutant = aqstat$pollutant,
          avg.time = aqstat$roll.avg.time,
          statistic = aqstat$roll.avg.stat
        )
    }
  }
  
  if (aqstat$stat != "limit") {
    thedata <-
      openair::timeAverage(
        thedata,
        avg.time = period,
        statistic = aqstat$stat,
        percentile = aqstat$percentile
      )
  } else {
    thedata <-
      thedata %>%
      openair::cutData(period) %>%
      dplyr::mutate(date = min(date, na.rm = TRUE),
                    .by = dplyr::all_of(period)) %>%
      dplyr::summarise(calcstat = sum(.data[[aqstat$pollutant]] > aqstat$limit, na.rm = TRUE),
                       .by = "date")
  }
  
  if (is.null(aqstat$name)) {
    newname <- paste(aqstat$pollutant, aqstat$stat, aqstat$avg.time, sep = ".")
    if (!is.na(aqstat$roll)) {
      if (!is.na(aqstat$roll.avg.time)) {
        newname <- paste(newname, paste0(aqstat$roll.avg.stat, aqstat$roll.avg.time, "roll", aqstat$roll), sep = ".")
      } else {
        newname <- paste(newname, paste0("roll", aqstat$roll), sep = ".")
      }
    }
    if (aqstat$stat == "limit") {
      newname <- paste(newname, aqstat$limit, sep = ".")
    }
    if (aqstat$stat == "percentile") {
      newname <- paste(newname, aqstat$percentile, sep = ".")
    }
  } else {
    newname <- aqstat$name
  }
  
  names(thedata)[2] <- newname
  
  return(thedata)
}

#' @method print aqStat
#' @author Jack Davison
#' @export
print.aqStat <- function(x) {
  cli::cli_h2("Air Quality Statistic")
  cli::cli_inform(c("i" = "Please use in {.fun calcAQStats}"))
  
  cli::cli_h3("Parameters")
  purrr::imap(x, ~ paste0("{.strong ", toupper(.y), "}: '", .x, "'")) %>%
    purrr::list_c() %>%
    cli::cli_inform()
}

