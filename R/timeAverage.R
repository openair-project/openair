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
#' When a data capture threshold is set through \code{data.thresh} it is
#' necessary for \code{timeAverage} to know what the original time interval of
#' the input time series is. The function will try and calculate this interval
#' based on the most common time gap (and will print the assumed time gap to the
#' screen). This works fine most of the time but there are occasions where it
#' may not e.g. when very few data exist in a data frame or the data are monthly
#' (i.e. non-regular time interval between months). In this case the user can
#' explicitly specify the interval through \code{interval} in the same format as
#' \code{avg.time} e.g. \code{interval = "month"}. It may also be useful to set
#' \code{start.date} and \code{end.date} if the time series do not span the
#' entire period of interest. For example, if a time series ended in October and
#' annual means are required, setting \code{end.date} to the end of the year
#' will ensure that the whole period is covered and that \code{data.thresh} is
#' correctly calculated. The same also goes for a time series that starts later
#' in the year where \code{start.date} should be set to the beginning of the
#' year.
#'
#' \code{timeAverage} should be useful in many circumstances where it is
#' necessary to work with different time average data. For example, hourly air
#' pollution data and 15-minute meteorological data. To merge the two data sets
#' \code{timeAverage} can be used to make the meteorological data 1-hour means
#' first. Alternatively, \code{timeAverage} can be used to expand the hourly
#' data to 15 minute data - see example below.
#'
#' For the research community \code{timeAverage} should be useful for dealing
#' with outputs from instruments where there are a range of time periods used.
#'
#' It is also very useful for plotting data using \code{\link{timePlot}}. Often
#' the data are too dense to see patterns and setting different averaging
#' periods easily helps with interpretation.
#'
#' @param mydata A data frame containing a \code{date} field . Can be class
#'   \code{POSIXct} or \code{Date}.
#' @param avg.time This defines the time period to average to. Can be
#'   \dQuote{sec}, \dQuote{min}, \dQuote{hour}, \dQuote{day}, \dQuote{DSTday},
#'   \dQuote{week}, \dQuote{month}, \dQuote{quarter} or \dQuote{year}. For much
#'   increased flexibility a number can precede these options followed by a
#'   space. For example, a timeAverage of 2 months would be \code{period = "2
#'   month"}. In addition, \code{avg.time} can equal \dQuote{season}, in which
#'   case 3-month seasonal values are calculated with spring defined as March,
#'   April, May and so on.
#'
#'   Note that \code{avg.time} can be \emph{less} than the time interval of the
#'   original series, in which case the series is expanded to the new time
#'   interval. This is useful, for example, for calculating a 15-minute time
#'   series from an hourly one where an hourly value is repeated for each new
#'   15-minute period. Note that when expanding data in this way it is necessary
#'   to ensure that the time interval of the original series is an exact
#'   multiple of \code{avg.time} e.g. hour to 10 minutes, day to hour. Also, the
#'   input time series must have consistent time gaps between successive
#'   intervals so that \code{timeAverage} can work out how much \sQuote{padding}
#'   to apply. To pad-out data in this way choose \code{fill = TRUE}.
#' @param data.thresh The data capture threshold to use (%). A value of zero
#'   means that all available data will be used in a particular period
#'   regardless if of the number of values available. Conversely, a value of 100
#'   will mean that all data will need to be present for the average to be
#'   calculated, else it is recorded as \code{NA}. See also \code{interval},
#'   \code{start.date} and \code{end.date} to see whether it is advisable to set
#'   these other options.
#'
#' @param statistic The statistic to apply when aggregating the data; default is
#'   the mean. Can be one of \dQuote{mean}, \dQuote{max}, \dQuote{min},
#'   \dQuote{median}, \dQuote{frequency}, \dQuote{sum}, \dQuote{sd},
#'   \dQuote{percentile}. Note that \dQuote{sd} is the standard deviation,
#'   \dQuote{frequency} is the number (frequency) of valid records in the period
#'   and \dQuote{data.cap} is the percentage data capture. \dQuote{percentile}
#'   is the percentile level (%) between 0-100, which can be set using the
#'   \dQuote{percentile} option --- see below. Not used if \code{avg.time =
#'   "default"}.
#'
#' @param type \code{type} allows \code{timeAverage} to be applied to cases
#'   where there are groups of data that need to be split and the function
#'   applied to each group. The most common example is data with multiple sites
#'   identified with a column representing site name e.g. \code{type = "site"}.
#'   More generally, \code{type} should be used where the date repeats for a
#'   particular grouping variable. However, if type is not supplied the data
#'   will still be averaged but the grouping variables (character or factor)
#'   will be dropped.
#' @param percentile The percentile level used when \code{statistic =
#'   "percentile"}. The default is 95%.
#' @param start.date A string giving a start date to use. This is sometimes
#'   useful if a time series starts between obvious intervals. For example, for
#'   a 1-minute time series that starts \dQuote{2009-11-29 12:07:00} that needs
#'   to be averaged up to 15-minute means, the intervals would be
#'   \dQuote{2009-11-29 12:07:00}, \dQuote{2009-11-29 12:22:00} etc. Often,
#'   however, it is better to round down to a more obvious start point e.g.
#'   \dQuote{2009-11-29 12:00:00} such that the sequence is then
#'   \dQuote{2009-11-29 12:00:00}, \dQuote{2009-11-29 12:15:00} \ldots{}
#'   \code{start.date} is therefore used to force this type of sequence.
#' @param end.date A string giving an end date to use. This is sometimes useful
#'   to make sure a time series extends to a known end point and is useful when
#'   \code{data.thresh} > 0 but the input time series does not extend up to the
#'   final full interval. For example, if a time series ends sometime in October
#'   but annual means are required with a data capture of >75 % then it is
#'   necessary to extend the time series up until the end of the year. Input in
#'   the format yyyy-mm-dd HH:MM.
#' @param interval The \code{timeAverage} function tries to determine the
#'   interval of the original time series (e.g. hourly) by calculating the most
#'   common interval between time steps. The interval is needed for calculations
#'   where the \code{data.thresh} >0. For the vast majority of regular time
#'   series this works fine. However, for data with very poor data capture or
#'   irregular time series the automatic detection may not work. Also, for time
#'   series such as monthly time series where there is a variable difference in
#'   time between months users should specify the time interval explicitly e.g.
#'   \code{interval = "month"}. Users can also supply a time interval to
#'   \emph{force} on the time series. See \code{avg.time} for the format.
#'
#'   This option can sometimes be useful with \code{start.date} and
#'   \code{end.date} to ensure full periods are considered e.g. a full year when
#'   \code{avg.time = "year"}.
#' @param vector.ws Should vector averaging be carried out on wind speed if
#'   available? The default is \code{FALSE} and scalar averages are calculated.
#'   Vector averaging of the wind speed is carried out on the u and v wind
#'   components. For example, consider the average of two hours where the wind
#'   direction and speed of the first hour is 0 degrees and 2m/s and 180 degrees
#'   and 2m/s for the second hour. The scalar average of the wind speed is
#'   simply the arithmetic average = 2m/s and the vector average is 0m/s.
#'   Vector-averaged wind speeds will always be lower than scalar-averaged
#'   values.
#' @param fill When time series are expanded i.e. when a time interval is less
#'   than the original time series, data are \sQuote{padded out} with \code{NA}.
#'   To \sQuote{pad-out} the additional data with the first row in each original
#'   time interval, choose \code{fill = TRUE}.
#' @param progress Show a progress bar when many groups make up `type`? Defaults
#'   to `TRUE`.
#' @param ... Additional arguments for other functions calling
#'   \code{timeAverage}.
#' @import dplyr
#' @export
#' @return Returns a data frame with date in class \code{POSIXct}.
#' @author David Carslaw
#' @seealso See \code{\link{timePlot}} that plots time series data and uses
#'   \code{timeAverage} to aggregate data where necessary.
#' @examples
#'
#' ## daily average values
#' daily <- timeAverage(mydata, avg.time = "day")
#'
#' ## daily average values ensuring at least 75 % data capture
#' ## i.e. at least 18 valid hours
#' \dontrun{daily <- timeAverage(mydata, avg.time = "day", data.thresh = 75)}
#'
#' ## 2-weekly averages
#' \dontrun{fortnight <- timeAverage(mydata, avg.time = "2 week")}
#'
#' ## make a 15-minute time series from an hourly one
#' \dontrun{
#' min15 <-  timeAverage(mydata, avg.time = "15 min", fill = TRUE)
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
timeAverage <- function(mydata, avg.time = "day", data.thresh = 0,
                        statistic = "mean", type = "default", percentile = NA,
                        percentileInterpolation=TRUE,
                        start.date = NA, end.date = NA, interval = NA,
                        vector.ws = FALSE, fill = FALSE, progress = TRUE, ...) {

  ## get rid of R check annoyances
  year <- season <- month <- Uu <- Vv <- site <- default <- wd <- ws <- NULL

  ## extract variables of interest
  vars <- unique(c("date", names(mydata)))

  ## whether a time series has already been padded to fill time gaps
  padded <- FALSE

  mydata <- checkPrep(mydata, vars,
    type = "default", remove.calm = FALSE,
    strip.white = FALSE
  )

  ## time zone of data
  TZ <- attr(mydata$date, "tzone")
  if (is.null(TZ)) TZ <- "GMT" ## as it is on Windows for BST

  if (!is.na(percentile)) {
    percentile <- percentile / 100
    if (percentile < 0 | percentile > 100) stop("Percentile range outside 0-100")
  }
  if (data.thresh < 0 | data.thresh > 100) stop("Data capture range outside 0-100")

  ## make data.thresh a number between 0 and 1
  data.thresh <- data.thresh / 100

  if (!statistic %in% c(
    "mean", "median", "frequency", "max", "min", "sum",
    "sd", "percentile", "data.cap"
  )) {
    stop("Statistic not recognised")
  }

  if (statistic == "mean") FUN <- function(x) mean(x, na.rm = TRUE)
  if (statistic == "median") FUN <- function(x) median(x, na.rm = TRUE)
  if (statistic == "frequency") FUN <- function(x) length(na.omit(x))
  if (statistic == "max") {
    FUN <- function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    }
  }
  if (statistic == "min") FUN <- function(x) min(x, na.rm = TRUE)
  if (statistic == "sum") {
    FUN <- function(x) {
      if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
    }
  }

  if (statistic == "sd") FUN <- function(x) sd(x, na.rm = TRUE)
  if (statistic == "data.cap") {
    FUN <- function(x) {
      if (all(is.na(x))) {
        res <- 0
      } else {
        res <- 100 * (1 - sum(is.na(x)) / length(x))
      }
      res
    }
  }

  if (statistic == "percentile" & percentileInterpolation==TRUE) {
    FUN <- function(x)
      quantile(x, probs = percentile,na.rm = TRUE)
  } else if (statistic == "percentile" & percentileInterpolation==FALSE){
    FUN <-function(x){
      sorted_data <- sort(na.omit(x))
      n <- length(sorted_data)
      position <- trunc(((percentile) * n)+sign(((percentile) * n))*0.5)
      return(sorted_data[position])
    }
  }

  calc.mean <- function(mydata, start.date) { ## function to calculate means

    ## need to check whether avg.time is > or < actual time gap of data
    ## then data will be expanded or aggregated accordingly

    ## start from a particular time, if given
    if (!is.na(start.date)) {
      firstLine <- data.frame(date = as.POSIXct(start.date, tz = TZ))

      ## add in type
      #firstLine[[type]] <- mydata[[type]][1]
      firstLine[type] <- mydata[1, type]
      mydata <- bind_rows(firstLine, mydata)

      ## for cutting data must ensure it is in GMT because combining
      ## data frames when system is not GMT puts it in local time!...
      ## and then cut makes a string/factor levels with tz lost...

      mydata$date <- as.POSIXct(format(mydata$date), tz = TZ)
    }

    if (!is.na(end.date)) {
      lastLine <- data.frame(date = as.POSIXct(end.date, tz = TZ))
      #lastLine[[type]] <- mydata[[type]][1]
      lastLine[type] <- mydata[1, type]

      mydata <- bind_rows(mydata, lastLine)

      ## for cutting data must ensure it is in GMT because combining
      ## data frames when system is not GMT puts it in local time!...
      ## and then cut makes a string/factor levels with tz lost...

      mydata$date <- as.POSIXct(format(mydata$date), tz = TZ)
    }

    ## if interval specified, then use it
    if (!is.na(interval)) {
      mydata <- mydata %>%
        group_by(across(type)) %>%
        do(date.pad2(., type = type, interval = interval))

      ## make sure missing types are inserted
      #  mydata[[type]] <- mydata[[type]][1]
      mydata[type] <- mydata[type] <- mydata[1, type]

      padded <- TRUE
    }

    ## If interval of original time series not specified, calculate it
    ## time diff in seconds of original data
    timeDiff <- as.numeric(strsplit(
      find.time.interval(mydata$date),
      " "
    )[[1]][1])

    ## time diff of new interval
    by2 <- strsplit(avg.time, " ", fixed = TRUE)[[1]]

    seconds <- 1
    if (length(by2) > 1) seconds <- as.numeric(by2[1])
    units <- by2[length(by2)]


    if (units == "sec") int <- 1
    if (units == "min") int <- 60
    if (units == "hour") int <- 3600
    if (units == "day") int <- 3600 * 24
    if (units == "week") int <- 3600 * 24 * 7
    if (units == "month") int <- 3600 * 24 * 31 ## approx
    if (units == "quarter" || units == "season") int <- 3600 * 24 * 31 * 3 ## approx
    if (units == "year") int <- 3600 * 8784 ## approx

    seconds <- seconds * int ## interval in seconds
    if (is.na(timeDiff)) timeDiff <- seconds ## when only one row

    ## check to see if we need to expand data rather than aggregate it
    ## i.e. chosen time interval less than that of data
    if (seconds < timeDiff) {

      ## original dates
      theDates <- mydata$date

      ## need to add a date to the end when expanding times
      interval <- find.time.interval(mydata$date)

      ## equivalent number of days, used to refine interval for month/year
      days <- as.numeric(strsplit(interval, split = " ")[[1]][1]) /
        24 / 3600

      ## find time interval of data
      if (class(mydata$date)[1] == "Date") {
        interval <- paste(days, "day")
      } else {
        ## this will be in seconds
        interval <- find.time.interval(mydata$date)
      }

      ## better interval, most common interval in a year
      if (days %in% c(30, 31)) interval <- "month"
      if (days %in% c(365, 366)) interval <- "year"

      allDates <- seq(min(mydata$date), max(mydata$date), by = interval)
      allDates <- c(allDates, max(allDates) + timeDiff)

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
        inflateFac <- timeDiff / seconds
        if (timeDiff %% seconds != 0) {
          stop("Non-regular time expansion selected, or non-regular input time series.")
        }

        ## ids of original dates in new dates
        ids <- which(mydata$date %in% theDates)

        date <- mydata$date
        mydata <- subset(mydata, select = -date)

        for (i in 1:(inflateFac - 1)) {
          mydata[ids + i, ] <- mydata[ids, ]
        }

        mydata <- cbind(date, mydata)
        mydata <- mydata[1:nrow(mydata) - 1, ] ## don't need last row
      }

      return(mydata)
    }



    ## calculate wind components
    if ("wd" %in% names(mydata)) {
      if (is.numeric(mydata$wd) && "ws" %in% names(mydata)) {
        mydata <- transform(mydata,
          Uu = ws * sin(2 * pi * wd / 360),
          Vv = ws * cos(2 * pi * wd / 360)
        )
      }

      if (is.numeric(mydata$wd) && !"ws" %in% names(mydata)) {
        mydata <- transform(mydata,
          Uu = sin(2 * pi * wd / 360),
          Vv = cos(2 * pi * wd / 360)
        )
      }
    }

    if (avg.time == "season") {
      ## special case for season
      ## need to group specific months: Dec/Jan/Feb etc

      # don't cut again if type = "season"
      if (!"season" %in% type)
        mydata <- cutData(mydata, type = "season", ...)

      ## remove any missing seasons e.g. through type = "season"
      mydata <- mydata[!is.na(mydata$season), ]

      ## calculate year
      mydata <- mutate(mydata,
        year = year(date),
        month = month(date)
      )

      ## ids where month = 12, make December part of following year's season
      ids <- which(mydata$month == 12)
      mydata$year[ids] <- mydata$year[ids] + 1

      ## find mean date in year-season
      mydata <- transform(mydata, date = ave(date, list(year, season),
        FUN = mean
      ))

      mydata <- subset(mydata, select = -c(year, month))
    }

    ## Aggregate data

    ## variables to split by
    vars <- c(type, "date")

    if (avg.time == "season") vars <- unique(c(vars, "season"))

    if (data.thresh != 0) { ## take account of data capture

      ## need to make sure all data are present..
      ## print out time interval assumed for input time series
      ## useful for debugging
      if (!padded)
        mydata <- date.pad(mydata, type = type)
      
      if (avg.time != "season")
        mydata$date <-
          lubridate::as_datetime(as.character(cut(mydata$date, avg.time)), tz = TZ)
      
      if (statistic == "mean") { ## faster for some reason?

        avmet <- mydata %>%
          group_by(across(vars)) %>%
          summarise(
            across(everything(),
              ~ if (sum(is.na(.x)) / length(.x) <= 1 - data.thresh) {
                mean(.x, na.rm = TRUE)
              } else {
                NA
              }
            )
          )
      } else {
        avmet <- mydata %>%
          group_by(across(vars)) %>%
          summarise(
            across(everything(),
              ~ if (sum(is.na(.x)) / length(.x) <= 1 - data.thresh) {
                FUN(.x)
              } else {
                NA
              }
            )
          )
      }
    } else {

      ## faster if do not need data capture
      if (avg.time != "season") {
        mydata$date <-
          lubridate::as_datetime(as.character(cut(mydata$date, avg.time)), tz = TZ)
        # mydata$date <- as.POSIXct(cut(mydata$date, avg.time), tz = TZ)
      }

      avmet <- # select(mydata, -date) %>%
        mydata %>%
        group_by(across(vars))

      # This is much faster for some reason
      if (statistic == "mean") {
        avmet <- avmet %>%
          summarise(across(everything(), ~mean(.x, na.rm = TRUE)))
      } else {
        avmet <- avmet %>%
          summarise(across(everything(), ~FUN(.x)))
      }
    }


    if ("wd" %in% names(mydata) && statistic != "data.cap") {
      if (is.numeric(mydata$wd)) {

        ## mean wd
        avmet <- transform(avmet,
          wd = as.vector(atan2(Uu, Vv) * 360 / 2 / pi)
        )

        ## correct for negative wind directions
        ids <- which(avmet$wd < 0) ## ids where wd < 0
        avmet$wd[ids] <- avmet$wd[ids] + 360

        ## vector average ws
        if ("ws" %in% names(mydata)) {
          if (vector.ws) {
            avmet <- transform(avmet, ws = (Uu ^ 2 + Vv ^ 2) ^ 0.5)
          }
        }

        avmet <- subset(avmet, select = c(-Uu, -Vv))
      }
    }

    ## fill missing gaps - but only for non-dst data
    if (avg.time != "season" && !any(dst(avmet$date))) {
      avmet <- date.pad2(avmet, type = type, interval = avg.time)
    }

    avmet
  }

  ## cut data into intervals
  mydata <- cutData(mydata, type)

  ## ids of numeric columns, type and date
  ids <- c(
    which(names(mydata) %in% c("date", type)),
    which(sapply(mydata, function(x) !is.factor(x) && !is.character(x)))
  )
  mydata <- mydata[, unique(ids)]


  ## some LAQN data seem to have the odd missing site name
  if ("site" %in% names(mydata)) { ## split by site

    ## remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id, ]
    }

    mydata$site <- factor(mydata$site) ## removes empty factors
  }

  ## calculate stats split by type
  if (progress) progress <- "Calculating Time Averages"
  mydata <- mydata %>%
    group_by(across(type)) %>%
    group_split() %>%
    purrr::map(calc.mean, start.date = start.date,
               .progress = progress) %>%
    purrr::list_rbind() %>% 
    as_tibble()

  ## don't need default column
  if ("default" %in% names(mydata)) {
    mydata <- subset(mydata, select = -default)
  }

  mydata
}
