#' Subset a data frame based on date
#'
#' Utility function to filter a data frame by a date range or specific date
#' periods (month, year, etc.). All options are applied in turn, meaning this
#' function can be used to select quite complex dates simply.
#'
#' @param mydata A data frame containing a `date` field in Date or POSIXct
#'   format.
#' @param start A start date or date-time string in the form d/m/yyyy, m/d/yyyy,
#'   d/m/yyyy HH:MM, m/d/yyyy HH:MM, d/m/yyyy HH:MM:SS, m/d/yyyy HH:MM:SS,
#' yyyy-mm-dd, yyyy-mm-dd HH:MM or yyyy-mm-dd HH:MM:SS.
#' @param end See `start` for format.
#' @param year A year or years to select e.g. `year = 1998:2004` to select
#'   1998-2004 inclusive or `year = c(1998, 2004)` to select 1998 and 2004.
#' @param month A month or months to select. Can either be numeric e.g. `month =
#'   1:6` to select months 1-6 (January to June), or by name e.g. `month =
#'   c("January", "December")`. Names can be abbreviated to 3 letters and be in
#'   lower or upper case.
#' @param day A day name or or days to select. `day` can be numeric (1 to 31) or
#'   character. For example `day = c("Monday", "Wednesday")` or `day = 1:10` (to
#'   select the 1st to 10th of each month). Names can be abbreviated to 3
#'   letters and be in lower or upper case. Also accepts `"weekday"` (Monday -
#'   Friday) and `"weekend"` for convenience.
#' @param hour An hour or hours to select from 0-23 e.g. `hour = 0:12` to select
#'   hours 0 to 12 inclusive.
#' @export
#' @author David Carslaw
#' @examples
#'
#' ## select all of 1999
#' data.1999 <- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999 23:00")
#' head(data.1999)
#' tail(data.1999)
#'
#' # or...
#' data.1999 <- selectByDate(mydata, start = "1999-01-01", end = "1999-12-31 23:00")
#'
#' # easier way
#' data.1999 <- selectByDate(mydata, year = 1999)
#'
#'
#' # more complex use: select weekdays between the hours of 7 am to 7 pm
#' sub.data <- selectByDate(mydata, day = "weekday", hour = 7:19)
#'
#' # select weekends between the hours of 7 am to 7 pm in winter (Dec, Jan, Feb)
#' sub.data <- selectByDate(mydata,
#'   day = "weekend", hour = 7:19, month =
#'     c("dec", "jan", "feb")
#' )
#'
selectByDate <- function(
  mydata,
  start = "1/1/2008",
  end = "31/12/2008",
  year = 2008,
  month = 1,
  day = "weekday",
  hour = 1
) {
  ## extract variables of interest
  vars <- names(mydata)

  ## check data - mostly date format
  mydata <- checkPrep(
    mydata,
    vars,
    "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )

  weekday.names <- format(ISOdate(2000, 1, 3:9), "%A")

  date_formats <- c("dmy", "ymd", "mdy")

  if (!missing(start)) {
    if (lubridate::is.Date(mydata$date)) {
      start <- lubridate::as_date(start, format = date_formats)
    } else {
      start <- lubridate::parse_date_time(
        start,
        orders = c(
          "dmy_HM",
          "dmy_HMS",
          "ymd_HM",
          "ymd_HMS",
          "dmy",
          "mdy",
          "ymd"
        )
      )
    }

    mydata <- filter(mydata, date >= start)
  }

  if (!missing(end)) {
    if (lubridate::is.Date(mydata$date)) {
      end <- lubridate::as_date(end, format = date_formats)
    } else {
      end <- lubridate::parse_date_time(
        end,
        orders = c(
          "dmy_HM",
          "dmy_HMS",
          "ymd_HM",
          "ymd_HMS",
          "dmy",
          "mdy",
          "ymd"
        )
      )

      # check to see if supplied 'end' includes time component i.e. with a ":"
      if (!grepl(":", end)) {
        # get the maximum date in data on that day
        end <- end + 3600 * 24 - 1
      }
    }

    mydata <- filter(mydata, date <= end)
  }

  if (!missing(year)) {
    mydata <- mydata[which(year(mydata$date) %in% year), ]
  }

  if (!missing(month)) {
    if (is.numeric(month)) {
      if (any(month < 1 | month > 12)) {
        stop("Month must be between 1 to 12.")
      }

      mydata <- mydata[which(month(mydata$date) %in% month), ]
    } else {
      mydata <- subset(
        mydata,
        substr(
          tolower(format(
            date,
            "%B"
          )),
          1,
          3
        ) %in%
          substr(tolower(month), 1, 3)
      )
    }
  }
  if (!missing(hour)) {
    if (any(hour < 0 | hour > 23)) {
      stop("Hour must be between 0 to 23.")
    }

    mydata <- mydata[which(hour(mydata$date) %in% hour), ]
  }

  if (!missing(day)) {
    days <- day

    if (is.numeric(day)) {
      if (any(day < 1 | day > 31)) {
        stop("Day must be between 1 to 31.")
      }
      mydata <- mydata[which(day(mydata$date) %in% day), ]
    } else {
      if (day[1] == "weekday") {
        days <- weekday.names[1:5]
      }
      if (day[1] == "weekend") {
        days <- weekday.names[6:7]
      }
      mydata <- subset(
        mydata,
        substr(tolower(format(date, "%A")), 1, 3) %in%
          substr(tolower(days), 1, 3)
      )
    }
  }
  mydata
}
