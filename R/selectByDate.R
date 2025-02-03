#' Subset a data frame based on date
#'
#' Utility function to filter a data frame by a date range or specific date
#' periods (month, year, etc.). All options are applied in turn, meaning this
#' function can be used to select quite complex dates simply.
#'
#' Dates are assumed to be inclusive, so \code{start = "1/1/1999"} means that
#' times are selected from hour zero. Similarly, \code{end = "31/12/1999"} will
#' include all hours of the 31st December. \code{start} and \code{end} can also
#' be in standard R format as a string i.e. "YYYY-mm-dd", so \code{start =
#' "1999-01-01"} is fine.
#'
#' @param mydata A data frame containing a \code{date} field in hourly or high
#'   resolution format.
#' @param start A start date string in the form d/m/yyyy e.g. \dQuote{1/2/1999}
#'   or in \sQuote{R} format i.e. \dQuote{YYYY-mm-dd}, \dQuote{1999-02-01}
#' @param end See \code{start} for format.
#' @param year A year or years to select e.g. \code{year = 1998:2004} to select
#'   1998-2004 inclusive or \code{year = c(1998, 2004)} to select 1998 and 2004.
#' @param month A month or months to select. Can either be numeric e.g.
#'   \code{month = 1:6} to select months 1-6 (January to June), or by name e.g.
#'   \code{month = c("January", "December")}. Names can be abbreviated to 3
#'   letters and be in lower or upper case.
#' @param day A day name or or days to select. \code{day} can be numeric (1 to
#'   31) or character. For example \code{day = c("Monday", "Wednesday")} or
#'   \code{day = 1:10} (to select the 1st to 10th of each month). Names can be
#'   abbreviated to 3 letters and be in lower or upper case. Also accepts
#'   \dQuote{weekday} (Monday - Friday) and \dQuote{weekend} for convenience.
#' @param hour An hour or hours to select from 0-23 e.g. \code{hour = 0:12} to
#'   select hours 0 to 12 inclusive.
#' @export
#' @author David Carslaw
#' @examples
#'
#' ## select all of 1999
#' data.1999 <- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999")
#' head(data.1999)
#' tail(data.1999)
#'
#' # or...
#' data.1999 <- selectByDate(mydata, start = "1999-01-01", end = "1999-12-31")
#'
#' # easier way
#' data.1999 <- selectByDate(mydata, year = 1999)
#'
#'
#' # more complex use: select weekdays between the hours of 7 am to 7 pm
#' sub.data <- selectByDate(mydata, day = "weekday", hour = 7:19)
#'
#' # select weekends between the hours of 7 am to 7 pm in winter (Dec, Jan, Feb)
#' sub.data <- selectByDate(mydata, day = "weekend", hour = 7:19, month =
#' c("dec", "jan", "feb"))
#' 
selectByDate <- function(mydata, start = "1/1/2008",
                         end = "31/12/2008", year = 2008,
                         month = 1, day = "weekday", hour = 1) {
  ## extract variables of interest
  vars <- names(mydata)
  
  ## check data - mostly date format
  mydata <- checkPrep(
    mydata, vars, "default",
    remove.calm = FALSE,
    strip.white = FALSE
  )
  
  weekday.names <- format(ISOdate(2000, 1, 3:9), "%A")
  
  
  if (!missing(start)) {
    
    
    ## assume R date format
    start <- as_date(parse_date_time(start, c("ymd", "dmy")))
    
    mydata <- subset(mydata, as_date(date) >= start)
  }
  
  if (!missing(end)) {
    
    
    ## assume R date format
    end <-as_date(parse_date_time(end, c("ymd", "dmy")))
    
    mydata <- subset(mydata, as_date(date) <= end)
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
    }
    
    else {
      mydata <- subset(mydata, substr(tolower(format(
        date,
        "%B"
      )), 1, 3) %in% substr(tolower(month), 1, 3))
    }
  }
  if (!missing(hour)) {
    if (any(hour < 0 | hour > 23)) stop("Hour must be between 0 to 23.")
    
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
      mydata <- subset(mydata, substr(tolower(format(date, "%A")), 1, 3) %in%
                         substr(tolower(days), 1, 3))
    }
  }
  mydata
}