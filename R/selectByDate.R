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
library(dplyr)
library(lubridate)

selectByDate <- function(
  mydata,
  start = NULL,
  end = NULL,
  year = NULL,
  month = NULL,
  day = NULL,
  hour = NULL
) {
  # 1. Basic Checks
  if (!"date" %in% names(mydata)) {
    stop("The input data frame must contain a column named 'date'.")
  }

  # Check if date is strictly Date or POSIXt (POSIXct/POSIXlt)
  if (!inherits(mydata$date, "Date") && !inherits(mydata$date, "POSIXt")) {
    stop("The 'date' column must be in Date or POSIXct format.")
  }

  # 2. Filter by Date Range (Start/End)
  if (!is.null(start)) {
    start_date <- lubridate::parse_date_time(
      start,
      orders = c("dmy_HM", "dmy_HMS", "ymd_HM", "ymd_HMS", "dmy", "mdy", "ymd"),
      quiet = TRUE
    )

    if (is.na(start_date)) {
      stop("Could not parse 'start' date format.")
    }
    mydata <- filter(mydata, date >= start_date)
  }

  if (!is.null(end)) {
    end_date <- lubridate::parse_date_time(
      end,
      orders = c("dmy_HM", "dmy_HMS", "ymd_HM", "ymd_HMS", "dmy", "mdy", "ymd"),
      quiet = TRUE
    )

    if (is.na(end_date)) {
      stop("Could not parse 'end' date format.")
    }

    # If input is just a date (e.g. "2023-01-01"), include the full day (until 23:59:59)
    if (is.character(end) && !grepl(":", end)) {
      end_date <- end_date + days(1) - seconds(1)
    }
    mydata <- filter(mydata, date <= end_date)
  }

  # 3. Filter by Year
  if (!is.null(year)) {
    mydata <- filter(mydata, lubridate::year(date) %in% !!year)
  }

  # 4. Filter by Month
  if (!is.null(month)) {
    target_months <- month

    # If user provided names (e.g. "Jan", "february"), convert to integers 1-12
    if (is.character(month)) {
      # Create lookup: "jan" -> 1, "feb" -> 2...
      lookup <- setNames(1:12, tolower(month.abb))

      # Clean input: lowercase and first 3 chars
      clean_input <- substr(tolower(month), 1, 3)
      target_months <- lookup[clean_input]

      if (any(is.na(target_months))) stop("Invalid month name provided.")
    }

    # Perform integer filter (fast)
    mydata <- filter(mydata, lubridate::month(date) %in% !!target_months)
  }

  # 5. Filter by Hour
  if (!is.null(hour)) {
    mydata <- filter(mydata, lubridate::hour(date) %in% !!hour)
  }

  # 6. Filter by Day
  if (!is.null(day)) {
    # CASE A: Numeric input implies day of month (1-31)
    if (is.numeric(day)) {
      mydata <- filter(mydata, lubridate::mday(date) %in% !!day)

      # CASE B: Character input implies day of week (Mon-Sun) or "weekday"/"weekend"
    } else {
      day_lower <- tolower(day)
      target_wdays <- integer() # Store days as integers (Sun=1, Mon=2...Sat=7)

      # Handle keywords
      if ("weekday" %in% day_lower) {
        target_wdays <- c(target_wdays, 2:6)
      }
      if ("weekend" %in% day_lower) {
        target_wdays <- c(target_wdays, 1, 7)
      }

      # Handle specific names (e.g., "Monday")
      specific <- day_lower[!day_lower %in% c("weekday", "weekend")]

      if (length(specific) > 0) {
        # Create lookup: "sun"->1 ... "sat"->7
        # We manually map this to ensure consistency regardless of locale
        # Note: lubridate::wday returns 1 for Sunday, 7 for Saturday
        wday_lookup <- c(
          "sun" = 1,
          "mon" = 2,
          "tue" = 3,
          "wed" = 4,
          "thu" = 5,
          "fri" = 6,
          "sat" = 7
        )

        clean_specific <- substr(specific, 1, 3)
        mapped <- wday_lookup[clean_specific]

        if (any(is.na(mapped))) {
          stop("Invalid day name provided.")
        }
        target_wdays <- c(target_wdays, mapped)
      }

      target_wdays <- unique(target_wdays)

      # Filter using wday integer (fast)
      mydata <- filter(mydata, lubridate::wday(date) %in% !!target_wdays)
    }
  }

  return(mydata)
}
