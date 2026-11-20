#' Subset a data frame based on date
#'
#' Utility function to filter a data frame by a date range or specific date
#' periods (month, year, etc.). All options are applied in turn, meaning this
#' function can be used to select quite complex dates simply.
#'
#' @param mydata A data frame containing a `date` field in Date or POSIXct
#'   format.
#'
#' @param start A single start date. Can be a `Date`, a `POSIXct`, or a
#'   character string in one of the formats given by `format`.
#'
#' @param end A single end date, in any of the forms accepted by `start`. If no
#'   time is given (including when `end` is a `Date`), the whole of that day is
#'   included. More generally, a character `end` includes the whole of the
#'   smallest period its format specifies, e.g., the whole month with `format =
#'   "%Y-%m"`.
#'
#' @param year A year or years to select e.g. `year = 1998:2004` to select
#'   1998-2004 inclusive or `year = c(1998, 2004)` to select 1998 and 2004.
#'
#' @param month A month or months to select. Can either be numeric e.g. `month =
#'   1:6` to select months 1-6 (January to June), or by name e.g. `month =
#'   c("January", "December")`. Names can be abbreviated to 3 letters and be in
#'   lower or upper case.
#'
#' @param day A day name or or days to select. `day` can be numeric (1 to 31) or
#'   character. For example `day = c("Monday", "Wednesday")` or `day = 1:10` (to
#'   select the 1st to 10th of each month). Names can be abbreviated to 3
#'   letters and be in lower or upper case. Also accepts `"weekday"` (Monday -
#'   Friday) and `"weekend"` for convenience.
#'
#' @param hour An hour or hours to select from 0-23 e.g. `hour = 0:12` to select
#'   hours 0 to 12 inclusive.
#'
#' @param format When `start` and/or `end` are character strings, the format(s)
#'   used to read them, in [strptime()] notation. Formats are tried in order and
#'   the first that matches the whole string is used. The defaults accept
#'   yyyy-mm-dd and dd/mm/yyyy dates, optionally followed by HH:MM or HH:MM:SS.
#'   For example, use `format = "%m/%d/%Y"` for US-style dates, or `format =
#'   "%Y-%m"` to give `start` and `end` as months.
#'
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
#' data.1999 <- selectByDate(mydata, start = "1999-01-01", end = "1999-12-31")
#'
#' # US-style dates, or whole months, can be given using `format`
#' data.1999 <- selectByDate(
#'   mydata,
#'   start = "01/01/1999",
#'   end = "12/31/1999",
#'   format = "%m/%d/%Y"
#' )
#' data.summer <- selectByDate(
#'   mydata,
#'   start = "1999-06",
#'   end = "1999-08",
#'   format = "%Y-%m"
#' )
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
  start = NULL,
  end = NULL,
  year = NULL,
  month = NULL,
  day = NULL,
  hour = NULL,
  format = c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%d",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d/%m/%Y"
  )
) {
  # 1. Basic Checks
  if (!"date" %in% names(mydata)) {
    cli::cli_abort("{.arg mydata} must contain a {.field date} column.")
  }

  # Check if date is strictly Date or POSIXt (POSIXct/POSIXlt)
  if (!inherits(mydata$date, "Date") && !inherits(mydata$date, "POSIXt")) {
    cli::cli_abort(
      "{.code mydata$date} must be in {.cls Date} or {.cls POSIXct} format, \\
      not {.obj_type_friendly {mydata$date}}."
    )
  }

  # check numeric selectors are within their valid ranges
  check_sbd_range(month, 1:12)
  check_sbd_range(hour, 0:23)
  if (is.numeric(day)) {
    check_sbd_range(day, 1:31)
  }

  # Date columns have no time of day to select on
  if (!is.null(hour) && inherits(mydata$date, "Date")) {
    cli::cli_abort(
      "{.arg hour} can't be used when {.code mydata$date} is a {.cls Date}, as it has no time of day."
    )
  }

  # 2. Filter by Date Range (Start/End)
  # parse in the time zone of the data so that boundaries are local times
  data_tz <- lubridate::tz(mydata$date)

  if (!is.null(start)) {
    start_date <- parse_sbd_date(start, format, tz = data_tz, is_end = FALSE)
    mydata <- dplyr::filter(mydata, date >= start_date)
  }

  if (!is.null(end)) {
    end_date <- parse_sbd_date(end, format, tz = data_tz, is_end = TRUE)
    mydata <- dplyr::filter(mydata, date <= end_date)
  }

  # 3. Filter by Year
  if (!is.null(year)) {
    mydata <- dplyr::filter(mydata, lubridate::year(date) %in% !!year)
  }

  # 4. Filter by Month
  if (!is.null(month)) {
    target_months <- month

    # If user provided names (e.g. "Jan", "february"), convert to integers 1-12
    if (is.character(month)) {
      # Create lookup: "jan" -> 1, "feb" -> 2...
      lookup <- stats::setNames(1:12, tolower(month.abb))

      # Clean input: lowercase and first 3 chars
      clean_input <- substr(tolower(month), 1, 3)
      target_months <- lookup[clean_input]

      if (anyNA(target_months)) {
        cli::cli_abort(
          "Invalid {.arg month} name{?s}: {.val {month[is.na(target_months)]}}."
        )
      }
    }

    # Perform integer dplyr::filter (fast)
    mydata <- dplyr::filter(mydata, lubridate::month(date) %in% !!target_months)
  }

  # 5. Filter by Hour
  if (!is.null(hour)) {
    mydata <- dplyr::filter(mydata, lubridate::hour(date) %in% !!hour)
  }

  # 6. Filter by Day
  if (!is.null(day)) {
    # CASE A: Numeric input implies day of month (1-31)
    if (is.numeric(day)) {
      mydata <- dplyr::filter(mydata, lubridate::mday(date) %in% !!day)

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

        if (anyNA(mapped)) {
          cli::cli_abort(
            "Invalid {.arg day} name{?s}: {.val {specific[is.na(mapped)]}}."
          )
        }
        target_wdays <- c(target_wdays, mapped)
      }

      target_wdays <- unique(target_wdays)

      # Filter using wday integer (fast)
      mydata <- dplyr::filter(mydata, lubridate::wday(date) %in% !!target_wdays)
    }
  }

  return(mydata)
}

#' Parse a `start` or `end` date for `selectByDate()`
#'
#' Accepts a single `Date`, `POSIXct` or character date. Character dates are
#' parsed strictly, using the first of `format` that matches the whole string.
#' When `is_end = TRUE`, a date with no time is extended to the end of the
#' smallest period it specifies (the whole day for a `Date` or "%Y-%m-%d", the
#' whole month for "%Y-%m", etc.).
#' @noRd
parse_sbd_date <- function(x, format, tz, is_end, call = rlang::caller_env()) {
  arg <- if (is_end) "end" else "start"

  if (length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single date, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  # the period to extend `end` by, if it has no time
  period <- NULL

  if (inherits(x, "POSIXt")) {
    out <- x
  } else if (inherits(x, "Date")) {
    out <- lubridate::make_datetime(
      lubridate::year(x),
      lubridate::month(x),
      lubridate::day(x),
      tz = tz
    )
    period <- lubridate::days(1)
  } else if (is.character(x)) {
    # strict parsing; the first format matching the whole string is used
    x <- trimws(x)
    out <- as.POSIXct(NA)
    for (fmt in format) {
      out <- lubridate::parse_date_time(
        x,
        orders = fmt,
        exact = TRUE,
        tz = tz,
        quiet = TRUE
      )
      if (!is.na(out)) {
        period <- sbd_format_period(fmt)
        break
      }
    }
  } else {
    out <- as.POSIXct(NA)
  }

  if (is.na(out)) {
    cli::cli_abort(
      c(
        "x" = "Could not parse {.arg {arg}} date {.val {format(x)}}.",
        "i" = "Use a {.cls Date}, a {.cls POSIXct}, or a string in one of these {.arg format}s: {.val {format}}."
      ),
      call = call
    )
  }

  # an `end` with no time includes the whole of the period it specifies
  if (is_end && !is.null(period)) {
    out <- out + period - lubridate::seconds(1)
  }

  out
}

#' The smallest period specified by a strptime format, or `NULL` if the format
#' includes a time of day
#' @noRd
sbd_format_period <- function(fmt) {
  fmt <- gsub("%%", "", fmt, fixed = TRUE)
  has <- function(codes) grepl(paste0("%O?[", codes, "]"), fmt)
  if (has("HIMSTRpkl")) {
    NULL
  } else if (has("dejDFx")) {
    lubridate::days(1)
  } else if (has("mbBh")) {
    lubridate::period(1, "month")
  } else {
    lubridate::years(1)
  }
}

#' Check numeric `selectByDate()` selectors are within their valid range
#' @noRd
check_sbd_range <- function(
  x,
  valid,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (is.null(x) || !is.numeric(x)) {
    return(invisible(NULL))
  }
  bad <- x[!x %in% valid]
  if (length(bad) > 0) {
    cli::cli_abort(
      "{.arg {arg}} must be between {.val {min(valid)}} and {.val {max(valid)}}, not {.val {bad}}.",
      call = call
    )
  }
  invisible(NULL)
}
