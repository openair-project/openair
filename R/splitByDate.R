#' Divide up a data frame by time
#'
#' This function partitions a data frame up into different time segments. It
#' produces a new column called controlled by `name` that can be used in many
#' `openair` functions. Note that there must be one more `labels` than there are
#' `dates`.
#'
#' @param mydata A data frame containing a `date` field in an hourly or high
#'   resolution format.
#' @param dates A date or dates to split data by. Can be passed as R date(time)
#'   objects or as characters. If passed as a character, [splitByDate()] expects
#'   either "DD/MM/YYYY" or "YYYY/MM/DD" by default, but this can be chaned
#'   using the `format` argument.
#' @param labels Labels for each time partition. Should always be one more
#'   `label` than there are `dates`; for example, if `dates = "2020/01/01`,
#'   [splitByDate()] requires one label for *before* that date and one label for
#'   *after*.
#' @param name The name to give the new column to identify the periods split.
#'   Defaults to `"split.by"`.
#' @param format When `dates` are provided as character strings, this option
#'   defines the formats [splitByDate()] will use to coerce `dates` into R
#'   `Date` or `POSIXCt` objects. Passed to [lubridate::as_date()] or
#'   [lubridate::as_datetime()]. See [strptime()] for more information.
#' @export
#' @author David Carslaw
#' @examples
#' # split data up into "before" and "after"
#' mydata <- splitByDate(mydata,
#'   dates = "1/04/2000",
#'   labels = c("before", "after")
#' )
#'
#' # split data into 3 partitions
#' mydata <- splitByDate(mydata,
#'   dates = c("1/1/2000", "1/3/2003"),
#'   labels = c("before", "during", "after")
#' )
#'
#' # if you have modelled data - could split into modelled and measured by the
#' # break date
#' dummy <- data.frame(
#'   date = Sys.Date() + (-5:5),
#'   nox = 100 + seq(-50, 50, 10)
#' )
#' splitByDate(dummy,
#'   dates = Sys.Date(),
#'   labels = c("measured", "modelled"),
#'   name = "data_type"
#' )
splitByDate <- function(mydata,
                        dates = "1/1/2003",
                        labels = c("before", "after"),
                        name = "split.by",
                        format = c("%d/%m/%Y",
                                   "%Y/%m/%d",
                                   "%d/%m/%Y %H:%M:%S",
                                   "%Y/%m/%d %H:%M:%S")) {
  # check data
  mydata <- checkPrep(mydata, names(mydata), "default", remove.calm = FALSE)
  
  # check there are sufficent labels for number of dates
  if (length(dates) != length(labels) - 1) {
    cli::cli_abort(
      c("x" = "There is a mis-match between the number of {.field dates} ({.val {length(dates)}}) and {.field labels} ({.val {length(labels)}}).", "i" = "There should be one more {.field label} than {.field date}.")
    )
  }
  
  # get input timezone of the input data, if date-time
  input_tz <- lubridate::tz(mydata$date)
  
  # handles slightly differently for Date vs POSIXct
  if (is.character(dates)) {
    if (inherits(mydata$date, "Date")) {
      dates <- lubridate::as_date(dates, format = format)
      min_date <- lubridate::as_date(ISOdate(
        year = 0,
        month = 1,
        day = 1
      ))
    } else {
      dates <- lubridate::as_datetime(dates, format = format, tz = input_tz)
      min_date <- ISOdate(
        year = 0,
        month = 1,
        day = 1,
        hour = 0,
        tz = input_tz
      )
    }
  } else if (lubridate::is.POSIXct(dates)) {
    dates <- lubridate::force_tz(dates, tzone = input_tz)
    min_date <- ISOdate(
      year = 0,
      month = 1,
      day = 1,
      hour = 0,
      tz = input_tz
    )
  } else if (lubridate::is.Date(dates)) {
    min_date <- lubridate::as_date(ISOdate(
      year = 0,
      month = 1,
      day = 1
    ))
  }
  
  # perform cut
  mydata[[name]] <- cut(
    mydata$date,
    breaks = c(min_date, dates, max(mydata$date)),
    labels = labels,
    ordered_result = TRUE,
    right = TRUE,
    include.lowest = TRUE
  )
  
  # return data
  return(mydata)
}
