#' Calculate rolling mean pollutant values
#'
#' This is a utility function mostly designed to calculate rolling mean
#' statistics relevant to some pollutant limits, e.g., 8 hour rolling means for
#' ozone and 24 hour rolling means for PM10. However, the function has a more
#' general use in helping to display rolling mean values in flexible ways with
#' the rolling window width left, right or centre aligned. The function will try
#' and fill in missing time gaps to get a full time sequence but return a data
#' frame with the same number of rows supplied.
#'
#' @param mydata A data frame containing a `date` field. `mydata` must contain a
#'   `date` field in `Date` or `POSIXct` format. The input time series must be
#'   regular, e.g., hourly, daily.
#' @param pollutant The name of a pollutant, e.g., `pollutant = "o3"`.
#' @param width The averaging period (rolling window width) to use, e.g., `width
#'   = 8` will generate 8-hour rolling mean values when hourly data are
#'   analysed.
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param data.thresh The % data capture threshold. No values are calculated if
#'   data capture over the period of interest is less than this value. For
#'   example, with `width = 8` and `data.thresh = 75` at least 6 hours are
#'   required to calculate the mean, else `NA` is returned.
#' @param align Specifies how the moving window should be aligned. `"right"`
#'   means that the previous hours (including the current) are averaged.
#'   `"left"` means that the forward hours are averaged. `"centre"` (or
#'   `"center"` - the default) centres the current hour in the window.
#' @param new.name The name given to the new column. If not supplied it will
#'   create a name based on the name of the pollutant and the averaging period
#'   used.
#' @param ... Additional parameters passed to [cutData()]. For use with `type`.
#' @export
#' @author David Carslaw
#' @examples
#' # rolling 8-hour mean for ozone
#' mydata <- rollingMean(mydata,
#'   pollutant = "o3", width = 8, new.name =
#'     "rollingo3", data.thresh = 75, align = "right"
#' )
rollingMean <- function(
  mydata,
  pollutant = "o3",
  width = 8L,
  type = "default",
  data.thresh = 75,
  align = c("centre", "center", "left", "right"),
  new.name = NULL,
  ...
) {
  # check inputs
  align <- rlang::arg_match(align, multiple = FALSE)

  # data.thresh must be between 0 & 100
  if (data.thresh < 0 || data.thresh > 100) {
    cli::cli_abort(
      "{.field data.thresh} must be between {.val {0L}} and {.val {100L}}."
    )
  }

  # pollutant should be numeric
  if (!is.numeric(mydata[[pollutant]])) {
    cli::cli_abort(
      "mydata{.field ${pollutant}} is not numeric - it is {class(mydata[[pollutant]])}."
    )
  }

  # create new name if not provided
  if (is.null(new.name)) {
    new.name <- paste("rolling", width, pollutant, sep = "")
  }

  # setting width < 1 crashes R
  if (width < 1L) {
    width <- 1L
  }

  # deal with type
  if (type == "default" & "site" %in% names(mydata)) {
    # back compatibility to avoid breaking change
    cli::cli_inform(
      '{.field type} is "default" but {.code site} column identified - setting {.field type} to "site".'
    )
    type <- "site"
  }
  
  mydata <- cutData(mydata, type = type, ...)

  checkDuplicateRows(mydata, type)
  
  # function to perform rolling average
  calc.rolling <- function(mydata) {
    # need to know whether dates added
    dates <- mydata$date

    # pad missing hours
    mydata <- date.pad(mydata)

    # make sure function is not called with window width longer than data
    if (width > nrow(mydata)) {
      return(mydata)
    }

    # call C code
    mydata[[new.name]] <- .Call(
      "rollMean",
      mydata[[pollutant]],
      width,
      data.thresh,
      align,
      PACKAGE = "openair"
    )

    # return what was put in; avoids adding missing data e.g. for factors
    if (length(dates) != nrow(mydata)) {
      mydata <- mydata[mydata$date %in% dates, ]
    }

    # return
    return(mydata)
  }

  # split if several sites
  mydata <-
    purrr::map(split(mydata, mydata[[type]], drop = TRUE), calc.rolling) %>%
    purrr::list_rbind()

  # drop default column
  if (type == "default") {
    mydata$default <- NULL
  }

  # return
  return(mydata)
}
