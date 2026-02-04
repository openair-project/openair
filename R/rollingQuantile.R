#' Calculate rolling quantile pollutant values
#'
#' This is a utility function mostly designed to calculate rolling quantile
#' statistics. The function will try
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
#' @param probs Probability for quantile calculate. A number between 0 and 1. Can be more than length one e.g. `probs = c(0.05, 0.95)`.
#' @param date.pad Should missing dates be padded? Default is `FALSE`.
#' @param ... Additional parameters passed to [cutData()]. For use with `type`.
#' @export
#' @return A tibble with new columns for the rolling quantile value and the number of valid values used.
#' @author David Carslaw
#' @examples
#' # rolling 24-hour 0.05 and 0.95 quantile for ozone
#' mydata <- rollingQuantile(mydata,
#'   pollutant = "o3", width = 24, data.thresh = 75, align = "right", probs = c(0.05, 0.95)
#' )
rollingQuantile <- function(
  mydata,
  pollutant = "o3",
  width = 8L,
  type = "default",
  data.thresh = 75,
  align = c("centre", "center", "left", "right"),
  probs = 0.5,
  date.pad = FALSE,
  ...
) {
  # check inputs
  align <- rlang::arg_match(align, multiple = FALSE)

  if (align == "center") {
    align = "centre"
  }

  # data.thresh must be between 0 & 100
  if (data.thresh < 0 || data.thresh > 100) {
    cli::cli_abort(
      "{.field data.thresh} must be between {.val {0L}} and {.val {100L}}."
    )
  }

  if (!all(probs >= 0 & probs <= 1)) {
    cli::cli_abort(
      "{.field probs} must be between {.val {0L}} and {.val {1L}}."
    )
  }

  # pollutant should be numeric
  if (!is.numeric(mydata[[pollutant]])) {
    cli::cli_abort(
      "mydata{.field ${pollutant}} is not numeric - it is {class(mydata[[pollutant]])}."
    )
  }

  # setting width < 1 crashes R
  if (width < 1L) {
    width <- 1L
  }

  # cut data
  mydata <- cutData(mydata, type = type, ...)

  # error if duplicate dates
  checkDuplicateRows(mydata, type, fn = cli::cli_abort)

  # function to perform rolling average
  calc.rolling <- function(mydata) {
    # need to know whether dates added
    dates <- mydata$date

    # pad missing hours
    if (date.pad) {
      mydata <- datePad(mydata)
    }

    # make sure function is not called with window width longer than data
    if (width > nrow(mydata)) {
      return(mydata)
    }

    # call C code

    data.thresh = data.thresh / 100
    results <- rolling_average_cpp(
      mydata[[pollutant]],
      width,
      align,
      data.thresh,
      statistic = "quantile",
      probs = probs
    )

    results <- as.data.frame(results)
    names(results)[1:length(probs)] <- paste0("q_", pollutant, "_", probs)

    mydata <- bind_cols(mydata, results)

    # return what was put in; avoids adding missing data e.g. for factors
    if (length(dates) != nrow(mydata)) {
      mydata <- mydata[mydata$date %in% dates, ]
    }

    # return
    return(mydata)
  }

  # split if several sites
  mydata <- mapType(
    mydata,
    type = type,
    fun = calc.rolling,
    .include_default = TRUE
  )

  # drop default column
  if (any(type == "default")) {
    mydata$default <- NULL
  }

  # return
  return(mydata)
}
