#' Calculate Whittaker-Eilers Smoothing and Interpolation
#'
#' This function applies the Whittaker-Eilers smoothing and interpolation method
#' to a specified pollutant in a data frame. The method is based on penalised
#' least squares and is designed to handle time series data with missing values,
#' providing a smoothed estimate of the pollutant concentrations over time. The
#' function allows for flexible control over the amount of smoothing through the
#' `lambda` parameter and can be applied to multiple pollutants simultaneously.
#'
#' The function is designed to work with regularly spaced time series.
#'
#' @param mydata A data frame containing a `date` field. `mydata` must contain a
#'   `date` field in `Date` or `POSIXct` format.
#' @param pollutant The name of a pollutant, e.g., `pollutant = "o3"`. More than
#'   one pollutant can be supplied as a vector, e.g., `pollutant = c("o3",
#'   "nox")`.
#' @param lambda The value of `lambda` to use in the smoothing. This controls
#'   the amount of smoothing, with higher values leading to smoother results. If
#'   `lambda = NA` Generalised Cross Validation (GCV) is used to select the
#'   optimal value of `lambda` for each pollutant. This can be time consuming,
#'   so a fixed value of `lambda` is recommended for large datasets or multiple
#'   pollutants. Note that the value of `lambda` needs to increase exponetially
#'   to smooth long time series data of several years e.g. `lambda = 10e9`.
#' @param d The order used to penalise the roughness of the data. By default
#'   this is set to 2, which penalises the second derivative of the data.
#'   Setting `d = 1` will penalise the first derivative, which can be useful for
#'   smoothing data with sharp peaks or troughs. Setting `d = 1` will
#'   effectively linearly interpolate across missing data.
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param new.name The name given to the new column(s). If not supplied it will
#'   create a name based on the name of the pollutant.
#' @param date.pad Should missing dates be padded? Default is `FALSE`.
#' @param ... Additional parameters passed to [cutData()]. For use with `type`.
#' @export
#' @return A tibble with new columns for the smoothed pollutant values.
#' @author David Carslaw
#' @references Paul H. C. Eilers, A Perfect Smoother, Analytical Chemistry 2003
#'   75 (14), 3631-3636, DOI: 10.1021/ac034173t
#' @examples
#' # Smoothing with lambda = 24
#' mydata <- WhittakerSmooth(mydata, pollutant = "o3", lambda = 24)
WhittakerSmooth <- function(
  mydata,
  pollutant = "o3",
  lambda = 24L,
  d = 2,
  type = "default",
  new.name = NULL,
  date.pad = FALSE,
  ...
) {
  # check inputs

  # Loop through all provided pollutants to check they are numeric
  for (p in pollutant) {
    if (!is.numeric(mydata[[p]])) {
      cli::cli_abort(
        "mydata{.field ${p}} is not numeric - it is {class(mydata[[p]])}."
      )
    }
  }

  # create new names if not provided or if length mismatch
  if (is.null(new.name) || length(new.name) != length(pollutant)) {
    # If the user supplied a single new.name for multiple pollutants, warn and revert to default
    if (!is.null(new.name) && length(pollutant) > 1) {
      cli::cli_warn(
        "Length of {.arg new.name} does not match length of {.arg pollutant}. Using generated names."
      )
    }
    new.name <- paste0("smooth_", pollutant)
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

    # Loop over each pollutant
    for (i in seq_along(pollutant)) {
      current_poll <- pollutant[i]
      current_name <- new.name[i]

      # call C code
      results <- whittaker_smooth(
        mydata[[current_poll]],
        lambda = lambda,
        d = d
      )

      # Assign results to specific columns
      mydata[[current_name]] <- results[[1]]
    }

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
