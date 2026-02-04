#' Calculate rolling Gaussian smooth of pollutant values
#'
#' This is a utility function designed to calculate rolling Gaussian smooth (kernel smoothing). The function will try
#' and fill in missing time gaps to get a full time sequence but return a data
#' frame with the same number of rows supplied.
#'
#' The function provides centre-aligned smoothing out to 3 sigma, which captures 99.7% of the data.
#'
#' @param mydata A data frame containing a `date` field. `mydata` must contain a
#'   `date` field in `Date` or `POSIXct` format. The input time series must be
#'   regular, e.g., hourly, daily.
#' @param pollutant The name of a pollutant, e.g., `pollutant = "o3"`.
#' @param sigma The value of `sigma` to use in the Gaussian.
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param data.thresh The % data capture threshold. No values are calculated if
#'   data capture over the period of interest is less than this value.
#' @param new.name The name given to the new column. If not supplied it will
#'   create a name based on the name of the pollutant and the averaging period
#'   used.
#' @param ... Additional parameters passed to [cutData()]. For use with `type`.
#' @export
#' @return A tibble with two new columns for the Gaussian smooth value.
#' @author David Carslaw
#' @examples
#' # Gaussian smoother with sigma = 24
#' mydata <- GaussianSmooth(mydata,
#'   pollutant = "o3", sigma = 24, data.thresh = 75)
GaussianSmooth <- function(
  mydata,
  pollutant = "o3",
  sigma = 24L,
  type = "default",
  data.thresh = 0,
  new.name = NULL,
  ...
) {
  # check inputs

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
    mydata <- date.pad(mydata)

    # call C code

    data.thresh = data.thresh / 100
    results <- rolling_gaussian_cpp(
      mydata[[pollutant]],
      sigma,
      data.thresh
    )

    mydata[[new.name]] <- results[[1]]
    mydata[[paste0("n_", pollutant)]] <- results[[2]]

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
