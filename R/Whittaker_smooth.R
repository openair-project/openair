#' Calculate Whittaker-Eilers Smoothing, Interpolation and Baseline
#' Determination
#'
#' This function applies the Whittaker-Eilers smoothing and interpolation method
#' to a specified pollutant in a data frame. The method is based on penalised
#' least squares and is designed to handle time series data with missing values,
#' providing a smoothed estimate of the pollutant concentrations over time. The
#' function allows for flexible control over the amount of smoothing through the
#' `lambda` parameter and can be applied to multiple pollutants simultaneously.
#'
#' In addition to smoothing, the function can also perform baseline estimation
#' using Asymmetric Least Squares (ALS) when the `p` parameter is provided. This
#' allows for the separation of the underlying baseline from the observed data,
#' which can be particularly useful for identifying trends or correcting for
#' background levels in pollutant concentrations.
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
#' @param p The asymmetry weight parameter used exclusively for baseline
#'   estimation (Asymmetric Least Squares). It defines how the algorithm treats
#'   points that fall above the fitted line versus points that fall below it. It
#'   takes a value between 0 and 1. When p is very small, the algorithm assigns
#'   a massive penalty to the curve if it rises above the data points, but
#'   almost no penalty if it drops below them. This forces the curve to "hug"
#'   the bottom of the signal, effectively ignoring the positive peaks. Typical
#'   Values: 0.01 to 0.05.
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
  p = NULL,
  ...
) {
  # check inputs: pollutants numeric
  for (pname in pollutant) {
    if (!is.numeric(mydata[[pname]])) {
      cli::cli_abort(
        "mydata{.field ${pname}} is not numeric - it is {class(mydata[[pname]])}."
      )
    }
  }

  # if p provided validate range and length
  if (!is.null(p)) {
    if (!all(is.numeric(p))) {
      cli::cli_abort("{.arg p} must be numeric in [0, 1].")
    }
    if (any(p < 0 | p > 1)) {
      cli::cli_abort("{.arg p} values must be between 0 and 1.")
    }
    if (!(length(p) %in% c(1, length(pollutant)))) {
      cli::cli_abort(
        "{.arg p} must be length 1 or the same length as {.arg pollutant}."
      )
    }
    if (!is.null(new.name)) {
      cli::cli_warn(
        "{.arg new.name} is ignored when {.arg p} is provided; baseline/corrected names will be generated from pollutant names."
      )
    }
  }

  # create new names only for the smoothing (p == NULL) branch
  if (is.null(p)) {
    if (is.null(new.name) || length(new.name) != length(pollutant)) {
      if (!is.null(new.name) && length(pollutant) > 1) {
        cli::cli_warn(
          "Length of {.arg new.name} does not match length of {.arg pollutant}. Using generated names."
        )
      }
      new.name <- paste0("smooth_", pollutant)
    }
  }

  # cut data
  mydata <- cutData(mydata, type = type, ...)

  # error if duplicate dates
  checkDuplicateRows(mydata, type, fn = cli::cli_abort)

  # replace NaN with NA to avoid issues in C++ code
  mydata <- mydata |>
    mutate(across(where(is.numeric), \(x) ifelse(is.nan(x), NA, x)))

  # function to perform rolling / baseline
  calc.rolling <- function(mydata) {
    # original dates to allow restoring subset if padding added
    dates <- mydata$date

    # pad missing hours if requested
    if (date.pad) {
      mydata <- datePad(mydata)
    }

    # Loop over each pollutant
    for (i in seq_along(pollutant)) {
      current_poll <- pollutant[i]

      if (is.null(p)) {
        # old behaviour: single smoothed series via C++ whittaker_smooth
        results <- whittaker_smooth(
          mydata[[current_poll]],
          lambda = lambda,
          d = d
        )
        current_name <- new.name[i]
        mydata[[current_name]] <- results[[1]]
      } else {
        # new behaviour: baseline + corrected via whittaker_baseline
        p_val <- if (length(p) == 1) p else p[i]
        out <- whittaker_baseline(
          mydata[[current_poll]],
          lambda = lambda,
          d = d,
          p = p_val
        )
        base_name <- paste0(current_poll, "_baseline")
        corr_name <- paste0(current_poll, "_increment")
        mydata[[base_name]] <- out[[1]] # baseline
        mydata[[corr_name]] <- out[[2]] # increment
      }
    }

    # return what was put in; avoids adding missing data e.g. for factors
    if (length(dates) != nrow(mydata)) {
      mydata <- mydata[mydata$date %in% dates, ]
    }

    # return
    mydata
  }

  # split if several sites / types
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

  mydata
}
