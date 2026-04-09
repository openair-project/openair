#' Kolmogorov-Zurbenko (KZ) Filter
#'
#' Applies a Kolmogorov-Zurbenko filter to one or more pollutant columns in a
#' data frame at multiple window sizes and returns the decomposed components.
#' The KZ filter is a low-pass filter formed by iterating a simple moving
#' average \code{k} times with window size \code{m}.
#'
#' With the default window sizes of 5, 24, 168 and 720 (suited to hourly data),
#' the function returns four intermediate filtered columns and five physical
#' components derived by differencing:
#' \enumerate{
#'   \item \strong{comp_1} — short-term (sub-hourly residual, \code{pollutant - kz_5})
#'   \item \strong{comp_2} — diurnal/synoptic (\code{kz_5 - kz_24})
#'   \item \strong{comp_3} — weather/weekly (\code{kz_24 - kz_168})
#'   \item \strong{comp_4} — seasonal (\code{kz_168 - kz_720})
#'   \item \strong{comp_5} — long-term trend (\code{kz_720})
#' }
#'
#' @param mydata A data frame containing a \code{date} field in \code{Date} or
#'   \code{POSIXct} format. The input time series must be regular, e.g., hourly
#'   or daily.
#' @param pollutant The name of a pollutant, e.g., \code{pollutant = "o3"}.
#'   More than one pollutant can be supplied as a vector, e.g.,
#'   \code{pollutant = c("o3", "nox")}.
#' @param m Integer vector of window sizes. Defaults to \code{c(5, 24, 168,
#'   720)} (suited to hourly data). All values must be >= 3.
#' @param k Integer. The number of iterations applied at each window size.
#' @param min_valid Integer. Minimum number of valid (non-\code{NA}) points
#'   required in a window to return a value.
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param components Logical. If \code{TRUE} (default) and more than one
#'   \code{m} value is supplied, component columns are added by differencing
#'   adjacent filtered series.
#' @param comp.names Character vector of names for the component columns. Must
#'   have length \code{length(m) + 1}. Defaults to \code{c("short",
#'   "synoptic", "weather", "seasonal", "trend")} to match the default
#'   \code{m} values. If the length does not match, numbered names
#'   (\code{comp_1}, \code{comp_2}, \ldots) are used with a warning.
#' @param ... Passed to [cutData()] for use with \code{type}.
#'
#' @return A tibble with the original columns plus:
#'   \itemize{
#'     \item Intermediate filter columns named \code{kz_{m}} (single pollutant)
#'       or \code{kz_{m}_{pollutant}} (multiple pollutants).
#'     \item Component columns named according to \code{comp.names} (single
#'       pollutant) or \code{{comp.name}_{pollutant}} (multiple pollutants).
#'   }
#' @author David Carslaw
#' @export
#' @examples
#' # Default: 4 window sizes, 5 descriptively named components returned
#' mydata <- kzFilter(mydata, pollutant = "pm25")
#'
#' # Single window size (no component decomposition)
#' mydata <- kzFilter(mydata, pollutant = "o3", m = 24, k = 5)
kzFilter <- function(
  mydata,
  pollutant = "o3",
  m = c(5L, 24L, 168L, 720L),
  k = 3L,
  min_valid = 1L,
  type = "default",
  components = TRUE,
  comp.names = c("short", "synoptic", "weather", "seasonal", "trend"),
  ...
) {
  if (any(m < 3L)) {
    cli::cli_abort("All values of {.arg m} must be at least {.val {3L}}.")
  }

  for (p in pollutant) {
    if (!p %in% names(mydata)) {
      cli::cli_abort("Column {.field {p}} not found in {.arg mydata}.")
    }
    if (!is.numeric(mydata[[p]])) {
      cli::cli_abort(
        "{.field {p}} is not numeric \u2014 it is {.cls {class(mydata[[p]])}}."
      )
    }
  }

  multi_poll <- length(pollutant) > 1L

  # resolve component names
  n_comp <- length(m) + 1L
  if (length(comp.names) != n_comp) {
    cli::cli_warn(
      "{.arg comp.names} has length {length(comp.names)} but {n_comp} components will be produced. Using numbered names."
    )
    comp.names <- paste0("comp_", seq_len(n_comp))
  }

  mydata <- cutData(mydata, type = type, ...)
  check_duplicate_rows(mydata, type, fn = cli::cli_abort)

  calc_kz_df <- function(mydata) {
    for (p in pollutant) {
      filt_cols <- character(length(m))

      for (i in seq_along(m)) {
        col_name <- if (multi_poll) paste0("kz_", m[i], "_", p) else paste0("kz_", m[i])
        mydata[[col_name]] <- kz_cpp(
          mydata[[p]],
          as.integer(m[i]),
          as.integer(k),
          as.integer(min_valid)
        )
        filt_cols[i] <- col_name
      }

      if (components && length(m) > 1L) {
        n <- length(m)
        for (j in seq_len(n + 1L)) {
          comp_name <- if (multi_poll) paste0(comp.names[j], "_", p) else comp.names[j]
          if (j == 1L) {
            mydata[[comp_name]] <- mydata[[p]] - mydata[[filt_cols[1L]]]
          } else if (j <= n) {
            mydata[[comp_name]] <- mydata[[filt_cols[j - 1L]]] - mydata[[filt_cols[j]]]
          } else {
            mydata[[comp_name]] <- mydata[[filt_cols[n]]]
          }
        }
      }
    }
    mydata
  }

  mydata <- map_type(mydata, type = type, fun = calc_kz_df, .include_default = TRUE)

  if (any(type == "default")) {
    mydata$default <- NULL
  }

  return(mydata)
}

#' Adaptive Kolmogorov-Zurbenko (KZA) Filter
#'
#' Applies an adaptive Kolmogorov-Zurbenko filter to one or more pollutant
#' columns in a data frame at multiple window sizes and returns the decomposed
#' components. The KZA filter uses a standard KZ filter to detect structural
#' breaks and shrinks the window near those breaks to preserve sharp features.
#'
#' With the default window sizes of 5, 24, 168 and 720 (suited to hourly data),
#' the function returns four intermediate filtered columns and five physical
#' components derived by differencing:
#' \enumerate{
#'   \item \strong{comp_1} — short-term (sub-hourly residual, \code{pollutant - kza_5})
#'   \item \strong{comp_2} — diurnal/synoptic (\code{kza_5 - kza_24})
#'   \item \strong{comp_3} — weather/weekly (\code{kza_24 - kza_168})
#'   \item \strong{comp_4} — seasonal (\code{kza_168 - kza_720})
#'   \item \strong{comp_5} — long-term trend (\code{kza_720})
#' }
#'
#' @param mydata A data frame containing a \code{date} field in \code{Date} or
#'   \code{POSIXct} format. The input time series must be regular, e.g., hourly
#'   or daily.
#' @param pollutant The name of a pollutant, e.g., \code{pollutant = "o3"}.
#'   More than one pollutant can be supplied as a vector, e.g.,
#'   \code{pollutant = c("o3", "nox")}.
#' @param m Integer vector of maximum window sizes. Defaults to \code{c(5, 24,
#'   168, 720)} (suited to hourly data).
#' @param k Integer. The number of iterations for the baseline KZ filter used
#'   to detect structural breaks.
#' @param sensitivity Numeric. Controls how aggressively the window shrinks at
#'   structural breaks (higher = more aggressive).
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param components Logical. If \code{TRUE} (default) and more than one
#'   \code{m} value is supplied, component columns are added by differencing
#'   adjacent filtered series.
#' @param comp.names Character vector of names for the component columns. Must
#'   have length \code{length(m) + 1}. Defaults to \code{c("short",
#'   "synoptic", "weather", "seasonal", "trend")} to match the default
#'   \code{m} values. If the length does not match, numbered names
#'   (\code{comp_1}, \code{comp_2}, \ldots) are used with a warning.
#' @param ... Passed to [cutData()] for use with \code{type}.
#'
#' @return A tibble with the original columns plus:
#'   \itemize{
#'     \item Intermediate filter columns named \code{kza_{m}} (single pollutant)
#'       or \code{kza_{m}_{pollutant}} (multiple pollutants).
#'     \item Component columns named according to \code{comp.names} (single
#'       pollutant) or \code{{comp.name}_{pollutant}} (multiple pollutants).
#'   }
#' @author David Carslaw
#' @export
#' @examples
#' # Default: 4 window sizes, 5 descriptively named components returned
#' mydata <- kzaFilter(mydata, pollutant = "pm25")
#'
#' # Single window size (no component decomposition)
#' mydata <- kzaFilter(mydata, pollutant = "o3", m = 24, k = 5)
kzaFilter <- function(
  mydata,
  pollutant = "o3",
  m = c(5L, 24L, 168L, 720L),
  k = 3L,
  sensitivity = 1.0,
  type = "default",
  components = TRUE,
  comp.names = c("short", "synoptic", "weather", "seasonal", "trend"),
  ...
) {
  for (p in pollutant) {
    if (!p %in% names(mydata)) {
      cli::cli_abort("Column {.field {p}} not found in {.arg mydata}.")
    }
    if (!is.numeric(mydata[[p]])) {
      cli::cli_abort(
        "{.field {p}} is not numeric \u2014 it is {.cls {class(mydata[[p]])}}."
      )
    }
  }

  multi_poll <- length(pollutant) > 1L

  # resolve component names
  n_comp <- length(m) + 1L
  if (length(comp.names) != n_comp) {
    cli::cli_warn(
      "{.arg comp.names} has length {length(comp.names)} but {n_comp} components will be produced. Using numbered names."
    )
    comp.names <- paste0("comp_", seq_len(n_comp))
  }

  mydata <- cutData(mydata, type = type, ...)
  check_duplicate_rows(mydata, type, fn = cli::cli_abort)

  calc_kza_df <- function(mydata) {
    for (p in pollutant) {
      filt_cols <- character(length(m))

      for (i in seq_along(m)) {
        col_name <- if (multi_poll) paste0("kza_", m[i], "_", p) else paste0("kza_", m[i])
        mydata[[col_name]] <- kza_cpp(
          mydata[[p]],
          as.integer(m[i]),
          as.integer(k),
          as.numeric(sensitivity)
        )
        filt_cols[i] <- col_name
      }

      if (components && length(m) > 1L) {
        n <- length(m)
        for (j in seq_len(n + 1L)) {
          comp_name <- if (multi_poll) paste0(comp.names[j], "_", p) else comp.names[j]
          if (j == 1L) {
            mydata[[comp_name]] <- mydata[[p]] - mydata[[filt_cols[1L]]]
          } else if (j <= n) {
            mydata[[comp_name]] <- mydata[[filt_cols[j - 1L]]] - mydata[[filt_cols[j]]]
          } else {
            mydata[[comp_name]] <- mydata[[filt_cols[n]]]
          }
        }
      }
    }
    mydata
  }

  mydata <- map_type(mydata, type = type, fun = calc_kza_df, .include_default = TRUE)

  if (any(type == "default")) {
    mydata$default <- NULL
  }

  return(mydata)
}
