#' Kolmogorov-Zurbenko (KZ) Filter
#'
#' Applies a Kolmogorov-Zurbenko filter to one or more pollutant columns in a
#' data frame at multiple window sizes and returns the decomposed components.
#' The KZ filter is a low-pass filter formed by iterating a simple moving
#' average \code{k} times with window size \code{m}.
#'
#' With the default window sizes of 5, 25, 169 and 8761 (suited to hourly data),
#' the function returns four intermediate filtered columns and five physical
#' components derived by differencing:
#' \enumerate{
#'   \item \strong{short} — sub-hourly fluctuations (\code{pollutant - kz_5})
#'   \item \strong{diurnal} — daily cycle (\code{kz_5 - kz_25})
#'   \item \strong{synoptic} — 2–7 day weather systems (\code{kz_25 - kz_169})
#'   \item \strong{seasonal} — weekly to annual variability (\code{kz_169 - kz_8761})
#'   \item \strong{trend} — multi-year trend (\code{kz_8761})
#' }
#'
#' @section Edge effects:
#' At the start and end of the series the filter window is silently truncated
#' rather than padded, so no \code{NA}s are introduced. However, values within
#' the affected boundary zone are averaged over fewer points than the interior
#' and should be interpreted with caution.
#'
#' The affected length at each end of the series for a single filter pass is
#' \code{floor(m / 2)} observations. Because the filter is iterated \code{k}
#' times (each pass consuming the output of the previous one), the total
#' affected zone at each end is approximately \code{k * floor(m / 2)}
#' observations. With the default \code{m = c(5, 25, 169, 8761)} and
#' \code{k = 5}, the affected zones are roughly 12 h, 60 h, 420 h (~17 days),
#' and 21,900 h (~2.5 years) at each end respectively. The \code{trend}
#' component therefore requires at least 5–6 years of data for the interior
#' estimates to be unaffected.
#'
#' @param mydata A data frame containing a \code{date} field in \code{Date} or
#'   \code{POSIXct} format. The input time series must be regular, e.g., hourly
#'   or daily.
#' @param pollutant The name of a pollutant, e.g., \code{pollutant = "o3"}.
#'   More than one pollutant can be supplied as a vector, e.g.,
#'   \code{pollutant = c("o3", "nox")}.
#' @param m Integer vector of window sizes. Defaults to \code{c(5, 25, 169,
#'   8761)} (suited to hourly data). All values must be >= 3. Values of
#'   \code{m} should be odd; even values will produce a symmetric window of
#'   \code{m + 1} points rather than \code{m}.
#' @param k Integer. The number of iterations applied at each window size.
#' @param data.thresh Numeric (0--1). Minimum fraction of valid (non-\code{NA})
#'   values required within a window for a filtered value to be returned;
#'   otherwise \code{NA} is returned. Applies to the actual window size, which
#'   is smaller near the series edges. Default is \code{0.5} (50%).
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param components Logical. If \code{TRUE} (default) and more than one
#'   \code{m} value is supplied, component columns are added by differencing
#'   adjacent filtered series.
#' @param comp.names Character vector of names for the component columns. Must
#'   have length \code{length(m) + 1}. Defaults to \code{c("short", "diurnal",
#'   "synoptic", "seasonal", "trend")} to match the default \code{m} values.
#'   If the length does not match, numbered names (\code{comp_1},
#'   \code{comp_2}, \ldots) are used with a warning.
#' @param to_narrow Logical. If \code{TRUE}, return the data in tidy (long)
#'   format with a \code{component} column and a \code{value} column instead
#'   of one column per component. Intermediate filter columns (\code{kz_*})
#'   are dropped. Default is \code{FALSE}. Ignored when \code{components =
#'   FALSE} or a single \code{m} value is supplied.
#' @param ... Passed to [cutData()] for use with \code{type}.
#'
#' @return When \code{to_narrow = FALSE} (default), a tibble with the original
#'   columns plus intermediate filter columns (\code{kz_{m}}) and component
#'   columns. When \code{to_narrow = TRUE}, a tidy tibble with a
#'   \code{component} column (factor ordered fast to slow) and a \code{value}
#'   column.
#' @author David Carslaw
#' @export
#' @examples
#'
#' \dontrun{
#' # Default: 4 window sizes, 5 descriptively named components returned
#' mydata <- kzFilter(mydata, pollutant = "nox")
#'
#' # Tidy long format
#' mydata <- kzFilter(mydata, pollutant = "nox", to_narrow = TRUE)
#'
#' # Single window size (no component decomposition)
#' mydata <- kzFilter(mydata, pollutant = "nox", m = 24, k = 5)
#' }
kzFilter <- function(
  mydata,
  pollutant = "o3",
  m = c(5L, 25L, 169L, 8761L),
  k = 5L,
  data.thresh = 0.5,
  type = "default",
  components = TRUE,
  comp.names = c("short", "diurnal", "synoptic", "seasonal", "trend"),
  to_narrow = FALSE,
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
        col_name <- if (multi_poll) {
          paste0("kz_", m[i], "_", p)
        } else {
          paste0("kz_", m[i])
        }
        mydata[[col_name]] <- kz_cpp(
          mydata[[p]],
          as.integer(m[i]),
          as.integer(k),
          as.numeric(data.thresh)
        )
        filt_cols[i] <- col_name
      }

      if (components && length(m) > 1L) {
        n <- length(m)
        for (j in seq_len(n + 1L)) {
          comp_name <- if (multi_poll) {
            paste0(comp.names[j], "_", p)
          } else {
            comp.names[j]
          }
          if (j == 1L) {
            mydata[[comp_name]] <- mydata[[p]] - mydata[[filt_cols[1L]]]
          } else if (j <= n) {
            mydata[[comp_name]] <- mydata[[filt_cols[j - 1L]]] -
              mydata[[filt_cols[j]]]
          } else {
            mydata[[comp_name]] <- mydata[[filt_cols[n]]]
          }
        }
      }
    }
    mydata
  }

  mydata <- map_type(
    mydata,
    type = type,
    fun = calc_kz_df,
    .include_default = TRUE
  )

  if (any(type == "default")) {
    mydata$default <- NULL
  }

  if (to_narrow && components && length(m) > 1L) {
    actual_comp_cols <- if (multi_poll) {
      as.vector(outer(comp.names, pollutant, paste, sep = "_"))
    } else {
      comp.names
    }
    mydata <- mydata |>
      dplyr::select(-dplyr::matches("^kz_\\d+")) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(actual_comp_cols),
        names_to = "component",
        values_to = "value"
      ) |>
      dplyr::mutate(
        component = factor(.data$component, levels = actual_comp_cols)
      )
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
#' With the default window sizes of 5, 25, 169 and 8761 (suited to hourly data),
#' the function returns four intermediate filtered columns and five physical
#' components derived by differencing:
#' \enumerate{
#'   \item \strong{short} — sub-hourly fluctuations (\code{pollutant - kza_5})
#'   \item \strong{diurnal} — daily cycle (\code{kza_5 - kza_25})
#'   \item \strong{synoptic} — 2–7 day weather systems (\code{kza_25 - kza_169})
#'   \item \strong{seasonal} — weekly to annual variability (\code{kza_169 - kza_8761})
#'   \item \strong{trend} — multi-year trend (\code{kza_8761})
#' }
#'
#' @section Edge effects:
#' At the start and end of the series the filter window is silently truncated
#' rather than padded, so no \code{NA}s are introduced. However, values within
#' the affected boundary zone are averaged over fewer points than the interior
#' and should be interpreted with caution.
#'
#' The affected length at each end of the series for a single filter pass is
#' \code{floor(m / 2)} observations. Because the filter is iterated \code{k}
#' times (each pass consuming the output of the previous one), the total
#' affected zone at each end is approximately \code{k * floor(m / 2)}
#' observations. With the default \code{m = c(5, 25, 169, 8761)} and
#' \code{k = 5}, the affected zones are roughly 12 h, 60 h, 420 h (~17 days),
#' and 21,900 h (~2.5 years) at each end respectively. The \code{trend}
#' component therefore requires at least 5–6 years of data for the interior
#' estimates to be unaffected.
#'
#' @param mydata A data frame containing a \code{date} field in \code{Date} or
#'   \code{POSIXct} format. The input time series must be regular, e.g., hourly
#'   or daily.
#' @param pollutant The name of a pollutant, e.g., \code{pollutant = "o3"}.
#'   More than one pollutant can be supplied as a vector, e.g.,
#'   \code{pollutant = c("o3", "nox")}.
#' @param m Integer vector of maximum window sizes. Defaults to \code{c(5, 25,
#'   169, 8761)} (suited to hourly data). Values of \code{m} should be odd;
#'   even values will produce a symmetric window of \code{m + 1} points rather
#'   than \code{m}.
#' @param k Integer. The number of iterations for the baseline KZ filter used
#'   to detect structural breaks.
#' @param sensitivity Numeric. Controls how aggressively the window shrinks at
#'   structural breaks (higher = more aggressive).
#' @param data.thresh Numeric (0--1). Minimum fraction of valid (non-\code{NA})
#'   values required within a window for a filtered value to be returned;
#'   otherwise \code{NA} is returned. Applies to the actual window size, which
#'   is smaller near the series edges. Default is \code{0.5} (50%).
#' @param type Used for splitting the data further. Passed to [cutData()].
#' @param components Logical. If \code{TRUE} (default) and more than one
#'   \code{m} value is supplied, component columns are added by differencing
#'   adjacent filtered series.
#' @param comp.names Character vector of names for the component columns. Must
#'   have length \code{length(m) + 1}. Defaults to \code{c("short", "diurnal",
#'   "synoptic", "seasonal", "trend")} to match the default \code{m} values.
#'   If the length does not match, numbered names (\code{comp_1},
#'   \code{comp_2}, \ldots) are used with a warning.
#' @param to_narrow Logical. If \code{TRUE}, return the data in tidy (long)
#'   format with a \code{component} column and a \code{value} column instead
#'   of one column per component. Intermediate filter columns (\code{kza_*})
#'   are dropped. Default is \code{FALSE}. Ignored when \code{components =
#'   FALSE} or a single \code{m} value is supplied.
#' @param ... Passed to [cutData()] for use with \code{type}.
#'
#' @return When \code{to_narrow = FALSE} (default), a tibble with the original
#'   columns plus intermediate filter columns (\code{kza_{m}}) and component
#'   columns. When \code{to_narrow = TRUE}, a tidy tibble with a
#'   \code{component} column (factor ordered fast to slow) and a \code{value}
#'   column.
#' @author David Carslaw
#' @export
#' @examples
#'
#' \dontrun{
#' # Default: 4 window sizes, 5 descriptively named components returned
#' mydata <- kzaFilter(mydata, pollutant = "nox")
#'
#' # Tidy long format
#' mydata <- kzaFilter(mydata, pollutant = "nox", to_narrow = TRUE)
#'
#' # Single window size (no component decomposition)
#' mydata <- kzaFilter(mydata, pollutant = "nox", m = 24, k = 5)
#' }
kzaFilter <- function(
  mydata,
  pollutant = "o3",
  m = c(5L, 25L, 169L, 8761L),
  k = 5L,
  sensitivity = 1.0,
  data.thresh = 0.5,
  type = "default",
  components = TRUE,
  comp.names = c("short", "diurnal", "synoptic", "seasonal", "trend"),
  to_narrow = FALSE,
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
        col_name <- if (multi_poll) {
          paste0("kza_", m[i], "_", p)
        } else {
          paste0("kza_", m[i])
        }
        mydata[[col_name]] <- kza_cpp(
          mydata[[p]],
          as.integer(m[i]),
          as.integer(k),
          as.numeric(sensitivity),
          as.numeric(data.thresh)
        )
        filt_cols[i] <- col_name
      }

      if (components && length(m) > 1L) {
        n <- length(m)
        for (j in seq_len(n + 1L)) {
          comp_name <- if (multi_poll) {
            paste0(comp.names[j], "_", p)
          } else {
            comp.names[j]
          }
          if (j == 1L) {
            mydata[[comp_name]] <- mydata[[p]] - mydata[[filt_cols[1L]]]
          } else if (j <= n) {
            mydata[[comp_name]] <- mydata[[filt_cols[j - 1L]]] -
              mydata[[filt_cols[j]]]
          } else {
            mydata[[comp_name]] <- mydata[[filt_cols[n]]]
          }
        }
      }
    }
    mydata
  }

  mydata <- map_type(
    mydata,
    type = type,
    fun = calc_kza_df,
    .include_default = TRUE
  )

  if (any(type == "default")) {
    mydata$default <- NULL
  }

  if (to_narrow && components && length(m) > 1L) {
    actual_comp_cols <- if (multi_poll) {
      as.vector(outer(comp.names, pollutant, paste, sep = "_"))
    } else {
      comp.names
    }
    mydata <- mydata |>
      dplyr::select(-dplyr::matches("^kza_\\d+")) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(actual_comp_cols),
        names_to = "component",
        values_to = "value"
      ) |>
      dplyr::mutate(
        component = factor(.data$component, levels = actual_comp_cols)
      )
  }

  return(mydata)
}
