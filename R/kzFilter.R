#' (Adaptive) Kolmogorov-Zurbenko (KZ) Filter
#'
#' [kzFilter()] applies a Kolmogorov-Zurbenko filter to one or more pollutant
#' columns in a data frame at multiple window sizes and returns the decomposed
#' components. The KZ filter is a low-pass filter formed by iterating a simple
#' moving average `k` times with window size `m`. [kzaFilter()] applies an
#' *adaptive* Kolmogorov-Zurbenko filter to one or more pollutant columns in a
#' data frame at multiple window sizes and returns the decomposed components.
#' The KZA filter uses a standard KZ filter to detect structural breaks and
#' shrinks the window near those breaks to preserve sharp features.
#'
#' @section Default window sizes:
#'
#'   With the default window sizes of 25, 169, 721 and 8761 (suited to hourly
#'   data), the function returns four intermediate filtered columns and five
#'   physical components derived by differencing:
#'
#'   - **short** — daily cycle and short-term variations within it
#'   (`pollutant - kz(a)_25`)
#'
#'   - **synoptic** — 2–7 day weather systems (`kz(a)_25 - kz(a)_169`)
#'
#'   - **intermediate** — weekly to monthly variability
#'   (`kz(a)_169 - kz(a)_721`)
#'
#'   - **seasonal** — monthly to annual variability (`kz(a)_721 - kz(a)_8761`)
#'
#'   - **trend** — multi-year trend (`kz(a)_8761`)
#'
#' @section Edge effects:
#'
#'   At the start and end of the series the filter window is silently truncated
#'   rather than padded, so no `NA`s are introduced. However, values within the
#'   affected boundary zone are averaged over fewer points than the interior and
#'   should be interpreted with caution.
#'
#'   The affected length at each end of the series for a single filter pass is
#'   `floor(m / 2)` observations. Because the filter is iterated `k` times (each
#'   pass consuming the output of the previous one), the total affected zone at
#'   each end is approximately `k * floor(m / 2)` observations. With the default
#'   `m = c(25, 169, 721, 8761)` and `k = 5`, the affected zones are roughly 60
#'   h (~2.5 days), 420 h (~17 days), 1,800 h (~75 days), and 21,900 h (~2.5
#'   years) at each end respectively. The `trend` component therefore requires
#'   at least 5–6 years of data for the interior estimates to be unaffected.
#'
#' @param mydata A data frame containing a `date` field in `Date` or `POSIXct`
#'   format. The input time series must be regular, e.g., hourly or daily.
#'
#' @param pollutant The name of a pollutant, e.g., `pollutant = "o3"`. More than
#'   one pollutant can be supplied as a vector, e.g., `pollutant = c("o3",
#'   "nox")`.
#'
#' @param m Integer vector of window sizes ([kzFilter()]) or maximum window
#'   sizes ([kzaFilter()]). Defaults to `c(25, 169, 721, 8761)` (suited to
#'   hourly data). All values must be >= 3. Values of `m` should be odd; even
#'   values will produce a symmetric window of `m + 1` points rather than `m`.
#'
#' @param k Integer. The number of iterations applied at each window size
#'   ([kzFilter()]) or the number of iterations for the baseline KZ filter used
#'   to detect structural breaks ([kzaFilter()]).
#'
#' @param data.thresh Numeric (0--1). Minimum fraction of valid (non-`NA`)
#'   values required within a window for a filtered value to be returned;
#'   otherwise `NA` is returned. Applies to the actual window size, which is
#'   smaller near the series edges. Default is `0.5` (50%).
#'
#' @param type Used for splitting the data further. Passed to [cutData()].
#'
#' @param components Logical. If `TRUE` (default) and more than one `m` value is
#'   supplied, component columns are added by differencing adjacent filtered
#'   series.
#'
#' @param comp.names Character vector of names for the component columns. Must
#'   have length `length(m) + 1`. Defaults to `c("short", "synoptic",
#'   "intermediate", "seasonal", "trend")` to match the default `m` values. If
#'   the length does not match, numbered names (`comp_1`, `comp_2`, \ldots) are
#'   used with a warning.
#'
#' @param to_narrow Logical. If `TRUE`, return the data in tidy (long) format
#'   with a `component` column and a `value` column instead of one column per
#'   component. Intermediate filter columns (`kz(a)_*`) are dropped. Default is
#'   `FALSE`. Ignored when `components = FALSE` or a single `m` value is
#'   supplied.
#'
#' @param ... Passed to [cutData()] for use with `type`.
#'
#' @return When `to_narrow = FALSE` (default), a tibble with the original
#'   columns plus intermediate filter columns (`kz(a)_{m}`) and component
#'   columns. When `to_narrow = TRUE`, a tidy tibble with a `component` column
#'   (factor ordered fast to slow) and a `value` column.
#'
#' @author David Carslaw
#' @order 1
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
  m = c(25L, 169L, 721L, 8761L),
  k = 5L,
  data.thresh = 0.5,
  type = "default",
  components = TRUE,
  comp.names = c("short", "synoptic", "intermediate", "seasonal", "trend"),
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

#' @param sensitivity Numeric. Controls how aggressively the window shrinks at
#'   structural breaks (higher = more aggressive). Used in [kzaFilter()].
#' @rdname kzFilter
#' @order 2
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
  m = c(25L, 169L, 721L, 8761L),
  k = 5L,
  sensitivity = 1.0,
  data.thresh = 0.5,
  type = "default",
  components = TRUE,
  comp.names = c("short", "synoptic", "intermediate", "seasonal", "trend"),
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
