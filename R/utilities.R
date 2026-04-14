# these are pre-defined type that need a field "date"; used by cutData
dateTypes <- c(
  "year",
  "hour",
  "weekday",
  "weekend",
  "week",
  "dst",
  "gmtbst",
  "bstgmt",
  "month",
  "monthyear",
  "yearmonth",
  "season",
  "seasonyear",
  "yearseason",
  "quarter",
  "quarteryear",
  "yearquarter",
  "daylight"
)

# get date components
get_first_year <- function(dat) {
  as.numeric(format(min(sort(dat, na.last = TRUE)), "%Y"))
}
get_last_year <- function(dat) {
  as.numeric(format(max(sort(dat, na.last = TRUE)), "%Y"))
}
get_first_month <- function(dat) {
  as.numeric(format(min(sort(dat, na.last = TRUE)), "%m"))
}
get_last_month <- function(dat) {
  as.numeric(format(max(sort(dat, na.last = TRUE)), "%m"))
}

#' Find the dominant time interval in a vector of dates/times
#'
#' Inspect a vector of Date or POSIXt objects and return a human-readable
#' description of the most common difference between consecutive unique,
#' sorted timestamps. Small floating-point noise is rounded and common
#' intervals (1 sec, 1 min, 1 hour, 1 day, 1 month, 1 year) are detected.
#' For uncommon intervals the function returns the interval in seconds
#' (e.g. "15 sec").
#'
#' @param dates A vector of Date or POSIXt timestamps.
#' @return A character string describing the detected interval.
#' @noRd
find_time_interval <- function(dates, return.seconds = FALSE) {
  # make sure data in POSIXct format
  if (inherits(dates, "Date")) {
    dates <- as.POSIXct(dates)
  }
  # 1. Safety check for insufficient data
  if (length(dates) < 2) {
    if (return.seconds) {
      return(1)
    } # Return numeric 1 if requested
    return("1 sec")
  }

  # 2. Sort and unique to prepare for diff
  d_sorted <- sort(unique(dates))

  # 3. Calculate differences
  # Round to 3 decimal places to avoid floating point noise
  diffs <- diff(as.numeric(d_sorted))
  diffs_rounded <- round(diffs, 3)

  # 4. Find the mode (most common interval)
  mode_seconds <- as.numeric(names(which.max(table(diffs_rounded))))

  # --- NEW: Early return if numeric seconds requested ---
  if (return.seconds) {
    return(mode_seconds)
  }

  # 5. Convert to string format compatible with seq() (if return.seconds = FALSE)
  if (abs(mode_seconds - 86400) < 10) {
    return("1 day")
  }
  if (abs(mode_seconds - 3600) < 5) {
    return("1 hour")
  }
  if (abs(mode_seconds - 60) < 1) {
    return("1 min")
  }

  # Logic for Month/Year
  days <- mode_seconds / 86400
  if (days >= 28 && days <= 31) {
    return("1 month")
  }
  if (days >= 365 && days <= 366) {
    return("1 year")
  }

  # Default fallback if no special interval is matched
  return(paste(mode_seconds, "sec"))
}

# simple rounding function from plyr
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

# function to check variables are numeric, if not force with warning
check_numeric <- function(mydata, vars) {
  for (i in seq_along(vars)) {
    if (!is.numeric(mydata[[vars[i]]])) {
      mydata[[vars[i]]] <- as.numeric(as.character(mydata[[vars[i]]]))

      warning(
        paste(vars[i], "is not numeric, forcing to numeric..."),
        call. = FALSE
      )
    }
  }

  return(mydata)
}

#' Function to check if duplicate dates are present in mydata by type
#' @param mydata Data input
#' @param type `type` from parent function
#' @param fn One of `cli::cli_warn` or `cli::cli_abort`
#' @noRd
check_duplicate_rows <- function(mydata, type = NULL, fn = cli::cli_warn) {
  if (is.null(type)) {
    flag <- length(mydata$date) != length(unique(mydata$date))
  } else {
    flag <-
      split(mydata, mydata[type], drop = TRUE) |>
      purrr::map_vec(function(x) {
        dates <- x$date
        unique_dates <- unique(x$date)
        length(dates) != length(unique_dates)
      }) |>
      any()
  }

  if (flag) {
    fn(
      c(
        "!" = "Duplicate dates detected in mydata{.field $date}.",
        "i" = 'Are there multiple sites in {.code mydata}? Use the {.field type} argument to condition them separately.'
      ),
      call = NULL
    )
  }
}

#' Flexibly map a function over a dataframe using `type` to split. The `type`
#' columns are always re-appended if the output is a dataframe.
#'
#' @param mydata A `data.frame` to split
#'
#' @param type Column or columns to split by; note that this function does not
#'   run [cutData()] itself.
#'
#' @param fun The function to apply; should be a function of a dataframe.
#'
#' @param .include_default If `default` is the only `type`, should any of the
#'   splitting actually happen? If `FALSE`, no `default` column will be
#'   returned.
#'
#' @param .progress Show a progress bar?
#'
#' @param fun A function
#'
#' @noRd
#' @examples
#' map_type(openairmaps::polar_data, fun = head, type = c("site", "site_type"))
map_type <- function(
  mydata,
  type,
  fun,
  .include_default = FALSE,
  .row_bind = TRUE,
  .progress = FALSE
) {
  if ((all(type == "default") || is.null(type)) && !.include_default) {
    return(fun(mydata))
  }

  out <-
    purrr::map(
      .x = split(mydata, mydata[type], drop = TRUE),
      .f = function(df) {
        out <- fun(df)
        out[type] <- df[1, type, drop = TRUE]
        return(out)
      },
      .progress = .progress
    )

  if (.row_bind) {
    out <-
      out |>
      dplyr::bind_rows() |>
      dplyr::relocate(dplyr::any_of(type))
  }

  return(out)
}

#' Create nice labels out of breaks, if only breaks are provided
#' @noRd
get_labels_from_breaks <- function(breaks, labels = NULL, sep = " - ") {
  if (is.null(labels) || anyNA(labels)) {
    labels <- paste(
      format(
        utils::head(breaks, -1),
        scientific = FALSE,
        trim = TRUE,
        drop0trailing = TRUE
      ),
      format(
        utils::tail(breaks, -1),
        scientific = FALSE,
        trim = TRUE,
        drop0trailing = TRUE
      ),
      sep = sep
    )
  }
  labels
}

#' pad out a set of numbers with zeroes to create consistent width
#' @noRd
pad_string <- function(y, n = NULL) {
  y <- as.character(y)
  n <- n %||% max(nchar(y))
  while (any(nchar(y) < n)) {
    id <- nchar(y) < n
    y[id] <- paste("0", y[id], sep = "")
  }
  y
}

#' Fill missing values in a monthly ts object using lm per calendar month
#'
#' For each calendar month, fits a linear model of value ~ time index using
#' available observations, then predicts at missing positions. Falls back to
#' the single observed value (1 obs) or the overall mean (0 obs).
#'
#' @param myts A \code{ts} object with frequency 12.
#' @param pollutant Character string naming the pollutant, used in the message.
#' @noRd
fill_ts_gaps <- function(myts, pollutant) {
  n_missing <- sum(is.na(myts))
  val_str <- sprintf(
    "%d missing monthly value%s found in '%s'.",
    n_missing,
    if (n_missing == 1) "" else "s",
    pollutant
  )
  gap_str <- sprintf(
    "%s filled using a linear model fitted per calendar month before deseasonalising.",
    if (n_missing == 1) "Gap" else "Gaps"
  )
  cli::cli_inform(c("!" = val_str, "i" = gap_str))
  overall_mean <- mean(myts, na.rm = TRUE)
  filled <- dplyr::tibble(
    value = as.numeric(myts),
    month = stats::cycle(myts),
    t = seq_along(myts)
  ) |>
    tidyr::nest(.by = "month") |>
    dplyr::mutate(
      pred = purrr::map(.data$data, function(d) {
        obs <- dplyr::filter(d, !is.na(.data$value))
        fitted <- if (nrow(obs) >= 2) {
          as.numeric(stats::predict(
            stats::lm(value ~ t, data = obs),
            newdata = d
          ))
        } else {
          rep(if (nrow(obs) == 1) obs$value else overall_mean, nrow(d))
        }
        dplyr::mutate(d, pred = fitted)
      })
    ) |>
    dplyr::select("pred") |>
    tidyr::unnest("pred") |>
    dplyr::arrange(.data$t) |>
    dplyr::mutate(
      value = dplyr::if_else(is.na(.data$value), .data$pred, .data$value)
    ) |>
    dplyr::pull("value")
  myts[] <- filled
  myts
}

#' Simple block bootstrap, overlapping blocks, no wrap-around,
#' no matching of ends
#' @param n length of data
#' @param b bootstrap replicates
#' @noRd
samp_boot_block <- function(n, b, block_length = 20) {
  nblocks <- ceiling(n / block_length)
  x <- sample.int((n - block_length + 1), b * nblocks, replace = TRUE)
  dim(x) <- c(nblocks, b)
  apply(x, 2, function(y, L) (0:(L - 1)) + rep(y, each = L), L = block_length)[
    1:n,
  ]
}

# check use of deprecated key.header and key.footer
check_key_header <- function(key.title, extra.args) {
  if (
    "key.header" %in%
      names(extra.args) ||
      "key.footer" %in% names(extra.args)
  ) {
    cli::cli_warn(
      "{.arg key.header} and {.arg key.footer} are deprecated. Please use {.arg key.title} to set a single legend name."
    )
    key.title <- paste(extra.args$key.header, extra.args$key.footer, sep = "\n")
  }
  key.title
}

# check use of deprecated key arg
check_key_position <- function(key.position, key) {
  if (!is.null(key)) {
    cli::cli_warn(
      'The {.arg key} argument is deprecated. Please use {.arg key.position = "none"} to remove a legend.'
    )

    if (isFALSE(key)) {
      key.position <- "none"
    }
  }
  key.position <- rlang::arg_match(
    key.position,
    c("top", "bottom", "left", "right", "none")
  )

  key.position
}

# function to get ... from plotting functions and remap parameters to ggplot2
# parameter names, with warnings about any parameters that are being remapped or
# ignored
capture_dots <- function(...) {
  extra.args <- rlang::list2(...)

  lattice_map <- list(
    pch = list(to = "shape", transform = NULL),
    col = list(to = "colour", transform = NULL),
    bg = list(to = "fill", transform = NULL),
    cex = list(to = "size", transform = function(x) x * 5),
    lty = list(to = "linetype", transform = NULL),
    lwd = list(to = "linewidth", transform = NULL),
    font = list(to = "fontface", transform = NULL),
    fontfamily = list(to = "family", transform = NULL),
    main = list(to = "title", transform = NULL),
    sub = list(to = "caption", transform = NULL),
    layout = list(to = c("ncol", "nrow"), transform = function(x) {
      list(ncol = x[1], nrow = x[2])
    })
  )

  lattice_only <- c(
    "fin",
    "pin",
    "adj",
    "srt",
    "mgp",
    "mar",
    "oma",
    "xpd",
    "las",
    "tcl"
  )

  found_convertible <- names(extra.args)[
    names(extra.args) %in% names(lattice_map)
  ]
  if (length(found_convertible) > 0) {
    bullets <- vapply(
      found_convertible,
      function(p) {
        cli::format_inline(
          "{.arg {p}} {cli::symbol$arrow_right} {.arg {lattice_map[[p]]$to}}"
        )
      },
      character(1)
    )
    names(bullets) <- rep("*", length(bullets))
    cli::cli_warn(c(
      "The following {.pkg lattice}/{.pkg base} graphical parameters will be converted to {.pkg ggplot2} equivalents:",
      bullets
    ))
  }

  found_no_equivalent <- names(extra.args)[names(extra.args) %in% lattice_only]
  if (length(found_no_equivalent) > 0) {
    cli::cli_warn(c(
      "The following {.pkg lattice}/{.pkg base} graphical parameters have no {.pkg ggplot2} equivalent and will be ignored. Some of these parameters may have equivalents within {.fun ggplot2::theme}.",
      purrr::map_vec(
        found_no_equivalent,
        \(p) {
          cli::format_inline("{.arg {p}}")
        }
      ) |>
        stats::setNames(rep("*", length(found_no_equivalent)))
    ))
  }

  for (param in names(lattice_map)) {
    if (param %in% names(extra.args)) {
      mapping <- lattice_map[[param]]
      value <- extra.args[[param]]
      transformed <- if (!is.null(mapping$transform)) {
        mapping$transform(value)
      } else {
        stats::setNames(list(value), mapping$to)
      }
      if (is.list(transformed) && !is.null(names(transformed))) {
        for (nm in names(transformed)) {
          extra.args[[nm]] <- transformed[[nm]]
        }
      } else {
        extra.args[[mapping$to]] <- transformed
      }
      extra.args[[param]] <- NULL
    }
  }

  extra.args[found_no_equivalent] <- NULL

  return(extra.args)
}

# Handle cutting numeric vectors for discretising plots
cut_plot_breaks <- function(x, breaks, labels) {
  if (!is.null(breaks)) {
    categorical <- TRUE

    # ensure breaks covers all data
    if (max(breaks) < max(x, na.rm = TRUE)) {
      breaks[length(breaks)] <- ceiling(max(x, na.rm = TRUE))
    }
    if (min(breaks) > min(x, na.rm = TRUE)) {
      breaks[1] <- floor(min(x, na.rm = TRUE))
    }
    breaks <- sort(unique(breaks))

    # assign labels if no labels are given
    labels <- get_labels_from_breaks(breaks, labels)

    x <- cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
  }
  x
}

# Recycle helper
recycle_to_length <- function(x, n, expect1 = FALSE) {
  if (length(x) == n) {
    return(x)
  }
  if (length(x) == 1) {
    return(rep(x, n))
  }

  if (expect1) {
    cli::cli_abort(
      "Length mismatch: argument must be length 1 or same length as 'h'/'v'"
    )
  } else {
    while (length(x) < n) {
      x <- c(x, x)
    }
    x <- x[seq_len(x)]
  }
}
