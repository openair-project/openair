# these are pre-defined type that need a field "date"; used by cutData
dateTypes <- c(
  "year",
  "hour",
  "month",
  "season",
  "weekday",
  "weekend",
  "monthyear",
  "gmtbst",
  "bstgmt",
  "dst",
  "daylight",
  "week",
  "seasonyear",
  "yearseason"
)

# get date components
startYear <- function(dat) {
  as.numeric(format(min(dat[order(dat)]), "%Y"))
}
endYear <- function(dat) {
  as.numeric(format(max(dat[order(dat)]), "%Y"))
}
startMonth <- function(dat) {
  as.numeric(format(min(dat[order(dat)]), "%m"))
}
endMonth <- function(dat) {
  as.numeric(format(max(dat[order(dat)]), "%m"))
}

# function to test of a suggested package is available and warn if not
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }

  stop(
    "Package `",
    package,
    "` required for `",
    fun,
    "`.\n",
    "Please install and try again.",
    call. = FALSE
  )
}

# -------------------------------------------------------------------------
# Helper: Robust Time Interval Finder
# -------------------------------------------------------------------------
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
#' @keywords internal
find.time.interval <- function(dates, return.seconds = FALSE) {
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
# -------------------------------------------------------------------------
# Main Function: Date Pad with "Block" Filling
# -------------------------------------------------------------------------
#' Pad a time-series dataframe and optionally fill values by block
#'
#' Expand a dataframe that contains a 'date' column to a regular sequence
#' of timestamps between specified start and end dates. The function can
#' operate in two modes:
#' - fill = FALSE: simply complete the sequence at the target interval.
#' - fill = TRUE: regularise the data at the native interval to create
#'   explicit blocks, then expand to the target interval and carry the
#'   block's values forward so that intra-block timestamps inherit the
#'   block's measured value (block-filling behaviour).
#'
#' The function detects the native input interval automatically if
#' 'interval' is not supplied, supports grouping via 'type', and preserves
#' timezones for POSIXt date columns.
#'
#' @param mydata Data.frame or tibble containing at least a 'date'
#'   column (Date or POSIXt).
#' @param type NULL or character vector of column names to group by.
#' @param interval NULL or character string describing target interval
#'   (e.g. "1 min", "1 hour"). If NULL, the native interval is used.
#' @param start.date Optional start date/time. If NULL, the group's
#'   minimum date is used.
#' @param end.date Optional end date/time. If NULL, the group's maximum
#'   date is used.
#' @param fill Logical; when TRUE performs block-based filling described
#'   above. When FALSE just completes the sequence leaving NA values.
#' @param print.int Logical; when TRUE prints detected/selected
#'   interval messages.
#' @return A dataframe expanded to the requested sequence with values
#'   filled according to 'fill'. The returned object preserves the
#'   'date' column type and timezone (for POSIXt).
#' @examples
#' df <- mydata[-c(2, 4, 7), ] # Remove some rows to create gaps
#' datePad(df)
#' @export
# -------------------------------------------------------------------------
# Main Function: Date Pad with "Block" Filling
# -------------------------------------------------------------------------
datePad <- function(
  mydata,
  type = NULL,
  interval = NULL,
  start.date = NULL,
  end.date = NULL,
  fill = FALSE,
  print.int = FALSE
) {
  # Basic validation
  if (nrow(mydata) < 2) {
    return(mydata)
  }
  if (!"date" %in% names(mydata)) {
    stop("Dataframe must contain a 'date' column.")
  }

  # 1. Detect Native Interval (The resolution of the INPUT data)
  # We need this to correctly establish the "blocks" of time before expanding
  native_interval <- find.time.interval(mydata$date)

  # 2. Determine Target Interval
  if (!is.null(interval)) {
    target_interval <- interval
    if (print.int) message("Target interval (User): ", target_interval)
  } else {
    target_interval <- native_interval
    if (print.int) message("Target interval (Auto): ", target_interval)
  }

  # 3. Handle Timezones/Dates
  tz_str <- attr(mydata$date, "tzone") %||% "GMT" # Helper if NULL

  align_date <- function(input, ref, tz) {
    if (is.null(input)) {
      return(NULL)
    }
    if (inherits(ref, "POSIXt")) {
      return(as.POSIXct(input, tz = tz))
    }
    if (inherits(ref, "Date")) {
      return(as.Date(input))
    }
    input
  }

  start.date <- align_date(start.date, mydata$date, tz_str)
  end.date <- align_date(end.date, mydata$date, tz_str)

  # -----------------------------------------------------------------------
  # Core Logic Helper
  # -----------------------------------------------------------------------
  process_group <- function(df) {
    # A. Define limits for this group
    s_date <- if (is.null(start.date)) {
      min(df$date, na.rm = TRUE)
    } else {
      start.date
    }
    e_date <- if (is.null(end.date)) max(df$date, na.rm = TRUE) else end.date

    # B. If fill=TRUE, we must strictly respect the 'Native' blocks.
    #    We first pad to the NATIVE interval to materialize missing NAs.
    if (fill) {
      # 1. Regularize at NATIVE resolution (creates explicit NAs for gaps)
      df_native <- df |>
        tidyr::complete(date = seq(s_date, e_date, by = native_interval)) |>
        dplyr::mutate(.block_id = dplyr::row_number()) # Unique ID for every native step

      # 2. Expand to TARGET resolution
      df_expanded <- df_native |>
        dplyr::select(date, .block_id) |> # Keep only ID and Date for structure
        tidyr::complete(date = seq(s_date, e_date, by = target_interval)) |>
        tidyr::fill(.block_id, .direction = "down") # Carry the ID down (ID 1 covers 10:00, 10:15...)

      # 3. Join original values back using the Block ID
      #    This ensures 10:15 gets 10:00's value (5), and 11:15 gets 11:00's value (NA)
      df_out <- df_expanded |>
        dplyr::left_join(
          select(df_native, -date),
          by = dplyr::join_by(.block_id == .block_id)
        ) |>
        dplyr::select(-.block_id)

      return(df_out)
    } else {
      # C. Simple case (No fill) - Just expand
      df |>
        tidyr::complete(date = seq(s_date, e_date, by = target_interval))
    }
  }

  # 4. Execution
  if (!is.null(type)) {
    out <- mydata |>
      dplyr::group_by(dplyr::across(dplyr::all_of(type))) |>
      dplyr::group_modify(~ process_group(.x)) |>
      dplyr::ungroup()
  } else {
    out <- process_group(mydata)
  }

  # 5. Restore Timezone
  if (inherits(mydata$date, "POSIXt")) {
    attr(out$date, "tzone") <- tz_str
  }

  return(out)
}

# Tiny helper for NULL checks
`%||%` <- function(a, b) if (is.null(a)) b else a

# from Deepayan Sarkar
panel.smooth.spline <-
  function(
    x,
    y,
    w = NULL,
    df,
    spar = NULL,
    cv = FALSE,
    lwd = lwd,
    lty = plot.line$lty,
    col,
    col.line = plot.line$col,
    type,
    horizontal = FALSE,
    all.knots = TRUE,
    ...
  ) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) {
      return()
    }
    if (!missing(col)) {
      if (missing(col.line)) {
        col.line <- col
      }
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal) {
      spline <-
        smooth.spline(
          y[ok],
          x[ok],
          w = w,
          df = df,
          spar = spar,
          cv = cv
        )
      panel.lines(
        x = spline$y,
        y = spline$x,
        col = col.line,
        lty = lty,
        lwd = lwd,
        ...
      )
    } else {
      spline <-
        smooth.spline(
          x[ok],
          y[ok],
          w = w,
          df = df,
          spar = spar,
          cv = cv
        )
      panel.lines(
        x = spline$x,
        y = spline$y,
        col = col.line,
        lty = lty,
        lwd = lwd,
        ...
      )
    }
  }

# panel functions for plots based on lattice #
panel.gam <- function(
  x,
  y,
  form = y ~ x,
  method = "loess",
  k = k,
  Args,
  ...,
  simulate = FALSE,
  n.sim = 200,
  autocor = FALSE,
  se = TRUE,
  level = 0.95,
  n = 100,
  col = plot.line$col,
  col.se = col,
  lty = plot.line$lty,
  lwd = plot.line$lwd,
  alpha = plot.line$alpha,
  alpha.se = 0.20,
  border = NA,
  subscripts,
  group.number,
  group.value,
  type,
  col.line,
  col.symbol,
  fill,
  pch,
  cex,
  font,
  fontface,
  fontfamily
) {
  # panel function to add a smooth line to a plot
  # Uses a GAM (mgcv) to fit smooth
  # Optionally can plot 95% confidence intervals and run bootstrap simulations
  # to estimate uncertainties. Simple block bootstrap is also available for correlated data

  # get rid of R check annoyances#
  plot.line <- NULL

  thedata <- data.frame(x = x, y = y)
  thedata <- na.omit(thedata)

  tryCatch(
    {
      if (!simulate) {
        if (is.null(k)) {
          mod <- suppressWarnings(mgcv::gam(
            y ~ s(x),
            select = TRUE,
            data = thedata,
            ...
          ))
        } else {
          mod <- suppressWarnings(mgcv::gam(
            y ~ s(x, k = k),
            select = TRUE,
            data = thedata,
            ...
          ))
        }

        lims <- current.panel.limits()
        xrange <- c(
          max(min(lims$x), min(x, na.rm = TRUE)),
          min(max(lims$x), max(x, na.rm = TRUE))
        )
        xseq <- seq(xrange[1], xrange[2], length = n)

        # for uncertainties
        std <- qnorm(level / 2 + 0.5)

        pred <- predict(mod, data.frame(x = xseq), se = TRUE)

        panel.lines(
          xseq,
          pred$fit,
          col = col,
          alpha = alpha,
          lty = lty,
          lwd = 2
        )

        results <- data.frame(
          date = xseq,
          pred = pred$fit,
          lower = pred$fit - std * pred$se,
          upper = pred$fit + std * pred$se
        )

        if (se) {
          panel.polygon(
            x = c(xseq, rev(xseq)),
            y = c(
              pred$fit -
                std * pred$se,
              rev(pred$fit + std * pred$se)
            ),
            col = col.se,
            alpha = alpha.se,
            border = border
          )
          pred <- pred$fit
        }
      } else {
        # simulations required

        x <- thedata$x
        y <- thedata$y

        sam.size <- length(x)

        lims <- current.panel.limits()
        xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
        xseq <- seq(xrange[1], xrange[2], length = sam.size)

        boot.pred <- matrix(nrow = sam.size, ncol = n.sim)

        message("Taking bootstrap samples. Please wait...")

        # set up bootstrap
        block.length <- 1

        if (autocor) {
          block.length <- round(sam.size^(1 / 3))
        }
        index <- samp.boot.block(sam.size, n.sim, block.length)

        # predict first
        if (is.null(k)) {
          mod <- mgcv::gam(y ~ s(x), data = thedata, ...)
        } else {
          mod <- mgcv::gam(y ~ s(x, k = k), data = thedata, ...)
        }

        residuals <- residuals(mod) # residuals of the model

        pred.input <- predict(mod, thedata)

        for (i in 1:n.sim) {
          # make new data
          new.data <- data.frame(
            x = xseq,
            y = pred.input + residuals[index[, i]]
          )

          mod <- mgcv::gam(y ~ s(x), data = new.data, ...)

          pred <- predict(mod, new.data)

          boot.pred[, i] <- as.vector(pred)
        }

        # calculate percentiles
        percentiles <- apply(
          boot.pred,
          1,
          function(x) quantile(x, probs = c(0.025, 0.975))
        )

        results <- as.data.frame(cbind(
          pred = rowMeans(boot.pred),
          lower = percentiles[1, ],
          upper = percentiles[2, ]
        ))

        if (se) {
          panel.polygon(
            x = c(xseq, rev(xseq)),
            y = c(results$lower, rev(results$upper)),
            col = col.se,
            alpha = alpha.se,
            border = border
          )
        }

        panel.lines(
          xseq,
          pred.input,
          col = col,
          alpha = alpha,
          lty = lty,
          lwd = 2
        )
      }
      results
    },
    error = function(x) {
      return()
    }
  )
}


# version of GAM fitting not for plotting - need to rationalise both...
fitGam <- function(
  thedata,
  x = "date",
  y = "conc",
  form = y ~ x,
  k = k,
  Args,
  ...,
  simulate = FALSE,
  n.sim = 200,
  autocor = FALSE,
  se = TRUE,
  level = 0.95,
  n = 100
) {
  # panel function to add a smooth line to a plot
  # Uses a GAM (mgcv) to fit smooth
  # Optionally can plot 95% confidence intervals and run bootstrap simulations
  # to estimate uncertainties. Simple block bootstrap is also available for correlated data

  data.orig <- thedata # return this if all else fails

  id <- which(names(thedata) == x)
  names(thedata)[id] <- "x"
  id <- which(names(thedata) == y)
  names(thedata)[id] <- "y"

  # can only fit numeric, so convert back after fitting
  class_x <- class(thedata$x)

  thedata$x <- as.numeric(thedata$x)

  tryCatch(
    {
      if (!simulate) {
        if (is.null(k)) {
          mod <- suppressWarnings(mgcv::gam(
            y ~ s(x),
            select = TRUE,
            data = thedata,
            ...
          ))
        } else {
          mod <- suppressWarnings(mgcv::gam(
            y ~ s(x, k = k),
            select = TRUE,
            data = thedata,
            ...
          ))
        }

        xseq <- seq(
          min(thedata$x, na.rm = TRUE),
          max(thedata$x, na.rm = TRUE),
          length = n
        )

        # for uncertainties
        std <- qnorm(level / 2 + 0.5)

        pred <- predict(mod, data.frame(x = xseq), se = se)

        results <- data.frame(
          date = xseq,
          pred = pred$fit,
          lower = pred$fit - std * pred$se,
          upper = pred$fit + std * pred$se
        )
      } else {
        # simulations required

        sam.size <- nrow(thedata)

        xseq <- seq(
          min(thedata$x, na.rm = TRUE),
          max(thedata$x, na.rm = TRUE),
          length = n
        )

        boot.pred <- matrix(nrow = sam.size, ncol = n.sim)

        message("Taking bootstrap samples. Please wait...")

        # set up bootstrap
        block.length <- 1

        if (autocor) {
          block.length <- round(sam.size^(1 / 3))
        }
        index <- samp.boot.block(sam.size, n.sim, block.length)

        # predict first
        if (is.null(k)) {
          mod <- mgcv::gam(y ~ s(x), data = thedata, ...)
        } else {
          mod <- mgcv::gam(y ~ s(x, k = k), data = thedata, ...)
        }

        residuals <- residuals(mod) # residuals of the model

        pred.input <- predict(mod, thedata)

        for (i in 1:n.sim) {
          # make new data
          new.data <- data.frame(
            x = xseq,
            y = pred.input + residuals[index[, i]]
          )

          mod <- mgcv::gam(y ~ s(x), data = new.data, ...)

          pred <- predict(mod, new.data)

          boot.pred[, i] <- as.vector(pred)
        }

        # calculate percentiles
        percentiles <- apply(
          boot.pred,
          1,
          function(x) quantile(x, probs = c(0.025, 0.975))
        )

        results <- as.data.frame(cbind(
          pred = rowMeans(boot.pred),
          lower = percentiles[1, ],
          upper = percentiles[2, ]
        ))
      }

      # convert class back to original
      class(results[[x]]) <- class_x
      return(results)
    },
    error = function(x) {
      data.orig
    }
  )
}


# list update function
# for lattice type object structure and ... handling
listUpdate <- function(
  a,
  b,
  drop.dots = TRUE,
  subset.a = NULL,
  subset.b = NULL
) {
  if (drop.dots) {
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if (!is.null(subset.a)) {
    a <- a[names(a) %in% subset.a]
  }
  if (!is.null(subset.b)) {
    b <- b[names(b) %in% subset.b]
  }
  if (length(names(b) > 0)) {
    a <- modifyList(a, b)
  }
  a
}

# common code for making legend list
# objects for use with drawOpenkey outputs
# uses listUpdate in utilities
makeOpenKeyLegend <- function(key, default.key, fun.name = "function") {
  # handle logicals and lists
  if (is.logical(key)) {
    legend <- if (key) default.key else NULL
  } else if (is.list(key)) {
    legend <- listUpdate(default.key, key)
  } else {
    if (!is.null(key)) {
      warning(
        paste(
          "In ",
          fun.name,
          "(...):\n unrecognised key not exported/applied\n",
          " [see ?drawOpenKey for key structure/options]",
          sep = ""
        ),
        call. = FALSE
      )
    }
    legend <- NULL
  }

  # structure like legend for drawOpenKey
  if (!is.null(legend)) {
    legend <- list(
      right = list(
        fun = drawOpenKey,
        args = list(key = legend),
        draw = FALSE
      )
    )
    if ("space" %in% names(legend$right$args$key)) {
      names(legend)[[1]] <- legend$right$args$key$space
    }
  }
  legend
}

# polygon that can deal with missing data for use in lattice plots with groups
poly.na <- function(
  x1,
  y1,
  x2,
  y2,
  group.number,
  myColors,
  alpha = 0.4,
  border = NA
) {
  for (i in seq(2, length(x1))) {
    if (!any(is.na(y2[c(i - 1, i)]))) {
      lpolygon(
        c(x1[i - 1], x1[i], x2[i], x2[i - 1]),
        c(y1[i - 1], y1[i], y2[i], y2[i - 1]),
        col = myColors[group.number],
        border = border,
        alpha = alpha
      )
    }
  }
}


# gives names of lattice strips
strip.fun <- function(results.grid, type, auto.text) {
  # proper names of labelling #
  pol.name <- sapply(
    levels(factor(results.grid[[type[1]]])),
    function(x) quickText(x, auto.text)
  )
  strip <- strip.custom(factor.levels = pol.name)

  if (length(type) == 1) {
    strip.left <- FALSE
  } else {
    # two conditioning variables

    pol.name <- sapply(
      levels(factor(results.grid[[type[2]]])),
      function(x) quickText(x, auto.text)
    )
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  if (length(type) == 1 & type[1] == "default") {
    strip <- FALSE
  } # remove strip
  list(strip, strip.left, pol.name)
}


# from lattice
chooseFace <- function(fontface = NULL, font = 1) {
  if (is.null(fontface)) {
    font
  } else {
    fontface
  }
}


# .smoothScatterCalcDensity() is also in graphics, but not exported.
.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x) {
  if (!("KernSmooth" %in% loadedNamespaces())) {
    ns <- try(loadNamespace("KernSmooth"))
    if (isNamespace(ns)) {
      message("(loaded the KernSmooth namespace)")
    } else {
      stop(
        "panel.smoothScatter() requires the KernSmooth package, but unable to load KernSmooth namespace"
      )
    }
  }
  if (length(nbin) == 1) {
    nbin <- c(nbin, nbin)
  }
  if (!is.numeric(nbin) || (length(nbin) != 2)) {
    stop("'nbin' must be numeric of length 1 or 2")
  }
  if (missing(bandwidth)) {
    bandwidth <- diff(apply(
      x,
      2,
      quantile,
      probs = c(0.05, 0.95),
      na.rm = TRUE
    )) /
      25
  } else {
    if (!is.numeric(bandwidth)) stop("'bandwidth' must be numeric")
  }
  bandwidth[bandwidth == 0] <- 1
  # create density map
  if (missing(range.x)) {
    rv <- KernSmooth::bkde2D(x, gridsize = nbin, bandwidth = bandwidth)
  } else {
    rv <- KernSmooth::bkde2D(
      x,
      gridsize = nbin,
      bandwidth = bandwidth,
      range.x = range.x
    )
  }
  rv$bandwidth <- bandwidth
  return(rv)
}


# simple rounding function from plyr
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

# pretty gap calculator
prettyGap <- function(x, n = 100) {
  return(diff(pretty(x, n))[1])
}

# function to check variables are numeric, if not force with warning
checkNum <- function(mydata, vars) {
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
checkDuplicateRows <- function(mydata, type = NULL, fn = cli::cli_warn) {
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
#' mapType(openairmaps::polar_data, fun = head, type = c("site", "site_type"))
mapType <- function(
  mydata,
  type,
  fun,
  .include_default = FALSE,
  .progress = FALSE
) {
  if ((all(type == "default") || is.null(type)) && !.include_default) {
    return(fun(mydata))
  }

  purrr::map(
    .x = split(mydata, mydata[type], drop = TRUE),
    .f = function(df) {
      out <- fun(df)
      out[type] <- df[1, type, drop = TRUE]
      return(out)
    },
    .progress = .progress
  ) |>
    dplyr::bind_rows() |>
    dplyr::relocate(dplyr::any_of(type))
}

#' Create nice labels out of breaks, if only breaks are provided
#' @noRd
breaksToLabels <- function(breaks, labels = NA) {
  if (any(is.na(labels)) || is.null(labels)) {
    labels <- c()
    for (i in seq_along(breaks)) {
      lhs <- breaks[i]
      rhs <- breaks[i + 1]
      str <- paste(lhs, rhs, sep = " - ")
      labels <- append(labels, str)
    }
    labels <- labels[-i]
  }
  return(labels)
}

#' pad out a set of numbers with zeroes to create consistent width
#' @noRd
strpad <- function(y, n = NULL) {
  y <- as.character(y)
  n <- n %||% max(nchar(y))
  while (any(nchar(y) < n)) {
    id <- nchar(y) < n
    y[id] <- paste("0", y[id], sep = "")
  }
  y
}
