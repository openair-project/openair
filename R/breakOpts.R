#' Define `breaks` options for `openair` plots
#'
#' This function provides a convenient way to set default options for `breaks`
#' arguments in `openair` plots, which cut continuous colour scales into
#' discrete bins.
#'
#' @param breaks,labels If a categorical colour scale is required, `breaks`
#'   should be specified. This can be either of:
#'
#'   - A single value, which will use the strategy specified by `method` to
#'   bin the scale. By default, this uses the same logic as [cutData()], which
#'   splits the scale into quantiles.
#'
#'   - A numeric vector, which will define the specific breakpoints. For
#'   example, `c(0, 50, 100)` will bin the data into `0 to 50`, `50 to 100`, and
#'   so on. If `breaks` does not cover the full range of the data, the outer
#'   limits will be extended so that the full colour scale is covered while
#'   retaining the desired number of breaks.
#'
#'   By default, `breaks` will generate nicely formatted labels for each
#'   category. The `labels` argument overrides this - for example, a user could
#'   define `breaks = 3, labels = c("low", "medium", "high")`. Care should be
#'   taken to provide the appropriate number of `labels` - it should always be
#'   equal to the number of bins, or one less than the number of breakpoints.
#'
#' @param method When `breaks` is a single integer (e.g., `breaks = 5`),
#'   `method` controls how this value is used to create breaks. Can be one of:
#'
#'   - `"number"` - splits the range into *n* quantiles (i.e., bins with roughly
#'   the same number of measurements in them). This is equivalent to
#'   [cutData()].
#'
#'   - `"interval"` - splits the range into *n* bins of equal width.
#'
#'   - `"width"` - splits the range into some number of bins with width *n*.
#'
#'   - `"pretty"` - splits the range into *approximately* *n* bins with
#'   aesthetically pleasing breakpoints using [pretty()].
#'
#' @param dig.lab Integer which is used when labels are not given. It determines
#'   the number of digits used in formatting the break numbers.
#'
#' @export
breakOpts <- function(
  breaks = NULL,
  labels = NULL,
  method = c("number", "interval", "width", "pretty"),
  dig.lab = NULL
) {
  method <- rlang::arg_match(method, multiple = FALSE)
  list(
    breaks = breaks,
    labels = labels,
    method = method,
    dig.lab = dig.lab
  )
}

#' Standardise the use of breaks
#' @noRd
resolve_break_opts <- function(x, extra.args, ...) {
  # if NULL, don't use breaks
  if (is.null(x)) {
    return(breakOpts())
  }

  # if numeric, use default strategy
  if (is.numeric(x)) {
    return(breakOpts(breaks = x, labels = extra.args$labels))
  }

  # if not a named list, error
  if (!rlang::is_list(x) || !rlang::is_named(x)) {
    cli::cli_abort(
      c(
        "x" = "The {.arg breaks} argument must be either a numeric vector or a named list.",
        "i" = "See {.fun openair::breakOpts} for more detail."
      )
    )
  }

  if ("labels" %in% names(extra.args)) {
    cli::cli_warn(
      "{.arg labels} should now be provided to the {.arg breaks} argument \\
      via {.fun openair::breakOpts}.",
      .frequency = "regularly",
      .frequency_id = "openair_labels_extra_args"
    )
  }

  # get the defaults, overriding anything function-specific with ...
  # need to do it this way to catch lattice args
  default_opts <- breakOpts(...)

  out <-
    breakOpts(
      breaks = x$breaks %||% default_opts$breaks,
      labels = x$labels %||% extra.args$labels %||% default_opts$labels,
      method = x$method %||% default_opts$method,
      dig.lab = x$dig.lab %||% extra.args$dig.lab %||% default_opts$dig.lab
    )

  out
}


#' Handle cutting numeric vectors for discretising plots
#' @noRd
cut_plot_breaks <- function(x, opts) {
  breaks <- opts$breaks
  labels <- opts$labels
  method <- opts$method

  if (is.null(breaks)) {
    return(x)
  }

  # if only one break, use as number of intervals
  if (length(breaks) == 1) {
    # get range of data
    x_range <- range(x, na.rm = TRUE)

    # number - get quantiles
    if (method == "number") {
      breaks <- quantile(
        x,
        probs = seq(0, 1, length.out = breaks),
        na.rm = TRUE
      ) |>
        unname()
    }

    # interval - get range of values w/ length "breaks"
    if (method == "interval") {
      breaks <- seq(x_range[1], x_range[2], length.out = breaks)
    }

    # width - get range of values w/ width "breaks"
    if (method == "width") {
      # ensure start/end divide into breaks
      x_range[1] <- floor(x_range[1] / breaks) * breaks
      x_range[2] <- ceiling(x_range[2] / breaks) * breaks
      breaks <- seq(x_range[1], x_range[2], by = breaks)
    }

    # pretty - get nice breakpoints
    if (method == "pretty") {
      breaks <- pretty(x_range, n = breaks)
    }
  }

  # ensure breaks look nice by rounding to appropriate decimal places
  breaks <- round_safely(breaks, n = 10, n_min = opts$dig.lab %||% 0)

  # assign labels if no labels are given
  labels <- labels %||%
    get_labels_from_breaks(breaks, labels, sep = " to ", dig.lab = opts$dig.lab)

  # ensure breaks covers all data
  if (max(breaks) < max(x, na.rm = TRUE)) {
    breaks[length(breaks)] <- ceiling(max(x, na.rm = TRUE))
  }
  if (min(breaks) > min(x, na.rm = TRUE)) {
    breaks[1] <- floor(min(x, na.rm = TRUE))
  }

  # cut x into breaks
  breaks <- sort(unique(breaks))
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

#' Create nice labels out of breaks, if only breaks are provided
#' @noRd
get_labels_from_breaks <- function(
  breaks,
  labels = NULL,
  sep = " - ",
  dig.lab = NULL
) {
  if (is.null(labels) || anyNA(labels)) {
    labels <- paste(
      format(
        utils::head(breaks, -1),
        scientific = FALSE,
        trim = TRUE,
        nsmall = dig.lab %||% 0L,
        drop0trailing = is.null(dig.lab)
      ),
      format(
        utils::tail(breaks, -1),
        scientific = FALSE,
        trim = TRUE,
        nsmall = dig.lab %||% 0L,
        drop0trailing = is.null(dig.lab)
      ),
      sep = sep
    )
  }
  labels
}
