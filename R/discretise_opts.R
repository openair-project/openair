#' Discretise continuous scales in `openair` plotting functions
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   These functions are used in the `discretise` argument of many `openair`
#'   plotting functions to bin a continuous colour scale.
#'
#'   - [disc_breaks()] is a way to define breaks directly, by giving direct access to the `breaks` argument.
#'
#'   - [disc_interval()], [disc_number()] and [disc_width()] use [ggplot2::cut_interval()], [ggplot2::cut_number()] or [ggplot2::cut_width()], respectively. These give convenient ways to discretise a scale.
#'
#'   - [disc_width_n()] acts like [disc_width()] for a certain number of intervals, then combines the rest of the data into a single bin. This is similar logic to [windRose()].
#'
#'   - [disc_pretty()] uses [pretty()] to work out pleasantly spaced breakpoints. Note that `n` is *approximate* in this case; it may not be possible to create exactly `n` 'pretty' breakpoints. The upper and lower limits are also not likely to be 'pretty' as they will be set to the limits of the scale.
#'
#' @inheritParams base::cut
#' @inheritParams ggplot2::cut_width
#'
#' @param n_ints In [disc_width_n()], the number of intervals to create using
#'   `width`.
#'
#' @param start In [disc_width_n()], where to start interval creation. If
#'   `NULL`, will pick a 'pretty' start point based on the scale limits.
#'
#' @family ggplot2 plot utilities
#' @rdname disc_ggplot
#' @order 1
#' @export
#'
#' @examples
#' plot_heatmap(mydata, "no2")
#'
#' # 5 groups with equal range
#' plot_heatmap(mydata, "no2", discretise = disc_interval(5))
#'
#' # 5 groups with the same number of observations
#' plot_heatmap(mydata, "no2", discretise = disc_number(5))
#'
#' # however many groups with width 5
#' plot_heatmap(mydata, "no2", discretise = disc_width(5))
#'
#' # 4 groups with width 5, and then the rest of the data
#' plot_heatmap(mydata, "no2", discretise = disc_width_n(5, 4))
#'
#' # approximately 5 'pretty' breaks
#' plot_heatmap(mydata, "no2", discretise = disc_pretty(5))
#'
#' # defined (irregular) breaks - with optional labels
#' plot_heatmap(
#'   mydata,
#'   "no2",
#'   discretise = disc_breaks(
#'     c(20, 25, 30, 40, 50, 100),
#'     labels = c("Very Low", "Low", "Medium", "High", "Very High")
#'   )
#' )
disc_breaks <- function(
  breaks,
  labels = NULL,
  closed = "right",
  dig.lab = 3
) {
  closed <- rlang::arg_match(closed, c("left", "right"))
  as_openair_disc(
    method = "breaks",
    breaks = breaks,
    labels = labels,
    right = closed == "right",
    dig.lab = dig.lab
  )
}

#' @rdname disc_ggplot
#' @order 2
#' @export
disc_interval <- function(
  n = NULL,
  length = NULL,
  labels = NULL
) {
  as_openair_disc(
    method = "interval",
    n = n,
    length = length,
    labels = labels
  )
}

#' @rdname disc_ggplot
#' @order 3
#' @export
disc_number <- function(
  n = NULL,
  labels = NULL
) {
  as_openair_disc(
    method = "number",
    n = n,
    labels = labels
  )
}

#' @rdname disc_ggplot
#' @order 4
#' @export
disc_width <- function(
  width = NULL,
  center = NULL,
  boundary = NULL,
  labels = NULL,
  closed = "right"
) {
  closed <- rlang::arg_match(closed, c("left", "right"))
  as_openair_disc(
    method = "width",
    width = width,
    center = center,
    boundary = boundary,
    labels = labels,
    closed = closed
  )
}

#' @rdname disc_ggplot
#' @order 5
#' @export
disc_width_n <- function(
  width = NULL,
  n_ints = 4L,
  start = NULL,
  closed = "right",
  dig.lab = 3
) {
  closed <- rlang::arg_match(closed, c("left", "right"))
  as_openair_disc(
    method = "width_n",
    width = width,
    n_ints = n_ints,
    start = start,
    closed = closed,
    dig.lab = dig.lab
  )
}

#' @rdname disc_ggplot
#' @order 6
#' @export
disc_pretty <- function(
  n = NULL,
  closed = "right",
  dig.lab = 3
) {
  closed <- rlang::arg_match(closed, c("left", "right"))
  as_openair_disc(
    method = "pretty",
    n = n,
    closed = "right",
    dig.lab = 3
  )
}

#' @noRd
cut_discrete_values <- function(x, opts) {
  if (is.null(opts)) {
    return(x)
  }

  if (is.numeric(opts) || is.integer(opts)) {
    opts <- disc_number(
      n = opts
    )
  }

  if (opts$method == "interval") {
    x <- ggplot2::cut_interval(
      x = x,
      n = opts$args$n,
      length = opts$args$length,
      labels = opts$args$labels
    )
  } else if (opts$method == "number") {
    x <- ggplot2::cut_number(
      x = x,
      n = opts$args$n,
      labels = opts$args$labels
    )
  } else if (opts$method == "width") {
    x <- ggplot2::cut_width(
      x = x,
      width = opts$args$width,
      labels = opts$args$labels
    )
  } else if (opts$method == "width_n") {
    start <- opts$args$start %||% min(pretty(x), na.rm = TRUE)

    breaks <- seq(start, by = opts$args$width, length.out = opts$args$n_ints)

    if (min(x, na.rm = TRUE) < min(breaks)) {
      breaks <- c(min(x, na.rm = TRUE), breaks)
    }

    if (max(x, na.rm = TRUE) > max(breaks)) {
      breaks <- c(breaks, max(x, na.rm = TRUE))
    }

    x <- cut(
      x,
      breaks = breaks,
      right = opts$args$closed == "right",
      dig.lab = opts$args$dig.lab,
      include.lowest = TRUE
    )
  } else if (opts$method == "pretty") {
    breaks <- pretty(x, n = opts$args$n)
    breaks <- breaks[
      breaks > min(x, na.rm = TRUE) & breaks < max(x, na.rm = TRUE)
    ]

    if (min(x, na.rm = TRUE) < min(breaks)) {
      breaks <- c(min(x, na.rm = TRUE), breaks)
    }

    if (max(x, na.rm = TRUE) > max(breaks)) {
      breaks <- c(breaks, max(x, na.rm = TRUE))
    }

    x <- cut(
      x,
      breaks = breaks,
      right = opts$args$closed == "right",
      dig.lab = opts$args$dig.lab,
      include.lowest = TRUE
    )
  } else {
    x <- cut(
      x,
      breaks = opts$args$breaks,
      labels = opts$args$labels,
      right = opts$args$right,
      dig.lab = opts$args$dig.lab,
      include.lowest = TRUE
    )
  }

  if (is.null(opts$args$labels)) {
    levels(x) <- gsub("\\(|\\]|\\[|\\)", "", levels(x))
    levels(x) <- gsub(",", " to ", levels(x))
  }

  x
}

#' @noRd
as_openair_disc <- function(method, ...) {
  x <- list(
    method = method,
    args = list(...)
  )
  class(x) <- "openair_disc"
  x
}

#' @method print openair_disc
#' @export
print.openair_disc <- function(x, ...) {
  cli::cli_h1("{.pkg openair} discretisation helper")
  cli::cli_text(
    "Provide this to the {.arg discretise} argument in an {.pkg openair} plotting function."
  )
  cli::cli_h2("Details")
  cli::cli_text("This helper uses the {.field {x$method}} method.")
  cli::cli_ul(id = "print_openair_disc")
  for (i in seq_along(x$args)) {
    cli::cli_li("{.field {names(x$args[i])}}: {x$args[i]}")
  }
  cli::cli_end(id = "print_openair_disc")
}
