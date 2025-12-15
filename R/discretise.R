#' Discretise continuous scales in `openair` plotting functions
#'
#' These functions are used in the `discretise` argument of many `openair`
#' plotting functions. [discretise_breaks()] is a way to define breaks directly.
#' The other functions use one of [ggplot2::cut_interval()],
#' [ggplot2::cut_number()] or [ggplot2::cut_width()] to discretise a continuous
#' scale, which can be more useful short-hands.
#'
#' @inheritParams base::cut
#' @inheritParams ggplot2::cut_width
#'
#' @family ggplot2 plot utilities
#' @rdname discretise_ggplot
#' @order 1
#' @export
discretise_breaks <- function(
  breaks,
  labels = NULL,
  include.lowest = FALSE,
  right = TRUE,
  dig.lab = 3
) {
  list(
    method = "breaks",
    breaks = breaks,
    labels = labels,
    include.lowest = include.lowest,
    right = right,
    dig.lab = dig.lab
  )
}

#' @rdname discretise_ggplot
#' @order 2
#' @export
discretise_interval <- function(
  n = NULL,
  length = NULL,
  labels = NULL
) {
  list(
    method = "interval",
    n = n,
    length = length,
    labels = labels
  )
}

#' @rdname discretise_ggplot
#' @order 3
#' @export
discretise_number <- function(
  n = NULL,
  labels = NULL
) {
  list(
    method = "number",
    n = n,
    labels = labels
  )
}

#' @rdname discretise_ggplot
#' @order 4
#' @export
discretise_width <- function(
  width = NULL,
  center = NULL,
  boundary = NULL,
  labels = NULL,
  closed = "right"
) {
  list(
    method = "width",
    width = width,
    center = center,
    boundary = boundary,
    labels = labels,
    closed = closed
  )
}

#' @noRd
cut_discrete_values <- function(x, opts) {
  if (is.numeric(opts) || is.integer(opts)) {
    opts <- discretise_interval(
      n = opts
    )
  }

  if (opts$method == "interval") {
    x <- ggplot2::cut_interval(
      x = x,
      n = opts$n,
      length = opts$length,
      labels = opts$labels
    )
  } else if (opts$method == "number") {
    x <- ggplot2::cut_number(
      x = x,
      n = opts$n,
      labels = opts$labels
    )
  } else if (opts$method == "width") {
    x <- ggplot2::cut_width(
      x = x,
      width = opts$width,
      labels = opts$labels
    )
  } else {
    x <- cut(x, breaks = opts$breaks, labels = opts$labels)
  }

  if (is.null(opts$labels)) {
    levels(x) <- gsub("\\(|\\]|\\[|\\)", "", levels(x))
    levels(x) <- gsub(",", " to ", levels(x))
  }

  x
}
