#' Define 'faceting' options for openair `ggplot2` plots
#'
#' This function conveniently allows for options to be defined in relation to
#' how plots are *faceted* (i.e., split into multiple panels). These arguments
#' are automatically passed to [ggplot2::facet_wrap()], [ggplot2::facet_grid()]
#' or similar extension functions depending on the value the user has passed to
#' `type`.
#'
#' @inheritParams ggplot2::facet_grid
#' @inheritParams ggplot2::facet_wrap
#'
#' @param nrow,ncol The number of rows and/or columns for a 'wrap' layout (i.e.,
#'   when there is only one `type`/'faceting' variable).
#'
#' @author Jack Davison
#' @family ggplot2 plot utilities
#'
#' @export
facet_opts <- function(
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  space = "fixed",
  labeller = "label_value",
  drop = FALSE,
  strip.position = "top",
  switch = NULL,
  axes = "margins",
  axis.labels = "all"
) {
  list(
    nrow = nrow,
    ncol = ncol,
    scales = scales,
    space = space,
    drop = drop,
    strip.position = strip.position,
    switch = switch,
    axes = axes,
    axis.labels = axis.labels
  )
}

#' Define 'discretisation' options for openair `ggplot2` plots
#'
#' This function conveniently allows for certain plots which plot a continuous
#' scale.
#'
#' @param method How the discretisation should occur. One of:
#' - `"intervals"` (the default, which makes *n* groups with equal range. Requires `n` or `length`)
#' - `"number"` (which makes *n* groups with roughly equal numbers of observations. Requires `n`)
#' - `"width"` (which makes groups of width *width*. Requires `width`.)
#' - `"breaks"` (where `breaks` must be set manually)
#'
#' @inheritParams ggplot2::cut_width
#' @inheritParams base::cut
#'
#' @author Jack Davison
#' @family ggplot2 plot utilities
#'
#' @export
discretise_opts <- function(
  method = c("interval", "number", "width", "breaks"),
  n = NULL,
  length = NULL,
  width = NULL,
  labels = NULL,
  breaks = NULL
) {
  method <- rlang::arg_match(method)

  list(
    method = method,
    breaks = breaks,
    labels = labels,
    n = n,
    length = length,
    width = width
  )
}

#' @noRd
cut_discrete_values <- function(x, opts) {
  if (is.numeric(opts) || is.integer(opts)) {
    opts <- discretise_opts(
      method = "interval",
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
  x
}
