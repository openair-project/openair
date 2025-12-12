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

#' Define 'windflow' options for openair `ggplot2` plots
#'
#' Various `openair` functions include the `windflow` argument to include wind
#' speed & direction arrows. This function can be provided to the `windflow`
#' argument in place of `TRUE`/`FALSE` to better control the appearance of the
#' arrows.
#'
#' @param limits The limits of wind speed to include in the windflow annotation.
#'   Any wind speeds outside of this range will be set to the maximum/minimum
#'   value, as appropriate. Useful for ensuring arrows are consistent between
#'   plots, or - for example - extending the wind speed limit to zero using
#'   `range = c(0, NA)`.
#'
#' @param range The range of possible sizes of the windflow arrows. The default
#'   is broadly appropriate throughout `openair`, but if plots are being saved
#'   at different resolutions it may be useful to tweak this.
#'
#' @param arrow See [ggplot2::arrow()].
#'
#' @param windflow Include a windflow layer on the plot? Defaults to `TRUE`, and
#'   used internally by `openair` functions that may wish to override the user's
#'   options.
#'
#' @author Jack Davison
#' @family ggplot2 plot utilities
#'
#' @export
windflow_opts <- function(
  limits = c(NA, NA),
  range = c(0.1, 1),
  arrow = ggplot2::arrow(
    angle = 15,
    length = ggplot2::unit(0.5, "lines"),
    ends = "last",
    type = "closed"
  ),
  windflow = TRUE
) {
  list(
    limits = limits,
    range = range,
    arrow = arrow,
    windflow = windflow
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
