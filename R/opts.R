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

#' Define scale options for openair `ggplot2` plots
#'
#' This function conveniently allows for options to be defined in relation to
#' the various scales of `openair` plots. These arguments are automatically
#' passed to [ggplot2::continuous_scale()]. Note that, for certain plots, not
#' all of these may be controllable for the user (for example, if a scale
#' transformation would not make sense as in the y-axis of
#' [plot_polar_heatmap()]). `date_breaks` and `date_labels` only apply to
#' date(time) axes (e.g., in [plot_trend_bars()]).
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritParams ggplot2::scale_x_datetime
#'
#' @author Jack Davison
#' @family ggplot2 plot utilities
#'
#' @export
scale_opts <- function(
  limits = NULL,
  breaks = ggplot2::waiver(),
  labels = ggplot2::waiver(),
  date_breaks = ggplot2::waiver(),
  date_labels = ggplot2::waiver(),
  transform = scales::transform_identity(),
  position = NULL,
  sec.axis = ggplot2::waiver()
) {
  list(
    limits = limits,
    breaks = breaks,
    labels = labels,
    date_breaks = date_breaks,
    date_labels = date_labels,
    transform = transform,
    position = position,
    sec.axis = sec.axis
  )
}

#' Function that resolves scale option logic
#' @noRd
resolve_scale_opts <- function(x) {
  if (is.numeric(x)) {
    if (length(x) == 1) {
      x <- scale_opts(limits = c(NA, x[!is.na(x)]))
    } else if (length(x) == 2) {
      x <- scale_opts(limits = x)
    } else {
      x <- scale_opts(limits = range(x, na.rm = TRUE))
    }
  }
  return(x)
}
