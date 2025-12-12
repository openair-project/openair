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
