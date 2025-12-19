#' @name shared_ggplot_params
#' @title Shared ggplot parameters
#' @description This is a central place for describing typical parameters. It
#'   ensures consistency throughout openair.
#'
#' @param data A data frame.
#'
#' @param pollutant The names of the pollutants to plot; these should be columns
#'   in `data`.
#'
#' @param type The column(s) of `data` to use to split the plot into separate
#'   facets, passed to [cutData()]. `type` may be `NULL` or a vector with
#'   maximum length 2, which creates a 2D grid of plots.
#'
#' @param group The column of `data` to use to condition the data within each
#'   panel, passed to [cutData()]. This is usually achieved using colour, so
#'   will conflict with providing multiple `pollutant`s or other options which
#'   use the colour aesthetic.
#'
#' @param cols The colour palette to use. See [openColours()].
#'
#' @param auto_text Perform automatic text formatting on common character
#'   strings? `TRUE` will automatically, for example, subscript the 'x' in NOx
#'   in axis labels. `FALSE` will leave text unchanged, but users may refine it
#'   with, e.g., [ggplot2::labs()] or [ggplot2::guides()].
#'
#' @param discretise Options to discretise the continuous scale into discrete
#'   bins. Providing an integer will split the scale into that number of
#'   quantiles. Alternatively, [discretise_breaks()] allows for extensive
#'   customisation (e.g., defining specific bin widths, breaks, labels, and so
#'   on).
#'
#' @param windflow If `TRUE`, arrows showing the average magnitude and direction
#'   of wind within each bin will be shown. Note that this requires the columns
#'   `"ws"` and `"wd"` to be present in `data`. Alternatively, [windflow_opts()]
#'   allows for greater customisation of the arrows (e.g., wind speed ranges,
#'   relative size of arrows, arrow appearance).
#'
#' @param facet_opts A list of options to help control how 'facets' (different
#'   panels accessed through `type`) behave, e.g., whether different panels
#'   should share the same scales. Use [facet_opts()] as a convenient way to
#'   provide all necessary options.
#'
#' @param plot Should a plot be produced? `FALSE` will instead return the
#'   plotting data, and can be useful when analysing data to extract plot
#'   components and plotting them in other ways.
#'
#' @param ... Passed to [cutData()].
#'
#' @aliases docs-ggplot-internal
#' @keywords internal
#' @aliases NULL
NULL
