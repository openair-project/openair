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
#' @param r_axis_inside Either a numeric value, setting a theta axis value at
#'   which the axis should be placed inside the panel, or `FALSE`, which will
#'   place it outside of panel on a vertical axis.
#'
#' @param plot Should a plot be produced? `FALSE` will instead return the
#'   plotting data, and can be useful when analysing data to extract plot
#'   components and plotting them in other ways.
#'
#' @param ... Passed to [cutData()].
#'
#' @section Conditioning with `type`:
#'
#'   Many `openair` plotting functions can be conditioned by using the `type`
#'   argument. This splits a single plot into a grid of smaller plots,
#'   popularised under the name "small multiples" by Edward Tufte and more
#'   recently referred to as "faceting" (e.g., in [ggplot2::facet_wrap()]).
#'   `type` is always passed to [cutData()], meaning various presets like
#'   `"year"` or `"month"` can be used, and numeric variables are automatically
#'   binned.
#'
#'   If a single `type` is given, the default behaviour is for each panel to
#'   wrap into a roughly square grid. If two `type`s are given, the first will
#'   be used for the rows of the resulting grid and the second for the columns.
#'
#'   Not all plots can take two `type`s. This is usually because one facet
#'   dimension is already reserved for the specific plot type. For example, in
#'   [plot_calendar()], the plot is always faceted by `"month"` even if `type =
#'   NULL`.
#'
#'   ```
#'   # one type (square 2D grid)
#'   plot_heatmap(mydata, "no2", type = "year")
#'
#'   # two types (two 'axes' of types)
#'   plot_heatmap(mydata, "no2", type = c("o3", "weekend"))
#'   ```
#'
#'   Functions with `type` also possess the `facet_opts` argument, which takes
#'   the [facet_opts()] function. This allows for finer control of how faceting
#'   works. This includes:
#'
#'   - The number of rows/columns when only one `type` has been given.
#'
#'   - Whether axes should share the same x/y limits, or if each panel should
#'   have unique scales.
#'
#'   - If scales are free, whether each panel should have a different size
#'   proportional to their scales.
#'
#'   - On which side of the plot where the facet labels should be drawn.
#'
#'   - Whether all axes and axis labels should be drawn, or whether they should
#'   only be drawn on the margins.
#'
#'   Note that not all options will apply to all plots. For example, plots on a
#'   polar axis always have fixed x-axes that are mapped to the compass
#'   directions.
#'
#'   ```
#'   plot_heatmap(
#'     mydata,
#'     "no2",
#'     type = "year",
#'     facet_opts = facet_opts(nrow = 2, axes = "all_y", strip.position = "right")
#'   )
#'   ```
#'
#' @aliases docs-ggplot-internal
#' @keywords internal
#' @aliases NULL
NULL
