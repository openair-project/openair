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
#' @param scale_x Options for the x-axis scale of the plot. If a single number,
#'   this will set an upper scale limit. If greater than one number, the range
#'   of the input will be used as the axis limits. Can also be [scale_opts()],
#'   which permit further customisation such as setting axis breaks, labels and
#'   scale transforms.
#'
#' @param scale_y Options for the y-axis scale of the plot. If a single number,
#'   this will set an upper scale limit. If greater than one number, the range
#'   of the input will be used as the axis limits. Can also be [scale_opts()],
#'   which permit further customisation such as setting axis breaks, labels and
#'   scale transforms.
#'
#' @param scale_col Options for the colour scale of the plot. If a single
#'   number, this will set an upper scale limit. If greater than one number, the
#'   range of the input will be used as the axis limits. Can also be
#'   [scale_opts()], which permit further customisation such as setting axis
#'   breaks, labels and scale transforms. Note that these options will not apply
#'   if `discretise` is used, as this overrides the continuous scale.
#'
#' @param wd_angle The angle to bin the wind direction to. Should divide into
#'   360 with no remainders; e.g., `5`, `10`, `12`, `15`, etc.
#'
#' @param r_axis_inside Either a numeric value, setting a theta axis value at
#'   which the axis should be placed inside the panel, or `FALSE`, which will
#'   place it outside of panel on a vertical axis.
#'
#' @param inner_radius A numeric between 0 and 1 setting the size of a inner
#'   radius hole.
#'
#' @param min_bin The minimum number of measurements to use to calculate a
#'   binned statistic. If the number is below `min_bin`, the value will be
#'   dropped from the returned plot and/or data.
#'
#' @param plot Should a plot be produced? `FALSE` will instead return the
#'   plotting data, and can be useful when analysing data to extract plot
#'   components and plotting them in other ways.
#'
#' @param ... Passed to [cutData()].
#'
#' @section Controlling scales:
#'
#'   In `openair`, a 'scale' is anything expressed in a numeric or date range.
#'   This includes axes and colour bars. Most `openair` plotting functions
#'   possess arguments in the pattern `scale_*` to control these - e.g.,
#'   `scale_x`, `scale_y`, and `scale_col`.
#'
#'   If these arguments are passed a single number, this will set the maximum of
#'   the scale to be that number. If two numbers are provided, these will be
#'   used as the scale range.
#'
#'   ```
#'   # x axis goes up to 100
#'   plot_xy_scatter(mydata, "nox", "no2", scale_x = 100)
#'
#'   # x axis is between 50 and 150
#'   plot_xy_scatter(mydata, "nox", "no2", scale_x = c(50, 150))
#'   ```
#'
#'   These `scale_*` options can also be provided with the [scale_opts()]
#'   function, which allows for further customisation, such as:
#'
#'   - Where to add major axis/legend breaks, and how to label them.
#'
#'   - Whether to transform the scale (e.g., a log or sqrt transform).
#'
#'   - Where to place the axis, and whether to draw a second one (axes only).
#'
#'   ```
#'   plot_xy_scatter(
#'     mydata,
#'     "nox",
#'     "no2",
#'     scale_x = scale_opts(
#'       transform = "pseudo_log",
#'       breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000),
#'       sec.axis = ggplot2::sec_axis(~.)
#'     )
#'   )
#'   ```
#'
#'   Take care with date scales (e.g., `scale_x` in [plot_trend_bars()]);
#'   `limits` should be provided as POSIXct objects. This is likely to be
#'   refined in future. Note that, for date scales, `date_breaks` and
#'   `date_labels` can be used to more easily refine the appearance of the
#'   scale.
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
