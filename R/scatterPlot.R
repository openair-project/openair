#' Flexible scatter plots
#'
#' Scatter plots with conditioning and three main approaches: conventional
#' scatterPlot, hexagonal binning and kernel density estimates. The former also
#' has options for fitting smooth fits and linear models with uncertainties
#' shown.
#'
#' [scatterPlot()] is the basic function for plotting scatter plots in flexible
#' ways in `openair`. It is flexible enough to consider lots of conditioning
#' variables and takes care of fitting smooth or linear relationships to the
#' data.
#'
#' There are four main ways of plotting the relationship between two variables,
#' which are set using the `method` option. The default `"scatter"` will plot a
#' conventional scatterPlot. In cases where there are lots of data and
#' over-plotting becomes a problem, then `method = "hexbin"` or `method =
#' "density"` can be useful. The former requires the `hexbin` package to be
#' installed.
#'
#' There is also a `method = "level"` which will bin the `x` and `y` data
#' according to the intervals set for `x.inc` and `y.inc` and colour the bins
#' according to levels of a third variable, `z`. Sometimes however, a far better
#' understanding of the relationship between three variables (`x`, `y` and `z`)
#' is gained by fitting a smooth surface through the data. See examples below.
#'
#' A smooth fit is shown if `smooth = TRUE` which can help show the overall form
#' of the data e.g. whether the relationship appears to be linear or not. Also,
#' a linear fit can be shown using `linear = TRUE` as an option.
#'
#' The user has fine control over the choice of colours and symbol type used.
#'
#' Another way of reducing the number of points used in the plots which can
#' sometimes be useful is to aggregate the data. For example, hourly data can be
#' aggregated to daily data. See [timePlot()] for examples here.
#'
#' @inheritParams shared_openair_params
#' @inheritParams timeAverage
#'
#' @param mydata A data frame containing at least two numeric variables to plot.
#'
#' @param x Name of the x-variable to plot. Note that x can be a date field or a
#'   factor. For example, `x` can be one of the `openair` built in types such as
#'   `"year"` or `"season"`.
#'
#' @param y Name of the numeric y-variable to plot.
#'
#' @param z Name of the numeric z-variable to plot for `method = "scatter"` or
#'   `method = "level"`. Note that for `method = "scatter"` points will be
#'   coloured according to a continuous colour scale, whereas for `method =
#'   "level"` the surface is coloured.
#'
#' @param method Methods include \dQuote{scatter} (conventional scatter plot),
#'   \dQuote{hexbin} (hexagonal binning using the `hexbin` package).
#'   \dQuote{level} for a binned or smooth surface plot and \dQuote{density} (2D
#'   kernel density estimates).
#'
#' @param group The grouping variable to use, if any. Setting this to a variable
#'   in the data frame has the effect of plotting several series in the same
#'   panel using different symbols/colours etc. If set to a variable that is a
#'   character or factor, those categories or factor levels will be used
#'   directly. If set to a numeric variable, it will split that variable in to
#'   quantiles.
#'
#' @param smooth A smooth line is fitted to the data if `TRUE`; optionally with
#'   95 percent confidence intervals shown. For `method = "level"` a smooth
#'   surface will be fitted to binned data.
#'
#' @param spline A smooth spline is fitted to the data if `TRUE`. This is
#'   particularly useful when there are fewer data points or when a connection
#'   line between a sequence of points is required.
#'
#' @param linear A linear model is fitted to the data if `TRUE`; optionally with
#'   95 percent confidence intervals shown. The equation of the line and R2
#'   value is also shown.
#'
#' @param ci Should the confidence intervals for the smooth/linear fit be shown?
#'
#' @param mod.line If `TRUE` three lines are added to the scatter plot to help
#'   inform model evaluation. The 1:1 line is solid and the 1:0.5 and 1:2 lines
#'   are dashed. Together these lines help show how close a group of points are
#'   to a 1:1 relationship and also show the points that are within a factor of
#'   two (FAC2).
#'
#' @param plot.type Type of plot: \dQuote{p} (points, default), \dQuote{l}
#'   (lines) or \dQuote{b} (both points and lines).
#'
#' @param log.x,log.y Should the x-axis/y-axis appear on a log scale? The
#'   default is `FALSE`. If `TRUE` a well-formatted log10 scale is used. This
#'   can be useful for checking linearity once logged.
#'
#' @param x.inc,y.inc The x/y-interval to be used for binning data when `method
#'   = "level"`.
#'
#' @param limits For `method = "level"` the function does its best to choose
#'   sensible limits automatically. However, there are circumstances when the
#'   user will wish to set different ones. The limits are set in the form
#'   `c(lower, upper)`, so `limits = c(0, 100)` would force the plot limits to
#'   span 0-100.
#'
#' @param k Smoothing parameter supplied to `gam` for fitting a smooth surface
#'   when `method = "level"`.
#'
#' @param dist When plotting smooth surfaces (`method = "level"` and `smooth =
#'   TRUE`), `dist` controls how far from the original data the predictions
#'   should be made. See `exclude.too.far` from the `mgcv` package. Data are
#'   first transformed to a unit square. Values should be between 0 and 1.
#'
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available, varying somewhat in different
#'   plotting functions:
#'
#'   - `title`, `subtitle`, `caption`, `xlab` and `ylab` control the plot
#'   title, subtitle, caption, x-axis label and y-axis label. All of these are
#'   passed through to [quickText()] if `auto.text = TRUE`.
#'
#'   - `xlim`, `ylim` and `limits` control the limits of the x-axis, y-axis and
#'   colorbar scales.
#'
#'   - `ncol` and `nrow` set the number of columns and rows in a faceted plot.
#'
#'   - `fontsize` overrides the overall font size of the plot by setting the
#'   `text` argument of [ggplot2::theme()]. It may also be applied
#'   proportionately to any `openair` annotations (e.g., N/E/S/W labels on polar
#'   coordinate plots).
#'
#'   - Various graphical parameters are also supported: `linewidth`,
#'   `linetype`,` shape`, `size`, `border`, and `alpha`. Not all parameters
#'   apply to all plots. These can take a single value, or a vector of multiple
#'   values - e.g., `shape = c(1, 2)` - which will be recycled to the length of
#'   values needed.
#'
#'   - For `method = "hexbin"` a log-scale fill is applied by default;
#'   pass `trans = NULL` to disable or provide custom `trans` and `inv`
#'   transform functions. `bins` controls the number of bins.
#'
#'   - `date.format` controls the format of date-time x-axes.
#'
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @seealso [timePlot()] and [timeAverage()] for details on selecting averaging
#'   times and other statistics in a flexible way
#' @examples
#' # load openair data if not loaded already
#' dat2004 <- selectByDate(mydata, year = 2004)
#'
#' # basic use, single pollutant
#'
#' scatterPlot(dat2004, x = "nox", y = "no2")
#' \dontrun{
#' # scatterPlot by year
#' scatterPlot(mydata, x = "nox", y = "no2", type = "year")
#' }
#'
#' # scatterPlot by day of the week, removing key at bottom
#' scatterPlot(dat2004,
#'   x = "nox", y = "no2", type = "weekday", key =
#'     FALSE
#' )
#'
#' # example of the use of continuous where colour is used to show
#' # different levels of a third (numeric) variable
#' # plot daily averages and choose a filled plot symbol (shape = 16)
#' # select only 2004
#' \dontrun{
#'
#' scatterPlot(dat2004, x = "nox", y = "no2", z = "co", avg.time = "day", shape = 16)
#'
#' # show linear fit, by year
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", smooth =
#'     FALSE, linear = TRUE
#' )
#'
#' # do the same, but for daily means...
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", smooth =
#'     FALSE, linear = TRUE, avg.time = "day"
#' )
#'
#' # log scales
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", smooth =
#'     FALSE, linear = TRUE, avg.time = "day", log.x = TRUE, log.y = TRUE
#' )
#'
#' # also works with the x-axis in date format (alternative to timePlot)
#' scatterPlot(mydata,
#'   x = "date", y = "no2", avg.time = "month",
#'   key = FALSE
#' )
#'
#' ## multiple types and grouping variable and continuous colour scale
#' scatterPlot(mydata, x = "nox", y = "no2", z = "o3", type = c("season", "weekend"))
#'
#' # use hexagonal binning
#' scatterPlot(mydata, x = "nox", y = "no2", method = "hexbin")
#'
#' # scatterPlot by year
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", type = "year", method =
#'     "hexbin"
#' )
#'
#' ## bin data and plot it - can see how for high NO2, O3 is also high
#' scatterPlot(mydata, x = "nox", y = "no2", z = "o3", method = "level", dist = 0.02)
#'
#' ## fit surface for clearer view of relationship
#' scatterPlot(mydata,
#'   x = "nox", y = "no2", z = "o3", method = "level",
#'   x.inc = 10, y.inc = 2, smooth = TRUE
#' )
#' }
scatterPlot <- function(
  mydata,
  x = "nox",
  y = "no2",
  z = NA,
  method = "scatter",
  group = NA,
  avg.time = "default",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  type = "default",
  smooth = FALSE,
  spline = FALSE,
  linear = FALSE,
  ci = TRUE,
  mod.line = FALSE,
  cols = "hue",
  plot.type = "p",
  key.title = group,
  key.columns = 1,
  key.position = "right",
  log.x = FALSE,
  log.y = FALSE,
  x.inc = NULL,
  y.inc = NULL,
  limits = NULL,
  windflow = NULL,
  ref.x = NULL,
  ref.y = NULL,
  k = NA,
  dist = 0.02,
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
  ...
) {
  # check key.position
  key.position <- check_key_position(key.position, key)

  ## extra args
  extra.args <- capture_dots(...)
  extra.args$size <- extra.args$size %||% 1
  extra.args$linewidth <- extra.args$linewidth %||% 1
  extra.args$linetype <- extra.args$linetype %||% 1
  extra.args$alpha <- extra.args$alpha %||% NA
  extra.args$border <- extra.args$border %||% NA

  ## validate
  if (is.na(z) && method == "level") {
    cli::cli_abort(
      "Need to specify {.arg z} when using {.code method = 'level'}."
    )
  }
  if (!is.na(group) && any(group %in% type)) {
    cli::cli_abort("Can't have {.arg group} also in {.arg type}.")
  }
  rlang::arg_match(method, c("scatter", "hexbin", "level", "density"))
  rlang::arg_match(plot.type, c("p", "l", "b", "s", "S", "spline"))

  ## handle windflow
  windflow <- resolve_windflow_opts(windflow)

  ## axis labels / title
  xlab <- quickText(extra.args$xlab %||% x, auto.text)
  ylab <- quickText(extra.args$ylab %||% y, auto.text)
  title <- quickText(extra.args$title, auto.text)
  subtitle <- quickText(extra.args$subtitle, auto.text)
  caption <- quickText(extra.args$caption, auto.text)
  tag <- quickText(extra.args$tag, auto.text)

  ## prepare data
  mydata <- prepare_scatter_data(
    mydata = mydata,
    x = x,
    y = y,
    z = z,
    group = group,
    type = type,
    avg.time = avg.time,
    data.thresh = data.thresh,
    statistic = statistic,
    percentile = percentile,
    windflow = windflow,
    log.x = log.x,
    log.y = log.y,
    ...
  )

  ## dispatch to method
  out <- switch(
    method,
    scatter = scatter_scatter(
      mydata = mydata,
      x = x,
      y = y,
      z = z,
      group = group,
      type = type,
      cols = cols,
      plot.type = plot.type,
      smooth = smooth,
      spline = spline,
      linear = linear,
      ci = ci,
      mod.line = mod.line,
      windflow = windflow,
      log.x = log.x,
      log.y = log.y,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      key.title = key.title,
      key.columns = key.columns,
      limits = limits,
      k = k,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag,
      extra.args = extra.args
    ),
    hexbin = scatter_hexbin(
      mydata = mydata,
      x = x,
      y = y,
      group = group,
      type = type,
      cols = cols,
      border = extra.args$border,
      linear = linear,
      ci = ci,
      mod.line = mod.line,
      log.x = log.x,
      log.y = log.y,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag,
      extra.args = extra.args
    ),
    level = scatter_level(
      mydata = mydata,
      x = x,
      y = y,
      z = z,
      type = type,
      cols = cols,
      statistic = statistic,
      smooth = smooth,
      mod.line = mod.line,
      x.inc = x.inc,
      y.inc = y.inc,
      limits = limits,
      log.x = log.x,
      log.y = log.y,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      k = k,
      dist = dist,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag,
      extra.args = extra.args
    ),
    density = scatter_density(
      mydata = mydata,
      x = x,
      y = y,
      type = type,
      cols = cols,
      mod.line = mod.line,
      log.x = log.x,
      log.y = log.y,
      ref.x = ref.x,
      ref.y = ref.y,
      key.position = key.position,
      auto.text = auto.text,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag,
      extra.args = extra.args
    )
  )

  if (plot) {
    print(out$plot)
  }

  output <- list(plot = out$plot, data = out$data, call = match.call())
  class(output) <- "openair"
  invisible(output)
}


# Data preparation --------------------------------------------------------

#' Prepare data for scatterPlot
#' @noRd
prepare_scatter_data <- function(
  mydata,
  x,
  y,
  z,
  group,
  type,
  avg.time,
  data.thresh,
  statistic,
  percentile,
  windflow,
  log.x,
  log.y,
  ...
) {
  ## types needed for time averaging
  types <- if (!is.na(group)) c(type, group) else type

  ## time average if needed
  if (avg.time != "default") {
    mydata <- cutData(mydata, types, ...)
    if ("default" %in% types) {
      mydata$default <- 0L
    }
    mydata <- timeAverage(
      mydata,
      type = types,
      avg.time = avg.time,
      statistic = statistic,
      percentile = percentile,
      data.thresh = data.thresh
    )
  }

  ## build required variable list
  vars <- if (any(type %in% dateTypes) || avg.time != "default") {
    c("date", x, y)
  } else {
    c(x, y)
  }

  if (!is.na(group)) {
    if (
      group %in% dateTypes || avg.time != "default" || any(type %in% dateTypes)
    ) {
      vars <- if (group %in% dateTypes) {
        unique(c(vars, "date"))
      } else {
        unique(c(vars, "date", group))
      }
    } else {
      vars <- unique(c(vars, group))
    }
  }

  if (windflow$windflow) {
    vars <- unique(c(vars, "wd", "ws"))
  }
  if (!is.na(z)) {
    vars <- c(vars, z)
  }

  mydata <- checkPrep(mydata, unique(vars), type)

  ## filter for log scales (require strictly positive)
  if (log.x) {
    mydata <- mydata[!is.na(mydata[[x]]) & mydata[[x]] > 0, ]
  }
  if (log.y) {
    mydata <- mydata[!is.na(mydata[[y]]) & mydata[[y]] > 0, ]
  }

  mydata
}


# Scatter method ----------------------------------------------------------

#' Build ggplot for method = "scatter"
#' @noRd
scatter_scatter <- function(
  mydata,
  x,
  y,
  z,
  group,
  type,
  cols,
  plot.type,
  smooth,
  spline,
  linear,
  ci,
  mod.line,
  windflow,
  log.x,
  log.y,
  ref.x,
  ref.y,
  key.position,
  key.title,
  key.columns,

  limits,
  k,
  auto.text,
  xlab,
  ylab,
  title = title,
  subtitle = subtitle,
  caption = caption,
  tag = tag,
  extra.args
) {
  has_z <- !is.na(z)
  has_group <- !is.na(group)

  ## cut data for conditioning and group
  mydata <- cutData(mydata, type)
  if (has_group) {
    mydata <- cutData(mydata, group)
  }

  ## colour palette info
  if (has_z) {
    ## continuous z overrides group
    n_groups <- 1L
    group_var <- NULL
    group_levels <- "all"
    if (missing(cols) || identical(cols, "hue")) cols <- "default"
  } else if (has_group) {
    group_levels <- levels(as.factor(mydata[[group]]))
    n_groups <- length(group_levels)
    group_var <- group
  } else {
    n_groups <- 1L
    group_var <- NULL
    group_levels <- "all"
  }

  myColors <- resolve_colour_opts(cols, n_groups)

  ## panel column names for LM labels and facet
  panel_cols <- setdiff(type, "default")

  ## point size / alpha
  pt_size <- extra.args$size %||% 1
  pt_alpha <- extra.args$alpha %||% NA
  pt_shape <- extra.args$shape %||%
    (if (has_z) {
      16L
    } else if (has_group) {
      NULL
    } else {
      16L
    })
  ln_width <- extra.args$linewidth %||% 1
  ln_type <- extra.args$linetype %||% 1

  ## base plot
  plt <- ggplot2::ggplot(mydata, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  ## add points / lines based on plot.type and colour mode
  if (has_z) {
    z_aes <- ggplot2::aes(color = .data[[z]])
    plt <- plt +
      .scatter_geoms(
        plot.type,
        z_aes,
        pt_size,
        pt_alpha,
        pt_shape,
        ln_width,
        ln_type,
        extra.args = extra.args
      )

    ## continuous colour scale for z
    if (is.null(limits)) {
      z_limits <- NULL
      z_labs <- ggplot2::waiver()
    } else {
      z_limits <- limits
      z_labs <- .scatter_limit_labels(mydata[[z]], limits)
    }
    plt <- plt +
      ggplot2::scale_color_gradientn(
        colors = resolve_colour_opts(cols, 200),
        limits = z_limits,
        oob = scales::oob_squish,
        labels = z_labs,
        guide = ggplot2::guide_colorbar(
          position = key.position
        ),
        aesthetics = c("colour", "fill")
      ) +
      ggplot2::labs(
        color = quickText(z, auto.text)
      )
  } else if (has_group) {
    grp_aes <- ggplot2::aes(
      color = .data[[group]],
      shape = .data[[group]]
    )
    plt <- plt +
      .scatter_geoms(
        plot.type,
        grp_aes,
        pt_size,
        pt_alpha,
        NULL,
        ln_width,
        ln_type,
        extra.args = extra.args
      )

    shape_vals <- if (!is.null(pt_shape)) {
      rep_len(pt_shape, n_groups)
    } else {
      seq_len(n_groups) %% 25 + 1L
    }

    pol_name <- sapply(group_levels, function(x) quickText(x, auto.text))

    theGuide <- ggplot2::guide_legend(
      ncol = key.columns,
      theme = ggplot2::theme(
        legend.title.position = if (key.position %in% c("left", "right")) {
          "top"
        } else {
          key.position
        }
      )
    )

    plt <- plt +
      ggplot2::scale_color_manual(
        values = myColors,
        labels = pol_name,
        guide = theGuide,
        aesthetics = c("colour", "fill")
      ) +
      ggplot2::scale_shape_manual(
        values = shape_vals,
        labels = pol_name,
        guide = theGuide
      ) +
      ggplot2::labs(
        color = quickText(key.title, auto.text),
        shape = quickText(key.title, auto.text)
      )
  } else {
    ## single series — fixed colour, no legend
    plt <- plt +
      .scatter_geoms(
        plot.type,
        ggplot2::aes(),
        pt_size,
        pt_alpha,
        pt_shape %||% 16L,
        ln_width,
        ln_type,
        fixed_color = myColors[1L],
        extra.args = extra.args
      )
  }

  ## smooth fit
  if (smooth) {
    gam_formula <- if (is.na(k)) {
      y ~ s(x, bs = "cs")
    } else {
      stats::as.formula(paste0("y ~ s(x, k=", k, ", bs='cs')"))
    }
    plt <- plt +
      ggplot2::geom_smooth(
        method = "gam",
        formula = gam_formula,
        se = ci,
        color = "grey20"
      )
  }

  ## spline fit
  if (spline || plot.type %in% c("s", "S", "spline")) {
    plt <- plt +
      ggplot2::geom_smooth(
        method = "loess",
        se = FALSE,
        color = "grey20",
        linewidth = ln_width
      )
  }

  ## linear fit
  if (linear) {
    if (has_group) {
      ## per-group lines: explicitly map colour/fill so geom_smooth produces one
      ## line per group (the base ggplot() call has no group aesthetic)
      plt <- plt +
        ggplot2::geom_smooth(
          mapping = ggplot2::aes(
            color = .data[[group_var]],
            fill = .data[[group_var]]
          ),
          method = "lm",
          formula = y ~ x,
          se = ci,
          linewidth = 1,
          linetype = 5,
          show.legend = FALSE
        )
    } else {
      plt <- plt +
        ggplot2::geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = ci,
          color = "black",
          fill = "grey80",
          linewidth = 1,
          linetype = 5
        )
    }
    lm_labs <- compute_lm_labels(
      mydata = mydata,
      x = x,
      y = y,
      group_var = group_var,
      panel_cols = panel_cols,
      x_nam = x,
      y_nam = y,
      auto.text = auto.text
    )
    if (!is.null(lm_labs) && nrow(lm_labs) > 0L) {
      if (has_group) {
        plt <-
          plt +
          ggplot2::geom_text(
            data = lm_labs |>
              dplyr::mutate(y = 0.98 - ((.data$g_idx - 1) * 0.05)),
            mapping = ggplot2::aes(
              x = I(0.025),
              y = I(.data$y),
              label = .data$label,
              color = .data[[group_var]]
            ),
            hjust = 0,
            vjust = 1,
            inherit.aes = FALSE,
            parse = TRUE,
            size = 2.5,
            show.legend = FALSE
          )
      } else {
        plt <- plt +
          ggplot2::geom_text(
            data = lm_labs |>
              dplyr::mutate(y = 0.98 - ((.data$g_idx - 1) * 0.05)),
            mapping = ggplot2::aes(
              x = I(0.025),
              y = I(.data$y),
              label = .data$label,
            ),
            hjust = 0,
            vjust = 1,
            inherit.aes = FALSE,
            parse = TRUE,
            size = 2.5,
            color = "black"
          )
      }
    }
  }

  ## model evaluation lines (FAC2)
  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  ## windflow arrows
  if (windflow$windflow) {
    plt <- plt +
      layer_windflow_opts(data = NULL, windflow_opts = windflow)
  }

  ## reference lines
  plt <- plt +
    .layer_ref_scatter(ref.x, "x", mydata[[x]]) +
    .layer_ref_scatter(ref.y, "y", mydata[[y]])

  ## axis scales
  plt <- plt +
    .scatter_x_scale(log.x, x, mydata, extra.args) +
    .scatter_y_scale(log.y, y, mydata, extra.args) +
    gg_coord_limits(extra.args)

  ## faceting
  plt <- plt +
    get_facet(
      type,
      extra.args,
      auto.text = auto.text,

      drop = FALSE,
      wd.res = extra.args$wd.res %||% 8
    )

  ## theme + labels + strip control
  plt <- plt +
    theme_openair(
      key.position = if (n_groups == 1L && !has_z) "none" else key.position,
      extra.args = extra.args
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag
    )

  return(
    list(
      plot = plt,
      data = mydata
    )
  )
}


# Hexbin method -----------------------------------------------------------

#' Build ggplot for method = "hexbin"
#' @noRd
scatter_hexbin <- function(
  mydata,
  x,
  y,
  group,
  type,
  cols,
  border,
  linear,
  ci,
  mod.line,
  log.x,
  log.y,
  ref.x,
  ref.y,
  key.position,

  auto.text,
  xlab,
  ylab,
  title = title,
  subtitle = subtitle,
  caption = caption,
  tag,
  extra.args
) {
  mydata <- cutData(mydata, type)

  myColors <- resolve_colour_opts(cols, 200)

  ## hexbin fill transform (default: log; trans = NULL → none)
  hex_transform <- if (
    "trans" %in% names(extra.args) && is.null(extra.args$trans)
  ) {
    "identity"
  } else if ("trans" %in% names(extra.args) && is.function(extra.args$trans)) {
    inv_fn <- extra.args$inv %||% function(x) x
    scales::new_transform("hex_custom", extra.args$trans, inv_fn)
  } else {
    "log"
  }

  plt <- ggplot2::ggplot(mydata, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_hex(bins = extra.args$bins %||% 30, colour = border) +
    ggplot2::scale_fill_gradientn(
      colors = myColors,
      transform = hex_transform,
      labels = scales::label_number(accuracy = 1),
      guide = ggplot2::guide_colorbar(
        position = key.position
      )
    ) +
    ggplot2::labs(
      fill = "count"
    )

  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  if (linear) {
    plt <- plt +
      ggplot2::geom_smooth(
        method = "lm",
        se = ci,
        color = "black",
        fill = "grey80",
        linewidth = 1,
        linetype = 5
      )
    lm_labs <- compute_lm_labels(
      mydata = mydata,
      x = x,
      y = y,
      group_var = NA,
      panel_cols = setdiff(type, "default"),
      x_nam = x,
      y_nam = y,
      auto.text = auto.text
    )
    if (!is.null(lm_labs) && nrow(lm_labs) > 0L) {
      plt <- plt +
        ggplot2::geom_text(
          data = lm_labs,
          mapping = ggplot2::aes(
            x = .data$x_pos,
            y = .data$y_pos,
            label = .data$label,
            vjust = .data$g_idx * 1.5
          ),
          hjust = 0,
          inherit.aes = FALSE,
          parse = TRUE,
          size = 2.5
        )
    }
  }

  plt <- plt +
    .layer_ref_scatter(ref.x, "x", mydata[[x]]) +
    .layer_ref_scatter(ref.y, "y", mydata[[y]]) +
    .scatter_x_scale(log.x, x, mydata, extra.args) +
    .scatter_y_scale(log.y, y, mydata, extra.args) +
    gg_coord_limits(extra.args) +
    get_facet(
      type,
      extra.args,
      auto.text = auto.text,

      drop = FALSE,
      wd.res = extra.args$wd.res %||% 8
    ) +
    theme_openair(key.position = key.position, extra.args = extra.args) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag
    )

  return(
    list(
      plot = plt,
      data = mydata
    )
  )
}


# Level method ------------------------------------------------------------

#' Build ggplot for method = "level"
#' @noRd
scatter_level <- function(
  mydata,
  x,
  y,
  z,
  type,
  cols,
  statistic,
  smooth,
  mod.line,
  x.inc,
  y.inc,
  limits,
  log.x,
  log.y,
  ref.x,
  ref.y,
  key.position,

  k,
  dist,
  auto.text,
  xlab,
  ylab,
  title = title,
  subtitle = subtitle,
  caption = caption,
  tag,
  extra.args
) {
  mydata <- cutData(mydata, type)

  # bin intervals
  prettyGap <- function(x, n = 100) {
    return(diff(pretty(x, n))[1])
  }
  if (is.null(x.inc)) {
    x.inc <- prettyGap(mydata[[x]])
  }
  if (is.null(y.inc)) {
    y.inc <- prettyGap(mydata[[y]])
  }

  mydata$xgrid <- round_any(mydata[[x]], x.inc)
  mydata$ygrid <- round_any(mydata[[y]], y.inc)

  ## type columns (cutData creates a "default" column for type = "default")
  type_cols <- type # keep all, including "default"
  vars <- c("xgrid", "ygrid", type_cols)

  ## aggregate only if needed (data not already gridded)
  if (nrow(unique(mydata[c("xgrid", "ygrid")])) != nrow(mydata)) {
    vars_select <- c(vars, z)
    ## retain only needed columns but preserve all type cols
    avail_cols <- intersect(vars_select, names(mydata))
    agg_by_cols <- intersect(vars, names(mydata))
    mydata <- mydata |>
      dplyr::select(dplyr::all_of(avail_cols)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(agg_by_cols))) |>
      dplyr::summarise(
        !!z := switch(
          statistic,
          frequency = length(.data[[z]]),
          median = stats::median(.data[[z]], na.rm = TRUE),
          mean(.data[[z]], na.rm = TRUE) # default: mean
        ),
        .groups = "drop"
      )
  }

  ## smooth surface via GAM
  if (smooth) {
    k_val <- if (is.na(k)) 10L else k
    mydata <- map_type(
      mydata,
      type = type,
      fun = function(df) smooth_level_grid(df, z, k_val, dist),
      .include_default = TRUE
    )
  }

  ## colour scale limits
  z_vals <- mydata[[z]]
  if (is.null(limits)) {
    z_limits <- range(z_vals, na.rm = TRUE)
    scale_labs <- ggplot2::waiver()
  } else {
    z_limits <- limits
    scale_labs <- .scatter_limit_labels(z_vals, limits)
    ## clip values to limits
    mydata[[z]] <- pmin(pmax(mydata[[z]], limits[1L]), limits[2L])
  }

  if (missing(cols) || identical(cols, "hue")) {
    cols <- "default"
  }
  myColors <- resolve_colour_opts(cols, 200)

  geom_fn <- if (smooth) ggplot2::geom_raster else ggplot2::geom_tile

  plt <- ggplot2::ggplot(
    mydata,
    ggplot2::aes(x = .data$xgrid, y = .data$ygrid, fill = .data[[z]])
  ) +
    geom_fn() +
    ggplot2::scale_fill_gradientn(
      colors = myColors,
      limits = z_limits,
      oob = scales::oob_squish,
      labels = scale_labs,
      na.value = "transparent",
      guide = ggplot2::guide_colorbar(
        position = key.position
      )
    ) +
    ggplot2::labs(
      fill = quickText(z, auto.text)
    )

  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  plt <- plt +
    .layer_ref_scatter(ref.x, "x", mydata$xgrid) +
    .layer_ref_scatter(ref.y, "y", mydata$ygrid) +
    .scatter_x_scale(log.x, "xgrid", mydata, extra.args) +
    .scatter_y_scale(log.y, "ygrid", mydata, extra.args) +
    gg_coord_limits(extra.args) +
    get_facet(
      type,
      extra.args,
      auto.text = auto.text,

      drop = FALSE,
      wd.res = extra.args$wd.res %||% 8
    ) +
    theme_openair(key.position = key.position, extra.args = extra.args) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag
    )

  return(
    list(
      plot = plt,
      data = mydata
    )
  )
}

#' Fit smooth GAM surface for level plot
#' @noRd
smooth_level_grid <- function(mydata, z, k, dist) {
  res <- 201L
  mydata <- stats::na.omit(mydata)

  myform <- stats::as.formula(paste0(
    z,
    " ~ ti(xgrid, k=",
    k,
    ") + ti(ygrid, k=",
    k,
    ") + ti(xgrid, ygrid, k=",
    k,
    ")"
  ))

  Mgam <- mgcv::gam(myform, data = mydata)
  xseq <- seq(min(mydata$xgrid), max(mydata$xgrid), length.out = res)
  yseq <- seq(min(mydata$ygrid), max(mydata$ygrid), length.out = res)
  new.dat <- expand.grid(xgrid = xseq, ygrid = yseq)

  new.dat[[z]] <- as.vector(mgcv::predict.gam(Mgam, newdata = new.dat))

  ## exclude predictions too far from original data
  too_far <- mgcv::exclude.too.far(
    rep(xseq, res),
    rep(yseq, each = res),
    mydata$xgrid,
    mydata$ygrid,
    dist = dist
  )
  new.dat[[z]][too_far] <- NA_real_

  new.dat
}


# Density method ----------------------------------------------------------

#' Compute 2-D kernel density grid for one facet group
#' @noRd
#'
# Copied from grDevices (GPL-2), with attribution
.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x) {
  if (length(nbin) == 1) {
    nbin <- c(nbin, nbin)
  }
  if (!is.numeric(nbin) || length(nbin) != 2) {
    stop("'nbin' must be numeric of length 1 or 2")
  }
  if (missing(bandwidth)) {
    bandwidth <- diff(apply(
      x,
      2,
      stats::quantile,
      probs = c(0.05, 0.95),
      na.rm = TRUE,
      names = FALSE
    )) /
      25
    bandwidth[bandwidth == 0] <- 1
  } else {
    if (!is.numeric(bandwidth)) {
      stop("'bandwidth' must be numeric")
    }
    if (any(bandwidth <= 0)) {
      stop("'bandwidth' must be positive")
    }
  }

  rv <- KernSmooth::bkde2D(
    x,
    bandwidth = bandwidth,
    gridsize = nbin,
    range.x = range.x
  )

  rv$bandwidth <- bandwidth
  rv
}

.density_kde_grid <- function(subdata, x, y) {
  xv <- subdata[[x]]
  yv <- subdata[[y]]
  xy <- grDevices::xy.coords(xv, yv)
  mat <- cbind(xy$x, xy$y)
  mat <- mat[is.finite(mat[, 1L]) & is.finite(mat[, 2L]), , drop = FALSE]
  n <- nrow(mat)

  Map <- .smoothScatterCalcDensity(mat, nbin = 256L)
  grid <- expand.grid(x = Map$x1, y = Map$x2)
  grid$z <- as.vector(Map$fhat) * n
  ## suppress near-zero KDE bleed — anything below 1 % of peak → transparent
  grid$z[grid$z < max(grid$z) * 0.001] <- NA_real_
  grid
}

#' Build ggplot for method = "density"
#' @noRd
scatter_density <- function(
  mydata,
  x,
  y,
  type,
  cols,
  mod.line,
  log.x,
  log.y,
  ref.x,
  ref.y,
  key.position,

  auto.text,
  xlab,
  ylab,
  title,
  subtitle,
  caption,
  tag,
  extra.args
) {
  mydata <- cutData(mydata, type)

  if (missing(cols) || identical(cols, "hue")) {
    cols <- "default"
  }

  grid_data <- map_type(
    mydata,
    type = type,
    fun = function(df) .density_kde_grid(df, x, y),
    .include_default = TRUE
  )

  myColors <- resolve_colour_opts(cols, 200)

  plt <- ggplot2::ggplot(
    grid_data,
    ggplot2::aes(x = .data$x, y = .data$y, fill = .data$z)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(
      colors = myColors,
      na.value = "transparent",
      guide = ggplot2::guide_colorbar(
        position = key.position
      )
    ) +
    ggplot2::labs(
      fill = "intensity"
    )

  if (mod.line) {
    plt <- plt + gg_mod_lines()
  }

  plt <- plt +
    .layer_ref_scatter(ref.x, "x", mydata[["x"]]) +
    .layer_ref_scatter(ref.y, "y", mydata[["y"]]) +
    .scatter_x_scale(log.x, "x", grid_data, extra.args) +
    .scatter_y_scale(log.y, "y", grid_data, extra.args) +
    gg_coord_limits(extra.args) +
    get_facet(
      type,
      extra.args,
      auto.text = auto.text,

      drop = FALSE,
      wd.res = extra.args$wd.res %||% 8
    ) +
    theme_openair(key.position = key.position, extra.args = extra.args) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag
    )

  return(
    list(
      plot = plt,
      data = grid_data
    )
  )
}


# Shared helpers ----------------------------------------------------------

#' Add geom layers based on plot.type
#' @noRd
.scatter_geoms <- function(
  plot.type,
  mapping,
  size,
  alpha,
  shape,
  linewidth,
  linetype,
  fixed_color = NULL,
  extra.args
) {
  alpha_val <- if (is.na(alpha)) 1 else alpha
  pt_args <- list(mapping = mapping, size = size, alpha = alpha_val)
  if (!is.null(shape)) {
    pt_args$shape <- shape
  }
  if (!is.null(fixed_color)) {
    pt_args$color <- fixed_color
  }

  ln_args <- list(
    mapping = mapping,
    linewidth = linewidth,
    linetype = linetype,
    alpha = alpha_val,
    lineend = extra.args$lineend %||% "butt",
    linejoin = extra.args$linejoin %||% "round",
    linemitre = extra.args$linemitre %||% 10
  )
  if (!is.null(fixed_color)) {
    ln_args$color <- fixed_color
  }

  switch(
    plot.type,
    "p" = list(do.call(ggplot2::geom_point, pt_args)),
    "l" = list(do.call(ggplot2::geom_line, ln_args)),
    "b" = list(
      do.call(ggplot2::geom_line, ln_args),
      do.call(ggplot2::geom_point, pt_args)
    ),
    ## spline/S/s handled via smooth
    list(do.call(ggplot2::geom_point, pt_args))
  )
}

#' Build x-axis scale (log or linear)
#' @noRd
.scatter_x_scale <- function(log.x, x_col, mydata, extra.args) {
  if (log.x) {
    ggplot2::scale_x_continuous(
      transform = "log10",
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    )
  } else {
    x_vals <- mydata[[x_col]]
    is_date <- inherits(x_vals, c("Date", "POSIXct", "POSIXlt"))
    if (is_date) {
      if (inherits(x_vals, "Date")) {
        ggplot2::scale_x_date(
          labels = extra.args$date.format %||% ggplot2::waiver(),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        )
      } else {
        ggplot2::scale_x_datetime(
          date_labels = extra.args$date.format %||% ggplot2::waiver(),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        )
      }
    } else {
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.02, 0.02))
      )
    }
  }
}

#' Build y-axis scale (log or linear)
#' @noRd
.scatter_y_scale <- function(log.y, y_col, mydata, extra.args) {
  if (log.y) {
    ggplot2::scale_y_continuous(
      transform = "log10",
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    )
  } else {
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    )
  }
}

#' Apply xlim/ylim via coord_cartesian so data outside limits is not dropped
#' @noRd
gg_coord_limits <- function(extra.args) {
  xlim <- extra.args$xlim
  ylim <- extra.args$ylim
  if (!is.null(xlim) || !is.null(ylim)) {
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  } else {
    list()
  }
}

#' Build custom labels for clipped colour limits
#' @noRd
.scatter_limit_labels <- function(z_vals, limits) {
  function(breaks) {
    labs <- as.character(breaks)
    if (max(limits) < max(z_vals, na.rm = TRUE)) {
      labs[which.max(breaks)] <- paste(">", labs[which.max(breaks)])
    }
    if (min(limits) > min(z_vals, na.rm = TRUE)) {
      labs[which.min(breaks)] <- paste("<", labs[which.min(breaks)])
    }
    labs
  }
}

#' FAC2 model evaluation lines
#'
#' Adds three reference lines for model evaluation (1:1, 1:2, 1:0.5). Correct
#' on any axis transformation as `geom_function` operates in data space.
#' @noRd
gg_mod_lines <- function() {
  list(
    ggplot2::geom_function(fun = identity, linetype = 1L, color = "black"),
    ggplot2::geom_function(
      fun = function(x) 2 * x,
      linetype = 5L,
      color = "black"
    ),
    ggplot2::geom_function(
      fun = function(x) x / 2,
      linetype = 5L,
      color = "black"
    )
  )
}

## Convert a variable name to a plotmath-safe string fragment using quickText.
## quickText() returns an expression object; deparse(expr[[1]]) converts the
## inner call back to a string that can be embedded in a paste(...) plotmath
## expression rendered by geom_text(parse = TRUE).
.fmt_varname <- function(name, auto.text) {
  if (!auto.text) {
    return(paste0("'", name, "'"))
  }
  qt <- quickText(name, auto.text = TRUE)
  if (is.expression(qt)) {
    deparse(qt[[1]])
  } else {
    paste0("'", as.character(qt), "'")
  }
}

## Returns a data frame with one row per (panel × group) combination
## containing the fitted equation, R² and positioning info for geom_text.
## @noRd
compute_lm_labels <- function(
  mydata,
  x,
  y,
  group_var,
  panel_cols,
  x_nam,
  y_nam,
  auto.text
) {
  by_cols <- c(
    panel_cols,
    if (!is.null(group_var) && !is.na(group_var)) group_var else character(0L)
  )
  by_cols <- by_cols[by_cols != "default"]

  ## fit lm for a single subset; returns NULL on failure or < 3 obs
  fit_one <- function(df, g_idx = 1L) {
    lm_dat <- stats::na.omit(data.frame(xv = df[[x]], yv = df[[y]]))
    if (nrow(lm_dat) < 3L) {
      return(NULL)
    }
    tryCatch(
      {
        mod <- stats::lm(yv ~ xv, data = lm_dat)
        b <- stats::coef(mod)
        r2 <- summary(mod)$r.squared
        symb <- if (b[1L] >= 0) "+" else ""
        ## Build a plotmath string: variable names are deparsed quickText
        ## expressions embedded in paste(...); geom_text uses parse = TRUE.
        x_pm <- .fmt_varname(x_nam, auto.text)
        y_pm <- .fmt_varname(y_nam, auto.text)
        lbl <- paste0(
          "paste(",
          y_pm,
          ", '=",
          sprintf("%.2f", b[2L]),
          "[', ",
          x_pm,
          ", ']",
          symb,
          sprintf("%.2f", b[1L]),
          " ', R^2, '=",
          sprintf("%.2f", r2),
          "')"
        )
        data.frame(
          x_pos = -Inf,
          y_pos = Inf,
          g_idx = g_idx,
          label = lbl,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) NULL
    )
  }

  ## add group index within each panel for y-offset
  has_group <- !is.null(group_var) && !is.na(group_var)
  if (has_group) {
    panel_only <- setdiff(by_cols, group_var)
    if (length(panel_only) > 0L) {
      mydata <- mydata |>
        dplyr::group_by(dplyr::across(dplyr::all_of(panel_only))) |>
        dplyr::mutate(
          .g_idx = match(.data[[group_var]], unique(.data[[group_var]]))
        ) |>
        dplyr::ungroup()
    } else {
      mydata <- dplyr::mutate(
        mydata,
        .g_idx = match(.data[[group_var]], unique(.data[[group_var]]))
      )
    }
  } else {
    mydata <- dplyr::mutate(mydata, .g_idx = 1L)
  }

  if (length(by_cols) == 0L) {
    return(fit_one(mydata))
  }

  ## split by panel + group, fit lm for each subset
  split_cols <- c(by_cols, ".g_idx")
  grouped <- dplyr::group_by(mydata, dplyr::across(dplyr::all_of(split_cols)))
  keys <- dplyr::group_keys(grouped)
  groups <- dplyr::group_split(grouped)

  results <- lapply(seq_along(groups), function(i) {
    r <- fit_one(groups[[i]], g_idx = keys$.g_idx[[i]])
    if (is.null(r)) {
      return(NULL)
    }
    key_row <- keys[i, setdiff(names(keys), ".g_idx"), drop = FALSE]
    rownames(key_row) <- NULL
    rownames(r) <- NULL
    cbind(key_row, r)
  })

  dplyr::bind_rows(Filter(Negate(is.null), results))
}

# handle ref layers for scatterplot
.layer_ref_scatter <- function(ref, which, vec) {
  if (lubridate::is.POSIXct(vec)) {
    layer_ref(
      ref,
      which,
      type = "datetime",
      tz = lubridate::tz(vec)
    )
  } else if (lubridate::is.Date(vec)) {
    layer_ref(
      ref,
      which,
      type = "date"
    )
  } else {
    layer_ref(
      ref,
      which,
      type = "numeric"
    )
  }
}
