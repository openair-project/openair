#' Conditional quantile estimates for model evaluation
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Conditional quantiles are a very useful way of considering model
#'   performance against observations for continuous measurements (Wilks, 2005).
#'   The conditional quantile plot splits the data into evenly spaced bins. For
#'   each predicted value bin e.g. from 0 to 10~ppb the *corresponding* values
#'   of the observations are identified and the median, 25/75th and 10/90
#'   percentile (quantile) calculated for that bin. The data are plotted to show
#'   how these values vary across all bins. For a time series of observations
#'   and predictions that agree precisely the median value of the predictions
#'   will equal that for the observations for each bin.
#'
#' @inheritParams shared_ggplot_params
#'
#' @param bins Number of bins to be used in calculating the different quantile
#'   levels.
#'
#' @param cols The colour palette to use. See [openColours()]. The first colour
#'   will be used for the 10/90 percentile band, the second for the 25/75 band,
#'   and the third for the median line.
#'
#' @param col_ideal,col_histogram The colours to use for the 'ideal' case (the
#'   1:1 diagonal line and observed value histogram) and the modelled value
#'   histogram. Passed to [openColours()].
#'
#' @inheritSection shared_ggplot_params Controlling scales
#' @inheritSection shared_ggplot_params Conditioning with `type`
#'
#' @family ggplot2 model evaluation functions
#' @seealso the legacy [conditionalQuantile()] function
#' @author Jack Davison
#'
#' @export
#'
#' @references
#'
#' Murphy, A. H., B.G. Brown and Y. Chen. (1989) Diagnostic Verification of
#' Temperature Forecasts, Weather and Forecasting, Volume: 4, Issue: 4, Pages:
#' 485-501.
#'
#' Wilks, D. S., 2005. Statistical Methods in the Atmospheric Sciences, Volume
#' 91, Second Edition (International Geophysics), 2nd Edition. Academic Press.
#'
#' @examples
#' \dontrun{
#' # create a small dataset
#' mod_data <- dplyr::select(mydata, date, nox)
#'
#' # lets create some fake modelled values
#' mod_data <-
#'   dplyr::mutate(
#'     mod_data,
#'     mod = jitter(nox, factor = 1000)
#'   )
#'
#' # plot this on a taylor diagram
#' plot_conditional_quantile(mod_data, "nox", "mod")
#' }
plot_conditional_quantile <- function(
  data,
  obs,
  mod,
  type = NULL,
  bins = 30,
  min_bin = 20,
  scale_y = openair::scale_opts(),
  scale_x = openair::scale_opts(),
  cols = "viridis",
  col_ideal = "grey50",
  col_histogram = "grey70",
  facet_opts = openair::facet_opts(),
  auto_text = TRUE,
  plot = TRUE,
  ...
) {
  scale_y <- resolve_scale_opts(scale_y)
  scale_x <- resolve_scale_opts(scale_x)
  type <- type %||% "default"

  # minimum bin size
  bins[bins < 10] <- 10

  # get breaks - shared between observed and modelled values, but based on
  # modelled values
  mod_breaks <- seq(
    min(data[[mod]], na.rm = TRUE),
    max(data[[mod]], na.rm = TRUE),
    length.out = bins + 1
  )

  # cut data
  vars <- c(mod, obs)
  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }
  data <- checkPrep(
    data,
    vars,
    type = type,
    remove.calm = FALSE,
    remove.neg = FALSE
  ) |>
    dplyr::filter(.data[[mod]] >= 0, .data[[obs]] >= 0)
  data <- cutData(data, type, ...)

  # get quantiles & count per modelled value
  plotdata <-
    data |>
    dplyr::mutate(
      mod_bin = cut(
        .data[[mod]],
        breaks = mod_breaks,
        labels = FALSE,
        include.lowest = TRUE
      ),
      breaks_lower = mod_breaks[.data$mod_bin],
      breaks_upper = mod_breaks[.data$mod_bin + 1],
      {{ mod }} := (.data$breaks_lower + .data$breaks_upper) / 2
    ) |>
    dplyr::select(-c("mod_bin", "breaks_lower", "breaks_upper")) |>
    dplyr::reframe(
      q = quantile(
        .data[[obs]],
        probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
        na.rm = TRUE
      ),
      n_mod = dplyr::n(),
      .by = dplyr::all_of(c(mod, type))
    ) |>
    dplyr::mutate(
      p = c(0.1, 0.25, 0.5, 0.75, 0.9) * 100,
      p = factor(.data$p),
      .by = dplyr::all_of(c(mod, type))
    ) |>
    tidyr::pivot_wider(
      names_from = "p",
      values_from = "q",
      names_prefix = "p"
    ) |>
    dplyr::filter(!is.na(.data[[mod]]), .data$n_mod >= min_bin)

  # get counts for observations
  obs_counts <-
    data |>
    dplyr::mutate(
      obs_bin = cut(
        .data[[obs]],
        breaks = mod_breaks,
        labels = FALSE,
        include.lowest = TRUE
      ),
      breaks_lower = mod_breaks[.data$obs_bin],
      breaks_upper = mod_breaks[.data$obs_bin + 1],
      {{ obs }} := (.data$breaks_lower + .data$breaks_upper) / 2
    ) |>
    dplyr::select(-c("obs_bin", "breaks_lower", "breaks_upper")) |>
    dplyr::reframe(
      n_obs = dplyr::n(),
      .by = dplyr::all_of(c(obs, type))
    ) |>
    dplyr::filter(!is.na(.data[[obs]]), .data$n_obs >= min_bin)

  # replace 'obs' with mod - to bind data
  names(obs_counts)[names(obs_counts) == obs] <- mod

  # join two datasets
  plotdata <-
    dplyr::full_join(
      plotdata,
      obs_counts,
      by = c(mod, type)
    ) |>
    dplyr::arrange(.data[[mod]])

  # get barwidths to remove spacing
  barwidth <- mapType(
    plotdata,
    type,
    \(df) {
      diffs <- df[[mod]] - dplyr::lag(df[[mod]])
      dplyr::as_tibble(table(diffs)) |>
        dplyr::mutate(diffs = as.numeric(.data$diffs))
    }
  ) |>
    dplyr::slice_min(order_by = .data$diffs, with_ties = FALSE) |>
    dplyr::pull("diffs")

  # a nice range for both axes
  nice_range <- range(pretty(c(plotdata$p90, plotdata[[mod]]), na.rm = TRUE))

  # rescale counts to be on same axis as the above range
  plotdata$n_mod_rescaled <- scales::rescale(
    plotdata$n_mod,
    to = nice_range,
    from = c(0, max(plotdata$n_mod, na.rm = TRUE))
  )

  # rescale counts for observations - note that we're rescaling to the modelled
  # values to allow these to go 'off the scale' if needed
  plotdata$n_obs_rescaled <- scales::rescale(
    plotdata$n_obs,
    to = nice_range,
    from = c(0, max(plotdata$n_mod, na.rm = TRUE))
  )

  # create plot
  plt <-
    plotdata |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[mod]])) +
    ggplot2::geom_abline(
      color = openColours(col_ideal, n = 1),
      lty = 2,
      slope = 1,
      intercept = 0
    ) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data$n_mod_rescaled),
      fill = openColours(col_histogram, n = 1),
      alpha = 1 / 3,
      width = barwidth,
      na.rm = TRUE
    ) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data$n_obs_rescaled),
      color = openColours(col_ideal, n = 1),
      fill = NA,
      width = barwidth,
      na.rm = TRUE
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$p10,
        ymax = .data$p90,
        fill = "10/90th percentile"
      ),
      alpha = 2 / 3,
      na.rm = TRUE
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$p25,
        ymax = .data$p75,
        fill = "25/75th percentile"
      ),
      alpha = 2 / 3,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$p50, color = "median"),
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = openColours(cols, n = 3),
      aesthetics = c("color", "fill"),
      drop = TRUE
    ) +
    ggplot2::labs(
      color = NULL,
      fill = NULL,
      x = "predicted value",
      y = "observed value"
    ) +
    theme_oa_classic() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::coord_cartesian(
      ratio = 1,
      xlim = nice_range,
      ylim = nice_range
    ) +
    ggplot2::scale_y_continuous(
      breaks = scale_y$breaks,
      position = scale_y$position %||% "left",
      expand = ggplot2::expansion(c(0, .1)),
      sec.axis = ggplot2::sec_axis(
        ~ scales::rescale(., c(0, max(plotdata$n_mod, na.rm = TRUE))),
        name = "histogram sample size"
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = scale_x$breaks,
      position = scale_x$position %||% "bottom",
      expand = ggplot2::expansion(c(0, .1))
    ) +
    get_facet_fun(type, facet_opts, auto_text)

  # return
  if (plot) {
    return(plt)
  } else {
    return(
      dplyr::select(plotdata, -"n_obs_rescaled", -"n_mod_rescaled")
    )
  }
}

#' Conditional quantile estimates for model evaluation
#'
#' Function to calculate conditional quantiles with flexible conditioning. The
#' function is for use in model evaluation and more generally to help better
#' understand forecast predictions and how well they agree with observations.
#'
#' Conditional quantiles are a very useful way of considering model performance
#' against observations for continuous measurements (Wilks, 2005). The
#' conditional quantile plot splits the data into evenly spaced bins. For each
#' predicted value bin e.g. from 0 to 10~ppb the *corresponding* values of
#' the observations are identified and the median, 25/75th and 10/90 percentile
#' (quantile) calculated for that bin. The data are plotted to show how these
#' values vary across all bins. For a time series of observations and
#' predictions that agree precisely the median value of the predictions will
#' equal that for the observations for each bin.
#'
#' The conditional quantile plot differs from the quantile-quantile plot (Q-Q
#' plot) that is often used to compare observations and predictions. A Q-Q~plot
#' separately considers the distributions of observations and predictions,
#' whereas the conditional quantile uses the corresponding observations for a
#' particular interval in the predictions. Take as an example two time series,
#' the first a series of real observations and the second a lagged time series
#' of the same observations representing the predictions. These two time series
#' will have identical (or very nearly identical) distributions (e.g. same
#' median, minimum and maximum). A Q-Q plot would show a straight line showing
#' perfect agreement, whereas the conditional quantile will not. This is because
#' in any interval of the predictions the corresponding observations now have
#' different values.
#'
#' Plotting the data in this way shows how well predictions agree with
#' observations and can help reveal many useful characteristics of how well
#' model predictions agree with observations --- across the full distribution of
#' values. A single plot can therefore convey a considerable amount of
#' information concerning model performance. The `conditionalQuantile`
#' function in openair allows conditional quantiles to be considered in a
#' flexible way e.g. by considering how they vary by season.
#'
#' The function requires a data frame consisting of a column of observations and
#' a column of predictions. The observations are split up into `bins`
#' according to values of the predictions. The median prediction line together
#' with the 25/75th and 10/90th quantile values are plotted together with a line
#' showing a \dQuote{perfect} model. Also shown is a histogram of predicted
#' values (shaded grey) and a histogram of observed values (shown as a blue
#' line).
#'
#' Far more insight can be gained into model performance through conditioning
#' using `type`. For example, `type = "season"` will plot conditional
#' quantiles by each season. `type` can also be a factor or character field
#' e.g. representing different models used.
#'
#' See Wilks (2005) for more details and the examples below.
#'
#' @param mydata A data frame containing the field `obs` and `mod`
#'   representing observed and modelled values.
#' @param obs The name of the observations in `mydata`.
#' @param mod The name of the predictions (modelled values) in `mydata`.
#' @param type `type` determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce a single plot using the
#'   entire data. Type can be one of the built-in types as detailed in
#'   `cutData` e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so
#'   on. For example, `type = "season"` will produce four plots --- one for
#'   each season.
#'
#'   It is also possible to choose `type` as another variable in the data
#'   frame. If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Type can be up length two e.g. `type = c("season", "weekday")` will
#'   produce a 2x2 plot split by season and day of the week. Note, when two
#'   types are provided the first forms the columns and the second the rows.
#' @param bins Number of bins to be used in calculating the different quantile
#'   levels.
#' @param min.bin The minimum number of points required for the estimates of the
#'   25/75th and 10/90th percentiles.
#' @param xlab label for the x-axis, by default \dQuote{predicted value}.
#' @param ylab label for the y-axis, by default \dQuote{observed value}.
#' @param col Colours to be used for plotting the uncertainty bands and median
#'   line. Must be of length 5 or more.
#' @param key.columns Number of columns to be used in the key.
#' @param key.position Location of the key e.g. \dQuote{top}, \dQuote{bottom},
#'   \dQuote{right}, \dQuote{left}. See `lattice` `xyplot` for more
#'   details.
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE`
#'   titles and axis labels etc. will automatically try and format pollutant
#'   names and units properly e.g.  by subscripting the `2' in NO2.
#' @param \dots Other graphical parameters passed onto `cutData` and
#'   `lattice:xyplot`. For example, `conditionalQuantile` passes the
#'   option `hemisphere = "southern"` on to `cutData` to provide
#'   southern (rather than default northern) hemisphere handling of `type =
#'   "season"`. Similarly, common axis and title labelling options (such as
#'   `xlab`, `ylab`, `main`) are passed to `xyplot` via
#'   `quickText` to handle routine formatting.
#' @import latticeExtra purrr tidyr
#' @export
#' @author David Carslaw
#' @family model evaluation functions
#' @seealso The `verification` package for comprehensive functions for
#'   forecast verification.
#' @references
#'
#' Murphy, A. H., B.G. Brown and Y. Chen. (1989) Diagnostic Verification of
#' Temperature Forecasts, Weather and Forecasting, Volume: 4, Issue: 4, Pages:
#' 485-501.
#'
#' Wilks, D. S., 2005. Statistical Methods in the Atmospheric Sciences, Volume
#' 91, Second Edition (International Geophysics), 2nd Edition. Academic Press.
#' @examples
#' ## make some dummy prediction data based on 'nox'
#' mydata$mod <- mydata$nox * 1.1 + mydata$nox * runif(1:nrow(mydata))
#'
#' # basic conditional quantile plot
#' ## A "perfect" model is shown by the blue line
#' ## predictions tend to be increasingly positively biased at high nox,
#' ## shown by departure of median line from the blue one.
#' ## The widening uncertainty bands with increasing NOx shows that
#' ## hourly predictions are worse for higher NOx concentrations.
#' ## Also, the red (median) line extends beyond the data (blue line),
#' ## which shows in this case some predictions are much higher than
#' ## the corresponding measurements. Note that the uncertainty bands
#' ## do not extend as far as the median line because there is insufficient
#' # to calculate them
#' conditionalQuantile(mydata, obs = "nox", mod = "mod")
#'
#' ## can split by season to show seasonal performance (not very
#' ## enlightening in this case - try some real data and it will be!)
#'
#' \dontrun{
#' conditionalQuantile(mydata, obs = "nox", mod = "mod", type = "season")
#' }
conditionalQuantile <- function(
  mydata,
  obs = "obs",
  mod = "mod",
  type = "default",
  bins = 31,
  min.bin = c(10, 20),
  xlab = "predicted value",
  ylab = "observed value",
  col = brewer.pal(5, "YlOrRd"),
  key.columns = 2,
  key.position = "bottom",
  auto.text = TRUE,
  ...
) {
  ## partly based on from Wilks (2005) and package verification, with many modifications
  # keep R check quite
  data <- second <- third <- NULL

  if (length(type) > 2) {
    stop("Only two types can be used with this function")
  }

  ## extra.args setup
  extra.args <- list(...)

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # label controls
  # (xlab and ylab handled in formals because unique action)
  extra.args$main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  } else {
    quickText("", auto.text)
  }

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  if (length(col) == 1 && col == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    # other local colours
    ideal.col <- "black"
    col.1 <- grDevices::grey(0.75)
    col.2 <- grDevices::grey(0.5)
    col.5 <- grDevices::grey(0.25)
  } else {
    ideal.col <- "#0080ff"
    col.1 <- col[1]
    col.2 <- col[2]
    col.5 <- col[5]
  }

  vars <- c(mod, obs)

  if (any(type %in% dateTypes)) {
    vars <- c("date", vars)
  }

  ## check the data
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  mydata <- na.omit(mydata)
  mydata <- cutData(mydata, type)

  procData <- function(mydata) {
    mydata <- select_if(mydata, is.numeric)
    obs <- mydata[[obs]]
    pred <- mydata[[mod]]
    min.d <- min(mydata)
    max.d <- max(mydata)
    bins <- seq(floor(min.d), ceiling(max.d), length = bins)

    lo <- min(bins)
    hi <- max(bins)
    b <- bins[-length(bins)]
    labs <- b + 0.5 * diff(bins)
    obs.cut <- cut(
      obs,
      breaks = bins,
      include.lowest = TRUE,
      labels = labs
    )
    obs.cut[is.na(obs.cut)] <- labs[1]
    obs.cut <- as.numeric(as.character(obs.cut))
    pred.cut <- cut(
      pred,
      breaks = bins,
      include.lowest = TRUE,
      labels = labs
    )
    pred.cut[is.na(pred.cut)] <- labs[1]

    n <- length(labs)
    lng <- tapply(obs, pred.cut, length)
    med <- tapply(obs, pred.cut, median)
    q1 <- tapply(obs, pred.cut, quantile, probs = 0.25)
    q2 <- tapply(obs, pred.cut, quantile, probs = 0.75)
    q1[lng <= min.bin[1]] <- NA
    q2[lng <= min.bin[1]] <- NA
    q3 <- tapply(obs, pred.cut, quantile, probs = 0.1)
    q4 <- tapply(obs, pred.cut, quantile, probs = 0.9)
    q3[lng <= min.bin[2]] <- NA
    q4[lng <= min.bin[2]] <- NA

    results <- data.frame(
      x = as.numeric(levels(pred.cut)),
      lng,
      med,
      q1,
      q2,
      q3,
      q4
    )

    results.cut <- data.frame(
      pred.cut = as.numeric(as.character(pred.cut)),
      obs.cut = obs
    )

    ## range taken by observations
    results.obs <- data.frame(min = min(obs), max = max(obs))
    results <- list(results, results.cut, results.obs)
    results
  }

  lo <- min(mydata[c(mod, obs)])
  hi <- max(mydata[c(mod, obs)])

  all.results <- mydata |>
    group_by(across(type)) |>
    group_nest() |>
    mutate(
      results = map(data, procData),
      .keep = "unused"
    )

  results <- all.results |>
    mutate(first = map(results, 1)) |>
    unnest(first) |>
    dplyr::select(-"results")

  hist.results <- all.results |>
    mutate(second = map(results, 2)) |>
    unnest(second) |>
    dplyr::select(-"results")

  obs.results <- all.results |>
    mutate(third = map(results, 3)) |>
    unnest(third) |>
    dplyr::select(-"results")

  ## proper names of labelling #################################################
  pol.name <- sapply(
    levels(results[[type[1]]]),
    function(x) quickText(x, auto.text)
  )
  strip <- strip.custom(factor.levels = pol.name)

  if (length(type) == 1) {
    strip.left <- FALSE
    if (type == "default") strip <- FALSE
  } else {
    ## two conditioning variables

    pol.name <- sapply(
      levels(results[[type[2]]]),
      function(x) quickText(x, auto.text)
    )
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  ## ###########################################################################

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("x ~ med | ", temp, sep = ""))

  xyplot.args <- list(
    x = myform,
    data = results,
    xlim = c(lo, hi * 1.05),
    ylim = c(lo, hi * 1.05),
    ylab = quickText(ylab, auto.text),
    xlab = quickText(xlab, auto.text),
    as.table = TRUE,
    aspect = 1,
    strip = strip,
    strip.left = strip.left,
    key = list(
      lines = list(
        col = c(col.1, col.2, col.5, ideal.col),
        lwd = c(15, 15, 2, 1)
      ),
      lines.title = 1,
      title = "",
      text = list(
        lab = c(
          "25/75th percentile",
          "10/90th percentile",
          "median",
          "perfect model"
        )
      ),
      space = key.position,
      columns = key.columns
    ),
    par.strip.text = list(cex = 0.8),
    panel = function(x, subscripts, ...) {
      panel.grid(-1, -1, col = "grey95")

      poly.na(
        results$x[subscripts],
        results$q3[subscripts],
        results$x[subscripts],
        results$q4[subscripts],
        myColors = col.2,
        alpha = 1
      )
      poly.na(
        results$x[subscripts],
        results$q1[subscripts],
        results$x[subscripts],
        results$q2[subscripts],
        myColors = col.1,
        alpha = 1
      )

      # draw line of where observations lie
      theSubset <- inner_join(obs.results, results[subscripts[1], ], by = type)

      panel.lines(
        c(theSubset$min, theSubset$max),
        c(
          theSubset$min,
          theSubset$max
        ),
        col = ideal.col,
        lwd = 1.5
      )
      panel.lines(
        results$x[subscripts],
        results$med[subscripts],
        col = col.5,
        lwd = 2
      )
    }
  )

  # reset for extra.args

  xyplot.args <- listUpdate(xyplot.args, extra.args)

  # plot
  scatter <- do.call(xyplot, xyplot.args)

  temp <- paste(type, collapse = "+")
  myform <- formula(paste(" ~ pred.cut | ", temp, sep = ""))
  bins <- seq(floor(lo), ceiling(hi), length = bins)

  pred.cut <- NULL ## avoid R NOTES

  histo <- histogram(
    myform,
    data = hist.results,
    breaks = bins,
    type = "count",
    as.table = TRUE,
    strip = strip,
    strip.left = strip.left,
    col = "black",
    alpha = 0.1,
    border = NA,
    par.strip.text = list(cex = 0.8),
    ylab = "sample size for histograms",
    panel = function(
      x = pred.cut,
      col = "black",
      border = NA,
      alpha = 0.2,
      subscripts,
      ...
    ) {
      ## histogram of observations
      panel.histogram(
        x = hist.results[["obs.cut"]][subscripts],
        col = NA,
        alpha = 0.5,
        lwd = 0.5,
        border = ideal.col,
        ...
      )
      ## histogram of modelled values
      panel.histogram(x = x, col = "black", border, alpha = 0.15, ...)
    }
  )

  ## supress scaling warnings
  thePlot <- latticeExtra::doubleYScale(scatter, histo, add.ylab2 = TRUE)
  thePlot <- update(
    thePlot,
    par.settings = simpleTheme(col = c("black", "black"))
  )

  if (length(type) <= 1) {
    plot(thePlot)
  } else {
    plot(latticeExtra::useOuterStrips(
      thePlot,
      strip = strip,
      strip.left = strip.left
    ))
  }

  invisible(trellis.last.object())
  output <- list(
    plot = thePlot,
    data = all.results,
    call = match.call()
  )
  class(output) <- "openair"
  invisible(output)
}
