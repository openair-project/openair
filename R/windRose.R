#' Traditional wind rose plot
#'
#' The traditional wind rose plot that plots wind speed and wind direction by
#' different intervals. The pollution rose applies the same plot structure but
#' substitutes other measurements, most commonly a pollutant time series, for
#' wind speed.
#'
#' For `windRose` data are summarised by direction, typically by 45 or 30 (or
#' 10) degrees and by different wind speed categories. Typically, wind speeds
#' are represented by different width "paddles". The plots show the proportion
#' (here represented as a percentage) of time that the wind is from a certain
#' angle and wind speed range.
#'
#' By default `windRose` will plot a windRose in using "paddle" style segments
#' and placing the scale key below the plot.
#'
#' The argument `pollutant` uses the same plotting structure but substitutes
#' another data series, defined by `pollutant`, for wind speed. It is
#' recommended to use [pollutionRose()] for plotting pollutant concentrations.
#'
#' The option `statistic = "prop.mean"` provides a measure of the relative
#' contribution of each bin to the panel mean, and is intended for use with
#' `pollutionRose`.
#'
#' @inheritParams shared_openair_params
#'
#' @param mydata A data frame containing fields `ws` and `wd`
#'
#' @param ws Name of the column representing wind speed.
#'
#' @param wd Name of the column representing wind direction.
#'
#' @param ws2,wd2 The user can supply a second set of wind speed and wind
#'   direction values with which the first can be compared. See
#'   [pollutionRose()] for more details.
#'
#' @param ws.int The Wind speed interval. Default is 2 m/s but for low met masts
#'   with low mean wind speeds a value of 1 or 0.5 m/s may be better.
#'
#' @param angle Default angle of \dQuote{spokes} is 30. Other potentially useful
#'   angles are 45 and 10. Note that the width of the wind speed interval may
#'   need adjusting using `width`.
#'
#' @param calm.thresh By default, conditions are considered to be calm when the
#'   wind speed is zero. The user can set a different threshold for calms be
#'   setting `calm.thresh` to a higher value. For example, `calm.thresh = 0.5`
#'   will identify wind speeds **below** 0.5 as calm.
#'
#' @param bias.corr When `angle` does not divide exactly into 360 a bias is
#'   introduced in the frequencies when the wind direction is already supplied
#'   rounded to the nearest 10 degrees, as is often the case. For example, if
#'   `angle = 22.5`, N, E, S, W will include 3 wind sectors and all other angles
#'   will be two. A bias correction can made to correct for this problem. A
#'   simple method according to Applequist (2012) is used to adjust the
#'   frequencies.
#'
#' @param grid.line Grid line interval to use. If `NULL`, as in default, this is
#'   assigned based on the available data range. However, it can also be forced
#'   to a specific value, e.g. `grid.line = 10`. `grid.line` can also be a list
#'   to control the interval, line type and colour. For example `grid.line =
#'   list(value = 10, lty = 5, col = "purple")`.
#'
#' @param width For paddle = TRUE, the adjustment factor for width of wind speed
#'   intervals. For example, width = 1.5 will make the paddle width 1.5 times
#'   wider.
#'
#' @param seg `seg` determines with width of the segments. For example, `seg =
#'   0.5` will produce segments 0.5 * `angle`.
#'
#' @param paddle Either `TRUE` or `FALSE`. If `TRUE` plots rose using 'paddle'
#'   style spokes. If `FALSE` plots rose using 'wedge' style spokes.
#'
#' @param breaks Most commonly, the number of break points for wind speed. With
#'   the `ws.int` default of 2 m/s, the `breaks` default, 4, generates the break
#'   points 2, 4, 6, 8 m/s. However, `breaks` can also be used to set specific
#'   break points. For example, the argument `breaks = c(0, 1, 10, 100)` breaks
#'   the data into segments <1, 1-10, 10-100, >100.
#'
#' @param normalise If `TRUE` each wind direction segment is normalised to equal
#'   one. This is useful for showing how the concentrations (or other
#'   parameters) contribute to each wind sector when the proportion of time the
#'   wind is from that direction is low. A line showing the probability that the
#'   wind directions is from a particular wind sector is also shown.
#'
#' @param max.freq Controls the scaling used by setting the maximum value for
#'   the radial limits. This is useful to ensure several plots use the same
#'   radial limits.
#'
#' @param dig.lab The number of significant figures at which scientific number
#'   formatting is used in break point and key labelling. Default 5.
#'
#' @param include.lowest Logical. If `FALSE` (the default), the first interval
#'   will be left exclusive and right inclusive. If `TRUE`, the first interval
#'   will be left and right inclusive. Passed to the `include.lowest` argument
#'   of [cut()].
#'
#' @param statistic The `statistic` to be applied to each data bin in the plot.
#'   Options currently include \dQuote{prop.count}, \dQuote{prop.mean} and
#'   \dQuote{abs.count}. The default \dQuote{prop.count} sizes bins according to
#'   the proportion of the frequency of measurements.  Similarly,
#'   \dQuote{prop.mean} sizes bins according to their relative contribution to
#'   the mean. \dQuote{abs.count} provides the absolute count of measurements in
#'   each bin.
#'
#' @param pollutant Alternative data series to be sampled instead of wind speed.
#'   The [windRose()] default NULL is equivalent to `pollutant = "ws"`. Use in
#'   [pollutionRose()].
#'
#' @param annotate If `TRUE` then the percentage calm and mean values are
#'   printed in each panel together with a description of the statistic below
#'   the plot. If `FALSE` then only the statistic will be printed.
#'
#' @param border Border colour for shaded areas. Default is no border.
#'
#' @export
#' @return an [openair][openair-package] object. Summarised proportions can be
#'   extracted directly using the `$data` operator, e.g. `object$data` for
#'   `output <- windRose(mydata)`. This returns a data frame with three set
#'   columns: `cond`, conditioning based on `type`; `wd`, the wind direction;
#'   and `calm`, the `statistic` for the proportion of data unattributed to any
#'   specific wind direction because it was collected under calm conditions; and
#'   then several (one for each range binned for the plot) columns giving
#'   proportions of measurements associated with each `ws` or `pollutant` range
#'   plotted as a discrete panel.
#' @author David Carslaw
#' @author Karl Ropkins
#' @author Jack Davison
#' @family polar directional analysis functions
#'
#' @references Applequist, S, 2012: Wind Rose Bias Correction. J. Appl. Meteor.
#'   Climatol., 51, 1305-1309.
#'
#'   Droppo,  J.G. and B.A. Napier (2008) Wind Direction Bias in Generating Wind
#'   Roses and Conducting Sector-Based Air Dispersion Modeling, Journal of the
#'   Air & Waste Management Association, 58:7, 913-918.
#'
#' @examples
#' # basic plot
#' windRose(mydata)
#'
#' # one windRose for each year
#' windRose(mydata, type = "year")
#'
#' # windRose in 10 degree intervals with gridlines and width adjusted
#' \dontrun{
#' windRose(mydata, angle = 10, width = 0.2, grid.line = 1)
#' }
windRose <- function(
  mydata,
  ws = "ws",
  wd = "wd",
  ws2 = NA,
  wd2 = NA,
  ws.int = 2,
  angle = 30,
  type = "default",
  calm.thresh = 0,
  bias.corr = TRUE,
  cols = "default",
  grid.line = NULL,
  width = 0.9,
  seg = 0.9,
  auto.text = TRUE,
  breaks = 4,
  offset = 10,
  normalise = FALSE,
  max.freq = NULL,
  paddle = TRUE,
  key.title = "(m/s)",
  key.position = "bottom",
  strip.position = "top",
  dig.lab = 5,
  include.lowest = FALSE,
  statistic = "prop.count",
  pollutant = NULL,
  annotate = TRUE,
  angle.scale = 315,
  border = NA,
  plot = TRUE,
  key = NULL,
  ...
) {
  # check key.position
  key.position <- check_key_position(key.position, key)

  # greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    calm.col <- "black"
  } else {
    calm.col <- "forestgreen"
  }

  # make sure ws and wd and numeric
  mydata <- check_numeric(mydata, vars = c(ws, wd))

  if (!is.na(ws2) && !is.na(wd2) && missing(angle)) {
    angle <- 10
  }

  # check angle is sensible and warn about potential bias
  if (360 / angle != round(360 / angle)) {
    cli::cli_warn(
      c(
        "!" = "{.arg angle} will produce some spoke overlap.",
        "i" = "It is suggested to use an {.arg angle} which divides cleanly into 360, e.g., {seq(5, 60)[360 %% seq(5, 60) == 0]}."
      )
    )
  }
  angle[angle < 3] <- 3

  # extra.args args setup
  extra.args <- capture_dots(...)

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab, auto.text)
  extra.args$ylab <- quickText(extra.args$ylab, auto.text)
  extra.args$title <- quickText(extra.args$title, auto.text)
  extra.args$subtitle <- quickText(extra.args$subtitle, auto.text)

  # need separate handling to be overwritten
  if ("caption" %in% names(extra.args)) {
    extra.args$caption <- quickText(extra.args$caption %||% NULL, auto.text)
  }

  # preset statitistics
  if (is.character(statistic)) {
    # allowed cases
    ok.stat <- c("prop.count", "prop.mean", "abs.count", "frequency")
    rlang::arg_match(statistic, ok.stat)

    if (statistic == "prop.count") {
      stat.fun <- length
      stat.unit <- "%"
      stat.scale <- "all"
      stat.lab <- "Frequency of counts by wind direction (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), digits = dig.lab)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }

    if (statistic == "prop.mean") {
      stat.fun <- function(x) sum(x, na.rm = TRUE)
      stat.unit <- "%"
      stat.scale <- "panel"
      stat.lab <- "Proportion contribution to the mean (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), digits = 5)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }

    if (statistic == "abs.count" || statistic == "frequency") {
      stat.fun <- length
      stat.unit <- ""
      stat.scale <- "none"
      stat.lab <- "Count by wind direction"
      stat.fun2 <- function(x) round(length(x), 0)
      stat.lab2 <- "count"
      stat.labcalm <- function(x) round(x, 0)
    }
  }

  if (is.list(statistic)) {
    # IN DEVELOPMENT
    # this section has no testing/protection
    # but allows users to supply a function
    # scale it by total data or panel
    # convert proportions to percentage
    # label it

    stat.fun <- statistic$fun
    stat.unit <- statistic$unit
    stat.scale <- statistic$scale
    stat.lab <- statistic$lab
    stat.fun2 <- statistic$fun2
    stat.lab2 <- statistic$lab2
    stat.labcalm <- statistic$labcalm
  }

  # variables we need
  vars <- c(wd, ws)

  diff <- FALSE # i.e. not two sets of ws/wd
  rm.neg <- TRUE # will remove negative ws in check.prep

  # case where two met data sets are to be compared
  if (!is.na(ws2) && !is.na(wd2)) {
    vars <- c(vars, ws2, wd2)
    diff <- TRUE
    rm.neg <- FALSE

    # Speed bias (test - reference) and direction bias normalised to [-180, 180]
    mydata$.ws_bias <- mydata[[ws2]] - mydata[[ws]]
    mydata$.wd_bias <- ((mydata[[wd2]] - mydata[[wd]]) + 180) %% 360 - 180

    # Reference wd/ws retained for binning — do not overwrite them
    vars <- c(wd, ws, ".ws_bias", ".wd_bias")

    if (missing(cols)) {
      cols <- c("steelblue3", "white", "tomato2")
    }
    seg <- 1
  }

  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }

  if (!is.null(pollutant)) {
    vars <- c(vars, pollutant)
  }

  mydata <- cutData(mydata, type, ...)

  mydata <- checkPrep(
    mydata,
    vars,
    type,
    remove.calm = FALSE,
    remove.neg = rm.neg
  )

  # original data to use later
  mydata_orig <- mydata

  # remove lines where ws is missing
  # wd can be NA and ws 0 (calm)
  id <- which(is.na(mydata[[ws]]))

  if (length(id) > 0) {
    mydata <- mydata[-id, ]
  }

  if (is.null(pollutant)) {
    pollutant <- ws
  }

  mydata$x <- mydata[[pollutant]]

  mydata[[wd]] <- angle * ceiling(mydata[[wd]] / angle - 0.5)
  mydata[[wd]][mydata[[wd]] == 0] <- 360

  # flag calms as negatives
  if (calm.thresh == 0) {
    mydata[[wd]][mydata[, ws] == 0] <- -999 # set wd to flag where there are calms
  } else {
    mydata[[wd]][mydata[, ws] < calm.thresh] <- -999 # Note < not <=
  }

  mydata[[wd]][mydata[, ws] < calm.thresh] <- -999 # set wd to flag where there are calms
  # do after rounding or -999 changes

  # COMPARISON PLOT BRANCH (early return — must be before break setup)
  if (diff) {
    group_vars <- if (all(type == "default")) character(0) else type

    # Per-direction mean ws bias (calms excluded)
    diff_results <- mydata |>
      dplyr::filter(.data[[wd]] != -999, !is.na(.data$.ws_bias)) |>
      dplyr::summarise(
        mean_ws_bias = mean(.data$.ws_bias, na.rm = TRUE),
        n = dplyr::n(),
        .by = c(wd, dplyr::any_of(group_vars))
      )

    # Panel-level circular mean of wd bias and overall ws bias (for arrow/annotation)
    wd_bias_panel <- mydata |>
      dplyr::filter(!is.na(.data$.wd_bias)) |>
      dplyr::summarise(
        u = mean(sin(.data$.wd_bias * pi / 180), na.rm = TRUE),
        v = mean(cos(.data$.wd_bias * pi / 180), na.rm = TRUE),
        mean_ws_bias = mean(.data$.ws_bias, na.rm = TRUE),
        .by = dplyr::any_of(group_vars)
      ) |>
      dplyr::mutate(
        panel_wd_bias = (atan2(.data$u, .data$v) * 180 / pi) %% 360
      )

    # Radial scale: zero ring at r0, bars extend +/- max_abs_bias from it
    max_abs_bias <- max(abs(diff_results$mean_ws_bias), na.rm = TRUE)
    if (!is.finite(max_abs_bias) || max_abs_bias == 0) {
      max_abs_bias <- 1
    }
    if (!is.null(max.freq)) {
      max_abs_bias <- max.freq / 2
    }

    r0 <- max_abs_bias * 1.5
    total_r <- r0 + max_abs_bias * 1.2

    # Grid breaks centred on the zero ring
    bias_pretty <- pretty(c(-max_abs_bias * 1.1, max_abs_bias * 1.1))
    valid_breaks <- (r0 + bias_pretty) >= 0 & (r0 + bias_pretty) <= total_r
    grid_breaks_r <- r0 + bias_pretty[valid_breaks]
    grid_labels_v <- bias_pretty[valid_breaks]

    # Grid line appearance
    if (is.list(grid.line)) {
      grid.lty <- grid.line[["lty"]] %||% 1
      grid.col <- grid.line[["col"]] %||% "grey85"
    } else {
      grid.lty <- 1
      grid.col <- "grey85"
    }

    # Diverging colour scale (expect cols to be length-3 low/mid/high)
    diff_colors <- if (length(cols) >= 3) {
      cols[c(1, ceiling(length(cols) / 2), length(cols))]
    } else {
      c("steelblue3", "white", "tomato2")
    }

    fill_guide <- if (key.position == "none") {
      "none"
    } else {
      ggplot2::guide_colorbar(
        theme = ggplot2::theme(
          legend.title.position = ifelse(
            key.position %in% c("left", "right"),
            "top",
            key.position
          ),
          legend.text.position = key.position
        )
      )
    }

    if (missing(key.title)) {
      key.title <- "ws bias\n(m/s)"
    }

    thePlot <-
      ggplot2::ggplot(diff_results) +
      theme_openair_radial(key.position) +
      set_extra_fontsize(extra.args) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(
          colour = grid.col,
          linetype = grid.lty,
          linewidth = 0.25
        )
      ) +
      # Diverging bars from zero ring
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = .data[[wd]] - angle / 2 * seg,
          xmax = .data[[wd]] + angle / 2 * seg,
          ymin = pmin(r0, r0 + .data$mean_ws_bias),
          ymax = pmax(r0, r0 + .data$mean_ws_bias),
          fill = .data$mean_ws_bias
        )
      ) +
      # Zero-bias reference ring
      ggplot2::geom_hline(yintercept = r0, colour = "grey30", linewidth = 0.4) +
      # Overall wd bias arrow (centre to zero ring)
      ggplot2::geom_segment(
        data = wd_bias_panel,
        ggplot2::aes(
          x = .data$panel_wd_bias,
          xend = .data$panel_wd_bias,
          y = 0,
          yend = r0
        ),
        arrow = ggplot2::arrow(
          type = "closed",
          length = ggplot2::unit(0.12, "inches")
        ),
        colour = "grey20",
        linewidth = 0.8,
        inherit.aes = FALSE
      ) +
      ggplot2::ggproto(
        NULL,
        ggplot2::coord_radial(
          inner.radius = offset / 100,
          r.axis.inside = angle.scale,
          rlim = c(0, total_r),
          clip = "off"
        ),
        inner_radius = c(offset / 100, 1) * 0.475
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, total_r),
        breaks = grid_breaks_r,
        labels = paste0(grid_labels_v, " m/s"),
        expand = ggplot2::expansion(0, 0)
      ) +
      scale_x_compass() +
      ggplot2::scale_fill_gradient2(
        low = diff_colors[1],
        mid = diff_colors[2],
        high = diff_colors[3],
        midpoint = 0,
        name = quickText(
          key.title,
          auto.text = auto.text
        )
      ) +
      ggplot2::labs(
        x = extra.args$xlab,
        y = extra.args$ylab,
        title = extra.args$title,
        subtitle = extra.args$subtitle,
        caption = extra.args$caption %||%
          if (annotate) {
            "Mean wind speed bias (test \u2212 reference) by reference direction"
          }
      ) +
      ggplot2::guides(fill = fill_guide) +
      get_facet(
        type,
        extra.args,
        scales = "fixed",
        auto.text = auto.text,
        strip.position = strip.position,
        drop = FALSE,
        wd.res = extra.args$wd.res %||% 8
      )

    if (annotate) {
      thePlot <- thePlot +
        ggplot2::geom_text(
          data = wd_bias_panel,
          ggplot2::aes(
            label = paste0(
              "ws bias = ",
              round(.data$mean_ws_bias, 2),
              " m/s\n",
              "wd bias = ",
              wrap_wd_label(.data$panel_wd_bias),
              "\u00b0"
            )
          ),
          x = I(1),
          y = I(0),
          check_overlap = TRUE,
          size = 3,
          hjust = 1,
          vjust = 0,
          colour = calm.col,
          inherit.aes = FALSE
        )
    }

    if (key.position %in% c("top", "bottom")) {
      thePlot <- thePlot +
        ggplot2::theme(legend.key.height = ggplot2::rel(0.5))
    }

    if (annotate) {
      thePlot <- thePlot +
        annotate_compass_points(
          size = if (is.null(extra.args$fontsize)) {
            3
          } else {
            extra.args$fontsize / 3
          }
        )
    }

    if (plot) {
      plot(thePlot)
    }

    output <- list(plot = thePlot, data = diff_results, call = match.call())
    class(output) <- "openair"
    return(invisible(output))
  }

  if (length(breaks) == 1) {
    breaks <- 0:(breaks - 1) * ws.int
  }

  if (max(breaks) < max(mydata$x, na.rm = TRUE)) {
    breaks <- c(breaks, max(mydata$x, na.rm = TRUE))
  }

  if (min(breaks) > min(mydata$x, na.rm = TRUE)) {
    cli::cli_warn("Some values are below minimum break.")
  }

  breaks <- unique(breaks)
  interval_labels <- get_labels_from_breaks(breaks, sep = " to ")
  mydata$x <- cut(
    mydata$x,
    breaks = breaks,
    labels = interval_labels,
    include.lowest = include.lowest,
    dig.lab = dig.lab
  )

  # Build tidy summary: one row per (wd x interval) combination per panel
  prepare_rose_data <- function(mydata) {
    if (all(is.na(mydata$x))) {
      return(dplyr::tibble(
        wd = NA_real_,
        interval = factor(NA_character_, levels = interval_labels),
        value = NA_real_,
        freqs = NA_integer_,
        calm = 100,
        panel.fun = NA_character_,
        mean.wd = NA_real_
      ))
    }

    all_n <- stat.fun(mydata[[wd]])
    calm_n <- stat.fun(mydata[mydata[[wd]] == -999, ][[pollutant]])

    # pre-compute to avoid data-masking ambiguity inside complete()
    wd_vals <- sort(unique(mydata[[wd]][mydata[[wd]] != -999]))

    result <- mydata |>
      dplyr::filter(.data[[wd]] != -999) |>
      dplyr::rename(wd = dplyr::all_of(wd)) |>
      dplyr::summarise(
        value = stat.fun(.data[[pollutant]]),
        .by = c("wd", "x")
      ) |>
      # ensure all combinations of wd and interval are present, filling missing
      # with 0
      tidyr::complete(
        .data[["x"]],
        wd = wd_vals,
        fill = list(value = 0)
      ) |>
      dplyr::rename(interval = "x") |>
      # sometimes interval is NA (e.g., if breaks don't cover all of data - set
      # value to 0 if so)
      dplyr::mutate(
        value = dplyr::replace_when(.data$value, is.na(.data$interval) ~ 0)
      )

    if (stat.scale == "all") {
      calm_n <- calm_n / all_n
      result <- dplyr::mutate(result, value = .data$value / all_n)
    } else if (stat.scale == "panel") {
      total <- stat.fun(result$value) + calm_n
      calm_n <- calm_n / total
      result <- dplyr::mutate(result, value = .data$value / total)
    }

    if (stat.scale %in% c("all", "panel")) {
      result <- dplyr::mutate(result, value = .data$value * 100)
      calm_n <- calm_n * 100
    }

    # Count per direction (retained for output compatibility)
    freqs_df <- mydata |>
      dplyr::filter(.data[[wd]] != -999) |>
      dplyr::rename(wd = dplyr::all_of(wd)) |>
      dplyr::count(.data$wd, name = "freqs")

    # Circular mean wind direction
    u <- mean(sin(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
    v <- mean(cos(2 * pi * mydata[[wd]] / 360), na.rm = TRUE)
    mean.wd <- atan2(u, v) * 360 / 2 / pi
    if (!is.na(mean.wd)) {
      if (mean.wd < 0) {
        mean.wd <- mean.wd + 360
      }
      if (mean.wd > 180) mean.wd <- mean.wd - 360
    }

    result |>
      dplyr::left_join(freqs_df, by = "wd") |>
      dplyr::mutate(
        calm = calm_n,
        panel.fun = stat.fun2(mydata[[pollutant]]),
        mean.wd = mean.wd
      )
  }

  # Build tidy results for each type panel
  results <- map_type(
    mydata,
    fun = prepare_rose_data,
    type = type,
    .include_default = TRUE
  )

  # format panel-level summaries
  results$calm <- stat.labcalm(results$calm)
  results$mean.wd <- stat.labcalm(results$mean.wd)

  # Bias correction: adjust frequencies when wd data is rounded to nearest 10 degrees
  apply_bias_correction <- function(results) {
    wd_select <- dplyr::inner_join(
      mydata_orig,
      dplyr::distinct(results[type]),
      by = type
    )
    if (!all(round(wd_select[[wd]]) %% 10 == 0, na.rm = TRUE)) {
      return(results)
    }

    bins <- angle * ceiling(seq(10, 360, 10) / angle - 0.5)
    bins[bins == 0] <- 360
    counts <- table(bins)

    correction <- dplyr::tibble(
      wd = as.numeric(names(counts)),
      .cf = as.numeric(mean(counts) / counts)
    )

    dplyr::left_join(results, correction, by = "wd") |>
      dplyr::mutate(value = .data$value * dplyr::coalesce(.data$.cf, 1)) |>
      dplyr::select(-".cf")
  }

  if (bias.corr) {
    results <- map_type(
      results,
      type = type,
      fun = apply_bias_correction,
      .include_default = TRUE
    )
  }

  if (normalise) {
    results <- results |>
      dplyr::mutate(
        .total = sum(.data$value),
        .by = dplyr::all_of(c("wd", type))
      ) |>
      dplyr::mutate(
        freq = .data$.total / sum(.data$.total),
        .by = dplyr::all_of(type)
      ) |>
      dplyr::mutate(
        norm = .data$freq / max(.data$freq, na.rm = TRUE),
        value = dplyr::if_else(.data$.total > 0, .data$value / .data$.total, 0)
      ) |>
      dplyr::select(-".total")

    stat.lab <- "Normalised by wind sector"
    stat.unit <- ""
    seg <- 1
  } else {
    results <- dplyr::mutate(results, norm = NA_real_)
  }

  max.freq <- max.freq %||%
    {
      results |>
        dplyr::summarise(
          total = sum(.data$value, na.rm = TRUE),
          .by = dplyr::all_of(c("wd", type))
        ) |>
        dplyr::pull(.data$total) |>
        max(na.rm = TRUE)
    }

  # check to see if grid.line is a list or not and set grid line properties
  if (is.list(grid.line)) {
    grid.value <- grid.line[["value"]]
    grid.lty <- grid.line[["lty"]] %||% 1
    grid.col <- grid.line[["col"]] %||% "grey85"
  } else {
    grid.value <- grid.line
    grid.lty <- 1
    grid.col <- "grey85"
  }

  # results is already tidy (long); add cumulative columns for paddle rendering
  plot_data <- results |>
    dplyr::mutate(
      cum_value = cumsum(.data$value),
      lag_value = dplyr::lag(.data$cum_value, default = 0),
      norm2 = dplyr::if_else(
        dplyr::row_number() == 1L,
        dplyr::coalesce(.data$norm, 0),
        0
      ),
      .by = dplyr::all_of(c("wd", type))
    )

  key.title <- check_key_header(key.title, extra.args)
  key_label <- quickText(key.title, auto.text = auto.text)
  key_guide <- ggplot2::guide_legend(
    reverse = key.position %in% c("left", "right"),
    theme = ggplot2::theme(
      legend.title.position = ifelse(
        key.position %in% c("left", "right"),
        "top",
        key.position
      ),
      legend.text.position = key.position
    ),
    nrow = if (key.position %in% c("left", "right")) NULL else 1
  )

  thePlot <-
    plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$wd,
        y = .data$value
      )
    ) +
    theme_openair_radial(key.position, panel.ontop = normalise) +
    set_extra_fontsize(extra.args) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        colour = grid.col,
        linetype = grid.lty,
        linewidth = 0.25
      )
    ) +
    ggplot2::ggproto(
      NULL,
      ggplot2::coord_radial(
        inner.radius = offset / 100,
        r.axis.inside = angle.scale,
        rlim = c(0, max.freq),
        clip = "off"
      ),
      inner_radius = c(offset / 100, 1) * 0.475
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(if (normalise) c(0, 0) else c(0, 0.1)),
      breaks = if (is.null(grid.value)) {
        scales::pretty_breaks()
      } else {
        seq(0, max.freq, grid.value)
      },
      labels = \(x) paste0(x, stat.unit),
      limits = c(0, max.freq)
    ) +
    scale_x_compass() +
    ggplot2::scale_fill_manual(
      values = openColours(scheme = cols, n = length(interval_labels)),
      aesthetics = c("colour", "fill"),
      na.translate = FALSE
    ) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      title = extra.args$title,
      subtitle = extra.args$subtitle,
      caption = extra.args$caption %||% if (annotate) stat.lab
    ) +
    ggplot2::guides(
      fill = key_guide,
      color = key_guide,
      linewidth = key_guide
    ) +
    get_facet(
      type,
      extra.args,
      scales = "fixed",
      auto.text = auto.text,
      strip.position = strip.position,
      drop = FALSE,
      wd.res = extra.args$wd.res %||% 8
    )

  if (normalise) {
    thePlot <- thePlot +
      ggplot2::geom_col(
        ggplot2::aes(fill = .data$interval),
        position = ggplot2::position_stack(reverse = TRUE),
        width = seg * angle,
        color = border
      ) +
      ggplot2::geom_col(
        data = dplyr::filter(plot_data, .data$norm2 != 0),
        ggplot2::aes(y = .data$norm2),
        color = "black",
        fill = NA,
        position = ggplot2::position_stack(reverse = TRUE),
        width = seg * angle
      ) +
      ggplot2::labs(
        fill = key_label
      )
  } else {
    if (paddle) {
      # widths of paddles similar to original behaviour
      box.widths <- seq(
        0.002^0.25,
        0.016^0.25,
        length.out = length(interval_labels)
      )^4
      thePlot <-
        thePlot +
        geom_stroked_path(
          data = plot_data |>
            dplyr::mutate(id = dplyr::row_number()) |>
            tidyr::pivot_longer(
              cols = c("cum_value", "lag_value"),
              names_to = "__name__",
              values_to = "cum_y"
            ),
          ggplot2::aes(
            x = .data$wd,
            y = .data$cum_y,
            group = .data$id,
            color = .data$interval,
            linewidth = .data$interval
          ),
          stroke_colour = border
        ) +
        ggplot2::scale_linewidth_manual(
          values = 5 * width * box.widths
        ) +
        ggplot2::labs(
          linewidth = key_label,
          color = key_label
        )
    } else {
      thePlot <-
        thePlot +
        ggplot2::geom_col(
          ggplot2::aes(fill = .data$interval),
          position = ggplot2::position_stack(reverse = TRUE),
          width = seg * angle,
          color = border
        ) +
        ggplot2::labs(
          fill = key_label
        )
    }
  }

  if (annotate) {
    thePlot <-
      thePlot +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(
            stat.lab2,
            " = ",
            .data$panel.fun,
            "\n",
            "calm = ",
            .data$calm,
            stat.unit
          )
        ),
        x = I(1),
        y = I(0),
        check_overlap = TRUE,
        size = 3,
        hjust = 1,
        vjust = 0,
        color = calm.col
      )
  }

  # make key full width/height
  if (key.position %in% c("top", "bottom")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.width = ggplot2::rel(2)
      )
  }

  # add compass points
  if (annotate) {
    thePlot <- thePlot +
      annotate_compass_points(
        size = if (is.null(extra.args$fontsize)) 3 else extra.args$fontsize / 3
      )
  }

  # output
  if (plot) {
    plot(thePlot)
  }

  # Pivot back to wide format for output compatibility (Interval1, Interval2, ...)
  newdata <- results |>
    dplyr::filter(!is.na(.data$interval)) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(type)),
      .data$wd,
      match(as.character(.data$interval), interval_labels)
    ) |>
    dplyr::mutate(
      .iname = paste0(
        "Interval",
        match(as.character(.data$interval), interval_labels)
      ),
      cum_val = cumsum(.data$value),
      .by = dplyr::all_of(c("wd", type))
    ) |>
    dplyr::select(-"interval", -"value") |>
    tidyr::pivot_wider(names_from = ".iname", values_from = "cum_val")
  attr(newdata, "intervals") <- interval_labels

  # output
  output <- list(plot = thePlot, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}

#' Pollution rose variation of the traditional wind rose plot
#'
#' The traditional wind rose plot that plots wind speed and wind direction by
#' different intervals. The pollution rose applies the same plot structure but
#' substitutes other measurements, most commonly a pollutant time series, for
#' wind speed.
#'
#' [pollutionRose()] is a [windRose()] wrapper which brings `pollutant`
#' forward in the argument list, and attempts to sensibly rescale break points
#' based on the `pollutant` data range by by-passing `ws.int`.
#'
#' By default, [pollutionRose()] will plot a pollution rose of `nox` using
#' "wedge" style segments and placing the scale key to the right of the plot.
#'
#' It is possible to compare two wind speed-direction data sets using
#' [pollutionRose()]. There are many reasons for doing so e.g. to see how one
#' site compares with another or for meteorological model evaluation. In this
#' case, `ws` and `wd` are considered to the the reference data sets
#' with which a second set of wind speed and wind directions are to be compared
#' (`ws2` and `wd2`). The first set of values is subtracted from the
#' second and the differences compared. If for example, `wd2` was biased
#' positive compared with `wd` then `pollutionRose` will show the bias
#' in polar coordinates. In its default use, wind direction bias is colour-coded
#' to show negative bias in one colour and positive bias in another.
#'
#' @inheritParams windRose
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`.
#'
#' @param breaks Most commonly, the number of break points for pollutant
#'   concentrations. The default, 6, attempts to breaks the supplied data at
#'   approximately 6 sensible break points. However, `breaks` can also be
#'   used to set specific break points. For example, the argument `breaks =
#'   c(0, 1, 10, 100)` breaks the data into segments <1, 1-10, 10-100, >100.
#'
#' @inheritDotParams windRose -pollutant -key.title -key.position -key -breaks
#'   -seg -normalise -plot -paddle
#' @export
#' @return an [openair][openair-package] object. Summarised proportions can be
#'   extracted directly using the `$data` operator, e.g.
#'   `object$data` for `output <- windRose(mydata)`. This returns a
#'   data frame with three set columns: `cond`, conditioning based on
#'   `type`; `wd`, the wind direction; and `calm`, the
#'   `statistic` for the proportion of data unattributed to any specific
#'   wind direction because it was collected under calm conditions; and then
#'   several (one for each range binned for the plot) columns giving proportions
#'   of measurements associated with each `ws` or `pollutant` range
#'   plotted as a discrete panel.
#' @family polar directional analysis functions
#' @examples
#' # pollutionRose of nox
#' pollutionRose(mydata, pollutant = "nox")
#'
#' # source apportionment plot - contribution to mean
#' \dontrun{
#' pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")
#'
#' # example of comparing 2 met sites
#' # first we will make some new ws/wd data with a postive bias
#' mydata$ws2 <- mydata$ws + 2 * rnorm(nrow(mydata)) + 1
#' mydata$wd2 <- mydata$wd + 30 * rnorm(nrow(mydata)) + 30
#'
#' # need to correct negative wd
#' id <- which(mydata$wd2 < 0)
#' mydata$wd2[id] <- mydata$wd2[id] + 360
#'
#' # results show postive bias in wd and ws
#' pollutionRose(mydata, ws = "ws", wd = "wd", ws2 = "ws2", wd2 = "wd2")
#'
#' ## add some wd bias to some nighttime hours
#' id <- which(as.numeric(format(mydata$date, "%H")) %in% c(23, 1, 2, 3, 4, 5))
#' mydata$wd2[id] <- mydata$wd[id] + 30 * rnorm(length(id)) + 120
#' id <- which(mydata$wd2 < 0)
#' mydata$wd2[id] <- mydata$wd2[id] + 360
#'
#' pollutionRose(
#'   mydata,
#'   ws = "ws",
#'   wd = "wd",
#'   ws2 = "ws2",
#'   wd2 = "wd2",
#'   breaks = c(-11, -2, -1, -0.5, 0.5, 1, 2, 11),
#'   cols = c("dodgerblue4", "white", "firebrick"),
#'   type = "daylight"
#' )
#' }
pollutionRose <- function(
  mydata,
  pollutant = "nox",
  key.title = pollutant,
  key.position = "right",
  breaks = 6,
  paddle = FALSE,
  seg = 0.9,
  normalise = FALSE,
  plot = TRUE,
  key = NULL,
  ...
) {
  # extra.args args setup
  extra.args <- capture_dots(...)

  # check if key.header / key.footer are being used
  key.title <- check_key_header(key.title, extra.args)

  # check to see if two met data sets are being compared.
  # if so, set pollutant to one of the names
  if ("ws2" %in% names(extra.args)) {
    pollutant <- extra.args$ws
    if (missing(breaks)) breaks <- NA
  }

  if (is.null(breaks)) {
    breaks <- 6
  }

  if (is.numeric(breaks) && length(breaks) == 1) {
    # breaks from the minimum to 90th percentile, which generally gives sensible
    # spacing for skewed data. Maximum is added later.
    breaks <- unique(pretty(
      c(
        min(mydata[[pollutant]], na.rm = TRUE),
        stats::quantile(mydata[[pollutant]], probs = 0.9, na.rm = TRUE)
      ),
      breaks
    ))
  }

  windRose(
    mydata,
    pollutant = pollutant,
    seg = seg,
    key.position = key.position,
    key.title = key.title,
    key = key,
    breaks = breaks,
    normalise = normalise,
    paddle = paddle,
    plot = plot,
    ...
  )
}

# function to ensure that > 180
wrap_wd_label <- function(x) {
  x <- ifelse(x > 180, x - 360, x)
  x <- round(x, 1)
  ifelse(sign(x) != -1, paste0("+", x), as.character(x))
}
