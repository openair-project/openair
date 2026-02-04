#' Function to plot percentiles by wind direction
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   [plot_polar_percentile()] plots percentiles by wind direction with flexible
#'   conditioning. The plot can display any number of percentile lines, though
#'   useful defaults have been chosen. It is most useful for showing the
#'   distribution of concentrations by wind direction and often can reveal
#'   different sources, e.g., those that only affect high percentile
#'   concentrations such as a chimney stack.
#'
#' @inheritParams shared_ggplot_params
#'
#' @param pollutant One or more pollutants in `data` to calculate percentiles
#'   for. Multiple pollutants limit the number of allowed `type`s to one.
#'
#' @param percentile Any number of percentiles (`0` to `100`) to calculate.
#'
#' @inheritSection shared_ggplot_params Controlling scales
#' @inheritSection shared_ggplot_params Conditioning with `type`
#'
#' @seealso the legacy [percentileRose()] function
#' @family ggplot2 directional analysis functions
#' @author Jack Davison
#'
#' @export
plot_polar_percentile <- function(
  data,
  pollutant,
  type = NULL,
  percentile = c(25, 50, 75, 90, 95),
  wd_angle = 10,
  r_axis_inside = 315,
  inner_radius = 0.1,
  scale_y = openair::scale_opts(),
  cols = "turbo",
  auto_text = TRUE,
  facet_opts = openair::facet_opts(),
  plot = TRUE,
  ...
) {
  # scales
  scale_y <- resolve_scale_opts(scale_y)

  # make sure percentiles are in order
  percentile <- sort(percentile)

  # check arguments
  if (360 %% wd_angle != 0) {
    cli::cli_abort(
      "360 must be divisible by {.arg wd_angle}; e.g., {(10:45)[360 %% 10:45 == 0]}."
    )
  }

  # check type length & pollutant
  if (length(pollutant) > 1 && !is.null(type) && length(type) > 1) {
    cli::cli_abort(
      "If more than one {.arg pollutant} is defined, only one {.arg type} can be given."
    )
  }

  # always reshape data
  data <-
    tidyr::pivot_longer(
      data,
      cols = dplyr::all_of(pollutant),
      names_to = "pollutant_name",
      values_to = "pollutant_value"
    )

  # check input
  vars <- c("wd", "pollutant_name", "pollutant_value")
  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }

  # check and cut data
  data <- checkPrep(data, vars, type = type, remove.calm = TRUE)
  data <- cutData(data, type)

  # ammend type if multiple pollutants
  if (length(pollutant) > 1) {
    type <- c("pollutant_name", type)
  }

  # round wd to nearest wd_angle
  data$wd <- floor(data$wd / wd_angle) * wd_angle
  data$wd[data$wd == 360] <- 0

  # create all combinations of wd & type
  all_combos <-
    dplyr::tibble(wd = seq(0, 360 - wd_angle, wd_angle)) |>
    tidyr::crossing(
      data |>
        dplyr::select(dplyr::all_of(type)) |>
        dplyr::distinct()
    )

  # pad data with all wind directions
  data <-
    dplyr::full_join(
      all_combos,
      data,
      by = c(type, "wd")
    ) |>
    dplyr::arrange(.data$wd)

  # get quantile values
  plotdata <-
    data |>
    dplyr::reframe(
      value = quantile(
        .data$pollutant_value,
        percentile / 100,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(c(type, "wd"))
    ) |>
    dplyr::mutate(
      p = percentile,
      .by = dplyr::all_of(c(type, "wd"))
    ) |>
    dplyr::filter(!is.na(.data$wd))

  # fill missing values - useful w/ missing wds
  plotdata <-
    mapType(
      plotdata,
      c(type, "p"),
      \(df) tidyr::fill(df, dplyr::all_of("value"), .direction = "down")
    )

  # ensure 360 is represented to complete the circle
  plotdata <-
    mapType(
      plotdata,
      type,
      \(df) {
        dplyr::bind_rows(
          df,
          df |>
            dplyr::filter(.data$wd == 0) |>
            dplyr::mutate(wd = 360)
        ) |>
          dplyr::arrange(.data$wd)
      }
    )

  # ensure percentile is a factor
  plotdata$p <- factor(plotdata$p, levels = percentile)

  # create plot
  plt <-
    ggplot2::ggplot(
      plotdata,
      ggplot2::aes(x = .data$wd, y = .data$value)
    ) +
    ggplot2::geom_step(
      ggplot2::aes(color = .data$p),
      direction = "mid",
      linewidth = 1
    ) +
    ggplot2::coord_radial(
      r.axis.inside = r_axis_inside,
      inner.radius = inner_radius
    ) +
    scale_x_compass() +
    ggplot2::scale_y_continuous(
      breaks = scale_y$breaks,
      limits = scale_y$limits,
      labels = scale_y$labels,
      transform = scale_y$transform,
      sec.axis = scale_y$sec.axis,
      position = scale_y$position %||% "left",
      expand = ggplot2::expansion()
    ) +
    ggplot2::scale_color_manual(
      values = openColours(cols, n = length(percentile)),
      label = \(x) label_openair(x, auto_text = auto_text),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = label_openair(
        paste(
          paste(pollutant, collapse = ", "),
          "Percentile"
        ),
        auto_text = auto_text
      )
    ) +
    theme_oa_classic("polar") +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = T)
    ) +
    get_facet_fun(
      type,
      facet_opts = facet_opts,
      auto_text = auto_text
    )

  # return
  if (plot) {
    return(plt)
  } else {
    return(plotdata)
  }
}


#' Function to plot percentiles by wind direction
#'
#' `percentileRose` plots percentiles by wind direction with flexible
#' conditioning. The plot can display multiple percentile lines or filled areas.
#'
#' `percentileRose` calculates percentile levels of a pollutant and plots
#' them by wind direction. One or more percentile levels can be calculated and
#' these are displayed as either filled areas or as lines.
#'
#' The wind directions are rounded to the nearest 10 degrees, consistent with
#' surface data from the UK Met Office before a smooth is fitted. The levels by
#' wind direction are optionally calculated using a cyclic smooth cubic spline
#' using the option `smooth`. If `smooth = FALSE` then the data are
#' shown in 10 degree sectors.
#'
#' The `percentileRose` function compliments other similar functions
#' including [windRose()], [pollutionRose()],
#' [polarFreq()] or [polarPlot()]. It is most useful for
#' showing the distribution of concentrations by wind direction and often can
#' reveal different sources e.g. those that only affect high percentile
#' concentrations such as a chimney stack.
#'
#' Similar to other functions, flexible conditioning is available through the
#' `type` option. It is easy for example to consider multiple percentile
#' values for a pollutant by season, year and so on. See examples below.
#'
#' `percentileRose` also offers great flexibility with the scale used and
#' the user has fine control over both the range, interval and colour.
#'
#' @inheritParams polarPlot
#' @param mydata A data frame minimally containing `wd` and a numeric field
#'   to plot --- `pollutant`.
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`. More than one
#'   pollutant can be supplied e.g. `pollutant = c("no2", "o3")` provided
#'   there is only one `type`.
#' @param percentile The percentile value(s) to plot. Must be between 0--100. If
#'   `percentile = NA` then only a mean line will be shown.
#' @param smooth Should the wind direction data be smoothed using a cyclic
#'   spline?
#' @param method When `method = "default"` the supplied percentiles by wind
#'   direction are calculated. When `method = "cpf"` the conditional
#'   probability function (CPF) is plotted and a single (usually high)
#'   percentile level is supplied. The CPF is defined as CPF = my/ny, where my
#'   is the number of samples in the wind sector y with mixing ratios greater
#'   than the *overall* percentile concentration, and ny is the total
#'   number of samples in the same wind sector (see Ashbaugh et al., 1985).
#' @param angle Default angle of \dQuote{spokes} is when `smooth = FALSE`.
#' @param mean Show the mean by wind direction as a line?
#' @param mean.lty Line type for mean line.
#' @param mean.lwd Line width for mean line.
#' @param mean.col Line colour for mean line.
#' @param fill Should the percentile intervals be filled (default) or should
#'   lines be drawn (`fill = FALSE`).
#' @param intervals User-supplied intervals for the scale e.g. `intervals =
#'   c(0, 10, 30, 50)`
#' @param ... Other graphical parameters are passed onto `cutData` and
#'   `lattice:xyplot`. For example, `percentileRose` passes the option
#'   `hemisphere = "southern"` on to `cutData` to provide southern
#'   (rather than default northern) hemisphere handling of `type =
#'   "season"`. Similarly, common graphical arguments, such as `xlim` and
#'   `ylim` for plotting ranges and `lwd` for line thickness when
#'   using `fill = FALSE`, are passed on `xyplot`, although some local
#'   modifications may be applied by openair. For example, axis and title
#'   labelling options (such as `xlab`, `ylab` and `main`) are
#'   passed to `xyplot` via `quickText` to handle routine formatting.
#' @export
#' @return an [openair][openair-package] object
#' @seealso the newer [plot_polar_percentile()] function
#' @family polar directional analysis functions
#' @author David Carslaw
#' @references Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A residence time
#'   probability analysis of sulfur concentrations at ground canyon national
#'   park. Atmospheric Environment 19 (8), 1263-1270.
#' @examples
#' # basic percentile plot
#' percentileRose(mydata, pollutant = "o3")
#'
#' # 50/95th percentiles of ozone, with different colours
#' percentileRose(mydata, pollutant = "o3", percentile = c(50, 95), col = "brewer1")
#'
#' \dontrun{
#' # percentiles of ozone by year, with different colours
#' percentileRose(mydata, type = "year", pollutant = "o3", col = "brewer1")
#'
#' # percentile concentrations by season and day/nighttime..
#' percentileRose(mydata, type = c("season", "daylight"), pollutant = "o3", col = "brewer1")
#' }
percentileRose <- function(
  mydata,
  pollutant = "nox",
  wd = "wd",
  type = "default",
  percentile = c(25, 50, 75, 90, 95),
  smooth = FALSE,
  method = "default",
  cols = "default",
  angle = 10,
  mean = TRUE,
  mean.lty = 1,
  mean.lwd = 3,
  mean.col = "grey",
  fill = TRUE,
  intervals = NULL,
  angle.scale = 45,
  auto.text = TRUE,
  key.header = NULL,
  key.footer = "percentile",
  key.position = "bottom",
  key = TRUE,
  alpha = 1,
  plot = TRUE,
  ...
) {
  ## get rid of R check annoyances
  sub <- NULL

  ## calculate percetiles or just show mean?
  if (is.na(percentile[1])) {
    mean.only <- TRUE
    percentile <- 0
  } else {
    mean.only <- FALSE
  }

  if (tolower(method) == "cpf") {
    mean <- FALSE
    if (length(percentile) > 1) {
      stop("Only one percentile should be supplied when method = 'CPF'.")
    }
  }

  vars <- c(wd, pollutant)
  if (any(type %in% dateTypes)) {
    vars <- c(vars, "date")
  }

  # check to see if ws is in the data and is calm (need to remove as no wd)
  if ("ws" %in% names(mydata)) {
    id <- which(mydata$ws == 0 & mydata[[wd]] == 0)
    if (length(id) > 0) {
      mydata <- mydata[-id, ]
    }
  }

  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE, wd = wd)

  ## round wd
  mydata[[wd]] <- angle * ceiling(mydata[[wd]] / angle - 0.5)

  # when it generates angle at 0 and 360, make all 360
  if (0 %in% mydata$wd) {
    id <- which(mydata[[wd]] == 0)
    mydata[[wd]][id] <- 360
  }

  ## make sure all wds are present
  ids <- which(!seq(angle, 360, by = angle) %in% unique(mydata[[wd]]))
  if (length(ids) > 0 & smooth != TRUE) {
    extra <- mydata[rep(1, length(ids)), ]
    extra[[wd]] <- seq(angle, 360, by = angle)[ids]
    extra[[pollutant]] <- NA
    mydata <- rbind(mydata, extra)
  }

  ## need lowest value if shading
  if (fill) {
    percentile <- unique(c(0, percentile))
  }

  # number of pollutants
  npol <- length(pollutant)

  ## if more than one pollutant, need to stack the data and set type = "variable"
  ## this case is most relevent for model-measurement compasrions where data are in columns
  ## Can also do more than one pollutant and a single type that is not "default", in which
  ## case pollutant becomes a conditioning variable
  if (length(pollutant) > 1) {
    if (length(type) > 1) {
      warning(paste("Only type = '", type[1], "' will be used", sep = ""))
      type <- type[1]
    }
    ## use pollutants as conditioning variables

    mydata <- gather(mydata, key = variable, value = value, pollutant)
    ## now set pollutant to "value"
    pollutant <- "value"
    if (type == "default") {
      type <- "variable"
    } else {
      type <- c(type, "variable")
    }
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
  extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
    quickText(extra.args$xlab, auto.text)
  } else {
    quickText("", auto.text)
  }

  extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
    quickText(extra.args$ylab, auto.text)
  } else {
    quickText("", auto.text)
  }

  extra.args$main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  } else {
    quickText("", auto.text)
  }

  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }

  ## layout default
  if (!"layout" %in% names(extra.args)) {
    extra.args$layout <- NULL
  }

  ## lwd handling
  if (!"lwd" %in% names(extra.args)) {
    extra.args$lwd <- 2
  }

  ## mydata <- na.omit(mydata)
  id <- which(is.na(mydata[, wd]))
  if (length(id) > 0) {
    mydata <- mydata[-id, ]
  }

  ## greyscale handling
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }

  if (!fill) {
    ## labels depend on whether line or area are used
    theLabels <- percentile
  } else {
    values <- cbind(percentile[-length(percentile)], percentile[-1])
    theLabels <- paste(values[, 1], "-", values[, 2], sep = "")
  }

  prepare.grid <- function(mydata, stat, overall.lower, overall.upper) {
    overall.lower <- mydata$lower[1]
    overall.upper <- mydata$upper[1]

    # wd = NULL
    ## add zero wind angle = same as 360 for cyclic spline
    ids <- which(mydata[, wd] == 360)

    if (length(ids) > 0) {
      zero.wd <- mydata[ids, ]
      zero.wd[, wd] <- 0
      mydata <- bind_rows(mydata, zero.wd)
    }

    mod.percentiles <- function(i, mydata, overall.lower, overall.upper) {
      ## need to work out how many knots to use in smooth
      thedata <- subset(percentiles, percentile == i)

      if (smooth) {
        min.dat <- min(thedata)

        ## fit a spline through the data; making sure it goes through each wd value
        spline.res <- spline(
          x = thedata[[wd]],
          y = thedata[[pollutant]],
          n = 361,
          method = "natural"
        )

        pred <- data.frame(percentile = i, wd = 0:360, pollutant = spline.res$y)
        names(pred)[2] <- wd

        ## don't let interpolated percentile be lower than data
        pred$pollutant[pred$pollutant < min.dat] <- min.dat

        ## only plot where there are valid wd
        wds <- unique(percentiles[[wd]])
        ids <- lapply(
          wds,
          function(x) seq(from = x - angle / 2, to = x + angle / 2)
        )
        ids <- unique(do.call(c, ids))
        ids[ids < 0] <- ids[ids < 0] + 360
        pred$pollutant[-ids] <- min(c(
          0,
          min(percentiles[[pollutant]], na.rm = TRUE)
        ))
      } else {
        ## do not smooth
        dat1 <- thedata
        dat2 <- thedata
        dat1[[wd]] <- thedata[[wd]] - angle / 2
        dat2[[wd]] <- thedata[[wd]] + angle / 2
        dat1$id <- 2 * 1:nrow(dat1) - 1
        dat2$id <- 2 * 1:nrow(dat2)
        thedata <- rbind(dat1, dat2)

        thedata <- thedata[order(thedata$id), ]
        thedata$pollutant <- thedata[[eval(pollutant)]]
        pred <- thedata
      }
      pred
    }

    if (method == "default") {
      ## calculate percentiles
      percentiles <- group_by(mydata, wd) |>
        dplyr::reframe(
          {{ pollutant }} := quantile(
            .data[[pollutant]],
            probs = percentile / 100,
            na.rm = TRUE
          )
        ) |>
        group_by(wd) |>
        mutate(percentile = percentile)
    }

    if (tolower(method) == "cpf") {
      percentiles1 <- group_by(mydata, wd) |>
        summarise(across(
          where(is.numeric),
          ~ length(which(.x < overall.lower)) / length(.x)
        ))

      percentiles1$percentile <- min(percentile)

      percentiles2 <- group_by(mydata, wd) |>
        summarise(across(
          where(is.numeric),
          ~ length(which(.x > upper)) / length(.x)
        ))

      percentiles2$percentile <- max(percentile)

      if (fill) {
        percentiles <- rbind(percentiles1, percentiles2)
      } else {
        percentiles <- percentiles2
      }
    }

    results <-
      purrr::map(
        .x = percentile,
        .f = \(x) mod.percentiles(x, overall.lower, overall.upper)
      ) |>
      dplyr::bind_rows()

    ## calculate mean; assume a percentile of 999 to flag it later

    percentiles <- group_by(mydata, wd) |>
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

    percentiles$percentile <- 999

    Mean <- purrr::map(999, mod.percentiles) |>
      purrr::list_rbind()

    if (stat == "percentile") {
      results <- results
    } else {
      results <- Mean
    }
    results
  }

  mydata <- cutData(mydata, type, ...)

  ## overall.lower and overall.upper are the OVERALL upper/lower percentiles
  #  overall.lower <- quantile(mydata[[pollutant]], probs = min(percentile) / 100, na.rm = TRUE)
  #  overall.upper <- quantile(mydata[[pollutant]], probs = max(percentile) / 100, na.rm = TRUE)

  ## overall.lower and overall.upper are the OVERALL upper/lower percentiles, but pollutant specific
  if (npol > 1) {
    mydata <- mydata |>
      group_by(variable) |>
      mutate(
        lower = quantile(
          .data[[pollutant]],
          probs = min(percentile) / 100,
          na.rm = TRUE
        ),
        upper = quantile(
          .data[[pollutant]],
          probs = max(percentile) / 100,
          na.rm = TRUE
        )
      ) |>
      ungroup()
  } else {
    mydata <- mutate(
      mydata,
      lower = quantile(
        .data[[pollutant]],
        probs = min(percentile) / 100,
        na.rm = TRUE
      ),
      upper = quantile(
        .data[[pollutant]],
        probs = max(percentile) / 100,
        na.rm = TRUE
      )
    )
  }

  results.grid <-
    mapType(
      mydata,
      type = type,
      fun = \(x) prepare.grid(x, stat = "percentile"),
      .include_default = TRUE
    )

  if (method == "cpf") {
    ## useful labelling
    sub <- paste(
      "CPF at the ",
      max(percentile),
      "th percentile (=",
      round(
        max(quantile(
          mydata[[pollutant]],
          probs = percentile / 100,
          na.rm = TRUE
        )),
        1
      ),
      ")",
      sep = ""
    )
  }

  if (mean) {
    Mean <- mapType(
      mydata,
      type = type,
      fun = \(x) prepare.grid(x, stat = "mean"),
      .include_default = TRUE
    )

    results.grid <- bind_rows(results.grid, Mean)
  }

  ## proper names of labelling ###################################################
  strip.dat <- strip.fun(results.grid, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]

  col <- openColours(cols, length(theLabels))

  legend <- list(
    col = col,
    space = key.position,
    auto.text = auto.text,
    labels = theLabels,
    footer = key.footer,
    header = key.header,
    height = 0.60,
    width = 1.5,
    fit = "scale",
    plot.style = "other"
  )

  col <- grDevices::adjustcolor(col, alpha.f = alpha)

  legend <- makeOpenKeyLegend(key, legend, "percentileRose")

  if (mean.only || tolower(method) == "cpf") {
    legend <- NULL
  }

  temp <- paste(type, collapse = "+")
  myform <- formula(paste("y ~ x | ", temp, sep = ""))

  ## keep unstransformed copy in case data are negative
  results <- results.grid

  results.grid$x <- results.grid$pollutant * sin(results.grid[[wd]] * pi / 180)
  results.grid$y <- results.grid$pollutant * cos(results.grid[[wd]] * pi / 180)

  min.res <- min(results.grid$pollutant, na.rm = TRUE)

  newdata <- results.grid ## data to return

  ## nice intervals for pollutant concentrations
  tmp <- (results.grid$x^2 + results.grid$y^2)^0.5
  if (is.null(intervals)) {
    intervals <- pretty(c(min(tmp, na.rm = TRUE), max(tmp, na.rm = TRUE)))
  }

  labs <- intervals ## the labels

  ## if negative data, add to make all postive to plot properly
  min.int <- min(intervals, na.rm = TRUE)
  zero <- NA

  if (min.int < 0) {
    zero <- which(intervals == 0) ## the zero line
    intervals <- intervals + -1 * min.int
    results$pollutant <- results$pollutant + -1 * min.int
    results.grid <- transform(
      results,
      x = pollutant * sin(eval(wd) * pi / 180),
      y = pollutant * cos(eval(wd) * pi / 180)
    )
  }

  ## re-label if CPF plot
  if (tolower(method) == "cpf") {
    pollutant <- "probability"
  }

  xyplot.args <- list(
    x = myform,
    xlim = c(max(intervals) * -1, max(intervals) * 1),
    ylim = c(max(intervals) * -1, max(intervals) * 1),
    data = results.grid,
    type = "n",
    sub = sub,
    strip = strip,
    strip.left = strip.left,
    as.table = TRUE,
    aspect = 1,
    par.strip.text = list(cex = 0.8),
    scales = list(draw = FALSE),
    ...,
    panel = function(x, y, subscripts, ...) {
      if (fill) {
        ## filled polygons

        for (i in rev(seq_along(percentile))) {
          value <- percentile[i]

          if (i == 1) {
            subdata <- subset(results.grid[subscripts, ], percentile == value)

            if (length(percentile) > 1) {
              lpolygon(subdata$x, subdata$y, col = col[1], border = NA)
            } else {
              lpolygon(subdata$x, subdata$y, col = "white", border = NA)
            }
          } else {
            subdata1 <- results.grid[subscripts, ] |>
              filter(percentile == {{ value }})

            value2 <- percentile[i - 1]
            subdata2 <- results.grid[subscripts, ] |>
              filter(percentile == {{ value2 }})

            poly.na(
              x1 = subdata1$x,
              x2 = subdata2$x,
              y1 = subdata1$y,
              y2 = subdata2$y,
              myColors = col[i - 1],
              alpha = 1,
              border = col[i - 1]
            )
          }
        }
      }

      angles <- seq(0, 2 * pi, length = 360)
      sapply(intervals, function(x) {
        llines(
          x * sin(angles),
          x * cos(angles),
          col = "grey85",
          lty = 5
        )
      })

      ## zero line if needed
      if (!is.na(zero)) {
        llines(
          intervals[zero] * sin(angles),
          intervals[zero] * cos(angles),
          col = "grey85"
        )
      }

      ## add axis lines
      larrows(max(intervals) * -1, 0, max(intervals), 0, code = 3, length = 0.1)
      larrows(0, max(intervals) * -1, 0, max(intervals), code = 3, length = 0.1)

      ltext(
        0.7 * sin(pi * (angle.scale + 5) / 180) * max(intervals),
        0.7 * cos(pi * (angle.scale + 5) / 180) * max(intervals),
        quickText(pollutant, auto.text),
        srt = 0,
        cex = 0.8,
        pos = 4
      )

      ltext(max(intervals) * -1 * 0.95, 0.07 * max(intervals), "W", cex = 0.7)
      ltext(0.07 * max(intervals), max(intervals) * -1 * 0.95, "S", cex = 0.7)
      ltext(0.07 * max(intervals), max(intervals) * 0.95, "N", cex = 0.7)
      ltext(max(intervals) * 0.95, 0.07 * max(intervals), "E", cex = 0.7)

      ## draw lines if fill = FALSE
      if (!fill) {
        for (i in seq_along(percentile)) {
          value <- percentile[i]
          subdata <- subset(results.grid[subscripts, ], percentile == value)
          llines(subdata$x, subdata$y, col = col[i], lwd = extra.args$lwd)
        }
      }

      ## add mean line
      if (mean) {
        subdata <- subset(results.grid[subscripts, ], percentile == 999)
        llines(
          subdata$x,
          subdata$y,
          col = mean.col,
          lwd = mean.lwd,
          lty = mean.lty
        )
      }

      ltext(
        intervals * sin(pi * angle.scale / 180),
        intervals * cos(pi * angle.scale / 180),
        paste(labs, c("", "", rep("", 7))),
        cex = 0.7
      )
    },
    legend = legend
  )

  # reset for extra.args
  xyplot.args <- listUpdate(xyplot.args, extra.args)

  # plot
  plt <- do.call(xyplot, xyplot.args)

  ## output ####################################################################################

  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    } else {
      plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    }
  }

  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"

  invisible(output)
}
