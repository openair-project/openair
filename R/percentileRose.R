#' Function to plot percentiles by wind direction
#'
#' [percentileRose()] plots percentiles by wind direction with flexible
#' conditioning. The plot can display multiple percentile lines or filled areas.
#'
#' [percentileRose()] calculates percentile levels of a pollutant and plots them
#' by wind direction. One or more percentile levels can be calculated and these
#' are displayed as either filled areas or as lines.
#'
#' The wind directions are rounded to the nearest 10 degrees, consistent with
#' surface data from the UK Met Office before a smooth is fitted. The levels by
#' wind direction are optionally calculated using a cyclic smooth cubic spline
#' using the option `smooth`. If `smooth = FALSE` then the data are shown in 10
#' degree sectors.
#'
#' The `percentileRose` function compliments other similar functions including
#' [windRose()], [pollutionRose()], [polarFreq()] or [polarPlot()]. It is most
#' useful for showing the distribution of concentrations by wind direction and
#' often can reveal different sources e.g. those that only affect high
#' percentile concentrations such as a chimney stack.
#'
#' Similar to other functions, flexible conditioning is available through the
#' `type` option. It is easy for example to consider multiple percentile values
#' for a pollutant by season, year and so on. See examples below.
#'
#' `percentileRose` also offers great flexibility with the scale used and the
#' user has fine control over both the range, interval and colour.
#'
#' @inheritParams polarPlot
#'
#' @param mydata A data frame minimally containing `wd` and a numeric field to
#'   plot --- `pollutant`.
#'
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`. More than one
#'   pollutant can be supplied e.g. `pollutant = c("no2", "o3")` provided there
#'   is only one `type`.
#'
#' @param percentile The percentile value(s) to plot. Must be between 0--100. If
#'   `percentile = NA` then only a mean line will be shown.
#'
#' @param smooth Should the wind direction data be smoothed using a cyclic
#'   spline?
#'
#' @param method When `method = "default"` the supplied percentiles by wind
#'   direction are calculated. When `method = "cpf"` the conditional probability
#'   function (CPF) is plotted and a single (usually high) percentile level is
#'   supplied. The CPF is defined as CPF = my/ny, where my is the number of
#'   samples in the wind sector y with mixing ratios greater than the *overall*
#'   percentile concentration, and ny is the total number of samples in the same
#'   wind sector (see Ashbaugh et al., 1985).
#'
#' @param angle Default angle of \dQuote{spokes} is when `smooth = FALSE`.
#'
#' @param mean Show the mean by wind direction as a line?
#'
#' @param mean.lty Line type for mean line.
#'
#' @param mean.lwd Line width for mean line.
#'
#' @param mean.col Line colour for mean line.
#'
#' @param fill Should the percentile intervals be filled (default) or should
#'   lines be drawn (`fill = FALSE`).
#'
#' @param intervals User-supplied intervals for the scale e.g. `intervals = c(0,
#'   10, 30, 50)`.
#'
#' @param strip.position Location where the facet 'strips' are located when
#'   using `type`. When one `type` is provided, can be one of `"left"`,
#'   `"right"`, `"bottom"` or `"top"`. When two `type`s are provided, this
#'   argument defines whether the strips are "switched" and can take either
#'   `"x"`, `"y"`, or `"both"`. For example, `"x"` will switch the 'top' strip
#'   locations to the bottom of the plot.
#'
#' @param ... Other graphical parameters are passed onto `cutData` and
#'   `lattice:xyplot`. For example, `percentileRose` passes the option
#'   `hemisphere = "southern"` on to `cutData` to provide southern (rather than
#'   default northern) hemisphere handling of `type = "season"`. Similarly,
#'   common graphical arguments, such as `xlim` and `ylim` for plotting ranges
#'   and `lwd` for line thickness when using `fill = FALSE`, are passed on
#'   `xyplot`, although some local modifications may be applied by openair. For
#'   example, axis and title labelling options (such as `xlab`, `ylab` and
#'   `main`) are passed to `xyplot` via `quickText` to handle routine
#'   formatting.
#'
#' @export
#' @return an [openair][openair-package] object
#' @family polar directional analysis functions
#'
#' @author David Carslaw
#' @author Jack Davison
#'
#' @references Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A residence time
#'   probability analysis of sulfur concentrations at ground canyon national
#'   park. Atmospheric Environment 19 (8), 1263-1270.
#'
#' @examples
#' # basic percentile plot
#' percentileRose(mydata, pollutant = "o3")
#'
#' # 50/95th percentiles of ozone, with different colours
#' percentileRose(mydata, pollutant = "o3", percentile = c(50, 95), col = "brewer1")
#'
#' \dontrun{
#' # percentiles of ozone by year, with different colours
#' percentileRose(
#'   mydata,
#'   type = "year",
#'   pollutant = "o3",
#'   col = "brewer1",
#'   layout = c(4, 2)
#' )
#'
#' # percentile concentrations by season and day/nighttime..
#' percentileRose(
#'   mydata,
#'   type = c("daylight", "season"),
#'   pollutant = "o3",
#'   col = "brewer1"
#' )
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
  key = TRUE,
  key.header = NULL,
  key.footer = "percentile",
  key.position = "bottom",
  strip.position = "top",
  plot = TRUE,
  ...
) {
  if (rlang::is_logical(key) && !key) {
    key.position <- "none"
  }

  # calculate percentiles or just show mean?
  if (is.na(percentile[1])) {
    mean.only <- TRUE
    percentile <- 0
  } else {
    mean.only <- FALSE
  }

  if (tolower(method) == "cpf") {
    mean <- FALSE
    if (length(percentile) > 1) {
      cli::cli_abort(
        "Only one percentile should be supplied when {.arg method} = 'CPF'."
      )
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
  if (length(ids) > 0 && !smooth) {
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

  # if more than one pollutant, need to stack the data and set type = "variable"
  # this case is most relevent for model-measurement compasrions where data are in columns
  # Can also do more than one pollutant and a single type that is not "default", in which
  # case pollutant becomes a conditioning variable
  if (length(pollutant) > 1) {
    if (length(type) > 1) {
      cli::cli_warn("Only type = '{type[1]}' will be used.")
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

  # extra.args setup
  extra.args <- list(...)

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab, auto.text)
  extra.args$ylab <- quickText(extra.args$ylab, auto.text)
  extra.args$main <- quickText(extra.args$main, auto.text)

  extra.args$lwd <- extra.args$lwd %||% 2

  id <- which(is.na(mydata[, wd]))
  if (length(id) > 0) {
    mydata <- mydata[-id, ]
  }

  prepare.grid <- function(mydata, stat, overall.lower, overall.upper) {
    overall.lower <- mydata$lower[1]
    overall.upper <- mydata$upper[1]

    # add zero wind angle = same as 360 for cyclic spline
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
        dat1$id <- 2 * seq_len(nrow(dat1)) - 1
        dat2$id <- 2 * seq_len(nrow(dat2))
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

  # overall.lower and overall.upper are the OVERALL upper/lower percentiles, but
  # pollutant specific
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

  sub <- NULL
  if (method == "cpf") {
    ## useful labelling
    sub <- paste0(
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
      ")"
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

  # labels for factor levels
  if (fill) {
    fct_labels <- breaksToLabels(percentile)
  } else {
    fct_labels <- as.character(percentile)
  }

  # arrange data for plotting and make percentile a factor with appropriate
  # labels
  if (method == "cpf") {
    plot_data <-
      results.grid |>
      dplyr::ungroup() |>
      dplyr::filter(percentile != 0) |>
      dplyr::arrange(desc(percentile)) |>
      dplyr::mutate(
        percentile = factor(percentile, labels = fct_labels)
      )
  } else {
    plot_data <-
      results.grid |>
      dplyr::ungroup() |>
      dplyr::filter(percentile != 0) |>
      dplyr::arrange(desc(percentile)) |>
      dplyr::mutate(
        percentile = factor(
          percentile,
          rev(sort(unique(percentile))),
          labels = c("Mean", rev(fct_labels))
        )
      )
  }

  legend_guide <-
    ggplot2::guide_legend(
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
    dplyr::filter(percentile != "Mean") |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["wd"]], y = .data[["pollutant"]])
    ) +
    ggplot2::coord_radial(r.axis.inside = angle.scale) +
    scale_x_compass() +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0, 0.1)),
      limits = c(0, ifelse(is.null(intervals), NA, max(intervals))),
      breaks = intervals %||% scales::breaks_pretty()
    ) +
    theme_openair_radial(key.position = key.position, panel.ontop = TRUE) +
    set_extra_fontsize(extra.args) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      title = extra.args$main,
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        openair::openColours(cols, n = length(percentile[percentile != 0])),
        mean.col
      ),
      breaks = fct_labels,
      name = quickText(
        paste(
          key.header,
          key.footer,
          sep = ifelse(key.position %in% c("top", "bottom"), " ", "\n")
        ),
        auto.text = auto.text
      ),
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::guides(
      fill = legend_guide,
      color = legend_guide
    ) +
    get_facet(
      type = type,
      extra.args = extra.args,
      scales = "fixed",
      auto.text = auto.text,
      drop = FALSE,
      strip.position = strip.position
    )

  if (!mean.only) {
    if (fill) {
      thePlot <-
        thePlot +
        ggplot2::geom_area(
          ggplot2::aes(fill = .data[["percentile"]]),
          show.legend = method != "cpf",
          key_glyph = ggplot2::draw_key_rect,
          position = ggplot2::position_identity()
        )
    } else {
      thePlot <-
        thePlot +
        ggplot2::geom_line(
          ggplot2::aes(colour = .data[["percentile"]]),
          linewidth = extra.args$lwd / 3,
          show.legend = method != "cpf",
          key_glyph = ggplot2::draw_key_rect,
          position = ggplot2::position_identity()
        )
    }
  }

  if (mean) {
    thePlot <-
      thePlot +
      ggplot2::geom_line(
        data = plot_data |> dplyr::filter(.data$percentile == "Mean"),
        colour = mean.col,
        linewidth = mean.lwd / 3,
        linetype = mean.lty
      )
  }

  if (method == "cpf") {
    thePlot <- thePlot + ggplot2::labs(caption = sub)
  }

  # make key full width/height
  if (key.position %in% c("left", "right")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.height = ggplot2::rel(2)
      )
  }
  if (key.position %in% c("top", "bottom")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.width = ggplot2::rel(2)
      )
  }

  if (plot) {
    plot(thePlot)
  }

  output <- list(plot = thePlot, data = results.grid, call = match.call())
  class(output) <- "openair"

  invisible(output)
}
