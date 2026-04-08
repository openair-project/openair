#' Temporal variation plots with flexible panel control
#'
#' Plots temporal variation for different variables, typically pollutant
#' concentrations, across user-defined time scales. Multiple panels can be
#' shown, such as hour of the day, day of the week, week of the year, month of
#' the year, annual mean, or any other time-based grouping the user specifies.
#' By default, this function plots the diurnal, day of the week and monthly
#' variation for different variables, typically pollutant concentrations. Four
#' separate plots are produced. This is a convenient alternative to using
#' [variationPlot()] and assembling the plots manually.
#'
#' The variation of pollutant concentrations by time can reveal many interesting
#' features that relate to source types and meteorology. For traffic sources,
#' there are often important differences in the way vehicles vary by type -
#' e.g., fewer heavy vehicles at weekends.
#'
#' Users can supply their own `ylim`, e.g. `ylim = c(0, 200)`, which will be
#' used for all plots. Alternatively, `ylim` can be a list equal to the length
#' of `panels` to control y-limits for each individual panel, e.g. `ylim =
#' list(c(-100,500), c(200, 300), c(-400,400), c(50,70))`.
#'
#' Note also that the [timeVariation()] function works well on a subset of data
#' and in conjunction with other plots. For example, a [polarPlot()] may
#' highlight an interesting feature for a particular wind speed/direction range.
#' By filtering for those conditions [timeVariation()] can help determine
#' whether the temporal variation of that feature differs from other features
#' --- and help with source identification.
#'
#' @inheritParams shared_openair_params
#' @inheritParams variationPlot
#' @inheritParams timePlot
#'
#' @param panels A vector of character values which can be passed to
#'   [cutData()]; used to define each panel in the plot. The first panel will
#'   take up the entire first row, and any remaining panels will make up the
#'   bottom row. If a single panel is given, it will take up the entire plotting
#'   area. Combining two `type` strings delimited with a full stop (e.g.,
#'   `"hour.weekday"`) will use the first as the x-axis variable the second as a
#'   facet.
#'
#' @param xlab x-axis label; one for each `panel`. Defaults to the x-axis
#'   variable defined in `panels`. Must be the same length as `panels`.
#'
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in [cutData()],
#'   e.g., `"season"`, `"year"`, `"weekday"` and so on. For example, `type =
#'   "season"` will produce four plots --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data frame.
#'   If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   Only one `type` is allowed in [timeVariation()], and it is applied to each
#'   `panel`. For additional splits, use the `"x.type"` syntax in the `panels`
#'   argument (e.g, `panels = c("hour.weekday")`).
#'
#' @param key By default [timeVariation()] produces four plots on one page.
#'   While it is useful to see these plots together, it is sometimes necessary
#'   just to use one for a report. If `key` is `TRUE`, a key is added to all
#'   plots allowing the extraction of a single plot *with* key. If `key` is
#'   `FALSE`, no key is shown for any plot.
#'
#' @param panel.gap The gap between panels in any split panel (e.g., the default
#'   `"hour.weekday"` panel).
#'
#' @export
#'
#' @return an [openair][openair-package] object. The components of
#'   [timeVariation()] are named after `panels`. `main.plot` is a
#'   [patchwork][patchwork::patchwork-package] assembly.
#'
#' @author David Carslaw
#' @author Jack Davison
#'
#' @family time series and trend functions
#' @examples
#'
#' # basic use
#' timeVariation(mydata, pollutant = "nox")
#'
#' # for a subset of conditions
#' \dontrun{
#' timeVariation(subset(mydata, ws > 3 & wd > 100 & wd < 270),
#'   pollutant = "pm10", ylab = "pm10 (ug/m3)"
#' )
#'
#' # multiple pollutants with concentrations normalised
#' timeVariation(mydata, pollutant = c("nox", "co"), normalise = TRUE)
#'
#' # show BST/GMT variation (see ?cutData for more details)
#' # the NOx plot shows the profiles are very similar when expressed in
#' # local time, showing that the profile is dominated by a local source
#' # that varies by local time and not by GMT i.e. road vehicle emissions
#'
#' timeVariation(mydata, pollutant = "nox", type = "dst", local.tz = "Europe/London")
#'
#' # In this case it is better to group the results for clarity:
#' timeVariation(mydata, pollutant = "nox", group = "dst", local.tz = "Europe/London")
#'
#' # By contrast, a variable such as wind speed shows a clear shift when
#' #  expressed in local time. These two plots can help show whether the
#' #  variation is dominated by man-made influences or natural processes
#'
#' timeVariation(mydata, pollutant = "ws", group = "dst", local.tz = "Europe/London")
#'
#' # It is also possible to plot several variables and set type. For
#' # example, consider the NOx and NO2 split by levels of O3:
#'
#' timeVariation(mydata, pollutant = c("nox", "no2"), type = "o3", normalise = TRUE)
#'
#' # difference in concentrations
#' timeVariation(mydata, poll = c("pm25", "pm10"), difference = TRUE)
#'
#' # It is also useful to consider how concentrations vary by
#' # considering two different periods e.g. in intervention
#' # analysis. In the following plot NO2 has clearly increased but much
#' # less so at weekends - perhaps suggesting vehicles other than cars
#' # are important because flows of cars are approximately invariant by
#' # day of the week
#'
#' mydata <- splitByDate(mydata, dates = "1/1/2003", labels = c("before Jan. 2003", "After Jan. 2003"))
#' timeVariation(mydata, pollutant = "no2", group = "split.by", difference = TRUE)
#'
#' # sub plots can be extracted from the openair object
#' myplot <- timeVariation(mydata, pollutant = "no2")
#' myplot$plot$hour.weekday
#'
#' # individual plots
#' myplot$plot$hour.weekday
#' myplot$plot$hour
#' myplot$plot$day
#' myplot$plot$month
#'
#' # numerical results (mean, lower/upper uncertainties)
#' myplot$data$hour.weekday
#' myplot$data$hour
#' myplot$data$day
#' myplot$data$month
#'
#' # plot quantiles and median
#' timeVariation(
#'   mydata,
#'   statistic = "median",
#'   poll = "pm10",
#'   cols = "firebrick"
#' )
#'
#' # with different intervals
#' timeVariation(
#'   mydata,
#'   statistic = "median",
#'   poll = "pm10",
#'   conf.int = c(0.75, 0.99),
#'   cols = "firebrick"
#' )
#'
#' # with different (arbitrary) panels
#' # note 'hemisphere' is passed to cutData() for season
#' timeVariation(
#'   mydata,
#'   pollutant = "no2",
#'   panels = c("weekday.season", "year", "wd"),
#'   hemisphere = "southern"
#' )
#' }
timeVariation <- function(
  mydata,
  pollutant = "nox",
  panels = c(
    "hour.weekday",
    "hour",
    "month",
    "weekday"
  ),
  local.tz = NULL,
  normalise = FALSE,
  xlab = NULL,
  name.pol = NULL,
  type = "default",
  group = NULL,
  difference = FALSE,
  statistic = "mean",
  conf.int = NULL,
  B = 100,
  ci = TRUE,
  cols = "hue",
  ref.y = NULL,
  key = NULL,
  key.columns = NULL,
  key.position = "top",
  strip.position = "top",
  panel.gap = 1.5,
  auto.text = TRUE,
  alpha = 0.4,
  plot = TRUE,
  ...
) {
  if (length(type) > 1) {
    cli::cli_abort("Only one 'type' permitted.")
  }

  if (length(panels) == 1) {
    cli::cli_warn(
      c(
        "!" = "Instead of setting {.arg panel} to a single variable, use {.fun openair::plotVariation}.",
        "i" = 'i.e., {.code plotVariation(..., x = "{panels}")}'
      )
    )
  }

  extra.args <- capture_dots(...)

  # month.last deprecation
  if ("month.last" %in% names(extra.args)) {
    if (isTRUE(extra.args$month.last)) {
      cli::cli_warn(c(
        "!" = "{.arg month.last} has been deprecated. Please use the {.arg panels} argument for flexible control over panels.",
        "i" = "Setting {.arg panels} to {.code c('hour.weekday', 'hour', 'weekday', 'month')}."
      ))
      panels <- c("hour.weekday", "hour", "weekday", "month")
    }
    extra.args$month.last <- NULL
  }

  # label controls
  # xlab handled in formals and code because unique
  extra.args$ylab <- quickText(
    extra.args$ylab %||%
      ifelse(normalise, "normalised level", toString(pollutant)),
    auto.text
  )
  extra.args$title <- quickText(extra.args$title %||% "", auto.text)
  extra.args$subtitle <- quickText(extra.args$subtitle %||% "", auto.text)
  extra.args$caption <- quickText(
    extra.args$caption %||% create_varplot_sub_text(statistic, conf.int),
    auto.text
  )

  # if user supplies separate ylims for each plot
  if ("ylim" %in% names(extra.args)) {
    if (is.list(extra.args$ylim)) {
      if (length(extra.args$ylim) != length(panels)) {
        cli::cli_abort(
          "{.arg ylim} should be equal in length to {.arg panels} ({length(panels)})."
        )
      }
      ylim_list <- extra.args$ylim
    } else {
      ylim_list <- rep(list(extra.args$ylim), length(panels))
    }
  } else {
    ylim_list <- rep(list(c(NA, NA)), length(panels))
  }
  extra.args$ylim <- NULL

  # handle xlabs
  if (is.null(xlab)) {
    panels_xlabs <- rep(list(NULL), length(panels))
  } else {
    if (length(xlab) != length(panels)) {
      cli::cli_abort(
        "{.arg xlab} should be equal in length to {.arg panels} ({length(panels)})."
      )
    }
    panels_xlabs <- xlab
  }

  # title for overall and individual plots
  overall.title <- extra.args$title
  extra.args$title <- ""
  overall.subtitle <- extra.args$subtitle
  extra.args$subtitle <- ""
  overall.caption <- extra.args$caption
  extra.args$caption <- ""

  # get the xvars and facets for each panel
  panels_x <- list()
  panels_facet <- list()
  for (i in panels) {
    if (grepl("\\.", i)) {
      x <- strsplit(i, "\\.")[[1]]
      panels_x <- append(panels_x, x[1])
      panels_facet <- append(panels_facet, x[2])
    } else {
      panels_x <- append(panels_x, i)
      panels_facet <- append(panels_facet, list(NULL))
    }
  }

  outputs <-
    purrr::map(
      .x = seq_along(panels),
      .f = \(i) {
        types <- c(panels_facet[[i]], type[type != "default"])
        if (length(types) == 0) {
          types <- "default"
        }
        args <- list(
          mydata = mydata,
          pollutant = pollutant,
          x = panels_x[[i]],
          statistic = statistic,
          type = types,
          group = group %||% "default",
          normalise = normalise,
          difference = difference,
          conf.int = conf.int,
          B = B,
          local.tz = local.tz,
          ci = ci,
          cols = cols,
          alpha = alpha,
          strip.position = strip.position,
          key.position = key.position,
          key.columns = key.columns,
          name.pol = name.pol,
          auto.text = auto.text,
          ylim = ylim_list[[i]],
          plot = FALSE,
          xlab = panels_xlabs[[i]]
        )

        args <- append(args, extra.args)

        if (i == 1) {
          args$nrow <- 1L
          args$ncol <- NULL
        }

        do.call(variationPlot, args)
      }
    )

  # extract plots from outputs
  plots <- outputs |> purrr::map("plot") |> stats::setNames(panels)
  datum <- outputs |> purrr::map("data") |> stats::setNames(panels)

  # add reference if requested
  if (!is.null(ref.y)) {
    plots <- purrr::map(plots, \(plt) plt + gg_ref_y(ref.y = ref.y))
  }

  # if more than one plot, use patchwork to combine
  if (length(plots) > 1) {
    # "bottom row"
    bottom <- patchwork::wrap_plots(
      plots[-1],
      nrow = 1
    )

    thePlot <-
      patchwork::wrap_plots(
        plots[[1]],
        bottom,
        ncol = 1,
        heights = c(0.4, 0.6)
      ) +
      patchwork::plot_layout(guides = "collect") &
      patchwork::plot_annotation(
        title = overall.title,
        subtitle = overall.subtitle,
        caption = overall.caption
      ) &
      theme_openair(key.position) &
      ggplot2::theme(
        panel.spacing = ggplot2::rel(panel.gap),
        plot.margin = ggplot2::unit(rep(0.25, 4), "cm")
      )
  } else {
    thePlot <- plots[[1]]
  }

  # control key
  if (rlang::is_logical(key)) {
    if (key) {
      thePlot <- thePlot & patchwork::plot_layout(guides = "keep")
    } else {
      thePlot <- thePlot & ggplot2::theme(legend.position = "none")
    }
  }

  if (plot) {
    plot(thePlot)
  }

  output <- list(
    plot = append(plots, list(subsets = panels)),
    data = append(datum, list(subsets = panels)),
    call = match.call(),
    main.plot = thePlot
  )
  class(output) <- "openair"

  invisible(output)
}
