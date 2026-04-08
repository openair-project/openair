#' Variation Plot
#'
#' The `variationPlot()` function is designed to explore how the distribution of
#' a pollutant (or other variable) changes by another variable (`x`). For
#' example, it can be used to explore how the distribution of `nox` varies by
#' `season` or by `weekday`. This plot can be extensively conditioned using the
#' `type` and `group` arguments, both of which are passed to [cutData()]. An
#' appropriate plot type will be chosen based on the type of `x` - e.g., ordered
#' variables will be joined by a line.
#'
#' When `statistic = "mean"`, the plot shows the 95% confidence intervals in the
#' mean. The 95% confidence intervals are calculated through bootstrap
#' simulations, which will provide more robust estimates of the confidence
#' intervals (particularly when there are relatively few data).
#'
#' Users can supply their own `ylim`, e.g. `ylim = c(0, 200)`.
#'
#' The `difference` option calculates the difference in means between two
#' pollutants, along with bootstrap estimates of the 95\% confidence intervals
#' in the difference. This works in two ways: either two pollutants are supplied
#' in separate columns (e.g. `pollutant = c("no2", "o3")`), or there are two
#' unique values of `group`. The difference is calculated as the second
#' pollutant minus the first and is labelled accordingly. This feature is
#' particularly useful for model evaluation and identifying where models diverge
#' from observations across time scales.
#'
#' Depending on the choice of statistic, a subheading is added. Users can
#' control the text in the subheading through the use of `sub` e.g. `sub = ""`
#' will remove any subheading.
#'
#' @inheritParams timePlot
#' @inheritParams cutData
#'
#' @param x A character value to be passed to [cutData()]; used to define the
#'   category by which `pollutant` will be varied and plotted.
#'
#' @param normalise Should variables be normalised? The default is `FALSE`. If
#'   `TRUE` then the variable(s) are divided by their mean values. This helps to
#'   compare the shape of the diurnal trends for variables on very different
#'   scales.
#'
#' @param group This sets the grouping variable to be used. For example, if a
#'   data frame had a column `site` setting `group = "site"` will plot all sites
#'   together in each panel. Passed to [cutData()].
#'
#' @param difference If two pollutants are chosen then setting `difference =
#'   TRUE` will also plot the difference in means between the two variables as
#'   `pollutant[2] - pollutant[1]`. Bootstrap 95\% confidence intervals of the
#'   difference in means are also calculated. A horizontal dashed line is shown
#'   at y = 0. The difference can also be calculated if there is a column that
#'   identifies two groups, e.g., having used [splitByDate()]. In this case it
#'   is possible to call the function with the option `group = "split.by"` and
#'   `difference = TRUE`.
#'
#' @param statistic Can be `"mean"` (default) or `"median"`. If the statistic is
#'   `"mean"` then the mean line and the 95% confidence interval in the mean are
#'   plotted by default. If the statistic is `"median"` then the median line is
#'   plotted together with the 5/95 and 25/75th quantiles are plotted. Users can
#'   control the confidence intervals with `conf.int`.
#'
#' @param conf.int The confidence intervals to be plotted. If `statistic =
#'   "mean"` then the confidence intervals in the mean are plotted. If
#'   `statistic = "median"` then the `conf.int` and `1 - conf.int` *quantiles*
#'   are plotted. Any number of `conf.int`s can be provided.
#'
#' @param B Number of bootstrap replicates to use. Can be useful to reduce this
#'   value when there are a large number of observations available to increase
#'   the speed of the calculations without affecting the 95% confidence interval
#'   calculations by much.
#'
#' @param ci Should confidence intervals be shown? The default is `TRUE`.
#'   Setting this to `FALSE` can be useful if multiple pollutants are chosen
#'   where over-lapping confidence intervals can over complicate plots.
#'
#' @param alpha The alpha transparency used for plotting confidence intervals.
#'   `0` is fully transparent and 1 is opaque. The default is `0.4`.
#'
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available:
#'   - `xlab`, `ylab` and `title` override the x-axis label, y-axis label, and plot title.
#'   - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'   - `linewidth` and `linetype` control various graphical parameters.
#'   - `fontsize` overrides the overall font size of the plot.
#'   - `ylim` controls axis limits.
#'
#' @export
#'
#' @seealso [timeVariation()], which conveniently assembles many time-related
#'   variation plots into a single plot
#'
#' @return an [openair][openair-package] object.
#'
#' @author Jack Davison
#' @author David Carslaw
#'
#' @examples
#' # example using the 'mydata' dataset
#' variationPlot(
#'   mydata,
#'   pollutant = c("nox", "o3"),
#'   x = "hour",
#'   type = "season",
#'   normalise = TRUE
#' )
variationPlot <- function(
  mydata,
  pollutant = "nox",
  x = "hour",
  statistic = "mean",
  type = "default",
  group = "default",
  normalise = FALSE,
  difference = FALSE,
  conf.int = NULL,
  B = 100,
  local.tz = NULL,
  ci = TRUE,
  cols = "hue",
  alpha = 0.4,
  strip.position = "top",
  key.position = "top",
  key.columns = NULL,
  name.pol = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  if (is.null(conf.int)) {
    if (statistic == "median") {
      conf.int <- c(0.75, 0.95)
    } else {
      conf.int <- c(0.95)
    }
  }

  # validate inputs
  validate_varplot_inputs(
    mydata = mydata,
    group = group,
    pollutant = pollutant,
    type = type,
    statistic = statistic,
    conf.int = conf.int
  )

  # extra.args setup
  extra.args <- capture_dots(...)

  # labels
  extra.args$ylab <- quickText(
    extra.args$ylab %||%
      ifelse(normalise, "normalised level", toString(pollutant)),
    auto.text
  )
  extra.args$xlab <- quickText(extra.args$x %||% x, auto.text)
  extra.args$title <- quickText(extra.args$title %||% "", auto.text)
  extra.args$subtitle <- quickText(extra.args$subtitle %||% "", auto.text)
  extra.args$caption <- quickText(
    extra.args$caption %||% create_varplot_sub_text(statistic, conf.int),
    auto.text
  )
  extra.args$linetype <- extra.args$linetype %||% 1

  drop <- extra.args$drop %||% "none"
  extra.args$drop <- NULL

  # check & cut data
  # always need date for the 'difference' path
  vars <- c(pollutant, "date")

  if (!x %in% dateTypes) {
    vars <- c(vars, x)
  }
  if (!any(type %in% dateTypes)) {
    vars <- c(vars, type[!type %in% dateTypes])
  }
  if (!group %in% dateTypes) {
    vars <- c(vars, group)
  }
  vars <- vars[vars != "default"]

  # how many 'types' are we working with?
  type_cols <- "type"
  if (length(type) == 2) {
    type_cols <- c("type_y", "type_x")
  }

  # data checks
  # need special handling of cutData as `drop` needs to be passed separately
  mydata <- mydata |>
    checkPrep(vars, c(type, group), remove.calm = FALSE) |>
    (\(df) {
      do.call(
        cutData,
        append(
          list(
            x = df,
            type = x,
            names = "x",
            local.tz = local.tz,
            is.axis = TRUE,
            drop = drop
          ),
          extra.args
        )
      )
    })() |>
    (\(df) {
      do.call(
        cutData,
        append(
          list(
            x = df,
            type = type,
            names = type_cols,
            local.tz = local.tz,
            drop = drop
          ),
          extra.args
        )
      )
    })() |>
    (\(df) {
      do.call(
        cutData,
        append(
          list(
            x = df,
            type = group,
            names = "group",
            local.tz = local.tz,
            drop = drop
          ),
          extra.args
        )
      )
    })()

  # put in local time if needed
  if (!is.null(local.tz)) {
    attr(mydata$date, "tzone") <- local.tz
  }

  # reformat data for plotting
  if (group == "default") {
    mydata$group <- NULL
    mydata <- tidyr::pivot_longer(
      mydata,
      cols = dplyr::all_of(pollutant),
      names_to = "group",
      values_to = "value"
    ) |>
      dplyr::mutate(
        group = factor(.data$group)
      )
  } else {
    names(mydata)[names(mydata) == pollutant] <- "value"
  }

  # if x is hour or week, make sure it's numeric for plotting
  if (x %in% c("hour", "week")) {
    mydata$x <- as.numeric(as.character(mydata$x))
  }

  # before summary stats - decompose wd into u/v
  wd_present <- "wd" %in% levels(mydata$group)
  if (wd_present) {
    mydata <- mydata |>
      dplyr::mutate(
        group = as.character(.data$group),
        value = dplyr::case_when(
          .data$group == "wd" ~ sin(pi * .data$value / 180), # becomes wd_u
          .default = .data$value
        ),
        group = dplyr::if_else(.data$group == "wd", "wd_u", .data$group)
      ) |>
      dplyr::bind_rows(
        mydata |>
          dplyr::filter(.data$group == "wd") |>
          dplyr::mutate(
            value = cos(pi * .data$value / 180),
            group = "wd_v"
          )
      ) |>
      dplyr::mutate(group = factor(.data$group))
  }

  # calculate summary values and confidence intervals
  if (difference) {
    # identify the two things being differenced
    if (nlevels(mydata$group) != 2L) {
      cli::cli_abort("Need 2 'groups' to calculate a difference.")
    }
    the_levels <- levels(mydata$group) # already set by pivot_longer or group cutData
    poll1 <- the_levels[1]
    poll2 <- the_levels[2]

    # pivot wide so both groups are columns - needed for bootMeanDiff
    mydata_wide <- mydata |>
      tidyr::pivot_wider(names_from = "group", values_from = "value")

    if (statistic == "mean") {
      mydata <- purrr::map(
        conf.int,
        \(q) {
          dplyr::reframe(
            mydata_wide,
            bootMeanDiff(
              dplyr::pick(dplyr::everything()),
              x = poll1,
              y = poll2,
              conf.int = q,
              B = B
            ),
            .by = dplyr::all_of(c("x", type_cols))
          ) |>
            dplyr::rename(mid = "Mean", min = "Lower", max = "Upper") |>
            dplyr::mutate(ci = q)
        }
      ) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
          group = factor(
            .data$variable,
            levels = c(poll1, poll2, paste(poll2, "-", poll1))
          )
        ) |>
        dplyr::select(-"variable")
    } else {
      mydata_wide <-
        dplyr::mutate(
          mydata_wide,
          diff = .data[[poll2]] - .data[[poll1]]
        )

      names(mydata_wide)[names(mydata_wide) == "diff"] <- paste(
        poll2,
        "-",
        poll1
      )

      mydata <-
        mydata_wide |>
        tidyr::pivot_longer(
          dplyr::all_of(c(poll1, poll2, paste(poll2, "-", poll1))),
          names_to = "group"
        ) |>
        dplyr::mutate(
          group = factor(
            .data$group,
            levels = c(poll1, poll2, paste(poll2, "-", poll1))
          )
        )

      mydata <-
        purrr::map(
          conf.int,
          \(q) {
            qs <- sort(c(q, 1 - q))

            dplyr::summarise(
              mydata,
              mid = stats::quantile(.data$value, 0.5, na.rm = TRUE),
              min = stats::quantile(.data$value, qs[1], na.rm = TRUE),
              max = stats::quantile(.data$value, qs[2], na.rm = TRUE),
              .by = dplyr::all_of(c("x", type_cols, "group"))
            ) |>
              dplyr::mutate(
                ci = q
              )
          }
        ) |>
        dplyr::bind_rows()
    }
  } else {
    if (statistic == "mean") {
      mydata <-
        purrr::map(
          conf.int,
          \(q) {
            dplyr::summarise(
              mydata,
              bootMeanDF(.data$value, conf.int = q, B = B),
              .by = dplyr::all_of(c("x", type_cols, "group"))
            ) |>
              dplyr::rename("mid" = "mean") |>
              dplyr::mutate(
                ci = q
              )
          }
        ) |>
        dplyr::bind_rows() |>
        dplyr::select(-"n")
    } else {
      mydata <-
        purrr::map(
          conf.int,
          \(q) {
            qs <- sort(c(q, 1 - q))

            dplyr::summarise(
              mydata,
              mid = stats::quantile(.data$value, 0.5, na.rm = TRUE),
              min = stats::quantile(.data$value, qs[1], na.rm = TRUE),
              max = stats::quantile(.data$value, qs[2], na.rm = TRUE),
              .by = dplyr::all_of(c("x", type_cols, "group"))
            ) |>
              dplyr::mutate(
                ci = q
              )
          }
        ) |>
        dplyr::bind_rows()
    }
  }

  # after summary stats - convert wd_u/wd_v back to wd
  if (wd_present) {
    wd_data <- mydata |>
      dplyr::filter(.data$group %in% c("wd_u", "wd_v")) |>
      tidyr::pivot_wider(
        names_from = "group",
        values_from = c("mid", "min", "max")
      ) |>
      dplyr::mutate(
        mid = (atan2(.data$mid_wd_u, .data$mid_wd_v) * 180 / pi) %% 360,
        min = (atan2(.data$min_wd_u, .data$min_wd_v) * 180 / pi) %% 360,
        max = (atan2(.data$max_wd_u, .data$max_wd_v) * 180 / pi) %% 360,
        group = factor("wd")
      ) |>
      dplyr::select(-dplyr::ends_with(c("_wd_u", "_wd_v")))

    mydata <- mydata |>
      dplyr::filter(!.data$group %in% c("wd_u", "wd_v")) |>
      dplyr::bind_rows(wd_data) |>
      dplyr::mutate(group = factor(.data$group))
  }

  # normalise the pattern by group (i.e., colour) if requested
  if (normalise) {
    mydata <- map_type(mydata, type = "group", fun = function(x) {
      avg <- mean(x$mid, na.rm = TRUE)
      x$mid <- x$mid / avg
      x$min <- x$min / avg
      x$max <- x$max / avg
      x
    })
  }

  # introduce NA gaps for missing x values so lines don't connect across absent
  # data
  gap_cols <- intersect(c(type_cols, "group", "ci"), names(mydata))
  if (is.numeric(mydata$x)) {
    full_x <- switch(
      x,
      "hour" = 0:23,
      "week" = 1:53,
      seq(min(mydata$x, na.rm = TRUE), max(mydata$x, na.rm = TRUE))
    )
    mydata <- tidyr::complete(
      mydata,
      x = full_x,
      tidyr::nesting(!!!rlang::syms(gap_cols))
    )
  } else if (is.factor(mydata$x)) {
    mydata <- tidyr::complete(
      mydata,
      x,
      tidyr::nesting(!!!rlang::syms(gap_cols))
    )
  }

  # set pollutant labels for legend
  poll_labels <- name.pol %||% levels(mydata$group)

  # linetypes
  ltys <- extra.args$linetype
  while (length(ltys) < nlevels(mydata$group)) {
    ltys <- c(ltys, ltys)
  }
  ltys <- ltys[1:nlevels(mydata$group)]

  # construct plot
  thePlot <-
    ggplot2::ggplot(mydata, ggplot2::aes(x = .data$x)) +
    theme_openair(key.position = key.position) +
    set_extra_fontsize(extra.args) +
    get_facet(
      if (all(type == "default")) "default" else type_cols,
      extra.args,
      scales = "fixed",
      auto.text = auto.text,
      drop = FALSE,
      strip.position = strip.position,
      wd.res = extra.args$wd.res %||% 8
    ) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      color = NULL,
      fill = NULL,
      linetype = NULL,
      title = extra.args$title,
      subtitle = extra.args$subtitle,
      caption = extra.args$caption
    ) +
    ggplot2::coord_cartesian(
      ylim = extra.args$ylim
    ) +
    ggplot2::scale_color_manual(
      values = openColours(
        scheme = cols,
        n = nlevels(mydata$group)
      ),
      labels = stats::setNames(
        label_openair(poll_labels, auto_text = auto.text),
        levels(mydata$group)
      ),
      guide = ggplot2::guide_legend(ncol = key.columns),
      aesthetics = c("fill", "colour"),
      drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = ltys,
      labels = stats::setNames(
        label_openair(poll_labels, auto_text = auto.text),
        levels(mydata$group)
      ),
      guide = ggplot2::guide_legend(ncol = key.columns),
      drop = FALSE
    ) +
    ggplot2::guides(
      x = ggplot2::guide_axis(check.overlap = TRUE)
    )

  # add difference line if needed
  if (difference) {
    thePlot <-
      thePlot +
      ggplot2::geom_hline(yintercept = 0, linetype = 2)
  }

  # add geometries
  if (is.numeric(mydata$x) && !(x == "hour" && group == "daylight")) {
    if (x == "hour") {
      xbreaks <- c(seq(0, 20, 4), 23)
    } else if (x == "week") {
      xbreaks <- seq(0, 54, 6)
    }

    if (ci) {
      thePlot <-
        thePlot +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymax = .data$max,
            ymin = .data$min,
            fill = .data$group,
            group = interaction(.data$group, .data$ci)
          ),
          show.legend = FALSE,
          na.rm = TRUE,
          alpha = alpha / dplyr::n_distinct(mydata$ci)
        )
    }

    thePlot <-
      thePlot +
      ggplot2::geom_line(
        ggplot2::aes(
          y = .data$mid,
          colour = .data$group,
          linetype = .data$group
        ),
        show.legend = TRUE,
        na.rm = TRUE,
        key_glyph = "point"
      ) +
      ggplot2::scale_x_continuous(
        breaks = xbreaks,
        expand = ggplot2::expansion()
      )
  } else {
    if (ci) {
      widths <- rev(seq(0.4, 0.8, length.out = length(unique(mydata$ci))))
      splits <- split(mydata, mydata$ci)
      splits <- splits[sort(names(splits))]

      for (i in seq_along(splits)) {
        thePlot <-
          thePlot +
          ggplot2::geom_crossbar(
            data = splits[[i]],
            ggplot2::aes(
              ymax = .data$max,
              ymin = .data$min,
              y = .data$mid,
              fill = .data$group,
              group = interaction(.data$group, .data$ci)
            ),
            show.legend = FALSE,
            color = NA,
            alpha = alpha / dplyr::n_distinct(mydata$ci),
            width = widths[[i]],
            na.rm = TRUE
          )
      }
    }

    # use points for non-ordered factors, but lines for ordered factors (e.g.
    # month) - unless there's an unusual combination of x and group that'd split
    # lines
    use_point <- !is.ordered(mydata$x) ||
      (x == "month" && group == "season") ||
      (x == "weekday" && group == "weekend") ||
      ((mydata |>
        tidyr::drop_na() |>
        dplyr::pull("x") |>
        dplyr::n_distinct()) ==
        1L)

    if (!use_point) {
      thePlot <-
        thePlot +
        ggplot2::geom_line(
          ggplot2::aes(
            y = .data$mid,
            colour = .data$group,
            group = .data$group,
            linetype = .data$group
          ),
          show.legend = TRUE,
          na.rm = TRUE,
          key_glyph = "point"
        )
    } else {
      thePlot <-
        thePlot +
        ggplot2::geom_point(
          ggplot2::aes(
            y = .data$mid,
            colour = .data$group,
            group = .data$group
          ),
          show.legend = TRUE,
          key_glyph = "point",
          na.rm = TRUE
        )
    }

    thePlot <- thePlot +
      ggplot2::scale_x_discrete(
        labels = \(x) label_openair(x, auto_text = auto.text),
        drop = FALSE
      )
  }

  if (plot) {
    plot(thePlot)
  }

  output <- list(
    plot = thePlot,
    data = dplyr::tibble(mydata) |>
      dplyr::mutate(statistic = statistic),
    call = match.call()
  )
  class(output) <- "openair"
  invisible(output)
}

# validate timevar inputs
validate_varplot_inputs <- function(
  mydata,
  group,
  pollutant,
  type,
  statistic,
  conf.int
) {
  if (length(type) > 2) {
    cli::cli_abort(
      "Can only have one {.arg type} for {.fun openair::variationPlot}."
    )
  }

  # validate inputs
  if (any(group != "default") && length(pollutant) > 1) {
    cli::cli_abort(
      "Can only have one {.arg pollutant} and one {.arg group}, or several {.arg pollutant}s and no {.arg group}."
    )
  }

  if (any(type %in% pollutant)) {
    cli::cli_abort(
      "{.arg type} cannot be in {.arg pollutant}. Problem variable: {type[type %in% pollutant]}."
    )
  }

  if (any(group != "default")) {
    if (any(group %in% pollutant)) {
      cli::cli_abort(
        "{.arg group} cannot be in {.arg pollutant}. Problem variable: {group[group %in% pollutant]}."
      )
    }
  }

  # statistic check
  rlang::arg_match(statistic, c("mean", "median"))
}


# sub heading stat info
create_varplot_sub_text <- function(statistic, conf.int) {
  if (statistic == "mean") {
    ci_str <- paste(paste0(100 * conf.int, "%"), collapse = " and ")
    return(paste0("mean and ", ci_str, " confidence interval in mean"))
  }

  if (statistic == "median") {
    quantile_pairs <- sapply(conf.int, \(x) paste0(100 * (1 - x), "/", 100 * x))
    quantile_str <- paste(quantile_pairs, collapse = " and ")
    return(paste0("median and ", quantile_str, "th percentiles"))
  }
}
