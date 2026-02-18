#' Time series plot with categories shown as a stacked bar chart
#'
#' This function shows time series plots as stacked bar charts. The different
#' categories in the bar chart are made up from a character or factor variable
#' in a data frame. The function is primarily developed to support the plotting
#' of cluster analysis output from [polarCluster()] and [trajCluster()] that
#' consider local and regional (back trajectory) cluster analysis respectively.
#' However, the function has more general use for understanding time series
#' data.
#'
#' In order to plot time series in this way, some sort of time aggregation is
#' needed, which is controlled by the option `avg.time`.
#'
#' The plot shows the value of `pollutant` on the y-axis (averaged according to
#' `avg.time`). The time intervals are made up of bars split according to
#' `proportion`. The bars therefore show how the total value of `pollutant` is
#' made up for any time interval.
#'
#' @inheritParams timePlot
#'
#' @param mydata A data frame containing the fields `date`, `pollutant` and a
#'   splitting variable `proportion`
#'
#' @param pollutant Name of the pollutant to plot contained in `mydata`.
#'
#' @param proportion The splitting variable that makes up the bars in the bar
#'   chart, defaulting to `"wd"`. Could be `"cluster"` if the output from
#'   [polarCluster()] or [trajCluster()] is being analysed. If `proportion` is a
#'   numeric variable it is split into 4 quantiles (by default) by [cutData()].
#'   If `proportion` is a factor or character variable then the categories are
#'   used directly.
#'
#' @param avg.time This defines the time period to average to. Can be `"sec"`,
#'   `"min"`, `"hour"`, `"day"`, `"DSTday"`, `"week"`, `"month"`, `"quarter"` or
#'   `"year"`. For much increased flexibility a number can precede these options
#'   followed by a space. For example, an average of 2 months would be `avg.time
#'   = "2 month"`. In addition, `avg.time` can equal `"season"`, in which case
#'   3-month seasonal values are calculated with spring defined as March, April,
#'   May and so on.
#'
#'   Note that `avg.time` when used in `timeProp` should be greater than the
#'   time gap in the original data. For example, `avg.time = "day"` for hourly
#'   data is OK, but `avg.time = "hour"` for daily data is not.
#'
#' @param normalise If `normalise = TRUE` then each time interval is scaled to
#'   100. This is helpful to show the relative (percentage) contribution of the
#'   proportions.
#'
#' @param key.title The title of the key.
#'
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available:
#'   - `xlab`, `ylab` and `main` override the x-axis label, y-axis label, and plot title.
#'   - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'   - `fontsize` overrides the overall font size of the plot.
#'   - `border` sets the border colour of each bar.
#'
#' @export
#' @return an [openair][openair-package] object
#' @author David Carslaw
#' @author Jack Davison
#' @family time series and trend functions
#' @family cluster analysis functions
#' @examples
#' # monthly plot of SO2 showing the contribution by wind sector
#' timeProp(mydata, pollutant = "so2", avg.time = "month", proportion = "wd")
timeProp <- function(
  mydata,
  pollutant = "nox",
  proportion = "wd",
  avg.time = "day",
  type = "default",
  cols = "Set1",
  normalise = FALSE,
  x.relation = "same",
  y.relation = "same",
  key = TRUE,
  key.columns = 1,
  key.position = "right",
  key.title = proportion,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  # extra.args setup
  extra.args <- list(...)

  # label controls
  main <- quickText(extra.args$main %||% "", auto.text)
  xlab <- quickText(extra.args$xlab %||% "date", auto.text)
  ylab <- quickText(
    extra.args$ylab %||%
      ifelse(normalise, paste("% contribution to", pollutant), pollutant),
    auto.text
  )
  sub <- extra.args$sub %||% "contribution weighted by mean"

  # variables needed
  vars <- c("date", pollutant)

  # check the data
  mydata <- checkPrep(mydata, vars, c(type, proportion), remove.calm = FALSE)

  # cut data
  mydata <- cutData(mydata, c(type, proportion), ...)

  # time zone of input data
  tzone <- attr(mydata$date, "tzone")

  # groups for dplyr
  group_1 <- c("xleft", "xright", type)
  group_2 <- c(type, "xleft", "xright", proportion)

  # calculate left and right extremes of each bar, add the most common
  # non-zero time interval to left to get right
  if (avg.time == "season") {
    if (any(c("season", "seasonyear") %in% type)) {
      cli::cli_abort(
        "In {.fun openair::timeProp}, {.arg type} and {.arg avg.time} cannot both be 'season'."
      )
    }
    results <- mydata |>
      cutData(type = "seasonyear") |>
      dplyr::mutate(xleft = min(.data$date), .by = c("seasonyear", type)) |>
      dplyr::mutate(
        xright = .data$xleft + median(diff(.data$xleft)[diff(.data$xleft) != 0])
      ) |>
      dplyr::select(-dplyr::any_of("seasonyear"))
  } else {
    results <-
      dplyr::mutate(
        mydata,
        xleft = as.POSIXct(cut(.data$date, avg.time), tz = tzone),
        xright = .data$xleft + median(diff(.data$xleft)[diff(.data$xleft) != 0])
      )
  }

  # summarise by proportion, type etc
  results <-
    results |>
    # calculate group mean
    dplyr::mutate(
      mean_value = mean(.data[[pollutant]], na.rm = TRUE),
      .by = dplyr::all_of(group_1)
    ) |>
    # calculate mean & count per type & pollutant, retain type mean
    dplyr::summarise(
      {{ pollutant }} := mean(.data[[pollutant]], na.rm = TRUE),
      mean_value = mean(.data$mean_value, na.rm = TRUE),
      n = dplyr::n(),
      .by = dplyr::all_of(group_2)
    ) |>
    # needs specific arrangement for lattice
    dplyr::arrange(
      dplyr::pick(dplyr::all_of(type)),
      .data$xleft,
      .data$xright,
      .data[[proportion]]
    ) |>
    # weighted mean, with cumulative sum for bar heights
    dplyr::mutate(
      weighted_mean = .data[[pollutant]] * n / sum(n),
      Var1 = tidyr::replace_na(.data$weighted_mean, 0),
      var2 = cumsum(.data$Var1),
      var2lag = dplyr::lag(.data$var2, default = 0),
      date = .data$xleft,
      .by = dplyr::all_of(group_1)
    )

  # normalise to 100 if needed
  if (normalise) {
    results <-
      dplyr::mutate(
        results,
        Var1 = .data$Var1 * (100 / sum(.data$Var1, na.rm = TRUE)),
        var2 = cumsum(.data$Var1),
        var2lag = dplyr::lag(.data$var2, default = 0),
        .by = dplyr::all_of(c(type, "date"))
      )
  }

  # make sure we know order of data frame for adding other dates
  results <- dplyr::arrange(results, dplyr::pick(dplyr::all_of(type)), "date")

  # set limits, if not set by user
  xlim <- extra.args$xlim %||% NULL
  ylim <- extra.args$ylim %||% NULL

  # set up key, if required
  if (!key) {
    key.position = "none"
  }

  # x-axis scale function
  if (lubridate::is.Date(results$xleft)) {
    x_scale_fun <- ggplot2::scale_x_date
  } else {
    x_scale_fun <- ggplot2::scale_x_datetime
  }

  # plot
  thePlot <-
    ggplot2::ggplot(
      results,
      ggplot2::aes(
        xmin = .data[["xleft"]],
        xmax = .data[["xright"]],
        ymin = .data[["var2"]],
        ymax = .data[["var2lag"]],
        fill = .data[[proportion]]
      )
    ) +
    ggplot2::geom_rect(
      show.legend = TRUE,
      colour = extra.args$border %||% "transparent"
    ) +
    ggplot2::scale_fill_manual(
      values = openColours(cols, dplyr::n_distinct(results[[proportion]])),
      breaks = levels(results[[proportion]]),
      labels = \(x) label_openair(x, auto_text = auto.text),
      drop = FALSE
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        reverse = TRUE,
        theme = ggplot2::theme(
          legend.title.position = ifelse(
            key.position %in% c("left", "right"),
            "top",
            key.position
          ),
          legend.text.position = ifelse(
            key.position %in% c("top", "bottom"),
            "right",
            key.position
          )
        ),
        ncol = if (key.position %in% c("left", "right")) {
          NULL
        } else {
          dplyr::n_distinct(results[[proportion]])
        }
      )
    ) +
    x_scale_fun(
      breaks = scales::breaks_pretty(date.breaks),
      date_labels = date.format %||% ggplot2::waiver(),
      limits = xlim,
      expand = ggplot2::expansion()
    ) +
    ggplot2::scale_y_continuous(
      limits = ylim,
      expand = ggplot2::expansion(if (normalise) c(0, 0) else c(0, 0.1))
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = main,
      caption = sub,
      fill = quickText(key.title, auto.text = auto.text)
    ) +
    theme_openair(key.position) +
    set_extra_fontsize(extra.args) +
    get_facet(
      type,
      extra.args,
      scales = relation_to_facet_scales(x.relation, y.relation),
      auto.text,
      drop = TRUE
    )

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

  output <- list(
    plot = thePlot,
    data = results,
    call = match.call()
  )
  class(output) <- "openair"
  invisible(output)
}
