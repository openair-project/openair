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
#' @inheritParams shared_openair_params
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
#'   Note that `avg.time` when used in [timeProp()] should be no shorter than the
#'   time gap in the original data. For example, `avg.time = "day"` for hourly
#'   data is OK, but `avg.time = "hour"` for daily data is not, and will give a
#'   warning (or an error if `date` is a `Date`).
#'
#' @param normalise If `normalise = TRUE` then each time interval is scaled to
#'   100. This is helpful to show the relative (percentage) contribution of the
#'   proportions.
#'
#' @param statistic How each category's contribution to a bar is calculated. Only
#'   statistics that add up across categories can be stacked, so the height of
#'   each bar is always meaningful. Can be:
#'
#'   - `"mean"` (the default): the bar shows the mean of `pollutant` over the
#'   time interval. Each category contributes its own mean, weighted by its
#'   share of the valid data.
#'
#'   - `"sum"`: the bar shows the sum of `pollutant` over the time interval, and
#'   each category contributes its own sum. Useful for quantities that
#'   accumulate, such as rainfall or emissions. Note that missing data will
#'   reduce the sum.
#'
#'   - `"frequency"`: the bar shows the number of valid `pollutant` readings in
#'   the time interval, and each category contributes its own number of
#'   readings. With `normalise = TRUE` this is the percentage of time in each
#'   category.
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
#'
#' # monthly percentage of time spent in each wind sector
#' timeProp(
#'   mydata,
#'   pollutant = "so2",
#'   avg.time = "month",
#'   statistic = "frequency",
#'   normalise = TRUE
#' )
timeProp <- function(
  mydata,
  pollutant = "nox",
  proportion = "wd",
  avg.time = "day",
  type = "default",
  cols = "Set1",
  theme = "default",
  normalise = FALSE,
  statistic = "mean",
  ref.x = NULL,
  ref.y = NULL,
  key.columns = NULL,
  key.rows = NULL,
  key.position = "right",
  key.title = proportion,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
  ...
) {
  # default colour based on theme
  if (missing(cols)) {
    cols <- get_theme_cols(cols, theme, "qual")
  }

  check_type(type, max = 2)

  # extra.args setup
  extra.args <- capture_dots(...)

  # check if deprecated key.header / key.footer are being used
  if (uses_key_header(extra.args)) {
    key.title <- check_key_header(key.title, extra.args)
  }

  statistic <- rlang::arg_match(statistic, c("mean", "sum", "frequency"))

  # variables needed
  vars <- c("date", pollutant)

  # check the data
  mydata <- check_prep(
    mydata,
    vars,
    c(type, proportion),
    require_data = TRUE
  ) |>
    check_numeric(vars = pollutant)

  if (
    normalise &&
      statistic != "frequency" &&
      any(mydata[[pollutant]] < 0, na.rm = TRUE)
  ) {
    cli::cli_abort(
      c(
        "x" = "{.code normalise = TRUE} cannot be used when {.field {pollutant}} contains negative values.",
        "i" = "Percentage contributions are not meaningful for data that can be negative."
      )
    )
  }

  check_avg_time(mydata$date, avg.time)

  # cut data
  mydata <- cutData(mydata, c(type, proportion), ...)

  # time zone of input data
  tzone <- attr(mydata$date, "tzone")

  # groups for dplyr
  group_1 <- c("xleft", "xright", type)
  group_2 <- c(type, "xleft", "xright", proportion)

  # calculate the left and right extremes of each bar
  if (avg.time == "season") {
    # seasons always start in Dec, Mar, Jun or Sep and last three months
    results <- mydata |>
      dplyr::mutate(
        xleft = lubridate::floor_date(.data$date, "month") -
          lubridate::period(
            month = (lubridate::month(.data$date) - 12) %% 3
          ),
        xright = add_interval(.data$xleft, "3 month")
      )
  } else {
    # keep the class of the input dates (Date or POSIXct)
    xleft <- cut(mydata$date, avg.time)
    results <-
      dplyr::mutate(
        mydata,
        xleft = if (lubridate::is.Date(.data$date)) {
          as.Date(xleft)
        } else {
          as.POSIXct(xleft, tz = tzone %||% "")
        },
        xright = add_interval(.data$xleft, avg.time)
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
      n = sum(!is.na(.data[[pollutant]])),
      {{ pollutant }} := mean(.data[[pollutant]], na.rm = TRUE),
      mean_value = mean(.data$mean_value, na.rm = TRUE),
      .by = dplyr::all_of(group_2)
    ) |>
    # needs specific arrangement
    dplyr::arrange(
      dplyr::pick(dplyr::all_of(type)),
      .data$xleft,
      .data$xright,
      .data[[proportion]]
    ) |>
    # bar heights: each category's contribution to the interval total
    dplyr::mutate(
      weighted_mean = .data[[pollutant]] * .data$n / sum(.data$n),
      Var1 = switch(
        statistic,
        mean = .data$weighted_mean,
        sum = .data[[pollutant]] * .data$n,
        frequency = .data$n
      ) |>
        tidyr::replace_na(0),
      date = .data$xleft,
      .by = dplyr::all_of(group_1)
    ) |>
    add_stack(by = group_1)

  # normalise to 100 if needed
  if (normalise) {
    results <-
      dplyr::mutate(
        results,
        # intervals with no total (all zero or missing) are left empty
        total = sum(.data$Var1, na.rm = TRUE),
        Var1 = dplyr::if_else(
          .data$total == 0,
          0,
          .data$Var1 * (100 / .data$total)
        ),
        .by = dplyr::all_of(c(type, "date"))
      ) |>
      dplyr::select(-"total") |>
      add_stack(by = c(type, "date"))
  }

  # make sure we know order of data frame for adding other dates
  results <- dplyr::arrange(results, dplyr::pick(dplyr::all_of(type)), .data$date)

  # check key.position
  key.position <- check_key_position(key.position, key)

  # x-axis scale type
  x_type <- if (lubridate::is.Date(results$xleft)) "date" else "datetime"

  # default y-axis label
  ylab_default <- switch(
    statistic,
    mean = if (normalise) paste("% contribution to", pollutant) else pollutant,
    sum = if (normalise) {
      paste("% contribution to sum of", pollutant)
    } else {
      paste("sum of", pollutant)
    },
    frequency = if (normalise) {
      paste("% of", pollutant, "readings")
    } else {
      paste("number of", pollutant, "readings")
    }
  )

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
      colour = extra.args$border[1] %||% "transparent",
      linewidth = extra.args$linewidth[1] %||% 0.25,
      linetype = extra.args$linetype[1] %||% 1
    ) +
    layer_ref(
      ref = ref.x,
      which = "x",
      type = x_type,
      other_type = "numeric",
      tz = lubridate::tz(results$xleft)
    ) +
    layer_ref(
      ref = ref.y,
      which = "y",
      type = "numeric",
      other_type = x_type,
      tz = lubridate::tz(results$xleft)
    ) +
    ggplot2::scale_fill_manual(
      values = resolve_colour_opts(
        cols,
        nlevels(results[[proportion]])
      ),
      breaks = levels(results[[proportion]]),
      labels = \(x) label_openair(x, auto_text = auto.text),
      drop = FALSE
    ) +
    ggplot2::guides(
      fill = guide_openair(
        key.position,
        reverse = TRUE,
        ncol = key.columns,
        nrow = key.rows
      )
    ) +
    scale_x_openair_date(
      date.breaks = date.breaks,
      date.format = date.format,
      type = x_type,
      expand = ggplot2::expansion(c(0, 0)),
      limits = extra.args$xlim
    ) +
    ggplot2::scale_y_continuous(
      limits = extra.args$ylim,
      expand = ggplot2::expansion(if (normalise) c(0, 0) else c(0, 0.1))
    ) +
    labs_openair(
      extra.args,
      auto.text,
      x = "date",
      y = ylab_default,
      caption = if (statistic == "mean") "contribution weighted by mean",
      fill = key.title
    ) +
    theme_openair(
      theme = theme,
      coord = "cartesian",
      key.position,
      extra.args = extra.args
    ) +
    get_facet(
      type,
      extra.args,
      auto.text,
      drop = TRUE,
      wd.res = extra.args$wd.res %||% 8
    )

  # make key full width/height
  thePlot <- thePlot +
    theme_legend_key(key.position, size = ggplot2::rel(2), spacing = NULL)

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

# end of each bar: start of the next `avg.time` interval, so that bars for
# months, quarters, DST days etc. have their true (unequal) widths
add_interval <- function(x, by) {
  starts <- unique(x)
  ends <- purrr::map(starts, \(d) seq(d, by = by, length.out = 2)[2]) |>
    purrr::list_c()
  ends[match(x, starts)]
}

# bar edges for each category within each bar. Positive and negative
# contributions are stacked separately, upwards and downwards from zero, so
# that bars with mixed signs do not overlap
add_stack <- function(data, by) {
  dplyr::mutate(
    data,
    stack_pos = cumsum(pmax(.data$Var1, 0)),
    stack_neg = cumsum(pmin(.data$Var1, 0)),
    var2 = dplyr::if_else(.data$Var1 < 0, .data$stack_neg, .data$stack_pos),
    var2lag = .data$var2 - .data$Var1,
    .by = dplyr::all_of(by)
  ) |>
    dplyr::select(-"stack_pos", -"stack_neg")
}

# `avg.time` must be a valid specification and no finer than the time
# resolution of the data (e.g., not "hour" for daily data), as bars would
# otherwise be thin slivers (or, for Dates, impossible to draw)
check_avg_time <- function(date, avg.time, call = rlang::caller_env()) {
  start <- as.POSIXct(min(date))
  by <- if (identical(avg.time, "season")) "3 month" else avg.time

  end <- tryCatch(seq(start, by = by, length.out = 2)[2], error = \(e) NULL)
  if (is.null(end)) {
    cli::cli_abort(
      c(
        "x" = "{.arg avg.time} must be a valid time interval, not {.val {avg.time}}.",
        "i" = "See {.arg avg.time} in {.fun timeProp} for the options available."
      ),
      call = call
    )
  }

  avg_secs <- as.numeric(end) - as.numeric(start)
  data_secs <- find_time_interval(date, return.seconds = TRUE)

  # tolerance as months and years differ in length
  if (avg_secs < 0.9 * data_secs) {
    msg <- c(
      "x" = "{.arg avg.time} ({.val {avg.time}}) is shorter than the time interval of the data.",
      "i" = "{.arg avg.time} should be at least as long as the time gap in the data; e.g., {.code avg.time = \"day\"} for hourly data is fine, but {.code avg.time = \"hour\"} for daily data is not."
    )
    # Dates cannot be cut into units shorter than a day
    if (inherits(date, "Date") && avg_secs < 86400) {
      cli::cli_abort(msg, call = call)
    }
    cli::cli_warn(msg, call = call)
  }

  invisible(avg.time)
}
