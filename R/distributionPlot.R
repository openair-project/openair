#' Plot the distribution of a variable with conditioning
#'
#' This function plots the distribution of a pollutant or other variable as
#' either a histogram or a kenel density estimate function. The former is often
#' more easily interpretable and shows the 'real data' (albeit binned), but can
#' easily get cluttered with lots of different groups and is heavily dependent
#' on your choice of bin width. The latter appears 'cleaner' with many
#' overlapping groups, but can be more challenging to interpret and represents a
#' smoothed density function which may extend beyond the bounds of the input
#' data.
#'
#' @inheritParams shared_openair_params
#' @inheritParams timePlot
#'
#' @param mydata A data frame to plot.
#'
#' @param pollutant Name of the pollutant(s) to plot contained in `mydata`.
#'
#' @param method One of `"histogram"` (the default) or `"density"`.
#'
#' @param binwidth,bins Used when `method = "histogram"`. `binwidth` sets the
#'   width of the bins. `bins` sets teh number of bins, defaulting to `30`.
#'   `bins` is overriden by `binwidth`. This has no effect wehn `method =
#'   "density"`.
#'
#' @param position A string representing a `ggplot2` "position" - see
#'   [ggplot2::position_identity()] and similar functions. When `NULL`, will use
#'   `"stack"` for histograms and `"identity"` for density functions. Also
#'   useful is `"fill"` in conjunction with the `group` argument which will
#'   'normalise' the y-axis to show a percentage rather than an absolute count
#'   or density estimate.
#'
#' @export
#' @return an [openair][openair-package] object
#' @author Jack Davison
#' @examples
#' distributionPlot(mydata, pollutant = "no2", group = "season")
#'
#' \dontrun{
#' distributionPlot(
#'   mydata,
#'   pollutant = "no2",
#'   group = "weekend",
#'   method = "density",
#'   cols = "tol"
#' )
#'
#' distributionPlot(
#'   mydata,
#'   pollutant = "no2",
#'   group = "wd",
#'   position = "fill",
#'   wd.res = 4,
#'   alpha = 0.75,
#'   cols = "tol"
#' )
#' }
distributionPlot <- function(
  mydata,
  pollutant = "nox",
  method = c("histogram", "density"),
  binwidth = NULL,
  bins = NULL,
  position = NULL,
  group = "default",
  type = "default",
  cols = "brewer1",
  theme = "default",
  key.title = group,
  key.position = "right",
  log.x = FALSE,
  log.y = FALSE,
  ref.x = NULL,
  ref.y = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  method <- rlang::arg_match(method, multiple = FALSE)

  if (length(pollutant) > 1 && group != "default") {
    cli::cli_abort(
      "In {.fun openair::distributionPlot}, cannot use {.arg group} and \\
      have more than one {.arg pollutant}."
    )
  }

  # default colour based on theme
  if (missing(cols)) {
    cols <- get_theme_cols(cols, theme, "qual")
  }

  # extra.args setup
  extra.args <- capture_dots(...)

  # labels
  extra.args$ylab <- quickText(
    extra.args$ylab %||%
      dplyr::case_when(
        position == "fill" ~ "Proportion",
        method == "density" ~ "Density",
        .default = "Count"
      ),
    auto.text
  )
  extra.args$xlab <- quickText(
    extra.args$xlab %||% paste(pollutant, collapse = ", "),
    auto.text
  )
  extra.args$title <- quickText(extra.args$title %||% "", auto.text)
  extra.args$subtitle <- quickText(extra.args$subtitle %||% "", auto.text)
  extra.args$caption <- quickText(extra.args$caption %||% "", auto.text)
  extra.args$tag <- quickText(extra.args$tag, auto.text)

  extra.args$linetype <- extra.args$linetype %||% 1
  extra.args$linewidth <- extra.args$linewidth %||% 0.75

  # check & cut data
  vars <- c(pollutant)

  if (any(c(type, group) %in% dateTypes)) {
    vars <- c(vars, "date")
  }
  if (!any(type %in% dateTypes)) {
    vars <- c(vars, type[!type %in% dateTypes])
  }
  if (!group %in% dateTypes) {
    vars <- c(vars, group)
  }
  vars <- vars[vars != "default"]

  # data checks
  # need special handling of cutData as `drop` needs to be passed separately
  mydata <- checkPrep(mydata, vars, c(type, group), remove.calm = FALSE) |>
    cutData(type = type, names = "type", ...) |>
    cutData(type = group, names = "group", ...)

  # standardise column names based on what is being grouped by
  if (group == "default") {
    mydata$group <- NULL
    mydata <- tidyr::pivot_longer(
      mydata,
      cols = dplyr::all_of(pollutant),
      names_to = "group",
      values_to = "x"
    ) |>
      dplyr::mutate(
        group = factor(.data[["group"]], levels = pollutant)
      )
    if (missing(key.title)) {
      key.title <- NULL
    }
  } else {
    names(mydata)[names(mydata) == pollutant] <- "x"
  }

  if (method == "histogram") {
    geom_dist <- ggplot2::geom_histogram(
      ggplot2::aes(
        color = .data$group,
        fill = ggplot2::after_scale(ggplot2::alpha(
          .data$colour,
          extra.args$alpha %||% 0.25
        ))
      ),
      position = position %||% "stack",
      linewidth = extra.args$linewidth,
      linetype = extra.args$linetype,
      bins = bins,
      binwidth = binwidth,
      na.rm = TRUE,
      show.legend = dplyr::n_distinct(mydata$group) > 1
    )
  }

  if (method == "density") {
    geom_dist <- ggplot2::geom_density(
      ggplot2::aes(
        color = .data$group,
        fill = ggplot2::after_scale(ggplot2::alpha(
          .data$colour,
          extra.args$alpha %||% 0.25
        ))
      ),
      position = position %||% "identity",
      linewidth = extra.args$linewidth,
      linetype = extra.args$linetype,
      trim = FALSE,
      na.rm = TRUE,
      show.legend = dplyr::n_distinct(mydata$group) > 1
    )
  }

  thePlot <-
    ggplot2::ggplot(mydata, ggplot2::aes(x = .data$x)) +
    geom_dist +
    theme_openair(
      theme = theme,
      coord = "cartesian",
      key.position = key.position,
      extra.args = extra.args
    ) +
    coord_cartesian(
      xlim = extra.args$xlim,
      ylim = extra.args$ylim
    ) +
    scale_y_continuous(
      expand = ggplot2::expansion(c(
        0,
        ifelse(position %||% "default" == "fill", 0, 0.1)
      )),
      transform = ifelse(log.y, "log10", "identity"),
      labels = if (position %||% "default" == "fill") {
        scales::label_percent()
      } else {
        scales::label_comma()
      }
    ) +
    scale_x_continuous(
      expand = ggplot2::expansion(),
      transform = ifelse(log.x, "log10", "identity")
    ) +
    layer_ref(ref = ref.x, which = "x", type = "numeric") +
    layer_ref(ref = ref.y, which = "y", type = "numeric") +
    get_facet(
      type = ifelse(type == "default", "default", "type"),
      extra.args = extra.args,
      auto.text = auto.text,
      drop = FALSE,
      wd.res = extra.args$wd.res %||% 8
    ) +
    labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      color = quickText(key.title, auto.text),
      title = extra.args$title,
      subtitle = extra.args$subtitle,
      caption = extra.args$caption,
      tag = extra.args$tag
    ) +
    ggplot2::scale_color_manual(
      values = resolve_colour_opts(
        cols,
        dplyr::n_distinct(mydata$group)
      ),
      breaks = levels(mydata$group),
      labels = \(x) label_openair(x, auto_text = auto.text),
      drop = FALSE
    )

  # outputs
  if (plot) {
    plot(thePlot)
  }

  output <- list(
    plot = thePlot,
    data = mydata,
    call = match.call()
  )

  invisible(output)
}
