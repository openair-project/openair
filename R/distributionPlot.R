#' Plot the distribution of a variable with conditioning
#'
#' This function plots the distribution of one or more pollutants or other
#' variables as a histogram, kernel density estimate function, or empirical
#' cumulative distribution function. The choice of method comes with trade-offs;
#' for example, histograms are often more easily interpretable but can easily
#' get cluttered and are heavily dependent on bin width, whereas density
#' functions appear 'cleaner' with many overlapping groups, but can be more
#' challenging to interpret.
#'
#' @inheritParams shared_openair_params
#'
#' @param mydata A data frame.
#'
#' @param pollutant Name of the pollutant(s) to plot contained in `mydata`.
#'
#' @param group This sets the grouping variable to be used. For example, if a
#'   data frame had a column `site` setting `group = "site"` will plot all sites
#'   together in each panel. Passed to [cutData()].
#'
#' @param log Should the x-axis appear on a log scale? The default is `FALSE`.
#'   If `TRUE` a well-formatted log10 scale is used.
#'
#' @param method One of: `"histogram"`, `"freqpoly"`, `"density"`, or `"ecdf"`.
#'   Note that `"freqpoly"` is effectively a line chart equivalent of a
#'   histogram, and may appear less cluttered with many groups.
#'
#' @param binwidth,bins Used when `method = "histogram"` or `"freqpoly"`.
#'   `binwidth` sets the width of the bins. `bins` sets the number of bins,
#'   defaulting to `30`. `bins` is overridden by `binwidth`.
#'
#' @param position A string representing a `ggplot2` "position" - see
#'   [ggplot2::position_identity()] and similar functions. When `NULL`, will use
#'   `"stack"` for histograms and `"identity"` for other methods. Also useful is
#'   `"fill"` in conjunction with the `group` argument which will 'normalise'
#'   the y-axis to show a percentage rather than an absolute count or density
#'   estimate. Not used when `method = "ecdf"`, which must be `"identity"`. Note
#'   that density functions will use 'count' over 'density' for non-identity
#'   `position`s.
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
  method = c("histogram", "freqpoly", "density", "ecdf"),
  binwidth = NULL,
  bins = 30,
  position = NULL,
  log = FALSE,
  group = "default",
  type = "default",
  cols = "hue",
  theme = "default",
  key.title = group,
  key.position = "top",
  ref.x = NULL,
  ref.y = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  method <- rlang::arg_match(method, multiple = FALSE)
  if (!is.null(position)) {
    position <- rlang::arg_match(
      position,
      values = c("identity", "stack", "fill", "dodge"),
      multiple = FALSE
    )
  }

  if (length(pollutant) > 1 && length(type) > 2) {
    cli::cli_abort(
      "In {.fun openair::distributionPlot}, cannot have more than one \
      {.arg pollutant} and have two {.arg type}s."
    )
  }

  # restrictions to certain parameters
  if (method == "ecdf") {
    position <- "identity"
  }
  if (method == "density" && position == "dodge") {
    position <- "identity"
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
        method == "ecdf" ~ "ECDF",
        (position %||% "identity") == "fill" ~ "Proportion",
        (method == "density" &&
          (position %||% "identity") == "identity") ~ "Density",
        .default = "Count"
      ),
    auto.text
  )
  extra.args$xlab <- quickText(
    extra.args$xlab %||% paste(pollutant, collapse = ", "),
    auto.text
  )
  extra.args$title <- quickText(extra.args$title, auto.text)
  extra.args$subtitle <- quickText(extra.args$subtitle, auto.text)
  extra.args$caption <- quickText(extra.args$caption, auto.text)
  extra.args$tag <- quickText(extra.args$tag, auto.text)

  extra.args$linetype <- extra.args$linetype %||% 1
  extra.args$linewidth <- extra.args$linewidth %||% 0.5

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
    tidyr::pivot_longer(
      cols = dplyr::all_of(pollutant),
      names_to = "pollutant",
      values_to = "x"
    ) |>
    dplyr::mutate(pollutant = factor(.data$pollutant, levels = {{ pollutant }}))

  # if there's more than one pollutant, it needs adding to 'type'
  if (length(pollutant) > 1L) {
    type <- c("pollutant", type)
  }

  # how many 'types' are we working with?
  type_cols <- "type"
  if (length(type) == 2) {
    type_cols <- c("type_y", "type_x")
  }
  if (length(type) > 2) {
    cli::cli_abort("Only a maximum of two {.arg type}s can be used.")
  }

  # if group is default, colour by pollutant
  if (group == "default") {
    group <- "pollutant"
    if (missing(key.title)) key.title <- NULL
  }

  # cut data to prep columns
  mydata <-
    mydata |>
    cutData(type = type, names = type_cols, ...) |>
    cutData(type = group, names = "group", ...) |>
    dplyr::select(-"pollutant")

  # geometries
  if (method == "histogram") {
    if ("border" %in% names(extra.args)) {
      geom_dist <- ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$group),
        color = extra.args$border,
        alpha = extra.args$alpha %||% 0.25,
        position = (position %||% "stack"),
        linewidth = extra.args$linewidth,
        linetype = extra.args$linetype,
        bins = bins,
        binwidth = binwidth,
        na.rm = TRUE,
        show.legend = dplyr::n_distinct(mydata$group) > 1
      )
    } else {
      geom_dist <- ggplot2::geom_histogram(
        ggplot2::aes(
          color = .data$group,
          fill = ggplot2::after_scale(ggplot2::alpha(
            .data$colour,
            extra.args$alpha %||% 0.25
          ))
        ),
        position = (position %||% "stack"),
        linewidth = extra.args$linewidth,
        linetype = extra.args$linetype,
        bins = bins,
        binwidth = binwidth,
        na.rm = TRUE,
        show.legend = dplyr::n_distinct(mydata$group) > 1
      )
    }
  }

  if (method == "density") {
    position <- position %||% "identity"

    if ("border" %in% names(extra.args)) {
      geom_dist <- ggplot2::geom_density(
        mapping = ggplot2::aes(
          y = ggplot2::after_stat(.data[[ifelse(
            position %in% c("stack", "fill"),
            "density",
            "count"
          )]]),
          fill = .data$group
        ),
        color = extra.args$border,
        alpha = extra.args$alpha %||% 0.25,
        position = (position %||% "identity"),
        linewidth = extra.args$linewidth,
        linetype = extra.args$linetype,
        trim = FALSE,
        na.rm = TRUE,
        show.legend = dplyr::n_distinct(mydata$group) > 1
      )
    } else {
      geom_dist <- ggplot2::geom_density(
        mapping = ggplot2::aes(
          y = ggplot2::after_stat(.data[[ifelse(
            position %in% c("stack", "fill"),
            "density",
            "count"
          )]]),
          color = .data$group,
          fill = ggplot2::after_scale(ggplot2::alpha(
            .data$colour,
            extra.args$alpha %||% 0.25
          ))
        ),
        position = (position %||% "identity"),
        linewidth = extra.args$linewidth,
        linetype = extra.args$linetype,
        trim = FALSE,
        na.rm = TRUE,
        show.legend = dplyr::n_distinct(mydata$group) > 1
      )
    }
  }

  if (method == "freqpoly") {
    geom_dist <- ggplot2::geom_freqpoly(
      ggplot2::aes(
        color = .data$group
      ),
      position = (position %||% "identity"),
      linewidth = extra.args$linewidth,
      linetype = extra.args$linetype,
      lineend = extra.args$lineend %||% "butt",
      linejoin = extra.args$linejoin %||% "round",
      linemitre = extra.args$linemitre %||% 10,
      bins = bins,
      binwidth = binwidth,
      na.rm = TRUE,
      show.legend = dplyr::n_distinct(mydata$group) > 1
    )
  }

  if (method == "ecdf") {
    geom_dist <- ggplot2::stat_ecdf(
      ggplot2::aes(
        color = .data$group
      ),
      position = "identity",
      linewidth = extra.args$linewidth,
      linetype = extra.args$linetype,
      lineend = extra.args$lineend %||% "butt",
      linejoin = extra.args$linejoin %||% "round",
      linemitre = extra.args$linemitre %||% 10,
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
    ggplot2::coord_cartesian(
      xlim = extra.args$xlim,
      ylim = extra.args$ylim
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(
        0,
        ifelse((position %||% "default") == "fill", 0, 0.1)
      )),
      labels = if ((position %||% "default") == "fill") {
        scales::label_percent()
      } else {
        scales::label_comma()
      }
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(),
      transform = ifelse(log, "log10", "identity")
    ) +
    layer_ref(ref = ref.x, which = "x", type = "numeric") +
    layer_ref(ref = ref.y, which = "y", type = "numeric") +
    get_facet(
      type = if (all(type == "default")) {
        "default"
      } else {
        type_cols[type != "default"]
      },
      extra.args = extra.args,
      auto.text = auto.text,
      drop = FALSE,
      wd.res = extra.args$wd.res %||% 8
    ) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
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
      name = quickText(key.title, auto.text),
      breaks = levels(mydata$group),
      labels = \(x) label_openair(x, auto_text = auto.text),
      drop = FALSE,
      aesthetics = c("colour", "fill")
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
