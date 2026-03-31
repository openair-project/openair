#' Polar plots considering changes in concentrations between two time periods
#'
#' This function provides a way of showing the differences in concentrations
#' between two time periods as a polar plot. There are several uses of this
#' function, but the most common will be to see how source(s) may have changed
#' between two periods.
#'
#' While the function is primarily intended to compare two time periods at the
#' same location, it can be used for any two data sets that contain the same
#' pollutant. For example, data from two sites that are close to one another, or
#' two co-located instruments.
#'
#' The analysis works by calculating the polar plot surface for the `before` and
#' `after` periods and then subtracting the `before` surface from the `after`
#' surface.
#'
#' @inheritParams polarPlot
#' @param before,after Data frames representing the "before" and "after" cases.
#'   See [polarPlot()] for details of different input requirements.
#' @inheritDotParams polarPlot -mydata -pollutant -x -limits -plot -type
#' @family polar directional analysis functions
#' @return an [openair][openair-package] plot.
#' @export
#'
#' @examples
#' \dontrun{
#' before_data <- selectByDate(mydata, year = 2002)
#' after_data <- selectByDate(mydata, year = 2003)
#'
#' polarDiff(before_data, after_data, pollutant = "no2")
#'
#' # with some options
#' polarDiff(
#'   before_data,
#'   after_data,
#'   pollutant = "no2",
#'   cols = "RdYlBu",
#'   limits = c(-20, 20)
#' )
#' }
polarDiff <- function(
  before,
  after,
  pollutant = "nox",
  type = "default",
  x = "ws",
  limits = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  # extra args setup
  Args <- list(...)

  # variables needed, check for York regression where x and y error needed
  vars <- if (all(c("x_error", "y_error") %in% names(Args))) {
    c(x, "wd", pollutant, Args$x_error, Args$y_error)
  } else {
    c(x, "wd", pollutant)
  }

  # collapse multi-pollutant name once, used throughout
  pollutant_name <- paste(pollutant, collapse = "_")

  # prepare before/after, tagging each with a period label
  prepare <- function(data, period) {
    data <- cutData(data, type = type) |>
      checkPrep(vars, type, remove.calm = FALSE) |>
      dplyr::mutate(period = period)
    if (type == "default") {
      data$default <- "default"
    }
    data
  }

  before <- prepare(before, "before")
  after <- prepare(after, "after")

  all_data <- dplyr::bind_rows(before, after)

  # map over type levels - subtract 'before' surface from 'after' surface
  polar_data <-
    purrr::map(
      .x = unique(c(before[[type]], after[[type]])),
      .f = function(i) {
        theData <- all_data[all_data[[type]] == i, ]

        polar_plt <- polarPlot(
          theData,
          pollutant = pollutant,
          x = x,
          type = "period",
          plot = FALSE,
          ...
        )

        out <-
          polar_plt$data |>
          tidyr::pivot_wider(
            id_cols = "u":"v",
            names_from = "period",
            values_from = "z"
          ) |>
          dplyr::mutate(
            {{ pollutant_name }} := .data$after - .data$before,
            {{ x }} := (.data$u^2 + .data$v^2)^0.5,
            wd = 180 * atan2(.data$u, .data$v) / pi,
            wd = ifelse(.data$wd < 0, .data$wd + 360, .data$wd)
          )

        out[[type]] <- theData[[type]][1]
        out
      }
    ) |>
    purrr::list_rbind()

  # resolve limits: user-supplied or symmetric pretty range
  resolved_limits <- if (is.null(limits)) {
    lims_adj <- pretty(seq(
      0,
      max(abs(polar_data[[pollutant_name]]), na.rm = TRUE),
      5
    ))
    lims_val <- lims_adj[length(lims_adj) - 1]
    c(-lims_val, lims_val)
  } else {
    limits
  }

  # build final polarPlot args, merging defaults with user-supplied Args
  plot_args <- utils::modifyList(
    list(
      cols = c(
        "#002F70",
        "#3167BB",
        "#879FDB",
        "#C8D2F1",
        "#F6F6F6",
        "#F4C8C8",
        "#DA8A8B",
        "#AE4647",
        "#5F1415"
      ),
      key.position = "none",
      par.settings = list(axis.line = list(col = "black")),
      alpha = 1,
      key.title = paste("Difference", pollutant_name, sep = " "),
      main = ""
    ),
    Args[intersect(
      names(Args),
      c(
        "cols",
        "key.position",
        "par.settings",
        "alpha",
        "key.title",
        "main"
      )
    )]
  )

  # for non-default types, rename type column to avoid polarPlot date coercion
  if (type != "default") {
    names(polar_data)[names(polar_data) == type] <- "finaltype"
    plot_args$type <- "finaltype"
  }

  plt <- do.call(
    polarPlot,
    c(
      list(
        mydata = polar_data,
        pollutant = pollutant,
        x = x,
        limits = resolved_limits,
        force.positive = FALSE,
        plot = FALSE,
        auto.text = auto.text
      ),
      plot_args
    )
  )

  if (plot) {
    plot(plt$plot)
  }

  out <- list(plot = plt$plot, data = polar_data, call = match.call())
  class(out) <- "openair"
  invisible(out)
}
