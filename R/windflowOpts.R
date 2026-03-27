#' Define `windflow` options for `openair` plots
#'
#' This function provides a convenient way to set default options for `windflow`
#' layers in `openair` plots, which typically show the mean wind speed and
#' direction as compass arrows. It returns a list of options that can be passed
#' to `layer_windflow()`.
#'
#' @param limits Numeric vector of length 2 specifying the limits for wind
#'   speed. By default, it is set to `c(NA, NA)`, which means the limits will be
#'   determined automatically based on the data.
#'
#' @param range Numeric vector of length 2 specifying the range of possible
#'   sizes of the windflow arrows. The default is broadly appropriate throughout
#'   `openair`, but if plots are beings saved at different resolutions it may be
#'   appropriate to to tweak this.
#'
#' @param arrow A [grid::arrow()] object specifying the appearance of the
#'   arrows.
#'
#' @param lineend The style of line endings. Options include `"butt"`,
#'   `"round"`, and `"square"`. Default is `"butt"`.
#'
#' @param alpha Numeric value between 0 and 1 specifying the transparency of the
#'   lines. Default is 1 (fully opaque).
#'
#' @param colour,color Colour of the lines. Default is `"black"`. `colour` and
#'   `color` are interchangeable, but `colour` is used preferentially if both
#'   are given.
#'
#' @param linetype Line type. Can be an integer (e.g., 1 for solid, 2 for
#'   dashed) or a string (e.g., "solid", "dashed"). Default is 1 (solid).
#'
#' @param linewidth Numeric value specifying the width of the lines. Default is
#'   0.5.
#'
#' @param windflow Logical value indicating whether to include the `windflow`
#'   layer. Default is `TRUE`. Used internally by `openair` functions.
#'
#' @return A list of options that can be passed to the `windflow` argument of
#'   functions like [trendLevel()].
#'
#' @export
#'
#' @examples
#'
#' # `windflow` can be `TRUE` to use defaults
#' trendLevel(mydata, type = "default", cols = "greyscale", windflow = TRUE)
#'
#' # use the `windflowOpts()` function to customise arrows
#' trendLevel(
#'   mydata,
#'   type = "default",
#'   cols = "greyscale",
#'   windflow = windflowOpts(
#'     colour = "white",
#'     alpha = 0.8,
#'     linewidth = 0.25,
#'     linetype = 2
#'   )
#' )
windflowOpts <- function(
  limits = c(NA, NA),
  range = c(0.1, 1),
  arrow = ggplot2::arrow(
    angle = 15,
    length = ggplot2::unit(0.5, "lines"),
    ends = "last",
    type = "closed"
  ),
  lineend = "butt",
  alpha = 1,
  colour = "black",
  linetype = 1,
  linewidth = 0.5,
  color = NULL,
  windflow = TRUE
) {
  if (missing(colour) && !is.null(color)) {
    colour <- color
  }

  list(
    limits = limits,
    range = range,
    arrow = arrow,
    lineend = lineend,
    alpha = alpha,
    colour = colour,
    linetype = linetype,
    linewidth = linewidth,
    windflow = windflow
  )
}

# Function to resolve windflow options, allowing for logical input or a list of
# options, including legacy lattice options
resolve_windflow_opts <- function(x, ...) {
  if (is.null(x)) {
    return(windflowOpts(windflow = FALSE, ...))
  }

  if (rlang::is_logical(x)) {
    if (isTRUE(x)) {
      out <- windflowOpts(windflow = TRUE, ...)
    } else {
      out <- windflowOpts(windflow = FALSE, ...)
    }

    return(out)
  }

  if (!rlang::is_list(x) || !rlang::is_named(x)) {
    cli::cli_abort(
      c(
        "x" = "The {.arg windflow} argument must be either `TRUE`, `FALSE`, or a named list.",
        "i" = "See {.fun openair::windflowOpts} for more detail."
      )
    )
  }

  # warn if any lattice arguments have been used
  lattice_args <- c("col", "lty", "lwd", "scale")
  used_lattice_args <- lattice_args[lattice_args %in% names(x)]
  if (length(used_lattice_args) > 0) {
    cli::cli_warn(
      c(
        "!" = "The following lattice-style arguments were used in the {.arg windflow} list: {used_lattice_args}.",
        "i" = "These have been automatically translated to their {.pkg ggplot2} equivalents, but please update your code to use ggplot2-style arguments (e.g., {.code colour} instead of {.code col}).",
        "i" = "See {.fun openair::windflowOpts} for more detail on the available options."
      )
    )
  }

  # get the defaults, overriding anything function-specific with ...
  # need to do it this way to catch lattice args
  default_opts <- windflowOpts(...)

  if ("scale" %in% names(x)) {
    default_opts$range <- default_opts$range * x$scale
  }

  out <-
    windflowOpts(
      limits = x$limits %||% default_opts$limits,
      range = x$range %||% default_opts$range,
      arrow = x$arrow %||% default_opts$arrow,
      lineend = x$lineend %||% default_opts$lineend,
      alpha = x$alpha %||% default_opts$alpha,
      colour = x$colour %||% x$col %||% default_opts$colour,
      linetype = x$linetype %||% x$lty %||% default_opts$linetype,
      linewidth = x$linewidth %||% x$lwd %||% default_opts$linewidth,
      windflow = x$windflow %||% default_opts$windflow
    )

  out
}

# Function to create a windflow layer using the resolved options
layer_windflow_opts <- function(data, windflow_opts) {
  layer_windflow(
    data = data,
    ggplot2::aes(ws = .data$ws, wd = .data$wd),
    limits = windflow_opts$limits,
    range = windflow_opts$range,
    arrow = windflow_opts$arrow,
    lineend = windflow_opts$lineend,
    alpha = windflow_opts$alpha,
    colour = windflow_opts$colour,
    linetype = windflow_opts$linetype,
    linewidth = windflow_opts$linewidth
  )
}
