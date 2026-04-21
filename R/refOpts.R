#' Define `ref.x` or `ref.y` options for `openair` plots
#'
#' This function provides a convenient way to set default options for `ref.x` or
#' `ref.y` layers in `openair` plots, which show some form of horizontal or
#' vertical reference line. `intercept` can be a vector of any length; all other
#' arguments will be recycled to be equal to that length.
#'
#' @param intercept The axis intercept(s) for the reference lines. Should be
#'   numeric, dates, or date-times depending on the axis types. If another data
#'   type is provided, it will attempt to be coerced to the correct type using
#'   [as.numeric()], [lubridate::as_date()] or [lubridate::as_datetime()],
#'   respectively.
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
#'   1.
#'
#' @return A list of options that can be passed to the `ref.x` or `ref.y`
#'   arguments of functions like [timePlot()].
#'
#' @export
#'
#' @examples
#' # `ref.y` can just be a value to plot
#' timePlot(mydata, avg.time = "month", ref.y = 250, ref.x = "2002/01/01")
#'
#' # use the `refOpts()` function to customise reference lines
#' timePlot(
#'   mydata,
#'   avg.time = "month",
#'   ref.y = refOpts(
#'     c(250, 300),
#'     alpha = c(0.5, 1),
#'     colour = c("grey50", "blue"),
#'     linetype = c(2, 1),
#'     linewidth = c(1, 2)
#'   )
#' )
refOpts <- function(
  intercept,
  alpha = 1,
  colour = "black",
  linetype = 1,
  linewidth = 1,
  color = NULL
) {
  if (missing(colour) && !is.null(color)) {
    colour <- color
  }
  list(
    intercept = intercept,
    alpha = alpha,
    colour = colour,
    linetype = linetype,
    linewidth = linewidth
  )
}

# Convert refOpts (or just values) to ggplot2 layers
layer_ref <- function(
  ref,
  which = c("y", "x"),
  type = c("numeric", "datetime", "date"),
  tz = NULL
) {
  # if ref is missing, do nothing
  if (is.null(ref)) {
    return(NULL)
  }

  # if ref is just a value, use that as the intercept
  if (!is.list(ref)) {
    ref <- list(intercept = ref)
  }

  # look for intercept, v and h (latter two legacy values)
  intercept <- ref$intercept %||% ref$h %||% ref$v
  n <- length(intercept)

  if (type == "numeric" && !is.numeric(intercept)) {
    intercept_original <- intercept
    intercept <- as.numeric(intercept)
    if (anyNA(intercept)) {
      cli::cli_abort(
        "{.arg {paste0(which, '.ref')}} expects {.type {1}} but wasn't able \\
        to coerce the given intercept ({.type {intercept_original}}) to \\
        {.type {1}}."
      )
    }
  }

  if (type == "datetime" && !lubridate::is.POSIXct(intercept)) {
    intercept_original <- intercept
    intercept <- lubridate::as_datetime(intercept, tz = tz)
    if (anyNA(intercept)) {
      cli::cli_abort(
        "{.arg {paste0(which, '.ref')}} expects {.type {Sys.time()}} but \\
        wasn't able to coerce the given intercept \\
        ({.type {intercept_original}}) to {.type {Sys.time()}}."
      )
    }
  }

  if (type == "date" && !lubridate::is.Date(intercept)) {
    intercept_original <- intercept
    intercept <- lubridate::as_date(intercept)
    if (anyNA(intercept)) {
      cli::cli_abort(
        "{.arg {paste0(which, '.ref')}} expects {.type {Sys.Date()}} but \\
        wasn't able to coerce the given intercept \\
        ({.type {intercept_original}}) to {.type {Sys.Date()}}."
      )
    }
  }

  # recycle aesthetics if needed
  alpha <- recycle_to_length(ref$alpha %||% 1, n)
  colour <- recycle_to_length(ref$colour %||% ref$col %||% "black", n)
  linetype <- recycle_to_length(ref$linetype %||% ref$lty %||% 1, n)
  linewidth <- recycle_to_length(ref$linewidth %||% ref$lwd %||% 0.5, n)

  # choose appropriate function
  if (which == "x") {
    fun <- \(intercept, ...) {
      ggplot2::geom_vline(xintercept = intercept, ..., inherit.aes = FALSE)
    }
  } else if (which == "y") {
    fun <- \(intercept, ...) {
      ggplot2::geom_hline(yintercept = intercept, ..., inherit.aes = FALSE)
    }
  }

  # Build list of geoms
  purrr::pmap(
    .l = list(intercept, alpha, colour, linetype, linewidth),
    .f = function(intercept, alpha, colour, linetype, linewidth) {
      fun(
        intercept = intercept,
        alpha = alpha,
        colour = colour,
        linetype = linetype,
        linewidth = linewidth
      )
    }
  )
}
