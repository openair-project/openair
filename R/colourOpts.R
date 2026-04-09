#' Define `cols` options for `openair` plots
#'
#' This function provides a convenient way to set options for customising
#' colours in openair plots. It returns a list of options that can be passed to
#' the `cols` argument of most functions. All arguments are the same as
#' [openColours()], but passing [colourOpts()] allows each function to set an
#' appropriate `n` value and overwrite certain options if required.
#' `colourOpts()` and `colorOpts()` are synonyms.
#'
#' @inheritParams openColours
#'
#' @export
#' @return A list of options that can be passed to the `cols` argument of
#'   plotting functions like [polarPlot()].
#'
#' @examples
#' trendLevel(
#'   mydata,
#'   "no2",
#'   cols = colourOpts("viridis", direction = 1, alpha = 0.5)
#' )
colourOpts <- function(
  scheme = "default",
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1
) {
  fix_01 <- function(x) {
    x[x < 0] <- 0
    x[x > 1] <- 1
    x
  }

  list(
    scheme = scheme,
    alpha = fix_01(alpha),
    begin = fix_01(begin),
    end = fix_01(end),
    direction = sign(direction)
  )
}

#' @rdname colourOpts
#' @export
colorOpts <- colourOpts

# helper to handle character pals or named lists
resolve_colour_opts <- function(x, n, ...) {
  if (is.character(x)) {
    return(openColors(scheme = x, n, ...))
  }

  if (!rlang::is_list(x) || !rlang::is_named(x)) {
    cli::cli_abort(
      c(
        "x" = "The {.arg cols} argument must be either {.type {letters}} or a named list.",
        "i" = "See {.fun openair::colourOpts} for more detail."
      )
    )
  }

  defaults <- colourOpts(...)

  openColours(
    scheme = x$scheme %||% defaults$scheme,
    n = n,
    alpha = x$alpha %||% defaults$alpha,
    begin = x$begin %||% defaults$begin,
    end = x$end %||% defaults$end,
    direction = x$direction %||% defaults$direction
  )
}
