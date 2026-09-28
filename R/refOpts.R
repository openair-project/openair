#' Define `ref.x` or `ref.y` options for `openair` plots
#'
#' This function provides a convenient way to set default options for `ref.x` or
#' `ref.y` layers in `openair` plots, which show some form of horizontal or
#' vertical reference line or band. `intercept` can be a vector of any length
#' (for lines) or an even numbered vector representing pairs of bounds (for
#' bands); all other arguments besides `label` will be recycled to be the
#' correct length. Use `method` to change between lines and bands.
#'
#' @param intercept The axis intercept(s) for the reference lines. Should be
#'   numeric, dates, or date-times depending on the axis types. If another data
#'   type is provided, it will attempt to be coerced to the correct type using
#'   [as.numeric()], [lubridate::as_date()] or [lubridate::as_datetime()],
#'   respectively.
#'
#' @param alpha Numeric value between `0` and `1` specifying the transparency of
#'   the lines. Default is `1` (fully opaque) for lines and `0.1` for bands.
#'
#' @param colour,color Colour of the lines and/or bands. Default is `"black"`.
#'   `colour` and `color` are interchangeable, but `colour` is used
#'   preferentially if both are given.
#'
#' @param linetype Line type. Can be an integer (e.g., `1` for solid, `2` for
#'   dashed) or a string (e.g., `"solid"`, `"dashed"`). Default is `1` (solid).
#'
#' @param linewidth Numeric value specifying the width of the lines. Default is
#'   `1` for lines and `0` for bands.
#'
#' @param label,label_size,label_colour,label_color `label` takes character
#'   string to add a direct label to the reference line. For `ref.x` this will
#'   be on the right hand side of the plot, and for `ref.y` this will be on top.
#'   `label_size` and `label_colour` set label aesthetics, with the latter
#'   defaulting to `colour` if not set.
#'
#' @param method One of `"line"` or `"band"`. The former will create any number
#'   of horizontal or vertical reference lines at `intercept` values. The latter
#'   will use pairs of `intercept` values to create any number of horizontal or
#'   vertical shaded areas/bands.
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
#'
#' # use the 'bands' method for shaded areas
#' timePlot(
#'   mydata,
#'   avg.time = "month",
#'   ref.y = refOpts(
#'     c(200, 225, 250, 275),
#'     colour = c("purple", "green"),
#'     method = "band"
#'   )
#' )
refOpts <- function(
  intercept,
  alpha = 1,
  colour = "black",
  linetype = 1,
  linewidth = 0.6,
  label = NULL,
  label_size = 10,
  label_colour = NULL,
  color = NULL,
  label_color = NULL,
  method = c("line", "band")
) {
  method <- rlang::arg_match(method)

  if (method == "band") {
    if (missing(alpha)) {
      alpha <- 0.1
    }
    if (missing(linewidth)) {
      linewidth <- 0
    }
  }

  if (missing(colour) && !is.null(color)) {
    colour <- color
  }
  list(
    intercept = intercept,
    alpha = alpha,
    colour = colour,
    linetype = linetype,
    linewidth = linewidth,
    label = label,
    label_size = label_size,
    label_colour = label_colour %||% label_color %||% colour,
    method = method
  )
}

# Convert refOpts (or just values) to ggplot2 layers
layer_ref <- function(
  ref,
  which = c("y", "x"),
  type = c("numeric", "datetime", "date"),
  other_type = c("numeric", "datetime", "date"),
  tz = NULL
) {
  # if ref is missing, do nothing
  if (is.null(ref)) {
    return(NULL)
  }

  # if ref is just a value, use that as the intercept
  if (!is.list(ref)) {
    ref <- refOpts(intercept = ref)
  }

  # look for intercept, v and h (latter two legacy values)
  intercept <- ref$intercept %||% ref$h %||% ref$v
  n <- length(intercept)

  # check there are an even number of intercepts for bands method
  if (ref$method == "band" && n %% 2 != 0) {
    cli::cli_abort(
      "{.fun openair::refOpts} requires an even number of \\
      {.field intercept} values when {.field method} is 'band'."
    )
  }

  # coerce intercepts to correct types
  if (type == "numeric") {
    if (!is.numeric(intercept)) {
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
  }

  if (type == "datetime") {
    if (!lubridate::is.POSIXct(intercept)) {
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

  # need to format "Inf" correctly for ggplot2 bands
  if (other_type == "numeric") {
    neg_inf <- -Inf
    pos_inf <- Inf
  } else if (other_type == "datetime") {
    neg_inf <- datetime_inf(-Inf, tz = tz)
    pos_inf <- datetime_inf(Inf, tz = tz)
  } else if (other_type == "date") {
    neg_inf <- date_inf(-Inf)
    pos_inf <- date_inf(Inf)
  }

  # recycle aesthetics if needed
  n_needed <- ifelse(ref$method == "band", n / 2, n)
  alpha <- recycle_to_length(ref$alpha %||% 1, n_needed)
  colour <- recycle_to_length(ref$colour %||% ref$col %||% "black", n_needed)
  linetype <- recycle_to_length(ref$linetype %||% ref$lty %||% 0.6, n_needed)
  linewidth <- recycle_to_length(ref$linewidth %||% ref$lwd %||% 0.5, n_needed)
  label_size <- recycle_to_length(ref$label_size %||% 10, n_needed)
  label_colour <- recycle_to_length(ref$label_colour %||% 10, n_needed)

  label <- ref$label
  use_label <- !all(is.null(label))
  if (use_label) {
    if (length(label) != n_needed) {
      if (ref$method == "band") {
        cli::cli_abort("One {.arg label} needed per pair of {.arg intercept}s.")
      } else {
        cli::cli_abort("One {.arg label} needed per {.arg intercept}.")
      }
    }
    rlang::check_installed(
      "legendry",
      reason = "to add labels to reference lines.",
      version = "0.3.0"
    )
  }

  # choose appropriate function
  if (which == "x") {
    if (ref$method == "band") {
      geom_fun <- \(intercept, ...) {
        ggplot2::annotate(
          geom = "rect",
          xmin = intercept[1],
          xmax = intercept[2],
          ymin = neg_inf,
          ymax = pos_inf,
          ...
        )
      }
    } else {
      geom_fun <- \(intercept, ...) {
        ggplot2::geom_vline(xintercept = intercept, ..., inherit.aes = FALSE)
      }
    }
    annotate_fun <- \(intercept, label, ...) {
      legendry::annotate_top(aesthetic = intercept, label = label, ...)
    }
  } else if (which == "y") {
    if (ref$method == "band") {
      geom_fun <- \(intercept, ...) {
        ggplot2::annotate(
          geom = "rect",
          ymin = intercept[1],
          ymax = intercept[2],
          xmin = neg_inf,
          xmax = pos_inf,
          ...
        )
      }
    } else {
      geom_fun <- \(intercept, ...) {
        ggplot2::geom_hline(yintercept = intercept, ..., inherit.aes = FALSE)
      }
    }
    annotate_fun <- \(intercept, label, ...) {
      legendry::annotate_right(aesthetic = intercept, label = label, ...)
    }
  }

  # Build list of geoms
  if (ref$method == "band") {
    # pair up the intercepts
    ids <- ceiling(seq_along(intercept) / 2)
    pairs <- list()
    for (i in unique(ids)) {
      pairs <- append(pairs, list(intercept[which(ids == i)]))
    }

    geoms <- purrr::pmap(
      .l = list(pairs, alpha, colour, linetype, linewidth),
      .f = function(intercept, alpha, colour, linetype, linewidth) {
        geom_fun(
          intercept = intercept,
          alpha = alpha,
          fill = colour,
          colour = colour,
          linetype = linetype,
          linewidth = linewidth
        )
      }
    )
  } else {
    geoms <- purrr::pmap(
      .l = list(intercept, alpha, colour, linetype, linewidth),
      .f = function(intercept, alpha, colour, linetype, linewidth) {
        geom_fun(
          intercept = intercept,
          alpha = alpha,
          colour = colour,
          linetype = linetype,
          linewidth = linewidth
        )
      }
    )
  }

  if (use_label) {
    if (ref$method == "band") {
      intercept <- purrr::map_vec(pairs, \(x) {
        x[1] + (x[2] - x[1]) / 2
      })
    }
    return(append(
      geoms,
      annotate_fun(
        intercept = intercept,
        label = label,
        size = label_size,
        colour = label_colour,
        linetype = linetype,
        linewidth = linewidth
      )
    ))
  } else {
    return(geoms)
  }
}

#' Avoid warning messages by forcing an Infinite value to be a datetime
#' @noRd
datetime_inf <- function(x, tz = NULL) {
  structure(
    x,
    class = c("POSIXct", "POSIXt"),
    tzone = tz %||% "UTC"
  )
}

#' @noRd
date_inf <- function(x) {
  structure(
    x,
    class = "Date"
  )
}
