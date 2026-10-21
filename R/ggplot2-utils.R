# Facet -------------------------------------------------------------------

# work out the faceting strategy
get_facet <- function(
  type,
  extra.args,
  auto.text,
  drop = FALSE,
  wd.res = 8
) {
  fun <- NULL
  if (any(type != "default")) {
    # need special handling for strip.position
    strip.position <- extra.args$strip.position %||% "top"

    # one type - wrapped 2D grid
    if (length(type) == 1) {
      # if invalid strip.position, just set it to top
      if (!strip.position %in% c("top", "bottom", "left", "right")) {
        strip.position <- "top"
      }

      # special handling for wd, else normal facet_wrap
      if (type %in% c("wd", "wd_type")) {
        fun <-
          facet_wd(
            ggplot2::vars(.data[[type]]),
            labeller = labeller_openair(auto_text = auto.text),
            scales = extra.args$scales %||% "fixed",
            axes = extra.args$axes %||% "margins",
            axis.labels = extra.args$axis.labels %||% "all",
            strip.position = strip.position,
            resolution = wd.res
          )
      } else {
        fun <-
          ggplot2::facet_wrap(
            drop = drop,
            facets = ggplot2::vars(.data[[type]]),
            labeller = labeller_openair(auto_text = auto.text),
            ncol = if (rlang::is_integerish(extra.args$ncol, finite = TRUE)) {
              extra.args$ncol
            },
            nrow = if (rlang::is_integerish(extra.args$nrow, finite = TRUE)) {
              extra.args$nrow
            },
            scales = extra.args$scales %||% "fixed",
            space = extra.args$space %||% "fixed",
            axes = extra.args$axes %||% "margins",
            axis.labels = extra.args$axis.labels %||% "all",
            strip.position = strip.position
          )
      }
    } else {
      # two types - 2D matrix
      if (!strip.position %in% c("x", "y", "both")) {
        strip.position <- NULL
      }

      fun <-
        ggplot2::facet_grid(
          drop = drop,
          cols = ggplot2::vars(.data[[type[1]]]),
          rows = ggplot2::vars(.data[[type[2]]]),
          labeller = labeller_openair(auto_text = auto.text),
          scales = extra.args$scales %||% "fixed",
          space = extra.args$space %||% "fixed",
          axes = extra.args$axes %||% "margins",
          axis.labels = extra.args$axis.labels %||% "all",
          switch = extra.args$switch %||% extra.args$strip.position
        )
    }
  }

  fun
}

# Guides ------------------------------------------------------------------

# ggplot2 legend, colourbar or coloursteps with openair's title/text placement.
# Titles sit above legends placed on the left/right. High values should sit at
# the top of a left/right key and at the right of a top/bottom key; continuous
# guides already do this, so `reverse = TRUE` is only applied to left/right
# legends, and should be used where a legend shows binned (e.g., `breaks`)
# values. `...` is passed to the guide function (e.g., `ncol`, `nrow`,
# `override.aes`).
guide_openair <- function(
  key.position,
  type = c("legend", "colourbar", "coloursteps"),
  reverse = FALSE,
  var_text_position = TRUE,
  ...
) {
  type <- rlang::arg_match(type)
  is_side <- key.position %in% c("left", "right")

  theme <- ggplot2::theme(
    legend.title.position = if (is_side) {
      "top"
    } else {
      key.position
    },
    legend.text.position = if (var_text_position || is_side) {
      key.position
    } else {
      NULL
    }
  )

  switch(
    type,
    legend = ggplot2::guide_legend(
      reverse = reverse && is_side,
      theme = theme,
      ...
    ),
    colourbar = ggplot2::guide_colourbar(theme = theme, ...),
    coloursteps = ggplot2::guide_coloursteps(theme = theme, ...)
  )
}

# Scales ------------------------------------------------------------------

# ggplot2 scale for radial plots
scale_x_compass <- function(
  expand = ggplot2::expansion(),
  oob = scales::oob_keep,
  ...
) {
  list(
    ggplot2::scale_x_continuous(
      limits = c(0, 360),
      labels = NULL,
      breaks = seq(0, 360 - 90, 90),
      expand = expand,
      oob = oob,
      ...,
      sec.axis = ggplot2::dup_axis(
        guide = ggplot2::guide_axis_theta()
      )
    )
  )
}

# ggplot2 date/datetime x-axis scale using openair's `date.breaks` and
# `date.format` arguments. `date.breaks` can be `NULL` (ggplot2 defaults), a
# number (the approximate number of breaks) or a string giving the distance
# between breaks (e.g., "2 weeks"). `...` is passed to the scale function (e.g.,
# `limits`, `expand`).
scale_x_openair_date <- function(
  date.breaks = NULL,
  date.format = NULL,
  type = c("datetime", "date"),
  expand = ggplot2::expansion(c(0.02, 0.02)),
  ...,
  call = rlang::caller_env()
) {
  type <- rlang::arg_match(type)
  scale_fun <- switch(
    type,
    datetime = ggplot2::scale_x_datetime,
    date = ggplot2::scale_x_date
  )

  breaks_args <- if (is.null(date.breaks)) {
    list()
  } else if (rlang::is_string(date.breaks)) {
    list(date_breaks = date.breaks)
  } else if (
    rlang::is_scalar_double(date.breaks) ||
      rlang::is_scalar_integer(date.breaks)
  ) {
    list(breaks = scales::breaks_pretty(date.breaks))
  } else {
    cli::cli_abort(
      "{.arg date.breaks} must be a single number, a single character, or \\
      {.code NULL}, not {.obj_type_friendly {date.breaks}}.",
      call = call
    )
  }

  rlang::exec(
    scale_fun,
    !!!breaks_args,
    date_labels = date.format %||% ggplot2::waiver(),
    expand = expand,
    ...
  )
}

# turn a 'trans' argument into a ggplot2 scale transformer
get_scale_transform <- function(
  transform,
  default = scales::transform_log10()
) {
  if (inherits(transform, "transform")) {
    return(transform)
  }

  if (rlang::is_function(transform)) {
    if (inherits(transform, "transform")) {
      return(transform)
    }
  }

  if (rlang::is_logical(transform)) {
    if (transform) {
      return(default)
    } else {
      return(scales::transform_identity())
    }
  }

  if (rlang::is_character(transform)) {
    return(eval(parse(text = paste0("scales::transform_", transform, "()"))))
  }

  cli::cli_abort(
    "{.arg trans} must be logical, a string, or a {.pkg scales} 'transform' object."
  )
}

# Layers ------------------------------------------------------------------

annotate_compass_points <- function(
  size,
  theme,
  labels = c("N", "E", "S", "W")
) {
  theme <- theme %||% "default"
  if (ggplot2::is_theme(theme)) {
    theme <- "default"
  }
  if (theme == "dark") {
    colour <- "white"
  } else {
    colour <- "black"
  }

  compass_annotate <- function(y, x, label, ...) {
    ggplot2::annotate(
      y = y,
      x = x,
      label = label,
      geom = "text",
      ...,
      size = size,
      fontface = "bold",
      colour = colour
    )
  }

  list(
    compass_annotate(
      y = I(0.935),
      x = I(0.52),
      label = labels[1],
      hjust = 0
    ),
    compass_annotate(
      y = I(1 - 0.935),
      x = I(0.52),
      label = labels[3],
      hjust = 0
    ),
    compass_annotate(
      y = I(0.52),
      x = I(1 - 0.935),
      label = labels[4],
      vjust = 0
    ),
    compass_annotate(
      y = I(0.52),
      x = I(0.935),
      label = labels[2],
      vjust = 0
    )
  )
}
