# Facet -------------------------------------------------------------------

# work out the faceting strategy. `wd_layout` controls whether a single `type`
# is laid out as a compass (via `facet_wd()`); by default this is inferred from
# the column name, but functions which rename the `type` column (e.g.,
# `variationPlot()`) need to set it explicitly
get_facet <- function(
  type,
  extra.args,
  auto.text,
  drop = FALSE,
  wd.res = 8,
  wd_layout = NULL
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
      if (wd_layout %||% (type %in% c("wd", "wd_type"))) {
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

# Labels ------------------------------------------------------------------

# Build the labels of an openair plot as a `ggplot2::labs()` object.
#
# Every openair plotting function should set its labels through this function
# so that they are all handled in the same way:
#
# 1. A value supplied by the user via `...` (`xlab`, `ylab`, `title`,
#    `subtitle`, `caption`, `tag`) takes precedence over the function's
#    default. Users can pass `NULL` (e.g., `caption = NULL`) to remove a default
#    label.
#
# 2. The label is passed through `quickText()` if `auto.text` applies to that
#    part of the plot (see `use_quicktext()`).
#
# Legend titles are supplied by aesthetic via `...` (e.g., `fill = key.title`)
# and are set here rather than via `name` in a scale or `title` in a guide, so
# that users can override them with `+ ggplot2::labs()`. Labels for aesthetics
# that aren't used by the plot are ignored by ggplot2, so it is safe to supply
# the legend title for every aesthetic a plot might use.
labs_openair <- function(
  extra.args = list(),
  auto.text = TRUE,
  x = NULL,
  y = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  ...
) {
  # plot-level labels: user-supplied value (if any), else function default
  defaults <- list(
    x = x,
    y = y,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag
  )
  user_names <- c(
    x = "xlab",
    y = "ylab",
    title = "title",
    subtitle = "subtitle",
    caption = "caption",
    tag = "tag"
  )

  labels <- lapply(names(defaults), function(part) {
    label <- resolve_label(extra.args, user_names[[part]], defaults[[part]])
    label_part(label, part, auto.text)
  })
  names(labels) <- names(defaults)

  # legend titles, supplied by aesthetic
  legend <- lapply(rlang::list2(...), label_part, "legend", auto.text)

  # build with `do.call()` so that NULL labels remove the label
  do.call(ggplot2::labs, c(labels, legend))
}

# Overall title, subtitle and caption for a multi-panel (patchwork) openair
# plot, resolved in the same way as `labs_openair()`. Axis labels and legend
# titles belong to the individual panels, so are set with `labs_openair()`
# there. `...` is passed to `patchwork::plot_annotation()` (e.g., `theme`).
annotation_openair <- function(
  extra.args = list(),
  auto.text = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  defaults <- list(title = title, subtitle = subtitle, caption = caption)
  labels <- purrr::imap(defaults, \(default, part) {
    label_part(resolve_label(extra.args, part, default), part, auto.text)
  })

  patchwork::plot_annotation(
    title = labels$title,
    subtitle = labels$subtitle,
    caption = labels$caption,
    ...
  )
}

# The value for a label: the user's value if they supplied one (even `NULL`),
# else the default
resolve_label <- function(extra.args, name, default) {
  if (name %in% names(extra.args)) {
    extra.args[[name]]
  } else {
    default
  }
}

# Pass a single label through `quickText()` if `auto.text` applies to `part`
label_part <- function(text, part, auto.text) {
  if (is.null(text)) {
    return(NULL)
  }
  quickText(text, auto.text = use_quicktext(auto.text, part))
}

# Should `quickText()` be applied to a given part of the plot?
#
# `part` is one of "x", "y", "title", "subtitle", "caption", "tag" or
# "legend". Currently `auto.text` is a single `TRUE`/`FALSE` applying to all
# parts; this is the single place to extend if `auto.text` is to target
# specific parts of a plot.
use_quicktext <- function(auto.text, part) {
  isTRUE(auto.text)
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

# Theme setting the length of the legend key along the side of the plot it sits
# on. By default this makes the key span the full height/width of the plot with
# no gaps; `spacing = NULL` leaves key spacing unchanged. Returns `NULL` for
# other positions (e.g., "none"), so it can always be added to a plot.
theme_legend_key <- function(
  key.position,
  size = ggplot2::unit(1, "null"),
  spacing = ggplot2::unit(0, "cm")
) {
  if (key.position %in% c("left", "right")) {
    ggplot2::theme(legend.key.height = size, legend.key.spacing.y = spacing)
  } else if (key.position %in% c("top", "bottom")) {
    ggplot2::theme(legend.key.width = size, legend.key.spacing.x = spacing)
  }
}

# Scales ------------------------------------------------------------------

# ggplot2 scale for radial plots
scale_x_compass <- function(
  expand = ggplot2::expansion(),
  oob = scales::oob_keep,
  points = NULL,
  ...
) {
  points <- resolve_compass_points(points)
  list(
    ggplot2::scale_x_continuous(
      limits = c(0, 360),
      labels = NULL,
      breaks = seq(0, 360 - 360 / points, 360 / points),
      expand = expand,
      oob = oob,
      ...,
      sec.axis = ggplot2::dup_axis(
        guide = ggplot2::guide_axis_theta()
      )
    )
  )
}

# number of compass points used for polar plot gridlines and labels; set by
# users via `compass.points` in `...`
resolve_compass_points <- function(points) {
  points <- points %||% 4
  if (!rlang::is_scalar_integerish(points) || !points %in% c(4, 8, 16)) {
    cli::cli_abort(
      "{.arg compass.points} must be one of {.val {4}}, {.val {8}} or {.val {16}}."
    )
  }
  points
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
    "{.arg trans} must be logical, a string, or a {.pkg scales} {.cls transform} object."
  )
}

# Layers ------------------------------------------------------------------

annotate_compass_points <- function(
  size,
  theme,
  labels = c("N", "E", "S", "W"),
  points = NULL
) {
  points <- resolve_compass_points(points)

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

  # (secondary) intercardinal points sit at the same radius as N/E/S/W, nudged
  # clockwise off their gridline so they don't clash with radial axis labels
  # (which ggplot2 draws on the anticlockwise side of `angle.scale`, often set
  # to a diagonal).
  intercardinal <- NULL
  if (points > 4) {
    radius <- 0.435
    all_points <- c(
      "N",
      "NNE",
      "NE",
      "ENE",
      "E",
      "ESE",
      "SE",
      "SSE",
      "S",
      "SSW",
      "SW",
      "WSW",
      "W",
      "WNW",
      "NW",
      "NNW"
    )
    angles <- seq(0, 360 - 360 / points, 360 / points)
    # N/E/S/W are drawn separately below
    angles <- angles[angles %% 90 != 0]
    point_labels <- all_points[angles / 22.5 + 1]
    theta <- (angles + 5) * pi / 180
    intercardinal <- purrr::map2(
      theta,
      point_labels,
      \(angle, label) {
        compass_annotate(
          y = I(0.5 + radius * cos(angle)),
          x = I(0.5 + radius * sin(angle)),
          label = label
        )
      }
    )
  }

  # by default, N/S sit to the right of their line and E/W above it. With more
  # compass points, all labels sit clockwise of their line for consistency, so
  # S moves to the left of its line and E below it.
  clockwise <- points > 4

  list(
    intercardinal,
    compass_annotate(
      y = I(0.935),
      x = I(0.52),
      label = labels[1],
      hjust = 0
    ),
    compass_annotate(
      y = I(1 - 0.935),
      x = I(if (clockwise) 0.48 else 0.52),
      label = labels[3],
      hjust = if (clockwise) 1 else 0
    ),
    compass_annotate(
      y = I(0.52),
      x = I(1 - 0.935),
      label = labels[4],
      vjust = 0
    ),
    compass_annotate(
      y = I(if (clockwise) 0.48 else 0.52),
      x = I(0.935),
      label = labels[2],
      vjust = if (clockwise) 1 else 0
    )
  )
}
