# Theme -------------------------------------------------------------------

# generic theme that makes a ggplot2 look like the old lattice plots
theme_openair <- function(key.position, extra.args) {
  fontsize <- set_extra_fontsize(extra.args)

  if (user_has_set_theme()) {
    return(
      list(
        ggplot2::theme(
          legend.position = key.position,
          legend.ticks.length = structure(
            if (key.position %in% c("bottom", "right")) {
              c(-0.2, 0)
            } else {
              c(0, -0.2)
            },
            class = "rel"
          )
        ),
        fontsize
      )
    )
  }

  list(
    ggplot2::theme_bw() +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "white"),
        panel.spacing = ggplot2::rel(2.5),
        legend.position = key.position,
        legend.background = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.margin = ggplot2::rel(4),
        legend.frame = ggplot2::element_rect(
          fill = NA,
          color = "black",
          linewidth = 0.25
        ),
        legend.title = ggplot2::element_text(hjust = 0.5),
        legend.ticks = ggplot2::element_line(),
        legend.ticks.length = structure(
          if (key.position %in% c("bottom", "right")) {
            c(-0.2, 0)
          } else {
            c(0, -0.2)
          },
          class = "rel"
        ),
        plot.background = ggplot2::element_rect(
          fill = "transparent",
          color = NA
        ),
        strip.text = ggplot2::element_text(margin = ggplot2::margin_auto(1))
      ),
    fontsize
  )
}

# theme for radial plots
theme_openair_radial <- function(
  key.position,
  extra.args,
  panel.ontop = FALSE
) {
  if (user_has_set_theme()) {
    return(theme_openair(key.position, extra.args))
  }

  list(
    theme_openair(key.position, extra.args),
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "black"),
      axis.ticks.theta = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(
        colour = "grey75"
      ),
      axis.line.x.top = ggplot2::element_line(
        colour = "grey75"
      ),
      axis.line.x.bottom = ggplot2::element_line(
        colour = "grey75"
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = "grey10",
        linewidth = 0.4
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey75",
        linetype = 2,
        linewidth = 0.25
      ),
      panel.spacing = ggplot2::rel(1),
      panel.ontop = panel.ontop,
      panel.background = ggplot2::element_rect(fill = "transparent"),
      strip.text = ggplot2::element_text(margin = ggplot2::margin_auto(0.5))
    )
  )
}

# adapted theme_openair with a (by default) blue dashed gridline
theme_openair_sf <- function(key.position, extra.args, grid.col) {
  if (user_has_set_theme()) {
    return(theme_openair(key.position, extra.args))
  }

  list(
    theme_openair(key.position, extra.args),
    ggplot2::theme(
      panel.grid = ggplot2::element_line(
        colour = grid.col,
        linetype = 2,
        linewidth = 0.25
      ),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = grid.col),
      panel.ontop = TRUE,
      panel.background = ggplot2::element_blank()
    )
  )
}

# handle "fontsize" arg - used in theme_openair
set_extra_fontsize <- function(extra.args) {
  if ("fontsize" %in% names(extra.args)) {
    list(
      ggplot2::theme(
        text = ggplot2::element_text(size = extra.args$fontsize)
      )
    )
  } else {
    list()
  }
}

# check if a theme is set
user_has_set_theme <- function() {
  !identical(ggplot2::get_theme(), ggplot2::theme_gray())
}

# Facet -------------------------------------------------------------------

# work out the faceting strategy
get_facet <- function(
  type,
  extra.args,
  scales,
  auto.text,
  drop = FALSE,
  strip.position = "top",
  wd.res = 8,
  ...
) {
  fun <- NULL
  if (any(type != "default")) {
    if (length(type) == 1) {
      if (!strip.position %in% c("top", "bottom", "left", "right")) {
        strip.position <- "top"
      }

      if (type %in% c("wd", "wd_type")) {
        fun <-
          facet_wd(
            ggplot2::vars(.data[[type]]),
            labeller = labeller_openair(auto_text = auto.text),
            scales = scales,
            strip.position = strip.position,
            resolution = wd.res,
            ...
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
            scales = scales,
            strip.position = strip.position,
            ...
          )
      }
    } else {
      if (!strip.position %in% c("x", "y", "both")) {
        strip.position <- NULL
      }

      fun <-
        ggplot2::facet_grid(
          drop = drop,
          cols = ggplot2::vars(.data[[type[1]]]),
          rows = ggplot2::vars(.data[[type[2]]]),
          labeller = labeller_openair(auto_text = auto.text),
          scales = scales,
          switch = strip.position,
          ...
        )
    }
  }
  fun
}

# convert `lattice` 'relation' to `ggplot2`
relation_to_facet_scales <- function(x.relation, y.relation) {
  x.relation <- x.relation == "free"
  y.relation <- y.relation == "free"
  dplyr::case_when(
    x.relation && !y.relation ~ "free_x",
    !x.relation && y.relation ~ "free_y",
    x.relation && y.relation ~ "free",
    !x.relation && !y.relation ~ "fixed"
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


# Layers ------------------------------------------------------------------

annotate_compass_points <- function(size, labels = c("N", "E", "S", "W")) {
  list(
    ggplot2::annotate(
      y = I(0.935),
      x = I(0.52),
      geom = "text",
      label = labels[1],
      hjust = 0,
      size = size,
      fontface = "bold"
    ),
    ggplot2::annotate(
      y = I(1 - 0.935),
      x = I(0.52),
      geom = "text",
      label = labels[3],
      hjust = 0,
      size = size,
      fontface = "bold"
    ),
    ggplot2::annotate(
      y = I(0.52),
      x = I(1 - 0.935),
      geom = "text",
      label = labels[4],
      vjust = 0,
      size = size,
      fontface = "bold"
    ),
    ggplot2::annotate(
      y = I(0.52),
      x = I(0.935),
      geom = "text",
      label = labels[2],
      vjust = 0,
      size = size,
      fontface = "bold"
    )
  )
}
