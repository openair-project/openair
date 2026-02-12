# generic theme that makes a ggplot2 look like the old lattice plots
theme_openair <- function(key.position) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.spacing = ggplot2::rel(2.5),
      legend.position = key.position,
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
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      )
    )
}

# take the extra.args and set a different global fontsize, if present
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

# work out the faceting strategy
get_facet <- function(
  type,
  extra.args,
  scales,
  auto.text,
  drop = FALSE,
  independent = FALSE,
  ...
) {
  fun <- NULL
  if (any(type != "default")) {
    if (length(type) == 1) {
      if (type == "wd") {
        fun <-
          facet_wd(
            ggplot2::vars(.data[[type]]),
            labeller = labeller_openair(auto_text = auto.text),
            scales = scales,
            ...
          )
      } else {
        fun <-
          ggplot2::facet_wrap(
            drop = drop,
            facets = ggplot2::vars(.data[[type]]),
            labeller = labeller_openair(auto_text = auto.text),
            ncol = extra.args$layout[1],
            nrow = extra.args$layout[2],
            scales = scales,
            ...
          )
      }
    } else {
      if (independent) {
        rlang::check_installed("ggh4x")
        fun <-
          ggh4x::facet_grid2(
            drop = drop,
            rows = ggplot2::vars(.data[[type[1]]]),
            cols = ggplot2::vars(.data[[type[2]]]),
            labeller = labeller_openair(auto_text = auto.text),
            scales = scales,
            independent = TRUE,
            ...
          )
      } else {
        fun <-
          ggplot2::facet_grid(
            drop = drop,
            rows = ggplot2::vars(.data[[type[1]]]),
            cols = ggplot2::vars(.data[[type[2]]]),
            labeller = labeller_openair(auto_text = auto.text),
            scales = scales,
            ...
          )
      }
    }
  }
  fun
}

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
