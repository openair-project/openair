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
  strip.position = "top",
  ...
) {
  fun <- NULL
  if (any(type != "default")) {
    if (length(type) == 1) {
      if (!strip.position %in% c("top", "bottom", "left", "right")) {
        strip.position <- "top"
      }

      if (type == "wd") {
        fun <-
          facet_wd(
            ggplot2::vars(.data[[type]]),
            labeller = labeller_openair(auto_text = auto.text),
            scales = scales,
            strip.position = strip.position,
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
            strip.position = strip.position,
            ...
          )
      }
    } else {
      if (!strip.position %in% c("x", "y", "both")) {
        strip.position <- NULL
      }

      if (independent) {
        rlang::check_installed("ggh4x")
        fun <-
          ggh4x::facet_grid2(
            drop = drop,
            rows = ggplot2::vars(.data[[type[1]]]),
            cols = ggplot2::vars(.data[[type[2]]]),
            labeller = labeller_openair(auto_text = auto.text),
            scales = scales,
            switch = strip.position,
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
            switch = strip.position,
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

# Recycle helper similar to lattice behaviour
recycle_to_length <- function(x, n) {
  if (length(x) == n) {
    return(x)
  }
  if (length(x) == 1) {
    return(rep(x, n))
  }
  cli::cli_abort(
    "Length mismatch: argument must be length 1 or same length as 'h'/'v'"
  )
}

# Convert lattice-style ref.y list to ggplot2 geom_hline layers
gg_ref_y <- function(ref.y) {
  if (is.null(ref.y) || is.null(ref.y$h)) {
    return(NULL)
  }

  h <- ref.y$h
  n <- length(h)

  # Recycle aesthetics if needed
  lty <- recycle_to_length(ref.y$lty %||% 1, n)
  col <- recycle_to_length(ref.y$col %||% "black", n)
  lwd <- recycle_to_length(ref.y$lwd %||% 0.5, n)

  # Build list of geoms
  Map(
    function(y, lty_i, col_i, lwd_i) {
      ggplot2::geom_hline(
        yintercept = y,
        linetype = lty_i,
        colour = col_i,
        linewidth = lwd_i,
        inherit.aes = FALSE
      )
    },
    h,
    lty,
    col,
    lwd
  )
}

# Convert lattice-style ref.x list to ggplot2 geom_vline layers
gg_ref_x <- function(ref.x) {
  if (is.null(ref.x) || is.null(ref.x$v)) {
    return(NULL)
  }

  v <- ref.x$v
  n <- length(v)

  lty <- recycle_to_length(ref.x$lty %||% 1, n)
  col <- recycle_to_length(ref.x$col %||% "black", n)
  lwd <- recycle_to_length(ref.x$lwd %||% 0.5, n)

  Map(
    function(x, lty_i, col_i, lwd_i) {
      ggplot2::geom_vline(
        xintercept = x,
        linetype = lty_i,
        colour = col_i,
        linewidth = lwd_i,
        inherit.aes = FALSE
      )
    },
    v,
    lty,
    col,
    lwd
  )
}

# `%||%` for convenience
`%||%` <- function(a, b) if (!is.null(a)) a else b
