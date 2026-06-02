# Helpers -----------------------------------------------------------------

# helper to get the correct theme function
theme_openair <- function(
  theme = c("classic", "dark", "modern", "soft", "print"),
  coord = c("cartesian", "radial", "sf"),
  key.position,
  extra.args,
  ...
) {
  if (ggplot2::is_theme(theme)) {
    extra_theme <- theme
    theme <- "classic"
  } else {
    extra_theme <- ggplot2::theme()
    theme <- rlang::arg_match(theme)
  }

  if (theme == "classic") {
    fun <- switch(
      coord,
      "cartesian" = theme_openair_classic,
      "radial" = theme_openair_classic_radial,
      "sf" = theme_openair_classic_sf
    )
  }

  if (theme == "dark") {
    fun <- switch(
      coord,
      "cartesian" = theme_openair_dark,
      "radial" = theme_openair_dark_radial,
      "sf" = theme_openair_dark_sf
    )
  }

  if (theme == "modern") {
    fun <- switch(
      coord,
      "cartesian" = theme_openair_modern,
      "radial" = theme_openair_modern_radial,
      "sf" = theme_openair_modern_sf
    )
  }

  if (theme == "soft") {
    fun <- switch(
      coord,
      "cartesian" = theme_openair_soft,
      "radial" = theme_openair_soft_radial,
      "sf" = theme_openair_soft_sf
    )
  }

  if (theme == "print") {
    fun <- switch(
      coord,
      "cartesian" = theme_openair_print,
      "radial" = theme_openair_print_radial,
      "sf" = theme_openair_print_sf
    )
  }

  theme <- fun(key.position, ...)

  theme <- ggplot2::`%+replace%`(theme, extra_theme)

  if ("fontsize" %in% names(extra.args)) {
    theme <- ggplot2::`%+replace%`(
      theme,
      ggplot2::theme(
        text = ggplot2::element_text(size = extra.args$fontsize)
      )
    )
  }

  theme
}

# check if the user has set a theme
user_has_set_theme <- function() {
  !identical(ggplot2::get_theme(), ggplot2::theme_gray())
}

# theme colours
get_theme_cols <- function(cols, theme, type) {
  if (ggplot2::is_theme(theme)) {
    theme <- "classic"
  }

  if (theme == "classic") {
    return(cols)
  }

  .theme_palettes <- list(
    modern = list(
      qual = "observable",
      seq = "turbo"
    ),
    dark = list(
      qual = "okabeito",
      seq = "batlowK"
    ),
    soft = list(
      qual = "tol.muted",
      seq = "lajolla"
    ),
    print = list(
      qual = "greyscale",
      seq = "greyscale"
    )
  )

  .theme_palettes[[theme]][[type]]
}

get_theme_col_na <- function(col.na, theme) {
  if (ggplot2::is_theme(theme)) {
    theme <- "classic"
  }
  if (theme == "dark") {
    return("black")
  } else {
    return(col.na)
  }
}

get_theme_map <- function(theme) {
  if (ggplot2::is_theme(theme)) {
    theme <- "classic"
  }
  .theme_maps <- list(
    classic = list(
      fill = "grey80",
      border = "black",
      grid = "deepskyblue",
      lwd = 1,
      lty = 1,
      alpha = 0.5
    ),
    dark = list(
      fill = "#2F2F2F",
      border = "#555555",
      grid = "deepskyblue",
      lwd = 0.5,
      lty = 1,
      alpha = 0.6
    ),
    modern = list(
      fill = "#F3F4F6",
      border = "#D1D5DB",
      grid = "#D1D5DB",
      lwd = 0.4,
      lty = 1,
      alpha = 0.8
    ),
    soft = list(
      fill = "#EEECE7",
      border = "#D6D3D1",
      grid = "#D6D3D1",
      lwd = 0.4,
      lty = 1,
      alpha = 0.8
    ),
    print = list(
      fill = "white",
      border = "grey40",
      grid = "#9CA3AF",
      lwd = 0.6,
      lty = 1,
      alpha = 0.5
    )
  )

  .theme_maps[[theme]]
}

# Classic -----------------------------------------------------------------
# generic theme that makes a ggplot2 look like the old lattice plots

theme_openair_classic <- function(key.position) {
  if (user_has_set_theme()) {
    return(
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
      )
    )
  }

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(),
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.spacing = ggplot2::rel(2.5),
      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      plot.caption = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.margin = ggplot2::rel(4),
      legend.key = ggplot2::element_blank(),
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
    )
  )
}

theme_openair_classic_radial <- function(
  key.position,
  panel.ontop = FALSE
) {
  if (user_has_set_theme()) {
    return(theme_openair_classic(key.position))
  }

  ggplot2::`%+replace%`(
    theme_openair_classic(key.position),
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

theme_openair_classic_sf <- function(key.position, grid.col) {
  if (user_has_set_theme()) {
    return(theme_openair_classic(key.position))
  }

  ggplot2::`%+replace%`(
    theme_openair_classic(key.position),
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


# Dark --------------------------------------------------------------------
# a darker theme similar to the classic theme

theme_openair_dark <- function(key.position) {
  # If user already set theme, just tweak positioning like your base fn
  if (user_has_set_theme()) {
    return(
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
      )
    )
  }

  bg <- "#1E1E1E" # main background
  panel_bg <- "#252526" # slightly lighter for panels
  grid_col <- "#3C3F41" # muted grid
  text_col <- "#E6E6E6" # main text
  accent_col <- "#569CD6" # soft blue accent (optional)

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_bg, colour = NA),

      # gridlines (retain structure like lattice)
      panel.grid.major = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # facet strips (keep lattice feel)
      strip.background = ggplot2::element_rect(
        fill = panel_bg,
        colour = grid_col,
        linewidth = 0.4
      ),
      strip.text = ggplot2::element_text(
        colour = text_col,
        margin = ggplot2::margin_auto(1)
      ),

      # text
      text = ggplot2::element_text(colour = text_col),
      axis.text = ggplot2::element_text(colour = text_col),
      axis.title = ggplot2::element_text(colour = text_col),

      # axes
      axis.line = ggplot2::element_line(colour = grid_col),
      axis.ticks = ggplot2::element_line(colour = grid_col),

      # title styling (consistent with openair)
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        colour = text_col
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        colour = text_col
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        colour = text_col
      ),

      # legend (retain framed lattice-style legend)
      legend.position = key.position,
      legend.background = ggplot2::element_rect(
        fill = panel_bg,
        colour = grid_col,
        linewidth = 0.25
      ),
      legend.key = ggplot2::element_rect(fill = panel_bg, colour = NA),
      legend.title = ggplot2::element_text(hjust = 0.5, colour = text_col),
      legend.text = ggplot2::element_text(colour = text_col),
      legend.ticks = ggplot2::element_line(colour = text_col),
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) {
          c(-0.2, 0)
        } else {
          c(0, -0.2)
        },
        class = "rel"
      ),

      # spacing
      panel.spacing = ggplot2::rel(2),
      plot.margin = ggplot2::rel(4)
    )
  )
}

theme_openair_dark_radial <- function(
  key.position,
  panel.ontop = FALSE
) {
  if (user_has_set_theme()) {
    return(theme_openair_dark(key.position))
  }

  bg <- "#1E1E1E"
  panel_bg <- "#252526"
  grid_major <- "#3C3F41"
  grid_minor <- "#2A2D2E"
  text_col <- "#E6E6E6"

  ggplot2::`%+replace%`(
    theme_openair_dark(key.position),
    ggplot2::theme(
      # text
      axis.text = ggplot2::element_text(color = text_col),

      # remove theta ticks
      axis.ticks.theta = ggplot2::element_blank(),

      # panel + borders
      panel.border = ggplot2::element_blank(),
      panel.background = if (panel.ontop) {
        ggplot2::element_rect(
          fill = scales::alpha(panel_bg, 0.15),
          colour = NA
        )
      } else {
        ggplot2::element_rect(fill = panel_bg, colour = NA)
      },

      # circular grid styling
      axis.line.x = ggplot2::element_line(colour = grid_major),
      axis.line.x.top = ggplot2::element_line(colour = grid_major),
      axis.line.x.bottom = ggplot2::element_line(colour = grid_major),

      panel.grid.major.x = ggplot2::element_line(
        colour = grid_major,
        linewidth = 0.4
      ),
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_major,
        linetype = 2,
        linewidth = 0.25
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # layout
      panel.spacing = ggplot2::rel(1),
      panel.ontop = panel.ontop,

      # strip formatting
      strip.text = ggplot2::element_text(
        colour = text_col,
        margin = ggplot2::margin_auto(0.5)
      )
    )
  )
}

theme_openair_dark_sf <- function(
  key.position,
  grid.col = "#4FC3F7"
) {
  if (user_has_set_theme()) {
    return(theme_openair_dark(key.position))
  }

  bg <- "#1E1E1E"
  panel_bg <- "#252526"
  text_col <- "#E6E6E6"

  ggplot2::`%+replace%`(
    theme_openair_dark(key.position),
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        colour = grid.col,
        linetype = 2,
        linewidth = 0.25
      ),

      # remove axis clutter (as per sf version)
      axis.ticks = ggplot2::element_blank(),

      # text coloured to match grid (like your light sf theme)
      axis.text = ggplot2::element_text(colour = grid.col),

      # clean map-style panel
      panel.background = if (TRUE) {
        ggplot2::element_rect(
          fill = scales::alpha(panel_bg, 0.15),
          colour = NA
        )
      } else {
        ggplot2::element_rect(fill = panel_bg, colour = NA)
      },
      panel.ontop = TRUE
    )
  )
}


# Modern ------------------------------------------------------------------
# a more contemporary plot theme

theme_openair_modern <- function(key.position) {
  if (user_has_set_theme()) {
    return(
      ggplot2::theme(
        legend.position = key.position
      )
    )
  }

  grid_col <- "#E5E7EB"
  axis_col <- "#9CA3AF"
  xline_col <- "#374151" # darker baseline
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      # === GRID ===
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.5
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.line.x = ggplot2::element_line(
        colour = xline_col,
        linewidth = 0.6
      ),
      axis.line.y = ggplot2::element_blank(),

      axis.ticks.x = ggplot2::element_line(colour = axis_col),
      axis.ticks.y = ggplot2::element_blank(),

      axis.text.x = ggplot2::element_text(colour = text_col),
      axis.text.y = ggplot2::element_text(colour = text_col),

      # === FACETS ===
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        margin = ggplot2::margin_auto(2)
      ),

      # === TEXT ===
      text = ggplot2::element_text(colour = text_col),

      plot.title = ggplot2::element_text(
        hjust = 0,
        face = "bold",
        size = ggplot2::rel(1.4)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        colour = "#4B5563"
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = "#6B7280"
      ),

      # === LEGEND ===
      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(hjust = 0.5, face = "bold"),

      # === LAYOUT ===
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::rel(2),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  )
}

theme_openair_modern_radial <- function(
  key.position,
  panel.ontop = FALSE
) {
  if (user_has_set_theme()) {
    return(theme_openair_modern(key.position))
  }

  grid_col <- "#E5E7EB"
  axis_col <- "#374151"
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    theme_openair_modern(key.position),
    ggplot2::theme(
      # === GRID ===
      # keep BOTH, but light and balanced
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linetype = 2,
        linewidth = 0.25
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4 # slightly lighter than radial
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.line.x = ggplot2::element_line(
        colour = "#D1D5DB",
        linewidth = 0.4
      ),
      axis.line.y = ggplot2::element_blank(),

      # remove ticks, keep clean
      axis.ticks = ggplot2::element_blank(),

      # keep directional labels strong
      axis.text.x = ggplot2::element_text(
        colour = text_col
      ),
      axis.text.y = ggplot2::element_text(
        colour = axis_col
      ),

      axis.ticks.theta = ggplot2::element_blank(),

      # === PANEL ===
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),

      panel.spacing = ggplot2::rel(1),
      panel.ontop = panel.ontop,

      # === FACETS ===
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        margin = ggplot2::margin_auto(1)
      )
    )
  )
}

theme_openair_modern_sf <- function(
  key.position,
  grid.col = "#E5E7EB"
) {
  if (user_has_set_theme()) {
    return(
      ggplot2::theme(
        legend.position = key.position
      )
    )
  }

  axis_col <- "#374151"
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      # === GRID ===
      # very subtle, optional feel
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),

      # === AXES ===
      # no bold baseline for sf — maps shouldn't feel axis-driven
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        colour = axis_col
      ),

      # === FACETS ===
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        margin = ggplot2::margin_auto(2)
      ),

      # === TEXT ===
      text = ggplot2::element_text(colour = text_col),

      plot.title = ggplot2::element_text(
        hjust = 0,
        face = "bold",
        size = ggplot2::rel(1.4)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        colour = "#4B5563"
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = "#6B7280"
      ),

      # === LEGEND ===
      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold"),

      # === PANEL ===
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),

      panel.spacing = ggplot2::rel(2),

      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  )
}


# Soft --------------------------------------------------------------------

theme_openair_soft <- function(key.position) {
  if (user_has_set_theme()) {
    ggplot2::theme(
      legend.position = key.position
    )
  }

  bg <- "#FAFAF8"
  panel_bg <- "#F5F5F3"
  grid_col <- "#E7E5E4"
  text_col <- "#1F2937"
  axis_col <- "#6B7280"

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_bg, colour = NA),

      panel.grid.major = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4
      ),
      panel.grid.minor = ggplot2::element_blank(),

      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),

      axis.text = ggplot2::element_text(colour = axis_col),
      text = ggplot2::element_text(colour = text_col),

      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        colour = text_col,
        face = "bold"
      ),

      plot.title = ggplot2::element_text(
        hjust = 0,
        face = "bold",
        size = ggplot2::rel(1.3)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        colour = axis_col
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = axis_col
      ),

      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(hjust = 0.5),

      panel.spacing = ggplot2::rel(2),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  )
}

theme_openair_soft_radial <- function(
  key.position,
  panel.ontop = FALSE
) {
  if (user_has_set_theme()) {
    return(theme_openair_soft(key.position))
  }

  bg <- "#FAFAF8"
  panel_bg <- "#F5F5F3"
  grid_col <- "#E7E5E4"
  text_col <- "#1F2937"
  axis_col <- "#6B7280"

  ggplot2::`%+replace%`(
    theme_openair_soft(key.position),
    ggplot2::theme(
      # === GRID ===
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4,
        linetype = 2
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.35
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),

      axis.text.x = ggplot2::element_text(
        colour = text_col # N/E/S/W
      ),
      axis.text.y = ggplot2::element_text(
        colour = axis_col
      ),

      axis.ticks.theta = ggplot2::element_blank(),

      # === PANEL ===
      panel.background = if (panel.ontop) {
        ggplot2::element_rect(
          fill = scales::alpha(panel_bg, 0.15),
          colour = NA
        )
      } else {
        ggplot2::element_rect(fill = panel_bg, colour = NA)
      },
      panel.border = ggplot2::element_blank(),

      panel.spacing = ggplot2::rel(1),
      panel.ontop = panel.ontop,

      # === FACETS ===
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        margin = ggplot2::margin_auto(1)
      )
    )
  )
}

theme_openair_soft_sf <- function(
  key.position,
  grid.col = "#E7E5E4"
) {
  if (user_has_set_theme()) {
    return(
      ggplot2::theme(
        legend.position = key.position
      )
    )
  }

  bg <- "#FAFAF8"
  panel_bg <- "#F5F5F3"
  text_col <- "#1F2937"
  axis_col <- "#6B7280"

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      # === GRID ===
      # extremely subtle (almost invisible)
      panel.grid.major = ggplot2::element_line(
        colour = grid.col,
        linewidth = 0.25
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = axis_col),

      # === FACETS ===
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        margin = ggplot2::margin_auto(2)
      ),

      # === TEXT ===
      text = ggplot2::element_text(colour = text_col),

      plot.title = ggplot2::element_text(
        hjust = 0,
        face = "bold",
        size = ggplot2::rel(1.3)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        colour = axis_col
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = axis_col
      ),

      # === LEGEND ===
      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold"),

      # === PANEL ===
      panel.background = if (TRUE) {
        ggplot2::element_rect(
          fill = scales::alpha(panel_bg, 0.15),
          colour = NA
        )
      } else {
        ggplot2::element_rect(fill = panel_bg, colour = NA)
      },
      panel.border = ggplot2::element_blank(),

      panel.spacing = ggplot2::rel(2),
      panel.ontop = TRUE,

      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  )
}


# Print -------------------------------------------------------------------

theme_openair_print <- function(key.position) {
  if (user_has_set_theme()) {
    return(
      ggplot2::theme(
        legend.position = key.position
      )
    )
  }

  grid_col <- "#BDBDBD"
  axis_col <- "#4B5563"
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = 11),
    ggplot2::theme(
      # === GRID ===
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.5
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = NA),

      # === AXES ===
      axis.line = ggplot2::element_line(
        colour = axis_col,
        linewidth = 0.5
      ),
      axis.ticks = ggplot2::element_line(colour = axis_col),

      axis.text = ggplot2::element_text(colour = text_col),

      # === FACETS ===
      strip.background = ggplot2::element_rect(
        fill = "grey90",
        colour = "black",
        linewidth = 0.4
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col
      ),

      # === TEXT ===
      text = ggplot2::element_text(colour = text_col),

      plot.title = ggplot2::element_text(
        hjust = 0,
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5
      ),

      # === LEGEND ===
      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(hjust = 0.5, face = "bold"),

      # === LAYOUT ===
      panel.spacing = ggplot2::rel(1.5),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  )
}

theme_openair_print_radial <- function(
  key.position,
  panel.ontop = FALSE
) {
  if (user_has_set_theme()) {
    return(theme_openair_print(key.position))
  }

  grid_col <- "#BDBDBD"
  axis_col <- "#4B5563"
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    theme_openair_print(key.position),
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.5,
        linetype = 2
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4
      ),
      panel.grid.minor = ggplot2::element_blank(),

      axis.line.x = ggplot2::element_line(
        colour = axis_col,
        linewidth = 0.4
      ),
      axis.line.y = ggplot2::element_blank(),

      axis.ticks = ggplot2::element_blank(),

      axis.text.x = ggplot2::element_text(colour = text_col),
      axis.text.y = ggplot2::element_text(colour = axis_col),

      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.ontop = panel.ontop
    )
  )
}

theme_openair_print_sf <- function(
  key.position,
  grid.col = "#BDBDBD"
) {
  if (user_has_set_theme()) {
    return(
      ggplot2::theme(
        legend.position = key.position
      )
    )
  }

  axis_col <- "#4B5563"
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    theme_openair_print(key.position),
    ggplot2::theme(
      panel.grid = ggplot2::element_line(
        colour = grid.col,
        linewidth = 0.3
      ),

      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = axis_col),

      strip.background = ggplot2::element_rect(
        fill = "grey90",
        colour = "black"
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col
      ),

      text = ggplot2::element_text(colour = text_col),

      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.ontop = FALSE,

      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
  )
}
