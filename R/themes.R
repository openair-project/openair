# Helpers -----------------------------------------------------------------

# helper to get the correct theme function
theme_openair <- function(
  theme = c("classic", "dark", "modern", "soft", "print"),
  coord = c("cartesian", "radial", "sf"),
  key.position,
  extra.args,
  ...
) {
  theme <- theme %||% "classic"

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
  theme <- theme %||% "classic"

  if (ggplot2::is_theme(theme)) {
    theme <- "classic"
  }

  if (theme == "classic") {
    return(cols)
  }

  .theme_palettes <- list(
    modern = list(
      qual = "observable",
      seq = "viridis"
    ),
    dark = list(
      qual = "okabeito",
      seq = colourOpts("batlowW", begin = 0.15, end = 1)
    ),
    soft = list(
      qual = "tol.muted",
      seq = "lipari"
    ),
    print = list(
      qual = colourOpts("grayC", begin = 0.1, end = 0.9),
      seq = colourOpts("grayC", begin = 0.1, end = 0.9)
    )
  )

  .theme_palettes[[theme]][[type]]
}

get_theme_col_na <- function(col.na, theme) {
  theme <- theme %||% "classic"

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
  theme <- theme %||% "classic"

  if (ggplot2::is_theme(theme)) {
    theme <- "classic"
  }
  .theme_maps <- list(
    classic = list(
      fill = "grey85",
      border = "grey40",
      grid = "deepskyblue",
      lwd = 1,
      lty = 1,
      alpha = 0.5
    ),
    dark = list(
      fill = "#2F2F2F",
      border = "#555555",
      grid = "#3C82F6",
      lwd = 0.5,
      lty = 1,
      alpha = 0.6
    ),
    modern = list(
      fill = "#E5E7EB",
      border = "#D1D5DB",
      grid = "#D1D5DB",
      lwd = 0.4,
      lty = 1,
      alpha = 0.8
    ),
    soft = list(
      fill = "#E3DFD7",
      border = "#D6D3D1",
      grid = "#D6D3D1",
      lwd = 0.4,
      lty = 1,
      alpha = 0.8
    ),
    print = list(
      fill = "grey95",
      border = "black",
      grid = "grey60",
      lwd = 1,
      lty = 1,
      alpha = 1
    )
  )
  .theme_maps[[theme]]
}

# Classic -----------------------------------------------------------------
# Makes ggplot2 look like the old openair lattice plots.
# The reference theme — structured, familiar, no surprises.
# Centred titles, framed legend, white strip backgrounds, black panel border.

theme_openair_classic <- function(key.position) {
  if (user_has_set_theme()) {
    return(ggplot2::theme(
      legend.position = key.position,
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      )
    ))
  }

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = 11),
    ggplot2::theme(
      # === BACKGROUND ===
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),

      # === PANEL ===
      # Black border retained — this is the lattice look
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.5
      ),
      panel.spacing = ggplot2::rel(2),

      # === GRID ===
      # Both directions, light grey — classic lattice default
      panel.grid.major = ggplot2::element_line(
        colour = "grey85",
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(3, "pt"),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),

      # === FACETS ===
      # White strip background with black border — classic lattice strip style
      strip.background = ggplot2::element_rect(
        fill = "white",
        colour = "black",
        linewidth = 0.4
      ),
      strip.text = ggplot2::element_text(
        colour = "black",
        face = "bold",
        margin = ggplot2::margin_auto(1)
      ),

      # === TEXT ===
      # Centred titles throughout — the classic/dark convention
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),

      # === LEGEND ===
      # Framed legend is the most distinctive classic feature —
      # thin black box around the colourbar, no background fill
      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(hjust = 0.5),
      legend.ticks = ggplot2::element_line(colour = "black"),
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      ),
      legend.frame = ggplot2::element_rect(
        fill = NA,
        colour = "black",
        linewidth = 0.25
      ),

      # === LAYOUT ===
      plot.margin = ggplot2::margin(10, 10, 10, 10)
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
      # === GRID ===
      # Spokes dark and solid, rings dashed and light —
      # consistent with the dashed-rings/solid-spokes convention
      panel.grid.major.x = ggplot2::element_line(
        colour = "grey20",
        linewidth = 0.4,
        linetype = "solid" # spokes
      ),
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey75",
        linewidth = 0.25,
        linetype = "dashed" # rings
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.text = ggplot2::element_text(colour = "black"),
      axis.ticks.theta = ggplot2::element_blank(),

      # Circular boundary ring in grey to match the lattice polar look
      axis.line.x = ggplot2::element_line(colour = "grey75", linewidth = 0.4),
      axis.line.x.top = ggplot2::element_line(
        colour = "grey75",
        linewidth = 0.4
      ),
      axis.line.x.bottom = ggplot2::element_line(
        colour = "grey75",
        linewidth = 0.4
      ),

      # === PANEL ===
      # No border on radial — the circular axis line handles the boundary
      panel.border = ggplot2::element_blank(),
      panel.background = if (panel.ontop) {
        ggplot2::element_rect(fill = NA, colour = NA)
      } else {
        ggplot2::element_rect(fill = "white", colour = NA)
      },
      panel.ontop = panel.ontop,
      panel.spacing = ggplot2::rel(1),

      # === FACETS ===
      strip.text = ggplot2::element_text(
        colour = "black",
        face = "bold",
        margin = ggplot2::margin_auto(0.5)
      )
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
      # === GRID ===
      # Dashed graticule in the caller-supplied colour (typically deepskyblue)
      panel.grid.major = ggplot2::element_line(
        colour = grid.col,
        linewidth = 0.25,
        linetype = "dashed"
      ),

      # === AXES ===
      # Coordinate labels coloured to match the graticule — classic openair map style
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        colour = grid.col,
        size = ggplot2::rel(0.8)
      ),

      # === PANEL ===
      # Transparent so map geometry shows through; panel.ontop keeps grid on top
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.5
      ),
      panel.ontop = TRUE
    )
  )
}

# Dark --------------------------------------------------------------------
# Deep, screen-optimised theme. High contrast on dark backgrounds.
# Think VS Code, dark mode dashboards, or night-mode reporting tools.
# Distinct from modern: structured panel contrast, visible grid, centred titles.

theme_openair_dark <- function(key.position) {
  if (user_has_set_theme()) {
    return(ggplot2::theme(
      legend.position = key.position,
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      )
    ))
  }

  bg <- "#1E1E1E"
  panel_bg <- "#252526"
  grid_col <- "#3C3F41"
  text_col <- "#E6E6E6"
  axis_col <- "#94A3B8"
  accent_col <- "#94A3B8"

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      # === BACKGROUND ===
      # Inverted contrast: panel slightly darker than surround (opposite of light themes)
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_bg, colour = NA),

      # === GRID ===
      # Both directions, low contrast — enough to read values, not enough to dominate
      panel.grid.major = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.35
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),

      # === AXES ===
      # Accent-coloured axis lines give the dark theme its visual identity
      axis.line = ggplot2::element_line(colour = accent_col, linewidth = 0.4),
      axis.ticks = ggplot2::element_line(colour = accent_col, linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(3, "pt"),

      axis.text = ggplot2::element_text(colour = text_col),
      axis.title = ggplot2::element_text(colour = text_col),

      # === FACETS ===
      # Subtle border using accent — ties strips to the axis line colour
      strip.background = ggplot2::element_rect(
        fill = panel_bg,
        colour = grid_col,
        linewidth = 0.4
      ),
      strip.text = ggplot2::element_text(
        colour = text_col,
        face = "bold",
        margin = ggplot2::margin_auto(1)
      ),

      # === TEXT ===
      text = ggplot2::element_text(colour = text_col),

      # Centred titles — consistent with classic, suits dark mode's symmetry
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        colour = text_col,
        size = ggplot2::rel(1.2)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        colour = axis_col
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = axis_col,
        size = ggplot2::rel(0.8)
      ),

      # === LEGEND ===
      # Framed with accent border — stands out on the dark background
      legend.position = key.position,
      legend.background = ggplot2::element_rect(
        fill = panel_bg,
        colour = NA,
        linewidth = 0.4
      ),
      legend.key = ggplot2::element_rect(fill = panel_bg, colour = NA),
      legend.title = ggplot2::element_text(
        hjust = 0.5,
        colour = text_col,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(colour = text_col),
      legend.ticks = ggplot2::element_line(colour = text_col),
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      ),
      legend.frame = ggplot2::element_rect(
        fill = NA,
        colour = accent_col,
        linewidth = 0.3
      ),

      # === LAYOUT ===
      panel.spacing = ggplot2::rel(2),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
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
  grid_col <- "#3C3F41"
  text_col <- "#E6E6E6"
  axis_col <- "#94A3B8"
  accent_col <- "#94A3B8"

  ggplot2::`%+replace%`(
    theme_openair_dark(key.position),
    ggplot2::theme(
      # === GRID ===
      # Rings dashed, spokes solid — consistent convention across all themes
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.35,
        linetype = "dashed" # rings
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.35,
        linetype = "solid" # spokes
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      # Accent ring around the polar plot
      axis.line.x = ggplot2::element_line(colour = accent_col, linewidth = 0.4),
      axis.line.x.top = ggplot2::element_line(
        colour = accent_col,
        linewidth = 0.4
      ),
      axis.line.x.bottom = ggplot2::element_line(
        colour = accent_col,
        linewidth = 0.4
      ),
      axis.line.y = ggplot2::element_blank(),

      axis.ticks = ggplot2::element_blank(),
      axis.ticks.theta = ggplot2::element_blank(),

      # Cardinal labels bright, radial scale labels muted
      axis.text.x = ggplot2::element_text(colour = text_col, face = "bold"),
      axis.text.y = ggplot2::element_text(
        colour = axis_col,
        size = ggplot2::rel(0.85)
      ),

      # === PANEL ===
      panel.border = ggplot2::element_blank(),
      panel.background = if (panel.ontop) {
        ggplot2::element_rect(fill = NA, colour = NA)
      } else {
        ggplot2::element_rect(fill = panel_bg, colour = NA)
      },
      panel.ontop = panel.ontop,
      panel.spacing = ggplot2::rel(1),

      # === FACETS ===
      strip.text = ggplot2::element_text(
        colour = text_col,
        face = "bold",
        margin = ggplot2::margin_auto(0.5)
      )
    )
  )
}

theme_openair_dark_sf <- function(
  key.position,
  grid.col = "#2D3561"
) {
  if (user_has_set_theme()) {
    return(theme_openair_dark(key.position))
  }

  bg <- "#1E1E1E"
  panel_bg <- "#252526"
  grid_col <- "#3C3F41"
  text_col <- "#E6E6E6"
  axis_col <- "#94A3B8"
  accent_col <- "#94A3B8"

  ggplot2::`%+replace%`(
    theme_openair_dark(key.position),
    ggplot2::theme(
      # === GRID ===
      # Dashed graticule in the same blue-grey as the rest of the theme
      panel.grid.major = ggplot2::element_line(
        colour = grid.col,
        linewidth = 0.25,
        linetype = "dashed"
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      # Coordinate labels in muted axis_col — readable but not competing with map
      axis.text = ggplot2::element_text(
        colour = axis_col,
        size = ggplot2::rel(0.8)
      ),

      # === PANEL ===
      # Transparent so map geometry shows through; panel.ontop keeps grid on top
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.border = ggplot2::element_blank(),
      panel.ontop = TRUE
    )
  )
}

# Modern ------------------------------------------------------------------
# Dashboard-first theme. Clean, digital, screen-optimised.
# Think Observable notebooks, Plotly defaults, or a BI tool export.
# Horizontal gridlines only, strong x baseline, no panel border, cool greys.

theme_openair_modern <- function(key.position) {
  if (user_has_set_theme()) {
    return(ggplot2::theme(
      legend.position = key.position,
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      )
    ))
  }

  grid_col <- "#E5E7EB" # cool grey gridlines
  axis_col <- "#9CA3AF" # muted labels
  xline_col <- "#1F2937" # near-black baseline — the structural anchor
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      # === GRID ===
      # Horizontal only — the x baseline does the structural work instead
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.5
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),

      # === AXES ===
      # Strong x baseline is the visual signature of this theme;
      # y-axis is intentionally absent — gridlines replace it
      axis.line.x = ggplot2::element_line(colour = xline_col, linewidth = 0.7),
      axis.line.y = ggplot2::element_blank(),

      axis.ticks.x = ggplot2::element_line(colour = axis_col, linewidth = 0.4),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.length.x = ggplot2::unit(3, "pt"),

      axis.text.x = ggplot2::element_text(colour = text_col),
      axis.text.y = ggplot2::element_text(colour = axis_col), # muted — gridlines do the reading
      axis.title.x = ggplot2::element_text(colour = text_col, face = "bold"),
      axis.title.y = ggplot2::element_text(colour = axis_col),

      # === FACETS ===
      # No background box — just a bold label, clean separator
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        size = ggplot2::rel(0.95),
        margin = ggplot2::margin_auto(2)
      ),

      # === TEXT ===
      text = ggplot2::element_text(colour = text_col),

      # Large, left-aligned title — dashboard headers are assertive
      plot.title = ggplot2::element_text(
        hjust = 0,
        face = "bold",
        size = ggplot2::rel(1.4),
        colour = text_col
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        colour = "#4B5563",
        size = ggplot2::rel(0.95)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = "#6B7280",
        size = ggplot2::rel(0.8)
      ),

      # === LEGEND ===
      # No border — floats cleanly on the white panel background
      legend.position = key.position,
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        colour = text_col
      ),
      legend.text = ggplot2::element_text(colour = text_col),
      legend.ticks = ggplot2::element_line(colour = axis_col),
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      ),
      # No legend.frame — borderless legend fits the clean dashboard aesthetic

      # === LAYOUT ===
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
  axis_col <- "#9CA3AF"
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    theme_openair_modern(key.position),
    ggplot2::theme(
      # === GRID ===
      # Radial needs both directions — rings dashed, spokes solid
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4,
        linetype = "dashed" # rings
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4,
        linetype = "solid" # spokes
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      # The x baseline concept doesn't translate to polar — use a subtle ring
      axis.line.x = ggplot2::element_line(colour = grid_col, linewidth = 0.4),
      axis.line.x.top = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4
      ),
      axis.line.x.bottom = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4
      ),
      axis.line.y = ggplot2::element_blank(),

      axis.ticks = ggplot2::element_blank(),
      axis.ticks.theta = ggplot2::element_blank(),

      # Cardinal labels strong, radial scale labels muted
      axis.text.x = ggplot2::element_text(colour = text_col, face = "bold"),
      axis.text.y = ggplot2::element_text(
        colour = axis_col,
        size = ggplot2::rel(0.85)
      ),

      # === PANEL ===
      panel.border = ggplot2::element_blank(),
      panel.background = if (panel.ontop) {
        ggplot2::element_rect(fill = NA, colour = NA)
      } else {
        ggplot2::element_blank()
      },
      panel.ontop = panel.ontop,
      panel.spacing = ggplot2::rel(1),

      # === FACETS ===
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        size = ggplot2::rel(0.95),
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
    return(ggplot2::theme(
      legend.position = key.position,
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      )
    ))
  }

  axis_col <- "#9CA3AF"
  text_col <- "#111827"

  ggplot2::`%+replace%`(
    theme_openair_modern(key.position),
    ggplot2::theme(
      # === GRID ===
      # Subtle dashed graticule — present for reference, not dominant
      panel.grid.major = ggplot2::element_line(
        colour = grid.col,
        linewidth = 0.25,
        linetype = "dashed"
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      # No baseline on maps — the panel border handles framing
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        colour = axis_col,
        size = ggplot2::rel(0.8)
      ),
      axis.line.y = ggplot2::element_line(colour = "#1F2937", linewidth = 0.7),

      # === PANEL ===
      # Transparent so map geometry shows through; panel.ontop keeps grid on top
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.border = ggplot2::element_blank(),
      panel.ontop = TRUE
    )
  )
}

# Soft --------------------------------------------------------------------
# Warm, editorial feel. Off-white backgrounds, muted grid, no hard borders.
# Think magazine data journalism rather than dashboard or journal article.

theme_openair_soft <- function(key.position) {
  if (user_has_set_theme()) {
    return(ggplot2::theme(
      legend.position = key.position,
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      )
    ))
  }

  bg <- "#F7F4EF" # warmer, more distinct from white
  panel_bg <- "#EDEAE3" # noticeably different from bg — was only 5 hex steps
  grid_col <- "#D6D3CB" # warm grey, visible but gentle
  text_col <- "#1F2937"
  axis_col <- "#6B7280"

  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = 12), # slightly larger — editorial feel
    ggplot2::theme(
      # === BACKGROUND ===
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_bg, colour = NA),

      # === GRID ===
      # Both directions, solid, warm grey — no dashes, softness comes from colour
      panel.grid.major = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === PANEL BORDER ===
      # No border — the panel_bg fill does the job of demarcating the plot area
      panel.border = ggplot2::element_blank(),

      # === AXES ===
      # No lines or ticks — gridlines are sufficient, hard edges would clash
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),

      axis.text = ggplot2::element_text(colour = axis_col),
      axis.title = ggplot2::element_text(colour = text_col),

      # === FACETS ===
      # No strip box — just bold text, letting the warm bg carry the separation
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        colour = text_col,
        face = "bold",
        margin = ggplot2::margin_auto(1)
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
        colour = axis_col,
        size = ggplot2::rel(0.95)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = axis_col,
        size = ggplot2::rel(0.85)
      ),

      # === LEGEND ===
      # Soft warm border rather than hard black or nothing
      legend.position = key.position,
      legend.background = ggplot2::element_rect(
        fill = panel_bg,
        colour = NA,
        linewidth = 0.4
      ),
      legend.key = ggplot2::element_rect(fill = panel_bg, colour = NA),
      legend.title = ggplot2::element_text(hjust = 0.5, colour = text_col),
      legend.text = ggplot2::element_text(colour = text_col),
      legend.ticks = ggplot2::element_line(colour = axis_col),
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      ),
      legend.frame = ggplot2::element_rect(
        fill = NA,
        colour = grid_col,
        linewidth = 0.4
      ),

      # === LAYOUT ===
      panel.spacing = ggplot2::rel(2),
      plot.margin = ggplot2::margin(12, 12, 12, 12) # slightly more generous
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

  panel_bg <- "#EDEAE3"
  grid_col <- "#D6D3CB"
  text_col <- "#1F2937"
  axis_col <- "#6B7280"

  ggplot2::`%+replace%`(
    theme_openair_soft(key.position),
    ggplot2::theme(
      # === GRID ===
      # Rings dashed, spokes solid — same convention as print radial
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4,
        linetype = "dashed" # rings
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.35,
        linetype = "solid" # spokes
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.ticks.theta = ggplot2::element_blank(),

      # Cardinal labels darker, radial scale labels muted
      axis.text.x = ggplot2::element_text(colour = text_col),
      axis.text.y = ggplot2::element_text(
        colour = axis_col,
        size = ggplot2::rel(0.85)
      ),

      # === PANEL ===
      panel.border = ggplot2::element_blank(),
      panel.background = if (panel.ontop) {
        ggplot2::element_rect(fill = NA, colour = NA)
      } else {
        ggplot2::element_rect(fill = panel_bg, colour = NA)
      },
      panel.ontop = panel.ontop,
      panel.spacing = ggplot2::rel(1),

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
  grid.col = "#D6D3CB"
) {
  if (user_has_set_theme()) {
    return(ggplot2::theme(
      legend.position = key.position,
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      )
    ))
  }

  bg <- "#F7F4EF"
  text_col <- "#1F2937"
  axis_col <- "#6B7280"

  ggplot2::`%+replace%`(
    theme_openair_soft(key.position),
    ggplot2::theme(
      # === GRID ===
      # Very subtle graticule — present but not competing with map data
      panel.grid.major = ggplot2::element_line(
        colour = grid.col,
        linewidth = 0.25,
        linetype = "dashed"
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        colour = axis_col,
        size = ggplot2::rel(0.8)
      ),

      # === PANEL ===
      # Transparent so map geometry shows through; panel.ontop keeps grid on top
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.border = ggplot2::element_blank(),
      panel.ontop = TRUE
    )
  )
}

# Print -------------------------------------------------------------------
# A true print-ready theme: pure black/white/grey, no colour anywhere.
# Designed to look like output from a journal article or newspaper.

theme_openair_print <- function(key.position) {
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

  grid_col <- "#CCCCCC" # true mid-grey, no blue tint
  axis_col <- "#000000" # pure black axes — commit to print
  text_col <- "#000000" # pure black text
  strip_col <- "#E0E0E0" # light grey strip background

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = 11),
    ggplot2::theme(
      # === BACKGROUND ===
      # Pure white everywhere — no warm or cool tint
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),

      # === PANEL BORDER ===
      # Explicit black box — journal plots have a visible frame
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.6
      ),

      # === GRID ===
      # Horizontal only, light grey, dashed — readable but not dominant
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4,
        linetype = "dashed"
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # === AXES ===
      # Pure black, slightly heavier ticks to survive print reproduction
      axis.line = ggplot2::element_blank(), # panel.border handles the frame
      axis.ticks = ggplot2::element_line(colour = axis_col, linewidth = 0.4),
      axis.ticks.length = ggplot2::unit(3, "pt"),
      axis.text = ggplot2::element_text(
        colour = text_col,
        size = ggplot2::rel(0.9)
      ),
      axis.title = ggplot2::element_text(colour = text_col, face = "bold"),

      # === FACETS ===
      # Grey fill with black border — classic journal facet style
      strip.background = ggplot2::element_rect(
        fill = strip_col,
        colour = "black",
        linewidth = 0.5
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = text_col,
        size = ggplot2::rel(0.9),
        margin = ggplot2::margin_auto(1)
      ),

      # === TEXT ===
      text = ggplot2::element_text(colour = text_col, family = ""),

      plot.title = ggplot2::element_text(
        hjust = 0,
        face = "bold",
        size = ggplot2::rel(1.1)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        colour = "#333333", # very dark grey — only concession away from black
        size = ggplot2::rel(0.95)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        colour = "#333333",
        size = ggplot2::rel(0.8)
      ),

      # === LEGEND ===
      legend.position = key.position,
      legend.background = ggplot2::element_rect(
        fill = "white",
        colour = NA,
        linewidth = 0.4
      ),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        colour = text_col
      ),
      legend.text = ggplot2::element_text(colour = text_col),
      legend.ticks = ggplot2::element_line(colour = text_col),
      legend.ticks.length = structure(
        if (key.position %in% c("bottom", "right")) c(-0.2, 0) else c(0, -0.2),
        class = "rel"
      ),
      legend.frame = ggplot2::element_rect(
        fill = NA,
        colour = "black",
        linewidth = 0.4
      ),

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

  grid_col <- "#CCCCCC"
  axis_col <- "#000000"
  text_col <- "#000000"

  ggplot2::`%+replace%`(
    theme_openair_print(key.position),
    ggplot2::theme(
      # Both grid directions needed for radial — dashed rings, solid spokes
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4,
        linetype = "dashed" # rings
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_col,
        linewidth = 0.4,
        linetype = "solid" # spokes
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # Radial plots don't use a panel border the same way
      panel.border = ggplot2::element_blank(),

      # Circular axis ring in black
      axis.line.x = ggplot2::element_line(colour = axis_col, linewidth = 0.4),
      axis.line.x.top = ggplot2::element_line(
        colour = axis_col,
        linewidth = 0.4
      ),
      axis.line.x.bottom = ggplot2::element_line(
        colour = axis_col,
        linewidth = 0.4
      ),
      axis.line.y = ggplot2::element_blank(),

      axis.ticks = ggplot2::element_blank(),
      axis.ticks.theta = ggplot2::element_blank(),

      # Cardinal labels in black, radial labels in dark grey
      axis.text.x = ggplot2::element_text(colour = text_col, face = "bold"),
      axis.text.y = ggplot2::element_text(
        colour = "#555555",
        size = ggplot2::rel(0.8)
      ),

      panel.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      panel.ontop = panel.ontop,
      panel.spacing = ggplot2::rel(1)
    )
  )
}

theme_openair_print_sf <- function(
  key.position,
  grid.col = "#BBBBBB"
) {
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

  axis_col <- "#555555" # slightly softened for lat/lon labels on maps
  text_col <- "#000000"

  ggplot2::`%+replace%`(
    theme_openair_print(key.position),
    ggplot2::theme(
      # Subtle grey graticule lines — present but not distracting
      panel.grid = ggplot2::element_line(
        colour = grid.col,
        linewidth = 0.25,
        linetype = "dashed"
      ),

      # Maps don't need axis lines — the panel border does that job
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        colour = axis_col,
        size = ggplot2::rel(0.8)
      ),

      # Thin black panel border doubles as map frame
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.5
      ),

      panel.background = ggplot2::element_rect(
        fill = "transparent", # ocean/background = white, not transparent
        colour = NA
      ),
      panel.ontop = TRUE
    )
  )
}
