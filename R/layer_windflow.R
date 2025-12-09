layer_windflow <- function(
  mapping = NULL,
  data = NULL,
  stat = "windflow",
  position = "identity",
  ...,
  arrow = grid::arrow(
    angle = 15,
    length = grid::unit(0.5, "lines"),
    ends = "last",
    type = "closed"
  ),
  limits = c(NA, NA),
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE
) {
  list(
    ggplot2::layer(
      geom = GeomWindflow,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = rlang::list2(
        arrow = arrow,
        na.rm = na.rm,
        ...
      )
    ),
    continuous_scale(
      guide = ggplot2::guide_none(),
      "ws",
      limits = limits,
      palette = scales::pal_area(c(0, 1))
    )
  )
}

GeomWindflow <- ggplot2::ggproto(
  "GeomWindflow",
  ggplot2::Geom,
  required_aes = c("x", "y", "ws", "wd"),
  default_aes = ggplot2::aes(
    color = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_abline,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    arrow = arrow
  ) {
    # transform coords
    coords <- coord$transform(data, panel_scales)

    # scale arrow length w/ normalised ws
    arrow$length <- data$norm_ws * arrow$length

    # create windflow grob
    grid::grob(
      x = grid::unit(coords$x, "npc"),
      y = grid::unit(coords$y, "npc"),
      wd = coords$wd,
      length = ggplot2::unit(coords$ws, "cm"),
      pivot = 0.5,
      default.units = "native",
      arrow = arrow,
      gp = grid::gpar(
        col = coords$colour,
        fill = scales::alpha(coords$colour, coords$alpha),
        alpha = ifelse(is.na(coords$alpha), 1, coords$alpha),
        lwd = coords$linewidth * .pt,
        lty = coords$linetype,
        lineend = "butt"
      ),
      cl = "windflow"
    )
  }
)

StatWindflow <- ggplot2::ggproto(
  "StatWindflow",
  ggplot2::Stat,
  required_aes = c("x", "y", "ws", "wd"),
  compute_group = function(data, scales) {
    data
  },
  setup_data = function(data, params) {
    data |>
      dplyr::filter(
        .data$ws > 0
      ) |>
      dplyr::mutate(
        # convert met angle to math angle
        wd = -.data$wd - 90,
        # calculate vector components
        dx = .data$ws * cos(.data$wd * pi / 180),
        dy = .data$ws * sin(.data$wd * pi / 180),
        # normalise for arrow scaling
        norm_ws = .data$ws / max(.data$ws, na.rm = TRUE)
      )
  }
)

#' @export
makeContent.windflow <- function(x) {
  # Convert to mm units for calculations
  x_mm <- grid::convertX(grid::unit(x$x, "npc"), "mm", valueOnly = TRUE)
  y_mm <- grid::convertY(grid::unit(x$y, "npc"), "mm", valueOnly = TRUE)
  len_mm <- grid::convertUnit(
    grid::unit(x$length, "cm"),
    "mm",
    valueOnly = TRUE
  )

  # Calculate vector components
  dx <- len_mm * cos(x$wd * pi / 180)
  dy <- len_mm * sin(x$wd * pi / 180)

  # Create start and end points
  n <- length(x_mm)
  x_coords <- c(x_mm - dx * 0.5, x_mm + dx * (1 - 0.5))
  y_coords <- c(y_mm - dy * 0.5, y_mm + dy * (1 - 0.5))

  # Create polyline grob
  grid::polylineGrob(
    x = grid::unit(x_coords, "mm"),
    y = grid::unit(y_coords, "mm"),
    id = rep(seq_len(n), 2),
    arrow = x$arrow,
    gp = x$gp
  )
}
