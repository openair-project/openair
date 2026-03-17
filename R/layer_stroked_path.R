extrude_path <- function(x, y, width) {
  n <- length(x)
  dx <- diff(x)
  dy <- diff(y)
  angles <- atan2(dy, dx)
  angle_at <- numeric(n)
  angle_at[1] <- angles[1]
  angle_at[n] <- angles[n - 1]
  if (n > 2) {
    for (i in seq(2, n - 1)) {
      angle_at[i] <- atan2(
        sin(angles[i - 1]) + sin(angles[i]),
        cos(angles[i - 1]) + cos(angles[i])
      )
    }
  }
  perp <- angle_at + pi / 2
  half <- width / 2
  list(
    x = c(x + half * cos(perp), rev(x - half * cos(perp))),
    y = c(y + half * sin(perp), rev(y - half * sin(perp)))
  )
}

# Custom grob class so makeContent runs at actual draw time
stroked_path_grob <- function(x, y, id, linewidth, gp) {
  grid::gTree(
    x = x,
    y = y,
    id = id,
    linewidth = linewidth,
    gp = gp,
    cl = "stroked_path_grob"
  )
}

#' @export
makeContent.stroked_path_grob <- function(x) {
  # Convert x/y from native to mm — this runs at draw time so viewport is live
  x_mm <- grid::convertX(x$x, "mm", valueOnly = TRUE)
  y_mm <- grid::convertY(x$y, "mm", valueOnly = TRUE)

  groups <- unique(x$id)
  all_x <- numeric(0)
  all_y <- numeric(0)
  id_lengths <- integer(0)

  for (g in seq_along(groups)) {
    rows <- x$id == groups[g]
    gx <- x_mm[rows]
    gy <- y_mm[rows]
    if (sum(rows) < 2) {
      next
    }

    # Width in mm at draw time — linewidth stored as NPC fraction so it scales
    w <- grid::convertWidth(
      grid::unit(x$linewidth[which(rows)[1]], "npc"),
      "mm",
      valueOnly = TRUE
    )

    poly <- extrude_path(gx, gy, w)
    all_x <- c(all_x, poly$x)
    all_y <- c(all_y, poly$y)
    id_lengths <- c(id_lengths, length(poly$x))
  }

  if (length(all_x) == 0) {
    return(grid::setChildren(x, grid::gList(ggplot2::zeroGrob())))
  }

  grob <- grid::pathGrob(
    x = grid::unit(all_x, "mm"),
    y = grid::unit(all_y, "mm"),
    id.lengths = id_lengths,
    rule = "evenodd",
    gp = x$gp
  )

  grid::setChildren(x, grid::gList(grob))
}

GeomStrokedPath <- ggplot2::ggproto(
  "GeomStrokedPath",
  ggplot2::GeomPath,

  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 1,
    linetype = 1,
    alpha = NA,
    stroke_colour = NA,
    stroke_width = 0.25
  ),

  draw_key = function(data, params, size) {
    fill_col <- if (is.na(data$alpha)) data$colour else scales::alpha(data$colour, data$alpha)
    stroke_col <- data$stroke_colour
    # linewidth is an NPC fraction; scale to key rect height using a ~100mm reference panel
    h_mm <- min(data$linewidth * 100, size[2] * 0.9)
    grid::rectGrob(
      width = grid::unit(0.6, "npc"),
      height = grid::unit(h_mm, "mm"),
      gp = grid::gpar(
        fill = fill_col,
        col = if (is.na(stroke_col)) NA else stroke_col,
        lwd = data$stroke_width * ggplot2::.pt
      )
    )
  },

  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE
  ) {
    data <- data[order(data$group), , drop = FALSE]
    data <- ggplot2::coord_munch(coord, data, panel_params)
    groups <- unique(data$group)

    grobs <- lapply(groups, function(g) {
      rows <- data$group == g
      gdata <- data[rows, , drop = FALSE]
      if (nrow(gdata) < 2) {
        return(ggplot2::zeroGrob())
      }

      first <- gdata[1, ]
      fill_col <- if (is.na(first$alpha)) {
        first$colour
      } else {
        scales::alpha(first$colour, first$alpha)
      }

      stroked_path_grob(
        x = grid::unit(gdata$x, "native"),
        y = grid::unit(gdata$y, "native"),
        id = rep(1L, nrow(gdata)),
        linewidth = gdata$linewidth,
        gp = grid::gpar(
          col = first$stroke_colour,
          fill = fill_col,
          lwd = first$stroke_width * ggplot2::.pt,
          lty = first$linetype,
          linejoin = linejoin,
          linemitre = linemitre,
          lineend = lineend
        )
      )
    })

    grid::gTree(children = do.call(grid::gList, grobs))
  }
)

geom_stroked_path <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStrokedPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  )
}
