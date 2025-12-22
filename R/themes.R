theme_oa_classic <- function(
  type = c("cartesian", "polar"),
  base_size = 11,
  base_family = "",
  header_family = NULL,
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  if (type == "cartesian") {
    theme <- ggplot2::`%+replace%`(
      ggplot2::theme_bw(
        base_size = base_size,
        base_family = base_family,
        header_family = header_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
      ),
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "white")
      )
    )
  } else if (type == "polar") {
    theme <- ggplot2::`%+replace%`(
      ggplot2::theme_bw(
        base_size = base_size,
        base_family = base_family,
        header_family = header_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
      ),
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "white"),
        panel.spacing = ggplot2::unit(0, "cm"),
        axis.line.theta = ggplot2::element_line(colour = "grey80"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(
          color = "grey20",
          arrow = ggplot2::arrow(
            angle = 15,
            length = ggplot2::unit(0.5, "lines"),
            ends = "last",
            type = "closed"
          )
        )
      )
    )
  }
  return(theme)
}

theme_oa_modern <- function(
  base_size = 11,
  base_family = "",
  header_family = NULL,
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(
      base_size = base_size,
      base_family = base_family,
      header_family = header_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ),
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(hjust = 1),
      axis.title.y = ggplot2::element_text(vjust = 1),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.5, "cm")
    )
  )
}
