theme_oa_classic <- function(
  base_size = 11,
  base_family = "",
  header_family = NULL,
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  ggplot2::`%+replace%`(
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
