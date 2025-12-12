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
      axis.text.x = marquee::element_marquee(),
      axis.text.y = marquee::element_marquee(),
      axis.title.x = marquee::element_marquee(),
      axis.title.y = marquee::element_marquee(angle = 90),
      strip.text.x = marquee::element_marquee(),
      strip.text.y = marquee::element_marquee(),
      legend.text = marquee::element_marquee(),
      legend.title = marquee::element_marquee(),
      plot.title = marquee::element_marquee(),
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
      axis.text.x = marquee::element_marquee(),
      axis.text.y = marquee::element_marquee(),
      axis.title.x = marquee::element_marquee(hjust = 1),
      axis.title.y = marquee::element_marquee(vjust = 1),
      strip.text.x = marquee::element_marquee(),
      strip.text.y = marquee::element_marquee(),
      legend.text = marquee::element_marquee(),
      legend.title = marquee::element_marquee(),
      plot.title = marquee::element_marquee(),
      axis.ticks = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1, "cm")
    )
  )
}
