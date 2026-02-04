#' Function for 'polar' axes
#' @noRd
scale_x_compass <- function(oob = scales::oob_keep) {
  ggplot2::scale_x_continuous(
    limits = c(0, 360),
    expand = ggplot2::expansion(),
    oob = oob,
    breaks = seq(0, 270, 90),
    labels = c("N", "E", "S", "W")
  )
}
