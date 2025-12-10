# Work out which faceting function is needed for a plot, based on type input and
# facet options
get_facet_fun <- function(type, facet_opts) {
  facet_fun <- NULL
  if (!is.null(type)) {
    if (length(type) == 2) {
      facet_fun <-
        ggplot2::facet_grid(
          rows = ggplot2::vars(.data[[type[1]]]),
          cols = ggplot2::vars(.data[[type[2]]]),
          scales = facet_opts$scales,
          space = facet_opts$space,
          switch = facet_opts$switch,
          drop = facet_opts$drop,
          axes = facet_opts$axes,
          axis.labels = facet_opts$axis.labels
        )
    } else {
      if (type == "wd") {
        facet_fun <-
          facet_wd(
            facets = ggplot2::vars(.data[[type]]),
            scales = facet_opts$scales,
            strip.position = facet_opts$strip.position,
            axes = facet_opts$axes,
            axis.labels = facet_opts$axis.labels,
            resolution = "medium"
          )
      } else {
        facet_fun <-
          ggplot2::facet_wrap(
            facets = ggplot2::vars(.data[[type]]),
            nrow = facet_opts$nrow,
            ncol = facet_opts$ncol,
            scales = facet_opts$scales,
            space = facet_opts$space,
            drop = facet_opts$drop,
            strip.position = facet_opts$strip.position,
            axes = facet_opts$axes,
            axis.labels = facet_opts$axis.labels
          )
      }
    }
  }
  return(facet_fun)
}
