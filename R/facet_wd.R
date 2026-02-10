#' (Presently) internal function for handling compass direction faceting
#' @noRd
facet_wd <- function(
  facets,
  scales = "free_y",
  strip.position = "top",
  labeller = "label_value",
  axes = "margins",
  axis.labels = "all",
  resolution = c("medium")
) {
  # Process the scales parameter like facet_wrap does
  free <- list(
    x = identical(scales, "free_x") || identical(scales, "free"),
    y = identical(scales, "free_y") || identical(scales, "free")
  )

  ggplot2::ggproto(
    NULL,
    FacetWinddir,
    params = list(
      facets = rlang::quos_auto_name(facets),
      strip.position = strip.position,
      labeller = labeller,
      free = free,
      axes = axes,
      drop = FALSE,
      axis.labels = axis.labels,
      resolution = resolution
    )
  )
}

#' ggproto object for facet wind dir
#' @noRd
FacetWinddir <- ggplot2::ggproto(
  "FacetWinddir",
  ggplot2::FacetWrap,
  setup_params = function(data, params) {
    params <- ggplot2::FacetWrap$setup_params(data, params)
    # Instead of forcing free to FALSE, use the value passed from the constructor
    # or default to FALSE if not specified
    if (is.null(params$free)) {
      params$free <- list(x = FALSE, y = FALSE)
    }

    draw_axes <- rlang::arg_match0(
      params$axes,
      c("margins", "all_x", "all_y", "all")
    )
    params$draw_axes <- list(
      x = params$free$x || any(draw_axes %in% c("all_x", "all")),
      y = params$free$y || any(draw_axes %in% c("all_y", "all"))
    )

    axis_labels <- rlang::arg_match0(
      params$axis.labels,
      c("margins", "all_x", "all_y", "all")
    )
    params$axis_labels <- list(
      x = params$free$x ||
        !params$draw_axes$x ||
        any(axis_labels %in% c("all_x", "all")),
      y = params$free$y ||
        !params$draw_axes$y ||
        any(axis_labels %in% c("all_y", "all"))
    )

    return(params)
  },
  compute_layout = function(data, params) {
    # create a data frame with one column per facetting variable
    panels <- ggplot2::combine_vars(
      data = data,
      env = params$plot_env,
      vars = params$facets,
      drop = params$drop
    )

    # Assign each panel a location and scale
    if (params$resolution == "low") {
      layout <- data.frame(
        PANEL = 1:4,
        ROW = c(1, 2, 3, 2),
        COL = c(2, 3, 2, 1),
        # If scales are free, each panel gets its own scale index
        # Otherwise, all panels share scale 1
        SCALE_X = if (isTRUE(params$free$x)) 1L:4L else 1L,
        SCALE_Y = if (isTRUE(params$free$y)) 1L:4L else 1L,
        PANEL_NAME = c("N", "E", "S", "W")
      )
    }

    if (params$resolution == "medium") {
      layout <- data.frame(
        PANEL = 1:8,
        ROW = c(1, 1, 1, 2, 2, 3, 3, 3),
        COL = c(1, 2, 3, 3, 1, 1, 2, 3),
        # If scales are free, each panel gets its own scale index
        # Otherwise, all panels share scale 1
        SCALE_X = if (isTRUE(params$free$x)) 1L:8L else 1L,
        SCALE_Y = if (isTRUE(params$free$y)) 1L:8L else 1L,
        PANEL_NAME = c("NW", "N", "NE", "E", "W", "SW", "S", "SE")
      )
    }

    if (params$resolution == "high") {
      layout <- data.frame(
        PANEL = 1:16,
        ROW = c(1, 1, 2, 2, 3, 4, 4, 5, 5, 5, 4, 4, 3, 2, 2, 1),
        COL = c(3, 4, 4, 5, 5, 5, 4, 4, 3, 2, 2, 1, 1, 1, 2, 2),
        # If scales are free, each panel gets its own scale index
        # Otherwise, all panels share scale 1
        SCALE_X = if (isTRUE(params$free$x)) 1L:16L else 1L,
        SCALE_Y = if (isTRUE(params$free$y)) 1L:16L else 1L,
        PANEL_NAME = c(
          "N",
          "NNE",
          "NE",
          "ENE",
          "E",
          "ESE",
          "SE",
          "SSE",
          "S",
          "SSW",
          "SW",
          "WSW",
          "W",
          "WNW",
          "NW",
          "NNW"
        )
      )
    }

    if (any(!panels[[1]] %in% layout$PANEL_NAME)) {
      stop("Wind direction not recognised")
    }

    if (anyNA(panels[[1]])) {
      stop("Missing wind directions not allowed")
    }

    names(layout)[names(layout) == "PANEL_NAME"] <- names(panels)

    return(layout)
  },
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- ggplot2::FacetWrap$init_scales(layout, x_scale, y_scale, params)

    # Determine which panels are on the left and right edges
    min_col <- min(layout$COL)
    max_col <- max(layout$COL)

    left_panels <- layout$PANEL[layout$COL == min_col]
    right_panels <- layout$PANEL[layout$COL == max_col]

    # Set position for y-axis scales
    if (!is.null(scales$y)) {
      for (i in seq_along(scales$y)) {
        if (i %in% right_panels) {
          scales$y[[i]]$position <- "right"
        } else if (i %in% left_panels) {
          scales$y[[i]]$position <- "left"
        } else {
          # For center panels, hide the axis labels
          scales$y[[i]]$breaks <- NULL
          scales$y[[i]]$labels <- NULL
        }
      }
    }

    scales
  }
)
