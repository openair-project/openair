#' Correlation matrices with conditioning
#'
#' Function to to draw and visualise correlation matrices using lattice. The
#' primary purpose is as a tool for exploratory data analysis. Hierarchical
#' clustering is used to group similar variables.
#'
#' The [corPlot()] function plots correlation matrices. The implementation relies
#' heavily on that shown in Sarkar (2007), with a few extensions.
#'
#' Correlation matrices are a very effective way of understating relationships
#' between many variables. The [corPlot()] shows the correlation coded in three
#' ways: by shape (ellipses), colour and the numeric value. The ellipses can be
#' thought of as visual representations of scatter plot. With a perfect positive
#' correlation a line at 45 degrees positive slope is drawn. For zero
#' correlation the shape becomes a circle. See examples below.
#'
#' With many different variables it can be difficult to see relationships
#' between variables, i.e., which variables tend to behave most like one
#' another. For this reason hierarchical clustering is applied to the
#' correlation matrices to group variables that are most similar to one another
#' (if `cluster = TRUE`).
#'
#' If clustering is chosen it is also possible to add a dendrogram using the
#' option `dendrogram = TRUE`. Note that dendrogramscan only be plotted for
#' `type = "default"` i.e. when there is only a single panel. The dendrogram can
#' also be recovered from the plot object itself and plotted more clearly; see
#' examples below.
#'
#' It is also possible to use the `openair` type option to condition the data in
#' many flexible ways, although this may become difficult to visualise with too
#' many panels.
#'
#' @param mydata A data frame which should consist of some numeric columns.
#'
#' @param pollutants the names of data-series in `mydata` to be plotted by
#'   `corPlot`. The default option `NULL` and the alternative `"all"` use all
#'   available valid (numeric) data.
#'
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. For example, `type = "season"` will produce four plots ---
#'   one for each season. See [cutData()] for more information.
#'
#' @param cluster Should the data be ordered according to cluster analysis. If
#'   `TRUE` hierarchical clustering is applied to the correlation matrices using
#'   [hclust()] to group similar variables together. With many variables
#'   clustering can greatly assist interpretation.
#'
#' @param method The correlation method to use. Can be `"pearson"`, `"spearman"`
#'   or `"kendall"`.
#'
#' @param use How to handle missing values in the `cor` function. The default is
#'   `"pairwise.complete.obs"`. Care should be taken with the choice of how to
#'   handle missing data when considering pair-wise correlations.
#'
#' @param annotate What to annotate each correlation tile with. One of:
#' - `"cor"`, the correlation coefficient to 2 decimal places.
#' - `"signif"`, an X marker if the correlation is significant.
#' - `"stars"`, standard significance stars.
#' - `"none"`, no annotation.
#'
#' @param dendrogram Should a dendrogram be plotted? When `TRUE` a dendrogram is
#'   shown on the plot. Note that this will only work for `type = "default"`.
#'   Defaults to `FALSE`.
#'
#' @param diagonal Should the 'diagonal' of the correlation plot be shown? The
#'   diagonal of a correlation matrix is axiomatically always `1` as it
#'   represents correlating a variable with itself. Defaults to `TRUE`.
#'
#' @param triangle Which 'triangles' of the correlation plot should be shown?
#'   Can be `"both"`, `"lower"` or `"upper"`. Defaults to `"both"`.
#'
#' @param cols Colours to be used for plotting. See [openColours()] for more
#'   details.
#'
#' @param r.thresh Values of greater than `r.thresh` will be shown in bold type.
#'   This helps to highlight high correlations.
#'
#' @param text.col The colour of the text used to show the correlation values.
#'   The first value controls the colour of negative correlations and the second
#'   positive.
#'
#' @param key.header Used to control the title of the plot key. Defaults to
#'   `NULL`; no header.
#'
#' @param key.position Location where the scale key is to plotted. Allowed
#'   arguments currently include `"top"`, `"right"`, `"bottom"` and `"left"`.
#'
#' @param key Should a key be shown? In [corPlot()] this defaults to `FALSE`.
#'
#' @param auto.text Either `TRUE` (default) or `FALSE`. If `TRUE` titles and
#'   axis labels will automatically try and format pollutant names and units
#'   properly, e.g., by subscripting the `2' in NO2.
#'
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract corPlot components and plotting them in other ways.
#'
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available:
#'   - `xlab`, `ylab` and `main` override the x-axis label, y-axis label, and plot title.
#'   - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'   - `fontsize` overrides the overall font size of the plot.
#'   - `border` sets the border colour of each ellipse.
#'
#' @author David Carslaw
#' @author Jack Davison
#' @author Adapted from the approach taken by Sarkar (2007)
#'
#' @return an [openair][openair-package] object
#' @export
#'
#' @references Sarkar, D. (2007). Lattice Multivariate Data Visualization with
#'   R. New York: Springer.
#'
#'   Friendly, M. (2002). Corrgrams : Exploratory displays for correlation
#'   matrices. American Statistician, 2002(4), 1-16. doi:10.1198/000313002533
#'
#' @examples
#' # basic corrgram plot
#' corPlot(mydata)
#'
#' # plot by season
#' corPlot(mydata, type = "season")
#'
#' # recover dendrogram when cluster = TRUE and plot it
#' res <- corPlot(mydata, plot = FALSE)
#' plot(res$clust)
#' \dontrun{
#' # a more interesting are hydrocarbon measurements
#' hc <- importAURN(site = "my1", year = 2005, hc = TRUE)
#'
#' # now it is possible to see the hydrocarbons that behave most
#' # similarly to one another
#' corPlot(hc)
#' }
#'
corPlot <- function(
  mydata,
  pollutants = NULL,
  type = "default",
  cluster = TRUE,
  method = "pearson",
  use = "pairwise.complete.obs",
  annotate = c("cor", "signif", "stars", "none"),
  dendrogram = FALSE,
  triangle = c("both", "upper", "lower"),
  diagonal = TRUE,
  cols = "default",
  r.thresh = 0.8,
  text.col = c("black", "black"),
  key.header = NULL,
  key.position = "right",
  key = FALSE,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  if (rlang::is_logical(key) && !key) {
    key.position <- "none"
  }

  # extra.args setup
  extra.args <- list(...)

  # deprecated lower arg
  if ("lower" %in% names(extra.args)) {
    cli::cli_warn(
      c(
        "!" = "The {.arg lower} argument has been deprecated. Setting {.arg triangle} to 'lower'.",
        "i" = "Please use the {.arg triangle} argument directly in {.fun openair::corPlot}."
      )
    )
    triangle <- "upper"
  }

  # label controls
  extra.args$xlab <- quickText(extra.args$xlab %||% NULL, auto.text)
  extra.args$ylab <- quickText(extra.args$ylab %||% NULL, auto.text)
  extra.args$main <- quickText(extra.args$main %||% NULL, auto.text)

  # check triangle is set properly
  triangle <- rlang::arg_match(triangle)
  annotate <- rlang::arg_match(annotate)

  # if not clustering or type isn't default, can't add dendrogram
  if (!cluster || !all(type == "default")) {
    dendrogram <- FALSE
  }

  # null and all cases
  if (is.null(pollutants) || any(pollutants == "all")) {
    pollutants <- names(mydata)
  }

  # keep date if about
  pollutants <- if ("date" %in% names(mydata)) {
    unique(c("date", pollutants))
  } else {
    unique(c(pollutants))
  }

  # check input data
  mydata <- checkPrep(
    mydata,
    pollutants,
    type = type,
    remove.calm = FALSE
  )

  # cut data depending on type
  mydata <- cutData(mydata, type, ...)

  # remove variables where all are NA, or values are constant
  # done per-type to remove, e.g., lat/lng
  mydata <-
    mapType(
      mydata,
      type = type,
      fun = \(df) {
        df[, sapply(df, function(x) {
          dplyr::n_distinct(x, na.rm = TRUE) > 1L
        })]
      },
      .include_default = TRUE
    )

  # proper names of labelling
  pollutants <- names(mydata[, sapply(mydata, is.numeric)])

  # if insufficient number of variables, stop
  if (length(pollutants) < 2) {
    cli::cli_abort("Need at least two valid numeric fields to compare.")
  }

  # create plot data
  cor_data <-
    mapType(
      mydata,
      type,
      \(df) {
        # get types to bind to data
        types_df <- dplyr::distinct(df[type])

        # the tag - gives unique tag levels based on type
        tag <-
          types_df |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
          unlist() |>
          paste(collapse = "___")

        # select chosen pollutants
        df <- dplyr::select(df, dplyr::all_of(pollutants))

        # extra check for empty/constant columns
        df <- df[, sapply(df, function(x) {
          dplyr::n_distinct(x, na.rm = TRUE) > 1L
        })]

        # variables
        vars <- names(df)

        # create grid of all pollutants vs all others
        cor_matrix_df <-
          expand.grid(x = vars, y = vars, stringsAsFactors = FALSE) |>
          # act in a row-wise way
          dplyr::rowwise() |>
          # calculate correlation test scores
          dplyr::mutate(
            test = list(stats::cor.test(
              df[[.data$x]],
              df[[.data$y]],
              use = use,
              method = method,
              ...
            )),
            cor = .data$test$estimate,
            pval = .data$test$p.value,
            psig = dplyr::if_else(
              .data$pval < 0.05,
              "X",
              ""
            ),
            pstars = dplyr::case_when(
              .data$pval < 0.001 ~ "\U2736\U2736\U2736",
              .data$pval < 0.01 ~ "\U2736\U2736",
              .data$pval < 0.05 ~ "\U2736",
              TRUE ~ ""
            )
          ) |>
          # drop unnecessary test column
          dplyr::select(-"test")

        # clustering
        if (cluster) {
          # create a matrix
          cor_matrix <-
            df |>
            dplyr::select(dplyr::all_of(vars)) |>
            cor(use = use, method = method)

          # cluster the matrix
          hc <- hclust(as.dist(1 - cor_matrix), method = "complete")

          # get a new variable order
          var_order <- hc$labels[hc$order]

          # turn x/y into factor - labels are the pollutant plus the type tag
          cor_matrix_df$x <-
            factor(
              cor_matrix_df$x,
              levels = var_order,
              labels = paste(tag, var_order, sep = "___")
            )
          cor_matrix_df$y <-
            factor(
              cor_matrix_df$y,
              levels = var_order,
              labels = paste(tag, var_order, sep = "___")
            )
        } else {
          hc <- NULL
          cor_matrix_df$x <- factor(cor_matrix_df$x)
          cor_matrix_df$y <- factor(cor_matrix_df$y)
        }

        return(
          list(
            data = cor_matrix_df |>
              dplyr::mutate(tag = tag, types_df),
            hc = hc
          )
        )
      },
      .include_default = TRUE,
      .row_bind = FALSE
    )

  # get plotting data out of returned object
  plotdata <-
    purrr::map(cor_data, "data") |>
    dplyr::bind_rows()

  # if clustering, grab the first
  if (cluster) {
    hc <- purrr::map(cor_data, "hc")[[1]]
  } else {
    hc <- NULL
  }

  # remove certain cells based on triangle/diagonal args
  if (triangle == "upper") {
    plotdata <- dplyr::filter(
      plotdata,
      as.numeric(.data$x) <= as.numeric(.data$y)
    )
  }
  if (triangle == "lower") {
    plotdata <- dplyr::filter(
      plotdata,
      as.numeric(.data$x) >= as.numeric(.data$y)
    )
  }
  if (!diagonal) {
    plotdata <- dplyr::filter(
      plotdata,
      as.numeric(.data$x) != as.numeric(.data$y)
    )
  }

  # need to turn the matrix into a bigger grid of polygons for drawing angled
  # ellipses
  ellipse_data <-
    plotdata |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # if nearly 1, set to a high value - makes the ellipse visible
      cor_dummy = dplyr::if_else(dplyr::near(.data$cor, 1), 0.999, .data$cor),
      # construct ellipse data
      ellipse_data = list(
        ellipse(.data$cor_dummy) |>
          as.data.frame() |>
          dplyr::rename("xe" = "x", "ye" = "y") |>
          # ellipse returns roughly -2.5 to 2.5, needs to be -0.5 to 0.5 to fit
          # in a square grid
          dplyr::mutate(
            xe = scales::rescale(
              .data$xe,
              from = c(-2.5, 2.5),
              to = c(-0.5, 0.5)
            ),
            ye = scales::rescale(
              .data$ye,
              from = c(-2.5, 2.5),
              to = c(-0.5, 0.5)
            )
          )
      )
    ) |>
    tidyr::unnest(ellipse_data) |>
    # adjust based on value of factor
    dplyr::mutate(
      xe = .data$xe + as.numeric(.data$x),
      ye = .data$ye + as.numeric(.data$y)
    )

  # need different scales if we're using dendrograms
  if (dendrogram) {
    rlang::check_installed("legendry", version = "0.2.4")
    x_axis_scale <- function(...) {
      legendry::scale_x_dendro(clust = hc, ...)
    }
    y_axis_scale <- function(...) {
      legendry::scale_y_dendro(clust = hc, ...)
    }
  } else {
    x_axis_scale <- function(...) {
      ggplot2::scale_x_continuous(breaks = seq_along(levels(plotdata$x)), ...)
    }
    y_axis_scale <- function(...) {
      ggplot2::scale_y_continuous(breaks = seq_along(levels(plotdata$y)), ...)
    }
  }

  # strip tags away
  remove_tag <- function(x) {
    for (i in unique(paste0(plotdata$tag, "___"))) {
      x <- gsub(
        pattern = i,
        replacement = "",
        x,
        fixed = TRUE
      )
    }
    x
  }

  # construct plot
  thePlot <-
    ellipse_data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_polygon(
      ggplot2::aes(
        x = .data$xe,
        y = .data$ye,
        group = interaction(.data$x, .data$y),
        fill = .data$cor
      ),
      color = extra.args$border %||% "transparent"
    ) +
    get_facet(
      type,
      extra.args,
      scales = "free",
      auto.text = auto.text,
      # need to use ggh4x if clustered & multiple types
      independent = cluster
    ) +
    theme_openair(key.position) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), aspect.ratio = 1) +
    set_extra_fontsize(extra.args) +
    x_axis_scale(
      labels = label_openair(
        remove_tag(levels(plotdata$x)),
        auto_text = auto.text
      ),
      expand = ggplot2::expansion(c(0.01, 0.01))
    ) +
    y_axis_scale(
      labels = label_openair(
        remove_tag(levels(plotdata$y)),
        auto_text = auto.text
      ),
      expand = ggplot2::expansion(c(0.01, 0.01))
    ) +
    ggplot2::scale_fill_gradientn(
      colours = openair::openColours(cols, n = 100),
      limits = c(-1, 1),
      oob = scales::oob_squish
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        theme = ggplot2::theme(
          legend.title.position = ifelse(
            key.position %in% c("left", "right"),
            "top",
            key.position
          ),
          legend.text.position = key.position
        )
      )
    ) +
    ggplot2::labs(
      x = extra.args$xlab,
      y = extra.args$ylab,
      title = extra.args$main,
      fill = key.header
    )

  # if dendrogram, need to use legendry to switch dendro to the opposite side of
  # the plot. else just use the base ggplot2 guides to check overlaps
  if (dendrogram) {
    thePlot <- thePlot +
      ggplot2::guides(
        y = legendry::guide_axis_base(check.overlap = TRUE),
        y.sec = legendry::primitive_segments("dendro", vanish = TRUE),
        x = legendry::guide_axis_base(angle = 90, check.overlap = TRUE),
        x.sec = legendry::primitive_segments("dendro", vanish = TRUE)
      )
  } else {
    thePlot <- thePlot +
      ggplot2::guides(
        y = ggplot2::guide_axis(check.overlap = TRUE),
        x = ggplot2::guide_axis(angle = 90, check.overlap = TRUE)
      )
  }

  # add text annotations, if requested
  if (annotate != "none") {
    ellipse_data$cor_fmt <- round(ellipse_data$cor * 100)

    annotation_column <-
      dplyr::case_when(
        annotate == "cor" ~ "cor_fmt",
        annotate == "signif" ~ "psig",
        annotate == "stars" ~ "pstars",
        .default = NULL
      )

    thePlot <- thePlot +
      ggplot2::geom_text(
        data = dplyr::filter(ellipse_data, abs(.data$cor) < r.thresh),
        ggplot2::aes(
          x = as.numeric(.data$x),
          y = as.numeric(.data$y),
          label = .data[[annotation_column]],
          color = factor(sign(.data$cor), c("-1", "0", "1"))
        ),
        size = 3,
        check_overlap = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(ellipse_data, abs(.data$cor) >= r.thresh),
        ggplot2::aes(
          x = as.numeric(.data$x),
          y = as.numeric(.data$y),
          label = .data[[annotation_column]],
          color = factor(sign(.data$cor), c("-1", "0", "1"))
        ),
        size = 3,
        check_overlap = TRUE,
        fontface = "bold",
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "-1" = text.col[1],
          "0" = text.col[2],
          "1" = text.col[2]
        )
      )
  }

  # make key full width/height
  if (key.position %in% c("left", "right")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.height = ggplot2::unit(1, "null"),
        legend.key.spacing.y = ggplot2::unit(0, "cm")
      )
  }
  if (key.position %in% c("top", "bottom")) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.key.width = ggplot2::unit(1, "null"),
        legend.key.spacing.x = ggplot2::unit(0, "cm")
      )
  }

  # plot
  if (plot) {
    plot(thePlot)
  }

  # tidy newdata for output
  newdata <- plotdata
  rownames(newdata) <- NULL
  names(newdata)[names(newdata) == "z"] <- "cor"
  names(newdata)[names(newdata) == "x"] <- "row"
  names(newdata)[names(newdata) == "y"] <- "col"
  levels(newdata$row) <-
    gsub(
      pattern = unique(paste(paste0(plotdata$tag, "___"), collapse = "|")),
      replacement = "",
      levels(newdata$row)
    )
  levels(newdata$col) <-
    gsub(
      pattern = unique(paste(paste0(plotdata$tag, "___"), collapse = "|")),
      replacement = "",
      levels(newdata$col)
    )
  newdata <- dplyr::select(newdata, -"tag", -dplyr::any_of(type))

  # main handling
  output <-
    list(
      plot = thePlot,
      data = dplyr::tibble(newdata),
      call = match.call(),
      clust = hc
    )
  class(output) <- "openair"
  invisible(output)
}

# from ellipse package
ellipse <- function(
  x,
  scale = c(1, 1),
  centre = c(0, 0),
  level = 0.95,
  t = sqrt(qchisq(level, 2)),
  which = c(1, 2),
  npoints = 100,
  ...
) {
  names <- c("x", "y")
  if (is.matrix(x)) {
    xind <- which[1]
    yind <- which[2]
    r <- x[xind, yind]
    if (missing(scale)) {
      scale <- sqrt(c(x[xind, xind], x[yind, yind]))
      if (scale[1] > 0) {
        r <- r / scale[1]
      }
      if (scale[2] > 0) r <- r / scale[2]
    }
    if (!is.null(dimnames(x)[[1]])) {
      names <- dimnames(x)[[1]][c(xind, yind)]
    }
  } else {
    r <- x
  }
  r <- min(max(r, -1), 1) # clamp to -1..1, in case of rounding errors
  d <- acos(r)
  a <- seq(0, 2 * pi, len = npoints)
  matrix(
    c(
      t * scale[1] * cos(a + d / 2) + centre[1],
      t * scale[2] * cos(a - d / 2) + centre[2]
    ),
    npoints,
    2,
    dimnames = list(
      NULL,
      names
    )
  )
}
