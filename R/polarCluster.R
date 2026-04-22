#' K-means clustering of bivariate polar plots
#'
#' Function for identifying clusters in bivariate polar plots ([polarPlot()]);
#' identifying clusters in the original data for subsequent processing.
#'
#' Bivariate polar plots generated using the `polarPlot` function provide a very
#' useful graphical technique for identifying and characterising different air
#' pollution sources. While bivariate polar plots provide a useful graphical
#' indication of potential sources, their location and wind-speed or other
#' variable dependence, they do have several limitations. Often, a `feature'
#' will be detected in a plot but the subsequent analysis of data meeting
#' particular wind speed/direction criteria will be based only on the judgement
#' of the investigator concerning the wind speed-direction intervals of
#' interest. Furthermore, the identification of a feature can depend on the
#' choice of the colour scale used, making the process somewhat arbitrary.
#'
#' `polarCluster` applies Partition Around Medoids (PAM) clustering techniques
#' to [polarPlot()] surfaces to help identify potentially interesting features
#' for further analysis. Details of PAM can be found in the `cluster` package (a
#' core R package that will be pre-installed on all R systems). PAM clustering
#' is similar to k-means but has several advantages e.g. is more robust to
#' outliers. The clustering is based on the equal contribution assumed from the
#' u and v wind components and the associated concentration. The data are
#' standardized before clustering takes place.
#'
#' The function works best by first trying different numbers of clusters and
#' plotting them. This is achieved by setting `n.clusters` to be of length more
#' than 1. For example, if `n.clusters = 2:10` then a plot will be output
#' showing the 9 cluster levels 2 to 10.
#'
#' The clustering can also be applied to differences in polar plot surfaces (see
#' [polarDiff()]). On this case a second data frame (`after`) should be
#' supplied.
#'
#' Note that clustering is computationally intensive and the function can take a
#' long time to run --- particularly when the number of clusters is increased.
#' For this reason it can be a good idea to run a few clusters first to get a
#' feel for it e.g. `n.clusters = 2:5`.
#'
#' Once the number of clusters has been decided, the user can then run
#' `polarCluster` to return the original data frame together with a new column
#' `cluster`, which gives the cluster number as a character (see example). Note
#' that any rows where the value of `pollutant` is `NA` are ignored so that the
#' returned data frame may have fewer rows than the original.
#'
#' Note that there are no automatic ways in ensuring the most appropriate number
#' of clusters as this is application dependent. However, there is often
#' a-priori information available on what different features in polar plots
#' correspond to. Nevertheless, the appropriateness of different clusters is
#' best determined by post-processing the data. The Carslaw and Beevers (2012)
#' paper discusses these issues in more detail.
#'
#' Note that unlike most other `openair` functions only a single `type`
#' \dQuote{default} is allowed.
#'
#' @inheritParams polarPlot
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`. Only one pollutant
#'   can be chosen.
#' @param x Name of variable to plot against wind direction in polar
#'   coordinates, the default is wind speed, \dQuote{ws}.
#' @param n.clusters Number of clusters to use. If `n.clusters` is more than
#'   length 1, then a faceted plot will be output showing the clusters
#'   identified for each one of `n.clusters`.
#' @param after The function can be applied to differences between polar plot
#'   surfaces (see [polarDiff] for details). If an `after` data frame is
#'   supplied, the clustering will be carried out on the differences between
#'   `after` and `mydata` in the same way as [polarDiff].
#' @param plot.data By default, the `data` component of `polarCluster()`
#'   contains the original data frame appended with a new "cluster" column. When
#'   `plot.data = TRUE`, the `data` component instead contains data to reproduce
#'   the clustered polar plot itself (similar to `data` returned by
#'   [polarPlot()]). This may be useful for re-plotting the `polarCluster()`
#'   plot in other ways.
#' @inheritDotParams polarPlot -mydata -pollutant -x -wd -cols -angle.scale
#'   -units -auto.text -plot
#' @export
#' @return an [openair][openair-package] object. The object includes four main
#'   components: `call`, the command used to generate the plot; `data`, by
#'   default the original data frame with a new field `cluster` identifying the
#'   cluster, `clust_stats` giving the contributions made by each cluster to
#'   number of measurements, their percentage and the percentage by pollutant;
#'   and `plot`, the plot itself. Note that any rows where the value of
#'   `pollutant` is `NA` are ignored so that the returned data frame may have
#'   fewer rows than the original.
#'
#'   If the clustering is carried out considering differences, i.e., an `after`
#'   data frame is supplied, the output also includes the `after` data frame
#'   with cluster identified.
#' @author David Carslaw
#' @family polar directional analysis functions
#' @family cluster analysis functions
#' @references
#'
#' Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). Detecting and
#' quantifying aircraft and other on-airport contributions to ambient nitrogen
#' oxides in the vicinity of a large international airport.  Atmospheric
#' Environment. 40/28 pp 5424-5434.
#'
#' Carslaw, D.C., & Beevers, S.D. (2013). Characterising and understanding
#' emission sources using bivariate polar plots and k-means clustering.
#' Environmental Modelling & Software, 40, 325-329.
#' doi:10.1016/j.envsoft.2012.09.005
#' @examples
#' \dontrun{
#' # plot 2-8 clusters. Warning! This can take several minutes...
#' polarCluster(mydata, pollutant = "nox", n.clusters = 2:8)
#'
#' # basic plot with 6 clusters
#' results <- polarCluster(mydata, pollutant = "nox", n.clusters = 6)
#'
#' # get results, could read into a new data frame to make it easier to refer to
#' # e.g. results <- results$data...
#' head(results$data)
#'
#' # how many points are there in each cluster?
#' table(results$data$cluster)
#'
#' # plot clusters 3 and 4 as a timeVariation plot using SAME colours as in
#' # cluster plot
#' timeVariation(subset(results$data, cluster %in% c("3", "4")),
#'   pollutant = "nox",
#'   group = "cluster", col = openColours("Paired", 6)[c(3, 4)]
#' )
#' }
#'
polarCluster <-
  function(
    mydata,
    pollutant = "nox",
    x = "ws",
    wd = "wd",
    n.clusters = 6,
    after = NA,
    cols = "Paired",
    angle.scale = 315,
    units = x,
    auto.text = TRUE,
    plot = TRUE,
    plot.data = FALSE,
    ...
  ) {
    # avoid R check annoyances
    u <- v <- z <- strip <- strip.left <- NULL

    # add id for later merging
    mydata <- dplyr::mutate(mydata, .id = seq_len(nrow(mydata)))

    if (is.data.frame(after)) {
      after <- dplyr::mutate(after, .id = seq_len(nrow(after)))
      data.orig.after <- after
    }

    data.orig <-
      mydata # keep original data so cluster can be merged with it
    type <- "default"
    vars <- c("wd", x, pollutant)
    vars <- c(vars, "date", ".id")

    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    if (is.data.frame(after)) {
      after <- checkPrep(after, vars, type, remove.calm = FALSE)
    }

    max.x <- ceiling(max(mydata[, x], na.rm = TRUE))
    min.ws <- floor(min(mydata[[x]], na.rm = TRUE))
    upper <- max.x

    min.scale <- floor(min(mydata[[x]], na.rm = TRUE))

    extra.args <- capture_dots(...)

    # label controls
    extra.args$xlab <- quickText(extra.args$xlab, auto.text)
    extra.args$ylab <- quickText(extra.args$ylab, auto.text)
    extra.args$title <- quickText(extra.args$title, auto.text)
    extra.args$subtitle <- quickText(extra.args$subtitle, auto.text)
    extra.args$caption <- quickText(extra.args$caption, auto.text)
    extra.args$tag <- quickText(extra.args$tag, auto.text)

    # if considering differences
    if (is.data.frame(after)) {
      results.grid <- polarDiff(
        before = mydata,
        after = after,
        plot = FALSE,
        pollutant = pollutant,
        cluster = TRUE,
        ...
      )$data

      results.grid$z <- results.grid[[pollutant]]
      results.grid$x <- sqrt(results.grid$u^2 + results.grid$v^2)
    } else {
      results.grid <- polarPlot(
        mydata,
        plot = FALSE,
        pollutant = pollutant,
        x = x,
        cluster = TRUE,
        ...
      )$data
    }

    # remove missing because we don't want to find clusters for those points
    # saves a lot on computation
    results.grid <- stats::na.omit(results.grid)
    cols_to_keep <- c("u", "v", "z", "x", wd)
    results.grid <- dplyr::select(
      results.grid,
      dplyr::all_of(cols_to_keep)
    )

    # sequence of u or v, based on unique values that already exist
    uv.id <- with(results.grid, sort(unique(c(u, v))))

    make.clust <- function(i, results.grid) {
      i <- n.clusters[i]
      dat.orig <- results.grid
      clusters <- cluster::pam(results.grid, i, stand = TRUE, pamonce = 3)
      dat.orig$cluster <- clusters$clustering
      dat.orig$nclust <- paste(i, "clusters")
      dat.orig
    }

    results.grid <-
      purrr::map(
        .x = seq_along(n.clusters),
        .f = make.clust,
        results.grid = results.grid
      ) |>
      purrr::list_rbind()

    results.grid$nclust <-
      ordered(results.grid$nclust, levels = paste(n.clusters, "clusters"))

    # auto scaling
    nlev <- max(n.clusters) + 1
    breaks <- c(0, 1:max(n.clusters))
    nlev2 <- length(breaks)
    col <- resolve_colour_opts(cols, (nlev2 - 1))
    col.scale <- breaks

    myform <- stats::formula("cluster ~ u * v | nclust")

    # find ids of u and v if only one cluster used
    if (length(n.clusters) == 1L) {
      # find indices in u-v space
      results.grid$u.id <- findInterval(results.grid$u, uv.id)
      results.grid$v.id <- findInterval(results.grid$v, uv.id)

      # build cluster lookup matrix once (some cells missing due to exclude.missing in polarPlot)
      mat.dim <- max(results.grid[, c("u.id", "v.id")])
      clust_mat <- matrix(NA, ncol = mat.dim, nrow = mat.dim)
      clust_mat[cbind(
        results.grid$u.id,
        results.grid$v.id
      )] <- results.grid$cluster

      # map a data frame's observations to clusters via u-v grid lookup
      map_to_clusters <- function(df) {
        df <- stats::na.omit(df)
        df <- transform(
          df,
          u = get(x) * sin(wd * pi / 180),
          v = get(x) * cos(wd * pi / 180)
        )
        df$u.id <- findInterval(df$u, uv.id, all.inside = TRUE)
        df$v.id <- findInterval(df$v, uv.id, all.inside = TRUE)
        df$cluster <- as.factor(clust_mat[cbind(df$u.id, df$v.id)])
        dplyr::select(df, "date", "cluster", ".id")
      }

      mydata <- stats::na.omit(mydata)
      results <- dplyr::left_join(
        data.orig,
        map_to_clusters(mydata),
        by = c(".id", "date")
      )
      myform <- stats::formula("cluster ~ u * v")

      if (is.data.frame(after)) {
        after <- dplyr::left_join(
          data.orig.after,
          map_to_clusters(after),
          by = c(".id", "date")
        )
      }
    }

    key.position <- extra.args$key.position %||% "right"

    # plot clusters
    thePlot <-
      results.grid |>
      dplyr::arrange(!is.na(.data$cluster), .data$cluster) |>
      ggplot2::ggplot(ggplot2::aes(x = .data$wd, y = .data$x)) +
      ggplot2::geom_point(
        ggplot2::aes(
          colour = factor(.data$cluster, levels = sort(unique(.data$cluster)))
        ),
        shape = 15,
        show.legend = TRUE
      ) +
      ggplot2::ggproto(
        NULL,
        ggplot2::coord_radial(r.axis.inside = angle.scale),
        inner_radius = c(0, 1) * 0.475
      ) +
      scale_x_compass() +
      ggplot2::scale_y_continuous(
        limits = range(pretty(results.grid$x, 6)),
        expand = ggplot2::expansion()
      ) +
      ggplot2::scale_color_manual(
        values = resolve_colour_opts(
          cols,
          n = dplyr::n_distinct(results.grid$cluster)
        ),
        drop = FALSE
      ) +
      theme_openair_radial(
        key.position %||% "right",
        extra.args = extra.args,
        panel.ontop = TRUE
      ) +
      annotate_compass_points(
        size = ifelse(
          extra.args$annotate %||% TRUE,
          if (is.null(extra.args$fontsize)) 3 else extra.args$fontsize / 3,
          0
        )
      ) +
      ggplot2::labs(
        color = "Cluster",
        x = extra.args$xlab,
        y = extra.args$ylab,
        title = extra.args$title,
        subtitle = extra.args$subtitle,
        caption = extra.args$caption,
        tag = extra.args$tag
      ) +
      get_facet(
        ifelse(dplyr::n_distinct(results.grid$nclust) > 1, "nclust", "default"),
        extra.args = extra.args,
        auto.text = auto.text,
        wd.res = extra.args$wd.res %||% 8
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          override.aes = list(size = 5),
          theme = ggplot2::theme(
            legend.title.position = ifelse(
              key.position %in% c("left", "right"),
              "top",
              key.position
            ),
            legend.text.position = key.position
          )
        )
      )

    # output
    if (plot) {
      plot(thePlot)
    }

    out_data <- results
    # currently conflicts with length(n.clusters) > 1 - resolve
    if (is.function(out_data)) {
      out_data <- NULL
    }

    # change cluster output to C1, C2 etc
    if ("cluster" %in% names(out_data)) {
      out_data$cluster <- paste0("C", out_data$cluster)
      out_data$cluster[out_data$cluster == "CNA"] <- NA_character_
    }

    if (is.data.frame(after) && "cluster" %in% names(after)) {
      after$cluster <- paste0("C", after$cluster)
      after$cluster[after$cluster == "CNA"] <- NA_character_
    }

    # print the stats but only for cluster of length 1

    var_mean <- paste0("mean_", pollutant)
    var_percent <- paste0(pollutant, "_percent")

    if (length(n.clusters) == 1L && length(pollutant) == 1L) {
      clust_stats <-
        out_data |>
        dplyr::group_by(.data$cluster) |>
        dplyr::summarise(
          {{ var_mean }} := mean(.data[[pollutant]], na.rm = TRUE),
          n = dplyr::n()
        ) |>
        stats::na.omit() |>
        dplyr::mutate(
          n_mean = .data$n * .data[[var_mean]],
          n_percent = round(100 * .data$n / sum(.data$n), 1),
          {{ var_percent }} := round(100 * .data$n_mean / sum(.data$n_mean), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-"n_mean")
    } else {
      clust_stats <- NULL
    }

    if (plot && length(n.clusters) == 1L) {
      print(clust_stats)
    }

    if (plot.data) {
      out_data <- results.grid
    }

    # output
    if (is.data.frame(after)) {
      output <-
        list(
          plot = thePlot,
          data = out_data,
          after = after,
          call = match.call(),
          clust_stats = clust_stats
        )
    } else {
      output <- list(
        plot = thePlot,
        data = out_data,
        call = match.call(),
        clust_stats = clust_stats
      )
    }

    class(output) <- "openair"
    invisible(output)
  }
