#' K-means clustering of bivariate polar plots
#'
#' Function for identifying clusters in bivariate polar plots ([polarPlot()]);
#' identifying clusters in the original data for subsequent processing.
#'
#' Bivariate polar plots generated using the `polarPlot` function provide a
#' very useful graphical technique for identifying and characterising different
#' air pollution sources. While bivariate polar plots provide a useful graphical
#' indication of potential sources, their location and wind-speed or other
#' variable dependence, they do have several limitations. Often, a `feature'
#' will be detected in a plot but the subsequent analysis of data meeting
#' particular wind speed/direction criteria will be based only on the judgement
#' of the investigator concerning the wind speed-direction intervals of
#' interest. Furthermore, the identification of a feature can depend on the
#' choice of the colour scale used, making the process somewhat arbitrary.
#'
#' `polarCluster` applies Partition Around Medoids (PAM) clustering
#' techniques to [polarPlot()] surfaces to help identify potentially interesting
#' features for further analysis. Details of PAM can be found in the
#' `cluster` package (a core R package that will be pre-installed on all R
#' systems). PAM clustering is similar to k-means but has several advantages
#' e.g. is more robust to outliers. The clustering is based on the equal
#' contribution assumed from the u and v wind components and the associated
#' concentration. The data are standardized before clustering takes place.
#'
#' The function works best by first trying different numbers of clusters and
#' plotting them. This is achieved by setting `n.clusters` to be of length
#' more than 1. For example, if `n.clusters = 2:10` then a plot will be
#' output showing the 9 cluster levels 2 to 10.
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
#' `polarCluster` to return the original data frame together with a new
#' column `cluster`, which gives the cluster number as a character (see
#' example). Note that any rows where the value of `pollutant` is `NA`
#' are ignored so that the returned data frame may have fewer rows than the
#' original.
#'
#' Note that there are no automatic ways in ensuring the most appropriate number
#' of clusters as this is application dependent. However, there is often
#' a-priori information available on what different features in polar plots
#' correspond to. Nevertheless, the appropriateness of different clusters is
#' best determined by post-processing the data. The Carslaw and Beevers (2012)
#' paper discusses these issues in more detail.
#'
#' Note that unlike most other `openair` functions only a single
#' `type` \dQuote{default} is allowed.
#'
#' @inheritParams polarPlot
#' @param pollutant Mandatory. A pollutant name corresponding to a variable in a
#'   data frame should be supplied e.g. `pollutant = "nox"`. Only one
#'   pollutant can be chosen.
#' @param x Name of variable to plot against wind direction in polar
#'   coordinates, the default is wind speed, \dQuote{ws}.
#' @param n.clusters Number of clusters to use. If `n.clusters` is more
#'   than length 1, then a `lattice` panel plot will be output showing the
#'   clusters identified for each one of `n.clusters`.
#' @param after The function can be applied to differences between polar plot
#'   surfaces (see [polarDiff] for details). If an `after` data frame
#'   is supplied, the clustering will be carried out on the differences between
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
#' @import cluster
#' @return an [openair][openair-package] object. The object includes four main
#'   components: `call`, the command used to generate the plot;
#'   `data`, by default the original data frame with a new field
#'   `cluster` identifying the cluster, `clust_stats` giving the
#'   contributions made by each cluster to number of measurements, their
#'   percentage and the percentage by pollutant; and `plot`, the plot
#'   itself. Note that any rows where the value of `pollutant` is `NA`
#'   are ignored so that the returned data frame may have fewer rows than the
#'   original.
#'
#'   If the clustering is carried out considering differences, i.e., an
#'   `after` data frame is supplied, the output also includes the
#'   `after` data frame with cluster identified.
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
#' ## plot 2-8 clusters. Warning! This can take several minutes...
#' polarCluster(mydata, pollutant = "nox", n.clusters = 2:8)
#'
#' # basic plot with 6 clusters
#' results <- polarCluster(mydata, pollutant = "nox", n.clusters = 6)
#'
#' ## get results, could read into a new data frame to make it easier to refer to
#' ## e.g. results <- results$data...
#' head(results$data)
#'
#' ## how many points are there in each cluster?
#' table(results$data$cluster)
#'
#' ## plot clusters 3 and 4 as a timeVariation plot using SAME colours as in
#' ## cluster plot
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
    ## avoid R check annoyances
    u <- v <- z <- strip <- strip.left <- NULL

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
      trellis.par.set(list(strip.background = list(col = "white")))
    }

    ## set graphics
    current.strip <- trellis.par.get("strip.background")
    current.font <- trellis.par.get("fontsize")

    ## reset graphic parameters
    on.exit(trellis.par.set(fontsize = current.font))

    # add id for later merging
    mydata <- mutate(mydata, .id = 1:nrow(mydata))

    if (is.data.frame(after)) {
      after <- mutate(after, .id = 1:nrow(after))
      data.orig.after <- after
    }

    data.orig <-
      mydata ## keep original data so cluster can be merged with it
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

    extra.args <- list(...)

    ## label controls
    extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
      quickText(extra.args$xlab, auto.text)
    } else {
      quickText("", auto.text)
    }
    extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
      quickText(extra.args$ylab, auto.text)
    } else {
      quickText("", auto.text)
    }
    extra.args$main <- if ("main" %in% names(extra.args)) {
      quickText(extra.args$main, auto.text)
    } else {
      quickText("", auto.text)
    }

    if ("fontsize" %in% names(extra.args)) {
      trellis.par.set(fontsize = list(text = extra.args$fontsize))
    }

    ## layout default
    if (!"layout" %in% names(extra.args)) {
      extra.args$layout <- NULL
    }

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

    ## remove missing because we don't want to find clusters for those points
    ## saves a lot on computation
    results.grid <- na.omit(results.grid)
    results.grid <- subset(results.grid, select = c(u, v, z))

    ## sequence of u or v, based on unique values that already exist
    uv.id <- with(results.grid, sort(unique(c(u, v))))

    make.clust <- function(i, results.grid) {
      i <- n.clusters[i]
      dat.orig <- results.grid
      clusters <- pam(results.grid, i, stand = TRUE, pamonce = 3)
      dat.orig$cluster <- clusters$clustering
      dat.orig$nclust <- paste(i, "clusters")
      dat.orig
    }

    results.grid <-
      purrr::map(
        .x = seq_along(n.clusters),
        .f = make.clust,
        results.grid = results.grid,
        .progress = "Calculating Clusters"
      ) |>
      purrr::list_rbind()

    results.grid$nclust <-
      ordered(results.grid$nclust, levels = paste(n.clusters, "clusters"))

    ## auto scaling
    nlev <- max(n.clusters) + 1
    breaks <- c(0, 1:max(n.clusters))
    nlev2 <- length(breaks)
    col <- openColours(cols, (nlev2 - 1))
    col.scale <- breaks

    myform <- formula("cluster ~ u * v | nclust")

    ## find ids of u and v if only one cluster used
    if (length(n.clusters) == 1L) {
      ## find indices in u-v space
      results.grid$u.id <- findInterval(results.grid$u, uv.id)
      results.grid$v.id <- findInterval(results.grid$v, uv.id)

      mydata <- na.omit(mydata)

      mydata <- transform(
        mydata,
        u = get(x) * sin(wd * pi / 180),
        v = get(x) * cos(wd * pi / 180)
      )
      mydata$u.id <- findInterval(mydata$u, uv.id, all.inside = TRUE)
      mydata$v.id <- findInterval(mydata$v, uv.id, all.inside = TRUE)

      ## convert to matrix for direct lookup
      ## need to do this because some data are missing due to exclude.missing in polarPlot
      mat.dim <-
        max(results.grid[, c("u.id", "v.id")]) ## size of lookup matrix
      temp <- matrix(NA, ncol = mat.dim, nrow = mat.dim)

      ## matrix of clusters by u.id, v.id with missings
      temp[cbind(results.grid$u.id, results.grid$v.id)] <-
        results.grid$cluster

      ## match u.id, v.id in mydata to cluster
      mydata$cluster <-
        as.factor(temp[cbind(mydata$u.id, mydata$v.id)])

      mydata <-
        select(mydata, date, cluster, .id) ## just need date/cluster
      mydata <- left_join(data.orig, mydata, by = c(".id", "date"))
      results <- mydata
      myform <- formula("cluster ~ u * v")

      # also find clusters in after data if there is any
      if (is.data.frame((after))) {
        after <- na.omit(after)

        after <- transform(
          after,
          u = get(x) * sin(wd * pi / 180),
          v = get(x) * cos(wd * pi / 180)
        )
        after$u.id <- findInterval(after$u, uv.id, all.inside = TRUE)
        after$v.id <- findInterval(after$v, uv.id, all.inside = TRUE)

        ## convert to matrix for direct lookup
        ## need to do this because some data are missing due to exclude.missing in polarPlot
        mat.dim <-
          max(results.grid[, c("u.id", "v.id")]) ## size of lookup matrix
        temp <- matrix(NA, ncol = mat.dim, nrow = mat.dim)

        ## matrix of clusters by u.id, v.id with missings
        temp[cbind(results.grid$u.id, results.grid$v.id)] <-
          results.grid$cluster

        ## match u.id, v.id in after to cluster
        after$cluster <-
          as.factor(temp[cbind(after$u.id, after$v.id)])

        after <-
          select(after, date, cluster, .id) ## just need date/cluster
        after <-
          left_join(data.orig.after, after, by = c(".id", "date"))
      }
    }

    ## scaling of 'zeroed' data
    ## scale data by subtracting the min value
    ## this helps with dealing with data with offsets - e.g. negative data
    mydata[[x]] <- mydata[[x]] - min(mydata[[x]], na.rm = TRUE)
    intervals <- pretty(range(mydata[, x], na.rm = TRUE))

    ## labels for scaling
    labels <- pretty(intervals + min.scale)
    upper <- max(mydata[[x]], na.rm = TRUE)

    ## offset the lines/labels if necessary
    intervals <- intervals + (min(labels) - min.scale)

    ## add zero in the middle if it exists
    if (min.scale != 0) {
      labels <- labels[-1]
      intervals <- intervals[-1]
    }

    # interpolate grid, but first must make rectangular

    # interval
    int <-
      as.numeric(tail(
        names(sort(table(
          diff(results.grid$u)
        ))),
        1
      ))

    # extent of grid required
    extent <- max(abs(c(results.grid$u, results.grid$v)))

    new_grid <- expand.grid(
      u = seq(-extent, extent, by = int),
      v = seq(-extent, extent, by = int)
    )

    levelplot.args <- list(
      x = myform,
      results.grid,
      axes = FALSE,
      as.table = TRUE,
      col.regions = col,
      region = TRUE,
      aspect = 1,
      at = col.scale,
      par.strip.text = list(cex = 0.8),
      scales = list(draw = FALSE),
      xlim = c(-upper * 1.025, upper * 1.025),
      ylim = c(-upper * 1.025, upper * 1.025),
      colorkey = FALSE,
      # legend = legend,
      key = list(
        rectangles = list(
          col = openColours(
            cols,
            max(n.clusters)
          ),
          border = NA
        ),
        text = list(lab = as.character(1:max(n.clusters))),
        space = "right",
        columns = 1,
        title = "cluster",
        cex.title = 1,
        lines.title = 2
      ),
      panel = function(x, y, z, subscripts, ...) {
        panel.levelplot(
          x,
          y,
          z,
          subscripts,
          at = col.scale,
          pretty = TRUE,
          col.regions = col,
          labels = FALSE
        )

        angles <- seq(0, 2 * pi, length = 360)

        sapply(intervals, function(x) {
          llines(
            x * sin(angles),
            x * cos(angles),
            col = "grey",
            lty = 5
          )
        })

        ltext(
          1.07 * intervals * sin(pi * angle.scale / 180),
          1.07 * intervals * cos(pi * angle.scale / 180),
          sapply(
            paste(
              labels,
              c(
                "",
                "",
                units,
                rep("", 7)
              )
            ),
            function(x) {
              quickText(x, auto.text)
            }
          ),
          cex = 0.7,
          pos = 4
        )

        ## add axis line to central polarPlot
        larrows(-upper, 0, upper, 0, code = 3, length = 0.1)
        larrows(0, -upper, 0, upper, code = 3, length = 0.1)

        ltext(upper * -1 * 0.95, 0.07 * upper, "W", cex = 0.7)
        ltext(0.07 * upper, upper * -1 * 0.95, "S", cex = 0.7)
        ltext(0.07 * upper, upper * 0.95, "N", cex = 0.7)
        ltext(upper * 0.95, 0.07 * upper, "E", cex = 0.7)
      }
    )

    ## reset for extra.args
    levelplot.args <- listUpdate(levelplot.args, extra.args)

    ## plot
    plt <- do.call(levelplot, levelplot.args)

    ## output ################################################################
    if (plot) {
      if (length(type) == 1L) {
        plot(plt)
      } else {
        plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
      }
    }

    out_data <- results

    # currently conflicts with length(n.clusters) > 1 - resolve
    if (is.function(out_data)) {
      out_data <- NULL
    }

    # change cluster output to C1, C2 etc
    if ("cluster" %in% names(out_data)) {
      out_data$cluster <- paste("C", out_data$cluster, sep = "")
      out_data$cluster[out_data$cluster == "CNA"] <- NA_character_
    }

    # print the stats but only for cluster of length 1

    var_mean <- paste0("mean_", pollutant)
    var_percent <- paste0(pollutant, "_percent")

    if (length(n.clusters) == 1L && length(pollutant) == 1L) {
      clust_stats <-
        out_data |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(
          {{ var_mean }} := mean(.data[[pollutant]], na.rm = TRUE),
          n = dplyr::n()
        ) |>
        na.omit() |>
        dplyr::mutate(
          n_mean = n * .data[[var_mean]],
          n_percent = round(100 * n / sum(n), 1),
          {{ var_percent }} := round(100 * n_mean / sum(n_mean), 1)
        ) |>
        dplyr::select(-n_mean)
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
          plot = plt,
          data = out_data,
          after = after,
          call = match.call(),
          clust_stats = clust_stats
        )
    } else {
      output <- list(
        plot = plt,
        data = out_data,
        call = match.call(),
        clust_stats = clust_stats
      )
    }

    class(output) <- "openair"
    invisible(output)
  }
