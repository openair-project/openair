#' Calculate clusters for back trajectories
#'
#' This function carries out cluster analysis of HYSPLIT back trajectories. The
#' function is specifically designed to work with the trajectories imported
#' using the `openair` `importTraj` function, which provides
#' pre-calculated back trajectories at specific receptor locations.
#'
#' Two main methods are available to cluster the back trajectories using two
#' different calculations of the distance matrix. The default is to use the
#' standard Euclidian distance between each pair of trajectories. Also available
#' is an angle-based distance matrix based on Sirois and Bottenheim (1995). The
#' latter method is useful when the interest is the direction of the
#' trajectories in clustering.
#'
#' The distance matrix calculations are made in C++ for speed. For data sets of
#' up to 1 year both methods should be relatively fast, although the
#' `method = "Angle"` does tend to take much longer to calculate. Further
#' details of these methods are given in the openair manual.
#'
#' @inheritParams trajPlot
#' @param traj An openair trajectory data frame resulting from the use of
#'   `importTraj`.
#' @param method Method used to calculate the distance matrix for the back
#'   trajectories. There are two methods available: \dQuote{Euclid} and
#'   \dQuote{Angle}.
#' @param n.cluster Number of clusters to calculate.
#' @param type `type` determines how the data are split i.e. conditioned,
#'   and then plotted. The default is will produce a single plot using the
#'   entire data. Type can be one of the built-in types as detailed in
#'   `cutData` e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so
#'   on. For example, `type = "season"` will produce four plots --- one for
#'   each season. Note that the cluster calculations are separately made of each
#'   level of "type".
#' @param cols Colours to be used for plotting. Options include
#'   \dQuote{default}, \dQuote{increment}, \dQuote{heat}, \dQuote{jet} and
#'   `RColorBrewer` colours --- see the `openair` `openColours`
#'   function for more details. For user defined the user can supply a list of
#'   colour names recognised by R (type `colours()` to see the full list).
#'   An example would be `cols = c("yellow", "green", "blue")`
#' @param split.after For `type` other than \dQuote{default} e.g.
#'   \dQuote{season}, the trajectories can either be calculated for each level
#'   of `type` independently or extracted after the cluster calculations
#'   have been applied to the whole data set.
#' @param by.type The percentage of the total number of trajectories is given
#'   for all data by default. Setting `by.type = TRUE` will make each panel
#'   add up to 100.
#' @param plot Should a plot be produced? `FALSE` can be useful when
#'   analysing data to extract plot components and plotting them in other ways.
#' @param ... Other graphical parameters passed onto `lattice:levelplot`
#'   and `cutData`. Similarly, common axis and title labelling options
#'   (such as `xlab`, `ylab`, `main`) are passed to
#'   `levelplot` via `quickText` to handle routine formatting.
#' @export
#' @useDynLib openair, .registration = TRUE
#' @import cluster
#' @return an [openair][openair-package] object. The `data` component contains
#'   both `traj` (the original data appended with its cluster) and `results`
#'   (the average trajectory path per cluster, shown in the `trajCluster()`
#'   plot.)
#' @family trajectory analysis functions
#' @family cluster analysis functions
#' @author David Carslaw
#' @references
#'
#' Sirois, A. and Bottenheim, J.W., 1995. Use of backward trajectories to
#' interpret the 5-year record of PAN and O3 ambient air concentrations at
#' Kejimkujik National Park, Nova Scotia. Journal of Geophysical Research, 100:
#' 2867-2881.
#' @examples
#' \dontrun{
#' ## import trajectories
#' traj <- importTraj(site = "london", year = 2009)
#' ## calculate clusters
#' clust <- trajCluster(traj, n.cluster = 5)
#' head(clust$data) ## note new variable 'cluster'
#' ## use different distance matrix calculation, and calculate by season
#' traj <- trajCluster(traj, method = "Angle", type = "season", n.cluster = 4)
#' }
trajCluster <- function(
  traj,
  method = "Euclid",
  n.cluster = 5,
  type = "default",
  cols = "Set1",
  split.after = FALSE,
  map.fill = TRUE,
  map.cols = "grey40",
  map.border = "black",
  map.alpha = 0.4,
  map.lwd = 1,
  map.lty = 1,
  projection = "lambert",
  parameters = c(51, 51),
  orientation = c(90, 0, 0),
  by.type = FALSE,
  origin = TRUE,
  plot = TRUE,
  ...
) {
  # silence R check
  freq <- hour.inc <- default <- NULL

  if (tolower(method) == "euclid") {
    method <- "distEuclid"
  } else {
    method <- "distAngle"
  }

  # remove any missing lat/lon
  traj <- filter(traj, !is.na(lat), !is.na(lon))

  # check to see if all back trajectories are the same length
  traj <- group_by(traj, date) |>
    mutate(traj_len = length(date))

  if (length(unique(traj$traj_len)) > 1) {
    ux <- unique(traj$traj_len)
    nmax <- ux[which.max(tabulate(match(traj$traj_len, ux)))]
    traj <- ungroup(traj) |>
      filter(traj_len == nmax)
  }

  Args <- list(...)

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  ## label controls
  Args$plot.type <- if ("plot.type" %in% names(Args)) {
    Args$plot.type
  } else {
    Args$plot.type <- "l"
  }
  Args$lwd <- if ("lwd" %in% names(Args)) {
    Args$lwd
  } else {
    Args$lwd <- 4
  }

  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  calcTraj <- function(traj) {
    ## make sure ordered correctly
    traj <- traj[order(traj$date, traj$hour.inc), ]

    ## length of back trajectories
    traj <- group_by(traj, date) |>
      mutate(len = length(date))

    ## find length of back trajectories
    ## 96-hour back trajectories with origin: length should be 97
    n <- max(abs(traj$hour.inc)) + 1

    traj <- subset(traj, len == n)
    len <- nrow(traj) / n

    ## lat/lon input matrices
    x <- matrix(traj$lon, nrow = n)
    y <- matrix(traj$lat, nrow = n)

    z <- matrix(0, nrow = n, ncol = len)
    res <- matrix(0, nrow = len, ncol = len)

    if (method == "distEuclid") {
      res <- .Call("distEuclid", x, y, res)
    }

    if (method == "distAngle") {
      res <- .Call("distAngle", x, y, res)
    }

    res[is.na(res)] <- 0 ## possible for some to be NA if trajectory does not move between two hours?

    dist.res <- as.dist(res)
    clusters <- pam(dist.res, n.cluster)
    cluster <- rep(clusters$clustering, each = n)
    traj$cluster <- as.character(paste("C", cluster, sep = ""))
    traj
  }

  ## this bit decides whether to separately calculate trajectories for each level of type

  if (split.after) {
    traj$default <- "default"
    traj <- mapType(
      traj,
      type = "default",
      fun = calcTraj
    )
    traj <- cutData(traj, type)
  } else {
    traj <- cutData(traj, type)
    traj <- mapType(
      traj,
      type = type,
      fun = calcTraj
    )
  }

  # trajectory origin
  origin_xy <- head(subset(traj, hour.inc == 0), 1) ## origin
  tmp <- mapproject(
    x = origin_xy[["lon"]][1],
    y = origin_xy[["lat"]][1],
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )
  receptor <- c(tmp$x, tmp$y)

  ## calculate the mean trajectories by cluster

  vars <- c("lat", "lon", "date", "cluster", "hour.inc", type)
  vars2 <- c("cluster", "hour.inc", type)

  agg <- select(traj, vars) |>
    group_by(across(vars2)) |>
    summarise(across(everything(), mean))

  # the data frame we want to return before it is transformed
  resRtn <- agg

  ## proportion of total clusters

  vars <- c(type, "cluster")

  clusters <- traj |>
    group_by(across(vars)) |>
    tally() |>
    mutate(freq = round(100 * n / sum(n), 1))

  ## make each panel add up to 100
  if (by.type) {
    clusters <- clusters |>
      group_by(across(type)) |>
      mutate(freq = 100 * freq / sum(freq))

    clusters$freq <- round(clusters$freq, 1)
  }

  ## make sure date is in correct format
  class(agg$date) <- class(traj$date)
  attr(agg$date, "tzone") <- "GMT"

  ## xlim and ylim set by user
  if (!"xlim" %in% names(Args)) {
    Args$xlim <- range(agg$lon)
  }

  if (!"ylim" %in% names(Args)) {
    Args$ylim <- range(agg$lat)
  }

  ## extent of data (or limits set by user) in degrees
  trajLims <- c(Args$xlim, Args$ylim)

  ## need *outline* of boundary for map limits
  Args <- setTrajLims(traj, Args, projection, parameters, orientation)

  ## transform data for map projection
  tmp <- mapproject(
    x = agg[["lon"]],
    y = agg[["lat"]],
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )
  agg[["lon"]] <- tmp$x
  agg[["lat"]] <- tmp$y

  plot.args <- list(
    agg,
    x = "lon",
    y = "lat",
    group = "cluster",
    col = cols,
    type = type,
    map = TRUE,
    map.fill = map.fill,
    map.cols = map.cols,
    map.border = map.border,
    map.alpha = map.alpha,
    map.lwd = map.lwd,
    map.lty = map.lty,
    projection = projection,
    parameters = parameters,
    orientation = orientation,
    traj = TRUE,
    trajLims = trajLims,
    clusters = clusters,
    receptor = receptor,
    origin = origin
  )

  ## reset for Args
  plot.args <- listUpdate(plot.args, Args)

  plot.args <- listUpdate(plot.args, list(plot = plot))

  ## plot
  plt <- do.call(scatterPlot, plot.args)

  ## create output with plot
  output <-
    list(
      plot = plt,
      data = list(
        traj = traj,
        results = dplyr::left_join(resRtn, clusters, by = c("cluster", type)),
        subsets = c("traj", "results")
      ),
      call = match.call()
    )
  class(output) <- "openair"
  invisible(output)
}
