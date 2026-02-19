#' Calculate clusters for back trajectories
#'
#' This function carries out cluster analysis of HYSPLIT back trajectories. The
#' function is specifically designed to work with the trajectories imported
#' using the `openair` [importTraj()] function, which provides pre-calculated
#' back trajectories at specific receptor locations.
#'
#' Two main methods are available to cluster the back trajectories using two
#' different calculations of the distance matrix. The default is to use the
#' standard Euclidian distance between each pair of trajectories. Also available
#' is an angle-based distance matrix based on Sirois and Bottenheim (1995). The
#' latter method is useful when the interest is the direction of the
#' trajectories in clustering.
#'
#' The distance matrix calculations are made in C++ for speed. For data sets of
#' up to 1 year both methods should be relatively fast, although the `method =
#' "Angle"` does tend to take much longer to calculate. Further details of these
#' methods are given in the openair manual.
#'
#' @inheritParams trajPlot
#' @param traj An openair trajectory data frame resulting from the use of
#'   [importTraj()].
#' @param method Method used to calculate the distance matrix for the back
#'   trajectories. There are two methods available: \dQuote{Euclid} and
#'   \dQuote{Angle}.
#' @param n.cluster Number of clusters to calculate.
#' @param type `type` determines how the data are split i.e. conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in `cutData` e.g.
#'   \dQuote{season}, \dQuote{year}, \dQuote{weekday} and so on. For example,
#'   `type = "season"` will produce four plots --- one for each season. Note
#'   that the cluster calculations are separately made of each level of "type".
#' @param split.after For `type` other than \dQuote{default} e.g.
#'   \dQuote{season}, the trajectories can either be calculated for each level
#'   of `type` independently or extracted after the cluster calculations have
#'   been applied to the whole data set.
#' @param by.type The percentage of the total number of trajectories is given
#'   for all data by default. Setting `by.type = TRUE` will make each panel add
#'   up to 100.
#' @param ... Passed to [trajPlot()].
#'
#' @export
#'
#' @useDynLib openair, .registration = TRUE
#' @import cluster
#'
#' @return an [openair][openair-package] object. The `data` component contains
#'   both `traj` (the original data appended with its cluster) and `results`
#'   (the average trajectory path per cluster, shown in the `trajCluster()`
#'   plot.)
#'
#' @family trajectory analysis functions
#' @family cluster analysis functions
#'
#' @author David Carslaw
#' @author Jack Davison
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
  split.after = FALSE,
  by.type = FALSE,
  crs = 4326,
  cols = "Set1",
  plot = TRUE,
  ...
) {
  rlang::check_installed("sf")

  if (tolower(method) == "euclid") {
    method <- "distEuclid"
  } else {
    method <- "distAngle"
  }

  # remove any missing lat/lon
  traj <- dplyr::filter(traj, !is.na(.data$lat), !is.na(.data$lon))

  # check to see if all back trajectories are the same length
  traj <- dplyr::mutate(traj, traj_len = length(.data$date), .by = "date")

  if (length(unique(traj$traj_len)) > 1) {
    ux <- unique(traj$traj_len)
    nmax <- ux[which.max(tabulate(match(traj$traj_len, ux)))]
    traj <- dplyr::filter(traj, .data$traj_len == nmax)
  }

  calcTraj <- function(traj) {
    # make sure ordered correctly
    traj <- traj[order(traj$date, traj$hour.inc), ]

    # length of back trajectories
    traj <- dplyr::mutate(traj, len = length(.data$date), .by = "date")

    # find length of back trajectories
    # 96-hour back trajectories with origin: length should be 97
    n <- max(abs(traj$hour.inc)) + 1

    traj <- dplyr::filter(traj, .data$len == n)
    len <- nrow(traj) / n

    ## lat/lon input matrices
    x <- matrix(traj$lon, nrow = n)
    y <- matrix(traj$lat, nrow = n)

    z <- matrix(0, nrow = n, ncol = len)
    res <- matrix(0, nrow = len, ncol = len)

    if (method == "distEuclid") {
      res <- distEuclid(x, y)
    }

    if (method == "distAngle") {
      res <- distAngle(x, y)
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

  # calculate the mean trajectories by cluster
  vars <- c("lat", "lon", "date", "cluster", "hour.inc", type)
  vars2 <- c("cluster", "hour.inc", type)

  newdata <- traj |>
    dplyr::select(dplyr::all_of(vars)) |>
    summarise(across(everything(), mean), .by = dplyr::all_of(vars2))

  # count observations in each cluster/type
  clusters <- dplyr::count(
    traj,
    dplyr::across(dplyr::all_of(c(type, "cluster")))
  )

  # make each panel add up to 100
  if (by.type) {
    clusters <- clusters |>
      dplyr::mutate(
        freq = 100 * .data$n / sum(.data$n),
        .by = dplyr::all_of(type)
      )
  } else {
    clusters <- clusters |>
      dplyr::mutate(freq = 100 * .data$n / sum(.data$n))
  }

  # round to 1 decimal place
  clusters$freq <- round(clusters$freq, digits = 1)

  # make sure date is in correct format
  class(newdata$date) <- class(traj$date)
  attr(newdata$date, "tzone") <- "GMT"

  # create plot
  thePlot <-
    newdata |>
    # need a dummy 'date2' variable so `trajPlot()` will plot
    dplyr::mutate(date2 = date + dplyr::row_number(), height = 1) |>
    trajPlot(
      group = "cluster",
      crs = crs,
      cols = cols,
      type = type,
      ...,
      plot = FALSE
    ) |>
    purrr::pluck("plot") +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = F)
    )

  # get the ends of the lines
  line_ends <-
    newdata |>
    dplyr::slice_head(n = 1, by = dplyr::all_of(c("cluster", type))) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = crs)

  # if they're all the same, its a forward trajectory, get other end
  if (nrow(dplyr::distinct(line_ends, .data$geometry)) == 1) {
    line_ends <-
      newdata |>
      dplyr::slice_tail(n = 1, by = dplyr::all_of(c("cluster", type))) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      sf::st_transform(crs = crs)
  }

  # get labels per cluster/type
  line_ends <-
    line_ends |>
    dplyr::left_join(
      clusters,
      by = dplyr::all_of(c(type, "cluster"))
    ) |>
    dplyr::mutate(
      label = scales::label_percent(accuracy = 0.1, scale = 1)(.data$freq)
    )

  # add to plot
  thePlot <-
    thePlot +
    ggplot2::geom_sf_text(
      data = line_ends,
      ggplot2::aes(label = .data$label),
      nudge_y = 1
    )

  # plot if requested
  if (plot) {
    plot(thePlot)
  }

  # create output with plot
  output <-
    list(
      data = list(
        traj = traj,
        results = dplyr::left_join(newdata, clusters, by = c("cluster", type)),
        subsets = c("traj", "results")
      ),
      call = match.call()
    )
  class(output) <- "openair"
  invisible(output)
}
