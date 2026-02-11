#' Trajectory level plots with conditioning
#'
#' This function plots gridded back trajectories. This function requires that
#' data are imported using the [importTraj()] function.
#'
#' An alternative way of showing the trajectories compared with plotting
#' trajectory lines is to bin the points into latitude/longitude intervals. For
#' these purposes [trajLevel()] should be used. There are several trajectory
#' statistics that can be plotted as gridded surfaces. First, `statistic` can be
#' set to "frequency" to show the number of back trajectory points in a grid
#' square. Grid squares are by default at 1 degree intervals, controlled by
#' `lat.inc` and `lon.inc`. Such plots are useful for showing the frequency of
#' air mass locations. Note that it is also possible to set `method = "hexbin"`
#' for plotting frequencies (not concentrations), which will produce a plot by
#' hexagonal binning.
#'
#' If `statistic = "difference"` the trajectories associated with a
#' concentration greater than `percentile` are compared with the the full set of
#' trajectories to understand the differences in frequencies of the origin of
#' air masses of the highest concentration trajectories compared with the
#' trajectories on average. The comparison is made by comparing the percentage
#' change in gridded frequencies. For example, such a plot could show that the
#' top 10\% of concentrations of PM10 tend to originate from air-mass origins to
#' the east.
#'
#' If `statistic = "pscf"` then the Potential Source Contribution Function is
#' plotted. The PSCF calculates the probability that a source is located at
#' latitude \eqn{i} and longitude \eqn{j} (Pekney et al., 2006).The basis of
#' PSCF is that if a source is located at (i,j), an air parcel back trajectory
#' passing through that location indicates that material from the source can be
#' collected and transported along the trajectory to the receptor site. PSCF
#' solves \deqn{PSCF = m_{ij}/n_{ij}} where \eqn{n_{ij}} is the number of times
#' that the trajectories passed through the cell (i,j) and \eqn{m_{ij}} is the
#' number of times that a source concentration was high when the trajectories
#' passed through the cell (i,j). The criterion for determining \eqn{m_{ij}} is
#' controlled by `percentile`, which by default is 90. Note also that cells with
#' few data have a weighting factor applied to reduce their effect.
#'
#' A limitation of the PSCF method is that grid cells can have the same PSCF
#' value when sample concentrations are either only slightly higher or much
#' higher than the criterion. As a result, it can be difficult to distinguish
#' moderate sources from strong ones. Seibert et al. (1994) computed
#' concentration fields to identify source areas of pollutants. The
#' Concentration Weighted Trajectory (CWT) approach considers the concentration
#' of a species together with its residence time in a grid cell. The CWT
#' approach has been shown to yield similar results to the PSCF approach. The
#' openair manual has more details and examples of these approaches.
#'
#' A further useful refinement is to smooth the resulting surface, which is
#' possible by setting `smooth = TRUE`.
#'
#' @note This function is under active development and is likely to change
#'
#' @inheritParams trajPlot
#' @param smooth Should the trajectory surface be smoothed?
#' @param statistic Statistic to use for [trajLevel()]. By default, the function
#'   will plot the trajectory frequencies (`statistic = "frequency"`). As an
#'   alternative way of viewing trajectory frequencies, the argument `method =
#'   "hexbin"` can be used. In this case hexagonal binning of the trajectory
#'   *points* (i.e., a point every three hours along each back trajectory).
#'   The plot then shows the trajectory frequencies uses hexagonal binning.
#'
#'   There are also various ways of plotting concentrations.
#'
#'   It is possible to set `statistic = "difference"`. In this case trajectories
#'   where the associated concentration is greater than `percentile` are
#'   compared with the the full set of trajectories to understand the
#'   differences in frequencies of the origin of air masses. The comparison is
#'   made by comparing the percentage change in gridded frequencies. For
#'   example, such a plot could show that the top 10\% of concentrations of PM10
#'   tend to originate from air-mass origins to the east.
#'
#'   If `statistic = "pscf"` then a Potential Source Contribution Function map
#'   is produced. This statistic method interacts with `percentile`.
#'
#'   If `statistic = "cwt"` then concentration weighted trajectories are
#'   plotted.
#'
#'   If `statistic = "sqtba"` then Simplified Quantitative Transport Bias
#'   Analysis is undertaken. This statistic method interacts with `.combine` and
#'   `sigma`.
#' @param percentile The percentile concentration of `pollutant` against which
#'   the all trajectories are compared.
#' @param lon.inc,lat.inc The longitude and latitude intervals to be used for
#'   binning data.
#' @param min.bin The minimum number of unique points in a grid cell. Counts
#'   below `min.bin` are set as missing.
#' @param .combine When statistic is "SQTBA" it is possible to combine lots of
#'   receptor locations to derive a single map. `.combine` identifies the column
#'   that differentiates different sites (commonly a column named `"site"`).
#'   Note that individual site maps are normalised first by dividing by their
#'   mean value.
#' @param sigma For the SQTBA approach `sigma` determines the amount of back
#'   trajectory spread based on the Gaussian plume equation. Values in the
#'   literature suggest 5.4 km after one hour. However, testing suggests lower
#'   values reveal source regions more effectively while not introducing too
#'   much noise.
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#' @param ... other arguments are passed to [cutData()] and [scatterPlot()].
#'   This provides access to arguments used in both these functions and
#'   functions that they in turn pass arguments on to. For example,
#'   [trajLevel()] passes the argument `cex` on to [scatterPlot()] which in
#'   turn passes it on to [lattice::xyplot()] where it is applied to set the
#'   plot symbol size.
#' @export
#' @return an [openair][openair-package] object
#' @family trajectory analysis functions
#' @author David Carslaw
#' @references
#'
#' Pekney, N. J., Davidson, C. I., Zhou, L., & Hopke, P. K. (2006). Application
#' of PSCF and CPF to PMF-Modeled Sources of PM 2.5 in Pittsburgh. Aerosol
#' Science and Technology, 40(10), 952-961.
#'
#' Seibert, P., Kromp-Kolb, H., Baltensperger, U., Jost, D., 1994. Trajectory
#' analysis of high-alpine air pollution data. NATO Challenges of Modern Society
#' 18, 595-595.
#'
#' Xie, Y., & Berkowitz, C. M. (2007). The use of conditional probability
#' functions and potential source contribution functions to identify source
#' regions and advection pathways of hydrocarbon emissions in Houston, Texas.
#' Atmospheric Environment, 41(28), 5831-5847.
#' @examples
#'
#' # show a simple case with no pollutant i.e. just the trajectories
#' # let's check to see where the trajectories were coming from when
#' # Heathrow Airport was closed due to the Icelandic volcanic eruption
#' # 15--21 April 2010.
#' # import trajectories for London and plot
#' \dontrun{
#' lond <- importTraj("london", 2010)
#' }
#' # more examples to follow linking with concentration measurements...
#'
#' # import some measurements from KC1 - London
#' \dontrun{
#' kc1 <- importAURN("kc1", year = 2010)
#' # now merge with trajectory data by 'date'
#' lond <- merge(lond, kc1, by = "date")
#'
#' # trajectory plot, no smoothing - and limit lat/lon area of interest
#' # use PSCF
#' trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
#'   pollutant = "pm10", statistic = "pscf"
#' )
#'
#' # can smooth surface, suing CWT approach:
#' trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
#'   pollutant = "pm2.5", statistic = "cwt", smooth = TRUE
#' )
#'
#' # plot by season:
#' trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
#'   pollutant = "pm2.5",
#'   statistic = "pscf", type = "season"
#' )
#' }
trajLevel <- function(
  mydata,
  lon = "lon",
  lat = "lat",
  pollutant = "height",
  type = "default",
  smooth = FALSE,
  statistic = "frequency",
  percentile = 90,
  map = TRUE,
  lon.inc = 1.0,
  lat.inc = 1.0,
  min.bin = 1,
  .combine = NA,
  sigma = 1.5,
  map.fill = TRUE,
  map.res = "default",
  map.cols = "grey40",
  map.border = "black",
  map.alpha = 0.3,
  map.lwd = 1,
  map.lty = 1,
  projection = "lambert",
  parameters = c(51, 51),
  orientation = c(90, 0, 0),
  grid.col = "deepskyblue",
  grid.nx = 9,
  grid.ny = grid.nx,
  origin = TRUE,
  plot = TRUE,
  ...
) {
  ## mydata can be a list of several trajectory files; in which case combine them
  ## before averaging
  hour.inc <- N <- NULL

  ## variables needed in trajectory plots
  vars <- c("date", "lat", "lon", "hour.inc", pollutant)

  statistic <- tolower(statistic)

  # to combine the effects of several receptors
  if (!is.na(.combine)) {
    vars <- c(vars, .combine)
  }

  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  if (statistic == "sqtba" && type != "default") {
    warning("'type' option not available yet with SQTBA", call. = FALSE)
    type <- "default"
  }

  ## Args
  Args <- list(...)

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  if (!"ylab" %in% names(Args)) {
    Args$ylab <- ""
  }

  if (!"xlab" %in% names(Args)) {
    Args$xlab <- ""
  }

  if (!"main" %in% names(Args)) {
    Args$main <- ""
  }

  if (!"border" %in% names(Args)) {
    Args$border <- NA
  }

  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  if (!"key.header" %in% names(Args)) {
    if (statistic == "frequency") {
      Args$key.header <- "% trajectories"
    }
    if (statistic == "pscf") {
      Args$key.header <- "PSCF \nprobability"
    }
    if (statistic == "sqtba") {
      Args$key.header <- paste0("SQTBA \n", pollutant)
    }
    if (statistic == "sqtba" && !is.na(.combine)) {
      Args$key.header <- paste0("SQTBA \n(Normalised)\n)", pollutant)
    }
    if (statistic == "difference") {
      Args$key.header <- quickText(paste(
        "gridded differences",
        "\n(",
        percentile,
        "th percentile)",
        sep = ""
      ))
    }
  }

  if (!"key.footer" %in% names(Args)) {
    Args$key.footer <- ""
  }

  ## xlim and ylim set by user
  if (!"xlim" %in% names(Args)) {
    Args$xlim <- range(mydata$lon)

    # tweak for SQTBA
    Args$xlim <- c(
      round(quantile(mydata$lon, probs = 0.002)),
      round(quantile(mydata$lon, probs = 0.998))
    )
  }

  if (!"ylim" %in% names(Args)) {
    Args$ylim <- range(mydata$lat)

    # tweak for SQTBA
    Args$ylim <- c(
      round(quantile(mydata$lat, probs = 0.002)),
      round(quantile(mydata$lat, probs = 0.998))
    )
  }

  ## extent of data (or limits set by user) in degrees
  trajLims <- c(Args$xlim, Args$ylim)

  ## need *outline* of boundary for map limits
  Args <- setTrajLims(mydata, Args, projection, parameters, orientation)

  Args$trajStat <- statistic

  if (!"method" %in% names(Args)) {
    method <- "traj"
  } else {
    method <- Args$method
    statistic <- "XX" ## i.e. it wont touch the data
  }

  ## location of receptor for map projection, used to show location on maps
  origin_xy <- mydata |>
    filter(hour.inc == 0) |>
    group_by(lat, lon) |>
    slice_head(n = 1)

  tmp <- mapproject(
    x = origin_xy[["lon"]],
    y = origin_xy[["lat"]],
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )

  receptor <- tibble(x = tmp$x, y = tmp$y)

  if (method == "hexbin") {
    ## transform data for map projection
    tmp <- mapproject(
      x = mydata[["lon"]],
      y = mydata[["lat"]],
      projection = projection,
      parameters = parameters,
      orientation = orientation
    )
    mydata[["lon"]] <- tmp$x
    mydata[["lat"]] <- tmp$y
  }

  if (method == "density") {
    stop("Use trajPlot with method = 'density' instead")
  }

  if (is.list(mydata)) {
    mydata <- bind_rows(mydata)
  }

  mydata <- cutData(mydata, type, ...)

  ## bin data
  if (method == "traj") {
    mydata$ygrid <- round_any(mydata[[lat]], lat.inc)
    mydata$xgrid <- round_any(mydata[[lon]], lon.inc)
  } else {
    mydata$ygrid <- mydata[[lat]]
    mydata$xgrid <- mydata[[lon]]
  }

  rhs <- c("xgrid", "ygrid", type)
  rhs <- paste(rhs, collapse = "+")

  if (statistic == "sqtba") {
    mydata <- mydata |>
      select(any_of(na.omit(c(
        "date",
        "lon",
        "lat",
        "hour.inc",
        type,
        pollutant,
        .combine
      ))))
  } else {
    mydata <- mydata[, c("date", "xgrid", "ygrid", "hour.inc", type, pollutant)]
    ids <- which(names(mydata) %in% c("xgrid", "ygrid", type))
  }

  # grouping variables
  vars <- c("xgrid", "ygrid", type)

  ## plot mean concentration - CWT method
  if (statistic %in% c("cwt", "median")) {
    ## calculate the mean of points in each cell
    mydata <- mydata |>
      group_by(across(vars)) |>
      summarise(
        N = length(date),
        date = head(date, 1),
        count = mean(.data[[pollutant]], na.rm = TRUE)
      )

    mydata[[pollutant]] <- mydata$count

    ## adjust at edges

    id <- which(mydata$N > 20 & mydata$N <= 80)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.7

    id <- which(mydata$N > 10 & mydata$N <= 20)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.42

    id <- which(mydata$N <= 10)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.05
    attr(mydata$date, "tzone") <- "GMT" ## avoid warning messages about TZ

    # prep output data
    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(-dplyr::any_of(c("date", "count"))) |>
      dplyr::rename("count" = "N") |>
      dplyr::mutate(
        statistic = statistic,
        .before = dplyr::everything()
      )
  }

  ## plot trajectory frequecies
  if (statistic == "frequency" && method != "hexbin") {
    ## count % of times a cell contains a trajectory point
    ## need date for later use of type

    mydata <- mydata |>
      group_by(across(vars)) |>
      summarise(count = length(date), date = head(date, 1))

    mydata[[pollutant]] <- 100 * mydata$count / max(mydata$count)

    # prep output data
    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(-dplyr::any_of(c("date"))) |>
      dplyr::mutate(
        statistic = statistic,
        .before = dplyr::everything()
      )
  }

  ## Poential Source Contribution Function
  if (statistic == "pscf") {
    ## high percentile
    Q90 <- quantile(mydata[[pollutant]], probs = percentile / 100, na.rm = TRUE)

    ## calculate the proportion of points in cell with value > Q90
    mydata <- mydata |>
      group_by(across(vars)) |>
      summarise(
        N = length(date),
        date = head(date, 1),
        count = length(which(.data[[pollutant]] > Q90)) / N
      )

    mydata[[pollutant]] <- mydata$count

    ## ## adjust at edges
    n <- mean(mydata$N)
    id <- which(mydata$N > n & mydata$N <= 2 * n)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.75

    id <- which(mydata$N > (n / 2) & mydata$N <= n)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.5

    id <- which(mydata$N <= (n / 2))
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.15

    # prep output data
    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(-dplyr::any_of(c("date", "count"))) |>
      dplyr::rename("count" = "N") |>
      dplyr::mutate(
        statistic = statistic,
        percentile = percentile,
        .before = dplyr::everything()
      )
  }

  # simplified quantitative transport bias analysis  ------------------------

  if (tolower(statistic) == "sqtba") {
    # calculate sigma
    mydata <- mydata |>
      mutate(sigma = sigma * abs(hour.inc)) |>
      drop_na({{ pollutant }})

    # receptor grid
    # use trajectory data to determine grid size - don't go to extremes
    r_grid <- expand_grid(
      lat = seq(
        round(quantile(mydata$lat, probs = 0.002)),
        round(quantile(mydata$lat, probs = 0.998)),
        by = lat.inc
      ),
      lon = seq(
        round(quantile(mydata$lon, probs = 0.002)),
        round(quantile(mydata$lon, probs = 0.998)),
        by = lon.inc
      )
    ) |>
      as.matrix()
    browser()
    # just run
    if (is.na(.combine)) {
      mydata <- calc_SQTBA_dplyr(mydata, r_grid, pollutant, min.bin) |>
        rename({{ pollutant }} := SQTBA)
    } else {
      # process by site, normalise contributions by default
      mydata <- mydata |>
        group_by(across(.combine)) |>
        group_modify(~ calc_SQTBA(.x, r_grid, pollutant, min.bin)) |>
        mutate(SQTBA_norm = SQTBA / mean(SQTBA)) |>
        group_by(ygrid, xgrid) |>
        summarise(
          SQTBA = mean(SQTBA),
          SQTBA_norm = mean(SQTBA_norm)
        ) |>
        ungroup() |>
        mutate(SQTBA_norm = SQTBA_norm * mean(SQTBA)) |>
        rename({{ pollutant }} := SQTBA_norm)
    }

    # prep output data
    names(mydata)[names(mydata) == "n"] <- "count"

    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(
        -dplyr::any_of(c("lat_rnd", "lon_rnd", "Q", "Q_c", "SQTBA"))
      ) |>
      dplyr::relocate(dplyr::any_of("count"), .before = pollutant) |>
      dplyr::relocate("xgrid", .before = "ygrid") |>
      dplyr::mutate(
        statistic = statistic,
        sigma = sigma,
        combine = .combine,
        .before = dplyr::everything()
      )
  }

  ## plot trajectory frequency differences e.g. top 10% concs cf. mean
  if (statistic == "difference") {
    ## calculate percentage of points for all data

    base <- mydata |>
      group_by(across(vars)) |>
      summarise(count = length(date), date = head(date, 1))

    base[[pollutant]] <- 100 * base$count / max(base$count)

    ## high percentile
    Q90 <- quantile(mydata[[pollutant]], probs = percentile / 100, na.rm = TRUE)

    ## calculate percentage of points for high data
    high <- mydata |>
      group_by(across(vars)) |>
      summarise(
        N = length(date),
        date = head(date, 1),
        count = length(which(.data[[pollutant]] > Q90))
      )

    high[[pollutant]] <- 100 * high$count / max(high$count)

    ## calculate percentage absolute difference
    mydata <- base
    mydata[[pollutant]] <- high[[pollutant]] - mydata[[pollutant]]

    ## select only if > min.bin points in grid cell
    mydata <- subset(mydata, count >= min.bin)

    # prep output data
    out_data <- dplyr::ungroup(mydata) |>
      dplyr::select(-dplyr::any_of(c("date"))) |>
      dplyr::mutate(
        statistic = statistic,
        percentile = percentile,
        .before = dplyr::everything()
      )
  }

  ## change x/y names to gridded values
  lon <- "xgrid"
  lat <- "ygrid"

  ## the plot, note k is the smoothing parameter when surface smooth fitted
  scatterPlot.args <- list(
    mydata,
    x = lon,
    y = lat,
    z = pollutant,
    type = type,
    method = method,
    smooth = smooth,
    map = map,
    x.inc = lon.inc,
    y.inc = lat.inc,
    map.fill = map.fill,
    map.res = map.res,
    map.cols = map.cols,
    map.border = map.border,
    map.alpha = map.alpha,
    map.lwd = map.lwd,
    map.lty = map.lty,
    traj = TRUE,
    projection = projection,
    parameters = parameters,
    orientation = orientation,
    grid.col = grid.col,
    grid.nx = grid.nx,
    grid.ny = grid.ny,
    trajLims = trajLims,
    receptor = receptor,
    origin = origin,
    dist = 0.05,
    k = 50
  )

  ## reset for Args
  scatterPlot.args <- listUpdate(scatterPlot.args, Args)
  scatterPlot.args <- listUpdate(scatterPlot.args, list(plot = plot))

  ## plot
  plt <- do.call(scatterPlot, scatterPlot.args)

  output <-
    list(
      plot = plt$plot,
      data = if (exists("out_data")) {
        out_data
      } else {
        mydata
      },
      call = match.call()
    )
  class(output) <- "openair"

  invisible(output)
}

# SQTBA functions
# use matrices for speed; Haversine distances away from Gaussian plume
# centreline
pred_Q <- function(i, traj_data, r_grid, pollutant) {
  x_origin <- traj_data$lon[i]
  y_origin <- traj_data$lat[i]
  Q <- numeric(nrow(r_grid))
  Q_c <- Q

  # only calculate near receptor (within 4 degrees)
  id <- which(
    r_grid[, "lat"] > y_origin - 4 &
      r_grid[, "lat"] < y_origin + 4 &
      r_grid[, "lon"] > x_origin - 4 &
      r_grid[, "lon"] < x_origin + 4
  )

  # subtract 1E-7 from acos - numerical imprecision results in value being greater than 1
  dist <- acos(
    sin(y_origin * pi / 180) *
      sin(r_grid[id, "lat"] * pi / 180) +
      cos(y_origin * pi / 180) *
        cos(r_grid[id, "lat"] * pi / 180) *
        cos(r_grid[id, "lon"] * pi / 180 - x_origin * pi / 180) -
      1e-7
  ) *
    6378.137

  Q[id] <- (1 / traj_data$sigma[i]^2) *
    exp(-0.5 * (dist / traj_data$sigma[i])^2)
  Q_c[id] <- Q[id] * traj_data[[pollutant]][i]

  cbind(Q, Q_c)
}

make_grid <- function(traj_data, r_grid, pollutant) {
  # go through points on back trajectory
  # makes probability surface
  out_grid <- purrr::map(
    2:max(abs(traj_data$hour.inc)),
    pred_Q,
    traj_data,
    r_grid,
    pollutant
  )
  out_grid <- reduce(out_grid, `+`) / length(out_grid)

  return(out_grid)
}

calc_SQTBA <- function(mydata, r_grid, pollutant, min.bin) {
  q_calc <- mydata |>
    select(date, lat, lon, hour.inc, {{ pollutant }}, sigma) |>
    group_by(date) |>
    nest() |>
    mutate(out = purrr::map(data, make_grid, r_grid, pollutant))

  # combine all trajectory grids, add coordinates back in
  output <- reduce(q_calc$out, `+`) |>
    cbind(r_grid) |>
    as_tibble(.) |>
    mutate(
      SQTBA = Q_c / Q,
      lat_rnd = round(lat),
      lon_rnd = round(lon)
    )

  # number of trajectories in a grid square
  grid_out <- mydata |>
    mutate(lat_rnd = round(lat), lon_rnd = round(lon)) |>
    group_by(lat_rnd, lon_rnd) |>
    tally() |>
    filter(n >= min.bin)

  # add in number of trajectories in each grid square to act as a filter
  mydata <- inner_join(output, grid_out, by = c("lat_rnd", "lon_rnd"))
  mydata <- rename(mydata, xgrid = lon, ygrid = lat)
  return(mydata)
}


library(dplyr)
library(tidyr)
library(purrr)

calc_SQTBA_dplyr <- function(mydata, r_grid, pollutant, min.bin) {
  # --- 1. PREPARE GRID ---
  # Convert matrix to tibble, add IDs, and pre-calc radians
  grid_df <- as_tibble(r_grid) %>%
    mutate(
      grid_id = row_number(),
      g_lat_rad = lat * pi / 180,
      g_lon_rad = lon * pi / 180,
      # Create integer "buckets" for joining
      lat_rnd = round(lat),
      lon_rnd = round(lon)
    )

  # --- 2. PREPARE TRAJECTORIES ---
  # Filter valid hours, calc weights, and pre-calc radians
  traj_df <- mydata %>%
    # Original logic: points 2 to max(hour.inc)
    filter(abs(hour.inc) > 1) %>%
    group_by(date) %>%
    mutate(
      weight = 1 / n(),
      t_lat_rad = lat * pi / 180,
      t_lon_rad = lon * pi / 180,
      # Create integer "buckets"
      lat_base = round(lat),
      lon_base = round(lon)
    ) %>%
    ungroup()

  # --- 3. CREATE NEIGHBORHOOD LOOKUP (The Magic Step) ---
  # Instead of checking the whole grid, we look at the 9x9 integer square around a point.
  # We create offsets (-4 to +4) to bridge the gap between buckets.

  offsets <- tidyr::crossing(
    lat_off = -4:4,
    lon_off = -4:4
  )

  # Expand trajectories: Duplicate rows for every potential neighbor bucket
  # (This temporarily increases row count but makes the join fast)
  traj_expanded <- traj_df %>%
    cross_join(offsets) %>%
    mutate(
      search_lat = lat_base + lat_off,
      search_lon = lon_base + lon_off
    )

  # --- 4. JOIN & FILTER ---
  # Join trajectory buckets to grid buckets
  pairs <- traj_expanded %>%
    inner_join(
      grid_df,
      by = c("search_lat" = "lat_rnd", "search_lon" = "lon_rnd")
    ) %>%
    # Now strictly filter the exact window (original logic: +/- 4 degrees)
    filter(
      lat.y > lat.x - 4,
      lat.y < lat.x + 4,
      lon.y > lon.x - 4,
      lon.y < lon.x + 4
    )

  # --- 5. VECTORIZED MATH ---
  # Calculate distance and Q for all pairs at once
  results <- pairs %>%
    mutate(
      # Vectorized Haversine/ACos distance
      dist_km = (acos(
        sin(t_lat_rad) *
          sin(g_lat_rad) +
          cos(t_lat_rad) * cos(g_lat_rad) * cos(g_lon_rad - t_lon_rad) -
          1e-7
      ) *
        6378.137),

      # Gaussian Plume
      Q_val = (1 / sigma^2) * exp(-0.5 * (dist_km / sigma)^2),

      # Apply trajectory weight immediately
      Q_weighted = Q_val * weight,
      Qc_weighted = Q_val * .data[[pollutant]] * weight
    ) %>%
    # Aggregate by Grid Cell
    group_by(grid_id, lat.y, lon.y) %>% # lat.y/lon.y are grid coords
    summarise(
      Q = sum(Q_weighted, na.rm = TRUE),
      Q_c = sum(Qc_weighted, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SQTBA = Q_c / Q) %>%
    rename(lat = lat.y, lon = lon.y)

  # --- 6. MERGE BACK TO FULL GRID & FILTER ---
  # Ensure all original grid points exist (even if 0)
  final_output <- grid_df %>%
    select(lat, lon) %>%
    left_join(results, by = c("lat", "lon")) %>%
    mutate(
      SQTBA = coalesce(SQTBA, 0), # Replace NA with 0
      lat_rnd = round(lat),
      lon_rnd = round(lon)
    )

  # --- 7. MIN.BIN FILTER (Original Logic) ---
  grid_counts <- mydata %>%
    mutate(lat_rnd = round(lat), lon_rnd = round(lon)) %>%
    count(lat_rnd, lon_rnd) %>%
    filter(n >= min.bin)

  final_data <- final_output %>%
    inner_join(grid_counts, by = c("lat_rnd", "lon_rnd")) %>%
    rename(xgrid = lon, ygrid = lat)

  return(final_data)
}
