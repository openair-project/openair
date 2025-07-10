#' Trajectory line plots with conditioning
#'
#' This function plots back trajectories. This function requires that data are
#' imported using the `importTraj` function.
#'
#' Several types of trajectory plot are available. `trajPlot` by default will
#' plot each lat/lon location showing the origin of each trajectory, if no
#' `pollutant` is supplied.
#'
#' If a pollutant is given, by merging the trajectory data with concentration
#' data (see example below), the trajectories are colour-coded by the
#' concentration of `pollutant`. With a long time series there can be lots of
#' overplotting making it difficult to gauge the overall concentration pattern.
#' In these cases setting `alpha` to a low value e.g. 0.1 can help.
#'
#' The user can also show points instead of lines by `plot.type = "p"`.
#'
#' Note that `trajPlot` will plot only the full length trajectories. This should
#' be remembered when selecting only part of a year to plot.
#'
#'
#' @param mydata Data frame, the result of importing a trajectory file using
#'   `importTraj`.
#' @param lon Column containing the longitude, as a decimal.
#' @param lat Column containing the latitude, as a decimal.
#' @param pollutant Pollutant to be plotted. By default the trajectory height is
#'   used.
#' @param type `type` determines how the data are split, i.e., conditioned, and
#'   then plotted. The default is will produce a single plot using the entire
#'   data. Type can be one of the built-in types as detailed in `cutData` e.g.
#'   "season", "year", "weekday" and so on. For example, `type = "season"` will
#'   produce four plots --- one for each season.
#'
#'   It is also possible to choose `type` as another variable in the data frame.
#'   If that variable is numeric, then the data will be split into four
#'   quantiles (if possible) and labelled accordingly. If type is an existing
#'   character or factor variable, then those categories/levels will be used
#'   directly. This offers great flexibility for understanding the variation of
#'   different variables and how they depend on one another.
#'
#'   `type` can be up length two e.g. `type = c("season", "weekday")` will
#'   produce a 2x2 plot split by season and day of the week. Note, when two
#'   types are provided the first forms the columns and the second the rows.
#' @param map Should a base map be drawn? If `TRUE` the world base map from the
#'   `maps` package is used.
#' @param group It is sometimes useful to group and colour trajectories
#'   according to a grouping variable. See example below.
#' @param map.fill Should the base map be a filled polygon? Default is to fill
#'   countries.
#' @param map.res The resolution of the base map. By default the function uses
#'   the \sQuote{world} map from the `maps` package. If `map.res = "hires"` then
#'   the (much) more detailed base map \sQuote{worldHires} from the `mapdata`
#'   package is used. Use `library(mapdata)`. Also available is a map showing
#'   the US states. In this case `map.res = "state"` should be used.
#' @param map.cols If `map.fill = TRUE` `map.cols` controls the fill colour.
#'   Examples include `map.fill = "grey40"` and `map.fill =
#'   openColours("default", 10)`. The latter colours the countries and can help
#'   differentiate them.
#' @param map.border The colour to use for the map outlines/borders. Defaults to
#'   `"black"`.
#' @param map.alpha The transparency level of the filled map which takes values
#'   from 0 (full transparency) to 1 (full opacity). Setting it below 1 can help
#'   view trajectories, trajectory surfaces etc. *and* a filled base map.
#' @param map.lwd The map line width, a positive number, defaulting to `1`.
#' @param map.lty The map line type. Line types can either be specified as an
#'   integer (`0` = blank, `1` = solid (default), `2` = dashed, `3` = dotted,
#'   `4` = dotdash, `5` = longdash, `6` = twodash) or as one of the character
#'   strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or
#'   "twodash", where "blank" uses 'invisible lines' (i.e., does not draw them).
#' @param projection The map projection to be used. Different map projections
#'   are possible through the `mapproj` package. See `?mapproject` for extensive
#'   details and information on setting other parameters and orientation (see
#'   below).
#' @param parameters From the `mapproj` package. Optional numeric vector of
#'   parameters for use with the projection argument. This argument is optional
#'   only in the sense that certain projections do not require additional
#'   parameters. If a projection does not require additional parameters then set
#'   to null i.e. `parameters = NULL`.
#' @param orientation From the `mapproj` package. An optional vector c(latitude,
#'   longitude, rotation) which describes where the "North Pole" should be when
#'   computing the projection. Normally this is c(90, 0), which is appropriate
#'   for cylindrical and conic projections. For a planar projection, you should
#'   set it to the desired point of tangency. The third value is a clockwise
#'   rotation (in degrees), which defaults to the midrange of the longitude
#'   coordinates in the map.
#' @param grid.col The colour of the map grid to be used. To remove the grid set
#'   `grid.col = "transparent"`.
#' @param npoints A dot is placed every `npoints` along each full trajectory.
#'   For hourly back trajectories points are plotted every `npoint` hours. This
#'   helps to understand where the air masses were at particular times and get a
#'   feel for the speed of the air (points closer together correspond to slower
#'   moving air masses). If `npoints = NA` then no points are added.
#' @param origin If true a filled circle dot is shown to mark the receptor
#'   point.
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#' @param ... other arguments are passed to `cutData` and `scatterPlot`. This
#'   provides access to arguments used in both these functions and functions
#'   that they in turn pass arguments on to. For example, `plotTraj` passes the
#'   argument `cex` on to `scatterPlot` which in turn passes it on to the
#'   `lattice` function `xyplot` where it is applied to set the plot symbol
#'   size.
#' @export
#' @family trajectory analysis functions
#' @author David Carslaw
#' @examples
#'
#' # show a simple case with no pollutant i.e. just the trajectories
#' # let's check to see where the trajectories were coming from when
#' # Heathrow Airport was closed due to the Icelandic volcanic eruption
#' # 15--21 April 2010.
#' # import trajectories for London and plot
#' \dontrun{
#' lond <- importTraj("london", 2010)
#' # well, HYSPLIT seems to think there certainly were conditions where trajectories
#' # orginated from Iceland...
#' trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"))
#' }
#'
#' # plot by day, need a column that makes a date
#' \dontrun{
#' lond$day <- as.Date(lond$date)
#' trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
#'   type = "day"
#' )
#' }
#'
#' # or show each day grouped by colour, with some other options set
#' \dontrun{
#' trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
#'   group = "day", col = "turbo", lwd = 2, key.pos = "right", key.col = 1
#' )
#' }
#' # more examples to follow linking with concentration measurements...
#'
trajPlot <- function(
  mydata,
  lon = "lon",
  lat = "lat",
  pollutant = "height",
  type = "default",
  map = TRUE,
  group = NA,
  map.fill = TRUE,
  map.res = "default",
  map.cols = "grey40",
  map.border = "black",
  map.alpha = 0.4,
  map.lwd = 1,
  map.lty = 1,
  projection = "lambert",
  parameters = c(51, 51),
  orientation = c(90, 0, 0),
  grid.col = "deepskyblue",
  npoints = 12,
  origin = TRUE,
  plot = TRUE,
  ...
) {
  len <- NULL
  hour.inc <- NULL ## silence R check

  ## variables needed in trajectory plots
  vars <- c("date", "lat", "lon", "hour.inc", pollutant)

  ## if group is present, need to add that list of variables unless it is a
  ## pre-defined date-based one
  if (!is.na(group)) {
    if (group %in% dateTypes | any(type %in% dateTypes)) {
      if (group %in% dateTypes) {
        vars <- unique(c(vars, "date")) ## don't need group because it is
        ## defined by date
      } else {
        vars <- unique(c(vars, "date", group))
      }
    } else {
      vars <- unique(c(vars, group))
    }
  }

  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

  mydata <- filter(mydata, !is.na(lat), !is.na(lon))

  ## slect only full length trajectories
  mydata <- mydata[order(mydata$date, mydata$hour.inc), ]

  ## length of back mydataectories
  mydata$len <- ave(mydata$lat, mydata$date, FUN = length)

  ## find length of back trajectories, choose most frequent
  ## so that partial trajectories are not plotted
  n <- as.numeric(names(which.max(table(abs(mydata$len)))))

  mydata <- subset(mydata, len == n)

  ## Args
  Args <- list(...)
  method <- "scatter"

  ## location of receptor for map projection, used to show location on maps
  origin_xy <- head(subset(mydata, hour.inc == 0), 1) ## origin
  tmp <- mapproject(
    x = origin_xy[["lon"]][1],
    y = origin_xy[["lat"]][1],
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )
  receptor <- c(tmp$x, tmp$y)

  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")

  ## reset graphic parameters
  on.exit(trellis.par.set(
    fontsize = current.font
  ))

  # aspect, cex
  if (!"plot.type" %in% names(Args)) {
    Args$plot.type <- "l"
  }

  if (!"cex" %in% names(Args)) {
    Args$cex <- 0.1
  }

  if (!"ylab" %in% names(Args)) {
    Args$ylab <- ""
  }

  if (!"xlab" %in% names(Args)) {
    Args$xlab <- ""
  }

  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }

  ## xlim and ylim set by user
  if (!"xlim" %in% names(Args)) {
    Args$xlim <- range(mydata$lon)
  }

  if (!"ylim" %in% names(Args)) {
    Args$ylim <- range(mydata$lat)
  }

  ## extent of data (or limits set by user) in degrees
  trajLims <- c(Args$xlim, Args$ylim)

  ## need *outline* of boundary for map limits
  Args <- setTrajLims(mydata, Args, projection, parameters, orientation)

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

  if (missing(pollutant)) {
    ## don't need key

    if (is.na(group)) {
      key <- FALSE
    } else {
      key <- TRUE
    }

    if (!"main" %in% names(Args)) {
      Args$main <- NULL
    }

    scatterPlot.args <- list(
      mydata,
      x = lon,
      y = lat,
      z = NA,
      type = type,
      method = method,
      map = map,
      key = key,
      group = group,
      map.fill = map.fill,
      map.res = map.res,
      map.cols = map.cols,
      map.border = map.border,
      map.alpha = map.alpha,
      map.lty = map.lty,
      map.lwd = map.lwd,
      traj = TRUE,
      projection = projection,
      parameters = parameters,
      orientation = orientation,
      grid.col = grid.col,
      trajLims = trajLims,
      receptor = receptor,
      npoints = npoints,
      origin = origin
    )
  } else {
    if (!"main" %in% names(Args)) {
      Args$main <- pollutant
    }

    scatterPlot.args <- list(
      mydata,
      x = lon,
      y = lat,
      z = pollutant,
      type = type,
      method = method,
      map = map,
      group = group,
      map.fill = map.fill,
      map.res = map.res,
      map.cols = map.cols,
      map.alpha = map.alpha,
      traj = TRUE,
      projection = projection,
      parameters = parameters,
      orientation = orientation,
      grid.col = grid.col,
      trajLims = trajLims,
      receptor = receptor,
      npoints = npoints,
      origin = origin
    )
  }

  # reset for Args
  scatterPlot.args <- listUpdate(scatterPlot.args, Args)
  scatterPlot.args <- listUpdate(scatterPlot.args, list(plot = plot))

  # plot
  plt <- do.call(scatterPlot, scatterPlot.args)

  output <-
    list(
      plot = plt$plot,
      data = dplyr::tibble(mydata),
      call = match.call()
    )
  class(output) <- "openair"

  invisible(output)
}


setTrajLims <- function(mydata, Args, projection, parameters, orientation) {
  ## xlim and ylim set by user
  if ("xlim" %in% names(Args) & !all(is.na(Args$xlim))) {
    x1 <- Args$xlim[1]
    x2 <- Args$xlim[2]
  } else {
    x1 <- min(mydata$lon, na.rm = TRUE)
    x2 <- max(mydata$lon, na.rm = TRUE)
  }

  if ("ylim" %in% names(Args) & !all(is.na(Args$ylim))) {
    y1 <- Args$ylim[1]
    y2 <- Args$ylim[2]
  } else {
    y1 <- min(mydata$lat, na.rm = TRUE)
    y2 <- max(mydata$lat, na.rm = TRUE)
  }

  n <- 40 ## number of points along each vertex

  X <- c(
    seq(x1, x1, length.out = n),
    seq(x1, x2, length.out = n),
    seq(x2, x2, length.out = n),
    seq(x2, x1, length.out = n)
  )

  Y <- c(
    seq(y1, y2, length.out = n),
    seq(y2, y2, length.out = n),
    seq(y2, y1, length.out = n),
    seq(y1, y1, length.out = n)
  )

  tmp <- mapproject(
    x = X,
    y = Y,
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )

  Args$xlim <- tmp$range[1:2]
  Args$ylim <- tmp$range[3:4]
  Args
}

## function from mapproj to add grid lines to a map
map.grid2 <- function(
  lim,
  nx = 9,
  ny = 9,
  labels = TRUE,
  pretty = TRUE,
  cex = 1,
  col = "deepskyblue",
  lty = 2,
  font = 1,
  projection = "rectangular",
  parameters = 52,
  orientation = c(90, 0, 0),
  ...
) {
  pretty.range <- function(lim, ...) {
    x <- pretty(lim, ...)
    if (abs(x[1] - lim[1]) > abs(x[2] - lim[1])) {
      x <- x[-1]
    }
    n <- length(x)
    if (abs(x[n] - lim[2]) > abs(x[n - 1] - lim[2])) {
      x <- x[-n]
    }
    x[1] <- lim[1]
    x[length(x)] <- lim[2]
    x
  }
  auto.format <- function(x) {
    for (digits in 0:6) {
      s <- formatC(x, digits = digits, format = "f")
      if (all(duplicated(s) == duplicated(x))) {
        break
      }
    }
    s
  }
  if (missing(lim)) {
    lim <- maps::.map.range()
  }
  if (is.list(lim)) {
    lim <- lim$range
  }
  if (lim[2] - lim[1] > 360) {
    lim[2] <- lim[1] + 360
  }
  if (pretty) {
    x <- pretty.range(lim[1:2], n = nx)
    y <- pretty.range(lim[3:4], n = ny)
  } else {
    x <- seq(lim[1], lim[2], len = nx)
    y <- seq(lim[3], lim[4], len = ny)
  }
  p <- mapproject(
    expand.grid(
      x = c(
        seq(lim[1], lim[2], len = 100),
        NA
      ),
      y = y
    ),
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )
  p <- maps::map.wrap(p)
  llines(p, col = col, lty = lty, ...)
  llines(
    mapproject(
      expand.grid(
        y = c(
          seq(lim[3], lim[4], len = 100),
          NA
        ),
        x = x
      ),
      projection = projection,
      parameters = parameters,
      orientation = orientation
    ),
    col = col,
    lty = lty,
    ...
  )
  if (labels) {
    tx <- x[2]
    xinc <- median(diff(x))
    ty <- y[length(y) - 2]
    yinc <- median(diff(y))
    ltext(
      mapproject(
        expand.grid(
          x = x + xinc * 0.05,
          y = ty +
            yinc * 0.5
        ),
        projection = projection,
        parameters = parameters,
        orientation = orientation
      ),
      labels = auto.format(x),
      cex = cex,
      adj = c(0, 0),
      col = col,
      font = font,
      ...
    )
    ltext(
      mapproject(
        expand.grid(
          x = tx + xinc * 0.5,
          y = y +
            yinc * 0.05
        ),
        projection = projection,
        parameters = parameters,
        orientation = orientation
      ),
      labels = auto.format(y),
      cex = cex,
      adj = c(0, 0),
      col = col,
      font = font,
      ...
    )
  }
}
