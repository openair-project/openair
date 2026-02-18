#' Trajectory line plots with conditioning
#'
#' This function plots back trajectories. This function requires that data are
#' imported using the [importTraj()] function, or matches that structure.
#'
#' Several types of trajectory plot are available:
#'
#' - [trajPlot()] by default will plot each lat/lon location showing the origin
#' of each trajectory, if no `pollutant` is supplied.
#'
#' - If a pollutant is given, by merging the trajectory data with concentration
#' data, the trajectories are colour-coded by the concentration of `pollutant`.
#' With a long time series there can be lots of overplotting making it difficult
#' to gauge the overall concentration pattern. In these cases setting `alpha` to
#' a low value e.g. 0.1 can help.
#'
#' The user can also show points instead of lines by `plot.type = "p"`.
#'
#' Note that [trajPlot()] will plot only the full length trajectories. This should
#' be remembered when selecting only part of a year to plot.
#'
#' @inheritParams scatterPlot
#'
#' @param mydata Data frame, the result of importing a trajectory file using
#'   [importTraj()].
#'
#' @param lon,lat Columns containing the decimal longitude and latitude.
#'
#' @param pollutant Pollutant to be plotted. By default the trajectory height is
#'   used.
#'
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
#'
#' @param cols Colours for plotting. Passed to [openColours()].
#'
#' @param crs The coordinate reference system to use for plotting. Defaults to
#'   `4326`, which is the WGS84 geographic coordinate system, the standard,
#'   unprojected latitude/longitude system used in GPS, Google Earth, and GIS
#'   mapping. Other `crs` values are available - for example, `27700` will use
#'   the the OSGB36/British National Grid.
#'
#' @param map Should a base map be drawn? If `TRUE` the world base map provided
#'   by [ggplot2::map_data()] will be used.
#'
#' @param group It is sometimes useful to group and colour trajectories
#'   according to a grouping variable. See example below.
#'
#' @param map.fill Should the base map be a filled polygon? Default is to fill
#'   countries.
#'
#' @param map.cols If `map.fill = TRUE` `map.cols` controls the fill colour.
#'   Examples include `map.fill = "grey40"` and `map.fill =
#'   openColours("default", 10)`. The latter colours the countries and can help
#'   differentiate them.
#' @param map.border The colour to use for the map outlines/borders. Defaults to
#'   `"black"`.
#'
#' @param map.alpha The transparency level of the filled map which takes values
#'   from 0 (full transparency) to 1 (full opacity). Setting it below 1 can help
#'   view trajectories, trajectory surfaces etc. *and* a filled base map.
#'
#' @param map.lwd The map line width, a positive number, defaulting to `1`.
#'
#' @param map.lty The map line type. Line types can either be specified as an
#'   integer (`0` = blank, `1` = solid (default), `2` = dashed, `3` = dotted,
#'   `4` = dotdash, `5` = longdash, `6` = twodash) or as one of the character
#'   strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or
#'   "twodash", where "blank" uses 'invisible lines' (i.e., does not draw them).
#'
#' @param grid.col The colour of the map grid to be used. To remove the grid set
#'   `grid.col = "transparent"`.
#'
#' @param grid.nx,grid.ny The approximate number of ticks to draw on the map
#'   grid. `grid.nx` defaults to `9`, and `grid.ny` defaults to whatever value
#'   is passed to `grid.nx`. Setting both values to `0` will remove the grid
#'   entirely. The number of ticks is approximate as this value is passed to
#'   [scales::breaks_pretty()] to determine nice-looking, round breakpoints.
#'
#' @param npoints A dot is placed every `npoints` along each full trajectory.
#'   For hourly back trajectories points are plotted every `npoint` hours. This
#'   helps to understand where the air masses were at particular times and get a
#'   feel for the speed of the air (points closer together correspond to slower
#'   moving air masses). If `npoints = NA` then no points are added.
#'
#' @param origin If true a filled circle dot is shown to mark the receptor
#'   point.
#'
#' @param key.title The title of the key.
#'
#' @param key.position Location where the scale key should be plotted. Allowed
#'   arguments currently include `"top"`, `"right"`, `"bottom"`, and `"left"`.
#'
#' @param key Should a key be drawn? Defaults to `TRUE`.
#'
#' @param key.columns Number of columns to be used in the key.
#'
#' @param plot Should a plot be produced? `FALSE` can be useful when analysing
#'   data to extract plot components and plotting them in other ways.
#'
#' @param ... Addition options are passed on to [cutData()] for `type` handling.
#'   Some additional arguments are also available:
#'    - `xlab`, `ylab` and `main` override the x-axis label, y-axis label, and plot title.
#'    - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have 2 columns and 5 rows.
#'    - `fontsize` overrides the overall font size of the plot.
#'    - `border` sets the border colour of each bar.
#'
#' @export
#' @family trajectory analysis functions
#' @author David Carslaw
#' @author Jack Davison
#' @examples
#' \dontrun{
#' # show a simple case with no pollutant i.e. just the trajectories
#' # let's check to see where the trajectories were coming from when
#' # Heathrow Airport was closed due to the Icelandic volcanic eruption
#' # 15--21 April 2010.
#' # import trajectories for London and plot
#'
#' lond <- importTraj("london", 2010)
#'
#' # well, HYSPLIT seems to think there certainly were conditions where trajectories
#' # orginated from Iceland...
#' trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"))
#'
#' # plot by day, need a column that makes a date
#' lond$day <- as.Date(lond$date)
#' trajPlot(
#'   selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
#'   type = "day"
#' )
#'
#' # or show each day grouped by colour, with some other options set
#' trajPlot(
#'   selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
#'   group = "day",
#'   cols = "turbo",
#'   key.position = "right",
#'   key.columns = 1,
#'   lwd = 2,
#'   cex = 4
#' )
#' }
trajPlot <- function(
  mydata,
  lon = "lon",
  lat = "lat",
  pollutant = "height",
  type = "default",
  map = TRUE,
  group = NULL,
  cols = "default",
  crs = 4326,
  map.fill = TRUE,
  map.cols = "grey40",
  map.border = "black",
  map.alpha = 0.4,
  map.lwd = 1,
  map.lty = 1,
  grid.col = "deepskyblue",
  grid.nx = 9,
  grid.ny = grid.nx,
  npoints = 12,
  origin = TRUE,
  key = TRUE,
  key.title = group,
  key.position = "right",
  key.columns = 1,
  auto.text = TRUE,
  plot = TRUE,
  ...
) {
  rlang::check_installed("sf")

  if (rlang::is_logical(key) && !key) {
    key.position <- "none"
  }

  # variables needed in trajectory plots
  vars <- c("date", "date2", "lat", "lon", "hour.inc", pollutant)

  # if group is present, need to add that list of variables unless it is a
  # pre-defined date-based one
  if (!is.null(group)) {
    if (group %in% dateTypes || any(type %in% dateTypes)) {
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

  # check data and prepare for plotting
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  mydata <- cutData(mydata, type = type, ...)

  # prepare group (NB: using suffix as we don't want numeric cuts)
  suffix <- NULL
  if (!is.null(group)) {
    if (group %in% names(mydata)) {
      if (
        is.numeric(mydata[[group]]) ||
          lubridate::is.Date(mydata[[group]]) ||
          lubridate::is.POSIXct(mydata[[group]])
      ) {
        suffix <- "__cuts"
      }
    }
  }
  mydata <- cutData(mydata, type = group, suffix = suffix)

  # if no group at all, need a dummy group
  if (is.null(group)) {
    mydata$theGroup <- factor("(all)")
    group <- "theGroup"
  }

  # remove any rows with missing lat/lon
  mydata <- filter(mydata, !is.na(lat), !is.na(lon))

  # slect only full length trajectories
  mydata <- mydata[order(mydata$date, mydata$hour.inc), ]

  # length of back mydataectories
  mydata$len <- ave(mydata$lat, mydata$date, FUN = length)

  # find length of back trajectories, choose most frequent
  # so that partial trajectories are not plotted
  n <- as.numeric(names(which.max(table(abs(mydata$len)))))
  mydata <- dplyr::filter(mydata, .data$len == n)

  # ensure there are no duplicate trajectories in dataframe
  count_check <-
    dplyr::count(
      mydata,
      dplyr::across(dplyr::all_of(c("date", "date2", group, type)))
    )
  if (any(count_check$n > 1)) {
    cli::cli_abort(
      c(
        "x" = "Duplicates in {.field date2} detected. Does {.arg mydata} contain multiple trajectories from different origins?",
        "i" = "Try setting `type` or `group` to silence this error."
      )
    )
  }

  # extra.args
  extra.args <- list(...)

  # aspect, cex
  extra.args$plot.type <- extra.args$plot.type %||% "l"
  extra.args$cex <- extra.args$cex %||% 1
  extra.args$lty <- extra.args$lty %||% 1
  extra.args$lwd <- extra.args$lwd %||% 1
  extra.args$ylab <- extra.args$ylab %||% ""
  extra.args$xlab <- extra.args$xlab %||% ""
  extra.args$main <- extra.args$main %||% NULL
  extra.args$alpha <- extra.args$alpha %||% 1

  # convert traj data to simple features
  sf_points <- sf::st_as_sf(mydata, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = crs)

  # get origin
  sf_origins <- dplyr::filter(sf_points, .data$hour.inc == 0) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("geometry", type, group))))

  # get bbox - axis limits
  bbox <- sf::st_bbox(sf_points)
  extra.args$xlim <- extra.args$xlim %||% unname(bbox[c(1, 3)])
  extra.args$ylim <- extra.args$ylim %||% unname(bbox[c(2, 4)])

  # create lines from points, grouped by date and type (and group if present)
  sf_lines <- sf_points |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "date",
      type,
      group
    )))) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("LINESTRING")

  # get points every npoints hours
  sf_points <- dplyr::filter(
    sf_points,
    .data$hour.inc %% npoints == 0
  )

  # if plot.type is points, remove lines
  if (extra.args$plot.type == "p") {
    sf_lines <- head(sf_lines, n = 0)
  }

  # to allow for basemaps to be multicoloured, we need to work out the number of
  # panels in the plot
  n_types <- c()
  for (i in type) {
    n_types <- c(n_types, length(levels(sf_lines[[i]])))
  }
  n_types <- purrr::reduce(n_types, .f = `*`)

  # base plot & themes
  thePlot <- ggplot2::ggplot() +
    theme_openair_sf(key.position, grid.col = grid.col) +
    set_extra_fontsize(extra.args) +
    get_facet(
      type,
      extra.args,
      scales = "fixed",
      auto.text = auto.text
    ) +
    ggplot2::labs(
      x = quickText(extra.args$xlab, auto.text),
      y = quickText(extra.args$ylab, auto.text),
      title = quickText(extra.args$main, auto.text),
      colour = quickText(key.title, auto.text)
    )

  # add map if requested
  if (map) {
    thePlot <- thePlot +
      layer_worldmap(
        crs,
        n_maps = n_types,
        map.fill = map.fill,
        map.cols = map.cols,
        map.border = map.border,
        map.alpha = map.alpha,
        map.lwd = map.lwd,
        map.lty = map.lty
      )
  }

  # add lines/points - needs to be done before setting coordinates
  thePlot <- thePlot +
    ggplot2::geom_sf(
      data = sf_lines,
      ggplot2::aes(color = .data[[group]]),
      linetype = extra.args$lty,
      linewidth = extra.args$lwd / 1.5,
      alpha = extra.args$alpha
    ) +
    ggplot2::geom_sf(
      data = sf_points,
      ggplot2::aes(color = .data[[group]]),
      size = extra.args$cex,
      alpha = extra.args$alpha
    )

  # add origin point if requested
  if (origin) {
    thePlot <- thePlot +
      ggplot2::geom_sf(data = sf_origins)
  }

  # axis scales
  thePlot <-
    thePlot +
    ggplot2::coord_sf(
      xlim = extra.args$xlim,
      ylim = extra.args$ylim,
      default_crs = crs,
      crs = crs
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(grid.nx)) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(grid.ny))

  # colours
  if (is.factor(mydata[[group]])) {
    n_cols <- dplyr::n_distinct(levels(mydata[[group]]))
    thePlot <-
      thePlot +
      ggplot2::scale_color_manual(
        values = openair::openColours(
          ifelse(n_cols == 1, "grey20", cols),
          n = n_cols
        ),
        drop = FALSE
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          reverse = key.position %in% c("left", "right"),
          theme = ggplot2::theme(
            legend.title.position = ifelse(
              key.position %in% c("left", "right"),
              "top",
              key.position
            ),
            legend.text.position = key.position
          ),
          ncol = if (missing(key.columns)) {
            if (key.position %in% c("left", "right")) {
              NULL
            } else {
              n_cols
            }
          } else {
            key.columns
          }
        )
      )

    if (n_cols <= 1) {
      thePlot <- thePlot + ggplot2::guides(color = ggplot2::guide_none())
    }
  } else {
    thePlot <-
      thePlot +
      ggplot2::scale_color_gradientn(
        colours = openair::openColours(cols),
        oob = scales::oob_squish,
        label = if (lubridate::is.POSIXct(mydata[[group]])) {
          \(x) scales::label_date()(as.POSIXct(x))
        } else if (lubridate::is.Date(mydata[[group]])) {
          \(x) scales::label_date()(as.Date(x))
        } else {
          scales::label_comma()
        }
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_colorbar(
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
  }

  if (plot) {
    plot(thePlot)
  }

  output <-
    list(
      plot = thePlot,
      data = dplyr::tibble(mydata),
      call = match.call()
    )
  class(output) <- "openair"

  invisible(output)
}

# Function that adds a world map as an sf layer
layer_worldmap <- function(
  crs,
  n_maps,
  map.fill,
  map.cols,
  map.border,
  map.alpha,
  map.lwd,
  map.lty
) {
  map_sf <- make_sf_world_map(crs)

  if (!map.fill) {
    map.cols <- "transparent"
  }

  # back-compatibility - need to be able to provide a vector of colours
  n_groups <- dplyr::n_distinct(map_sf$group) * n_maps
  while (length(map.cols) < n_groups) {
    map.cols <- c(map.cols, map.cols)
  }
  map.cols <- map.cols[1:n_groups]

  ggplot2::geom_sf(
    data = map_sf,
    colour = map.border,
    fill = map.cols,
    alpha = map.alpha,
    linewidth = map.lwd / 10,
    linetype = map.lty
  )
}

# function to create a world map from ggplot2 map_data
make_sf_world_map <- function(crs = 4326) {
  ggplot2::map_data("world") |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "region",
      "subregion",
      "group"
    )))) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("POLYGON") |>
    sf::st_transform(crs = crs)
}

# adapted theme_openair with a (by default) blue dashed gridline
theme_openair_sf <- function(key.position, grid.col) {
  list(
    theme_openair(key.position),
    ggplot2::theme(
      panel.grid = ggplot2::element_line(
        colour = grid.col,
        linetype = 2,
        linewidth = 0.25
      ),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = grid.col),
      panel.ontop = TRUE,
      panel.background = ggplot2::element_blank()
    )
  )
}
