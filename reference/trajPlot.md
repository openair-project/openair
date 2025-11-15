# Trajectory line plots with conditioning

This function plots back trajectories. This function requires that data
are imported using the `importTraj` function.

## Usage

``` r
trajPlot(
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
  grid.nx = 9,
  grid.ny = grid.nx,
  npoints = 12,
  origin = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- mydata:

  Data frame, the result of importing a trajectory file using
  `importTraj`.

- lon:

  Column containing the longitude, as a decimal.

- lat:

  Column containing the latitude, as a decimal.

- pollutant:

  Pollutant to be plotted. By default the trajectory height is used.

- type:

  `type` determines how the data are split, i.e., conditioned, and then
  plotted. The default is will produce a single plot using the entire
  data. Type can be one of the built-in types as detailed in `cutData`
  e.g. "season", "year", "weekday" and so on. For example,
  `type = "season"` will produce four plots — one for each season.

  It is also possible to choose `type` as another variable in the data
  frame. If that variable is numeric, then the data will be split into
  four quantiles (if possible) and labelled accordingly. If type is an
  existing character or factor variable, then those categories/levels
  will be used directly. This offers great flexibility for understanding
  the variation of different variables and how they depend on one
  another.

  `type` can be up length two e.g. `type = c("season", "weekday")` will
  produce a 2x2 plot split by season and day of the week. Note, when two
  types are provided the first forms the columns and the second the
  rows.

- map:

  Should a base map be drawn? If `TRUE` the world base map from the
  `maps` package is used.

- group:

  It is sometimes useful to group and colour trajectories according to a
  grouping variable. See example below.

- map.fill:

  Should the base map be a filled polygon? Default is to fill countries.

- map.res:

  The resolution of the base map. By default the function uses the
  ‘world’ map from the `maps` package. If `map.res = "hires"` then the
  (much) more detailed base map ‘worldHires’ from the `mapdata` package
  is used. Use
  [`library(mapdata)`](https://rdrr.io/r/base/library.html). Also
  available is a map showing the US states. In this case
  `map.res = "state"` should be used.

- map.cols:

  If `map.fill = TRUE` `map.cols` controls the fill colour. Examples
  include `map.fill = "grey40"` and
  `map.fill = openColours("default", 10)`. The latter colours the
  countries and can help differentiate them.

- map.border:

  The colour to use for the map outlines/borders. Defaults to `"black"`.

- map.alpha:

  The transparency level of the filled map which takes values from 0
  (full transparency) to 1 (full opacity). Setting it below 1 can help
  view trajectories, trajectory surfaces etc. *and* a filled base map.

- map.lwd:

  The map line width, a positive number, defaulting to `1`.

- map.lty:

  The map line type. Line types can either be specified as an integer
  (`0` = blank, `1` = solid (default), `2` = dashed, `3` = dotted, `4` =
  dotdash, `5` = longdash, `6` = twodash) or as one of the character
  strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash",
  or "twodash", where "blank" uses 'invisible lines' (i.e., does not
  draw them).

- projection:

  The map projection to be used. Different map projections are possible
  through the `mapproj` package. See `?mapproject` for extensive details
  and information on setting other parameters and orientation (see
  below).

- parameters:

  From the `mapproj` package. Optional numeric vector of parameters for
  use with the projection argument. This argument is optional only in
  the sense that certain projections do not require additional
  parameters. If a projection does not require additional parameters
  then set to null i.e. `parameters = NULL`.

- orientation:

  From the `mapproj` package. An optional vector c(latitude, longitude,
  rotation) which describes where the "North Pole" should be when
  computing the projection. Normally this is c(90, 0), which is
  appropriate for cylindrical and conic projections. For a planar
  projection, you should set it to the desired point of tangency. The
  third value is a clockwise rotation (in degrees), which defaults to
  the midrange of the longitude coordinates in the map.

- grid.col:

  The colour of the map grid to be used. To remove the grid set
  `grid.col = "transparent"`.

- grid.nx, grid.ny:

  The approximate number of ticks to draw on the map grid. `grid.nx`
  defaults to `9`, and `grid.ny` defaults to whatever value is passed to
  `grid.nx`. Setting both values to `0` will remove the grid entirely.
  The number of ticks is approximate as this value is passed to
  [`pretty()`](https://rdrr.io/r/base/pretty.html) to determine
  nice-looking, round breakpoints.

- npoints:

  A dot is placed every `npoints` along each full trajectory. For hourly
  back trajectories points are plotted every `npoint` hours. This helps
  to understand where the air masses were at particular times and get a
  feel for the speed of the air (points closer together correspond to
  slower moving air masses). If `npoints = NA` then no points are added.

- origin:

  If true a filled circle dot is shown to mark the receptor point.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  other arguments are passed to `cutData` and `scatterPlot`. This
  provides access to arguments used in both these functions and
  functions that they in turn pass arguments on to. For example,
  `plotTraj` passes the argument `cex` on to `scatterPlot` which in turn
  passes it on to the `lattice` function `xyplot` where it is applied to
  set the plot symbol size.

## Details

Several types of trajectory plot are available. `trajPlot` by default
will plot each lat/lon location showing the origin of each trajectory,
if no `pollutant` is supplied.

If a pollutant is given, by merging the trajectory data with
concentration data (see example below), the trajectories are
colour-coded by the concentration of `pollutant`. With a long time
series there can be lots of overplotting making it difficult to gauge
the overall concentration pattern. In these cases setting `alpha` to a
low value e.g. 0.1 can help.

The user can also show points instead of lines by `plot.type = "p"`.

Note that `trajPlot` will plot only the full length trajectories. This
should be remembered when selecting only part of a year to plot.

## See also

Other trajectory analysis functions:
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md),
[`trajLevel()`](https://openair-project.github.io/openair/reference/trajLevel.md)

## Author

David Carslaw

## Examples

``` r
# show a simple case with no pollutant i.e. just the trajectories
# let's check to see where the trajectories were coming from when
# Heathrow Airport was closed due to the Icelandic volcanic eruption
# 15--21 April 2010.
# import trajectories for London and plot
if (FALSE) { # \dontrun{
lond <- importTraj("london", 2010)
# well, HYSPLIT seems to think there certainly were conditions where trajectories
# orginated from Iceland...
trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"))
} # }

# plot by day, need a column that makes a date
if (FALSE) { # \dontrun{
lond$day <- as.Date(lond$date)
trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
  type = "day"
)
} # }

# or show each day grouped by colour, with some other options set
if (FALSE) { # \dontrun{
trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
  group = "day", col = "turbo", lwd = 2, key.pos = "right", key.col = 1
)
} # }
# more examples to follow linking with concentration measurements...
```
