# Trajectory line plots with conditioning

This function plots back trajectories. This function requires that data
are imported using the
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md)
function, or matches that structure.

## Usage

``` r
trajPlot(
  mydata,
  lon = "lon",
  lat = "lat",
  pollutant = NULL,
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
)
```

## Arguments

- mydata:

  Data frame, the result of importing a trajectory file using
  [`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md).

- lon, lat:

  Columns containing the decimal longitude and latitude.

- pollutant:

  Pollutant (or any numeric column) to be plotted, if any.
  Alternatively, use `group`.

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

  Should a base map be drawn? If `TRUE` the world base map provided by
  [`ggplot2::map_data()`](https://ggplot2.tidyverse.org/reference/map_data.html)
  will be used.

- group:

  A condition to colour the plot by, passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  An alternative to `pollutant`, and used preferentially to `pollutant`
  if both are set.

- cols:

  Colours for plotting. Passed to
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md).

- crs:

  The coordinate reference system to use for plotting. Defaults to
  `4326`, which is the WGS84 geographic coordinate system, the standard,
  unprojected latitude/longitude system used in GPS, Google Earth, and
  GIS mapping. Other `crs` values are available - for example, `27700`
  will use the the OSGB36/British National Grid.

- map.fill:

  Should the base map be a filled polygon? Default is to fill countries.

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

- grid.col:

  The colour of the map grid to be used. To remove the grid set
  `grid.col = "transparent"`.

- grid.nx, grid.ny:

  The approximate number of ticks to draw on the map grid. `grid.nx`
  defaults to `9`, and `grid.ny` defaults to whatever value is passed to
  `grid.nx`. Setting both values to `0` will remove the grid entirely.
  The number of ticks is approximate as this value is passed to
  [`scales::breaks_pretty()`](https://scales.r-lib.org/reference/breaks_pretty.html)
  to determine nice-looking, round breakpoints.

- npoints:

  A dot is placed every `npoints` along each full trajectory. For hourly
  back trajectories points are plotted every `npoint` hours. This helps
  to understand where the air masses were at particular times and get a
  feel for the speed of the air (points closer together correspond to
  slower moving air masses). If `npoints = NA` then no points are added.

- origin:

  If true a filled circle dot is shown to mark the receptor point.

- key:

  Should a key be drawn? Defaults to `TRUE`.

- key.title:

  The title of the key.

- key.position:

  Location where the scale key should be plotted. Allowed arguments
  currently include `"top"`, `"right"`, `"bottom"`, and `"left"`.

- key.columns:

  Number of columns to be used in the key.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly
  e.g. by subscripting the ‘2’ in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  Addition options are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for `type` handling. Some additional arguments are also available:

  - `xlab`, `ylab` and `main` override the x-axis label, y-axis label,
    and plot title.

  - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have
    2 columns and 5 rows.

  - `fontsize` overrides the overall font size of the plot.

  - `border` sets the border colour of each bar.

## Details

Several types of trajectory plot are available:

- `trajPlot()` by default will plot each lat/lon location showing the
  origin of each trajectory, if no `pollutant` is supplied.

- If a pollutant is given, by merging the trajectory data with
  concentration data, the trajectories are colour-coded by the
  concentration of `pollutant`. With a long time series there can be
  lots of overplotting making it difficult to gauge the overall
  concentration pattern. In these cases setting `alpha` to a low value
  e.g. 0.1 can help.

The user can also show points instead of lines by `plot.type = "p"`.

Note that `trajPlot()` will plot only the full length trajectories. This
should be remembered when selecting only part of a year to plot.

## See also

Other trajectory analysis functions:
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md),
[`trajLevel()`](https://openair-project.github.io/openair/reference/trajLevel.md)

## Author

David Carslaw

Jack Davison

## Examples

``` r
if (FALSE) { # \dontrun{
# show a simple case with no pollutant i.e. just the trajectories
# let's check to see where the trajectories were coming from when
# Heathrow Airport was closed due to the Icelandic volcanic eruption
# 15--21 April 2010.
# import trajectories for London and plot

lond <- importTraj("london", 2010)

# well, HYSPLIT seems to think there certainly were conditions where trajectories
# orginated from Iceland...
trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"))

# plot by day, need a column that makes a date
lond$day <- as.Date(lond$date)
trajPlot(
  selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
  type = "day"
)

# or show each day grouped by colour, with some other options set
trajPlot(
  selectByDate(lond, start = "15/4/2010", end = "21/4/2010"),
  group = "day",
  cols = "turbo",
  key.position = "right",
  key.columns = 1,
  lwd = 2,
  cex = 4
)
} # }
```
