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
  group = NULL,
  cols = "default",
  crs = 4326,
  map = TRUE,
  map.res = "medium",
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
  key.title = group,
  key.position = "right",
  key.columns = 1,
  strip.position = "top",
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
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

  Character string(s) defining how data should be split/conditioned
  before plotting. `"default"` produces a single panel using the entire
  dataset. Any other options will split the plot into different panels -
  a roughly square grid of panels if one `type` is given, or a 2D matrix
  of panels if two `types` are given. `type` is always passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md),
  and can therefore be any of:

  - A built-in type defined in
    [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
    (e.g., `"season"`, `"year"`, `"weekday"`, etc.). For example,
    `type = "season"` will split the plot into four panels, one for each
    season.

  - The name of a numeric column in `mydata`, which will be split into
    `n.levels` quantiles (defaulting to 4).

  - The name of a character or factor column in `mydata`, which will be
    used as-is. Commonly this could be a variable like `"site"` to
    ensure data from different monitoring sites are handled and
    presented separately. It could equally be any arbitrary column
    created by the user (e.g., whether a nearby possible pollutant
    source is active or not).

  Most `openair` plotting functions can take two `type` arguments. If
  two are given, the first is used for the columns and the second for
  the rows.

- group:

  A condition to colour the plot by, passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  An alternative to `pollutant`, and used preferentially to `pollutant`
  if both are set.

- cols:

  Colours to use for plotting. Can be a pre-set palette (e.g.,
  `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a user-defined
  vector of R colours (e.g., `c("yellow", "green", "blue", "black")` -
  see [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
  list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`). See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for more details.

- crs:

  The coordinate reference system to use for plotting. Defaults to
  `4326`, which is the WGS84 geographic coordinate system, the standard,
  unprojected latitude/longitude system used in GPS, Google Earth, and
  GIS mapping. Other `crs` values are available - for example, `27700`
  will use the the OSGB36/British National Grid.

- map:

  Should a base map be drawn? If `TRUE` the world base map provided by
  [`ggplot2::map_data()`](https://ggplot2.tidyverse.org/reference/map_data.html)
  will be used.

- map.res:

  The scale of the map to use. One of `110`, `50`, `10` or `small`,
  `medium`, `large`. Passed to
  [`rnaturalearth::ne_countries()`](https://docs.ropensci.org/rnaturalearth/reference/ne_countries.html).

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

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

- key.columns:

  Number of columns to be used in a categorical legend. With many
  categories a single column can make to key too wide. The user can thus
  choose to use several columns by setting `key.columns` to be less than
  the number of categories.

- strip.position:

  Location where the facet 'strips' are located when using `type`. When
  one `type` is provided, can be one of `"left"`, `"right"`, `"bottom"`
  or `"top"`. When two `type`s are provided, this argument defines
  whether the strips are "switched" and can take either `"x"`, `"y"`, or
  `"both"`. For example, `"x"` will switch the 'top' strip locations to
  the bottom of the plot.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the "2" in "NO2". Passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md).

- plot:

  When `openair` plots are created they are automatically printed to the
  active graphics device. `plot = FALSE` deactivates this behaviour.
  This may be useful when the plot *data* is of more interest, or the
  plot is required to appear later (e.g., later in a Quarto document, or
  to be saved to a file).

- key:

  Deprecated; please use `key.position`. If `FALSE`, sets `key.position`
  to `"none"`.

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
