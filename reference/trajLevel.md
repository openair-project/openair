# Trajectory level plots with conditioning

This function plots gridded back trajectories. This function requires
that data are imported using the
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md)
function.

## Usage

``` r
trajLevel(
  mydata,
  lon = "lon",
  lat = "lat",
  pollutant = "height",
  type = "default",
  smooth = FALSE,
  statistic = "frequency",
  percentile = 90,
  lon.inc = 1,
  lat.inc = lon.inc,
  min.bin = 1,
  .combine = NULL,
  sigma = 1.5,
  cols = "default",
  crs = 4326,
  map = TRUE,
  map.fill = TRUE,
  map.cols = "grey40",
  map.border = "black",
  map.alpha = 0.3,
  map.lwd = 1,
  map.lty = 1,
  grid.col = "deepskyblue",
  grid.nx = 9,
  grid.ny = grid.nx,
  origin = TRUE,
  key = TRUE,
  key.position = "right",
  key.columns = NULL,
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

- smooth:

  Should the trajectory surface be smoothed?

- statistic:

  One of:

  - `"frequency"` (the default) shows trajectory frequencies.

  - `"hexbin"`, which is similar to `"frequency"` but shows a hexagonal
    grid of counts.

  - `"difference"` - in this case trajectories where the associated
    concentration is greater than `percentile` are compared with the the
    full set of trajectories to understand the differences in
    frequencies of the origin of air masses. The comparison is made by
    comparing the percentage change in gridded frequencies. For example,
    such a plot could show that the top 10\\ to the east.

  - `"pscf"` for a Potential Source Contribution Function map. This
    statistic method interacts with `percentile`.

  - `"cwt"` for concentration weighted trajectories.

  - `"sqtba"` to undertake Simplified Quantitative Transport Bias
    Analysis. This statistic method interacts with `.combine` and
    `sigma`.

- percentile:

  The percentile concentration of `pollutant` against which the all
  trajectories are compared.

- lon.inc, lat.inc:

  The longitude and latitude intervals to be used for binning data. If
  `statistic = "hexbin"`, the minimum value out of of `lon.inc` and
  `lat.inc` is passed to the `binwidth` argument of
  [`ggplot2::geom_hex()`](https://ggplot2.tidyverse.org/reference/geom_hex.html).

- min.bin:

  The minimum number of unique points in a grid cell. Counts below
  `min.bin` are set as missing.

- .combine:

  When statistic is "SQTBA" it is possible to combine lots of receptor
  locations to derive a single map. `.combine` identifies the column
  that differentiates different sites (commonly a column named
  `"site"`). Note that individual site maps are normalised first by
  dividing by their mean value.

- sigma:

  For the SQTBA approach `sigma` determines the amount of back
  trajectory spread based on the Gaussian plume equation. Values in the
  literature suggest 5.4 km after one hour. However, testing suggests
  lower values reveal source regions more effectively while not
  introducing too much noise.

- cols:

  Colours for plotting. Passed to
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md).

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

- origin:

  If true a filled circle dot is shown to mark the receptor point.

- key:

  Should a key be drawn? Defaults to `TRUE`.

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

  - `border` sets the border colour of each tile.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Details

An alternative way of showing the trajectories compared with plotting
trajectory lines is to bin the points into latitude/longitude intervals.
For these purposes `trajLevel()` should be used. There are several
trajectory statistics that can be plotted as gridded surfaces. First,
`statistic` can be set to "frequency" to show the number of back
trajectory points in a grid square. Grid squares are by default at 1
degree intervals, controlled by `lat.inc` and `lon.inc`. Such plots are
useful for showing the frequency of air mass locations. Note that it is
also possible to set `statistic = "hexbin"` for plotting frequencies
(not concentrations), which will produce a plot by hexagonal binning.

If `statistic = "difference"` the trajectories associated with a
concentration greater than `percentile` are compared with the the full
set of trajectories to understand the differences in frequencies of the
origin of air masses of the highest concentration trajectories compared
with the trajectories on average. The comparison is made by comparing
the percentage change in gridded frequencies. For example, such a plot
could show that the top 10\\ the east.

If `statistic = "pscf"` then the Potential Source Contribution Function
is plotted. The PSCF calculates the probability that a source is located
at latitude \\i\\ and longitude \\j\\ (Pekney et al., 2006).The basis of
PSCF is that if a source is located at (i,j), an air parcel back
trajectory passing through that location indicates that material from
the source can be collected and transported along the trajectory to the
receptor site. PSCF solves \$\$PSCF = m\_{ij}/n\_{ij}\$\$ where
\\n\_{ij}\\ is the number of times that the trajectories passed through
the cell (i,j) and \\m\_{ij}\\ is the number of times that a source
concentration was high when the trajectories passed through the cell
(i,j). The criterion for determining \\m\_{ij}\\ is controlled by
`percentile`, which by default is 90. Note also that cells with few data
have a weighting factor applied to reduce their effect.

A limitation of the PSCF method is that grid cells can have the same
PSCF value when sample concentrations are either only slightly higher or
much higher than the criterion. As a result, it can be difficult to
distinguish moderate sources from strong ones. Seibert et al. (1994)
computed concentration fields to identify source areas of pollutants.
The Concentration Weighted Trajectory (CWT) approach considers the
concentration of a species together with its residence time in a grid
cell. The CWT approach has been shown to yield similar results to the
PSCF approach. The openair manual has more details and examples of these
approaches.

A further useful refinement is to smooth the resulting surface, which is
possible by setting `smooth = TRUE`.

## Note

This function is under active development and is likely to change

## References

Pekney, N. J., Davidson, C. I., Zhou, L., & Hopke, P. K. (2006).
Application of PSCF and CPF to PMF-Modeled Sources of PM 2.5 in
Pittsburgh. Aerosol Science and Technology, 40(10), 952-961.

Seibert, P., Kromp-Kolb, H., Baltensperger, U., Jost, D., 1994.
Trajectory analysis of high-alpine air pollution data. NATO Challenges
of Modern Society 18, 595-595.

Xie, Y., & Berkowitz, C. M. (2007). The use of conditional probability
functions and potential source contribution functions to identify source
regions and advection pathways of hydrocarbon emissions in Houston,
Texas. Atmospheric Environment, 41(28), 5831-5847.

## See also

Other trajectory analysis functions:
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md),
[`trajPlot()`](https://openair-project.github.io/openair/reference/trajPlot.md)

## Author

David Carslaw

Jack Davison

## Examples

``` r
# show a simple case with no pollutant i.e. just the trajectories
# let's check to see where the trajectories were coming from when
# Heathrow Airport was closed due to the Icelandic volcanic eruption
# 15--21 April 2010.
# import trajectories for London and plot
if (FALSE) { # \dontrun{
lond <- importTraj("london", 2010)
} # }
# more examples to follow linking with concentration measurements...

# import some measurements from KC1 - London
if (FALSE) { # \dontrun{
kc1 <- importAURN("kc1", year = 2010)
# now merge with trajectory data by 'date'
lond <- merge(lond, kc1, by = "date")

# trajectory plot, no smoothing - and limit lat/lon area of interest
# use PSCF
trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
  pollutant = "pm10", statistic = "pscf"
)

# can smooth surface, suing CWT approach:
trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
  pollutant = "pm2.5", statistic = "cwt", smooth = TRUE
)

# plot by season:
trajLevel(subset(lond, lat > 40 & lat < 70 & lon > -20 & lon < 20),
  pollutant = "pm2.5",
  statistic = "pscf", type = "season"
)
} # }
```
