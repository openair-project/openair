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
  map.res = "medium",
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
  key.title = NULL,
  key.position = "right",
  key.columns = NULL,
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
