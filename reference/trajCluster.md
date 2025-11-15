# Calculate clusters for back trajectories

This function carries out cluster analysis of HYSPLIT back trajectories.
The function is specifically designed to work with the trajectories
imported using the `openair` `importTraj` function, which provides
pre-calculated back trajectories at specific receptor locations.

## Usage

``` r
trajCluster(
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
)
```

## Arguments

- traj:

  An openair trajectory data frame resulting from the use of
  `importTraj`.

- method:

  Method used to calculate the distance matrix for the back
  trajectories. There are two methods available: “Euclid” and “Angle”.

- n.cluster:

  Number of clusters to calculate.

- type:

  `type` determines how the data are split i.e. conditioned, and then
  plotted. The default is will produce a single plot using the entire
  data. Type can be one of the built-in types as detailed in `cutData`
  e.g. “season”, “year”, “weekday” and so on. For example,
  `type = "season"` will produce four plots — one for each season. Note
  that the cluster calculations are separately made of each level of
  "type".

- cols:

  Colours to be used for plotting. Options include “default”,
  “increment”, “heat”, “jet” and `RColorBrewer` colours — see the
  `openair` `openColours` function for more details. For user defined
  the user can supply a list of colour names recognised by R (type
  [`colours()`](https://rdrr.io/r/grDevices/colors.html) to see the full
  list). An example would be `cols = c("yellow", "green", "blue")`

- split.after:

  For `type` other than “default” e.g. “season”, the trajectories can
  either be calculated for each level of `type` independently or
  extracted after the cluster calculations have been applied to the
  whole data set.

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

- by.type:

  The percentage of the total number of trajectories is given for all
  data by default. Setting `by.type = TRUE` will make each panel add up
  to 100.

- origin:

  If true a filled circle dot is shown to mark the receptor point.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  Other graphical parameters passed onto `lattice:levelplot` and
  `cutData`. Similarly, common axis and title labelling options (such as
  `xlab`, `ylab`, `main`) are passed to `levelplot` via `quickText` to
  handle routine formatting.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object. The `data` component contains both `traj` (the original data
appended with its cluster) and `results` (the average trajectory path
per cluster, shown in the `trajCluster()` plot.)

## Details

Two main methods are available to cluster the back trajectories using
two different calculations of the distance matrix. The default is to use
the standard Euclidian distance between each pair of trajectories. Also
available is an angle-based distance matrix based on Sirois and
Bottenheim (1995). The latter method is useful when the interest is the
direction of the trajectories in clustering.

The distance matrix calculations are made in C++ for speed. For data
sets of up to 1 year both methods should be relatively fast, although
the `method = "Angle"` does tend to take much longer to calculate.
Further details of these methods are given in the openair manual.

## References

Sirois, A. and Bottenheim, J.W., 1995. Use of backward trajectories to
interpret the 5-year record of PAN and O3 ambient air concentrations at
Kejimkujik National Park, Nova Scotia. Journal of Geophysical Research,
100: 2867-2881.

## See also

Other trajectory analysis functions:
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`trajLevel()`](https://openair-project.github.io/openair/reference/trajLevel.md),
[`trajPlot()`](https://openair-project.github.io/openair/reference/trajPlot.md)

Other cluster analysis functions:
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`timeProp()`](https://openair-project.github.io/openair/reference/timeProp.md)

## Author

David Carslaw

## Examples

``` r
if (FALSE) { # \dontrun{
## import trajectories
traj <- importTraj(site = "london", year = 2009)
## calculate clusters
clust <- trajCluster(traj, n.cluster = 5)
head(clust$data) ## note new variable 'cluster'
## use different distance matrix calculation, and calculate by season
traj <- trajCluster(traj, method = "Angle", type = "season", n.cluster = 4)
} # }
```
