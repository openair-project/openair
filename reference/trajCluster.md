# Calculate clusters for back trajectories

This function carries out cluster analysis of HYSPLIT back trajectories.
The function is specifically designed to work with the trajectories
imported using the `openair`
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md)
function, which provides pre-calculated back trajectories at specific
receptor locations.

## Usage

``` r
trajCluster(
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
)
```

## Arguments

- traj:

  An openair trajectory data frame resulting from the use of
  [`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md).

- method:

  Method used to calculate the distance matrix for the back
  trajectories. There are two methods available: “Euclid” and “Angle”.

- n.cluster:

  Number of clusters to calculate.

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

- split.after:

  For `type` other than “default” e.g. “season”, the trajectories can
  either be calculated for each level of `type` independently or
  extracted after the cluster calculations have been applied to the
  whole data set.

- by.type:

  The percentage of the total number of trajectories is given for all
  data by default. Setting `by.type = TRUE` will make each panel add up
  to 100.

- crs:

  The coordinate reference system to use for plotting. Defaults to
  `4326`, which is the WGS84 geographic coordinate system, the standard,
  unprojected latitude/longitude system used in GPS, Google Earth, and
  GIS mapping. Other `crs` values are available - for example, `27700`
  will use the the OSGB36/British National Grid.

- cols:

  Colours to use for plotting. Can be a pre-set palette (e.g.,
  `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a user-defined
  vector of R colours (e.g., `c("yellow", "green", "blue", "black")` -
  see [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
  list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`).
  Alternatively, can be a list of arguments to control the colour
  palette more closely (e.g., `palette`, `direction`, `alpha`, etc.).
  See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  and
  [`colourOpts()`](https://openair-project.github.io/openair/reference/colourOpts.md)
  for more details.

- plot:

  When `openair` plots are created they are automatically printed to the
  active graphics device. `plot = FALSE` deactivates this behaviour.
  This may be useful when the plot *data* is of more interest, or the
  plot is required to appear later (e.g., later in a Quarto document, or
  to be saved to a file).

- ...:

  Passed to
  [`trajPlot()`](https://openair-project.github.io/openair/reference/trajPlot.md).

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

Jack Davison

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
