# K-means clustering of bivariate polar plots

Function for identifying clusters in bivariate polar plots
([`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md));
identifying clusters in the original data for subsequent processing.

## Usage

``` r
polarCluster(
  mydata,
  pollutant = "nox",
  x = "ws",
  wd = "wd",
  n.clusters = 6,
  after = NA,
  cols = "Paired",
  angle.scale = 315,
  units = x,
  auto.text = TRUE,
  plot = TRUE,
  plot.data = FALSE,
  ...
)
```

## Arguments

- mydata:

  A data frame minimally containing a decimal wind direction, another
  variable to plot in polar coordinates (the default is a column `"ws"`
  — wind speed) and a pollutant. Should also contain `date` if plots by
  time period are required.

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`. Only one pollutant
  can be chosen.

- x:

  Name of variable to plot against wind direction in polar coordinates,
  the default is wind speed, “ws”.

- wd:

  The name of the column in `mydata` representing the decimal wind
  direction, 0 to 360 where 0/360 are North and 180 is South. Defaults
  to `"wd"`.

- n.clusters:

  Number of clusters to use. If `n.clusters` is more than length 1, then
  a faceted plot will be output showing the clusters identified for each
  one of `n.clusters`.

- after:

  The function can be applied to differences between polar plot surfaces
  (see
  [polarDiff](https://openair-project.github.io/openair/reference/polarDiff.md)
  for details). If an `after` data frame is supplied, the clustering
  will be carried out on the differences between `after` and `mydata` in
  the same way as
  [polarDiff](https://openair-project.github.io/openair/reference/polarDiff.md).

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

- angle.scale:

  In radial plots (e.g.,
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)),
  the radial scale is drawn directly on the plot itself. While suitable
  defaults have been chosen, sometimes the placement of the scale may
  interfere with an interesting feature. `angle.scale` can take any
  value between `0` and `360` to place the scale at a different angle,
  or `FALSE` to move it to the side of the plots.

- units:

  The units shown on the polar axis scale.

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

- plot.data:

  By default, the `data` component of `polarCluster()` contains the
  original data frame appended with a new "cluster" column. When
  `plot.data = TRUE`, the `data` component instead contains data to
  reproduce the clustered polar plot itself (similar to `data` returned
  by
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)).
  This may be useful for re-plotting the `polarCluster()` plot in other
  ways.

- ...:

  Other arguments passed on to
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md).

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object. The object includes four main components: `call`, the command
used to generate the plot; `data`, by default the original data frame
with a new field `cluster` identifying the cluster, `clust_stats` giving
the contributions made by each cluster to number of measurements, their
percentage and the percentage by pollutant; and `plot`, the plot itself.
Note that any rows where the value of `pollutant` is `NA` are ignored so
that the returned data frame may have fewer rows than the original.

If the clustering is carried out considering differences, i.e., an
`after` data frame is supplied, the output also includes the `after`
data frame with cluster identified.

## Details

Bivariate polar plots generated using the `polarPlot` function provide a
very useful graphical technique for identifying and characterising
different air pollution sources. While bivariate polar plots provide a
useful graphical indication of potential sources, their location and
wind-speed or other variable dependence, they do have several
limitations. Often, a \`feature' will be detected in a plot but the
subsequent analysis of data meeting particular wind speed/direction
criteria will be based only on the judgement of the investigator
concerning the wind speed-direction intervals of interest. Furthermore,
the identification of a feature can depend on the choice of the colour
scale used, making the process somewhat arbitrary.

`polarCluster` applies Partition Around Medoids (PAM) clustering
techniques to
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)
surfaces to help identify potentially interesting features for further
analysis. Details of PAM can be found in the `cluster` package (a core R
package that will be pre-installed on all R systems). PAM clustering is
similar to k-means but has several advantages e.g. is more robust to
outliers. The clustering is based on the equal contribution assumed from
the u and v wind components and the associated concentration. The data
are standardized before clustering takes place.

The function works best by first trying different numbers of clusters
and plotting them. This is achieved by setting `n.clusters` to be of
length more than 1. For example, if `n.clusters = 2:10` then a plot will
be output showing the 9 cluster levels 2 to 10.

The clustering can also be applied to differences in polar plot surfaces
(see
[`polarDiff()`](https://openair-project.github.io/openair/reference/polarDiff.md)).
On this case a second data frame (`after`) should be supplied.

Note that clustering is computationally intensive and the function can
take a long time to run — particularly when the number of clusters is
increased. For this reason it can be a good idea to run a few clusters
first to get a feel for it e.g. `n.clusters = 2:5`.

Once the number of clusters has been decided, the user can then run
`polarCluster` to return the original data frame together with a new
column `cluster`, which gives the cluster number as a character (see
example). Note that any rows where the value of `pollutant` is `NA` are
ignored so that the returned data frame may have fewer rows than the
original.

Note that there are no automatic ways in ensuring the most appropriate
number of clusters as this is application dependent. However, there is
often a-priori information available on what different features in polar
plots correspond to. Nevertheless, the appropriateness of different
clusters is best determined by post-processing the data. The Carslaw and
Beevers (2012) paper discusses these issues in more detail.

Note that unlike most other `openair` functions only a single `type`
“default” is allowed.

## References

Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). Detecting
and quantifying aircraft and other on-airport contributions to ambient
nitrogen oxides in the vicinity of a large international airport.
Atmospheric Environment. 40/28 pp 5424-5434.

Carslaw, D.C., & Beevers, S.D. (2013). Characterising and understanding
emission sources using bivariate polar plots and k-means clustering.
Environmental Modelling & Software, 40, 325-329.
doi:10.1016/j.envsoft.2012.09.005

## See also

Other polar directional analysis functions:
[`percentileRose()`](https://openair-project.github.io/openair/reference/percentileRose.md),
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md),
[`polarDiff()`](https://openair-project.github.io/openair/reference/polarDiff.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md),
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md),
[`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md),
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)

Other cluster analysis functions:
[`timeProp()`](https://openair-project.github.io/openair/reference/timeProp.md),
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md)

## Author

David Carslaw

## Examples

``` r
if (FALSE) { # \dontrun{
# plot 2-8 clusters. Warning! This can take several minutes...
polarCluster(mydata, pollutant = "nox", n.clusters = 2:8)

# basic plot with 6 clusters
results <- polarCluster(mydata, pollutant = "nox", n.clusters = 6)

# get results, could read into a new data frame to make it easier to refer to
# e.g. results <- results$data...
head(results$data)

# how many points are there in each cluster?
table(results$data$cluster)

# plot clusters 3 and 4 as a timeVariation plot using SAME colours as in
# cluster plot
timeVariation(subset(results$data, cluster %in% c("3", "4")),
  pollutant = "nox",
  group = "cluster", col = openColours("Paired", 6)[c(3, 4)]
)
} # }
```
