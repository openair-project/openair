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

  A data frame minimally containing `wd`, another variable to plot in
  polar coordinates (the default is a column “ws” — wind speed) and a
  pollutant. Should also contain `date` if plots by time period are
  required.

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`. Only one pollutant
  can be chosen.

- x:

  Name of variable to plot against wind direction in polar coordinates,
  the default is wind speed, “ws”.

- wd:

  Name of wind direction field.

- n.clusters:

  Number of clusters to use. If `n.clusters` is more than length 1, then
  a `lattice` panel plot will be output showing the clusters identified
  for each one of `n.clusters`.

- after:

  The function can be applied to differences between polar plot surfaces
  (see
  [polarDiff](https://openair-project.github.io/openair/reference/polarDiff.md)
  for details). If an `after` data frame is supplied, the clustering
  will be carried out on the differences between `after` and `mydata` in
  the same way as
  [polarDiff](https://openair-project.github.io/openair/reference/polarDiff.md).

- cols:

  Colours to be used for plotting. Options include “default”,
  “increment”, “heat”, “jet” and `RColorBrewer` colours — see the
  `openair` `openColours` function for more details. For user defined
  the user can supply a list of colour names recognised by R (type
  [`colours()`](https://rdrr.io/r/grDevices/colors.html) to see the full
  list). An example would be `cols = c("yellow", "green", "blue")`.
  `cols` can also take the values `"viridis"`, `"magma"`, `"inferno"`,
  or `"plasma"` which are the viridis colour maps ported from Python's
  Matplotlib library.

- angle.scale:

  Sometimes the placement of the scale may interfere with an interesting
  feature. The user can therefore set `angle.scale` to any value between
  0 and 360 degrees to mitigate such problems. For example
  `angle.scale = 45` will draw the scale heading in a NE direction.

- units:

  The units shown on the polar axis scale.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly
  e.g. by subscripting the \`2' in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

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

  Arguments passed on to
  [`polarPlot`](https://openair-project.github.io/openair/reference/polarPlot.md)

  `type`

  :   `type` determines how the data are split i.e. conditioned, and
      then plotted. The default is will produce a single plot using the
      entire data. Type can be one of the built-in types as detailed in
      `cutData` e.g. “season”, “year”, “weekday” and so on. For example,
      `type = "season"` will produce four plots — one for each season.

      It is also possible to choose `type` as another variable in the
      data frame. If that variable is numeric, then the data will be
      split into four quantiles (if possible) and labelled accordingly.
      If type is an existing character or factor variable, then those
      categories/levels will be used directly. This offers great
      flexibility for understanding the variation of different variables
      and how they depend on one another.

      Type can be up length two e.g. `type = c("season", "weekday")`
      will produce a 2x2 plot split by season and day of the week. Note,
      when two types are provided the first forms the columns and the
      second the rows.

  `statistic`

  :   The statistic that should be applied to each wind speed/direction
      bin. Because of the smoothing involved, the colour scale for some
      of these statistics is only to provide an indication of overall
      pattern and should not be interpreted in concentration units e.g.
      for `statistic = "weighted.mean"` where the bin mean is multiplied
      by the bin frequency and divided by the total frequency. In many
      cases using `polarFreq` will be better. Setting
      `statistic = "weighted.mean"` can be useful because it provides an
      indication of the concentration \* frequency of occurrence and
      will highlight the wind speed/direction conditions that dominate
      the overall mean.Can be:

      - “mean” (default), “median”, “max” (maximum), “frequency”.
        “stdev” (standard deviation), “weighted.mean”.

      - `statistic = "nwr"` Implements the Non-parametric Wind
        Regression approach of Henry et al. (2009) that uses kernel
        smoothers. The `openair` implementation is not identical because
        Gaussian kernels are used for both wind direction and speed. The
        smoothing is controlled by `ws_spread` and `wd_spread`.

      - `statistic = "cpf"` the conditional probability function (CPF)
        is plotted and a single (usually high) percentile level is
        supplied. The CPF is defined as CPF = my/ny, where my is the
        number of samples in the y bin (by default a wind direction,
        wind speed interval) with mixing ratios greater than the
        *overall* percentile concentration, and ny is the total number
        of samples in the same wind sector (see Ashbaugh et al., 1985).
        Note that percentile intervals can also be considered; see
        `percentile` for details.

      - When `statistic = "r"` or `statistic = "Pearson"`, the Pearson
        correlation coefficient is calculated for *two* pollutants. The
        calculation involves a weighted Pearson correlation coefficient,
        which is weighted by Gaussian kernels for wind direction an the
        radial variable (by default wind speed). More weight is assigned
        to values close to a wind speed-direction interval. Kernel
        weighting is used to ensure that all data are used rather than
        relying on the potentially small number of values in a wind
        speed-direction interval.

      - When `statistic = "Spearman"`, the Spearman correlation
        coefficient is calculated for *two* pollutants. The calculation
        involves a weighted Spearman correlation coefficient, which is
        weighted by Gaussian kernels for wind direction an the radial
        variable (by default wind speed). More weight is assigned to
        values close to a wind speed-direction interval. Kernel
        weighting is used to ensure that all data are used rather than
        relying on the potentially small number of values in a wind
        speed-direction interval.

      - `"robust_slope"` is another option for pair-wise statistics and
        `"quantile.slope"`, which uses quantile regression to estimate
        the slope for a particular quantile level (see also `tau` for
        setting the quantile level).

      - `"york_slope"` is another option for pair-wise statistics which
        uses the *York regression method* to estimate the slope. In this
        method the uncertainties in `x` and `y` are used in the
        determination of the slope. The uncertainties are provided by
        `x_error` and `y_error` — see below.

  `limits`

  :   The function does its best to choose sensible limits
      automatically. However, there are circumstances when the user will
      wish to set different ones. An example would be a series of plots
      showing each year of data separately. The limits are set in the
      form `c(lower, upper)`, so `limits = c(0, 100)` would force the
      plot limits to span 0-100.

  `exclude.missing`

  :   Setting this option to `TRUE` (the default) removes points from
      the plot that are too far from the original data. The smoothing
      routines will produce predictions at points where no data exist
      i.e. they predict. By removing the points too far from the
      original data produces a plot where it is clear where the original
      data lie. If set to `FALSE` missing data will be interpolated.

  `uncertainty`

  :   Should the uncertainty in the calculated surface be shown? If
      `TRUE` three plots are produced on the same scale showing the
      predicted surface together with the estimated lower and upper
      uncertainties at the 95% confidence interval. Calculating the
      uncertainties is useful to understand whether features are real or
      not. For example, at high wind speeds where there are few data
      there is greater uncertainty over the predicted values. The
      uncertainties are calculated using the GAM and weighting is done
      by the frequency of measurements in each wind speed-direction bin.
      Note that if uncertainties are calculated then the type is set to
      "default".

  `percentile`

  :   If `statistic = "percentile"` then `percentile` is used, expressed
      from 0 to 100. Note that the percentile value is calculated in the
      wind speed, wind direction ‘bins’. For this reason it can also be
      useful to set `min.bin` to ensure there are a sufficient number of
      points available to estimate a percentile. See `quantile` for more
      details of how percentiles are calculated.

      `percentile` is also used for the Conditional Probability Function
      (CPF) plots. `percentile` can be of length two, in which case the
      percentile *interval* is considered for use with CPF. For example,
      `percentile = c(90, 100)` will plot the CPF for concentrations
      between the 90 and 100th percentiles. Percentile intervals can be
      useful for identifying specific sources. In addition, `percentile`
      can also be of length 3. The third value is the ‘trim’ value to be
      applied. When calculating percentile intervals many can cover very
      low values where there is no useful information. The trim value
      ensures that values greater than or equal to the trim \* mean
      value are considered *before* the percentile intervals are
      calculated. The effect is to extract more detail from many source
      signatures. See the manual for examples. Finally, if the trim
      value is less than zero the percentile range is interpreted as
      absolute concentration values and subsetting is carried out
      directly.

  `weights`

  :   At the edges of the plot there may only be a few data points in
      each wind speed-direction interval, which could in some situations
      distort the plot if the concentrations are high. `weights` applies
      a weighting to reduce their influence. For example and by default
      if only a single data point exists then the weighting factor is
      0.25 and for two points 0.5. To not apply any weighting and use
      the data as is, use `weights = c(1, 1, 1)`.

      An alternative to down-weighting these points they can be removed
      altogether using `min.bin`.

  `min.bin`

  :   The minimum number of points allowed in a wind speed/wind
      direction bin. The default is 1. A value of two requires at least
      2 valid records in each bin an so on; bins with less than 2 valid
      records are set to NA. Care should be taken when using a value \>
      1 because of the risk of removing real data points. It is
      recommended to consider your data with care. Also, the `polarFreq`
      function can be of use in such circumstances.

  `mis.col`

  :   When `min.bin` is \> 1 it can be useful to show where data are
      removed on the plots. This is done by shading the missing data in
      `mis.col`. To not highlight missing data when `min.bin` \> 1
      choose `mis.col = "transparent"`.

  `alpha`

  :   The alpha transparency to use for the plotting surface (a value
      between 0 and 1 with zero being fully transparent and 1 fully
      opaque). Setting a value below 1 can be useful when plotting
      surfaces on a map using the package `openairmaps`.

  `upper`

  :   This sets the upper limit wind speed to be used. Often there are
      only a relatively few data points at very high wind speeds and
      plotting all of them can reduce the useful information in the
      plot.

  `force.positive`

  :   The default is `TRUE`. Sometimes if smoothing data with steep
      gradients it is possible for predicted values to be negative.
      `force.positive = TRUE` ensures that predictions remain positive.
      This is useful for several reasons. First, with lots of missing
      data more interpolation is needed and this can result in artefacts
      because the predictions are too far from the original data.
      Second, if it is known beforehand that the data are all positive,
      then this option carries that assumption through to the
      prediction. The only likely time where setting
      `force.positive = FALSE` would be if background concentrations
      were first subtracted resulting in data that is legitimately
      negative. For the vast majority of situations it is expected that
      the user will not need to alter the default option.

  `k`

  :   This is the smoothing parameter used by the `gam` function in
      package `mgcv`. Typically, value of around 100 (the default) seems
      to be suitable and will resolve important features in the plot.
      The most appropriate choice of `k` is problem-dependent; but
      extensive testing of polar plots for many different problems
      suggests a value of `k` of about 100 is suitable. Setting `k` to
      higher values will not tend to affect the surface predictions by
      much but will add to the computation time. Lower values of `k`
      will increase smoothing. Sometimes with few data to plot
      `polarPlot` will fail. Under these circumstances it can be worth
      lowering the value of `k`.

  `normalise`

  :   If `TRUE` concentrations are normalised by dividing by their mean
      value. This is done *after* fitting the smooth surface. This
      option is particularly useful if one is interested in the patterns
      of concentrations for several pollutants on different scales e.g.
      NOx and CO. Often useful if more than one `pollutant` is chosen.

  `key.header`

  :   Adds additional text/labels to the scale key. For example, passing
      the options `key.header = "header", key.footer = "footer1"` adds
      addition text above and below the scale key. These arguments are
      passed to `drawOpenKey` via `quickText`, applying the `auto.text`
      argument, to handle formatting.

  `key.footer`

  :   see `key.footer`.

  `key.position`

  :   Location where the scale key is to plotted. Allowed arguments
      currently include `"top"`, `"right"`, `"bottom"` and `"left"`.

  `key`

  :   Fine control of the scale key via `drawOpenKey`. See `drawOpenKey`
      for further details.

  `ws_spread`

  :   The value of sigma used for Gaussian kernel weighting of wind
      speed when `statistic = "nwr"` or when correlation and regression
      statistics are used such as *r*. Default is `0.5`.

  `wd_spread`

  :   The value of sigma used for Gaussian kernel weighting of wind
      direction when `statistic = "nwr"` or when correlation and
      regression statistics are used such as *r*. Default is `4`.

  `x_error`

  :   The `x` error / uncertainty used when `statistic = "york_slope"`.

  `y_error`

  :   The `y` error / uncertainty used when `statistic = "york_slope"`.

  `kernel`

  :   Type of kernel used for the weighting procedure for when
      correlation or regression techniques are used. Only `"gaussian"`
      is supported but this may be enhanced in the future.

  `formula.label`

  :   When pair-wise statistics such as regression slopes are calculated
      and plotted, should a formula label be displayed? `formula.label`
      will also determine whether concentration information is printed
      when `statistic = "cpf"`.

  `tau`

  :   The quantile to be estimated when `statistic` is set to
      `"quantile.slope"`. Default is `0.5` which is equal to the median
      and will be ignored if `"quantile.slope"` is not used.

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
## plot 2-8 clusters. Warning! This can take several minutes...
polarCluster(mydata, pollutant = "nox", n.clusters = 2:8)

# basic plot with 6 clusters
results <- polarCluster(mydata, pollutant = "nox", n.clusters = 6)

## get results, could read into a new data frame to make it easier to refer to
## e.g. results <- results$data...
head(results$data)

## how many points are there in each cluster?
table(results$data$cluster)

## plot clusters 3 and 4 as a timeVariation plot using SAME colours as in
## cluster plot
timeVariation(subset(results$data, cluster %in% c("3", "4")),
  pollutant = "nox",
  group = "cluster", col = openColours("Paired", 6)[c(3, 4)]
)
} # }
```
