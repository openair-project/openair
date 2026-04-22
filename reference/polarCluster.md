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

  Arguments passed on to
  [`polarPlot`](https://openair-project.github.io/openair/reference/polarPlot.md)

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

  `col.na`

  :   When `min.bin` is \> 1 it can be useful to show where data are
      removed on the plots. This is done by shading the missing data in
      `col.na`. To not highlight missing data when `min.bin` \> 1 choose
      `col.na = "transparent"`.

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

  `type`

  :   Character string(s) defining how data should be split/conditioned
      before plotting. `"default"` produces a single panel using the
      entire dataset. Any other options will split the plot into
      different panels - a roughly square grid of panels if one `type`
      is given, or a 2D matrix of panels if two `types` are given.
      `type` is always passed to
      [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md),
      and can therefore be any of:

      - A built-in type defined in
        [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
        (e.g., `"season"`, `"year"`, `"weekday"`, etc.). For example,
        `type = "season"` will split the plot into four panels, one for
        each season.

      - The name of a numeric column in `mydata`, which will be split
        into `n.levels` quantiles (defaulting to 4).

      - The name of a character or factor column in `mydata`, which will
        be used as-is. Commonly this could be a variable like `"site"`
        to ensure data from different monitoring sites are handled and
        presented separately. It could equally be any arbitrary column
        created by the user (e.g., whether a nearby possible pollutant
        source is active or not).

      Most `openair` plotting functions can take two `type` arguments.
      If two are given, the first is used for the columns and the second
      for the rows.

  `breaks,labels`

  :   If a categorical colour scale is required, `breaks` should be
      specified. This can be either of:

      - A single value, which will divide the scale into `breaks` levels
        using the same logic as
        [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
        For example, `breaks = 5` will split the scale into five
        quantiles.

      - A numeric vector, which will define the specific breakpoints.
        For example, `c(0, 50, 100)` will bin the data into `0 to 50`,
        `50 to 100`, and so on. If `breaks` does not cover the full
        range of the data, the outer limits will be extended so that the
        full colour scale is covered while retaining the desired number
        of breaks.

      By default, `breaks` will generate nicely formatted labels for
      each category. The `labels` argument overrides this - for example,
      a user could define
      `breaks = 3, labels = c("low", "medium", "high")`. Care should be
      taken to provide the appropriate number of `labels` - it should be
      equal to `breaks` if a single value is given, or equal to
      `length(breaks)-1` if `breaks` is a vector.

  `key.position`

  :   Location where the legend is to be placed. Allowed arguments
      include `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the
      last of which removes the legend entirely.

  `key.title`

  :   Used to set the title of the legend. The legend title is passed to
      [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
      if `auto.text = TRUE`.

  `key`

  :   Deprecated; please use `key.position`. If `FALSE`, sets
      `key.position` to `"none"`.

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
