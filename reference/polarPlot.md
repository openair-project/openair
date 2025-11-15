# Function for plotting bivariate polar plots with smoothing.

Function for plotting pollutant concentration in polar coordinates
showing concentration by wind speed (or another numeric variable) and
direction. Mean concentrations are calculated for wind speed-direction
‘bins’ (e.g. 0-1, 1-2 m/s,... and 0-10, 10-20 degrees etc.). To aid
interpretation, `gam` smoothing is carried out using `mgcv`.

## Usage

``` r
polarPlot(
  mydata,
  pollutant = "nox",
  x = "ws",
  wd = "wd",
  type = "default",
  statistic = "mean",
  limits = NULL,
  exclude.missing = TRUE,
  uncertainty = FALSE,
  percentile = NA,
  cols = "default",
  weights = c(0.25, 0.5, 0.75),
  min.bin = 1,
  mis.col = "grey",
  upper = NA,
  angle.scale = 315,
  units = x,
  force.positive = TRUE,
  k = 100,
  normalise = FALSE,
  key.header = statistic,
  key.footer = pollutant,
  key.position = "right",
  key = TRUE,
  auto.text = TRUE,
  ws_spread = 1.5,
  wd_spread = 5,
  x_error = NA,
  y_error = NA,
  kernel = "gaussian",
  formula.label = TRUE,
  tau = 0.5,
  alpha = 1,
  plot = TRUE,
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
  frame should be supplied e.g. `pollutant = "nox"`. There can also be
  more than one pollutant specified e.g. `pollutant = c("nox", "no2")`.
  The main use of using two or more pollutants is for model evaluation
  where two species would be expected to have similar concentrations.
  This saves the user stacking the data and it is possible to work with
  columns of data directly. A typical use would be
  `pollutant = c("obs", "mod")` to compare two columns “obs” (the
  observations) and “mod” (modelled values). When pair-wise statistics
  such as Pearson correlation and regression techniques are to be
  plotted, `pollutant` takes two elements too. For example,
  `pollutant = c("bc", "pm25")` where `"bc"` is a function of `"pm25"`.

- x:

  Name of variable to plot against wind direction in polar coordinates,
  the default is wind speed, “ws”.

- wd:

  Name of wind direction field.

- type:

  `type` determines how the data are split i.e. conditioned, and then
  plotted. The default is will produce a single plot using the entire
  data. Type can be one of the built-in types as detailed in `cutData`
  e.g. “season”, “year”, “weekday” and so on. For example,
  `type = "season"` will produce four plots — one for each season.

  It is also possible to choose `type` as another variable in the data
  frame. If that variable is numeric, then the data will be split into
  four quantiles (if possible) and labelled accordingly. If type is an
  existing character or factor variable, then those categories/levels
  will be used directly. This offers great flexibility for understanding
  the variation of different variables and how they depend on one
  another.

  Type can be up length two e.g. `type = c("season", "weekday")` will
  produce a 2x2 plot split by season and day of the week. Note, when two
  types are provided the first forms the columns and the second the
  rows.

- statistic:

  The statistic that should be applied to each wind speed/direction bin.
  Because of the smoothing involved, the colour scale for some of these
  statistics is only to provide an indication of overall pattern and
  should not be interpreted in concentration units e.g. for
  `statistic = "weighted.mean"` where the bin mean is multiplied by the
  bin frequency and divided by the total frequency. In many cases using
  `polarFreq` will be better. Setting `statistic = "weighted.mean"` can
  be useful because it provides an indication of the concentration \*
  frequency of occurrence and will highlight the wind speed/direction
  conditions that dominate the overall mean.Can be:

  - “mean” (default), “median”, “max” (maximum), “frequency”. “stdev”
    (standard deviation), “weighted.mean”.

  - `statistic = "nwr"` Implements the Non-parametric Wind Regression
    approach of Henry et al. (2009) that uses kernel smoothers. The
    `openair` implementation is not identical because Gaussian kernels
    are used for both wind direction and speed. The smoothing is
    controlled by `ws_spread` and `wd_spread`.

  - `statistic = "cpf"` the conditional probability function (CPF) is
    plotted and a single (usually high) percentile level is supplied.
    The CPF is defined as CPF = my/ny, where my is the number of samples
    in the y bin (by default a wind direction, wind speed interval) with
    mixing ratios greater than the *overall* percentile concentration,
    and ny is the total number of samples in the same wind sector (see
    Ashbaugh et al., 1985). Note that percentile intervals can also be
    considered; see `percentile` for details.

  - When `statistic = "r"` or `statistic = "Pearson"`, the Pearson
    correlation coefficient is calculated for *two* pollutants. The
    calculation involves a weighted Pearson correlation coefficient,
    which is weighted by Gaussian kernels for wind direction an the
    radial variable (by default wind speed). More weight is assigned to
    values close to a wind speed-direction interval. Kernel weighting is
    used to ensure that all data are used rather than relying on the
    potentially small number of values in a wind speed-direction
    interval.

  - When `statistic = "Spearman"`, the Spearman correlation coefficient
    is calculated for *two* pollutants. The calculation involves a
    weighted Spearman correlation coefficient, which is weighted by
    Gaussian kernels for wind direction an the radial variable (by
    default wind speed). More weight is assigned to values close to a
    wind speed-direction interval. Kernel weighting is used to ensure
    that all data are used rather than relying on the potentially small
    number of values in a wind speed-direction interval.

  - `"robust_slope"` is another option for pair-wise statistics and
    `"quantile.slope"`, which uses quantile regression to estimate the
    slope for a particular quantile level (see also `tau` for setting
    the quantile level).

  - `"york_slope"` is another option for pair-wise statistics which uses
    the *York regression method* to estimate the slope. In this method
    the uncertainties in `x` and `y` are used in the determination of
    the slope. The uncertainties are provided by `x_error` and `y_error`
    — see below.

- limits:

  The function does its best to choose sensible limits automatically.
  However, there are circumstances when the user will wish to set
  different ones. An example would be a series of plots showing each
  year of data separately. The limits are set in the form
  `c(lower, upper)`, so `limits = c(0, 100)` would force the plot limits
  to span 0-100.

- exclude.missing:

  Setting this option to `TRUE` (the default) removes points from the
  plot that are too far from the original data. The smoothing routines
  will produce predictions at points where no data exist i.e. they
  predict. By removing the points too far from the original data
  produces a plot where it is clear where the original data lie. If set
  to `FALSE` missing data will be interpolated.

- uncertainty:

  Should the uncertainty in the calculated surface be shown? If `TRUE`
  three plots are produced on the same scale showing the predicted
  surface together with the estimated lower and upper uncertainties at
  the 95% confidence interval. Calculating the uncertainties is useful
  to understand whether features are real or not. For example, at high
  wind speeds where there are few data there is greater uncertainty over
  the predicted values. The uncertainties are calculated using the GAM
  and weighting is done by the frequency of measurements in each wind
  speed-direction bin. Note that if uncertainties are calculated then
  the type is set to "default".

- percentile:

  If `statistic = "percentile"` then `percentile` is used, expressed
  from 0 to 100. Note that the percentile value is calculated in the
  wind speed, wind direction ‘bins’. For this reason it can also be
  useful to set `min.bin` to ensure there are a sufficient number of
  points available to estimate a percentile. See `quantile` for more
  details of how percentiles are calculated.

  `percentile` is also used for the Conditional Probability Function
  (CPF) plots. `percentile` can be of length two, in which case the
  percentile *interval* is considered for use with CPF. For example,
  `percentile = c(90, 100)` will plot the CPF for concentrations between
  the 90 and 100th percentiles. Percentile intervals can be useful for
  identifying specific sources. In addition, `percentile` can also be of
  length 3. The third value is the ‘trim’ value to be applied. When
  calculating percentile intervals many can cover very low values where
  there is no useful information. The trim value ensures that values
  greater than or equal to the trim \* mean value are considered
  *before* the percentile intervals are calculated. The effect is to
  extract more detail from many source signatures. See the manual for
  examples. Finally, if the trim value is less than zero the percentile
  range is interpreted as absolute concentration values and subsetting
  is carried out directly.

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

- weights:

  At the edges of the plot there may only be a few data points in each
  wind speed-direction interval, which could in some situations distort
  the plot if the concentrations are high. `weights` applies a weighting
  to reduce their influence. For example and by default if only a single
  data point exists then the weighting factor is 0.25 and for two points
  0.5. To not apply any weighting and use the data as is, use
  `weights = c(1, 1, 1)`.

  An alternative to down-weighting these points they can be removed
  altogether using `min.bin`.

- min.bin:

  The minimum number of points allowed in a wind speed/wind direction
  bin. The default is 1. A value of two requires at least 2 valid
  records in each bin an so on; bins with less than 2 valid records are
  set to NA. Care should be taken when using a value \> 1 because of the
  risk of removing real data points. It is recommended to consider your
  data with care. Also, the `polarFreq` function can be of use in such
  circumstances.

- mis.col:

  When `min.bin` is \> 1 it can be useful to show where data are removed
  on the plots. This is done by shading the missing data in `mis.col`.
  To not highlight missing data when `min.bin` \> 1 choose
  `mis.col = "transparent"`.

- upper:

  This sets the upper limit wind speed to be used. Often there are only
  a relatively few data points at very high wind speeds and plotting all
  of them can reduce the useful information in the plot.

- angle.scale:

  Sometimes the placement of the scale may interfere with an interesting
  feature. The user can therefore set `angle.scale` to any value between
  0 and 360 degrees to mitigate such problems. For example
  `angle.scale = 45` will draw the scale heading in a NE direction.

- units:

  The units shown on the polar axis scale.

- force.positive:

  The default is `TRUE`. Sometimes if smoothing data with steep
  gradients it is possible for predicted values to be negative.
  `force.positive = TRUE` ensures that predictions remain positive. This
  is useful for several reasons. First, with lots of missing data more
  interpolation is needed and this can result in artefacts because the
  predictions are too far from the original data. Second, if it is known
  beforehand that the data are all positive, then this option carries
  that assumption through to the prediction. The only likely time where
  setting `force.positive = FALSE` would be if background concentrations
  were first subtracted resulting in data that is legitimately negative.
  For the vast majority of situations it is expected that the user will
  not need to alter the default option.

- k:

  This is the smoothing parameter used by the `gam` function in package
  `mgcv`. Typically, value of around 100 (the default) seems to be
  suitable and will resolve important features in the plot. The most
  appropriate choice of `k` is problem-dependent; but extensive testing
  of polar plots for many different problems suggests a value of `k` of
  about 100 is suitable. Setting `k` to higher values will not tend to
  affect the surface predictions by much but will add to the computation
  time. Lower values of `k` will increase smoothing. Sometimes with few
  data to plot `polarPlot` will fail. Under these circumstances it can
  be worth lowering the value of `k`.

- normalise:

  If `TRUE` concentrations are normalised by dividing by their mean
  value. This is done *after* fitting the smooth surface. This option is
  particularly useful if one is interested in the patterns of
  concentrations for several pollutants on different scales e.g. NOx and
  CO. Often useful if more than one `pollutant` is chosen.

- key.header:

  Adds additional text/labels to the scale key. For example, passing the
  options `key.header = "header", key.footer = "footer1"` adds addition
  text above and below the scale key. These arguments are passed to
  `drawOpenKey` via `quickText`, applying the `auto.text` argument, to
  handle formatting.

- key.footer:

  see `key.footer`.

- key.position:

  Location where the scale key is to plotted. Allowed arguments
  currently include `"top"`, `"right"`, `"bottom"` and `"left"`.

- key:

  Fine control of the scale key via `drawOpenKey`. See `drawOpenKey` for
  further details.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly
  e.g. by subscripting the \`2' in NO2.

- ws_spread:

  The value of sigma used for Gaussian kernel weighting of wind speed
  when `statistic = "nwr"` or when correlation and regression statistics
  are used such as *r*. Default is `0.5`.

- wd_spread:

  The value of sigma used for Gaussian kernel weighting of wind
  direction when `statistic = "nwr"` or when correlation and regression
  statistics are used such as *r*. Default is `4`.

- x_error:

  The `x` error / uncertainty used when `statistic = "york_slope"`.

- y_error:

  The `y` error / uncertainty used when `statistic = "york_slope"`.

- kernel:

  Type of kernel used for the weighting procedure for when correlation
  or regression techniques are used. Only `"gaussian"` is supported but
  this may be enhanced in the future.

- formula.label:

  When pair-wise statistics such as regression slopes are calculated and
  plotted, should a formula label be displayed? `formula.label` will
  also determine whether concentration information is printed when
  `statistic = "cpf"`.

- tau:

  The quantile to be estimated when `statistic` is set to
  `"quantile.slope"`. Default is `0.5` which is equal to the median and
  will be ignored if `"quantile.slope"` is not used.

- alpha:

  The alpha transparency to use for the plotting surface (a value
  between 0 and 1 with zero being fully transparent and 1 fully opaque).
  Setting a value below 1 can be useful when plotting surfaces on a map
  using the package `openairmaps`.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  Other graphical parameters passed onto `lattice:levelplot` and
  `cutData`. For example, `polarPlot` passes the option
  `hemisphere = "southern"` on to `cutData` to provide southern (rather
  than default northern) hemisphere handling of `type = "season"`.
  Similarly, common axis and title labelling options (such as `xlab`,
  `ylab`, `main`) are passed to `levelplot` via `quickText` to handle
  routine formatting.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object. `data` contains four set columns: `cond`, conditioning based on
`type`; `u` and `v`, the translational vectors based on `ws` and `wd`;
and the local `pollutant` estimate.

## Details

The bivariate polar plot is a useful diagnostic tool for quickly gaining
an idea of potential sources. Wind speed is one of the most useful
variables to use to separate source types (see references). For example,
ground-level concentrations resulting from buoyant plumes from chimney
stacks tend to peak under higher wind speed conditions. Conversely,
ground-level, non-buoyant plumes such as from road traffic, tend to have
highest concentrations under low wind speed conditions. Other sources
such as from aircraft engines also show differing characteristics by
wind speed.

The function has been developed to allow variables other than wind speed
to be plotted with wind direction in polar coordinates. The key issue is
that the other variable plotted against wind direction should be
discriminating in some way. For example, temperature can help reveal
high-level sources brought down to ground level in unstable atmospheric
conditions, or show the effect a source emission dependent on
temperature e.g. biogenic isoprene.

The plots can vary considerably depending on how much smoothing is done.
The approach adopted here is based on the very flexible and capable
`mgcv` package that uses *Generalized Additive Models*. While methods do
exist to find an optimum level of smoothness, they are not necessarily
useful. The principal aim of `polarPlot` is as a graphical analysis
rather than for quantitative purposes. In this respect the smoothing
aims to strike a balance between revealing interesting (real) features
and overly noisy data. The defaults used in `polarPlot()` are based on
the analysis of data from many different sources. More advanced users
may wish to modify the code and adopt other smoothing approaches.

Various statistics are possible to consider e.g. mean, maximum, median.
`statistic = "max"` is often useful for revealing sources. Pair-wise
statistics between two pollutants can also be calculated.

The function can also be used to compare two pollutant species through a
range of pair-wise statistics (see help on `statistic`) and Grange et
al. (2016) (open-access publication link below).

Wind direction is split up into 10 degree intervals and the other
variable (e.g. wind speed) 30 intervals. These 2D bins are then used to
calculate the statistics.

These plots often show interesting features at higher wind speeds (see
references below). For these conditions there can be very few
measurements and therefore greater uncertainty in the calculation of the
surface. There are several ways in which this issue can be tackled.
First, it is possible to avoid smoothing altogether and use
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md).
Second, the effect of setting a minimum number of measurements in each
wind speed-direction bin can be examined through `min.bin`. It is
possible that a single point at high wind speed conditions can strongly
affect the surface prediction. Therefore, setting `min.bin = 3`, for
example, will remove all wind speed-direction bins with fewer than 3
measurements *before* fitting the surface. Third, consider setting
`uncertainty = TRUE`. This option will show the predicted surface
together with upper and lower 95% confidence intervals, which take
account of the frequency of measurements.

Variants on `polarPlot` include
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md)
and
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md).

## References

Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A residence time
probability analysis of sulfur concentrations at ground canyon national
park. Atmospheric Environment 19 (8), 1263-1270.

Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006). Detecting
and quantifying aircraft and other on-airport contributions to ambient
nitrogen oxides in the vicinity of a large international airport.
Atmospheric Environment. 40/28 pp 5424-5434.

Carslaw, D.C., & Beevers, S.D. (2013). Characterising and understanding
emission sources using bivariate polar plots and k-means clustering.
Environmental Modelling & Software, 40, 325-329. DOI:
10.1016/j.envsoft.2012.09.005.

Henry, R.C., Chang, Y.S., Spiegelman, C.H., 2002. Locating nearby
sources of air pollution by nonparametric regression of atmospheric
concentrations on wind direction. Atmospheric Environment 36 (13),
2237-2244.

Henry, R., Norris, G.A., Vedantham, R., Turner, J.R., 2009. Source
region identification using Kernel smoothing. Environ. Sci. Technol. 43
(11), 4090e4097. DOI: 10.1021/es8011723.

Uria-Tellaetxe, I. and D.C. Carslaw (2014). Source identification using
a conditional bivariate Probability function. Environmental Modelling &
Software, Vol. 59, 1-9.

Westmoreland, E.J., N. Carslaw, D.C. Carslaw, A. Gillah and E. Bates
(2007). Analysis of air quality within a street canyon using statistical
and dispersion modelling techniques. Atmospheric Environment. Vol.
41(39), pp. 9195-9205.

Yu, K.N., Cheung, Y.P., Cheung, T., Henry, R.C., 2004. Identifying the
impact of large urban airports on local air quality by nonparametric
regression. Atmospheric Environment 38 (27), 4501-4507.

Grange, S. K., Carslaw, D. C., & Lewis, A. C. 2016. Source apportionment
advances with bivariate polar plots, correlation, and regression
techniques. Atmospheric Environment. 145, 128-134. DOI:
10.1016/j.atmosenv.2016.09.016.

## See also

Other polar directional analysis functions:
[`percentileRose()`](https://openair-project.github.io/openair/reference/percentileRose.md),
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md),
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`polarDiff()`](https://openair-project.github.io/openair/reference/polarDiff.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md),
[`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md),
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)

## Author

David Carslaw

## Examples

``` r
# basic plot
polarPlot(mydata, pollutant = "nox")

if (FALSE) { # \dontrun{

# polarPlots by year on same scale
polarPlot(mydata, pollutant = "so2", type = "year", main = "polarPlot of so2")

# set minimum number of bins to be used to see if pattern remains similar
polarPlot(mydata, pollutant = "nox", min.bin = 3)

# plot by day of the week
polarPlot(mydata, pollutant = "pm10", type = "weekday")

# show the 95% confidence intervals in the surface fitting
polarPlot(mydata, pollutant = "so2", uncertainty = TRUE)


# Pair-wise statistics
# Pearson correlation
polarPlot(mydata, pollutant = c("pm25", "pm10"), statistic = "r")

# Robust regression slope, takes a bit of time
polarPlot(mydata, pollutant = c("pm25", "pm10"), statistic = "robust.slope")

# Least squares regression works too but it is not recommended, use robust
# regression
# polarPlot(mydata, pollutant = c("pm25", "pm10"), statistic = "slope")
} # }
```
