# Polar plots considering changes in concentrations between two time periods

This function provides a way of showing the differences in
concentrations between two time periods as a polar plot. There are
several uses of this function, but the most common will be to see how
source(s) may have changed between two periods.

## Usage

``` r
polarDiff(
  before,
  after,
  pollutant = "nox",
  type = "default",
  x = "ws",
  limits = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- before, after:

  Data frames representing the "before" and "after" cases. See
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)
  for details of different input requirements.

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

- x:

  Name of variable to plot against wind direction in polar coordinates,
  the default is wind speed, “ws”.

- limits:

  The function does its best to choose sensible limits automatically.
  However, there are circumstances when the user will wish to set
  different ones. An example would be a series of plots showing each
  year of data separately. The limits are set in the form
  `c(lower, upper)`, so `limits = c(0, 100)` would force the plot limits
  to span 0-100.

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

- ...:

  Arguments passed on to
  [`polarPlot`](https://openair-project.github.io/openair/reference/polarPlot.md)

  `wd`

  :   Name of wind direction field.

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

  `upper`

  :   This sets the upper limit wind speed to be used. Often there are
      only a relatively few data points at very high wind speeds and
      plotting all of them can reduce the useful information in the
      plot.

  `units`

  :   The units shown on the polar axis scale.

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

  `cols`

  :   Colours to use for plotting. Can be a pre-set palette (e.g.,
      `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a
      user-defined vector of R colours (e.g.,
      `c("yellow", "green", "blue", "black")` - see
      [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
      list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`).
      See
      [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
      for more details.

  `angle.scale`

  :   In radial plots (e.g.,
      [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)),
      the radial scale is drawn directly on the plot itself. While
      suitable defaults have been chosen, sometimes the placement of the
      scale may interfere with an interesting feature. `angle.scale` can
      take any value between `0` and `360` to place the scale at a
      different angle, or `FALSE` to move it to the side of the plots.

  `key.position`

  :   Location where the legend is to be placed. Allowed arguments
      include `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the
      last of which removes the legend entirely.

  `key.title`

  :   Used to set the title of the legend. The legend title is passed to
      [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
      if `auto.text = TRUE`.

  `strip.position`

  :   Location where the facet 'strips' are located when using `type`.
      When one `type` is provided, can be one of `"left"`, `"right"`,
      `"bottom"` or `"top"`. When two `type`s are provided, this
      argument defines whether the strips are "switched" and can take
      either `"x"`, `"y"`, or `"both"`. For example, `"x"` will switch
      the 'top' strip locations to the bottom of the plot.

  `key`

  :   Deprecated; please use `key.position`. If `FALSE`, sets
      `key.position` to `"none"`.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
plot.

## Details

While the function is primarily intended to compare two time periods at
the same location, it can be used for any two data sets that contain the
same pollutant. For example, data from two sites that are close to one
another, or two co-located instruments.

The analysis works by calculating the polar plot surface for the
`before` and `after` periods and then subtracting the `before` surface
from the `after` surface.

## See also

Other polar directional analysis functions:
[`percentileRose()`](https://openair-project.github.io/openair/reference/percentileRose.md),
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md),
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md),
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md),
[`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md),
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)

## Examples

``` r
if (FALSE) { # \dontrun{
before_data <- selectByDate(mydata, year = 2002)
after_data <- selectByDate(mydata, year = 2003)

polarDiff(before_data, after_data, pollutant = "no2")

# with some options
polarDiff(
  before_data,
  after_data,
  pollutant = "no2",
  cols = "RdYlBu",
  limits = c(-20, 20)
)
} # }
```
