# Calculate nonparametric smooth trends

Use non-parametric methods to calculate time series trends

## Usage

``` r
smoothTrend(
  mydata,
  pollutant = "nox",
  avg.time = "month",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  k = NULL,
  deseason = FALSE,
  simulate = FALSE,
  n = 200,
  autocor = FALSE,
  type = "default",
  cols = "brewer1",
  x.relation = "same",
  y.relation = "same",
  ref.x = NULL,
  ref.y = NULL,
  key = TRUE,
  key.columns = 1,
  key.position = "bottom",
  name.pol = pollutant,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  ci = TRUE,
  alpha = 0.2,
  shade = "grey95",
  plot = TRUE,
  progress = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame of time series. Must include a `date` field and at least
  one variable to plot.

- pollutant:

  Name of variable to plot. Two or more pollutants can be plotted, in
  which case a form like `pollutant = c("nox", "co")` should be used.

- avg.time:

  This defines the time period to average to. Can be `"sec"`, `"min"`,
  `"hour"`, `"day"`, `"DSTday"`, `"week"`, `"month"`, `"quarter"` or
  `"year"`. For much increased flexibility a number can precede these
  options followed by a space. For example, an average of 2 months would
  be `avg.time = "2 month"`. In addition, `avg.time` can equal
  `"season"`, in which case 3-month seasonal values are calculated with
  spring defined as March, April, May and so on.

  Note that `avg.time` can be *less* than the time interval of the
  original series, in which case the series is expanded to the new time
  interval. This is useful, for example, for calculating a 15-minute
  time series from an hourly one where an hourly value is repeated for
  each new 15-minute period. Note that when expanding data in this way
  it is necessary to ensure that the time interval of the original
  series is an exact multiple of `avg.time` e.g. hour to 10 minutes, day
  to hour. Also, the input time series must have consistent time gaps
  between successive intervals so that
  [`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md)
  can work out how much 'padding' to apply. To pad-out data in this way
  choose `fill = TRUE`.

- data.thresh:

  The data capture threshold to use (%). A value of zero means that all
  available data will be used in a particular period regardless if of
  the number of values available. Conversely, a value of 100 will mean
  that all data will need to be present for the average to be
  calculated, else it is recorded as `NA`. See also `interval`,
  `start.date` and `end.date` to see whether it is advisable to set
  these other options.

- statistic:

  Statistic used for calculating monthly values. Default is `"mean"`,
  but can also be `"percentile"`. See
  [`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md)
  for more details.

- percentile:

  Percentile value(s) to use if `statistic = "percentile"` is chosen.
  Can be a vector of numbers e.g. `percentile = c(5, 50, 95)` will plot
  the 5th, 50th and 95th percentile values together on the same plot.

- k:

  This is the smoothing parameter used by the
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) function in
  package `mgcv`. By default it is not used and the amount of smoothing
  is optimised automatically. However, sometimes it is useful to set the
  smoothing amount manually using `k`.

- deseason:

  Should the data be de-deasonalized first? If `TRUE` the function `stl`
  is used (seasonal trend decomposition using loess). Note that if
  `TRUE` missing data are first imputed using a Kalman filter and Kalman
  smooth.

- simulate:

  Should simulations be carried out to determine the Mann-Kendall tau
  and p-value. The default is `FALSE`. If `TRUE`, bootstrap simulations
  are undertaken, which also account for autocorrelation.

- n:

  Number of bootstrap simulations if `simulate = TRUE`.

- autocor:

  Should autocorrelation be considered in the trend uncertainty
  estimates? The default is `FALSE`. Generally, accounting for
  autocorrelation increases the uncertainty of the trend estimate
  sometimes by a large amount.

- type:

  `type` determines how the data are split i.e. conditioned, and then
  plotted. The default is will produce a single plot using the entire
  data. Type can be one of the built-in types as detailed in
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md),
  e.g., `"season"`, `"year"`, `"weekday"` and so on. For example,
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

- cols:

  Colours to be used for plotting; see
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for details.

- x.relation, y.relation:

  This determines how the x- and y-axis scales are plotted. `"same"`
  ensures all panels use the same scale and `"free"` will use
  panel-specific scales. The latter is a useful setting when plotting
  data with very different values.

- ref.x:

  See `ref.y` for details. In this case the correct date format should
  be used for a vertical line e.g.
  `ref.x = list(v = as.POSIXct("2000-06-15"), lty = 5)`.

- ref.y:

  A list with details of the horizontal lines to be added representing
  reference line(s). For example, `ref.y = list(h = 50, lty = 5)` will
  add a dashed horizontal line at 50. Several lines can be plotted e.g.
  `ref.y = list(h = c(50, 100), lty = c(1, 5), col = c("green", "blue"))`.
  See `panel.abline` in the `lattice` package for more details on
  adding/controlling lines.

- key:

  Should a key be drawn? The default is `TRUE`.

- key.columns:

  Number of columns to be used in the key. With many pollutants a single
  column can make to key too wide. The user can thus choose to use
  several columns by setting `columns` to be less than the number of
  pollutants.

- key.position:

  Location where the scale key is to plotted. Can include `"top"`,
  `"bottom"`, `"right"` and `"left"`.

- name.pol:

  This option can be used to give alternative names for the variables
  plotted. Instead of taking the column headings as names, the user can
  supply replacements. For example, if a column had the name "nox" and
  the user wanted a different description, then setting
  `name.pol = "nox before change"` can be used. If more than one
  pollutant is plotted then use `c` e.g.
  `name.pol = c("nox here", "o3 there")`.

- date.breaks:

  Number of major x-axis intervals to use. The function will try and
  choose a sensible number of dates/times as well as formatting the
  date/time appropriately to the range being considered. This does not
  always work as desired automatically. The user can therefore increase
  or decrease the number of intervals by adjusting the value of
  `date.breaks` up or down.

- date.format:

  This option controls the date format on the x-axis. While
  [`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md)
  generally sets the date format sensibly there can be some situations
  where the user wishes to have more control. For format types see
  [`strptime()`](https://rdrr.io/r/base/strptime.html). For example, to
  format the date like "Jan-2012" set `date.format = "\%b-\%Y"`.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the '2' in NO2.

- ci:

  Should confidence intervals be plotted? The default is `TRUE`.

- alpha:

  The alpha transparency of shaded confidence intervals - if plotted. A
  value of 0 is fully transparent and 1 is fully opaque.

- shade:

  The colour used for marking alternate years. Use `"white"` or
  `"transparent"` to remove shading.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- progress:

  Show a progress bar when many groups make up `type`? Defaults to
  `TRUE`.

- ...:

  Other graphical parameters are passed onto
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  and
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).
  For example, `smoothTrend()` passes the option
  `hemisphere = "southern"` on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  to provide southern (rather than default northern) hemisphere handling
  of `type = "season"`. Similarly, common graphical arguments, such as
  `xlim` and `ylim` for plotting ranges and `pch` and `cex` for plot
  symbol type and size, are passed on
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html),
  although some local modifications may be applied by openair. For
  example, axis and title labelling options (such as `xlab`, `ylab` and
  `main`) are passed to
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html) via
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  to handle routine formatting. One special case here is that many
  graphical parameters can be vectors when used with
  `statistic = "percentile"` and a vector of `percentile` values, see
  examples below.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Details

The `smoothTrend()` function provides a flexible way of estimating the
trend in the concentration of a pollutant or other variable. Monthly
mean values are calculated from an hourly (or higher resolution) or
daily time series. There is the option to deseasonalise the data if
there is evidence of a seasonal cycle.

`smoothTrend()` uses a Generalized Additive Model (GAM) from the
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) package to find
the most appropriate level of smoothing. The function is particularly
suited to situations where trends are not monotonic (see discussion with
[`TheilSen()`](https://openair-project.github.io/openair/reference/TheilSen.md)
for more details on this). The `smoothTrend()` function is particularly
useful as an exploratory technique e.g. to check how linear or
non-linear trends are.

95% confidence intervals are shown by shading. Bootstrap estimates of
the confidence intervals are also available through the `simulate`
option. Residual resampling is used.

Trends can be considered in a very wide range of ways, controlled by
setting `type` - see examples below.

## See also

Other time series and trend functions:
[`TheilSen()`](https://openair-project.github.io/openair/reference/TheilSen.md),
[`calendarPlot()`](https://openair-project.github.io/openair/reference/calendarPlot.md),
[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md),
[`timeProp()`](https://openair-project.github.io/openair/reference/timeProp.md),
[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md),
[`trendLevel()`](https://openair-project.github.io/openair/reference/trendLevel.md)

## Author

David Carslaw

## Examples

``` r
# trend plot for nox
smoothTrend(mydata, pollutant = "nox")


# trend plot by each of 8 wind sectors
if (FALSE) { # \dontrun{
smoothTrend(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")

# several pollutants, no plotting symbol
smoothTrend(mydata, pollutant = c("no2", "o3", "pm10", "pm25"), pch = NA)

# percentiles
smoothTrend(mydata,
  pollutant = "o3", statistic = "percentile",
  percentile = 95
)

# several percentiles with control over lines used
smoothTrend(mydata,
  pollutant = "o3", statistic = "percentile",
  percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5)
)
} # }
```
