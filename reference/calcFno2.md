# Estimate NO2/NOX emission ratios from monitoring data

Given hourly NOX and NO2 from a roadside site and hourly NOX, NO2 and O3
from a background site the function will estimate the emissions ratio of
NO2/NOX — the level of primary NO2

## Usage

``` r
calcFno2(input, tau = 60, user.fno2, main = "", xlab = "year", ...)
```

## Arguments

- input:

  A data frame with the following fields. `nox` and`no2` (roadside NOX
  and NO2 concentrations), `back_nox`, `back_no2` and `back_o3` (hourly
  background concentrations of each pollutant). In addition `temp`
  (temperature in degrees Celsius) and `cl` (cloud cover in Oktas). Note
  that if `temp` and `cl` are not available, typical means values of 11
  deg. C and cloud = 3.5 will be used.

- tau:

  Mixing time scale. It is unlikely the user will need to adjust this.
  See details below.

- user.fno2:

  User-supplied f-NO2 fraction e.g. 0.1 is a NO2/NOX ratio of 10% by
  volume. `user.no2` will be applied to the whole time series and is
  useful for testing "what if" questions.

- main:

  Title of plot if required.

- xlab:

  x-axis label.

- ...:

  Arguments passed on to
  [`scatterPlot`](https://openair-project.github.io/openair/reference/scatterPlot.md)

  `mydata`

  :   A data frame containing at least two numeric variables to plot.

  `x`

  :   Name of the x-variable to plot. Note that x can be a date field or
      a factor. For example, `x` can be one of the `openair` built in
      types such as `"year"` or `"season"`.

  `y`

  :   Name of the numeric y-variable to plot.

  `z`

  :   Name of the numeric z-variable to plot for `method = "scatter"` or
      `method = "level"`. Note that for `method = "scatter"` points will
      be coloured according to a continuous colour scale, whereas for
      `method = "level"` the surface is coloured.

  `method`

  :   Methods include “scatter” (conventional scatter plot), “hexbin”
      (hexagonal binning using the `hexbin` package). “level” for a
      binned or smooth surface plot and “density” (2D kernel density
      estimates).

  `group`

  :   The grouping variable to use, if any. Setting this to a variable
      in the data frame has the effect of plotting several series in the
      same panel using different symbols/colours etc. If set to a
      variable that is a character or factor, those categories or factor
      levels will be used directly. If set to a numeric variable, it
      will split that variable in to quantiles.

  `avg.time`

  :   This defines the time period to average to. Can be “sec”, “min”,
      “hour”, “day”, “DSTday”, “week”, “month”, “quarter” or “year”. For
      much increased flexibility a number can precede these options
      followed by a space. For example, a timeAverage of 2 months would
      be `period = "2 month"`. See function `timeAverage` for further
      details on this. This option se useful as one method by which the
      number of points plotted is reduced i.e. by choosing a longer
      averaging time.

  `data.thresh`

  :   The data capture threshold to use (\\ the data using `avg.time`. A
      value of zero means that all available data will be used in a
      particular period regardless if of the number of values available.
      Conversely, a value of 100 will mean that all data will need to be
      present for the average to be calculated, else it is recorded as
      `NA`. Not used if `avg.time = "default"`.

  `statistic`

  :   The statistic to apply when aggregating the data; default is the
      mean. Can be one of "mean", "max", "min", "median", "frequency",
      "sd", "percentile". Note that "sd" is the standard deviation and
      "frequency" is the number (frequency) of valid records in the
      period. "percentile" is the percentile level (\\ "percentile"
      option - see below. Not used if `avg.time = "default"`.

  `percentile`

  :   The percentile level in percent used when
      `statistic = "percentile"` and when aggregating the data with
      `avg.time`. The default is 95. Not used if `avg.time = "default"`.

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

  `smooth`

  :   A smooth line is fitted to the data if `TRUE`; optionally with 95
      percent confidence intervals shown. For `method = "level"` a
      smooth surface will be fitted to binned data.

  `spline`

  :   A smooth spline is fitted to the data if `TRUE`. This is
      particularly useful when there are fewer data points or when a
      connection line between a sequence of points is required.

  `linear`

  :   A linear model is fitted to the data if `TRUE`; optionally with 95
      percent confidence intervals shown. The equation of the line and
      R2 value is also shown.

  `ci`

  :   Should the confidence intervals for the smooth/linear fit be
      shown?

  `mod.line`

  :   If `TRUE` three lines are added to the scatter plot to help inform
      model evaluation. The 1:1 line is solid and the 1:0.5 and 1:2
      lines are dashed. Together these lines help show how close a group
      of points are to a 1:1 relationship and also show the points that
      are within a factor of two (FAC2). `mod.line` is appropriately
      transformed when x or y axes are on a log scale.

  `cols`

  :   Colours to be used for plotting. Options include “default”,
      “increment”, “heat”, “jet” and `RColorBrewer` colours — see the
      `openair` `openColours` function for more details. For user
      defined the user can supply a list of colour names recognised by R
      (type [`colours()`](https://rdrr.io/r/grDevices/colors.html) to
      see the full list). An example would be
      `cols = c("yellow", "green", "blue")`

  `plot.type`

  :   `lattice` plot type. Can be “p” (points — default), “l” (lines) or
      “b” (lines and points).

  `key`

  :   Should a key be drawn? The default is `TRUE`.

  `key.title`

  :   The title of the key (if used).

  `key.columns`

  :   Number of columns to be used in the key. With many pollutants a
      single column can make to key too wide. The user can thus choose
      to use several columns by setting `columns` to be less than the
      number of pollutants.

  `key.position`

  :   Location where the scale key is to plotted. Allowed arguments
      currently include “top”, “right”, “bottom” and “left”.

  `strip`

  :   Should a strip be drawn? The default is `TRUE`.

  `log.x`

  :   Should the x-axis appear on a log scale? The default is `FALSE`.
      If `TRUE` a well-formatted log10 scale is used. This can be useful
      for checking linearity once logged.

  `log.y`

  :   Should the y-axis appear on a log scale? The default is `FALSE`.
      If `TRUE` a well-formatted log10 scale is used. This can be useful
      for checking linearity once logged.

  `x.inc`

  :   The x-interval to be used for binning data when
      `method = "level"`.

  `y.inc`

  :   The y-interval to be used for binning data when
      `method = "level"`.

  `limits`

  :   For `method = "level"` the function does its best to choose
      sensible limits automatically. However, there are circumstances
      when the user will wish to set different ones. The limits are set
      in the form `c(lower, upper)`, so `limits = c(0, 100)` would force
      the plot limits to span 0-100.

  `windflow`

  :   This option allows a scatter plot to show the wind speed/direction
      shows as an arrow. The option is a list e.g.
      `windflow = list(col = "grey", lwd = 2, scale = 0.1)`. This option
      requires wind speed (`ws`) and wind direction (`wd`) to be
      available.

      The maximum length of the arrow plotted is a fraction of the plot
      dimension with the longest arrow being `scale` of the plot x-y
      dimension. Note, if the plot size is adjusted manually by the user
      it should be re-plotted to ensure the correct wind angle. The list
      may contain other options to `panel.arrows` in the `lattice`
      package. Other useful options include `length`, which controls the
      length of the arrow head and `angle`, which controls the angle of
      the arrow head.

      This option works best where there are not too many data to ensure
      over-plotting does not become a problem.

  `y.relation`

  :   This determines how the y-axis scale is plotted. “same” ensures
      all panels use the same scale and “free” will use panel-specific
      scales. The latter is a useful setting when plotting data with
      very different values.

  `x.relation`

  :   This determines how the x-axis scale is plotted. “same” ensures
      all panels use the same scale and “free” will use panel-specific
      scales. The latter is a useful setting when plotting data with
      very different values.

  `ref.x`

  :   See `ref.y` for details.

  `ref.y`

  :   A list with details of the horizontal lines to be added
      representing reference line(s). For example,
      `ref.y = list(h = 50, lty = 5)` will add a dashed horizontal line
      at 50. Several lines can be plotted e.g.
      `ref.y = list(h = c(50, 100), lty = c(1, 5), col = c("green", "blue"))`.
      See `panel.abline` in the `lattice` package for more details on
      adding/controlling lines.

  `k`

  :   Smoothing parameter supplied to `gam` for fitting a smooth surface
      when `method = "level"`.

  `dist`

  :   When plotting smooth surfaces (`method = "level"` and
      `smooth = TRUE`, `dist` controls how far from the original data
      the predictions should be made. See `exclude.too.far` from the
      `mgcv` package. Data are first transformed to a unit square.
      Values should be between 0 and 1.

  `auto.text`

  :   Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis
      labels will automatically try and format pollutant names and units
      properly e.g. by subscripting the ‘2’ in NO2.

  `plot`

  :   Should a plot be produced? `FALSE` can be useful when analysing
      data to extract plot components and plotting them in other ways.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Details

The principal purpose of this function is to estimate the level of
primary (or direct) NO2 from road vehicles. When hourly data of NOX, NO2
and O3 are available, the total oxidant method of Clapp and Jenkin
(2001) can be used. If roadside O3 measurements are available see
[`linearRelation()`](https://openair-project.github.io/openair/reference/linearRelation.md)
for details of how to estimate the primary NO2 fraction.

In the absence of roadside O3 measurements, it is rather more
problematic to calculate the fraction of primary NO2. Carslaw and
Beevers (2005c) developed an approach based on
[`linearRelation()`](https://openair-project.github.io/openair/reference/linearRelation.md)
the analysis of roadside and background measurements. The increment in
roadside NO2 concentrations is primarily determined by direct emissions
of NO2 and the availability of One to react with NO to form NO2. The
method aims to quantify the amount of NO2 formed through these two
processes by seeking the optimum level of primary NO2 that gives the
least error.

## References

Clapp, L.J., Jenkin, M.E., 2001. Analysis of the relationship between
ambient levels of O3, NO2 and NO as a function of NOX in the UK.
Atmospheric Environment 35 (36), 6391-6405.

Carslaw, D.C. and N Carslaw (2007). Detecting and characterising small
changes in urban nitrogen dioxide concentrations. Atmospheric
Environment. Vol. 41, 4723-4733.

Carslaw, D.C., Beevers, S.D. and M.C. Bell (2007). Risks of exceeding
the hourly EU limit value for nitrogen dioxide resulting from increased
road transport emissions of primary nitrogen dioxide. Atmospheric
Environment 41 2073-2082.

Carslaw, D.C. (2005a). Evidence of an increasing NO2/NOX emissions ratio
from road traffic emissions. Atmospheric Environment, 39(26) 4793-4802.

Carslaw, D.C. and Beevers, S.D. (2005b). Development of an urban
inventory for road transport emissions of NO2 and comparison with
estimates derived from ambient measurements. Atmospheric Environment,
(39): 2049-2059.

Carslaw, D.C. and Beevers, S.D. (2005c). Estimations of road vehicle
primary NO2 exhaust emission fractions using monitoring data in London.
Atmospheric Environment, 39(1): 167-177.

Carslaw, D. C. and S. D. Beevers (2004). Investigating the Potential
Importance of Primary NO2 Emissions in a Street Canyon. Atmospheric
Environment 38(22): 3585-3594.

Carslaw, D. C. and S. D. Beevers (2004). New Directions: Should road
vehicle emissions legislation consider primary NO2? Atmospheric
Environment 38(8): 1233-1234.

## See also

[`linearRelation()`](https://openair-project.github.io/openair/reference/linearRelation.md)
if you have roadside ozone measurements.

## Author

David Carslaw
