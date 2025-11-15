# Pollution rose variation of the traditional wind rose plot

The traditional wind rose plot that plots wind speed and wind direction
by different intervals. The pollution rose applies the same plot
structure but substitutes other measurements, most commonly a pollutant
time series, for wind speed.

## Usage

``` r
pollutionRose(
  mydata,
  pollutant = "nox",
  key.footer = pollutant,
  key.position = "right",
  key = TRUE,
  breaks = 6,
  paddle = FALSE,
  seg = 0.9,
  normalise = FALSE,
  alpha = 1,
  plot = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame containing fields `ws` and `wd`

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`.

- key.footer:

  Adds additional text/labels below the scale key. See `key.header` for
  further information.

- key.position:

  Location where the scale key is to plotted. Allowed arguments
  currently include “top”, “right”, “bottom” and “left”.

- key:

  Fine control of the scale key via
  [`drawOpenKey()`](https://openair-project.github.io/openair/reference/drawOpenKey.md).

- breaks:

  Most commonly, the number of break points for pollutant
  concentrations. The default, 6, attempts to breaks the supplied data
  at approximately 6 sensible break points. However, `breaks` can also
  be used to set specific break points. For example, the argument
  `breaks = c(0, 1, 10, 100)` breaks the data into segments \<1, 1-10,
  10-100, \>100.

- paddle:

  Either `TRUE` or `FALSE`. If `TRUE` plots rose using 'paddle' style
  spokes. If `FALSE` plots rose using 'wedge' style spokes.

- seg:

  When `paddle = TRUE`, `seg` determines with width of the segments. For
  example, `seg = 0.5` will produce segments 0.5 \* `angle`.

- normalise:

  If `TRUE` each wind direction segment is normalised to equal one. This
  is useful for showing how the concentrations (or other parameters)
  contribute to each wind sector when the proportion of time the wind is
  from that direction is low. A line showing the probability that the
  wind directions is from a particular wind sector is also shown.

- alpha:

  The alpha transparency to use for the plotting surface (a value
  between 0 and 1 with zero being fully transparent and 1 fully opaque).
  Setting a value below 1 can be useful when plotting surfaces on a map
  using the package `openairmaps`.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  Arguments passed on to
  [`windRose`](https://openair-project.github.io/openair/reference/windRose.md)

  `ws`

  :   Name of the column representing wind speed.

  `wd`

  :   Name of the column representing wind direction.

  `ws2,wd2`

  :   The user can supply a second set of wind speed and wind direction
      values with which the first can be compared. See `pollutionRose()`
      for more details.

  `ws.int`

  :   The Wind speed interval. Default is 2 m/s but for low met masts
      with low mean wind speeds a value of 1 or 0.5 m/s may be better.

  `angle`

  :   Default angle of “spokes” is 30. Other potentially useful angles
      are 45 and 10. Note that the width of the wind speed interval may
      need adjusting using `width`.

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

  `calm.thresh`

  :   By default, conditions are considered to be calm when the wind
      speed is zero. The user can set a different threshold for calms be
      setting `calm.thresh` to a higher value. For example,
      `calm.thresh = 0.5` will identify wind speeds **below** 0.5 as
      calm.

  `bias.corr`

  :   When `angle` does not divide exactly into 360 a bias is introduced
      in the frequencies when the wind direction is already supplied
      rounded to the nearest 10 degrees, as is often the case. For
      example, if `angle = 22.5`, N, E, S, W will include 3 wind sectors
      and all other angles will be two. A bias correction can made to
      correct for this problem. A simple method according to
      Applequist (2012) is used to adjust the frequencies.

  `cols`

  :   Colours to be used for plotting. Options include “default”,
      “increment”, “heat”, “jet”, “hue” and user defined. For user
      defined the user can supply a list of colour names recognised by R
      (type [`colours()`](https://rdrr.io/r/grDevices/colors.html) to
      see the full list). An example would be
      `cols = c("yellow", "green", "blue", "black")`.

  `grid.line`

  :   Grid line interval to use. If `NULL`, as in default, this is
      assigned based on the available data range. However, it can also
      be forced to a specific value, e.g. `grid.line = 10`. `grid.line`
      can also be a list to control the interval, line type and colour.
      For example
      `grid.line = list(value = 10, lty = 5, col = "purple")`.

  `width`

  :   For `paddle = TRUE`, the adjustment factor for width of wind speed
      intervals. For example, `width = 1.5` will make the paddle width
      1.5 times wider.

  `auto.text`

  :   Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis
      labels will automatically try and format pollutant names and units
      properly, e.g., by subscripting the ‘2’ in NO2.

  `offset`

  :   The size of the 'hole' in the middle of the plot, expressed as a
      percentage of the polar axis scale, default 10.

  `max.freq`

  :   Controls the scaling used by setting the maximum value for the
      radial limits. This is useful to ensure several plots use the same
      radial limits.

  `key.header`

  :   Adds additional text/labels above the scale key. For example,
      passing `windRose(mydata, key.header = "ws")` adds the addition
      text as a scale header. Note: This argument is passed to
      [`drawOpenKey()`](https://openair-project.github.io/openair/reference/drawOpenKey.md)
      via
      [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md),
      applying the auto.text argument, to handle formatting.

  `dig.lab`

  :   The number of significant figures at which scientific number
      formatting is used in break point and key labelling. Default 5.

  `include.lowest`

  :   Logical. If `FALSE` (the default), the first interval will be left
      exclusive and right inclusive. If `TRUE`, the first interval will
      be left and right inclusive. Passed to the `include.lowest`
      argument of [`cut()`](https://rdrr.io/r/base/cut.html).

  `statistic`

  :   The `statistic` to be applied to each data bin in the plot.
      Options currently include “prop.count”, “prop.mean” and
      “abs.count”. The default “prop.count” sizes bins according to the
      proportion of the frequency of measurements. Similarly,
      “prop.mean” sizes bins according to their relative contribution to
      the mean. “abs.count” provides the absolute count of measurements
      in each bin.

  `annotate`

  :   If `TRUE` then the percentage calm and mean values are printed in
      each panel together with a description of the statistic below the
      plot. If `" "` then only the statistic is below the plot. Custom
      annotations may be added by setting value to
      `c("annotation 1", "annotation 2")`.

  `angle.scale`

  :   The scale is by default shown at a 315 degree angle. Sometimes the
      placement of the scale may interfere with an interesting feature.
      The user can therefore set `angle.scale` to another value (between
      0 and 360 degrees) to mitigate such problems. For example
      `angle.scale = 45` will draw the scale heading in a NE direction.

  `border`

  :   Border colour for shaded areas. Default is no border.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object. Summarised proportions can be extracted directly using the
`$data` operator, e.g. `object$data` for `output <- windRose(mydata)`.
This returns a data frame with three set columns: `cond`, conditioning
based on `type`; `wd`, the wind direction; and `calm`, the `statistic`
for the proportion of data unattributed to any specific wind direction
because it was collected under calm conditions; and then several (one
for each range binned for the plot) columns giving proportions of
measurements associated with each `ws` or `pollutant` range plotted as a
discrete panel.

## Details

`pollutionRose()` is a
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)
wrapper which brings `pollutant` forward in the argument list, and
attempts to sensibly rescale break points based on the `pollutant` data
range by by-passing `ws.int`.

By default, `pollutionRose()` will plot a pollution rose of `nox` using
"wedge" style segments and placing the scale key to the right of the
plot.

It is possible to compare two wind speed-direction data sets using
`pollutionRose()`. There are many reasons for doing so e.g. to see how
one site compares with another or for meteorological model evaluation.
In this case, `ws` and `wd` are considered to the the reference data
sets with which a second set of wind speed and wind directions are to be
compared (`ws2` and `wd2`). The first set of values is subtracted from
the second and the differences compared. If for example, `wd2` was
biased positive compared with `wd` then `pollutionRose` will show the
bias in polar coordinates. In its default use, wind direction bias is
colour-coded to show negative bias in one colour and positive bias in
another.

## See also

Other polar directional analysis functions:
[`percentileRose()`](https://openair-project.github.io/openair/reference/percentileRose.md),
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md),
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`polarDiff()`](https://openair-project.github.io/openair/reference/polarDiff.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md),
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md),
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)

## Examples

``` r
# pollutionRose of nox
pollutionRose(mydata, pollutant = "nox")


## source apportionment plot - contribution to mean
if (FALSE) { # \dontrun{
pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")
} # }

## example of comparing 2 met sites
## first we will make some new ws/wd data with a postive bias
mydata$ws2 <- mydata$ws + 2 * rnorm(nrow(mydata)) + 1
mydata$wd2 <- mydata$wd + 30 * rnorm(nrow(mydata)) + 30

## need to correct negative wd
id <- which(mydata$wd2 < 0)
mydata$wd2[id] <- mydata$wd2[id] + 360

## results show postive bias in wd and ws
pollutionRose(mydata, ws = "ws", wd = "wd", ws2 = "ws2", wd2 = "wd2")
```
