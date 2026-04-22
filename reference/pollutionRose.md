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
  key.title = pollutant,
  key.position = "right",
  breaks = 6,
  paddle = FALSE,
  seg = 0.9,
  normalise = FALSE,
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame containing fields `ws` and `wd`

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`.

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

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

  `seg` determines with width of the segments. For example, `seg = 0.5`
  will produce segments 0.5 \* `angle`.

- normalise:

  If `TRUE` each wind direction segment is normalised to equal one. This
  is useful for showing how the concentrations (or other parameters)
  contribute to each wind sector when the proportion of time the wind is
  from that direction is low. A line showing the probability that the
  wind directions is from a particular wind sector is also shown.

- plot:

  When `openair` plots are created they are automatically printed to the
  active graphics device. `plot = FALSE` deactivates this behaviour.
  This may be useful when the plot *data* is of more interest, or the
  plot is required to appear later (e.g., later in a Quarto document, or
  to be saved to a file).

- key:

  Deprecated; please use `key.position`. If `FALSE`, sets `key.position`
  to `"none"`.

- ...:

  Arguments passed on to
  [`windRose`](https://openair-project.github.io/openair/reference/windRose.md)

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

  `grid.line`

  :   Grid line interval to use. If `NULL`, as in default, this is
      assigned based on the available data range. However, it can also
      be forced to a specific value, e.g. `grid.line = 10`. `grid.line`
      can also be a list to control the interval, line type and colour.
      For example
      `grid.line = list(value = 10, lty = 5, col = "purple")`.

  `width`

  :   For paddle = TRUE, the adjustment factor for width of wind speed
      intervals. For example, width = 1.5 will make the paddle width 1.5
      times wider.

  `max.freq`

  :   Controls the scaling used by setting the maximum value for the
      radial limits. This is useful to ensure several plots use the same
      radial limits.

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
      plot. If `FALSE` then only the statistic will be printed.

  `border`

  :   Border colour for shaded areas. Default is no border.

  `ws`

  :   The name of the column in `mydata` representing the wind speed.
      Defaults to `"ws"`.

  `wd`

  :   The name of the column in `mydata` representing the decimal wind
      direction, 0 to 360 where 0/360 are North and 180 is South.
      Defaults to `"wd"`.

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

  `cols`

  :   Colours to use for plotting. Can be a pre-set palette (e.g.,
      `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a
      user-defined vector of R colours (e.g.,
      `c("yellow", "green", "blue", "black")` - see
      [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
      list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`).
      Alternatively, can be a list of arguments to control the colour
      palette more closely (e.g., `palette`, `direction`, `alpha`,
      etc.). See
      [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
      and
      [`colourOpts()`](https://openair-project.github.io/openair/reference/colourOpts.md)
      for more details.

  `angle.scale`

  :   In radial plots (e.g.,
      [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)),
      the radial scale is drawn directly on the plot itself. While
      suitable defaults have been chosen, sometimes the placement of the
      scale may interfere with an interesting feature. `angle.scale` can
      take any value between `0` and `360` to place the scale at a
      different angle, or `FALSE` to move it to the side of the plots.

  `offset`

  :   `offset` controls the size of the 'hole' in the middle and is
      expressed on a scale of `0` to `100`, where `0` is no hole and
      `100` is a hole that takes up the entire plotting area.

  `auto.text`

  :   Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis
      labels will automatically try and format pollutant names and units
      properly, e.g., by subscripting the "2" in "NO2". Passed to
      [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md).

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


# source apportionment plot - contribution to mean
if (FALSE) { # \dontrun{
pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")

# example of comparing 2 met sites
# first we will make some new ws/wd data with a postive bias
mydata$ws2 <- mydata$ws + 2 * rnorm(nrow(mydata)) + 1
mydata$wd2 <- mydata$wd + 30 * rnorm(nrow(mydata)) + 30

# need to correct negative wd
id <- which(mydata$wd2 < 0)
mydata$wd2[id] <- mydata$wd2[id] + 360

# results show postive bias in wd and ws
pollutionRose(mydata, ws = "ws", wd = "wd", ws2 = "ws2", wd2 = "wd2")

## add some wd bias to some nighttime hours
id <- which(as.numeric(format(mydata$date, "%H")) %in% c(23, 1, 2, 3, 4, 5))
mydata$wd2[id] <- mydata$wd[id] + 30 * rnorm(length(id)) + 120
id <- which(mydata$wd2 < 0)
mydata$wd2[id] <- mydata$wd2[id] + 360

pollutionRose(
  mydata,
  ws = "ws",
  wd = "wd",
  ws2 = "ws2",
  wd2 = "wd2",
  breaks = c(-11, -2, -1, -0.5, 0.5, 1, 2, 11),
  cols = c("dodgerblue4", "white", "firebrick"),
  type = "daylight"
)
} # }
```
