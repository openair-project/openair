# Traditional wind rose plot

The traditional wind rose plot that plots wind speed and wind direction
by different intervals. The pollution rose applies the same plot
structure but substitutes other measurements, most commonly a pollutant
time series, for wind speed.

## Usage

``` r
windRose(
  mydata,
  ws = "ws",
  wd = "wd",
  ws2 = NA,
  wd2 = NA,
  ws.int = 2,
  angle = 30,
  type = "default",
  calm.thresh = 0,
  bias.corr = TRUE,
  cols = "default",
  grid.line = NULL,
  width = 0.9,
  seg = 0.9,
  auto.text = TRUE,
  breaks = 4,
  offset = 10,
  normalise = FALSE,
  max.freq = NULL,
  paddle = TRUE,
  key.title = "(m/s)",
  key.position = "bottom",
  strip.position = "top",
  dig.lab = 5,
  include.lowest = FALSE,
  statistic = "prop.count",
  pollutant = NULL,
  annotate = TRUE,
  angle.scale = 315,
  border = NA,
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame containing fields `ws` and `wd`

- ws:

  Name of the column representing wind speed.

- wd:

  Name of the column representing wind direction.

- ws2, wd2:

  The user can supply a second set of wind speed and wind direction
  values with which the first can be compared. See
  [`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md)
  for more details.

- ws.int:

  The Wind speed interval. Default is 2 m/s but for low met masts with
  low mean wind speeds a value of 1 or 0.5 m/s may be better.

- angle:

  Default angle of “spokes” is 30. Other potentially useful angles are
  45 and 10. Note that the width of the wind speed interval may need
  adjusting using `width`.

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

- calm.thresh:

  By default, conditions are considered to be calm when the wind speed
  is zero. The user can set a different threshold for calms be setting
  `calm.thresh` to a higher value. For example, `calm.thresh = 0.5` will
  identify wind speeds **below** 0.5 as calm.

- bias.corr:

  When `angle` does not divide exactly into 360 a bias is introduced in
  the frequencies when the wind direction is already supplied rounded to
  the nearest 10 degrees, as is often the case. For example, if
  `angle = 22.5`, N, E, S, W will include 3 wind sectors and all other
  angles will be two. A bias correction can made to correct for this
  problem. A simple method according to Applequist (2012) is used to
  adjust the frequencies.

- cols:

  Colours to use for plotting. Can be a pre-set palette (e.g.,
  `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a user-defined
  vector of R colours (e.g., `c("yellow", "green", "blue", "black")` -
  see [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
  list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`). See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for more details.

- grid.line:

  Grid line interval to use. If `NULL`, as in default, this is assigned
  based on the available data range. However, it can also be forced to a
  specific value, e.g. `grid.line = 10`. `grid.line` can also be a list
  to control the interval, line type and colour. For example
  `grid.line = list(value = 10, lty = 5, col = "purple")`.

- width:

  For paddle = TRUE, the adjustment factor for width of wind speed
  intervals. For example, width = 1.5 will make the paddle width 1.5
  times wider.

- seg:

  `seg` determines with width of the segments. For example, `seg = 0.5`
  will produce segments 0.5 \* `angle`.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the "2" in "NO2". Passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md).

- breaks:

  Most commonly, the number of break points for wind speed. With the
  `ws.int` default of 2 m/s, the `breaks` default, 4, generates the
  break points 2, 4, 6, 8 m/s. However, `breaks` can also be used to set
  specific break points. For example, the argument
  `breaks = c(0, 1, 10, 100)` breaks the data into segments \<1, 1-10,
  10-100, \>100.

- offset:

  `offset` controls the size of the 'hole' in the middle and is
  expressed on a scale of `0` to `100`, where `0` is no hole and `100`
  is a hole that takes up the entire plotting area.

- normalise:

  If `TRUE` each wind direction segment is normalised to equal one. This
  is useful for showing how the concentrations (or other parameters)
  contribute to each wind sector when the proportion of time the wind is
  from that direction is low. A line showing the probability that the
  wind directions is from a particular wind sector is also shown.

- max.freq:

  Controls the scaling used by setting the maximum value for the radial
  limits. This is useful to ensure several plots use the same radial
  limits.

- paddle:

  Either `TRUE` or `FALSE`. If `TRUE` plots rose using 'paddle' style
  spokes. If `FALSE` plots rose using 'wedge' style spokes.

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

- strip.position:

  Location where the facet 'strips' are located when using `type`. When
  one `type` is provided, can be one of `"left"`, `"right"`, `"bottom"`
  or `"top"`. When two `type`s are provided, this argument defines
  whether the strips are "switched" and can take either `"x"`, `"y"`, or
  `"both"`. For example, `"x"` will switch the 'top' strip locations to
  the bottom of the plot.

- dig.lab:

  The number of significant figures at which scientific number
  formatting is used in break point and key labelling. Default 5.

- include.lowest:

  Logical. If `FALSE` (the default), the first interval will be left
  exclusive and right inclusive. If `TRUE`, the first interval will be
  left and right inclusive. Passed to the `include.lowest` argument of
  [`cut()`](https://rdrr.io/r/base/cut.html).

- statistic:

  The `statistic` to be applied to each data bin in the plot. Options
  currently include “prop.count”, “prop.mean” and “abs.count”. The
  default “prop.count” sizes bins according to the proportion of the
  frequency of measurements. Similarly, “prop.mean” sizes bins according
  to their relative contribution to the mean. “abs.count” provides the
  absolute count of measurements in each bin.

- pollutant:

  Alternative data series to be sampled instead of wind speed. The
  `windRose()` default NULL is equivalent to `pollutant = "ws"`. Use in
  [`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md).

- annotate:

  If `TRUE` then the percentage calm and mean values are printed in each
  panel together with a description of the statistic below the plot. If
  `FALSE` then only the statistic will be printed.

- angle.scale:

  In radial plots (e.g.,
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)),
  the radial scale is drawn directly on the plot itself. While suitable
  defaults have been chosen, sometimes the placement of the scale may
  interfere with an interesting feature. `angle.scale` can take any
  value between `0` and `360` to place the scale at a different angle,
  or `FALSE` to move it to the side of the plots.

- border:

  Border colour for shaded areas. Default is no border.

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

  Addition options are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for `type` handling. Some additional arguments are also available,
  varying somewhat in different plotting functions:

  - `title`, `subtitle`, `caption`, `xlab` and `ylab` control the plot
    title, subtitle, caption, x-axis label and y-axis label. All of
    these are passed through to
    [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
    if `auto.text = TRUE`.

  - `xlim`, `ylim` and `limits` control the limits of the x-axis, y-axis
    and colorbar scales.

  - `ncol` and `nrow` set the number of columns and rows in a faceted
    plot.

  - `fontsize` overrides the overall font size of the plot by setting
    the `text` argument of
    [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
    It may also be applied proportionately to any `openair` annotations
    (e.g., N/E/S/W labels on polar coordinate plots).

  - Various graphical parameters are also supported: `linewidth`,
    `linetype`,` shape`, `size`, `border`, and `alpha`. Not all
    parameters apply to all plots. These can take a single value, or a
    vector of multiple values - e.g., `shape = c(1, 2)` - which will be
    recycled to the length of values needed.

  - `lineend`, `linejoin` and `linemitre` tweak the appearance of line
    plots; see
    [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
    for more information.

  - In polar coordinate plots, `annotate = FALSE` will remove the
    N/E/S/W labels and any other annotations.

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

For `windRose` data are summarised by direction, typically by 45 or 30
(or 10) degrees and by different wind speed categories. Typically, wind
speeds are represented by different width "paddles". The plots show the
proportion (here represented as a percentage) of time that the wind is
from a certain angle and wind speed range.

By default `windRose` will plot a windRose in using "paddle" style
segments and placing the scale key below the plot.

The argument `pollutant` uses the same plotting structure but
substitutes another data series, defined by `pollutant`, for wind speed.
It is recommended to use
[`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md)
for plotting pollutant concentrations.

The option `statistic = "prop.mean"` provides a measure of the relative
contribution of each bin to the panel mean, and is intended for use with
`pollutionRose`.

## References

Applequist, S, 2012: Wind Rose Bias Correction. J. Appl. Meteor.
Climatol., 51, 1305-1309.

Droppo, J.G. and B.A. Napier (2008) Wind Direction Bias in Generating
Wind Roses and Conducting Sector-Based Air Dispersion Modeling, Journal
of the Air & Waste Management Association, 58:7, 913-918.

## See also

Other polar directional analysis functions:
[`percentileRose()`](https://openair-project.github.io/openair/reference/percentileRose.md),
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md),
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`polarDiff()`](https://openair-project.github.io/openair/reference/polarDiff.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md),
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md),
[`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md)

## Author

David Carslaw

Karl Ropkins

Jack Davison

## Examples

``` r
# basic plot
windRose(mydata)


# one windRose for each year
windRose(mydata, type = "year")


# windRose in 10 degree intervals with gridlines and width adjusted
if (FALSE) { # \dontrun{
windRose(mydata, angle = 10, width = 0.2, grid.line = 1)
} # }
```
