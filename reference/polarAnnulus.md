# Bivariate polarAnnulus plot

Typically plots the concentration of a pollutant by wind direction and
as a function of time as an annulus. The function is good for
visualising how concentrations of pollutants vary by wind direction and
a time period e.g. by month, day of week.

## Usage

``` r
polarAnnulus(
  mydata,
  pollutant = "nox",
  wd = "wd",
  resolution = "fine",
  local.tz = NULL,
  period = "hour",
  type = "default",
  statistic = "mean",
  percentile = NA,
  limits = NULL,
  cols = "default",
  col.na = "grey",
  offset = 50,
  angle.scale = 0,
  min.bin = 1,
  exclude.missing = TRUE,
  date.pad = FALSE,
  force.positive = TRUE,
  k = c(20, 10),
  normalise = FALSE,
  breaks = NULL,
  key.title = paste(statistic, pollutant, sep = " "),
  key.position = "right",
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame minimally containing `date`, a wind direction and a
  pollutant.

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`. There can also be
  more than one pollutant specified e.g. `pollutant = c("nox", "no2")`.
  The main use of using two or more pollutants is for model evaluation
  where two species would be expected to have similar concentrations.
  This saves the user stacking the data and it is possible to work with
  columns of data directly. A typical use would be
  `pollutant = c("obs", "mod")` to compare two columns “obs” (the
  observations) and “mod” (modelled values).

- wd:

  The name of the column in `mydata` representing the decimal wind
  direction, 0 to 360 where 0/360 are North and 180 is South. Defaults
  to `"wd"`.

- resolution:

  Two plot resolutions can be set: “normal” and “fine” (the default).

- local.tz:

  Should the results be calculated in local time that includes a
  treatment of daylight savings time (DST)? The default is not to
  consider DST issues, provided the data were imported without a DST
  offset. Emissions activity tends to occur at local time e.g. rush hour
  is at 8 am every day. When the clocks go forward in spring, the
  emissions are effectively released into the atmosphere typically 1
  hour earlier during the summertime i.e. when DST applies. When
  plotting diurnal profiles, this has the effect of “smearing-out” the
  concentrations. Sometimes, a useful approach is to express time as
  local time. This correction tends to produce better-defined diurnal
  profiles of concentration (or other variables) and allows a better
  comparison to be made with emissions/activity data. If set to `FALSE`
  then GMT is used. Examples of usage include
  `local.tz = "Europe/London"`, `local.tz = "America/New_York"`. See
  `cutData` and `import` for more details.

- period:

  This determines the temporal period to consider. Options are “hour”
  (the default, to plot diurnal variations), “season” to plot variation
  throughout the year, “weekday” to plot day of the week variation and
  “trend” to plot the trend by wind direction.

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

- statistic:

  The statistic that should be applied to each wind speed/direction bin.
  Can be “mean” (default), “median”, “max” (maximum), “frequency”.
  “stdev” (standard deviation), “weighted.mean” or “cpf” (Conditional
  Probability Function). Because of the smoothing involved, the colour
  scale for some of these statistics is only to provide an indication of
  overall pattern and should not be interpreted in concentration units
  e.g. for `statistic = "weighted.mean"` where the bin mean is
  multiplied by the bin frequency and divided by the total frequency. In
  many cases using `polarFreq` will be better. Setting
  `statistic = "weighted.mean"` can be useful because it provides an
  indication of the concentration \* frequency of occurrence and will
  highlight the wind speed/direction conditions that dominate the
  overall mean.

- percentile:

  If `statistic = "percentile"` or `statistic = "cpf"` then `percentile`
  is used, expressed from 0 to 100. Note that the percentile value is
  calculated in the wind speed, wind direction ‘bins’. For this reason
  it can also be useful to set `min.bin` to ensure there are a
  sufficient number of points available to estimate a percentile. See
  `quantile` for more details of how percentiles are calculated.

- limits:

  The function does its best to choose sensible limits automatically.
  However, there are circumstances when the user will wish to set
  different ones. An example would be a series of plots showing each
  year of data separately. The limits are set in the form
  `c(lower, upper)`, so `limits = c(0, 100)` would force the plot limits
  to span 0-100.

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

- col.na:

  Colour to be used to show missing data.

- offset:

  `offset` controls the size of the 'hole' in the middle and is
  expressed on a scale of `0` to `100`, where `0` is no hole and `100`
  is a hole that takes up the entire plotting area.

- angle.scale:

  In radial plots (e.g.,
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)),
  the radial scale is drawn directly on the plot itself. While suitable
  defaults have been chosen, sometimes the placement of the scale may
  interfere with an interesting feature. `angle.scale` can take any
  value between `0` and `360` to place the scale at a different angle,
  or `FALSE` to move it to the side of the plots.

- min.bin:

  The minimum number of points allowed in a wind speed/wind direction
  bin. The default is 1. A value of two requires at least 2 valid
  records in each bin an so on; bins with less than 2 valid records are
  set to NA. Care should be taken when using a value \> 1 because of the
  risk of removing real data points. It is recommended to consider your
  data with care. Also, the `polarFreq` function can be of use in such
  circumstances.

- exclude.missing:

  Setting this option to `TRUE` (the default) removes points from the
  plot that are too far from the original data. The smoothing routines
  will produce predictions at points where no data exist i.e. they
  predict. By removing the points too far from the original data
  produces a plot where it is clear where the original data lie. If set
  to `FALSE` missing data will be interpolated.

- date.pad:

  For `type = "trend"` (default), `date.pad = TRUE` will pad-out missing
  data to the beginning of the first year and the end of the last year.
  The purpose is to ensure that the trend plot begins and ends at the
  beginning or end of year.

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

  The smoothing value supplied to `gam` for the temporal and wind
  direction components, respectively. In some cases e.g. a trend plot
  with less than 1-year of data the smoothing with the default values
  may become too noisy and affected more by outliers. Choosing a lower
  value of `k` (say 10) may help produce a better plot.

- normalise:

  If `TRUE` concentrations are normalised by dividing by their mean
  value. This is done *after* fitting the smooth surface. This option is
  particularly useful if one is interested in the patterns of
  concentrations for several pollutants on different scales e.g. NOx and
  CO. Often useful if more than one `pollutant` is chosen.

- breaks:

  `breaks` bins a continuous axis into discrete bins. It can either take
  a single number (e.g., `breaks = 5`) to split the scale into
  quantiles, a vector of numbers (e.g.,
  `breaks = c(0, 50, 100, 200, 500`) to define specific break-points, or
  a named list. See
  [`breakOpts()`](https://openair-project.github.io/openair/reference/breakOpts.md)
  for more details.

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

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

- key:

  Deprecated; please use `key.position`. If `FALSE`, sets `key.position`
  to `"none"`.

- ...:

  Addition options are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for `type` handling. Some additional arguments are also available,
  varying somewhat in different plotting functions:

  - `title`, `subtitle`, `caption`, `tag`, `xlab` and `ylab` control the
    plot title, subtitle, caption, tag, x-axis label and y-axis label,
    passed to
    [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
    via
    [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
    if `auto.text = TRUE`.

  - `xlim`, `ylim` and `limits` control the limits of the x-axis, y-axis
    and colorbar scales.

  - `ncol` and `nrow` set the number of columns and rows in a faceted
    plot.

  - `scales` can be `"fixed"`, `"free_x"`, `"free_y"` or `"free"` to
    control whether axes are shared across facets when using `type`.
    Also supported are the legacy `x.relation` and `y.relation`, which
    can be either `"same"` or `"free"` and get remapped to `scales`
    automatically.

  - Similarly, `space`, `axes`, `axis.labels`, `switch` and
    `strip.position` can be used to customise the appearance of faceted
    plots. See
    [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
    and
    [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
    for the arguments these take.

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
object

## Details

The `polarAnnulus` function shares many of the properties of the
`polarPlot`. However, `polarAnnulus` is focussed on displaying
information on how concentrations of a pollutant (values of another
variable) vary with wind direction and time. Plotting as an annulus
helps to reduce compression of information towards the centre of the
plot. The circular plot is easy to interpret because wind direction is
most easily understood in polar rather than Cartesian coordinates.

The inner part of the annulus represents the earliest time and the outer
part of the annulus the latest time. The time dimension can be shown in
many ways including "trend", "hour" (hour or day), "season" (month of
the year) and "weekday" (day of the week). Taking hour as an example,
the plot will show how concentrations vary by hour of the day and wind
direction. Such plots can be very useful for understanding how different
source influences affect a location.

For `type = "trend"` the amount of smoothing does not vary linearly with
the length of the time series i.e. a certain amount of smoothing per
unit interval in time. This is a deliberate choice because should one be
interested in a subset (in time) of data, more detail will be provided
for the subset compared with the full data set. This allows users to
investigate specific periods in more detail. Full flexibility is given
through the smoothing parameter `k`.

## See also

Other polar directional analysis functions:
[`percentileRose()`](https://openair-project.github.io/openair/reference/percentileRose.md),
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`polarDiff()`](https://openair-project.github.io/openair/reference/polarDiff.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md),
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md),
[`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md),
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)

## Author

David Carslaw

Jack Davison

## Examples

``` r
# diurnal plot for PM10 at Marylebone Rd
if (FALSE) { # \dontrun{
polarAnnulus(mydata,
  pollutant = "pm10",
  title = "diurnal variation in pm10 at Marylebone Road"
)
} # }

# seasonal plot for PM10 at Marylebone Rd
if (FALSE) { # \dontrun{
polarAnnulus(mydata, poll = "pm10", period = "season")
} # }

# trend in coarse particles (PMc = PM10 - PM2.5), calculate PMc first

mydata$pmc <- mydata$pm10 - mydata$pm25
if (FALSE) { # \dontrun{
polarAnnulus(mydata,
  poll = "pmc", period = "trend",
  title = "trend in pmc at Marylebone Road"
)
} # }
```
