# Function to plot percentiles by wind direction

`percentileRose()` plots percentiles by wind direction with flexible
conditioning. The plot can display multiple percentile lines or filled
areas.

## Usage

``` r
percentileRose(
  mydata,
  pollutant = "nox",
  ws = "ws",
  wd = "wd",
  type = "default",
  percentile = c(25, 50, 75, 90, 95),
  smooth = FALSE,
  method = "default",
  cols = "default",
  angle = 10,
  mean = TRUE,
  mean.lty = 1,
  mean.lwd = 1,
  mean.col = "grey",
  fill = TRUE,
  intervals = NULL,
  angle.scale = 45,
  offset = 0,
  auto.text = TRUE,
  key.title = NULL,
  key.position = "bottom",
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame minimally containing a decimal wind direction and a
  numeric field to plot.

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`. More than one
  pollutant can be supplied e.g. `pollutant = c("no2", "o3")` provided
  there is only one `type`.

- ws:

  The name of the column in `mydata` representing the wind speed.
  Defaults to `"ws"`.

- wd:

  The name of the column in `mydata` representing the decimal wind
  direction, 0 to 360 where 0/360 are North and 180 is South. Defaults
  to `"wd"`.

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

- percentile:

  The percentile value(s) to plot. Must be between 0–100. If
  `percentile = NA` then only a mean line will be shown.

- smooth:

  Should the wind direction data be smoothed using a cyclic spline?

- method:

  When `method = "default"` the supplied percentiles by wind direction
  are calculated. When `method = "cpf"` the conditional probability
  function (CPF) is plotted and a single (usually high) percentile level
  is supplied. The CPF is defined as CPF = my/ny, where my is the number
  of samples in the wind sector y with mixing ratios greater than the
  *overall* percentile concentration, and ny is the total number of
  samples in the same wind sector (see Ashbaugh et al., 1985).

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

- angle:

  Default angle of “spokes” is when `smooth = FALSE`.

- mean:

  Show the mean by wind direction as a line?

- mean.lty:

  Line type for mean line.

- mean.lwd:

  Line width for mean line.

- mean.col:

  Line colour for mean line.

- fill:

  Should the percentile intervals be filled (default) or should lines be
  drawn (`fill = FALSE`).

- intervals:

  User-supplied intervals for the scale e.g.
  `intervals = c(0, 10, 30, 50)`.

- angle.scale:

  In radial plots (e.g.,
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md)),
  the radial scale is drawn directly on the plot itself. While suitable
  defaults have been chosen, sometimes the placement of the scale may
  interfere with an interesting feature. `angle.scale` can take any
  value between `0` and `360` to place the scale at a different angle,
  or `FALSE` to move it to the side of the plots.

- offset:

  `offset` controls the size of the 'hole' in the middle and is
  expressed on a scale of `0` to `100`, where `0` is no hole and `100`
  is a hole that takes up the entire plotting area.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the "2" in "NO2". Passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md).

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

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

`percentileRose()` calculates percentile levels of a pollutant and plots
them by wind direction. One or more percentile levels can be calculated
and these are displayed as either filled areas or as lines.

The wind directions are rounded to the nearest 10 degrees, consistent
with surface data from the UK Met Office before a smooth is fitted. The
levels by wind direction are optionally calculated using a cyclic smooth
cubic spline using the option `smooth`. If `smooth = FALSE` then the
data are shown in 10 degree sectors.

The `percentileRose` function compliments other similar functions
including
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md),
[`pollutionRose()`](https://openair-project.github.io/openair/reference/pollutionRose.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md)
or
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md).
It is most useful for showing the distribution of concentrations by wind
direction and often can reveal different sources e.g. those that only
affect high percentile concentrations such as a chimney stack.

Similar to other functions, flexible conditioning is available through
the `type` option. It is easy for example to consider multiple
percentile values for a pollutant by season, year and so on. See
examples below.

`percentileRose` also offers great flexibility with the scale used and
the user has fine control over both the range, interval and colour.

## References

Ashbaugh, L.L., Malm, W.C., Sadeh, W.Z., 1985. A residence time
probability analysis of sulfur concentrations at ground canyon national
park. Atmospheric Environment 19 (8), 1263-1270.

## See also

Other polar directional analysis functions:
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md),
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
# basic percentile plot
percentileRose(mydata, pollutant = "o3")


# 50/95th percentiles of ozone, with different colours
percentileRose(mydata, pollutant = "o3", percentile = c(50, 95), col = "brewer1")


if (FALSE) { # \dontrun{
# percentiles of ozone by year, with different colours
percentileRose(
  mydata,
  type = "year",
  pollutant = "o3",
  col = "brewer1",
  ncol = 4,
  nrow = 2
)

# percentile concentrations by season and day/nighttime..
percentileRose(
  mydata,
  type = c("daylight", "season"),
  pollutant = "o3",
  col = "brewer1"
)
} # }
```
