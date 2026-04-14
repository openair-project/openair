# Plot heat maps of atmospheric composition data

The `trendLevel()` function provides a way of rapidly showing a large
amount of data in a condensed form. In one plot, the variation in the
concentration of one pollutant can to shown as a function of between two
and four categorical properties. The default arguments plot hour of day
on the x-axis and month of year on the y-axis. However, `x`, `y` and
`type` and summarising statistics can all be modified to provide a range
of other similar plots, all being passed to
[`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
for discretisation. The average wind speed and direction in each bin can
also be plotted using the `windflow` argument.

## Usage

``` r
trendLevel(
  mydata,
  pollutant = "nox",
  x = "month",
  y = "hour",
  type = "default",
  rotate.axis = c(90, 0),
  n.levels = c(10, 10, 4),
  windflow = NULL,
  limits = NULL,
  min.bin = 1,
  cols = "default",
  auto.text = TRUE,
  key.title = paste("use.stat.name", pollutant, sep = " "),
  key.position = "right",
  strip.position = "top",
  labels = NULL,
  breaks = NULL,
  statistic = c("mean", "max", "min", "median", "frequency", "sum", "sd", "percentile"),
  percentile = 95,
  stat.args = NULL,
  stat.safe.mode = TRUE,
  drop.unused.types = TRUE,
  col.na = "white",
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  The openair data frame to use to generate the `trendLevel()` plot.

- pollutant:

  The name of the data series in `mydata` to sample to produce the
  `trendLevel()` plot.

- x, y, type:

  The name of the data series to use as the `trendLevel()` x-axis,
  y-axis or conditioning variable, passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  These are used before applying `statistic`. `trendLevel()` does not
  allow duplication in `x`, `y` and `type` options.

- rotate.axis:

  The rotation to be applied to `trendLevel` `x` and `y` axes. The
  default, `c(90, 0)`, rotates the x axis by 90 degrees but does not
  rotate the y axis. If only one value is supplied, this is applied to
  both axes; if more than two values are supplied, only the first two
  are used.

- n.levels:

  The number of levels to split `x`, `y` and `type` data into if
  numeric. The default, `c(10, 10, 4)`, cuts numeric `x` and `y` data
  into ten levels and numeric `type` data into four levels. This option
  is ignored for date conditioning and factors. If less than three
  values are supplied, three values are determined by recursion; if more
  than three values are supplied, only the first three are used.

- windflow:

  If `TRUE`, the vector-averaged wind speed and direction will be
  plotted using arrows. Alternatively, can be a list of arguments to
  control the appearance of the arrows (colour, linewidth, alpha value,
  etc.). See
  [`windflowOpts()`](https://openair-project.github.io/openair/reference/windflowOpts.md)
  for details.

- limits:

  The colour scale range to use when generating the `trendLevel()` plot.

- min.bin:

  The minimum number of records required in a bin to show a value. Bins
  with fewer than `min.bin` records are set to `NA`. The default is 1,
  i.e., all bins with no records are set to `NA`. Setting `min.bin` to a
  value greater than 1 can be useful to exclude bins with very few
  records that might produce unreliable statistic values.

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

- strip.position:

  Location where the facet 'strips' are located when using `type`. When
  one `type` is provided, can be one of `"left"`, `"right"`, `"bottom"`
  or `"top"`. When two `type`s are provided, this argument defines
  whether the strips are "switched" and can take either `"x"`, `"y"`, or
  `"both"`. For example, `"x"` will switch the 'top' strip locations to
  the bottom of the plot.

- breaks, labels:

  If a categorical colour scale is required, `breaks` should be
  specified. This can be either of:

  - A single value, which will divide the scale into `breaks` levels
    using the same logic as
    [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
    For example, `breaks = 5` will split the scale into five quantiles.

  - A numeric vector, which will define the specific breakpoints. For
    example, `c(0, 50, 100)` will bin the data into `0 to 50`,
    `50 to 100`, and so on. If `breaks` does not cover the full range of
    the data, the outer limits will be extended so that the full colour
    scale is covered while retaining the desired number of breaks.

  By default, `breaks` will generate nicely formatted labels for each
  category. The `labels` argument overrides this - for example, a user
  could define `breaks = 3, labels = c("low", "medium", "high")`. Care
  should be taken to provide the appropriate number of `labels` - it
  should be equal to `breaks` if a single value is given, or equal to
  `length(breaks)-1` if `breaks` is a vector.

- statistic:

  The statistic to apply when aggregating the data; default is the mean.
  Can be one of `"mean"`, `"max"`, `"min"`, `"median"`, `"frequency"`,
  `"sum"`, `"sd"`, `"percentile"`. Note that `"sd"` is the standard
  deviation, `"frequency"` is the number (frequency) of valid records in
  the period and `"data.cap"` is the percentage data capture.
  `"percentile"` is the percentile level (%) between 0-100, which can be
  set using the `"percentile"` option. Functions can also be sent
  directly via `statistic`; see 'Details' for more information.

- percentile:

  The percentile level used when `statistic = "percentile"`. The default
  is 95%.

- stat.args:

  Additional options to be used with `statistic` if this is a function.
  The extra options should be supplied as a list of named parameters;
  see 'Details' for more information.

- stat.safe.mode:

  An addition protection applied when using functions directly with
  `statistic` that most users can ignore. This option returns `NA`
  instead of running `statistic` on binned sub samples that are empty.
  Many common functions terminate with an error message when applied to
  an empty dataset. So, this option provides a mechanism to work with
  such functions. For a very few cases, e.g., for a function that
  counted missing entries, it might need to be set to `FALSE`; see
  'Details' for more information.

- drop.unused.types:

  Hide unused/empty `type` conditioning cases. Some conditioning options
  may generate empty cases for some data sets, e.g. a hour of the day
  when no measurements were taken. Empty `x` and `y` cases generate
  'holes' in individual plots. However, empty `type` cases would produce
  blank panels if plotted. Therefore, the default, `TRUE`, excludes
  these empty panels from the plot. The alternative `FALSE` plots all
  `type` panels.

- col.na:

  Colour to be used to show missing data.

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
    plot title, subtitle, caption, tag, x-axis label and y-axis label.
    All of these are passed through to
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
object.

## Details

`trendLevel()` allows the use of third party summarising functions via
the `statistic` option. Any additional function arguments not included
within a function called using `statistic` should be supplied as a list
of named parameters and sent using `stat.args`. For example, the encoded
option `statistic = "mean"` is equivalent to
`statistic = mean, stat.args = list(na.rm = TRUE)` or the R command
`mean(x, na.rm = TRUE)`. Many R functions and user's own code could be
applied in a similar fashion, subject to the following restrictions: the
first argument sent to the function must be the data series to be
analysed; the name 'x' cannot be used for any of the extra options
supplied in `stat.args`; and the function should return the required
answer as a numeric or `NA`. Note: If the supplied function returns more
than one answer, currently only the first of these is retained and used
by `trendLevel()`. All other returned information will be ignored
without warning. If the function terminates with an error when it is
sent an empty data series, the option `stat.safe.mode` should not be set
to `FALSE` or `trendLevel()` may fail. Note: The `stat.safe.mode = TRUE`
option returns an NA without warning for empty data series.

## Author

Karl Ropkins

David Carslaw

Jack Davison

## Examples

``` r
# basic use
# default statistic = "mean"
trendLevel(mydata, pollutant = "nox")


# applying same as 'own' statistic
my.mean <- function(x) mean(x, na.rm = TRUE)
trendLevel(mydata, pollutant = "nox", statistic = my.mean)


# alternative for 'third party' statistic
# trendLevel(mydata, pollutant = "nox", statistic = mean,
#           stat.args = list(na.rm = TRUE))

if (FALSE) { # \dontrun{
# example with categorical scale
trendLevel(mydata,
  pollutant = "no2",
  border = "white", statistic = "max",
  breaks = c(0, 50, 100, 500),
  labels = c("low", "medium", "high"),
  cols = c("forestgreen", "yellow", "red")
)
} # }
```
