# Time series plot with categories shown as a stacked bar chart

This function shows time series plots as stacked bar charts. The
different categories in the bar chart are made up from a character or
factor variable in a data frame. The function is primarily developed to
support the plotting of cluster analysis output from
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md)
and
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md)
that consider local and regional (back trajectory) cluster analysis
respectively. However, the function has more general use for
understanding time series data.

## Usage

``` r
timeProp(
  mydata,
  pollutant = "nox",
  proportion = "wd",
  avg.time = "day",
  type = "default",
  cols = "Set1",
  normalise = FALSE,
  x.relation = "same",
  y.relation = "same",
  key = TRUE,
  key.columns = 1,
  key.position = "right",
  key.title = proportion,
  strip.position = "top",
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame containing the fields `date`, `pollutant` and a splitting
  variable `proportion`

- pollutant:

  Name of the pollutant to plot contained in `mydata`.

- proportion:

  The splitting variable that makes up the bars in the bar chart,
  defaulting to `"wd"`. Could be `"cluster"` if the output from
  [`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md)
  or
  [`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md)
  is being analysed. If `proportion` is a numeric variable it is split
  into 4 quantiles (by default) by
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  If `proportion` is a factor or character variable then the categories
  are used directly.

- avg.time:

  This defines the time period to average to. Can be `"sec"`, `"min"`,
  `"hour"`, `"day"`, `"DSTday"`, `"week"`, `"month"`, `"quarter"` or
  `"year"`. For much increased flexibility a number can precede these
  options followed by a space. For example, an average of 2 months would
  be `avg.time = "2 month"`. In addition, `avg.time` can equal
  `"season"`, in which case 3-month seasonal values are calculated with
  spring defined as March, April, May and so on.

  Note that `avg.time` when used in `timeProp` should be greater than
  the time gap in the original data. For example, `avg.time = "day"` for
  hourly data is OK, but `avg.time = "hour"` for daily data is not.

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

- cols:

  Colours to use for plotting. Can be a pre-set palette (e.g.,
  `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a user-defined
  vector of R colours (e.g., `c("yellow", "green", "blue", "black")` -
  see [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
  list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`). See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for more details.

- normalise:

  If `normalise = TRUE` then each time interval is scaled to 100. This
  is helpful to show the relative (percentage) contribution of the
  proportions.

- x.relation, y.relation:

  This determines how the x- and y-axis scales are plotted. `"same"`
  ensures all panels use the same scale and `"free"` will use
  panel-specific scales. The latter is a useful setting when plotting
  data with very different values.

- key:

  Deprecated; please use `key.position`. If `FALSE`, sets `key.position`
  to `"none"`.

- key.columns:

  Number of columns to be used in a categorical legend. With many
  categories a single column can make to key too wide. The user can thus
  choose to use several columns by setting `key.columns` to be less than
  the number of categories.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- strip.position:

  Location where the facet 'strips' are located when using `type`. When
  one `type` is provided, can be one of `"left"`, `"right"`, `"bottom"`
  or `"top"`. When two `type`s are provided, this argument defines
  whether the strips are "switched" and can take either `"x"`, `"y"`, or
  `"both"`. For example, `"x"` will switch the 'top' strip locations to
  the bottom of the plot.

- date.breaks:

  Number of major x-axis intervals to use. The function will try and
  choose a sensible number of dates/times as well as formatting the
  date/time appropriately to the range being considered. The user can
  override this behaviour by adjusting the value of `date.breaks` up or
  down.

- date.format:

  This option controls the date format on the x-axis. A sensible format
  is chosen by default, but the user can set `date.format` to override
  this. For format types see
  [`strptime()`](https://rdrr.io/r/base/strptime.html). For example, to
  format the date like "Jan-2012" set `date.format = "\%b-\%Y"`.

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

  Addition options are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for `type` handling. Some additional arguments are also available:

  - `xlab`, `ylab` and `main` override the x-axis label, y-axis label,
    and plot title.

  - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have
    2 columns and 5 rows.

  - `fontsize` overrides the overall font size of the plot.

  - `border` sets the border colour of each bar.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Details

In order to plot time series in this way, some sort of time aggregation
is needed, which is controlled by the option `avg.time`.

The plot shows the value of `pollutant` on the y-axis (averaged
according to `avg.time`). The time intervals are made up of bars split
according to `proportion`. The bars therefore show how the total value
of `pollutant` is made up for any time interval.

## See also

Other time series and trend functions:
[`TheilSen()`](https://openair-project.github.io/openair/reference/TheilSen.md),
[`calendarPlot()`](https://openair-project.github.io/openair/reference/calendarPlot.md),
[`smoothTrend()`](https://openair-project.github.io/openair/reference/smoothTrend.md),
[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md),
[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md)

Other cluster analysis functions:
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md)

## Author

David Carslaw

Jack Davison

## Examples

``` r
# monthly plot of SO2 showing the contribution by wind sector
timeProp(mydata, pollutant = "so2", avg.time = "month", proportion = "wd")
#> Warning: ! Removing 219 rows due to missing `wd` data.
```
