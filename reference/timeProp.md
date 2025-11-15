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
  proportion = "cluster",
  avg.time = "day",
  type = "default",
  normalise = FALSE,
  cols = "Set1",
  date.breaks = 7,
  date.format = NULL,
  key.columns = 1,
  key.position = "right",
  key.title = proportion,
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

  The splitting variable that makes up the bars in the bar chart e.g.
  `proportion = "cluster"` if the output from `polarCluster` is being
  analysed. If `proportion` is a numeric variable it is split into 4
  quantiles (by default) by `cutData`. If `proportion` is a factor or
  character variable then the categories are used directly.

- avg.time:

  This defines the time period to average to. Can be “sec”, “min”,
  “hour”, “day”, “DSTday”, “week”, “month”, “quarter” or “year”. For
  much increased flexibility a number can precede these options followed
  by a space. For example, a timeAverage of 2 months would be
  `period = "2 month"`.

  Note that `avg.time` when used in `timeProp` should be greater than
  the time gap in the original data. For example, `avg.time = "day"` for
  hourly data is OK, but `avg.time = "hour"` for daily data is not.

- type:

  `type` determines how the data are split i.e. conditioned, and then
  plotted. The default is will produce a single plot using the entire
  data. Type can be one of the built-in types as detailed in `cutData`
  e.g. "season", "year", "weekday" and so on. For example,
  `type = "season"` will produce four plots — one for each season.

  It is also possible to choose `type` as another variable in the data
  frame. If that variable is numeric, then the data will be split into
  four quantiles (if possible) and labelled accordingly. If type is an
  existing character or factor variable, then those categories/levels
  will be used directly. This offers great flexibility for understanding
  the variation of different variables and how they depend on one
  another.

  `type` must be of length one.

- normalise:

  If `normalise = TRUE` then each time interval is scaled to 100. This
  is helpful to show the relative (percentage) contribution of the
  proportions.

- cols:

  Colours to be used for plotting. Options include “default”,
  “increment”, “heat”, “jet” and `RColorBrewer` colours — see the
  `openair` `openColours` function for more details. For user defined
  the user can supply a list of colour names recognised by R (type
  [`colours()`](https://rdrr.io/r/grDevices/colors.html) to see the full
  list). An example would be `cols = c("yellow", "green", "blue")`

- date.breaks:

  Number of major x-axis intervals to use. The function will try and
  choose a sensible number of dates/times as well as formatting the
  date/time appropriately to the range being considered. This does not
  always work as desired automatically. The user can therefore increase
  or decrease the number of intervals by adjusting the value of
  `date.breaks` up or down.

- date.format:

  This option controls the date format on the x-axis. While `timePlot`
  generally sets the date format sensibly there can be some situations
  where the user wishes to have more control. For format types see
  `strptime`. For example, to format the date like “Jan-2012” set
  `date.format = "\%b-\%Y"`.

- key.columns:

  Number of columns to be used in the key. With many pollutants a single
  column can make to key too wide. The user can thus choose to use
  several columns by setting `columns` to be less than the number of
  pollutants.

- key.position:

  Location where the scale key is to plotted. Allowed arguments
  currently include “top”, “right”, “bottom” and “left”.

- key.title:

  The title of the key.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  etc. will automatically try and format pollutant names and units
  properly e.g. by subscripting the \`2' in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  Other graphical parameters passed onto `timeProp` and `cutData`. For
  example, `timeProp` passes the option `hemisphere = "southern"` on to
  `cutData` to provide southern (rather than default northern)
  hemisphere handling of `type = "season"`. Similarly, common axis and
  title labelling options (such as `xlab`, `ylab`, `main`) are passed to
  `xyplot` via `quickText` to handle routine formatting.

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
[`runRegression()`](https://openair-project.github.io/openair/reference/runRegression.md),
[`smoothTrend()`](https://openair-project.github.io/openair/reference/smoothTrend.md),
[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md),
[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md),
[`trendLevel()`](https://openair-project.github.io/openair/reference/trendLevel.md)

Other cluster analysis functions:
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md)

## Author

David Carslaw

## Examples

``` r
## monthly plot of SO2 showing the contribution by wind sector
timeProp(mydata, pollutant = "so2", avg.time = "month", proportion = "wd")
#> Warning: ! Removing 219 rows due to missing `wd` data.
```
