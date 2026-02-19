# Plot time series, perhaps for multiple pollutants, grouped or in separate panels.

The `timePlot()` is the basic time series plotting function in
`openair`. Its purpose is to make it quick and easy to plot time series
for pollutants and other variables. The other purpose is to plot
potentially many variables together in as compact a way as possible.

## Usage

``` r
timePlot(
  mydata,
  pollutant = "nox",
  group = FALSE,
  stack = FALSE,
  normalise = NULL,
  avg.time = "default",
  data.thresh = 0,
  statistic = "mean",
  percentile = NA,
  date.pad = FALSE,
  type = "default",
  cols = "brewer1",
  log = FALSE,
  windflow = NULL,
  smooth = FALSE,
  ci = TRUE,
  x.relation = "same",
  y.relation = "same",
  ref.x = NULL,
  ref.y = NULL,
  key = TRUE,
  key.columns = NULL,
  key.position = "bottom",
  strip.position = "top",
  name.pol = pollutant,
  date.breaks = 7,
  date.format = NULL,
  auto.text = TRUE,
  plot = TRUE,
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

- group:

  If more than one pollutant is chosen, should they all be plotted on
  the same graph together? The default is `FALSE`, which means they are
  plotted in separate panels with their own scaled. If `TRUE` then they
  are plotted on the same plot with the same scale.

- stack:

  If `TRUE` the time series will be stacked by year. This option can be
  useful if there are several years worth of data making it difficult to
  see much detail when plotted on a single plot.

- normalise:

  Should variables be normalised? The default is is not to normalise the
  data. `normalise` can take two values, either `"mean"` or a string
  representing a date in UK format e.g. "1/1/1998" (in the format
  dd/mm/YYYY). If `normalise = "mean"` then each time series is divided
  by its mean value. If a date is chosen, then values at that date are
  set to 100 and the rest of the data scaled accordingly. Choosing a
  date (say at the beginning of a time series) is very useful for
  showing how trends diverge over time. Setting `group = TRUE` is often
  useful too to show all time series together in one panel.

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

  The statistic to apply when aggregating the data; default is the mean.
  Can be one of `"mean"`, `"max"`, `"min"`, `"median"`, `"frequency"`,
  `"sum"`, `"sd"`, `"percentile"`. Note that `"sd"` is the standard
  deviation, `"frequency"` is the number (frequency) of valid records in
  the period and `"data.cap"` is the percentage data capture.
  `"percentile"` is the percentile level (%) between 0-100, which can be
  set using the `"percentile"` option — see below. Not used if
  `avg.time = "default"`.

- percentile:

  The percentile level in percent used when `statistic = "percentile"`
  and when aggregating the data with `avg.time`. More than one
  percentile level is allowed for `type = "default"` e.g.
  `percentile = c(50, 95)`. Not used if `avg.time = "default"`.

- date.pad:

  Should missing data be padded-out? This is useful where a data frame
  consists of two or more "chunks" of data with time gaps between them.
  By setting `date.pad = TRUE` the time gaps between the chunks are
  shown properly, rather than with a line connecting each chunk. For
  irregular data, set to `FALSE`. Note, this should not be set for
  `type` other than `default`.

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

  `type` must be of length one.

- cols:

  Colours to be used for plotting; see
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for details.

- log:

  Should the y-axis appear on a log scale? The default is `FALSE`. If
  `TRUE` a well-formatted log10 scale is used. This can be useful for
  plotting data for several different pollutants that exist on very
  different scales. It is therefore useful to use `log = TRUE` together
  with `group = TRUE`.

- windflow:

  This option allows a scatter plot to show the wind speed/direction as
  an arrow. The option is a list e.g.
  `windflow = list(col = "grey", lwd = 2)`. This option requires wind
  speed (`ws`) and wind direction (`wd`) to be available.

  Any of the arguments in
  [`ggplot2::arrow()`](https://ggplot2.tidyverse.org/reference/reexports.html)
  can be passed as a list, as well as `col` which controls the arrow's
  colour and `lwd` which controls the line width.

  This option works best where there are not too many data to ensure
  over-plotting does not become a problem.

- smooth:

  Should a smooth line be applied to the data? The default is `FALSE`.

- ci:

  If a smooth fit line is applied, then `ci` determines whether the 95
  percent confidence intervals are shown.

- x.relation, y.relation:

  This determines how the x- or y-axis scale is plotted. `"same"`
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

- strip.position:

  Location where the facet 'strips' are located when using `type`. When
  one `type` is provided, can be one of `"left"`, `"right"`, `"bottom"`
  or `"top"`. When two `type`s are provided, this argument defines
  whether the strips are "switched" and can take either `"x"`, `"y"`, or
  `"both"`. For example, `"x"` will switch the 'top' strip locations to
  the bottom of the plot.

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

  This option controls the date format on the x-axis. While `timePlot()`
  generally sets the date format sensibly there can be some situations
  where the user wishes to have more control. For format types see
  [`strptime()`](https://rdrr.io/r/base/strptime.html). For example, to
  format the date like "Jan-2012" set `date.format = "\%b-\%Y"`.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the '2' in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  Other graphical parameters are passed onto
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  and
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).
  For example, `timePlot()` passes the option `hemisphere = "southern"`
  on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  to provide southern (rather than default northern) hemisphere handling
  of `type = "season"`. Similarly, most common plotting parameters, such
  as `layout` for panel arrangement and `pch` and `cex` for plot symbol
  type and size and `lty` and `lwd` for line type and width, as passed
  to [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html),
  although some maybe locally managed by `openair` on route, e.g., axis
  and title labelling options (such as `xlab`, `ylab`, `main`) are
  passed via
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  to handle routine formatting. See examples below.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Details

The function is flexible enough to plot more than one variable at once.
If more than one variable is chosen plots it can either show all
variables on the same plot (with different line types) *on the same
scale*, or (if `group = FALSE`) each variable in its own panels with its
own scale.

The general preference is not to plot two variables on the same graph
with two different y-scales. It can be misleading to do so and difficult
with more than two variables. If there is in interest in plotting
several variables together that have very different scales, then it can
be useful to normalise the data first, which can be down be setting the
`normalise` option.

The user has fine control over the choice of colours, line width and
line types used. This is useful for example, to emphasise a particular
variable with a specific line type/colour/width.

`timePlot()` works very well with
[`selectByDate()`](https://openair-project.github.io/openair/reference/selectByDate.md),
which is used for selecting particular date ranges quickly and easily.
See examples below.

## See also

Other time series and trend functions:
[`TheilSen()`](https://openair-project.github.io/openair/reference/TheilSen.md),
[`calendarPlot()`](https://openair-project.github.io/openair/reference/calendarPlot.md),
[`smoothTrend()`](https://openair-project.github.io/openair/reference/smoothTrend.md),
[`timeProp()`](https://openair-project.github.io/openair/reference/timeProp.md),
[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md),
[`trendLevel()`](https://openair-project.github.io/openair/reference/trendLevel.md)

## Author

David Carslaw

Jack Davison

## Examples

``` r
# basic use, single pollutant
timePlot(mydata, pollutant = "nox")


# two pollutants in separate panels
if (FALSE) { # \dontrun{
timePlot(mydata, pollutant = c("nox", "no2"))

# two pollutants in the same panel with the same scale
timePlot(mydata, pollutant = c("nox", "no2"), group = TRUE)

# alternative by normalising concentrations and plotting on the same scale
timePlot(
  mydata,
  pollutant = c("nox", "co", "pm10", "so2"),
  group = TRUE,
  avg.time = "year",
  normalise = "1/1/1998",
  lwd = 3,
  lty = 1
)

# examples of selecting by date

# plot for nox in 1999
timePlot(selectByDate(mydata, year = 1999), pollutant = "nox")

# select specific date range for two pollutants
timePlot(
  selectByDate(mydata, start = "6/8/2003", end = "13/8/2003"),
  pollutant = c("no2", "o3")
)

# choose different line styles etc
timePlot(mydata, pollutant = c("nox", "no2"), lty = 1)

# choose different line styles etc
timePlot(
  selectByDate(mydata, year = 2004, month = 6),
  pollutant = c("nox", "no2"),
  lwd = c(1, 2),
  col = "black"
)

# different averaging times

# daily mean O3
timePlot(mydata, pollutant = "o3", avg.time = "day")

# daily mean O3 ensuring each day has data capture of at least 75%
timePlot(mydata, pollutant = "o3", avg.time = "day", data.thresh = 75)

# 2-week average of O3 concentrations
timePlot(mydata, pollutant = "o3", avg.time = "2 week")
} # }
```
