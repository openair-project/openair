# Plot time series values in a conventional calendar format

This function will plot data by month laid out in a conventional
calendar format. The main purpose is to help rapidly visualise
potentially complex data in a familiar way. Users can also choose to
show daily mean wind vectors if wind speed and direction are available.

## Usage

``` r
calendarPlot(
  mydata,
  pollutant = "nox",
  year = NULL,
  month = NULL,
  annotate = "date",
  statistic = "mean",
  data.thresh = 0,
  percentile = NA,
  cols = "heat",
  limits = NULL,
  lim = NULL,
  col.lim = c("grey30", "black"),
  col.arrow = "black",
  col.na = "white",
  font.lim = c(1, 2),
  cex.lim = c(0.6, 0.9),
  cex.date = 0.6,
  digits = 0,
  labels = NULL,
  breaks = NULL,
  w.shift = 0,
  w.abbr.len = 1,
  remove.empty = TRUE,
  show.year = TRUE,
  key.header = statistic,
  key.footer = pollutant,
  key.position = "right",
  key = TRUE,
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

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox". `

- year:

  Year to plot e.g. `year = 2003`. If not supplied and `mydata` contains
  more than one year, the first year of the data will be automatically
  selected. Manually setting `year` to `NULL` will use all available
  years.

- month:

  If only certain month are required. By default the function will plot
  an entire year even if months are missing. To only plot certain months
  use the `month` option where month is a numeric 1:12 e.g.
  `month = c(1, 12)` to only plot January and December.

- annotate:

  This option controls what appears on each day of the calendar. Can be:

  - `"date"` — shows day of the month

  - `"wd"` — shows vector-averaged wind direction

  - `"ws"` — shows vector-averaged wind direction scaled by wind speed

  - `"value"` — shows the daily mean value

- statistic:

  Statistic passed to
  [`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md).
  Note that if `statistic %in% c("max", "min")` and `annotate` is "ws"
  or "wd", the hour corresponding to the maximum/minimum concentration
  of `polluant` is used to provide the associated `ws` or `wd` and not
  the maximum/minimum daily `ws` or `wd`.

- data.thresh:

  The data capture threshold to use (%). A value of zero means that all
  available data will be used in a particular period regardless if of
  the number of values available. Conversely, a value of 100 will mean
  that all data will need to be present for the average to be
  calculated, else it is recorded as `NA`. See also `interval`,
  `start.date` and `end.date` to see whether it is advisable to set
  these other options.

- percentile:

  The percentile level in percent used when `statistic = "percentile"`
  and when aggregating the data with `avg.time`. More than one
  percentile level is allowed for `type = "default"` e.g.
  `percentile = c(50, 95)`. Not used if `avg.time = "default"`.

- cols:

  Colours to be used for plotting; see
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for details.

- limits:

  Use this option to manually set the colour scale limits. This is
  useful in the case when there is a need for two or more plots and a
  consistent scale is needed on each. Set the limits to cover the
  maximum range of the data for all plots of interest. For example, if
  one plot had data covering 0–60 and another 0–100, then set
  `limits = c(0, 100)`. Note that data will be ignored if outside the
  limits range.

- lim:

  A threshold value to help differentiate values above and below `lim`.
  It is used when `annotate = "value"`. See next few options for control
  over the labels used.

- col.lim:

  For the annotation of concentration labels on each day. The first sets
  the colour of the text below `lim` and the second sets the colour of
  the text above `lim`.

- col.arrow:

  The colour of the annotated wind direction / wind speed arrows.

- col.na:

  Colour to be used to show missing data.

- font.lim:

  For the annotation of concentration labels on each day. The first sets
  the font of the text below `lim` and the second sets the font of the
  text above `lim`. Note that font = 1 is normal text and font = 2 is
  bold text.

- cex.lim:

  For the annotation of concentration labels on each day. The first sets
  the size of the text below `lim` and the second sets the size of the
  text above `lim`.

- cex.date:

  The base size of the annotation text for the date.

- digits:

  The number of digits used to display concentration values when
  `annotate = "value"`.

- labels:

  If a categorical scale is defined using `breaks`, then `labels` can be
  used to override the default category labels, e.g.,
  `labels = c("good", "bad", "very bad")`. Note there is one less label
  than break.

- breaks:

  If a categorical scale is required then these breaks will be used. For
  example, `breaks = c(0, 50, 100, 1000)`. In this case "good"
  corresponds to values between 0 and 50 and so on. Users should set the
  maximum value of `breaks` to exceed the maximum data value to ensure
  it is within the maximum final range e.g. 100–1000 in this case.

- w.shift:

  Controls the order of the days of the week. By default the plot shows
  Saturday first (`w.shift = 0`). To change this so that it starts on a
  Monday for example, set `w.shift = 2`, and so on.

- w.abbr.len:

  The default (`1`) abbreviates the days of the week to a single letter
  (e.g., in English, S/S/M/T/W/T/F). `w.abbr.len` defines the number of
  letters to abbreviate until. For example, `w.abbr.len = 3` will
  abbreviate "Monday" to "Mon".

- remove.empty:

  Should months with no data present be removed? Default is `TRUE`.

- show.year:

  If only a single year is being plotted, should the calendar labels
  include the year label? `TRUE` creates labels like "January-2000",
  `FALSE` labels just as "January". If multiple years of data are
  detected, this option is forced to be `TRUE`.

- key.header, key.footer:

  Used to control the title of the plot key. By default, `key.header` is
  the `statistic` and `key.footer` is the `pollutant`. These are pasted
  together to form the key title.

- key.position:

  Location where the scale key is to plotted. Allowed arguments
  currently include `"top"`, `"right"`, `"bottom"` and `"left"`.

- key:

  Fine control of the scale key via
  [`drawOpenKey()`](https://openair-project.github.io/openair/reference/drawOpenKey.md).
  See
  [`drawOpenKey()`](https://openair-project.github.io/openair/reference/drawOpenKey.md)
  for further details.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the '2' in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  The following additional arguments are available:

  - `xlab`, `ylab` and `main` override the x-axis label, y-axis label,
    and plot title.

  - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have
    2 columns and 5 rows.

  - `fontsize` overrides the overall font size of the plot.

  - `border` sets the border colour of each tile.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Details

`calendarPlot()` will plot data in a conventional calendar format, i.e.,
by month and day of the week. Daily statistics are calculated using
[`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md),
which by default will calculate the daily mean concentration.

If wind direction is available it is then possible to plot the wind
direction vector on each day. This is very useful for getting a feel for
the meteorological conditions that affect pollutant concentrations. Note
that if hourly or higher time resolution are supplied, then
`calendarPlot()` will calculate daily averages using
[`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md),
which ensures that wind directions are vector-averaged.

If wind speed is also available, then setting the option
`annotate = "ws"` will plot the wind vectors whose length is scaled to
the wind speed. Thus information on the daily mean wind speed and
direction are available.

It is also possible to plot categorical scales. This is useful where,
for example, an air quality index defines concentrations as bands, e.g.,
"good", "poor". In these cases users must supply `labels` and
corresponding `breaks`.

Note that is is possible to pre-calculate concentrations in some way
before passing the data to `calendarPlot()`. For example
[`rollingMean()`](https://openair-project.github.io/openair/reference/rollingMean.md)
could be used to calculate rolling 8-hour mean concentrations. The data
can then be passed to `calendarPlot()` and `statistic = "max"` chosen,
which will plot maximum daily 8-hour mean concentrations.

## See also

Other time series and trend functions:
[`TheilSen()`](https://openair-project.github.io/openair/reference/TheilSen.md),
[`smoothTrend()`](https://openair-project.github.io/openair/reference/smoothTrend.md),
[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md),
[`timeProp()`](https://openair-project.github.io/openair/reference/timeProp.md),
[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md),
[`trendLevel()`](https://openair-project.github.io/openair/reference/trendLevel.md)

## Author

David Carslaw

## Examples

``` r
# basic plot
calendarPlot(mydata, pollutant = "o3", year = 2003)


# show wind vectors
calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "wd")
#> Warning: Removed 139 rows containing non-finite outside the scale range
#> (`stat_windflow()`).

if (FALSE) { # \dontrun{
# show wind vectors scaled by wind speed and different colours
calendarPlot(mydata,
  pollutant = "o3", year = 2003, annotate = "ws",
  cols = "heat"
)

# show only specific months with selectByDate
calendarPlot(selectByDate(mydata, month = c(3, 6, 10), year = 2003),
  pollutant = "o3", year = 2003, annotate = "ws", cols = "heat"
)

# categorical scale example
calendarPlot(mydata,
  pollutant = "no2", breaks = c(0, 50, 100, 150, 1000),
  labels = c("Very low", "Low", "High", "Very High"),
  cols = c("lightblue", "green", "yellow", "red"), statistic = "max"
)

# UK daily air quality index
pm10.breaks <- c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000)
calendarPlot(mydata, "pm10",
  year = 1999, breaks = pm10.breaks,
  labels = c(1:10), cols = "daqi", statistic = "mean", key.header = "DAQI"
)
} # }
```
