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
  type = "month",
  statistic = "mean",
  data.thresh = 0,
  percentile = NA,
  annotate = "date",
  windflow = NULL,
  cols = "heat",
  limits = NULL,
  lim = NULL,
  col.lim = c("grey30", "black"),
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
  key.title = paste(statistic, pollutant, sep = " "),
  key.position = "right",
  strip.position = "top",
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame of time series. Must include a `date` field and at least
  one variable to plot.

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`.

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

- type:

  `type` determines how the data are split, i.e., conditioned, and then
  plotted. Only one type can be used with this function, as one faceting
  'direction' is reserved by the month of the year. If a single `type`
  is given, it will form the "rows" of the resulting grid.
  Alternatively, `c(type, "month")` can be used can be specified for
  `type` to be used as the "columns" instead.

  `type = "year"` is a special case for `calendarPlot()` and will
  automatically prevent a single year from being selected (unless
  specified using the `year` argument) and set `show.year` to `FALSE`.

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

- annotate:

  This option controls what appears on each day of the calendar. Can be:

  - `"date"` — shows day of the month

  - `"value"` — shows the daily mean value

  - `"none"` — shows no label

- windflow:

  If `TRUE`, the vector-averaged wind speed and direction will be
  plotted using arrows. Alternatively, can be a list of arguments to
  control the appearance of the arrows (colour, linewidth, alpha value,
  etc.). See
  [`windflowOpts()`](https://openair-project.github.io/openair/reference/windflowOpts.md)
  for details.

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

- breaks, labels:

  If a categorical colour scale is required then `breaks` should be
  specified. These should be provided as a numeric vector, e.g.,
  `breaks = c(0, 50, 100, 1000)`. Users should set the maximum value of
  `breaks` to exceed the maximum data value to ensure it is within the
  maximum final range, e.g., 100–1000 in this case. Labels will
  automatically be generated, but can be customised by passing a
  character vector to `labels`, e.g.,
  `labels = c("good", "bad", "very bad")`. In this example, `0 - 50`
  will be `"good"` and so on. Note there is one less label than break.

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
[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md)

## Author

David Carslaw

## Examples

``` r
# basic plot
calendarPlot(mydata, pollutant = "o3", year = 2003)


# show wind vectors
calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "wd")
#> Warning: ! `annotate` in `openair::calendarPlot()` no longer supports `'ws'` or `'wd'`.
#> ℹ Please use the `windflow` argument instead for more thorough control over the
#>   apperance of the 'windflow' arrow.
#> ℹ Setting `windflow` to TRUE.

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
calendarPlot(
  mydata,
  "pm10",
  year = 1999,
  breaks = pm10.breaks,
  labels = c(1:10),
  cols = "daqi",
  statistic = "mean",
  key.title = "PM10 DAQI"
)
} # }
```
