# Function to calculate time averages for data frames

Function to flexibly aggregate or expand data frames by different time
periods, calculating vector-averaged wind direction where appropriate.
The averaged periods can also take account of data capture rates.

## Usage

``` r
timeAverage(
  mydata,
  avg.time = "day",
  data.thresh = 0,
  statistic = "mean",
  type = "default",
  percentile = NA,
  start.date = NA,
  end.date = NA,
  interval = NA,
  vector.ws = FALSE,
  fill = FALSE,
  progress = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame containing a `date` field . Can be class `POSIXct` or
  `Date`.

- avg.time:

  This defines the time period to average to. Can be `"sec"`, `"min"`,
  `"hour"`, `"day"`, `"DSTday"`, `"week"`, `"month"`, `"quarter"` or
  `"year"`. For much increased flexibility a number can precede these
  options followed by a space. For example, a timeAverage of 2 months
  would be `period = "2 month"`. In addition, `avg.time` can equal
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
  between successive intervals so that `timeAverage()` can work out how
  much 'padding' to apply. To pad-out data in this way choose
  `fill = TRUE`.

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

- type:

  `type` allows `timeAverage()` to be applied to cases where there are
  groups of data that need to be split and the function applied to each
  group. The most common example is data with multiple sites identified
  with a column representing site name e.g. `type = "site"`. More
  generally, `type` should be used where the date repeats for a
  particular grouping variable. However, if type is not supplied the
  data will still be averaged but the grouping variables (character or
  factor) will be dropped.

- percentile:

  The percentile level used when `statistic = "percentile"`. The default
  is 95%.

- start.date:

  A string giving a start date to use. This is sometimes useful if a
  time series starts between obvious intervals. For example, for a
  1-minute time series that starts `2009-11-29 12:07:00` that needs to
  be averaged up to 15-minute means, the intervals would be
  `2009-11-29 12:07:00`, `2009-11-29 12:22:00`, etc. Often, however, it
  is better to round down to a more obvious start point, e.g.,
  `2009-11-29 12:00:00` such that the sequence is then
  `2009-11-29 12:00:00`, `2009-11-29 12:15:00`, and so on. `start.date`
  is therefore used to force this type of sequence. Note that this
  option does not truncate a time series if it already starts earlier
  than `start.date`; see
  [`selectByDate()`](https://openair-project.github.io/openair/reference/selectByDate.md)
  for that functionality.

- end.date:

  A string giving an end date to use. This is sometimes useful to make
  sure a time series extends to a known end point and is useful when
  `data.thresh > 0` but the input time series does not extend up to the
  final full interval. For example, if a time series ends sometime in
  October but annual means are required with a data capture of \>75 %
  then it is necessary to extend the time series up until the end of the
  year. Input in the format yyyy-mm-dd HH:MM. Note that this option does
  not truncate a time series if it already ends later than `end.date`;
  see
  [`selectByDate()`](https://openair-project.github.io/openair/reference/selectByDate.md)
  for that functionality.

- interval:

  The `timeAverage()` function tries to determine the interval of the
  original time series (e.g. hourly) by calculating the most common
  interval between time steps. The interval is needed for calculations
  where the `data.thresh` \>0. For the vast majority of regular time
  series this works fine. However, for data with very poor data capture
  or irregular time series the automatic detection may not work. Also,
  for time series such as monthly time series where there is a variable
  difference in time between months users should specify the time
  interval explicitly e.g. `interval = "month"`. Users can also supply a
  time interval to *force* on the time series. See `avg.time` for the
  format.

  This option can sometimes be useful with `start.date` and `end.date`
  to ensure full periods are considered e.g. a full year when
  `avg.time = "year"`.

- vector.ws:

  Should vector averaging be carried out on wind speed if available? The
  default is `FALSE` and scalar averages are calculated. Vector
  averaging of the wind speed is carried out on the u and v wind
  components. For example, consider the average of two hours where the
  wind direction and speed of the first hour is 0 degrees and 2m/s and
  180 degrees and 2m/s for the second hour. The scalar average of the
  wind speed is simply the arithmetic average = 2m/s and the vector
  average is 0m/s. Vector-averaged wind speeds will always be lower than
  scalar-averaged values.

- fill:

  When time series are expanded, i.e., when a time interval is less than
  the original time series, data are 'padded out' with `NA`. To
  'pad-out' the additional data with the first row in each original time
  interval, choose `fill = TRUE`.

- progress:

  Show a progress bar when many groups make up `type`? Defaults to
  `TRUE`.

- ...:

  Additional arguments for other functions calling `timeAverage()`.

## Value

Returns a data frame with date in class `POSIXct`.

## Details

This function calculates time averages for a data frame. It also treats
wind direction correctly through vector-averaging. For example, the
average of 350 degrees and 10 degrees is either 0 or 360 - not 180. The
calculations therefore average the wind components.

When a data capture threshold is set through `data.thresh` it is
necessary for `timeAverage()` to know what the original time interval of
the input time series is. The function will try and calculate this
interval based on the most common time gap (and will print the assumed
time gap to the screen). This works fine most of the time but there are
occasions where it may not e.g. when very few data exist in a data frame
or the data are monthly (i.e. non-regular time interval between months).
In this case the user can explicitly specify the interval through
`interval` in the same format as `avg.time` e.g. `interval = "month"`.
It may also be useful to set `start.date` and `end.date` if the time
series do not span the entire period of interest. For example, if a time
series ended in October and annual means are required, setting
`end.date` to the end of the year will ensure that the whole period is
covered and that `data.thresh` is correctly calculated. The same also
goes for a time series that starts later in the year where `start.date`
should be set to the beginning of the year.

`timeAverage()` should be useful in many circumstances where it is
necessary to work with different time average data. For example, hourly
air pollution data and 15-minute meteorological data. To merge the two
data sets `timeAverage()` can be used to make the meteorological data
1-hour means first. Alternatively, `timeAverage()` can be used to expand
the hourly data to 15 minute data - see example below.

For the research community `timeAverage()` should be useful for dealing
with outputs from instruments where there are a range of time periods
used.

It is also very useful for plotting data using
[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md).
Often the data are too dense to see patterns and setting different
averaging periods easily helps with interpretation.

## See also

[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md)
that plots time series data and uses `timeAverage()` to aggregate data
where necessary.

[`calcPercentile()`](https://openair-project.github.io/openair/reference/calcPercentile.md)
that wraps `timeAverage()` to allow multiple percentiles to be
calculated at once.

## Author

David Carslaw

## Examples

``` r
# daily average values
daily <- timeAverage(mydata, avg.time = "day")

# daily average values ensuring at least 75 % data capture
# i.e., at least 18 valid hours
if (FALSE) { # \dontrun{
daily <- timeAverage(mydata, avg.time = "day", data.thresh = 75)
} # }

# 2-weekly averages
if (FALSE) { # \dontrun{
fortnight <- timeAverage(mydata, avg.time = "2 week")
} # }

# make a 15-minute time series from an hourly one
if (FALSE) { # \dontrun{
min15 <- timeAverage(mydata, avg.time = "15 min", fill = TRUE)
} # }

# average by grouping variable
if (FALSE) { # \dontrun{
dat <- importAURN(c("kc1", "my1"), year = 2011:2013)
timeAverage(dat, avg.time = "year", type = "site")

# can also retain site code
timeAverage(dat, avg.time = "year", type = c("site", "code"))

# or just average all the data, dropping site/code
timeAverage(dat, avg.time = "year")
} # }
```
