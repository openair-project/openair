# Calculate percentile values from a time series

Calculates multiple percentile values from a time series, with flexible
time aggregation. This function is a wrapper for
[`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md),
making it easier to calculate several percentiles at once. Like
[`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md),
it requires a data frame with a `date` field and one other numeric
variable.

## Usage

``` r
calcPercentile(
  mydata,
  pollutant = "o3",
  avg.time = "month",
  percentile = 50,
  type = "default",
  data.thresh = 0,
  start.date = NA,
  end.date = NA,
  prefix = "percentile."
)
```

## Arguments

- mydata:

  A data frame containing a `date` field . Can be class `POSIXct` or
  `Date`.

- pollutant:

  Name of column containing variable to summarise, likely a pollutant
  (e.g., `"o3"`).

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

- percentile:

  A vector of percentile values; for example, `percentile = 50` will
  calculate median values. Multiple values may also be provided as a
  vector, e.g., `percentile = c(5, 50, 95)` or
  `percentile = seq(0, 100, 10)`.

- type:

  `type` allows
  [`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md)
  to be applied to cases where there are groups of data that need to be
  split and the function applied to each group. The most common example
  is data with multiple sites identified with a column representing site
  name e.g. `type = "site"`. More generally, `type` should be used where
  the date repeats for a particular grouping variable. However, if type
  is not supplied the data will still be averaged but the grouping
  variables (character or factor) will be dropped.

- data.thresh:

  The data capture threshold to use (%). A value of zero means that all
  available data will be used in a particular period regardless if of
  the number of values available. Conversely, a value of 100 will mean
  that all data will need to be present for the average to be
  calculated, else it is recorded as `NA`. See also `interval`,
  `start.date` and `end.date` to see whether it is advisable to set
  these other options.

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

- prefix:

  Each new column is named by appending a `prefix` to `percentile`. For
  example, the default `"percentile."` will name the new column as
  `percentile.95` when `percentile = 95`.

## Value

Returns a `data.frame` with a `date` column plus an additional column
for each given `percentile`.

## See also

[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md),
[`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md)

## Author

David Carslaw

## Examples

``` r
# 95th percentile monthly o3 concentrations
percentiles <- calcPercentile(mydata,
  pollutant = "o3",
  avg.time = "month", percentile = 95
)

head(percentiles)
#> # A tibble: 6 × 2
#>   date                percentile.95
#>   <dttm>                      <dbl>
#> 1 1998-01-01 00:00:00            14
#> 2 1998-02-01 00:00:00             7
#> 3 1998-03-01 00:00:00            15
#> 4 1998-04-01 00:00:00            20
#> 5 1998-05-01 00:00:00            25
#> 6 1998-06-01 00:00:00            15

# 5, 50, 95th percentile monthly o3 concentrations
if (FALSE) { # \dontrun{
percentiles <- calcPercentile(mydata,
  pollutant = "o3",
  avg.time = "month", percentile = c(5, 50, 95)
)

head(percentiles)
} # }
```
