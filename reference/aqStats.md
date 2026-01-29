# Calculate summary statistics for air pollution data by year

This function calculates a range of common and air pollution-specific
statistics from a data frame. The statistics are calculated on an annual
basis and the input is assumed to be hourly data. The function can cope
with several sites and years, e.g., using `type = "site"`. The user can
control the output by setting `transpose` appropriately. Note that the
input data is assumed to be in mass units, e.g., ug/m3 for all species
except CO (mg/m3).

## Usage

``` r
aqStats(
  mydata,
  pollutant = "no2",
  type = "default",
  data.thresh = 0,
  percentile = c(95, 99),
  transpose = FALSE,
  progress = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame containing a `date` field of hourly data.

- pollutant:

  The name of a pollutant e.g. `pollutant = c("o3", "pm10")`. Additional
  statistics will be calculated if
  `pollutant %in% c("no2", "pm10", "o3")`.

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

- percentile:

  Percentile values to calculate for each pollutant.

- transpose:

  The default is to return a data frame with columns representing the
  statistics. If `transpose = TRUE` then the results have columns for
  each pollutant-type combination.

- progress:

  Show a progress bar when many groups make up `type`? Defaults to
  `TRUE`.

- ...:

  Other arguments, currently unused.

## Details

The following statistics are calculated:

For all pollutants:

- **data.capture** — percentage data capture over a full year.

- **mean** — annual mean.

- **minimum** — minimum hourly value.

- **maximum** — maximum hourly value.

- **median** — median value.

- **max.daily** — maximum daily mean.

- **max.rolling.8** — maximum 8-hour rolling mean.

- **max.rolling.24** — maximum 24-hour rolling mean.

- **percentile.95** — 95th percentile. Note that several percentiles can
  be calculated.

When `pollutant == "o3"`:

- **roll.8.O3.gt.100** — number of days when the daily maximum rolling
  8-hour mean ozone concentration is \>100 ug/m3. This is the target
  value.

- **roll.8.O3.gt.120** — number of days when the daily maximum rolling
  8-hour mean ozone concentration is \>120 ug/m3. This is the Limit
  Value not to be exceeded \> 10 days a year.

- **AOT40** — is the accumulated amount of ozone over the threshold
  value of 40 ppb for daylight hours in the growing season (April to
  September). Note that `latitude` and `longitude` can also be passed to
  this calculation.

When `pollutant == "no2"`:

- **hours** — number of hours NO2 is more than 200 ug/m3.

When `pollutant == "pm10"`:

- **days** — number of days PM10 is more than 50 ug/m3.

For the rolling means, the user can supply the option `align`, which can
be "centre" (default), "left" or "right". See
[`rollingMean()`](https://openair-project.github.io/openair/reference/rollingMean.md)
for more details.

There can be small discrepancies with the AURN due to the treatment of
rounding data. The `aqStats()` function does not round, whereas AURN
data can be rounded at several stages during the calculations.

## Author

David Carslaw

## Examples

``` r
# Statistics for 2004. NOTE! these data are in ppb/ppm so the
# example is for illustrative purposes only
aqStats(selectByDate(mydata, year = 2004), pollutant = "no2")
#> # A tibble: 1 × 17
#>   default   pollutant  year date                dat.cap  mean   min   max median
#>   <fct>     <chr>     <dbl> <dttm>                <dbl> <dbl> <int> <int>  <dbl>
#> 1 01 Janua… no2        2004 2004-01-01 00:00:00    99.8  55.0     0   185     51
#> # ℹ 8 more variables: max_daily <dbl>, roll_8_max <dbl>, n_value.x <int>,
#> #   roll_24_max <dbl>, n_value.y <int>, percentile.95 <dbl>,
#> #   percentile.99 <dbl>, hours <int>
```
