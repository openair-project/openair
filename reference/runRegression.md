# Rolling regression for pollutant source characterisation.

This function calculates rolling regressions for input data with a set
window width. The principal use of the function is to identify "dilution
lines" where the ratio between two pollutant concentrations is
invariant. The original idea is based on the work of Bentley (2004).

## Usage

``` r
runRegression(mydata, x = "nox", y = "pm10", run.len = 3, date.pad = TRUE)
```

## Arguments

- mydata:

  A data frame with columns for `date` and at least two variables for
  use in a regression.

- x:

  The column name of the `x` variable for use in a linear regression
  `y = m.x + c`.

- y:

  The column name of the `y` variable for use in a linear regression
  `y = m.x + c`.

- run.len:

  The window width to be used for a rolling regression. A value of 3 for
  example for hourly data will consider 3 one-hour time sequences.

- date.pad:

  Should gaps in time series be filled before calculations are made?

## Value

A tibble with `date` and calculated regression coefficients and other
information to plot dilution lines.

## Details

The intended use is to apply the approach to air pollution data to
extract consecutive points in time where the ratio between two pollutant
concentrations changes by very little. By filtering the output for high
R2 values (typically more than 0.90 to 0.95), conditions where local
source dilution is dominant can be isolated for post processing. The
function is more fully described and used in the `openair` online
manual, together with examples.

## References

For original inspiration:

Bentley, S. T. (2004). Graphical techniques for constraining estimates
of aerosol emissions from motor vehicles using air monitoring network
data. Atmospheric Environment,(10), 1491–1500.
https://doi.org/10.1016/j.atmosenv.2003.11.033

Example for vehicle emissions high time resolution data:

Farren, N. J., Schmidt, C., Juchem, H., Pöhler, D., Wilde, S. E.,
Wagner, R. L., Wilson, S., Shaw, M. D., & Carslaw, D. C. (2023).
Emission ratio determination from road vehicles using a range of remote
emission sensing techniques. Science of The Total Environment, 875.
https://doi.org/10.1016/j.scitotenv.2023.162621.

## See also

Other time series and trend functions:
[`TheilSen()`](https://openair-project.github.io/openair/reference/TheilSen.md),
[`calendarPlot()`](https://openair-project.github.io/openair/reference/calendarPlot.md),
[`smoothTrend()`](https://openair-project.github.io/openair/reference/smoothTrend.md),
[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md),
[`timeProp()`](https://openair-project.github.io/openair/reference/timeProp.md),
[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md),
[`trendLevel()`](https://openair-project.github.io/openair/reference/trendLevel.md)

## Examples

``` r
# Just use part of a year of data
output <- runRegression(selectByDate(mydata, year = 2004, month = 1:3),
  x = "nox", y = "pm10", run.len = 3
)

output
#> # A tibble: 2,067 × 12
#>    date                date_start          date_end            intercept   slope
#>    <dttm>              <dttm>              <dttm>                  <dbl>   <dbl>
#>  1 2004-01-01 01:00:00 2004-01-01 00:00:00 2004-01-01 02:00:00    47.4   -0.199 
#>  2 2004-01-01 02:00:00 2004-01-01 01:00:00 2004-01-01 03:00:00     2.15   0.0996
#>  3 2004-01-01 03:00:00 2004-01-01 02:00:00 2004-01-01 04:00:00     1.87   0.0898
#>  4 2004-01-01 04:00:00 2004-01-01 03:00:00 2004-01-01 05:00:00     2.39   0.0897
#>  5 2004-01-01 05:00:00 2004-01-01 04:00:00 2004-01-01 06:00:00   -10.6    0.297 
#>  6 2004-01-01 06:00:00 2004-01-01 05:00:00 2004-01-01 07:00:00    20     -0.167 
#>  7 2004-01-01 07:00:00 2004-01-01 06:00:00 2004-01-01 08:00:00     8.76   0.0129
#>  8 2004-01-01 08:00:00 2004-01-01 07:00:00 2004-01-01 09:00:00     5.81   0.0596
#>  9 2004-01-01 09:00:00 2004-01-01 08:00:00 2004-01-01 10:00:00     0.108  0.113 
#> 10 2004-01-01 10:00:00 2004-01-01 09:00:00 2004-01-01 11:00:00    -5.59   0.149 
#> # ℹ 2,057 more rows
#> # ℹ 7 more variables: r_squared <dbl>, nox_1 <int>, nox_2 <int>, pm10_1 <dbl>,
#> #   pm10_2 <dbl>, delta_nox <int>, delta_pm10 <dbl>
```
