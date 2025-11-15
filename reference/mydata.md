# Example air quality monitoring data for openair

The `mydata` dataset is provided as an example dataset as part of the
openair package. The dataset contains hourly measurements of air
pollutant concentrations, wind speed and wind direction collected at the
Marylebone (London) air quality monitoring supersite between 1st January
1998 and 23rd June 2005.

## Usage

``` r
mydata
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
65533 rows and 10 columns.

## Source

`mydata` was compiled from data archived in the London Air Quality
Archive. See <https://londonair.org.uk> for site details.

## Details

- date:

  Observation date/time stamp in year-month-day hour:minute:second
  format (POSIXct).

- ws:

  Wind speed, in m/s, as numeric vector.

- wd:

  Wind direction, in degrees from North, as a numeric vector.

- nox:

  Oxides of nitrogen concentration, in ppb, as a numeric vector.

- no2:

  Nitrogen dioxide concentration, in ppb, as a numeric vector.

- o3:

  Ozone concentration, in ppb, as a numeric vector.

- pm10:

  Particulate PM10 fraction measurement, in ug/m3 (raw TEOM), as a
  numeric vector.

- so2:

  Sulfur dioxide concentration, in ppb, as a numeric vector.

- co:

  Carbon monoxide concentration, in ppm, as a numeric vector.

- pm25:

  Particulate PM2.5 fraction measurement, in ug/m3, as a numeric vector.

## Note

[openair](https://openair-project.github.io/openair/reference/openair-package.md)
functions generally require data frames with a field "date" that can be
in either `POSIXct` or `Date` format

## Examples

``` r
# basic structure
head(mydata)
#> # A tibble: 6 × 10
#>   date                   ws    wd   nox   no2    o3  pm10   so2    co  pm25
#>   <dttm>              <dbl> <int> <int> <int> <int> <int> <dbl> <dbl> <int>
#> 1 1998-01-01 00:00:00  0.6    280   285    39     1    29  4.72  3.37    NA
#> 2 1998-01-01 01:00:00  2.16   230    NA    NA    NA    37 NA    NA       NA
#> 3 1998-01-01 02:00:00  2.76   190    NA    NA     3    34  6.83  9.60    NA
#> 4 1998-01-01 03:00:00  2.16   170   493    52     3    35  7.66 10.2     NA
#> 5 1998-01-01 04:00:00  2.4    180   468    78     2    34  8.07  8.91    NA
#> 6 1998-01-01 05:00:00  3      190   264    42     0    16  5.50  3.05    NA
```
