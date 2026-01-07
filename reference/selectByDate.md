# Subset a data frame based on date

Utility function to filter a data frame by a date range or specific date
periods (month, year, etc.). All options are applied in turn, meaning
this function can be used to select quite complex dates simply.

## Usage

``` r
selectByDate(
  mydata,
  start = "1/1/2008",
  end = "31/12/2008",
  year = 2008,
  month = 1,
  day = "weekday",
  hour = 1
)
```

## Arguments

- mydata:

  A data frame containing a `date` field in Date or POSIXct format.

- start:

  A start date or date-time string in the form d/m/yyyy, m/d/yyyy,
  d/m/yyyy HH:MM, m/d/yyyy HH:MM, d/m/yyyy HH:MM:SS, m/d/yyyy HH:MM:SS,
  yyyy-mm-dd, yyyy-mm-dd HH:MM or yyyy-mm-dd HH:MM:SS.

- end:

  See `start` for format.

- year:

  A year or years to select e.g. `year = 1998:2004` to select 1998-2004
  inclusive or `year = c(1998, 2004)` to select 1998 and 2004.

- month:

  A month or months to select. Can either be numeric e.g. `month = 1:6`
  to select months 1-6 (January to June), or by name e.g.
  `month = c("January", "December")`. Names can be abbreviated to 3
  letters and be in lower or upper case.

- day:

  A day name or or days to select. `day` can be numeric (1 to 31) or
  character. For example `day = c("Monday", "Wednesday")` or
  `day = 1:10` (to select the 1st to 10th of each month). Names can be
  abbreviated to 3 letters and be in lower or upper case. Also accepts
  `"weekday"` (Monday - Friday) and `"weekend"` for convenience.

- hour:

  An hour or hours to select from 0-23 e.g. `hour = 0:12` to select
  hours 0 to 12 inclusive.

## Author

David Carslaw

## Examples

``` r
## select all of 1999
data.1999 <- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999 23:00")
head(data.1999)
#> # A tibble: 6 × 10
#>   date                   ws    wd   nox   no2    o3  pm10   so2    co  pm25
#>   <dttm>              <dbl> <int> <int> <int> <int> <int> <dbl> <dbl> <int>
#> 1 1999-01-01 00:00:00  5.04   140    88    35     4    21  3.84 1.02     18
#> 2 1999-01-01 01:00:00  4.08   160   132    41     3    17  5.24 2.7      11
#> 3 1999-01-01 02:00:00  4.8    160   168    40     4    17  6.51 2.87      8
#> 4 1999-01-01 03:00:00  4.92   150    85    36     3    15  4.18 1.62     10
#> 5 1999-01-01 04:00:00  4.68   150    93    37     3    16  4.25 1.02     11
#> 6 1999-01-01 05:00:00  3.96   160    74    29     5    14  3.88 0.725    NA
tail(data.1999)
#> # A tibble: 6 × 10
#>   date                   ws    wd   nox   no2    o3  pm10   so2    co  pm25
#>   <dttm>              <dbl> <int> <int> <int> <int> <int> <dbl> <dbl> <int>
#> 1 1999-12-31 18:00:00  4.68   190   226    39    NA    29  5.46  2.38    23
#> 2 1999-12-31 19:00:00  3.96   180   202    37    NA    27  4.78  2.15    23
#> 3 1999-12-31 20:00:00  3.36   190   246    44    NA    30  5.88  2.45    23
#> 4 1999-12-31 21:00:00  3.72   220   231    35    NA    28  5.28  2.22    23
#> 5 1999-12-31 22:00:00  4.08   200   217    41    NA    31  4.79  2.17    26
#> 6 1999-12-31 23:00:00  3.24   200   181    37    NA    28  3.48  1.78    22

# or...
data.1999 <- selectByDate(mydata, start = "1999-01-01", end = "1999-12-31 23:00")

# easier way
data.1999 <- selectByDate(mydata, year = 1999)


# more complex use: select weekdays between the hours of 7 am to 7 pm
sub.data <- selectByDate(mydata, day = "weekday", hour = 7:19)

# select weekends between the hours of 7 am to 7 pm in winter (Dec, Jan, Feb)
sub.data <- selectByDate(mydata,
  day = "weekend", hour = 7:19, month =
    c("dec", "jan", "feb")
)
```
