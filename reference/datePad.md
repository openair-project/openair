# Pad a time-series dataframe and optionally fill values by block

Expand a dataframe that contains a 'date' column to a regular sequence
of timestamps between specified start and end dates. The function can
operate in two modes:

- fill = FALSE: simply complete the sequence at the target interval.

- fill = TRUE: regularise the data at the native interval to create
  explicit blocks, then expand to the target interval and carry the
  block's values forward so that intra-block timestamps inherit the
  block's measured value (block-filling behaviour).

## Usage

``` r
datePad(
  mydata,
  type = NULL,
  interval = NULL,
  start.date = NULL,
  end.date = NULL,
  fill = FALSE,
  print.int = FALSE
)
```

## Arguments

- mydata:

  Data.frame or tibble containing at least a 'date' column (Date or
  POSIXt).

- type:

  NULL or character vector of column names to group by.

- interval:

  NULL or character string describing target interval (e.g. "1 min", "1
  hour"). If NULL, the native interval is used.

- start.date:

  Optional start date/time. If NULL, the group's minimum date is used.

- end.date:

  Optional end date/time. If NULL, the group's maximum date is used.

- fill:

  Logical; when TRUE performs block-based filling described above. When
  FALSE just completes the sequence leaving NA values.

- print.int:

  Logical; when TRUE prints detected/selected interval messages.

## Value

A dataframe expanded to the requested sequence with values filled
according to 'fill'. The returned object preserves the 'date' column
type and timezone (for POSIXt).

## Details

The function detects the native input interval automatically if
'interval' is not supplied, supports grouping via 'type', and preserves
timezones for POSIXt date columns.

## Examples

``` r
df <- mydata[-c(2, 4, 7), ] # Remove some rows to create gaps
datePad(df)
#> # A tibble: 65,533 × 10
#>    date                   ws    wd   nox   no2    o3  pm10   so2    co  pm25
#>    <dttm>              <dbl> <int> <int> <int> <int> <int> <dbl> <dbl> <int>
#>  1 1998-01-01 00:00:00  0.6    280   285    39     1    29  4.72  3.37    NA
#>  2 1998-01-01 01:00:00 NA       NA    NA    NA    NA    NA NA    NA       NA
#>  3 1998-01-01 02:00:00  2.76   190    NA    NA     3    34  6.83  9.60    NA
#>  4 1998-01-01 03:00:00 NA       NA    NA    NA    NA    NA NA    NA       NA
#>  5 1998-01-01 04:00:00  2.4    180   468    78     2    34  8.07  8.91    NA
#>  6 1998-01-01 05:00:00  3      190   264    42     0    16  5.50  3.05    NA
#>  7 1998-01-01 06:00:00 NA       NA    NA    NA    NA    NA NA    NA       NA
#>  8 1998-01-01 07:00:00  3      170   195    51     0    12  3.88  2.00    NA
#>  9 1998-01-01 08:00:00  3.36   170   137    42     1    12  3.35  1.46    NA
#> 10 1998-01-01 09:00:00  3.96   170   113    39     2    12  2.92  1.20    NA
#> # ℹ 65,523 more rows
```
