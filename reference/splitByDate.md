# Divide up a data frame by time

This function partitions a data frame up into different time segments.
It produces a new column called controlled by `name` that can be used in
many `openair` functions. Note that there must be one more `labels` than
there are `dates`.

## Usage

``` r
splitByDate(
  mydata,
  dates = "1/1/2003",
  labels = c("before", "after"),
  name = "split.by",
  format = c("%d/%m/%Y", "%Y/%m/%d", "%d/%m/%Y %H:%M:%S",
    "%Y/%m/%d %H:%M:%S")
)
```

## Arguments

- mydata:

  A data frame containing a `date` field in an hourly or high resolution
  format.

- dates:

  A date or dates to split data by. Can be passed as R date(time)
  objects or as characters. If passed as a character, `splitByDate()`
  expects either "DD/MM/YYYY" or "YYYY/MM/DD" by default, but this can
  be chaned using the `format` argument.

- labels:

  Labels for each time partition. Should always be one more `label` than
  there are `dates`; for example, if `dates = "2020/01/01`,
  `splitByDate()` requires one label for *before* that date and one
  label for *after*.

- name:

  The name to give the new column to identify the periods split.
  Defaults to `"split.by"`.

- format:

  When `dates` are provided as character strings, this option defines
  the formats `splitByDate()` will use to coerce `dates` into R `Date`
  or `POSIXCt` objects. Passed to
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)
  or
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html).
  See [`strptime()`](https://rdrr.io/r/base/strptime.html) for more
  information.

## Author

David Carslaw

## Examples

``` r
# split data up into "before" and "after"
mydata <- splitByDate(mydata,
  dates = "1/04/2000",
  labels = c("before", "after")
)

# split data into 3 partitions
mydata <- splitByDate(mydata,
  dates = c("1/1/2000", "1/3/2003"),
  labels = c("before", "during", "after")
)

# if you have modelled data - could split into modelled and measured by the
# break date
dummy <- data.frame(
  date = Sys.Date() + (-5:5),
  nox = 100 + seq(-50, 50, 10)
)
splitByDate(dummy,
  dates = Sys.Date(),
  labels = c("measured", "modelled"),
  name = "data_type"
)
#>          date nox data_type
#> 1  2026-02-19  50  measured
#> 2  2026-02-20  60  measured
#> 3  2026-02-21  70  measured
#> 4  2026-02-22  80  measured
#> 5  2026-02-23  90  measured
#> 6  2026-02-24 100  measured
#> 7  2026-02-25 110  modelled
#> 8  2026-02-26 120  modelled
#> 9  2026-02-27 130  modelled
#> 10 2026-02-28 140  modelled
#> 11 2026-03-01 150  modelled
```
