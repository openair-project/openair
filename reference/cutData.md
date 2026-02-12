# Function to split data in different ways for conditioning

Utility function to split data frames up in various ways for
conditioning plots. Widely used by many `openair` functions usually
through the option `type`.

## Usage

``` r
cutData(
  x,
  type = "default",
  names = NULL,
  suffix = NULL,
  hemisphere = "northern",
  n.levels = 4,
  start.day = 1,
  start.season = "spring",
  is.axis = FALSE,
  local.tz = NULL,
  latitude = 51,
  longitude = -0.5,
  drop = c("default", "empty", "outside", "none"),
  ...
)
```

## Arguments

- x:

  A data frame containing a field `date`.

- type:

  A string giving the way in which the data frame should be split.
  Pre-defined values are: `"default"`, `"year"`, `"hour"`, `"month"`,
  `"season"`, `"weekday"`, `"site"`, `"weekend"`, `"monthyear"`,
  `"daylight"`, `"dst"` (daylight saving time).

  `type` can also be the name of a numeric or factor. If a numeric
  column name is supplied `cutData()` will split the data into four
  quantiles. Factors levels will be used to split the data without any
  adjustment.

- names:

  By default, the columns created by `cutData()` are named after their
  `type` option. Specifying `names` defines other names for the columns,
  which map onto the `type` options in the same order they are given.
  The length of `names` should therefore be equal to the length of
  `type`.

- suffix:

  If `name` is not specified, `suffix` will be appended to any added
  columns that would otherwise overwrite existing columns. For example,
  `cutData(mydata, "nox", suffix = "_cuts")` would append a `nox_cuts`
  column rather than overwriting `nox`.

- hemisphere:

  Can be `"northern"` or `"southern"`, used to split data into seasons.

- n.levels:

  Number of quantiles to split numeric data into.

- start.day:

  What day of the week should the `type = "weekday"` start on? The user
  can change the start day by supplying an integer between 0 and 6.
  Sunday = 0, Monday = 1, ... For example to start the weekday plots on
  a Saturday, choose `start.day = 6`.

- start.season:

  What order should the season be. By default, the order is spring,
  summer, autumn, winter. `start.season = "winter"` would plot winter
  first.

- is.axis:

  A logical (`TRUE`/`FALSE`), used to request shortened cut labels for
  axes.

- local.tz:

  Used for identifying whether a date has daylight savings time (DST)
  applied or not. Examples include `local.tz = "Europe/London"`,
  `local.tz = "America/New_York"`, i.e., time zones that assume DST.
  <https://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones> shows time
  zones that should be valid for most systems. It is important that the
  original data are in GMT (UTC) or a fixed offset from GMT.

- latitude, longitude:

  The decimal latitude and longitudes used when `type = "daylight"`.
  Note that locations west of Greenwich have negative longitudes.

- drop:

  How to handle empty factor levels. One of:

  - `"default"`: Sensible defaults selected on a case-by-case basis for
    different `type` options.

  - `"empty"`: Drop all empty factor levels.

  - `"none"`: Retain all empty factor levels, where possible. For
    example, for `type = "hour"`, all factor levels from `0` and `23`
    will be represented.

  - `"outside"`: Retain empty factor levels within the range of the
    data. For example, for `type = "hour"` when the data only contains
    data for 1 AM and 5 AM, the factor levels, `1`, `2`, `3`, `4` and
    `5` will be retained.

  Some of these options only apply to certain `type` options. For
  example, for `type = "year"`, `"outside"` is equivalent to `"none"` as
  there is no fixed range of years to use in the `"none"` case.

- ...:

  All additional parameters are passed on to next function(s).

## Value

Returns the data frame, `x`, with columns appended as defined by `type`
and `name`.

## Details

This section give a brief description of each of the define levels of
`type`. Note that all time dependent types require a column `date`.

- `"default"` does not split the data but will describe the levels as a
  date range in the format "day month year".

- `"year"` splits the data by each year.

- `"month"` splits the data by month of the year.

- `"hour"` splits the data by hour of the day.

- `"monthyear"` splits the data by year and month. It differs from month
  in that a level is defined for each month of the data set. This is
  useful sometimes to show an ordered sequence of months if the data set
  starts half way through a year; rather than starting in January.

- `"weekend"` splits the data by weekday and weekend.

- `"weekday"` splits the data by day of the week - ordered to start
  Monday.

- `"season"` splits data up by season. In the northern hemisphere winter
  = December, January, February; spring = March, April, May etc. These
  definitions will change of `hemisphere = "southern"`.

- `"seasonyear"` (or `"yearseason"`) will split the data into
  year-season intervals, keeping the months of a season together. For
  example, December 2010 is considered as part of winter 2011 (with
  January and February 2011). This makes it easier to consider
  contiguous seasons. In contrast, `type = "season"` will just split the
  data into four seasons regardless of the year.

- `"daylight"` splits the data relative to estimated sunrise and sunset
  to give either daylight or nighttime. The cut is made by `cutDaylight`
  but more conveniently accessed via `cutData`, e.g.
  `cutData(mydata, type = "daylight", latitude = my.latitude, longitude = my.longitude)`.
  The daylight estimation, which is valid for dates between 1901 and
  2099, is made using the measurement location, date, time and
  astronomical algorithms to estimate the relative positions of the Sun
  and the measurement location on the Earth's surface, and is based on
  NOAA methods. Measurement location should be set using `latitude` (+
  to North; - to South) and `longitude` (+ to East; - to West).

- `"dst"` will split the data by hours that are in daylight saving time
  (DST) and hours that are not for appropriate time zones. The option
  also requires that the local time zone is given e.g.
  `local.tz = "Europe/London"`, `local.tz = "America/New_York"`. Each of
  the two periods will be in *local time*. The main purpose of this
  option is to test whether there is a shift in the diurnal profile when
  DST and non-DST hours are compared. This option is particularly useful
  with the
  [`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md)
  function. For example, close to the source of road vehicle emissions,
  "rush-hour" will tend to occur at the same *local time* throughout the
  year, e.g., 8 am and 5 pm. Therefore, comparing non-DST hours with DST
  hours will tend to show similar diurnal patterns (at least in the
  timing of the peaks, if not magnitude) when expressed in local time.
  By contrast a variable such as wind speed or temperature should show a
  clear shift when expressed in local time. In essence, this option when
  used with
  [`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md)
  may help determine whether the variation in a pollutant is driven by
  man-made emissions or natural processes.

- `"wd"` splits the data by 8 wind sectors and requires a column `wd`:
  "NE", "E", "SE", "S", "SW", "W", "NW", "N".

Note that all the date-based types, e.g., `"month"`/`"year"` are derived
from a column `date`. If a user already has a column with a name of one
of the date-based types it will not be used.

## Author

David Carslaw

Jack Davison

Karl Ropkins (`"daylight"` option)

## Examples

``` r
# split data by day of the week
mydata <- cutData(mydata, type = "weekday")
names(mydata)
#>  [1] "date"    "ws"      "wd"      "nox"     "no2"     "o3"      "pm10"   
#>  [8] "so2"     "co"      "pm25"    "weekday"
head(mydata)
#> # A tibble: 6 × 11
#>   date                   ws    wd   nox   no2    o3  pm10   so2    co  pm25
#>   <dttm>              <dbl> <int> <int> <int> <int> <int> <dbl> <dbl> <int>
#> 1 1998-01-01 00:00:00  0.6    280   285    39     1    29  4.72  3.37    NA
#> 2 1998-01-01 01:00:00  2.16   230    NA    NA    NA    37 NA    NA       NA
#> 3 1998-01-01 02:00:00  2.76   190    NA    NA     3    34  6.83  9.60    NA
#> 4 1998-01-01 03:00:00  2.16   170   493    52     3    35  7.66 10.2     NA
#> 5 1998-01-01 04:00:00  2.4    180   468    78     2    34  8.07  8.91    NA
#> 6 1998-01-01 05:00:00  3      190   264    42     0    16  5.50  3.05    NA
#> # ℹ 1 more variable: weekday <ord>
```
