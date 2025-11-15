# Calculate rolling mean pollutant values

This is a utility function mostly designed to calculate rolling mean
statistics relevant to some pollutant limits, e.g., 8 hour rolling means
for ozone and 24 hour rolling means for PM10. However, the function has
a more general use in helping to display rolling mean values in flexible
ways with the rolling window width left, right or centre aligned. The
function will try and fill in missing time gaps to get a full time
sequence but return a data frame with the same number of rows supplied.

## Usage

``` r
rollingMean(
  mydata,
  pollutant = "o3",
  width = 8L,
  type = "default",
  data.thresh = 75,
  align = c("centre", "center", "left", "right"),
  new.name = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame containing a `date` field. `mydata` must contain a `date`
  field in `Date` or `POSIXct` format. The input time series must be
  regular, e.g., hourly, daily.

- pollutant:

  The name of a pollutant, e.g., `pollutant = "o3"`.

- width:

  The averaging period (rolling window width) to use, e.g., `width = 8`
  will generate 8-hour rolling mean values when hourly data are
  analysed.

- type:

  Used for splitting the data further. Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

- data.thresh:

  The % data capture threshold. No values are calculated if data capture
  over the period of interest is less than this value. For example, with
  `width = 8` and `data.thresh = 75` at least 6 hours are required to
  calculate the mean, else `NA` is returned.

- align:

  Specifies how the moving window should be aligned. `"right"` means
  that the previous hours (including the current) are averaged. `"left"`
  means that the forward hours are averaged. `"centre"` (or `"center"` -
  the default) centres the current hour in the window.

- new.name:

  The name given to the new column. If not supplied it will create a
  name based on the name of the pollutant and the averaging period used.

- ...:

  Additional parameters passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  For use with `type`.

## Author

David Carslaw

## Examples

``` r
# rolling 8-hour mean for ozone
mydata <- rollingMean(mydata,
  pollutant = "o3", width = 8, new.name =
    "rollingo3", data.thresh = 75, align = "right"
)
```
