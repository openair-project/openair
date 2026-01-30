# Calculate rolling quantile pollutant values

This is a utility function mostly designed to calculate rolling quantile
statistics. The function will try and fill in missing time gaps to get a
full time sequence but return a data frame with the same number of rows
supplied.

## Usage

``` r
rollingQuantile(
  mydata,
  pollutant = "o3",
  width = 8L,
  type = "default",
  data.thresh = 75,
  align = c("centre", "center", "left", "right"),
  probs = 0.5,
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

- probs:

  Probability for quantile calculate. A number between 0 and 1. Can be
  more than length one e.g. `probs = c(0.05, 0.95)`.

- ...:

  Additional parameters passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  For use with `type`.

## Value

A tibble with new columns for the rolling quantile value and the number
of valid values used.

## Author

David Carslaw

## Examples

``` r
# rolling 24-hour 0.05 and 0.95 quantile for ozone
mydata <- rollingQuantile(mydata,
  pollutant = "o3", width = 24, data.thresh = 75, align = "right", probs = c(0.05, 0.95)
)
```
