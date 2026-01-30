# Calculate rolling Gaussian smooth of pollutant values

This is a utility function designed to calculate rolling Gaussian smooth
(kernel smoothing). The function will try and fill in missing time gaps
to get a full time sequence but return a data frame with the same number
of rows supplied.

## Usage

``` r
GaussianSmooth(
  mydata,
  pollutant = "o3",
  sigma = 24L,
  type = "default",
  data.thresh = 75,
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

- sigma:

  The value of `sigma` to use in the Gaussian.

- type:

  Used for splitting the data further. Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

- data.thresh:

  The % data capture threshold. No values are calculated if data capture
  over the period of interest is less than this value.

- new.name:

  The name given to the new column. If not supplied it will create a
  name based on the name of the pollutant and the averaging period used.

- ...:

  Additional parameters passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  For use with `type`.

## Value

A tibble with two new columns for the Gaussian smooth value.

## Details

The function provides centre-aligned smoothing out to 3 sigma, which
captures 99.7% of the data.

## Author

David Carslaw

## Examples

``` r
# Gaussian smoother with sigma = 24
mydata <- GaussianSmooth(mydata,
  pollutant = "o3", sigma = 24, data.thresh = 75)
```
