# Calculate Whittaker-Eilers Smoothing, Interpolation and Baseline Determination

This function applies the Whittaker-Eilers smoothing and interpolation
method to a specified pollutant in a data frame. The method is based on
penalised least squares and is designed to handle time series data with
missing values, providing a smoothed estimate of the pollutant
concentrations over time. The function allows for flexible control over
the amount of smoothing through the `lambda` parameter and can be
applied to multiple pollutants simultaneously.

## Usage

``` r
WhittakerSmooth(
  mydata,
  pollutant = "o3",
  lambda = 24L,
  d = 2,
  type = "default",
  new.name = NULL,
  date.pad = FALSE,
  p = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame containing a `date` field. `mydata` must contain a `date`
  field in `Date` or `POSIXct` format.

- pollutant:

  The name of a pollutant, e.g., `pollutant = "o3"`. More than one
  pollutant can be supplied as a vector, e.g.,
  `pollutant = c("o3", "nox")`.

- lambda:

  The value of `lambda` to use in the smoothing. This controls the
  amount of smoothing, with higher values leading to smoother results.
  If `lambda = NA` Generalised Cross Validation (GCV) is used to select
  the optimal value of `lambda` for each pollutant. This can be time
  consuming, so a fixed value of `lambda` is recommended for large
  datasets or multiple pollutants. Note that the value of `lambda` needs
  to increase exponetially to smooth long time series data of several
  years e.g. `lambda = 10e9`.

- d:

  The order used to penalise the roughness of the data. By default this
  is set to 2, which penalises the second derivative of the data.
  Setting `d = 1` will penalise the first derivative, which can be
  useful for smoothing data with sharp peaks or troughs. Setting `d = 1`
  will effectively linearly interpolate across missing data.

- type:

  Used for splitting the data further. Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

- new.name:

  The name given to the new column(s). If not supplied it will create a
  name based on the name of the pollutant.

- date.pad:

  Should missing dates be padded? Default is `FALSE`.

- p:

  The asymmetry weight parameter used exclusively for baseline
  estimation (Asymmetric Least Squares). It defines how the algorithm
  treats points that fall above the fitted line versus points that fall
  below it. It takes a value between 0 and 1. When p is very small, the
  algorithm assigns a massive penalty to the curve if it rises above the
  data points, but almost no penalty if it drops below them. This forces
  the curve to "hug" the bottom of the signal, effectively ignoring the
  positive peaks. Typical Values: 0.01 to 0.05.

- ...:

  Additional parameters passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  For use with `type`.

## Value

A tibble with new columns for the smoothed pollutant values.

## Details

In addition to smoothing, the function can also perform baseline
estimation using Asymmetric Least Squares (ALS) when the `p` parameter
is provided. This allows for the separation of the underlying baseline
from the observed data, which can be particularly useful for identifying
trends or correcting for background levels in pollutant concentrations.

The function is designed to work with regularly spaced time series.

## References

Paul H. C. Eilers, A Perfect Smoother, Analytical Chemistry 2003 75
(14), 3631-3636, DOI: 10.1021/ac034173t

## Author

David Carslaw

## Examples

``` r
# Smoothing with lambda = 24
mydata <- WhittakerSmooth(mydata, pollutant = "o3", lambda = 24)
```
