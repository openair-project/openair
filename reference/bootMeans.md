# Bin data, calculate mean and bootstrap confidence interval in the mean

`binData()` summarises data by intervals and calculates the mean and
bootstrap confidence intervals (by default 95% CI) in the mean of a
chosen variable in a data frame. Any other numeric variables are
summarised by their mean intervals. This occurs via `bootMeanDF()`,
which calculates the uncertainty intervals in the mean of a vector.

## Usage

``` r
binData(
  mydata,
  bin = "nox",
  uncer = "no2",
  type = "default",
  n = 40,
  interval = NA,
  breaks = NA,
  conf.int = 0.95,
  B = 250,
  ...
)

bootMeanDF(x, conf.int = 0.95, B = 1000)
```

## Arguments

- mydata:

  Name of the data frame to process.

- bin:

  The name of the column to divide into intervals.

- uncer:

  The name of the column for which the mean, lower and upper
  uncertainties should be calculated for each interval of `bin`.

- type:

  Used for splitting the data further. Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  Note that intervals are calculated on the whole dataset before the
  data is categorised, meaning intervals will be the same for the
  different groups.

- n:

  The number of intervals to split `bin` into.

- interval:

  The interval to be used for binning the data.

- breaks:

  User specified breaks to use for binning.

- conf.int:

  The confidence interval, defaulting to `0.95` (i.e., the 95%
  Confidence Interval).

- B:

  The number of bootstrap simulations.

- ...:

  Other parameters that are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md),
  for use with `type`.

- x:

  A vector from which the mean and bootstrap confidence intervals in the
  mean are to be calculated

## Value

Returns a summarised data frame with new columns for the mean and upper
/ lower confidence intervals in the mean.

## Details

There are three options for binning. The default is to bin `bin` into 40
intervals. Second, the user can choose an binning `interval`, e.g.,
`interval = 5`. Third, the user can supply their own `breaks` to use as
binning intervals. Note that intervals are calculated on the whole
dataset before the data is cut into categories using `type`.

## Examples

``` r
# work with vectors
test <- rnorm(20, mean = 10)
bootMeanDF(test)
#>          mean      min      max  n
#> Mean 9.689424 9.321958 10.11362 20

# how does nox vary by intervals of wind speed?
results <- binData(mydata, bin = "ws", uncer = "nox")
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(results, aes(x = ws, y = mean, ymin = min, ymax = max)) +
  geom_pointrange()
} # }

# what about weekend vs weekday?
results2 <- binData(mydata, bin = "ws", uncer = "nox", type = "weekend")
if (FALSE) { # \dontrun{
ggplot(results2, aes(x = ws, y = mean, ymin = min, ymax = max)) +
  geom_pointrange() +
  facet_wrap(vars(weekend))
} # }
```
