# Define `breaks` options for `openair` plots

This function provides a convenient way to set default options for
`breaks` arguments in `openair` plots, which cut continuous colour
scales into discrete bins.

## Usage

``` r
breakOpts(
  breaks = NULL,
  labels = NULL,
  method = c("number", "interval", "width", "pretty", "wd"),
  max.bins = NULL,
  include.lowest = NULL,
  right = NULL,
  dig.lab = NULL
)
```

## Arguments

- breaks, labels:

  If a categorical colour scale is required, `breaks` should be
  specified. This can be either of:

  - A single value, which will use the strategy specified by `method` to
    bin the scale. By default, this uses the same logic as
    [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md),
    which splits the scale into quantiles.

  - A numeric vector, which will define the specific breakpoints. For
    example, `c(0, 50, 100)` will bin the data into `0 to 50`,
    `50 to 100`, and so on. If `breaks` does not cover the full range of
    the data, the outer limits will be extended so that the full colour
    scale is covered while retaining the desired number of breaks.

  By default, `breaks` will generate nicely formatted labels for each
  category. The `labels` argument overrides this - for example, a user
  could define `breaks = 3, labels = c("low", "medium", "high")`. Care
  should be taken to provide the appropriate number of `labels` - it
  should always be equal to the number of bins, or one less than the
  number of breakpoints.

- method:

  When `breaks` is a single number (e.g., `breaks = 5`), `method`
  controls how this value is used to create breaks. Can be one of:

  - `"number"` - splits the range into *n* quantiles (i.e., bins with
    roughly the same number of measurements in them). This is equivalent
    to
    [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

  - `"interval"` - splits the range into *n* bins of equal width.

  - `"width"` - splits the range into some number of bins with width
    *n*.

  - `"pretty"` - splits the range into *approximately* *n* bins with
    aesthetically pleasing breakpoints using
    [`pretty()`](https://rdrr.io/r/base/pretty.html).

  - `"wd"` - splits the range into wind direction bins by passing it to
    [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
    Note that this option requires `breaks` to be one of `4`, `8`, `16`
    or `32` and ignores all other arguments.

- max.bins:

  If `max.bins` is set, only that number of bins will ever be created.
  For example, if `breaks = c(0, 10, 20, 30, 40, 50)` and
  `max.bins = 3`, `breaks` will be set to `c(0, 10, 20, 50)` to ensure
  only three bins are made.

- include.lowest:

  logical, indicating if an `x[i]` equal to the lowest (or highest, for
  `right = FALSE`) 'breaks' value should be included.

- right:

  logical, indicating if the intervals should be closed on the right
  (and open on the left) or vice versa.

- dig.lab:

  Integer which is used when labels are not given. It determines the
  number of digits used in formatting the break numbers. If `NULL`, the
  labels will use the minimum number of decimal places possible to
  ensure the breakpoints are unique, dropping all trailing zeroes.
