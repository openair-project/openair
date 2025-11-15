# Function to extract run lengths greater than a threshold

This is a utility function to extract runs of values above a certain
threshold. For example, for a data frame of hourly NOx values we would
like to extract all those hours where the concentration is at least 500
for contiguous periods of 5 or more hours.

## Usage

``` r
selectRunning(
  mydata,
  pollutant = "nox",
  criterion = ">",
  run.len = 5L,
  threshold = 500,
  type = "default",
  name = "criterion",
  result = c("yes", "no"),
  mode = c("flag", "filter"),
  ...
)
```

## Arguments

- mydata:

  A data frame with a `date` field and at least one numeric `pollutant`
  field to analyse.

- pollutant:

  Name of variable to process.

- criterion:

  Condition to select run lengths e.g. `">"` with select data more than
  `threshold`.

- run.len:

  Run length for extracting contiguous values of `pollutant` meeting the
  `criterion` in relation to the `threshold`.

- threshold:

  The threshold value for `pollutant` above which data should be
  extracted.

- type:

  Used for splitting the data further. Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

- name:

  The name of the column to be appended to the data frame when
  `mode = "flag"`.

- result:

  A vector of length 2, defining how to label the run lengths when
  `mode = "flag"`. The first object should be the label for the `TRUE`
  label, and the second the `FALSE` label - e.g., `c("yes", "no")`.

- mode:

  Changes how the function behaves. When `mode = "flag"`, the default,
  the function appends a column flagging where the criteria was met.
  Alternatively, `"filter"` will filter `mydata` to only return rows
  where the criteria was met.

- ...:

  Additional parameters passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
  For use with `type`.

## Value

A data frame

## Details

This function is useful, for example, for selecting pollution episodes
from a data frame where concentrations remain elevated for a certain
period of time. It may also be of more general use when analysing air
pollution and atmospheric composition data. For example,
`selectRunning()` could be used to extract continuous periods of
rainfall — which could be important for particle concentrations.

## Author

David Carslaw

## Examples

``` r
# extract those hours where there are at least 5 consecutive NOx
# concentrations above 500 units
mydata <- selectRunning(mydata, run.len = 5, threshold = 500)

# make a polar plot of those conditions, which shows that those
# conditions are dominated by low wind speeds, not
# in-canyon recirculation
if (FALSE) { # \dontrun{
polarPlot(mydata, pollutant = "nox", type = "criterion")
} # }
```
