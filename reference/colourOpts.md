# Define `cols` options for `openair` plots

This function provides a convenient way to set options for customising
colours in openair plots. It returns a list of options that can be
passed to the `cols` argument of most functions. All arguments are the
same as
[`openColours()`](https://openair-project.github.io/openair/reference/openColours.md),
but passing `colourOpts()` allows each function to set an appropriate
`n` value and overwrite certain options if required. `colourOpts()` and
`colorOpts()` are synonyms.

## Usage

``` r
colourOpts(scheme = "default", alpha = 1, begin = 0, end = 1, direction = 1)

colorOpts(scheme = "default", alpha = 1, begin = 0, end = 1, direction = 1)
```

## Arguments

- scheme:

  Any one of the pre-defined `openair` schemes (e.g., `"increment"`) or
  a user-defined palette (e.g., `c("red", "orange", "gold")`). See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for a full list of available schemes.

- alpha:

  The alpha transparency level (between `0` and `1`) for the colours.
  `0` is fully transparent, and `1` is fully opaque.

- begin, end:

  For sequential schemes, the fraction (between `0` and `1`) of the
  colour scheme to use. For example, if `begin = 0.2` and `end = 0.8`,
  only the middle 60% of the colour scheme will be used. This can be
  useful for avoiding very light or dark colours at the ends of schemes.

- direction:

  The order of the colours. `1` is the default and gives the normal
  order. `-1` will reverse the order of the colours.

## Value

A list of options that can be passed to the `cols` argument of plotting
functions like
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md).

## Examples

``` r
trendLevel(
  mydata,
  "no2",
  cols = colourOpts("viridis", direction = 1, alpha = 0.5)
)
```
