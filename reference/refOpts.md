# Define `ref.x` or `ref.y` options for `openair` plots

This function provides a convenient way to set default options for
`ref.x` or `ref.y` layers in `openair` plots, which show some form of
horizontal or vertical reference line or band. `intercept` can be a
vector of any length (for lines) or an even numbered vector representing
pairs of bounds (for bands); all other arguments besides `label` will be
recycled to be the correct length. Use `method` to change between lines
and bands.

## Usage

``` r
refOpts(
  intercept,
  alpha = 1,
  colour = "black",
  linetype = 1,
  linewidth = 0.6,
  label = NULL,
  label_size = 10,
  label_colour = NULL,
  color = NULL,
  label_color = NULL,
  method = c("line", "band")
)
```

## Arguments

- intercept:

  The axis intercept(s) for the reference lines. Should be numeric,
  dates, or date-times depending on the axis types. If another data type
  is provided, it will attempt to be coerced to the correct type using
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html),
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)
  or
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html),
  respectively.

- alpha:

  Numeric value between `0` and `1` specifying the transparency of the
  lines. Default is `1` (fully opaque) for lines and `0.1` for bands.

- colour, color:

  Colour of the lines and/or bands. Default is `"black"`. `colour` and
  `color` are interchangeable, but `colour` is used preferentially if
  both are given.

- linetype:

  Line type. Can be an integer (e.g., `1` for solid, `2` for dashed) or
  a string (e.g., `"solid"`, `"dashed"`). Default is `1` (solid).

- linewidth:

  Numeric value specifying the width of the lines. Default is `1` for
  lines and `0` for bands.

- label, label_size, label_colour, label_color:

  `label` takes character string to add a direct label to the reference
  line. For `ref.x` this will be on the right hand side of the plot, and
  for `ref.y` this will be on top. `label_size` and `label_colour` set
  label aesthetics, with the latter defaulting to `colour` if not set.

- method:

  One of `"line"` or `"band"`. The former will create any number of
  horizontal or vertical reference lines at `intercept` values. The
  latter will use pairs of `intercept` values to create any number of
  horizontal or vertical shaded areas/bands.

## Value

A list of options that can be passed to the `ref.x` or `ref.y` arguments
of functions like
[`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md).

## Examples

``` r
# `ref.y` can just be a value to plot
timePlot(mydata, avg.time = "month", ref.y = 250, ref.x = "2002/01/01")


# use the `refOpts()` function to customise reference lines
timePlot(
  mydata,
  avg.time = "month",
  ref.y = refOpts(
    c(250, 300),
    alpha = c(0.5, 1),
    colour = c("grey50", "blue"),
    linetype = c(2, 1),
    linewidth = c(1, 2)
  )
)


# use the 'bands' method for shaded areas
timePlot(
  mydata,
  avg.time = "month",
  ref.y = refOpts(
    c(200, 225, 250, 275),
    colour = c("purple", "green"),
    method = "band"
  )
)
```
