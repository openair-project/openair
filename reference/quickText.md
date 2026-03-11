# Automatic text formatting for openair

Workhorse function that automatically applies routine text formatting to
common expressions and data names used in openair.

## Usage

``` r
quickText(text, auto.text = TRUE, ...)
```

## Arguments

- text:

  A character vector.

- auto.text:

  A logical option. The default, `TRUE`, applies `quickText()` to `text`
  and returns the result. The alternative, `FALSE`, returns `text`
  unchanged. (A number of `openair` functions enable/disable
  `quickText()` using this option).

- ...:

  Not used.

## Value

The function returns an expression for graphical evaluation.

## Details

`quickText()` is routine formatting lookup table. It screens the
supplied character vector `text` and automatically applies formatting to
any recognised character sub-series. The function is used in a number of
`openair` functions and can also be used directly by users to format
text components of their own graphs (see below).

## Author

Karl Ropkins

David Carslaw

Jack Davison

## Examples

``` r
# see axis formatting in an openair plot, e.g.:
scatterPlot(
  mydata,
  x = "no2",
  y = "pm10"
)


# using quickText in other plots
plot(
  mydata$no2,
  mydata$pm10,
  xlab = quickText("my no2 label"),
  ylab = quickText("pm10 [ ug.m-3 ]")
)
```
