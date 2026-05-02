# Pollution rose variation of the traditional wind rose plot

The traditional wind rose plot that plots wind speed and wind direction
by different intervals. The pollution rose applies the same plot
structure but substitutes other measurements, most commonly a pollutant
time series, for wind speed.

## Usage

``` r
pollutionRose(
  mydata,
  pollutant = "nox",
  key.title = pollutant,
  key.position = "right",
  breaks = 6,
  paddle = FALSE,
  seg = 0.9,
  normalise = FALSE,
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame containing fields `ws` and `wd`

- pollutant:

  Mandatory. A pollutant name corresponding to a variable in a data
  frame should be supplied e.g. `pollutant = "nox"`.

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

- breaks:

  Most commonly, the number of break points for pollutant
  concentrations. The default, 6, attempts to breaks the supplied data
  at approximately 6 sensible break points. However, `breaks` can also
  be used to set specific break points. For example, the argument
  `breaks = c(0, 1, 10, 100)` breaks the data into segments \<1, 1-10,
  10-100, \>100.

- paddle:

  Either `TRUE` or `FALSE`. If `TRUE` plots rose using 'paddle' style
  spokes. If `FALSE` plots rose using 'wedge' style spokes.

- seg:

  `seg` determines with width of the segments. For example, `seg = 0.5`
  will produce segments 0.5 \* `angle`.

- normalise:

  If `TRUE` each wind direction segment is normalised to equal one. This
  is useful for showing how the concentrations (or other parameters)
  contribute to each wind sector when the proportion of time the wind is
  from that direction is low. A line showing the probability that the
  wind directions is from a particular wind sector is also shown.

- plot:

  When `openair` plots are created they are automatically printed to the
  active graphics device. `plot = FALSE` deactivates this behaviour.
  This may be useful when the plot *data* is of more interest, or the
  plot is required to appear later (e.g., later in a Quarto document, or
  to be saved to a file).

- key:

  Deprecated; please use `key.position`. If `FALSE`, sets `key.position`
  to `"none"`.

- ...:

  Other arguments passed on to
  [`windRose()`](https://openair-project.github.io/openair/reference/windRose.md).

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object. Summarised proportions can be extracted directly using the
`$data` operator, e.g. `object$data` for `output <- windRose(mydata)`.
This returns a data frame with three set columns: `cond`, conditioning
based on `type`; `wd`, the wind direction; and `calm`, the `statistic`
for the proportion of data unattributed to any specific wind direction
because it was collected under calm conditions; and then several (one
for each range binned for the plot) columns giving proportions of
measurements associated with each `ws` or `pollutant` range plotted as a
discrete panel.

## Details

`pollutionRose()` is a
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)
wrapper which brings `pollutant` forward in the argument list, and
attempts to sensibly rescale break points based on the `pollutant` data
range by by-passing `ws.int`.

By default, `pollutionRose()` will plot a pollution rose of `nox` using
"wedge" style segments and placing the scale key to the right of the
plot.

It is possible to compare two wind speed-direction data sets using
`pollutionRose()`. There are many reasons for doing so e.g. to see how
one site compares with another or for meteorological model evaluation.
In this case, `ws` and `wd` are considered to the the reference data
sets with which a second set of wind speed and wind directions are to be
compared (`ws2` and `wd2`). The first set of values is subtracted from
the second and the differences compared. If for example, `wd2` was
biased positive compared with `wd` then `pollutionRose` will show the
bias in polar coordinates. In its default use, wind direction bias is
colour-coded to show negative bias in one colour and positive bias in
another.

## See also

Other polar directional analysis functions:
[`percentileRose()`](https://openair-project.github.io/openair/reference/percentileRose.md),
[`polarAnnulus()`](https://openair-project.github.io/openair/reference/polarAnnulus.md),
[`polarCluster()`](https://openair-project.github.io/openair/reference/polarCluster.md),
[`polarDiff()`](https://openair-project.github.io/openair/reference/polarDiff.md),
[`polarFreq()`](https://openair-project.github.io/openair/reference/polarFreq.md),
[`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md),
[`windRose()`](https://openair-project.github.io/openair/reference/windRose.md)

## Examples

``` r
# pollutionRose of nox
pollutionRose(mydata, pollutant = "nox")


# source apportionment plot - contribution to mean
if (FALSE) { # \dontrun{
pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")

# example of comparing 2 met sites
# first we will make some new ws/wd data with a postive bias
mydata$ws2 <- mydata$ws + 2 * rnorm(nrow(mydata)) + 1
mydata$wd2 <- mydata$wd + 30 * rnorm(nrow(mydata)) + 30

# need to correct negative wd
id <- which(mydata$wd2 < 0)
mydata$wd2[id] <- mydata$wd2[id] + 360

# results show postive bias in wd and ws
pollutionRose(mydata, ws = "ws", wd = "wd", ws2 = "ws2", wd2 = "wd2")

## add some wd bias to some nighttime hours
id <- which(as.numeric(format(mydata$date, "%H")) %in% c(23, 1, 2, 3, 4, 5))
mydata$wd2[id] <- mydata$wd[id] + 30 * rnorm(length(id)) + 120
id <- which(mydata$wd2 < 0)
mydata$wd2[id] <- mydata$wd2[id] + 360

pollutionRose(
  mydata,
  ws = "ws",
  wd = "wd",
  ws2 = "ws2",
  wd2 = "wd2",
  breaks = c(-11, -2, -1, -0.5, 0.5, 1, 2, 11),
  cols = c("dodgerblue4", "white", "firebrick"),
  type = "daylight"
)
} # }
```
