# Function to rapidly provide an overview of air quality data

This function provides a quick graphical and numerical summary of data.
The location presence/absence of data are shown, with summary statistics
and plots of variable distributions. `summaryPlot()` can also provide
summaries of a single pollutant across many sites.

## Usage

``` r
summaryPlot(
  mydata,
  na.len = 24,
  clip = TRUE,
  percentile = 0.99,
  type = "histogram",
  pollutant = "nox",
  period = "years",
  avg.time = "day",
  print.datacap = TRUE,
  breaks = NULL,
  plot.type = "l",
  col.trend = "darkgoldenrod2",
  col.data = "lightblue",
  col.mis = "#A60A12",
  col.hist = "forestgreen",
  cols = NULL,
  date.breaks = 7,
  auto.text = TRUE,
  plot = TRUE,
  debug = FALSE,
  ...
)
```

## Arguments

- mydata:

  A data frame to be summarised. Must contain a `date` field and at
  least one other parameter.

- na.len:

  Missing data are only shown with at least `na.len` *contiguous*
  missing vales. The purpose of setting `na.len` is for clarity: with
  long time series it is difficult to see where individual missing hours
  are. Furthermore, setting `na.len = 96`, for example would show where
  there are at least 4 days of continuous missing data.

- clip:

  When data contain outliers, the histogram or density plot can fail to
  show the distribution of the main body of data. Setting `clip = TRUE`,
  will remove the top 1 % of data to yield what is often a better
  display of the overall distribution of the data. The amount of
  clipping can be set with `percentile`.

- percentile:

  This is used to clip the data. For example, `percentile = 0.99` (the
  default) will remove the top 1 percentile of values i.e. values
  greater than the 99th percentile will not be used.

- type:

  `type` is used to determine whether a histogram (the default) or a
  density plot is used to show the distribution of the data.

- pollutant:

  `pollutant` is used when there is a field `site` and there is more
  than one site in the data frame.

- period:

  `period` is either `years` (the default) or `months`. Statistics are
  calculated depending on the `period` chosen.

- avg.time:

  This defines the time period to average the time series plots. Can be
  "sec", "min", "hour", "day" (the default), "week", "month", "quarter"
  or "year". For much increased flexibility a number can precede these
  options followed by a space. For example, a
  [`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md)
  of 2 months would be `avg.time = "2 month"`.

- print.datacap:

  Should the data capture % be shown for each period?

- breaks:

  Number of histogram bins. Sometime useful but not easy to set a single
  value for a range of very different variables.

- plot.type:

  The `lattice` plot type, which is a line (`plot.type = "l"`) by
  default. Another useful option is `plot.type = "h"`, which draws
  vertical lines.

- col.trend:

  Colour to be used to show the monthly trend of the data, shown as a
  shaded region. Type
  [`colors()`](https://rdrr.io/r/grDevices/colors.html) into R to see
  the full range of colour names.

- col.data:

  Colour to be used to show the *presence* of data. Type
  [`colors()`](https://rdrr.io/r/grDevices/colors.html) into R to see
  the full range of colour names.

- col.mis:

  Colour to be used to show missing data.

- col.hist:

  Colour for the histogram or density plot.

- cols:

  Predefined colour scheme, currently only enabled for `"greyscale"`.

- date.breaks:

  Number of major x-axis intervals to use. The function will try and
  choose a sensible number of dates/times as well as formatting the
  date/time appropriately to the range being considered. This does not
  always work as desired automatically. The user can therefore increase
  or decrease the number of intervals by adjusting the value of
  `date.breaks` up or down.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the '2' in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- debug:

  Should data types be printed to the console? `TRUE` can be useful for
  debugging.

- ...:

  Other graphical parameters. Commonly used examples include the axis
  and title labelling options (such as `xlab`, `ylab` and `main`), which
  are all passed to the plot via
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  to handle routine formatting. As `summaryPlot()` has two components,
  the axis labels may be a vector. For example, the default case
  (`type = "histogram"`) sets y labels equivalent to
  `ylab = c("", "Percent of Total")`.

## Details

`summaryPlot()` produces two panels of plots: one showing the
presence/absence of data and the other the distributions. The left panel
shows time series and codes the presence or absence of data in different
colours. By stacking the plots one on top of another it is easy to
compare different pollutants/variables. Overall statistics are given for
each variable: mean, maximum, minimum, missing hours (also expressed as
a percentage), median and the 95th percentile. For each year the data
capture rate (expressed as a percentage of hours in that year) is also
given.

The right panel shows either a histogram or a density plot depending on
the choice of `type`. Density plots avoid the issue of arbitrary bin
sizes that can sometimes provide a misleading view of the data
distribution. Density plots are often more appropriate, but their
effectiveness will depend on the data in question.

`summaryPlot()` will only show data that are numeric or integer type.
This is useful for checking that data have been imported properly. For
example, if for some reason a column representing wind speed erroneously
had one or more fields with characters in, the whole column would be
either character or factor type. The absence of a wind speed variable in
the `summaryPlot()` plot would therefore indicate a problem with the
input data. In this particular case, the user should go back to the
source data and remove the characters or remove them using R functions.

If there is a field `site`, which would generally mean there is more
than one site, `summaryPlot()` will provide information on a *single*
pollutant across all sites, rather than provide details on all
pollutants at a *single* site. In this case the user should also provide
a name of a pollutant e.g. `pollutant = "nox"`. If a pollutant is not
provided the first numeric field will automatically be chosen.

**It is strongly recommended that the `summaryPlot()` function is
applied to all new imported data sets to ensure the data are imported as
expected.**

## Author

David Carslaw

## Examples

``` r
# do not clip density plot data
if (FALSE) { # \dontrun{
summaryPlot(mydata, clip = FALSE)
} # }

# exclude highest 5 % of data etc.
if (FALSE) { # \dontrun{
summaryPlot(mydata, percentile = 0.95)
} # }

# show missing data where there are at least 96 contiguous missing
# values (4 days)
if (FALSE) { # \dontrun{
summaryPlot(mydata, na.len = 96)
} # }

# show data in green
if (FALSE) { # \dontrun{
summaryPlot(mydata, col.data = "green")
} # }

# show missing data in yellow
if (FALSE) { # \dontrun{
summaryPlot(mydata, col.mis = "yellow")
} # }

# show density plot line in black
if (FALSE) { # \dontrun{
summaryPlot(mydata, col.dens = "black")
} # }
```
