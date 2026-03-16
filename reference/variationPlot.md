# Variation Plot

The `variationPlot()` function is designed to explore how the
distribution of a pollutant (or other variable) changes by another
variable (`x`). For example, it can be used to explore how the
distribution of `nox` varies by `season` or by `weekday`. This plot can
be extensively conditioned using the `type` and `group` arguments, both
of which are passed to
[`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).
An appropriate plot type will be chosen based on the type of `x` - e.g.,
ordered variables will be joined by a line.

## Usage

``` r
variationPlot(
  mydata,
  pollutant = "nox",
  x = "hour",
  statistic = "mean",
  type = "default",
  group = "default",
  normalise = FALSE,
  difference = FALSE,
  conf.int = NULL,
  B = 100,
  local.tz = NULL,
  ci = TRUE,
  cols = "hue",
  alpha = 0.4,
  strip.position = "top",
  key.position = "top",
  key.columns = NULL,
  name.pol = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame of time series. Must include a `date` field and at least
  one variable to plot.

- pollutant:

  Name of variable to plot. Two or more pollutants can be plotted, in
  which case a form like `pollutant = c("nox", "co")` should be used.

- x:

  A character value to be passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md);
  used to define the category by which `pollutant` will be varied and
  plotted.

- statistic:

  Can be `"mean"` (default) or `"median"`. If the statistic is `"mean"`
  then the mean line and the 95% confidence interval in the mean are
  plotted by default. If the statistic is `"median"` then the median
  line is plotted together with the 5/95 and 25/75th quantiles are
  plotted. Users can control the confidence intervals with `conf.int`.

- type:

  `type` determines how the data are split, i.e., conditioned, and then
  plotted. The default is will produce a single plot using the entire
  data. Type can be one of the built-in types as detailed in
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md),
  e.g., `"season"`, `"year"`, `"weekday"` and so on. For example,
  `type = "season"` will produce four plots — one for each season.

  It is also possible to choose `type` as another variable in the data
  frame. If that variable is numeric, then the data will be split into
  four quantiles (if possible) and labelled accordingly. If type is an
  existing character or factor variable, then those categories/levels
  will be used directly. This offers great flexibility for understanding
  the variation of different variables and how they depend on one
  another.

  Two `types` are allowed in `variationPlot()` and one `type` in
  [`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md).
  For the latter, `type` is applied to each `panel` and for additional
  splits use the `"x.type"` syntax in the `panels` argument (e.g,
  `panels = c("hour.weekday")`).

- group:

  This sets the grouping variable to be used. For example, if a data
  frame had a column `site` setting `group = "site"` will plot all sites
  together in each panel. Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

- normalise:

  Should variables be normalised? The default is `FALSE`. If `TRUE` then
  the variable(s) are divided by their mean values. This helps to
  compare the shape of the diurnal trends for variables on very
  different scales.

- difference:

  If two pollutants are chosen then setting `difference = TRUE` will
  also plot the difference in means between the two variables as
  `pollutant[2] - pollutant[1]`. Bootstrap 95\\ difference in means are
  also calculated. A horizontal dashed line is shown at y = 0. The
  difference can also be calculated if there is a column that identifies
  two groups, e.g., having used
  [`splitByDate()`](https://openair-project.github.io/openair/reference/splitByDate.md).
  In this case it is possible to call the function with the option
  `group = "split.by"` and `difference = TRUE`.

- conf.int:

  The confidence intervals to be plotted. If `statistic = "mean"` then
  the confidence intervals in the mean are plotted. If
  `statistic = "median"` then the `conf.int` and `1 - conf.int`
  *quantiles* are plotted. Any number of `conf.int`s can be provided.

- B:

  Number of bootstrap replicates to use. Can be useful to reduce this
  value when there are a large number of observations available to
  increase the speed of the calculations without affecting the 95%
  confidence interval calculations by much.

- local.tz:

  Used for identifying whether a date has daylight savings time (DST)
  applied or not. Examples include `local.tz = "Europe/London"`,
  `local.tz = "America/New_York"`, i.e., time zones that assume DST.
  <https://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones> shows time
  zones that should be valid for most systems. It is important that the
  original data are in GMT (UTC) or a fixed offset from GMT.

- ci:

  Should confidence intervals be shown? The default is `TRUE`. Setting
  this to `FALSE` can be useful if multiple pollutants are chosen where
  over-lapping confidence intervals can over complicate plots.

- cols:

  Colours to be used for plotting; see
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for details.

- alpha:

  The alpha transparency used for plotting confidence intervals. `0` is
  fully transparent and 1 is opaque. The default is `0.4`.

- strip.position:

  Location where the facet 'strips' are located when using `type`. When
  one `type` is provided, can be one of `"left"`, `"right"`, `"bottom"`
  or `"top"`. When two `type`s are provided, this argument defines
  whether the strips are "switched" and can take either `"x"`, `"y"`, or
  `"both"`. For example, `"x"` will switch the 'top' strip locations to
  the bottom of the plot.

- key.position:

  Location where the scale key is to plotted. Can include `"top"`,
  `"bottom"`, `"right"` and `"left"`.

- key.columns:

  Number of columns to be used in the key. With many pollutants a single
  column can make to key too wide. The user can thus choose to use
  several columns by setting `columns` to be less than the number of
  pollutants.

- name.pol:

  This option can be used to give alternative names for the variables
  plotted. Instead of taking the column headings as names, the user can
  supply replacements. For example, if a column had the name "nox" and
  the user wanted a different description, then setting
  `name.pol = "nox before change"` can be used. If more than one
  pollutant is plotted then use `c` e.g.
  `name.pol = c("nox here", "o3 there")`.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the '2' in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract plot components and plotting them in other ways.

- ...:

  Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object.

## Details

When `statistic = "mean"`, the plot shows the 95% confidence intervals
in the mean. The 95% confidence intervals are calculated through
bootstrap simulations, which will provide more robust estimates of the
confidence intervals (particularly when there are relatively few data).

Users can supply their own `ylim`, e.g. `ylim = c(0, 200)`.

The `difference` option calculates the difference in means between two
pollutants, along with bootstrap estimates of the 95\\ in the
difference. This works in two ways: either two pollutants are supplied
in separate columns (e.g. `pollutant = c("no2", "o3")`), or there are
two unique values of `group`. The difference is calculated as the second
pollutant minus the first and is labelled accordingly. This feature is
particularly useful for model evaluation and identifying where models
diverge from observations across time scales.

Depending on the choice of statistic, a subheading is added. Users can
control the text in the subheading through the use of `sub` e.g.
`sub = ""` will remove any subheading.

## See also

[`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md),
which conveniently assembles many time-related variation plots into a
single plot

## Author

Jack Davison

David Carslaw

## Examples

``` r
# example using the 'mydata' dataset
variationPlot(
  mydata,
  pollutant = c("nox", "o3"),
  x = "hour",
  type = "season",
  normalise = TRUE
)
```
