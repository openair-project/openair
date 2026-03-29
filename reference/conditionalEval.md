# Conditional quantile estimates with additional variables for model evaluation

This function enhances
[`conditionalQuantile()`](https://openair-project.github.io/openair/reference/conditionalQuantile.md)
by also considering how other variables vary over the same intervals.
Conditional quantiles are very useful on their own for model evaluation,
but provide no direct information on how other variables change at the
same time. For example, a conditional quantile plot of ozone
concentrations may show that low concentrations of ozone tend to be
under-predicted. However, the cause of the under-prediction can be
difficult to determine. However, by considering how well the model
predicts other variables over the same intervals, more insight can be
gained into the underlying reasons why model performance is poor.

## Usage

``` r
conditionalEval(
  mydata,
  obs = "obs",
  mod = "mod",
  var.obs = "var.obs",
  var.mod = "var.mod",
  type = "default",
  bins = 31,
  statistic = "MB",
  cols = "YlOrRd",
  col.var = "Set1",
  var.names = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame containing the field `obs` and `mod` representing
  observed and modelled values.

- obs:

  The name of the observations in `mydata`.

- mod:

  The name of the predictions (modelled values) in `mydata`.

- var.obs:

  Other variable observations for which statistics should be calculated.
  Can be more than length one e.g. `var.obs = c("nox.obs", "ws.obs")`.

- var.mod:

  Other variable predictions for which statistics should be calculated.
  Can be more than length one e.g. `var.mod = c("nox.mod", "ws.mod")`.

- type:

  Character string(s) defining how data should be split/conditioned
  before plotting. `"default"` produces a single panel using the entire
  dataset. Any other options will split the plot into different panels -
  a roughly square grid of panels if one `type` is given, or a 2D matrix
  of panels if two `types` are given. `type` is always passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md),
  and can therefore be any of:

  - A built-in type defined in
    [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
    (e.g., `"season"`, `"year"`, `"weekday"`, etc.). For example,
    `type = "season"` will split the plot into four panels, one for each
    season.

  - The name of a numeric column in `mydata`, which will be split into
    `n.levels` quantiles (defaulting to 4).

  - The name of a character or factor column in `mydata`, which will be
    used as-is. Commonly this could be a variable like `"site"` to
    ensure data from different monitoring sites are handled and
    presented separately. It could equally be any arbitrary column
    created by the user (e.g., whether a nearby possible pollutant
    source is active or not).

  Most `openair` plotting functions can take two `type` arguments. If
  two are given, the first is used for the rows and the second for the
  columns.

- bins:

  Number of bins to be used in calculating the different quantile
  levels.

- statistic:

  Statistic(s) to be plotted. Can be “MB”, “NMB”, “r”, “COE”, “MGE”,
  “NMGE”, “RMSE” and “FAC2”. `statistic` can also be a variable name in
  the data frame or a date-based type (e.g. “season”), in which case the
  plot shows the proportions of that variable across the prediction
  intervals. A special case is “cluster”.

- cols:

  Colours to use for plotting. Can be a pre-set palette (e.g.,
  `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a user-defined
  vector of R colours (e.g., `c("yellow", "green", "blue", "black")` -
  see [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
  list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`). See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for more details.

- col.var:

  Colours for the additional variables. See `openColours` for more
  details.

- var.names:

  Variable names to be shown in the legend for `var.obs` and `var.mod`.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the "2" in "NO2". Passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md).

- plot:

  When `openair` plots are created they are automatically printed to the
  active graphics device. `plot = FALSE` deactivates this behaviour.
  This may be useful when the plot *data* is of more interest, or the
  plot is required to appear later (e.g., later in a Quarto document, or
  to be saved to a file).

- ...:

  Other graphical parameters passed onto
  [`conditionalQuantile()`](https://openair-project.github.io/openair/reference/conditionalQuantile.md)
  and
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

## Details

The `conditionalEval` function provides information on how other
variables vary across the same intervals as shown on the conditional
quantile plot. There are two types of variable that can be considered by
setting the value of `statistic`. First, `statistic` can be another
variable in the data frame. In this case the plot will show the
different proportions of `statistic` across the range of predictions.
For example `statistic = "season"` will show for each interval of `mod`
the proportion of predictions that were spring, summer, autumn or
winter. This is useful because if model performance is worse for example
at high concentrations of `mod` then knowing that these tend to occur
during a particular season etc. can be very helpful when trying to
understand *why* a model fails. See
[`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
for more details on the types of variable that can be `statistic`.
Another example would be `statistic = "ws"` (if wind speed were
available in the data frame), which would then split wind speed into
four quantiles and plot the proportions of each.

Second, `conditionalEval` can simultaneously plot the model performance
of other observed/predicted variable **pairs** according to different
model evaluation statistics. These statistics derive from the
[`modStats()`](https://openair-project.github.io/openair/reference/modStats.md)
function and include “MB”, “NMB”, “r”, “COE”, “MGE”, “NMGE”, “RMSE” and
“FAC2”. More than one statistic can be supplied e.g.
`statistic = c("NMB", "COE")`. Bootstrap samples are taken from the
corresponding values of other variables to be plotted and their
statistics with 95\\ intervals calculated. In this case, the model
*performance* of other variables is shown across the same intervals of
`mod`, rather than just the values of single variables. In this second
case the model would need to provide observed/predicted pairs of other
variables.

For example, a model may provide predictions of NOx and wind speed (for
which there are also observations available). The `conditionalEval`
function will show how well these other variables are predicted for the
same intervals of the main variables assessed in the conditional
quantile e.g. ozone. In this case, values are supplied to `var.obs`
(observed values for other variables) and `var.mod` (modelled values for
other variables). For example, to consider how well the model predicts
NOx and wind speed `var.obs = c("nox.obs", "ws.obs")` and
`var.mod = c("nox.mod", "ws.mod")` would be supplied (assuming
`nox.obs, nox.mod, ws.obs, ws.mod` are present in the data frame). The
analysis could show for example, when ozone concentrations are
under-predicted, the model may also be shown to over-predict
concentrations of NOx at the same time, or under-predict wind speeds.
Such information can thus help identify the underlying causes of poor
model performance.

A special case is `statistic = "cluster"`. In this case a data frame is
provided that contains the cluster calculated by
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md)
and
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md).
Note that `statistic = "cluster"` cannot be used together with the
ordinary model evaluation statistics such as MB. The output will be a
bar chart showing the proportion of each interval of `mod` by cluster
number.

## References

Wilks, D. S., 2005. Statistical Methods in the Atmospheric Sciences,
Volume 91, Second Edition (International Geophysics), 2nd Edition.
Academic Press.

## See also

The `verification` package for comprehensive functions for forecast
verification.

Other model evaluation functions:
[`TaylorDiagram()`](https://openair-project.github.io/openair/reference/TaylorDiagram.md),
[`conditionalQuantile()`](https://openair-project.github.io/openair/reference/conditionalQuantile.md),
[`modStats()`](https://openair-project.github.io/openair/reference/modStats.md)

## Author

David Carslaw
