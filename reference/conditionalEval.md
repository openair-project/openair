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
  xlab = "predicted value",
  ylab = "statistic",
  col = brewer.pal(5, "YlOrRd"),
  col.var = "Set1",
  var.names = NULL,
  auto.text = TRUE,
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
  Note that including other variables could reduce the number of data
  available to plot due to the need of having non-missing data for all
  variables.

- var.mod:

  Other variable predictions for which statistics should be calculated.
  Can be more than length one e.g. `var.obs = c("nox.obs", "ws.obs")`.

- type:

  `type` determines how the data are split i.e. conditioned, and then
  plotted. The default is will produce a single plot using the entire
  data. Type can be one of the built-in types as detailed in `cutData`
  e.g. "season", "year", "weekday" and so on. For example,
  `type = "season"` will produce four plots — one for each season.

  It is also possible to choose `type` as another variable in the data
  frame. If that variable is numeric, then the data will be split into
  four quantiles (if possible) and labelled accordingly. If type is an
  existing character or factor variable, then those categories/levels
  will be used directly. This offers great flexibility for understanding
  the variation of different variables and how they depend on one
  another.

- bins:

  Number of bins to be used in calculating the different quantile
  levels.

- statistic:

  Statistic(s) to be plotted. Can be “MB”, “NMB”, “r”, “COE”, “MGE”,
  “NMGE”, “RMSE” and “FAC2”, as described in `modStats`. When these
  statistics are chosen, they are calculated from `var.mod` and
  `var.mod`.

  `statistic` can also be a value that can be supplied to `cutData`. For
  example, `statistic = "season"` will show how model performance varies
  by season across the distribution of predictions which might highlight
  that at high concentrations of NOx the model tends to underestimate
  concentrations and that these periods mostly occur in winter.
  `statistic` can also be another variable in the data frame — see
  `cutData` for more information. A special case is
  `statistic = "cluster"` if clusters have been calculated using
  `trajCluster`.

- xlab:

  label for the x-axis, by default “predicted value”.

- ylab:

  label for the y-axis, by default “observed value”.

- col:

  Colours to be used for plotting the uncertainty bands and median line.
  Must be of length 5 or more.

- col.var:

  Colours for the additional variables to be compared. See `openColours`
  for more details.

- var.names:

  Variable names to be shown on plot for plotting `var.obs` and
  `var.mod`.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  etc. will automatically try and format pollutant names and units
  properly e.g. by subscripting the \`2' in NO2.

- ...:

  Other graphical parameters passed onto `conditionalQuantile` and
  `cutData`. For example, `conditionalQuantile` passes the option
  `hemisphere = "southern"` on to `cutData` to provide southern (rather
  than default northern) hemisphere handling of `type = "season"`.
  Similarly, common axis and title labelling options (such as `xlab`,
  `ylab`, `main`) are passed to `xyplot` via `quickText` to handle
  routine formatting.

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
model performance. For example, an under-prediction in wind speed could
result in higher surface NOx concentrations and lower ozone
concentrations. Similarly if wind speed predictions were good and NOx
was over predicted it might suggest an over-estimate of NOx emissions.
One or more additional variables can be plotted.

A special case is `statistic = "cluster"`. In this case a data frame is
provided that contains the cluster calculated by
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md)
and
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md).
Alternatively users could supply their own pre-calculated clusters.
These calculations can be very useful in showing whether certain back
trajectory clusters are associated with poor (or good) model
performance. Note that in the case of `statistic = "cluster"` there will
be fewer data points used in the analysis compared with the ordinary
statistics above because the trajectories are available for every three
hours. Also note that `statistic = "cluster"` cannot be used together
with the ordinary model evaluation statistics such as MB. The output
will be a bar chart showing the proportion of each interval of `mod` by
cluster number.

Far more insight can be gained into model performance through
conditioning using `type`. For example, `type = "season"` will plot
conditional quantiles and the associated model performance statistics of
other variables by each season. `type` can also be a factor or character
field e.g. representing different models used.

See Wilks (2005) for more details of conditional quantile plots.

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
