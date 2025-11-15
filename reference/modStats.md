# Calculate common model evaluation statistics

Function to calculate common numerical model evaluation statistics with
flexible conditioning.

## Usage

``` r
modStats(
  mydata,
  mod = "mod",
  obs = "obs",
  statistic = c("n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA"),
  type = "default",
  rank.name = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame.

- mod:

  Name of a variable in `mydata` that represents modelled values.

- obs:

  Name of a variable in `mydata` that represents measured values.

- statistic:

  The statistic to be calculated. See details below for a description of
  each.

- type:

  `type` determines how the data are split i.e. conditioned, and then
  plotted. The default is will produce statistics using the entire data.
  `type` can be one of the built-in types as detailed in `cutData` e.g.
  “season”, “year”, “weekday” and so on. For example, `type = "season"`
  will produce four sets of statistics — one for each season.

  It is also possible to choose `type` as another variable in the data
  frame. If that variable is numeric, then the data will be split into
  four quantiles (if possible) and labelled accordingly. If type is an
  existing character or factor variable, then those categories/levels
  will be used directly. This offers great flexibility for understanding
  the variation of different variables and how they depend on one
  another.

  More than one type can be considered e.g.
  `type = c("season", "weekday")` will produce statistics split by
  season and day of the week.

- rank.name:

  Simple model ranking can be carried out if `rank.name` is supplied.
  `rank.name` will generally refer to a column representing a model
  name, which is to ranked. The ranking is based the COE performance, as
  that indicator is arguably the best single model performance indicator
  available.

- ...:

  Arguments passed on to
  [`cutData`](https://openair-project.github.io/openair/reference/cutData.md)

  `x`

  :   A data frame containing a field `date`.

  `names`

  :   By default, the columns created by
      [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
      are named after their `type` option. Specifying `names` defines
      other names for the columns, which map onto the `type` options in
      the same order they are given. The length of `names` should
      therefore be equal to the length of `type`.

  `suffix`

  :   If `name` is not specified, `suffix` will be appended to any added
      columns that would otherwise overwrite existing columns. For
      example, `cutData(mydata, "nox", suffix = "_cuts")` would append a
      `nox_cuts` column rather than overwriting `nox`.

  `hemisphere`

  :   Can be `"northern"` or `"southern"`, used to split data into
      seasons.

  `n.levels`

  :   Number of quantiles to split numeric data into.

  `start.day`

  :   What day of the week should the `type = "weekday"` start on? The
      user can change the start day by supplying an integer between 0
      and 6. Sunday = 0, Monday = 1, ... For example to start the
      weekday plots on a Saturday, choose `start.day = 6`.

  `is.axis`

  :   A logical (`TRUE`/`FALSE`), used to request shortened cut labels
      for axes.

  `local.tz`

  :   Used for identifying whether a date has daylight savings time
      (DST) applied or not. Examples include
      `local.tz = "Europe/London"`, `local.tz = "America/New_York"`,
      i.e., time zones that assume DST.
      <https://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones> shows
      time zones that should be valid for most systems. It is important
      that the original data are in GMT (UTC) or a fixed offset from
      GMT.

  `latitude,longitude`

  :   The decimal latitude and longitudes used when `type = "daylight"`.
      Note that locations west of Greenwich have negative longitudes.

  `drop`

  :   How to handle empty factor levels. One of:

      - `"default"`: Sensible defaults selected on a case-by-case basis
        for different `type` options.

      - `"empty"`: Drop all empty factor levels.

      - `"none"`: Retain all empty factor levels, where possible. For
        example, for `type = "hour"`, all factor levels from `0` and
        `23` will be represented.

      - `"outside"`: Retain empty factor levels within the range of the
        data. For example, for `type = "hour"` when the data only
        contains data for 1 AM and 5 AM, the factor levels, `1`, `2`,
        `3`, `4` and `5` will be retained.

      Some of these options only apply to certain `type` options. For
      example, for `type = "year"`, `"outside"` is equivalent to
      `"none"` as there is no fixed range of years to use in the
      `"none"` case.

## Value

Returns a data frame with model evaluation statistics.

## Details

This function is under development and currently provides some common
model evaluation statistics. These include (to be mathematically defined
later):

- \\n\\, the number of complete pairs of data.

- \\FAC2\\, fraction of predictions within a factor of two.

- \\MB\\, the mean bias.

- \\MGE\\, the mean gross error.

- \\NMB\\, the normalised mean bias.

- \\NMGE\\, the normalised mean gross error.

- \\RMSE\\, the root mean squared error.

- \\r\\, the Pearson correlation coefficient. Note, can also supply and
  argument `method` e.g. `method = "spearman"`. Also returned is the P
  value of the correlation coefficient, \\P\\, which may present as `0`
  for very low values.

- \\COE\\, the *Coefficient of Efficiency* based on Legates and McCabe
  (1999, 2012). There have been many suggestions for measuring model
  performance over the years, but the COE is a simple formulation which
  is easy to interpret.

  A perfect model has a COE = 1. As noted by Legates and McCabe although
  the COE has no lower bound, a value of COE = 0.0 has a fundamental
  meaning. It implies that the model is no more able to predict the
  observed values than does the observed mean. Therefore, since the
  model can explain no more of the variation in the observed values than
  can the observed mean, such a model can have no predictive advantage.

  For negative values of COE, the model is less effective than the
  observed mean in predicting the variation in the observations.

- \\IOA\\, the Index of Agreement based on Willmott et al. (2011), which
  spans between -1 and +1 with values approaching +1 representing better
  model performance.

  An IOA of 0.5, for example, indicates that the sum of the
  error-magnitudes is one half of the sum of the observed-deviation
  magnitudes. When IOA = 0.0, it signifies that the sum of the
  magnitudes of the errors and the sum of the observed-deviation
  magnitudes are equivalent. When IOA = -0.5, it indicates that the sum
  of the error-magnitudes is twice the sum of the perfect
  model-deviation and observed-deviation magnitudes. Values of IOA near
  -1.0 can mean that the model-estimated deviations about O are poor
  estimates of the observed deviations; but, they also can mean that
  there simply is little observed variability - so some caution is
  needed when the IOA approaches -1.

All statistics are based on complete pairs of `mod` and `obs`.

Conditioning is possible through setting `type`, which can be a vector
e.g. `type = c("weekday", "season")`.

## References

Legates DR, McCabe GJ. (1999). Evaluating the use of goodness-of-fit
measures in hydrologic and hydroclimatic model validation. Water
Resources Research 35(1): 233-241.

Legates DR, McCabe GJ. (2012). A refined index of model performance: a
rejoinder, International Journal of Climatology.

Willmott, C.J., Robeson, S.M., Matsuura, K., 2011. A refined index of
model performance. International Journal of Climatology.

## See also

Other model evaluation functions:
[`TaylorDiagram()`](https://openair-project.github.io/openair/reference/TaylorDiagram.md),
[`conditionalEval()`](https://openair-project.github.io/openair/reference/conditionalEval.md),
[`conditionalQuantile()`](https://openair-project.github.io/openair/reference/conditionalQuantile.md)

## Author

David Carslaw

## Examples

``` r
## the example below is somewhat artificial --- assuming the observed
## values are given by NOx and the predicted values by NO2.

modStats(mydata, mod = "no2", obs = "nox")
#> # A tibble: 1 × 12
#>   default      n  FAC2    MB   MGE    NMB  NMGE  RMSE     r     P    COE   IOA
#>   <fct>    <int> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1 all data 63095 0.170 -130.  130. -0.725 0.726  167. 0.787     0 -0.337 0.331

## evaluation stats by season

modStats(mydata, mod = "no2", obs = "nox", type = "season")
#> # A tibble: 4 × 12
#>   season        n   FAC2    MB   MGE    NMB  NMGE  RMSE     r     P    COE   IOA
#>   <ord>     <int>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1 spring (… 17343 0.258  -108.  108. -0.685 0.685  140. 0.820     0 -0.250 0.375
#> 2 summer (… 14658 0.171  -116.  116. -0.705 0.705  145. 0.757     0 -0.420 0.290
#> 3 autumn (… 14775 0.0983 -154.  154. -0.753 0.753  192. 0.788     0 -0.437 0.281
#> 4 winter (… 16319 0.142  -143.  143. -0.749 0.751  185. 0.826     0 -0.321 0.340
```
