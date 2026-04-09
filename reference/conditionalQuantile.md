# Conditional quantile estimates for model evaluation

Function to calculate conditional quantiles with flexible conditioning.
The function is for use in model evaluation and more generally to help
better understand forecast predictions and how well they agree with
observations.

## Usage

``` r
conditionalQuantile(
  mydata,
  obs = "obs",
  mod = "mod",
  type = "default",
  bins = 31,
  min.bin = c(10, 20),
  cols = "YlOrRd",
  key.columns = 2,
  key.position = "bottom",
  strip.position = "top",
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
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
  two are given, the first is used for the columns and the second for
  the rows.

- bins:

  Number of bins to be used in calculating the different quantile
  levels.

- min.bin:

  The minimum number of points required for the estimates of the 25/75th
  and 10/90th percentiles.

- cols:

  Colours to use for plotting. Can be a pre-set palette (e.g.,
  `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a user-defined
  vector of R colours (e.g., `c("yellow", "green", "blue", "black")` -
  see [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
  list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`).
  Alternatively, can be a list of arguments to control the colour
  palette more closely (e.g., `palette`, `direction`, `alpha`, etc.).
  See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  and
  [`colourOpts()`](https://openair-project.github.io/openair/reference/colourOpts.md)
  for more details.

- key.columns:

  Number of columns to be used in a categorical legend. With many
  categories a single column can make to key too wide. The user can thus
  choose to use several columns by setting `key.columns` to be less than
  the number of categories.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

- strip.position:

  Location where the facet 'strips' are located when using `type`. When
  one `type` is provided, can be one of `"left"`, `"right"`, `"bottom"`
  or `"top"`. When two `type`s are provided, this argument defines
  whether the strips are "switched" and can take either `"x"`, `"y"`, or
  `"both"`. For example, `"x"` will switch the 'top' strip locations to
  the bottom of the plot.

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

- key:

  Deprecated; please use `key.position`. If `FALSE`, sets `key.position`
  to `"none"`.

- ...:

  Addition options are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for `type` handling. Some additional arguments are also available,
  varying somewhat in different plotting functions:

  - `title`, `subtitle`, `caption`, `xlab` and `ylab` control the plot
    title, subtitle, caption, x-axis label and y-axis label. All of
    these are passed through to
    [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
    if `auto.text = TRUE`.

  - `xlim`, `ylim` and `limits` control the limits of the x-axis, y-axis
    and colorbar scales.

  - `ncol` and `nrow` set the number of columns and rows in a faceted
    plot.

  - `fontsize` overrides the overall font size of the plot by setting
    the `text` argument of
    [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
    It may also be applied proportionately to any `openair` annotations
    (e.g., N/E/S/W labels on polar coordinate plots).

  - Various graphical parameters are also supported: `linewidth`,
    `linetype`,` shape`, `size`, `border`, and `alpha`. Not all
    parameters apply to all plots. These can take a single value, or a
    vector of multiple values - e.g., `shape = c(1, 2)` - which will be
    recycled to the length of values needed.

  - `lineend`, `linejoin` and `linemitre` tweak the appearance of line
    plots; see
    [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
    for more information.

  - In polar coordinate plots, `annotate = FALSE` will remove the
    N/E/S/W labels and any other annotations.

## Details

Conditional quantiles are a very useful way of considering model
performance against observations for continuous measurements (Wilks,
2005). The conditional quantile plot splits the data into evenly spaced
bins. For each predicted value bin e.g. from 0 to 10~ppb the
*corresponding* values of the observations are identified and the
median, 25/75th and 10/90 percentile (quantile) calculated for that bin.
The data are plotted to show how these values vary across all bins. For
a time series of observations and predictions that agree precisely the
median value of the predictions will equal that for the observations for
each bin.

The conditional quantile plot differs from the quantile-quantile plot
(Q-Q plot) that is often used to compare observations and predictions. A
Q-Q~plot separately considers the distributions of observations and
predictions, whereas the conditional quantile uses the corresponding
observations for a particular interval in the predictions. Take as an
example two time series, the first a series of real observations and the
second a lagged time series of the same observations representing the
predictions. These two time series will have identical (or very nearly
identical) distributions (e.g. same median, minimum and maximum). A Q-Q
plot would show a straight line showing perfect agreement, whereas the
conditional quantile will not. This is because in any interval of the
predictions the corresponding observations now have different values.

Plotting the data in this way shows how well predictions agree with
observations and can help reveal many useful characteristics of how well
model predictions agree with observations — across the full distribution
of values. A single plot can therefore convey a considerable amount of
information concerning model performance. The `conditionalQuantile`
function in openair allows conditional quantiles to be considered in a
flexible way e.g. by considering how they vary by season.

The function requires a data frame consisting of a column of
observations and a column of predictions. The observations are split up
into `bins` according to values of the predictions. The median
prediction line together with the 25/75th and 10/90th quantile values
are plotted together with a line showing a “perfect” model. Also shown
is a histogram of predicted values (shaded grey) and a histogram of
observed values (shown as a blue outline).

Far more insight can be gained into model performance through
conditioning using `type`. For example, `type = "season"` will plot
conditional quantiles by each season. `type` can also be a factor or
character field e.g. representing different models used.

See Wilks (2005) for more details and the examples below.

## References

Murphy, A. H., B.G. Brown and Y. Chen. (1989) Diagnostic Verification of
Temperature Forecasts, Weather and Forecasting, Volume: 4, Issue: 4,
Pages: 485-501.

Wilks, D. S., 2005. Statistical Methods in the Atmospheric Sciences,
Volume 91, Second Edition (International Geophysics), 2nd Edition.
Academic Press.

## See also

The `verification` package for comprehensive functions for forecast
verification.

Other model evaluation functions:
[`TaylorDiagram()`](https://openair-project.github.io/openair/reference/TaylorDiagram.md),
[`conditionalEval()`](https://openair-project.github.io/openair/reference/conditionalEval.md),
[`modStats()`](https://openair-project.github.io/openair/reference/modStats.md)

## Author

David Carslaw

Jack Davison

## Examples

``` r
# make some dummy prediction data based on 'nox'
mydata$mod <- mydata$nox * 1.1 + mydata$nox * runif(seq_len(nrow(mydata)))

# basic conditional quantile plot
# A "perfect" model is shown by the blue line
# predictions tend to be increasingly positively biased at high nox,
# shown by departure of median line from the blue one.
# The widening uncertainty bands with increasing NOx shows that
# hourly predictions are worse for higher NOx concentrations.
# Also, the red (median) line extends beyond the data (blue line),
# which shows in this case some predictions are much higher than
# the corresponding measurements. Note that the uncertainty bands
# do not extend as far as the median line because there is insufficient
# to calculate them
conditionalQuantile(mydata, obs = "nox", mod = "mod")


# can split by season to show seasonal performance (not very
# enlightening in this case - try some real data and it will be!)

if (FALSE) { # \dontrun{
conditionalQuantile(mydata, obs = "nox", mod = "mod", type = "season")
} # }
```
