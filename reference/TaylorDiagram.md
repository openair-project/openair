# Taylor Diagram for model evaluation with conditioning

Function to draw Taylor Diagrams for model evaluation. The function
allows conditioning by any categorical or numeric variables, which makes
the function very flexible.

## Usage

``` r
TaylorDiagram(
  mydata,
  obs = "obs",
  mod = "mod",
  group = NULL,
  type = "default",
  normalise = FALSE,
  pos.cor = NULL,
  cols = "brewer1",
  rms.col = "darkgoldenrod",
  cor.col = "black",
  arrow.lwd = 3,
  annotate = "centred\nRMS error",
  text.obs = "observed",
  key.title = group,
  key.columns = 1,
  key.position = "right",
  strip.position = "top",
  auto.text = TRUE,
  plot = TRUE,
  key = NULL,
  ...
)
```

## Arguments

- mydata:

  A data frame minimally containing a column of observations and a
  column of predictions.

- obs:

  A column of observations with which the predictions (`mod`) will be
  compared.

- mod:

  A column of model predictions. Note, `mod` can be of length 2 i.e. two
  lots of model predictions. If two sets of predictions are are present
  e.g. `mod = c("base", "revised")`, then arrows are shown on the Taylor
  Diagram which show the change in model performance in going from the
  first to the second. This is useful where, for example, there is
  interest in comparing how one model run compares with another using
  different assumptions e.g. input data or model set up. See examples
  below.

- group:

  The `group` column is used to differentiate between different models
  and can be a factor or character. The total number of models compared
  will be equal to the number of unique values of `group`.

  `group` can also be of length two e.g. `group = c("model", "site")`.
  In this case all model-site combinations will be shown but they will
  only be differentiated by colour/symbol by the first grouping variable
  ("model" in this case). In essence the plot removes the
  differentiation by the second grouping variable. Because there will be
  different values of `obs` for each group, `normalise = TRUE` should be
  used.

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

- normalise:

  Should the data be normalised by dividing the standard deviation of
  the observations? The statistics can be normalised (and
  non-dimensionalised) by dividing both the RMS difference and the
  standard deviation of the `mod` values by the standard deviation of
  the observations (`obs`). In this case the “observed” point is plotted
  on the x-axis at unit distance from the origin. This makes it possible
  to plot statistics for different species (maybe with different units)
  on the same plot. The normalisation is done by each `group`/`type`
  combination.

- pos.cor:

  Show only positive correlations (`TRUE`) or include negative
  correlations (`FALSE`). If negative correlations are shown, the Taylor
  Diagram will show two quadrants. The default, `NULL`, will use two
  quadrants if any negative correlations are present in the data and one
  quadrant if all correlations are positive.

- cols:

  Colours to use for plotting. Can be a pre-set palette (e.g.,
  `"turbo"`, `"viridis"`, `"tol"`, `"Dark2"`, etc.) or a user-defined
  vector of R colours (e.g., `c("yellow", "green", "blue", "black")` -
  see [`colours()`](https://rdrr.io/r/grDevices/colors.html) for a full
  list) or hex-codes (e.g., `c("#30123B", "#9CF649", "#7A0403")`). See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for more details.

- rms.col:

  Colour for centred-RMS lines and text.

- cor.col:

  Colour for correlation coefficient lines and text.

- arrow.lwd:

  Width of arrow used when used for comparing two model outputs.

- annotate:

  Annotation shown for RMS error.

- text.obs:

  The plot annotation for observed values; default is "observed".

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

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
  for `type` handling. Some additional arguments are also available:

  - `xlab`, `ylab` and `main` override the x-axis label, y-axis label,
    and plot title.

  - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have
    2 columns and 5 rows.

  - `fontsize` overrides the overall font size of the plot.

  - `cex`, `lwd`, and `pch` control various graphical parameters.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object. If retained, e.g., using
`output <- TaylorDiagram(thedata, obs = "nox", mod = "mod")`, this
output can be used to recover the data, reproduce or rework the original
plot or undertake further analysis. For example, `output$data` will be a
data frame consisting of the group, type, correlation coefficient (R),
the standard deviation of the observations and measurements.

## Details

The Taylor Diagram is a very useful model evaluation tool. The diagram
provides a way of showing how three complementary model performance
statistics vary simultaneously. These statistics are the correlation
coefficient R, the standard deviation (sigma) and the (centred)
root-mean-square error. These three statistics can be plotted on one
(2D) graph because of the way they are related to one another which can
be represented through the Law of Cosines.

The `openair` version of the Taylor Diagram has several enhancements
that increase its flexibility. In particular, the straightforward way of
producing conditioning plots should prove valuable under many
circumstances (using the `type` option). Many examples of Taylor
Diagrams focus on model-observation comparisons for several models using
all the available data. However, more insight can be gained into model
performance by partitioning the data in various ways e.g. by season,
daylight/nighttime, day of the week, by levels of a numeric variable
e.g. wind speed or by land-use type etc.

To consider several pollutants on one plot, a column identifying the
pollutant name can be used e.g. `pollutant`. Then the Taylor Diagram can
be plotted as (assuming a data frame `thedata`):

`TaylorDiagram(thedata, obs = "obs", mod = "mod", group = "model", type = "pollutant")`

which will give the model performance by pollutant in each panel.

Note that it is important that each panel represents data with the same
mean observed data across different groups. Therefore
`TaylorDiagram(mydata, group = "model", type = "season")` is OK, whereas
`TaylorDiagram(mydata, group = "season", type = "model")` is not because
each panel (representing a model) will have four different mean values —
one for each season. Generally, the option `group` is either missing
(one model being evaluated) or represents a column giving the model
name. However, the data can be normalised using the `normalise` option.
Normalisation is carried out on a per `group`/`type` basis making it
possible to compare data on different scales e.g.
`TaylorDiagram(mydata, group = "season", type = "model", normalise = TRUE)`.
In this way it is possible to compare different pollutants, sites etc.
in the same panel.

Also note that if multiple sites are present it makes sense to use
`type = "site"` to ensure that each panel represents an individual site
with its own specific standard deviation etc. If this is not the case
then select a single site from the data first e.g.
`subset(mydata, site == "Harwell")`.

## References

Taylor, K.E.: Summarizing multiple aspects of model performance in a
single diagram. J. Geophys. Res., 106, 7183-7192, 2001 (also see PCMDI
Report 55).

## See also

Other model evaluation functions:
[`conditionalEval()`](https://openair-project.github.io/openair/reference/conditionalEval.md),
[`conditionalQuantile()`](https://openair-project.github.io/openair/reference/conditionalQuantile.md),
[`modStats()`](https://openair-project.github.io/openair/reference/modStats.md)

## Author

David Carslaw

Jack Davison

## Examples
