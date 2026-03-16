# Correlation matrices with conditioning

Function to to draw and visualise correlation matrices using lattice.
The primary purpose is as a tool for exploratory data analysis.
Hierarchical clustering is used to group similar variables.

## Usage

``` r
corPlot(
  mydata,
  pollutants = NULL,
  type = "default",
  cluster = TRUE,
  method = "pearson",
  use = "pairwise.complete.obs",
  annotate = c("cor", "signif", "stars", "none"),
  dendrogram = FALSE,
  triangle = c("both", "upper", "lower"),
  diagonal = TRUE,
  cols = "default",
  r.thresh = 0.8,
  text.col = c("black", "black"),
  key.header = NULL,
  key.position = "right",
  key = FALSE,
  auto.text = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame which should consist of some numeric columns.

- pollutants:

  the names of data-series in `mydata` to be plotted by `corPlot`. The
  default option `NULL` and the alternative `"all"` use all available
  valid (numeric) data.

- type:

  `type` determines how the data are split i.e. conditioned, and then
  plotted. For example, `type = "season"` will produce four plots — one
  for each season. See
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for more information.

- cluster:

  Should the data be ordered according to cluster analysis. If `TRUE`
  hierarchical clustering is applied to the correlation matrices using
  [`hclust()`](https://rdrr.io/r/stats/hclust.html) to group similar
  variables together. With many variables clustering can greatly assist
  interpretation.

- method:

  The correlation method to use. Can be `"pearson"`, `"spearman"` or
  `"kendall"`.

- use:

  How to handle missing values in the `cor` function. The default is
  `"pairwise.complete.obs"`. Care should be taken with the choice of how
  to handle missing data when considering pair-wise correlations.

- annotate:

  What to annotate each correlation tile with. One of:

  - `"cor"`, the correlation coefficient to 2 decimal places.

  - `"signif"`, an X marker if the correlation is significant.

  - `"stars"`, standard significance stars.

  - `"none"`, no annotation.

- dendrogram:

  Should a dendrogram be plotted? When `TRUE` a dendrogram is shown on
  the plot. Note that this will only work for `type = "default"`.
  Defaults to `FALSE`.

- triangle:

  Which 'triangles' of the correlation plot should be shown? Can be
  `"both"`, `"lower"` or `"upper"`. Defaults to `"both"`.

- diagonal:

  Should the 'diagonal' of the correlation plot be shown? The diagonal
  of a correlation matrix is axiomatically always `1` as it represents
  correlating a variable with itself. Defaults to `TRUE`.

- cols:

  Colours to be used for plotting. See
  [`openColours()`](https://openair-project.github.io/openair/reference/openColours.md)
  for more details.

- r.thresh:

  Values of greater than `r.thresh` will be shown in bold type. This
  helps to highlight high correlations.

- text.col:

  The colour of the text used to show the correlation values. The first
  value controls the colour of negative correlations and the second
  positive.

- key.header:

  Used to control the title of the plot key. Defaults to `NULL`; no
  header.

- key.position:

  Location where the scale key is to plotted. Allowed arguments
  currently include `"top"`, `"right"`, `"bottom"` and `"left"`.

- key:

  Should a key be shown? In `corPlot()` this defaults to `FALSE`.

- auto.text:

  Either `TRUE` (default) or `FALSE`. If `TRUE` titles and axis labels
  will automatically try and format pollutant names and units properly,
  e.g., by subscripting the \`2' in NO2.

- plot:

  Should a plot be produced? `FALSE` can be useful when analysing data
  to extract corPlot components and plotting them in other ways.

- ...:

  Addition options are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for `type` handling. Some additional arguments are also available:

  - `xlab`, `ylab` and `main` override the x-axis label, y-axis label,
    and plot title.

  - `layout` sets the layout of facets - e.g., `layout(2, 5)` will have
    2 columns and 5 rows.

  - `fontsize` overrides the overall font size of the plot.

  - `border` sets the border colour of each ellipse.

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Details

The `corPlot()` function plots correlation matrices. The implementation
relies heavily on that shown in Sarkar (2007), with a few extensions.

Correlation matrices are a very effective way of understating
relationships between many variables. The `corPlot()` shows the
correlation coded in three ways: by shape (ellipses), colour and the
numeric value. The ellipses can be thought of as visual representations
of scatter plot. With a perfect positive correlation a line at 45
degrees positive slope is drawn. For zero correlation the shape becomes
a circle. See examples below.

With many different variables it can be difficult to see relationships
between variables, i.e., which variables tend to behave most like one
another. For this reason hierarchical clustering is applied to the
correlation matrices to group variables that are most similar to one
another (if `cluster = TRUE`).

If clustering is chosen it is also possible to add a dendrogram using
the option `dendrogram = TRUE`. Note that dendrogramscan only be plotted
for `type = "default"` i.e. when there is only a single panel. The
dendrogram can also be recovered from the plot object itself and plotted
more clearly; see examples below.

It is also possible to use the `openair` type option to condition the
data in many flexible ways, although this may become difficult to
visualise with too many panels.

## References

Sarkar, D. (2007). Lattice Multivariate Data Visualization with R. New
York: Springer.

Friendly, M. (2002). Corrgrams : Exploratory displays for correlation
matrices. American Statistician, 2002(4), 1-16. doi:10.1198/000313002533

## Author

David Carslaw

Jack Davison

Adapted from the approach taken by Sarkar (2007)

## Examples

``` r
# basic corrgram plot
corPlot(mydata)


# plot by season
corPlot(mydata, type = "season")


# recover dendrogram when cluster = TRUE and plot it
res <- corPlot(mydata, plot = FALSE)
plot(res$clust)


if (FALSE) { # \dontrun{
# a more interesting are hydrocarbon measurements
hc <- importAURN(site = "my1", year = 2005, hc = TRUE)

# now it is possible to see the hydrocarbons that behave most
# similarly to one another
corPlot(hc)
} # }
```
