# Plot the distribution of a variable with conditioning

This function plots the distribution of one or more pollutants or other
variables as a histogram, kernel density estimate function, or empirical
cumulative distribution function. The choice of method comes with
trade-offs; for example, histograms are often more easily interpretable
but can easily get cluttered and are heavily dependent on bin width,
whereas density functions appear 'cleaner' with many overlapping groups,
but can be more challenging to interpret.

## Usage

``` r
distPlot(
  mydata,
  pollutant = "nox",
  method = c("histogram", "freqpoly", "density", "ecdf"),
  binwidth = NULL,
  bins = 30,
  position = NULL,
  log = FALSE,
  group = "default",
  type = "default",
  cols = "hue",
  theme = "default",
  key.title = group,
  key.position = "top",
  ref.x = NULL,
  ref.y = NULL,
  auto.text = TRUE,
  plot = TRUE,
  ...
)
```

## Arguments

- mydata:

  A data frame.

- pollutant:

  Name of the pollutant(s) to plot contained in `mydata`.

- method:

  One of: `"histogram"`, `"freqpoly"`, `"density"`, or `"ecdf"`. Note
  that `"freqpoly"` is effectively a line chart equivalent of a
  histogram, and may appear less cluttered with many groups.

- binwidth, bins:

  Used when `method = "histogram"` or `"freqpoly"`. `binwidth` sets the
  width of the bins. `bins` sets the number of bins, defaulting to `30`.
  `bins` is overridden by `binwidth`.

- position:

  A string representing a `ggplot2` "position" - see
  [`ggplot2::position_identity()`](https://ggplot2.tidyverse.org/reference/position_identity.html)
  and similar functions. When `NULL`, will use `"stack"` for histograms
  and `"identity"` for other methods. Also useful is `"fill"` in
  conjunction with the `group` argument which will 'normalise' the
  y-axis to show a percentage rather than an absolute count or density
  estimate. Not used when `method = "ecdf"`, which must be `"identity"`.
  Note that density functions will use 'count' over 'density' for
  non-identity `position`s.

- log:

  Should the x-axis appear on a log scale? The default is `FALSE`. If
  `TRUE` a well-formatted log10 scale is used.

- group:

  This sets the grouping variable to be used. For example, if a data
  frame had a column `site` setting `group = "site"` will plot all sites
  together in each panel. Passed to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md).

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

- theme:

  A string representing an overall plot theme, defaulting to
  `"default"`. This option makes sweeping changes to non-data plot
  features such as fonts, colours, line widths, and so on, and may also
  change default arguments like `cols` if not set by the user. Can also
  take a
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  object, which will be used to modify the `"default"` theme. Pre-set
  options include:

  - `"default"`, a lattice-inspired theme resembling the traditional
    `openair` look, with structured panels and visible gridlines.

  - `"dark"`, a dark-background variant of the default theme, designed
    for presentations and low-light viewing, using high-contrast text
    and colour palettes optimised for visibility against dark panels.

  - `"modern"`, a minimalist, contemporary theme inspired by tools such
    as Plotly and Observable Plot, with reduced visual clutter,
    horizontal emphasis in gridlines, a clean legend style, and
    typography suited to dashboards and reports.

  - `"soft"`, a low-contrast, 'editorial' theme with warm background
    tones, subtle gridlines, and gently desaturated colours, designed
    for reports and publication-style figures, particularly where a
    calmer appearance improves readability.

  - `"print"`, a strictly greyscale theme optimised for black-and-white
    reproduction, with stronger structural elements such as clearer
    gridlines and axis definitions to ensure good contrast and
    readability in printed or photocopied outputs.

  Please note that if a global theme is set with
  [`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  to anything other than the default
  [`ggplot2::theme_grey()`](https://ggplot2.tidyverse.org/reference/ggtheme.html),
  the selected openair theme will not be fully applied; instead, only
  minimal adjustments (such as legend positioning) will be made.

- key.title:

  Used to set the title of the legend. The legend title is passed to
  [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
  if `auto.text = TRUE`.

- key.position:

  Location where the legend is to be placed. Allowed arguments include
  `"top"`, `"right"`, `"bottom"`, `"left"` and `"none"`, the last of
  which removes the legend entirely.

- ref.x:

  Either a single value or values representing the x axis intercepts to
  draw lines, or a list such as that provided by
  [`refOpts()`](https://openair-project.github.io/openair/reference/refOpts.md)
  to customise the colour/width/type/etc. of each line. See
  [`refOpts()`](https://openair-project.github.io/openair/reference/refOpts.md)
  for more details.

- ref.y:

  Either a single value or values representing the y axis intercepts to
  draw lines, or a list such as that provided by
  [`refOpts()`](https://openair-project.github.io/openair/reference/refOpts.md)
  to customise the colour/width/type/etc. of each line. See
  [`refOpts()`](https://openair-project.github.io/openair/reference/refOpts.md)
  for more details.

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

  Addition options are passed on to
  [`cutData()`](https://openair-project.github.io/openair/reference/cutData.md)
  for `type` handling. Some additional arguments are also available,
  varying somewhat in different plotting functions:

  - `title`, `subtitle`, `caption`, `tag`, `xlab` and `ylab` control the
    plot title, subtitle, caption, tag, x-axis label and y-axis label,
    passed to
    [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
    via
    [`quickText()`](https://openair-project.github.io/openair/reference/quickText.md)
    if `auto.text = TRUE`.

  - `xlim`, `ylim` and `limits` control the limits of the x-axis, y-axis
    and colorbar scales.

  - `ncol` and `nrow` set the number of columns and rows in a faceted
    plot.

  - `scales` can be `"fixed"`, `"free_x"`, `"free_y"` or `"free"` to
    control whether axes are shared across facets when using `type`.
    Also supported are the legacy `x.relation` and `y.relation`, which
    can be either `"same"` or `"free"` and get remapped to `scales`
    automatically.

  - Similarly, `space`, `axes`, `axis.labels`, `switch` and
    `strip.position` can be used to customise the appearance of faceted
    plots. See
    [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
    and
    [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
    for the arguments these take.

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

## Value

an
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
object

## Author

Jack Davison

## Examples

``` r
distPlot(mydata, pollutant = "no2", group = "season")


if (FALSE) { # \dontrun{
distPlot(
  mydata,
  pollutant = "no2",
  group = "weekend",
  method = "density",
  cols = "tol"
)

distPlot(
  mydata,
  pollutant = "no2",
  group = "wd",
  position = "fill",
  wd.res = 4,
  alpha = 0.75,
  cols = "tol"
)
} # }
```
