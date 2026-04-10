# Pre-defined openair colours and definition of user-defined colours

This in primarily an internal openair function to make it easy for users
to select particular colour schemes, or define their own range of
colours of a user-defined length. `openColours()` and `openColors()` are
synonyms.

## Usage

``` r
openColours(
  scheme = "default",
  n = NULL,
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1
)

openColors(
  scheme = "default",
  n = NULL,
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1
)
```

## Arguments

- scheme:

  Any one of the pre-defined `openair` schemes (e.g., `"increment"`) or
  a user-defined palette (e.g., `c("red", "orange", "gold")`). See
  `openColours()` for a full list of available schemes.

- n:

  The whole number of colours required. If not provided, sequential
  palettes will return `100` colours and qualitative palettes will
  return the maximum number of colours available for that scheme.

- alpha:

  The alpha transparency level (between `0` and `1`) for the colours.
  `0` is fully transparent, and `1` is fully opaque.

- begin, end:

  For sequential schemes, the fraction (between `0` and `1`) of the
  colour scheme to use. For example, if `begin = 0.2` and `end = 0.8`,
  only the middle 60% of the colour scheme will be used. This can be
  useful for avoiding very light or dark colours at the ends of schemes.

- direction:

  The order of the colours. `1` is the default and gives the normal
  order. `-1` will reverse the order of the colours.

## Value

A character vector of hex codes

## Schemes

The following schemes are made available by `openColours()`:

**Sequential Colours:**

- "default", "increment", "brewer1", "heat", "jet", "turbo", "hue",
  "greyscale".

- Simplified versions of the `viridis` colours: "viridis", "plasma",
  "magma", "inferno", "cividis", "turbo", "rocket" and "mako".

- Simplified versions of the `RColorBrewer` sequential palettes:
  "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
  "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
  "YlOrBr", "YlOrRd".

- Simplified versions of Fabio Crameri's sequential palettes: "acton",
  "bamO", "bamako", "batlow", "batlowK", "batlowW", "bilbao", "brocO",
  "buda", "corkO", "davos", "devon", "glasgow", "grayC", "hawaii",
  "imola", "lajolla", "lapaz", "lipari", "navia", "naviaW", "nuuk",
  "oslo", "romaO", "tokyo", "turku", "vikO".

**Diverging Palettes:**

- Simplified versions of the `RColorBrewer` diverging palettes: "BrBG",
  "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
  "Spectral".

- Simplified versions of Fabio Crameri's diverging palettes: "bam",
  "berlin", "broc", "cork", "lisbon", "managua", "roma", "tofino",
  "vanimo", "vik"

**Qualitative Palettes:**

- Simplified versions of the `RColorBrewer` qualitative palettes:
  "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2",
  "Set3".

- "okabeito" (or "cbPalette"), a colour-blind safe palette based on the
  work of Masataka Okabe and Kei Ito
  (<https://jfly.uni-koeln.de/color/>)

- "tol.bright" (or "tol"), "tol.highcontrast", "tol.vibrant",
  "tol.muted", "tol.mediumcontrast", "tol.pale", "tol.dark", and
  "tol.light"; colour-blind safe palettes based on the work of Paul Tol.

- "tableau" and "observable", aliases for the "Tableau10"
  (<https://www.tableau.com/blog/colors-upgrade-tableau-10-56782>) and
  "Observable10" (<https://observablehq.com/blog/crafting-data-colors>)
  colour palettes. These could be useful for consistency between openair
  plots and with figures made in Tableau or Observable Plot.

**UK Government Palettes:**

- "daqi" and "daqi.bands", the colours associated with the UK daily air
  quality index; "daqi" (a palette of 10 colours, corresponding to each
  index value) or "daqi.bands" (4 colours, corresponding to each band -
  Low, Moderate, High, and Very High). These colours were taken directly
  from <https://check-air-quality.service.gov.uk/> and may be useful in
  figures like
  [`calendarPlot()`](https://openair-project.github.io/openair/reference/calendarPlot.md).

- "gaf.cat", "gaf.focus" and "gaf.seq", colours recommended by the UK
  Government Analysis function
  (<https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>).
  "gaf.cat" will return the 'categorical' palette (max 6 colours),
  "gaf.focus" the 'focus' palette (max 2 colours), and "gaf.seq" the
  'sequential' palette.

## Details

Because of the way many of the schemes have been developed they only
exist over certain number of colour gradations (typically 3–10). If less
than or more than the required number of colours is supplied then
`openair` will interpolate the colours.

Each of the pre-defined schemes have merits and their use will depend on
a particular situation. For showing incrementing concentrations, e.g.,
high concentrations emphasised, then "default", "heat", "jet", "turbo",
and "increment" are very useful. See also the description of
`RColorBrewer` schemes for the option `scheme`.

To colour-code categorical-type problems, e.g., colours for different
pollutants, "hue" and "brewer1" are useful.

When publishing in black and white, "greyscale" is often convenient.
With most openair functions, as well as generating a greyscale colour
gradient, it also resets strip background and other coloured text and
lines to greyscale values.

Failing that, the user can define their own schemes based on R colour
names. To see the full list of names, type
[`colors()`](https://rdrr.io/r/grDevices/colors.html) into R.

## References

Color Brewer: <https://colorbrewer2.org/>

DAQI Colours: <https://check-air-quality.service.gov.uk/>

UK Government Analysis Function:
<https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>

Fabio Crameri's Color Schemes: Crameri, F. 2023. Scientific Colour Maps
(version 8.0.1). Zenodo. DOI:
[doi:10.5281/zenodo.1243862](https://doi.org/10.5281/zenodo.1243862) .

## Author

David Carslaw

Jack Davison

## Examples

``` r
# to return 5 colours from the "jet" scheme:
cols <- openColours("jet", 5)
cols
#> [1] "#00007FFF" "#007FFFFF" "#7FFF7FFF" "#FF7F00FF" "#7F0000FF"

# to interpolate between named colours e.g. 10 colours from yellow to
#  green to red:
cols <- openColours(c("yellow", "green", "red"), 10)
cols
#>  [1] "#FFFF00FF" "#C6FF00FF" "#8DFF00FF" "#55FF00FF" "#1CFF00FF" "#1CE200FF"
#>  [7] "#54AA00FF" "#8D7100FF" "#C63800FF" "#FF0000FF"
```
