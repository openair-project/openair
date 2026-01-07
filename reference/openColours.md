# Pre-defined openair colours and definition of user-defined colours

This in primarily an internal openair function to make it easy for users
to select particular colour schemes, or define their own range of
colours of a user-defined length.

## Usage

``` r
openColours(scheme = "default", n = 100)
```

## Arguments

- scheme:

  Any one of the pre-defined `openair` schemes (e.g., `"increment"`) or
  a user-defined palette (e.g., `c("red", "orange", "gold")`). See
  `?openColours` for a full list of available schemes.

- n:

  number of colours required.

## Value

A character vector of hex codes

## Schemes

The following schemes are made available by `openColours()`:

**Sequential Colours:**

- "default", "increment", "brewer1", "heat", "jet", "turbo", "hue",
  "greyscale".

- Simplified versions of the `viridis` colours: "viridis", "plasma",
  "magma", "inferno", "cividis", and "turbo".

- Simplified versions of the `RColorBrewer` sequential palettes:
  "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
  "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
  "YlOrBr", "YlOrRd".

**Diverging Palettes:**

- Simplified versions of the `RColorBrewer` diverging palettes: "BrBG",
  "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
  "Spectral".

**Qualitative Palettes:**

- Simplified versions of the `RColorBrewer` qualitative palettes:
  "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2",
  "Set3".

- "okabeito" (or "cbPalette"), a colour-blind safe palette based on the
  work of Masataka Okabe and Kei Ito
  (<https://jfly.uni-koeln.de/color/>)

- "tol.bright" (or "tol"), "tol.muted" and "tol.light", colour-blind
  safe palettes based on the work of Paul Tol.

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
exist over certain number of colour gradations (typically 3–10) — see
`?brewer.pal` for actual details. If less than or more than the required
number of colours is supplied then `openair` will interpolate the
colours.

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

<https://colorbrewer2.org/>

<https://check-air-quality.service.gov.uk/>

<https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>

## Author

David Carslaw

Jack Davison

## Examples

``` r
# to return 5 colours from the "jet" scheme:
cols <- openColours("jet", 5)
cols
#> [1] "#00007F" "#007FFF" "#7FFF7F" "#FF7F00" "#7F0000"

# to interpolate between named colours e.g. 10 colours from yellow to
#  green to red:
cols <- openColours(c("yellow", "green", "red"), 10)
cols
#>  [1] "#FFFF00" "#C6FF00" "#8DFF00" "#55FF00" "#1CFF00" "#1CE200" "#54AA00"
#>  [8] "#8D7100" "#C63800" "#FF0000"
```
