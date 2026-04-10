# List available colour schemes in `openair`

This function returns a table of the available colour schemes in
`openair`, along with their type (sequential, diverging, or qualitative)
and the maximum number of colours available for that scheme (where
applicable). This can be useful for users to explore the available
schemes and to check which schemes can provide a certain number of
colours.

## Usage

``` r
openSchemes(palette_type = c("seq", "div", "qual"), n = NULL)
```

## Arguments

- palette_type:

  A character vector specifying which types of palettes to include in
  the output. Options are `"seq"` for sequential, `"div"` for diverging,
  and `"qual"` for qualitative. Multiple options can be selected (e.g.,
  `c("seq", "div")`). All three are returned by default.

- n:

  An optional whole number to filter the schemes by their maximum number
  of colours. Only schemes that can provide at least `n` colours will be
  included in the output. This only applies to qualitative schemes, as
  sequential and diverging schemes can be interpolated to any number of
  colours.

## Author

Jack Davison
