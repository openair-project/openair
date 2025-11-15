# Import air quality data from European database until February 2024

This function is a simplified version of the `saqgetr` package (see
<https://github.com/skgrange/saqgetr>) for accessing European air
quality data. As `saqgetr` was retired in February 2024, this function
has also been retired, but can still access European air quality data up
until that retirement date. Consider using the EEA Air Quality Download
Service instead (<https://eeadmz1-downloads-webapp.azurewebsites.net/>).

## Usage

``` r
importEurope(
  site = "debw118",
  year = 2018,
  tz = "UTC",
  meta = FALSE,
  to_narrow = FALSE,
  progress = TRUE
)
```

## Arguments

- site:

  The code of the site(s).

- year:

  Year or years to import. To import a sequence of years from 1990 to
  2000 use `year = 1990:2000`. To import several specific years use
  `year = c(1990, 1995, 2000)` for example.

- tz:

  Not used

- meta:

  Should meta data be returned? If `TRUE` the site type, latitude and
  longitude are returned.

- to_narrow:

  By default the returned data has a column for each pollutant/variable.
  When `to_narrow = TRUE` the data are stacked into a narrow format with
  a column identifying the pollutant name.

- progress:

  Show a progress bar when many sites/years are being imported? Defaults
  to `TRUE`.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## See also

Other import functions:
[`importADMS()`](https://openair-project.github.io/openair/reference/importADMS.md),
[`importAURN()`](https://openair-project.github.io/openair/reference/importUKAQ-wrapper.md),
[`importImperial()`](https://openair-project.github.io/openair/reference/importImperial.md),
[`importMeta()`](https://openair-project.github.io/openair/reference/importMeta.md),
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)

## Examples

``` r
# import data for Stuttgart Am Neckartor (S)
if (FALSE) { # \dontrun{
stuttgart <- importEurope("debw118", year = 2010:2019, meta = TRUE)
} # }
```
