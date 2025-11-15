# Import monitoring site meta data for UK and European networks

Function to import meta data for air quality monitoring sites. By
default, the function will return the site latitude, longitude and site
type, as well as the code used in functions like
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md),
[`importImperial()`](https://openair-project.github.io/openair/reference/importImperial.md)
and
[`importEurope()`](https://openair-project.github.io/openair/reference/importEurope.md).
Additional information may optionally be returned.

## Usage

``` r
importMeta(source = "aurn", all = FALSE, year = NA, duplicate = FALSE)
```

## Arguments

- source:

  One or more air quality networks for which data is available through
  openair. Available networks include:

  - `"aurn"`, The UK Automatic Urban and Rural Network.

  - `"aqe"`, The Air Quality England Network.

  - `"saqn"`, The Scottish Air Quality Network.

  - `"waqn"`, The Welsh Air Quality Network.

  - `"ni"`, The Northern Ireland Air Quality Network.

  - `"local"`, Locally managed air quality networks in England.

  - `"imperial"`, Imperial College London (formerly King's College
    London) networks.

  - `"europe"`, European AirBase/e-reporting data.

  There are two additional options provided for convenience:

  - `"ukaq"` will return metadata for all networks for which data is
    imported by
    [`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)
    (i.e., AURN, AQE, SAQN, WAQN, NI, and the local networks).

  - `"all"` will import all available metadata (i.e., `"ukaq"` plus
    `"imperial"` and `"europe"`).

- all:

  When `all = FALSE` only the site code, site name, latitude and
  longitude and site type are imported. Setting `all = TRUE` will import
  all available meta data and provide details (when available) or the
  individual pollutants measured at each site.

- year:

  If a single year is selected, only sites that were open at some point
  in that year are returned. If `all = TRUE` only sites that measured a
  particular pollutant in that year are returned. Year can also be a
  sequence e.g. `year = 2010:2020` or of length 2 e.g.
  `year = c(2010, 2020)`, which will return only sites that were open
  over the duration. Note that `year` is ignored when the `source` is
  either `"imperial"` or `"europe"`.

- duplicate:

  Some UK air quality sites are part of multiple networks, so could
  appear more than once when `source` is a vector of two or more. The
  default argument, `FALSE`, drops duplicate sites. `TRUE` will return
  them.

## Value

A data frame with meta data.

## Details

This function imports site meta data from several networks in the UK and
Europe:

- `"aurn"`, The [UK Automatic Urban and Rural
  Network](https://uk-air.defra.gov.uk/).

- `"aqe"`, The [Air Quality England
  Network](https://www.airqualityengland.co.uk/).

- `"saqn"`, The [Scottish Air Quality
  Network](https://www.scottishairquality.scot/).

- `"waqn"`, The [Welsh Air Quality
  Network](https://airquality.gov.wales/).

- `"ni"`, The [Northern Ireland Air Quality
  Network](https://www.airqualityni.co.uk/).

- `"local"`, [Locally
  managed](https://uk-air.defra.gov.uk/networks/network-info?view=nondefraaqmon)
  air quality networks in England.

- `"imperial"`, Imperial College London (formerly King's College London)
  networks.

- `"europe"`, Hourly European data (Air Quality e-Reporting) based on a
  simplified version of the `{saqgetr}` package.

By default, the function will return the site latitude, longitude and
site type. If the option `all = TRUE` is used, much more detailed
information is returned. The following metadata columns are available in
the complete dataset:

- **source**: The network with which the site is associated. Note that
  some monitoring sites are part of multiple networks (e.g., the AURN &
  SAQN) so the same site may feature twice under different sources.

- **code**: The site code, used to import data from specific sites of
  interest.

- **site**: The site name, which is more human-readable than the site
  code.

- **site_type**: A description of the site environment. Read more at
  <https://uk-air.defra.gov.uk/networks/site-types>.

- **latitude** and **longitude**: The coordinates of the monitoring
  station, using the World Geodetic System (<https://epsg.io/4326>).

- **start_date** and **end_date**: The opening and closing dates of the
  monitoring station. If `by_pollutant = TRUE`, these dates are instead
  the first and last dates at which specific pollutants were measured. A
  missing value, `NA`, indicates that monitoring is ongoing.

- **ratified_to**: The date to which data has been ratified (i.e.,
  'quality checked'). Data after this date is subject to change.

- **zone** and **agglomeration**: The UK is divided into agglomeration
  zones (large urban areas) and non-agglomeration zones for air quality
  assessment, which are given in these columns.

- **local_authority**: The local authority in which the monitoring
  station is found.

- **provider** and **code**: The specific provider of the locally
  managed dataset (e.g., `"londonair"`).

Thanks go to Trevor Davies (Ricardo), Dr Stuart Grange (EMPA) and Dr Ben
Barratt (KCL) and for making these data available.

## See also

the `networkMap()` function from the `openairmaps` package which can
visualise site metadata on an interactive map.

Other import functions:
[`importADMS()`](https://openair-project.github.io/openair/reference/importADMS.md),
[`importAURN()`](https://openair-project.github.io/openair/reference/importUKAQ-wrapper.md),
[`importEurope()`](https://openair-project.github.io/openair/reference/importEurope.md),
[`importImperial()`](https://openair-project.github.io/openair/reference/importImperial.md),
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)

## Author

David Carslaw

## Examples

``` r
if (FALSE) { # \dontrun{
# basic info:
meta <- importMeta(source = "aurn")

# more detailed information:
meta <- importMeta(source = "aurn", all = TRUE)

# from the Scottish Air Quality Network:
meta <- importMeta(source = "saqn", all = TRUE)

# from multiple networks:
meta <- importMeta(source = c("aurn", "aqe", "local"))
} # }
```
