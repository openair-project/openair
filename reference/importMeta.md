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
importMeta(
  source = "aurn",
  year = NULL,
  pollutant = NULL,
  code = NULL,
  site = NULL,
  site_type = NULL,
  lat = NULL,
  lng = NULL,
  crs = 4326,
  max_dist = NULL,
  max_n = NULL,
  all = FALSE,
  duplicate = FALSE
)
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

- year:

  If a single year is selected, only sites that were open at some point
  in that year are returned. If `all = TRUE` only sites that measured a
  particular pollutant in that year are returned. Year can also be a
  sequence e.g. `year = 2010:2020` or of length 2 e.g.
  `year = c(2010, 2020)`, which will return only sites that were open
  over the duration.

- pollutant:

  Character vectors used to search the metadata for the specified
  `source`s. `pollutant` is case-insensitive. For example,
  `pollutant = c("nox", "o3")` will return sites which measure *either*
  NOx *or* O3. Can also take the shorthand `"hc"`, will returns all
  hydrocarbons. Similar to `code`, values are matched exactly (e.g.,
  `pollutant = "no"` will only return NO and not NO2 or NOx). Note that
  `pollutant` only applies to networks available through
  [`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md).

- site, code, site_type:

  Character vectors used to search the metadata for the specified
  `source`s. All of `code`, `site` and `site_type` are case-insensitive.
  `code` and `site_type` are matched exactly, but `site` is 'pattern
  matched' - e.g., `site = "Sunderland"` and `source = "aurn"` will
  return data for `"Sunderland"`, `"Sunderland Silksworth"` and
  `"Sunderland Wessington Way"` (plus any future sites with the string
  `"Sunderland"` in their name).

- lat, lng:

  Decimal latitude (`lat`) and longitude (`lng`) (or other Y/X
  coordinate if using a different `crs`). If provided, the data will be
  returned with a `distance_km` column displaying the distance of each
  station from the target coordinate. The data will also be
  automatically sorted by this column.

- crs:

  The coordinate reference system (CRS) of the data, passed to
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).
  By default this is [EPSG:4326](https://epsg.io/4326), the CRS
  associated with the commonly used latitude and longitude coordinates.
  Different coordinate systems can be specified using `crs` (e.g.,
  `crs = 27700` for the British National Grid). Note that non-lat/lng
  coordinate systems will be re-projected to EPSG:4326 for comparison
  with the site metadata.

- max_dist, max_n:

  If `lat` and `lng` are provided, `max_dist` and `max_n` further filter
  the metadata. `max_dist` defines a maximum distance from the target
  coordinate in kilometers, and `max_n` a maximum number of sites to be
  returned. `max_n` is applied after `max_dist`.

- all:

  When `all = FALSE` only the site code, site name, latitude and
  longitude and site type are imported. Setting `all = TRUE` will import
  all available meta data and provide details (when available) or the
  individual pollutants measured at each site.

- duplicate:

  Some UK air quality sites are part of multiple networks, so could
  appear more than once when `source` is a vector of two or more. The
  default argument, `FALSE`, drops duplicate sites. `TRUE` will return
  them.

## Value

A data frame

## Available Networks

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
  simplified version of the `{saqgetr}` package. Note that this data is
  only available until February 2024; see
  [`importEurope()`](https://openair-project.github.io/openair/reference/importEurope.md)
  for more information.

## Order of Operations

This function contains various arguments which allow the user to filter
the metadata before it is returned. These arguments are applied in the
following order:

- `source`

- `year`

- `pollutant` (where possible)

- `code`

- `site`

- `site_type`

- `max_dist`

- `max_n`

Note that `max_n` is not *always* the number of rows returned by the
function; it is the maximum number of possible sites to be returned. If
`all = TRUE`, multiple rows will be present per site. Further, if
previous filtering steps mean that fewer than `max_n` sites are
remaining in the data, `max_n` will have no effect.

If the combination of arguments provided results in the removal of all
sites, this function will return an empty dataframe with a warning.

## Data Dictionary

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

- **site_type**: A description of the site environment.

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

## References

Thanks go to Trevor Davies (WSP), Dr Stuart Grange (EMPA) and Dr Ben
Barratt (Imperial College) for making these data available.

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

Jack Davison

## Examples

``` r
if (FALSE) { # \dontrun{
# Get information for the AURN
meta <- importMeta(source = "aurn")

# More detailed information
meta <- importMeta(source = "aurn", all = TRUE)

# From the AURN and SAQN
meta <- importMeta(source = c("aurn", "saqn"))

# Sites in the UK measuring SO2 or CO
meta <- importMeta(source = "ukaq", pollutant = c("so2", "co"))

# English sites within 5km of Buckingham Palace
meta <- importMeta(
  source = c("aurn", "aqe", "local"),
  lat = 51.50101,
  lng = -0.141563,
  max_dist = 5
)
} # }
```
