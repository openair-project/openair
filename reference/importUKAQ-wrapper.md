# Import data from individual UK Air Pollution Networks

These functions act as wrappers for
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)
to import air pollution data from a range of UK networks including the
Automatic Urban and Rural Network (AURN), the individual England (AQE),
Scotland (SAQN), Wales (WAQN) and Northern Ireland (NI) Networks, and
many "locally managed" monitoring networks across England. While
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)
allows for data to be imported more flexibly, including across multiple
monitoring networks, these functions are provided for convenience and
back-compatibility.

## Usage

``` r
importAURN(
  site = "my1",
  year = 2009,
  data_type = "hourly",
  pollutant = "all",
  hc = FALSE,
  meta = FALSE,
  meta_columns = c("site_type", "latitude", "longitude"),
  meteo = TRUE,
  ratified = FALSE,
  to_narrow = FALSE,
  verbose = FALSE,
  progress = TRUE
)

importAQE(
  site = "yk13",
  year = 2018,
  data_type = "hourly",
  pollutant = "all",
  meta = FALSE,
  meta_columns = c("site_type", "latitude", "longitude"),
  meteo = TRUE,
  ratified = FALSE,
  to_narrow = FALSE,
  verbose = FALSE,
  progress = TRUE
)

importSAQN(
  site = "gla4",
  year = 2009,
  data_type = "hourly",
  pollutant = "all",
  meta = FALSE,
  meta_columns = c("site_type", "latitude", "longitude"),
  meteo = TRUE,
  ratified = FALSE,
  to_narrow = FALSE,
  verbose = FALSE,
  progress = TRUE
)

importWAQN(
  site = "card",
  year = 2018,
  data_type = "hourly",
  pollutant = "all",
  meta = FALSE,
  meta_columns = c("site_type", "latitude", "longitude"),
  meteo = TRUE,
  ratified = FALSE,
  to_narrow = FALSE,
  verbose = FALSE,
  progress = TRUE
)

importNI(
  site = "bel0",
  year = 2018,
  data_type = "hourly",
  pollutant = "all",
  meta = FALSE,
  meta_columns = c("site_type", "latitude", "longitude"),
  meteo = TRUE,
  ratified = FALSE,
  to_narrow = FALSE,
  verbose = FALSE,
  progress = TRUE
)

importLocal(
  site = "ad1",
  year = 2018,
  data_type = "hourly",
  pollutant = "all",
  meta = FALSE,
  meta_columns = c("site_type", "latitude", "longitude"),
  to_narrow = FALSE,
  verbose = FALSE,
  progress = TRUE
)
```

## Arguments

- site:

  Site code of the site to import, e.g., `"my1"` is Marylebone Road.
  Site codes can be discovered through the use of
  [`importMeta()`](https://openair-project.github.io/openair/reference/importMeta.md).
  Several sites can be imported at once. For example,
  `site = c("my1", "nott")` imports both Marylebone Road and Nottingham.

- year:

  Year(s) to import. To import a series of years use, e.g., `2000:2020`.
  To import several specific years use `year = c(2000, 2010, 2020)`.

- data_type:

  The type of data to be returned, defaulting to `"hourly"` data.
  Alternative data types include:

  - `"daily"`: Daily average data.

  - `"monthly"`: Monthly average data with data capture information for
    the whole network.

  - `"annual"`: Annual average data with data capture information for
    the whole network.

  - `"15_min"`: 15-minute average SO2 concentrations.

  - `"8_hour"`: 8-hour rolling mean concentrations for O3 and CO.

  - `"24_hour"`: 24-hour rolling mean concentrations for particulates.

  - `"daily_max_8"`: Maximum daily rolling 8-hour maximum for O3 and CO.

  - `"daqi"`: Daily Air Quality Index (DAQI). See
    [here](https://uk-air.defra.gov.uk/air-pollution/daqi?view=more-info)
    for more details of how the index is defined. Note that this
    `data_type` is not available for locally managed monitoring
    networks.

- pollutant:

  Pollutants to import. If omitted will import all pollutants from a
  site. To import only NOx and NO2 for example use
  `pollutant = c("nox", "no2")`. Pollutant names can be upper or lower
  case.

- hc:

  Include hydrocarbon measurements in the imported data? Defaults to
  `FALSE` as most users will not be interested in using hydrocarbon
  data.

- meta:

  Append metadata columns to data for each selected `site`? Defaults to
  `FALSE`. Columns are defined using `meta_columns`.

- meta_columns:

  The specific columns to append when `meta = TRUE`. Defaults to site
  type, latitude and longitude. Can be any of `"site_type"`,
  `"latitude"`, `"longitude"`, `"zone"`, `"agglomeration"`, and
  `"local_authority"` (as well as `"provider"` for locally managed
  data). See
  [`importMeta()`](https://openair-project.github.io/openair/reference/importMeta.md)
  for more complete information.

- meteo:

  Append modelled meteorological data, if available? Defaults to `TRUE`,
  which will return wind speed (`ws`), wind direction (`wd`) and ambient
  temperature (`air_temp`). The variables are calculated from using the
  WRF model run by Ricardo Energy & Environment and are available for
  most but not all networks. Setting `meteo = FALSE` is useful if you
  have other meteorological data to use in preference, for example from
  the `worldmet` package.

- ratified:

  Append `qc` column(s) to hourly data indicating whether each species
  was ratified (i.e., quality-checked)? Defaults to `FALSE`.

- to_narrow:

  Return the data in a "narrow"/"long"/"tidy" format? By default the
  returned data is "wide" and has a column for each pollutant/variable.
  When `to_narrow = TRUE` the data are returned with a column
  identifying the pollutant name and a column containing the
  corresponding concentration/statistic. Defaults to `FALSE`.

- verbose:

  Print messages to the console if hourly data cannot be imported?
  Default is `FALSE`. `TRUE` is useful for debugging as the specific
  `year`(s), `site`(s) and `source`(s) which cannot be imported will be
  returned.

- progress:

  Show a progress bar when many sites/years are being imported? Defaults
  to `TRUE`.

## Importing UK Air Pollution Data

This family of functions has been written to make it easy to import data
from across several UK air quality networks. Ricardo have provided
.RData files (R workspaces) of all individual sites and years, as well
as up to date meta data. These files are updated on a daily basis. This
approach requires a link to the Internet to work.

There are several advantages over the web portal approach where .csv
files are downloaded.

- First, it is quick to select a range of sites, pollutants and periods
  (see examples below).

- Second, storing the data as .RData objects is very efficient as they
  are about four times smaller than .csv files — which means the data
  downloads quickly and saves bandwidth.

- Third, the function completely avoids any need for data manipulation
  or setting time formats, time zones etc. The function also has the
  advantage that the proper site name is imported and used in
  [openair](https://openair-project.github.io/openair/reference/openair-package.md)
  functions.

Users should take care if using data from both
[openair](https://openair-project.github.io/openair/reference/openair-package.md)
*and* web portals (for example, [UK
AIR](https://uk-air.defra.gov.uk/data/)). One key difference is that the
data provided by openair is date *beginning*, whereas the web portal
provides date *ending*. Hourly concentrations may therefore appear
offset by an hour, for example.

The data are imported by stacking sites on top of one another and will
have field names `site`, `code` (the site code) and `pollutant`.

By default, the function returns hourly average data. However, annual,
monthly, daily and 15 minute data (for SO2) can be returned using the
option `data_type`. Annual and monthly data provide whole network
information including data capture statistics.

All units are expressed in mass terms for gaseous species (ug/m3 for NO,
NO2, NOx (as NO2), SO2 and hydrocarbons; and mg/m3 for CO). PM10
concentrations are provided in gravimetric units of ug/m3 or scaled to
be comparable with these units. Over the years a variety of instruments
have been used to measure particulate matter and the technical issues of
measuring PM10 are complex. In recent years the measurements rely on
FDMS (Filter Dynamics Measurement System), which is able to measure the
volatile component of PM. In cases where the FDMS system is in use there
will be a separate volatile component recorded as 'v10' and non-volatile
component 'nv10', which is already included in the absolute PM10
measurement. Prior to the use of FDMS the measurements used TEOM
(Tapered Element Oscillating. Microbalance) and these concentrations
have been multiplied by 1.3 to provide an estimate of the total mass
including the volatile fraction.

Some sites report hourly and daily PM10 and / or PM2.5. When
`data_type = "daily"` and there are both hourly and 'proper' daily
measurements available, these will be returned as e.g. "pm2.5" and
"gr_pm2.5"; the former corresponding to data based on original hourly
measurements and the latter corresponding to daily gravimetric
measurements.

The function returns modelled hourly values of wind speed (`ws`), wind
direction (`wd`) and ambient temperature (`air_temp`) if available
(generally from around 2010). These values are modelled using the WRF
model operated by Ricardo.

The BAM (Beta-Attenuation Monitor) instruments that have been
incorporated into the network throughout its history have been scaled by
1.3 if they have a heated inlet (to account for loss of volatile
particles) and 0.83 if they do not have a heated inlet. The few TEOM
instruments in the network after 2008 have been scaled using VCM
(Volatile Correction Model) values to account for the loss of volatile
particles. The object of all these scaling processes is to provide a
reasonable degree of comparison between data sets and with the reference
method and to produce a consistent data record over the operational
period of the network, however there may be some discontinuity in the
time series associated with instrument changes.

No corrections have been made to the PM2.5 data. The volatile component
of FDMS PM2.5 (where available) is shown in the 'v2.5' column.

## See also

Other import functions:
[`importADMS()`](https://openair-project.github.io/openair/reference/importADMS.md),
[`importEurope()`](https://openair-project.github.io/openair/reference/importEurope.md),
[`importImperial()`](https://openair-project.github.io/openair/reference/importImperial.md),
[`importMeta()`](https://openair-project.github.io/openair/reference/importMeta.md),
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)
