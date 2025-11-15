# Import data from Imperial College London networks

Function for importing hourly mean data from Imperial College London
networks, formerly the King's College London networks. Files are
imported from a remote server operated by Imperial College London that
provides air quality data files as R data objects.

## Usage

``` r
importImperial(
  site = "my1",
  year = 2009,
  pollutant = "all",
  meta = FALSE,
  meteo = FALSE,
  extra = FALSE,
  units = "mass",
  to_narrow = FALSE,
  progress = TRUE
)

importKCL(
  site = "my1",
  year = 2009,
  pollutant = "all",
  met = FALSE,
  units = "mass",
  extra = FALSE,
  meta = FALSE,
  to_narrow = FALSE,
  progress = TRUE
)
```

## Arguments

- site:

  Site code of the network site to import e.g. "my1" is Marylebone Road.
  Several sites can be imported with `site = c("my1", "kc1")` — to
  import Marylebone Road and North Kensignton for example.

- year:

  Year(s) to import. To import a series of years use, e.g., `2000:2020`.
  To import several specific years use `year = c(2000, 2010, 2020)`.

- pollutant:

  Pollutants to import. If omitted will import all pollutants from a
  site. To import only NOx and NO2 for example use
  `pollutant = c("nox", "no2")`. Pollutant names can be upper or lower
  case.

- meta:

  Append metadata columns to data for each selected `site`? Defaults to
  `FALSE`. Columns are defined using `meta_columns`.

- meteo, met:

  Should meteorological data be added to the import data? The default is
  `FALSE`. If `TRUE` wind speed (m/s), wind direction (degrees), solar
  radiation and rain amount are available. See details below.

- extra:

  Defaults to `FALSE`. When `TRUE`, returns additional data.

- units:

  By default the returned data frame expresses the units in mass terms
  (ug/m3 for NOx, NO2, O3, SO2; mg/m3 for CO). Use `units = "volume"` to
  use ppb etc. PM10_raw TEOM data are multiplied by 1.3 and PM2.5 have
  no correction applied. See details below concerning PM10
  concentrations.

- to_narrow:

  Return the data in a "narrow"/"long"/"tidy" format? By default the
  returned data is "wide" and has a column for each pollutant/variable.
  When `to_narrow = TRUE` the data are returned with a column
  identifying the pollutant name and a column containing the
  corresponding concentration/statistic. Defaults to `FALSE`.

- progress:

  Show a progress bar when many sites/years are being imported? Defaults
  to `TRUE`.

## Value

Returns a data frame of hourly mean values with date in POSIXct class
and time zone GMT.

## Details

The `importImperial()` function has been written to make it easy to
import data from the Imperial College London air pollution networks.
Imperial have provided .RData files (R workspaces) of all individual
sites and years for the Imperial networks. These files are updated on a
weekly basis. This approach requires a link to the Internet to work.

There are several advantages over the web portal approach where .csv
files are downloaded. First, it is quick to select a range of sites,
pollutants and periods (see examples below). Second, storing the data as
.RData objects is very efficient as they are about four times smaller
than .csv files — which means the data downloads quickly and saves
bandwidth. Third, the function completely avoids any need for data
manipulation or setting time formats, time zones etc. Finally, it is
easy to import many years of data beyond the current limit of about
64,000 lines. The final point makes it possible to download several long
time series in one go. The function also has the advantage that the
proper site name is imported and used in \`openair“ functions.

The site codes and pollutant names can be upper or lower case. The
function will issue a warning when data less than six months old is
downloaded, which may not be ratified.

The data are imported by stacking sites on top of one another and will
have field names `date`, `site`, `code` (the site code) and
pollutant(s). Sometimes it is useful to have columns of site data. This
can be done using the
[`reshape()`](https://rdrr.io/r/stats/reshape.html) function — see
examples below.

The situation for particle measurements is not straightforward given the
variety of methods used to measure particle mass and changes in their
use over time. The `importImperial()` function imports two measures of
PM10 where available. `PM10_raw` are TEOM measurements with a 1.3 factor
applied to take account of volatile losses. The `PM10` data is a current
best estimate of a gravimetric equivalent measure as described below.
NOTE! many sites have several instruments that measure PM10 or PM2.5. In
the case of FDMS measurements, these are given as separate site codes
(see below). For example "MY1" will be TEOM with VCM applied and "MY7"
is the FDMS data.

Where FDMS data are used the volatile and non-volatile components are
separately reported i.e. v10 = volatile PM10, v2.5 = volatile PM2.5,
nv10 = non-volatile PM10 and nv2.5 = non-volatile PM2.5. Therefore, PM10
= v10 + nv10 and PM2.5 = v2.5 + nv2.5.

For the assessment of the EU Limit Values, PM10 needs to be measured
using the reference method or one shown to be equivalent to the
reference method. Defra carried out extensive trials between 2004 and
2006 to establish which types of particulate analysers in use in the UK
were equivalent. These trials found that measurements made using
Partisol, FDMS, BAM and SM200 instruments were shown to be equivalent to
the PM10 reference method. However, correction factors need to be
applied to measurements from the SM200 and BAM instruments. Importantly,
the TEOM was demonstrated as not being equivalent to the reference
method due to the loss of volatile PM, even when the 1.3 correction
factor was applied. The Volatile Correction Model (VCM) was developed
for Defra at King's College to allow measurements of PM10 from TEOM
instruments to be converted to reference equivalent; it uses the
measurements of volatile PM made using nearby FDMS instruments to
correct the measurements made by the TEOM. It passed the equivalence
testing using the same methodology used in the Defra trials and is now
the recommended method for correcting TEOM measurements (Defra, 2009).
VCM correction of TEOM measurements can only be applied after 1st
January 2004, when sufficiently widespread measurements of volatile PM
became available. The 1.3 correction factor is now considered redundant
for measurements of PM10 made after 1st January 2004. Further
information on the VCM can be found at
<http://www.volatile-correction-model.info/>.

All PM10 statistics on the LondonAir web site, including the bulletins
and statistical tools (and in the RData objects downloaded using
`importImperial()`), now report PM10 results as reference equivalent.
For PM10 measurements made by BAM and SM200 analysers the applicable
correction factors have been applied. For measurements from TEOM
analysers the 1.3 factor has been applied up to 1st January 2004, then
the VCM method has been used to convert to reference equivalent.

The meteorological data are meant to represent 'typical' conditions in
London, but users may prefer to use their own data. The data provide a
an estimate of general meteorological conditions across Greater London.
For meteorological species (wd, ws, rain, solar) each data point is
formed by averaging measurements from a subset of LAQN monitoring sites
that have been identified as having minimal disruption from local
obstacles and a long term reliable dataset. The exact sites used varies
between species, but include between two and five sites per species.
Therefore, the data should represent 'London scale' meteorology, rather
than local conditions.

`importKCL()` is equivalent to `importImperial()` and is provided for
back-compatibility reasons only. New users should use
`importImperial()`.

## See also

Other import functions:
[`importADMS()`](https://openair-project.github.io/openair/reference/importADMS.md),
[`importAURN()`](https://openair-project.github.io/openair/reference/importUKAQ-wrapper.md),
[`importEurope()`](https://openair-project.github.io/openair/reference/importEurope.md),
[`importMeta()`](https://openair-project.github.io/openair/reference/importMeta.md),
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)

## Author

David Carslaw and Ben Barratt

## Examples

``` r
## import all pollutants from Marylebone Rd from 1990:2009
if (FALSE) { # \dontrun{
mary <- importImperial(site = "my1", year = 2000:2009)
} # }

## import nox, no2, o3 from Marylebone Road and North Kensington for 2000
if (FALSE) { # \dontrun{
thedata <-
  importImperial(
    site = c("my1", "kc1"),
    year = 2000,
    pollutant = c("nox", "no2", "o3")
  )
} # }

## import met data too...
if (FALSE) { # \dontrun{
my1 <- importImperial(site = "my1", year = 2008, meteo = TRUE)
} # }
```
