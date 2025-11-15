# Import pre-calculated HYSPLIT 96-hour back trajectories

Function to import pre-calculated back trajectories using the NOAA
HYSPLIT model. The trajectories have been calculated for a select range
of locations which will expand in time. They cover the last 20 years or
so and can be used together with other `openair` functions.

## Usage

``` r
importTraj(site = "london", year = 2009, local = NA, progress = TRUE)
```

## Arguments

- site:

  Site code of the network site to import e.g. "london". Only one site
  can be imported at a time. The following sites are typically available
  from 2000-2012, although some UK ozone sites go back to 1988 (code,
  location, lat, lon, year):

  |            |                             |           |            |           |
  |------------|-----------------------------|-----------|------------|-----------|
  | abudhabi   | Abu Dhabi                   | 24.43000  | 54.408000  | 2012-2013 |
  | ah         | Aston Hill                  | 52.50385  | -3.041780  | 1988-2013 |
  | auch       | Auchencorth Moss            | 55.79283  | -3.242568  | 2006-2013 |
  | berlin     | Berlin, Germany             | 52.52000  | 13.400000  | 2000-2013 |
  | birm       | Birmigham Centre            | 52.47972  | -1.908078  | 1990-2013 |
  | boston     | Boston, USA                 | 42.32900  | -71.083000 | 2008-2013 |
  | bot        | Bottesford                  | 52.93028  | -0.814722  | 1990-2013 |
  | bukit      | Bukit Kototabang, Indonesia | -0.19805  | 100.318000 | 1996-2013 |
  | chittagong | Chittagong, Bangladesh      | 22.37000  | 91.800000  | 2010-2013 |
  | dhaka      | Dhaka, Bangladesh           | 23.70000  | 90.375000  | 2010-2013 |
  | ed         | Edinburgh                   | 55.95197  | -3.195775  | 1990-2013 |
  | elche      | Elche, Spain                | 38.27000  | -0.690000  | 2004-2013 |
  | esk        | Eskdalemuir                 | 55.31530  | -3.206110  | 1998-2013 |
  | gibraltar  | Gibraltar                   | 36.13400  | -5.347000  | 2005-2010 |
  | glaz       | Glazebury                   | 53.46008  | -2.472056  | 1998-2013 |
  | groningen  | Groningen                   | 53.40000  | 6.350000   | 2007-2013 |
  | har        | Harwell                     | 51.57108  | -1.325283  | 1988-2013 |
  | hk         | Hong Kong                   | 22.29000  | 114.170000 | 1998-2013 |
  | hm         | High Muffles                | 54.33500  | -0.808600  | 1988-2013 |
  | kuwait     | Kuwait City                 | 29.36700  | 47.967000  | 2008-2013 |
  | lb         | Ladybower                   | 53.40337  | -1.752006  | 1988-2013 |
  | london     | Central London              | 51.50000  | -0.100000  | 1990-2013 |
  | lh         | Lullington Heath            | 50.79370  | 0.181250   | 1988-2013 |
  | ln         | Lough Navar                 | 54.43951  | -7.900328  | 1988-2013 |
  | mh         | Mace Head                   | 53.33000  | -9.900000  | 1988-2013 |
  | ny-alesund | Ny-Alesund, Norway          | 78.91763  | 11.894653  | 2009-2013 |
  | oslo       | Oslo                        | 59.90000  | 10.750000  | 2010-2013 |
  | paris      | Paris, France               | 48.86200  | 2.339000   | 2000-2013 |
  | roch       | Rochester Stoke             | 51.45617  | 0.634889   | 1988-2013 |
  | rotterdam  | Rotterdam                   | 51.91660  | 4.475000   | 2010-2013 |
  | saopaulo   | Sao Paulo                   | -23.55000 | -46.640000 | 2000-2013 |
  | sib        | Sibton                      | 52.29440  | 1.463970   | 1988-2013 |
  | sv         | Strath Vaich                | 57.73446  | -4.776583  | 1988-2013 |
  | wuhan      | Wuhan, China                | 30.58300  | 114.280000 | 2008-2013 |
  | yw         | Yarner Wood                 | 50.59760  | -3.716510  | 1988-2013 |

- year:

  Year or years to import. To import a sequence of years from 1990 to
  2000 use `year = 1990:2000`. To import several specific years use
  `year = c(1990, 1995, 2000)` for example.

- local:

  File path to .RData trajectory files run by user and not stored on the
  Ricardo web server. These files would have been generated from the
  Hysplit trajectory code shown in the appendix of the openair manual.
  An example would be `local = 'c:/users/david/TrajFiles/'`.

- progress:

  Show a progress bar when many receptors/years are being imported?
  Defaults to `TRUE`.

## Value

Returns a data frame with pre-calculated back trajectories.

## Details

This function imports pre-calculated back trajectories using the HYSPLIT
trajectory model (Hybrid Single Particle Lagrangian Integrated
Trajectory Model. Back trajectories provide some very useful information
for air quality data analysis. However, while they are commonly
calculated by researchers it is generally difficult for them to be
calculated on a routine basis and used easily. In addition, the
availability of back trajectories over several years can be very useful,
but again difficult to calculate.

Trajectories are run at 3-hour intervals and stored in yearly files (see
below). The trajectories are started at ground-level (10m) and
propagated backwards in time.

These trajectories have been calculated using the Global NOAA-NCEP/NCAR
reanalysis data archives. The global data are on a latitude-longitude
grid (2.5 degree). Note that there are many different meteorological
data sets that can be used to run HYSPLIT e.g. including ECMWF data.
However, in order to make it practicable to run and store trajectories
for many years and sites, the NOAA-NCEP/NCAR reanalysis data is most
useful. In addition, these archives are available for use widely, which
is not the case for many other data sets e.g. ECMWF. HYSPLIT calculated
trajectories based on archive data may be distributed without
permission. For those wanting, for example, to consider higher
resolution meteorological data sets it may be better to run the
trajectories separately.

We are extremely grateful to NOAA for making HYSPLIT available to
produce back trajectories in an open way. We ask that you cite HYSPLIT
if used in published work.

Users can supply their own trajectory files to plot in openair. These
files must have the following fields: date, lat, lon and hour.inc (see
details below).

The files consist of the following information:

- date:

  This is the arrival point time and is repeated the number of times
  equal to the length of the back trajectory — typically 96 hours
  (except early on in the file). The format is `POSIXct`. It is this
  field that should be used to link with air quality data. See example
  below.

- receptor:

  Receptor number, currently only 1.

- year:

  The year

- month:

  Month 1-12

- day:

  Day of the month 1-31

- hour:

  Hour of the day 0-23 GMT

- hour.inc:

  Number of hours back in time e.g. 0 to -96.

- lat:

  Latitude in decimal format.

- lon:

  Longitude in decimal format.

- height:

  Height of trajectory (m).

- pressure:

  Pressure of trajectory (kPa).

## Note

The trajectories were run using the February 2011 HYSPLIT model. The
function is primarily written to investigate a single site at a time for
a single year. The trajectory files are quite large and care should be
exercised when importing several years and/or sites.

## See also

Other import functions:
[`importADMS()`](https://openair-project.github.io/openair/reference/importADMS.md),
[`importAURN()`](https://openair-project.github.io/openair/reference/importUKAQ-wrapper.md),
[`importEurope()`](https://openair-project.github.io/openair/reference/importEurope.md),
[`importImperial()`](https://openair-project.github.io/openair/reference/importImperial.md),
[`importMeta()`](https://openair-project.github.io/openair/reference/importMeta.md),
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)

Other trajectory analysis functions:
[`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md),
[`trajLevel()`](https://openair-project.github.io/openair/reference/trajLevel.md),
[`trajPlot()`](https://openair-project.github.io/openair/reference/trajPlot.md)

## Author

David Carslaw

## Examples

``` r
## import trajectory data for London in 2009
if (FALSE) { # \dontrun{
mytraj <- importTraj(site = "london", year = 2009)
} # }

## combine with measurements
if (FALSE) { # \dontrun{
theData <- importAURN(site = "kc1", year = 2009)
mytraj <- merge(mytraj, theData, by = "date")
} # }
```
