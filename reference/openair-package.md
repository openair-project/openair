# openair: Tools for the Analysis of Air Pollution Data

Tools to analyse, interpret and understand air pollution data. Data are
typically regular time series and air quality measurement,
meteorological data and dispersion model output can be analysed. The
package is described in Carslaw and Ropkins (2012,
[doi:10.1016/j.envsoft.2011.09.008](https://doi.org/10.1016/j.envsoft.2011.09.008)
) and subsequent papers.

## Details

This is a UK Natural Environment Research Council (NERC) funded
knowledge exchange project that aims to make available innovative
analysis tools for air pollution data; with additional support from
Defra. The tools have generally been developed to analyse data of hourly
resolution (or at least a regular time series) both for air pollution
monitoring and dispersion modelling. The availability of meteorological
data at the same time resolution greatly enhances the capabilities of
these tools.

`openair` contains collection of functions to analyse air pollution
data. Typically it is expected that data are hourly means, although most
functions consider other time periods. The principal aim to make
available analysis techniques that most users of air quality data and
model output would not normally have access to. The functions consist of
those developed by the authors and a growing number from other
researchers.

The package also provides access to a wide range of data sources
including the UK Automatic Urban and Rural Network (AURN), networks run
by Imperial College London (e.g., the LAQN) and the Scottish Air Quality
Network (SAQN).

The package has a number of requirements for input data and these are
discussed in the manual (available in the `openair` book at
<https://openair-project.github.io/openair/>). The key requirements are
that a date or date-time field must have the name `date` (and can be
`Date` or `POSIXct` format), that wind speed is represented as `ws` and
that wind direction is `wd`.

Most functions work in a very straightforward way, but offer many
options for finer control and perhaps more in-depth analysis.

NOTE: openair assumes that data are not expressed in local time where
'Daylight Saving Time' is used. All functions check that this is the
case and issue a warning if TRUE. It is recommended that data are
expressed in UTC/GMT (or a fixed offset from) to avoid potential
problems with R and `openair` functions. The `openair` book provides
advice on these issues (available on the website).

To check to see if `openair` has been correctly installed, try some of
the examples below.

## The `openair` class

As well as generating the plots themselves, `openair` plotting functions
also return an object of class `"openair"`. The object includes three
main components:

- `call`, the command used to generate the plot.

- `data`, the data frame of summarised information used to make the
  plot.

- `plot`, the plot itself.

If retained, e.g., using `output <- polarPlot(mydata, "nox")`, this
output can be used to recover the data, reproduce or rework the original
plot or undertake further analysis.

An `openair` output can be manipulated using a number of generic
operations, including `print`, `plot` and `summary`. The examples below
show some examples of using an `openair` object.

## References

Most reference details are given under the specific functions. The
principal reference is below.

- Carslaw, D.C. and K. Ropkins, (2012) openair — an R package for air
  quality data analysis. Environmental Modelling & Software. Volume
  27-28, 52-61.

## See also

See <https://openair-project.github.io/openair/> for up to date
information on the project, and the openair book
(<https://openair-project.github.io/book/>) for thorough documentation
and examples.

## Author

**Maintainer**: David Carslaw <david.carslaw@york.ac.uk>
([ORCID](https://orcid.org/0000-0003-0991-950X))

Authors:

- Jack Davison <jack.davison@ricardo.com>
  ([ORCID](https://orcid.org/0000-0003-2653-6615))

- Karl Ropkins <K.Ropkins@its.leeds.ac.uk>
  ([ORCID](https://orcid.org/0000-0002-0294-6997))

## Examples

``` r
if (FALSE) { # \dontrun{
# load package
library(openair)

# summarise data in a compact way
summaryPlot(mydata)

# traditional wind rose
windRose(mydata)

# polar plot
polar_nox <- polarPlot(mydata, pollutant = "nox")

# see call
polar_nox$call

# get data
polar_nox$data

# could, e.g., re-plot in {ggplot2}
library(ggplot2)
ggplot(polar_nox$data, aes(u, v, fill = z)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = openair::openColours(), na.value = NA)
} # }
```
