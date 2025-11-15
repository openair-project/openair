![](reference/figures/logo.png)

## **openair**

### open source tools for air quality data analysis

**openair** is an R package developed for the purpose of analysing air
quality data — or more generally atmospheric composition data. The
package is extensively used in academia, the public and private sectors.
The project was initially funded by the UK Natural Environment Research
Council ([NERC](https://www.ukri.org/councils/nerc/)), with additional
funds from the UK Department for Environment Food & Rural Affairs
([Defra](https://www.gov.uk/government/organisations/department-for-environment-food-rural-affairs)).

*Part of the openair toolkit*

[![openair](https://img.shields.io/badge/openair_core-06D6A0?style=flat-square)](https://openair-project.github.io/openair/)
\|
[![worldmet](https://img.shields.io/badge/worldmet-26547C?style=flat-square)](https://openair-project.github.io/worldmet/)
\|
[![openairmaps](https://img.shields.io/badge/openairmaps-FFD166?style=flat-square)](https://openair-project.github.io/openairmaps/)
\|
[![deweather](https://img.shields.io/badge/deweather-EF476F?style=flat-square)](https://openair-project.github.io/deweather/)

------------------------------------------------------------------------

## 💡 Core Features

**openair** has developed over many years to form an extensive toolkit
of functions for analysing air quality and atmospheric composition data.

- **Access to data** from several hundred UK air pollution monitoring
  sites through the
  [`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)
  family of functions.

- **Time Series & Trend analysis** to explore how air quality
  concentrations vary over time (e.g., through
  [`timePlot()`](https://openair-project.github.io/openair/reference/timePlot.md),
  [`timeVariation()`](https://openair-project.github.io/openair/reference/timeVariation.md),
  and
  [`calendarPlot()`](https://openair-project.github.io/openair/reference/calendarPlot.md)).

- **Directional analysis** to help characterise different sources of
  pollution, including the creation of **bivariate polar plots** using
  [`polarPlot()`](https://openair-project.github.io/openair/reference/polarPlot.md).

- **Trajectory analysis** to examine NOAA Hysplit trajectories, with
  plotting
  ([`trajPlot()`](https://openair-project.github.io/openair/reference/trajPlot.md)),
  heatmap
  ([`trajLevel()`](https://openair-project.github.io/openair/reference/trajLevel.md))
  and clustering
  ([`trajCluster()`](https://openair-project.github.io/openair/reference/trajCluster.md))
  functionality.

- **Utility functions**, such as
  [`timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.md)
  and
  [`selectByDate()`](https://openair-project.github.io/openair/reference/selectByDate.md)
  to make it easier to manipulate atmospheric composition data.

- **Flexible plot conditioning** to easily plot data by hour or the day,
  day of the week, season of the year, etc., through the `type` option
  available in most functions.

![](reference/figures/feature-banner.png)

------------------------------------------------------------------------

## 📖 Documentation

All **openair** functions are fully documented; access documentation
using R in your IDE of choice.

``` r
?openair::polarPlot
```

Documentation is also hosted online on the **package website**.

[![website](https://img.shields.io/badge/website-documentation-blue)](https://openair-project.github.io/openair/)

A guide to the openair toolkit can be found in the **online book**,
which contains lots of code snippets, demonstrations of functionality,
and ideas for the application of **openair**’s various functions.

[![book](https://img.shields.io/badge/book-code_demos_and_ideas-blue)](https://openair-project.github.io/book/)

------------------------------------------------------------------------

## 🗃️ Installation

**openair** can be installed from **CRAN** with:

``` r
install.packages("openair")
```

You can also install the development version of **openair** from GitHub
using [pak](https://pak.r-lib.org/):

``` r
# install.packages("pak")
pak::pak("openair-project/openair")
```

------------------------------------------------------------------------

🏛️ **openair** is primarily maintained by [David
Carslaw](https://github.com/davidcarslaw).

📃 **openair** is licensed under the [MIT
License](https://openair-project.github.io/openair/LICENSE.html).

🧑‍💻 Contributions are welcome from the wider community. See the
[contributing
guide](https://openair-project.github.io/openair/CONTRIBUTING.html) and
[code of
conduct](https://openair-project.github.io/openair/CODE_OF_CONDUCT.html)
for more information.
