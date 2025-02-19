#' Example air quality monitoring data for openair
#'
#' The `mydata` dataset is provided as an example dataset as part of the openair
#' package. The dataset contains hourly measurements of air pollutant
#' concentrations, wind speed and wind direction collected at the Marylebone
#' (London) air quality monitoring supersite between 1st January 1998 and 23rd
#' June 2005.
#'
#' \describe{
#' \item{date}{Observation date/time stamp in year-month-day hour:minute:second
#' format (POSIXct).}
#' \item{ws}{Wind speed, in m/s, as numeric vector.}
#' \item{wd}{Wind direction, in degrees from North, as a numeric vector.}
#' \item{nox}{Oxides of nitrogen concentration, in ppb, as a numeric vector.}
#' \item{no2}{Nitrogen dioxide concentration, in ppb, as a numeric vector.}
#' \item{o3}{Ozone concentration, in ppb, as a numeric vector.}
#' \item{pm10}{Particulate PM10 fraction measurement, in ug/m3 (raw TEOM), as a
#' numeric vector.}
#' \item{so2}{Sulfur dioxide concentration, in ppb, as a numeric vector.}
#' \item{co}{Carbon monoxide concentration, in ppm, as a numeric vector.}
#' \item{pm25}{Particulate PM2.5 fraction measurement, in ug/m3, as a numeric
#' vector.}
#' }
#' @note [openair][openair-package] functions generally require data frames with
#'   a field "date" that can be in either `POSIXct` or `Date` format
#' @source `mydata` was compiled from data archived in the London Air Quality
#'   Archive.  See <https://www.londonair.org.uk> for site details.
#' @examples
#' # basic structure
#' head(mydata)
"mydata"
