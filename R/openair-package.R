#' @details This is a UK Natural Environment Research Council (NERC) funded
#'   knowledge exchange project that aims to make available innovative analysis
#'   tools for air pollution data; with additional support from Defra. The tools
#'   have generally been developed to analyse data of hourly resolution (or at
#'   least a regular time series) both for air pollution monitoring and
#'   dispersion modelling. The availability of meteorological data at the same
#'   time resolution greatly enhances the capabilities of these tools.
#'
#'   `openair` contains collection of functions to analyse air pollution data.
#'   Typically it is expected that data are hourly means, although most
#'   functions consider other time periods. The principal aim to make available
#'   analysis techniques that most users of air quality data and model output
#'   would not normally have access to. The functions consist of those developed
#'   by the authors and a growing number from other researchers.
#'
#'   The package also provides access to a wide range of data sources including
#'   the UK Automatic Urban and Rural Network (AURN), networks run by Imperial
#'   College London (e.g., the LAQN) and the Scottish Air Quality Network
#'   (SAQN).
#'
#'   The package has a number of requirements for input data and these are
#'   discussed in the manual (available in the `openair` book at
#'   <https://openair-project.github.io/openair/>). The key requirements are
#'   that a date or date-time field must have the name `date` (and can be `Date`
#'   or `POSIXct` format), that wind speed is represented as `ws` and that wind
#'   direction is `wd`.
#'
#'   Most functions work in a very straightforward way, but offer many options
#'   for finer control and perhaps more in-depth analysis.
#'
#'   NOTE: openair assumes that data are not expressed in local time where
#'   'Daylight Saving Time' is used. All functions check that this is the case
#'   and issue a warning if TRUE. It is recommended that data are expressed in
#'   UTC/GMT (or a fixed offset from) to avoid potential problems with R and
#'   `openair` functions. The `openair` book provides advice on these issues
#'   (available on the website).
#'
#'   To check to see if `openair` has been correctly installed, try some of the
#'   examples below.
#'
#' @section The `openair` class:
#'
#'   As well as generating the plots themselves, `openair` plotting functions
#'   also return an object of class `"openair"`. The object includes three main
#'   components:
#'
#'   -  `call`, the command used to generate the plot.
#'
#'   -  `data`, the data frame of summarised information used to make the
#'   plot.
#'
#'   -  `plot`, the plot itself.
#'
#'   If retained, e.g., using `output <- polarPlot(mydata, "nox")`, this output
#'   can be used to recover the data, reproduce or rework the original plot or
#'   undertake further analysis.
#'
#'   An `openair` output can be manipulated using a number of generic
#'   operations, including `print`, `plot` and `summary`. The examples below
#'   show some examples of using an `openair` object.
#'
#' @references Most reference details are given under the specific functions.
#'   The principal reference is below.
#'
#'    -  Carslaw, D.C. and K. Ropkins, (2012) openair --- an R package for 
#'    air quality data analysis.  Environmental Modelling & Software. 
#'    Volume 27-28, 52-61.
#'
#' @seealso See <https://openair-project.github.io/openair/> for up to date
#'   information on the project, and the openair book
#'   (<https://openair-project.github.io/book/>) for thorough documentation and
#'   examples.
#'
#' @examples
#' \dontrun{
#' # load package
#' library(openair)
#'
#' # summarise data in a compact way
#' summaryPlot(mydata)
#'
#' # traditional wind rose
#' windRose(mydata)
#'
#' # polar plot
#' polar_nox <- polarPlot(mydata, pollutant = "nox")
#'
#' # see call
#' polar_nox$call
#'
#' # get data
#' polar_nox$data
#'
#' # could, e.g., re-plot in {ggplot2}
#' library(ggplot2)
#' ggplot(polar_nox$data, aes(u, v, fill = z)) + geom_tile() + coord_equal() +
#' scale_fill_gradientn(colours = openair::openColours(), na.value = NA)
#' }
#'
#' @keywords internal
"_PACKAGE"
## usethis namespace: start
#' @importFrom latticeExtra useOuterStrips
#' @importFrom lubridate as_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate day
#' @importFrom lubridate dmy
#' @importFrom lubridate dst
#' @importFrom lubridate floor_date
#' @importFrom lubridate force_tz
#' @importFrom lubridate hour
#' @importFrom lubridate month
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate round_date
#' @importFrom lubridate wday
#' @importFrom lubridate year
#' @importFrom methods is
#' @importFrom Rcpp evalCpp
#' @importFrom rlang %||%
#' @importFrom rlang .data
#' @importFrom stats aggregate
#' @importFrom stats approx
#' @importFrom stats arima
#' @importFrom stats as.dendrogram
#' @importFrom stats as.dist
#' @importFrom stats as.ts
#' @importFrom stats ave
#' @importFrom stats coef
#' @importFrom stats complete.cases
#' @importFrom stats cor
#' @importFrom stats dist
#' @importFrom stats fitted
#' @importFrom stats formula
#' @importFrom stats frequency
#' @importFrom stats hclust
#' @importFrom stats KalmanRun
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats optimize
#' @importFrom stats order.dendrogram
#' @importFrom stats predict
#' @importFrom stats qchisq
#' @importFrom stats qnorm
#' @importFrom stats qt
#' @importFrom stats quantile
#' @importFrom stats reshape
#' @importFrom stats residuals
#' @importFrom stats sd
#' @importFrom stats smooth.spline
#' @importFrom stats spline
#' @importFrom stats stl
#' @importFrom stats StructTS
#' @importFrom stats ts
#' @importFrom stats tsp
#' @importFrom stats tsSmooth
#' @importFrom stats update
#' @importFrom stats var
#' @importFrom tibble tibble
#' @importFrom utils compareVersion
#' @importFrom utils head
#' @importFrom utils modifyList
#' @importFrom utils read.csv
#' @importFrom utils tail
## usethis namespace: end
NULL
