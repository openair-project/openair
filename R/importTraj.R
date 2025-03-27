#' Import pre-calculated HYSPLIT 96-hour back trajectories
#'
#' Function to import pre-calculated back trajectories using the NOAA HYSPLIT
#' model. The trajectories have been calculated for a select range of locations
#' which will expand in time. They cover the last 20 years or so and can be used
#' together with other `openair` functions.
#'
#' This function imports pre-calculated back trajectories using the HYSPLIT
#' trajectory model (Hybrid Single Particle Lagrangian Integrated Trajectory
#' Model. Back trajectories provide some very useful information for air quality
#' data analysis. However, while they are commonly calculated by researchers it
#' is generally difficult for them to be calculated on a routine basis and used
#' easily. In addition, the availability of back trajectories over several years
#' can be very useful, but again difficult to calculate.
#'
#' Trajectories are run at 3-hour intervals and stored in yearly files (see
#' below). The trajectories are started at ground-level (10m) and propagated
#' backwards in time.
#'
#' These trajectories have been calculated using the Global NOAA-NCEP/NCAR
#' reanalysis data archives. The global data are on a latitude-longitude grid
#' (2.5 degree). Note that there are many different meteorological data sets
#' that can be used to run HYSPLIT e.g. including ECMWF data. However, in order
#' to make it practicable to run and store trajectories for many years and
#' sites, the NOAA-NCEP/NCAR reanalysis data is most useful. In addition, these
#' archives are available for use widely, which is not the case for many other
#' data sets e.g. ECMWF. HYSPLIT calculated trajectories based on archive data
#' may be distributed without permission. For those wanting, for example, to
#' consider higher resolution meteorological data sets it may be better to run
#' the trajectories separately.
#'
#' We are extremely grateful to NOAA for making HYSPLIT available to produce
#' back trajectories in an open way. We ask that you cite HYSPLIT if used in
#' published work.
#'
#' Users can supply their own trajectory files to plot in openair. These files
#' must have the following fields: date, lat, lon and hour.inc (see details
#' below).
#'
#' The files consist of the following information:
#'
#' \describe{ \item{date}{This is the arrival point time and is repeated the
#' number of times equal to the length of the back trajectory --- typically 96
#' hours (except early on in the file). The format is `POSIXct`. It is this
#' field that should be used to link with air quality data. See example below.}
#' \item{receptor}{Receptor number, currently only 1.} \item{year}{The year}
#' \item{month}{Month 1-12} \item{day}{Day of the month 1-31} \item{hour}{Hour
#' of the day 0-23 GMT} \item{hour.inc}{Number of hours back in time e.g. 0 to
#' -96.} \item{lat}{Latitude in decimal format.}  \item{lon}{Longitude in
#' decimal format.}  \item{height}{Height of trajectory (m).}
#' \item{pressure}{Pressure of trajectory (kPa).}  }
#' @param site Site code of the network site to import e.g. "london". Only one
#'   site can be imported at a time. The following sites are typically available
#'   from 2000-2012, although some UK ozone sites go back to 1988 (code,
#'   location, lat, lon, year):
#' \tabular{llrrl}{
#' abudhabi   \tab Abu Dhabi                    \tab  24.43000 \tab  54.408000 \tab 2012-2013\cr
#' ah         \tab Aston Hill                   \tab  52.50385 \tab  -3.041780 \tab 1988-2013\cr
#' auch       \tab Auchencorth Moss             \tab  55.79283 \tab  -3.242568 \tab 2006-2013\cr
#' berlin     \tab Berlin, Germany              \tab  52.52000 \tab  13.400000 \tab 2000-2013\cr
#' birm       \tab Birmigham Centre             \tab  52.47972 \tab  -1.908078 \tab 1990-2013\cr
#' boston     \tab Boston, USA                  \tab  42.32900 \tab -71.083000 \tab 2008-2013\cr
#' bot        \tab Bottesford                   \tab  52.93028 \tab  -0.814722 \tab 1990-2013\cr
#' bukit      \tab Bukit Kototabang, Indonesia  \tab  -0.19805 \tab 100.318000 \tab 1996-2013\cr
#' chittagong \tab Chittagong, Bangladesh       \tab  22.37000 \tab  91.800000 \tab 2010-2013\cr
#' dhaka      \tab Dhaka, Bangladesh            \tab  23.70000 \tab  90.375000 \tab 2010-2013\cr
#' ed         \tab Edinburgh                    \tab  55.95197 \tab  -3.195775 \tab 1990-2013\cr
#' elche      \tab Elche, Spain                 \tab  38.27000 \tab  -0.690000 \tab 2004-2013\cr
#' esk        \tab Eskdalemuir                  \tab  55.31530 \tab  -3.206110 \tab 1998-2013\cr
#' gibraltar  \tab Gibraltar                    \tab  36.13400 \tab  -5.347000 \tab 2005-2010\cr
#' glaz       \tab Glazebury                    \tab  53.46008 \tab  -2.472056 \tab 1998-2013\cr
#' groningen  \tab Groningen                    \tab  53.40000 \tab   6.350000 \tab 2007-2013\cr
#' har        \tab Harwell                      \tab  51.57108 \tab  -1.325283 \tab 1988-2013\cr
#' hk         \tab Hong Kong                    \tab  22.29000 \tab 114.170000 \tab 1998-2013\cr
#' hm         \tab High Muffles                 \tab  54.33500 \tab  -0.808600 \tab 1988-2013\cr
#' kuwait     \tab Kuwait City                  \tab  29.36700 \tab  47.967000 \tab 2008-2013\cr
#' lb         \tab Ladybower                    \tab  53.40337 \tab  -1.752006 \tab 1988-2013\cr
#' london     \tab Central London               \tab  51.50000 \tab  -0.100000 \tab 1990-2013\cr
#' lh         \tab Lullington Heath             \tab  50.79370 \tab   0.181250 \tab 1988-2013\cr
#' ln         \tab Lough Navar                  \tab  54.43951 \tab  -7.900328 \tab 1988-2013\cr
#' mh         \tab Mace Head                    \tab  53.33000 \tab  -9.900000 \tab 1988-2013\cr
#' ny-alesund \tab Ny-Alesund, Norway           \tab  78.91763 \tab  11.894653 \tab 2009-2013\cr
#' oslo       \tab Oslo                         \tab  59.90000 \tab  10.750000 \tab 2010-2013\cr
#' paris      \tab Paris, France                \tab  48.86200 \tab   2.339000 \tab 2000-2013\cr
#' roch       \tab Rochester Stoke              \tab  51.45617 \tab   0.634889 \tab 1988-2013\cr
#' rotterdam  \tab Rotterdam                    \tab  51.91660 \tab   4.475000 \tab 2010-2013\cr
#' saopaulo   \tab Sao Paulo                    \tab -23.55000 \tab -46.640000 \tab 2000-2013\cr
#' sib        \tab Sibton                       \tab  52.29440 \tab   1.463970 \tab 1988-2013\cr
#' sv         \tab Strath Vaich                 \tab  57.73446 \tab  -4.776583 \tab 1988-2013\cr
#' wuhan      \tab Wuhan, China                 \tab  30.58300 \tab 114.280000 \tab 2008-2013\cr
#' yw         \tab Yarner Wood                  \tab  50.59760 \tab  -3.716510 \tab 1988-2013
#' }
#' @param progress Show a progress bar when many receptors/years are being
#'   imported? Defaults to `TRUE`.
#' @param year Year or years to import. To import a sequence of years from
#'   1990 to 2000 use `year = 1990:2000`. To import several specific years
#'   use `year = c(1990, 1995, 2000)` for example.
#'
#' @param local File path to .RData trajectory files run by user and
#' not stored on the Ricardo web server. These files would have been
#' generated from the Hysplit trajectory code shown in the appendix
#' of the openair manual. An example would be `local =
#' 'c:/users/david/TrajFiles/'`.
#' @export
#' @return Returns a data frame with pre-calculated back trajectories.
#' @author David Carslaw
#' @note The trajectories were run using the February 2011 HYSPLIT model.
#'  The function is primarily written to investigate a single
#' site at a time for a single year. The trajectory files are quite
#' large and care should be exercised when importing several years and/or sites.
#' @family import functions
#' @family trajectory analysis functions
#' @examples
#'
#' ## import trajectory data for London in 2009
#' \dontrun{mytraj <- importTraj(site = "london", year = 2009)}
#'
#' ## combine with measurements
#' \dontrun{theData <- importAURN(site = "kc1", year = 2009)
#' mytraj <- merge(mytraj, theData, by = "date")}
importTraj <-
  function(site = "london",
           year = 2009,
           local = NA,
           progress = TRUE) {

  ## get rid of R check annoyances
  traj <- NULL

  if (length(site) > 1) stop("Only one site can be imported at a time.")
  site <- tolower(site)

  files <- lapply(site, function(x) paste(x, year, sep = ""))
  files <- do.call(c, files)

  loadData <- function(x) {
    tryCatch(
      {
        if (is.na(local)) {
          fileName <- paste(
            "http://met-data.ricardo-aea.com/trajectories/", x, ".RData",
            sep = ""
          )

          con <- url(fileName)
          load(con)
        } else { ## load from local file system

          con <- paste(local, x, ".RData", sep = "")
          load(con)
        }


        traj
      },
      error = function(ex) {
        warning(x, "does not exist - ignoring that one.\n")
        NULL
      },
      finally = {
        if (is.na(local)) close(con)
      }
    )
  }

  if (progress) progress <- "Importing Trajectories"
  thedata <- purrr::map(files, loadData, .progress = progress) %>%
    purrr::list_rbind()

  ## change names
  names(thedata) <- tolower(names(thedata))

  dplyr::tibble(thedata)
}
