#' Import data from Imperial College London networks
#'
#' Function for importing hourly mean data from Imperial College London
#' networks, formerly the King's College London networks. Files are imported
#' from a remote server operated by Imperial College London that provides air
#' quality data files as R data objects.
#'
#' The [importImperial()] function has been written to make it easy to import
#' data from the Imperial College London air pollution networks. Imperial have
#' provided .RData files (R workspaces) of all individual sites and years for
#' the Imperial networks. These files are updated on a weekly basis. This
#' approach requires a link to the Internet to work.
#'
#' There are several advantages over the web portal approach where .csv files
#' are downloaded. First, it is quick to select a range of sites, pollutants and
#' periods (see examples below). Second, storing the data as .RData objects is
#' very efficient as they are about four times smaller than .csv files --- which
#' means the data downloads quickly and saves bandwidth. Third, the function
#' completely avoids any need for data manipulation or setting time formats,
#' time zones etc. Finally, it is easy to import many years of data beyond the
#' current limit of about 64,000 lines. The final point makes it possible to
#' download several long time series in one go. The function also has the
#' advantage that the proper site name is imported and used in `openair``
#' functions.
#'
#' The site codes and pollutant names can be upper or lower case. The function
#' will issue a warning when data less than six months old is downloaded, which
#' may not be ratified.
#'
#' The data are imported by stacking sites on top of one another and will have
#' field names `date`, `site`, `code` (the site code) and
#' pollutant(s). Sometimes it is useful to have columns of site data. This can
#' be done using the [reshape()] function --- see examples below.
#'
#' The situation for particle measurements is not straightforward given the
#' variety of methods used to measure particle mass and changes in their use
#' over time. The [importImperial()] function imports two measures of PM10
#' where available. `PM10_raw` are TEOM measurements with a 1.3 factor
#' applied to take account of volatile losses. The `PM10` data is a current
#' best estimate of a gravimetric equivalent measure as described below. NOTE!
#' many sites have several instruments that measure PM10 or PM2.5. In the case
#' of FDMS measurements, these are given as separate site codes (see below). For
#' example "MY1" will be TEOM with VCM applied and "MY7" is the FDMS data.
#'
#' Where FDMS data are used the volatile and non-volatile components are
#' separately reported i.e. v10 = volatile PM10, v2.5 = volatile PM2.5, nv10 =
#' non-volatile PM10 and nv2.5 = non-volatile PM2.5. Therefore, PM10 = v10 +
#' nv10 and PM2.5 = v2.5 + nv2.5.
#'
#' For the assessment of the EU Limit Values, PM10 needs to be measured using
#' the reference method or one shown to be equivalent to the reference method.
#' Defra carried out extensive trials between 2004 and 2006 to establish which
#' types of particulate analysers in use in the UK were equivalent. These trials
#' found that measurements made using Partisol, FDMS, BAM and SM200 instruments
#' were shown to be equivalent to the PM10 reference method. However, correction
#' factors need to be applied to measurements from the SM200 and BAM
#' instruments. Importantly, the TEOM was demonstrated as not being equivalent
#' to the reference method due to the loss of volatile PM, even when the 1.3
#' correction factor was applied.  The Volatile Correction Model (VCM) was
#' developed for Defra at King's College to allow measurements of PM10 from TEOM
#' instruments to be converted to reference equivalent; it uses the measurements
#' of volatile PM made using nearby FDMS instruments to correct the measurements
#' made by the TEOM. It passed the equivalence testing using the same
#' methodology used in the Defra trials and is now the recommended method for
#' correcting TEOM measurements (Defra, 2009). VCM correction of TEOM
#' measurements can only be applied after 1st January 2004, when sufficiently
#' widespread measurements of volatile PM became available. The 1.3 correction
#' factor is now considered redundant for measurements of PM10 made after 1st
#' January 2004.  Further information on the VCM can be found at
#' <http://www.volatile-correction-model.info/>.
#'
#' All PM10 statistics on the LondonAir web site, including the bulletins and
#' statistical tools (and in the RData objects downloaded using
#' [importImperial()]), now report PM10 results as reference equivalent. For
#' PM10 measurements made by BAM and SM200 analysers the applicable correction
#' factors have been applied. For measurements from TEOM analysers the 1.3
#' factor has been applied up to 1st January 2004, then the VCM method has been
#' used to convert to reference equivalent.
#'
#' The meteorological data are meant to represent 'typical' conditions in
#' London, but users may prefer to use their own data. The data provide a an
#' estimate of general meteorological conditions across Greater London. For
#' meteorological species (wd, ws, rain, solar) each data point is formed by
#' averaging measurements from a subset of LAQN monitoring sites that have been
#' identified as having minimal disruption from local obstacles and a long term
#' reliable dataset. The exact sites used varies between species, but include
#' between two and five sites per species. Therefore, the data should represent
#' 'London scale' meteorology, rather than local conditions.
#'
#' [importKCL()] is equivalent to [importImperial()] and is provided for
#' back-compatibility reasons only. New users should use [importImperial()].
#'
#' @inheritParams importAURN
#' @param site Site code of the network site to import e.g. "my1" is Marylebone
#'   Road. Several sites can be imported with `site = c("my1", "kc1")` ---
#'   to import Marylebone Road and North Kensignton for example.
#' @param meteo,met Should meteorological data be added to the import data? The
#'   default is `FALSE`. If `TRUE` wind speed (m/s), wind direction
#'   (degrees), solar radiation and rain amount are available. See details
#'   below.
#' @param extra Defaults to `FALSE`. When `TRUE`, returns additional data.
#' @param units By default the returned data frame expresses the units in mass
#'   terms (ug/m3 for NOx, NO2, O3, SO2; mg/m3 for CO). Use `units =
#'   "volume"` to use ppb etc. PM10_raw TEOM data are multiplied by 1.3 and
#'   PM2.5 have no correction applied. See details below concerning PM10
#'   concentrations.
#' @param progress Show a progress bar when many sites/years are being imported?
#'   Defaults to `TRUE`.
#' @export
#' @return Returns a data frame of hourly mean values with date in POSIXct class
#'   and time zone GMT.
#' @author David Carslaw and Ben Barratt
#' @family import functions
#'
#' @rdname importImperial
#' @order 1
#'
#' @examples
#' ## import all pollutants from Marylebone Rd from 1990:2009
#' \dontrun{
#' mary <- importImperial(site = "my1", year = 2000:2009)
#' }
#'
#' ## import nox, no2, o3 from Marylebone Road and North Kensington for 2000
#' \dontrun{
#' thedata <-
#'   importImperial(
#'     site = c("my1", "kc1"),
#'     year = 2000,
#'     pollutant = c("nox", "no2", "o3")
#'   )
#' }
#'
#' ## import met data too...
#' \dontrun{
#' my1 <- importImperial(site = "my1", year = 2008, meteo = TRUE)
#' }
importImperial <-
  function(
    site = "my1",
    year = 2009,
    pollutant = "all",
    meta = FALSE,
    meteo = FALSE,
    extra = FALSE,
    units = "mass",
    to_narrow = FALSE,
    progress = TRUE
  ) {
    ## get rid of R check annoyances
    sites <- NULL
    v10 <- NULL
    v2.5 <- NULL

    site <- toupper(site)

    ## rows with these site codes
    ## this preserves order of site names
    con <-
      url(
        (paste(
          "http://londonair.org.uk/r_data/",
          "sites",
          ".RData",
          sep = ""
        ))
      )
    load(con)
    close(con)

    id <-
      sapply(site, function(x) {
        which(sites$SiteCode %in% toupper(x))
      })
    site.name <- sites$SiteName[id]

    ## RData files to import
    files <- lapply(site, function(x) {
      paste(x, "_", year, sep = "")
    })
    files <- do.call(c, files)

    loadData <- function(x) {
      tryCatch(
        {
          fileName <-
            paste("http://londonair.org.uk/r_data/", x, ".RData", sep = "")
          con <- url(fileName)
          load(con)

          ## need to check the date starts at start of year...
          start <- ISOdatetime(
            year = as.numeric(format(x$date[1], "%Y")),
            month = 1,
            day = 1,
            hour = 0,
            min = 0,
            sec = 0,
            tz = "GMT"
          )

          if (x$date[1] != start) {
            ## add first row
            x1 <- data.frame(date = start, site = x$site[1])
            x <- bind_rows(x1, x)
          }

          x <- datePad(x, type = "site") ## pad out missing dates
          x
        },
        error = function(ex) {
          warning(x, "does not exist - ignoring that one.")
          NULL
        },
        finally = {
          close(con)
        }
      )
    }

    if (progress) {
      progress <- "Importing Air Quality Data"
    }
    thedata <-
      purrr::map(files, loadData, .progress = progress) |>
      purrr::list_rbind()

    if (is.null(thedata)) {
      warning("No data to import - check site codes and year.", call. = FALSE)
      return()
    }

    if (nrow(thedata) < 1) {
      warning("No data to import - check site codes and year.", call. = FALSE)
      return()
    }

    thedata$code <- thedata$site

    thedata$site <-
      factor(thedata$site, labels = site.name, levels = site)

    ## change names
    names(thedata) <- tolower(names(thedata))

    ## if particular pollutants have been selected
    if (!missing(pollutant)) {
      if (pollutant != "all") {
        thedata <- thedata[, c("date", pollutant, "site", "code")]
      }
    }

    ## change units to mass units, use values in ugm3Conversion table
    if (units == "mass") {
      if ("nox" %in% names(thedata)) {
        thedata$nox <- thedata$nox * 1.91
      }
      if ("no2" %in% names(thedata)) {
        thedata$no2 <- thedata$no2 * 1.91
      }
      if ("o3" %in% names(thedata)) {
        thedata$o3 <- thedata$o3 * 2.00
      }
      if ("so2" %in% names(thedata)) {
        thedata$so2 <- thedata$so2 * 2.66
      }
      if ("co" %in% names(thedata)) {
        thedata$co <- thedata$co * 1.16
      }
      if ("pm10_raw" %in% names(thedata)) {
        thedata$pm10_raw <- thedata$pm10_raw * 1.30
      }

      msg <-
        c(
          "i" = "{.strong NOTE: Mass units are used}.",
          "*" = "ug/m3 for NOx, NO2, SO2, O3",
          "*" = "mg/m3 for CO",
          "*" = "PM10_raw is raw data multiplied by 1.3"
        )
    }

    ## rename PM volatile/non volatile components if present

    if ("pmfr" %in% names(thedata)) {
      thedata <- rename(thedata, v10 = pmfr)
      thedata <- transform(thedata, v10 = -1 * v10)
    }

    if ("p2fr" %in% names(thedata)) {
      thedata <- rename(thedata, v2.5 = p2fr)
      thedata <- transform(thedata, v2.5 = -1 * v2.5)
    }

    if ("pmfb" %in% names(thedata)) {
      thedata <- rename(thedata, nv10 = pmfb)
    }
    if ("p2fb" %in% names(thedata)) {
      thedata <- rename(thedata, nv2.5 = p2fb)
    }

    if (units != "mass") {
      if ("pm10" %in% names(thedata)) {
        thedata$pm10_raw <- thedata$pm10_raw * 1.30
      }

      msg <-
        c(
          "i" = "{.strong NOTE: Volume units are used}.",
          "*" = "ppbv for NOx, NO2, SO2, O3",
          "*" = "ppmv for CO",
          "*" = "PM10_raw is raw data multiplied by 1.3"
        )
    }

    ## don't add additional species
    if (!extra) {
      theNames <- c(
        "date",
        "co",
        "nox",
        "no2",
        "no",
        "o3",
        "so2",
        "pm10",
        "pm10_raw",
        "pm25",
        "v10",
        "v2.5",
        "nv10",
        "nv2.5",
        "code",
        "site"
      )
      thedata <- thedata[, which(names(thedata) %in% theNames)]
    }

    if (is.null(nrow(thedata))) {
      return()
    }

    ## warning about recent, possibly unratified data
    timeDiff <-
      difftime(Sys.time(), max(thedata$date), units = "days")
    if (timeDiff < 180) {
      warning("Some of the more recent data may not be ratified.")
    }

    if (meteo) {
      met <- NULL
      ## merge met data
      load(url(
        paste(
          "http://londonair.org.uk/r_data/",
          "metData",
          ".RData",
          sep = ""
        )
      ))
      # closeAllConnections()
      thedata <- merge(thedata, met, by = "date")
    }

    ## make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"
    thedata <- thedata[order(thedata$site, thedata$date), ]

    # add meta data
    if (meta) {
      meta_data <- importMeta(source = "imperial")
      # suppress warnings about factors
      thedata <-
        suppressWarnings(inner_join(thedata, meta_data, by = c("code", "site")))
    }

    if (to_narrow) {
      if (meta) {
        thedata <-
          pivot_longer(
            thedata,
            -c(date, site, code, latitude, longitude, site.type),
            names_to = "pollutant"
          ) |>
          arrange(site, code, pollutant, date)
      } else {
        thedata <-
          pivot_longer(
            thedata,
            -c(date, site, code),
            names_to = "pollutant"
          ) |>
          arrange(site, code, pollutant, date)
      }
    }

    cli::cli_inform(msg)

    return(as_tibble(thedata))
  }

#' @rdname importImperial
#' @order 2
#' @export
importKCL <-
  function(
    site = "my1",
    year = 2009,
    pollutant = "all",
    met = FALSE,
    units = "mass",
    extra = FALSE,
    meta = FALSE,
    to_narrow = FALSE,
    progress = TRUE
  ) {
    cli::cli_warn(
      c(
        "i" = "{.fun importKCL} has been superseded by, and is equivalent to, {.fun importImperial}. {.strong Please use {.fun importImperial} going forward.}"
      ),
      .frequency = "regularly",
      .frequency_id = "imperial"
    )

    importImperial(
      site = site,
      year = year,
      pollutant = pollutant,
      meteo = met,
      units = units,
      meta = meta,
      extra = extra,
      to_narrow = to_narrow,
      progress = progress
    )
  }
