#' Import monitoring site meta data for UK and European networks
#'
#' Function to import meta data for air quality monitoring sites. By default,
#' the function will return the site latitude, longitude and site type, as well
#' as the code used in functions like [importUKAQ()], [importImperial()] and
#' [importEurope()]. Additional information may optionally be returned.
#'
#' @section Available Networks:
#'
#'   This function imports site meta data from several networks in the UK and
#'   Europe:
#'
#' - `"aurn"`,  The [UK Automatic Urban and Rural Network](https://uk-air.defra.gov.uk/).
#'
#' - `"aqe"`,  The [Air Quality England Network](https://www.airqualityengland.co.uk/).
#'
#' - `"saqn"`,  The [Scottish Air Quality Network](https://www.scottishairquality.scot/).
#'
#' - `"waqn"`,  The [Welsh Air Quality Network](https://airquality.gov.wales/).
#'
#' - `"ni"`,  The [Northern Ireland Air Quality Network](https://www.airqualityni.co.uk/).
#'
#' - `"local"`,  [Locally managed](https://uk-air.defra.gov.uk/networks/network-info?view=nondefraaqmon)
#'   air quality networks in England.
#'
#' - `"imperial"`,  Imperial College London (formerly King's College London) networks.
#'
#' - `"europe"`,  Hourly European data (Air Quality e-Reporting) based on a
#'   simplified version of the `{saqgetr}` package. Note that this data is only
#'   available until February 2024; see [importEurope()] for more information.
#'
#' @section Order of Operations:
#'
#'   This function contains various arguments which allow the user to filter the
#'   metadata before it is returned. These arguments are applied in the
#'   following order:
#'
#'   - `source`
#'
#'   - `year`
#'
#'   - `pollutant` (where possible)
#'
#'   - `code`
#'
#'   - `site`
#'
#'   - `site_type`
#'
#'   - `max_dist`
#'
#'   - `max_n`
#'
#'   Note that `max_n` is not *always* the number of rows returned by the
#'   function; it is the maximum number of possible sites to be returned. If
#'   `all = TRUE`, multiple rows will be present per site. Further, if previous
#'   filtering steps mean that fewer than `max_n` sites are remaining in the
#'   data, `max_n` will have no effect.
#'
#'   If the combination of arguments provided results in the removal of all
#'   sites, this function will return an empty dataframe with a warning.
#'
#' @section Data Dictionary:
#'
#'   By default, the function will return the site latitude, longitude and site
#'   type. If the option `all = TRUE` is used, much more detailed information is
#'   returned. The following metadata columns are available in the complete
#'   dataset:
#'
#' - **source**: The network with which the site is associated. Note that
#'   some monitoring sites are part of multiple networks (e.g., the AURN & SAQN)
#'   so the same site may feature twice under different sources.
#'
#' - **code**: The site code, used to import data from specific sites of
#'   interest.
#'
#' - **site**: The site name, which is more human-readable than the site code.
#'
#' - **site_type**: A description of the site environment.
#'
#' - **latitude** and **longitude**: The coordinates of the monitoring
#'   station, using the World Geodetic System (<https://epsg.io/4326>).
#'
#' - **start_date** and **end_date**: The opening and closing dates of the
#'   monitoring station. If `by_pollutant = TRUE`, these dates are instead the
#'   first and last dates at which specific pollutants were measured. A missing
#'   value, `NA`, indicates that monitoring is ongoing.
#'
#' - **ratified_to**: The date to which data has been ratified
#'   (i.e., 'quality checked'). Data after this date is subject to change.
#'
#' - **zone** and **agglomeration**: The UK is divided into agglomeration
#'   zones (large urban areas) and non-agglomeration zones for air quality
#'   assessment, which are given in these columns.
#'
#' - **local_authority**: The local authority in which the monitoring station
#'   is found.
#'
#' - **provider** and **code**: The specific provider of the locally
#'   managed dataset (e.g., `"londonair"`).
#'
#' @param source One or more air quality networks for which data is available
#'   through openair. Available networks include:
#'
#'   - `"aurn"`,  The UK Automatic Urban and Rural Network.
#'   - `"aqe"`,  The Air Quality England Network.
#'   - `"saqn"`,  The Scottish Air Quality Network.
#'   - `"waqn"`,  The Welsh Air Quality Network.
#'   - `"ni"`,  The Northern Ireland Air Quality Network.
#'   - `"local"`,  Locally managed air quality networks in England.
#'   - `"imperial"`, Imperial College London (formerly King's College London) networks.
#'   - `"europe"`, European AirBase/e-reporting data.
#'
#'   There are two additional options provided for convenience:
#'
#'   - `"ukaq"` will return metadata for all networks for which data is
#'   imported by [importUKAQ()] (i.e., AURN, AQE, SAQN, WAQN, NI, and the local
#'   networks).
#'   - `"all"` will import all available metadata (i.e., `"ukaq"` plus
#'   `"imperial"` and `"europe"`).
#'
#' @param year If a single year is selected, only sites that were open at some
#'   point in that year are returned. If `all = TRUE` only sites that measured a
#'   particular pollutant in that year are returned. Year can also be a sequence
#'   e.g. `year = 2010:2020` or of length 2 e.g. `year = c(2010, 2020)`, which
#'   will return only sites that were open over the duration.
#'
#' @param site,code,site_type Character vectors used to search the metadata for
#'   the specified `source`s. All of `code`, `site` and `site_type` are
#'   case-insensitive. `code` and `site_type` are matched exactly, but `site` is
#'   'pattern matched' - e.g., `site = "Sunderland"` and `source = "aurn"` will
#'   return data for `"Sunderland"`, `"Sunderland Silksworth"` and `"Sunderland
#'   Wessington Way"` (plus any future sites with the string `"Sunderland"` in
#'   their name).
#'
#' @param pollutant Character vectors used to search the metadata for the
#'   specified `source`s. `pollutant` is case-insensitive. For example,
#'   `pollutant = c("nox", "o3")` will return sites which measure *either* NOx
#'   *or* O3. Can also take the shorthand `"hc"`, will returns all hydrocarbons.
#'   Similar to `code`, values are matched exactly (e.g., `pollutant = "no"`
#'   will only return NO and not NO2 or NOx). Note that `pollutant` only applies
#'   to networks available through [importUKAQ()].
#'
#' @param lat,lng Decimal latitude (`lat`) and longitude (`lng`) (or other Y/X
#'   coordinate if using a different `crs`). If provided, the data will be
#'   returned with a `distance_km` column displaying the distance of each
#'   station from the target coordinate. The data will also be automatically
#'   sorted by this column.
#'
#' @param crs The coordinate reference system (CRS) of the data, passed to
#'   [sf::st_crs()]. By default this is [EPSG:4326](https://epsg.io/4326), the
#'   CRS associated with the commonly used latitude and longitude coordinates.
#'   Different coordinate systems can be specified using `crs` (e.g., `crs =
#'   27700` for the British National Grid). Note that non-lat/lng coordinate
#'   systems will be re-projected to EPSG:4326 for comparison with the site
#'   metadata.
#'
#' @param max_dist,max_n If `lat` and `lng` are provided, `max_dist` and `max_n`
#'   further filter the metadata. `max_dist` defines a maximum distance from the
#'   target coordinate in kilometers, and `max_n` a maximum number of sites to
#'   be returned. `max_n` is applied after `max_dist`.
#'
#' @param all When `all = FALSE` only the site code, site name, latitude and
#'   longitude and site type are imported. Setting `all = TRUE` will import all
#'   available meta data and provide details (when available) or the individual
#'   pollutants measured at each site.
#'
#' @param duplicate Some UK air quality sites are part of multiple networks, so
#'   could appear more than once when `source` is a vector of two or more. The
#'   default argument, `FALSE`, drops duplicate sites. `TRUE` will return them.
#'
#' @return A data frame
#'
#' @author David Carslaw
#' @author Jack Davison
#'
#' @family import functions
#'
#' @seealso the `networkMap()` function from the `openairmaps` package which can
#'   visualise site metadata on an interactive map.
#'
#' @references Thanks go to Trevor Davies (WSP), Dr Stuart Grange (EMPA) and Dr
#'   Ben Barratt (Imperial College) for making these data available.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get information for the AURN
#' meta <- importMeta(source = "aurn")
#'
#' # More detailed information
#' meta <- importMeta(source = "aurn", all = TRUE)
#'
#' # From the AURN and SAQN
#' meta <- importMeta(source = c("aurn", "saqn"))
#'
#' # Sites in the UK measuring SO2 or CO
#' meta <- importMeta(source = "ukaq", pollutant = c("so2", "co"))
#'
#' # English sites within 5km of Buckingham Palace
#' meta <- importMeta(
#'   source = c("aurn", "aqe", "local"),
#'   lat = 51.50101,
#'   lng = -0.141563,
#'   max_dist = 5
#' )
#' }
importMeta <-
  function(
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
  ) {
    if (!is.null(year) && is.na(year)) {
      year <- NULL
    }

    # ensure lower case
    source <- tolower(source)

    # alias source arguments
    source[source == "kcl"] <- "imperial"
    source[source == "saqd"] <- "saqn"
    source[source == "niaqn"] <- "ni"
    source[source == "lmam"] <- "local"

    # special source args
    if (any(source == "ukaq")) {
      source <- c("aurn", "aqe", "saqn", "waqn", "ni", "local")
    }
    if (any(source == "all")) {
      source <-
        c("aurn", "aqe", "saqn", "waqn", "ni", "local", "imperial", "europe")
    }

    # check sources are valid
    source <- rlang::arg_match(
      source,
      c(
        "aurn",
        "imperial",
        "saqn",
        "waqn",
        "aqe",
        "local",
        "ni",
        "europe"
      ),
      multiple = TRUE
    )

    # function to import any of the source networks
    get_meta <- function(source, all) {
      if (!source %in% c("imperial", "europe")) {
        url <- switch(
          source,
          aurn = "https://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData",
          saqn = "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData",
          saqd = "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData",
          ni = "https://www.airqualityni.co.uk/openair/R_data/NI_metadata.RData",
          waqn = "https://airquality.gov.wales/sites/default/files/openair/R_data/WAQ_metadata.RData",
          aqe = "https://airqualityengland.co.uk/assets/openair/R_data/AQE_metadata.RData",
          local = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/LMAM_metadata.RData",
          lmam = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/LMAM_metadata.RData"
        )

        meta <- read_wsp_meta(
          url,
          all = all,
          year = year,
          pollutant = pollutant
        )
      }

      # KCL
      if (source == "imperial") {
        meta <- read_imperial_meta(
          "https://londonair.org.uk/r_data/sites.RData",
          all = all,
          year = year
        )
      }

      # EUROPE
      if (source == "europe") {
        meta <- read_saqgetr_meta(
          "http://aq-data.ricardo-aea.com/R_data/saqgetr/helper_tables/sites.gz",
          year = year
        )
      }

      # return data source
      meta <- dplyr::mutate(
        meta,
        source = source,
        .before = dplyr::everything()
      )

      return(meta)
    }

    # import meta data
    meta <-
      purrr::map(.x = source, .f = ~ get_meta(source = .x, all = all)) |>
      purrr::list_rbind() |>
      dplyr::tibble()

    if (nrow(meta) == 0) {
      warn_empty_meta()
      return(meta)
    }

    # filter code
    if (!is.null(code) && "code" %in% names(meta)) {
      meta <- dplyr::filter(
        meta,
        tolower(.data$code) %in% tolower({{ code }})
      )

      if (nrow(meta) == 0) {
        warn_empty_meta()
        return(meta)
      }
    }

    # search site name
    if (!is.null(site) && "site" %in% names(meta)) {
      all_sites <- paste(site, collapse = "|")
      meta <- dplyr::filter(
        meta,
        grepl(!!all_sites, .data$site, ignore.case = TRUE)
      )

      if (nrow(meta) == 0) {
        warn_empty_meta()
        return(meta)
      }
    }

    # search site type
    if (!is.null(site_type) && "site_type" %in% names(meta)) {
      meta <- dplyr::filter(
        meta,
        tolower(.data$site_type) %in% tolower({{ site_type }})
      )

      if (nrow(meta) == 0) {
        warn_empty_meta()
        return(meta)
      }
    }

    # drop extra columns if not "all"
    if (!all) {
      meta <-
        dplyr::select(
          meta,
          dplyr::all_of(
            c(
              "source",
              "site",
              "code",
              "latitude",
              "longitude",
              "site_type"
            )
          )
        )
    }

    # deal with duplicates
    if (!duplicate) {
      if (all && "variable" %in% names(meta)) {
        meta <- dplyr::distinct(
          meta,
          .data$code,
          .data$latitude,
          .data$longitude,
          .data$variable,
          .keep_all = TRUE
        )
      } else {
        meta <- dplyr::distinct(
          meta,
          .data$code,
          .data$latitude,
          .data$longitude,
          .keep_all = TRUE
        )
      }
    }

    # spatial filtering
    if (!is.null(lat) && !is.null(lng)) {
      target <- sf::st_as_sf(
        dplyr::tibble(lat = lat, lng = lng),
        coords = c("lng", "lat"),
        crs = crs
      ) |>
        sf::st_transform(crs = 4326)

      meta_old <- meta
      meta <- tidyr::drop_na(meta, dplyr::all_of(c("latitude", "longitude")))

      if (nrow(meta) != nrow(meta_old)) {
        cli::cli_warn(
          "Dropping {nrow(meta_old) - nrow(meta)} station{?s} \\
                      with missing latitude & longitude."
        )
      }

      meta_sf <- sf::st_as_sf(
        meta,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      )

      meta_sf$distance_km <- as.numeric(sf::st_distance(meta_sf, target)) / 1000

      # add distance column, drop geometry
      meta <- dplyr::arrange(meta_sf, .data$distance_km) |>
        sf::st_drop_geometry()

      # filter max distance
      if (!is.null(max_dist)) {
        meta <- dplyr::filter(meta, .data$distance_km <= max_dist)
        if (nrow(meta) == 0) {
          warn_empty_meta()
          return(meta)
        }
      }

      # filter max number
      if (!is.null(max_n)) {
        # need semi_join approach as could have multiple rows per site when all
        # = TRUE
        max_n_sites <-
          meta |>
          dplyr::distinct(source, code, distance_km) |>
          dplyr::slice_min(
            order_by = .data$distance_km,
            n = max_n,
            with_ties = TRUE
          )

        meta <- dplyr::semi_join(meta, max_n_sites, by = c("source", "code"))
      }
    }

    dplyr::tibble(meta)
  }

# Read data from WSP's networks
read_wsp_meta <- function(url, all, year, pollutant) {
  tmp <- tempfile()

  # load data
  con <- url(url)
  meta <- get(load(con))
  close(con)

  # manipulate data
  meta <- dplyr::rename(
    .data = meta,
    code = "site_id",
    site = "site_name",
    site_type = "location_type",
    variable = "parameter"
  ) |>
    dplyr::mutate(
      start_date = lubridate::ymd(.data$start_date, tz = "GMT"),
      site_type = gsub(
        pattern = "Urban traffic",
        replacement = "Urban Traffic",
        .data$site_type
      ),
      site_type = gsub(
        pattern = "unknown unknown",
        replacement = "Unknown",
        .data$site_type
      )
    )

  # sort out ratified to (not needed in LMAM data)
  if ("ratified_to" %in% names(meta)) {
    meta$ratified_to <-
      lubridate::ymd(
        meta$ratified_to,
        tz = lubridate::tz(meta$start_date),
        quiet = TRUE
      )
  }

  # format end_date - set "ongoing" to current date
  id <- which(meta$end_date == "ongoing")
  meta$end_date[id] <- as.character(Sys.Date())
  meta$end_date <- lubridate::as_datetime(
    meta$end_date,
    tz = lubridate::tz(meta$start_date)
  )

  # select year or period when sites were open
  if (!is.null(year)) {
    meta$end_year <- lubridate::year(meta$end_date)
    meta$end_date[id] <- NA
    meta$start_year <- lubridate::year(meta$start_date)
    meta <-
      dplyr::filter(
        meta,
        .data$start_year <= min(year) &
          .data$end_year >= max(year)
      ) |>
      dplyr::select(
        -"end_year",
        -"start_year"
      )
  } else {
    meta$end_date[id] <- NA
  }

  # change some variable names
  meta$variable[meta$variable == "NOXasNO2"] <- "NOx"

  # filter for pollutant
  if (!is.null(pollutant)) {
    if ("hc" %in% pollutant) {
      pollutant <- c(
        pollutant,
        c(
          "ETHANE",
          "ETHENE",
          "ETHYNE",
          "PROPANE",
          "PROPENE",
          "iBUTANE",
          "nBUTANE",
          "1BUTENE",
          "t2BUTENE",
          "c2BUTENE",
          "iPENTANE",
          "nPENTANE",
          "13BDIENE",
          "t2PENTEN",
          "1PENTEN",
          "c2PENTEN",
          "2MEPENT",
          "3MEPENT",
          "ISOPRENE",
          "nHEXANE",
          "nHEPTANE",
          "iOCTANE",
          "nOCTANE",
          "BENZENE",
          "TOLUENE",
          "ETHBENZ",
          "mpXYLENE",
          "oXYLENE",
          "123TMB",
          "124TMB",
          "135TMB"
        )
      )
      pollutant <- pollutant[pollutant != "hc"]
    }

    meta <- dplyr::filter(
      meta,
      tolower(.data$variable) %in% tolower({{ pollutant }})
    )
  }

  # only extract one line per site to make it easier to use file
  # mostly interested in coordinates
  if (!all) {
    meta <- dplyr::distinct(meta, .data$site, .keep_all = TRUE)
  }

  return(meta)
}

# read metadata from Imperial College London
read_imperial_meta <- function(url, all, year) {
  con <- url(url)
  meta <- get(load(con))
  close(con)

  ## rename to match imported names e.g. importKCL
  meta <- dplyr::rename(
    meta,
    code = "SiteCode",
    site = "SiteName",
    site_type = "Classification",
    latitude = "Latitude",
    longitude = "Longitude"
  )

  # select year or period when sites were open
  if (!is.null(year)) {
    # format end_date - set "ongoing" to current date
    meta$end_year <-
      lubridate::year(as.Date(meta$ClosingDate))
    meta$end_year <-
      ifelse(
        is.na(meta$end_year),
        as.numeric(format(Sys.Date(), "%Y")),
        meta$end_year
      )
    meta$start_year <- lubridate::year(meta$OpeningDate)
    meta <-
      dplyr::filter(
        meta,
        .data$start_year <= min(year) &
          .data$end_year >= max(year)
      )
  }

  meta
}

# read data from the saqgetr database
read_saqgetr_meta <- function(url, year) {
  msg <-
    c(
      "!" = "{.fun importEurope} has been discontinued and cannot import \\
      data after February 2024.",
      "i" = "Consider using the EEA Air Quality Download Service \\
      instead {.url https://eeadmz1-downloads-webapp.azurewebsites.net/}"
    )

  cli::cli_inform(msg, .frequency = "regularly", .frequency_id = "europe")

  # Define data types
  col_types <- readr::cols(
    site = readr::col_character(),
    site_name = readr::col_character(),
    latitude = readr::col_double(),
    longitude = readr::col_double(),
    elevation = readr::col_double(),
    country = readr::col_character(),
    country_iso_code = readr::col_character(),
    site_type = readr::col_character(),
    site_area = readr::col_character(),
    date_start = readr::col_character(),
    date_end = readr::col_character(),
    network = readr::col_character(),
    eu_code = readr::col_character(),
    eoi_code = readr::col_character(),
    data_source = readr::col_character()
  )

  # Read data and parse dates
  meta <-
    readr::read_csv(url, col_types = col_types, progress = FALSE) |>
    dplyr::mutate(
      date_start = lubridate::ymd_hms(.data$date_start, tz = "UTC"),
      date_end = lubridate::ymd_hms(.data$date_end, tz = "UTC")
    )

  # select year or period when sites were open
  if (!is.null(year)) {
    # format end_date - set "ongoing" to current date
    meta$end_year <-
      lubridate::year(as.Date(meta$date_end))
    meta$end_year <-
      ifelse(
        is.na(meta$end_year),
        as.numeric(format(Sys.Date(), "%Y")),
        meta$end_year
      )
    meta$start_year <- lubridate::year(meta$date_start)
    meta <-
      dplyr::filter(
        meta,
        .data$start_year <= min(year) &
          .data$end_year >= max(year)
      )
  }

  # rename some variables
  meta <- dplyr::rename(meta, code = "site", site = "site_name")

  meta
}

# Shared warning message if all data has been filtered away
warn_empty_meta <- function() {
  cli::cli_warn(
    c(
      "!" = "No metadata has been returned.",
      "i" = "Check the {.arg year}, {.arg code}, {.arg site}, \\
      {.arg pollutant} and/or {.arg max_dist} arguments."
    )
  )
}
