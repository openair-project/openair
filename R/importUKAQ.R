#' Import data from the UK Air Pollution Networks
#'
#' Functions for importing air pollution data from a range of UK networks
#' including the Automatic Urban and Rural Network (AURN), the individual
#' England (AQE), Scotland (SAQN), Wales (WAQN) and Northern Ireland (NI)
#' Networks, and many "locally managed" monitoring networks across England.
#' Files are imported from a remote server operated by Ricardo that provides air
#' quality data files as R data objects. For an up to date list of available
#' sites that can be imported, see [importMeta()].
#'
#' @section Importing UK Air Pollution Data:
#'
#'   This family of functions has been written to make it easy to import data
#'   from across several UK air quality networks. Ricardo have provided .RData
#'   files (R workspaces) of all individual sites and years, as well as up to
#'   date meta data. These files are updated on a daily basis. This approach
#'   requires a link to the Internet to work.
#'
#'   There are several advantages over the web portal approach where .csv files
#'   are downloaded.
#'
#'   -  First, it is quick to select a range of sites, pollutants
#'   and periods (see examples below).
#'
#'   -  Second, storing the data as .RData
#'   objects is very efficient as they are about four times smaller than .csv
#'   files --- which means the data downloads quickly and saves bandwidth.
#'
#'   -  Third, the function completely avoids any need for data manipulation or
#'   setting time formats, time zones etc. The function also has the advantage
#'   that the proper site name is imported and used in
#'   [openair][openair-package] functions.
#'
#'   Users should take care if using data from both [openair][openair-package]
#'   *and* web portals (for example, [UK
#'   AIR](https://uk-air.defra.gov.uk/data/)). One key difference is that the
#'   data provided by openair is date *beginning*, whereas the web portal
#'   provides date *ending*. Hourly concentrations may therefore appear offset
#'   by an hour, for example.
#'
#'   The data are imported by stacking sites on top of one another and will have
#'   field names `site`, `code` (the site code) and `pollutant`.
#'
#'   By default, the function returns hourly average data. However, annual,
#'   monthly, daily and 15 minute data (for SO2) can be returned using the
#'   option `data_type`. Annual and monthly data provide whole network
#'   information including data capture statistics.
#'
#'   All units are expressed in mass terms for gaseous species (ug/m3 for NO,
#'   NO2, NOx (as NO2), SO2 and hydrocarbons; and mg/m3 for CO). PM10
#'   concentrations are provided in gravimetric units of ug/m3 or scaled to be
#'   comparable with these units. Over the years a variety of instruments have
#'   been used to measure particulate matter and the technical issues of
#'   measuring PM10 are complex. In recent years the measurements rely on FDMS
#'   (Filter Dynamics Measurement System), which is able to measure the volatile
#'   component of PM. In cases where the FDMS system is in use there will be a
#'   separate volatile component recorded as 'v10' and non-volatile component
#'   'nv10', which is already included in the absolute PM10 measurement. Prior
#'   to the use of FDMS the measurements used TEOM (Tapered Element Oscillating.
#'   Microbalance) and these concentrations have been multiplied by 1.3 to
#'   provide an estimate of the total mass including the volatile fraction.
#'
#'   Some sites report hourly and daily PM10 and / or PM2.5. When `data_type =
#'   "daily"` and there are both hourly and 'proper' daily measurements
#'   available, these will be returned as e.g. "pm2.5" and "gr_pm2.5"; the
#'   former corresponding to data based on original hourly measurements and the
#'   latter corresponding to daily gravimetric measurements.
#'
#'   The function returns modelled hourly values of wind speed (`ws`), wind
#'   direction (`wd`) and ambient temperature (`air_temp`) if available
#'   (generally from around 2010). These values are modelled using the WRF model
#'   operated by Ricardo.
#'
#'   The BAM (Beta-Attenuation Monitor) instruments that have been incorporated
#'   into the network throughout its history have been scaled by 1.3 if they
#'   have a heated inlet (to account for loss of volatile particles) and 0.83 if
#'   they do not have a heated inlet. The few TEOM instruments in the network
#'   after 2008 have been scaled using VCM (Volatile Correction Model) values to
#'   account for the loss of volatile particles. The object of all these scaling
#'   processes is to provide a reasonable degree of comparison between data sets
#'   and with the reference method and to produce a consistent data record over
#'   the operational period of the network, however there may be some
#'   discontinuity in the time series associated with instrument changes.
#'
#'   No corrections have been made to the PM2.5 data. The volatile component of
#'   FDMS PM2.5 (where available) is shown in the 'v2.5' column.
#'
#' @param site Site code of the site to import, e.g., `"my1"` is Marylebone
#'   Road. Site codes can be discovered through the use of [importMeta()].
#'   Several sites can be imported at once. For example, `site = c("my1",
#'   "nott")` imports both Marylebone Road and Nottingham. Sites from different
#'   networks can be imported through also providing multiple `source`s. Site
#'   codes can be upper or lower case.
#' @param year Year(s) to import. To import a series of years use, e.g.,
#'   `2000:2020`. To import several specific years use `year = c(2000, 2010,
#'   2020)`.
#' @param source The network to which the `site`(s) belong. The default, `NULL`,
#'   allows [importUKAQ()] to guess the correct `source`, preferring national
#'   networks over locally managed networks. Alternatively, users can define a
#'   `source`. Providing a single network will attempt to import all of the
#'   given `site`s from the provided network. Alternatively, a vector of sources
#'   can be provided of the same length as `site` to indicate which network each
#'   `site` individually belongs. Available networks include:
#'   - `"aurn"`,  The UK Automatic Urban and Rural Network.
#'   - `"aqe"`,  The Air Quality England Network.
#'   - `"saqn"`,  The Scottish Air Quality Network.
#'   - `"waqn"`,  The Welsh Air Quality Network.
#'   - `"ni"`,  The Northern Ireland Air Quality Network.
#'   - `"local"`,  Locally managed air quality networks in England.
#' @param data_type The type of data to be returned, defaulting to `"hourly"`
#'   data. Alternative data types include:
#'   - `"daily"`: Daily average data.
#'   - `"monthly"`: Monthly average data with data capture information for the whole network.
#'   - `"annual"`: Annual average data with data capture information for the whole network.
#'   - `"15_min"`: 15-minute average SO2 concentrations.
#'   - `"8_hour"`: 8-hour rolling mean concentrations for O3 and CO.
#'   - `"24_hour"`: 24-hour rolling mean concentrations for particulates.
#'   - `"daily_max_8"`: Maximum daily rolling 8-hour maximum for O3 and CO.
#'   - `"daqi"`: Daily Air Quality Index (DAQI). See
#'   [here](https://uk-air.defra.gov.uk/air-pollution/daqi?view=more-info) for
#'   more details of how the index is defined. Note that this `data_type` is not
#'   available for locally managed monitoring networks.
#' @param pollutant Pollutants to import. If omitted will import all pollutants
#'   from a site. To import only NOx and NO2 for example use \code{pollutant =
#'   c("nox", "no2")}. Pollutant names can be upper or lower case.
#' @param hc Include hydrocarbon measurements in the imported data? Defaults to
#'   `FALSE` as most users will not be interested in using hydrocarbon data.
#' @param meta Append metadata columns to data for each selected `site`?
#'   Defaults to `FALSE`. Columns are defined using `meta_columns`.
#' @param meta_columns The specific columns to append when `meta = TRUE`.
#'   Defaults to site type, latitude and longitude. Should be any of the
#'   metadata columns available via [importMeta()].
#' @param meteo Append modelled meteorological data, if available? Defaults to
#'   `TRUE`, which will return wind speed (`ws`), wind direction (`wd`) and
#'   ambient temperature (`air_temp`). The variables are calculated from using
#'   the WRF model run by Ricardo Energy & Environment and are available for
#'   most but not all networks. Setting `meteo = FALSE` is useful if you have
#'   other meteorological data to use in preference, for example from the
#'   `worldmet` package.
#' @param ratified Append `qc` column(s) to hourly data indicating whether each
#'   species was ratified (i.e., quality-checked)?  Defaults to `FALSE`.
#' @param to_narrow Return the data in a "narrow"/"long"/"tidy" format? By
#'   default the returned data is "wide" and has a column for each
#'   pollutant/variable. When `to_narrow = TRUE` the data are returned with a
#'   column identifying the pollutant name and a column containing the
#'   corresponding concentration/statistic. Defaults to `FALSE`.
#' @param verbose Print messages to the console if hourly data cannot be
#'   imported? Default is `FALSE`. `TRUE` is useful for debugging as the
#'   specific `year`(s), `site`(s) and `source`(s) which cannot be imported will
#'   be returned.
#' @param progress Show a progress bar when many sites/years are being imported?
#'   Defaults to `TRUE`.
#'
#' @export
#' @return a [tibble][tibble::tibble-package]
#' @author David Carslaw, Trevor Davies, and Jack Davison
#' @family import functions
#' @examples
#' \dontrun{
#' # import a single site from the AURN
#' importUKAQ("my1", year = 2022)
#'
#' # import sites from another network
#' importUKAQ(c("bn1", "bn2"), year = 2022, source = "aqe")
#'
#' # import sites across multiple networks
#' importUKAQ(c("my1", "bn1", "bn2"),
#'   year = 2022,
#'   source = c("aurn", "aqe", "aqe")
#' )
#'
#' # get "long" format hourly data with a ratification flag
#' importUKAQ(
#'   "card",
#'   source = "waqn",
#'   year = 2022,
#'   to_narrow = TRUE,
#'   ratified = TRUE
#' )
#'
#' # import other data types, filtering by pollutant
#' importUKAQ(
#'   data_type = "annual",
#'   pollutant = c("no2", "pm2.5", "pm10"),
#'   source = c("aurn", "aqe")
#' )
#' }
importUKAQ <-
  function(site = "my1",
           year = 2022,
           source = NULL,
           data_type = "hourly",
           pollutant = "all",
           hc = FALSE,
           meta = FALSE,
           meta_columns = c("site_type", "latitude", "longitude"),
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           verbose = FALSE,
           progress = TRUE) {
    # warn if source == "local"
    if ("local" %in% source) {
      cli::cli_warn(
        c(
          "i" = "'local' data is associated with locally managed air quality network sites in England.",
          "!" = "These sites are not part of the AURN national network, and therefore may not have the same level of quality control applied to them."
        ),
        .frequency = "regularly",
        .frequency_id = "lmam"
      )
    }

    # guess sources
    if (is.null(source)) {
      if (data_type %in% c("annual", "monthly", "daqi") & missing(site)) {
        cli::cli_abort("Please provide a {.field source} when {.field data_type} is '{data_type}'.")
      }
      source <- guess_source(site)
    }
    
    # check correct "meta_columns" are passed
    if (meta) {
      meta_opts <- c("site_type",
                     "latitude",
                     "longitude",
                     "zone",
                     "agglomeration",
                     "local_authority")
      
      if ("local" %in% source) {
        meta_opts <- append(meta_opts, "provider")
      }
      
      meta_columns <-
        rlang::arg_match(
          meta_columns,
          meta_opts, 
          multiple = TRUE
        )
    }

    # obtain correct URL info for the source
    url_domain <- dplyr::case_match(
      source,
      "aurn" ~ "https://uk-air.defra.gov.uk/openair/R_data/",
      "aqe" ~ "https://airqualityengland.co.uk/assets/openair/R_data/",
      "saqn" ~ "https://www.scottishairquality.scot/openair/R_data/",
      "waqn" ~ "https://airquality.gov.wales/sites/default/files/openair/R_data/",
      "ni" ~ "https://www.airqualityni.co.uk/openair/R_data/",
      "local" ~ "https://uk-air.defra.gov.uk/openair/LMAM/R_data/"
    )

    url_abbr <- dplyr::case_match(
      source,
      "aurn" ~ "_AURN_",
      "aqe" ~ "_AQE_",
      "saqn" ~ "_SCOT_",
      "waqn" ~ "_WAQ_",
      "ni" ~ "_NI_",
      "local" ~ "_LMAM_"
    )

    # detect allowed types
    allowed_types <- c(
      "hourly",
      "daily",
      "monthly",
      "annual",
      "daqi",
      "15min",
      "15_min",
      "24_hour",
      "8_hour",
      "daily_max_8"
    )

    # handle 15_min for back-compatibility reasons
    if (data_type == "15_min") {
      data_type <- "15min"
    }

    if (!tolower(data_type) %in% allowed_types) {
      cli::cli_warn(
        c(
          "!" = "'{data_type}' not recognised. Setting {.arg data_type} to 'hourly'",
          "i" = "{.arg data_type} should be one of: {allowed_types}"
        )
      )

      data_type <- "hourly"
    }

    # Import Annual / Monthly Stats?
    if (data_type %in% c("annual", "monthly")) {
      if (missing(site)) {
        site <- "all"
      }

      # create file paths
      source <- unique(source)
      files <- paste0(url_domain, "summary_", data_type, url_abbr)
      files <- unique(files)

      # import statistics
      aq_data <- purrr::pmap(
        tidyr::crossing(tidyr::nesting(files, source), year),
        readSummaryData,
        data_type = data_type,
        to_narrow = to_narrow,
        hc = hc,
        .progress = ifelse(progress, "Importing Statistics", FALSE)
      ) %>%
        purrr::list_rbind()
    }

    # Import pre-calculated DAQI?
    if (data_type == "daqi") {
      if ("local" %in% source) {
        cli::cli_abort(c("!" = "{.arg data_type} 'DAQI' is not available for locally managed networks"))
      }
      if (missing(site)) {
        site <- "all"
      }

      # create file paths
      source <- unique(source)
      files <- paste0(url_domain, "annual_DAQI", url_abbr)
      files <- unique(files)

      # import DAQI
      aq_data <-
        purrr::pmap(
          tidyr::crossing(tidyr::nesting(files, source), year),
          readDAQI,
          .progress = ifelse(progress, "Importing DAQI", FALSE)
        ) %>%
        purrr::list_rbind() %>%
        dplyr::tibble() %>%
        dplyr::mutate(
          band = dplyr::case_when(
            .data$poll_index %in% 1:3 ~ "Low",
            .data$poll_index %in% 4:6 ~ "Moderate",
            .data$poll_index %in% 7:9 ~ "High",
            .data$poll_index == 10 ~ "Very High"
          ),
          band = factor(.data$band, c("Low", "Moderate", "High", "Very High")),
          .after = "poll_index"
        )
    }

    # Import any other stat
    if (!data_type %in% c("annual", "monthly", "daqi")) {
      site <- toupper(site)
      if (length(source) == 1) {
        source <- rep(source, times = length(site))
      } else if (length(source) != length(site)) {
        cli::cli_abort("Length of {.arg source} ({length(source)}) not equal to 1 or length of {.arg site} ({length(site)}).")
      }

      # deal with additional paths needed for local data
      if ("local" %in% source) {
        # get pcodes for file paths
        pcodes <-
          importMeta("local", all = TRUE) %>%
          dplyr::distinct(.data$site, .keep_all = TRUE) %>%
          select("code", "pcode")

        # get sites and pcodes
        site_info <-
          data.frame(
            code = site,
            source = source,
            url_data = url_domain
          ) %>%
          dplyr::left_join(pcodes,
            by = "code"
          ) %>%
          tidyr::crossing(year = year)
      } else {
        site_info <-
          data.frame(
            code = site,
            source = source,
            url_data = url_domain
          ) %>%
          dplyr::mutate(pcode = rep(NA, times = length(site))) %>%
          tidyr::crossing(year = year)
      }

      aq_data <-
        purrr::pmap(
          .l = site_info,
          .f = purrr::possibly(~ readUKAQData(
            site = ..1,
            lmam_subfolder = ..4,
            year = ..5,
            data_type,
            pollutant = pollutant,
            hc = hc,
            to_narrow = to_narrow,
            source = ..2,
            url_data = ..3,
            verbose = verbose
          ), quiet = !verbose),
          .progress = ifelse(progress, "Importing AQ Data", FALSE)
        ) %>%
        purrr::list_rbind()

      if (nrow(aq_data) == 0) {
        cli::cli_abort("No data returned. Check {.arg site}, {.arg year} and {.arg source}.",
          call = NULL
        )
      }

      if (ratified && data_type == "hourly") {
        aq_data <-
          add_ratified(
            aq_data = aq_data,
            source = source,
            to_narrow = to_narrow
          )
      }
    }

    # filter annual/monthly/DAQI data
    if (data_type %in% c("annual", "monthly", "daqi")) {
      aq_data <-
        filter_site_pollutant(
          aq_data,
          site = site,
          pollutant = pollutant,
          to_narrow = to_narrow,
          data_type = data_type
        )
    }

    # check to see if met data needed
    if (!meteo) {
      aq_data <- aq_data %>%
        select(-any_of(c("ws", "wd", "air_temp")))
    }

    # add meta data?
    if (meta) {
      aq_data <-
        add_meta(source = source, columns = meta_columns, aq_data)
    }

    # arrange output by source
    aq_data <- dplyr::arrange(aq_data, "source", "site")

    # return data
    return(as_tibble(aq_data))
  }

#' Import data from individual UK Air Pollution Networks
#'
#' These functions act as wrappers for [importUKAQ()] to import air pollution
#' data from a range of UK networks including the Automatic Urban and Rural
#' Network (AURN), the individual England (AQE), Scotland (SAQN), Wales (WAQN)
#' and Northern Ireland (NI) Networks, and many "locally managed" monitoring
#' networks across England. While [importUKAQ()] allows for data to be imported
#' more flexibly, including across multiple monitoring networks, these functions
#' are provided for convenience and back-compatibility.
#'
#' @inheritSection importUKAQ Importing UK Air Pollution Data
#'
#' @param site Site code of the site to import, e.g., `"my1"` is Marylebone
#'   Road. Site codes can be discovered through the use of [importMeta()].
#'   Several sites can be imported at once. For example, `site = c("my1",
#'   "nott")` imports both Marylebone Road and Nottingham.
#'
#' @inheritParams importUKAQ
#' @rdname importUKAQ-wrapper
#' @order 1
#' @family import functions
#' @export
importAURN <-
  function(site = "my1",
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
           progress = TRUE) {
    if (missing(site) & data_type %in% c("annual", "monthly", "daqi")) {
      site <- "all"
    }

    importUKAQ(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      hc = hc,
      meta = meta,
      meta_columns = meta_columns,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      verbose = verbose,
      source = "aurn"
    )
  }

#' @rdname importUKAQ-wrapper
#' @order 2
#' @export
importAQE <-
  function(site = "yk13",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meta_columns = c("site_type", "latitude", "longitude"),
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           verbose = FALSE,
           progress = TRUE) {
    if (missing(site) & data_type %in% c("annual", "monthly", "daqi")) {
      site <- "all"
    }

    importUKAQ(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meta_columns = meta_columns,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      verbose = verbose,
      source = "aqe"
    )
  }

#' @rdname importUKAQ-wrapper
#' @order 3
#' @export
importSAQN <-
  function(site = "gla4",
           year = 2009,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meta_columns = c("site_type", "latitude", "longitude"),
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           verbose = FALSE,
           progress = TRUE) {
    if (missing(site) & data_type %in% c("annual", "monthly", "daqi")) {
      site <- "all"
    }

    importUKAQ(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meta_columns = meta_columns,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      verbose = verbose,
      source = "saqn"
    )
  }

#' @rdname importUKAQ-wrapper
#' @order 4
#' @export
importWAQN <-
  function(site = "card",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meta_columns = c("site_type", "latitude", "longitude"),
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           verbose = FALSE,
           progress = TRUE) {
    if (missing(site) & data_type %in% c("annual", "monthly", "daqi")) {
      site <- "all"
    }

    importUKAQ(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meta_columns = meta_columns,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      verbose = verbose,
      source = "waqn"
    )
  }

#' @rdname importUKAQ-wrapper
#' @order 5
#' @export
importNI <-
  function(site = "bel0",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meta_columns = c("site_type", "latitude", "longitude"),
           meteo = TRUE,
           ratified = FALSE,
           to_narrow = FALSE,
           verbose = FALSE,
           progress = TRUE) {
    if (missing(site) & data_type %in% c("annual", "monthly", "daqi")) {
      site <- "all"
    }

    importUKAQ(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meta_columns = meta_columns,
      meteo = meteo,
      ratified = ratified,
      to_narrow = to_narrow,
      progress = progress,
      verbose = verbose,
      source = "ni"
    )
  }

#' @rdname importUKAQ-wrapper
#' @order 6
#' @export
importLocal <-
  function(site = "ad1",
           year = 2018,
           data_type = "hourly",
           pollutant = "all",
           meta = FALSE,
           meta_columns = c("site_type", "latitude", "longitude"),
           to_narrow = FALSE,
           verbose = FALSE,
           progress = TRUE) {
    if (missing(site) & data_type %in% c("annual", "monthly", "daqi")) {
      site <- "all"
    }

    importUKAQ(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meta_columns = meta_columns,
      meteo = TRUE,
      ratified = FALSE,
      to_narrow = to_narrow,
      progress = progress,
      verbose = verbose,
      source = "local"
    )
  }
