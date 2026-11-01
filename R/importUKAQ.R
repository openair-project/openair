#' Import data from the UK Air Pollution Networks
#'
#' Functions for importing air pollution data from a range of UK networks
#' including the Automatic Urban and Rural Network (AURN), the individual
#' England (AQE), Scotland (SAQN), Wales (WAQN) and Northern Ireland (NI)
#' Networks, and many "locally managed" monitoring networks across England.
#' Files are imported from a remote server operated by WSP that provides air
#' quality data files as R data objects. For an up to date list of available
#' sites that can be imported, see [importMeta()].
#'
#' @section Importing UK Air Pollution Data:
#'
#'   This family of functions has been written to make it easy to import data
#'   from across several UK air quality networks. WSP have provided .RData files
#'   (R workspaces) of all individual sites and years, as well as up to date
#'   meta data. These files are updated on a daily basis. This approach requires
#'   a link to the Internet to work.
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
#'   operated by WSP.
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
#'   codes can be upper or lower case. When `data_type` is `"annual"`,
#'   `"monthly"` or `"daqi"`, `site` can be omitted to import all sites in the
#'   given `source`(s).
#'
#' @param year Year(s) to import. To import a series of years use, e.g.,
#'   `2000:2020`. To import several specific years use `year = c(2000, 2010,
#'   2020)`.
#'
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
#'
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
#'   [here](https://www.gov.uk/government/publications/health-effects-of-air-pollution)
#'   for more details of how the index is defined. Note that this `data_type` is
#'   not available for locally managed monitoring networks.
#'
#' @param pollutant Pollutants to import. If omitted will import all pollutants
#'   from a site. To import only NOx and NO2 for example use `pollutant =
#'   c("nox", "no2")`. Pollutant names can be upper or lower case.
#'
#' @param hc Include hydrocarbon measurements in the imported data? Defaults to
#'   `FALSE` as most users will not be interested in using hydrocarbon data.
#'   Hydrocarbon data are only available for the AURN.
#'
#' @param meta Append metadata columns to data for each selected `site`?
#'   Defaults to `FALSE`. Columns are defined using `meta_columns`.
#'
#' @param meta_columns The specific columns to append when `meta = TRUE`.
#'   Defaults to site type, latitude and longitude. Can be any of `"site_type"`,
#'   `"latitude"`, `"longitude"`, `"zone"`, `"agglomeration"`, and
#'   `"local_authority"` (as well as `"provider"` for locally managed data). See
#'   [importMeta()] for more complete information.
#'
#' @param meteo Append modelled meteorological data, if available? Defaults to
#'   `TRUE`, which will return wind speed (`ws`), wind direction (`wd`) and
#'   ambient temperature (`air_temp`). The variables are calculated from using
#'   the WRF model run by WSP and are available for most but not all networks.
#'   They are not available for locally managed networks. Setting `meteo =
#'   FALSE` is useful if you have other meteorological data to use in
#'   preference, for example from the `worldmet` package.
#'
#' @param ratified Append `qc` column(s) to hourly data indicating whether each
#'   species was ratified (i.e., quality-checked)?  Defaults to `FALSE`.
#'   Ratification information is not available for locally managed networks.
#'
#' @param to_narrow Return the data in a "narrow"/"long"/"tidy" format? By
#'   default the returned data is "wide" and has a column for each
#'   pollutant/variable. When `to_narrow = TRUE` the data are returned with a
#'   column identifying the pollutant name and a column containing the
#'   corresponding concentration/statistic. Defaults to `FALSE`.
#'
#' @param verbose Print a message to the console if any data cannot be imported?
#'   Default is `FALSE`. `TRUE` is useful for debugging as the specific
#'   `year`(s), `site`(s) and `source`(s) which cannot be imported will be
#'   listed. Not used when `data_type` is `"annual"`, `"monthly"` or `"daqi"`.
#'
#' @param progress Show a progress bar when many sites/years are being imported?
#'   Defaults to `TRUE`.
#'
#' @export
#' @return a `tibble`
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
#' importUKAQ(
#'   site = c("my1", "bn1", "bn2"),
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
#'   year = 2020,
#'   data_type = "annual",
#'   pollutant = c("no2", "pm2.5", "pm10"),
#'   source = c("aurn", "aqe")
#' )
#' }
importUKAQ <-
  function(
    site,
    year,
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
    progress = TRUE
  ) {
    # pollutant names are matched in lower case
    pollutant <- tolower(pollutant)

    # check data_type
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

    data_type <- tolower(data_type)
    data_type <- rlang::arg_match(data_type, allowed_types, multiple = FALSE)

    if (ratified && data_type != "hourly") {
      cli::cli_warn(
        "{.arg ratified} is only available for hourly data, and will be ignored for {.arg data_type} '{data_type}'."
      )
    }

    # handle 15_min for back-compatibility reasons
    if (data_type == "15_min") {
      data_type <- "15min"
    }

    # network-wide statistics are stored differently to individual site data
    is_stats <- data_type %in% c("annual", "monthly", "daqi")

    # year is always required, but site can be omitted for network-wide
    # statistics to import all sites
    rlang::check_required(year)

    if (missing(site)) {
      if (!is_stats) {
        cli::cli_abort(
          c(
            "x" = "{.arg site} must be supplied when {.arg data_type} is '{data_type}'.",
            "i" = "{.arg site} can only be omitted when {.arg data_type} is 'annual', 'monthly' or 'daqi', to import all sites."
          )
        )
      }

      if (is.null(source)) {
        cli::cli_abort(
          "Please provide a {.field source} when {.field data_type} is '{data_type}'."
        )
      }

      site <- "all"
    }

    # check sources, allowing for the same aliases as importMeta()
    if (!is.null(source)) {
      source <- normalise_source(source)
      allowed_sources <- ukaq_networks$source
      source <- rlang::arg_match(source, allowed_sources, multiple = TRUE)
    }

    # guess sources
    if (is.null(source)) {
      source <- guess_site_source(site)
    }

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

    # check correct "meta_columns" are passed
    if (meta) {
      meta_opts <- c(
        "site_type",
        "latitude",
        "longitude",
        "zone",
        "agglomeration",
        "local_authority"
      )

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

    # DAQI is only available for national networks
    if (data_type == "daqi" && "local" %in% source) {
      cli::cli_abort(c(
        "!" = "{.arg data_type} 'DAQI' is not available for locally managed networks"
      ))
    }

    # each site needs a source
    if (!is_stats) {
      if (length(source) == 1) {
        source <- rep(source, times = length(site))
      } else if (length(source) != length(site)) {
        cli::cli_abort(
          "Length of {.arg source} ({length(source)}) not equal to 1 or length of {.arg site} ({length(site)})."
        )
      }
    }

    # import metadata once, if needed for ratification, metadata columns, or
    # the file paths of locally managed data
    add_qc <- ratified && data_type == "hourly"
    meta_data <- NULL

    if (meta || add_qc || (!is_stats && "local" %in% source)) {
      meta_data <- importMeta(unique(source), all = TRUE, duplicate = TRUE)
    }

    # import data
    if (is_stats) {
      aq_data <- import_network_stats(
        source = source,
        year = year,
        data_type = data_type,
        to_narrow = to_narrow,
        hc = hc,
        progress = progress
      )
    } else {
      aq_data <- import_site_data(
        site = site,
        source = source,
        year = year,
        data_type = data_type,
        pollutant = pollutant,
        hc = hc,
        to_narrow = to_narrow,
        verbose = verbose,
        progress = progress,
        meta_data = meta_data
      )
    }

    if (nrow(aq_data) == 0) {
      cli::cli_abort(
        "No data returned. Check {.arg site}, {.arg year} and {.arg source}.",
        call = NULL
      )
    }

    # add ratification flags to hourly data
    if (add_qc) {
      if ("local" %in% source) {
        cli::cli_warn(
          "Ratification information is not available for locally managed networks; their {.field qc} values will be {.val {NA}}."
        )
      }

      aq_data <- add_ratified_flags(aq_data, meta_data, to_narrow = to_narrow)
    }

    # filter annual/monthly/DAQI data
    if (is_stats) {
      aq_data <-
        filter_network_stats(
          aq_data,
          site = site,
          pollutant = pollutant,
          to_narrow = to_narrow,
          data_type = data_type
        )
    }

    # check to see if met data needed
    if (!meteo) {
      aq_data <- aq_data |>
        dplyr::select(-dplyr::any_of(c("ws", "wd", "air_temp")))
    }

    # use a consistent column order: identifiers (from least to most
    # variable), then pollutants, then meteorological data
    aq_data <- aq_data |>
      dplyr::relocate(dplyr::any_of(c("source", "site", "code", "date"))) |>
      dplyr::relocate(
        dplyr::any_of(c("ws", "wd", "air_temp")),
        .after = dplyr::last_col()
      )

    # add meta data?
    if (meta) {
      aq_data <- add_site_meta(aq_data, meta_data, columns = meta_columns)
    }

    # arrange output by source
    aq_data <- dplyr::arrange(aq_data, .data$source, .data$site)

    # return data
    return(dplyr::as_tibble(aq_data))
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
#' Not every argument is available in every wrapper, as not all data are
#' available for every network:
#'
#' - `hc` is only available in [importAURN()], as hydrocarbon data are only
#' measured by the AURN.
#'
#' - `meteo` and `ratified` are not available in [importLocal()], as neither
#' modelled meteorological data nor ratification information are available for
#' locally managed networks.
#'
#' @inheritSection importUKAQ Importing UK Air Pollution Data
#'
#' @param site Site code of the site to import, e.g., `"my1"` is Marylebone
#'   Road. Site codes can be discovered through the use of [importMeta()].
#'   Several sites can be imported at once. For example, `site = c("my1",
#'   "nott")` imports both Marylebone Road and Nottingham. When `data_type` is
#'   `"annual"`, `"monthly"` or `"daqi"`, `site` can be omitted to import all
#'   sites in the network.
#'
#' @inheritParams importUKAQ
#' @rdname importUKAQ-wrapper
#' @order 1
#' @family import functions
#' @export
importAURN <-
  function(
    site,
    year,
    data_type = "hourly",
    pollutant = "all",
    hc = FALSE,
    meta = FALSE,
    meta_columns = c("site_type", "latitude", "longitude"),
    meteo = TRUE,
    ratified = FALSE,
    to_narrow = FALSE,
    verbose = FALSE,
    progress = TRUE
  ) {
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
  function(
    site,
    year,
    data_type = "hourly",
    pollutant = "all",
    meta = FALSE,
    meta_columns = c("site_type", "latitude", "longitude"),
    meteo = TRUE,
    ratified = FALSE,
    to_narrow = FALSE,
    verbose = FALSE,
    progress = TRUE
  ) {
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
  function(
    site,
    year,
    data_type = "hourly",
    pollutant = "all",
    meta = FALSE,
    meta_columns = c("site_type", "latitude", "longitude"),
    meteo = TRUE,
    ratified = FALSE,
    to_narrow = FALSE,
    verbose = FALSE,
    progress = TRUE
  ) {
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
  function(
    site,
    year,
    data_type = "hourly",
    pollutant = "all",
    meta = FALSE,
    meta_columns = c("site_type", "latitude", "longitude"),
    meteo = TRUE,
    ratified = FALSE,
    to_narrow = FALSE,
    verbose = FALSE,
    progress = TRUE
  ) {
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
  function(
    site,
    year,
    data_type = "hourly",
    pollutant = "all",
    meta = FALSE,
    meta_columns = c("site_type", "latitude", "longitude"),
    meteo = TRUE,
    ratified = FALSE,
    to_narrow = FALSE,
    verbose = FALSE,
    progress = TRUE
  ) {
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
  function(
    site,
    year,
    data_type = "hourly",
    pollutant = "all",
    meta = FALSE,
    meta_columns = c("site_type", "latitude", "longitude"),
    to_narrow = FALSE,
    verbose = FALSE,
    progress = TRUE
  ) {
    importUKAQ(
      site = site,
      year = year,
      data_type = data_type,
      pollutant = pollutant,
      meta = meta,
      meta_columns = meta_columns,
      # no modelled met or ratification information for locally managed data
      meteo = TRUE,
      ratified = FALSE,
      to_narrow = to_narrow,
      progress = progress,
      verbose = verbose,
      source = "local"
    )
  }
