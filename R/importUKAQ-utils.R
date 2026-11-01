# Helpers for importUKAQ() and importMeta(). The networks' files are organised
# as:
#
# - Site data: one .RData file per site and year (e.g., "MY1_2020.RData"),
# containing an object for each `data_type` (e.g., "MY1_2020_daily_mean").
# Locally managed data has a subfolder per provider ("pcode").
#
# - Network statistics: one .rds file per network and year for each of the
# annual statistics, monthly statistics and DAQI.

# Networks and sources ----------------------------------------------------

#' The UK air quality networks for which WSP provide data, with the base URL of
#' their files and the abbreviation used in their file names
#' @noRd
ukaq_networks <- data.frame(
  source = c("aurn", "aqe", "saqn", "waqn", "ni", "local"),
  url = c(
    "https://uk-air.defra.gov.uk/openair/R_data/",
    "https://www.airqualityengland.co.uk/assets/openair/R_data/",
    "https://www.scottishairquality.scot/openair/R_data/",
    "https://airquality.gov.wales/sites/default/files/openair/R_data/",
    "https://www.airqualityni.co.uk/openair/R_data/",
    "https://uk-air.defra.gov.uk/openair/LMAM/R_data/"
  ),
  abbr = c("AURN", "AQE", "SCOT", "WAQ", "NI", "LMAM")
)

#' Look up the base URL or file abbreviation of one or more UK networks
#' @noRd
ukaq_network_info <- function(source, what = c("url", "abbr")) {
  what <- rlang::arg_match(what)
  ukaq_networks[[what]][match(source, ukaq_networks$source)]
}

#' Lower-case network names and resolve their aliases
#' @noRd
normalise_source <- function(source) {
  source <- tolower(source)
  aliases <- c(kcl = "imperial", saqd = "saqn", niaqn = "ni", lmam = "local")
  is_alias <- source %in% names(aliases)
  source[is_alias] <- unname(aliases[source[is_alias]])
  source
}

#' Guess the network of each site code, preferring national networks
#' @param site Sites passed to [importUKAQ()]
#' @param call The calling function, reported in errors
#' @noRd
guess_site_source <- function(site, call = rlang::caller_env()) {
  ukaq_meta <- importMeta("ukaq") |>
    dplyr::mutate(
      source = factor(
        .data$source,
        c("aurn", "saqn", "aqe", "waqn", "ni", "local")
      ),
      code = toupper(.data$code)
    ) |>
    dplyr::arrange(.data$source) |>
    dplyr::distinct(
      .data$site,
      .data$code,
      .data$latitude,
      .data$longitude,
      .keep_all = TRUE
    )

  source_tbl <-
    data.frame(code = toupper(site)) |>
    dplyr::left_join(ukaq_meta, by = "code")

  if (anyNA(source_tbl$source)) {
    ambiguous_codes <-
      source_tbl |>
      dplyr::filter(is.na(.data$source)) |>
      dplyr::pull(.data$code)

    cli::cli_abort(
      c(
        "x" = "Unknown site codes detected. Please ensure all site codes can be found in {.fun importMeta}.",
        "i" = "Unknown site codes: {ambiguous_codes}"
      ),
      call = call
    )
  }

  if (nrow(source_tbl) > length(site)) {
    source_tbl_all <- source_tbl

    source_tbl <- dplyr::slice_head(source_tbl, n = 1, by = "code")

    source_tbl_other <-
      dplyr::anti_join(
        source_tbl_all,
        source_tbl,
        by = dplyr::join_by(
          "code",
          "source",
          "site",
          "latitude",
          "longitude",
          "site_type"
        )
      ) |>
      dplyr::distinct()

    alternatives <-
      source_tbl_other |>
      dplyr::mutate(
        str = paste0(
          .data$code,
          " (could also be '",
          .data$site,
          "' from the source: '",
          .data$source,
          "'.)"
        )
      ) |>
      dplyr::pull(.data$str)

    msg <- c(
      "x" = "Ambiguous site codes detected. National networks are imported preferentially to locally managed networks.",
      alternatives,
      "i" = "Specify {.field source} to import sites from specific monitoring networks."
    )

    names(msg)[names(msg) == ""] <- "!"

    cli::cli_warn(msg)
  }

  return(as.character(source_tbl$source))
}

# Downloading -------------------------------------------------------------

#' Read a remote .rds or .RData file, returning `NULL` if it cannot be read.
#' .RData files are loaded into, and returned as, their own environment.
#' @noRd
read_remote_file <- function(url) {
  con <- url(url)
  on.exit(close(con))

  tryCatch(
    suppressWarnings(
      if (endsWith(url, ".rds")) {
        readRDS(con)
      } else {
        env <- new.env()
        load(con, envir = env)
        env
      }
    ),
    error = function(e) NULL
  )
}

# Site data ---------------------------------------------------------------

#' Import data for individual sites
#' @param site,source,year,data_type,pollutant,hc,to_narrow,verbose,progress
#'   Passed from [importUKAQ()]
#' @param meta_data Metadata for `source`, used to find the files of locally
#'   managed sites
#' @noRd
import_site_data <- function(
  site,
  source,
  year,
  data_type,
  pollutant,
  hc,
  to_narrow,
  verbose,
  progress,
  meta_data
) {
  site_info <- data.frame(code = toupper(site), source = source)

  # locally managed data is stored in a subfolder per provider ("pcode"); sites
  # can be listed by more than one provider, in which case each is tried
  if ("local" %in% source) {
    pcodes <-
      meta_data |>
      dplyr::filter(.data$source == "local") |>
      dplyr::distinct(dplyr::pick("source", "code", "pcode"))

    site_info <- dplyr::left_join(site_info, pcodes, by = c("source", "code"))
  } else {
    site_info$pcode <- NA
  }

  site_info <- tidyr::crossing(site_info, year = year)
  site_info$file <- paste0(site_info$code, "_", site_info$year)
  site_info$url <- paste0(
    ukaq_network_info(site_info$source, "url"),
    ifelse(site_info$source == "local", paste0(site_info$pcode, "/"), ""),
    site_info$file,
    ".RData"
  )

  aq_data <-
    purrr::pmap(
      list(site_info$url, site_info$file, site_info$source),
      purrr::possibly(function(url, file, source) {
        read_site_file(
          url = url,
          file = file,
          source = source,
          data_type = data_type,
          pollutant = pollutant,
          hc = hc,
          to_narrow = to_narrow
        )
      }),
      .progress = ifelse(progress, "Importing AQ Data", FALSE)
    )

  if (verbose) {
    warn_failed_downloads(
      site_info,
      imported = !purrr::map_lgl(aq_data, is.null)
    )
  }

  purrr::list_rbind(aq_data)
}

#' Read and tidy one site-year file, returning `NULL` if it has no data
#' @param url The URL of the .RData file
#' @param file The name of the file (e.g., "MY1_2020"), which is also the name
#'   of the hourly data object it contains
#' @param source,data_type,pollutant,hc,to_narrow Passed from [importUKAQ()]
#' @noRd
read_site_file <- function(
  url,
  file,
  source,
  data_type,
  pollutant,
  hc,
  to_narrow
) {
  data_env <- read_remote_file(url)

  # each data_type is stored in a different object in the file
  object_suffix <- c(
    hourly = "",
    `15min` = "_15min",
    daily = "_daily_mean",
    `8_hour` = "_8hour_mean",
    `24_hour` = "_24hour_mean",
    daily_max_8 = "_daily_max_8hour"
  )
  object <- paste0(file, object_suffix[[data_type]])

  if (
    is.null(data_env) || !exists(object, envir = data_env, inherits = FALSE)
  ) {
    return()
  }

  thedata <- get(object, envir = data_env, inherits = FALSE)

  # gravimetric PM is in a separate object. These are measured daily PM
  # measurements rather than daily mean hourly
  gr_object <- paste0(file, "_daily")

  if (
    data_type == "daily" &&
      exists(gr_object, envir = data_env, inherits = FALSE)
  ) {
    # suppress warnings (e.g., unequal factors, harmless)
    thedata <-
      suppressWarnings(dplyr::left_join(
        thedata,
        get(gr_object, envir = data_env, inherits = FALSE),
        by = c("date", "site", "code")
      )) |>
      dplyr::rename(dplyr::any_of(c(gr_pm2.5 = "GR2.5", gr_pm10 = "GR10")))
  }

  # make sure class is correct for lubridate
  class(thedata$date) <- c("POSIXct", "POSIXt")

  if (nrow(thedata) == 0) {
    return()
  }

  # change names, including nox as no2
  names(thedata) <- tolower(names(thedata))
  thedata <- dplyr::rename(thedata, dplyr::any_of(c(nox = "noxasno2")))

  # change code to character
  thedata$code <- as.character(thedata$code)

  # should hydrocarbons be imported?
  if (!hc) {
    thedata <- dplyr::select(
      thedata,
      dplyr::any_of(site_file_columns),
      dplyr::matches("_qc")
    )
  } else {
    thedata <- dplyr::relocate(
      thedata,
      dplyr::any_of(c("site", "code", "date"))
    )
  }

  # rename "temp" to "air_temp" if appropriate
  thedata <- dplyr::rename(thedata, dplyr::any_of(c(air_temp = "temp")))

  # if particular pollutants have been selected
  if (pollutant[1] != "all") {
    # also keep gravimetric PM measurements (in daily data)
    gr_pollutant <- paste0(
      "gr_",
      pollutant[pollutant %in% c("pm10", "pm2.5")]
    )
    pollutant <- c(pollutant, gr_pollutant)

    thedata <-
      dplyr::select(
        thedata,
        "date",
        "site",
        "code",
        dplyr::any_of(c(pollutant, "ws", "wd", "air_temp"))
      )
  }

  # make sure it is in GMT
  attr(thedata$date, "tzone") <- "GMT"

  # tidy data if requested
  if (to_narrow) {
    # variables which are not pollutants
    the_vars <- c("date", "site", "code", "ws", "wd", "air_temp")

    thedata <-
      tidyr::pivot_longer(
        thedata,
        cols = -dplyr::any_of(the_vars),
        names_to = "pollutant"
      )

    # clean tidied data before returning
    thedata <- thedata |>
      dplyr::relocate(dplyr::any_of(the_vars)) |>
      dplyr::arrange(.data$site, .data$code, .data$pollutant, .data$date)
  }

  # add source to output
  thedata <-
    dplyr::mutate(thedata, source = source, .before = dplyr::everything())

  dplyr::as_tibble(thedata)
}

#' Columns kept from site files when `hc = FALSE` (i.e., all but hydrocarbons)
#' @noRd
site_file_columns <- c(
  "site",
  "code",
  "date",
  "co",
  "nox",
  "no2",
  "no",
  "o3",
  "so2",
  "pm10",
  "pm2.5",
  "v10",
  "v2.5",
  "nv10",
  "nv2.5",
  "gr_pm10",
  "gr_pm2.5",
  "ws",
  "wd",
  "temp"
)

#' Warn about all of the site/year combinations which could not be imported
#' @param site_info Data frame of sites, sources and years passed to
#'   `read_site_file()`
#' @param imported Logical vector, the same length as `site_info`, of whether
#'   each row was imported
#' @noRd
warn_failed_downloads <- function(site_info, imported) {
  # sites can be tried in multiple local subfolders, so only count a site/year
  # as failed if all attempts failed
  failed <-
    dplyr::mutate(site_info, imported = imported) |>
    dplyr::summarise(
      imported = any(.data$imported),
      .by = c("source", "code", "year")
    ) |>
    dplyr::filter(!.data$imported)

  if (nrow(failed) == 0) {
    return(invisible())
  }

  n_failed <- nrow(failed)

  failed <-
    failed |>
    dplyr::summarise(
      year = paste(sort(.data$year), collapse = ", "),
      .by = c("source", "code")
    ) |>
    dplyr::mutate(
      str = paste0(.data$code, " (", .data$source, "): ", .data$year)
    )

  cli::cli_warn(
    c(
      "!" = "Could not import data for {n_failed} site-year combination{?s}:",
      rlang::set_names(failed$str, "*")
    )
  )
}

# Network statistics ------------------------------------------------------

#' Import network-wide annual, monthly or DAQI statistics
#' @param source,year,data_type,to_narrow,hc,progress Passed from
#'   [importUKAQ()]
#' @noRd
import_network_stats <- function(
  source,
  year,
  data_type,
  to_narrow,
  hc,
  progress
) {
  files <- tidyr::crossing(source = unique(source), year = year)

  if (data_type == "daqi") {
    file_stem <- "annual_DAQI_"
    read_file <- function(url, source) read_daqi_file(url, source)
    progress_msg <- "Importing DAQI"
  } else {
    file_stem <- paste0("summary_", data_type, "_")
    read_file <- function(url, source) {
      read_summary_file(url, source, data_type, to_narrow = to_narrow, hc = hc)
    }
    progress_msg <- "Importing Statistics"
  }

  files$url <- paste0(
    ukaq_network_info(files$source, "url"),
    file_stem,
    ukaq_network_info(files$source, "abbr"),
    "_",
    files$year,
    ".rds"
  )

  purrr::map2(
    files$url,
    files$source,
    read_file,
    .progress = ifelse(progress, progress_msg, FALSE)
  ) |>
    purrr::list_rbind()
}

#' Read and tidy one annual or monthly statistics file, returning `NULL` if it
#' cannot be read
#' @noRd
read_summary_file <- function(url, source, data_type, to_narrow, hc) {
  thedata <- read_remote_file(url)

  if (is.null(thedata)) {
    return()
  }

  names(thedata)[names(thedata) == "NOXasNO2.mean"] <- "nox"
  names(thedata)[names(thedata) == "NOXasNO2.capture"] <-
    "nox_capture"
  names(thedata) <- tolower(names(thedata))
  names(thedata) <- gsub(".mean", "", names(thedata), fixed = TRUE)
  names(thedata) <- gsub(
    ".capture",
    "_capture",
    names(thedata),
    fixed = TRUE
  )

  if (!hc) {
    thedata <- dplyr::select(thedata, dplyr::any_of(summary_file_columns))
  }

  if (data_type == "monthly") {
    thedata <- dplyr::mutate(thedata, date = lubridate::ymd(date, tz = "UTC"))
  }

  if (data_type == "annual") {
    thedata <- dplyr::rename(thedata, date = year) |>
      tidyr::drop_na(date) |>
      dplyr::mutate(date = lubridate::ymd(paste0(date, "-01-01"), tz = "UTC"))
  }

  if (to_narrow) {
    # make sure numbers are numbers
    values <- dplyr::select(thedata, !dplyr::contains("capture")) |>
      dplyr::select(!dplyr::matches("uka_code"))

    capture <-
      dplyr::select(
        thedata,
        dplyr::contains("capture") | c("code", "date", "site")
      ) |>
      dplyr::select(!dplyr::matches("uka_code"))

    values <- tidyr::pivot_longer(
      values,
      -c("date", "code", "site"),
      values_to = "value",
      names_to = "species"
    )

    capture <- tidyr::pivot_longer(
      capture,
      -c("date", "code", "site"),
      values_to = "data_capture",
      names_to = "species"
    )

    capture$species <- gsub("_capture", "", capture$species)

    thedata <- dplyr::full_join(
      values,
      capture,
      by = c("date", "code", "site", "species")
    )
  }

  thedata <- thedata |>
    dplyr::mutate(
      site = as.character(.data$site),
      code = as.character(.data$code)
    ) |>
    dplyr::mutate(
      source = source,
      .before = dplyr::everything()
    )

  return(thedata)
}

#' Columns kept from statistics files when `hc = FALSE` (i.e., all but
#' hydrocarbons)
#' @noRd
summary_file_columns <- c(
  "date",
  "uka_code",
  "code",
  "site",
  "year",
  "o3",
  "o3_capture",
  "o3.summer_capture",
  "o3.daily.max.8hour",
  "o3.aot40v",
  "o3.aot40f",
  "somo35",
  "somo35_capture",
  "no",
  "no_capture",
  "no2",
  "no2_capture",
  "nox",
  "nox_capture",
  "so2",
  "so2_capture",
  "co",
  "co_capture",
  "pm10",
  "pm10_capture",
  "nv10",
  "nv10_capture",
  "v10",
  "v10_capture",
  "pm2.5",
  "pm2.5_capture",
  "nv2.5",
  "nv2.5_capture",
  "v2.5",
  "v2.5_capture",
  "gr10",
  "gr10_capture",
  "gr2.5",
  "gr2.5_capture",
  "gr_pm2.5",
  "gr_pm10"
)

#' Read and tidy one DAQI file, returning `NULL` if it cannot be read
#' @noRd
read_daqi_file <- function(url, source) {
  thedata <- read_remote_file(url)

  if (is.null(thedata)) {
    return()
  }

  thedata <- thedata |>
    dplyr::mutate(
      code = as.character(.data$code),
      site = as.character(.data$site),
      pollutant = as.character(.data$pollutant),
      date = lubridate::ymd(.data$Date, tz = "GMT"),
      measurement_period = as.character(.data$measurement_period)
    ) |>
    dplyr::select(-"Date") |>
    dplyr::relocate("date", .after = "pollutant") |>
    dplyr::mutate(
      source = source,
      .before = dplyr::everything()
    ) |>
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

  dplyr::tibble(thedata)
}

#' Filter annual, monthly or DAQI statistics by site and pollutant
#' @param site,pollutant,to_narrow,data_type Passed from [importUKAQ()]
#' @noRd
filter_network_stats <- function(
  aq_data,
  site,
  pollutant,
  to_narrow,
  data_type
) {
  # if site isn't missing, filter by sites
  if (any(site != "all")) {
    aq_data <- aq_data[tolower(aq_data$code) %in% tolower(site), ]
  }

  # if pollutant isn't "all", filter pollutants
  if (any(pollutant != "all")) {
    pollutant <- tolower(pollutant)

    # match pollutants exactly, but keep their capture rates, gravimetric
    # measurements and any derived statistics (e.g., "o3_capture" and
    # "o3.daily.max.8hour" for "o3", or "gr10" and "gr_pm10" for "pm10")
    prefixes <- paste0(pollutant, ".")
    match_pollutant <- function(x) {
      x <- sub("_capture$", "", tolower(x))
      x <- sub("^gr_?(pm)?", "pm", x)
      is_derived <- purrr::map_lgl(x, ~ any(startsWith(.x, prefixes)))
      x %in% pollutant | is_derived
    }

    if (data_type != "daqi") {
      if (to_narrow) {
        aq_data <- aq_data[match_pollutant(aq_data$species), ]
      } else {
        id_cols <- c("source", "uka_code", "code", "site", "date")
        aq_data <- aq_data[
          names(aq_data) %in% id_cols | match_pollutant(names(aq_data))
        ]
      }
    } else {
      aq_data <- aq_data[tolower(aq_data$pollutant) %in% pollutant, ]
    }
  }

  # return output
  return(aq_data)
}

# Adding flags and metadata -----------------------------------------------

#' Add `qc` flag(s) to hourly data, indicating whether each value is ratified
#' @param meta metadata from `importMeta(all = TRUE, duplicate = TRUE)`
#' @noRd
add_ratified_flags <- function(aq_data, meta, to_narrow) {
  # locally managed networks have no ratification information
  if (!"ratified_to" %in% names(meta)) {
    meta$ratified_to <- as.POSIXct(NA, tz = "GMT")
  }

  meta <- meta |>
    dplyr::filter(
      .data$code %in% aq_data$code,
      !.data$variable %in%
        c(
          "V10",
          "NV10",
          "V2.5",
          "NV2.5",
          "ws",
          "wd",
          "temp"
        )
    ) |>
    dplyr::select("source", "code", "variable", "ratified_to") |>
    dplyr::mutate(variable = tolower(.data$variable)) |>
    # sites can be listed more than once in a network (e.g., by multiple local
    # providers)
    dplyr::distinct(
      .data$source,
      .data$code,
      .data$variable,
      .keep_all = TRUE
    )

  if (to_narrow) {
    meta <-
      dplyr::rename(meta, "pollutant" = "variable", "qc" = "ratified_to") |>
      dplyr::filter(.data$pollutant %in% tolower(aq_data$pollutant))

    aq_data <-
      aq_data |>
      dplyr::left_join(
        meta,
        by = dplyr::join_by("source", "code", "pollutant")
      ) |>
      dplyr::mutate(qc = lubridate::floor_date(.data$date, "day") <= .data$qc)

    return(aq_data)
  }

  meta <-
    meta |>
    dplyr::filter(.data$variable %in% names(aq_data)) |>
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "ratified_to",
      names_glue = "{variable}_qc"
    )

  aq_data <-
    aq_data |>
    dplyr::left_join(meta, by = dplyr::join_by("source", "code")) |>
    dplyr::mutate(dplyr::across(dplyr::contains("_qc"), function(x) {
      lubridate::floor_date(.data$date, "day") <= x
    }))

  return(aq_data)
}

#' Add metadata columns (e.g., site type) to imported data
#' @param aq_data imported air quality data (annual, daqi, or otherwise)
#' @param meta_data metadata from `importMeta(all = TRUE, duplicate = TRUE)`
#' @param columns metadata columns to add
#' @noRd
add_site_meta <- function(aq_data, meta_data, columns) {
  # join by code only, as site names can differ between data and metadata
  meta_data <- dplyr::distinct(
    meta_data,
    .data[["source"]],
    .data[["code"]],
    .keep_all = TRUE
  ) |>
    dplyr::select("source", "code", dplyr::all_of(columns))

  aq_data <- dplyr::left_join(
    aq_data,
    meta_data,
    by = c("source", "code")
  )

  return(aq_data)
}
