#' worker function that downloads data from a range of networks run by Ricardo
#' @noRd
readUKAQData <-
  function(site = "my1",
           year = 2009,
           data_type = "hourly",
           pollutant = "all",
           hc = FALSE,
           ratified = FALSE,
           to_narrow = FALSE,
           verbose = FALSE,
           source = "aurn",
           url_data,
           lmam_subfolder) {
    # add to path if source = "local"
    if (source == "local") {
      url_data <- paste0(url_data, lmam_subfolder, "/")
    }

    # For file name matching, needs to be exact
    site <- toupper(site)

    files <- paste0(site, "_", year)

    # Download and load data.
    thedata <-
      suppressWarnings(loadData(
        x = files,
        verbose = verbose,
        url_data = url_data,
        data_type = data_type
      ))

    # suppress warnings for now - unequal factors, harmless
    if (is.null(thedata)) {
      cli::cli_abort("No data to import for {.arg site} {.field {site}} and {.arg year} {.field {year}} from {.arg source} {.field {source}}.")
    }

    # Return if no data
    if (nrow(thedata) == 0) {
      return()
    } ## no data

    # change names
    names(thedata) <- tolower(names(thedata))

    # change nox as no2
    id <- which(names(thedata) %in% "noxasno2")
    if (length(id) == 1) names(thedata)[id] <- "nox"

    # change code to character
    thedata$code <- as.character(thedata$code)

    # should hydrocarbons be imported?
    if (!hc) {
      ## no hydrocarbons - therefore select conventional pollutants
      theNames <- c(
        "site", "code", "date", "co", "nox", "no2", "no", "o3", "so2", "pm10",
        "pm2.5", "v10", "v2.5", "nv10", "nv2.5", "gr_pm10", "gr_pm2.5",
        "ws", "wd", "temp"
      )

      thedata <- select(thedata, any_of(theNames), matches("_qc"))
    } else {
      thedata <- dplyr::relocate(thedata, dplyr::any_of(c("site", "code", "date")))
    }

    # rename "temp" to "air_temp" if appropriate
    if ("temp" %in% names(thedata)) {
      thedata <- rename(thedata, air_temp = temp)
    }

    # if particular pollutants have been selected
    if (pollutant[1] != "all") {
      thedata <-
        dplyr::select(thedata, "date", "site", "code",
                      dplyr::any_of(c(pollutant, "ws", "wd", "air_temp")))
    }

    # make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"

    # tidy data if requested
    if (to_narrow) {
      # variables to select or not select
      the_vars <- c(
        "date", "site", "code",
        "latitude", "longitude", "site_type",
        "ws", "wd", "air_temp"
      )

      thedata <-
        tidyr::pivot_longer(thedata,
          cols = -dplyr::any_of(the_vars),
          names_to = "pollutant"
        )

      # clean tidied data before returning
      thedata <- thedata %>%
        dplyr::relocate(dplyr::any_of(the_vars)) %>%
        dplyr::arrange(site, code, pollutant, date)
    }

    # add source to output
    thedata <-
      dplyr::mutate(thedata, source = source, .before = dplyr::everything())

    as_tibble(thedata)
  }

#' Define downloading and loading function
#' @noRd
loadData <- function(x, verbose, url_data, data_type) {
  tryCatch(
    {
      # Build the file name
      fileName <- paste0(
        url_data, x,
        ".RData"
      )

      # Load the rdata object
      con <- url(fileName)
      load(con)

      # Find appropriate extension per `data_type`
      x <- switch(data_type,
        hourly = x,
        `15min` = paste0(x, "_15min"),
        daily = paste0(x, "_daily_mean"),
        `8_hour` = paste0(x, "_8hour_mean"),
        `24_hour` = paste0(x, "_24hour_mean"),
        daily_max_8 = paste0(x, "_daily_max_8hour")
      )

      # Gravimetric PM is in separate file
      # These are measured daily PM measurements rather than daily mean hourly
      x2 <- gsub("_daily_mean", "_daily", x)

      # Reasign
      dat <- get(x)

      # if there are two daily data frames to combine
      if (data_type == "daily" & exists(x2)) {
        dat2 <- get(x2)
        dat <- left_join(dat, dat2,
          by = c("date", "site", "code")
        )

        lookup <- c(gr_pm2.5 = "GR2.5", gr_pm10 = "GR10")

        dat <- dat %>%
          rename(any_of(lookup))
      }

      # make sure class is correct for lubridate
      class(dat$date) <- c("POSIXct", "POSIXt")

      return(dat)
    },
    error = function(ex) {
      # Print a message
      if (verbose) {
        cli::cli_warn(c("i" = "{x} does not exist - ignoring that one."))
        return(NULL)
      }
    },
    finally = {
      close(con)
    }
  )
}

#' function to read annual or monthly files
#' @noRd
readSummaryData <-
  function(files, year, source, data_type, to_narrow, meta, hc) {
    fileName <- purrr::map(year, function(x) paste0(files, x, ".rds")) %>%
      purrr::list_c()

    thedata <- try(readRDS(url(fileName)), TRUE)

    if (inherits(thedata, "try-error")) {
      return()
    }

    names(thedata)[names(thedata) == "NOXasNO2.mean"] <- "nox"
    names(thedata)[names(thedata) == "NOXasNO2.capture"] <-
      "nox_capture"
    names(thedata) <- tolower(names(thedata))
    names(thedata) <- gsub(".mean", "", names(thedata))
    names(thedata) <- gsub(".capture", "_capture", names(thedata))

    if (!hc) {
      thedata <- thedata %>%
        select(any_of(
          c(
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
        ))
    }

    if (data_type == "monthly") {
      thedata <- mutate(thedata, date = ymd(date, tz = "UTC"))
    }

    if (data_type == "annual") {
      thedata <- rename(thedata, date = year) %>%
        drop_na(date) %>%
        mutate(date = ymd(paste0(date, "-01-01"), tz = "UTC"))
    }

    if (to_narrow) {
      # make sure numbers are numbers
      values <- select(thedata, !contains("capture")) %>%
        select(!matches("uka_code"))

      capture <-
        select(thedata, contains("capture") | c(code, date, site)) %>%
        select(!matches("uka_code"))

      values <- pivot_longer(values,
        -c(date, code, site),
        values_to = "value",
        names_to = "species"
      )

      capture <- pivot_longer(capture,
        -c(date, code, site),
        values_to = "data_capture",
        names_to = "species"
      )

      capture$species <- gsub("_capture", "", capture$species)

      thedata <- full_join(values, capture,
        by = c("date", "code", "site", "species")
      )
    }

    thedata <- thedata %>%
      mutate(
        site = as.character(site),
        code = as.character(code)
      ) %>%
      mutate(
        source = source,
        .before = dplyr::everything()
      )

    return(thedata)
  }

#' Function to read DAQI data from a file
#' @noRd
readDAQI <- function(files, year, source) {
  fileName <- purrr::map(year, function(x) paste0(files, x, ".rds")) %>%
    purrr::list_c()

  thedata <- try(readRDS(url(fileName)), TRUE)

  if (inherits(thedata, "try-error")) {
    return()
  }

  thedata <- thedata %>%
    mutate(
      code = as.character(code),
      site = as.character(site),
      pollutant = as.character(pollutant),
      date = ymd(Date, tz = "GMT"),
      measurement_period = as.character(measurement_period)
    ) %>%
    select(-Date) %>%
    relocate(date, .after = pollutant) %>%
    mutate(
      source = source,
      .before = dplyr::everything()
    )

  return(thedata)
}

#' function to add meta data based on network and supply of aq data
#' @param source network of interest (e.g., "aurn")
#' @param aq_data imported air quality data (annual, daqi, or otherwise)
#' @noRd
add_meta <- function(source, columns, aq_data) {
  meta_data <-
    importMeta(source = unique(source),
               all = TRUE,
               duplicate = TRUE)

  meta_data <- distinct(meta_data, source, site, .keep_all = TRUE) %>%
    select(source, site, code, dplyr::all_of(columns))

  aq_data <- left_join(aq_data, meta_data, by = c("source", "code", "site"))

  return(aq_data)
}

#' Function to add ratified flag to hourly data
#' @noRd
add_ratified <- function(aq_data, source, to_narrow) {
  meta <-
    importMeta(unique(source), all = T) %>%
    dplyr::filter(
      code %in% aq_data$code,
      !variable %in% c(
        "V10", "NV10", "V2.5", "NV2.5",
        "ws", "wd", "temp"
      )
    ) %>%
    dplyr::select(source, code, variable, ratified_to) %>%
    dplyr::mutate(variable = tolower(variable))

  if (to_narrow) {
    meta <-
      dplyr::rename(meta, "pollutant" = variable, "qc" = ratified_to) %>%
      dplyr::filter(pollutant %in% tolower(aq_data$pollutant))
    aq_data <-
      aq_data %>%
      dplyr::left_join(meta, by = dplyr::join_by(source, code, pollutant)) %>%
      dplyr::mutate(qc = date <= .data$qc)

    return(aq_data)
  }

  meta <-
    meta %>%
    dplyr::filter(variable %in% names(aq_data)) %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = ratified_to,
      names_glue = "{variable}_qc"
    )

  aq_data <-
    aq_data %>%
    dplyr::left_join(meta, by = dplyr::join_by(source, code)) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("_qc"), function(x) {
      date <= x
    }))

  return(aq_data)
}


#' Function to filter annual/DAQI stats using
#' @param site,pollutant,to_narrow Inherits from parent function
#' @noRd
filter_site_pollutant <- function(aq_data, site, pollutant, to_narrow, data_type) {
  # if site isn't missing, filter by sites
  if (any(site != "all")) {
    aq_data <- aq_data[tolower(aq_data$code) %in% tolower(site), ]
  }

  # if pollutant isn't "all", filter pollutants
  if (any(pollutant != "all")) {
    polls <- paste(c("source", "uka_code", "code", "site", "date", "pollutant", pollutant), collapse = "|")
    if (data_type != "daqi") {
      if (to_narrow) {
        aq_data <- aq_data[grepl(polls, aq_data$species, ignore.case = TRUE), ]
      } else {
        aq_data <- aq_data[grepl(polls, names(aq_data), ignore.case = TRUE)]
      }
    } else {
      aq_data <- aq_data[grepl(polls, aq_data$pollutant, ignore.case = TRUE), ]
    }
  }

  # return output
  return(aq_data)
}

#' Helper function to guess the source of UKAQ data
#' @param site Sites passed to [importUKAQ()]
#' @noRd
guess_source <- function(site) {
  ukaq_meta <- importMeta("ukaq") %>%
    dplyr::mutate(source = factor(.data$source, c("aurn", "saqn", "aqe", "waqn", "ni", "local"))) %>%
    dplyr::arrange(.data$source) %>%
    dplyr::distinct(.data$site, .data$latitude, .data$longitude, .keep_all = TRUE)
  
  source_tbl <-
    data.frame(code = toupper(site)) %>%
    dplyr::left_join(ukaq_meta, by = "code")

  if (any(is.na(source_tbl$source))) {
    ambiguous_codes <-
      source_tbl %>% 
      dplyr::filter(is.na(.data$source)) %>%
      dplyr::pull(.data$code)

    cli::cli_abort(
      c(
        "x" = "Unknown site codes detected. Please ensure all site codes can be found in {.fun importMeta}.",
        "i" = "Unknown site codes: {ambiguous_codes}"
      )
    )
  }

  if (nrow(source_tbl) > length(site)) {
    source_tbl_all <- source_tbl

    source_tbl <- dplyr::slice_head(source_tbl, n = 1, by = "code")
    
    source_tbl_other <-
      dplyr::anti_join(source_tbl_all, source_tbl, by = join_by("code", "source", "site", "latitude", "longitude", "site_type"))

    alternatives <-
      source_tbl_other %>%
      dplyr::mutate(str = paste0(.data$code, " (could also be '", .data$site, "' from the source: '", .data$source, "'.)")) %>%
      dplyr::pull(.data$str)

    msg <- c(
      "x" = "Ambiguous site codes detected. National networks are imported preferentially to locally managed networks.",
      alternatives,
      "i" = "Specify {.field source} to import sites from specific monitoring networks.")

    names(msg)[names(msg) == ""] <- "!"
    
    cli::cli_warn(msg)
  }

  return(source_tbl$source)
}
