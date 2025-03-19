#' Import air quality data from European database until February 2024
#'
#' This function is a simplified version of the `saqgetr` package (see
#' <https://github.com/skgrange/saqgetr>) for accessing European air quality
#' data. As `saqgetr` was retired in February 2024, this function has also been
#' retired, but can still access European air quality data up until that
#' retirement date. Consider using the EEA Air Quality Download Service instead
#' (<https://eeadmz1-downloads-webapp.azurewebsites.net/>).
#'
#' @param site The code of the site(s).
#' @param year Year or years to import. To import a sequence of years from 1990
#'   to 2000 use `year = 1990:2000`. To import several specific years use `year
#'   = c(1990, 1995, 2000)` for example.
#' @param tz Not used
#' @param meta Should meta data be returned? If `TRUE` the site type, latitude
#'   and longitude are returned.
#' @param to_narrow By default the returned data has a column for each
#'   pollutant/variable. When `to_narrow = TRUE` the data are stacked into a
#'   narrow format with a column identifying the pollutant name.
#' @param progress Show a progress bar when many sites/years are being imported?
#'   Defaults to `TRUE`.
#'
#' @return a [tibble][tibble::tibble-package]
#' @family import functions
#' @export
#'
#' @examples
#'
#' # import data for Stuttgart Am Neckartor (S)
#' \dontrun{
#' stuttgart <- importEurope("debw118", year = 2010:2019, meta = TRUE)
#' }
#'
importEurope <- function(site = "debw118",
                         year = 2018,
                         tz = "UTC",
                         meta = FALSE,
                         to_narrow = FALSE,
                         progress = TRUE) {
  # warn/error w/ deprecation
  msg <-
    c("!" = "{.fun importEurope} has been discontinued and cannot import data after February 2024.",
      "i" = "Consider using the EEA Air Quality Download Service instead {.url https://eeadmz1-downloads-webapp.azurewebsites.net/}")
  if (year > 2024) {
    cli::cli_abort(msg)
  } else {
    cli::cli_inform(msg,
                    .frequency = "regularly",
                    .frequency_id = "europe")
  }
  
  site <- tolower(site)
  
  # The directory
  remote_path <-
    "http://aq-data.ricardo-aea.com/R_data/saqgetr/observations"
  
  # Produce file names
  file_remote <- crossing(site = site,
                          year = year) %>%
    arrange(site,
            year) %>%
    mutate(
      file_remote = paste0(
        remote_path,
        "/",
        year,
        "/",
        "air_quality_data_site_",
        site,
        "_",
        year,
        ".csv.gz"
      )
    ) %>%
    pull(file_remote)
  
  # Load files
  if (progress)
    progress <- "Importing Air Quality Data"
  df <- purrr::map(file_remote,
                   ~ get_saq_observations_worker(file = .x, tz = tz),
                   .progress = progress) %>%
    purrr::list_rbind()
  
  if (nrow(df) == 0L) {
    warning("No data available,")
    return()
  }
  
  # just hourly observations
  df <- filter(df, summary == 1)
  
  if (!to_narrow) {
    df <- make_saq_observations_wider(df)
  } else {
    df <- select(df, -summary, -process, -validity)
  }
  
  # don't need end date
  df <- select(df, -date_end) %>%
    rename(code = site)
  
  if (meta) {
    meta <- importMeta("europe")
    df <- left_join(df, meta, by = "code")
  }
  
  df <- arrange(df, code, date)
  
  return(df)
}


get_saq_observations_worker <- function(file, tz) {
  # Read data
  df <- read_saq_observations(file, tz)
  
  if (nrow(df) == 0) {
    return()
  }
  
  df <- filter(df, validity %in% c(1, 2, 3) | is.na(validity))
  
  return(df)
}


# Reading function
read_saq_observations <- function(file, tz = tz, verbose) {
  # Data types
  col_types <- cols(
    date = col_character(),
    date_end = col_character(),
    site = col_character(),
    variable = col_character(),
    process = col_integer(),
    summary = col_integer(),
    validity = col_integer(),
    unit = col_character(),
    value = col_double()
  )
  
  # Create gz connection
  con <- file %>%
    url() %>%
    gzcon()
  
  df <- tryCatch({
    # Read and parse dates, quiet supresses time zone conversion messages and
    # warning supression is for when url does not exist
    suppressWarnings(
      readr::read_csv(con, col_types = col_types, progress = FALSE) %>%
        mutate(
          date = lubridate::ymd_hms(date, tz = tz, quiet = TRUE),
          date_end = lubridate::ymd_hms(date_end, tz = tz, quiet = TRUE)
        )
    )
  },
  error = function(e) {
    # Close the connection on error
    close.connection(con)
    tibble()
  })
  
  if (nrow(df) == 0) {
    warning(paste(basename(file), "is missing."))
  }
  
  return(df)
}


make_saq_observations_wider <- function(df) {
  tryCatch({
    df %>%
      select(date,
             date_end,
             site,
             variable,
             value) %>%
      spread(variable, value)
  },
  error = function(e) {
    warning(
      "Duplicated date-site-variable combinations detected, observations have been removed...",
      call. = FALSE
    )
    
    df %>%
      select(date,
             date_end,
             site,
             variable,
             value) %>%
      distinct(date,
               site,
               variable,
               .keep_all = TRUE) %>%
      spread(variable, value)
  })
}
