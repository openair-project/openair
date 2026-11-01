# importUKAQ tests
#
# The first sections run offline (and on CRAN) by replacing the downloading of
# files (`read_remote_file()`) and metadata (`importMeta()`) with fake versions
# that mimic the structure of the files on the WSP servers. The final section
# checks the real servers, so is skipped on CRAN and if they can't be reached.

# --- Fake WSP files ----------------------------------------------------------

fake_hours <- as.POSIXct("2020-01-01", tz = "GMT") + 3600 * 0:47
fake_days <- as.POSIXct("2020-01-01", tz = "GMT") + 86400 * 0:1

# one site-year .RData file, with hourly, daily mean and gravimetric objects
fake_site_file <- function(code, year = 2020, site = paste("Site", code)) {
  file <- paste0(code, "_", year)
  env <- new.env()
  env[[file]] <- data.frame(
    date = fake_hours,
    NO = 1,
    NO2 = 2,
    NOXasNO2 = 3,
    O3 = 4,
    PM10 = 5,
    ETHANE = 6,
    wd = 180,
    ws = 2,
    temp = 10,
    site = site,
    code = factor(code)
  )
  env[[paste0(file, "_daily_mean")]] <- data.frame(
    date = fake_days,
    NO2 = 2,
    PM10 = 5,
    site = site,
    code = factor(code)
  )
  env[[paste0(file, "_daily")]] <- data.frame(
    date = fake_days,
    GR10 = 15,
    site = site,
    code = factor(code)
  )
  env
}

fake_annual <- data.frame(
  Year = 2020L,
  code = factor(c("MY1", "NOTT")),
  site = factor(c("Site MY1", "Site NOTT")),
  NO.mean = 1,
  NO.capture = 90,
  NO2.mean = 2,
  NO2.capture = 91,
  NOXasNO2.mean = 3,
  NOXasNO2.capture = 92,
  O3.mean = 4,
  O3.capture = 93,
  O3.summer.capture = 94,
  O3.mean.daily.max.8hour = 5,
  PM10.mean = 6,
  PM10.capture = 95,
  GR10.mean = 7,
  GR10.capture = 96,
  ETHANE.mean = 8,
  ETHANE.capture = 97
)

fake_monthly <- data.frame(
  Date = factor(c("2020-01-01", "2020-02-01")),
  code = factor("MY1"),
  site = factor("Site MY1"),
  NO2.mean = 2,
  NO2.capture = 91
)

fake_daqi <- data.frame(
  Date = factor("2020-01-01"),
  code = factor("MY1"),
  site = factor("Site MY1"),
  pollutant = factor(c("no2", "o3", "pm10", "pm2.5")),
  concentration = 10,
  poll_index = c(1L, 4L, 7L, 10L),
  measurement_period = factor("Daily mean")
)

# metadata, as returned by `importMeta(all = TRUE)`
fake_meta <- data.frame(
  source = c(rep("aurn", 4), "aurn", "saqn", "local", "local"),
  code = c(rep("MY1", 4), "NOTT", "GLA4", "EL3", "EL3"),
  site = c(
    rep("Site MY1", 4),
    "Site NOTT",
    "Site GLA4",
    "Site EL3",
    "Site EL3"
  ),
  site_type = c(
    rep("Urban Traffic", 4),
    "Urban Background",
    "Urban Traffic",
    "Urban Traffic",
    "Unknown"
  ),
  latitude = c(rep(51.52, 4), 52.95, 55.86, 51.40, 51.41),
  longitude = c(rep(-0.15, 4), -1.15, -4.26, -0.34, -0.35),
  variable = c("NO2", "O3", "NOx", "PM10", "NO2", "NO2", "NO2", "NO2"),
  ratified_to = as.POSIXct(
    c(rep("2020-01-01 12:00", 6), NA, NA),
    tz = "GMT"
  ),
  zone = "Zone",
  agglomeration = "Agglomeration",
  local_authority = "Authority",
  provider = c(rep(NA, 6), "Londonair", "AQDM"),
  pcode = c(rep(NA, 6), "london", "aqdm")
)

fake_import_meta <- function(source = "aurn", ...) {
  if ("ukaq" %in% source) {
    source <- ukaq_networks$source
  }
  fake_meta[fake_meta$source %in% source, ]
}

# replace downloads with `files`, a named list of fake files where the names are
# (part of) their URLs; returns a function listing the URLs requested
local_fake_ukaq <- function(files = list(), env = parent.frame()) {
  requested <- character()

  local_mocked_bindings(
    read_remote_file = function(url) {
      requested <<- c(requested, url)
      hit <- purrr::keep(names(files), ~ grepl(.x, url, fixed = TRUE))
      if (length(hit) == 0) NULL else files[[hit[1]]]
    },
    importMeta = fake_import_meta,
    .env = env
  )

  function() requested
}

aurn_url <- "https://uk-air.defra.gov.uk/openair/R_data/"

# --- Argument checks ---------------------------------------------------------

test_that("invalid arguments give informative errors", {
  local_fake_ukaq()
  rlang::local_options(rlib_warning_verbosity = "quiet")

  expect_error(
    importUKAQ("my1", 2020, source = "aurn", data_type = "anual"),
    "annual"
  )
  expect_error(importUKAQ("my1"), "year")
  expect_error(importUKAQ(year = 2020), "site.*must be supplied")
  expect_error(importUKAQ(year = 2020, data_type = "annual"), "source")
  expect_error(importUKAQ("my1", 2020, source = "aurm"), "aurn")
  expect_error(
    importUKAQ(c("my1", "nott", "gla4"), 2020, source = c("aurn", "saqn")),
    "Length of"
  )
  expect_error(
    importUKAQ("el3", 2020, source = "local", data_type = "daqi"),
    "not available for locally managed"
  )
  expect_error(
    importUKAQ(
      "my1",
      2020,
      source = "aurn",
      meta = TRUE,
      meta_columns = "provider"
    )
  )
})

test_that("`ratified` warns for data types other than hourly", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  expect_warning(
    importUKAQ(
      "my1",
      2020,
      source = "aurn",
      data_type = "daily",
      ratified = TRUE,
      progress = FALSE
    ),
    "only available for hourly"
  )
})

test_that("`source` and `data_type` are case-insensitive and resolve aliases", {
  requested <- local_fake_ukaq(list(GLA4_2020 = fake_site_file("GLA4")))

  out <- importUKAQ(
    "GLA4",
    2020,
    source = "SAQD",
    data_type = "Hourly",
    progress = FALSE
  )

  expect_equal(unique(out$source), "saqn")
  expect_equal(
    requested(),
    "https://www.scottishairquality.scot/openair/R_data/GLA4_2020.RData"
  )
})

# --- Site data ---------------------------------------------------------------

test_that("hourly data is tidied consistently", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  out <- importUKAQ("my1", 2020, source = "aurn", progress = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out)[1], "source")
  expect_true(all(c("date", "site", "code") %in% names(out)))
  expect_equal(nrow(out), 48L)
  expect_type(out$code, "character")
  expect_type(out$source, "character")
  expect_equal(attr(out$date, "tzone"), "GMT")
  expect_true(all(c("nox", "air_temp", "ws", "wd") %in% names(out)))
  expect_false(any(c("noxasno2", "temp", "ethane") %in% names(out)))
})

test_that("`hc = TRUE` keeps hydrocarbons", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  out <- importUKAQ("my1", 2020, source = "aurn", hc = TRUE, progress = FALSE)

  expect_true("ethane" %in% names(out))
})

test_that("`pollutant` is case-insensitive and keeps meteorological data", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  out <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    pollutant = c("NO2", "o3"),
    progress = FALSE
  )

  expect_equal(
    sort(names(out)),
    sort(c(
      "source",
      "date",
      "site",
      "code",
      "no2",
      "o3",
      "ws",
      "wd",
      "air_temp"
    ))
  )
})

test_that("`meteo = FALSE` drops meteorological data", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  out <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    meteo = FALSE,
    progress = FALSE
  )

  expect_false(any(c("ws", "wd", "air_temp") %in% names(out)))
})

test_that("daily data includes gravimetric PM, including when `pollutant` is set", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  out <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    data_type = "daily",
    progress = FALSE
  )
  out_pm10 <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    data_type = "daily",
    pollutant = "pm10",
    progress = FALSE
  )

  expect_equal(nrow(out), 2L)
  expect_true(all(c("no2", "pm10", "gr_pm10") %in% names(out)))
  expect_equal(
    names(out_pm10),
    c("source", "date", "site", "code", "pm10", "gr_pm10")
  )
})

test_that("`to_narrow = TRUE` returns a pollutant column", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  out <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    pollutant = c("no2", "o3"),
    to_narrow = TRUE,
    progress = FALSE
  )

  expect_true(all(c("pollutant", "value") %in% names(out)))
  expect_setequal(unique(out$pollutant), c("no2", "o3"))
  expect_equal(nrow(out), 96L)
})

test_that("output is sorted by source and site", {
  local_fake_ukaq(list(
    MY1_2020 = fake_site_file("MY1", site = "B site"),
    NOTT_2020 = fake_site_file("NOTT", site = "A site"),
    GLA4_2020 = fake_site_file("GLA4", site = "C site")
  ))

  out <- importUKAQ(
    c("gla4", "my1", "nott"),
    2020,
    source = c("saqn", "aurn", "aurn"),
    pollutant = "no2",
    progress = FALSE
  )

  expect_equal(
    unique(paste(out$source, out$code)),
    c("aurn NOTT", "aurn MY1", "saqn GLA4")
  )
})

test_that("an error is raised if no data can be imported", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  # no 15 minute data in the file
  expect_error(
    importUKAQ(
      "my1",
      2020,
      source = "aurn",
      data_type = "15min",
      progress = FALSE
    ),
    "No data returned"
  )
  expect_error(
    importUKAQ("my1", 2019, source = "aurn", progress = FALSE),
    "No data returned"
  )
})

test_that("`verbose = TRUE` reports all failed downloads in one warning", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  expect_silent(
    importUKAQ(
      c("my1", "nott"),
      2019:2020,
      source = "aurn",
      pollutant = "no2",
      progress = FALSE
    )
  )

  warning <- expect_warning(
    out <- importUKAQ(
      c("my1", "nott"),
      2019:2020,
      source = "aurn",
      pollutant = "no2",
      verbose = TRUE,
      progress = FALSE
    )
  )
  msg <- cli::ansi_strip(conditionMessage(warning))

  expect_match(msg, "3 site-year combinations", fixed = TRUE)
  expect_match(msg, "MY1 (aurn): 2019", fixed = TRUE)
  expect_match(msg, "NOTT (aurn): 2019, 2020", fixed = TRUE)
  expect_equal(unique(out$code), "MY1")
})

test_that("`ratified = TRUE` adds qc flags", {
  local_fake_ukaq(list(MY1_2020 = fake_site_file("MY1")))

  wide <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    pollutant = c("no2", "o3"),
    ratified = TRUE,
    progress = FALSE
  )
  narrow <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    pollutant = c("no2", "o3"),
    ratified = TRUE,
    to_narrow = TRUE,
    progress = FALSE
  )

  # ratified up to midday on the first day
  expect_equal(wide$no2_qc, rep(c(TRUE, FALSE), each = 24))
  expect_equal(wide$o3_qc, rep(c(TRUE, FALSE), each = 24))
  expect_type(narrow$qc, "logical")
  expect_equal(sum(narrow$qc), 48L)
})

test_that("`meta = TRUE` adds metadata columns by site code", {
  local_fake_ukaq(list(
    MY1_2020 = fake_site_file("MY1", site = "A different name")
  ))

  out <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    pollutant = "no2",
    meta = TRUE,
    meta_columns = c("site_type", "zone"),
    progress = FALSE
  )

  expect_equal(unique(out$site_type), "Urban Traffic")
  expect_equal(unique(out$zone), "Zone")
})

test_that("locally managed data tries every provider's subfolder", {
  rlang::local_options(rlib_warning_verbosity = "quiet")
  requested <- local_fake_ukaq(list(`aqdm/EL3_2020` = fake_site_file("EL3")))

  expect_no_warning(
    out <- importUKAQ(
      "el3",
      2020,
      source = "local",
      pollutant = "no2",
      verbose = TRUE,
      progress = FALSE
    )
  )

  expect_equal(nrow(out), 48L)
  expect_setequal(
    requested(),
    paste0(
      "https://uk-air.defra.gov.uk/openair/LMAM/R_data/",
      c("london", "aqdm"),
      "/EL3_2020.RData"
    )
  )
})

test_that("locally managed data warns about quality control and ratification", {
  local_fake_ukaq(list(`aqdm/EL3_2020` = fake_site_file("EL3")))
  rlang::local_options(rlib_warning_verbosity = "verbose")

  expect_warning(
    importUKAQ(
      "el3",
      2020,
      source = "local",
      pollutant = "no2",
      progress = FALSE
    ),
    "locally managed"
  )

  out <- suppressWarnings(
    importUKAQ(
      "el3",
      2020,
      source = "local",
      pollutant = "no2",
      ratified = TRUE,
      progress = FALSE
    )
  )
  expect_true(all(is.na(out$no2_qc)))
})

test_that("`source` is guessed from site codes, preferring national networks", {
  local_fake_ukaq(list(
    MY1_2020 = fake_site_file("MY1"),
    GLA4_2020 = fake_site_file("GLA4")
  ))

  out <- importUKAQ(c("my1", "gla4"), 2020, pollutant = "no2", progress = FALSE)

  expect_equal(unique(paste(out$source, out$code)), c("aurn MY1", "saqn GLA4"))
})

test_that("unknown site codes give an error from importUKAQ()", {
  local_fake_ukaq()

  err <- expect_error(importUKAQ("zzz9", 2020), "Unknown site codes")
  expect_equal(conditionCall(err)[[1]], quote(importUKAQ))
})

test_that("wrappers import from their network", {
  requested <- local_fake_ukaq(list(BEL0_2020 = fake_site_file("BEL0")))

  out <- importNI("bel0", 2020, progress = FALSE)

  expect_equal(unique(out$source), "ni")
  expect_equal(
    requested(),
    "https://www.airqualityni.co.uk/openair/R_data/BEL0_2020.RData"
  )
})

# --- Network statistics ------------------------------------------------------

test_that("annual statistics are imported for all sites by default", {
  requested <- local_fake_ukaq(list(summary_annual_AURN_2020 = fake_annual))

  out <- importUKAQ(
    year = 2020,
    source = "aurn",
    data_type = "annual",
    progress = FALSE
  )

  expect_equal(requested(), paste0(aurn_url, "summary_annual_AURN_2020.rds"))
  expect_equal(out$code, c("MY1", "NOTT"))
  expect_equal(out$date, as.POSIXct(c("2020-01-01", "2020-01-01"), tz = "UTC"))
  expect_true(all(
    c("nox", "nox_capture", "o3.daily.max.8hour") %in% names(out)
  ))
  expect_false("ethane" %in% names(out))
})

test_that("annual statistics can be filtered by site and pollutant", {
  local_fake_ukaq(list(summary_annual_AURN_2020 = fake_annual))

  no <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    data_type = "annual",
    pollutant = "NO",
    progress = FALSE
  )
  o3 <- importUKAQ(
    year = 2020,
    source = "aurn",
    data_type = "annual",
    pollutant = "o3",
    progress = FALSE
  )
  pm10 <- importUKAQ(
    year = 2020,
    source = "aurn",
    data_type = "annual",
    pollutant = "pm10",
    progress = FALSE
  )

  id_cols <- c("source", "code", "site", "date")

  expect_equal(no$code, "MY1")
  expect_setequal(names(no), c(id_cols, "no", "no_capture"))
  expect_setequal(
    names(o3),
    c(id_cols, "o3", "o3_capture", "o3.summer_capture", "o3.daily.max.8hour")
  )
  expect_setequal(
    names(pm10),
    c(id_cols, "pm10", "pm10_capture", "gr10", "gr10_capture")
  )
})

test_that("annual statistics in narrow format have values and data capture", {
  local_fake_ukaq(list(summary_annual_AURN_2020 = fake_annual))

  out <- importUKAQ(
    year = 2020,
    source = "aurn",
    data_type = "annual",
    pollutant = c("no2", "o3"),
    to_narrow = TRUE,
    progress = FALSE
  )

  expect_true(all(c("species", "value", "data_capture") %in% names(out)))
  expect_setequal(
    unique(out$species),
    c("no2", "o3", "o3.summer", "o3.daily.max.8hour")
  )
  expect_equal(out$data_capture[out$code == "MY1" & out$species == "no2"], 91)
})

test_that("monthly statistics have monthly dates", {
  local_fake_ukaq(list(summary_monthly_AURN_2020 = fake_monthly))

  out <- importUKAQ(
    year = 2020,
    source = "aurn",
    data_type = "monthly",
    progress = FALSE
  )

  expect_equal(out$date, as.POSIXct(c("2020-01-01", "2020-02-01"), tz = "UTC"))
})

test_that("DAQI data has bands and can be filtered by pollutant", {
  requested <- local_fake_ukaq(list(annual_DAQI_AURN_2020 = fake_daqi))

  out <- importUKAQ(
    year = 2020,
    source = "aurn",
    data_type = "daqi",
    progress = FALSE
  )
  pm <- importUKAQ(
    "my1",
    2020,
    source = "aurn",
    data_type = "daqi",
    pollutant = "PM2.5",
    progress = FALSE
  )

  expect_equal(requested()[1], paste0(aurn_url, "annual_DAQI_AURN_2020.rds"))
  expect_equal(levels(out$band), c("Low", "Moderate", "High", "Very High"))
  expect_equal(
    as.character(out$band),
    c("Low", "Moderate", "High", "Very High")
  )
  expect_equal(pm$pollutant, "pm2.5")
})

# --- Helpers -----------------------------------------------------------------

test_that("network names are normalised", {
  expect_equal(
    normalise_source(c("AURN", "saqd", "LMAM", "niaqn", "kcl", "waqn")),
    c("aurn", "saqn", "local", "ni", "imperial", "waqn")
  )
})

test_that("network details can be looked up", {
  expect_equal(ukaq_network_info(c("waqn", "local"), "abbr"), c("WAQ", "LMAM"))
  expect_equal(ukaq_network_info("aurn", "url"), aurn_url)
})

# --- Real servers ------------------------------------------------------------

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

ni_hourly <- tryCatch(
  suppressWarnings(importUKAQ(
    "bel0",
    2023,
    source = "ni",
    ratified = TRUE,
    progress = FALSE
  )),
  error = function(e) NULL
)

skip_if(is.null(ni_hourly), "WSP servers cannot be reached")

test_that("real hourly data has the expected structure", {
  expect_equal(nrow(ni_hourly), 8760L)
  expect_equal(names(ni_hourly)[1], "source")
  expect_true(all(c("date", "site", "code") %in% names(ni_hourly)))
  expect_true(all(
    c("no2", "no2_qc", "ws", "wd", "air_temp") %in% names(ni_hourly)
  ))
  expect_type(ni_hourly$no2_qc, "logical")
})

test_that("real sources can be guessed", {
  out <- suppressWarnings(importUKAQ(
    "bel0",
    2023,
    pollutant = "no2",
    progress = FALSE
  ))
  expect_equal(unique(out$source), "ni")
})

test_that("real daily data includes gravimetric PM", {
  out <- importUKAQ(
    "born",
    2001,
    source = "aurn",
    data_type = "daily",
    pollutant = "pm10",
    progress = FALSE
  )
  expect_true("gr_pm10" %in% names(out))
})

test_that("real network statistics have the expected structure", {
  annual <- importUKAQ(
    year = 2023,
    source = "ni",
    data_type = "annual",
    pollutant = "no2",
    progress = FALSE
  )
  monthly <- importUKAQ(
    year = 2023,
    source = "ni",
    data_type = "monthly",
    pollutant = "no2",
    progress = FALSE
  )
  daqi <- importUKAQ(
    year = 2023,
    source = "ni",
    data_type = "daqi",
    progress = FALSE
  )

  expect_true(all(c("no2", "no2_capture") %in% names(annual)))
  expect_equal(length(unique(monthly$date)), 12L)
  expect_s3_class(daqi$band, "factor")
})

test_that("real locally managed data can be imported", {
  out <- suppressWarnings(
    importUKAQ(
      "el3",
      2023,
      source = "local",
      pollutant = "no2",
      progress = FALSE
    )
  )
  expect_equal(nrow(out), 8760L)
})
