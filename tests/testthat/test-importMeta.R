# importMeta tests
#
# The first sections run offline (and on CRAN) by replacing the downloading of
# metadata files (`read_remote_file()`) with fake versions that mimic the raw
# metadata on the WSP servers. The final section checks the real servers, so
# is skipped on CRAN and if they can't be reached.

# --- Fake WSP metadata -------------------------------------------------------

fake_raw_meta <- function(
  code,
  site,
  location_type,
  latitude,
  longitude,
  parameter,
  start_date,
  end_date,
  ...
) {
  data.frame(
    site_id = code,
    site_name = site,
    location_type = location_type,
    latitude = latitude,
    longitude = longitude,
    parameter = parameter,
    Parameter_name = parameter,
    start_date = start_date,
    end_date = end_date,
    ratified_to = "2020-12-31",
    zone = "Zone",
    agglomeration = NA,
    local_authority = "Authority",
    ...
  )
}

fake_aurn_meta <- fake_raw_meta(
  code = c("MY1", "MY1", "MY1", "NOTT", "NOTT", "CLOS", "GLA4"),
  site = c(
    rep("London Marylebone Road", 3),
    rep("Nottingham Centre", 2),
    "Closed Site",
    "Glasgow Kerbside"
  ),
  location_type = c(
    rep("Urban traffic", 3),
    rep("Urban Background", 2),
    "unknown unknown",
    "Urban Traffic"
  ),
  latitude = c(rep(51.52, 3), rep(52.95, 2), 53, 55.86),
  longitude = c(rep(-0.15, 3), rep(-1.15, 2), -1, -4.26),
  parameter = c("NO2", "NOXasNO2", "O3", "NO2", "BENZENE", "NO2", "NO2"),
  start_date = c(
    "1997-01-01",
    "1997-01-01",
    "2010-01-01",
    "2000-01-01",
    "2000-01-01",
    "1990-01-01",
    "2000-01-01"
  ),
  end_date = c(
    "ongoing",
    "ongoing",
    "ongoing",
    "ongoing",
    "2005-06-30",
    "2000-12-31",
    "ongoing"
  )
)

# GLA4 is in both the AURN and SAQN
fake_saqn_meta <- fake_raw_meta(
  code = "GLA4",
  site = "Glasgow Kerbside",
  location_type = "Urban Traffic",
  latitude = 55.86,
  longitude = -4.26,
  parameter = "NO2",
  start_date = "2000-01-01",
  end_date = "ongoing"
)

# EL3 is listed by two local providers
fake_local_meta <- fake_raw_meta(
  code = c("EL3", "EL3"),
  site = c(
    "Elmbridge - Hampton Court Parade",
    "Elmbridge Hampton Court Parade"
  ),
  location_type = c("unknown unknown", "Urban traffic"),
  latitude = c(51.401, 51.402),
  longitude = c(-0.343, -0.344),
  parameter = "NO2",
  start_date = "2009-01-01",
  end_date = "ongoing",
  provider = c("Londonair", "AQDM"),
  pcode = c("london", "aqdm")
)

# local metadata has no ratification information
fake_local_meta$ratified_to <- NULL

# replace downloads with `files`, a named list of fake metadata where the
# names are their network abbreviations (e.g., "AURN"); returns a function
# listing the URLs requested
local_fake_meta <- function(
  files = list(
    AURN = fake_aurn_meta,
    SCOT = fake_saqn_meta,
    LMAM = fake_local_meta
  ),
  env = parent.frame()
) {
  requested <- character()

  local_mocked_bindings(
    read_remote_file = function(url) {
      requested <<- c(requested, url)
      abbr <- sub("_metadata.RData$", "", basename(url))

      if (is.null(files[[abbr]])) {
        return(NULL)
      }

      meta_env <- new.env()
      meta_env$meta <- files[[abbr]]
      meta_env
    },
    .env = env
  )

  function() requested
}

# --- Argument checks ---------------------------------------------------------

test_that("invalid arguments give informative errors", {
  local_fake_meta()

  expect_error(importMeta("aurm"), "aurn")
  expect_error(importMeta(lat = 51.5), "Both `lat` and `lng`")
  expect_error(importMeta(lat = c(51, 52), lng = c(-1, -2)), "single number")
  expect_error(importMeta(lat = "51.5", lng = -0.1), "single number")
  expect_warning(importMeta(max_n = 3), "ignored unless")
})

test_that("network names are case-insensitive and resolve aliases", {
  requested <- local_fake_meta()

  out <- importMeta("SAQD")

  expect_equal(unique(out$source), "saqn")
  expect_equal(
    requested(),
    "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData"
  )
})

test_that("`source = 'ukaq'` imports all UK networks", {
  requested <- local_fake_meta(list(
    AURN = fake_aurn_meta,
    AQE = fake_aurn_meta,
    SCOT = fake_saqn_meta,
    WAQ = fake_saqn_meta,
    NI = fake_saqn_meta,
    LMAM = fake_local_meta
  ))

  importMeta("ukaq")

  expect_equal(
    basename(requested()),
    paste0(ukaq_networks$abbr, "_metadata.RData")
  )
})

# --- Basic metadata ----------------------------------------------------------

test_that("default metadata has one row per site with key columns", {
  local_fake_meta()

  out <- importMeta("aurn")

  expect_s3_class(out, "tbl_df")
  expect_equal(
    names(out),
    c("source", "site", "code", "latitude", "longitude", "site_type")
  )
  expect_equal(out$code, c("MY1", "NOTT", "CLOS", "GLA4"))
  expect_equal(
    out$site_type,
    c("Urban Traffic", "Urban Background", "Unknown", "Urban Traffic")
  )
})

test_that("`all = TRUE` returns a row per pollutant with dates", {
  local_fake_meta()

  out <- importMeta("aurn", all = TRUE)

  expect_equal(nrow(out), nrow(fake_aurn_meta))
  expect_true(all(
    c("variable", "start_date", "end_date", "ratified_to") %in% names(out)
  ))
  expect_true("NOx" %in% out$variable)
  expect_false("NOXasNO2" %in% out$variable)
  expect_s3_class(out$start_date, "POSIXct")
  expect_s3_class(out$end_date, "POSIXct")

  # "ongoing" monitoring has no end date
  expect_equal(is.na(out$end_date), fake_aurn_meta$end_date == "ongoing")
})

# --- Filtering ---------------------------------------------------------------

test_that("`year` returns sites open throughout the given years", {
  local_fake_meta()

  expect_equal(importMeta("aurn", year = 1995)$code, "CLOS")
  expect_equal(importMeta("aurn", year = 2003)$code, c("MY1", "NOTT", "GLA4"))
  expect_equal(importMeta("aurn", year = 1999:2003)$code, "MY1")

  # with all = TRUE, years apply to each pollutant
  out <- importMeta("aurn", year = 2008, all = TRUE)
  expect_false("BENZENE" %in% out$variable)
  expect_false("O3" %in% out$variable)
})

test_that("`pollutant` matches exactly and case-insensitively", {
  local_fake_meta()

  expect_equal(importMeta("aurn", pollutant = "O3")$code, "MY1")
  expect_equal(importMeta("aurn", pollutant = "nox")$code, "MY1")
  expect_equal(importMeta("aurn", pollutant = "hc")$code, "NOTT")
  expect_equal(
    importMeta("aurn", pollutant = c("o3", "benzene"))$code,
    c("MY1", "NOTT")
  )
  expect_warning(importMeta("aurn", pollutant = "no"), "No metadata")
})

test_that("`code`, `site` and `site_type` filter the metadata", {
  local_fake_meta()

  expect_equal(
    importMeta("aurn", code = c("my1", "NOTT"))$code,
    c("MY1", "NOTT")
  )
  expect_equal(importMeta("aurn", site = "nottingham")$code, "NOTT")
  expect_equal(
    importMeta("aurn", site = c("london", "glasgow"))$code,
    c("MY1", "GLA4")
  )
  expect_equal(
    importMeta("aurn", site_type = "urban traffic")$code,
    c("MY1", "GLA4")
  )
})

test_that("filtering away all sites returns an empty data frame with a warning", {
  local_fake_meta()

  expect_warning(out <- importMeta("aurn", code = "zzz9"), "No metadata")
  expect_equal(nrow(out), 0L)
})

test_that("`duplicate` controls sites in multiple networks", {
  local_fake_meta()

  expect_equal(sum(importMeta(c("aurn", "saqn"))$code == "GLA4"), 1L)
  expect_equal(
    sum(importMeta(c("aurn", "saqn"), duplicate = TRUE)$code == "GLA4"),
    2L
  )
})

test_that("local sites listed by multiple providers are kept", {
  local_fake_meta()

  out <- importMeta("local", all = TRUE)

  expect_equal(out$pcode, c("london", "aqdm"))
  expect_false("ratified_to" %in% names(out))
})

test_that("sites can be filtered by distance", {
  skip_if_not_installed("sf")
  local_fake_meta()

  nearest <- importMeta("aurn", lat = 51.5, lng = -0.14, max_n = 2)
  near <- importMeta("aurn", lat = 51.5, lng = -0.14, max_dist = 50)

  expect_equal(nearest$code, c("MY1", "NOTT"))
  expect_false(is.unsorted(nearest$distance_km))
  expect_equal(near$code, "MY1")
})

# --- Unavailable networks ----------------------------------------------------

test_that("unavailable networks are skipped with a warning", {
  local_fake_meta(list(AURN = fake_aurn_meta))

  expect_warning(
    out <- importMeta(c("aurn", "saqn")),
    "Could not import metadata for `source` \"saqn\""
  )
  expect_equal(unique(out$source), "aurn")
})

test_that("an error is raised if no networks are available", {
  local_fake_meta(list())

  expect_error(
    suppressWarnings(importMeta(c("aurn", "saqn"))),
    "Could not import metadata for any"
  )
})

# --- Real servers ------------------------------------------------------------

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

aurn_meta <- tryCatch(
  importMeta("aurn", all = TRUE),
  error = function(e) NULL
)

skip_if(is.null(aurn_meta), "WSP servers cannot be reached")

test_that("real metadata has the expected structure", {
  expect_true(all(
    c(
      "source",
      "code",
      "site",
      "site_type",
      "latitude",
      "longitude",
      "variable",
      "start_date",
      "end_date",
      "ratified_to",
      "zone",
      "agglomeration",
      "local_authority"
    ) %in%
      names(aurn_meta)
  ))
  expect_s3_class(aurn_meta$ratified_to, "POSIXct")
  expect_true("NOx" %in% aurn_meta$variable)
})

test_that("real metadata has one row per site by default", {
  out <- importMeta("aurn")
  expect_false(anyDuplicated(out$code) > 0)
  expect_gt(nrow(out), 100)
})

test_that("real metadata can be filtered", {
  no <- importMeta("aurn", pollutant = "no", all = TRUE)
  my1 <- importMeta("aurn", code = "my1")

  expect_equal(unique(no$variable), "NO")
  expect_equal(my1$site, "London Marylebone Road")
})

test_that("real local metadata includes provider codes", {
  out <- importMeta("local", all = TRUE)
  expect_true(all(c("provider", "pcode") %in% names(out)))
  expect_false(anyNA(out$pcode))
})
